#' Extract objects from a model
#'
#' @param model an object of type "lm" or "coxph"
#' @param vector A character vector of object to extract
#'
#' @return A vector
#' @export
#'
#' @examples
extract_from_model <- function(model, vector){
  extracted <- model %>%
    broom::tidy() %>%
    dplyr::filter(term != "(Intercept)" & term != "Residuals")

  extract2(extracted, vector) %>%
    setNames(extracted$term)
}

is_model_possible <- function(model){
  isTRUE(class(model)[1] == "lm" && df.residual(model) != 0 && deviance(model) >= sqrt(.Machine$double.eps) |
           inherits(model, "glm") && isTRUE(model$converged) |
           inherits(model, "coxph") && !any(is.nan(sqrt(diag(model$var)))))
}

remove_alias <- function(vars, mod, correction = FALSE) {
  tab_mod <- broom::tidy(mod)
  alias <- dplyr::filter(tab_mod, is.nan(statistic) | is.infinite(statistic) | abs(estimate) < .Machine$double.eps) %>%
    magrittr::extract2("term")
  alias <- c(alias, names(which(is.na(coef(mod)))))
  tab <- mod$model[-1]
  expanded_vars <- map(names(tab), function(x){
    if (is.factor(tab[[x]])){
      paste0(x, levels(tab[[x]]))
    } else x
  })
  vari <- map_lgl(expanded_vars, ~ isTRUE(any(alias %in% .)))
  if(correction){
    corrected <- map_lgl(vars[vari], function(x){
      ligne <- which(names(mod$xlevels) == x)
      if(length(ligne)){
        nb <- paste0(names(mod$xlevels[ligne]), mod$xlevels[[ligne]]) %in% alias %>%
          sum()
        if(nb == length(mod$xlevels[[ligne]]) - 1) TRUE else FALSE
      } else TRUE
    })
    vari[which(vari)] <- corrected
  }

  return(vari)

}

get_big_vif <- function(tab, vardep, varindep, varajust, type, mod, left_form){
  vars <- c(varindep, varajust)
  elimine <- character(0)

  if(length(varindep) > 1) { ## first, test multicolinearity among varindep only
    mod_indep <- update_mod(tab, mod, vardep, varindep, type, left_form)
    infl <- suppressWarnings(car::vif(mod_indep))
    if(!is.null(dim(infl))) infl <- infl[, 1, drop = TRUE]
    elimine <- remove_big_vif(tab, vardep, varindep, character(0), type, infl)
    if(length(elimine)){
      varindep <- remove_elements(varindep, elimine)
      vars <- c(varindep, varajust)
      if (length(vars) > 1){
        mod <- update_mod(tab, mod, vardep, vars, type, left_form)
        infl <- suppressWarnings(car::vif(mod))
        if(!is.null(dim(infl))) infl <- infl[, 1, drop = TRUE]
      }
    }
  }


  if (length(vars) > 1){
    infl <- suppressWarnings(car::vif(mod))
    if(!is.null(dim(infl))) infl <- infl[, 1, drop = TRUE]
    elimine_ajust <- remove_big_vif(tab, vardep, varindep, varajust, type, infl, only_varajust = TRUE) # ##  test multicolinearity among varajust + varindep only without removing varindep
    if(length(elimine_ajust)){
      vars <- remove_elements(vars, elimine_ajust)
      if (length(vars) > 1){
        mod <- update_mod(tab, mod, vardep, vars, type, left_form)
        infl <- suppressWarnings(car::vif(mod))
        if(!is.null(dim(infl))) infl <- infl[, 1, drop = TRUE]
      }
      elimine <- c(elimine, elimine_ajust)
    }
    # big_vif <- which(infl > 5)
    # if (length(big_vif)){
    #   if (varindep[1] %in% names(big_vif)) infl[varindep[1]] <- 0
    #   remaining_varindep <- intersect(vars, varindep)
    #   if (all(remaining_varindep %in% names(big_vif))) infl[remaining_varindep] <- 0
      elimine <- append(elimine,
                        remove_big_vif(tab, vardep, intersect(varindep, vars), intersect(varajust, vars), type, infl, only_varajust = TRUE)) # if necessary, remove all other vars
    #}
  }
  return(elimine)
}

get_choix_var <- function(tab){
  lab <- label(tab)
  names(tab) %>%
    setNames(lab) %>%
    sort()
}

drop_levels <- function(tab, remove = FALSE){
  exLabels <- label(tab)
  tab %<>% droplevels()
  label(tab, self = FALSE) <- exLabels
  if (remove){
    enleve <- map(seq_len(ncol(tab)), function(i){
      x <- tab[[i]]
      if (count_items(x) < 2) i
    }) %>% compact() %>% flatten_dbl()
    if(length(enleve)) tab <- tab[-enleve]
  }

  return(tab)
}

prepare_model <- function(tab, remove = FALSE){
  na.exclude(tab) %>%
    drop_levels(remove)
}

#' Get the nearest (ceiling) thousand
#'
#' @param x a length one numeric vector
#'
#' @return the nearest thousand
#' @export
#'
#' @examples
nearest_up_thousand <- function(x){
  order <- 10^3
  divide_by(x, order) %>%
    ceiling %>%
    magrittr::multiply_by(order)
}


#' Get the predicted number of terms in a statistical model
#'
#' @param tab The data frame
#' @param varajust A character vector of adjustment variables
#'
#' @return
#' @export
#'
#' @examples
get_nvar_mod <- function(tab, varajust){
  if(!is.null(varajust)) {
    tab %<>%
      dplyr::select(-dplyr::one_of(varajust))
  }
  map_dbl(tab, function(x){
    if(is.numeric(x)) 1
    else if (is.factor(x)) nlevels(x) - 1
  }) %>%
    sum() %>%
    magrittr::subtract(1)
}

#' Format numbers
#'
#' @param numbers a numeric vector
#' @param digits number of digits
#'
#' @return a character vector
#' @export
#'
#' @examples
format_number <- function(numbers, digits = 3){
  map_chr(numbers, function(x){
    if (is.nan(x)) return("NaN")
    if (is.na(x)) return("-")
    if  (x > 1E6) return("+Inf")
    if  (x < -1E6) return("-Inf")
    if (abs(x) < 1E-4) return(base::format(0))
    puiss <- floor(log10(abs(x)) + 1)
    nsmall <- ifelse (puiss >= 3, 0, digits - puiss)
    if (nsmall < 0) nsmall <- 0
    if (digits < 0) {
      x <- round(x, digits)
      digits <- 0
    }
    base::format(x, digits = digits, nsmall = nsmall)
  })
}

#' Use sprintf for displaying numbers with the right digit number
#'
#' @param string the string
#' @param ... values to be passed into string. Only logical, integer, real and character vectors are supported, but some coercion will be done: see the ‘Details’ section. Up to 100.
#'
#' @return A formatted string
#' @export
#'
#' @examples
sprintf_number_table <- function(string, ...){
  .dots <- list(...) %>%
    map_if(is.numeric, format_number)
  do.call(sprintf, c(list(fmt = string), .dots))
}

remove_na <- function(x, y, ..., drop_factor = FALSE){
  dots <- list(...)
  if(length(dots) > 0){
    names(dots) <- paste0("V", seq_along(dots))
    supp <- as_tibble(dots)
  }
  else supp <- NULL
  no_na <- tibble(x, y) %>%
    bind_cols(supp)
  if(!any(label(no_na) == ".missing", na.rm = TRUE)) no_na %<>% na.exclude
  if (any(map_lgl(no_na, is.factor)) & drop_factor){
    no_na <- mutate_if(no_na, is.factor, fct_drop)
  }
  label(no_na, self = FALSE)[c("x","y")] <- c(label(x), label(y))
  no_na
}


#' @export
add_varname <- function(x, y, ...){
  UseMethod("add_varname", y)
}

#' @export
add_varname.default <- function(tab, x, noms, ...){
  add_column(tab, id = noms, variable = label(x), .before = 1)
}

#' @export
add_varname.factor <- function(tab, x, noms, one_line = FALSE, add_niveau = TRUE){
  if (one_line) add_varname.default(tab, x, noms)
  else {
    id <- c(rep(noms, nlevels(x)))
    variable <- c(rep(label(x), nlevels(x)))
    niveau <- levels(x)
    if (add_niveau) add_column(tab, id, variable, niveau, .before = 1)
    else add_column(tab, id, variable, .before = 1)
  }
}

#' @export
add_varname.boot <- function(tableRet, resBoot){
  map2_df(resBoot$data[-1], names(resBoot$data[-1]), function(x,y){
    if(is.numeric(x)) {
      variable <- label(x)
      if(grepl("^I\\(", y)){
        rec <- stringr::str_match(y, "^I\\((.*?)/([0-9]\\.?[0-9]*)\\)")
        multiple <- as.numeric(rec[[3]])
        id <- rec[[2]]
      } else {
        multiple <- 1
        id <- y
      }
      niveau <- ""
    }
    else {
      id <- rep(y, nlevels(x) - 1)
      variable <- rep(label(x), nlevels(x) - 1)
      niveau <- sprintf("%s vs %s", levels(x)[-1], rep(levels(x)[1], nlevels(x) - 1))
      multiple <- NA
    }
    tibble(id = id, variable = variable, niveau = niveau, multiple = multiple)
  }) %>%
    bind_cols(as_tibble(tableRet)) %>%
    add_class("tabboot")
}

#' Get the number(s) formatted in percentage
#'
#' @param nb a numeric vector
#' @param symbol displays "\%"
#'
#' @export
#'
#' @examples
pourcent <- function(nb, symbol = TRUE, arrondi = NULL){
  map_chr(nb, function(x){
    if (is.nan(x) | is.na(x)) return("-")
    if (is.null(arrondi)) arrondi <- 2
    val <- multiply_by(x, 100)
    val <- if (val > 1E-3) {
      base::format(val, digits = arrondi, nsmall = arrondi - 2)
    } else 0

    if (symbol) paste0(val, "%") else format(val)
  })
}

#' Nice display of the analysis
#'
#' @param table The analysed data frame
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
show_table_markdown <- function(table){
  table %>%
    prepare_table_export()
}

prepare_table_export <- function(tab){
  tab %<>% select(-variable)
  if (nrow(tab) > 1){
    tab$id[2:nrow(tab)] <- NA
  }
  tab
}

are_enough_levels <- function(tab, x){
  extract2(tab, x) %>%
    as.factor() %>%
    fct_drop() %>%
    nlevels() %>%
    is_greater_than(1)
}

are_enough_cor <- function(tab, x, y, univ){
  min_rows <- ifelse(univ, 2L, 3L)
  if (is.numeric(tab[[x]]) & is.numeric(tab[[y]])) {
    nrow(tab) > min_rows
  } else if (is.factor(tab[[x]])) {
    all(table(tab[[x]]) > min_rows)
  } else {
    TRUE
  }
}

remove_big_vif <- function(tab, vardep, varindep, varajust, type, infl, only_varajust = FALSE) {
  vars <- c(varindep, varajust)
  selected_vars <- (if(only_varajust) varajust else vars)
  ajust <- infl[which(names(infl) %in% selected_vars)]
  elimine <- character(0)

  while (length(ajust) > 0 && max(ajust) > 5 & length(vars) > 1){
    big_vif <- which(ajust > 5)
    if (varindep[1] %in% names(big_vif)) ajust[varindep[1]] <- 0

    gros <- names(ajust[which.max(ajust)])
    selected_vars <- selected_vars[selected_vars != gros]
    vars <- vars[vars != gros]
    elimine <- append(elimine, gros)
    if (length(vars) > 1){
      formule <- paste(vardep, "~", paste(vars, collapse = "+"))
      if (type == "logistic")
        model <- glm(as.formula(formule), data = tab, family = "binomial")
      else if (type == "linear")
        model <- lm(as.formula(formule), data = tab)
      else if (type == "survival"){
        tab2 <- tab[, c(".time", vardep, vars)] %>%
          na.exclude()
        formule <- sprintf("Surv(.time, %s) ~ %s", vardep, paste(vars, collapse = "+"))
        model <- survival::coxph(formula = as.formula(formule), model = TRUE, data = tab2)
      }
      alias <- remove_alias(vars, model)
      while (any(alias)){
        elimine <- append(elimine, vars[alias])
        vars <- vars[!alias]
        model <- update_mod(tab, model, vardep, vars, type, sprintf("Surv(.time, %s)", vardep))
        alias <- remove_alias(vars, model)
      }
      infl <- suppressWarnings(car::vif(model))
      if(!is.null(dim(infl))) infl <- infl[, 1, drop = TRUE]
      ajust <- infl[which(names(infl) %in% selected_vars)]
    }
  }
  elimine
}

#' @export
add_class <- function(x, classe){
  structure(x, class = c(classe, class(x)))
}


no_multibyte <- function(x){
  UseMethod("no_multibyte")
}

no_multibyte.data.frame <- function(x){
  map_lgl(x, function(y){
    no_multibyte.default(y)
  }) %>%
    all(na.rm = TRUE)
}

no_multibyte.default <- function(x){
  stri_enc_isutf8(x) %>%
    all(na.rm = TRUE)
}

remove_multibyte <- function(x){
  UseMethod("remove_multibyte")
}
remove_multibyte.data.frame <- function(x){
  modify_if(x, Negate(no_multibyte.default), function(y) {
    remove_multibyte.default(y)
  })
}

remove_multibyte.default <- function(x){
  iconv(x, sub="")
}

remove_multibyte_if_any <- function(x){
  if(!no_multibyte(x)){
    remove_multibyte(x)
  } else {
    x
  }
}

solve_contrast <- function(tab, vardep, x, univ = FALSE) {
  if(!is.null(x)){
    if (identical(class(x), class(tab[[vardep]])) && isTRUE(all.equal(x, tab[[vardep]], check.attributes = FALSE))) return(TRUE)
    if(count_items(x) < 2) return(FALSE)
    tmp <- data.frame(a = x, b = tab[[vardep]]) %>%
      na.exclude()
    if (is.factor(tmp$a) & is.factor(tmp$b)){
      if (length(compare_terms(tmp))) return(FALSE)
    }
    are_enough_cor(tmp, "a", "b", univ) &&
      are_enough_levels(tmp, "a") && are_enough_levels(tmp, "b")
  } else FALSE
}

update_mod <- function(tab, model, vardep, vars, type, left_form = NULL){
  stats::update(model,
                       as.formula(
                         sprintf("%s ~ %s",
                                 ifelse(type == "survival", left_form, vardep),
                                 paste(vars, collapse = " + ")
                         )),
                       data = tab)
}

compare_terms <- function(tmp){
  f1 <- as.formula(sprintf("~ %s - 1", names(tmp)[1]))
  f2 <- as.formula(sprintf("~ %s - 1", names(tmp)[2]))
  ma <- model.matrix(f1, data = tmp)
  mb <- model.matrix(f2, data = tmp)
  m <- cbind(ma, mb)
  l <- ncol(m)
  map(seq_len(l), function(i){
    if (i < l){
      map(seq.int(i + 1, l), function(j){
        if (sum(m[, i] == m[, j]) == nrow(m)){
          names(tmp)
        }
      }) %>% compact()
    }
  }) %>% compact()
}

identical_model_frame <- function(tab, formula, type){
  mf <- model.frame(formula, data = tab)
  if (type == "survival"){
    exLabel <- names(which(label(tab) == attr(mf[[1]], "inputAttributes")$event$label))
    mf <- cbind(mf[[1]][, 2], mf)
    mf[[2]] <- mf[[2]][, 1]
    names(mf)[1] <- ".time"
    names(mf)[2] <- exLabel
  }
  l <- ncol(mf)
  ide <- map(seq_len(l), function(i){
    if (i < l){
      map(seq.int(i + 1, l), function(j){
        tmp <- mf[c(i,j)] %>%
          na.exclude()
        if (is.factor(tmp[[1]]) & is.factor(tmp[[2]])) {
          compare_terms(tmp) %>%
          unlist() %>%
          unique()
        } else if (sum(as.numeric(mf[[i]]) == as.numeric(mf[[j]])) > 0.95 * nrow(mf)){
                names(mf)[c(i, j)]
        }
      }) %>%
        compact()
    }
  }) %>%
    compact()
  if(length(ide)){
    reduce(ide, union)
  } else NULL
}

#' @export
remove_elements <- function(vector, ...){
  dots <- list(...) %>% unlist
  vector[!vector %in% dots]
}

#' @export
is_entier <- function(x){
  if(is.factor(x)) {
    lev <- suppressWarnings(as.numeric(as.character(levels(x))))
    all(is_wholenumber(lev), na.rm = TRUE) & nlevels(x) < 10 & nlevels(x) >= 2 &
      all(lev < 10, na.rm = TRUE)
  } else {
    lev <- unique(na.exclude(x))
    all(is_wholenumber(lev), na.rm = TRUE) & length(lev) < 10 & length(lev) >= 2 &
      all(lev < 10, na.rm = TRUE)
  }
}

is_wholenumber <-
  function(x, tol = .Machine$double.eps^0.5)  {
    if (any(is.na(x)))
      FALSE
    else
      abs(x - round(x)) < tol
  }

#' Find object in the parents of the calling environment
#'
#' @param name a length 1 character vector of the objet to find
#' @param parent list of environments
#'
#' @return the last environment where the  object is found
#'
find_env <- function(name, parents = parent.frame()){
  n <- 0
  env <- sys.frame(n)
  while(!identical(env, parents)){
    if (name %in% ls(env)) {
      found <-  env
    }
    n <- n + 1
    env <- sys.frame(n)
  }
  return(found)
}

create_tab_cens <- function(x, time, censure){
  tab_cens <- remove_na(time, x, censure, drop_factor = TRUE)
  names(tab_cens) <- c(".time", "x", "censure")
  tab_cens$censure %<>% as.character() %>% as.numeric()
  tab_cens
}


coef_to_prob <- function(x) exp(x) / (1 + exp(x))


regroup_quantile_calibration <- function(x, quantiles){
  q <- quantiles
  quantiles[length(quantiles)] <- 1
  x$pred <- cut(x$M, breaks = quantiles,
                labels = ((quantiles+lag(quantiles))/2)[-1]) %>%
    as.character() %>%
    as.numeric()
  if(is.factor(x$D)){
    x$D <- as.numeric(x$D) - 1
  }
  x %>%
    group_by(pred) %>%
    summarise(obs = list(binom.test(sum(D), length(D))))
}

#' @export
add_elements <- function(vector, ...){
  dots <- list(...) %>% unlist
  append(vector, dots)
}

keep_warning <- function(expr) {
  myWarnings <- NULL
  wHandler <- function(w) {
    if(!grepl("Vectorizing",w$message)) myWarnings <<- c(myWarnings, w$message)
    invokeRestart("muffleWarning")
  }
  val <- withCallingHandlers(expr, warning = wHandler)
  if (is.null(val)) return(NULL)
  structure(val, warning = myWarnings)
}

filter_glm_fit <- function(mod, tab, varindep, varajust, pred = 0){
  tab %<>% na.exclude()
  eps <- .Machine$double.eps*1000
  p <- predict(mod, type = "response")
  t <- tab[p < 1-eps & p > eps,]
  if(pred == 2){
    allVars <- prepare_variables(t, varindep, varajust, pred)
    formule <- sprintf(". ~ %s", paste(purrr::flatten_chr(allVars), collapse = " + "))
    mod2 <- try2(update(mod, formula = formule, data = t))
  } else {
    mod2 <- try2(update(mod, data = t))
  }
  if (is_error(mod2)) return(NULL)
  idx <- if (inherits(mod, "coxph")) c(1,3) else 1:2
  if (all(round(coef(summary(mod))[, idx], 2) == round(coef(summary(mod2))[, idx], 2), na.rm = TRUE)) {
    return(mod2)
  } else {
    return(NULL)
  }
}

#' @export
get_min_class <- function(tab, vardep, type = "logistic"){
  min(table(tab[[vardep]]))
}

#' @export
find_best_precision <- function(variable){
  range(variable, na.rm = TRUE) %>%
    base::diff() %>%
    log10() %>%
    ceiling() %>%
    subtract(2) %>%
    10^.
}

qc <- function(...){
  exprs(...) %>%
    map_chr(as_name) %>%
    unname()
}

remove_missing_levels <- function(tab, mod){
  UseMethod("remove_missing_levels", mod)
}

remove_missing_levels.mira <- function(tab, mod){
  remove_missing_levels.default(tab, getfit(mod, 1))
}

remove_missing_levels.default <- function(tab, mod){
  train_lev <- mod$xlevels
  if(!length(train_lev)) return(tab)
  test_lev <- map(tab, function(x) levels(x))
  all_vars <- test_lev[names(train_lev)]
  if(identical(all_vars, train_lev)) return(tab)

  differences <- map(seq_along(all_vars), function(i) {
    setdiff(all_vars[[i]], train_lev[[i]])
  }) %>% setNames(names(all_vars))

  for (i in seq_along(differences)){
    if(length(differences[[i]])){
      levels(tab[[names(differences[i])]])[levels(tab[[names(differences[i])]]) == differences[[i]]] <- NA
    }
  }
  tab
}

tryCatch_all <- function(expr) {
  warn <- err <- NULL
  value <- withCallingHandlers(
    tryCatch(expr, error=function(e) {
      err <<- e
      NULL
    }), warning=function(w) {
      warn <<- w
      invokeRestart("muffleWarning")
    })
  structure(list(value=value, warning=warn, error=err),
            class = unique(c(class(err), class(warn), class(value))))
}

#' Simple tryCatch for errors and warnings
#'
#' @param expr expression to be evaluated.
#' @param errors character vector containing error message to bypass
#' @param warnings character vector containing warning message to bypass
#'
#' @return
#' @export
#'
try2 <- function(expr, errors, warnings){
  if(missing(errors)) errors <- NULL
  if(missing(warnings)) warnings <- NULL
  res <- tryCatch_all(expr)
  if (is_error(res)){
    all_cond <- map_lgl(errors, grepl, res$error$message)
    if (all(all_cond == FALSE, na.rm = TRUE)){
      warning(paste("Error: ", res$error$message))
    }
    return(structure(list(), message = res$error$message, class = "error"))
  }
  if (is_warning(res)){
    all_cond <- map_lgl(warnings, grepl, res$warning$message)
    if (all(all_cond == FALSE, na.rm = TRUE)){
      warning(paste("Error warning: ", res$warning$message))
    }
    return(structure(res$value, message = res$warning$message, class = unique(c(class(res$value), "warning"))))
  }
  res$value
}

#' Helper function to get error or warning class
#' @param x a variable
#' @export
is_error <- function(x){
  inherits(x, c("error", "try-error"))
}

#' @rdname is_error
#' @export
is_warning <- function(x){
  inherits(x, "warning") | !is.null(attr(x, "warning"))
}

#' @export
count_items <- function(x){
  length(unique(na.exclude(x)))
}
