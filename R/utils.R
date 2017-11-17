#' Extract objects from a model
#'
#' @param mod an object of type "lm" or "coxph"
#' @param vector A character vector of object to extract
#'
#' @return A vector
#' @export
#'
#' @examples
extract_from_model <- function(mod, vector){
  extracted <- mod %>%
    broom::tidy() %>%
    dplyr::filter(term != "(Intercept)" & term != "Residuals")

  extract2(extracted, vector) %>%
    setNames(extracted$term)
}

is_model_possible <- function(mod){
  isTRUE(class(mod)[[1]] == "lm" && df.residual(mod) != 0 & deviance(mod) >= sqrt(.Machine$double.eps) | class(mod)[[1]] != "lm")
}


get_choix_var <- function(tab){
  lab <- Hmisc::label(tab)
  names(tab) %>%
    setNames(lab) %>%
    sort
}

drop_levels <- function(tab){
  exLabels <- Hmisc::label(tab)
  tab %<>% droplevels()
  Hmisc::label(tab, self = FALSE) <- exLabels
  tab
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
#' @param var_ajust A character vector of adjustment variables
#'
#' @return
#' @export
#'
#' @examples
get_nvar_mod <- function(tab, var_ajust){
  if(!is.null(var_ajust)) {
    tab %<>%
      dplyr::select(-dplyr::one_of(var_ajust))
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
    if (is.na(x) | is.nan(x)){
      "-"
    } else if  (x > 1E6) "+Inf"
    else if  (x < -1E6) "-Inf"
    else if (abs(x) > 1E-6){
      puiss <- floor(log10(abs(x)) + 1)
      nsmall <- ifelse (puiss >= 3, 0, digits - puiss)
      if (nsmall < 0) nsmall <- 0
      base::format(x, digits = digits, nsmall = nsmall)
    }
    else {
      base::format(0)
    }
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
add_varname.default <- function(x, y, noms, ...){
  add_column(x, id = noms, variable = label(y), .before = 1)
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
add_varname.boot <- function(tableRet, resBoot, noms){
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
    data_frame(id = id, variable = variable, niveau = niveau, multiple = multiple)
  }) %>%
    bind_cols(as_data_frame(tableRet)) %>%
    add_class("tabboot")
}

#' Get the number(s) formatted in percentage
#'
#' @param nb a numeric vector
#' @param symbol displays "%"
#'
#' @return a character vector
#' @export
#'
#' @examples
pourcent <- function(nb, symbol = TRUE){
  map_chr(nb, function(x){
    if (!is.nan(x)){
      arrondi <- min(x, 1 - x, na.rm = TRUE) %>%
        signif(2) %>%
        get_nb_decimals
      val <- round(x, arrondi)%>%
        multiply_by(100) %>%
        base::format()
      if (symbol) paste0(val, "%")
      else val
    } else "-"
  })
}

get_nb_decimals <- function(x){
  if (grepl("\\.", x)) {
    as.character(x) %>%
      strsplit("\\.") %>%
      extract2(1) %>%
      magrittr::extract(2) %>%
      nchar
  }
  else {
    0
  }
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
    prepare_table_export() %>%
    knitr::kable()
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

#' @export
tidy.anova <- function(x, ...){
  renamers <- c(Df = "df", `Sum Sq` = "sumsq", `Mean Sq` = "meansq",
                `F value` = "statistic", `Pr(>F)` = "p.value", Res.Df = "res.df",
                RSS = "rss", `Sum of Sq` = "sumsq", F = "statistic",
                Chisq = "statistic", `P(>|Chi|)` = "p.value", Pr..Chisq. = "p.value",
                p.value = "p.value", Chi.sq = "statistic", edf = "edf",
                Ref.df = "ref.df", LR.Chisq = "statistic", `Pr(>|Chi|)` = "p.value",
                loglik = "logLik")
  names(renamers) <- make.names(names(renamers))
  x <- fix_data_frame(x)
  unknown_cols <- base::setdiff(colnames(x), c("term", names(renamers)))
  if (length(unknown_cols) > 0) {
    warning("The following column names in ANOVA output were not ",
            "recognized or transformed: ", paste(unknown_cols,
                                                 collapse = ", "))
  }
  ret <- plyr::rename(x, renamers, warn_missing = FALSE)
  if (!is.null(ret$term)) {
    ret <- ret %>% mutate(term = stringr::str_trim(term))
  }
  ret
}

remove_big_vif <- function(tab, type_var, vardep, vars, infl, elimine) {
  ajust <- infl[base::which(names(infl) %in% type_var)]
  while (length(ajust) > 0 && max(ajust) > 5 & length(vars) > 1){
    gros <- names(ajust[which.max(ajust)])
    type_var <- type_var[type_var != gros]
    vars <- vars[vars != gros]
    elimine <- append(elimine, gros)
    if (length(vars) > 1){
      formule <- paste(vardep, "~", paste(vars, collapse = "+"))
      if (type == "logistic")
        mod <- arm::bayesglm(as.formula(formule), data = tab, family = "binomial")
      else if (type == "linear")
        mod <- lm(as.formula(formule), data = tab)
      else if (type == "survival"){
        tab2 <- dplyr::select(tab, .time, !!rlang::sym(vardep), !!rlang::sym(vars)) %>%
          na.exclude()
        formule <- sprintf("Surv(.time, %s) ~ %s", vardep, paste(vars, collapse = "+"))
        mod <- survival::coxph(formula = as.formula(formule), data = tab2)
      }
      infl <- suppressWarnings(car::vif(mod))
      if(!is.null(dim(infl)))
        infl <- infl[, 1, drop = TRUE]
      ajust <- infl[base::which(names(infl) %in% type_var)]
    }
  }
  elimine
}
