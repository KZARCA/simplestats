#' @export
create_table_model <- function(mod, varajust = NULL, pred = 0){
  if (pred) c2 <- "pred"
  else if (inherits(mod, "mira")){
    c2 <- paste0("tab", class(getfit(mod, 1))[[1]])
  } else {
    c2 <- paste0("tab", class(mod)[[1]])
  }
  get_coefficients(mod, pred) %>%
    add_varname(mod) %>%
    {if (!pred) add_pval_glob(., mod) else (.)} %>%
    #dplyr::filter(!grepl("rcs\\(.*\\)[0-9]*", .$term)) %>%
    {if (!pred) dplyr::filter(., !id %in% varajust) else (.)} %>%
    add_class(c2)
}

#' @export
get_coefficients <- function(x, ...){
  UseMethod("get_coefficients")
}

#' @export
get_coefficients.lm <- function(mod, pred = 0){
  coef <- tidy(mod, conf.int = FALSE)
  confint <- NULL
  if (pred < 2) try(confint <- broom:::broom_confint_terms(mod) %>% select(-term), silent = FALSE)
  if (!is.null(confint)){
    res <- bind_cols(coef, confint) %>%
      dplyr::select(term, estimate, conf.low, conf.high, p.value)
  } else {
    res <- dplyr::select(coef, term, estimate, p.value)
  }
  if (pred == 0) {
    res <- slice(res, -1)
  }
  res
}

#' @export
get_coefficients.coxph <- function(mod, pred = 0){
  coef <- tidy(mod, conf.int = FALSE)
  confint <- NULL
  if (pred < 2) try(confint <- broom:::broom_confint_terms(mod) %>% select(-term), silent = FALSE)
  if (!is.null(confint)){
    res <- bind_cols(coef, confint) %>%
      dplyr::select(term, estimate, conf.low, conf.high, p.value)
  } else {
    res <- dplyr::select(coef, term, estimate, p.value)
  }
  res
}

#' @export
get_coefficients.mira <- function(mod, pred = 0){
  if (length(getfit(mod)) == 1) return(get_coefficients(getfit(mod, 1)))
  res <- mice::pool(mod) %>%
    summary(conf.int = TRUE) %>%
    as_tibble() %>%
    dplyr::select(term, estimate, conf.low = `2.5 %`, conf.high = `97.5 %`, p.value)
  if (pred == 0 && inherits(getfit(mod, 1), "lm")){
    res <- dplyr::slice(res, -1)
  }
  res
}

add_pval_glob <- function(tab_mod, mod, en_test = FALSE , pred = FALSE){
  vec_pval <- rep(NA, nrow(tab_mod))
  xlev <- if(inherits(mod, "mira")) getfit(mod, 1)$xlevels else mod$xlevels
  if (inherits(mod, "boot") | !is.null(xlev) && any(map_lgl(xlev, ~ length(.) > 2))){
    pval_glob <- extract_pval_glob(mod, en_test)
    vec_pval[seq_along(pval_glob)] <- pval_glob
  }
  mutate(tab_mod, p.global = vec_pval, p.global = ifelse(grepl("^ns\\(", term), NA, p.global)) %>%
    mutate(p.global = ifelse(!is.na(p.global), p.global,
                             ifelse(.variable == dplyr::lag(.variable) & dplyr::lag(.variable, default="NA") != "NA", NA, p.value))) %>%
    add_class(class(tab_mod))
}

add_varname <- function(x, y, ...){
  UseMethod("add_varname", y)
}

#' @export
add_varname.default <- function(tab, x, noms, ...){
  add_column(tab, id = noms, .variable = label(x), .before = 1)
}

#' @export
add_varname.factor <- function(tab, x, noms, one_line = FALSE, add_niveau = TRUE){
  if (one_line) add_varname.default(tab, x, noms)
  else {
    id <- c(rep(noms, nlevels(x)))
    variable <- c(rep(label(x), nlevels(x)))
    niveau <- levels(x)
    if (add_niveau) add_column(tab, id, .variable = variable, niveau, .before = 1)
    else add_column(tab, id, .variable = variable, .before = 1)
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
    tibble(id = id, .variable = variable, niveau = niveau, multiple = multiple)
  }) %>%
    bind_cols(as_tibble(tableRet)) %>%
    add_class("tabboot")
}

#' @export
add_varname.lm <- function(tab_mod, mod){
  left <- map2_df(mod$model[-1], names(mod$model[-1]), function(x, y){
    if(is.numeric(x)) {
      variable <- label(x)
      if(grepl("^I\\(", y)){
        rec <- stringr::str_match(y, "^I\\((.*?)/([0-9]\\.?[0-9]*)\\)")
        multiple <- rec[[3]] %>% as.numeric()
        id <- rec[[2]]
        niveau <- ""
      } else if(grepl("^ns\\(", y)){
        knots <- attr(x, "knots")
        labs <- label_cutOff(c(-Inf, knots, Inf), TRUE)
        nm <- str_extract(attr(mod$terms, "dataClasses")[y], "[0-9]")
        rec <- stringr::str_match(y, "^ns\\((.*?),.*\\)")
        multiple <- rep(1, length(labs))
        id <- rep(rec[[2]], length(labs))
        variable <- sprintf("spline(%s)", standardize_names(id))
        niveau <- labs
      } else {
        multiple <- 1
        id <- y
        niveau <- ""
      }
    }
    else {
      id <- rep(y, nlevels(x) - 1)
      variable <- rep(label(x), nlevels(x) - 1)
      niveau <- sprintf("%s vs %s", levels(x)[-1], rep(levels(x)[1], nlevels(x) - 1))
      multiple <- NA
    }
    tibble(id = id, .variable = variable, niveau = niveau, multiple = multiple)
  })
  if (tab_mod$term[1] == "(Intercept)") {
    left <- bind_rows(
      tibble(id = "intercept", .variable = "Intercept", niveau = "", multiple = NA),
      left
    )
  }
  bind_cols(left, tab_mod) %>%
    add_class(paste0("tab", class(mod)[1]))
}

#' @export
add_varname.mira <- function(tab_mod, mod){
  mod <- getfit(mod, 1)
  add_varname(tab_mod, mod)
}

#' @export
add_varname.coxph <- add_varname.lm

get_all_boots <- function(model_base, varajust = NULL, nCPU = 1, R = NULL){
  tab <- model.frame(model_base)
  names(tab)[1] <- ".vardep"
  if (is.null(R)) R <- tab %>% nrow() %>% nearest_up_thousand()
  if((!is.null(getDefaultReactiveDomain()))){
    progress <- Progress$new()
    progress$set(value = 0, message = "Bootstrap...")
    updateProgress <- function(detail = NULL) {
      progress$inc(1/R, detail = detail)
      #}
    }
  } else  updateProgress <- function(detail){}

  vec.out <- extract_from_model(model_base, "estimate")
  nvar_mod <- get_nvar_mod(tab, varajust)
  resBoot <- get_resBoot(tab, R, model_base, nCPU,updateProgress, varajust)
  if((!is.null(getDefaultReactiveDomain()))){
    progress$close()
    progress <- Progress$new()
    progress$set(value = 0, message = "Bootstrap...")
  }
  resBoot_p <- get_resBoot_p(tab, R, model_base, nCPU, updateProgress, varajust)
  if((!is.null(getDefaultReactiveDomain()))) progress$close()

  tab_ci <- get_confint_p_boot(resBoot, resBoot_p)
  xlevels <- model_base$xlevels %>%
    magrittr::extract(!names(model_base$xlevels) %in% varajust)

  resBoot_anova <- NULL
  if(!is.null(xlevels) && any(map_lgl(xlevels, ~ length(.) > 2))){
    if((!is.null(getDefaultReactiveDomain()))){
      progress <- Progress$new()
      progress$set(value = 0, message = "Bootstrap Anova...")
    }
    resBoot_anova <- get_resBoot_anova(tab, R, model_base, nCPU, updateProgress, varajust)
    if((!is.null(getDefaultReactiveDomain()))) progress$close()
  }
  return(list(resBoot = resBoot, resBoot_anova = resBoot_anova, tab_ci = tab_ci))
}

#' @export
create_table_bootstrap <- function(model_base, varajust = NULL, nCPU = 1, en_test = FALSE, R = NULL){
  list_boots <- get_all_boots(model_base, varajust, nCPU, R)
  resBoot <- list_boots$resBoot
  tab_ci <- list_boots$tab_ci
  resBoot_anova <- list_boots$resBoot_anova
  tab <- model.frame(model_base)
  names(tab)[1] <- ".vardep"
  tab_complete <- add_varname(tab_ci$tableRet, resBoot)

  if(en_test){
    nvar_mod <- get_nvar_mod(tab, varajust)
    tab_ci$tableRet$p.param <-
      extract_from_model(model_base, "p.value")[seq_len(nvar_mod)]
  }
  tab <- model.frame(model_base)
  if(!is.null(varajust) && length(varajust) > 0) varajust_label <- label(tab[, varajust, drop = FALSE])

  else varajust_label <- NULL

  if(!is.null(resBoot_anova)){
    tab_complete %<>% add_pval_glob(resBoot_anova, en_test = en_test)
  } else {
    tab_complete$p.global <- tab_complete$p.value
  }
  if(en_test & !is.null(resBoot_anova)){
    tab_complete$p.global.param <- extract_pval_glob(resBoot_anova$tab_anova_base, en_test = en_test)
  }
  c2 <- if("glm" %in% class(model_base)) "tabglm"
  else if ("lm" %in% class(model_base)) "tablm"
  else if ("coxph" %in% class(model_base)) "tabcoxph"
  tab_complete %<>%
    dplyr::filter(!.variable %in% varajust_label) %>%
    add_class(c2)
  return(tab_complete)
}
