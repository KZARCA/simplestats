#' Detect adjustment variables
#'
#' @param tab A data frame
#' @param vardep A character string of the dependant variable
#' @param varindep  A character vector of the independant variables
#' @param type A character string of the type of modeling, having a value among "linear", "logistic" or "survival"
#'
#' @return
#' @export
#'
#' @examples
define_varAjust <- function(tab, vardep, varindep, type, test = FALSE){
  a <- NULL
  vars <- create_tabi(tab, univ = FALSE) %>%
    get_choix_var()
  seuil <- min(0.2, 5/length(vars))
  map(seq_along(vars), function(i){
    mod <- NULL
    p <- NULL
    if (vars[i] != vardep & !vars[i] %in% varindep & vars[i] != ".time"){
      varsi <- ifelse(is.numeric(tab[[vars[i]]]),
                      paste0(ifelse(type == "survival", "ns(","ns("), vars[i], ")"),
                      vars[i])
      formule = paste(vardep, "~", varsi)
      if (type == "logistic"){
        formule2 <- paste(vardep, "~", vars[i])
        mod <- arm::bayesglm(formula = as.formula(formule), data = tab, family = "binomial")
        if (mod$converged == FALSE)  mod <- NULL
      } else if (type == "linear"){
        mod <- lm(formula = as.formula(formule), data = tab)
      } else if (type == "survival"){
        tab2 <- dplyr::select(tab, .time, !!rlang::sym(vardep), !!rlang::sym(vars[i])) %>%
          na.exclude
        formule <- sprintf("Surv(.time, %s) ~ %s", vardep, varsi)
        mod <- survival::coxph(formula = as.formula(formule), data = tab2)
      }
      else mod <- NULL
      if(!is.null(mod)){
        p <- extract_pval_glob(mod, show_df1 = TRUE)[1]
        names(p) <- vars[i]
        if (p < seuil | test == TRUE) return(p)
        else return(NULL)
      }
    }
    return(p)
  }) %>%
    purrr::compact() %>%
    purrr::flatten_dbl()
}

#' Search for multicolinearity
#'
#' This function removes all terms of the model with a VIF > 5
#'
#' @param tab A data frame
#' @param vardep A character string of the dependant variable
#' @param varindep A character vector of the independant variables
#' @param varAjust A character vector of the adjustment variables
#' @param type A character string of the type of modeling, having a value among "linear", "logistic" or "survival"

#'
#' @return A character vector of all terms with a VIF > 5
#' @export
#'
#' @examples
recherche_multicol <- function(tab, vardep, varindep, varAjust, type){
  vars <- c(varindep, varAjust)
  elimine <- NULL
  if (type == "survival") {
    tab <- na.exclude(tab[c(vardep, vars, ".time")])
  } else {
    tab <- na.exclude(tab[c(vardep, vars)])
  }
  analysables <- map_lgl(tab, function(x){
    if (is.factor(x)){
      if (length(table(droplevels(x))) > 1) TRUE else FALSE
    } else TRUE
  })
  elimine <- names(tab)[!analysables]
  if (length(elimine) > 0) {
    vars <- vars[-na.omit(match(elimine, vars))]
  }
  tab <- tab[analysables]
  formule <- as.formula(paste(vardep, "~", paste(vars, collapse = " + ")))
  if (type == "logistic"){
    mod <- arm::bayesglm(formule, data = tab, family = "binomial")
  } else if (type == "linear") {
    mod <- lm(formule, data = tab)
  } else if (type == "survival"){
    formule <- sprintf("Surv(.time, %s) ~ %s", vardep, paste(vars, collapse = " + ")) %>% as.formula()
    mod <- survival::coxph(formula = formule, data = tab)
  }
  if(!is_model_possible(mod)){
    if (length(varAjust) > 0) {
      elimine <- varAjust
      vars <- vars[-na.omit(match(elimine, vars))]
      mod <- stats::update(mod, formula = as.formula(sprintf(". ~ . -%s", paste(varAjust, collapse = " - "))))
    } else return("ERROR_MODEL")
  } else {
    if(any(is.na(coef(mod)))){ #remove alias
      alias <- names(which(is.na(coef(mod))))
      vari <- map_lgl(vars, ~ any(grepl(., alias)))
      elimine <- append(elimine, vars[vari])
      vars <-  vars[!vari]
      mod <- stats::update(mod, as.formula(paste(vardep, "~", paste(vars, collapse = " + "))))
    }
    if (length(vars) > 1){ #remove big vif
      infl <- suppressWarnings(car::vif(mod))
      if(!is.null(dim(infl))) infl <- infl[, 1, drop = TRUE]
      old_elimine <- elimine
      elimine <- remove_big_vif(tab, varAjust, vardep, vars, type, infl, elimine) # in priority, remove varAjust
      if(length(elimine) - length(old_elimine) > 0){
        vars <- vars[-na.omit(match(elimine, vars))]
        mod <- stats::update(mod, as.formula(paste(vardep, "~", paste(vars, collapse = " + "))))
        infl <- suppressWarnings(car::vif(mod))
        if(!is.null(dim(infl))) infl <- infl[, 1, drop = TRUE]
      }
      elimine <- remove_big_vif(tab, varindep, vardep, vars, type, infl, elimine) # if necessary, remove varindep
    }
    return(elimine)
  }
}

#' Format adjustment variables
#'
#' Models the numeric adjustment variables with the natural spline, to be used in a formula
#'
#' @param tab The data frame
#' @param var_ajust The adjustment variables
#' @param type A character string of the type of modeling, having a value among "linear", "logistic" or "survival"
#'
#' @return A character vector surrounded by "ns()" when relevant
#' @export
#'
#' @examples
prepare_varAjust <- function(tab, var_ajust, type){
  ifelse(
    map_lgl(tab[var_ajust], is.numeric),
    paste0(
      ifelse(type == "survival", "ns(","ns("),
      var_ajust, ")"),
    var_ajust)
}
