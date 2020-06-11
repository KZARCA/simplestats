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
  vars <- create_tabi(tab, "desc") %>%
    get_choix_var()
  seuil <- min(0.2, 5/length(vars))
  map(seq_along(vars), function(i){
    mod <- NULL
    p <- NULL
    tab <- if(type == "survival") {
      dplyr::select(tab, .time, !!rlang::sym(vardep), !!rlang::sym(vars[i]))
    } else {
      dplyr::select(tab, !!rlang::sym(vardep), !!rlang::sym(vars[i]))
    }
    tab %<>% prepare_model(remove = TRUE)
    if ((ncol(tab) > 1  & type != "survival" | ncol(tab) > 2)  && (vars[i] %in% names(tab) & vars[i] != vardep & !vars[i] %in% varindep & vars[i] != ".time" &
        solve_contrast(tab, vardep, tab[[vars[i]]]))){
      varsi <- ifelse(is.numeric(tab[[vars[i]]]),
                      paste0(ifelse(type == "survival", "ns(","ns("), vars[i], ")"),
                      vars[i])
      formule <- paste(vardep, "~", varsi)
      if (type == "logistic"){
        mod <- glm(formula = as.formula(formule), data = tab, family = "binomial")
        if (mod$converged == FALSE)  mod <- NULL
      } else if (type == "linear"){
        mod <- lm(formula = as.formula(formule), data = tab)
      } else if (type == "survival"){
          formule <- sprintf("Surv(.time, %s) ~ %s", vardep, varsi)
          mod <- tryCatch(survival::coxph(formula = as.formula(formule), data = tab),
                          warning=function(w) w, error=function(e) e)

          if (is(mod, "warning") && (grepl("beta may be infinite", mod$message) |
                                    grepl("converge", mod$message))) {
            mod <- NULL
          }
          if (is(mod, "error") && (grepl("NA/NaN/Inf", mod$message))){
            mod <- NULL
          }
      }
      if(!is.null(mod) && !is.na(coef(mod))){
        p <- tryCatch(extract_pval_glob(mod, show_df1 = TRUE)[1],
                      error = function(e)e)
        if (is(p, "error") && (grepl("residual sum of squares is 0", p$message))) {
          return(NULL)
        } else {
          names(p) <- vars[i]
          if (p < seuil | test == TRUE) return(p)
          else return(NULL)
        }
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
recherche_multicol <- function(tab, vardep, varindep, var_ajust, type, pred = FALSE) {
  if (length(var_ajust) == 0 && length(varindep) < 2) return(NULL)
  if (is.null(var_ajust)) var_ajust <- character(0)
  vars <- c(varindep, var_ajust)
  elimine <- NULL
  if (type == "survival") {
    tab <- tab[c(vardep, vars, ".time")]
  } else {
    tab <- tab[c(vardep, vars)]
  }
  # exLabel <- label(tab)
  # tab <- imputer(tab, vardep, type, var_ajust)
  # if (inherits(tab, "mids")) {
  #   tab <- suppressWarnings(complete(tab))
  #   label(tab, self = FALSE) <- exLabel
  # }
  tab %<>% prepare_model()
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
  if (type == "survival"){
    formule <- as.formula(sprintf("Surv(.time, %s) ~ %s", vardep, paste(vars, collapse = " + ")))
  } else {
    formule <- as.formula(paste(vardep, "~", paste(vars, collapse = " + ")))
  }
  ide <- identical_model_frame(tab, formule, type)
  if (length(ide)){
    elimine <- c(elimine,map(ide, function(x) x[-1]) %>% flatten_chr() %>% unique())
    vars <- vars[-na.omit(match(elimine, vars))]
    if (type == "survival"){
      formule <- as.formula(sprintf("Surv(.time, %s) ~ %s", vardep, paste(vars, collapse = " + ")))
    } else {
      formule <- as.formula(paste(vardep, "~", paste(vars, collapse = " + ")))
    }
  }
  left_form <- NULL
  if (type == "logistic"){
    mod <- glm(formule, data = tab, family = "binomial")
  } else if (type == "linear") {
    mod <- lm(formule, data = tab)
  } else if (type == "survival"){
    left_form <- sprintf("Surv(.time, %s)", vardep)
    formule <- sprintf("%s ~ %s", left_form, paste(vars, collapse = " + ")) %>% as.formula()
    mod <- survival::coxph(formula = formule, data = tab, model = TRUE)
  }
  if(!is_model_possible(mod)){
    if (length(var_ajust) > 0 & !pred){
      elimine <- var_ajust
      vars <- vars[-na.omit(match(elimine, vars))]
      mod <- stats::update(mod, formula = as.formula(sprintf(". ~ . -%s", paste(var_ajust, collapse = " - "))))
      if (!is_model_possible(mod)) return("ERROR_MODEL")
    } else return("ERROR_MODEL")
  }
  alias <- remove_alias(vars, mod)
  while (any(alias)){
    elimine <- append(elimine, vars[alias])
    vars <- vars[!alias]
    mod <- update_mod(tab, mod, vardep, vars, type, left_form)
    alias <- remove_alias(vars, mod)
  }
  # mod_indep <- update_mod(tab, mod, vardep, varindep, type, left_form)
  # big_vif_varindep <- get_big_vif(tab, vardep, intersect(varindep, vars), character(0), type, mod_indep, left_form)
  # if (length(big_vif_varindep)){
  #   elimine <- append(elimine, big_vif_varindep)
  #   varindep <- remove_elements(varindep, elimine)
  #   vars <- remove_elements(vars, elimine)
  #   mod <- update_mod(tab, mod, vardep, vars, type, left_form)
  # }
  big_vif <- get_big_vif(tab, vardep, intersect(varindep, vars), intersect(var_ajust, vars), type, mod, left_form)
  if (length(big_vif)){
    elimine <- append(elimine, big_vif)
  }

  return(setdiff(elimine, varindep[1]))
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
