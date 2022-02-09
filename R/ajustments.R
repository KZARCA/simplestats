#' Detect adjustment variables
#'
#' @param tab A data frame
#' @param vardep A character string of the dependant variable
#' @param varindep  A character vector of the independant variables
#' @param type A character string of the type of modeling, having a value among "linear", "logistic" or "survival"
#' @param by_lasso a logical: is the adjustment variable computed with lasso?
#' @return
#' @export
#'
#' @examples
define_varajust <- function(tab, vardep, varindep, type, by_lasso = TRUE, all_vars = FALSE){
  if (by_lasso){
    return(get_lasso_variables(tab, vardep, varindep, type))
  }
  a <- NULL
  vars <- create_tabi(tab, "desc") %>%
    get_choix_var()
  #seuil <- min(0.2, 5/length(vars))
  seuil <- 0.2
  varajust <- map(seq_along(vars), function(i){
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
          mod <- try2(survival::coxph(formula = as.formula(formule), data = tab))
          if (is_warning(mod) && (grepl("beta may be infinite", attr(mod, "message")) |
                                    grepl("converge", attr(mod, "message")))) {
            mod <- NULL
          }
          if (is_error(mod) && (grepl("NA/NaN/Inf", attr(mod, "message")))){
            mod <- NULL
          }
      }
      if(!is.null(mod) && !is.na(coef(mod))){
        p <- try2(extract_pval_glob(mod, show_df1 = TRUE)[1])
        if (is_error(p) && (grepl("residual sum of squares is 0", attr(p, "message")))) {
          return(NULL)
        } else {
          names(p) <- vars[i]
          if (p < seuil | all_vars == TRUE) return(p)
          else return(NULL)
        }
      }
    }
    return(p)
  }) %>%
    purrr::compact() %>%
    purrr::flatten_dbl()
  if(length(varajust)) structure(names(varajust), value = varajust)
}

#' Search for multicolinearity
#'
#' This function removes all terms of the model with a VIF > 5
#'
#' @param tab A data frame
#' @param vardep A character string of the dependant variable
#' @param varindep A character vector of the independant variables
#' @param varajust A character vector of the adjustment variables
#' @param type A character string of the type of modeling, having a value among "linear", "logistic" or "survival"

#'
#' @return A character vector of all terms with a VIF > 5
#' @export
#'
#' @examples
recherche_multicol <- function(tab, vardep, varindep, varajust, type, pred = FALSE) {
  if (length(varajust) == 0 && length(varindep) < 2) return(NULL)
  if (is.null(varajust)) varajust <- character(0)
  vars <- c(varindep, varajust)
  elimine <- NULL
  if (type == "survival") {
    tab <- tab[c(vardep, vars, ".time")]
  } else {
    tab <- tab[c(vardep, vars)]
  }
  # exLabel <- label(tab)
  # tab <- imputer(tab, vardep, type, varajust)
  # if (inherits(tab, "mids")) {
  #   tab <- suppressWarnings(complete(tab))
  #   label(tab, self = FALSE) <- exLabel
  # }
  tab %<>% prepare_model()
  analysables <- map_lgl(tab, function(x){
    if (is.factor(x)){
      if (count_items(droplevels(x)) > 1) TRUE else FALSE
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
    elimine <- c(elimine, map(ide, function(x) x[-1]) %>% flatten_chr() %>% unique())
    vars <- vars[-na.omit(match(elimine, vars))]
    if(!length(vars)) return(elimine)
    if (type == "survival"){
      formule <- as.formula(sprintf("Surv(.time, %s) ~ %s", vardep, paste(vars, collapse = " + ")))
    } else {
      formule <- as.formula(paste(vardep, "~", paste(vars, collapse = " + ")))
    }
  }
  set.seed(1234567)
  left_form <- NULL
  suppressWarnings(
    if (type == "logistic"){
      mod <- glm(formule, data = tab, family = "binomial")
    } else if (type == "linear") {
      mod <- lm(formule, data = tab)
    } else if (type == "survival"){
      left_form <- sprintf("Surv(.time, %s)", vardep)
      formule <- sprintf("%s ~ %s", left_form, paste(vars, collapse = " + ")) %>% as.formula()
      mod <- survival::coxph(formula = formule, data = tab, model = TRUE)
    }
  )
  if(!is_model_possible(mod)){
    var_inter <- intersect(varajust, c(vars, elimine))
    if (length(var_inter) > 0){
      elimine <- c(elimine, var_inter)
      vars <- vars[-na.omit(match(elimine, vars))]
      mod <- stats::update(mod, formula = as.formula(sprintf(". ~ . -%s", paste(var_inter, collapse = " - "))))
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
  big_vif <- get_big_vif(tab, vardep, intersect(varindep, vars), intersect(varajust, vars), type, mod, left_form)
  if (length(big_vif)){
    elimine <- append(elimine, big_vif)
  }
  if(pred){
    if (length(varindep) == 1 && varindep %in% elimine){
        return("ERROR_MODEL2")
    }
    return(setdiff(elimine, varindep[1]))
  } else {
    if (varindep[1] %in% elimine){
      return("ERROR_MODEL")
    }
    return(elimine)
  }
}


#' Variable selection by LASSO procedure
#'
#' @param tab A data.frame
#' @param vardep The dependant variable
#' @param varindep The independant variables to force into the model
#' @param type Type of analysis: linear, logistic or survival
#' @param sparse logical: should the variable selection be parsimonious
#'
#' @return A character vector of the predictor variables
#' @export
get_lasso_variables <- function(tab, vardep, varindep = character(0), type = "logistic", sparse = TRUE) {
  if (ncol(tab) == (length(c(vardep, varindep)) + as.numeric(type == "survival"))) return(character(0))
  set.seed(1234567)
  nona <- Filter(function(x) {
    solve_contrast(tab, vardep, x)
  }, tab) %>% na.exclude()
  if (length(varindep) >= ncol(nona) - 1) return(character(0))
  nb_max <- get_number_variables_max(nona, vardep, type)
  nb_remaining <- floor(nb_max - length(varindep))
  if (nb_remaining <= 0) return(character(0))
  formule <- paste(vardep, "~ .")
  if (type == "survival"){
    nona <- filter(nona, .time > 0)
    formule <- paste(formule, "-.time")
    y <- Surv(nona$.time, nona[[vardep]])
  } else {
    y <- nona[[vardep]]
  }
  mat <- model.matrix(as.formula(formule), data = nona)
  penalties <- rep(1, ncol(mat))
  expanded_fac <- map(names(nona), function(x){
    if (is.factor(nona[[x]])){
      lev <- levels(nona[[x]])
      paste0(x, lev[2:length(lev)]) %>% setNames(rep(names(nona[x]), length(lev)-1))
    } else {
      setNames(x, x)
    }
  }) %>% unlist()
  varindep_mat <- expanded_fac[names(expanded_fac) %in% varindep]
  penalties[which(colnames(mat) %in% varindep_mat)] <- 0
  family = dplyr::case_when(type == "logistic" ~ "binomial",
                            type == "survival" ~ "cox",
                            type == "linear" ~ "gaussian")

  cv <- try2({
    cv.glmnet(x = mat,
              y = y,
              foldid = sample(rep(seq(10), length.out = nrow(nona))),
              family = family,
              penalty.factor = penalties)
  }, errors = c("Matrices must have same number of columns", "y is constant", "need at least two non-NA values to interpolate"),
  warnings = c("Convergence", "Option grouped=FALSE enforced", "solutions for larger values of lambda returned",
               "binomial class has fewer than 8  observations", "empty model"))
  if (is_error(cv)){
    return("ERROR_MODEL")
  }

  lambda <- ifelse(sparse, "lambda.1se", "lambda.min")
  idx_lambda <- which(cv$lambda == cv[[lambda]])
  nzero <- cv$nzero

  res <- if (nzero[idx_lambda] <= nb_max){
    coef(cv, cv[[lambda]])
  } else {
    idx <- tail(which(nzero <= nb_remaining), n = 1)
    if (!length(idx)) idx <- 1
    coef(cv, cv$lambda[idx])
  }

  expanded_fac[expanded_fac %in% res@Dimnames[[1]][res@i+1]] %>%
    names() %>%
    unique() %>%
    setdiff(varindep)
}
