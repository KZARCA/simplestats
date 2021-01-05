#' Get adequate function and arguments for model
#'
#' @param type a character vector being one of "linear", "logistic" or "survival"
#'
#' @return a list
#' @export
get_fun <- function(type){
  .fun <- switch(type,
                 logistic = "glm",
                 linear = "lm",
                 survival = "coxph"
  )
  args_sup <- if (type == "logistic") list(family = "binomial")
  else if (type == "survival") list(model = TRUE)
  else NULL
  return(list(fun = .fun, args_sup = args_sup))
}

prepare_variables <- function(tab, varindep, varajust, pred = 0){

  n <- dplyr::case_when(nrow(tab) < 100 ~ 3,
            nrow(tab) < 500 ~ 4,
            TRUE ~ 5)

  pos <- if (n == 3){
    c(0.1, 0.5, 0.9)
  } else if (n == 4){
    c(0.05, 0.35, 0.65, 0.95)
  } else if (n >= 5){
    c(0.05, 0.275, 0.5, 0.725, 0.95)
  }

  vars <- c(varindep, varajust)
  if (pred > 0){
    list(varindep = map_chr(vars, function(x){
      knots <- attr(varindep, paste("knots", x, sep = "_"))
      if (!is.null(knots)){
        if (pred == 2){
          knots <- quantile(tab[[x]], pos, na.rm = TRUE)
        }
        sprintf("ns(%s, knots = c(%s))", x, paste(knots, collapse = ", "))
      } else {
        x
      }
    }))
  } else {
      varindep_m <- format_precision(tab, varindep)
     varajust_m <- map_chr(varajust, function(x){
        knots <- attr(varajust, paste("knots", x, sep = "_"))
        if (!is.null(knots)){
          sprintf("ns(%s, knots = c(%s))", x, paste(knots, collapse = ", "))
        } else {
          x
        }
      })
      list(varindep = varindep_m, varajust = varajust_m)
  }
}

#' Get the best precision for all indepedant variables
#'
#' @param tab The data.frame
#' @param varindep The independant variables
#'
#' @return a character vector
#' @export
format_precision <- function(tab, varindep){
  map_chr(varindep, function(x){
    if (is_entier(tab[[x]])) x
    else if(is.numeric(tab[[x]])){
      precision <- find_best_precision(tab[[x]])
      if (precision != 1) paste0("I(", x, "/", precision, ")") else x
    } else x
  })
}

#' Compute multivariable statistical model
#'
#' @param tab The data.frame
#' @param vardep The dependant variable
#' @param varindep The independant variables
#' @param varajust The adjustment variables
#' @param type Type: one of "linear", "logistic" or "survival"
#' @param pred 0 if the analysis is explanatory, 1 if the analysis is predictive, 2 if the model is computed for cross validation
#'
#' @return
#' @export
#'
#' @examples
compute_mod <- function(tab, vardep, varindep, varajust, type, pred = 0){
  vars <- c(vardep, varindep, varajust)
  if (type == "survival") vars %<>% add_elements(".time")
  tab <- tab[vars]
  if (any(is.na(tab))){
    n_imputation <- ifelse(type == "survival" && any(map(tab, nlevels) > 2), 20, 5)
    tab_m <- imputer(tab, vardep, type = type, n_imputation = n_imputation)
    resume_imputer <- TRUE
  } else {
    tab_m <- tab
    resume_imputer <- FALSE
  }

  # varajust_m <- prepare_varajust(tab, varajust, type)
  # varindep_m <- prepare_varindep(tab, varindep, pred)
  # allVars <- c(varindep_m, varajust_m)
  allVars <- prepare_variables(tab, varindep, varajust, pred)
  vardep_m <- ifelse(type == "survival", sprintf("Surv(.time, %s)", vardep), vardep)
  formule <- sprintf("%s ~ %s", vardep_m, paste(purrr::flatten_chr(allVars), collapse = " + "))
  formule2 <- sprintf("%s ~ %s", vardep_m, paste(unique(allVars$varindep, varajust), collapse = " + "))
  #formule2 <- formule
  if (pred == 2 && length(varindep) == 0 && length(varajust) == 0) {
    formule <- formule2 <- sprintf("%s ~ 1", vardep_m)
  }

  .fun <- get_fun(type)

  mod <- get_mod(tab_m, .fun, formule) %>%
    modify_mod(tab_m, varindep, varajust, pred)
  return(list(tab = tab_m, mod = mod, formule = formule, formule2 = formule2,
              imputer = resume_imputer))
}

#' @export
compute_mod_base_bootstrap <- function(tab, type, miss, exLabel, formule){
  if (miss) {
    tabBoot <- mice::complete(tab)
    #label(tabBoot, self = FALSE) <- exLabel
  }
  else tabBoot <- tab
  .fun <- get_fun(type)
  do.call(.fun$fun, c(list(formula = formule, data = as.name("tabBoot")), .fun$args_sup))
}

get_mod <- function(tab, .fun, formule){
  express <- parse(text = sprintf("%s(%s)", .fun$fun, formule))[[1]]
  if(!is.null(.fun$args_sup)){
    iwalk(.fun$args_sup, function(x, y) {
      express[[y]] <<- x
    })
  }
  mod <- keep_warning(with(tab, eval(express)))
  if (inherits(mod, "mira")) {
    mod$call[[1]] <- quote(mice:::with.mids)
    mod$call$expr <- express
  }
  mod
}

modify_mod <- function(x, ...){
  UseMethod("modify_mod")
}

modify_mod.default <- function(mod, tab, varindep, varajust, pred){
  warned <- attr(mod, "warning")
  if (is.null(warned)) return(mod)
  if(any(grepl("fitted probabilities numerically 0 or 1 occurred", warned), na.rm = TRUE)){
    m <- keep_warning(filter_glm_fit(mod, tab, varindep, varajust, pred))
    if (!is.null(m)) mod <- m
  }
  return(mod)
}


modify_mod.mira <- function(mod, tabm, varindep, varajust, pred){
  m <- tabm$m
  warned <- unique(attr(mod, "warning"))
  if (is.null(warned)) return(mod)
  mod2 <- mod
  for (i in seq_len(m)){
    attr(mod$analyses[[i]], "warning") <- warned
    mod2$analyses[[i]] <- modify_mod(mice::getfit(mod, i),
                                     mice::complete(tabm, i),
                                     varindep, varajust, pred)
    if(!is.null(attr(mod2$analyses[[i]], "warning"))){
      return(mod2$analyses[[i]])
    }
  }
  mod2
}
