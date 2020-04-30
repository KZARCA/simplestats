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
      precision <- find_best_precision(tab, x)
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
#' @param pred Logical: is the analysis predictive or explanatory
#'
#' @return
#' @export
#'
#' @examples
compute_mod <- function(tab, vardep, varindep, varajust, type, pred = FALSE, cv = FALSE){
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

  varajust_m <- if (!pred) prepare_varAjust(tab, varajust, type) else varajust
  varindep_m <- if(pred) varindep else format_precision(tab, varindep)
  allVars <- c(varindep_m, varajust_m)
  vardep_m <- ifelse(type == "survival", sprintf("Surv(.time, %s)", vardep), vardep)
  formule <- sprintf("%s ~ %s", vardep_m, paste(allVars, collapse = " + "))
  formule2 <- sprintf("%s ~ %s", vardep_m, paste(c(varindep_m, varajust), collapse = " +"))

  if (cv && length(varindep) == 0 && length(varajust) == 0) {
    formule <- formule2 <- sprintf("%s ~ 1", vardep_m)
  }

  .fun <- get_fun(type)

  mod <- get_mod(tab_m, .fun, formule) %>%
    modify_mod(tab_m)

  return(list(tab = tab_m, mod = mod, formule = formule, formule2 = formule2,
              imputer = resume_imputer))
}

#' @export
compute_mod_base_bootstrap <- function(tab, type, miss, formule){
  if (miss) {
    exLabel <- label(tab)
    tabBoot <- mice::complete(tab)
    label(tabBoot, self = FALSE) <- exLabel
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

modify_mod.default <- function(mod, tab){
  warned <- attr(mod, "warning")
  if (is.null(warned)) return(mod)
  if(any(grepl("fitted probabilities numerically 0 or 1 occurred", warned), na.rm = TRUE)){
    m <- keep_warning(filter_glm_fit(mod, tab))
    if (!is.null(m)) mod <- m
  }
  return(mod)
}


modify_mod.mira <- function(mod, tabm){
  m <- tabm$m
  warned <- unique(attr(mod, "warning"))
  if (is.null(warned)) return(mod)
  mod2 <- as.list(rep(0, m))
  for (i in seq_len(m)){
    attr(mod$analyses[[i]], "warning") <- warned
    mod2[[i]] <- modify_mod(mice::getfit(mod, i), mice::complete(tabm, i))
    if(!is.null(attr(mod2[[i]], "warning"))){
      return(mod2[[i]])
    }
  }
  mod2
}
