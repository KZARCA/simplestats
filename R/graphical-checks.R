create_spline <- function(tab, vardep, varindep, var_ajust = NULL, type){
  model <- NULL
  if (type == "survival") varindep <- remove_elements(varindep, ".time")
  varsnum <- Filter(is.numeric, tab[varindep]) %>% colnames()
  varsnumGam <- varsnum %>%
    map_chr(function(x) {
      if (length(table(tab[, x, drop = FALSE])) < 20){
        k <- min(9, length(table(tab[, x, drop = FALSE]))-1)
        paste0(x, ", k = ", k)
      } else
        as.character(x)
    })
  var_ajust_num <- Filter(is.numeric, tab[var_ajust]) %>% colnames()

  varsfac <- Filter(is.factor, tab[varindep]) %>% colnames()
  var_ajust_fac <- Filter(is.factor, tab[var_ajust]) %>% colnames()
  if(length(varsnum)){
    right <- paste0("s(", varsnumGam, ")", collapse=" + ")
    rightLin <- paste0(varsnum, collapse=" + ")
    if (length(varsfac)) {
      right <- paste(right, sprintf("+ %s", paste0(varsfac, collapse = " + ")))
      rightLin <- paste(rightLin, sprintf("+ %s", paste0(varsfac, collapse = " + ")))
    }
    if (length(var_ajust_num)){
      right <- paste(right, "+", paste0("ns(", var_ajust_num, ")", collapse=" + "))
      rightLin<- paste0(rightLin, sprintf("+ %s", paste0(var_ajust_num, collapse = " + ")))
    }
    if (length(var_ajust_fac)){
      right <- paste(right, sprintf("+ %s", paste0(var_ajust_fac, collapse = " + ")))
      rightLin<- paste0(rightLin, sprintf("+ %s", paste0(var_ajust_fac, collapse = " + ")))
    }
    if (type %in% c("linear", "logistic")) {
      formule <- paste(vardep, "~", right)
      formuleLin <- paste(vardep, "~", rightLin)
    } else if (type == "survival") {
      formule <- paste(".time ~", right)
      formuleLin <- paste0("Surv(.time, ", vardep, ") ~", rightLin)
    }
    suppressWarnings({
      if (type == "logistic"){
        graph <- gam(as.formula(formule), data=tab, family = "binomial")
        mLin <- bayesglm(as.formula(formuleLin), data=tab, family = "binomial", drop.unused.levels = FALSE)
        lin <- termplot(mLin, plot = FALSE)
      }
      else if (type == "linear") {
        graph <- gam(as.formula(formule), data=tab)
        mLin <- lm(as.formula(formuleLin), data=tab)
        lin <- termplot(mLin, plot = FALSE)
      }
      else if (type == "survival"){
        graph <- gam(as.formula(formule), family = cox.ph(), data = tab, weights = tab[[vardep]])
        mLin <- coxph(as.formula(formuleLin), data = tab)
        lin <- termplot(mLin,  rug = TRUE, se = TRUE, plot = FALSE)
      }
    })
    return(list(graph = graph, lin = lin, mod = mLin))
  }
}

plot_nth_spline <- function(spline_gen, n){
    coord <- plot(spline_gen$graph, select = n, scale = 0)[[n]]
    lin <- spline_gen$lin[[n]]
    abline(line(lin)$coef, col = 2)
    return(list(coord = coord, lin = lin))
}


#' Plots splines
#'
#' @param tab The data frame
#' @param vardep A character string of dependent variable
#' @param varindep A character vector of independant variables
#' @param var_ajust A character vector of adjustment variables
#' @param type A character string of the type of modeling, having a value among "linear", "logistic" or "survival"
#'
#' @return All plots
#' @export
#'
#' @examples
plot_all_splines <- function(tab, vardep, varindep, var_ajust, type){
  varSpline <- tab %>%
    dplyr::select(one_of(varindep)) %>%
    select_if(is.numeric) %>%
    colnames()

  spline_gen <- create_spline(tab, vardep, varindep, var_ajust, type)
  for (n in seq_along(varSpline)){
    plot_nth_spline(spline_gen, n)
  }
}

prepare_zph <- function(tab, vardep, varindep, var_ajust) {
  allVars <- c(varindep, var_ajust)
  formule <- as.formula(paste0("Surv(.time, ", vardep, ")", " ~ ", paste(allVars, collapse = " + ")))
  coxph(formule, data = tab)
}

plot_nth_zph <- function(model, n){
  plot(cox.zph(model), var = n, resid = FALSE)
  abline(h = model$coefficients[n], col = 2)
}

#' Title
#'
#' @param tab The data frame
#' @param vardep A character string of dependent variable
#' @param varindep A character vector of independant variables
#' @param var_ajust A character vector of adjustment variables
#'
#' @return All plots
#' @export
#'
#' @examples
plot_all_zph <- function(tab, vardep, varindep, var_ajust){
  model <- prepare_zph(tab, vardep, varindep, var_ajust)
  nb <- map_dbl(varindep, function(x){
    if (is.numeric(tab[[x]])) 1 else nlevels(tab[[x]]) - 1
  })
  for(n in seq_len(sum(nb))){
    plot_nth_zph(model, n)
  }
}
