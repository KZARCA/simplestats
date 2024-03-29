#' @export
add_knots <- function(allVars, variable, position){
  attr(allVars, paste("knots", variable, sep = "_")) <- position
  allVars
}


create_spline <- function(tab, vardep, varindep, varajust = NULL, type){
  vars <- c(vardep, varindep, varajust)
  if (type == "survival") vars <- append(vars, ".time")
  tab <- na.exclude(tab[vars])
  model <- NULL
  if (type == "survival") varindep <- remove_elements(varindep, ".time")
  varspline <- c(varindep, varajust)
  varsnum <- Filter(function(x) is.numeric(x) & count_items(x) > 10, tab[varspline]) %>% colnames()
  varsnumGam <- varsnum %>%
    map_chr(function(x) {
        ifelse(type == "survival", glue::glue("pspline({x})"), glue::glue("s({x}, k = 4)"))
    })
  varsfac <- Filter(function(x) is.factor(x) | count_items(x) <= 10, tab[varindep]) %>% colnames()
  varajust_fac <- Filter(function(x) is.factor(x) | count_items(x) <= 10, tab[varajust]) %>% colnames()
  if(length(varsnum)){
    right <- paste0(varsnumGam, collapse=" + ")
    rightLin <- paste0(varsnum, collapse=" + ")
    if (length(varsfac)) {
      right <- paste(right, sprintf("+ %s", paste0(varsfac, collapse = " + ")))
      rightLin <- paste(rightLin, sprintf("+ %s", paste0(varsfac, collapse = " + ")))
    }
    if (length(varajust_fac)){
      right <- paste(right, sprintf("+ %s", paste0(varajust_fac, collapse = " + ")))
      rightLin<- paste0(rightLin, sprintf("+ %s", paste0(varajust_fac, collapse = " + ")))
    }
    if (type %in% c("linear", "logistic")) {
      formule <- paste(vardep, "~", right)
      formuleLin <- paste(vardep, "~", rightLin)
    } else if (type == "survival") {
      formule <- paste0("Surv(.time, ", vardep, ") ~", right)
      formuleLin <- paste0("Surv(.time, ", vardep, ") ~", rightLin)
    }
    suppressWarnings({
      if (type == "logistic"){
        graph <- gam(as.formula(formule), data=tab, family = "binomial")
        mLin <- glm(as.formula(formuleLin), data=tab, family = "binomial")
        lin <- termplot(mLin, plot = FALSE)
      }
      else if (type == "linear") {
        graph <- gam(as.formula(formule), data=tab)
        mLin <- lm(as.formula(formuleLin), data=tab)
        lin <- termplot(mLin, plot = FALSE)
      }
      else if (type == "survival"){
        graph <- try2(coxph(as.formula(formule), data = tab))
        if  (is_error(graph)){
          formule <- gsub("pspline", "ns", formule)
          graph <- coxph(as.formula(formule), data = tab)
        }
        mLin <- coxph(as.formula(formuleLin), data = tab)
        lin <- termplot(mLin,  rug = TRUE, se = TRUE, plot = FALSE)
      }
    })
    return(list(graph = graph, lin = lin, mod = mLin))
  }
}

plot_nth_spline <- function(spline_gen, n){
  if (inherits(spline_gen$graph, "coxph")){
    coord <- termplot(spline_gen$graph, term = n, se=TRUE, col.term=1, col.se=1, plot = FALSE)[[1]]
    coord$fit <- coord$y
    termplot(spline_gen$graph, term = n, se=TRUE, col.term=1, col.se=1, plot = TRUE)
  } else {
    coord <- plot(spline_gen$graph, select = n, scale = 0)[[n]]
  }
    lin <- spline_gen$lin[[n]]
    abline(line(lin)$coef, col = 2)
    return(list(coord = coord, lin = lin))
}


#' Plots splines
#'
#' @param tab The data frame
#' @param vardep A character string of dependent variable
#' @param varindep A character vector of independant variables
#' @param varajust A character vector of adjustment variables
#' @param type A character string of the type of modeling, having a value among "linear", "logistic" or "survival"
#' @param pred A logical indicating whether the analysis is predictive
#'
#' @return All plots
#' @export
#'
#' @examples
plot_all_splines <- function(tab, vardep, varindep, varajust, type){
  varSpline <- tab %>%
    dplyr::select(one_of(varindep)) %>%
    select_if(is.numeric) %>%
    colnames()

  spline_gen <- create_spline(tab, vardep, varindep, varajust, type)
  for (n in seq_along(varSpline)){
    plot_nth_spline(spline_gen, n)
  }
}

prepare_zph <- function(tab, vardep, varindep, varajust) {
  allVars <- c(varindep, varajust)
  formule <- as.formula(paste0("Surv(.time, ", vardep, ")", " ~ ", paste(allVars, collapse = " + ")))
  coxph(formule, data = tab %>% prepare_model())
}

plot_nth_zph <- function(model, n){
  z <- try2(plot(cox.zph(model, terms = FALSE), var = n, resid = FALSE),
            c(gettext("Spline fit is singular", domain="R-simplestats"), gettext("Invalid variable")))
  if (is_error(z) && grepl(gettext("Spline fit is singular", domain="R-simplestats"), attr(z, "message"))){
    z <- try2(plot(cox.zph(model, terms = FALSE), var = n, resid = FALSE, df = 2))
    if(is_error(z)){
      stop(attr(z, "message"))
    }
  }
  abline(h = model$coefficients[n], col = 2)
}

#' Title
#'
#' @param tab The data frame
#' @param vardep A character string of dependent variable
#' @param varindep A character vector of independant variables
#' @param varajust A character vector of adjustment variables
#'
#' @return All plots
#' @export
#'
#' @examples
plot_all_zph <- function(tab, vardep, varindep, varajust){
  model <- prepare_zph(tab, vardep, varindep, varajust)
  nb <- map_dbl(varindep, function(x){
    if (is.numeric(tab[[x]])) 1 else nlevels(tab[[x]]) - 1
  })
  for(n in seq_len(sum(nb))){
    plot_nth_zph(model, n)
  }
}
