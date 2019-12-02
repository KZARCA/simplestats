get_pertinent_params <- function(x){
  UseMethod("get_pertinent_params")
}


get_pertinent_params.coxph <- function(x){
  res <- broom::glance(x) %>%
    mutate(likelihood = sprintf_number_table("%s (%s)", statistic.log, p.value.log),
         score = sprintf_number_table("%s (%s)", statistic.sc, p.value.sc),
         wald = sprintf_number_table("%s (%s)", statistic.wald, p.value.wald),
         squared_cox = sprintf_number_table("%s (%s)", r.squared, r.squared.max),
         concordance = sprintf_number_table("%s (±%s)", concordance, std.error.concordance)) %>%
    select(squared_cox, concordance, logLik, AIC, BIC, likelihood, score, wald)
}

get_pertinent_params.lm <- function(x){
  res <- broom::glance(x) %>%
    mutate(squared_lm = sprintf_number_table("%s (%s)", r.squared, adj.r.squared),
           stat_lm = sprintf_number_table("%s (%s, %s)", statistic, df, df.residual)) %>%
      select(squared_lm, stat_lm, deviance, logLik, AIC, BIC, sigma, p.value)

}

get_pertinent_params.glm <- function(x){
  res <- broom::glance(x) %>%
    mutate(null = sprintf_number_table("%s (%s)", null.deviance, df.null),
           residual = sprintf_number_table("%s (%s)", deviance, df.residual)) %>%
    select(null, residual, logLik, AIC, BIC)
}

get_pertinent_params.default <- function(x){
  test_name <- x$name
  res <- x$result
  if(test_name == "Pearson"){
    res <- broom::glance(res) %>%
    mutate(estimate_p = sprintf_number_table("%s (%s; %s)", estimate, conf.low, conf.high))
    return(select(res, estimate_p, parameter, statistic, p.value, method))
  }
  if(test_name == "Spearman"){
    res <- broom::glance(res)
    return(select(res, estimate_p2 = estimate, statistic, p.value, method))
  }
  if (test_name == "Welch"){
    res <- broom::glance(res)
    res <- mutate(res, estimate_t = sprintf_number_table("%s (%s; %s)", estimate, conf.low, conf.high))
    return(select(res, estimate_t, parameter, statistic, p.value, method))
  }
  if (test_name == "Mann-Whitney") {
    res <- broom::glance(res)
    return(select(res, statistic, p.value, method))
  }
  if (test_name == "Anova"){
    res <- broom::tidy(res) %>%
      mutate(method = c(gettext("Analysis of Variance"), rep(NA, nrow(res) - 1)))
    return(select(res, term, df, sumsq, statistic, p.value, method))
  }
  if (test_name == "Kruskal-Wallis") {
    res <- broom::glance(res)
    return(select(res, parameter, statistic, p.value, method))
  }
  if (test_name == "Fisher") {
    res <- broom::glance(res)
    return(select(res, p.value, method))
  }
  if (test_name == "Chi2") {
    res <- broom::glance(res)
    return(select(res, parameter, statistic, p.value, method))
  }
  if (test_name == "Logrank") {
    res <- broom::glance(res) %>%
      mutate(method = "Mantel-Haenszel - Logrank")
    return(select(res, df, statistic, p.value, method))
  }
}

rename_glance <- function(x){
  params <- c(df = gettext("Degrees of Freedom"),
              parameter = gettext("Degrees of Freedom"), estimate_t =  gettext("Mean Difference (CI95)"),
              estimate_p = gettext("Correlation Coefficient (CI95)"), estimate_p2 = gettext("Correlation Coefficient"),
              sumsq = gettext("Sum of Squares"), method = gettext("Test"), term = gettext("Variable"),
              p.value = "p", statistic = gettext("Test statistic"),
              logLik = gettext("Log-Likelihood"), AIC = gettext("AIC"),
              BIC = gettext("BIC"), r.squared = gettext("R Squared"),
              likelihood = gettext("Likelihood ratio test (p)"), wald = gettext("Wald test (p)"),
              score = gettext("Score test (p)"), squared_cox = gettext("Squared R (max)"),
              concordance = gettext("Concordance (SE)"), squared_lm = gettext("Multiple R Squared (Adjusted)"),
              stat_lm = gettext("Statistic (DF)"),
              sigma = gettext("Residuals Standard Error"), deviance = gettext("Deviance"),
              null =  gettext("Null Deviance (DF)"), residual = gettext("Residual deviance (DF)"))

  params <- c(params,
              estimate = gettext("Pooled complete data estimate"),
              ubar = gettext("Within-imputation variance of estimate"),
              b = gettext("Between-imputation variance of estimate"),
              t = gettext("Total variance of estimate"),
              dfcom = gettext("Degrees of freedom in complete data"),
              riv = gettext("Relative increase in variance"),
              lambda = gettext("Proportion attributable to the missingness"),
              fmi = gettext("Fraction of missing information"))
  varnames <- names(x)
  params <- params[varnames]

  n <- names(params)
  p <- unname(params)
  params <- setNames(n, p)
  select(x, !!params)
}


#' Get additional info on a test performed
#' @param x The dependant variable
#' @param ... Further arguments passed to or from other methods.
#' @param y Either the independant variable if survival = FALSE or follow-up time
#' @param survival logical, is it a survival analysis
#' @param censure  The censor variable
#'
#' @export
get_glance <- function(x, ...){
  UseMethod("get_glance")
}

#' @export
#' @rdname get_glance
get_glance.default <- function(x, y, survival = FALSE, censure = NULL){
  find_test(x, y, survival, censure) %>% get_pertinent_params() %>% rename_glance()
}

#' @export
#' @rdname get_glance
get_glance.lm <- function(x){
  x %>% get_pertinent_params() %>% rename_glance()
}

#' @export
#' @rdname get_glance
get_glance.coxph <- get_glance.lm

#' @export
#' @rdname get_glance
get_glance.mira <- function(x){
  pool(x)$pooled %>% rename_glance() %>% add_class("glance_mira")
}
