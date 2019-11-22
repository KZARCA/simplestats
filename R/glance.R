get_pertinent_params <- function(x){
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

prepare_glance <- function(x){
  params <- c(p.value = "p", df = gettext("Degree of Freedom"),
              parameter = gettext("Degree of Freedom"), estimate_t =  gettext("Mean Difference (CI95)"),
              estimate_p = gettext("Correlation Coefficient (CI95)"), estimate_p2 = gettext("Correlation Coefficient"),
              sumsq = gettext("Sum of Squares"), statistic = gettext("Test statistic"), method = gettext("Test"), term = gettext("Variable")
              )
  varnames <- names(x)
  params <- params[varnames]

  n <- names(params)
  p <- unname(params)
  params <- setNames(n, p)
  select(x, !!params) %>%
    mutate_if(is.numeric, format_number)
}

#' Get additional info on a test performed
#' @param x The dependant variable
#' @param y Either the independant variable if survival = FALSE or follow-up time
#' @param survival logical, is it a survival analysis
#' @param censure  The censor variable
#'
#' @export
get_glance <- function(x, y, survival = FALSE, censure = NULL){
  find_test(x, y, survival, censure) %>% get_pertinent_params() %>% prepare_glance()
}
