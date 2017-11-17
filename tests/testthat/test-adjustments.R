library(survival)

test_def <- function(tab, threshold, args){
  test <- do.call(define_varAjust, c(list(tab), args, list(test = TRUE)))
  expect_equal(test[test < threshold], do.call(define_varAjust, c(list(tab), args)))
  if(args[[3]] == 'linear') {
    .fun <- lm
    formule <- sprintf("%s ~ adhere", args[[1]]) %>% as.formula()
    i <- 1
  } else if (args[[3]] == "logistic"){
    .fun <- arm::bayesglm
    formule <- sprintf("%s ~ adhere", args[[1]]) %>% as.formula()
    i <- 1
  } else if (args[[3]] == "survival"){
    .fun <- coxph
    formule <- sprintf("Surv(.time, status) ~ adhere") %>% as.formula()
    i <- 2
  }
  do.call(.fun, c(list(data = tab, formula = formule), if (args[[3]] == "logistic") list(family = "binomial")))  %>%
    car::Anova() %>%
    broom::tidy() %>%
    magrittr::extract2("p.value") %>%
    magrittr::extract(i) %>%
    expect_equal(unname(test[1]))
}

test_that("define_varAjust returns variables with a univariate pvalue < 0.2", {
  tab <- standardize_tab(colon)
  expect_equal(define_varAjust(tab, "rx", "age", "multiple"), numeric(0))
  expect_equal(names(define_varAjust(tab, "age", "rx", "linear")), c("adhere", "node4", "nodes", "obstruct"))
  varindep <- "extent"
  test_def(tab, .2, args = c(list("age"), list(varindep), list("linear")))
  test_def(tab, .2, args = c(list("obstruct"), list(varindep), list("logistic")))
  tab <- make_tab_survival(tab, "status", var_time = "time")
  test_def(tab, .2, args = c(list("status"), list(varindep), list("survival")))

})

test_that("define_varAjust returns variables with a univariate pvalue < any threshold", {
  tab <- colon %>% standardize_tab()
  tab2 <- data.frame(cbind(tab, select(tab, -nodes)))
  varindep <- "extent"
  test_def(tab2, 5/31, args = c(list("nodes"), list(varindep), list("linear")))
  tab2 <- data.frame(cbind(tab, select(tab, -obstruct)))
  test_def(tab2, 5/31, args = c(list("obstruct"), list(varindep), list("logistic")))
  tab2 <- data.frame(cbind(tab, select(tab, -status, -time))) %>% make_tab_survival("status", var_time = "time")
  test_def(tab2, 5/30, args = c(list("status"), list(varindep), list("survival")))
  tab3 <- data.frame(cbind(tab, select(tab, -nodes), select(tab, -nodes)))
  test_def(tab3, 5/47, args = c(list("nodes"), list(varindep), list("linear")))
  tab3 <- data.frame(cbind(tab, select(tab, -obstruct), select(tab, -obstruct)))
  test_def(tab3, 5/47, args = c(list("obstruct"), list(varindep), list("logistic")))
  tab3 <- data.frame(cbind(tab, select(tab, -status, -time), select(tab, -status, -time))) %>% make_tab_survival("status", var_time = "time")
  test_def(tab3, 5/46, args = c(list("status"), list(varindep), list("survival")))

})

test_that("recherche_multicol removes 1-level factors", {
  tab <- standardize_tab(colon)
  tab$elim1 <- rep(1, nrow(tab)) %>% as.factor()
  recherche_multicol(tab, "age", "sex", c("obstruct", "elim1"), type = "linear") %>%
    expect_equal("elim1")
  recherche_multicol(tab, "age", c("sex", "elim1"), c("obstruct"), type = "linear") %>%
    expect_equal("elim1")
  recherche_multicol(tab, "node4", "sex", varAjust = character(0), type = "logistic") %>%
    expect_equal(character(0))
})

test_that("recherche_multicol removes aliased coefficients", {
  #find examples
})

test_that("recherche_multicol removes high vif covariates", {
  #find examples
})

test_that("recherche_multicol removes high vif varAjust in priority over varindep", {
})
