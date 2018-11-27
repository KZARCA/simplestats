context("adjustment")
library(survival)

test_def <- function(tab, threshold, args){
  test <- do.call(define_varAjust, c(list(tab), args, list(test = TRUE)))
  expect_equal(test[test < threshold], do.call(define_varAjust, c(list(tab), args)))
  if(args[[3]] == 'linear') {
    .fun <- lm
    formule <- sprintf("%s ~ adhere", args[[1]]) %>% as.formula()
    i <- 1
  } else if (args[[3]] == "logistic"){
    .fun <- glm
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

test_that("define_varAjust removes variables with contrasts problems", {
  tab <- data.frame(V1 = c(rep("non", 49), "oui"), V2 = c((1:49)^2, NA), V3 = rnorm(50), V4 = 1:50)
  expect_equal(names(define_varAjust(tab, "V2", "V3", "linear")), "V4")
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
  tab <- standardize_tab(colon) %>% make_tab_survival("status", var_time = "time")
  tab$elim1 <- rep(1, nrow(tab)) %>% as.factor()
  recherche_multicol(tab, "age", "sex", c("obstruct", "elim1"), type = "linear") %>%
    expect_equal("elim1")
  recherche_multicol(tab, "age", c("sex", "elim1"), c("obstruct"), type = "linear") %>%
    expect_equal("elim1")
  recherche_multicol(tab, "node4", "sex", var_ajust = character(0), type = "logistic") %>%
    expect_equal(character(0))
  recherche_multicol(tab, "status", c("sex", "nodes"), var_ajust = character(0), type = "survival") %>%
    expect_equal(character(0))
  recherche_multicol(tab, "age", "sex", character(0), "linear")
})

test_that("recherche_multicol works with all types of models", {
  tab <- standardize_tab(colon) %>% make_tab_survival("status", var_time = "time")
  vardep <- "age"
  varindep <- "sex"
  var_ajust <- define_varAjust(tab, vardep, varindep, "linear")
  recherche_multicol(tab, vardep, varindep , names(var_ajust), "linear") %>%
    expect_error(NA)
  vardep <- "sex"
  varindep <- "age"
  var_ajust <- define_varAjust(tab, vardep, varindep, "logistic")
  recherche_multicol(tab, vardep, varindep , names(var_ajust), "logistic") %>%
    expect_error(NA)
  vardep <- "status"
  varindep <- "sex"
  var_ajust <- define_varAjust(tab, vardep, varindep, "survival")
  recherche_multicol(tab, vardep, varindep , names(var_ajust), "survival") %>%
    expect_error(NA)
})

test_that("recherche_multicol removes aliased coefficients", {
  x1 <- rnorm( 100 )
  x2 <- 2 * x1
  y <- rnorm( 100 )
  tab <- data.frame(x1, x2, y)
  recherche_multicol(tab, "y", c("x1", "x2"), var_ajust=character(0), type="linear") %>%
    expect_equal("x2")
})

test_that("recherche_multicol removes high vif covariates", {
  recherche_multicol(mtcars, "mpg", c("disp", "hp", "wt", "qsec"), NULL, type = "linear") %>%
  expect_equal("wt")  # would have been disp if it was not the varindep[1]
  recherche_multicol(mtcars, "mpg", c("disp", "hp", "wt", "qsec", "cyl"), NULL, type = "linear") %>%
  expect_equal(c("cyl", "wt"))
  recherche_multicol(mtcars, "mpg", c("hp", "wt", "qsec"), "disp", type = "linear") %>%
  expect_equal("disp")
  recherche_multicol(carData::Duncan, "prestige", c("income", "education"), NULL, type = "linear") %>%
    expect_equal(character(0))
  recherche_multicol(carData::Duncan, "prestige", c("income", "education", "type"), NULL, type = "linear") %>%
    expect_equal("education")

  set.seed(1)
  mtcars$x1 <- rnorm(32)
  mtcars$x2 <- 2 * mtcars$x1

  recherche_multicol(mtcars, "mpg", c("disp", "hp", "wt", "qsec", "cyl", "x1", "x2"), NULL, type = "linear") %>%
    expect_equal(c("x2", "cyl", "wt"))
})

test_that("recherche_multicol removes high vif varAjust in priority over varindep", {
  recherche_multicol(mtcars, "mpg", c("hp", "wt", "qsec", "disp"), "cyl", type = "linear") %>%
    expect_equal(c("cyl", "disp"))
})
