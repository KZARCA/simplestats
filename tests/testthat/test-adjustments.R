context("adjustment")
library(survival)

test_def <- function(tab, threshold, args){
  test <- do.call(define_varajust, c(list(tab), args, list(all_vars = TRUE, by_lasso = FALSE)))
  vals <- attr(test, "value")
  expect_equal(vals[vals < threshold], attr(do.call(define_varajust, c(list(tab), args, list(by_lasso = FALSE))), "value"))
}

test_that("define_varajust returns variables with a univariate pvalue < 0.2", {
  tab <- standardize_tab(colon)
  expect_length(define_varajust(tab, "rx", "age", "multiple", by_lasso = FALSE), 0)
  expect_equivalent(define_varajust(tab, "age", "rx", "linear", by_lasso = FALSE), c("adhere", "node4", "nodes", "obstruct"))
  expect_length(define_varajust(tab, "age", "rx", "linear", by_lasso = TRUE), 0)
  varindep <- "extent"
 # test_def(tab, .2, args = c(list("age"), list(varindep), list("linear")))
  test_def(tab, .2, args = c(list("obstruct"), list(varindep), list("logistic")))
  tab <- make_tab_survival(tab, "status", var_time = "time")
  test_def(tab, .2, args = c(list("status"), list(varindep), list("survival")))

})

test_that("define_varajust removes variables with contrasts problems", {
  tab <- data.frame(V1 = c(rep("non", 49), "oui"), V2 = c((1:49)^2, NA), V3 = rnorm(50), V4 = 1:50, stringsAsFactors = TRUE)
  expect_equivalent(define_varajust(tab, "V2", "V3", "linear", by_lasso = FALSE), "V4")
  expect_equivalent(define_varajust(tab, "V2", "V3", "linear", by_lasso = TRUE), "V4")
})


test_that("find_multicol removes 1-level factors", {
  tab <- standardize_tab(colon) %>% make_tab_survival("status", var_time = "time")
  tab$elim1 <- rep(1, nrow(tab)) %>% as.factor()
  find_multicol(tab, "age", "sex", c("obstruct", "elim1"), type = "linear") %>%
    expect_equal("elim1")
  find_multicol(tab, "age", c("sex", "elim1"), c("obstruct"), type = "linear") %>%
    expect_equal("elim1")
  find_multicol(tab, "node4", "sex", varajust = character(0), type = "logistic") %>%
    expect_null()
  find_multicol(tab, "status", c("sex", "nodes"), varajust = character(0), type = "survival") %>%
    expect_equal(character(0))
  find_multicol(tab, "age", "sex", character(0), "linear")
})

test_that("find_multicol works with all types of models", {
  tab <- standardize_tab(colon) %>% make_tab_survival("status", var_time = "time")
  vardep <- "age"
  varindep <- "sex"
  # varajust <- define_varajust(tab, vardep, varindep, "linear", by_lasso = FALSE)
  # find_multicol(tab, vardep, varindep , varajust, "linear") %>%
  #   expect_error(NA)
  vardep <- "sex"
  varindep <- "age"
  varajust <- define_varajust(tab, vardep, varindep, "logistic", by_lasso = FALSE)
  find_multicol(tab, vardep, varindep , varajust, "logistic") %>%
    expect_error(NA)
  vardep <- "status"
  varindep <- "sex"
  varajust <- define_varajust(tab, vardep, varindep, "survival", by_lasso = FALSE)
  find_multicol(tab, vardep, varindep , varajust, "survival") %>%
    expect_error(NA)
})

test_that("find_multicol removes aliased coefficients", {
  x1 <- rnorm( 100 )
  x2 <- 2 * x1
  y <- rnorm( 100 )
  tab <- data.frame(x1, x2, y)
  find_multicol(tab, "y", c("x1", "x2"), varajust=character(0), type="linear") %>%
    expect_equal("x2")
})

test_that("find_multicol removes high vif covariates", {
  find_multicol(mtcars, "mpg", c("disp", "hp", "wt", "qsec"), NULL, type = "linear") %>%
  expect_equal("wt")  # would have been disp if it was not the varindep[1]
  find_multicol(mtcars, "mpg", c("disp", "hp", "wt", "qsec", "cyl"), NULL, type = "linear") %>%
  expect_equal(c("cyl", "wt"))
  find_multicol(mtcars, "mpg", c("hp", "wt", "qsec"), "disp", type = "linear") %>%
  expect_equal("disp")
  find_multicol(carData::Duncan, "prestige", c("income", "education"), NULL, type = "linear") %>%
    expect_equal(character(0))
  find_multicol(carData::Duncan, "prestige", c("income", "education", "type"), NULL, type = "linear") %>%
    expect_equal("education")

  set.seed(1)
  mtcars$x1 <- rnorm(32)
  mtcars$x2 <- 2 * mtcars$x1

  find_multicol(mtcars, "mpg", c("disp", "hp", "wt", "qsec", "cyl", "x1", "x2"), NULL, type = "linear") %>%
    expect_equal(c("x2", "cyl", "wt"))
})

test_that("find_multicol removes high vif varajust in priority over varindep", {
  find_multicol(mtcars, "mpg", c("hp", "wt", "qsec", "disp"), "cyl", type = "linear") %>%
    expect_equal(c("disp", "cyl"))
})
