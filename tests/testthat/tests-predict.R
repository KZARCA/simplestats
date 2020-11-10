context("predict")
library(survival)
tab <- standardize_tab(colon) %>% filter(etype == 1) %>% select(-id) %>% create_tabi("pred")

test_that("get_lasso_variables returns a character vector with no forced variable ", {
  expect_type(get_lasso_variables(tab, "status"), "character")
  expect_type(get_lasso_variables(tab, "adhere"), "character")
  expect_equal(get_lasso_variables(tab, "status"), "time")
  expect_length(get_lasso_variables(tab, "adhere"), 0)
})

test_that("get_lasso_variables returns a character vector with forced variable ", {
  expect_type(get_lasso_variables(tab, "status", c('age', "rx")), "character")
  expect_type(get_lasso_variables(tab, "adhere", c('age', "rx")), "character")
  expect_equal(get_lasso_variables(tab, "status", c("rx", "age")), c("time"))
  expect_equal(get_lasso_variables(tab, "adhere", c("rx", "age")), character(0))
})


test_that("get_cv_auc works with any number of cv and returns a list of double", {
  perf <- get_cv_auc(tab, "status")
  expect_error(perf, NA)
  expect_type(perf, "list")
  expect_length(perf, 10)
  perf2 <- get_cv_auc(tab, "status", n = 5)
  expect_length(perf2, 5)
  expect_error(flatten_dbl(perf), NA)
})

# test_that("get_cv_auc", {
#   vardep <- "status"
#   varindep <- NULL
#   varajust <- get_lasso_variables(tab, vardep)
#   tab <- tab[c(vardep, varindep, varajust)]
#   results <- compute_mod(tab, vardep, varindep, varajust = varajust, type = "logistic", pred = TRUE, cv = F)
#   v <- get_pred_perf(tab, vardep, varindep, "logistic", "boostrap", nCPU = 4, mod = results$mod)
# })
