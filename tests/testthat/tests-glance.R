context("glance")
tab <- standardize_tab(mtcars)
test_that("get_glance Mann Whitney is working", {
  t <- get_glance(tab$vs, tab$wt)
  expect_error(t, NA)
  expect_warning(t, NA)
  expect_equal(names(t), c("Test statistic", "p", "Test"))
  expect_equal(t$Test, "Wilcoxon rank sum test with continuity correction")
})
test_that("get_glance Fisher is working", {
  t <- get_glance(tab$vs, tab$cyl)
  expect_error(t, NA)
  expect_warning(t, NA)
  expect_equal(names(t), c("p", "Test"))
  expect_equal(t$Test, "Fisher's Exact Test for Count Data")
})
test_that("get_glance Chi2 is working", {
  t <- get_glance(tab$am, tab$vs)
  expect_error(t, NA)
  expect_warning(t, NA)
  expect_equal(names(t),  c("Degrees of Freedom", "Test statistic","p", "Test"))
  expect_equal(t$Test, "Pearson's Chi-squared test")
})

test_that("get_glance Pearson is working", {
  t <- get_glance(tab$mpg, tab$wt)
  expect_error(t, NA)
  expect_warning(t, NA)
  expect_equal(names(t), c("Correlation Coefficient (CI95)", "Degrees of Freedom", "Test statistic", "p", "Test"))
  expect_equal(t$Test, "Pearson's product-moment correlation")
})
test_that("get_glance Kruskall-Wallis is working", {
  t <- get_glance(tab$mpg, tab$gear)
  expect_error(t, NA)
  expect_warning(t, NA)
  expect_equal(names(t), c("Degrees of Freedom", "Test statistic", "p", "Test"))
  expect_equal(t$Test, "Kruskal-Wallis rank sum test")
})

test_that("get_glance Spearman is working", {
  t <- get_glance(tab$mpg, tab$hp)
  expect_error(t, NA)
  expect_warning(t, NA)
  expect_equal(names(t), c("Correlation Coefficient","Test statistic", "p", "Test"))
  expect_equal(t$Test, "Spearman's rank correlation rho")
})

library(survival)

tab <- standardize_tab(colon)
test_that("get_glance Welch is working", {
  t <- get_glance(tab$age, tab$sex)
  expect_error(t, NA)
  expect_warning(t, NA)
  expect_equal(names(t), c("Mean Difference (CI95)", "Degrees of Freedom", "Test statistic", "p", "Test"))
  expect_equal(t$Test, "Welch Two Sample t-test")
})
test_that("get_glance Anova is working", {
  t <- get_glance(tab$age, tab$rx)
  expect_error(t, NA)
  expect_warning(t, NA)
  expect_equal(names(t), c("Variable", "Degrees of Freedom", "Sum of Squares", "Test statistic", "p", "Test"))
  expect_equal(t$Test, c("Analysis of Variance", NA))
})
test_that("get_glance Logrank is working", {
  t <- get_glance(colon$sex, colon$time, survival = TRUE, colon$status)
  expect_error(t, NA)
  expect_warning(t, NA)
  expect_equal(names(t), c("Degrees of Freedom", "Test statistic", "p", "Test"))
  expect_equal(t$Test, "Mantel-Haenszel - Logrank")
})

test_that("get_glance lm is working", {
  t <- get_glance(lm(mpg ~ vs + wt, data = mtcars))
  expect_error(t, NA)
  expect_warning(t, NA)
  expect_equal(names(t), c("Multiple R Squared (Adjusted)", "Statistic (DF)", "Deviance",
                           "Log-Likelihood", "AIC", "BIC", "Residuals Standard Error", "p"))
})

test_that("get_glance glm is working", {
  t <- get_glance(glm(vs ~ mpg + wt, data = mtcars, family = binomial))
  expect_error(t, NA)
  expect_warning(t, NA)
  expect_equal(names(t), c("Null Deviance (DF)", "Residual deviance (DF)", "Log-Likelihood",
                           "AIC", "BIC"))
})

test_that("get_glance coxph is working", {
  t <- get_glance(coxph(Surv(time, status) ~ sex + rx + age, data = colon))
  expect_error(t, NA)
  expect_warning(t, NA)
  expect_equal(names(t), c("Squared R (max)", "Concordance (SE)", "Log-Likelihood",
                           "AIC", "BIC", "Likelihood ratio test (p)", "Score test (p)",
                           "Wald test (p)"  ))
})

test_that("get_glance mira is working", {
  library(mice)
  imp <- mice(nhanes, maxit = 2, m = 2)
  fit <- with(data = imp, exp = lm(bmi ~ hyp + chl))
  t <- get_glance(fit)
  expect_error(t, NA)
  expect_warning(t, NA)
  expect_equal(names(t), c("Pooled complete data estimate", "Within-imputation variance of estimate",
                           "Between-imputation variance of estimate", "Total variance of estimate",
                           "Degrees of freedom in complete data", "Degrees of Freedom",
                           "Relative increase in variance", "Proportion attributable to the missingness",
                           "Fraction of missing information")
               )

})
