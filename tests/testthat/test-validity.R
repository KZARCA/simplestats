context("validity")
test_that("is_number_enough works", {
  slice(colon, 1:31) %>%
    is_number_enough("age", c("rx", "sex")) %>%
    expect_true()
  slice(colon, 1:30) %>%
    is_number_enough("age", c("rx", "sex")) %>%
    expect_false()
  t1 <- colon %>%
    standardize_tab() %>%
    slice(1:60)
  is_number_enough(t1, "sex", c("age"), "logistic") %>%
    expect_true()
  is_number_enough(t1, "sex", c("age", "nodes"), "logistic") %>%
    expect_true()
  is_number_enough(t1, "sex", c("age", "rx"), "logistic") %>%
    expect_false()
  is_number_enough(t1, "status", c("age", "obstruct", "sex"), "survival", 1) %>%
    expect_true()
  is_number_enough(t1, "status", c("age", "rx", "sex"), "survival", 1) %>%
    expect_false()
})

test_that("is_normal is working", {
  replicate(100, expect_true(is_normal(rnorm(200))))
  replicate(100, expect_false(is_normal(rlnorm(1000))))
  replicate(100, expect_false(is_normal(rchisq(100, .5))))
  replicate(100, expect_false(is_normal(rf(100, 1, 1))))
  replicate(100, expect_true(is_normal(rpois(1000, 30))))
})

test_that("is_homoscedatic is working", {
  a <- c(rnorm(1000, 0, 1), rnorm(1000, 0, 1), rnorm(1000, 0, 1))
  b <- c(rep("A", 1000),rep("B", 1000),rep("C", 1000))
  c <- c(rnorm(1000, 0, 1), rnorm(1000, 0, sqrt(2)), rnorm(1000, 0, 1))
  d <- c(rnorm(1000, 0, 1), rnorm(1000, 0, 2), rnorm(1000, 0, 1))

  tab <- data.frame(a, b, c, d)

  expect_true(is_homoscedatic(b, a))
  expect_true(is_homoscedatic(b, c))
  expect_false(is_homoscedatic(b, d))

  mod <- lm(a ~ b, data = tab)
  expect_true(is_homoscedatic(mod))
  mod <- lm(c ~ b, data = tab)
  expect_true(is_homoscedatic(mod))
  mod <- lm(d ~ b, data = tab)
  expect_false(is_homoscedatic(mod))

  n <- rep(1:100, 2)
  a <- 0
  b <- 1
  sigma <- n^1.3
  eps <- rnorm(n, 0, sqrt(sigma))
  y <- a + b * n + eps
  mod <- lm(y ~ n)

  expect_false(is_homoscedatic(mod))
})
