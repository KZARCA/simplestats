context("imputation")
test_that("get_propDM works", {
  vec <- c(letters[1:5], NA, letters[7:9], NA)
  vec2 <- c(NA, letters[1:5], NA, letters[7:9])
  expect_equal(get_propDM(vec), 0.2)
  df1 <- data.frame(vec, vec, vec)
  expect_equal(get_propDM(df1), 0.2)
  df2 <- data.frame(vec, vec2, vec)
  expect_equal(get_propDM(df2), 0.4)
})


test_that("imputer does not impute when less than 5% of missing data", {
  tab <- data.frame(
    a = c(rep(NA, 2), seq_len(98)) %>% sample(),
    b = c(NA, as.character(seq_len(99))) %>% sample(),
    c = rep_len(1:2, 100) %>% as.factor(),
    .time = c(NA, runif(99, 1, 1000))
  )
  tab2 <- imputer(tab, "a", type = 'linear')
  expect_null(attr(tab2$a, "imputed"))
  expect_null(attr(tab2$b, "imputed"))
  expect_null(attr(tab2$c, "imputed"))
  expect_null(attr(tab2$d, "imputed"))
})

test_that("imputer uses mice for variables with more than 5% of missing data", {
  tab <- data.frame(
    a = c(rep(NA, 3), seq_len(97)) %>% sample(),
    b = c(rep(NA, 15), rep_len(1:2, 85)) %>% sample(),
    .time = floor(c(NA, runif(99, 1, 1000))),
    event = rbinom(100,1,0.2)
  )
  tab2 <- imputer(tab, "a", type = 'linear')
  expect_is(tab2, "mids")
  expect_length(tab2$imp$b[[1]], 15)
  tab3 <- imputer(tab, "event", type = 'survival')
  expect_is(tab3, "mids")
  expect_length(tab3$imp$b[[1]], 15)
  expect_length(tab3$imp$a[[1]], 3)
})


test_that("imputer uses impute for variables with more than 5% of missing data with large datasets", {
  set.seed(1)
  tab <- data.frame(
    a = c(rep(NA, 3), seq_len(5997)) %>% sample(),
    b = c(rep(NA, 600), rep_len(1:2, 5400)) %>% sample(),
    c = c(rep(NA, 600), rep_len(letters[1:9], 5400)) %>% sample(),
    d = c(rep(NA, 600), rep_len(LETTERS[1:9], 5400)) %>% sample(),
    stringsAsFactors = TRUE
  )
  expect_equivalent(imputer(tab, "a", type = "linear")$method, c("pmm", "pmm", "polyreg", "polyreg"))
  set.seed(1)
  tab2 <- dplyr::sample_frac(tab, 0.2)
  expect_equivalent(imputer(tab2, "a", type = "linear")$method, c("pmm", "pmm", "polyreg", "polyreg"))
})

# test_that("imputer does not remove labels", {
#   tab <- data.frame(
#     a = c(rep(NA, 3), seq_len(97)) %>% sample(),
#     b = c(rep(NA, 15), rep_len(1:2, 85)) %>% sample(),
#     .time = c(NA, runif(99, 1, 1000))
#   )
#   labs <- c("A", "B", "Time")
#   label(tab, self = FALSE) <- labs
#   tab2 <- imputer(tab, "a", type = 'linear')
#   mice::complete(tab2) %>%
#     label() %>%
#     expect_equivalent(labs)
# })
