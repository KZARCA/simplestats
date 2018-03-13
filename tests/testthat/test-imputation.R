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
    a = c(rep(NA, 3), seq_len(97)) %>% sample(),
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

test_that("imputer uses impute for variables with less than 5% of missing data", {
  tab <- data.frame(
    a = c(rep(NA, 3), seq_len(97)),
    b = c(as.character(seq_len(96)), rep(NA, 4)),
    c = rep_len(1:2, 100) %>% as.factor(),
    .time = c(rep(NA, 3), runif(97, 1, 1000))
  )
  tab2 <- imputer(tab, "a", type = 'linear')
  expect_length(attr(tab2$b, "imputed"), 4)
  expect_null(attr(tab2$c, "imputed"))
  expect_length(attr(tab2$.time, "imputed"), 3)
  tab3 <- imputer(tab, "c", type = "survival")
  expect_length(attr(tab3$a, "imputed"), 3)
  expect_length(attr(tab3$b, "imputed"), 4)
  expect_null(attr(tab3$c, "imputed"))
  expect_null(attr(tab3$.time, "imputed"))
})

test_that("imputer uses mice for variables with more than 5% of missing data", {
  tab <- data.frame(
    a = c(rep(NA, 3), seq_len(97)) %>% sample(),
    b = c(rep(NA, 15), rep_len(1:2, 85)) %>% sample(),
    .time = c(NA, runif(99, 1, 1000))
  )
  tab2 <- imputer(tab, "a", type = 'linear')
  expect_is(tab2, "mids")
  expect_null(tab2$imp$a[[1]])
  expect_length(tab2$imp$b[[1]], 15)
  expect_null(tab2$imp$.time[[1]])
  expect_length(attr(tab2$pad$data$.time, "imputed"), 1)
})