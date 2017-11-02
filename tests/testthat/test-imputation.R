test_that("get_propDM works", {
  vec <- c(letters[1:5], NA, letters[7:9], NA)
  vec2 <- c(NA, letters[1:5], NA, letters[7:9])
  expect_equal(get_propDM(vec), 0.2)
  df1 <- data.frame(vec, vec, vec)
  expect_equal(get_propDM(df1), 0.2)
  df2 <- data.frame(vec, vec2, vec)
  expect_equal(get_propDM(df2), 0.4)
})
