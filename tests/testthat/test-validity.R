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
  is_number_enough(t1, "sex", c("age")) %>%
    expect_true()
  is_number_enough(t1, "sex", c("age", "nodes")) %>%
    expect_true()
  is_number_enough(t1, "sex", c("age", "rx")) %>%
    expect_false()
})
