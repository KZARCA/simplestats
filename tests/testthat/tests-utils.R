test_that("get_choix_var works", {
  tab <- standardize_tab(colon)
  expect_equal(unname(get_choix_var(tab)), sort(names(tab)))
  expect_equal(names(get_choix_var(tab)), sort(Hmisc::capitalize(names(tab))))
})

test_that("pourcent works", {
  expect_equal(pourcent(0.1), "10%")
  expect_equal(pourcent(0.1, symbol = FALSE), "10")
  expect_equal(pourcent(c(0.1, 0.2)), c("10%", "20%"))
  expect_equal(pourcent(c(0.1, 0.2), symbol = FALSE), c("10", "20"))
  expect_equal(pourcent(0.01532), "1.5%")
  expect_equal(pourcent(0.415), "42%")
})
