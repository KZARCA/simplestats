test_that("read_tab_imp is working with csv", {
  tmp <- tempfile(fileext = ".csv")
  readr::write_csv(colon, tmp)
  readed <- read_tab_imp(tmp, firstImport = TRUE, sep = ";", dec=",")
  colon2 <- standardize_tab(colon)
  expect_equal(readed[[1]],  colon2)
  expect_equal(readed[[2]],  names(colon))
})
