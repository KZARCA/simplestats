test_that("read_tab_import is working with csv", {
  tmp <- tempfile(fileext = ".csv")
  readr::write_csv(colon, tmp)
  readed <- read_tab_import(tmp)
  colon2 <- standardize_tab(colon)
  expect_equal(readed[[1]],  colon2)
  expect_equal(readed[[2]],  names(colon))
})

test_that("read_tab_import is working with txt utf16", {
  tmp <- tempfile(fileext = ".txt")
  names(colon)[1] <- "id ée"
  colon[1,2] <- "idée"
  write.table(colon, tmp, fileEncoding="UTF-16", sep = "\t")
  readed <- read_tab_import(tmp, sep = "\t", dec=".")
  colon2 <- standardize_tab(colon)
  expect_equal(readed[[1]],  colon2)
  expect_equal(readed[[2]],  names(colon))
  expect_equal(names(readed[[1]])[1], "id.ée")
  expect_equal(as.character(readed[[1]][1,2]), "idée")
  expect_equal(readed[[2]][1], "id ée")
  expect_equivalent(label(readed[[1]])[1], "Id ée")
})


