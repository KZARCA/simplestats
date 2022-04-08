library(survival)

context("import")
test_that("read_tab_import is working with csv", {
  tmp <- tempfile(fileext = ".csv")
  readr::write_csv(colon, tmp)
  tab <- read_tab_import(tmp)
  colon2 <- standardize_tab(colon)
  expect_equivalent(tab, colon2)
  expect_equal(attr(tab, "name_matching")$noms,  names(colon))
})

test_that("read_tab_import is working with txt utf16", {
  tmp <- tempfile(fileext = ".txt")
  names(colon)[1] <- "id ée"
  colon[1,2] <- "idée"
  write.table(colon, tmp, fileEncoding="UTF-16", sep = "\t")
  tab <- read_tab_import(tmp, sep = "\t", dec=".")
  colon2 <- standardize_tab(colon)
  expect_equivalent(tab,  colon2)
  expect_equal(attr(tab, "name_matching")$noms, names(colon))
  expect_equal(names(tab)[1], "id_ée")
  expect_equal(as.character(tab[1,2]), "idée")
  expect_equal(attr(tab, "name_matching")$noms[1], "id ée")
  expect_equivalent(label(tab)[1], "Id ée")
})


