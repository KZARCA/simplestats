context("before-after")

tab <- data.frame(
  a = rnorm(100,5,5),
  b = rnorm(100,5,5),
  c = floor(runif(100, 1,5)) %>% as.factor(),
  d = floor(runif(100, 1,4)) %>% as.factor(),
  e = floor(runif(100, 3,7)) %>% as.factor()
) %>% standardize_tab()

test_that("create_ligne_desc_ba.numeric is working", {
  x <- tab$a
  y <- tab$b
  ligne <- create_ligne_desc_ba(x, y, invert = T)[4:10]
  expect_equal(names(ligne), c(gettext("mean (sd)", domain = "R-simplestats"),
                              gettext("median [Q25-75]", domain = "R-simplestats"),
                              gettext("min", domain = "R-simplestats"),
                              gettext("max", domain = "R-simplestats"),
                              gettext("n", domain = "R-simplestats"),
                              gettext("Δ mean", domain = "R-simplestats"),
                              "p"))
  qx <- quantile(x)
  qy <- quantile(y)
  expect_equal(ligne[[1]],  c(sprintf_number_table("%s (±%s)", mean(x), sd(x)),
                            sprintf_number_table("%s (±%s)", mean(y), sd(y))))
  expect_equal(ligne[[2]],  c(sprintf_number_table("%s [%s; %s]", qx[3], qx[2], qx[4]),
                              sprintf_number_table("%s [%s; %s]", qy[3], qy[2], qy[4])))
  expect_equivalent(ligne[[3]], format_number(c(qx[1], qy[1])))
  expect_equivalent(ligne[[4]], format_number(c(qx[5], qy[5])))
  expect_equal(ligne[[6]], c(NA, format_number(mean(y) - mean(x))))
  expect_equal(ligne[[7]], rep(t.test(x, y, paired = TRUE)$p.value, 2))
})


test_that("create_ligne_desc_ba.numeric is working with median", {
  x <- tab$a
  y <- tab$b
  ligne <- create_ligne_desc_ba(x, y, invert = FALSE, summary = "median")[2:7]
  qx <- quantile(x)
  qy <- quantile(y)
  expect_equal(ligne[[2]], sprintf_number_table("%s [%s; %s]", qx[3], qx[2], qx[4]))
  expect_equal(ligne[[3]], sprintf_number_table("%s [%s; %s]", qy[3], qy[2], qy[4]))
  expect_equal(ligne[[4]], format_number(mean(y) - mean(x)))
  expect_equal(ligne[[6]], t.test(x, y, paired = TRUE)$p.value)
})

test_that("create_ligne_desc_ba.factor is working with unequal number of categories", {
  x <- tab$c
  y <- tab$d
  z <- tab$e
  ligne <- create_ligne_desc_ba(x, y)
  expect_equal(names(ligne), c("id", "variable", "niveau", gettext("before", domain = "R-simplestats"),
                               gettext("after", domain = "R-simplestats"), "n", "p", "test"))
  expect_equal(ligne$niveau, as.character(1:4))
  expect_equivalent(ligne$after[4], "0 (0%)")
  expect_equal(ligne$test, c("McNemar-Bowker", rep(NA, 3)))
  ligne <- create_ligne_desc_ba(y, z)
  expect_equivalent(ligne$after[1:2], rep("0 (0%)", 2))
  expect_equivalent(ligne$before[4:6], rep("0 (0%)", 3))
  expect_equal(ligne$test, c("McNemar-Bowker", rep(NA, 5)))
})

