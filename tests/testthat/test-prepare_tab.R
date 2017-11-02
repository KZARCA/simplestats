library(survival)
test_that("make_tab_survival works with var_time, passage 1 & 2", {
  tab <- colon
  tab$status <- tab$status + 1
  tab %<>% standardize_tab()
  tab_modif <- make_tab_survival(tab, "status", var_time = "time")
  tab_modif2 <-  make_tab_survival(tab, "status", passage = 2, var_time = "time")
  tests <- function(tab_modif, passage = 1){
    expect_null(tab_modif$time)
    expect_equal(tab_modif$.time, tab$time)
    expect_type(tab_modif$status, "double")
    expect_true(all(tab_modif$status == 1 | tab_modif$status == 0))
    if (passage == 1) {
      expect_equal(as.numeric(tab_modif$status), colon$status)
    } else {
      expect_equal(as.numeric(tab_modif$status), 1-colon$status)
    }
  }
  tests(tab_modif)
  tests(tab_modif2, 2)
})

test_that("make_tab_survival works with dateSortie & Inclusion", {
  tab <- colon
  tab$inclusion <- as.Date(runif(nrow(tab), 1, 5000), "1990-01-01")
  tab$sortie <- as.Date(tab$inclusion + tab$time)
  tab %<>% standardize_tab()
  tab_modif <- make_tab_survival(tab, "status", dateSortie = "sortie", dateInclusion = "inclusion")
  expect_null(tab_modif$sortie)
  expect_null(tab_modif$inclusion)
  expect_equal(tab_modif$.time, as.numeric(tab$sortie - tab$inclusion))
})
