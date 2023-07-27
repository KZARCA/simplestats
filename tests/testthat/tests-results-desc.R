context("results-desc")
library(survival)

tab <- colon %>% standardize_tab() %>% make_tab_survival("status", var_time = "time")
test_that("create_ligne_desc.numeric is working", {
  line <- create_ligne_desc(tab$age, noms = "age")
  expect_equal(line$id, "age")
  expect_equal(line$.variable, "Age")
  expect_equal(line[[gettext("mean (sd)", domain = "R-simplestats")]],
               sprintf_number_table("%s (%s)", mean(tab$age), sd(tab$age)))
  expect_equal(line[[gettext("mean (sd)", domain = "R-simplestats")]], "59.8 (11.9)")
  expect_equal(line[[gettext("median [Q25-75]", domain = "R-simplestats")]],
               sprintf_number_table("%s [%s; %s]", median(tab$age), quantile(tab$age, 0.25), quantile(tab$age, 0.75)))
  expect_equal(line[[gettext("median [Q25-75]", domain = "R-simplestats")]], "61.0 [53.0; 69.0]")
  expect_equal(line$min,
               sprintf_number_table("%s",min(tab$age)))
  expect_equal(line$min, "18.0")
  expect_equal(line$max,
               sprintf_number_table("%s",max(tab$age)))
  expect_equal(line$max, "85.0")
  expect_equal(line$n,
               sum(!is.na((tab$age))))
  expect_equal(line$n, 1858)
})

test_that("create_ligne_desc.factor is working", {
  find_num_prop <- function(tab, vars, val, prop = FALSE){
    n <- sum(tab[[vars]] == val, na.rm = TRUE)
    if (!prop){
      return(n)
    } else {
      pourcent(n/sum(!is.na(tab[[vars]])))
    }
  }
  test_all <- function(tab, vars, val){
    sprintf_number_table("%s (%s)", find_num_prop(tab, vars, val),
                        find_num_prop(tab, vars, val, TRUE))
  }
  line <- create_ligne_desc(tab$differ, noms = "differ")
  expect_equal(line$id, "differ")
  expect_equal(line$.variable, "Differ")
  expect_equal(line$`1`, test_all(tab, "differ", "1"))
  expect_equal(line$`2`, test_all(tab, "differ", "2"))
  expect_equal(line$`3`, test_all(tab, "differ", "3"))

})

test_that("create_ligne_desc_export is working", {
  line <- create_ligne_desc_export(tab$differ, "differ")
  expect_equal(line$id, rep("differ", 3))
  expect_equal(line$.variable, rep("Differ", 3))
  expect_equal(line$niveau, as.character(seq_len(3)))
  expect_equal(line$`n (%)`, c("186 (10%)", "1326 (73%)", "300 (17%)"))
})

test_that("create_ligne_surv_desc is working", {
  surv <- survfit(Surv(.time, status) ~ 1, data = tab)
  table_surv <- surv %>%
    summary() %>%
    extract2("table")
  line <- create_ligne_surv_desc(tab$.time, tab$status)
  expect_equal(line$id, "survival")
  expect_equal(line$.variable, gettext("max follow-up", domain = "R-simplestats"))
  expect_equal(line[[gettext("median (95% CI)", domain = "R-simplestats")]],
               sprintf_number_table("%s (%s; %s)",
                                    table_surv[["median"]], table_surv[["0.95LCL"]], table_surv[["0.95UCL"]]))
  expect_equal(unname(line$n), table_surv[["n.start"]])
  expect_equal(line[[gettext("n events")]] %>% unname(), table_surv[["events"]])
  l <- length(surv$surv)
  expect_equal(line[[gettext("survival rate (95% CI)")]],
               "46.5% (43.5%; 49.7%)")
})
