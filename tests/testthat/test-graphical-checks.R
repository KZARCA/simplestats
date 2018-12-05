context("graphical-checks")

pdf(NULL) #prevents the creation of a Rplots.pdf file

test_that("prepare_zph is working", {
  tab <- standardize_tab(colon) %>%
    make_tab_survival("status", var_time = "time") %>%
    dplyr::select("status", "rx", "age", ".time", "sex", "obstruct")
  expect_equal(prepare_zph(tab, "status", "rx", "age") %>% broom::tidy(),
               coxph(Surv(.time, status) ~ rx + age, data = tab) %>% broom::tidy())
  expect_equal(prepare_zph(tab, "status", "rx", c("age", "sex")) %>% broom::tidy(),
               coxph(Surv(.time, status) ~ rx + age + sex, data = tab) %>% broom::tidy())
  expect_equal(prepare_zph(tab, "status", c("age", "obstruct"), c("rx", "sex")) %>% broom::tidy(),
               coxph(Surv(.time, status) ~ age + obstruct + rx + sex, data = tab) %>% broom::tidy())
})

test_that("plot_nth_zph is working", {
  tab <- standardize_tab(colon) %>%
    make_tab_survival("status", var_time = "time")
  mod <- coxph(Surv(.time, status) ~ age + obstruct + rx + sex, data = tab)
  expect_error(plot_nth_zph(mod, 6))
  walk(seq_len(5), function(i) expect_error(simplestats:::plot_nth_zph(mod, i), NA))
})

test_that("create_spline returns NULL when no independent variable is numeric", {
  tab <- standardize_tab(colon)
  expect_null(create_spline(tab, "age", "sex", type = "linear"))
  expect_null(create_spline(tab, "sex", c("rx", "differ"), type = "logistic"))
  tab %<>% make_tab_survival("status", var_time = "time")
  expect_null(create_spline(tab, "status", c("rx", "differ"), type = "survival"))
})

test_that("create_spline returns a length 3 list when at least 1 independant variable is numeric", {
  tab <- standardize_tab(colon)
  expect_length(create_spline(tab, "age", c("rx", "nodes"), type = "linear"), 3)
  expect_length(create_spline(tab, "sex", c("rx", "nodes", "age"), type = "logistic"), 3)
  tab %<>% make_tab_survival("status", var_time = "time")
  expect_length(create_spline(tab, "status", c("rx", "differ", "age"), type = "survival"), 3)
})

test_that("create_spline returns a list with as many graph and lin as numeric independant variables", {
  tab <- standardize_tab(colon)
  test_splines_graph <- function(tab, vardep, varindep, type){
    l <- length(select_if(tab[varindep], is.numeric))
    create_spline(tab, vardep, varindep, type = type) %>%
      extract2("graph") %>%
      extract2("smooth") %>%
      expect_length(l)
  }
  test_splines_lin <- function(tab, vardep, varindep, type){
    l <- length(select_if(tab[varindep], is.numeric))
    create_spline(tab, vardep, varindep, type = type) %>%
      extract2("lin") %>%
      expect_length(l)
  }
  test_splines_graph(tab, "age", c("sex", "nodes"), "linear")
  test_splines_graph(tab, "sex", c("age", "nodes"), "logistic")
  tab %<>% make_tab_survival("status", var_time = "time")
  test_splines_graph(tab, "status", c("age", "nodes"), "survival")
  test_splines_lin(tab, "age", c("nodes"), "linear")
  test_splines_lin(tab, "sex", c("age", "nodes"), "logistic")
  test_splines_lin(tab, "status", c("age", "nodes"), "survival")
})

test_that("create_spline surrounds with s() numerical dependant variables, ns() numerical adjustment variables", {
  tab <- standardize_tab(colon)
  vardep <- "extent"
  varindep <- c("sex", "nodes", "age")
  var_ajust <- c("rx", "age", "time", "differ")
  spl <- simplestats:::create_spline(tab, vardep, varindep, var_ajust, "logistic")
  expect_equal(spl$graph$formula, extent ~ s(nodes) + s(age) + sex + ns(age) + ns(time) + rx + differ)
  expect_equal(spl$mod$formula, extent ~ nodes + age + sex + age + time + rx + differ)
})


test_that("plot_nth_spline is working", {
  tab <- standardize_tab(colon) %>%
    make_tab_survival("status", var_time = "time")

  nth_spline <- function(tab, vardep, varindep, type, n){
    walk(n, function(i){
    create_spline(tab, vardep, varindep, type = type) %>%
        plot_nth_spline(i)
    })
  }

  suppressWarnings(expect_error(nth_spline(tab, "age", c("sex", "rx"), "linear", 1)))
  expect_error(nth_spline(tab, "age", c("sex", "nodes"), "linear", 1), NA)
  expect_error(nth_spline(tab, "age", c("sex", "nodes"), "linear", 1:2))
  expect_error(nth_spline(tab, "sex", c("age", "nodes"), "logistic", 1:2), NA)
  expect_error(nth_spline(tab, "status", c("age", "nodes"), "survival", 1:2), NA)
})

