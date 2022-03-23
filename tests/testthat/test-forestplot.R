context("forest plot")
tab <- standardize_tab(survival::colon)


test_that("format_forestplot gets the correct number of rows", {
  vardep <- "sex"
  varindep <- c("rx", "extent", "age")
  varajust <- "nodes"
  mod <- compute_mod(tab, vardep, varindep, varajust, "logistic")$mod
  t_mod <- create_table_forestplot(mod, "nodes") %>%
    prepare_forestplot()
  map_dbl(varindep, function(x){
   if (is.factor(tab[[x]])) nlevels(tab[[x]]) + 1 else 2
  }) %>%
    sum() %>%
  expect_equal(nrow(t_mod))
})
