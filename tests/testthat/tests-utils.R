context("utils")
test_that("get_choix_var works", {
  tab <- standardize_tab(colon)
  expect_equal(unname(get_choix_var(tab)), sort(names(tab)))
  expect_equal(names(get_choix_var(tab)), sort(capitalize(names(tab))))
})

test_that("pourcent works", {
  expect_equal(pourcent(0.1), "10%")
  expect_equal(pourcent(0.1, symbol = FALSE), "10")
  expect_equal(pourcent(c(0.1, 0.2)), c("10%", "20%"))
  expect_equal(pourcent(c(0.1, 0.2), symbol = FALSE), c("10", "20"))
  expect_equal(pourcent(0.01532), "1.5%")
  expect_equal(pourcent(0.415), "42%")
})

test_that("sprintf_number_table works", {
  expect_equal(sprintf_number_table("%s (%s)", 3.784378, 0.758), "3.78 (0.758)")
  expect_equal(sprintf_number_table("%s (%s)", 3456, 567890081), "3456 (+Inf)")
  expect_equal(sprintf_number_table("%s (%s)", .00000023, -23), "0 (-23.0)")
})

test_that("get_nvar_mod works", {
  tab <- standardize_tab(colon)
  nvar <- get_nvar_mod(tab[c("age", "sex", "rx", "nodes", "obstruct", "extent")], c("sex", "rx"))
  expect_equal(nvar, 5)
})

test_that("extract_from_model is working",{
  tab <- standardize_tab(colon) %>% make_tab_survival("status", var_time = "time")
  mod <- lm(age ~ sex + rx + nodes, data = tab)
  expect_equal(extract_from_model(mod, "estimate"), coef(mod)[-1])
  expect_equivalent(extract_from_model(car::Anova(mod), "statistic"), broom::tidy(car::Anova(mod))$statistic[seq_len(3)])
  mod2 <- coxph(Surv(.time, status) ~ sex + rx + nodes, data = tab)
  expect_equal(extract_from_model(mod2, "estimate"), coef(mod2))
})



