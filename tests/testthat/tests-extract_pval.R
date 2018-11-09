context("extract_pval")
test_that("extract_pval_glob.lm works", {
  tab <- standardize_tab(colon)
  mod <- lm(age ~ rx + nodes + extent + sex, data = tab)
  pglob <- broom::tidy(car::Anova(mod))$p.value
  expect_equal(extract_pval_glob(mod), c(pglob[1], NA, NA, pglob[3], NA, NA, NA))
  mod2 <- glm(sex ~ rx + nodes + extent + age, data = tab, family = binomial)
  pglob <- broom::tidy(car::Anova(mod2))$p.value
  expect_equal(extract_pval_glob(mod2), c(pglob[1], NA, NA, pglob[3], NA, NA, NA))
})

test_that("extract_pval_glob.coxph works", {
  tab <- standardize_tab(colon) %>% make_tab_survival("status", var_time = "time")
  mod <- coxph(Surv(.time, status) ~ age + rx + nodes + extent + sex, data = tab)
  pglob <- broom::tidy(car::Anova(mod))$p.value
  expect_equal(extract_pval_glob(mod), c(NA, pglob[2], NA, NA, pglob[4], NA, NA, NA))
})
