context("extract_pval")
library(survival)

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

test_that("extract_pval_glob.mira works", {
  tab <- standardize_tab(colon)
  tabm <- tab[c("sex", "age", "nodes", "differ", "rx")] %>%
    mice(printFlag = FALSE)

  suppressWarnings(mod <- mice:::with.mids(tabm, lm(age ~ sex + nodes + differ + rx)))
  pval1 <- extract_pval_glob(mod)
  expect_equal(pval1[c(1,2,4,6)], rep(NA_integer_, 4))
  expect_gte(pval1[3], map_dbl(mod$analyses, function(x) car::Anova(x) %>% tidy() %>% filter(term == "differ") %>% extract2("p.value")) %>% min())
  expect_lte(pval1[3], map_dbl(mod$analyses, function(x) car::Anova(x) %>% tidy() %>% filter(term == "differ") %>% extract2("p.value")) %>% max())
  expect_gte(pval1[5], map_dbl(mod$analyses, function(x) car::Anova(x) %>% tidy() %>% filter(term == "rx") %>% extract2("p.value")) %>% min())
  expect_lte(pval1[5], map_dbl(mod$analyses, function(x) car::Anova(x) %>% tidy() %>% filter(term == "rx") %>% extract2("p.value")) %>% max())

  suppressWarnings(mod <- mice:::with.mids(tabm, glm(sex ~ age + nodes + differ + rx, family = binomial)))
  pval2 <- extract_pval_glob(mod)
  expect_equal(extract_pval_glob(mod)[c(1,2,4,6)], rep(NA_integer_, 4))
  expect_gte(pval2[3], map_dbl(mod$analyses, function(x) car::Anova(x) %>% tidy() %>% filter(term == "differ") %>% extract2("p.value")) %>% min())
  expect_lte(pval2[3], map_dbl(mod$analyses, function(x) car::Anova(x) %>% tidy() %>% filter(term == "differ") %>% extract2("p.value")) %>% max())
  expect_gte(pval2[5], map_dbl(mod$analyses, function(x) car::Anova(x) %>% tidy() %>% filter(term == "rx") %>% extract2("p.value")) %>% min())
  expect_lte(pval2[5], map_dbl(mod$analyses, function(x) car::Anova(x) %>% tidy() %>% filter(term == "rx") %>% extract2("p.value")) %>% max())

  tabs <- standardize_tab(colon) %>% make_tab_survival("status", var_time = "time")
  tabms <- tabs[c("sex", "age", "nodes", "differ", "rx", ".time", "status")] %>%
    mice(printFlag = FALSE, m = 20)
  suppressWarnings(mod <- mice:::with.mids(tabms, coxph(Surv(.time, status) ~ sex + age + rx + differ, model=TRUE)))
  pval3 <- extract_pval_glob(mod) %>% round(4)
  expect_gte(pval3[3], map_dbl(mod$analyses, function(x) car::Anova(x) %>% tidy() %>% filter(term == "differ") %>% extract2("p.value")) %>% min() %>% round(4))
  expect_lte(pval3[3], map_dbl(mod$analyses, function(x) car::Anova(x) %>% tidy() %>% filter(term == "differ") %>% extract2("p.value")) %>% max() %>% round(4))
  expect_gte(pval3[5], map_dbl(mod$analyses, function(x) car::Anova(x) %>% tidy() %>% filter(term == "rx") %>% extract2("p.value")) %>% min() %>% round(4))
  expect_lte(pval3[5], map_dbl(mod$analyses, function(x) car::Anova(x) %>% tidy() %>% filter(term == "rx") %>% extract2("p.value")) %>% max() %>% round(4))
})
