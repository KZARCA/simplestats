context("results-multiv")


test_that("format_precision works", {
  tab <- data.frame(
    a = seq_len(100),
    b = seq.int(1, 1000, by = 10),
    c = seq.int(.1, 10, by = .1),
    d = seq.int(.1, 10000, by = 100)
  )
  fb <- format_precision(tab, c("a", "b", "c", "d"))
  expect_equal(fb, c("a", "I(b/10)", "I(c/0.1)", "I(d/100)"))
})

test_that("get_mod works when everything works correctly", {
  tab <- standardize_tab(colon)
  tabm <- tab[c("node4", "rx", "differ", "obstruct", "age")] %>% mice::mice(printFlag = FALSE)
  formule <- "node4 ~ rx + differ + obstruct + age"
  .fun <- get_fun("logistic")
  mod <- get_mod(tab, .fun, formule)
  expect_s3_class(mod, "glm")
  expect_equal(mod$call, quote(glm(formula = node4 ~ rx + differ + obstruct + age, family = "binomial")))
  modm <- get_mod(tabm, .fun, formule)
  expect_s3_class(modm, "mira")
  expect_equal(modm$call, quote(mice:::with.mids(data = tab, expr = glm(node4 ~ rx + differ + obstruct + age, family = "binomial"))))

  formule <- "age ~ rx + differ + obstruct + node4"
  expect_error(get_mod(tab, .fun))
  .fun <- get_fun("linear")
  mod <- get_mod(tab, .fun, formule)
  expect_s3_class(mod, "lm")
  expect_equal(mod$call, quote(lm(formula = age ~ rx + differ + obstruct + node4)))
  modm <- get_mod(tabm, .fun, formule)
  expect_s3_class(modm, "mira")
  expect_equal(modm$call, quote(mice:::with.mids(data = tab, expr = lm(age ~ rx + differ + obstruct + node4))))

  tabs <- standardize_tab(colon) %>% make_tab_survival("node4", 2, var_time = "time")
  tabsm <- tabs[c("node4", "rx", "differ", "obstruct", "age", ".time")] %>% mice::mice(printFlag = FALSE)
  formule <- "Surv(.time, node4) ~ rx + differ + obstruct + age"
  .fun <- get_fun("survival")
  mod <- get_mod(tabs, .fun, formule)
  expect_s3_class(mod, "coxph")
  expect_equal(mod$call, quote(coxph(formula = Surv(.time, node4) ~ rx + differ + obstruct + age, model = TRUE)))
  modm <- get_mod(tabsm, .fun, formule)
  expect_s3_class(modm, "mira")
  expect_equal(modm$call, quote(mice:::with.mids(data = tab, expr = coxph(Surv(.time, node4) ~ rx + differ + obstruct + age, model = TRUE))))
})

test_that("modify_mod works", {
  tab <- standardize_tab(colon)
  vardep <- "node4"
  varindep <- c("nodes", "rx", "differ", "obstruct")
  type <- "logistic"
  formule <- sprintf("%s ~ %s", vardep, paste(varindep, collapse = " + "))
  .fun <- get_fun(type)
  mod <- get_mod(tab, .fun, formule)
  expect_equal(coefficients(mod), coefficients(modify_mod(mod, tab, pred = 0)))
  mod$coefficients <- mod$coefficients - 0.2 # coefficient très différent --> erreur
  expect_equal(attr(modify_mod(mod, tab, pred = 0), 'warning'), "glm.fit: fitted probabilities numerically 0 or 1 occurred")
  # avec données manquantes
  tabm <- tab[, c(vardep, varindep)] %>% mice::mice(printFlag = FALSE)
  mod <- get_mod(tabm, .fun, formule)
  expect_equal(map(mice::getfit(mod), coefficients), map(mice::getfit(modify_mod(mod, tabm, pred = 0)), coefficients))
  mod$analyses[[3]]$coefficients <- mod$analyses[[3]]$coefficients - 0.2 # au moins 1 coefficient très différent --> 1 seule erreur
  expect_equal(attr(modify_mod(mod, tabm, pred = 0), 'warning'), "glm.fit: fitted probabilities numerically 0 or 1 occurred")
})

test_that("get_fun works", {
  tab <- colon %>% standardize_tab()
  get_fun( "linear") %>%
    expect_equal(list(fun = "lm", args_sup = NULL))
  get_fun("logistic") %>%
    expect_equal(list(fun = "glm", args_sup = list(family = "binomial")))
  get_fun("survival") %>%
    expect_equal(list(fun = "coxph", args_sup = list(model = TRUE)))
})
