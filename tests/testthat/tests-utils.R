context("utils")
library(survival)
test_that("get_choix_var works", {
  tab <- standardize_tab(colon)
  expect_equal(unname(get_choix_var(tab)), sort(names(tab)))
  expect_equal(names(get_choix_var(tab)), sort(capitalize(names(tab))))
})

test_that("format_numbers works", {
  expect_equal(format_number(43), "43.0")
  expect_equal(format_number(43, -1), "40")
  expect_equal(format_number(46.5650, 1), "47")
  expect_equal(format_number(1087, -1), "1090")
  expect_equal(format_number(0.01887, 1), "0.02")
  expect_equal(format_number(0.01887, 2), "0.019")
  expect_equal(format_number(0.01887, 3), "0.0189")
  expect_equal(format_number(-0.01887, 2), "-0.019")
})

test_that("pourcent works", {
  expect_equal(pourcent(0.1), "10%")
  expect_equal(pourcent(0.1, symbol = FALSE), "10")
  expect_equal(pourcent(c(0.1, 0.2)), c("10%", "20%"))
  expect_equal(pourcent(c(0.1, 0.2), symbol = FALSE), c("10", "20"))
  expect_equal(pourcent(0.01532), "1.5%")
  expect_equal(pourcent(0.00001532), "0.0015%")
  expect_equal(pourcent(0.000001532), "0%")
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
  expect_equivalent(extract_from_model(suppressWarnings(car::Anova(mod)), "statistic"), broom::tidy(suppressWarnings(car::Anova(mod)))$statistic[seq_len(3)])
  mod2 <- coxph(Surv(.time, status) ~ sex + rx + nodes, data = tab)
  expect_equal(extract_from_model(mod2, "estimate"), coef(mod2))
})

test_that("solve_contrast is working", {
  a <- 1:20
  b <- 41:60
  c <- c(1:3, rep(NA, 17))
  tab <- data.frame(a = a, b = b, c = c)
  expect_true(solve_contrast(tab, "a", b))
  expect_true(solve_contrast(tab, "a", a))
  expect_false(solve_contrast(tab, "b", c))
  expect_true(solve_contrast(tab, "b", c, univ = TRUE))
})

test_that("identical_model_frame is working", {
  tab <- data.frame(a = c(rep("a", 10),
                          rep("b", 10),
                          rep("c",10)),
                    b = c(rep("c", 10),
                            rep("d", 20)), stringsAsFactors = TRUE)
  expect_equal(identical_model_frame(tab, b ~ a, "logistic")[[1]], c("b", "a"))
})


test_that("prepare_model is working", {
  tab <- standardize_tab(colon) %>%
    make_tab_survival("status", var_time = "time") %>%
    filter(extent == 3 | extent == 4)

  mod <- coxph(Surv(.time, status) ~ age + extent, data = tab)
  expect_equal(names(coef(mod)), c("age", "extent2", "extent3", "extent4"))
  modlm <- lm(nodes ~ age + extent, data = tab)
  expect_equal(names(coef(modlm)), c("(Intercept)", "age", "extent4"))
  modglm <- glm(node4 ~ age + extent, family = binomial, data = tab)
  expect_equal(names(coef(modglm)), c("(Intercept)", "age", "extent4"))
  mod <- coxph(Surv(.time, status) ~ age + extent, data = prepare_model(tab))
  expect_equal(names(coef(mod)), c("age", "extent4"))
  tab2 <- filter(tab, extent == 3)
  expect_true(!is.null(tab2$extent))
  tab2 <- prepare_model(tab2, remove = TRUE)
  expect_null(tab2$extent)

  tab$age2 <- 55
  expect_true(!is.null(tab$age2))
  tab3 <- prepare_model(tab, remove = TRUE)
  expect_null(tab3$age2)
})

test_that("is_entier is working", {
  expect_true(
    is_entier(
      round(runif(100, 1, 8),0)
    ))
  expect_true(
    is_entier(
      round(runif(100, 1, 2),0)
    ))
})

