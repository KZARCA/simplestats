context("bootstrap")
library(survival)
tab <- colon %>% standardize_tab() %>% make_tab_survival("status", var_time = "time")

boot_tests <- function(tab, vardep, varindep, varajust){
  set.seed(1)
  tab <- tab[c(vardep, varindep, varajust)]
  R <- 200
  mod <- sprintf("%s ~ %s", vardep, paste(c(varindep, varajust), collapse = "+")) %>%
    as.formula() %>%
    lm(data = tab)
  tab <- model.frame(mod)
  names(tab)[1] <- ".vardep"
  resBoot <- get_resBoot(tab, R, mod, nCPU = 4, updateProgress = function(detail){}, varajust = varajust)
  resBoot_p <- get_resBoot_p(tab, R, mod, nCPU = 4, updateProgress = function(detail){}, varajust = varajust)
  confint_p <- get_confint_p_boot(resBoot, resBoot_p)$tableRet
  if (length(select_if(tab, function(x) is.factor(x) && nlevels(x) > 2)) > 0){
    resBoot_anova <- get_resBoot_anova(tab, R, mod, nCPU = 4, updateProgress = function(detail){}, varajust = varajust)
  } else resBoot_anova <- NULL
  test_that("get_resBoot works", {
    expect_is(resBoot, "boot")
    expect_equal(dim(resBoot$t), c(R, get_nvar_mod(tab, varajust)))
    expect_equal(resBoot$t0, coef(mod)[seq.int(2, get_nvar_mod(tab, varajust) + 1)])
  })
  test_that("get_resBoot_p works", {
    expect_is(resBoot_p, "boot")
    expect_equal(dim(resBoot_p$t)[2], get_nvar_mod(tab, varajust))
    expect_equivalent(resBoot_p$t0, broom::tidy(mod)$statistic[seq.int(2, get_nvar_mod(tab, varajust) + 1)])
  })
  if (!is.null(resBoot_anova)){
    test_that("get_resBoot_anova works", {
      expect_is(resBoot_anova, "boot")
      expect_equal(dim(resBoot_anova$t)[2], length(varindep))
      expect_equivalent(resBoot_anova$t0, broom::tidy(suppressWarnings(car::Anova(mod)))$statistic[seq_len(length(varindep))])
    })
  }
  test_that("get_confint_p_boot works", {
    expect_true(all(confint_p$estimate > confint_p$conf.low & confint_p$estimate < confint_p$conf.high))
    expect_true(all(confint_p$p.value >= 0 & confint_p$p.value <= 1))
  })
  test_that("add_varname.boot works", {
    added <<- add_varname(confint_p, resBoot)
    nrep <- map_dbl(varindep, function(x){
      if(is.factor(tab[[x]])) nlevels(tab[[x]]) - 1 else 1
    })
    niveau <- map(varindep, function(x){
      tabx <- tab[[x]]
      if (is.factor(tabx)) {
        paste(levels(tabx)[seq.int(2, nlevels(tabx))], "vs", levels(tabx)[1], sep = " ")
      } else {
        ""
      }
    }) %>% list_c()
    multiple <- map(varindep, function(x){
      tabx <- tab[[x]]
      if(is.numeric(tabx)) 1 else rep(NA, nlevels(tabx) - 1)
    }) %>% list_c
    term <- map(varindep, function(x){
      tabx <- tab[[x]]
      if (is.factor(tabx)) paste0(x, levels(tabx)[seq.int(2, nlevels(tabx))]) else x
    }) %>% list_c()
    expect_equal(added$id, map2(varindep, nrep, function(x, y) rep(x, y)) %>% list_c())
    expect_equal(added$variable, map2(varindep, nrep, function(x, y) rep(label(tab[[x]]), y)) %>% list_c())
    expect_equal(added$niveau, niveau)
    expect_true(all(is.na(added$multiple) == is.na(multiple)))
    expect_equal(added$term, term)
  })
  if (!is.null(resBoot_anova)){
    test_that("extract_pval_glob.boot works", {
      pglob <- extract_pval_glob(resBoot_anova)
      expect_length(pglob, nrow(added))
      ano <- map(varindep, function(x){
        tabx <- tab[[x]]
        if(is.factor(tabx) && nlevels(tabx) > 2){
          c(1, rep(NA, nlevels(tabx) - 2))
        } else {
          NA
        }
      }) %>% list_c()
      expect_true(all(is.na(ano) == is.na(pglob)))
    })

  }
}

boot_tests(tab, "age", "sex", NULL)
boot_tests(tab, "age", c("sex"), "rx")
boot_tests(tab, "age", c("sex", "nodes", "rx"), NULL)
boot_tests(tab, "age", c("sex", "nodes", "rx"), "obstruct")
boot_tests(tab, "age", c("sex", "nodes", "rx"), c("obstruct", "differ"))
boot_tests(tab, "nodes", c("rx", "age", "sex"), c("obstruct", "differ"))


test_that("print_all_boots works", {
  mod <- lm(age ~ nodes + sex, data = tab)
  booted <- print_all_boots(mod, nCPU = 3)
  expect_equal(booted$term, c("nodes", "sex1"))
})


test_that("get_all_boots is working", {
  set.seed(1)
  varajust <- NULL
  mod <- compute_mod(tab, "age", c("sex", "nodes", "rx"), varajust, "linear")$mod
  all_boots <- get_all_boots(mod, varajust, 4, 200)
  expect_equal(names(all_boots), c("resBoot", "resBoot_anova", "tab_ci"))
  expect_type(all_boots$resBoot_anova, "list")

  varajust <- "rx"
  mod <- compute_mod(tab, "age", c("sex", "nodes"), varajust, "linear")$mod
  all_boots <- get_all_boots(mod, varajust, 4, 200)
  expect_equal(names(all_boots), c("resBoot", "resBoot_anova", "tab_ci"))
  expect_null(all_boots$resBoot_anova)
})
