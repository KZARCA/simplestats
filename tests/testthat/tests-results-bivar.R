tab <- colon %>% standardize_tab() %>% make_tab_survival("status", var_time = "time")

test_that("create_ligne_bivar.factor_num is working", {
  line <- create_ligne_bivar(tab$differ, tab$age, noms = "age")
  expect_equal(line$id, rep("age", 3))
  expect_equal(line$variable, rep("Differ", 3))
  expect_equal(line$niveau, as.character(seq_len(3)))
  filter(tab, !is.na(differ)) %>%
    group_by(differ) %>%
    summarise(m = sprintf_number_table("%s (±%s)", mean(age), sd(age))) %>%
    extract2("m") %>%
    expect_equal(line[[gettext("mean (sd)", domain = "R-simplestats")]])
  filter(tab, !is.na(differ)) %>%
    group_by(differ) %>%
    summarise(m = sprintf_number_table("%s [%s - %s]",
                                       median(age), quantile(age, 0.25), quantile(age, 0.75))) %>%
    extract2("m") %>%
    expect_equal(line[[gettext("median [Q25-75]", domain = "R-simplestats")]])
  filter(tab, !is.na(differ)) %>%
    group_by(differ) %>%
    summarise(m = sprintf_number_table("%s", min(age))) %>%
    extract2("m") %>%
    expect_equal(line$min)
  filter(tab, !is.na(differ)) %>%
    group_by(differ) %>%
    summarise(m = sprintf_number_table("%s", max(age))) %>%
    extract2("m") %>%
    expect_equal(line$max)
  filter(tab, !is.na(differ)) %>%
    group_by(differ) %>%
    summarise(m = sum(!is.na(age))) %>%
    extract2("m") %>%
    expect_equal(line$n)
  lm(age ~ differ, data=tab) %>%
    car::Anova(type = 3) %>%
    broom::tidy() %>%
    extract2("p.value") %>%
    extract(2) %>%
    c(rep(NA,2)) %>%
    expect_equal(line$p)
  expect_equal(line$test, c("Anova", NA, NA))
})

test_that("create_ligne_bivar.factor_fac is working", {
  tab$node2 <- tab$node4
  tab$node2[1:100] <- NA
  line <- create_ligne_bivar(tab$node2, tab$differ, noms = "node2")
  expect_equal(line$id, rep("node2", 2))
  expect_equal(line$variable, rep("Node4", 2))
  expect_equal(line$niveau, c("0", "1"))

  test_num <- function(x){
    num <- filter(tab, !is.na(node2)) %>%
      group_by(node2) %>%
      filter(differ == x) %>%
      summarise(m = n()) %>%
      extract2("m")
    tot <- filter(tab, !is.na(node2)) %>%
      filter(differ == x) %>%
      summarise(m = n()) %>%
      extract2("m")
    sprintf_number_table("%s (%s)", num, pourcent(num/tot)) %>%
      expect_equal(line[[paste("Differ", x)]])
  }
  test_num("1")
  test_num("2")
  test_num("3")
  expect_equal(line$.n,
               filter(tab, !is.na(node2)) %>%
                 group_by(node2) %>%
                 summarise(m = sum(!is.na(differ))) %>%
                 extract2("m"))
  expect_equal(line$p, c(chisq.test(table(tab$node2, tab$differ))$p.value, NA))
  expect_equal(line$test, c("Chi2", NA))
})

test_that("create_ligne_bivar.num_fac is working", {
  line <- create_ligne_bivar(tab$nodes, tab$differ, noms = "nodes")
  expect_equal(line$id, "nodes")
  expect_equal(line$variable, "Nodes")
  test_m <- function(x){
    filter(tab, differ == x) %>%
      summarise(m = sprintf_number_table("%s (±%s)", mean(nodes, na.rm = TRUE), sd(nodes, na.rm = TRUE))) %>%
      extract2("m") %>%
      expect_equal(line[[paste("Differ", x)]])
  }
  test_m("1")
  test_m("2")
  test_m("3")
  expect_equal(line$.n, nrow(na.exclude(tab[c("nodes", "differ")])))
  expect_equal(line$p, kruskal.test(nodes ~ differ, data = tab)$p.value)
  expect_equal(line$test, "Kruskal-Wallis")
})

test_that("create_ligne_bivar.num_num is working", {
  line <- create_ligne_bivar(tab$nodes, tab$age, noms = "nodes")
  expect_equal(line$id, "nodes")
  expect_equal(line$variable, "Nodes")
  co <- cor.test(tab$nodes, tab$age) %>% broom::tidy()
  expect_equal(line$`Coefficient de corrélation (IC95)`, sprintf_number_table("%s (%s; %s)", co$estimate, co$conf.low, co$conf.high))
  expect_equal(line$p, co$p.value)
})

test_that("create_ligne_surv_bivar is working", {
  line <- create_ligne_surv_bivar(tab$differ, tab$.time, "differ", tab$status)
  expect_equal(line$id, rep("differ", 3))
  expect_equal(line$variable, rep("Differ", 3))
  expect_equal(line$niveau, as.character(seq_len(3)))
  surv <- survfit(Surv(.time, status) ~ differ, data = tab)
  table_surv <- summary(surv) %>%
    extract2("table")
  expect_equal(line[[gettext("median (CI95)", domain = "R-simplestats")]],
               sprintf_number_table("%s (%s; %s)", table_surv[, "median"], table_surv[, "0.95LCL"], table_surv[, "0.95UCL"]))
  expect_equal(line[[gettext("max follow-up", domain = "R-simplestats")]], surv$time[cumsum(surv$strata)])
  expect_equal(line$n, table_surv[, "n.max"] %>% unname())
  expect_equal(line[[gettext("n events", domain = "R-simplestats")]], table_surv[, "events"] %>% unname())
  expect_equal(round(line$p, 10), c(3.39E-7, NA, NA))
  expect_equal(line$test, c("Logrank", NA, NA))
})