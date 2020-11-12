context("prepare_tab")
library(survival)
test_that("make_tab_survival works with var_time, passage 1 & 2", {
  tab <- colon
  tab$status <- tab$status + 1
  tab %<>% standardize_tab()
  tab_modif <- make_tab_survival(tab, "status", var_time = "time")
  tab_modif2 <-  make_tab_survival(tab, "status", passage = 2, var_time = "time")
  tests <- function(tab_modif, passage = 1){
    expect_null(tab_modif$time)
    expect_equal(tab_modif$.time, tab$time)
    expect_type(tab_modif$status, "double")
    expect_true(all(tab_modif$status == 1 | tab_modif$status == 0))
    if (passage == 1) {
      expect_equal(as.numeric(tab_modif$status), colon$status)
    } else {
      expect_equal(as.numeric(tab_modif$status), 1-colon$status)
    }
  }
  tests(tab_modif)
  tests(tab_modif2, 2)
})
test_that("make_tab_survival works with limit", {
  tab <- standardize_tab(colon)
  lim0 <- as.numeric(make_tab_survival(tab, "status", var_time = "time", limit = 0)$status)
  lim10k <- make_tab_survival(tab, "status", var_time = "time", limit = 10000)$status
  expect_equal(lim0, rep(0, nrow(tab)))
  expect_equal(lim10k, make_tab_survival(tab, "status", var_time = "time")$status)
})

test_that("make_tab_survival works with dateSortie & Inclusion", {
  tab <- colon
  tab$inclusion <- as.Date(runif(nrow(tab), 1, 5000), "1990-01-01")
  tab$sortie <- as.Date(tab$inclusion + tab$time, "1990-01-01")
  tab %<>% standardize_tab()
  tab_modif <- make_tab_survival(tab, "status", typeCensure = 1, dateInclusion = tab$inclusion, dateSortie = tab$sortie)
  expect_equal(tab_modif$.time, as.numeric(tab$sortie - tab$inclusion))
})

test_that("create_tabi works", {
  expect_equal(create_tabi(colon, "desc"), select(colon, -study))
  tab <- colon
  tab$test <- rep_len(letters, nrow(colon))
  expect_equal(create_tabi(tab, "desc"),  select(colon, -study))
  tab$test %<>% as.factor()
  expect_equivalent(create_tabi(tab, "desc"),  select(colon, -study) %>% mutate(test = tab$test))
  tab %<>% select(-test)
  tab$dm <- c(rep(NA, 1000), seq_len(nrow(tab)-1000))
  expect_equivalent(create_tabi(tab, "desc"),  select(colon, -study) %>% mutate(dm = tab$dm))
  expect_equal(create_tabi(tab, "expl"),  select(colon, -study))
})

test_that("create tabi with pred keeps less than 20% of missing", {
  tab <- standardize_tab(mice::fdd) %>%
    create_tabi(type = "pred")
  expect_lt(get_propDM(tab), 0.2)
})

test_that("standardize_names works", {
  noms <- c("L histoire", "voici l histoire", "D avant", "salut d avant", "C est", "bonjour, c est",
             "  \"hello\"   ", "salut  toi\nça  va?", "jeu.du.sort..", "__bonjour_toi")
  noms2 <- c("L'histoire", "Voici l'histoire", "D'avant", "Salut d'avant", "C'est", "Bonjour, c'est",
             "Hello", "Salut toi ça va?", "Jeu du sort", "Bonjour toi")
  expect_equal(standardize_names(noms), noms2)
})

test_that("remove_na_rows works", {
  tab <- colon %>% as_tibble()
  tab <- rbind(rep(NA, ncol(tab)), tab, rep(NA, ncol(tab)))
  expect_equal(remove_na_rows(tab), colon %>% as_tibble)
})

test_that("remove_na_cols works", {
  tab <- colon
  tab <- cbind(rep(NA, nrow(tab)), tab, rep(NA, nrow(tab)))
  expect_equal(remove_na_cols(tab), colon)
})


test_that("replace_virgules works", {
  tab <- data.frame(
    a = c("12,3", NA, "8,8", "5,", "0,33"),
    b = c("Bonjour, toi", "3.5", NA, "5,", "0,33")
  )
  tab2 <- data.frame(
    a = c(12.3, NA, 8.8, 5, 0.33),
    b = c("Bonjour, toi", "3.5", NA, "5,", "0,33")
  )
  expect_equal(replace_virgules(tab), tab2)
})


test_that("transform_date works", {
  tab <- data.frame(
    a = c("20/09/1983", "13/12/2014", "10/09/2009"),
    b = c("20/09/83", "13/12/14", "10/09/09"),
    c = c("20-09-1983", "13-12-2014", "10-09-2009"),
    d = c("20-09-83", "13-12-14", "10-09-09"),
    e = c("1983-09-20", "2014-12-13", "2009-09-10")
  )
  tab2 <- data.frame(transform_date(tab))
  expect_equal(tab2$a, tab2$e)
  expect_equal(tab2$b, tab2$e)
  expect_equal(tab2$c, tab2$e)
  expect_equal(tab2$d, tab2$e)
  expect_is(tab2$e, "Date")
})

test_that("factor_strings works", {
  tab <- tibble(
    a = 1:11,
    b = letters[1:11],
    c = c(rep("fac1", 3), rep("fac2", 4), rep("fac3", 4)),
    d = as.factor(rep(1, 11)),
    e = c(rep("fac1", 6), rep("fac2", 5)),
  )
  tab2 <- factor_strings(tab)
  expect_is(tab2$a, "integer")
  expect_is(tab2$b, "character")
  expect_is(tab2$c, "factor")
  expect_is(tab2$d, "character")
  expect_is(tab2$e, "factor")

})

test_that("remove_guillemets works", {
  tab <- data.frame(
    a = c('"bonjour"', "'Comment vas-tu'", '"Mon ami"'),
    b = c('"1"', '"2"', '"3"'),
    stringsAsFactors = TRUE
  )
  tab2 <- remove_guillemets(tab)
  expect_equal(as.character(tab2$a), c("bonjour", "Comment vas-tu", "Mon ami"))
  expect_equal(tab2$b, c(1, 2, 3))
})



