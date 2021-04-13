find_test_ba <- function(x, y){
  f <- NULL
  if (is.numeric(x)){
    n <- nrow(na.exclude(data.frame(x,y)))
    if (n > 30) {
      f <- t.test(x, y, paired = TRUE)
      test <- "Paired Welch"
    } else if (n > 3) {
      f <- suppressWarnings(wilcox.test(x, y, paired = TRUE))
      test <- "Paired Mann-Whitney"
    }
  } else {
    x <- forcats::fct_expand(x, levels(y))
    y <- forcats::fct_expand(y, levels(x))
    y <- factor(y, levels = levels(x))
    ae <- x == y
    nb_dis <- sum(ae, na.rm = TRUE)
    if (nb_dis > 10 | nlevels(x) > 2) {
      f <- mcnemar.test(x, y)
      test <- ifelse(nlevels(x) > 2, "McNemar-Bowker", "McNemar")
    } else {
      f <- structure(mcnemar.exact(x, y), class = "htest")
      test <- "McNemar"
    }
  }
  if (!is.null(f)){
    return(list(result = f, name = test))
  } else {
    return(NULL)
  }
}

find_test <- function(x, y, survival = FALSE, censure = NULL){
  f <- NULL
  if (survival){
    f <- survdiff(Surv(y, censure) ~ x)
    test <- "Logrank"
  } else if (is.numeric(x) & is.numeric(y)){
    if (length(x) > 30 && is_homoscedatic(lm(y ~ x))){
      f <- try2(cor.test(x, y))
      test <- "Pearson"
    } else {
      f <- try2(cor.test(x, y, method = "spearman", exact = FALSE),
                warnings = c(gettext("NaNs produced")))
      test <- "Spearman"
    }
    if (is_error(f)) f <- NULL
  } else if (is.factor(x) & is.numeric(y) | is.numeric(x) & is.factor(y)){
    if (is.factor(y)){
      tmp <- y
      y <- x
      x <- tmp
    }
    formule <- as.formula(y ~ x)
    compte <- margin.table(table(x, y), 1)
    tryit <- try2({
      if (nlevels(x) == 2) {
        if (all(compte > 30, na.rm = TRUE)) {
          f <- t.test(formule)
          test <- "Welch"
        }
        else {
          f <- suppressWarnings(wilcox.test(formule))
          test <- "Mann-Whitney"
        }
      } else {
        mod <- lm(formule)
        if (all(compte > 30, na.rm = TRUE) && is_homoscedatic(mod)) {
          f <- anova(mod)
          test <- "Anova"
        }
        else {
          f <- kruskal.test(formule)
          test <- "Kruskal-Wallis"
        }
      }
    }, errors = "grouping factor must have exactly 2 levels")
  } else if (is.factor(x) & is.factor(y)){
    cont <- table(x, y)
    suppressWarnings(f <- chisq.test(cont, correct = FALSE))
    if (any(purrr::as_vector(f$expected, "double") < 5)){
      f <- NULL
      if(identical(dim(cont), c(2,2))){
        f <- fisher.exact(cont)
      } else {
        set.seed(1234567)
          f <- try2({
            fisher.test(cont, simulate.p.value = TRUE, B = 100000)
          }, "need 2 or more non-zero column marginals")
      }
      test <- "Fisher"
    } else test <- "Chi2"
  }
  if (length(f) > 0){
    return(list(result = f, name = test))
  } else {
    return(NULL)
  }
}

#' Performs univariate tests and extract p-value from objects
#'
#' @param x The dependant variable
#' @param y Either the independant variable if survival = FALSE or follow-up time
#' @param survival logical, is it a survival analysis
#' @param censure  The censor variable
#'
#' @return
#' @export
extract_pval <- function(x, y, survival = FALSE, censure = NULL, ba = FALSE){
  test <- if (ba){
    find_test_ba(x, y)
  } else {
    find_test(x, y, survival, censure)
  }
  if (is.null(test$result)) return(list(pval = NA, test = "-"))
  if (test$name == "Logrank"){
    pval <- broom::glance(test$result) %>%
      extract2("p.value")
    return(list(pval = pval, test = test$name))
  }
  pval <- test$result %>%
    broom::tidy() %>%
    magrittr::extract("p.value") %>%
    dplyr::slice(1) %>%
    purrr::as_vector("double") %>%
    round(10) %>%
    unname()
  return(list(pval = pval, test = test$name))
}

#' Extract Anova p-value from objects
#'
#' @param x The object to get p-value from, can be an object of class lm, glm, coxph, or boot
#' @param ... other arguments passed to the generic function
#'
#' @return
#' @export
#'
#' @examples
extract_pval_glob <- function(x, ...){
  UseMethod("extract_pval_glob")
}

#' @export
#' @rdname extract_pval_glob
extract_pval_glob.default <- function(mod, ...){
  tbl <- clean_anova(mod)
  put_pval_glob(tbl, ...)
}

#' @export
#' @rdname extract_pval_glob
extract_pval_glob.boot <- function(x, ...){
  R <- x$R
  tab_anova_base <- x$tab_anova_base
  tbl <- map_dbl(seq_along(x$t0), function(i){
    base::mean(x$t[, i, drop = TRUE] >= x$t0[i], na.rm = TRUE)
  }) %>% tibble::tibble(df = tab_anova_base$df, p.value = .)
  put_pval_glob(tbl, ...)
}

put_pval_glob <- function(x, ...){
  dots <- list(...)
  en_test <- dots$en_test %||% FALSE
  show_df1 <- dots$show_df1 %||% FALSE
  map2(x$df, x$p.value, function(n, m){
    if (n == 1) {
      if (en_test | show_df1) m
      else NA
    } else c(m, rep(NA, max(0, n - 1)))
  }) %>%
    purrr::flatten_dbl()
}

clean_anova <- function(x, ...){
  UseMethod("clean_anova")
}


clean_anova.mira <- function(mod){
  if(length(getfit(mod)) == 1) return(clean_anova(getfit(mod, 1)))

  find_pval <- function(x){
    x$result[4]
  }

  env <- find_env(deparse(mod$call[[2]]))

  N <- nrow(getfit(mod, 1)$model)
  terms <- base::labels(terms(getfit(mod, 1)))
  xlevels <- getfit(mod, 1)$xlevels
  purrr::map_dfr(terms, function(x){
    p.value <- if (length(xlevels[[x]]) <= 2) {
      NA
    } else {
      mod2 <- mod
      mod2$call$expr[[2]] <- update.formula(mod2$call$expr[[2]], paste0(". ~ . -", x)) %>%
        format() %>%
        parse(text = .) %>%
        extract2(1)
      suppressWarnings({
        if (length(getfit(mod)) >= 20) {
          suppressWarnings(D2(mod, eval(mod2$call, envir = env), use = "likelihood") %>% find_pval())
        } else {
          suppressWarnings(D1(mod, eval(mod2$call, envir = env)) %>% find_pval())
        }
      })
    }
    tibble(variable = x, df = ifelse(length(xlevels[[x]]), length(xlevels[[x]]) - 1, 1), p.value = p.value)
  })
}

clean_anova.default <- function(mod){
  car::Anova(mod) %>%
    broom::tidy() %>%
    dplyr::rename(variable = term) %>%
    dplyr::filter(variable != "(Intercept)" & variable != "Residuals" & variable != "NULL")
}
