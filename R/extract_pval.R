#' Performs univariate tests and extract p-value from objects
#'
#' @param x Object of class survdiff, or a numerical or factor vector
#' @param ... If x is a vector, this argument must be a vector of the same length
#'
#' @return
#' @export
#'
#' @examples
extract_pval <- function(x, ...){
  UseMethod("extract_pval")
}

#' @export
#' @rdname extract_pval
extract_pval.survdiff <- function(x, ...){
  if (is.matrix(x$obs)) {
    otmp <- apply(x$obs, 1, sum)
    etmp <- apply(x$exp, 1, sum)
  }
  else {
    otmp <- x$obs
    etmp <- x$exp
  }
  df <- (sum(1 * (etmp > 0))) - 1
  pval <- magrittr::subtract(1, pchisq(x$chisq, df)) %>%
    purrr::as_vector("double")
  return(list(pval = pval, test = "Logrank"))
}

#' @export
#' @rdname extract_pval
extract_pval.default <- function(x, y, ...){
  if (is.factor(x) & is.numeric(y) | is.numeric(x) & is.factor(y)){
    if (is.factor(y)){
      tmp <- y
      y <- x
      x <- tmp
    }
    formule <- as.formula(y ~ x)
    mod <- try(lm(formule), silent = TRUE) # le try c'est parce que parfois, il existe une classe de x avec n = 0
    compte <- margin.table(table(x, y), 1)
    f <- NULL
    try({
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
        if (all(compte > 30, na.rm = TRUE) && is_homoscedatic(mod)) {
          f <- anova(mod)
          test <- "Anova"
        }
        else {
          f <- kruskal.test(formule)
          test <- "Kruskal-Wallis"
        }
      }
    }, silent = TRUE)
  }
  else if (is.factor(x) & is.factor(y)){
    cont <- table(x, y)
    suppressWarnings(f <- chisq.test(cont))
    if (any(purrr::as_vector(f$expected, "double") < 5)){
      f <- NULL
      try({
        f <- fisher.test(cont)
        test <- "Fisher"
      }, silent = TRUE)
      if (is.null(f)){
        set.seed(1)
        f <- fisher.test(cont, simulate.p.value = TRUE, B = 100000)
        test <- "Fisher"
      }
    } else test <- "Chi2"
  }
  if(!is.null(f)){
    pval <- f %>%
      broom::tidy() %>%
      magrittr::extract("p.value") %>%
      dplyr::slice(1) %>%
      purrr::as_vector("double") %>%
      round(10)
    return(list(pval = pval, test = test))
  } else {
    return(list(pval = NA, test = "-"))
  }

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
extract_pval_glob.lm <- function(x, ...){
  df <- clean_anova(x)
  extract_pval_glob.default(df, ...)
}

#' @export
#' @rdname extract_pval_glob
extract_pval_glob.coxph <- extract_pval_glob.lm

#' @export
#' @rdname extract_pval_glob
extract_pval_glob.boot <- function(x, ...){
  R <- x$R
  tab_anova_base <- x$tab_anova_base
  tab_to_extract <- map_dbl(seq_along(x$t0), function(i){
    base::mean(x$t[, i, drop = TRUE] >= x$t0[i], na.rm = TRUE)
  }) %>% tibble::tibble(df = tab_anova_base$df, p.value = .)
  extract_pval_glob.default(tab_to_extract, ...)
}

#' @export
#' @rdname extract_pval_glob
extract_pval_glob.default <- function(x, ...){
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

clean_anova <- function(mod){
  car::Anova(mod, type = 3) %>%
    broom::tidy() %>%
    dplyr::rename(variable = term) %>%
    dplyr::filter(variable != "(Intercept)" & variable != "Residuals" & variable != "NULL")
}
