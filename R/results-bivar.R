#' Univariate analysis
#'
#' @param x The independant variable
#' @param y A vector to describe
#' @param noms The variable names
#' @param margin Index, or vector of indices to generate margin for.
#'    1 indicates rows, 2 indicates columns
#'
#' @return If x is a numeric vector: mean(sd), median(Q25-75), min, max, n, p.
#'     If x is a factor: n (%)
#' @export
#'
#' @examples
create_ligne_bivar <- function(x, ...){
  UseMethod("create_ligne_bivar")
}

#' @export
#' @rdname create_ligne_bivar
create_ligne_bivar.factor <- function(x, y, noms, margin = 2, ...){
  if (is.factor(y)){ #fac~fac
    no_na <- remove_na(x, y)
    if (nrow(no_na) > 0){
      x <- no_na$x
      y <- no_na$y
      cont <- table(x, y)
      prop <- prop.table(cont, margin) %>% map_chr(pourcent)
      d <- map2_chr(cont, prop, function(x, y) {
        sprintf_number_table("%s (%s)", x, y)
      }) %>%
        matrix(ncol = nlevels(y)) %>% as_tibble
      colnames(d) <- sprintf("%s %s", label(y), levels(y))#column_names
      d$.n <- base::rowSums(cont)
      d %<>% add_varname(x, noms)
      pval_test <- extract_pval(x,y) %>%
        map_df(~ c(., rep(NA, nlevels(x) - 1)))
      names(pval_test) <- c("p", "test")
      ligne <- bind_cols(d, pval_test)
      attr(ligne, "colSums") <- base::colSums(cont)
      ligne
    }
  } else if (is.numeric(y)){ #fac~num
    no_na <- remove_na(x, y, drop_factor = TRUE)
    if (nrow(no_na) > 0){
      x <- no_na$x
      y <- no_na$y
      d <- no_na %>%
        group_by(x) %>%
        summarise(moyenne = sprintf_number_table("%s (±%s)", base::mean(y, na.rm=TRUE), sd(y, na.rm=TRUE)),
                  mediane = sprintf_number_table("%s [%s - %s]", median(y, na.rm=TRUE), quantile(y, na.rm=TRUE)[2], quantile(y, na.rm=TRUE)[4]),
                  min = sprintf_number_table("%s", min(y, na.rm=TRUE)),
                  max = sprintf_number_table("%s", max(y, na.rm=TRUE)),
                  n = n()
        )
      names(d) <- c("niveau", gettext("mean (sd)", domain = "R-simplestats"),
                    gettext("median [Q25-75]", domain = "R-simplestats"),
                    gettext("min", domain = "R-simplestats"),
                    gettext("max", domain = "R-simplestats"), "n")
      pval_test <- extract_pval(x,y) %>%
        map_df(~ c(., rep(NA, max(0, nlevels(x) - 1))))
      names(pval_test) <- c("p", "test")
      ligne <-
        bind_cols(d, pval_test) %>%
        add_varname(x, noms, add_niveau = FALSE)
      names(ligne)[3] <- "niveau"
      ligne$niveau <- as.character(ligne$niveau)
      ligne
    }
  }
}

#' @export
#' @rdname create_ligne_bivar
create_ligne_bivar.numeric <- function(x, y, noms, ...){ #num~fac
  if(is.factor(y)){
    no_na <- remove_na(x, y)
    cont <- table(no_na$y)
    if (nrow(no_na) > 0){
      x <- no_na$x
      y <- no_na$y
      d <- no_na %>%
        group_by(y) %>%
        summarise(moyenne = sprintf_number_table("%s (±%s)", base::mean(x, na.rm=TRUE), sd(x, na.rm=TRUE))) %>%
        base::t() %>%
        as_tibble
      colnames(d) <- paste(label(y), d[1,])#column_names
      d <- d[2, ]
      d$.n <- sum(cont)
      pval_test <- extract_pval(x,y) %>%
        as_tibble
      names(pval_test) <- c("p", "test")

      ligne <- bind_cols(d, pval_test) %>%
        add_varname(x, noms)
      #add_column(label(x), .before = 1)
      attr(ligne, "colSums") <- table(fct_drop(no_na$y))
      #names(ligne)[1] <- "variable"
      ligne
    }
  } else {
    no_na <- remove_na(x, y, drop_factor = TRUE)
    res <- broom::tidy(cor.test(no_na$x, no_na$y))
    ligne <- tibble("Coefficient de corrélation (IC95)" = sprintf_number_table("%s (%s; %s)",
                                                                               res$estimate, res$conf.low, res$conf.high),
                    n = nrow(no_na),
                    p = res$p.value,
                    test = "Pearson") %>%
      add_varname(x, noms)
  }
}


#' Univariate analysis of survival
#'
#' @param x The independant variable
#' @param time The vector of follow-up time
#' @param censure The status indicator
#'
#' @return median(IC95), max follow-up, n, n events, p.
#' @export
#'
#' @examples
create_ligne_surv_bivar <- function(x, time, noms, censure){
  tab_cens <- remove_na(time, x, censure)
  if (nrow(tab_cens) > 0){
    names(tab_cens) <- c(".time", "x", "censure")
    tab_cens$censure %<>% as.character %>% as.numeric

    formule <- Surv(.time, censure) ~ x
    surv <- survfit(formule, data = tab_cens)
    resume <- base::summary(surv)$table
    max_time <- map_dbl(seq_along(surv$strata), ~ max(surv[.]$time))
    median <- map_chr(seq_along(surv$strata), function(i){
      sprintf_number_table("%s (%s; %s)",
                           resume[i, "median"],
                           resume[i, "0.95LCL"],
                           resume[i, "0.95UCL"])
    })
    n <-  resume[, "n.start", drop = TRUE]
    nEvent <- resume[, "events", drop = TRUE]

    d <- tibble(
      median,
      max_time,
      n,
      nEvent
    )
    names(d) <- c(gettext("median (CI95)", domain = "R-simplestats"),
                  gettext("max follow-up", domain = "R-simplestats"), "n", gettext("n events", domain = "R-simplestats"))

    pval_test <- survdiff(formule, data = tab_cens) %>%
      extract_pval %>%
      map_df(~ c(., rep(NA, max(0, nlevels(x) - 1))))
    names(pval_test) <- c("p", "test")

    ligne <- bind_cols(d, pval_test)
    ligne %>% add_varname(x, noms)
  }
}

#' Displays the univariate analysis in markdown
#'
#' @param x The independant variable
#' @param y The variable to describe or the follow-up time for survival analysis
#' @param censure The status indicator
#' @param type A character string of the type of modeling, having a value among "linear", "logistic" or "survival"
#'
#' @return A tibble displayed in markdown
#' @export
#'
#' @examples
print_line_bivar <- function(x, y, varname = label(x), censure = NULL, type = "linear"){
  shown <- if (type == "survival"){
    create_ligne_surv_bivar(x = x, time = y, noms = varname, censure = censure)
  } else {
    create_ligne_bivar(x, y, noms = varname)
  }
  show_table_markdown(shown)
}
