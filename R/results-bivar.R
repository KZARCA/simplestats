#' Univariate analysis
#'
#' @param x The independant variable
#' @param y A vector to describe
#' @param noms The variable names
#' @param margin Index, or vector of indices to generate margin for.
#'    1 indicates rows, 2 indicates columns
#' @param compute_p logical: should the function compute the pvalue
#'
#' @return If x is a numeric vector: mean(sd), median(Q25-75), min, max, n, p.
#'     If x is a factor: n (%)
#' @export
#'
#' @examples
create_line_bivar <- function(x, ...){
  UseMethod("create_line_bivar")
}

#' @export
#' @rdname create_line_bivar
create_line_bivar.factor <- function(x, y, noms, margin = 2, .drop = TRUE, compute_p = TRUE, ...){
  if(missing(noms)) noms <- tolower(make.names(label(x)))
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
        matrix(ncol = nlevels(y), dimnames = list(NULL, sprintf("%s %s", label(y), levels(y)))) %>% as_tibble()
      #colnames(d) <- sprintf("%s %s", label(y), levels(y))#column_names
      d$.n <- base::rowSums(cont)
      d %<>% add_varname(x, noms)
      ligne <- if (compute_p){
        pval_test <- extract_pval(x,y) %>%
          map_df(~ c(., rep(NA, nlevels(x) - 1)))
        names(pval_test) <- c("p", "test")
        bind_cols(d, pval_test)
      } else d
      attr(ligne, "colSums") <- base::colSums(cont)
      attr(ligne, "type") <- "fac-fac"
      ligne
    }
  } else if (is.numeric(y)){ #fac~num
    no_na <- remove_na(x, y, drop_factor = TRUE)
    if (nrow(no_na) > 0){
      x <- no_na$x
      y <- no_na$y
      d <- no_na %>%
        group_by(x, .drop = .drop) %>%
        summarise(moyenne = sprintf_number_table("%s (%s)", base::mean(y, na.rm=TRUE), sd(y, na.rm=TRUE)),
                  mediane = sprintf_number_table("%s [%s - %s]", median(y, na.rm=TRUE), quantile(y, na.rm=TRUE)[2], quantile(y, na.rm=TRUE)[4]),
                  min = sprintf_number_table("%s", min(y, na.rm=TRUE)),
                  max = sprintf_number_table("%s", max(y, na.rm=TRUE)),
                  n = n()
        )
      names(d) <- c("niveau", gettext("mean (sd)", domain = "R-simplestats"),
                    gettext("median [Q25-75]", domain = "R-simplestats"),
                    gettext("min", domain = "R-simplestats"),
                    gettext("max", domain = "R-simplestats"), "n")
      ligne <- if (compute_p){
        pval_test <- extract_pval(x,y) %>%
          map_df(~ c(., rep(NA, max(0, nlevels(x) - 1))))
        names(pval_test) <- c("p", "test")
        bind_cols(d, pval_test)
      } else {
         d
      }
      ligne %<>% add_varname(x, noms, add_niveau = FALSE)
      names(ligne)[3] <- "niveau"
      ligne$niveau <- as.character(ligne$niveau)
      attr(ligne, "type") <- "fac-num"
      ligne
    }
  }
}

#' @export
#' @rdname create_line_bivar
create_line_bivar.numeric <- function(x, y, noms, .drop = TRUE, compute_p = TRUE, summary = NULL){ #num~fac
  if(missing(noms)) noms <- tolower(make.names(label(x)))
  if(is.factor(y)){
    no_na <- remove_na(x, y)
    cont <- table(no_na$y)
    if (nrow(no_na) > 0){
      x <- no_na$x
      y <- no_na$y
      if (compute_p){
        pval_test <- extract_pval(x,y) %>%
          as_tibble()
        names(pval_test) <- c("p", "test")
      } else
        pval_test <- NULL
      d <- no_na %>%
        group_by(y, .drop = .drop)

      show_summary <- dplyr::case_when(!is.null(summary) && summary == "mean" ~ "mean",
                                !is.null(summary) && summary == "median" ~ "median",
                                !is.null(pval_test) && !grepl("Mann-Whitney", pval_test$test[1]) ~ "mean",
                                !is.null(pval_test) && grepl("Mann-Whitney", pval_test$test[1]) ~ "median",
                                TRUE ~ "mean")


      d <- if(show_summary == "median") {
          summarise(d,  sprintf_number_table("%s [%s; %s]", median(x, na.rm=TRUE),
                                                  quantile(x, na.rm=TRUE)[2], quantile(x, na.rm=TRUE)[4]))%>%
          t()
      } else {
        summarise(d, sprintf_number_table("%s (%s)", mean(x, na.rm=TRUE), sd(x, na.rm=TRUE)))%>%
          t()
      }
      colnames(d) <- paste(label(y), d[1,])#column_names
      d <- as_tibble(d)

      d <- d[2, ]
      d$.n <- sum(cont)

      ligne <- if (compute_p){
        bind_cols(d, pval_test)
      } else {
        d
      }
      ligne %<>% add_varname(x, noms)
      #add_column(label(x), .before = 1)
      attr(ligne, "colSums") <- table(fct_drop(no_na$y))
      attr(ligne, "type") <- "num-fac"
      attr(ligne, "summary") <- show_summary
      #names(ligne)[1] <- "variable"
      ligne
    }
  } else {
    no_na <- remove_na(x, y, drop_factor = TRUE)
    ligne <- create_line_cor(no_na$x, no_na$y, compute_p = compute_p)
    if (is.null(ligne)) return(NULL)
    ligne %<>%
      add_varname(x, noms)
    attr(ligne, "type") <- "num-num"
    ligne
  }
}


#' Univariate analysis of survival
#'
#' @param x The independant variable
#' @param time The vector of follow-up time
#' @param censure The status indicator
#' @param compute_p logical: should the function compute the pvalue
#'
#' @return median(IC95), max follow-up, n, n events, p.
#' @export
#'
#' @examples
create_line_surv_bivar <- function(x, time, noms, censure, compute_p = TRUE){
  if(missing(noms)) noms <- tolower(make.names(label(x)))
  tab_cens <- create_tab_cens(x, time, censure) #remove_na(time, x, censure, drop_factor = TRUE)
  if (nrow(tab_cens) > 0){
    formule <- Surv(.time, censure) ~ x
    surv <- survfit(formule, data = tab_cens)
    resume <- base::summary(surv)$table
    max_time <- map_chr(seq_along(surv$strata), ~ format_number(max(surv[.]$time)))
    median <- map_chr(seq_along(surv$strata), function(i){
      sprintf_number_table("%s (%s; %s)",
                           resume[i, "median"],
                           resume[i, "0.95LCL"],
                           resume[i, "0.95UCL"])
    })

    n <-  resume[, "n.start", drop = TRUE]
    nEvent <- resume[, "events", drop = TRUE]
    surv_rates <- map_chr(seq_along(surv$strata), function(i) {
      surv_i <- surv[i]
      l <- length(surv_i$surv)
      sprintf("%s (%s; %s)", pourcent(surv_i$surv[l], arrondi = 3),
              pourcent(surv_i$lower[l], arrondi = 3),
              pourcent(surv_i$upper[l], arrondi = 3))
    })
    surv_means <- map_chr(seq_along(surv$strata), function(i){
      sprintf_number_table("%s (±%s)", resume[i, "rmean", drop = TRUE],
                                       resume[i, "se(rmean)", drop = TRUE])
    })
    d <- tibble(
      median,
      max_time,
      n,
      nEvent,
      surv_rates,
      surv_means
    )
    names(d) <- c(gettext("median (95% CI)", domain = "R-simplestats"),
                  gettext("max follow-up", domain = "R-simplestats"),
                  "n",
                  gettext("n events", domain = "R-simplestats"),
                  gettext("survival rate (95% CI)", domain = "R-simplestats"),
                  gettext("rmeans (SE)", domain = "R-simplestats"))

    x <- tab_cens$x

    ligne <- if (compute_p){
      pval_test <- extract_pval(tab_cens$x, tab_cens$.time, survival = TRUE, tab_cens$censure) %>%
        map_df(~ c(., rep(NA, max(0, nlevels(x) - 1))))
      names(pval_test) <- c("p", "test")
      bind_cols(d, pval_test)
    } else {
      d
    }
    ligne %<>% add_varname(x, noms)
    attr(ligne, "type") <- "survival"
    ligne
  }
}

create_line_cor <- function(x, y, compute_p = TRUE) {
  l <- length(x)
  name_title <- gettext("correlation coefficient", domain = "R-simplestats")
  CI95 <- gettext("(95% CI)", domain = "R-simplestats")
  if  (compute_p) {
    no_na <- remove_na(x, y)
    x <- no_na$x
    y <- no_na$y
    test <- find_test(x, y)
    if (is.null(test)) return(NULL)
    res <- test$result %>%
      broom::tidy()
    if(test$name == "Pearson") {
      name_title <- paste(name_title, CI95)
      title <- sprintf_number_table("%s (%s; %s)",
                                    res$estimate, res$conf.low, res$conf.high)
    } else {
      title <- sprintf_number_table("%s", res$estimate)
    }
    ligne <- tibble(!!name_title := title,
                    n = l,
                    p = res$p.value,
                    test = test$name)
  } else {
    if ((is_normal(x) | length(x) > 30) & (is_normal(y) | length(y) > 30) && is_homoscedatic(lm(y ~ x))){
      name_title <- paste(name_title, CI95)
      estimate <- cor(x, y, use = "complete.obs")
      z <- atanh(estimate)
      sigma <- 1/sqrt(sum(complete.cases(x, y)) - 3)
      cint <- (z + c(-1, 1) * sigma * qnorm((1 + 0.95)/2)) %>%
        tanh() %>%
        as.list() %>%
        setNames(c("conf.low", "conf.high"))
      title <- sprintf_number_table("%s (%s; %s)",
                                    estimate, cint$conf.low, cint$conf.high)
    } else {
      estimate <- cor(x, y, use = "complete.obs", method = "spearman")
      title <- sprintf_number_table("%s", estimate)
    }
    ligne <- tibble(!!name_title := title,
                    n = l)
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
    create_line_surv_bivar(x = x, time = y, noms = varname, censure = censure)
  } else {
    create_line_bivar(x, y, noms = varname)
  }
  show_table_markdown(shown)
}
