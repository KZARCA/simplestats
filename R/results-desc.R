#' Descriptive analysis
#'
#' @param x A vector to describe
#' @param noms variable names
#'
#' @return If x is a numeric vector: mean(sd), median(Q25-75), min, max, n.
#'     If x is a factor: n (%)
#' @export
#'
#' @examples
create_ligne_desc <- function(x, ...){
  UseMethod("create_ligne_desc")
}

#' @export
#' @rdname create_ligne_desc
create_ligne_desc.numeric <- function(x, noms, ...){ #si la variable est numérique
  moy <- base::mean(x, na.rm=TRUE)
  et <- sd(x, na.rm=TRUE)
  quant <- quantile(x, na.rm = TRUE)
  n <- sum(!is.na(x))
  d <- tibble(
    sprintf_number_table("%s (%s)", moy, et),
    sprintf_number_table("%s [%s; %s]", quant[3], quant[2], quant[4]),
    sprintf_number_table("%s", quant[1]),
    sprintf_number_table("%s", quant[5]),
    n
  )
  names(d) <- c(gettext("mean (sd)", domain = "R-simplestats"),
                gettext("median [Q25-75]", domain = "R-simplestats"),
                gettext("min", domain = "R-simplestats"),
                gettext("max", domain = "R-simplestats"), "n")
  add_varname(d, x, noms)
}

#' @export
#' @rdname create_ligne_desc
create_ligne_desc.factor <- function(x, noms, ...){
  cont <- table(x)
  prop <- prop.table(cont) %>%  pourcent()
  map2(cont, prop, function(x, y) {
    sprintf_number_table("%s (%s)", x, y)
  }) %>%
    as_tibble() %>%
    add_varname(x, noms, one_line = TRUE)
}

#' Descriptive analysis of survival
#'
#' @param time The vector of follow-up time
#' @param censure The status indicator
#'
#' @return median(IC95), max follow-up, n, n events.
#' @export
#'
#' @examples
create_ligne_surv_desc <- function(time, censure){
  tab_cens <- remove_na(time, censure)
  if (nrow(tab_cens) > 0){
    names(tab_cens) <- c(".time", "censure")
    tab_cens$censure %<>% as.character %>% as.numeric
    formule <- Surv(.time, censure) ~ 1
    surv <- survfit(formule, data = tab_cens)
    l <- length(surv$surv)
    resume <- base::summary(surv)$table
    med <- resume["median"]
    CI <- resume[c("0.95LCL", "0.95UCL")]
    n <- resume["n.start"]
    nEvent <- resume["events"]
    max <- max(surv$time, na.rm = TRUE)
    d <- tibble(
      sprintf_number_table("%s (%s; %s)", med, CI[1], CI[2]),
      format_number(max),
      n,
      nEvent,
      sprintf("%s (%s; %s)", pourcent(surv$surv[l], arrondi = 3),
              pourcent(surv$lower[l], arrondi = 3),
              pourcent(surv$upper[l], arrondi = 3))
    )

    names(d) <- c(gettext("median (CI95)", domain = "R-simplestats"),
                  gettext("max follow-up", domain = "R-simplestats"), "n",
                  gettext("n events", domain = "R-simplestats"),
                  gettext("survival rate (CI95)", domain = "R-simplestats"))

    d %<>% add_column(id = "survival", variable = gettext("max follow-up", domain = "R-simplestats"), .before = 1)
  }
}

#' @export
#' @rdname create_ligne_desc
create_ligne_desc_export <- function(x, ...){
  UseMethod("create_ligne_desc_export")
}

#' @export
#' @rdname create_ligne_desc
create_ligne_desc_export.factor <- function(x, noms, ...){
  cont <- table(x) %>% unname()
  prop <- prop.table(cont) %>%  pourcent() %>% unname()
  d <- map2_chr(cont, prop, function(x, y) {
    sprintf_number_table("%s (%s)", x, y)
  }) %>% tibble()
  d %<>% add_varname(x, noms, add_niveau = TRUE)
  colnames(d) <- c("id", "variable", "niveau", "n (%)")
  d$niveau <- as.character(d$niveau)
  d
}

#' @export
#' @rdname create_ligne_desc
create_ligne_desc_export.default <- create_ligne_desc

#' Displays the descriptive analysis in markdown
#'
#' @param x The variable to describe
#' @param type A character string of the type of modeling, having a value among "linear", "logistic" or "survival"
#' @param time The vector of follow-up time
#'
#' @return A tibble displayed in markdown
#' @export
#'
#' @examples
print_line_desc <- function(x, varname = label(x), type = "linear", time = NULL){
  shown <- if (type == "survival"){
    create_ligne_surv_desc(time, x)
  } else {
    create_ligne_desc_export(x, noms = varname)
  }
  show_table_markdown(shown)
}

#' @export
#' @rdname create_ligne_desc_ba
create_ligne_desc_ba <- function(x, ...){
  UseMethod("create_ligne_desc_ba")
}

#' @export
#' @rdname create_ligne_desc_ba
create_ligne_desc_ba.numeric <- function(x, y, noms){
  if(missing(noms)) noms <- tolower(make.names(label(x)))
  meanx <- mean(x, na.rm =  TRUE)
  meany <- mean(y, na.rm = TRUE)
  sdx <- sd(x, na.rm =  TRUE)
  sdy <- sd(y, na.rm = TRUE)
  qx <- quantile(x, na.rm = TRUE)
  qy <- quantile(y, na.rm = TRUE)
  nx <- sum(!is.na(x))
  ny <- sum(!is.na(y))
  before <- tibble(
    sprintf_number_table("%s (%s)", meanx, sdx),
    sprintf_number_table("%s [%s; %s]", qx[3], qx[2], qx[4]),
    sprintf_number_table("%s", qx[1]),
    sprintf_number_table("%s", qx[5]),
    nx
  )
  names(before) <- c(gettext("mean (sd)", domain = "R-simplestats"),
                     gettext("median [Q25-75]", domain = "R-simplestats"),
                     gettext("min", domain = "R-simplestats"),
                     gettext("max", domain = "R-simplestats"),
                     "n")
  after <- tibble(
    sprintf_number_table("%s (%s)", meany, sdy),
    sprintf_number_table("%s [%s; %s]", qy[3], qy[2], qy[4]),
    sprintf_number_table("%s", qy[1]),
    sprintf_number_table("%s", qy[5]),
    ny
  )
  names(after) <- c(gettext("mean (sd)", domain = "R-simplestats"),
                     gettext("median [Q25-75]", domain = "R-simplestats"),
                     gettext("min", domain = "R-simplestats"),
                     gettext("max", domain = "R-simplestats"),
                    "n")

  delta_name  <- gettext("Δ mean")
  res <- rbind(before, after) %>%
    mutate(delta = c(sprintf_number_table("%s", meany - meanx), NA),
           p = c(extract_pval(x, y, ba = TRUE)$pval, NA))
  names(res)[6] <- delta_name
  add_varname(res, x, noms) %>%
    add_column(niveau = c(gettext("before"), gettext("after")),
                          .after = "variable")

}

#' @export
#' @rdname create_ligne_desc_ba
create_ligne_desc_ba.factor <- function(x, y, noms){
  if(missing(noms)) noms <- tolower(make.names(label(x)))
  x <- forcats::fct_expand(x, levels(y))
  y <- forcats::fct_expand(y, levels(x))
  y <- factor(y, levels = levels(x))
  contx <- table(x)
  propx <- prop.table(contx) %>%  pourcent()
  conty <- table(y)
  propy <- prop.table(conty) %>%  pourcent()

  before <- map2_chr(contx, propx, function(x, y) {
    sprintf_number_table("%s (%s)", x, y)
  }) %>%
   tibble::as_tibble_col(column_name = gettext("before"))

  after <-  map2_chr(conty, propy, function(x, y) {
    sprintf_number_table("%s (%s)", x, y)
  })%>%
    tibble::as_tibble_col(column_name = gettext("after"))

  pval_test <- extract_pval(x, y, ba = TRUE) %>%
    map_df(~ c(., rep(NA, nlevels(x) - 1)))

  names(pval_test) <- c("p", "test")

  ligne <- bind_cols(before, after, pval_test) %>%
    add_varname(x, noms)

  ligne
}



