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
  names(d) <- c(gettext("moyenne (écart-type)"), gettext("médiane [Q25-75]"), gettext("min"), gettext("max"), "n")
  d %<>% add_varname(x, noms)
}

#' @export
#' @rdname create_ligne_desc
create_ligne_desc.factor <- function(x, noms, ...){
  cont <- table(x)
  prop <- prop.table(cont) %>%  pourcent
  map2(cont, prop, function(x, y) {
    sprintf_number_table("%s (%s)", x, y)
  }) %>%
    as_tibble %>%
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
    resume <- base::summary(surv)$table
    med <- resume["median"]
    CI <- resume[c("0.95LCL", "0.95UCL")]
    n <- resume["n.start"]
    nEvent <- resume["events"]
    max <- max(surv$time, na.rm = TRUE)
    d <- tibble(
      sprintf_number_table("%s (%s - %s)", med, CI[1], CI[2]),
      max,
      n,
      nEvent
    )
    names(d) <- c(gettext("médiane (IC95)"), gettext("suivi max"), "n", gettext("n événements"))
    d %<>% add_column(id = "survival", variable = gettext("suivi max"), .before = 1)
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
  cont <- table(x)
  prop <- prop.table(cont) %>%  pourcent
  d <- map2_df(cont, prop, function(x, y) {
    sprintf_number_table("%s (%s)", x, y)  %>% tibble
  }, .id="")
  d %<>% add_varname(x, noms, add_niveau = TRUE)
  colnames(d) <- c("id", "variable", "niveau", "n (%)")
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
print_line_desc <- function(x, varname = NA, type = "linear", time = NULL){
  shown <- if (type == "survival"){
    create_ligne_surv_desc(time, x)
  } else {
    create_ligne_desc_export(x, noms = varname)
  }
  show_table_markdown(shown)
}


