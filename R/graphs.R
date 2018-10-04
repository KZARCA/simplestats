#' Plot descriptive graphs
#'
#' @param tab The data frame
#' @param vardep The dependent variable
#' @param varindep The independant variable
#' @param type A character string of the type of modeling, having a value among "linear", "logistic" or "survival"
#'
#' @return a ggplot2 graph
#' @export
#'
#' @examples
print_plot_desc <- function(tab, vardep = NULL, varindep = NULL, type = "linear"){
  if (type == "survival"){
    formule <- sprintf("Surv(.time, %s) ~ 1", vardep)
    r <- survfit(as.formula(formule), data = tab)
    ggsurv(r, ylab=label(tab[[vardep]]), cens.shape = "|", back.white = TRUE)  + scale_y_continuous(labels = scales::percent)
  } else if (is.numeric(tab[[varindep]])){
    ggplot(remove_missing(tab, na.rm = TRUE, vars = varindep)) + aes_string(x = varindep) + geom_histogram() + theme_bw() + labs(y = "Number")
  } else {
    if (nlevels(tab[[varindep]]) < 5){
      ggplot(tab) + aes_string(x = varindep, fill=varindep) + geom_bar(aes(y=(..count..)/sum(..count..)), na.rm = TRUE) + theme_bw() + scale_y_continuous(labels = scales::percent) + guides(fill=FALSE) + labs(y = "Proportion")
    } else {
      ggplot(tab) + aes_string(x = varindep, fill=varindep) + geom_bar(aes(y=(..count..)/sum(..count..)), na.rm = TRUE) + theme_bw() + scale_y_continuous(labels = scales::percent) + labs(y = "Proportion", fill=label(tab[[varindep]])) + scale_x_discrete(breaks = NULL)
    }
  }
}


#' Plot univariate graphs
#'
#' @param tab The data frame
#' @param vardep The dependent variable
#' @param varindep The independant variable
#' @param type A character string of the type of modeling, having a value among "linear", "logistic" or "survival"
#'
#' @return a ggplot2 graph
#' @export
#'
#' @examples
print_plot_bivar <- function(tab, vardep, varindep, type = "linear"){
  tvarn <- tab[[varindep]]
  if (type == "linear"){
    if (is.factor(tvarn)){
      boxplot_bivar(tab, vardep, varindep)
    } else {
      plot_reglin(tab, vardep, varindep)
    }
  } else if (type != "survival"){
    if (is.numeric(tvarn)){
      boxplot_bivar(tab, varindep, vardep)
    } else if (is.factor(tvarn)){
      barplot_bivar(tab, varindep, vardep)
    }
  } else {
    if (is.factor(tvarn)){
      formule <- sprintf("Surv(.time, %s) ~ %s", vardep, varindep)
      r <- survfit(as.formula(formule), data = tab)
      names(r$strata) <- gsub(varindep, label(tvarn), names(r$strata))
      ggsurv(r, ylab=label(tab[[vardep]]), back.white = TRUE, cens.shape = "|") + scale_y_continuous(labels = scales::percent)
    }
  }
}

#' Plots the relationship between 2 quantitative variables
#'
#' @param tab a data frame
#' @param x the independant variable
#' @param y the dependant variable
#'
#' @return a ggplot plot
#' @export
#'
#' @examples
plot_reglin <- function(tab, x, y, method = "lm"){
  tab <- remove_missing(tab, na.rm = TRUE, vars=c(x, y))
  ggplot(tab) + aes_string(x, y) + geom_point() + geom_smooth(method=method) + theme_bw()
}

#' Easy Boxplot
#'
#' @param tab a data frame
#' @param x the independant variable
#' @param y the dependant variable
#'
#' @return a ggplot boxplot
#' @export
#'
#' @examples
boxplot_bivar <- function(tab, x, y) {
  tab <- remove_missing(tab, na.rm = TRUE, vars=c(x, y))
  ggplot(tab) + aes_string(y, x, y, fill = y) + geom_boxplot() + theme_bw() + labs(x = label(tab[[y]]), y = label(tab[[x]])) + guides(fill=FALSE)
}

#' @export
#' @rdname boxplot_bivar
boxplot_bivar_bw <- function(tab, x, y) {
  tab <- remove_missing(tab, na.rm = TRUE, vars=c(x, y))
  ggplot(tab) + aes_string(y, x, y) + geom_boxplot() + theme_bw() + labs(x = label(tab[[y]]), y = label(tab[[x]])) + guides(fill=FALSE) + scale_fill_grey()
}

#' Easy Barplot
#'
#' @param tab a data frame
#' @param x the independant variable
#' @param y the dependant variable
#'
#' @return a ggplot boxplot
#' @export
#'
#' @examples
barplot_bivar <- function(tab, x, y, graphPercent = NULL, showGraphNA = NULL){
  if (is.null(graphPercent) || !graphPercent){
    tab2 <- dplyr::select(tab, !!sym(x), !!sym(y)) %>%
      group_by(!!sym(y)) %>%
      mutate(group_size = n()) %>%
      group_by(!!sym(x), !!sym(y)) %>%
      summarise(perc = n()/max(group_size))

    ggtab2 <- if (!is.null(showGraphNA) && !showGraphNA) {
      ggplot(remove_missing(tab2, na.rm = TRUE, vars = c(x, y)))
    } else
      ggplot(remove_missing(tab2, na.rm = TRUE, vars = y))

    graph <- ggtab2 + aes_string(x = x, fill = x, y = "perc") + geom_bar(stat = "identity")  +
      facet_grid(reformulate(paste(". ~ ", y))) + scale_y_continuous(labels = scales::percent) + labs(x = label(tab[[y]]), fill = label(tab[[x]]), y = gettext("Proportion", domain = "R-simplestats"))
  } else {
    ggtab <- if (!is.null(showGraphNA) && !showGraphNA) {
      ggplot(remove_missing(tab, na.rm = TRUE, vars = c(x, y)))
    } else {
      ggplot(remove_missing(tab, na.rm = TRUE, vars = y))
    }

    graph <- ggtab + aes_string(x, fill = x) + geom_bar(position = "dodge") +
      facet_grid(reformulate(paste(".~ ", y))) + labs(x = label(tab[[y]]), fill = label(tab[[x]]), y = gettext("Number", domain = "R-simplestats"))
  }

  if (nlevels(tab[[y]]) < 4)
    graph <- graph + facet_grid(reformulate(paste(".~ ", y)))
  else
    graph <- graph + facet_wrap(reformulate(paste("~ ", y)), ncol=4)

  graph <- graph + theme_bw()  + scale_x_discrete(breaks = NULL)
  return(graph)
}
