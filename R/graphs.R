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
      ggkm(r, ylab=label(tab[[vardep]]), back.white = TRUE, cens.shape = "|") + scale_y_continuous(labels = scales::percent)
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
#' @return a ggplot barplot
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

# this function for most part is inspired by ggkm: https://github.com/michaelway/ggkm
#' @export
ggsurv <- function(sfit,
                   table = FALSE,
                   xlabs = gettext("Time"),
                   ylabs = gettext("Survival"),
                   xlims = c(0, max(sfit$time)),
                   ylims = NULL,
                   ystratalabs = names(sfit$strata),
                   main = "",
                   CI = FALSE,
                   shape = "|",
                   subs = NULL,
                   linecols="default",
                   BW = FALSE
                   ) {
  breaks = scales::pretty_breaks(5)(sfit$time)
    #################################
    # sorting the use of subsetting #
    #################################
    if(is.null(subs)){
      if(length(levels(summary(sfit)$strata)) == 0) {
        subs1 <- 1
        subs2 <- 1:length(summary(sfit,censored=T)$time)
        subs3 <- 1:length(summary(sfit,times = breaks,extend = TRUE)$time)
      } else {
        subs1 <- 1:length(levels(summary(sfit)$strata))
        subs2 <- 1:length(summary(sfit,censored=T)$strata)
        subs3 <- 1:length(summary(sfit,times = breaks,extend = TRUE)$strata)
      }
    } else{
      for(i in 1:length(subs)){
        if(i==1){
          ssvar <- paste("(?=.*\\b=",subs[i],sep="")
        }
        if(i==length(subs)){
          ssvar <- paste(ssvar,"\\b)(?=.*\\b=",subs[i],"\\b)",sep="")
        }
        if(!i %in% c(1, length(subs))){
          ssvar <- paste(ssvar,"\\b)(?=.*\\b=",subs[i],sep="")
        }
        if(i==1 & i==length(subs)){
          ssvar <- paste("(?=.*\\b=",subs[i],"\\b)",sep="")
        }
      }
      subs1 <- which(regexpr(ssvar,levels(summary(sfit)$strata), perl=T)!=-1)
      subs2 <- which(regexpr(ssvar,summary(sfit,censored=T)$strata, perl=T)!=-1)
      subs3 <- which(regexpr(ssvar,summary(sfit,times = breaks,extend = TRUE)$strata, perl=T)!=-1)
    }


    ##################################
    # data manipulation pre-plotting #
    ##################################


  nstrata <- length(levels(summary(sfit)$strata))


    if(nstrata == 0) {
      #[subs1]
      if(is.null(ystratalabs)) ystratalabs <- as.character(sub("group=*","",""))
      Factor <- factor(rep("",length(subs2)))
    } else {
      #[subs1]
      if(is.null(ystratalabs)) ystratalabs <- as.character(sub("group=*","",names(sfit$strata)))
      Factor <- factor(summary(sfit, censored = TRUE)$strata[subs2])
    }

    m <- max(nchar(ystratalabs))


    #Data to be used in the survival plot
    df <- data.frame(
      time = sfit$time[subs2],
      n.risk = sfit$n.risk[subs2],
      n.event = sfit$n.event[subs2],
      n.censor = sfit$n.censor[subs2],
      surv = sfit$surv[subs2],
      strata = Factor,
      upper = sfit$upper[subs2],
      lower = sfit$lower[subs2]
    )


    #Final changes to data for survival plot
    levels(df$strata) <- ystratalabs
    zeros <- data.frame(time = 0, surv = 1,
                        strata = factor(ystratalabs, levels=levels(df$strata)),
                        upper = 1, lower = 1)
    df <- dplyr::bind_rows(zeros, df)
    d <- length(levels(df$strata))

    ###################################
    # specifying axis parameteres etc #
    ###################################


    if (BW | nstrata == 0){
      linetype=c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash", "1F", "F1", "4C88C488", "12345678")
      p <- ggplot(df, aes(x=time, y=surv, linetype=strata)) +
        ggtitle(main)
    } else {
      p <- ggplot(df, aes(x=time, y=surv, colour = strata)) +
        ggtitle(main)
    }

    p <- p + theme_bw() +
      theme(legend.title = element_blank()) +
      scale_x_continuous(xlabs, breaks = breaks, limits = xlims) +
      scale_y_continuous(ylabs, limits = ylims, labels = scales::percent)

    if (nstrata == 0){
      p <- p + theme(legend.position="none")
    } else {
      if (table == TRUE) p <- p + theme(legend.position="bottom")
    }

    if (table == TRUE){
      #Set up theme elements
      p <- p +
        theme(axis.title.x = element_text(vjust = 0.7),
              axis.line = element_line(size =0.5, colour = "black"),
              axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
              axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")
              )
    }


    if(nstrata == 0){
      p <- p + geom_step(aes(y = upper), color = "black", lty = 2, size = 0.5) +
        geom_step(aes(y = lower), color = "black", lty = 2, size = 0.5)
    }

    #Removes the legend:

    #Add lines to plot
    p <- p + geom_step(size = 0.75)

    if (BW | nstrata == 0){
      p <- p + scale_colour_grey()
    }

    #Add censoring marks to the line:
      p <- p + geom_point(data = subset(df, n.censor >= 1), aes(x = time, y = surv), shape = shape) +
      guides(colour = guide_legend(override.aes = list(shape = NA)))


    ###################################################
    # Create table graphic to include at-risk numbers #
    ###################################################

    if(length(levels(summary(sfit)$strata)) == 0) {
      Factor <- factor(rep("All",length(subs3)))
    } else {
      Factor <- factor(summary(sfit,times = breaks,extend = TRUE)$strata[subs3])
    }

    if(table == TRUE) {
      risk.data <- data.frame(
        strata = Factor,
        time = summary(sfit,times = breaks,extend = TRUE)$time[subs3],
        n.risk = summary(sfit,times = breaks,extend = TRUE)$n.risk[subs3]
      )

      risk.data$strata <- factor(risk.data$strata, levels=rev(levels(risk.data$strata)))


      data.table <- ggplot(risk.data,aes(x = time, y = strata, label = format(n.risk, nsmall = 0))) +
        geom_text(size = 3.5) + theme_bw() +
        scale_y_discrete(breaks = as.character(levels(risk.data$strata)),
                         labels = rev(ystratalabs)) +
        scale_x_continuous(gettext("Numbers at risk"), limits = xlims, breaks=breaks) +
        theme(axis.title.x = element_text(size = 10, vjust = 1),
              panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks = element_blank(),axis.text.y = element_text(face = "bold",hjust = 1))

      data.table <- data.table +
        theme(legend.position = "none") + xlab(NULL) + ylab(NULL)


    }


    #######################
    # Plotting the graphs #
    #######################

    if(table == TRUE){
      cowplot::plot_grid(p, data.table, ncol=1, align="v", rel_heights=c(2,0.4))
    } else {
      p
    }

}
