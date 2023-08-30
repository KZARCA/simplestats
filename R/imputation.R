#' Get proportion of missing data
#'
#' @param x A data frame or a vector
#'
#' @return The proportion of observations having at least one missing value.
#' @export
#'
#' @examples
get_propDM <- function(x){
  if (is.data.frame(x))
    1 - nrow(na.exclude(x))/nrow(x)
  else
    sum(is.na(x)/length(x))
}

#' Imputation strategy
#'
#' @param tab A data frame
#' @param vardep The dependant variable
#' @param type Can be one of "linear", "logistic", "survival"
#' @param n_imputation Number of imputations
#'
#' @return Either a data frame or a S3 object of class mids
#' @export
#'
#' @examples
imputer <- function(tab, vardep, type, n_imputation = 1, maxit = 5){
  # if (type == "survival") {
  #   tabm$cumhaz_EFS <- mice::nelsonaalen(data = tabm, timevar = ".time", statusvar = vardep)
  #   tabm <- dplyr::select(tabm, -.time, -vardep)
  # }
  if(get_propDM(tab) < 0.05){
    return(tab)
  } else {
    # for (i in 1:length(tabm)){
    #   if (get_propDM(tabm[[i]]) < 0.05) {
    #     tab[[names(tabm)[i]]] <- impute(tabm[[i]]) # median
    #   }
    # }
    if (any(is.na(tab[names(tab)]))){
      require(mice)
      if (type == "survival") {
        tmp <- data.frame(.time = tab$.time, status = tab[[vardep]]) # because nelsonaalen uses NSE
        tab$cumhaz_EFS <- mice::nelsonaalen(data = tmp, timevar = .time, statusvar = status)
        predmat_mice <- mice::make.predictorMatrix(tab)
        predmat_mice[, c(".time", vardep)] <- 0
        method <- mice::make.method(tab)
        method[c(".time", vardep, "cumhaz_EFS")] <- ""
      } else {
        predmat_mice <- mice::make.predictorMatrix(tab)
        method <- make.method(tab)
      }
      tabimp <- mice::mice(tab, printFlag = FALSE, seed = 1234567, m = n_imputation,
                           predictorMatrix = predmat_mice, maxit = maxit, method = method)
    } else {
      tabimp <- tab
    }
    return(tabimp)
  }
}


#' Get colnames with large number of missing values
#'
#' Loops over all the combinations of columns for which missing values
#' are more frequent and extracts them
#'
#'
#' @param tab A data frame
#'
#' @return A character vector of colnames
#' @export
#'
get_large_missing <- function(tab){
  if (ncol(tab) > 1){
    if  (any(is.na(tab))){
      pat <- mice::md.pattern(tab, plot = FALSE)
      line_missing <- pat[nrow(pat), ]
      all_vars <- names(tab)
      t <- tab[all_vars]
      elimine <- NULL
      while(get_propDM(t) > 0.2 & length(all_vars) > 2 &
            line_missing[ncol(pat) - 1] > 0.05 * nrow(t)) {
        elimine <- c(elimine, colnames(pat)[ncol(pat) - 1])
        all_vars <- setdiff(names(t), elimine)
        t <- t[all_vars]
        pat <- mice::md.pattern(t, plot = FALSE)
        line_missing <- pat[nrow(pat), ]
      }
      return(elimine)
    }
  } else return(NULL)
}

#' Find auxillary variables, ie predictors of data missingness
#'
#' @param tab A data.frame
#' @param vardep The dependant variable
#' @param varindep A character vector of independant variables
#' @param varajust A character vector of covariates
#' @param type Can be one of "linear", "logistic", "survival"
#'
#' @return
#' @export
#'
find_varaux <- function(tab, vardep, varindep = character(0), varajust = character(0), type){
  tabf <- if (type == "survival") {
    tab[c(vardep, varindep, varajust, ".time")]
  } else {
    tab[c(vardep, varindep, varajust)]
  }
  tab$.missing <- base::rowSums(is.na(tabf)) %>%
    as.logical() %>%
    as.factor()
  tab_aux <- tab[c(setdiff(names(tab), names(tabf)))]
  get_lasso_variables(tab_aux, ".missing", sparse = FALSE)
}

#' This code is a modification of the ggmice package - https://github.com/amices/ggmice)
#' @export
display_missing <- function(data, square = TRUE, rotate = TRUE) {
  vrb <- names(data)
  # get missing data pattern and extract info
  m <- mice::md.pattern(data, plot = FALSE)
  pat <- m[, m[nrow(m), , drop = FALSE] != 0, drop = FALSE]
  rws <- nrow(pat)
  cls <- ncol(pat)
  vrb <- colnames(pat)[-cls]
  frq <- row.names(pat)[-rws]
  n_tot <- sum(as.numeric(frq))
  frq <- sprintf("%s (%s)", frq, pourcent(as.numeric(frq)/n_tot))
  na_row <- pat[-rws, cls, drop = FALSE]
  na_col <- pat[rws, -cls, drop = FALSE]
  na_col <- sprintf("%s (%s)", na_col, pourcent(na_col/n_tot))
  # na_tot <- pat[rws, cls]

  pat_clean <- cbind(.opacity = 1, pat[-rws, vrb, drop = FALSE])

  # tidy the pattern
  long <- data.frame(.y = 1:(rws - 1), pat_clean, row.names = NULL) %>%
    tidyr::pivot_longer(cols = vrb, names_to = "x", values_to = ".where") %>%
    dplyr::mutate(
      .x = as.numeric(factor(.data$x, levels = vrb, ordered = TRUE)),
      .where = factor(.data$.where, levels = c(0, 1), labels = c("missing", "observed")),
      .opacity = as.numeric(.data$.opacity)
    )

  # create the plot
  gg <- ggplot2::ggplot(long, ggplot2::aes(.data$.x, .data$.y, fill = .data$.where, alpha = 0.1 + .data$.opacity / 2)) +
    ggplot2::geom_tile(color = "black") +
    ggplot2::scale_fill_manual(values = c("observed" = "#006CC2B3", "missing" = "#B61A51B3"),
                               labels = c(gettext("non missing"), gettext("missing"))) +
    ggplot2::scale_alpha_continuous(limits = c(0, 1), guide = "none") +
    ggplot2::scale_x_continuous(
      breaks = 1:(cls - 1),
      labels = na_col,
      sec.axis = ggplot2::dup_axis(
        labels = label(data[vrb]),
        name = NULL#gettext("Données manquantes pour chaque variable\nayant au moins une donnée manquante")
      )
    ) +
    ggplot2::scale_y_reverse(
      breaks = 1:(rws - 1),
      labels = frq,
      sec.axis = ggplot2::dup_axis(
        labels = na_row,
        name = NULL
      )
    ) +
    ggplot2::labs(
      x = NULL,
    #  y = "Pattern\n(frequency)",
      #x = NULL,
      y = NULL,
      fill = "",
      alpha = "" # ,
      # caption = paste("*total number of missing entries =", na_tot)
    ) +
    theme_custom()
  if (square) {
    gg <- gg + ggplot2::coord_fixed(expand = FALSE)
  } else {
    gg <- gg + ggplot2::coord_cartesian(expand = FALSE)
  }
  if (rotate | length(vrb) > 5) {
    gg <- gg + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
  }

  return(gg)
}

theme_custom <- function(vrb) {
  ggplot2::theme_minimal() +
    ggplot2::theme(
      # text = ggplot2::element_text(family="sans"),
      #legend.position = "bottom",
      #legend.justification = "right",
      #strip.placement = "outside",
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      #axis.ticks = ggplot2::element_line(colour = "black"),
      axis.title.y.right = ggplot2::element_text(margin = ggplot2::margin(l = 6))
    )
}

modify_imp <- function(tabm, mod){
  if(!inherits(tabm, "mids")) return(tabm)
  if (is.null(attr(mod, "remove"))) return(tabm)
  rem <- attr(mod, "remove")
  tabm$m <- tabm$m - length(rem)
  tabm$imp <- map(tabm$imp, function(x){
    x[-rem]
  })
  tabm
}
