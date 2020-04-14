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
#' @param type Can be one of "linear", "logistic", "survival"
#' @param vardep The dependant variable
#' @param n_imputation Number of imputations
#'
#' @return Either a data frame or a S3 object of class mids
#' @export
#'
#' @examples
imputer <- function(tab, vardep, type, exclude_mice = NULL, n_imputation = 1){
  tabm <- dplyr::select_if(tab, function(x) is.numeric(x) | is.factor(x)) %>%
    dplyr::select(-!!rlang::sym(vardep))
  if (type == "survival") tabm <- dplyr::select(tabm, -.time)
  if(get_propDM(tabm) < 0.05){
    return(tab)
  } else {
    for (i in 1:length(tabm)){
      if (get_propDM(tabm[[i]]) < 0.05 | names(tabm)[i] %in% exclude_mice & nrow(tab) > 5000) {
        tab[[names(tabm)[i]]] <- impute(tabm[[i]]) # median
      }
    }
    if (any(is.na(tab[names(tabm)]))){
      require(mice)
      where <- data.frame(is.na(tab))
      where[[vardep]] <- rep(FALSE, nrow(where))
      if (type == "survival") where[[".time"]] <- FALSE
      tabimp <- mice::mice(tab, printFlag = FALSE, seed = 1234567, m = n_imputation, where = where)
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
}
