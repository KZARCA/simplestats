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
imputer <- function(tab, vardep, type, n_imputation = 1){
  tabm <- dplyr::select_if(tab, function(x) is.numeric(x) | is.factor(x)) %>%
    dplyr::select(-!!rlang::sym(vardep))
  if (type == "survival") tabm <- dplyr::select(tabm, -.time)
  if(get_propDM(tabm) < 0.05){
    return(tab)
  } else {
    for (i in 1:length(tabm)){
      if (get_propDM(tabm[[i]]) < 0.05) {
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

