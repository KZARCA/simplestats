#' Get proportion of missing data
#'
#' @param tab A data frame
#'
#' @return The proportion of observations having at least one missing value.
#' @export
#'
#' @examples
get_propDM <- function(tab){
  if (is.data.frame(tab))
    1 - nrow(na.exclude(tab))/nrow(tab)
  else
    sum(is.na(tab)/length(tab))
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
imputer <- function(tab, type, vardep, n_imputation = 1){
  tabm <- dplyr::select_if(tab, function(x) is.numeric(x) | is.factor(x))
  if (type == "survival")
    tabm <- dplyr::select(tabm, -.time, -!!rlang::sym(vardep))
  for (i in 1:length(tabm)){
    if (get_propDM(tabm[[i]]) < 0.05)
      tab[[names(tabm)[i]]] <- Hmisc::impute(tabm[[i]]) #Hmisc : médiane
  }
  if (any(is.na(tab))){
    tabimp <- mice::mice(tab, printFlag = FALSE, seed = 1234567, m = n_imputation)
  } else {
    tabimp <- tab
  }
  return(tabimp)
}

