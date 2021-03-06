#' Is the number of observations enough to perform the modeling?
#'
#' @param tab A data frame
#' @param vars A character vector of the independant variables and the adjustment variables
#' @param vardep A character string of the dependant variable
#'
#' @return A logical vector of length 1
#' @export
#'
#' @examples
is_number_enough <- function(tab, vardep, vars, type = "linear"){
  tab <- na.exclude(tab[, c(vardep, vars), drop = FALSE])
  n_max <- get_number_variables_max(tab, vardep, type)
  nVars <- map_dbl(vars, function(x){
    tvars <- tab[[x]]
    if (is.factor(tvars)) nlevels(tvars) - 1
    else 1
  }) %>%
    sum()
  if (nVars <= n_max) TRUE else FALSE
}

get_number_variables_max <- function(tab, vardep, type = "linear"){
  threshold <- 10
  if (type == "linear"){
    nrow(tab)/threshold
  } else if (type == "logistic" | type == "survival") {
    N <- if(type == "logistic"){
      get_min_class(tab, vardep)
    } else {
      sum(tab[[vardep]] == 1)
    }
    N/threshold
  }
}

#' Can it be considered homoscedatic?
#'
#' @param x Object to verify
#' @param y The grouping variable
#' @param model The result of the lm function
#' @param ... other arguments passed to the generic function
#'
#' @return
#' @export
#'
#' @examples
is_homoscedatic <- function(x, ...){
  UseMethod("is_homoscedatic")
}

#' @export
#' @rdname is_homoscedatic
is_homoscedatic.lm <- function(x, ...){
  model <- x
  if(length(model$model) > 2 || is.numeric(model$model[[2]])){
    res <- rstudent(model)
    nb_breaks <- max(2, min(floor(count_items(model$fitted.values)/20), 5))
    x <- res
    breaks <- unique(quantile(model$fitted.values, probs = seq(0,1,1/nb_breaks)), include.lowest = TRUE)
    breaks[1] <- -Inf
    breaks[length(breaks)] <- Inf
    y <- cut(model$fitted.values,
             breaks = breaks, right = FALSE)
    is_homoscedatic.default(y, x)
  } else {
    is_homoscedatic.default(model$model[[2]], model$model[[1]])
  }
}

#' @export
#' @rdname is_homoscedatic
is_homoscedatic.default <- function(x, y, ...){
  vari <- tibble::tibble(y, x) %>%
    dplyr::group_by(x) %>%
    dplyr::summarise(variance = var(y, na.rm = TRUE)) %>%
    magrittr::extract2("variance")
  if (max(vari, na.rm = TRUE) < 3 * min(vari, na.rm = TRUE)) TRUE else FALSE
}

#' Can it be considered as normal?
#'
#' @param x Object to verify
#' @param model The result of the lm function
#'
#' @return
#' @export
#'
#' @examples
is_normal <- function(x){
  UseMethod("is_normal")
}

#' @export
#' @rdname is_normal
is_normal.lm <- function(x){
  rstudent(x) %>%
    is_normal.default()
}

#' @export
#' @rdname is_normal
is_normal.default <- function(x){
  f <- ecdf(x)
  percentile <- f(mean(x, na.rm=TRUE))
  med <- median(x, na.rm=TRUE)
  q25 <- f((med - abs(med - min(x, na.rm=TRUE))/2))
  q75 <- f((med + abs(max(x, na.rm=TRUE) - med)/2))
  if (percentile < 0.6 & percentile > 0.4 & q75-q25 > 0.5)
    TRUE
  else
    FALSE
}
