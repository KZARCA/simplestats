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
    is_homoscedatic.default(x = y, y = x)
  } else {
    is_homoscedatic.default(model$model[[2]], model$model[[1]])
  }
}

#' @export
#' @rdname is_homoscedatic
is_homoscedatic.default <- function(x, y, ...){
  vari <- tibble::tibble(x, y) %>%
    dplyr::group_by(x) %>%
    dplyr::summarise(variance = var(y, na.rm = TRUE)) %>%
    magrittr::extract2("variance")
  if (max(vari, na.rm = TRUE) < 3 * min(vari, na.rm = TRUE)) TRUE else FALSE
}

#' Can it be considered as normal?
#'
#' @param x Object to check
#' @param df data.frame with 2 columns, x being numeric and y being a factor
#' @param model The result of the lm function
#'
#' @return
#'
#' @details
#' is_normal.double first bootstraps the results and then pass the result to
#' is_normal.default
#' is_normal.default returns TRUE if the mean is between the 40th and 60th percentile
#' and the skewness is lower than 0.6
#'
#' @export
#'
#' @examples
is_normal <- function(x){
  UseMethod("is_normal")
}

#' @export
#' @rdname is_normalrb
is_normal.lm <- function(x){
  rstudent(x) %>%
    is_normal.default()
}

#' @export
#' @rdname is_normal
is_normal.data.frame <- function(df){
  lev <- unique(df$y)
    map_lgl(seq_along(lev), function(i){
      filter(df, y == lev[i]) %>%
        pull(x) %>%
        is_normal.numeric()
    }) %>%
      all()
  #} else TRUE
}

#' @export
#' @rdname is_normal
is_normal.numeric <- function(x){
  l <- length(x)
  res <- replicate(1000, mean(resample(x, l, replace = TRUE) ))
  res %>%
    is_normal.default()
}

is_normal.default <- function(x){
  f <- ecdf(x)
  percentile <- f(mean(x, na.rm=TRUE))
  skewness <- sum((x - mean(x, na.rm = TRUE))^3, na.rm = TRUE) / (length(x) * sd(x, na.rm = TRUE)^3)
  if (percentile < 0.6 & percentile > 0.4 & abs(skewness) < 0.6)
    TRUE
  else
    FALSE
}


#' #' @export
#' #' @rdname is_normal
#' is_normal.default <- function(x){
#'   f <- ecdf(x)
#'   percentile <- f(mean(x, na.rm=TRUE))
#'   med <- median(x, na.rm=TRUE)
#'   q25 <- f((med - abs(med - min(x, na.rm=TRUE))/2))
#'   q75 <- f((med + abs(max(x, na.rm=TRUE) - med)/2))
#'   if (percentile < 0.6 & percentile > 0.4 & q75-q25 > 0.5)
#'     TRUE
#'   else
#'     FALSE
#' }
