#' Create n subsamples for cross validations
#'
#' @param tab A data.frame
#' @param n Number of subsamples
#'
#' @return A list of data.frames
split_cv <- function(tab, n = 10){
  set.seed(1234567)
  tab <- dplyr::sample_frac(tab)
  cv <- cut(seq(1, nrow(tab)), breaks=n, labels=FALSE)
  split(tab, cv)
}


#' Variable selection by LASSO procedure
#'
#' @param tab A data.frame
#' @param vardep The dependant variable
#' @param varindep The independant variables to force into the model
#' @param type Type of analysis: linear, logistic or survival
#'
#' @return A character vector of the predictor variables
#' @export
get_lasso_variables <- function(tab, vardep, varindep = character(0), type = "logistic") {
  set.seed(1234567)
  nona <- na.exclude(tab) %>%
    standardize_num_vars()
  formule <- paste(vardep, "~ .")
  if (type == "survival"){
    formule <- paste(formule, "-.time")
    y <- Surv(nona$.time, nona[[vardep]])
  } else {
    y <- nona[[vardep]]
  }
  mat <- model.matrix(as.formula(formule), data = nona)
  penalties <- rep(1, ncol(mat))
  names_tab <- names(tab)
  expanded_fac <- map(names(nona), function(x){
    if (is.factor(nona[[x]])){
      lev <- levels(tab[[x]])
      paste0(x, lev[2:length(lev)]) %>% setNames(rep(names(tab[x]), length(lev)-1))
    } else {
      setNames(x, x)
    }
  }) %>% unlist()
  varindep_mat <- expanded_fac[names(expanded_fac) %in% varindep]
  penalties[which(colnames(mat) %in% varindep_mat)] <- 0
  family = dplyr::case_when(type == "logistic" ~ "binomial",
                            type == "survival" ~ "cox")
  cv <- cv.glmnet(x = mat,
                  y = y,
                  family = family,
                  penalty.factor = penalties)
  res <- coef(cv, cv$lambda.1se)
  expanded_fac[expanded_fac %in% res@Dimnames[[1]][res@i+1]] %>%
    names() %>%
    unique()
}


#' Creates a contingency table with predicted probabilities and actual classification
#' @param mod a glm or mira object
#'
#' @return data.frame
#' @export
create_pred_obs <- function(mod){
  label <- if (inherits(mod, "mira")) {
    getfit(mod, 1)$model[[1]]
  } else {
    mod$model[[1]]
  }
  label %<>% as.character() %>%
    as.numeric()
  data.frame(M = predict(mod, type = "response"),
         D = label)
}


predict.mira <- function(mod, ...){
  map(getfit(mod), ~ predict(., type = "link")) %>%
    as.data.frame() %>%
    rowMeans() %>%
    coef_to_prob()
}

#' Get performance measures of each subsample among a n-fold cross validation
#'
#' @param tab The data.frame
#' @param vardep The dependant variable
#' @param varindep The independant variable, forced into the model
#' @param type A character vector, one of "linear", "logistic" or "survival
#' @param n n-fold cross validation
#'
#' @return The mean AUC
#' @export
compute_cv_perf <- function(tab, vardep, varindep = NULL, type = "logistic", n = 10){
  get_cv_auc(tab, vardep, varindep, type, n)%>%
    get_mean_perf()
}

get_cv_auc <- function(tab, vardep, varindep = NULL, type = "logistic", n = 10){
  tab <- split_cv(tab, n)
  map(seq_along(tab), function(i){
    train <- suppressWarnings(dplyr::bind_rows(tab[-i]))
    test <- tab[[i]]
    varajust <- setdiff(get_lasso_variables(train, vardep, varindep, type), varindep)
    el <- recherche_multicol(train, vardep, varindep, varajust, type, pred = TRUE)
    varajust <- remove_elements(varajust, el)
    results <- compute_mod(train, vardep, varindep, varajust, type, pred = TRUE, cv = TRUE)
    calculate_auc(results$mod)
  })
}

calculate_auc <- function(mod){
  create_pred_obs(mod) %>%
    plot_ROC(showThreshold = FALSE) %>%
    plotROC::calc_auc() %>%
    extract2("AUC")
}

get_mean_perf <- function(x){
  flatten_dbl(x) %>% mean()
}


