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
  if (type == "survival" & ncol(tab) == length(c(vardep, varindep)) + 1) return(character(0))
  if (ncol(tab) == length(c(vardep, varindep))) return(character(0))
  set.seed(1234567)
  nona <- Filter(function(x) {
      solve_contrast(tab, vardep, x)
    }, tab) %>% na.exclude()
  if (length(varindep) >= ncol(nona) - 1) return(character(0))
  nb_max <- get_number_variables_max(nona, vardep, type)
  nb_remaining <- floor(nb_max - length(varindep))
  if (nb_remaining <= 0) return(character(0))
  formule <- paste(vardep, "~ .")
  if (type == "survival"){
    nona <- filter(nona, .time > 0)
    formule <- paste(formule, "-.time")
    y <- Surv(nona$.time, nona[[vardep]])
  } else {
    y <- nona[[vardep]]
  }
  mat <- model.matrix(as.formula(formule), data = nona)
  penalties <- rep(1, ncol(mat))
  expanded_fac <- map(names(nona), function(x){
    if (is.factor(nona[[x]])){
      lev <- levels(nona[[x]])
      paste0(x, lev[2:length(lev)]) %>% setNames(rep(names(nona[x]), length(lev)-1))
    } else {
      setNames(x, x)
    }
  }) %>% unlist()
  varindep_mat <- expanded_fac[names(expanded_fac) %in% varindep]
  penalties[which(colnames(mat) %in% varindep_mat)] <- 0
  family = dplyr::case_when(type == "logistic" ~ "binomial",
                            type == "survival" ~ "cox",
                            type == "linear" ~ "gaussian")

  cv <- try(
  cv.glmnet(x = mat,
                  y = y,
                  family = family,
                  penalty.factor = penalties))
  if (inherits(cv, "try-error")){
    return("ERROR_MODEL")
  }
  idx_lambda <- which(cv$lambda == cv$lambda.1se)
  nzero <- cv$nzero

  res <- if (nzero[idx_lambda] <= nb_max){
    coef(cv, cv$lambda.1se)
  } else {
    idx <- tail(which(nzero <= nb_remaining), n = 1)
    coef(cv, cv$lambda[idx])
  }

  expanded_fac[expanded_fac %in% res@Dimnames[[1]][res@i+1]] %>%
    names() %>%
    unique() %>%
  setdiff(varindep)
}


#' Creates a contingency table with predicted probabilities and actual classification
#' @param mod a glm or mira object
#'
#' @return data.frame
#' @export
create_pred_obs <- function(mod, tab = NULL, vardep = NULL, as_prob = TRUE){
  if (is.null(tab)){
    label <- if (inherits(mod, "mira")) {
      getfit(mod, 1)$model[[1]]
    } else {
      mod$model[[1]]
    }
    pred <- predict(mod)
  } else {
    label <- tab[[vardep]]
    tab <- remove_missing_levels(tab, mod)
    pred <- predict(mod, newdata = tab)
  }
  label %<>%
    as.factor() %>%
    as.numeric() %>%
    subtract(1)

  if (as_prob){
    pred <- coef_to_prob(pred)
  }
  data.frame(M = pred, D = label)
}

#' @export
predict.mira <- function(mod, ...){
  mods <- getfit(mod)
  pred <- map(mods, function(x) predict(x, ...))
  rownames <- map(pred, names) %>%
    Reduce(union, .)
  map(pred, function(x){
    diff <- setdiff(rownames, names(x))
    x[diff] <- NA
    x
  }) %>%
    as.data.frame() %>%
    rowMeans(na.rm = TRUE)
}

#' Get performance measures of a predictive model
#'
#' @param tab The data.frame
#' @param vardep The dependant variable
#' @param varindep The independant variable, forced into the model
#' @param type A character vector, one of "linear", "logistic" or "survival
#' @param R The number of bootstrap replicates
#' @param nCPU integer: number of processes to be used in parallel operation
#' @param type_validation character "cv" or "boot": either performs bootstrap, or cross validation
#' @return a list containing The mean AUC if cross validation; if bootstrap: the confidence interval and excess optimism
#' @export
get_pred_perf <- function(tab, vardep, varindep = NULL, type = "logistic",
                          type_validation = "cv", R = 100, nCPU = 1L, mod = NULL,
                          updateProgress = function(detail) detail = "", seed = 1234567){
  if (type_validation == "cv"){
    cv <- get_cv_auc(tab, vardep, varindep, type, n = min(10, get_min_class(tab, vardep, type)/12), progression = updateProgress)
    m <- map_dbl(cv, 1)  %>%
      mean(na.rm = TRUE)

    lambda <- get_lambda(mod) #heuristic formula (see Steyenberg)
    shrunk <- get_shrunk_coef(mod, lambda)
    return(list(mean = m, shrunk = shrunk, error = sum(map_dbl(cv, 2) / length(map_dbl(cv, 2)))))
  }
  set.seed(seed)
  res <- boot(tab, boot_auc, R = R, vardep = vardep, varindep = varindep,
       type = type, progression = updateProgress, parallel = "multicore", ncpus = nCPU)
  m <- mean(res$t[, 1], na.rm = TRUE)

  # Calculate the optimism (step 5) : bootstrap performance - test performance
  opti <- mean(res$t[, 1] - res$t[, 2], na.rm = TRUE)
  ci <- boot.ci(res, type = "basic", index = 1)$basic[4:5]
  lambda <- mean(res$t[, 3], na.rm = TRUE)
  shrunk <- get_shrunk_coef(mod, lambda)
  return(list(mean = m, ci = ci, optimism = opti, shrunk = shrunk, error = sum(res$t[, 4])/length(res$t[, 4])))
}

get_cv_auc <- function(tab, vardep, varindep = NULL, type = "logistic", n = 10, progression = function() cat("=")){
  tab <- split_cv(tab, n)
  map(seq_along(tab), function(i){
    progression()
    train <- do.call(rbind, tab[-i])
    test <- tab[[i]]
    error_lasso <- 0
    varajust <- get_lasso_variables(train, vardep, varindep, type)
    if (identical(varajust, "ERROR_MODEL")){
      varajust <- character(0)
      error_lasso <- 1
    } else {
      el <- recherche_multicol(train, vardep, varindep, varajust, type, pred = TRUE)
      varajust <- if (identical(el, "ERROR_MODEL")) character(0) else remove_elements(varajust, el)
    }
    results <- compute_mod(train, vardep, varindep, varajust, type, pred = TRUE, cv = TRUE)
    AUC <- create_pred_obs(results$mod) %>%
      calculate_auc()
    list(AUC, error_lasso)
  })
}

#' Calculate AUC
#'
#' @param mod The prediction model
#' @param orig_table The original tab
#' @param vardep The dependant varible
#'
#' @return
#' @export
calculate_auc <- function(pred_obs){
  plot_ROC(pred_obs, showThreshold = FALSE) %>%
  plotROC::calc_auc() %>%
  extract2("AUC")
}

boot_auc <- function(data, indices, progression, vardep, varindep = NULL, type) {
  progression()
  error_lasso <- 0
  train <- data %>%
    dplyr::slice(indices) %>%
    create_tabi("pred", keep = c(vardep, varindep))

  varajust <- get_lasso_variables(train, vardep, varindep, type)
  if (identical(varajust, "ERROR_MODEL")){
    varajust <- character(0)
    error_lasso <- 1
  } else {
    el <- recherche_multicol(train, vardep, varindep, varajust, type, pred = TRUE)
    varajust <- if (identical(el, "ERROR_MODEL")) character(0) else remove_elements(varajust, el)
  }
  results <- compute_mod(train, vardep, varindep, varajust, type, pred = TRUE, cv = TRUE)

  # for(i in seq_along(results$mod$xlevels)){
  #   results$mod$xlevels[[i]] <- union(results$mod$xlevels[[i]],
  #                                     levels(data[[names(results$mod$xlevels)[i]]]))
  # }

  # Model performance on the sample (step 3 p95 Steyerberg Clinical Prediction Models)
  pred_obs_boot <- create_pred_obs(results$mod, as_prob = FALSE) # Linear Predictor
  perf_boot <- calculate_auc(pred_obs_boot)
  # Model performance on the data (step 4)
  pred_obs_test <- create_pred_obs(results$mod, data[c(vardep, varindep, varajust)], vardep, as_prob = FALSE)
  perf_test <- calculate_auc(pred_obs_test)
  shrinkage_factor <- glm(D ~ M, data = pred_obs_test, family="binomial") %>%
    coef() %>%
    extract2(2)
  c(perf_boot, perf_test, shrinkage_factor, error_lasso)
}

get_shrunk_coef <- function(x, ...){
  UseMethod("get_shrunk_coef")
}

get_shrunk_coef.mira <- function(mod, lambda){
  purrr::map_dfc(getfit(mod), get_shrunk_coef.lm, lambda) %>%
    rowMeans()
}

get_shrunk_coef.lm <- function(mod, lambda){
  dataset <- cbind(model.matrix(mod), as.numeric(mod$model[[1]])-1)
  coefs <- as.matrix(coef(mod), ncol = 1)
  get_shrunk_coef.default(dataset, coefs, lambda)
}

get_shrunk_coef.default <- function(dataset, coefs, lambda){
  nc <- dim(dataset)[2]
  sdm <- matrix(c(0, rep(1, nc - 2)), nrow = 1)
  sdm <- t(sdm)
  B.shrunk <- matrix(diag(as.vector(coefs)) %*% sdm %*% lambda +
                       diag(as.vector(coefs)) %*% apply(1 - sdm, 1, min), ncol = 1)
  dataset <- as.matrix(dataset)
  X.i <-  matrix(dataset[, 1], ncol = 1)
  Y <- dataset[, nc]
  offs <- as.vector(as.matrix(dataset[, 2:(nc - 1)]) %*% B.shrunk[-1])
  new.int <- glm.fit(x=X.i, y=Y, family = binomial(link = "logit"),
                    offset = offs)$coefficients
  if (new.int > 10) new.int <- coefs[1]
  c(new.int, B.shrunk[-1])
}

get_lambda <- function(x){
  UseMethod("get_lambda")
}

get_lambda.default <- function(mod){
  LL1 <- logLik(mod)
  LL0 <- update(mod, ". ~ 1") %>%
    logLik()
  LR <- -2 * (LL0 - LL1)
  df <- mod$df.null - mod$df.residual
  (LR - df) / LR
}

get_lambda.mira <- function(mod){
  purrr::map_dfc(getfit(mod), get_lambda.default) %>%
    rowMeans()
}

#' @export
compute_sens_spe <- function(pred_obs, range = seq(0.1, 1, by=0.1)){
  purrr::map_dfr(range, function(x){
    m <- filter(pred_obs, D == 1)
    nm <- filter(pred_obs, D == 0)
    t <- filter(m, M >= x) %>%
      nrow()
    nt <- filter(nm, M <= x) %>%
      nrow()
    sen <- binom.test(t, nrow(m)) %>% broom::tidy()
    spe <- binom.test(nt, nrow(nm)) %>% broom::tidy()
    tibble(cutoff = x,
           sensitivity = sprintf_number_table("%s [%s - %s]", pourcent(sen$estimate),
                                              pourcent(sen$conf.low), pourcent(sen$conf.high)),
           specificity = sprintf_number_table("%s [%s - %s]", pourcent(spe$estimate),
                                              pourcent(spe$conf.low), pourcent(spe$conf.high)))
  })
}
