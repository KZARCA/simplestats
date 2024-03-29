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


modify_mod_shrunk <- function(mod, shrunk = numeric(0)){
  if (length(shrunk)){
    if (inherits(mod, "mira")){
      mod$analyses <- map(mod$analyses, function(x){
        x$coefficients <- shrunk
        x$linear.predictors <- model.matrix(x) %*% shrunk
        x
      })
    } else {
      mod$coefficients <- shrunk
      mod$linear.predictors <- model.matrix(mod) %*% shrunk
    }
  }
  mod
}

#' Creates a contingency table with predicted probabilities and actual classification
#' @param mod a glm or mira object
#'
#' @return data.frame
#' @export
create_pred_obs <- function(mod, tab = NULL, vardep = NULL, as_prob = TRUE, shrunk = numeric(0)){
  modify_mod_shrunk(mod, shrunk)
  if (is.null(tab)){
    pred <- predict(mod)
    label <- if (inherits(mod, "mira")) {
      mice::complete(mod$tab)[as.numeric(names(pred)), ][[1]]
    } else {
      mod$model[[1]]
    }
  } else {
    tab <- remove_missing_levels(tab, mod)
    label <- tab[[vardep]]
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
                          updateProgress = function(detail) detail = ""){
  set.seed(1234567)
  if (type_validation == "cv"){
    cv <- get_cv_auc(tab, vardep, varindep, type, n = min(10, floor(get_min_class(tab, vardep, type)/12)), progression = updateProgress)
    m <- map(cv, 1)  %>%
      compact() %>%
      list_c() %>%
      mean(na.rm = TRUE)
    auc_tot <- calculate_auc(create_pred_obs(mod))

    lambda <- get_lambda(mod) #heuristic formula (see Steyenberg)
    shrunk <- get_shrunk_coef(mod, lambda)
    return(
      structure(list(auc=auc_tot, mean = m, shrunk = shrunk, error = sum(map_dbl(cv, 2) / length(map_dbl(cv, 2)))),
                type_validation = "cv")
    )
  }
  res <- boot(tab, boot_auc, R = R, vardep = vardep, varindep = varindep,
       type = type, progression = updateProgress, parallel = "multicore", ncpus = nCPU)
  m <- res$t0[1]

  # Calculate the optimism (step 5) : bootstrap performance - test performance
  opti <- mean(res$t[, 1] - res$t[, 2], na.rm = TRUE)
  ci <- boot.ci(res, type = "basic", index = 1)$basic[4:5]
  lambda <- mean(res$t[, 3], na.rm = TRUE)
  shrunk <- get_shrunk_coef(mod, lambda)
  return(
    structure(
      list(mean = m, ci = ci, optimism = opti, shrunk = shrunk, error = sum(res$t[, 4])/length(res$t[, 4])),
      type_validation = "boot"
    )
  )
}

get_cv_auc <- function(tab, vardep, varindep = NULL, type = "logistic", n = 10, progression = function() cat("=")){
    tabs <- split_cv(tab, n)
    map(seq_along(tabs), function(i){
      progression()
      train <- do.call(rbind, tabs[-i]) %>%
        create_tabi("pred", keep = c(vardep, varindep))
      error_lasso <- 0
      varajust <- get_lasso_variables(train, vardep, varindep, type)
      if (identical(varajust, "ERROR_MODEL")){
        varajust <- character(0)
        return(list(NULL, 1))
      } else if (length(varajust)){
        el <- recherche_multicol(train, vardep, varindep, varajust, type, pred = TRUE)
        varajust <- if (identical(el, "ERROR_MODEL")) character(0) else remove_elements(varajust, el)
      }
      varaux <- if (get_propDM(train[c(vardep, varindep, varajust)]) > 0.05){
        find_varaux(train, vardep, varindep, varajust, type)
      }

      train <- train[c(vardep, varindep, varajust, varaux)]
      if (ncol(train) < 2){
        return(list(NULL, 1))
      }
      results <- if (identical(el, "ERROR_MODEL2")){
        compute_mod(train, vardep, character(0), varajust, type, pred = 2)
      } else {
        compute_mod(train, vardep, intersect(names(train), varindep), varajust, type, pred = 2)
      }
      if (!is_warning(results$mod)) {
        AUC <- create_pred_obs(results$mod) %>%
          calculate_auc()
      } else {
        AUC <- NULL
        error_lasso <- 1
      }
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
  try2(plot_ROC(pred_obs, thresholds = NA) %>%
  plotROC::calc_auc() %>%
  extract2("AUC"), warnings = "The following aesthetics")
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
    return(c(NA, NA, NA, 1))
  } else if (length(varajust)){
    el <- recherche_multicol(train, vardep, intersect(names(train), varindep), varajust, type, pred = TRUE)
    varajust <- if (identical(el, "ERROR_MODEL")) character(0) else remove_elements(varajust, el)
  }

  varaux <- if (get_propDM(train[c(vardep, varindep, varajust)]) > 0.05){
    find_varaux(train, vardep, varindep, varajust, type)
  }

  train <- train[c(vardep, varindep, varajust, varaux)]

  if (ncol(train) < 2){
    return(c(NA, NA, NA, 1))
  }

  results <- if (identical(el, "ERROR_MODEL2")){
    compute_mod(train, vardep, character(0), varajust, type, pred = 2)
  } else {
    compute_mod(train, vardep, intersect(names(train), varindep), varajust, type, pred = 2)
  }
  perf_boot <-  perf_test <- shrinkage_factor <- NULL
  if (!is_warning(results$mod)){
  # Model performance on the sample (step 3 p95 Steyerberg Clinical Prediction Models)
    pred_obs_boot <- create_pred_obs(results$mod, as_prob = FALSE) # Linear Predictor
    perf_boot <- calculate_auc(pred_obs_boot)
    # Model performance on the data (step 4)
    pred_obs_test <- create_pred_obs(results$mod, data[c(vardep, varindep, varajust)], vardep, as_prob = FALSE)
    perf_test <- calculate_auc(pred_obs_test)
    shrinkage_factor <- glm(D ~ M, data = pred_obs_test, family="binomial") %>%
      coef() %>%
      extract2(2)
  } else {
    return(c(NA, NA, NA, 1))
  }
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
  Y <- dataset[, nc]
  X.i <-  matrix(dataset[, 1], ncol = 1)

  offs <- as.vector(as.matrix(dataset[, 2:(nc - 1)]) %*% B.shrunk[-1])
  new.int <- glm.fit(x=X.i, y=Y, family = binomial(link = "logit"),
                                offset = offs)$coefficients

  if(abs(new.int) > 10) return(coefs)
  c(new.int, B.shrunk[-1])
}

get_lambda <- function(x, ...){
  UseMethod("get_lambda")
}

get_lambda.default <- function(mod){
  LL1 <- logLik(mod)
  LL0 <- update(mod, ". ~ 1", data = mod$data) %>%
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
compute_sens_spe <- function(pred_obs, thresholds = seq(0.1, 1, by=0.1), format = TRUE){
  purrr::map_dfr(thresholds, function(x){
    m <- filter(pred_obs, D == 1)
    nm <- filter(pred_obs, D == 0)
    t <- filter(m, M >= x) %>%
      nrow()
    nt <- filter(nm, M <= x) %>%
      nrow()
    sen <- binom.test(t, nrow(m)) %>% broom::tidy()
    spe <- binom.test(nt, nrow(nm)) %>% broom::tidy()

    res <- if (format){
      tibble(threshold = x,
             sensitivity = sprintf_number_table("%s [%s - %s]", pourcent(sen$estimate),
                                              pourcent(sen$conf.low), pourcent(sen$conf.high)),
             specificity = sprintf_number_table("%s [%s - %s]", pourcent(spe$estimate),
                                              pourcent(spe$conf.low), pourcent(spe$conf.high)))
    } else tibble(threshold = x,
                  sensitivity = sen$estimate,
                  specificity = spe$estimate)
  })
}



#' @export
prepare_pred <- function(y, x, boot = TRUE){
  tab_pred <- structure(tibble(M = x, D = as.numeric(y)-1), rev = FALSE)
  if (calculate_auc(tab_pred) < 0.5) {
    tab_pred <- structure(mutate(tab_pred, D = 1-D), rev = TRUE)
  }
  tab_pred
}

standardize_beta <- function(mod){
  if (inherits(mod, "mira")){
    mod <- getfit(mod, 1)
  }
  y <- mod$y
  x <- model.matrix(mod)[,-1, drop=FALSE]
  x <- cbind(1,apply(x, 2, scale))
  std <- do.call(paste(class(mod)[1], "fit", sep = "."),
          c(list(x = x, y = y),
            if(!is.null(mod$family)) list(family = mod$family)))
  return(coef(std)[-1])
}
