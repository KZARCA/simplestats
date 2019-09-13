modify_boot_formula <- function(tab, model_base){
  escaped <- attr(model_base$terms ,"term.labels")
  if (any(map_lgl(tab, ~ is.factor(.) && nlevels(.) < 2))){
    zz <- map2(tab, names(tab), function(x, y){
      if (is.factor(x) && nlevels(x) < 2)
        y
    }) %>%
      purrr::compact() %>%
      purrr::flatten_chr()
    escaped <- escaped[!escaped %in% zz]
  }
  if (length(escaped) > 0){
    formule <- ifelse(stringr::str_detect(escaped, "I\\("), sprintf("`%s`", escaped), escaped) %>%
      paste(collapse = " + ") %>%
      paste(".vardep", ., sep = " ~ ") %>%
      as.formula
  } else
    formule <- NULL
}


get_boot <- function(data, indices, progression, model_base, vec.out, nvar_mod) {
  progression(detail = gettext("Computation of Estimates...", domain = "R-simplestats"))
  data %<>%
    dplyr::slice(indices) %>%
    drop_levels

  formule <- modify_boot_formula(data, model_base)
  if (!is.null(formule)){
    mod <- do.call(stats::update, list(object = model_base, formule, data = data))
    up <- extract_from_model(mod, "estimate")
    if (length(vec.out) != length(up)){
      up <- up[match(names(vec.out), names(up))]
    }
    up[seq_len(nvar_mod)] %>%
      c(if("glm" %in% class(model_base)) mod$converged)
  } else NA
}


## Maybe we could do just 1 bootstrap?
get_boot_p <- function(data, indices, progression, model_base, vec.out, nvar_mod){
  progression(detail = gettext("P-values computation...", domain = "R-simplestats"))
  data$.vardep <- data$.vardep[indices]
  data %<>% drop_levels
  formule <- modify_boot_formula(data, model_base)
  if (!is.null(formule)){
    mod <- do.call(stats::update, list(object = model_base, formule, data = data))
    up <- extract_from_model(mod, "statistic")
    if (length(vec.out) != length(up)){
      up <- up[match(names(vec.out), names(up))]
    }
    up[seq_len(nvar_mod)] %>%
      c(if("glm" %in% class(model_base)) mod$converged)
  } else NA
}

get_boot_anova <- function(data, indices, progression, model_base, nvar){
  progression(detail = gettext("Global p-value computation...", domain = "R-simplestats"))
  data$.vardep <- data$.vardep[indices]
  data %<>% drop_levels
  formule <- modify_boot_formula(data, model_base)

  try({
    stats::update(model_base, formule, data = data)%>%
      clean_anova %>% #on enlève l'intercept et les résidus
      extract2("statistic")
  }, silent = FALSE)[seq_len(nvar)]
}

#' Extract CI and p-value from bootstraps
#'
#' @param resBoot an object of type boot returning coef
#' @param resBoot_p an object of type boot returning the test statistic
#'
#' @return A 2-items list:
#'   type_ci: indicating the type of Confidence Interval computed
#'   tabRet: the data frame
#' @export
#'
#' @examples
get_confint_p_boot <- function(resBoot, resBoot_p){
  ci <- NULL
  suppressWarnings(ICMB <- confint(resBoot$model_base))
  pMB <- extract_from_model(resBoot$model_base, "p.value")
  R <- resBoot$R
  l <- length(resBoot$t0)*2
  ok_bca <- isTRUE(nrow(resBoot$data) < 1000 & resBoot$R >=1000)
  if((!is.null(shiny::getDefaultReactiveDomain()))){
    shiny::withProgress(message = gettext("Confidence Intervals Computation", domain = "R-simplestats"), value = 0, {
      ci <-
        map(seq_along(resBoot$t0), function(i){
          shiny::incProgress(1/l,detail = names(resBoot$t0)[i])
          b <- boot::boot.ci(resBoot, type = ifelse(ok_bca, "bca", "perc"), index = i)
          shiny::incProgress(1/l)
          if (!is.null(b)) {
            if (ok_bca) b$bca[4:5]
            else b$perc[4:5]
          }
        }) %>%
        purrr::reduce(rbind)
    })
  } else {
    ci <-
      map(seq_along(resBoot$t0), function(i){
        b <- boot::boot.ci(resBoot, type = ifelse(ok_bca, "bca", "perc"), index = i)
        if (!is.null(b)) {
          if (ok_bca) b$bca[4:5]
          else b$perc[4:5]
        }
      }) %>%
      purrr::reduce(rbind)
  }
  type_ci <- ifelse(ok_bca, "bca", "perc")

  p <- map_dbl(seq_along(resBoot_p$t0), function(i){
    t <- resBoot_p$t[, i, drop = TRUE]
    sum(abs(t) > abs(resBoot_p$t0[i]), na.rm = TRUE) %>%
      magrittr::add(1) %>%
      divide_by(sum(!is.na(t)) + 1)
  })

  tableRet <- if (length(resBoot$t0) == 1) matrix(c(resBoot$t0, ci, p), nrow = 1, dimnames = list(names(resBoot$t0), NULL))
  else cbind(resBoot$t0, ci, p)

  term <- rownames(tableRet)
  tableRet %<>% as.data.frame
  names(tableRet) <- c("estimate", "conf.low", "conf.high", "p.value")
  tableRet %<>% tibble::add_column(term = term, .before = "estimate")
  return(list(type_ci = type_ci, tableRet = tableRet))
}





#' Perform bootstraps
#'
#' @param tab A tab
#' @param R Number of replications of the bootstrap
#' @param model_base The base model
#' @param nCPU The number of CPU used in parallel processing
#' @param updateProgress Function to show when used in Shiny
#' @param var_ajust The adjustment variables of the model
#'
#' @return An object of class boot
#'
#' @examples
#'
#'
#' @export
#' @describeIn get_resBoot Performs bootstrap to further compute the confidence interval
get_resBoot <- function(tab, R, model_base, nCPU, updateProgress = function(detail) detail, var_ajust = NULL){
  set.seed(124567)
  vec.out <- extract_from_model(model_base, "estimate")
  nvar_mod <- get_nvar_mod(tab, var_ajust)
  resBoot <- boot::boot(tab, get_boot, R, progression = updateProgress, model_base = model_base,
                  vec.out = vec.out, nvar_mod = nvar_mod, parallel = "multicore", ncpus = nCPU)
  if ("glm" %in% class(model_base)){
    l = dim(resBoot$t)[2]
    resBoot$t <- resBoot$t[resBoot$t[, l, drop = TRUE] == TRUE, -l, drop = FALSE] # on ne garde que ceux qui ont convergé + suppression dernière colonne
    resBoot$R <- dim(resBoot$t)[1]
    resBoot$t0 <- resBoot$t0[-length(resBoot$t0)]
  }
  resBoot$model_base <- model_base
  resBoot$data <- resBoot$data[!names(resBoot$data) %in% var_ajust]
  resBoot
}

#' @export
#' @describeIn get_resBoot Permutation test for p-values
get_resBoot_p <- function(tab, R, model_base, nCPU, updateProgress = function(detail) detail, var_ajust = NULL){
  set.seed(124567)
  vec.out <- extract_from_model(model_base, "estimate")
  nvar_mod <- get_nvar_mod(tab, var_ajust)
  resBoot_p <- boot::boot(tab, get_boot_p, R, progression = updateProgress, model_base = model_base, vec.out = vec.out, nvar_mod = nvar_mod, parallel = "multicore", ncpus = nCPU, sim = "permutation")
  if ("glm" %in% class(model_base)){
    l = dim(resBoot_p$t)[2]
    resBoot_p$t <- resBoot_p$t[resBoot_p$t[, l, drop = TRUE] == TRUE, -l, drop = FALSE]
    resBoot_p$R <- dim(resBoot_p$t)[1]
    resBoot_p$t0 <- resBoot_p$t0[-length(resBoot_p$t0)]
  }
  resBoot_p
}

#' @export
#' @describeIn get_resBoot Permutation test for anova p-values
get_resBoot_anova <- function(tab, R, model_base, nCPU, updateProgress = function(detail) detail, var_ajust = NULL){
  set.seed(124567)
  xlevels <- model_base$xlevels %>%
    magrittr::extract(!names(model_base$xlevels) %in% var_ajust)

  if(!is.null(xlevels) && any(map_lgl(xlevels, ~ length(.) > 2))){
    resBoot_anova <- boot::boot(tab, get_boot_anova, R, progression = updateProgress, model_base = model_base, nvar = ncol(tab) - (length(var_ajust) + 1), parallel = "multicore", ncpus = nCPU, sim = "permutation")
    resBoot_anova$tab_anova_base <- clean_anova(model_base) %>%
      dplyr::filter(!variable %in% var_ajust)
    resBoot_anova
  } else
    return(NULL)
}

#' Print all bootstraps
#'
#' Computes all necessary bootstraps
#'
#' @param model_base The base model
#' @param var_ajust The adjustment variables of the model
#' @param nCPU The number of CPU used in parallel processing
#' @param R Number of replications of the bootstraps
#'
#' @return A data frame with coefficients, p-values and global p-values (computed by ANOVA)
#' @export
#'
#' @examples
print_all_boots <- function(model_base, var_ajust = NULL, nCPU = 2, R = NULL){
  tab <- model.frame(model_base)
  names(tab)[1] <- ".vardep"
  if (is.null(R)) R <- tab %>% nrow() %>% nearest_up_thousand()
  resBoot <- get_resBoot(tab, R, model_base, nCPU, var_ajust = var_ajust)
  resBoot_p <- get_resBoot_p(tab, R, model_base, nCPU, var_ajust = var_ajust)
  resBoot_anova <- get_resBoot_anova(tab, R, model_base, nCPU, var_ajust = var_ajust)
  tab_ci <- get_confint_p_boot(resBoot, resBoot_p)
  if (!is.null(resBoot_anova)) {
    cbind(tab_ci$tableRet, p.global = extract_pval_glob(resBoot_anova))
  } else {
    tab_ci$tableRet
  }
}
