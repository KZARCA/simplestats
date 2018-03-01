#' simplestats: Stats made simple
#'
#' `simplestats` is a R toolset accompanying pvalue.io
#'
#' @docType package
#' @name simplestats
#'
#'
globalVariables(c(".", ".time", "term", "variable", "group_size", "..count..", ".n",
                  "test", "p.global", "niveau", "p", "id"))
#' @importFrom utils read.csv
#' @importFrom stats anova
#' @importFrom stats as.formula
#' @importFrom stats chisq.test
#' @importFrom stats coef
#' @importFrom stats confint
#' @importFrom stats deviance
#' @importFrom stats df.residual
#' @importFrom stats ecdf
#' @importFrom stats fisher.test
#' @importFrom stats kruskal.test
#' @importFrom stats lm
#' @importFrom stats median
#' @importFrom stats model.frame
#' @importFrom stats na.exclude
#' @importFrom stats pchisq
#' @importFrom stats quantile
#' @importFrom stats rstudent
#' @importFrom stats setNames
#' @importFrom stats t.test
#' @importFrom stats var
#' @importFrom stats sd
#' @importFrom stats wilcox.test
#' @importFrom stats reformulate
#' @importFrom stats reorder
#' @importFrom stats relevel
#' @importFrom stats termplot
#' @importFrom stats line
#' @importFrom dplyr group_by
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select_if
#' @importFrom dplyr summarise
#' @importFrom dplyr slice
#' @importFrom dplyr rename
#' @importFrom dplyr one_of
#' @importFrom dplyr mutate_if
#' @importFrom dplyr n
#' @importFrom dplyr bind_cols
#' @importFrom purrr map
#' @importFrom purrr map2
#' @importFrom purrr map_dbl
#' @importFrom purrr map_lgl
#' @importFrom purrr compact
#' @importFrom purrr flatten_chr
#' @importFrom purrr flatten_dbl
#' @importFrom purrr as_vector
#' @importFrom purrr map_if
#' @importFrom purrr map2_chr
#' @importFrom purrr map2_df
#' @importFrom purrr map_chr
#' @importFrom purrr map_df
#' @importFrom purrr walk
#' @importFrom purrr modify_if
#' @importFrom boot boot
#' @importFrom boot boot.ci
#' @importFrom survival coxph
#' @importFrom survival survdiff
#' @importFrom survival survfit
#' @importFrom survival Surv
#' @importFrom survival cox.zph
#' @importFrom graphics abline
#' @importFrom graphics plot
#' @importFrom shiny getDefaultReactiveDomain
#' @importFrom shiny withProgress
#' @importFrom shiny incProgress
#' @importFrom shiny modalDialog
#' @importFrom shiny modalButton
#' @importFrom car vif
#' @importFrom broom tidy
#' @importFrom broom fix_data_frame
#' @import mice
#' @importFrom stringi stri_enc_isutf8
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_trunc
#' @importFrom Hmisc impute
#' @importFrom Hmisc label
#' @importFrom Hmisc label<-
#' @importFrom Hmisc capitalize
#' @importFrom tibble tibble
#' @importFrom tibble as_tibble
#' @importFrom tibble add_column
#' @importFrom rlang sym
#' @importFrom rlang ":="
#' @importFrom rlang "%||%"
#' @importFrom forcats fct_drop
#' @importFrom readxl read_excel
#' @importFrom GGally ggsurv
#' @importFrom arm bayesglm
#' @importFrom mgcv gam
#' @importFrom mgcv cox.ph
#' @importFrom splines ns
#' @import ggplot2
#' @import magrittr
NULL
