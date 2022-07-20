create_table_forestplot <- function(mod, varajust = NULL, inv = FALSE){

  tab_model <- create_table_model(mod, varajust = varajust) %>%
    mutate(niveau = gsub(" vs.*$", "", niveau)) %>%
    select(id:p.value)

  if (inv){
    tab_model$estimate <- -tab_model$estimate
    tmp_conf.low <- tab_model$conf.low
    tab_model$conf.low <- -tab_model$conf.high
    tab_model$conf.high <- -tmp_conf.low
  }

  tab_n <- purrr::map_dfr(unique(tab_model$id), function(x){
    create_ligne_desc_export(mod$data[[x]], x, show_prop = FALSE) %>%
      mutate(prop = as.numeric(n)/sum(as.numeric(n), na.rm = TRUE)) %>%
      {if (!"niveau" %in% names(.)) mutate(., niveau = "") else .} %>%
      mutate(n = as.integer(n)) %>%
      select(id, variable, niveau, n, prop)
  })



  dplyr::left_join(tab_n, tab_model, by = c("id", "niveau", "variable")) %>%
    filter(!id %in% varajust) %>%
    structure(class = class(tab_model))
}


prepare_forestplot <- function(tab_mod, ...){
  exClass <- class(tab_mod)[1]
  if (inherits(tab_mod, "tabglm") | inherits(tab_mod, "tabcoxph")){
    fun = exp
  } else fun = function(x) x

  all_facs <- filter(tab_mod, niveau != "")
  facs <- all_facs %>% pull(id) %>% unique()
  nm <- all_facs %>% pull(variable) %>% unique()

  for (i in seq_along(facs)){
    tab_mod <- tibble::add_row(tab_mod, tibble(id=facs[i], variable=nm[i]), .before = which(tab_mod$id == facs[i]))
  }

  tab_mod %>%
    dplyr::group_split(id) %>%
    purrr::map_dfr(~ .x %>%
                     tibble::add_row(id = NA, .before = 1)) %>%
    mutate(
           variable = ifelse(!is.na(multiple) & multiple != 1, sprintf("%s (+%s)", variable, multiple), variable),
           variable = ifelse(is.na(niveau) | niveau == "", variable, sprintf("   %s", niveau)),
           estimate = fun(case_when(!is.na(estimate) ~ estimate,
                                    is.na(n) ~ NA_real_,
                                    TRUE ~ 0)),
           conf.low = fun(case_when(!is.na(conf.low) ~ conf.low,
                                    is.na(n) ~ NA_real_,
                                    TRUE ~ 0)),
           conf.high = fun(case_when(!is.na(conf.high) ~ conf.high,
                                     is.na(n) ~ NA_real_,
                                     TRUE ~ 0)),
           formatted_estimates = case_when(!is.na(estimate) & !is.na(p.value) ~ sprintf_number_table("%s [%s; %s]", estimate, conf.low, conf.high),
                                           !is.na(estimate) ~ "Reference",
                                           TRUE ~ NA_character_),
           n = n,
           n_prop = ifelse(prop == 1, n, sprintf_number_table("%s (%s)", n, pourcent(prop, symbol = FALSE))),
           p = format_pval(p.value, keepNA = TRUE)) %>%
      dplyr::select(variable, estimate, conf.low, conf.high, n, n_prop, formatted_estimates, p) %>%
    add_class(exClass)
}

#' @export
plot_forest <- function(mod, varajust = NULL, ...){
  .dots <- list(...)
  inv = .dots$inv %||% FALSE
  tab_mod <- create_table_forestplot(mod, varajust = varajust, inv) %>%
    prepare_forestplot()
  show_estimate <- .dots$show_estimate %||% TRUE
  title_n <- .dots$title_n %||% "N (%)"
  show_ticks <- .dots$show_ticks %||% TRUE
  style_box <- .dots$style_box %||% "normal"
  style_box <- getFromNamespace(sprintf("fpDraw%sCI", capitalize(style_box)), "forestplot")

  gpar <- grid::gpar
  lower <- min(tab_mod$conf.low, na.rm = TRUE)
  upper <- max(tab_mod$conf.high, na.rm = TRUE)

  if (inherits(tab_mod, "tabglm") | inherits(tab_mod, "tabcoxph")){
    fun <- exp
    # breaks <- exp(seq(-2,2, by = 0.2))
    # breaks <- round(breaks, ifelse(breaks < 0.8, 2,1))
    xlog <- TRUE
    if(inherits(tab_mod, "tabglm")){
      estimate_name <- "Odds Ratio"
    } else {
      estimate_name <- "Hazard Ratio"
    }
    breaks <- seq(log(lower), log(upper), by = 0.2)
    min_ci <- fun(.dots$min_ci %||% min(breaks))
    max_ci <- fun(.dots$max_ci %||% max(breaks))
    min_ci <- max(min_ci, 1E-3)
    breaks <- fun(breaks)

  } else {
    fun <- function(x) x
    estimate_name <- "Coefficients"
    xlog <- FALSE
    step <- case_when(
      upper-lower > 5 ~ 1,
      upper-lower > 2 ~ 0.5,
      upper-lower > .5 ~ 0.25,
      TRUE ~ 0.1)
    breaks <- seq(lower %/% step * step, (upper %/% step + 1) * step, step)
    min_ci <- .dots$min_ci %||% min(breaks)
    max_ci <- .dots$max_ci %||% max(breaks)
    if (length(breaks) > 10){
      breaks <- breaks[-c(1, length(breaks))]
    }
  }
  breaks <- round(breaks, ifelse(breaks < 0.8, 2,1))


  clip <- c(min_ci, max_ci)
  breaks <- breaks[breaks >= min_ci & breaks <= max_ci]
  breaks <- unique(c(breaks, fun(0)))
  # breaks <- if(length(breaks) < 2) {
  #   if(xlog) c(0.8,1,1.2) else c(-.2,0,.2)
  # } else breaks

  headers <- c(NA, title_n, if(show_estimate) estimate_name, "p")

  cols <- c("variable", "n_prop", if (show_estimate) "formatted_estimates", "p")


  text <- tab_mod[cols] %>%
    as.matrix()

  text[1, ] <- headers
  nvars <- get_nvar_mod(if(inherits(mod, "mira")) getfit(mod,1)$model else mod$model,
                        remove1 = FALSE)

  structure(forestplot::forestplot(tab_mod, mean = estimate, lower = conf.low, xlog = xlog,
                         upper = conf.high, labeltext = text, boxsize = 0.3,
                         is.summary = c(TRUE, rep(FALSE, nrow(tab_mod) -1)),
                         graph.pos = 3, hrzl_line = TRUE, ci.vertices = show_ticks,
                         lwd.ci=1, ci.vertices.height = 0.2,
                         clip = clip,
                         graphwidth = unit(ifelse(nvars < 6, 0.3, 0.5), "npc"),
                         align="l",
                         txt_gp = forestplot::fpTxtGp(cex = 1.1, summary= gpar(cex = 1.1), ticks = gpar(cex = .9)),
                         col=forestplot::fpColors(box="black", lines="black", zero = "gray70"),
                         xticks = breaks,
                         colgap = unit(0.02, "npc"),
                         fn.ci_norm = style_box,
                         lineheight = unit(8, "mm")
                         ),
            class = "gforge_forestplot",
            nvars = nvars)
}
