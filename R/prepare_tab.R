make_correspondance <- function(tab, trunc = TRUE, length = 40){
  df <- tibble(noms = names(tab))
  mutate(df,
         index = dplyr::row_number(),
         present = noms %in% names(remove_na_cols(tab)),
         correspondance = standardize_names_basic(noms),
         labs = standardize_names(noms, trunc = trunc, length = length))
}

remove_na_rows <- function(tab){
  na_rows <- which(rowSums(is.na(tab)) == ncol(tab))
  if (length(na_rows)){
    return(tab[-na_rows, , drop = FALSE])
  }
  return(tab)
}


remove_na_cols <- function(tab){
  na_cols <- which(colSums(is.na(tab)) == nrow(tab))
  if (length(na_cols)){
    return(tab[-na_cols])
  }
  return(tab)
}

factor_strings <- function(tab){
  as.data.frame(
    lapply(tab, function(x) {
      if(inherits(x, "POSIXct")) {
        return(as.character(x))
      }
      if (is.numeric(x) && min(x, na.rm = TRUE) > 1E5) x <- as.character(x)
      if(is.character(x) | is.factor(x)) {
        b <- factor(x)
        if (nlevels(b) >= 10L) return(as.character(x))
        x <- reorder(b, b, function(y)-length(y))
        levels(x) %<>% str_trunc(20)
      } else if (count_items(x) < 5){
        x <- factor(x)
        levels(x) %<>% str_trunc(20)
      }
      if (is.factor(x) & nlevels(x) < 2 | is.numeric(x) & count_items(x) < 2){
        return(as.character(x))
      }
      x
    }), stringsAsFactors = FALSE
  )
}

lower_tab <- function(tab){
  as.data.frame(
    lapply(tab, function(x) {
      if(is.character(x) | is.factor(x)) {
        if (!all(grepl("^[A-Z]+$", x), na.rm = TRUE)) {
          iconv(x, sub = "") #%>%
          #tolower()
        } else x
      }
      else x
    }), stringsAsFactors = FALSE
  )
}

replace_virgules <- function(tab){
  as.data.frame(
    lapply(tab, function(x) {
      if (all(grepl("^-?[0-9]*[,\\.]?[0-9]*$", x) | is.na(x), na.rm = TRUE)){
        x <- suppressWarnings(as.numeric(gsub(",", ".", x)))
      }
      else
        x
    }), stringsAsFactors = FALSE
  )
}

transform_date <- function(tab){
  possibleFormats <- cbind(
    c(
      "^[0-3][0-9]/[0-1][0-9]/[1-2][0-9][0-9][0-9]$",
      "^[0-3][0-9]/[0-1][0-9]/[0-9][0-9]$",
      "^[0-3][0-9]-[0-1][0-9]-[1-2][0-9][0-9][0-9]$",
      "^[0-3][0-9]-[0-1][0-9]-[0-9][0-9]$",
      "^[1-2][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9]$"
    ),
    c("%d/%m/%Y", "%d/%m/%y", "%d-%m-%Y", "%d-%m-%y", "%Y-%m-%d")
  )
  lapply(seq_along(tab), function(c) {
    column <- tab[[c]]
    walk(seq_len(nrow(possibleFormats)), function(i){
      format = possibleFormats[i, 1]
      if (any(grepl(format, column))) {
        conv <- map_dbl(column, function(cell){
          suppressWarnings(num <- as.numeric(cell))
          if (grepl(format, cell))
            as.Date(cell, format = possibleFormats[i, 2])
          else if (!is.na(num))
            as.Date(num, origin = "1899-12-30")
          else
            NA
        })

        tab[[c]] <<- as.Date(base::unname(conv), origin = "1970-01-01")
      }
    })
  })
  return(tab)
}


#' Create better colnames
#'
#' @param names character vector
#'
#' @return nice names vectors
#' @export
#'
#' @examples
standardize_names <- function(names, trunc = FALSE, length = 40){
  noms <- standardize_names_basic(names) %>%
    str_replace_all("_", " ") %>%
    str_replace("^L ","L'") %>%
    str_replace_all(" l "," l'") %>%
    str_replace("^C ","C'") %>%
    str_replace_all(" c "," c'") %>%
    str_replace("^D ","D'") %>%
    str_replace_all(" d "," d'") %>%
    str_replace_all("\n"," ") %>%
    trimws() %>%
    capitalize()
  if (trunc == TRUE){
    noms %<>% str_trunc(length)
  }
  noms
}

standardize_names_basic <- function(names){
  remove_multibyte_if_any(names) %>%
    trimws() %>%
    str_replace("^[\"\'](.*)[\"\']$", "\\1") %>%
    str_replace_all("[\\.]+"," ") %>%
    str_replace_all("[ ]+", " ") %>%
    str_replace_all("[\\[\\]]+", " ") %>%
    trimws()
}

remove_guillemets <- function(tab){
  mutate_if(tab, function(p) is.factor(p) & any(str_detect(p, "^[\"\'](.*)[\"\']"), na.rm = TRUE), function(x){
    re <- str_replace(x, "^[\"\'](.*)[\"\']", "\\1")
    suppressWarnings({
      if (!any(is.na(as.numeric(na.exclude(re))))){
        as.numeric(re)
      } else as.factor(re)
    })
  })
}

replace_infinite <- function(tab){
 lapply(tab, function(x){
   x[is.infinite(x)] <- NA
   x
 }) %>%
    as.data.frame(stringAsFactors = FALSE)
}


#' Prepare a data.frame for analysis
#'
#' This function \itemize{
#'    \item creates a label for each columns, allowing for a better display,
#'    \item removes empty cells on top of the data.frame
#'    \item standardize the dates
#'    \item lowers case of levels for character vectors
#'    \item factors qualitative variables
#'    \item removes quoting characters
#'}
#' @param tab the data.frame
#'
#' @return A standardized data.frame
#' @export
#'
#' @examples
standardize_tab <- function(tab){
  tab %<>% remove_na_rows() %>%
    remove_na_cols()
  tab <- tab[!is.na(names(tab)) & names(tab) != ""]
  labs <- standardize_names(names(tab), trunc = TRUE)
  names(tab) <- standardize_names_basic(names(tab))

  tab %<>%
    replace_infinite() %>%
    remove_na_rows() %>%
     replace_virgules() %>%
     transform_date() %>%
     lower_tab() %>%
     factor_strings() %>%
     remove_guillemets()

  names(tab) <- gsub("\\.", "_", names(tab))

  label(tab, self = FALSE) <- labs
  return(tab)
}

#' Add column .time
#'
#' Adds the column .time, which is the follow-up time
#' @param tab The data frame
#' @param vardep The dependent variable
#' @param passage If 1, The first level of the event variable is the absence of event; if 2, the second level is the absence of event
#' @param typeCensure If 1: Follow-up time computed from 2 dates: inclusion and censor; \
#' If 2: the follow-up time is already inside a column
#' @param dateInclusion Date vector of length equals to the number of rows of tab: inclusion date.
#' @param dateSortie Date vector of length equals to the number of rows of tab: censor or event.
#' @param var_time Name of the column indicating the follow-up time follow-up

#' @details This function requires either var_time or dateSortie and dateInclusion
#' @return A table with .time column
#' @export
#'
#' @examples
make_tab_survival <- function(tab, vardep, passage = 1, typeCensure = 2, dateInclusion = NULL,
                              dateSortie = NULL, var_time = NULL, limit = NULL){
  lev <- levels(tab[[vardep]])
  tab[[vardep]] <- relevel(tab[[vardep]], ref=ifelse(passage == 1, lev[1], lev[2]))
  lev <- levels(tab[[vardep]])
  #attr(tab[[vardep]], "scores") <- -table(tab[[vardep]])
  if (typeCensure == 1) {
    tab$.time <- as.numeric(dateSortie - dateInclusion)
  } else if (typeCensure == 2) {
    tab$.time <- tab[[var_time]]
    tab %<>% select(-one_of(var_time))
  }
  exLabel <- label(tab[[vardep]])
  tab[[vardep]] %<>%
    as.numeric %>%
    subtract(1)
  if(!is.null(limit) && !is.na(limit)){
    time2 <- pmin(limit, tab$.time)
    tab[[vardep]] <- ifelse(tab[[vardep]] == 1 & tab$.time > time2 | tab[[vardep]] == 0, 0, 1)
    tab$.time <- time2
  }
  label(tab[[vardep]]) <- exLabel
  tab
}


#' Create intermediate table
#'
#' @param tab a data frame
#' @param type a character vector. Possible values : "desc", "expl" and "pred"
#'
#' @return a curated data frame
#' @export
create_tabi <- function(tab, type, keep = NULL){
  tf <- Filter(function(x) count_items(x) > 1 & !inherits(x, "Date") & !is.character(x), tab)
  if (type == "desc"){
    return(tf)
  }
  if (type == "expl"){
    Filter(function(x) get_propDM(x) <= 0.2, tf)
  } else if (type == "pred"){
    if (ncol(tf) > 1){
    elimine <- get_large_missing(tf)
    filtered <- tf[setdiff(names(tf), elimine)]
    filtered <- Filter(function(x) get_propDM(x) <= 0.2, filtered)
    if (!is.null(keep)) tf[union(keep, names(filtered))] else filtered
    } else tf
  }
}

#' @export
homogeneise_tab <- function(tab, before, after){
  purrr::walk2(before, after, function(x, y){
    if (is.factor(tab[[x]]) & !is.factor(tab[[y]]) | !is.factor(tab[[x]]) & is.factor(tab[[y]])) {
      exlab <- label(tab[[x]])
      tab[[x]] <<- as.numeric(as.character(tab[[x]]))
      label(tab[[x]]) <<- exlab
      exlab <- label(tab[[y]])
      tab[[y]] <<- as.numeric(as.character(tab[[y]]))
      label(tab[[y]]) <<- exlab
    }
  })
  tab[c(before, after)]
}
