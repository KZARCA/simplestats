
remove_na_lines <- function(tab){
  max <- base::which(base::rowSums(is.na(tab)) == ncol(tab))[1]
  if(!is.na(max))
    tab <- tab[1:(max-1),]
  return(tab)
}

factor_strings <- function(tab){
  as.data.frame(
    lapply(tab, function(x) {
      if(is.character(x) | is.factor(x)) {
        b <- factor(x)
        if (nlevels(b) < 10)
          x <- reorder(b, b,function(y)-length(y))
      } else if (length(table(x)) < 5){
        x <- factor(x)
      }
      if (is.factor(x) & nlevels(x) < 2) x <- as.character(x)
      return(x)
    }), stringsAsFactors = FALSE
  )
}

lower_tab <- function(tab){
  as.data.frame(
    lapply(tab, function(x) {
      if(is.character(x)) {
        if (!all(grepl("^[[:upper:]]+$", x), na.rm = TRUE))
          tolower(x)
        else
          x
      }
      else x
    }), stringsAsFactors = FALSE
  )
}

replace_virgules <- function(tab){
  as.data.frame(
    lapply(tab, function(x) {
      if (all(grepl("^-?[0-9]*[,\\.]?[0-9]*$", x) | is.na(x), na.rm = TRUE)){
        x <- as.numeric(gsub(",", ".", x))
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


search_date <- function(tab){
  a <- as.data.frame(
    lapply(tab, function(x) {
      if (!all(is.na(x))) {
        if (all(grepl("^[0-3][0-9]/[0-1][0-9]/[1-2][0-9][0-9][0-9]$",x) | is.na(x),na.rm = TRUE))
          as.Date(x, "%d/%m/%Y")
        else if (all(grepl("^[0-3][0-9]/[0-1][0-9]/[0-9][0-9]$",x) | is.na(x),na.rm = TRUE))
          as.Date(x, "%d/%m/%y")
        else if (all(grepl("^[0-3][0-9]-[0-1][0-9]-[1-2][0-9][0-9][0-9]$",x) | is.na(x),na.rm = TRUE))
          as.Date(x, "%d-%m-%Y")
        else if (all(grepl("^[0-3][0-9]-[0-1][0-9]-[0-9][0-9]$",x) | is.na(x),na.rm = TRUE))
          as.Date(x, "%d-%m-%y")
        else if (all(grepl("^[1-2][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9]$",x) | is.na(x),na.rm = TRUE))
          as.Date(x)
        else
          x
      }
      else
        x
    }), stringsAsFactors = FALSE
  )
}

#' Create better colnames
#'
#' @param names character vector
#'
#' @return nice names vectors
#' @export
#'
#' @examples
standardize_names <- function(names, trunc = FALSE){
  noms <- trimws(names) %>%
    str_replace("^[\"\'](.*)[\"\']$", "\\1") %>%
    str_replace("[\\.]+"," ") %>%
    str_replace("^L ","L'") %>%
    str_replace(" l "," l'") %>%
    str_replace("^C ","C'") %>%
    str_replace(" c "," c'") %>%
    str_replace("^D ","D'") %>%
    str_replace(" d "," d'") %>%
    str_replace("\n"," ") %>%
    capitalize()
  if (trunc == TRUE){
    noms %<>% str_trunc(30)
  }
  noms
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
  tab <- tab[!is.na(names(tab)) & names(tab) != ""]
  labs <- standardize_names(names(tab))

  tab %<>%
    remove_na_lines() %>%
    replace_virgules() %>%
    transform_date() %>%
    lower_tab() %>%
    factor_strings() %>%
    remove_guillemets()


  names(tab) %<>%
    str_replace("^X\\.(.*)\\.$", "\\1") %>%
    stringi::stri_trans_general("latin-ascii") %>%
    make.names(unique = TRUE)

  label(tab, self = FALSE) <- labs
  return(tab)
}

#' Add column .time
#'
#' Adds the column .time, which is the follow-up time
#' @param tab The data frame
#' @param vardep The dependent variable
#' @param passage If 1, The first level of the event variable is the absence of event; if 2, the second level is the absence of event
#' @param dateSortie Name of the column indicating event or censor date
#' @param dateInclusion Name of the column indicating the inclusion date
#' @param var_time Name of the column indicating the follow-up time follow-up

#' @details This function requires either var_time or dateSortie and dateInclusion
#' @return A table with .time column
#' @export
#'
#' @examples
make_tab_survival <- function(tab, vardep, passage, dateSortie = NULL, dateInclusion = NULL, var_time = NULL){
  lev <- levels(tab[[vardep]])
  tab[[vardep]] <- relevel(tab[[vardep]], ref=ifelse(passage == 1, lev[1], lev[2]))
  attr(tab[[vardep]], "scores") <- -table(tab[[vardep]])
  if (!is.null(dateSortie)  && dateSortie != "") {
    tab$.time <- as.numeric(tab[[dateSortie]] - tab[[dateInclusion]])
    tab %<>% select(-one_of(dateSortie, dateInclusion))
  } else if (!is.null(var_time) && var_time != ""){
    tab$.time <- tab[[var_time]]
    tab %<>% select(-one_of(var_time))
  }
  exLabel <- label(tab[[vardep]])
  tab[[vardep]] %<>%
    as.numeric %>%
    subtract(1)
  label(tab[[vardep]]) <- exLabel
  tab
}
