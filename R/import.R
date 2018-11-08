import_delim <- function(file, sep, dec){
  tab <- NULL
  enc <- readr::guess_encoding(file, threshold = .9)$encoding[1]
  if (is.na(enc)) enc <- ""
  if (tolower(tools::file_ext(file)) == "txt" | sep != "\t" | dec != "."){
    tab <- read.delim(file, strip.white = TRUE, fileEncoding = enc,  na.strings=c("NA", "", " ", "."),
                      stringsAsFactors = FALSE, check.names = FALSE, sep = sep, dec = dec)
  } else if (tolower(tools::file_ext(file)) == "csv"){
    tab <- import_csv(file, enc = enc)
  }
  tab <- remove_multibyte_if_any(tab)
  return(tab)
}

import_csv <- function(file, enc = "") {
  tab <- NULL
  try({tab <- read.csv2(file, na.strings=c("NA", "", " ", "."), fileEncoding = enc,
                       strip.white = TRUE, stringsAsFactors = FALSE, check.names = FALSE)})
  #if (firstImport) {
    if(length(tab) == 1 | is.null(tab)){
      try(tab <- read.csv(file, na.strings=c("NA", ""," ", "."), strip.white = TRUE, fileEncoding = enc,
                          stringsAsFactors = FALSE, check.names = FALSE))
    }
  #}
  tab
}

#' Read file
#'
#' import txt, csv and xls files
#' @param file The file to read
#' @param sep The field separator character. Values on each line of the file are separated by this character.
#' @param dec The character used in the file for decimal points.
#'
#' @return A data.frame
#' @export
#'
#' @examples
read_tab_import <- function(file, sep = "\t", dec = "."){
  ext <- tolower(tools::file_ext(file))
  tab <- NULL
  err <- NULL

  if(ext %in% c("csv", "txt")){
    tab <- tryCatch(import_delim(file, sep = sep, dec = dec),
                    error = function(e) e)
    if (is(tab, "error")) {
      if (grepl("type.convert", tab$message)){
        err <- gettext("Unable to load this file because of unreadable characters.", domain = "R-simplestats")
      } else {
        err <- gettext("Unable to load this file.", domain = "R-simplestats")
      }
    }

    if(!is.null(err)){
      return(err)
    }
    ## correspondance : colnames, same as in the original csv file
    ## names(tab) : colnames, with make.names
    ## label(tab) : correspondance with standardize_tab
    correspondance <- names(tab) %>%
      standardize_names_basic()

    names(tab) <- make.names(correspondance)

  } else if (ext %in% c("xls", "xlsx")){
    tab <- tryCatch(readxl::read_excel(file, sheet = 1, guess_max = 10000),
                   error = function(e) e)
    if (is(tab, "error")) {
      if (grepl("Failed to open", tab$message)){
        err <- gettext("Unable to load this file. Try to convert it into xlsx.", domain = "R-simplestats")
      } else {
        err <- gettext("Unable to load this file.", domain = "R-simplestats")
      }
    }
    if(!is.null(err)){
      return(err)
    }
    correspondance <- names(tab)
  }
  tab <- standardize_tab(tab)
  return(list(tab, correspondance))
}

