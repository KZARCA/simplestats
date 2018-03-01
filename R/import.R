import_delim <- function(file, firstImport = TRUE, sep = "\t", dec = "."){
  tab <- NULL
  enc <- system2("file", args = c("-b", "--mime-encoding", shQuote(file, type = "cmd")), stdout = TRUE) %>%
    toupper()
  if (grepl("UNKNOWN", enc, ignore.case = TRUE)) enc <- ""
  if (tolower(tools::file_ext(file)) == "txt"){
    tab <- read.delim(file, strip.white = TRUE, fileEncoding = enc,  na.strings=c("NA", "", " ", "."),
                      stringsAsFactors = FALSE, check.names = FALSE, sep = sep, dec = dec)
  } else if (tolower(tools::file_ext(file)) == "csv"){
    tab <- import_csv(file, firstImport, enc = enc, sep=";", dec=",")
  }
  tab <- remove_multibyte_if_any(tab)
  return(tab)
}

import_csv <- function(file, firstImport, enc = "", sep, dec) {
  tab <- NULL
  try({tab <- read.csv(file, na.strings=c("NA", "", " ", "."), sep = sep, dec = dec, fileEncoding = enc,
                       strip.white = TRUE, stringsAsFactors = FALSE, check.names = FALSE)})
  #if (firstImport) {
    if(length(tab) == 1 | is.null(tab)){
      try(tab <- read.csv(file, na.strings=c("NA", ""," ", "."), strip.white = TRUE, fileEncoding = enc,
                          stringsAsFactors = FALSE, check.names = FALSE))
    }
  #}
  tab
}

read_tab_imp <- function(file, firstImport, sep, dec){
  ext <- tolower(tools::file_ext(file))
  tab <- NULL

  if(ext %in% c("csv", "txt")){
    tab <- import_delim(file, firstImport, sep = sep, dec = dec)
    #tries <- try(make.names(names(tab)), silent = TRUE)

    # if("try-error" %in% class(tries)) {
    #   encodings <- c("utf8","latin1", "MAC")
    #   for(enc in encodings){
    #     if ("try-error" %in% class(tries)){
    #       tab <- import_csv(file, firstImport, encoding = enc, sep = sep, dec = dec)
    #       tries <- try(make.names(names(tab)), silent = TRUE)
    #     }
    #   }
    #   if ("try-error" %in% class(tries)){
    #     stop(shiny::safeError(gettext("Unable to load this file because of unreadable characters in the column names. Delete these characters, or convert your file to Excel format", domain = "R-simplestats")))
    #   }
    # }
    ## correspondance : colnames, same as in the original csv file
    ## names(tab) : colnames, with make.names
    ## label(tab) : correspondance with standardize_tab
    correspondance <- names(tab) %>%
      standardize_names_basic()

    names(tab) <- make.names(correspondance)

  } else if (ext %in% c("xls", "xlsx")){
    tab <- readxl::read_excel(file, sheet = 1, guess_max = 10000)
    correspondance <- names(tab)
  }
  if (!is.null(tab)) tab <- standardize_tab(tab)
  return(list(tab, correspondance))
}

#' Read file
#'
#' This is a wrapper of read_tab_imp
#' @param file The file to read
#' @param sep The field separator character. Values on each line of the file are separated by this character. If sep = "" (the default for read.table) the separator is ‘white space’, that is one or more spaces, tabs, newlines or carriage returns.
#' @param dec The character used in the file for decimal points.
#'
#' @return A data.frame
#' @export
#'
#' @examples
read_tab_export <- function(file, sep = ",", dec = "."){
  read_tab_imp(file, TRUE, sep, dec)[[1]]
}
