import_csv <- function(file, firstImport, encoding = "unknown", sep, dec) {
  tab <- NULL
  try({tab <- read.csv(file, na.strings=c("NA", "", " ", "."), sep = sep, dec = dec,
                       strip.white = TRUE, stringsAsFactors = FALSE, check.names = FALSE, encoding = encoding)})
  if (firstImport) {
    if(length(tab) == 1 | is.null(tab)){
      try(tab <- read.csv(file, na.strings=c("NA", ""," ", "."), strip.white = TRUE,
                          stringsAsFactors = FALSE, check.names = FALSE, encoding = encoding))
    }
  }
  tab
}

read_tab_imp <- function(file, firstImport, sep, dec){
  ext <- tools::file_ext(file)
  tab <- NULL

  if(ext == "csv"){
    tab <- import_csv(file, firstImport, sep = sep, dec = dec)
    tries <- try(make.names(names(tab)), silent = TRUE)

    if("try-error" %in% class(tries)) {
      encodings <- c("utf8","latin1", "MAC")
      for(enc in encodings){
        if ("try-error" %in% class(tries)){
          tab <- import_csv(file, firstImport, encoding = enc, sep = sep, dec = dec)
          tries <- try(make.names(names(tab)), silent = TRUE)
        }
      }
      if ("try-error" %in% class(tries)){
        stop(shiny::safeError(gettext("Impossible de charger ce fichier, en raison de la présence de caractères illisibles dans le nom des colonnes.
                     Supprimez ces caractères, ou convertissez votre fichier en format Excel")))
      }
    }
    ## correspondance : colnames, same as in the original csv file
    ## names(tab) : colnames, with make.names
    ## label(tab) : correspondance with standardize_tab
    correspondance <- names(tab)
    names(tab) <- tries %>%
    str_replace_all("\\.*(.*)", "\\1")

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
