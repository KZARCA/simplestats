import_delim <- function(file, sep, dec){
  tab <- NULL
  enc <- readr::guess_encoding(file, threshold = .9)$encoding[1]
  if (is.na(enc) | enc == "ASCII") enc <- ""
  if (tolower(tools::file_ext(file)) == "txt" | sep != "\t" | dec != "."){
    tab <- read.delim(file, strip.white = TRUE, fileEncoding = enc,  na.strings=c("NA", "", " ", "."),
                      stringsAsFactors = FALSE, check.names = FALSE, sep = sep, dec = dec)
  } else if (tolower(tools::file_ext(file)) == "csv"){
    tab <- import_csv(file, enc = enc)
  }
  remove_multibyte_if_any(tab)
}

import_csv <- function(file, enc = "") {
  tab <- read.csv2(file, na.strings=c("NA", "", " ", "."), fileEncoding = enc,
                       strip.white = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
  if(length(tab) == 1){
    tab <- read.csv(file, na.strings=c("NA", ""," ", "."), strip.white = TRUE, fileEncoding = enc,
                        stringsAsFactors = FALSE, check.names = FALSE)
  }
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
read_tab_import <- function(file, sep = "\t", dec = ".", sheet = 1){
  ext <- tolower(tools::file_ext(file))
  tab <- NULL
  err <- NULL

  if(ext %in% c("csv", "txt")){
    tab <- try2(import_delim(file, sep = sep, dec = dec),
                errors = c(gettext("invalid multibyte"),
                           gettext("more columns"),
                           gettext("missing value")
                           ),
                warnings = c(gettext("embedded nul(s) found in input", domain = "R-simplestats"),
                           gettext("appears to contain embedded nulls", domain = "R-simplestats"),
                           gettext("incomplete final line")
                           ))
    if (is_error(tab)) {
      if (grepl("type.convert", attr(tab, "message"))){
        err <- gettext("Unable to load this file because of unreadable characters.", domain = "R-simplestats")
      } else {
        err <- gettext("Unable to load this file.", domain = "R-simplestats")
      }
    } else if (is.null(tab)){
      err <- gettext("Unable to load this file.", domain = "R-simplestats")
    }

    if(!is.null(err)){
      stop(err)
    }
    ## names(tab) : colnames, with make.names
    ## label(tab) : name_matching with standardize_tab
    ## name_matching : noms = names(tab), name_matching = standardize_names_basic(noms)
    name_matching <- try2(make_name_matching(tab), errors = "10000")

    if (is_error(name_matching) && grepl("10000", attr(name_matching, "message"))){
      err <- paste(
        gettext("Unable to load this file. Try changing the delimiter and/or the extension of the file", domain = "R-simplestats"),
        gettext("(extension .csv if the delimiter is a comma or a semicolon, extension .txt if the delimiter is a tabulation)", domain = "R-simplestats")
      )
    } else if (is_error(name_matching)){
      stop(gettext("Unable to load this file"))
    }
  } else if (ext %in% c("xls", "xlsx", "xlsm")){
    tab <- try2(readxl::read_excel(file, sheet = sheet, guess_max = 10000, .name_repair = "minimal"),
                errors = c("Evaluation error", "Unable to open file", "rId", "Unable to allocate memory"),
                warnings = c("Expecting", "NA inserted", "Coercing"))
    if (is_error(tab)) {
      if (grepl("Failed to open", attr(tab, "message"))){
        err <- gettext("Unable to load this file. Try to convert it into xlsx.", domain = "R-simplestats")
      } else if (grepl("more columns than column names", attr(tab, "message"))){
        err <- gettext("Unable to load this file. Try to convert it into xlsx.", domain = "R-simplestats")
      } else if (grepl("rId", attr(tab, "message"))){
        err <- gettext("Unable to read this sheet.", domain = "R-simplestats")
      }
      else {
        err <- gettext("Unable to load this file.", domain = "R-simplestats")
      }
    }
    if(!is.null(err)){
      stop(err)
    }

    name_matching <- make_name_matching(tab)
  }
  tab <- standardize_tab(tab)
  gc()
  return(structure(tab, name_matching=name_matching))
}

