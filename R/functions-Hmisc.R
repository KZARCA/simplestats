##"label<-"  <- function(x, value) {
##  attr(x, "label") <- value
##  x
##}
## exact=TRUE and [['label']] to prevent matching with a different attribute
## "labels" (Thanks: Ivan Puzek)

#' @export
label <- function(x, default=NULL, ...) UseMethod("label")

#' @export
#' @rdname label
label.default <- function(x, default=NULL, units=plot, plot=FALSE,
                          grid=FALSE, html=FALSE, ...)
{
  if(length(default) > 1)
    stop("the default string cannot be of length greater then one")

  at <- attributes(x)
  lab <- at[['label']]
  if(length(default) && (!length(lab) || lab==''))
    lab <- default

  #un  <- at$units
  # labelPlotmath(lab,
  #               if(units) un else NULL,
  #               plotmath=plot, grid=grid, html=html)
  if(! length(lab)) lab <- ''
  lab
}

#' @export
#' @rdname label
label.data.frame <- function(x, default=NULL, self=FALSE, ...) {
  if(self) {
    label.default(x)
  } else {
    if(length(default) > 0 && length(default) != length(x)) {
      stop('length of default must same as x')
    } else if(length(default) == 0) {
      default <- list(default)
    }

    labels <- mapply(FUN=label, x=x, default=default,
                     MoreArgs=list(self=TRUE), USE.NAMES=FALSE)
    names(labels) <- names(x)
    return(labels)
  }
}

#' @export
#' @rdname label
"label<-" <- function(x, ..., value) UseMethod("label<-")

#' @export
#' @rdname label
"label<-.default" <- function(x, ..., value)
{
  if(is.list(value)) {
    stop("cannot assign a list to be a object label")
  }

  if(length(value) != 1L) {
    stop("value must be character vector of length 1")
  }

  attr(x, 'label') <- value

  if(!'labelled' %in% class(x)) {
    class(x) <- c('labelled', class(x))
  }
  return(x)
}

#' @export
#' @rdname label
"label<-.data.frame" <- function(x, self=TRUE, ..., value) {
  if(!is.data.frame(x)) {
    stop("x must be a data.frame")
  }

  if(missing(self) && is.list(value)) {
    self <- FALSE
  }

  if(self) {
    xc <- class(x)
    xx <- unclass(x)
    label(xx) <- value
    class(xx) <- xc
    return(xx)
  } else {
    if(length(value) != length(x)) {
      stop("value must have the same length as x")
    }

    for (i in seq(along.with=x)) {
      label(x[[i]]) <- value[[i]]
    }
  }

  return(x)
}

#' @export
#' @rdname label
"[.labelled"<- function(x, ...) {
  tags <- valueTags(x)
  x <- NextMethod("[")
  valueTags(x) <- tags
  x
}

#' @export
#' @rdname label
print.labelled <- function(x, ...) {
  x.orig <- x
  u <- attr(x, 'units', exact=TRUE)
  if(length(u))
    attr(x,'units') <- NULL   # so won't print twice

  cat(attr(x, "label", exact=TRUE),
      if(length(u))
        paste('[', u, ']', sep=''),
      "\n")

  attr(x, "label") <- NULL
  class(x) <-
    if(length(class(x))==1 && class(x)=='labelled')
      NULL
  else
    class(x)[class(x) != 'labelled']

  ## next line works around print bug
  if(!length(attr(x,'class')))
    attr(x,'class') <- NULL

  NextMethod("print")
  invisible(x.orig)
}

#' @export
#' @rdname label
as.data.frame.labelled <- as.data.frame.vector

#' @export
#' @rdname label
relevel.labelled <- function(x, ...) {
  lab <- label(x)
  x <- NextMethod(x)
  label(x) <- lab
  x
}

## $Id$
.valueTagAttrs <- c(label="label", units="units", name="shortlabel")


valueTags <- function(x)
  attributes(x)[names(attributes(x)) %in% .valueTagAttrs]


"valueTags<-" <- function(x, value) {
  if(is.null(value) || length(value) == 0) {
    attributes(x)[names(attributes(x)) %in% .valueTagAttrs] <- NULL
    class(x) <- class(x)[class(x) != 'labelled']
    return(x)
  }

  if(!is.list(value)) {
    stop("list must be a named list of valueTags")
  }

  value[(!names(value) %in% .valueTagAttrs) |
          unlist(lapply(value, is.null))] <- NULL

  if(length(value) == 0) {
    attributes(x)[names(attributes(x)) %in% .valueTagAttrs] <- NULL
    class(x) <- class(x)[class(x) != 'labelled']
    return(x)
  }

  attributes(x)[setdiff(names(attributes(x))[names(attributes(x)) %in%
                                               .valueTagAttrs],
                        names(value))] <- NULL

  consolidate(attributes(x)) <- value

  if(all(class(x) != 'labelled'))
    class(x) <- c('labelled', class(x))

  return(x)
}

consolidate <- function(x, value, protect, ...) {
  UseMethod("consolidate")
}

'consolidate<-' <- function(x, protect=FALSE, ..., value)
  consolidate(x, value, protect, ...)

consolidate.default <- function(x, value, protect=FALSE, ...) {
  if(missing(x) || is.null(x))
    x <- vector()

  if(missing(value) || is.null(value))
    value <- vector()

  xNames <- names(x)
  valueNames <- names(value)

  if(is.null(xNames) || is.null(valueNames) || all(valueNames == "") ||
     all(xNames == ""))
    return(c(x, value))

  vars <- intersect(xNames, valueNames[valueNames != ""])
  if(!protect)
    x[vars] <- value[vars]

  c(x, value[!valueNames %in% vars])
}


impute <- function(x, fun=median, ...)
{
  m <- is.na(x)
  k <- sum(m)
  if(k==0)
    return(x)

  nam <- names(x)
  if(!length(nam)) {
    nam <- as.character(1:length(x)); names(x) <- nam
  }

  if(!is.function(fun)) {
    fill <- fun
    if(is.character(fill) && length(fill)==1 && fill=="random")
      fill <- sample(x[!is.na(x)], sum(is.na(x)), replace=TRUE)
  } else if(is.factor(x)) {
    freq <- table(x)
    fill <- names(freq)[freq==max(freq)][1]   #take first if not unique
  } else
    fill <-
    if(missing(fun) && is.logical(x))
      (if(sum(x[!m]) >= sum(!m)/2)
        TRUE
       else
         FALSE)
  else
    fun(x[!m])

  ## median(logical vector) doesn't work - know trying to get median
  ## if fun is omitted.  Get mode.

  if(length(fill)>1 && length(fill)!=k)
    stop("length of vector of imputed values != no. NAs in x")

  ## lab <- label(x)
  ## if(is.null(lab) || lab=="") lab <- name
  ## lab <- paste(lab,"with",sum(m),"NAs imputed to",format(fill))
  ## attr(x, "label") <- lab
  if(is.factor(x)) {
    newlev <- sort(unique(fill))
    if(any(!(z <- newlev %in% levels(x)))) {
      xc <- as.character(x)
      xc[m] <- fill
      x <- factor(xc, c(levels(x), newlev[!z]))
    } else x[m] <- fill
  } else x[m] <- fill

  structure(x, imputed=(1:length(x))[m],
            class=c('impute', attr(x, 'class')))
}

#' @export
capitalize <- function(string) {
  capped <- grep("^[A-Z]", string, invert = TRUE)
  substr(string[capped], 1, 1) <- toupper(substr(string[capped], 1, 1))
  return(string)
}
