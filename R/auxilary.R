
# SEARCH FOR
# LEFT_OFF

#' garantee a date is a date or and empty date
#'
#' @description
#' \preformatted{
#'
#' }
#'
#' @param date date
#' @return date
#' @examples
#' \dontrun{
#' # > initDate(date = NULL)
#' # Date of length 0
#'
#' # > initDate(c())
#' # Date of length 0
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom zoo as.Date
initDate <- function(date = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  if(is.null(date) | !(length(date))) {
    date <- zoo::as.Date(0L)[0]
  }
  date

})}


#' garantee a passed xts object or a zero length xts object
#'
#' @description
#' \preformatted{
#'
#' }
#'
#' @param xTs xts object
#' @return xts object
#' @examples
#' \dontrun{
#' # > initXts(xTs = NULL)
#' # Data:
#' # numeric(0)
#' #
#' # Index:
#' # Date of length 0
#' #
#' # > initXts(zoo::as.Date(0)[0])
#' # Data:
#' # numeric(0)
#' #
#' # Index:
#' # Date of length 0
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom zoo as.Date
initXts <- function(xTs = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  if(is.null(xTs)) {
    # empty xts
    xTs <-  xts(, zoo::as.Date(0)[0])
  } else if (is.timeBased(xTs)) {
    xTs <-  xts(, xTs)
  } else {}
  xTs

})}



#' internally detect and process inbound variable or string named like "mkdata"
#'
#' @description
#' \preformatted{
#' Assign to the calling environment the variables:
#'
#'   xTs:
#'     the result of work: could be the same as inbound xTs
#'                         could be massaged by initXts()
#'   InBndxTs:
#'     the inbound xTs symbols or name
#'     user must pre-call gather from "InBndxTs <- as.character(substitute(xTs))"
#'
#'   isInBndxTsMktData:
#'     boolean: whether or not the name or symbol "InBndxTs" is like "mktdata"
#'
#' # NOTE: if one wants to intialize use
#' data <-initXts(NULL) # default index class Date
#' # do not use
#' data <- xts::xts()   # default index class POSIXct,POSIXt
#' # "YYYY-MM-DD" time ranges input into quantmod::modelData
#' #  will cause error "unambigous data" error
#' #  uantmod::modelData by default looks for "POSIXct,POSIXt"
#'
#' }
#'
#' @param xTs object or NULL or bare symbol or string like "mktdata"
#' @param InBndxTs This is the xTs name as a symbol or a string
#' @return to the parent's parent env:
#' \describe{
#' \item{xTs}{(class xts object)}
#' \item{InBndxTs(Boolean)}{the string or symbol like "mktdata"}
#' \item{isInBndxTsMktData(Boolean)}{is the string or symbol like "mktdata":}
#' }
#' @importFrom stringr str_detect
#' @export
initMktData <- function(xTs = NULL, InBndxTs = NULL) {
initEnv();on.exit({uninitEnv()})
  tryCatchLog::tryCatchLog({

  isInBndxTsMktData <- stringr::str_detect(InBndxTs, "mktdata")
  if(!length(isInBndxTsMktData)) isInBndxTsMktData <- FALSE
  res <- try(is.null(xTs), silent = T)
  if(inherits(res, "try-error")) {
    xTs <- NULL
  }
  if(isInBndxTsMktData && is.character(xTs) &&
     is.vector(xTs) && (length(xTs) == 1)) {
     xTs  <- initXts(NULL)
  }
  xTs  <- initXts(xTs)

  assign("xTs", xTs, envir = parent.frame())
  assign("InBndxTs", InBndxTs, envir = parent.frame())
  assign("isInBndxTsMktData", isInBndxTsMktData, envir = parent.frame())
  invisible()

})}



#' Return internally processed outbound variable or string named like "mkdata"
#'
#'
#' @description
#' \preformatted{
#' Processes initMktData variables xTs, InBndxTs, and isInBndxTsMktData
#' Returns a "mktdata" variable to the caller's calling environment.
#' }
#'
#' @param xTs object or NULL or bare symbol or string like "mktdata"
#' @param InBndxTs This is the xTs name as a symbol or a string
#' @param isInBndxTsMktData Boolean default(FALSE). This is whether or not
#' xTs is a symbol or name that is a string like "mktdata"
#' @param xTsInvisible FALSE(default) if were to return the xTs then
#' do so invisibly.  Usefully TRUE in the case of printing.
#' @return xts object or assign a variable name InBndxTs with xts value of the xts object
#' @examples
#' \dontrun{
#'
#' fooY <- function(xTs = NULL) {
#'
#'   InBndxTs <- as.character(substitute(xTs))
#'   initMktData(xTs, InBndxTs)
#'
#'   # some work
#'   xTs <- xts(0, zoo::as.Date(0))
#'
#'   return(releaseMktData(xTs, InBndxTs, isInBndxTsMktData))
#' }
#' # fooY("mktdata")
#' # str(mktdata)
#' # An ‘xts’ object . . .
#' #
#'
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom rlang caller_frame
releaseMktData <- function(xTs = NULL, InBndxTs = NULL, isInBndxTsMktData = NULL, xTsInvisible = NULL) {
initEnv();on.exit({uninitEnv()})
  tryCatchLog::tryCatchLog({

  if(is.null(xTsInvisible)) xTsInvisible = FALSE

  if(isInBndxTsMktData) {
    assign(InBndxTs, xTs, envir = rlang::caller_frame(n = 2)$env)
    return(invisible())
  } else {
    if(xTsInvisible != TRUE) {
      return(xTs)
    } else {
      return(invisible(xTs))
    }
  }

})}



#' garantees return value value is a 'matrix of at least one dimension'or NULL
#'
#' @description
#' \preformatted{
#'
#' handles the edge case of coredata(<vector of element of size 1 or size 0>).
#' Meant specifically to input empty coredata data into xts(, index).
#' xts(,index) only accepts a non zero-dimension matrix or a NULL.
#'
#' }
#'
#' @param xTs xts object
#' @return matrix
#' @examples
#' \dontrun{
#' # > require(xts)
#' # > Coredata(NULL)
#' # NULL
#' #
#' # > Coredata(numeric(0))
#' # NULL
#' #
#' # > Coredata(11:13)
#' #      [,1]
#' # [1,]   11
#' # [2,]   12
#' # [3,]   13
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
Coredata <- function(xTs = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  xTs  <- initXts(xTs)
  cd <- coredata(xTs)
  cd <- as.matrix(cd)

  if(is.null(cd)) return(NULL)
  if((NROW(cd) == 0) || (NCOL(cd) == 0)) return(NULL)

  if(length(dim(cd)) > 2) stop("coredata is greater than two dimensions")

  if(is.matrix(cd)) {
    return(cd)
  } else {
    stop("could not convert coredata to a matrix")
  }
  invisible()

})}



#' zoo seq.yearqtr
#'
#' @description
#' \preformatted{
#' Originally meant to be a zoo S3 object.
#' Based on the code of seq.Date.
#'
#' The situation of "no existing seq.yearqtr" drove me nuts!
#' Therefore I programmed one.
#'
#' NOTE: this implementations complements the gist by adding "by methods"
#' See. the examples.
#'
#' }
#' @references
#' \cite{R zoo S3 object seq.yearqtr seq.yearmon
#' \url{https://gist.github.com/AndreMikulec/aceb20a0b6c170027b035519ca7a3adb}}
#' @param from  See ? seq.Date
#' @param to See ? seq.Date
#' @param by See ? seq.Date. See examples (below).
#' @param length.out  See ? seq.Date
#' @param along.with  See ? seq.Date
#' @param ... See ? seq.Date
#' @return zoo as.yearqtr class
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom zoo as.yearqtr
#' @examples
#' \dontrun{
#' seq.yearqtr(as.yearqtr("2000 Q1"), to = as.yearqtr("2002 Q1"))
#' # Error in seq.yearqtr(as.yearqtr("2000 Q1"), to = as.yearqtr("2002 Q1")) :
#' #  exactly two of 'to', 'by' and 'length.out' / 'along.with' must be specified
#' # That was the expected output!
#'
#' seq.yearqtr(as.yearqtr("2000 Q1"), to = as.yearqtr("2002 Q1"), length.out = 2)
#' # [1] "2000 Q1" "2002 Q1"
#'
#' seq.yearqtr(as.yearqtr("2000 Q1"), to = as.yearqtr("2002 Q1"), length.out = 3)
#' # [1] "2000 Q1" "2001 Q1" "2002 Q1"
#'
#' seq.yearqtr(as.yearqtr("2000 Q1"), to = as.yearqtr("2002 Q1"), length.out = 4)
#' # [1] "2000 Q1" "2000 Q4" "2001 Q2" "2002 Q1"
#'
#' seq.yearqtr(as.yearqtr("2000 Q1"), to = as.yearqtr("2002 Q1"), by = 1/4)
#' # [1] "2000 Q1" "2000 Q2" "2000 Q3" "2000 Q4" "2001 Q1" "2001 Q2" "2001 Q3"
#' # [8] "2001 Q4" "2002 Q1"
#'
#' seq.yearqtr(as.yearqtr("2000 Q1"), to = as.yearqtr("2002 Q1"), by = 2/4)
#' # [1] "2000 Q1" "2000 Q3" "2001 Q1" "2001 Q3" "2002 Q1"
#'
#' seq.yearqtr(as.yearqtr("2000 Q1"), to = as.yearqtr("2002 Q1"), by = "2 yearqtrs")
#' # [1] "2000 Q1" "2000 Q3" "2001 Q1" "2001 Q3" "2002 Q1"
#'
#' seq.yearqtr(as.yearqtr("2000 Q1"), to = as.yearqtr("2002 Q1"), by = "2 quarters")
#' # [1] "2000 Q1" "2000 Q3" "2001 Q1" "2001 Q3" "2002 Q1"
#'
#' seq.yearqtr(as.yearqtr("2002 Q1"), to = as.yearqtr("2000 Q1"), by = -1/4)
#' # [1] "2002 Q1" "2001 Q4" "2001 Q3" "2001 Q2" "2001 Q1" "2000 Q4" "2000 Q3"
#' # [8] "2000 Q2" "2000 Q1"
#'
#' seq.yearqtr(as.yearqtr("2002 Q1"), to = as.yearqtr("2000 Q1"), by = "-1 yearqtrs")
#' # [1] "2002 Q1" "2001 Q4" "2001 Q3" "2001 Q2" "2001 Q1" "2000 Q4" "2000 Q3"
#' # [8] "2000 Q2" "2000 Q1"
#'
#'}
#' @export
seq.yearqtr <- function(from, to, by, length.out = NULL, along.with = NULL, ...) {
tryCatchLog::tryCatchLog({
initEnv(); on.exit({uninitEnv()})

  # R version 3.3.1 (2016-06-21) -- "Bug in Your Hair"
  # zoo_1.7-13
  # R version 3.5.3 (2019-03-11)
  # zoo_1.8-4

  # exactly two of 'to', 'by' and 'length.out' / 'along.with' must be specified
  # seq.Date - missing

  if (missing(from))
      stop("'from' must be specified")
  if (!inherits(from, "yearqtr"))
      stop("'from' must be a \"yearqtr\" object")
  if (length(zoo::as.yearqtr(from)) != 1L)
      stop("'from' must be of length 1")
  if (!missing(to)) {
      if (!inherits(to, "yearqtr"))
          stop("'to' must be a \"yearqtr\" object")
      if (length(zoo::as.yearqtr(to)) != 1L)
          stop("'to' must be of length 1")
  }
  if (!missing(along.with)) {
      length.out <- length(along.with)
  }
  else if (!is.null(length.out)) {
      if (length(length.out) != 1L)
          stop("'length.out' must be of length 1")
      length.out <- ceiling(length.out)
  }
  status <- c(!missing(to), !missing(by), !is.null(length.out))
  if (sum(status) != 2L)
      stop("exactly two of 'to', 'by' and 'length.out' / 'along.with' must be specified")

  # seq.Date - by management

  if (missing(by)) {
      from <- unclass(zoo::as.yearqtr(from))
      to <- unclass(zoo::as.yearqtr(to))
      res <- seq.int(from, to, length.out = length.out)
      return(structure(res, class = "yearqtr"))
  }

  # see seq.Date
  if (length(by) != 1L)
      stop("'by' must be of length 1")
  valid <- 0L
  # # no implementation
  # if (inherits(by, "difftime")) {
  #     by <- switch(attr(by, "units"), secs = 1/86400, mins = 1/1440,
  #     hours = 1/24, days = 1, weeks = 7) * unclass(by)
  # }
  # else
  if (is.character(by)) {
      by2 <- strsplit(by, " ", fixed = TRUE)[[1L]]
      if (length(by2) > 2L || length(by2) < 1L)
          stop("invalid 'by' string")
      valid <- pmatch(by2[length(by2)], c("quarters", "yearqtrs"))
      if (is.na(valid))
          stop("invalid string for 'by'")
      if (valid <= 2L) { # always
          by <- c(1/4, 1/4)[valid]
          if (length(by2) == 2L)
              by <- by * as.integer(by2[1L])
      }
      else by <- if (length(by2) == 2L)
          as.integer(by2[1L])
      else 1
  }
  else if (!is.numeric(by))
      stop("invalid mode for 'by'")
  if (is.na(by))
      stop("'by' is NA")

  if (valid <= 2L) {
      from <- unclass(zoo::as.yearqtr(from))
      if (!is.null(length.out))
          res <- seq.int(from, by = by, length.out = length.out)
      else {
          to0 <- unclass(zoo::as.yearqtr(to))
          res <- seq.int(0, to0 - from, by) + from
      }
      res <- structure(res, class = "yearqtr")
  }
  # do not try to convert to POSIX__
  # so skip

  if (!missing(to)) {
      to <- zoo::as.yearqtr(to)
      res <- if (by > 0)
          res[res <= to]
      else res[res >= to]
  }
  res

})}



#' zoo seq.yearmon
#'
#' @description
#' \preformatted{
#' Originally meant to be a zoo S3 object.
#' Based on the code of seq.Date.
#'
#' The situation of "no existing seq.yearmon" drove me nuts!
#' Therefore I programmed one.
#'
#' NOTE: this implementations complements the gist by adding "by methods"
#' See. the examples.
#'
#' }
#' @references
#' \cite{R zoo S3 object seq.yearqtr seq.yearmon
#' \url{https://gist.github.com/AndreMikulec/aceb20a0b6c170027b035519ca7a3adb}}
#' @param from  See ? seq.Date
#' @param to See ? seq.Date
#' @param by See ? seq.Date. See examples (below).
#' @param length.out  See ? seq.Date
#' @param along.with  See ? seq.Date
#' @param ... See ? seq.Date
#' @return zoo as.yearmon class
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom zoo as.yearmon
#' @examples
#' \dontrun{
#'
#' seq.yearmon(as.yearmon("2000-01"), to = as.yearmon("2000-07"))
#' # Error in seq.yearmon(as.yearmon("2000-01"), to = as.yearmon("2000-07"))  :
#' #  exactly two of 'to', 'by' and 'length.out' / 'along.with' must be specified
#' # That is the expected output!
#'
#' seq.yearmon(as.yearmon("2000-01"), to = as.yearmon("2000-07"), length.out = 2)
#' # [1] "Jan 2000" "Jul 2000"
#'
#' seq.yearmon(as.yearmon("2000-01"), to = as.yearmon("2000-07"), length.out = 3)
#' # [1] "Jan 2000" "Apr 2000" "Jul 2000"
#'
#' seq.yearmon(as.yearmon("2000-01"), to = as.yearmon("2000-07"), length.out = 4)
#' # [1] "Jan 2000" "Mar 2000" "May 2000" "Jul 2000"
#'
#' seq.yearmon(as.yearmon("2000-01"), to = as.yearmon("2000-07"), by = 1/12)
#' # [1] "Jan 2000" "Feb 2000" "Mar 2000" "Apr 2000" "May 2000" "Jun 2000" "Jul 2000"
#'
#' seq.yearmon(as.yearmon("2000-01"), to = as.yearmon("2000-07"), by = 2/12)
#' # [1] "Jan 2000" "Mar 2000" "May 2000" "Jul 2000"
#'
#' seq.yearmon(as.yearmon("2000-01"), to = as.yearmon("2000-07"), by = "2 yearmons")
#' # [1] "Jan 2000" "Mar 2000" "May 2000" "Jul 2000"
#'
#' seq.yearmon(as.yearmon("2000-01"), to = as.yearmon("2000-07"), by = "2 months")
#' # [1] "Jan 2000" "Mar 2000" "May 2000" "Jul 2000"
#'
#' seq.yearmon(as.yearmon("2000-07"), to = as.yearmon("2000-01"), by = -1/12)
#' # [1] "Jul 2000" "Jun 2000" "May 2000" "Apr 2000" "Mar 2000" "Feb 2000" "Jan 2000"
#'
#' seq.yearmon(as.yearmon("2000-07"), to = as.yearmon("2000-01"), by = "-1 yearmons")
#' # [1] "Jul 2000" "Jun 2000" "May 2000" "Apr 2000" "Mar 2000" "Feb 2000" "Jan 2000"
#'
#' }
#' @export
seq.yearmon <- function(from, to, by, length.out = NULL, along.with = NULL, ...) {
tryCatchLog::tryCatchLog({
initEnv(); on.exit({uninitEnv()})

  # R version 3.3.1 (2016-06-21) -- "Bug in Your Hair"
  # zoo_1.7-13
  # R version 3.5.3 (2019-03-11)
  # zoo_1.8-4

  # exactly two of 'to', 'by' and 'length.out' / 'along.with' must be specified
  # seq.Date - missing

  if (missing(from))
      stop("'from' must be specified")
  if (!inherits(from, "yearmon"))
      stop("'from' must be a \"yearmon\" object")
  if (length(zoo::as.yearmon(from)) != 1L)
      stop("'from' must be of length 1")
  if (!missing(to)) {
      if (!inherits(to, "yearmon"))
          stop("'to' must be a \"yearmon\" object")
      if (length(zoo::as.yearmon(to)) != 1L)
          stop("'to' must be of length 1")
  }
  if (!missing(along.with)) {
      length.out <- length(along.with)
  }
  else if (!is.null(length.out)) {
      if (length(length.out) != 1L)
          stop("'length.out' must be of length 1")
      length.out <- ceiling(length.out)
  }
  status <- c(!missing(to), !missing(by), !is.null(length.out))
  if (sum(status) != 2L)
      stop("exactly two of 'to', 'by' and 'length.out' / 'along.with' must be specified")

  # seq.Date - by management

  if (missing(by)) {
      from <- unclass(zoo::as.yearmon(from))
      to <- unclass(zoo::as.yearmon(to))
      res <- seq.int(from, to, length.out = length.out)
      return(structure(res, class = "yearmon"))
  }

  # see seq.Date
  if (length(by) != 1L)
      stop("'by' must be of length 1")
  valid <- 0L
  # # no implementation
  # if (inherits(by, "difftime")) {
  #     by <- switch(attr(by, "units"), secs = 1/86400, mins = 1/1440,
  #     hours = 1/24, days = 1, weeks = 7) * unclass(by)
  # }
  # else
  if (is.character(by)) {
      by2 <- strsplit(by, " ", fixed = TRUE)[[1L]]
      if (length(by2) > 2L || length(by2) < 1L)
          stop("invalid 'by' string")
      valid <- pmatch(by2[length(by2)], c("months", "yearmons"))
      if (is.na(valid))
          stop("invalid string for 'by'")
      if (valid <= 2L) { # always
          by <- c(1/12, 1/12)[valid]
      if (length(by2) == 2L)
          by <- by * as.integer(by2[1L])
      }
      else by <- if (length(by2) == 2L)
          as.integer(by2[1L])
      else 1
  }
  else if (!is.numeric(by))
      stop("invalid mode for 'by'")
  if (is.na(by))
      stop("'by' is NA")

  if (valid <= 2L) {
      from <- unclass(zoo::as.yearmon(from))
      if (!is.null(length.out))
          res <- seq.int(from, by = by, length.out = length.out)
      else {
          to0 <- unclass( zoo::as.yearmon(to))
          res <- seq.int(0, to0 - from, by) + from
      }
      res <- structure(res, class = "yearmon")
  }
  # do not try to convert to POSIX__
  # so skip

  if (!missing(to)) {
      to <- zoo::as.yearmon(to)
      res <- if (by > 0)
          res[res <= to]
      else res[res >= to]
  }
  res

})}



#' Convert time series data to an monthly OHLC series
#'
#' @description
#' \preformatted{
#'
#' This is a wrapper around xts to.monthly.
#' Months between the first date and the last
#' date will have and entry filled in.
#' It will get the "firstof" or "lastof"
#' months between dates as the same in the call.
#'
#' }
#'
#' @param x ? xts::to.monthly and the index(x) class must
#' have an S3 method for "seq(by = "months")".
#' Classes "Date" and POSIXt are provided  S3 methods in package "base".
#' @param indexAt "firstof"(default). Different from xts::to.monthly.
#' See ? xts::to.monthly
#' @param drop.time See ? xts::to.monthly
#' @param name See ? xts::to.monthly
#' @param fillMissing TRUE(default). If indexAt is one of "firstof"
#' or "lastof", then fill in missing month dates, if any.
#' Otherwise FALSE, just pass throught to xts::to.monthly.
#' @param ... dots See. ? xts::to.monthly
#' @return See. ? xts::to.monthly
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom xts to.monthly
#' @importFrom DescTools LastDayOfMonth
#' @examples
#' \dontrun{
#' x <- xts(c(1,140), zoo::as.Date(c(0,139)))
#' To.Monthly(x, OHLC = FALSE, indexAt = "firstof")
#' To.Monthly(x, OHLC = FALSE, indexAt = "lastof")
#' To.Monthly(x, OHLC = FALSE, indexAt = "yearmon")
#'}
#' @export
To.Monthly <- function(x,indexAt='firstof',drop.time=TRUE,name, fillMissing = NULL, ...) {
tryCatchLog::tryCatchLog({
initEnv(); on.exit({uninitEnv()})

  fillMissing <- if(is.null(fillMissing)) { TRUE }

  # note: needed
  #  S3 seq.yearmon

  monthly <- xts::to.monthly(x = x,indexAt=indexAt,drop.time=drop.time,name=name,...)
  if(fillMissing && (NROW(monthly) > 1)) {
    if(indexAt %in% c("firstof", "lastof", "yearmon")) {

    # yearmon case: otherwise, pass-through
    # zoo::as.Date.yearmon(., frac = 0)
    From <- zoo::as.Date(head(index(monthly),1), frac = 0) ; DescTools::Day(From) <- 1
    To   <- zoo::as.Date(tail(index(monthly),1), frac = 0) ; DescTools::Day(To)   <- 1
                     # S3 dispatch
    intermediates <- seq(from = From, to = To, by = "months")

    }
    # further
    if(indexAt == "lastof") {
       intermediates <- DescTools::LastDayOfMonth(intermediates)
    }
    # further ...
    if(indexAt == "yearmon") {
    intermediates <- zoo::as.yearmon(intermediates)
    }

    newDates <- setDiff(c(intermediates, index(monthly)),index(monthly))
    monthly <- merge(monthly, xts(, newDates))
  }
  monthly

})}



#' number of variables
#'
#' @description
#' \preformatted{
#'
#' NCOL wrongly returns value one(1)
#' on non-data.frame 2nd dimension objects
#' with a 2nd dimension size of zero(0).
#'
#' }
#'
#' @param x xts object
#' @return integer number of variables
#' @rdname NVAR
#' @export
NVAR <- function(x = NULL) {
  # tryCatchLog is not allowed here
  UseMethod("NVAR")

}



#' @rdname NVAR
#' @export
#' @importFrom tryCatchLog tryCatchLog
NVAR.default <- function(x = NULL) {
tryCatchLog::tryCatchLog({

  if(is.null(x)) return(0L)
  NCOL(x)

})}



#' @rdname NVAR
#' @examples
#' \dontrun{
#' # > require(xts)
#' # > NVAR(xts(, zoo::as.Date("1970-01-12")))
#' # [1] 0
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
NVAR.xts <- function(x = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  x <- initXts(x)

  if(length(coredata(x))) {
    res <- NCOL(coredata(x))
  } else {
    res <- 0L
  }
  return(res)

})}


#' convenience function calling stringr str_detect
#'
#' @description
#' \preformatted{
#'
#' This is stricter. NA values are returned as FALSE.
#'
#' }
#'
#' @param x string
#' @param pattern ICU regular expression pattern
#' @return logical
#' @examples
#' \dontrun{
#'  "1234" %Like% "^\\d{4}$"
#'}
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_detect
#' @export
`%Like%` <- function(x, pattern) {
tryCatchLog::tryCatchLog({
initEnv(); on.exit({uninitEnv()})

    if (is.factor(x)) {
        Res <- as.integer(x) %in% stringr::str_detect(string = levels(x), pattern = pattern)
    }
    else {
        Res <- stringr::str_detect(string = x, pattern = pattern)
    }
    Res[is.na(Res)] <- FALSE
    Res

})}



#' search OS environment variables or R options
#'
#' @description
#' \preformatted{
#'
#' Look for truths or values in memory.
#' Used as an alternative instead of expensive re-quering.
#' If x is stored ins the OS environment, then x is stored as a string.
#' To be consistent with OS storage, a value stored in options are
#' also stored a string (see setWhat). Test exceptions are "TRUE","FALSE".
#' These are tested as "TRUE"/"FALSE" or TRUE/FALSE.
#'
#' }
#'
#' @param x character vector of size one(1)  of what to check
#' @param y TRUE(default): value to check against (often TRUE)
#' @param Storage "OS"(default). Otherwise "Options" or "Both"
#' @return requested value (often TRUE)
#' @examples
#' \dontrun{
#'
#' isWhatFnd("TAB_X_COL_Y_EXISTS")
#' isWhatFnd("TAB_X_COL_Y_EXISTS", TRUE)
#' # not 'preferred test'
#' isWhatFnd("TAB_X_COL_Y_EXISTS", "TRUE")
#' # not 'preferred test location'
#' isWhatFnd("TAB_X_COL_Y_EXISTS", TRUE, Storage = "Options")
#' # \*any\* one or \*both\* of the two
#' isWhatFnd("TAB_X_COL_Y_EXISTS", TRUE, Storage = "Both")
#' # \*any\* one or \*both\* of the two
#' isWhatFnd("TAB_X_COL_Y_EXISTS", TRUE, Storage = c("OS","Options")
#'
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @export
isWhatFnd <- function(x, y = TRUE, Storage = NULL) {
tryCatchLog::tryCatchLog({
## initEnv();on.exit({uninitEnv()})

  if(is.null(Storage)) Storage <- "OS"
  if(!any(Storage %in% c("OS", "Options", "Both")))
    stop("isWhatFnd parameter Storage must be \"OS\" or \"Options\" or \"Both\"")

  if(any(Storage  %in% c("OS", "Both"))) {
    # Sys.setenv() always stores its input as characters
    # Therefore, will test v.s. characters
    SystemResult <- Sys.getenv(x)
    if(SystemResult != "") {
      if(is.null(y) || y == TRUE) {
        if(SystemResult == "TRUE") return(TRUE)
      }
      if(y == FALSE) {
        if(SystemResult == "FALSE") return(FALSE)
      }
      # "TRUE"/"FALSE" or a "specific value"
      if(SystemResult == as.character(y)) return(TRUE)
    }
  }
  #
  # not found in the System OS

  # now search options
  if(any(Storage %in% c("Options", "Both"))) {

    OptionResult <- getOption(x)
    if(!is.null(OptionResult)) {
      if(is.null(y) || y == TRUE) {
       if(OptionResult == "TRUE"  || OptionResult == TRUE) return(TRUE)
      }
      if(y == FALSE) {
       if(OptionResult == "FALSE" || OptionResult == FALSE) return(FALSE)
      }
      # "TRUE"/"FALSE" or a "specific value"
      # should be just: OptionResult == as.character(y)
      # BUT If a LEAK into OPTIONS exists, so do "as.character(OptionResult)"
      if(as.character(OptionResult) == as.character(y) || OptionResult == y) return(TRUE)
    }
  }

  # not found in either place
  return(FALSE)

})}



#' set OS environment variables or R options
#'
#' @description
#' \preformatted{
#'
#' Set truths or values in memory.
#' Used as an alternative instead of expensive re-quering.
#' If x is stored in the OS environment, then x is stored as a string.
#' To be consistent with OS storage, a value stored in options are
#' also stored a string.
#' x is tested using isWhatFnd.
#'
#' }
#'
#' @param x character vector of size one(1)  of what to set
#' @param y TRUE(default): value to set (often TRUE).
#' Can be a specific value that is stored as a character or not.
#' E.g. TRUE/"TRUE", 1/"1"
#' @param Storage "OS"(default). Otherwise "Options" or "Both"
#' @return TRUE(if success) and FALSE(otherwise)
#' @examples
#' \dontrun{
#'
#' setWhat("TAB_X_COL_Y_EXISTS")
#' setWhat("TAB_X_COL_Y_EXISTS", TRUE)
#' # not 'preferred set'
#' setWhat("TAB_X_COL_Y_EXISTS", "TRUE")
#' # not 'preferred set location'
#' setWhat("TAB_X_COL_Y_EXISTS", "TRUE", Storage = "Options")
#' setWhat("TAB_X_COL_Y_EXISTS", "TRUE", Storage = "Both")
#' setWhat("TAB_X_COL_Y_EXISTS", "TRUE", Storage = c("OS","Options"))
#'
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_c
#' @importFrom rlang parse_expr eval_bare
#' @export
setWhat <- function(x, y = TRUE, Storage = NULL) {
tryCatchLog::tryCatchLog({
## initEnv();on.exit({uninitEnv()})

  if(is.null(Storage)) Storage <- "OS"
  if(!any(Storage %in% c("OS", "Options", "Both")))
    stop("setWhat parameter Storage must be \"OS\" or \"Options\" or \"Both\"")

  # Sys.setenv() always stores its input as a character string
  # Therefore, will set as characters (automatically by R)
  if(any(Storage %in% c("OS", "Both"))) {
    stringr::str_c("Sys.setenv(", x, "=", y, ")") %>%
      {rlang::parse_expr(.)} %>% {rlang::eval_bare(.)}
  }

  # consistent with OS environment variable storage
  y <- as.character(y)

  if(any(Storage %in% c("Options", "Both"))) {
    names(y) <- x
    List <- c(list(), y)
    options(y)
  }

  isWhatFnd(x, y, Storage = Storage)

})}


#' get OS environment variables or R options
#'
#' @description
#' \preformatted{
#'
#' Get truths or values in memory.
#' Used as an alternative instead of expensive re-quering.
#' If x is stored ins the OS environment, then x is retrieved as a string.
#' To be consistent with OS storage, a value stored in options are
#' also stored a string.  Retrieval exceptions are "TRUE","FALSE".
#' These are retrieved as TRUE/FALSE.
#'
#' }
#'
#' @param x character vector of size one(1)  of what to get
#' @param Storage "OS"(default). Otherwise "Options"
#' @return value otherwise NULL
#' @examples
#' \dontrun{
#'
#' getWhat("TAB_X_COL_Y_EXISTS")
#' # not 'preferred get location'
#' # choose "OS" xor "Options"
#' getWhat("TAB_X_COL_Y_EXISTS", Storage = "Options")
#'
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @export
getWhat <- function(x, Storage = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  if(is.null(Storage)) Storage <- "OS"
  if(!any(Storage %in% c("OS", "Options")))
    stop("getWhat parameter Storage must be \"OS\" or \"Options\"")

  # Sys.setenv() always stores its input as a character string
  # Therefore, will get as characters (automatically by R)
  if(Storage == "OS") {
   SystemResult <- Sys.getenv(x)
     if(SystemResult != "") {
       if(SystemResult == "TRUE")  return(TRUE)
       if(SystemResult == "FALSE") return(FALSE)
       return(SystemResult)
     } else {
       return(NULL)
     }
  }

  if(Storage == "Options") {
     OptionResult <- getOption(x)
     if(!is.null(OptionResult)) {
       # setWhat() had already stored as a character string
         if(OptionResult == "TRUE"   || OptionResult == TRUE)  return(TRUE)
         if(OptionResult == "FALSE"  || OptionResult == FALSE) return(FALSE)
       # should be just "return(OptionResult)"
       # BUT If a LEAK into OPTIONS exists, so do "return(as.character(OptionResult))"
       return(as.character(OptionResult))
     }
  }

  return(NULL)

})}



#' From a path-file get the file name without the extension?
#'
#' @description
#' \preformatted{
#'
#' VERIFY [ ]!
#' See Orcs pureBasename
#' funky rstudio problem (involves "gstat" (and "spacetime"))
#'
#' }
#'
pureBasename <- function(path, slash = FALSE) {

  Orcs__unlistStrsplit <- function (x, split, ...) {
      if (missing(split))
          split <- " "
      ls_split <- strsplit(x, split = split, ...)
      ch_split <- unlist(ls_split)
      return(ch_split)
  }

  Orcs__pureBasename <- function (path, slash = FALSE)
  {
      ch_purebasename = sapply(path, function(i) {
          ch_basename <- basename(i)
          ch_split <-  Orcs__unlistStrsplit(ch_basename, "\\.")
          ch_split = ifelse((len <- length(ch_split)) > 1, ch_split[-len],
              ch_split)
          paste(ch_split, collapse = ".")
      })
      if (slash)
          ch_purebasename <- file.path("", ch_purebasename)
      return(as.character(ch_purebasename))
  }

  return(Orcs__pureBasename(path = path, slash = slash))

}



#' From source files, create .fst files
#'
#' @description
#' \preformatted{
#'
#' Expected main directories (dirs), with exactly one level
#' of subdirectories. Each subdirectory contains files (with columns).
#'
#' }
#'
#' @param Dirs character vector of directories
#' @param SubDirExpr NULL(default).  NULL means "all." Character vector of directories
#' @param FileExpr NULL(default). NULL means "all." Full file vector of names
#' @param ColRmExpr NULL(default) NULL meand "none." Regular expression of columns to remove
#' @param Force False(default). If the target .fst file is there, then do not replace it.
#' Otherwise, replace it.
#' @return NULL silently.  Side-effect of a new .fst file created on disk in the old
#' directory location.
#' @examples
#' \dontrun{
#'
#' # earlest files
#' makeFSTFiles(Dirs = "W:\\AAIISIProDBFs", SubDirExpr = c("12055","12083"), FileExpr = c("si_ci.dbf","si_isq.dbf","si_bsq.dbf","si_cfq.dbf","si_psd.dbf","si_date.dbf"), ColRmExpr = c("^X_NullFlags$","^X.?\\d*$"), Force = TRUE)
#'
#' # all files
#' makeFSTFiles(Dirs = "W:\\AAIISIProDBFs", FileExpr = c("si_ci.dbf","si_isq.dbf","si_bsq.dbf","si_cfq.dbf","si_psd.dbf","si_date.dbf"), ColRmExpr = c("^X_NullFlags$","^X.?\\d*$"), Force = TRUE)
#'
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_subset str_c
#' @importFrom foreign read.dbf
#' @importFrom fst write.fst
#' @export
makeFSTFiles <- function(Dirs = NULL, SubDirExpr = NULL, FileExpr = NULL, ColRmExpr = NULL, Force = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  if(is.null(Dirs)) stop("gtData needs at least a Dirs")
  if(is.null(Force)) Force <- FALSE

  for(Dir_i in Dirs) {
    AllSubDirs <- dir(Dir_i)
    if(!is.null(SubDirExpr)) {
      AllSubDirs <- stringr::str_subset(AllSubDirs, stringr::str_c(SubDirExpr, collapse = "|"))
    }

    AllSubDirs <- normalizePath(stringr::str_c(Dir_i, AllSubDirs, sep = "\\"), winslash = "/", mustWork = FALSE)
    for(AllSubDirs_i in AllSubDirs) {
      AllFiles <- dir(AllSubDirs_i)
      if(!is.null(FileExpr)) {
        AllFiles <- stringr::str_subset(AllFiles, stringr::str_c(FileExpr, collapse = "|"))
      }
      for(File_i in AllFiles) {
        FullPathFileExt <- normalizePath(stringr::str_c(AllSubDirs_i, File_i, sep = "\\"), winslash = "/", mustWork = TRUE)
        FullPathFileExtFST <- stringr::str_c(dirname(FullPathFileExt), "/", pureBasename(FullPathFileExt), ".fst")

        if(file.exists(FullPathFileExtFST) && !Force ) next

        Dbf <- suppressWarnings(suppressMessages(foreign::read.dbf(file = FullPathFileExt, as.is = TRUE)))
        gc()
        # remove dead columns (I choose to do that here)
        if(!is.null(ColRmExpr)) {
          Dbf <- Dbf[, stringr::str_subset(colnames(Dbf), stringr::str_c(ColRmExpr, collapse = "|"), negate = TRUE),drop = F]
        }
        # write an FST file
        if(!is.null(getOption("tradeModel__makeFSTFiles__verbose")) && getOption("tradeModel__makeFSTFiles__verbose") == TRUE)
           message(stringr::str_c("makeFSTFiles starting to create file: ", FullPathFileExtFST))
        fst::write.fst(Dbf, path = FullPathFileExtFST, compress= 0L)
        if(!is.null(getOption("tradeModel__makeFSTFiles__verbose")) && getOption("tradeModel__makeFSTFiles__verbose") == TRUE)
           message(stringr::str_c("makeFSTFiles finished creating file: ", FullPathFileExtFST))
      }
    }
  }
  invisible()

})}


#' unset OS environment variables or R options
#'
#' @description
#' \preformatted{
#'
#' Unset truths or values in memory.
#'
#' }
#'
#' @param x character vector of size one(1)  of what to unset
#' @param Storage "OS"(default). Otherwise "Options" or "Both"
#' @return NULL invisibly
#' @examples
#' \dontrun{
#'
#' unsetWhat("TAB_X_COL_Y_EXISTS")
#' # not 'preferred unset location'
#' unsetWhat("TAB_X_COL_Y_EXISTS", Storage = "Options")
#' unsetWhat("TAB_X_COL_Y_EXISTS", Storage = "Both")
#' unsetWhat("TAB_X_COL_Y_EXISTS", Storage = c("OS", "Options"))
#'
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_c
#' @importFrom rlang parse_expr eval_bare
#' @export
unsetWhat <- function(x, Storage = NULL) {
tryCatchLog::tryCatchLog({
## initEnv();on.exit({uninitEnv()})

 if(is.null(Storage)) Storage <- "OS"
 if(!any(Storage %in% c("OS", "Options", "Both")))
   stop("unsetWhat parameter Storage must be \"OS\" or \"Options\" or \"Both\"")

 if(any(Storage  %in% c("OS", "Both")))
    Sys.unsetenv(x)

 if(any(Storage %in% c("Options", "Both")))
    stringr::str_c("options(", x, "=", "NULL", ")") %>%
      {rlang::parse_expr(.)} %>% {rlang::eval_bare(.)}

 invisible()

})}



#' sets the enviroment
#'
#' @description
#' \preformatted{
#'
#' This is a space-saver that is meant to be used at the beginning of a function.
#'
#' Variable ops from the calling environment sets R options.
#' Environment variable TZ using the calling environment variable oldtz sets.
#'
#' }
#'
#' @return envi, options, and TZ are set
#' @examples
#' \dontrun{
#' # > options(max.print=88888L)
#' # initEnv()
#' # > getOption("max.print")
#' # [1] 99999
#' }
#' @export
#' @importFrom futile.logger flog.threshold ERROR flog.appender appender.tee
#' @importFrom logging basicConfig
#' @importFrom rlang parse_expr eval_bare caller_env
#' @importFrom magrittr %>%
#' @importFrom tryCatchLog tryCatchLog
initEnv <- function(init = NULL) {
# tryCatchLog: what level to activate
futile.logger::flog.threshold(futile.logger::ERROR)
futile.logger::flog.appender(futile.logger::appender.tee("tryCatchLog.logged.txt"))
logging::basicConfig()
tryCatchLog::tryCatchLog({
  # so I have one
  action <- rlang::parse_expr("assign(\"envi\", environment())")
  envii  <- rlang::caller_env()
  rlang::eval_bare(action, env = envii)

  # LATER MOVE THIS DOWN INTO FUNCTIONS
  assign("%>%",  magrittr::`%>%` , envir = envii)

  # LATER MOVE THESE DOWN INTO FUNCTIONS
  if(!"quantmod" %in% search())                 require(quantmod)
  if(!"PerformanceAnalytics" %in% search())     require(PerformanceAnalytics)

  # TOO MUCH INFORMATION ( BUT DOES WORK )
  # action <- rlang::parse_expr("assign(\"ssc\", toString(sys.calls()[[length(sys.calls())]]) )")
  # rlang::eval_bare(action, env = envii)
  # action <- rlang::parse_expr("print(ssc)")
  # rlang::eval_bare(action, env = envii)

  # options(warn=2L)
  options(warn=1L) # 1L # so, I can use BELOW: options(tryCatchLog.write.error.dump.file = TRUE)
  options(width=10000L) # LIMIT # Note: set Rterm(64 bit) as appropriate
  # options(digits=if(is.null(init[["digits"]])) { 22L } else {init[["digits"]]})
  options(digits = 5L)
  options(max.print=99999L)
  options(scipen=255L) # Try these = width

  # BECAUSE MY "WARNINGS ARE CONVERTED TO ERRORS" ( ABOVE: options(warn=2L) )
  # I can not directly use this feature
  # Error in save(list = names(.GlobalEnv), file = outfile, version = version,  :
  # (converted from warning) 'package:stats' may not be available when loading
  options(tryCatchLog.write.error.dump.file = TRUE)

  # ops <- options()
  # # pre-save options to ignore
  # ops <- ops[!names(ops) %in% ignore_ops]
  # options(ops)
  # assign("ops", ops, envir = envii)

  # pre-save options to not-ignore
  # ops_not_ignored <- options()[!names(options()) %in% ignore_ops]
  # assign("ops", options(ops_not_ignored), envir = envii)

  ops <- options()
  options(ops)
  assign("ops", ops, envir = envii)

  #correct for TZ
  oldtz <- Sys.getenv("TZ")
  if(oldtz!="UTC") {
    Sys.setenv(TZ="UTC")
  }
  #
  assign("oldtz", oldtz, envir = envii)

  invisible()

})}




#' unsets the enviroment
#'
#' @description
#' \preformatted{
#'
#' This is a space-saver that is meant to be used at the beginning of a function.
#'
#' Variable ops from the calling environment resets R options.
#' Environment variable TZ using the calling environment variable oldtz resets.
#'
#' }
#'
#' @return options, and TZ are un-set
#' @examples
#' \dontrun{
#' # > uninitEnv()
#' # getOption("digits")
#' # [1] 5
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom rlang caller_env
uninitEnv <- function() {
tryCatchLog::tryCatchLog({

  # eventually calling envir will NOT BE the lexically calling environment
  envii <- rlang::caller_env()
  Sys.setenv(TZ=get("oldtz", envir = envii))

  # ops <- get("ops", envir = envii)
  # # post-save options to ignore
  # ops <- ops[!names(ops) %in% ignore_ops]
  # options(ops)

  # # post-save options to ignore
  # ops_ignored <- options()[names(options()) %in% ignore_ops]
  #
  # ops_temp <- get("ops", envir = envii)
  # # remove
  # ops <- ops_temp[!names(ops_temp)  %in% ignore_ops]
  # options(ops)
  #
  # # add back
  # options(ops_ignored)

  ops <- get("ops", envir = envii)
  options(ops)

  invisible()

})}


#' geometric investment results
#'
#' @description
#' \preformatted{
#'
#' xTs values less that zero will generate a numeric error
#'
#' }
#'
#' @param xTs xts object of absolute values
#' @return xts object of geometric returns
#' @examples
#' \dontrun{
#' require(xts)
#' xTs  <- xts(10:12,zoo::as.Date(0:2))
#' lr <- logReturns(xTs)
#' lr
#' #                         logrets
#' # 1970-01-01 0.000000000000000000
#' # 1970-01-02 0.095310179804324768
#' # 1970-01-03 0.087011376989629685
#'
#' # > as.vector(coredata(exp(cumsum(lr)) * 10L))
#' # [1] 10.000000000000000 10.999999999999998 11.999999999999996
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom TTR ROC
#' @importFrom stringr str_c
logReturns <- function(xTs = NULL)  {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  xTs  <- initXts(xTs)

  # percent change
  # also could have used: PerformanceAnalytics::Return.calculate()
  # SEE the references
  xTsLogRets <- TTR::ROC(xTs)        # which(is.na(xTsLogRets)) # logrithmic
  warning("TTR::ROC may only be an approximation and 'not fully' correct.")
  warning("See my package notes on PerformanceAnalytics::Return.calculate")
  xTsLogRets[is.na(xTsLogRets)] <- 0 # usually just the 1st observation
  colnames(xTsLogRets)[1] <- stringr::str_c(colnames(xTsLogRets)[1], "logrets")

  xTsLogRets

})}


#' absolute proportional change results
#'
#' @description
#' \preformatted{
#'
#' Must return SYMBOL*xxxrets,
#' such that the zone of xxx * starts with a non-uppercase character.
#' "xxx" is currently expected to "not be UPPERCASE characters"?
#' Later, the xts return result is expected to be passed to "Current" and "Leading"
#' where "rets" is replaced by "currentrets" and "leadingrets"
#'
#' xTs values less that zero will generate a numeric error
#'
#' }
#'
#' @param xTs xts object of absolute values
#' @param ... dots if a 'returns generator' looks for the list
#' named "ReturnsGeneratorFUNArgs".
#' @return xts object of absolute proportional change returns
#' @examples
#' \dontrun{
#'
#' require(xts)
#' xTs  <- xts(10:12,zoo::as.Date(0:2))
#' apcr <- Returns(xTs, ReturnsGeneratorFUNArgs = list(is.na.zero = TRUE,  Fun = list(Fun = "APC", lag = 1)))
#' apcr
#'             V1apc.1rets
#' 1970-01-01 0.000000
#' 1970-01-02 0.100000
#' 1970-01-03 0.090909
#'
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_c str_locate str_sub
Returns <- function(xTs = NULL, ...)  {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  xTs  <- initXts(xTs)
  Dots <- list(...)

  if(!is.null(Dots[["ReturnsGeneratorFUNArgs"]])) {
    ReturnsGeneratorFUNArgs <- Dots[Names(Dots) %in% "ReturnsGeneratorFUNArgs"][["ReturnsGeneratorFUNArgs"]]
  }

  is.na.zero <- FALSE
  if(!is.null(ReturnsGeneratorFUNArgs[["is.na.zero"]])) {
    is.na.zero <- ReturnsGeneratorFUNArgs[["is.na.zero"]]
  }

  if(!is.null(ReturnsGeneratorFUNArgs[["Fun"]])) {
    if(!is.null(ReturnsGeneratorFUNArgs[["Fun"]][["Fun"]])) {

      Fun <- ReturnsGeneratorFUNArgs[["Fun"]][["Fun"]]

      if(mode(Fun) == "function") {
        Fun = match.fun(Fun)
      } else {
        # character function
        Fun <- get(Fun)
      }

    } else {
      stop("Returns did not find in dots ...: Fun.  So, do not know what to do.")
    }
  } else {
    stop("Returns did not find in dots ...: Fun.  So, do not know what to do.")
  }

  DotsFun <- ReturnsGeneratorFUNArgs[["Fun"]][!Names(ReturnsGeneratorFUNArgs[["Fun"]]) %in% "Fun"]

  # Colnames <- colnames(xTs)
  Colname <- colnames(xTs)[1]
  # absolute proportional change
  # just ONE column

  #            ORIG.apc.1
  #            GSPC.apc.1
  #            GDP.apc.3
  # xTsRets <- APC(xTs)
  xTsRets <- DescTools::DoCall(Fun, c(list(), list(xTs), DotsFun))
  xTsRetsColname <- colnames(xTsRets)[1]
  if(is.na.zero) {               # which(is.na(xTsRets)) # "close to logrithmic
    xTsRets[is.na(xTsRets)] <- 0 # usually just the 1st observation
  }
  # colnames(xTsRets)[1] <- stringr::str_c(Colnames, "apcrets")

  NumbLoc <- stringr::str_locate(xTsRetsColname, Colname)
  change  <- stringr::str_sub(xTsRetsColname, stringr::invert_match(NumbLoc))[2]

  xtsAttributes(xTsRets)[["change"]] <- change
  xtsAttributes(xTsRets)[["carry.xts.attributes"]] <- unique(c(xtsAttributes(xTsRets)[["carry.xts.attributes"]], "change"))
  #
  colnames(xTsRets)[1] <- stringr::str_c(colnames(xTsRets)[1], "rets")

  xTsRets

})}



#' generate a good xts column name
#'
#' @description
#' \preformatted{
#'
#' }
#'
#' @param x single column xts with the old column name
#' @return single column xts with  the new column name
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_c
#' @importFrom stringr str_replace_all
newXtsColName <- function(xTs = NULL, Fun =  NULL, isCharFun = NULL, xTs1 = NULL, xTs2 = NULL, WhichCombo =  NULL, AltName = NULL, Prefix = NULL, FixedSep = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  if(is.null(isCharFun)) stop("newXtsColName need actual paramter isCharFun")

  if(is.null(AltName)) {
    if(isCharFun) {
      NewName <- stringr::str_replace_all(Fun,"[.]|::",FixedSep)
    } else {
      NewName <- "anon"
    }
  } else {
    NewName <- AltName
  }

  stringr::str_c(
  c(
    if(!is.null(xTs1) && (NCOL(xTs1) > 0)) { colnames(xTs1)[1] } else { NULL },
    if(!is.null(xTs2) && (NCOL(xTs2) > 0)) { colnames(xTs2)[1] } else { NULL }
  ), collapse = FixedSep) -> Colnames

  if(length(WhichCombo)) {
    WhichCombo <-  stringr::str_c(c(interleave(names(WhichCombo), unlist(WhichCombo))), collapse = FixedSep)
  } else {
    WhichCombo <- NULL
  }

  PreName <- NULL; PostName <- NULL
  NewNameWhichCombo <- stringr::str_c(c(NewName, WhichCombo), collapse = FixedSep)
  if(is.null(Prefix) || (Prefix == FALSE)) {
    PostName <- NewNameWhichCombo
  } else {
    PreName  <- NewNameWhichCombo
  }
  NewName <- stringr::str_c(c(PreName, Colnames, PostName), collapse = FixedSep)
  colnames(xTs)[1] <-NewName

  xTs

})}



#' pairwise interleave of columns
#'
#' @description
#' \preformatted{
#'
#' This works better on xts objects.
#' (lapply or plyr::llply structure is held together.)
#'
#' If one or the other has one column while the other
#' has N columns, then the one will be recycled to N columns.
#'
#' Helper to eXplode.
#'
#' }
#'
#' @param x1 data.frame or xts object
#' @param x2 data.frame or xts object
#' @return list of length two of two data.frames or xts objects
#' @examples
#' \dontrun{
#'#
#'# list(iris[1:2,1:2], airquality[1:2,1:2])
#'# [[1]]
#'#   Sepal.Length Sepal.Width
#'# 1          5.1         3.5
#'# 2          4.9         3.0
#'#
#'# [[2]]
#'#   Ozone Solar.R
#'# 1    41     190
#'# 2    36     118
#'#
#'# > str( pairWise( iris[1:2,1:2], airquality[1:2,1:2] ) )
#'# List of 2
#'#  $ :List of 2
#'#   ..$ Sepal.Length: num [1:2] 5.1 4.9
#'#   ..$ Ozone       : int [1:2] 41 36
#'#  $ :List of 2
#'#   ..$ Sepal.Width: num [1:2] 3.5 3
#'#   ..$ Solar.R    : int [1:2] 190 118
#'#
#'# # 1 by N recycling
#'# #
#'#  > str( pairWise( iris[1:2,1:2], airquality[1:2,1, drop = F] ) )
#'#  List of 2
#'#   $ :List of 2
#'#    ..$ Sepal.Length: num [1:2] 5.1 4.9
#'#    ..$ Ozone       : int [1:2] 41 36
#'#   $ :List of 2
#'#    ..$ Sepal.Width: num [1:2] 3.5 3
#'#    ..$ Ozone      : int [1:2] 41 36
#'#
#'# > require(xts)
#'# > data("sample_matrix", package = "xts")
#'# > str( pairWise(as.xts(sample_matrix)[,1:2], as.xts(sample_matrix)[,3:4] ) )
#'# List of 2
#'#  $ :List of 2
#'#   ..$ Open:An 'xts' object on 2007-01-02/2007-06-30 containing:
#'#   Data: num [1:180, 1] 50 50.2 50.4 50.4 50.2 ...
#'#  - attr(*, "dimnames")=List of 2
#'#   ..$ : NULL
#'#   ..$ : chr "Open"
#'#   Indexed by objects of class: [POSIXct,POSIXt] TZ:
#'#   xts Attributes:
#'#  NULL
#'#   ..$ Low :An 'xts' object on 2007-01-02/2007-06-30 containing:
#'#   Data: num [1:180, 1] 50 50.2 50.3 50.2 50.1 ...
#'#  - attr(*, "dimnames")=List of 2
#'#   ..$ : NULL
#'#   ..$ : chr "Low"
#'#   Indexed by objects of class: [POSIXct,POSIXt] TZ:
#'#   xts Attributes:
#'#  NULL
#'#  $ :List of 2
#'#   ..$ High :An 'xts' object on 2007-01-02/2007-06-30 containing:
#'#   Data: num [1:180, 1] 50.1 50.4 50.4 50.4 50.2 ...
#'#  - attr(*, "dimnames")=List of 2
#'#   ..$ : NULL
#'#   ..$ : chr "High"
#'#   Indexed by objects of class: [POSIXct,POSIXt] TZ:
#'#   xts Attributes:
#'#  NULL
#'#   ..$ Close:An 'xts' object on 2007-01-02/2007-06-30 containing:
#'#   Data: num [1:180, 1] 50.1 50.4 50.3 50.3 50.2 ...
#'#  - attr(*, "dimnames")=List of 2
#'#   ..$ : NULL
#'#   ..$ : chr "Close"
#'#   Indexed by objects of class: [POSIXct,POSIXt] TZ:
#'#   xts Attributes:
#'#  NULL
#'#
#'# > ibm <- quantmod::getSymbols("IBM", from = "1970-01-01", to = "1970-01-13", auto.assign = FALSE)
#'#
#'# > pairWise(tail(ibm[,c("IBM.Open","IBM.Close")]), xts(, zoo::as.Date(0)[0]))
#'# [[1]]
#'# [[1]]$IBM.Open
#'# IBM.Open
#'# 1970-01-05   18.300
#'# 1970-01-06   18.413
#'# 1970-01-07   18.425
#'# 1970-01-08   18.438
#'# 1970-01-09   18.475
#'# 1970-01-12   18.450
#'#
#'# [[1]][[2]]
#'# Data:
#'#   numeric(0)
#'#
#'# Index:
#'#   Date of length 0
#'#
#'#
#'# [[2]]
#'# [[2]]$IBM.Close
#'# IBM.Close
#'# 1970-01-05    18.413
#'# 1970-01-06    18.425
#'# 1970-01-07    18.438
#'# 1970-01-08    18.475
#'# 1970-01-09    18.450
#'# 1970-01-12    18.388
#'#
#'# [[2]][[2]]
#'# Data:
#'#   numeric(0)
#'#
#'# Index:
#'#   Date of length 0
#'#
#'# > pairWise(tail(ibm[,c("IBM.Open")]), initXts())
#'# [[1]]
#'# [[1]]$IBM.Open
#'# IBM.Open
#'# 1970-01-05   18.300
#'# 1970-01-06   18.413
#'# 1970-01-07   18.425
#'# 1970-01-08   18.438
#'# 1970-01-09   18.475
#'# 1970-01-12   18.450
#'#
#'# [[1]][[2]]
#'# Data:
#'#   numeric(0)
#'#
#'# Index:
#'#   Date of length 0
#'#
#'#
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom DescTools DoCall
#' @importFrom plyr llply
pairWise <- function(x1, x2) {

  # # recycling 1 to N recycling
  # #
  # if((NVAR(x1) != NVAR(x2)) && NVAR(x2) == 1) {
  #
  #   x2 <- DescTools::DoCall(cbind, rep(list(x2),NVAR(x1)))
  #   Names(x2) <- rep(Names(x2)[1], NVAR(x2))
  #
  # }
  # if((NVAR(x1) != NVAR(x2)) && NVAR(x1) == 1) {
  #
  #   x1 <- DescTools::DoCall(cbind, rep(list(x1),NVAR(x2)))
  #   Names(x1) <- rep(Names(x1)[1], NVAR(x1))
  # }

  # recycling 1 to N recycling
  #
  RepDone <- FALSE
  if(!RepDone && NVAR(x1) != NVAR(x2) && NVAR(x1) > 0 && NVAR(x2) == 1) {

    x2 <- DescTools::DoCall(cbind, rep(list(x2),NVAR(x1)))
    Names(x2) <- rep(Names(x2)[1], NVAR(x2))
    RepDone <- TRUE
  }
  if(!RepDone && NVAR(x1) != NVAR(x2) && NVAR(x2) > 0 && NVAR(x1) == 1) {

    x1 <- DescTools::DoCall(cbind, rep(list(x1),NVAR(x2)))
    Names(x1) <- rep(Names(x1)[1], NVAR(x1))
    RepDone <- TRUE
  }
  #
  # if(!RepDone && NVAR(x1) != NVAR(x2) && (NVAR(x2) == 0 || NVAR(x1) == 0)) {
  #
  #   if(NVAR(x1) == 0) {
  #     x1 <- NULL
  #   }
  #
  #   if(NVAR(x2) == 0) {
  #     x2 <- NULL
  #   }
  #
  #   RepDone <- TRUE
  # }


  if(!RepDone && NVAR(x1) != NVAR(x2) && (NVAR(x2) == 0 || NVAR(x1) == 0)) {

    if(!RepDone && NVAR(x1) == 0 && NVAR(x2) > 0) {
      x1 <- rep(list(x1), NVAR(x2))
       RepDone <- TRUE
    }

    if(!RepDone && NVAR(x1) > 0  && NVAR(x2) == 0) {
      x2 <- rep(list(x2), NVAR(x1))
      RepDone <- TRUE
    }

  }

  List <- c(list(),plyr::llply(x1, identity), plyr::llply(x2, identity))

  if(length(List) > 2 ) { # e.g. 4, 6, 8, ...
    L1coord <- seq(from = 1, by = 2, length.out = 0.5*length(List))
    L2coord <- seq(from = 2, by = 2, length.out = 0.5*length(List))
    res <- c(list(List[L1coord]),list(List[L2coord]))
  } else { # i.e. 2
    res <- list(List)
  }

  return(res)

}


#' Interleave two vectors of arbitrary length
#'
#' @description
#' \preformatted{
#'
#' from R CRAN package rmngb
#'
#' }
#'
#' @export
interleave <- function (x, y)
{
    iX <- 2 * seq_along(x) - 1
    iY <- 2 * seq_along(y)
    c(x, y)[order(c(iX, iY))]
}




#' liquify a data.frame
#'
#' @description
#' \preformatted{
#'
#' From a long data.frame, makes it into a wide data.frame.
#'
#' First, split-off into many data.frames using UniqueIDRegex.
#' (and 100 percent correlated ConstColsRegex)
#'
#' Next, generates new columns using FactorColsRegex and then
#' repositions data items into those new columns (see the example)
#' Data items are found in columns that are 'not members' of
#' UniqueIDRegex, ConstColsRegex, and FactorColsRegex
#'
#' Last, "smart" merges all of the data.frames together while
#' adding new columns contributed by each splitted-off data.frame.
#' Results, will/should be (hopfully) "less" rows with many more columns.
#'
#' }
#'
#' @param UniqueIDRegex unique IDs column names regular expression: determines the
#' character vector of the columns that compose of the "unique identifier" for the row.
#' This combination splits the input data.frame(x) into many data.frames(x - splitted).
#' @param ConstColsRegex constant column names regular expression: determines the
#' character vector of (1) the columns that compose of the "unique identifier" for the row.
#' (must include columns determined by parameter "UniqueIDRegex")
#' and (2) optionally its 100 percent correlated columns (with values that
#' do not vary with the values of the "unique identifer" columns.)
#' An examples would be another datatype or alias or alternate name: eg. 17000 "2016-07-18"
#' @param FactorColsRegex reqular expression that determines the columns
#' to be flattened. Also the program, it "subtracts off" columns found in parameter "ConstColsRegex".
#' @param FactorColsNAReplace of columns that are the "result of FactorColsRegex",
#' replacement of the NA values.  Good options may be "None" or "Unknown" or "NotAppl".
#' @param FactorColsFixedSep output FactorColsRegex divider(concatinator) characters
#' @param DetailColsFixedSep output "generated (new) columns" divider(concatinator) characters
#' @param SpaceFixedSep just before the data is returned, replace column name
#' character spaces with this value
#' @param AmperstandFixedSep just before the data is returned, replace column name
#' amperstand(&) characters with this value(And)
#' @examples
#' \dontrun{
#'#
#'# data.frame(
#'#   dateindexid = c(17000, 17000, 17000, 17000, 17500, 17500, 17500, 17500),
#'#   dateindexFact = c("2016-07-18", "2016-07-18", "2016-07-18", "2016-07-18",
#'#     "2017-11-30", "2017-11-30", "2017-11-30", "2017-11-30"),
#'#   ActionFact = c("Morn", "Morn", "Night", "Night", "Morn", "Aftern", "Aftern", "Night"),
#'#   ActionAtFact = c("Sell", "Buy", "Sell", "Buy", "Sell", "Buy", "Hold", "Buy"),
#'#   INSTR1 = c(8, 16, 32, 64, 108, 116, 132, 164),
#'#   INSTR2 = c(10008, 10016, 10032, 10064, 10108, 10116, 10132, 10164)
#'# , stringsAsFactors = FALSE
#'# )  -> DFS
#'#
#'# > DFS
#'#   dateindexid dateindexFact ActionFact ActionAtFact INSTR1 INSTR2
#'# 1       17000    2016-07-18       Morn         Sell      8  10008
#'# 2       17000    2016-07-18       Morn          Buy     16  10016
#'# 3       17000    2016-07-18      Night         Sell     32  10032
#'# 4       17000    2016-07-18      Night          Buy     64  10064
#'# 5       17500    2017-11-30       Morn         Sell    108  10108
#'# 6       17500    2017-11-30     Aftern          Buy    116  10116
#'# 7       17500    2017-11-30     Aftern         Hold    132  10132
#'# 8       17500    2017-11-30      Night          Buy    164  10164
#'#
#'# > options(width = 60)
#'#
#'# > liquifyDF(DFS)
#'#   dateindexid dateindexFact Morn.Sell.INSTR1
#'# 1       17000    2016-07-18                8
#'# 2       17500    2017-11-30              108
#'#   Morn.Buy.INSTR1 Night.Sell.INSTR1 Night.Buy.INSTR1
#'# 1              16                32               64
#'# 2              NA                NA              164
#'#   Morn.Sell.INSTR2 Morn.Buy.INSTR2 Night.Sell.INSTR2
#'# 1            10008           10016             10032
#'# 2            10108              NA                NA
#'#   Night.Buy.INSTR2 Aftern.Buy.INSTR1 Aftern.Hold.INSTR1
#'# 1            10064                NA                 NA
#'# 2            10164               116                132
#'#   Aftern.Buy.INSTR2 Aftern.Hold.INSTR2
#'# 1                NA                 NA
#'# 2             10116              10132
#'#
#' }
#'@export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_c
#' @importFrom stringr str_replace_all
#' @importFrom tidyselect vars_select
#' @importFrom tidyselect matches
#' @importFrom DescTools DoCall
#' @importFrom dplyr select
#' @importFrom seplyr select_se
#' @importFrom wrapr let
#' @importFrom R.utils wrap
#' @importFrom tidyr unite
#' @importFrom data.table rbindlist
liquifyDF <- function(x
                       , UniqueIDRegex =  "^dateindexid$"
                       , ConstColsRegex = "^dateindex"
                       , FactorColsRegex =        "Fact$"
                       , FactorColsNAReplace = NULL
                       , FactorColsFixedSep = "."
                       , DetailColsFixedSep = "."
                       , SpaceFixedSep =      "."
                       , AmperstandFixedSep = "And"
                      ) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  # probably can be redone using someone's [melt]/[d]cast,
  #   tidyr:: . . .(redone better)
  # see (along with reshape2):
  #   QuantTools::lmerge and MY(ANDRE) lmerge2 (startout generalization)

  xOrig <- x

  if(NROW(x) == 0) { message("liquifyDF found zero rows"); return(data.frame()) }

  # "unique id" columns (unique row identifier columns)
  # "dateindexid"
  UniqueIDCols <- tidyselect::vars_select(names(x), tidyselect::matches(UniqueIDRegex))

  # "unique id" columns (unique row identifier columns)
  # and 100% correlated columns
  # "dateindexid" "dateindexFact"(100% correlated column)
  ConstCols <- tidyselect::vars_select(names(x), tidyselect::matches(ConstColsRegex))

  UniqueIDInteraction <- DescTools::DoCall(interaction, list(x[, UniqueIDCols, drop = F], drop = T))
  xSplittedByUniqueID <- split(x, f = UniqueIDInteraction)

  resultsInList <- list()
  for(x in xSplittedByUniqueID) {

    # tidyselect::vars_select(names(x), tidyselect::matches(FactorColsRegex)) . . .
    # "dateindexFact"    "ActionFact"  "ActionAtFact"
    #
    # garantee no 'id/correlated' columns ( and the [rest of] factors )
    # expected to be 'flattened' are the following . . .
    # ActionFact"   "ActionAtFact"
    FCT_COLS_VECTOR <- setdiff(tidyselect::vars_select(names(x), tidyselect::matches(FactorColsRegex)), ConstCols)
    FCT_COLS_NAME   <- stringr::str_c(FCT_COLS_VECTOR, collapse = FactorColsFixedSep)
    FCT_COLS_SEP    <- stringr::str_c(FCT_COLS_VECTOR, collapse = ", ")

    if(!is.null(FactorColsNAReplace)) {
      for(coli in FCT_COLS_VECTOR) {
        x[is.na(x[, coli, drop = TRUE]),coli] <- FactorColsNAReplace
      }
    }

    LeftSideRow1  <-  seplyr::select_se(x, ConstCols)[1, , drop = FALSE]
    NotLeftSide   <-  seplyr::deselect(x, ConstCols)

    UNITE <- function(x) {
      wrapr::let(list(FCT_COLS_NAME = FCT_COLS_NAME, FCT_COLS_SEP = FCT_COLS_SEP),
        tidyr::unite(x, FCT_COLS_NAME, FCT_COLS_SEP, sep = FactorColsFixedSep)
      , subsMethod = "stringsubs", strict = FALSE)
      }

    # make ONE column to represent all factors
    NotLeftSide %>% UNITE %>%
      # change row.names to FCT_COLS_NAME, drop column 1
      `row.names<-`(.[[1]]) %>% dplyr::select(-1) %>%
        # to one dimension : one BIG wide ROW
        as.matrix %>% R.utils::wrap(sep = DetailColsFixedSep) -> NotLeftSide

    cbind(LeftSideRow1,as.data.frame(t(NotLeftSide))) -> results

    stringr::str_replace_all(colnames(results),"[ ]", SpaceFixedSep) ->
      colnames(results)
    stringr::str_replace_all(colnames(results),"&"  , AmperstandFixedSep) ->
      colnames(results)

    resultsInList <- c(resultsInList, list(results))

  }
  # choosing # data.table::rbindlist(resultsInList, fill = TRUE)
  # over purrr::map_dfr(resultsInList, identity)
  # because # data.table::rbindlist(resultsInList) # fill = FALSE(default)
  # can(flexibly) match by position ( I am matching by "name" )
  resultsOneDF <- as.data.frame(data.table::rbindlist(resultsInList, fill = TRUE), stringsAsFactors = FALSE)
  return(resultsOneDF)

})}



#' months after the event
#'
#' @description
#' \preformatted{
#'
#' }
#'
#' @param x xts object
#' @param dates vector of Dates of the events
#' @return xts object with one new column
#' @examples
#' \dontrun{
#'
#' # S&P 500
#' gspc <- To.Monthly(Cl(getSymbols("^GSPC", auto.assign = FALSE, from = "1900-01-01")), indexAt = "lastof", OHLC = F)
#' colnames(gspc) <- "GSPC"
#' # returns
#' gspc.ret <- PerformanceAnalytics::Return.calculate(gspc)
#'
#' # top 20 worst monthly returns in the S&P500 history
#' CrashDates <- zoo::as.Date(rownames(head(as.matrix(gspc.ret)[order(as.matrix(gspc.ret)),1, drop = F], 20)))
#'
#' monthsAfterEvent(gspc.ret, dates = CrashDates)
#'
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom mondate  as.mondate
#' @importFrom mondate MonthsBetween
#' @importFrom DescTools DoCall
#' @importFrom DataCombine MoveFront
#' @export
monthsAfterEvent <- function(x, dates) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  xTs <- initXts(x)
  EventDates <- initDate(dates)

  # index
  MostRecentEventDateInd <- findInterval(
    as.numeric(index(xTs)), as.numeric(sort(EventDates)) ,
    left.open = T, rightmost.closed = T
  ) %>%
    { .[. == 0] <- NA; . }

  MonthsAfterEventDay <- mondate::MonthsBetween(
     mondate::as.mondate(index(xTs)), mondate::as.mondate(sort(EventDates)[MostRecentEventDateInd])
  )

  NewClmName <- stringr::str_c(colnames(xTs)[1], "_MTHDLY")
  newClmList <- list()
  newClmList[[NewClmName]] <- MonthsAfterEventDay
  xTs <- DescTools::DoCall(cbind, c(list(),list(xTs), newClmList))
  xTs <- DataCombine::MoveFront(xTs, Var = colnames(xTs)[!colnames(xTs) %in% names(newClmList)])
  xTs

})}



#' days after the event
#'
#' @description
#' \preformatted{
#'
#' }
#'
#' @param x xts object
#' @param dates vector of Dates of the events
#' @return xts object with one new column
#' @examples
#' \dontrun{
#'
#' # S&P 500
#' gspc <- To.Monthly(Cl(getSymbols("^GSPC", auto.assign = FALSE, from = "1900-01-01")), indexAt = "lastof", OHLC = F)
#' colnames(gspc) <- "GSPC"
#' # returns
#' gspc.ret <- PerformanceAnalytics::Return.calculate(gspc)
#'
#' # top 20 worst monthly returns in the S&P500 history
#' CrashDates <- zoo::as.Date(rownames(head(as.matrix(gspc.ret)[order(as.matrix(gspc.ret)),1, drop = F], 20)))
#'
#' daysAfterEvent(gspc.ret, dates = CrashDates)
#'
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom mondate  as.mondate
#' @importFrom mondate DaysBetween
#' @importFrom DescTools DoCall
#' @importFrom DataCombine MoveFront
#' @export
daysAfterEvent <- function(x, dates) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  xTs <- initXts(x)
  EventDates <- initDate(dates)

  # index
  MostRecentEventDateInd <- findInterval(
    as.numeric(index(xTs)), as.numeric(sort(EventDates)) ,
    left.open = T, rightmost.closed = T
  ) %>%
    { .[. == 0] <- NA; . }

  DaysAfterEventDay <-   mondate::DaysBetween(
     mondate::as.mondate(index(xTs)), mondate::as.mondate(sort(EventDates)[MostRecentEventDateInd])
  )

  NewClmName <- stringr::str_c(colnames(xTs)[1], "_DAYDLY")
  newClmList <- list()
  newClmList[[NewClmName]] <- DaysAfterEventDay
  xTs <- DescTools::DoCall(cbind, c(list(),list(xTs), newClmList))
  xTs <- DataCombine::MoveFront(xTs, Var = colnames(xTs)[!colnames(xTs) %in% names(newClmList)])
  xTs

})}



#' days since the event
#'
#' wraps doBy::timeSinceEvent
#'
#' @description
#' \preformatted{
#'
#' }
#'
#' @param x xts object
#' @param dates vector of Dates of the events
#' @return xts object with one new column
#' @examples
#' \dontrun{
#'
#' # S&P 500
#' gspc <- To.Monthly(Cl(getSymbols("^GSPC", auto.assign = FALSE, from = "1900-01-01")), indexAt = "lastof", OHLC = F)
#' colnames(gspc) <- "GSPC"
#' # returns
#' gspc.ret <- PerformanceAnalytics::Return.calculate(gspc)
#'
#' # top 20 worst monthly returns in the S&P500 history
#' CrashDates <- zoo::as.Date(rownames(head(as.matrix(gspc.ret)[order(as.matrix(gspc.ret)),1, drop = F], 20)))
#'
#' daysSinceEvent(gspc.ret, dates = CrashDates)
#'
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom doBy timeSinceEvent
#' @importFrom DescTools DoCall
#' @importFrom DataCombine VarDrop MoveFront
#' @export
daysSinceEvent <- function(x, dates) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  xTs <- initXts(x)
  EventDates <- initDate(dates)

  # xts object of one column of values: NA or 1 (flag of Event on that Date)
  EventIdc <- merge(xts(rep(1,NROW(EventDates)),CrashDates), xts(,index(xTs)))

  DaysSinceEventDay <- doBy::timeSinceEvent( c(coredata(EventIdc)), index(EventIdc) )
  colnames(DaysSinceEventDay) <- c("EVENTIDC", "INDEX", "ABSTSE", "SIGNTSE", "EWIN", "RUN", "TAE", "TBE")

  # not used/ not useful(EWING)/can not use to predict
  DaysSinceEventDay <- DataCombine::VarDrop(DaysSinceEventDay, Var = c("INDEX", "ABSTSE", "SIGNTSE", "EWIN", "TBE"))
  colnames(DaysSinceEventDay) <- stringr::str_c(colnames(xTs)[1], "_", colnames(DaysSinceEventDay), "_DAYSNC")

  xTs <- DescTools::DoCall(cbind, c(list(),list(xTs), DaysSinceEventDay))
  xTs <- DataCombine::MoveFront(xTs, Var = colnames(xTs)[!colnames(xTs) %in% colnames(DaysSinceEventDay)])
  xTs

})}



#' months since the event
#'
#' wraps doBy::timeSinceEvent
#'
#' @description
#' \preformatted{
#'
#' }
#'
#' @param x xts object
#' @param dates vector of Dates of the events
#' @return xts object with one new column
#' @examples
#' \dontrun{
#'
#' # S&P 500
#' gspc <- To.Monthly(Cl(getSymbols("^GSPC", auto.assign = FALSE, from = "1900-01-01")), indexAt = "lastof", OHLC = F)
#' colnames(gspc) <- "GSPC"
#' # returns
#' gspc.ret <- PerformanceAnalytics::Return.calculate(gspc)
#'
#' # top 20 worst monthly returns in the S&P500 history
#' CrashDates <- zoo::as.Date(rownames(head(as.matrix(gspc.ret)[order(as.matrix(gspc.ret)),1, drop = F], 20)))
#'
#' monthsSinceEvent(gspc.ret, dates = CrashDates)
#'
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom doBy timeSinceEvent
#' @importFrom zoo as.Date as.yearmon
#' @importFrom plyr llply
#' @importFrom DescTools DoCall
#' @importFrom DataCombine VarDrop MoveFront
#' @export
monthsSinceEvent <- function(x, dates) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  xTs <- initXts(x)
  EventDates <- initDate(dates)

  # xts object of one column of values: NA or 1 (flag of Event on that Date)
  EventIdc <- merge(xts(rep(1,NROW(EventDates)),CrashDates), xts(,index(xTs)))

  # in year.fraction
  MonthsSinceEventDay <- doBy::timeSinceEvent(c(coredata(EventIdc)), as.numeric(as.yearmon(index(EventIdc))))

  plyr::llply( rlist::list.zip(ColName = colnames(MonthsSinceEventDay), ColData = MonthsSinceEventDay ), function(x) {

    if(x[["ColName"]] %in% c("yvar", "ewin", "run"))
      return(x[["ColData"]])
    if(x[["ColName"]] == "tvar")
      return(zoo::as.Date(zoo::as.yearmon(x[["ColData"]]), frac = 1))
    if(!x[["ColName"]] %in% c("yvar","tvar", "ewin", "run"))
      return(( as.yearmon(x[["ColData"]] ) - as.yearmon(0)) * 12)

  }) %>% as.data.frame -> MonthsSinceEventDay

  colnames(MonthsSinceEventDay) <- c("EVENTIDC", "INDEX", "ABSTSE", "SIGNTSE", "EWIN", "RUN", "TAE", "TBE")

  # not used/ not useful(EWING)/can not use to predict
  MonthsSinceEventDay <- DataCombine::VarDrop(MonthsSinceEventDay, Var = c("INDEX", "ABSTSE", "SIGNTSE", "EWIN", "TBE"))
  colnames(MonthsSinceEventDay) <- stringr::str_c(colnames(xTs)[1], "_", colnames(MonthsSinceEventDay), "_MTHSNC")

  xTs <- DescTools::DoCall(cbind, c(list(),list(xTs), MonthsSinceEventDay))
  xTs <- DataCombine::MoveFront(xTs, Var = colnames(xTs)[!colnames(xTs) %in% colnames(MonthsSinceEventDay)])
  xTs

})}



#' (true) sortino ratio
#'
#' @description
#' \preformatted{
#'
#' Sortino Ratio: Are you calculating it wrong?
#' https://www.rcmalternatives.com/2013/09/sortino-ratio-are-you-calculating-it-wrong/
#'
#' I CAN GET RID OF THIS FUNCTION
#' THIS WORKS CORRECTLY "AS IS".
#' See. . . .
#' Add feature zeroMAR to SortinoRatio ( actually DownsideDeviation ) #106
#' https://github.com/braverock/PerformanceAnalytics/issues/106
#'
#' }
#'
#' @examples
#' \dontrun{
#' # trueSortinoRatio(x, n, rf = 0.0, na.rm = FALSE)
#' }
#'@export
trueSortinoRatio <- function(x, MinRows, rf = 0.0, na.rm = FALSE) {
                                            # any NA, then entire thing returns NA
  # if not too short
  if(MinRows <=  NROW(x)) {
    (mean(x, na.rm = na.rm) - rf )/sd( local({x[x > rf] <- 0; x } ), na.rm = na.rm)
  } else { # too short
    NA_real_
  }
}



#' apply rolling
#'
#' @description
#' \preformatted{
#'
#' The best roller can roll anything with the most features.
#' The idea is that I can control(change) the INTERIOR data OF NAs founds
#' within the middle of the data.
#' This data and middle of data is meant to be sent
#' to package TTR/PerformanceAnaltics functions.
#' TTR functions reject(error out) on NAs found in the interior.
#'
#' Here I can estimate or replace those NAs found in the middle of the data
#' before that data reaches TTR functions
#'
#' }
#'
#' @param xTs xts object
#' @return modified xts object
#'
#' @examples
#' \dontrun{
#'#
#'#
#'# TTR::SMA(xts(c(0,1,2,3,NA), zoo::as.Date(0:4)), n = 2)
#'# Error in runSum(x, n) : Series contains non-leading NAs
#'#
#'# TTR::SMA(xts(c(0,1,NA,3,4), zoo::as.Date(0:4)), n = 2)
#'# Error in runSum(x, n) : Series contains non-leading NAs
#'#
#'# TTR::SMA(xts(c(NA,1,2,3,4), zoo::as.Date(0:4)), n = 2)
#'#            SMA
#'# 1970-01-01  NA
#'# 1970-01-02  NA
#'# 1970-01-03 1.5
#'# 1970-01-04 2.5
#'# 1970-01-05 3.5
#'#
#'# rollApply(xts(c(0,-1,2,-3,4), zoo::as.Date(0:4)),
#'#   Fun = trueSortinoRatio, AltName = "SRTN", partial = TRUE, width = 2, MinRows = 2)
#'#
#'#                          SRTN.2
#'# 1970-01-01                   NA
#'# 1970-01-02 -0.70710678118654746
#'# 1970-01-03  0.70710678118654746
#'# 1970-01-04 -0.23570226039551587
#'# 1970-01-05  0.23570226039551587
#'#
#'# # dimunitive SMA ( removes NA values )
#'# DSMA <- function(x, n, QTY) {
#'#                        # SMA requirement
#'#   if((n <= NROW(x)) && (2 <= NROW(x))) {
#'#
#'#     # remove data
#'#     if(is.vector(x))       x <- x[!is.na(x)]
#'#     if(inherits(x, "zoo")) x <- x[coredata(!is.na(x)),]
#'#
#'#                           # SMA requirement
#'#     if((n <= NROW(x)) && (2 <= NROW(x))) {
#'#       result <- TTR::SMA(x = x, n = n)
#'#       if(QTY == "All") {
#'#         result
#'#       } else if(QTY == "Last") {
#'#         tail(result,1)
#'#       } else { stop("Need to provide QTY") }
#'#     } else {
#'#       return(NA_real_)  # too short
#'#     }
#'#
#'#   } else {
#'#     return(NA_real_)  # too short
#'#   }
#'#
#'# }
#'# > xts(c(0,1,NA,3,4), zoo::as.Date(0:4))
#'#            [,1]
#'# 1970-01-01    0
#'# 1970-01-02    1
#'# 1970-01-03   NA
#'# 1970-01-04    3
#'# 1970-01-05    4
#'#
#'# DSMA(xts(c(0,1,NA,3,4), zoo::as.Date(0:4)), 2, QTY = "All")
#'#
#'#            SMA
#'# 1970-01-01  NA
#'# 1970-01-02 0.5  # I programmed to loose NA observations ( and its index entries)
#'# 1970-01-04 2.0  # unless, I come up with a \*better way\* to re-organize 'around' the [NA] data
#'# 1970-01-05 3.5  # then that is the best answer I have NOW.
#'#
#'# rollApply(xts(c(0,1,NA,3,4), zoo::as.Date(0:4))
#'#   , Fun = DSMA, AltName = "MATH", partial = FALSE, width = 2, n = 2, QTY = "Last")
#'#
#'# because I programmed around THIS "partial"
#'# rollApply(xts(c(0,1,NA,3,4), zoo::as.Date(0:4))
#'#   , Fun = DSMA, AltName = "MATH", partial = TRUE, width = 2, n = 2,QTY = "Last")
#'#
#'#            MATH.2
#'# 1970-01-01     NA
#'# 1970-01-02    0.5
#'# 1970-01-03     NA
#'# 1970-01-04     NA
#'# 1970-01-05    3.5
#'#
#'#
#'#
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom zoo rollapply
rollApply <- function(
        xTs = NULL, width = NULL, partial = NULL, align = NULL, Fun = NULL
      , by = NULL, by.column = NULL, fill = NULL, coredata = NULL
      , AltName = NULL, FixedSep  = NULL
      , ...) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  # NOTE: POSSIBLY to be rewritten with rowr::rollApply

  xTsOrig <- xTs
  tryXtsSuccess <- FALSE
  tryXts <- try(try.xts(xTsOrig), silent = T)
  if(any(class(xTs) %in% "try-error")) {
    stop("rollApply can not make an xts object")
  } else {
    tryXtsSuccess <- TRUE;
    xTs <- tryXts
  }

  if(mode(Fun) == "function") {
    Fun = match.fun(Fun)
    isCharFun <- FALSE
  } else {
    isCharFun <- TRUE
  }

  if(is.null(width))   { width <- 2 }
  if(is.null(partial)) { partial <- TRUE }
  if(is.null(align))   { align <- "right" }
  if(is.null(Fun))     { Fun   <- trueSortinoRatio }

  if(is.null(by))        { by <- 1 }
  if(is.null(by.column)) { by.column <- TRUE }
  if(is.null(fill))      { fill <- NA }
  if(is.null(coredata))  { coredata <- TRUE }

  # I must be explicit (I do not want to use xts:::rollapply.xts)
  zoo::rollapply(as.zoo(xTs)
      , width = width
      , partial = partial
      , align = align
      , FUN = Fun
      , by = by
      , by.column = by.column
      , fill = fill
      , coredata = coredata
      , ...) -> xTsResult

  # would/should always be/been true
  # else I may/have/never ever made it his far
  if(tryXtsSuccess) {
    xTsResult <- reclass(xTsResult, xTsOrig)
  }

  NewName <- "rollApply"
  if(!is.null(AltName)) NewName <- AltName
  if(is.null(FixedSep)) FixedSep <- "."

  colnames(xTsResult) <-  stringr::str_c( colnames(xTsResult), stringr::str_c(c(NewName, width), collapse = FixedSep), collapse = FixedSep)

  return(xTsResult)

})}



#' Allows row indexing without knowledge of dimensionality or class.
#'
#' @description
#' \preformatted{
#'
#' This is an improved version of rowr::rows.
#' This will not drop tables/arrays/data.frames
#' of one dimension down to dimensionless vectors.
#'
#' }
#'
#' @param data any R object
#' @param rownums indices of target rows
#' @param ... dots may include the user-sent parameter "drop = TRUE"
#' @examples
#' \dontrun{
#'
#' Rows(state.x77[, 1, drop = FALSE], 13:15)
#'          Population
#' Illinois      11197
#' Indiana        5313
#' Iowa           2861
#'
#' Rows(iris[, 1, drop = FALSE], 13:15)
#'    Sepal.Length
#' 13          4.8
#' 14          4.3
#' 15          5.8
#'
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @export
Rows <- function(data, rownums, ...) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  ops <- options()
  options(stringsAsFactors = FALSE)

 # alternative to drop=FALSE or changing default behavior [duplicate]
 # https://stackoverflow.com/questions/15886974/alternative-to-drop-false-or-changing-default-behavior
 # AND
 # Generally disable dimension dropping for matrices?
 # https://stackoverflow.com/questions/12196724/generally-disable-dimension-dropping-for-matrices

  Dots <- list(...)

  # data.table and 'tibble's do not have this problem
  if(class(data)[1] %in% c("data.frame", "matrix", "table", "array")) {
    # only extract "drop" (if present in ... )
    if(is.null(Dots[["drop"]])) {
      old <- `[`
      `[` <- function(...) { old(..., drop=FALSE) }
    }
  }

  if (is.null(dim(data))) {
    result <- data[rownums]
  }
  else {
    result <- data[rownums, ]
  }

  options(ops)

  return((result))

})}



#' rollapply anything
#'
#' @description
#' \preformatted{
#'
#' This is based on package rowr function rollApply (rollapply anything).
#' Uses Rows so single dimension objects are not dropped to vectors.
#'
#' }
#'
#' @param data	any R object
#' @param fun	the function to evaluate
#' @param window	window width defining the size of the subset available
#' to the fun at any given point
#' @param minimum	minimum width of the window.
#' Will not return results if the window is truncated below this value
#' at the end of the data set
#' @param align	whether to align the window right or left
#' @param ...	additional arguments to pass to fun
#' @examples
#' \dontrun{
#' all examples of rowr::rollApply work
#'
#' rollApply2(state.x77, function(x) { browser() }, window = 3, min = 3, align = "right")
#' Called from:
#' Browse[1]> print(x)
#'         Population Income Illiteracy Life Exp Murder HS Grad Frost   Area
#' Alabama       3615   3624        2.1    69.05   15.1    41.3    20  50708
#' Alaska         365   6315        1.5    69.31   11.3    66.7   152 566432
#' Arizona       2212   4530        1.8    70.55    7.8    58.1    15 113417
#'
#' States <- rollApply2(state.x77, function(x) { tail(x,1) }, window = 3, min = 3, align = "right")
#'
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom plyr llply
#' @importFrom DescTools DoCall
#' @export
rollApply2 <- function(x, fun, window = len(x), minimum = 1, align = "left",  ...) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  if (minimum > NROW(x)) return()
  FUN = match.fun(fun)
  if (align == "left") {
    result <- plyr::llply( 1:(NROW(x) - minimum + 1), function(x2) {
       FUN(Rows(x,      x2:(min(NROW(x), (x2 + window - 1)))     , ...), ...)
    })
  }
  if (align == "right") {
    result <- plyr::llply(            minimum:NROW(x), function(x2) {
       FUN(Rows(x,      max(1,       x2 - window + 1       ):x2  , ...), ...)
    })
  }
  if(is.null(dim(x))) {
    result <- do.call(c, result)
  } else {
    # assuming x has an rbind method
    result <- DescTools::DoCall(rbind, result)
  }


  return(result)

})}




#' rolling percent ranks grouped by epoch
#'
#' @description
#' \preformatted{
#'
#' The idea is group rolling ranks from the end of crash/recession
#' to the beginning of the next crash/recession (so that the
#' recession to recession social/economic behavior may be
#' recaptured.)
#'
#' [Maybe] this is (weakly) designed to run through "eXplode."
#'
#' }
#'
#' @param xTs1 xts object
#' @param xTs2 xts object of single column
#' (or multiple columns if doing interaction).
#' This is an xts object with a data flag of the groupings.
#' This is most likely calculated from this package's
#' days/months\*Since/After\*Event functions.
#' @param window size of zone of rank calculation
#' @param ranks number of ranks
#' @param incBeforeEpochLags FALSE(default), if TRUE include early data the size of "window - 1".
#' @param originalData FALSE(default), if TRUE include oringinal data in the output.
#' @param derivedDataDetailed FALSE(default) if TRUE, include the rank as part of the derived data column name.
#' @return xts object
#' @examples
#' \dontrun{
#'
#' # S&P 500
#' gspc <- To.Monthly(Cl(getSymbols("^GSPC", auto.assign = FALSE, from = "1900-01-01")), indexAt = "lastof", OHLC = F)
#' colnames(gspc) <- "GSPC"
#' # returns
#' gspc.ret <- PerformanceAnalytics::Return.calculate(gspc)
#'
#' # top 20 worst monthly returns in the S&P500 history
#' CrashDates <- zoo::as.Date(rownames(head(as.matrix(gspc.ret)[order(as.matrix(gspc.ret)),1, drop = F], 20)))
#'
#' mse <- monthsSinceEvent(gspc.ret, dates = CrashDates)
#' mse <- mse[ , c("GSPC","GSPC_TAE_MTHSNC", "GSPC_RUN_MTHSNC")]
#' # 20 (or near so) epochs
#' unique(mse$GSPC_RUN_MTHSNC)
#'
#' # nothing (no crash date) before 1962
#' tail(mse["/1962"], 10)
#'
#' # e.g. (will be used on function
#' # from crash to crash
#' # "RUN month(MTH) since(SNC) event"
#' smse <- split(mse[, c("GSPC", "GSPC_TAE_MTHSNC")], coredata(mse[, "GSPC_RUN_MTHSNC"]))
#'
#' # 20 periods
#' length(smse)
#' [1] 20
#'
#' # sample testing/demo data
#' ## mse <- mse[114:118,]
#'
#' # note USUALLY "incBeforeEpochLags == FALSE"
#' # to "fully" separate ONE epoch from the other.
#' # to get/give full separation before/after recession crashes
#' # incBeforeEpochLags = FALSE
#' rollEpochRanks(mse[, c("GSPC", "GSPC_TAE_MTHSNC")], xTs2 = mse[, "GSPC_RUN_MTHSNC"], incBeforeEpochLags = TRUE, originalData = TRUE, derivedDataDetailed = TRUE)
#'
#' #            GSPC GSPC_TAE_MTHSNC GSPC.rollEpochRanks4 GSPC_TAE_MTHSNC.rollEpochRanks4
#' # 1950-01-31   NA              NA                   NA                              NA
#'
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom plyr llply
#' @export
rollEpochRanks <- function(xTs1, xTs2, window = 10, ranks = 4, incBeforeEpochLags = FALSE, originalData = FALSE, derivedDataDetailed = FALSE) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  EpochIndexes <- plyr::llply(split(xTs2, interaction(coredata(xTs2))), function(x) index(x))

  AllEpochResults <- list()
  for(EpochIndex in EpochIndexes) {

    if(incBeforeEpochLags) {
      EarlierAndEpochIndex         <- c(index(xTs1)[index(xTs2) < head(EpochIndex,1)], EpochIndex)
      OnlyEarlyEpochIndex          <- setDiff(EarlierAndEpochIndex, EpochIndex)
      SavedLagsOnlyEarlyEpochIndex <- tail(OnlyEarlyEpochIndex, window - 1)
      NewEpochIndex                <- c(SavedLagsOnlyEarlyEpochIndex, EpochIndex)
    } else {
      NewEpochIndex <- EpochIndex
    }

    plyr::llply(xTs1[NewEpochIndex], function(x2, window, ranks) {
      res <- runRanksTTR(x2, window = window, ranks = ranks)
      colnames(res) <- colnames(x2)
      return(res)
    }, window = window, ranks = ranks) -> EpochRes
    # assuming it does have a cbind method
    EpochRes <- DescTools::DoCall(cbind, EpochRes)
    # take off the extra stuff that I had added
    if(incBeforeEpochLags == TRUE) {
      EpochRes <- tail(EpochRes, NROW(EpochRes) - (window - 1))
    }

    # Keep only the epoch records
    # (remove excess early 'window' head records (if any): This should NOT happen HERE))
    EpochRes <- EpochRes[head(EpochIndex,1) <= index(EpochRes)]

    EpochResList <- list()
    EpochResList[[as.character(index(tail(EpochRes,1)))]] <- EpochRes
    AllEpochResults <- c(list(), AllEpochResults, EpochResList)

  }
  # assuming have an rbind method (rbind.xts)
  Results <- DescTools::DoCall(rbind, AllEpochResults)

  res <- Results
  x <- xTs1
  xOrigColName <- colnames(x)
  if(NVAR(res) == 1) {

    # explodeXts compatible
    if(derivedDataDetailed == TRUE) {
        colnames(res) <- stringr::str_c("rollEpochRanks", ranks)
    } else {
      colnames(res) <- "rollEpochRanks"
    }
    if(originalData == TRUE) {
      x <- x[,1, drop = F]
      if(is.null(xOrigColName)) xOrigColName <- "Data"
      colnames(x)   <- xOrigColName
      x <- cbind(x, res)
    } else {
      x <- res
    }

  } else { # NVAR(res) > 1

    # explodeXts compatible
    if(derivedDataDetailed == TRUE) {
        colnames(res) <- stringr::str_c(stringr::str_c(colnames(res), "rollEpochRanks", sep = "."), ranks)
    } else {
      colnames(res) <- stringr::str_c(colnames(res), "rollEpochRanks", sep = ".")
    }
    if(originalData == TRUE) {
      if(any(is.null(xOrigColName))) xOrigColName <- stringr::str_c("Data", seq_len(NVAR(res)), sep = ".")
      colnames(x)   <- xOrigColName
      x <- cbind(x, res)
    } else {
      x <- res
    }

  }
  return(x)

})}



#' multi outer
#'
#' @description
#' \preformatted{
#'
#' Applies reduction of outer amoung multiple vectors.
#' FUN has a required formals that are similar to "paste". See the example.
#' This is NOT USED ANYWHERE.
#'
#' }
#'
#' @author Steven Walker
#' @references
#' \cite{R CRAN package multitable \url{https://cran.r-project.org/src/contrib/Archive/multitable}}
#'
#' \cite{R package multitable \url{https://github.com/stevencarlislewalker/multitable}}
#' @param x list or vectors of values
#' @param ... dots; one function FUN to applied to the vectors
#' @return array
#' @examples
#' \dontrun{
#'
#' multitable___mouter(list(LETTERS[1:3], letters[1:2], 1:3), paste0)
#' multitable___mouter(list(LETTERS[1:3], letters[1:2], 1:3), paste)
#'
#' # Require FUN function formals to be similar to "paste"
#' pasteDot <- function(..., sep = ".", collapse = NULL) { paste(..., sep = sep, collapse = collapse) }
#' multitable___mouter(list(LETTERS[1:3], letters[1:2], 1:3), pasteDot)
#'
#' # convert the array to a vector
#' c(multitable___mouter(list(LETTERS[1:3], letters[1:2], 1:3), pasteDot))

#' }
#' @export
multitable___mouter <- function(x, ...){
  spouter <- function(x, y) outer(x, y, ...)
  Reduce("spouter", x)
}


#' rolling ranks using TTR::runPercentRank
#'
#' @rdname runRanksTTR
#'
#' @description
#' \preformatted{
#'
#' This is a wrapper around TTR runPercentRank.
#' TTR runPercentRank gives skewed values
#' (but with the value are in the correct order).
#' This function uses that "proper ordering" and makes
#' usable running ranks.
#'
#' Note, this is the function before being converted to C.
#' the last fortran version may be better.
#' However, the C version has a correction(fix).
#' https://github.com/joshuaulrich/TTR/blob/9b30395f7604c37ea12a865961d81666bc167616/src/percentRank.f
#'
#' }
#'
#' @param x xts object
#' @param window 10(default) lag to determine the ranks.
#' If cumulative=TRUE, the number of observations to use
#' before the first result is returned. Not tested. So beware.
#' Must be between 1 and nrow(x), inclusive
#' @param ranks 4(default) number of ranks. A lower value
#' means a lower rank number.
#' @param cumulative FALSE(default) use from-inception calculation?
#' Not tested. So beware.
#' @param exact.multiplier The weight applied to identical values
#' in the window. Must be between 0 and 1, inclusive.
#' See ? TTR::runPercentRank
#' @param originalData FALSE(default). If TRUE include original data in the output.
#' @param derivedDataDetailed FALSE(default) if TRUE, include the original data in the output.
#' @param w alternative name for 'window'. Overrides window.
#' @param r alternative name for ranks.  Overrides ranks.
#' @return xts object
#' @examples
#' \dontrun{
#'
#' # explodeXts compatible
#' runRanksTTR(xts(sample(4,4,T), zoo::as.Date(0:3)), window = 4)
#' #            runRanksTTR
#' # 1970-01-01          NA
#' # 1970-01-02          NA
#' # 1970-01-03          NA
#' # 1970-01-04           3
#'
#' runRanksTTR(xts(sample(4,4,T), zoo::as.Date(0:3)), window = 4, originalData = TRUE, derivedDataDetailed = TRUE)
#'
#' runRanksTTR(xts(sample(4,4,T), zoo::as.Date(0:3)), window = 4, originalData = TRUE, derivedDataDetailed = TRUE)
#'
#' #            Data runRanksTTR4
#' # 1970-01-01    2           NA
#' # 1970-01-02    3           NA
#' # 1970-01-03    4           NA
#' # 1970-01-04    3            2
#'
#' runRanksTTR(xts(sample(10,10,T), zoo::as.Date(0:9)), window = 4, originalData = TRUE, derivedDataDetailed = TRUE)
#'
#' runRanksTTR(xts(sample(10,10,T), zoo::as.Date(0:9)), window = 3, ranks = 3, originalData = TRUE, derivedDataDetailed = TRUE)
#'
#' runRanksTTR(xts(sample(10,10,T), zoo::as.Date(0:9)), window = 4, ranks = 2, originalData = TRUE, derivedDataDetailed = TRUE)
#'
#' runRanksTTR(xts(sample(10,10,T), zoo::as.Date(0:9)), window = 4, cumulative = TRUE, originalData = TRUE, derivedDataDetailed = TRUE)
#'
#'}
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom TTR runPercentRank
#' @importFrom stringr str_c
#' @export
runRanksTTR <- function(x, window = 10, ranks = 4, cumulative = F, exact.multiplier = 0.5, originalData = FALSE, derivedDataDetailed = FALSE, w = NULL, r = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  # "runRanksTTR" xor "RNKS"
  MatchCallFnctName <- try( {as.character(as.list(match.call())[[1]])}, silent = T)
  if(inherits(MatchCallFnctName, "try-error")) {
    # may not-be-directly literally called (e.g. DescTools::DoCall)
    MatchCallFnctName <- "RNKS"
  }

  x <- initXts(x)
  # NOT required for eXplode
  if(NVAR(x) > 1) stop(stringr::str_c(MatchCallFnctName, " only allows a single column xts object"))

  xOrig <- x
  xOrigColName <- colnames(xOrig)[1]

  if(!is.null(w)) window <- w
  if(!is.null(r)) ranks <- r

  if(ranks <= 1 || window <= 1) stop(stringr::str_c(MatchCallFnctName, ": parameters \"windows\" and \"ranks\" must be greater than one(1)"))

  if(window <= NROW(x)) {
    x %>%
      # number between zero(0) and one(1)
      TTR::runPercentRank(n = window, cumulative = cumulative, exact.multiplier = exact.multiplier) %>%
        # number between zero(0) and "rank"
        {.* ranks} %>%                                   # very important
                           # splitter sequence # 1, 2, ... rank-1
          {findInterval(., vec = { seq_len(ranks - 1) }, left.open = TRUE) + 1} -> res
  } else {
     res <- rep(NA_real_, NROW(x))
  }

  res <- xts(res, index(x))

  # explodeXts compatible
  if(derivedDataDetailed == TRUE) {
    colnames(res) <- stringr::str_c(MatchCallFnctName, ranks)
  } else {
    colnames(res) <-  MatchCallFnctName
  }
  if(originalData == TRUE) {
    x <- x[,1, drop = F]
    if(is.null(xOrigColName)) xOrigColName <- "Data"
    colnames(x)   <- xOrigColName
    x <- cbind(x, res)
  } else {
    x <- res
  }
  return(x)

})}
#' @rdname runRanksTTR
#'
#'@export
RNKS <- runRanksTTR



#' collection of curves of the trends
#'
#' @description
#' \preformatted{
#' Of average order max/min, . . .
#' if UseAOMX = TRUE and UseAOMN = TRUE, then just after
#' AOMX is called, then AOMN is called.
#' }
#'
#' @param x mktdata object
#' @param col column of the mktdata object to perform the trends
#' @param UseAOMX FALSE(default) use average order max
#' @param UseAOMN FALSE(default) use average order min
#' @examples
#' \dontrun{
#'
#' # called
#' trends(xTs, "UNRATE", UseAOMX = TRUE)
#'
#'}
#' @importFrom tryCatchLog tryCatchLog
#' @export
trends <- function(xTs, col = NULL, UseAOMX = FALSE, UseAOMN = FALSE) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  # no longer works on mktdata
  # InBndxTs <- as.character(substitute(xTs))
  # initMktData(xTs, InBndxTs)

  if(is.null(col)) stop("trends: user must provide [a] column names vector or a TRUE/FALSE vector")

  xTs1 <- xTs
  AnyHasExploded <- FALSE
  if(UseAOMX) {
    if(AnyHasExploded) col <- colnames(xTs1)
    xTs1 <- explodeXts(xTs1[, col], Fun = "AOMX", Whiches = list(mm = 4, m = 3))
    AnyHasExploded <- TRUE
  }
  if(UseAOMN) {
    if(AnyHasExploded) col <- colnames(xTs1)
    xTs1 <- explodeXts(xTs1[, col], Fun = "AOMN", Whiches = list(mm = 4, m = 3))
    AnyHasExploded <- TRUE
  }
  if(!AnyHasExploded)
  {
    xTs1 <- xTs1[, col]
  }

  merge(
    #   explodeXts(xTs1, Fun = "RNKS", Whiches = list(w = 16, r = c(8)))
        explodeXts(xTs1, Fun = "RNKS", Whiches = list(w = 16, r = c(2,4,8)))
      , explodeXts(xTs1, Fun = "RNKS", Whiches = list(w =  8, r = c(2,4  )))
      , explodeXts(xTs1, Fun = "RNKS", Whiches = list(w =  4, r = c(2    )))
  ) -> xTs1

  # no longer works on mktdata
  # mktdata legacy
  # xTs <- combineXts(xTs, xTs1)
  xTs <- xTs1

  # no longer works on mktdata
  # xTs
  # return(releaseMktData(xTs, InBndxTs, isInBndxTsMktData))
  return(xTs)

})}



#' backward looking and forward looking Ranks using data.table
#'
#' @description
#' \preformatted{
#'
#' This is the rank over the "entire" data series (the window).
#' In general this is not useful. This is NOT USED ANYWHERE.
#' This uses package data.table
#'
#' }
#'
#' @param x xts object
#' @param ranks 4(default) number of ranks. A lower value
#' means a lower rank number.
#' @param ties.method "average"(default). Passed to data.table::frank parameter ties.method.
#' @param derivedDataDetailed FALSE(default) if TRUE, include the original data in the output.
#' @return xts object
#' @examples
#' \dontrun{
#'
#' runRanksDT(xts(sample(4,4,T), zoo::as.Date(0:3)), originalData = TRUE, derivedDataDetailed = TRUE)
#'
#' runRanksDT(xts(matrix(c(2,2,4,4),ncol=1), zoo::as.Date(0:3)), ties.method = "average", originalData = TRUE, derivedDataDetailed = TRUE)
#' runRanksDT(xts(matrix(c(2,2,4,4),ncol=1), zoo::as.Date(0:3)), ties.method = "first", originalData = TRUE, derivedDataDetailed = TRUE)
#' runRanksDT(xts(matrix(c(2,2,4,4),ncol=1), zoo::as.Date(0:3)), ties.method = "max", originalData = TRUE, derivedDataDetailed = TRUE)
#' runRanksDT(xts(matrix(c(2,2,4,4),ncol=1), zoo::as.Date(0:3)), ties.method = "min", originalData = TRUE, derivedDataDetailed = TRUE)
#' runRanksDT(xts(matrix(c(2,2,4,4),ncol=1), zoo::as.Date(0:3)), ties.method = "dense", originalData = TRUE, derivedDataDetailed = TRUE)
#'
#' runRanksDT(xts(sample(10,10,T), zoo::as.Date(0:9)), originalData = TRUE, derivedDataDetailed = TRUE)
#'
#' runRanksDT(xts(sample(10,10,T), zoo::as.Date(0:9)), ranks = 3, originalData = TRUE, derivedDataDetailed = TRUE)
#'
#' runRanksDT(xts(sample(10,10,T), zoo::as.Date(0:9)), ranks = 2, originalData = TRUE, derivedDataDetailed = TRUE)
#'
#'}
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom data.table frank
#' @importFrom stringr str_c
#' @export
runRanksDT <- function(x, ranks = 4, ties.method = "average", originalData = FALSE, derivedDataDetailed = FALSE) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  x <- initXts(x)
  # NOT required for eXplode
  if(NVAR(x) > 1) stop("runRanksDT only allows a single column xts object")

  xOrig <- x
  xOrigColName <- colnames(xOrig)[1]

  if(ranks <= 1) stop("runRanksDT: parameter \"ranks\" must be greater than one(1)")

  # this is not a "running"
  window <- NROW(x)

  # lower values mean lower ranks
  # "as.data.frame" required so that "data.table::frank"
  # does NOT choke (and die) on 'all NAs'
  # number between one(1) and the window
  x %>% as.data.frame(x) %>%
  data.table::frank(ties.method = ties.method, na.last= "keep")  %>%
                        #  window/ranks *splitter sequence # 1, 2, ... rank-1
      { findInterval(., vec = window/ranks * seq_len(ranks - 1), left.open = T) + 1} -> res

  res <- xts(res,index(x))

  # if(is.null(xOrigColName)) xOrigColName <- "Data"
  # colnames(x)   <- xOrigColName
  # colnames(res) <- stringr::str_c(xOrigColName, "_RNK", ranks)
  # x <- cbind(x, res)

  # explodeXts compatible
  if(derivedDataDetailed == TRUE) {
    colnames(res) <- stringr::str_c("runRanksDT", ranks)
  } else {
    colnames(res) <- "runRanksDT"
  }
  if(originalData == TRUE) {
    x <- x[,1, drop = F]
    if(is.null(xOrigColName)) xOrigColName <- "Data"
    colnames(x)   <- xOrigColName
    x <- cbind(x, res)
  } else {
    x <- res
  }

  return(x)

})}



#' rolling ranks using package matrixStats
#'
#' @description
#' \preformatted{
#'
#' Note: this may use MUCH memory.  The number of new  matrix columns
#' created is equal to "ranks - 1".  Also, time may be used to
#' create these columns.
#'
#' In most cases, one may be better off using the function: runRanksTTR.
#'
#' "base" and "ranks" can be cleverly manipulated to get, for example,
#' ranks from observations not directly adjacent to each other. An examle
#' is not shown.
#'
#' }
#'
#' @param x xts object
#' @param laggedCols character vector of regular expressions or column names.
#' This allows the user to give the names of the original column(the lag0 column)
#' and the "window - 1" columns(the lag1, ... lag(window -1)).  This may be VERY useful
#' if the columns already exist, so the columns (internally) do not
#' need to be EXPENSIVELY created.
#' If this parameter is used then parameters "base" and "window" are ignored.
#' @param base passed to LagXts.  See ? LagXts.
#' @param window 10(default) lag to determine the ranks.
#' @param ranks 4(default) number of ranks. A lower value
#' means a lower rank number.
#' @param ties.method "average"(default) passed to matrixStats::rowRanks parameter ties.method
#' @param na.pad passed to LagXts
#' @param derivedDataDetailed FALSE(default) if TRUE, include the original data in the output.
#' @param ... dots passed to LagXts
#' @return xts object
#' @examples
#' \dontrun{
#'
#' runRanksMS(xts(sample(4,4,T), zoo::as.Date(0:3)), window = 4, originalData = TRUE, derivedDataDetailed = TRUE)
#'
#' runRanksMS(xts(matrix(c(2,2,4,4),ncol=1), zoo::as.Date(0:3)), window = 4, ties.method = "max", originalData = TRUE, derivedDataDetailed = TRUE)
#' runRanksMS(xts(matrix(c(2,2,4,4),ncol=1), zoo::as.Date(0:3)), window = 4, ties.method = "average", originalData = TRUE, derivedDataDetailed = TRUE)
#' runRanksMS(xts(matrix(c(2,2,4,4),ncol=1), zoo::as.Date(0:3)), window = 4, ties.method = "min", originalData = TRUE, derivedDataDetailed = TRUE)
#'
#' runRanksMS(xts(sample(10,10,T), zoo::as.Date(0:9)), window = 4, originalData = TRUE, derivedDataDetailed = TRUE)
#'
#' runRanksMS(xts(sample(10,10,T), zoo::as.Date(0:9)), window = 3, ranks = 3, originalData = TRUE, derivedDataDetailed = TRUE)
#'
#' runRanksMS(xts(sample(10,10,T), zoo::as.Date(0:9)), window = 4, ranks = 2, originalData = TRUE, derivedDataDetailed = TRUE)
#'
#'}
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom matrixStats rowRanks
#' @importFrom stringr str_detect str_c
#' @export
runRanksMS <- function(x, laggedCols = NULL, base = 0, window = 10, ranks = 4, ties.method = "average", na.pad = TRUE, originalData = FALSE, derivedDataDetailed = FALSE, ...) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  x <- initXts(x)
  # NOT required for eXplode
  if(NVAR(x) > 1) stop("runRanksMS only allows a single column xts object")

  xOrig <- x
  xOrigColName <- colnames(xOrig)[1]

  if(ranks <= 1) stop("runRanksMS: parameter \"ranks\" must be greater than one(1)")

  if(is.null(laggedCols)) {
    x <- LagXts(x, k = base + c(0,seq_len(window - 1)), na.pad = na.pad, ...)
  } else {
    # if I already know the columns then go and get them
    for(colexpr in laggedCols) {
     x <- x[, stringr::str_detect(colnames(x), pattern = colexpr)]
    }
    window <- NVAR(x)
  }
  x <- coredata(x)

  # NAs possibly mess up some "rank" math
  x[!complete.cases(x),] <- NA

  # lower values mean lower ranks
  # [, 1] tail is only needed in a "running"
  matrixStats::rowRanks(x, ties.method = ties.method)[, 1] %>%
                                 #  window/ranks *splitter sequence # 1, 2, ... rank-1
      { findInterval(., vec = window/ranks * seq_len(ranks - 1), left.open = TRUE) + 1} -> res

  res <- xts(res,index(xOrig))

  # if(is.null(xOrigColName)) xOrigColName <- "Data"
  # colnames(xOrig)   <- xOrigColName
  # colnames(res) <- stringr::str_c(xOrigColName, "_RNK", ranks)
  # x <- cbind(xOrig, res)

  # explodeXts compatible
  if(derivedDataDetailed == TRUE) {
    colnames(res) <- stringr::str_c("runRanksMS", ranks)
  } else {
    colnames(res) <- "runRanksMS"
  }
  if(originalData == TRUE) {
    x <- x[,1, drop = F]
    if(is.null(xOrigColName)) xOrigColName <- "Data"
    colnames(x)   <- xOrigColName
    x <- cbind(x, res)
  } else {
    x <- res
  }

  return(x)

})}



#' paste with the default dot(.) separator
#'
#' @description
#' \preformatted{
#'
#' NOT USED ANYWHERE
#'
#' }
#'
#' @param ... dots as paste
#' @param sep as paste
#' @param collapse as paste
#' @export
pasteDot <- function(..., sep = ".", collapse = NULL) {
  paste(..., sep = sep, collapse = collapse)
}



#' choose local minima (or maxima)
#'
#' @description
#' \preformatted{
#'
#' Lags an xts column into a 2nd+ columns.
#' This chooses the lowest(or highest) value rowwise
#'
#' This is meant for removing noise from a monthly financial time series.
#' If the goal is a pessimisic (buy "puts") solution
#' then an optimal choice may be (lag = 0:1, take = "min")
#'
#' }
#'
#' @param xTs xts object
#' @param base choose -1 (or less) to look into the future
#' @param lag 0:1(default) periods to lag over: this(0) and previous(1)
#' @param take "min"(default).  Other option is "max". Finally,
#' the user can pass in a custom function "Fun."  If Fun is passed
#' then "take" is ignored.
#' @param Fun custom user passed function
#' @param ... dots passed to Fun

#' @examples
#' \dontrun{
#'
#' x <- xts(0:2, zoo::as.Date(0:2))
#' OneOf(x)
#'           [,1]
#' 1970-01-01   NA
#' 1970-01-02    0
#' 1970-01-03    1
#'
#'}
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom matrixStats rowMins
#' @importFrom matrixStats rowMaxs
#' @importFrom DescTools DoCall
#' @export
OneOf <-function(xTs, base = 0, lag = 0:1, take = "min" , Fun = NULL, ...) {
tryCatchLog::tryCatchLog({
# initEnv(); on.exit({uninitEnv()})

   Dots <- list(...)
   if(length(take) > 1) stop("OneOf parameter take has too many choices")

   xTsOrig     <- initXts(xTs)
   CoreDataOrig <- coredata(lag.xts(xTs, base + lag))

   if(take == "min")
     CoreDataNew <- matrixStats::rowMins(CoreDataOrig)
   if(take == "max")
     CoreDataNew <- matrixStats::rowMaxs(CoreDataOrig)
   if(!take %in% c("min","max")) {
     if(mode(Fun) == "function") Fun = match.fun(Fun)
     CoreDataNew <- DescTools::DoCall(Fun, c(list(), list(CoreDataOrig), Dots))
   }
   reclass(CoreDataNew, xTsOrig)

})}



#' specific orders of n  (smallest/largest) of values within a range
#'
#' @description
#' \preformatted{
#'
#' Within a range, find the smallest observation
#' of order (n). n == 1 is the smallest. n == 2 is the 2nd smallest.
#'
#' within range, within a subrange of smallest
#' observations defined by nt, find the sum
#' of those small values.
#'
#' Expected, to be used as a smoothing
#' item on past St. Louis FRED data.  Typical use
#' is to choose the minimal of the previous two(2)
#' observations before/or/after applying an SMA(x, 3).
#'
#' Other, expected be used on future data. e.g. find
#' the lowest valued two(2) months in the next(future)
#' six(6) months and add(sum) those values together.
#'
#' }
#'
#' @param x  xts object
#' @param base passed to LagXts.  See ? LagXts.
#' @param r (back) range: choose -2:-1 (or less) to look into the future.
#' Passed to LagXts k paramter
#' @param n the order of the minimum to be found.
#' n == 1 means smalled value. n == 2 means second smalled
#' value, etc.
#' @param View "min"(default): 'n == 1' means look for the lowest value(s).
#' Otherwise, "max" n == 1 means look for the highest values(s).
#' Note, to peform "max" math, the coredata is simply muliplied by
#' negative one(-1) before the code is executed.
#' After the code is executed the coredata is re-multiplied by
#' negative one(-1).
#' @param rt alternate way to specify rt == 2 means r == 1:2.
#' rt == -2 means r == -2:-1.  rt will override r.  This can be
#' useful when passing to eXplodeXts
#' @param nt  alternate way to specify nt == 2 means n == 1:2.
#' nt == -2 means n == 2:1.  nt will override n.  This can be
#' useful when passing to eXplodeXts.
#' @param na.pad passed to LagXts
#' @param derivedDataDetailed FALSE(default) if TRUE, include the original data in the output.
#' @param ... dots passed to LagXts
#' @examples
#' \dontrun{
#'
#' matrix(1:5,ncol = 1, dimnames = list(list(), list("Data")))
#'
#'            Data
#' 1970-01-01    1
#' 1970-01-02    2
#' 1970-01-03    3
#' 1970-01-04    4
#' 1970-01-05    5
#'
#' # futures
#' LagXts(xts(matrix(1:5,ncol = 1, dimnames = list(list(), list("Data"))), zoo::as.Date(0:4)),
#'   k =  c(-3:-1))
#'
#'           lead3 lead2 lead1
#' 1970-01-01     4     3     2
#' 1970-01-02     5     4     3
#' 1970-01-03    NA     5     4
#' 1970-01-04    NA    NA     5
#' 1970-01-05    NA    NA    NA
#'
#' # of the three(3) (future)leading values I want to find two(2) of
#' # the three(2) worst months and sum their 'two' values together.
#'
#' # nt == 2 means n == 1:2 (1st lowest and 2nd lowest)
#' #
#' sumOrdersXts(xts(matrix(1:5,ncol = 1, dimnames = list(list(), list("Data"))), zoo::as.Date(0:4)),
#'   r =  -3:-1, nt = 2)
#'
#' sumOrdersXts(xts(matrix(1:5,ncol = 1), zoo::as.Date(0:4)), r =  -3:-1, nt = 2)
#'            sumOrdersXts
#' 1970-01-01            5 # lead2 + lead1
#' 1970-01-02            7 # lead2 + lead1
#'
#' sumOrdersXts(xts(matrix(1:5,ncol = 1), zoo::as.Date(0:4)), r =  -3:-1, nt = 2, derivedDataDetailed = TRUE)
#'            Data sumOrdersXts
#' 1970-01-01    1            5 # lead2 + lead1
#' 1970-01-02    2            7 # lead2 + lead1
#' 1970-01-03    3           NA
#' 1970-01-04    4           NA
#' 1970-01-05    5           NA
#'
#' # find two(2) of the three(2) best months and
#' # sum their 'two' values together.
#' #
#' # View == "max" # n == 1 means greatest
#' #
#' sumOrdersXts(xts(matrix(1:5,ncol = 1, dimnames = list(list(), list("Data"))), zoo::as.Date(0:4)),
#'   r =  -3:-1, View = "max", nt = 2)
#'
#'            Data sumOrdersXts
#' 1970-01-01    1            7
#' 1970-01-02    2            9
#' 1970-01-03    3           NA
#' 1970-01-04    4           NA
#' 1970-01-05    5           NA
#'
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_c
#' @importFrom matrixStats rowAnyNAs rowOrderStats rowSums2
#' @importFrom DescTools DoCall
#' @export
sumOrdersXts <- function(x, base = 0, r = 0:1, n = 1, View = "min", na.pad = TRUE, derivedDataDetailed = FALSE, ...) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  if(!View %in% c("min","max")) stop("sumOrdersXts param View can only be \"min\" xor \"max\"")

  Dots <- list(...)
  if(!is.null(Dots[["rt"]]))
    r = sort(sign(Dots[["rt"]]) * seq_len(abs(-Dots[["rt"]])))
  if(!is.null(Dots[["nt"]]))
    n = sort(sign(Dots[["nt"]]) * seq_len(abs(-Dots[["nt"]])))

  x <- initXts(x)
  xOrig <- x
  xTs <- x

  # n == 1 now means largest value
  if(View == "max")
    coredata(xTs) <- -coredata(xTs)

  # matrixStats::rowOrderStats is not allowed
  # to have NAs in the rows, here I eliminate rows.
  xTs <- LagXts(xTs, k = base + r, na.pad = na.pad, ...)
  x   <- coredata(xTs)
  FullRowsXtsFnd <- !matrixStats::rowAnyNAs(x)
  x <- x[FullRowsXtsFnd,]

  # I Originally, I once had a more complicated technique
  # using spitting, applying, and findInterval (per layer),
  # but given a small set of
  # values per row, the situation may be
  # JUST EASIER (and simpler, but maybe not faster?)
  # to just RE-scan, so that is what I am doing.

  resList <- list()
  for(ni in n) {
    # ni = 1 lowest value, ni = 2 second lowest value etc.
    res <- matrixStats::rowOrderStats(x, which = ni) # , dim. = c(NROW(x), NVAR(x))
    resList <- c(list(), resList, list(res))
  }
  # matrix again
  x <- DescTools::DoCall(cbind, c(list(), resList))
  # e.g. requested about, e.g. n = 1:2, an 'aggregation'
  if(length(resList) > 1) {
    # sum across
    x <- matrixStats::rowSums2(x)
  }
  xTs <- xts(x, index(xTs)[FullRowsXtsFnd])

  # if(!is.null(colnames(xOrig))) {
  #   colnames(xTs) <- stringr::str_c(colnames(xOrig), "_RAGG")
  # } else {
  #   colnames(xTs) <- "RAGG"
  # }

  # sumOrdersXts explodeXts compatible
  if(derivedDataDetailed) {
    colnames(xOrig) <- "Data"
  }
  colnames(xTs)   <- "sumOrdersXts"

  # n == 1 at largest value: now return to original values
  if(View == "max")
    coredata(xTs) <- -coredata(xTs)

  if(derivedDataDetailed)
    xTs <- cbind(xOrig, xTs)

  xTs
})}



#' average order max (AOMX)
#'
#' @description
#' \preformatted{
#' Of the last mm observations(including self), find the hightest m observations,
#' and then average those m observations
#' }
#' @param x  xts object
#' @param mm  numer of last mm observations(including self)
#' @param m number of hightest m observations
#' @examples
#' \dontrun{
#' Of the last three(3) observations(including self), find the hightest two(2) observations,
#' and then average those two(2) observations.
#'
#' xts(matrix(1:5,ncol = 1, dimnames = list(list(), list("Data"))), zoo::as.Date(0:4))
#'
#' Data
#' 1970-01-01    1
#' 1970-01-02    2
#' 1970-01-03    3
#' 1970-01-04    4
#' 1970-01-05    5
#'
#' AOMX(xts(matrix(1:5,ncol = 1, dimnames = list(list(), list("Data"))), zoo::as.Date(0:4)), mm = 3, m  = 2)
#'
#' AOMX
#' 1970-01-03  2.5
#' 1970-01-04  3.5
#' 1970-01-05  4.5
#'
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom zoo na.trim
#' @export
AOMX <- function(x, mm = 4, m = 3) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  xTs <- sumOrdersXts(x,  r = 0:(mm-1), View = "max", nt = m)/m
  colnames(xTs)[1] <- "AOMX"
  xTs

})}



#' average order minimum (AOMN)
#'
#' @description
#' \preformatted{
#' Of the last mm observations(including self), find the lowest m observations,
#' and then average those m observations
#' }
#' @param x  xts object
#' @param mm  numer of last mm observations(including self)
#' @param m number of lowest m observations
#' @examples
#' \dontrun{
#' Of the last three(3) observations(including self), find the lowest two(2) observations,
#' and then average those two(2) observations.
#'
#' xts(matrix(1:5,ncol = 1, dimnames = list(list(), list("Data"))), zoo::as.Date(0:4))
#'
#' Data
#' 1970-01-01    1
#' 1970-01-02    2
#' 1970-01-03    3
#' 1970-01-04    4
#' 1970-01-05    5
#'
#' AOMN(xts(matrix(1:5,ncol = 1, dimnames = list(list(), list("Data"))), zoo::as.Date(0:4)), mm = 3, m  = 2)
#'
#' AOMN
#' 1970-01-03  1.5
#' 1970-01-04  2.5
#' 1970-01-05  3.5
#'
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom zoo na.trim
#' @export
AOMN <- function(x, mm = 4, m = 3) {
  tryCatchLog::tryCatchLog({
    initEnv();on.exit({uninitEnv()})

    xTs <- sumOrdersXts(x,  r = 0:(mm-1), View = "min", nt = m)/m
    colnames(xTs)[1] <- "AOMN"
    xTs

  })}





#' annualized an xts object
#'
#'
#' @description
#' \preformatted{
#'
#' Uses the xts index
#'
#' }
#'
#' @param x xts object of 'percent change'
#' @return xts object with values multiplied to get annualization
#' @examples
#' \dontrun{
#'
#' ANN(
#'   xts(1:3, c(zoo::as.Date("1980-01-31"), zoo::as.Date("1980-02-29"), zoo::as.Date("1980-03-31")))
#' )
#'            [,1]
#' 1980-01-31   12
#' 1980-02-29   24
#' 1980-03-31   36
#'
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_c
#' @export
ANN <- function(x) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  xTs <- x
  xTs <- initXts(xTs)

  Units <- periodicity(xTs)$scale

  if(Units == "quarters") {
    return(xTs *   4)
  } else if(Units == "monthly") {
    return(xTs *  12)
  } else if(Units == "weekly") {
    return(xTs *  52)
  } else if(Units == "daily") {
    return(xTs * 264) # 22 * 12
  } else {
    stop(stringr::str_c("ANN does not know haw to annualize: ", Units))
  }

})}



#' absolute change
#'
#' @description
#' \preformatted{
#'
#' }
#'
#' @param x xts object
#' @param base choose -1 (or less) to look into the future
#' @param lag observations backwards
#' @param ... dots passed to LagXts
#' @examples
#' \dontrun{
#'
#' matrix(1:4,ncol =2)
#'      [,1] [,2]
#' [1,]    1    3
#' [2,]    2    4
#'
#' AC(xts(matrix(1:4,ncol =2), zoo::as.Date(0:1)), lag = 1:2)
#'            V1ac.1 V2ac.1 V1ac.2 V2ac.2
#' 1970-01-01     NA     NA     NA     NA
#' 1970-01-02      1      1     NA     NA
#'
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_replace
#' @export
AC <- function(x, base = 0, lag = 1, ...) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  xTs <- x
  xTs <- initXts(xTs)
  xTs1 <- LagXts(xTs, k = base + rep(0,length(lag)), ...)
  xTs2 <- LagXts(xTs, k = base + lag, ...)
  xTs  <- xTs1 - xTs2
  colnames(xTs) <- stringr::str_replace(colnames(xTs2), "lag", "ac")
  xTs
})}



#' relative change
#'
#' @description
#' \preformatted{
#'
#' }
#'
#' @param x xts object
#' @param base choose -1 (or less) to look into the future
#' @param lag observations backwards
#' @examples
#' \dontrun{
#'
#' xts(matrix(c(1,-2,-4,8,16,32), ncol = 2), zoo::as.Date(0:2))
#'            [,1] [,2]
#' 1970-01-01    1    8
#' 1970-01-02   -2   16
#' 1970-01-03   -4   32
#'
#' RC(xts(matrix(c(1,-2,-4,8,16,32), ncol = 2), zoo::as.Date(0:2)))
#'            V1ac.1 V2ac.1
#' 1970-01-01     NA     NA
#' 1970-01-02     -2      2
#' 1970-01-03     -1      2
#'
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_replace
#' @export
RC <- function(x, base = 0, lag = 1, log = FALSE, ...) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  xTs <- x
  xTs <- initXts(xTs)

  xTs1 <- LagXts(xTs, k = base + rep(0,length(lag)), ...)
  xTs2 <- LagXts(xTs, k = base + lag               , ...)

  # ? `[`
  #
  # x[i]
  # x[i] <- value
  #
  # A third form of indexing is via a
  # numeric matrix with the
  #   one column for each dimension: each row of the index matrix
  #     then selects a single element of the array,
  #       and the result is a vector.
  # Negative indices are not allowed in the index matrix.
  # NA and zero values are allowed:
  #   rows of an index matrix containing a zero are ignored,
  #   whereas rows containing an NA produce an NA in the result.
  #
  # Indexing via a character matrix with one column per dimensions is also supported
  # if the array has dimension names.
  #   As with numeric matrix indexing, each row of the index matrix selects a single element of the array.
  #   Indices are matched against the appropriate dimension names. N
  #   A is allowed and will produce an NA in the result.
  #   Unmatched indices as well as the empty string ("") are not allowed and will result in an error.

  NegNegTest <- (LagXts(xTs, k = base + rep(0,length(lag)), ...) < 0) & (LagXts(xTs, k = base + lag, ...) < 0)

  # before any 'any/all' test
  if(anyNA(NegNegTest))
    NegNegTest[which(is.na(NegNegTest), arr.ind = TRUE)] <- FALSE

  if(any(coredata(NegNegTest) == TRUE)) {

    NewCoreXts <- coredata(xTs)

    arrayIndiciesNegNeg  <- which( coredata(NegNegTest), arr.ind = TRUE)
    arrayIndiciesRegular <- which(!coredata(NegNegTest), arr.ind = TRUE)

    # regular common case
    NewCoreXts[arrayIndiciesRegular] <-
                  coredata(xTs1)[arrayIndiciesRegular]/
                  coredata(xTs2)[arrayIndiciesRegular]

    # neg/neg rare case: (LagXts(xTs, base + rep(0,length(lag))) < 0) & (LagXts(xTs, base + lag) < 0)
    # bad interpretation

    # make sure one UNDERSTANDs the contexts of
    # NEG-nonlag / NEGlagged
    # use with with MUCH care

    # > sign(-4)*(-5 - (-4))/-4
    # [1] -0.25 # 25% percent less

    # > sign(-2)*(-4 - (-2))/-2
    # [1] -1 # one full proportion less
    NewCoreXts[arrayIndiciesNegNeg] <-
              sign(  coredata(LagXts(xTs, k = base + lag, ...))[arrayIndiciesNegNeg] ) *
                  (
                       coredata(xTs1)[arrayIndiciesNegNeg] -
                     ( coredata(xTs2)[arrayIndiciesNegNeg] )
                  ) /
                  coredata(xTs2)[arrayIndiciesNegNeg]

    coredata(xTs) <- NewCoreXts

  } else {
    xTs  <- xTs1/xTs2
  }

  colnames(xTs) <- stringr::str_replace(colnames(xTs2), "lag", "rc")
  if(log) {
    xTs <- log(xTs)
    colnames(xTs) <- stringr::str_replace(colnames(xTs), "rc", "logrc")
  }
  xTs

})}



#' absolute 'relative change' ARC
#'
#' @description
#' \preformatted{
#'
#' Results MAY include "negative numbers."
#'
#' }
#'
#' @export
ARC <- function(x, base = 0, lag = 1, log = FALSE, ...) { RC(x = x, base = base, lag = lag, log = log, ...) }



#' relative 'relative change' ARC
#'
#' @description
#' \preformatted{
#'
#' Results NEVER include "negative numbers."
#'
#' }
#'
#' @export
RRC <- function(x, base = 0, lag = 1, log = FALSE, ...) { abs(ARC(x = x, base = base, lag = lag, log = log, ...)) }



#' absolute proportional change
#'
#' @description
#' \preformatted{
#'
#' This is most useful for calculating velocity
#' and acceleration, (and jerk).
#' To get accelleration and jerk use this with
#' diffXts with differences 2 (and 3) respectively.
#'
#' Ninety-nine (99%) percent of the people in the world SHOULD HAVE BEEN using this one.
#'
#' }
#'
#' @param x xts object
#' @param base choose -1 (or less) to look into the future
#' @param lag observations backwards
#' @examples
#' \dontrun{
#'
#' xts(matrix(c(1,-2,-4,8,16,32), ncol = 2), zoo::as.Date(0:2))
#'            [,1] [,2]
#' 1970-01-01    1    8
#' 1970-01-02   -2   16
#' 1970-01-03   -4   32
#'
#' APC(xts(matrix(c(1,-2,-4,8,16,32), ncol = 2), zoo::as.Date(0:2)))
#'            V1apc.1 V2apc.1
#' 1970-01-01      NA      NA
#' 1970-01-02      -3       1
#' 1970-01-03      -1       1
#'
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_replace
#' @export
APC <- function(x, base = 0, lag = 1, ...) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  xTs <- x
  xTs <- initXts(xTs)
  xTs2 <- LagXts(xTs, k = base + lag, ...)

  xTs <- AC(xTs, base = base + rep(0,length(lag)), lag = lag, ...)/ abs(xTs2)
  colnames(xTs) <- stringr::str_replace(colnames(xTs2), "lag", "apc")
  xTs

})}



#' absolute percent change
#'
#' @description
#' \preformatted{
#' "absolute proportional change" multiplied by 100.00
#' See ? APC # absolute proportional change
#' }
#' @param x xts object
#' @param base choose -1 (or less) to look into the future
#' @param lag observations backwards
#' @examples
#' \dontrun{
#' See ? APC
#' }
#' @importFrom tryCatchLog tryCatchLog
APCTC <- function(x, base = 0, lag = 1, ...) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  xTs <- x
  xTs <- initXts(xTs)

  xTs <- APC(x, base = base, lag = lag, ...) * 100.000
  colnames(xTs) <- stringr::str_replace(colnames(xTs2), "apc", "apctc")
  xTs

})}


#' relative proportional change
#'
#' @description
#' \preformatted{
#'
#' Ninety-nine (99%) percent of the people in the world are using THIS WRONG ONE.
#'
#' }
#'
#' @param x xts object
#' @param base choose -1 (or less) to look into the future
#' @param lag observations backwards
#' @examples
#' \dontrun{
#'
#' xts(matrix(c(1,-2,-4,8,16,32), ncol = 2), zoo::as.Date(0:2))
#'            [,1] [,2]
#' 1970-01-01    1    8
#' 1970-01-02   -2   16
#' 1970-01-03   -4   32
#'
#' RPC(xts(matrix(c(1,-2,-4,8,16,32), ncol = 2), zoo::as.Date(0:2)))
#'            V1rpc.1 V2rpc.1
#' 1970-01-01      NA      NA
#' 1970-01-02      -3       1
#' 1970-01-03       1       1
#'
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_replace
#' @export
RPC <- function(x, base = 0, lag = 1, ...) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  xTs <- x
  xTs <- initXts(xTs)
  xTs2 <- LagXts(xTs, k = base + lag, ...)

  xTs <- AC(xTs, base = base + rep(0,length(lag)), lag = lag, ...)/xTs2
  colnames(xTs) <- stringr::str_replace(colnames(xTs2), "lag", "rpc")
  xTs

})}



#' relative percent change
#'
#' @description
#' \preformatted{
#' "relative proportional change" multiplied by 100.00
#' See ? RPC # absolute proportional change
#' }
#' @param x xts object
#' @param base choose -1 (or less) to look into the future
#' @param lag observations backwards
#' @examples
#' \dontrun{
#' See ? RPC
#' }
#' @importFrom tryCatchLog tryCatchLog
RPCTC <- function(x, base = 0, lag = 1, ...) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  xTs <- x
  xTs <- initXts(xTs)

  RPC(x, base = base, lag = lag, ...) * 100.000
  colnames(xTs) <- stringr::str_replace(colnames(xTs2), "rpc", "rpctc")
  xTs

})}



#' lag an xts object
#'
#' @description
#' \preformatted{
#'
#' This does not complain when: any(abs(k) > NROW(xTs)):
#' zoo_lag can not and will not handle. So k's are eliminated
#' beforehand.
#'
#' }
#'
#' @param x xts object
#' @param k choose -1 to look into the future
#' @param na.pad as lag.xts
#' @param ... dots passed to lag.xts
#' @examples
#' \dontrun{
#'
#' xts(1:4, zoo::as.Date(0:3))
#'            [,1]
#' 1970-01-01    1
#' 1970-01-02    2
#' 1970-01-03    3
#' 1970-01-04    4
#'
#' LagXts(xts(1:4, zoo::as.Date(0:3)), k =  c(-5:-3,0:1,3:5))
#'            V1lead.4 V1lead.3 V1lag.0 V1lag.1 V1lag.3 V1lag.4
#' 1970-01-01       NA        4       1      NA      NA      NA
#' 1970-01-02       NA       NA       2       1      NA      NA
#' 1970-01-03       NA       NA       3       2      NA      NA
#' 1970-01-04       NA       NA       4       3       1      NA
#'
#' xts(matrix(1:4,ncol =2), zoo::as.Date(0:1))
#'            [,1] [,2]
#' 1970-01-01    1    3
#' 1970-01-02    2    4
#'
#' LagXts(xts(matrix(1:4,ncol =2), zoo::as.Date(0:1)), k =  c(-5:-3,0:1,3:5))
#'            V1lag.0 V2lag.0 V1lag.1 V2lag.1
#' 1970-01-01       1       3      NA      NA
#' 1970-01-02       2       4       1       3
#'
#'
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_c
#' @export
LagXts <- function(x, k = 1, na.pad = TRUE, ...) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  xTs <- x
  xTs <- initXts(xTs)
  if(all(abs(k) > NROW(xTs))) {
    # do not bother trying
    coredata(xTs)[] <- NA
    return(xTs)
  }
  # just do the k's within range (that lag.xts  ACCEPTS to handle)
  # CURRENT - I MAY WANT TO CHANGE MY MIND LATER
  IsKineligibleFnd <- abs(k) <= NROW(xTs)
  # CURRENT UNUSED ... if used ... some very very clever interleaving
  kOther <- k[!IsKineligibleFnd]
  k      <- k[ IsKineligibleFnd]
  xTs <- lag.xts(xTs, k = k, na.pad = na.pad, ...)

  AreColNamesNULL <- if(is.null(colnames(x))) { TRUE } else { FALSE }
  NewColNames <- list()
  for(ki in k) {
    kiName <- abs(ki)
    for(NVARi in seq_len(NVAR(x))){
      if(AreColNamesNULL) {
        ColName <- stringr::str_c("V", NVARi)
      } else {
        ColName <- stringr::str_c(colnames(x)[NVARi],".")
      }
      if(ki < 0) {
        PostFix <- "lead"
      } else {
        PostFix <- "lag"
      }
      NewColNamesI <- stringr::str_c(ColName, PostFix, ".", kiName)
      NewColNames <- c(list(), NewColNames, list(NewColNamesI))
    }

  }
  # better names
  colnames(xTs) <- do.call(c, NewColNames)
  xTs

})}



#' lag and/or difference and/or use a function(Fun) upon and xts object
#'
#' @description
#' \preformatted{
#'
#' }
#'
#' @param x as diff.xts
#' @param lag as diff.xts
#' @param differences as diff.xts
#' @param arithmetic as diff.xts
#' @param log as diff.xts
#' @param na.pad as diff.xts
#' @param Fun differencing function.
#' Meant to change the xTs in some way.
#' (Default diff (expected: xts::diff.xts)).
#' Should accept or (accept and ignore) the parameters: lag;
#' for S3 compatibility, differences; for xts compatiblity,
#' arithmetic, log, and/or na.pad.
#' @examples
#' \dontrun{
#'
#' # based on xts::lag.xts
#'
#' # case 1
#' # arithmetic and not log
#' # AC
#'
#' xts(matrix(c(1,-2,-4,8,16,32), ncol = 2), zoo::as.Date(0:2))
#'            [,1] [,2]
#' 1970-01-01    1    8
#' 1970-01-02   -2   16
#' 1970-01-03   -4   32
#'
#' diffXts(xts(matrix(c(1,-2,-4,8,16,32), ncol = 2), zoo::as.Date(0:2)), differences = 2, Fun = AC)
#'            V1ac.1 V2ac.1
#' 1970-01-01     NA     NA
#' 1970-01-02     -3      8
#' 1970-01-03     -2     16
#'
#' # case 2
#' # arithmetic and log
#' # RC with log = TRUE
#'
#' xts(matrix(abs(c(1,-2,-4,8,16,32)), ncol = 2), zoo::as.Date(0:2))
#'            [,1] [,2]
#' 1970-01-01    1    8
#' 1970-01-02    2   16
#' 1970-01-03    4   32
#'
#' # NOTE: in REAL WORLD USAGE! most likely I would want to use "APC"
#' # "RC" here is just for math demonstration purposes
#' #
#' diffXts(xts(matrix(abs(c(1,-2,-4,8,16,32)), ncol = 2), zoo::as.Date(0:2)), differences = 1, Fun = RC, log = TRUE)
#'            V1logrc.1 V2logrc.1
#' 1970-01-01        NA        NA
#' 1970-01-02   0.69315   0.69315
#' 1970-01-03   0.69315   0.69315
#'
#' # case 3
#' # not arithmetic
#' # RC
#'
#' xts(matrix(c(1,-2,-4,8,16,32), ncol = 2), zoo::as.Date(0:2))
#'            [,1] [,2]
#' 1970-01-01    1    8
#' 1970-01-02   -2   16
#' 1970-01-03   -4   32
#'
#' diffXts(xts(matrix(c(1,-2,-4,8,16,32), ncol = 2), zoo::as.Date(0:2)), differences = , Fun = RC)
#'            V1rc.1 V2rc.1
#' 1970-01-01     NA     NA
#' 1970-01-02     -2      2
#' 1970-01-03     -1      2
#'
#' }
#' @param ... dots passed
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom DescTools DoCall
#' @export
diffXts <- function(x, lag=1, differences=1, arithmetic=TRUE, log=FALSE, na.pad=TRUE, Fun = diff, ...) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  Fun = match.fun(Fun)
  Dots <- list(...)

  if(!is.null(lag)) {
    if(!is.integer(lag) && any(is.na(as.integer(lag))))
      stop("'lag' must be integer")
  }

  if(!is.null(differences)) {
    differences <- as.integer(differences[1L])
    if(is.na(differences))
      stop("'differences' must be integer")
  }

  if(is.logical(x))
    x <- .xts(matrix(as.integer(x),ncol=NCOL(x)), .index(x), indexClass(x))

  # if the use is to wants to do some differencing
  if(!is.null(differences) && !is.na(differences) && is.numeric(differences)) {

    # basically just keep iteratively keep running the same function
    # differences is a 'dumb counter'
    if(differences > 1) {
      xTs <- DescTools::DoCall(Fun, c(list(), list(x),     lag=lag,   arithmetic=arithmetic, log = log, na.pad = na.pad, Fun = Fun, Dots))
      diffXts(xTs, lag=lag, differences=differences - 1,              arithmetic=arithmetic, log = log, na.pad = na.pad, Fun = Fun,   ...)
    } else {
      xTs <- DescTools::DoCall(Fun,  c(list(), list(x),    lag=lag,   arithmetic=arithmetic, log = log, na.pad = na.pad, Fun = Fun, Dots))
      return(xTs)
    }

  }

})}



#' expland out xts
#'
#' @description
#' \preformatted{
#'
#' from an xts function stub, create an xts object of derived columns
#'
#' Meant to create many package TTR/PerformanceAnalytics column results
#'
#' Idea was from
#'
#' Time series cross-validation 5
#' January 24, 2013
#' By Deane-Mayer
#' http://www.r-bloggers.com/time-series-cross-validation-5/
#' http://moderntoolmaking.blogspot.com/2013/01
#'       /time-series-cross-validation-5.html?utm_source=feedburner
#'       &utm_medium=feed&utm_campaign=Feed%3A+ModernToolMaking+%28Modern+Tool+Making%29
#'
#' NOTE: if any xTs2, then xTs1 and xTs2 are paired/matched column position to column position.
#'
#' }
#'
#' @param xTs1 xts object
#' @param xTs2 xts object
#' @param Fun function name in the "bare" or in literal quotes("")
#' @param Whiches list of possible varying parameters that are expanded
#' to all possible combinations by expand.grid
#' @param AltName string alternate name for "Fun"
#' @param Prefix boolan default is FALSE.  TRUE would place the column meta before the column name.
#' @param FixedSep string divider of meta items
#' @param quote boolean passed to DescTools DoCall
#' @param envir calling environment
#' @return new xts object of new derived columns
#' @examples
#' \dontrun{
#'
#' # require(quantmod)
#' # ibm <- getSymbols("IBM", from = "1970-01-01", to = "1970-01-13", auto.assign = FALSE)
#' # explodeXts(ibm[,c("IBM.Open","IBM.Close")], Fun = "TTR::SMA", Whiches = list(n = 2:3))
#' #
#' #            IBM.Open.TTR.SMA.n.2 IBM.Close.TTR.SMA.n.2 IBM.Open.TTR.SMA.n.3 IBM.Close.TTR.SMA.n.3
#' # 1970-01-02                   NA                    NA                   NA                    NA
#' # 1970-01-05               18.262                18.325                   NA                    NA
#' # 1970-01-06               18.356                18.419               18.312                18.358
#' # 1970-01-07               18.419                18.431               18.379                18.425
#' # 1970-01-08               18.431                18.456               18.425                18.446
#' # 1970-01-09               18.456                18.463               18.446                18.454
#' # 1970-01-12               18.463                18.419               18.454                18.438
#'
#' # > explodeXts(ibm[,c("IBM.Open")], Fun = "TTR::SMA", Whiches = list(n = 2:3))
#' # IBM.Open.TTR.SMA.n.2 IBM.Open.TTR.SMA.n.3
#' # 1970-01-02                   NA                   NA
#' # 1970-01-05               18.262                   NA
#' # 1970-01-06               18.356               18.312
#' # 1970-01-07               18.419               18.379
#' # 1970-01-08               18.431               18.425
#' # 1970-01-09               18.456               18.446
#' # 1970-01-12               18.463               18.454
#'
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom purrr transpose
#' @importFrom plyr llply
#' @importFrom DescTools DoCall
#' @export
explodeXts <- function(  xTs1 = NULL, xTs2 = NULL, Fun = NULL
                       , Whiches   = NULL
                       , AltName   = NULL, Prefix = NULL, FixedSep  = NULL
                       , quote     = FALSE, envir = parent.frame(2)
                       , ...){
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  xTs1  <- initXts(xTs1)
  xTs2  <- initXts(xTs2)
  if(is.null(FixedSep)) FixedSep = "."

  DescTools::DoCall(expand.grid, Whiches) %>%
      as.list %>%
        { purrr::transpose(.) } -> WhichesCombinations
  if(!NCOL(WhichesCombinations)){ return(initXts()) }

  if(mode(Fun) == "function") {
    Fun = match.fun(Fun)
    isCharFun <- FALSE
  } else {
    isCharFun <- TRUE
  }

  xTs <- initXts()
  FunctionEnv <- environment()

  plyr::llply(WhichesCombinations, function(WhichCombo) {

    plyr::llply(pairWise(xTs1, xTs2), function(xTsColumnSet) {

      xTs1 <- xTsColumnSet[[1]]
      xTs2 <- xTsColumnSet[[2]]
      # will no longer happen: pairWise always returns a 'list of pairs'
      # if(length(xTsColumnSet) >= 2) { xTs2 <- xTsColumnSet[[2]] } else { xTs2 <- NULL }

      if(NVAR(xTs2)) { xTs2List <- list(xTs2) } else { xTs2List <- NULL }
      Temp <- DescTools::DoCall(Fun, args = c(list(), list(xTs1), xTs2List, WhichCombo, list(...)), quote = quote, envir = envir)

      Temp <- newXtsColName( Temp, Fun = Fun, isCharFun = isCharFun, xTs1 = xTs1, xTs2 = xTs2, WhichCombo = WhichCombo
                           , AltName = AltName, Prefix = Prefix, FixedSep = FixedSep)

      assign("xTs", merge(xTs, Temp), envir = FunctionEnv)

      invisible()

    }) -> Empty

  }) -> Empty

  xTs

})}



#' leading
#'
#' @description
#' \preformatted{
#'
#' Pads the beginning date as necessary.
#'
#' Get the leading month near eom date.
#'
#' }
#'
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
#' @importFrom lubridate %m+%
#' @importFrom DescTools Day
#' @importFrom DescTools LastDayOfMonth
Leading <- function(xTs = NULL, Shift = NULL, NoColNameChange = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  `%m+%` <- lubridate::`%m+%`

  if(is.null(NoColNameChange)) NoColNameChange <- FALSE

  xTs <- initXts(xTs)
  if(is.null(Shift)) Shift = 1
  # compare to quantmod:::Lag.xts
  if(periodicity(xTs)[["scale"]] == "monthly") {
    if(DescTools::Day(head(index(xTs),1)) %in% c(28:31)) {
      refDates <- DescTools::LastDayOfMonth(head(index(xTs),1) %m+% months(-1 * seq_len(Shift)) )
      xTs <- merge(xTs, xts(, refDates) )
    }
  }
  xTs %>% { lag(.,-1 * Shift) } -> xTs
  if(!NoColNameChange) {
    if(stringr::str_detect(colnames(xTs)[1], "leadingrets$")) {
      colnames(xTs)[1] <- stringr::str_replace(colnames(xTs)[1], "laggingrets$", "rets")
    } else {
      colnames(xTs)[1] <- stringr::str_replace(colnames(xTs)[1], "rets$", "leadingrets")
    }
  }
  xTs
})}



#' lagging
#'
#' @description
#' \preformatted{
#'
#' Get the lagging month near eom date.
#'
#' }
#'
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
#' @importFrom lubridate %m+%
#' @importFrom DescTools Day
#' @importFrom DescTools LastDayOfMonth
Lagging <- function(xTs = NULL, Shift = NULL, NoColNameChange = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  `%m+%` <- lubridate::`%m+%`

  if(is.null(NoColNameChange)) NoColNameChange <- FALSE

  xTs <- initXts(xTs)
  if(is.null(Shift)) Shift = 1
  # compare to quantmod:::Lag.xts
  if(periodicity(xTs)[["scale"]] == "monthly") {
    if(DescTools::Day(head(index(xTs),1)) %in% c(28:31)) {
      refDates <- DescTools::LastDayOfMonth(tail(index(xTs),1) %m+% months( 1 * seq_len(Shift)) )
      xTs <- merge(xTs, xts(, refDates) )
    }
  }
  xTs %>% { lag(., 1 * Shift) } -> xTs
  if(!NoColNameChange){
    if(stringr::str_detect(colnames(xTs)[1], "leadingrets$")) {
      colnames(xTs)[1] <- stringr::str_replace(colnames(xTs)[1], "leadingrets$", "rets")
    } else {
      colnames(xTs)[1] <- stringr::str_replace(colnames(xTs)[1], "rets$", "laggingrets")
    }
  }
  xTs
})}



#' current
#'
#' @description
#' \preformatted{
#'
#' Pads beginning date as necessary.
#'
#' Get the current month near eom date.
#'
#' }
#'
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
#' @importFrom lubridate %m+%
#' @importFrom DescTools Day
#' @importFrom DescTools LastDayOfMonth
Current <- function(xTs = NULL, Shift = NULL, NoColNameChange = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  `%m+%` <- lubridate::`%m+%`

  if(is.null(NoColNameChange)) NoColNameChange <- FALSE

  xTs <- initXts(xTs)
  if(is.null(Shift)) Shift = 0
  # compare to quantmod:::Lag.xts
  if(periodicity(xTs)[["scale"]] == "monthly") {
    # if(DescTools::Day(head(index(xTs),1)) %in% c(28:31)) {
    #   # ERROR: CAN NOT BE ZERO(0)
    #   refDates <- DescTools::LastDayOfMonth(head(index(xTs),1) %m+% months(0 * seq_len(Shift)) )
    #   xTs <- merge(xTs, xts(, refDates) )
    # }
  }
  xTs %>% { lag(., 0 * Shift) } -> xTs
  if(!NoColNameChange){
    if(stringr::str_detect(colnames(xTs)[1], "currentrets$")) {
      colnames(xTs)[1] <- stringr::str_replace(colnames(xTs)[1], "currentrets$", "rets")
    } else {
      colnames(xTs)[1] <- stringr::str_replace(colnames(xTs)[1], "rets$", "currentrets")
    }
  }
  xTs
})}



#' time slices
#'
#' @description
#' \preformatted{
#' Given a matrix(Start, End) of cell Real values of YYYYMMDD.
#' Return a list of date ranges.
#' }
#'
#' @param timeSliceMatrix NULL(default) required.
#' Two column matrix of start and end dates.  The left column is named "Start."
#' The right column is named "End". The cell values are Real values in format YYYYMMDD.
#' The "Start" column contains beginning of month dates.
#' The "End" column contains end of month dates.
#' @param allSlicesStart NULL(default), Date of the earlies possible date.
#' This Date is the first day(1st) of a month. Note: this filter is applied LAST.
#' @param allSlicesEnd NULL(default), Date of the latest possbile date.
#' This Date is the last day(last) of a month. Note: this filter is applied late.
#' @param LongTimeSlices FALSE(default). If TRUE, include non-date range that
#' is before this date range.
#' @param LongestTimeSlice FALSE(default). If TRUE then the start value is the
#' beginning of "NBER dates" (and limited by allSlicesStart)
#' LongestTimeSlice = TRUE and LongTimeSlices = TRUE ARE mutually exclusive choices
#' of each other
#' @param OmitSliceFirstDate FALSE(default), All dates are end of month dates.
#' A previous timeslice tailing date is the the same date as the next timeslice heading date.
#' TRUE omits the heading date from each time slice.  This may be necessary to prevent
#' repetition of data, for example in sending timeslices to machine learning models.
#' @return a list of vectors of Dates of date Ranges
#' @examples
#' \dontrun{
#' str(timeSlices(timeSliceMatrix = tis::nberDates()))
#' str(timeSlices(timeSliceMatrix = tis::nberDates(), allSlicesStart = zoo::as.Date("1969-12-31")))
#' str(timeSlices(timeSliceMatrix = tis::nberDates(), allSlicesStart = zoo::as.Date("1969-12-31")
#'   , LongTimeSlices = TRUE)
#' )
#' str(timeSlices(timeSliceMatrix = tis::nberDates(), allSlicesStart = zoo::as.Date("1969-12-31")
#'   , LongTimeSlices = TRUE, OmitSliceFirstDate = TRUE)
#' )
#' str(timeSlices(timeSliceMatrix = tis::nberDates(), allSlicesStart = zoo::as.Date("1969-12-31")
#' , LongestTimeSlice = TRUE)
#' )
#' # now after seeing all of the date, THEN, choose the filter
#' str(timeSlices(timeSliceMatrix = tis::nberDates(), allSlicesStart = zoo::as.Date("1969-12-31")
#' , LongestTimeSlice = TRUE, allSlicesStart = ?, allSlicesEnd = ?)
#' )
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom plyr llply
#' @importFrom zoo as.Date
timeSlices <- function(timeSliceMatrix = NULL, allSlicesStart = NULL, allSlicesEnd = NULL, LongTimeSlices = NULL, LongestTimeSlice = NULL, OmitSliceFirstDate = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  if(is.null(timeSliceMatrix)) stop("timeSlices needs a timeSliceMatrix")

  if(!length(LongTimeSlices))   LongTimeSlices <- FALSE
  if(!length(LongestTimeSlice)) LongestTimeSlice <- FALSE

  datesFrom <-  function(x) zoo::as.Date(as.character(x), format = "%Y%m%d")
  beginDates <- datesFrom(timeSliceMatrix[, "Start"])
    endDates <- datesFrom(timeSliceMatrix[, "End"  ])
  xts(
      matrix(c(
          as.numeric(beginDates)
        , as.numeric(endDates)
        )
        , ncol = 2, dimnames = list(NULL, c("Start","End"))
      )
    , beginDates
  ) -> THEDates
  # previous recession's end
  LongStart <- lag(THEDates[,"End"]) + 1; colnames(LongStart)[1] <- "LongStart"
  THEDates <- merge(LongStart, THEDates)

  # filter out
  if(length(allSlicesStart)) THEDates <- THEDates[allSlicesStart   <= index(THEDates)]
    if(!NROW(THEDates)) stop("timeSlices allSlicesStart removed all data")
  if(length(allSlicesEnd))   THEDates <- THEDates[index(THEDates) <= allSlicesEnd]
    if(!NROW(THEDates)) stop("timeSlices allSlicesEnd removed all data")

  # FUTURE: instead, could have detected periodicity and used split.xts
  # determine
  split(as.data.frame(THEDates), seq_len(NROW(THEDates))) %>%
     plyr::llply(function(x) {

       if(LongestTimeSlice) {
         ActualStart <- index(THEDates[1,"Start"])
       } else if(LongTimeSlices) {
         ActualStart <- zoo::as.Date(x[["LongStart"]])[allSlicesStart <= zoo::as.Date(x[["LongStart"]])]
       } else {
         ActualStart <- zoo::as.Date(x[["Start"]])
       }
       # single case (earliest record)
       if(!length(ActualStart)) ActualStart <- zoo::as.Date(x[["Start"]])
       DateSeq <- seq(from = ActualStart, to = zoo::as.Date(x[["End"]]) + 1, by = "month") - 1
       # choose to omit the first observation
       if(!is.null(OmitSliceFirstDate) && OmitSliceFirstDate && length(DateSeq)) DateSeq <- DateSeq[-1]
       return(DateSeq)

     }) -> ListOfDateRanges

     ListOfDateRanges

})}







#' change the index date to the future
#'
#' @description
#' \preformatted{
#'
#' }
#'
#' @param x xts object or Date object
#' @return xts object or data object with the index moved up
#' @rdname eomIndex
#' @export
eomIndex <- function(x = NULL) {

  # tryCatchLog is not allowed here
  UseMethod("eomIndex")

}


#' @rdname eomIndex
#' @export
#' @importFrom tryCatchLog tryCatchLog
eomIndex.default <- function(x = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  stop("No eomIndex method for <input>")

})}


#' convert to an eom date
#'
#' @description
#' \preformatted{
#'
#' }
#'
#' @param x date object
#' @rdname eomIndex
#' @examples
#' \dontrun{
#' # > require(xts)
#' # > eomIndex(zoo::as.Date("1970-01-12"))
#' # [1] "1970-01-31"
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom lubridate days
#' @importFrom lubridate %m+%
#' @importFrom Hmisc truncPOSIXt
#' @importFrom zoo as.Date
eomIndex.Date <- function(x = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  days <-  lubridate::days
 `%m+%` <- lubridate::`%m+%`

  date <- initDate(x)
  Hmisc::truncPOSIXt(date, units = "months") %>%
    { zoo::as.Date(.)} %m+%
      months(1) %m+%
        days(-1)

})}


#' @rdname eomIndex
#' @examples
#' \dontrun{
#' # > require(xts)
#' # > xTs <- xts(, zoo::as.Date("1970-01-12"))
#' # > eomIndex(xTs)
#' # Data:
#' # numeric(0)
#' #
#' # Index:
#' #  Date[1:1], format: "1970-01-31"
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom DescTools DoCall
#' @importFrom plyr llply
eomIndex.xts <- function(x = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  xTs  <- initXts(x)
  # plyr::llply(index(xTs), eomIndex) %>%
  #   { DescTools::DoCall(c,.) } %>%
  #     { xts(Coredata(xTs),.) }

  xTs  <- initXts(x)
  eomIndex(index(xTs)) %>%
      { xts(Coredata(xTs),.) }

})}


#' get the end of month UNRATE from FRED
#'
#' @description
#' \preformatted{
#'
#' }
#'
#' @return xts object
#' @export
#' @importFrom tryCatchLog tryCatchLog
unRateEomData <- function() {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  # index adjust
  # last known unemployment rate: when I recieved it; one month later
  symbolData(Symbol = "UNRATE", src = "FRED") %>%
    eomIndex

})}


#' add UNRATE (unrate)
#'
#' @description
#' \preformatted{
#'
#' }
#'
#' @param xTs xts object
#' @return xts object with merged data into xTs
#' @export
#' @importFrom tryCatchLog tryCatchLog
addUnRateEomData <- function(xTs = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  InBndxTs <- as.character(substitute(xTs))
  initMktData(xTs, InBndxTs)
  # xTs  <- initXts(xTs)

  unRate <- unRateEomData()
  xTs <- combineXts(xTs, unRate)

  # xTs
  return(releaseMktData(xTs, InBndxTs, isInBndxTsMktData))

})}




#' University of Michigan: Consumer Sentiment
#'
#' @description
#' \preformatted{
#'
#' Get from FRED (historical) and briefing.com (recent) University of
#' Michigan: Consumer Sentiment voting results data.
#'
#' }
#'
#' @param Symbols  a character vector specifying the names of each symbol to be loaded
#' Possible Symbols are the following:
#' "UMichSentiment" and "UMCSENT". This currently the same data.
#' @param env where to create objects. (.GlobalEnv)
#' @param return.class desired class of returned object.
#' Can be xts, zoo, data.frame, or xts (default)
#' @param force FALSE(default) re-download data from FRED(historical)
#' and the University of Michigan (current/last_known) ( See the examples. )
#' The hidden variable ".UMichSentiment_path2file" located in the
#' environment of parameter env is the last known location of the "html" file.
#' Generally, using this parameter "force = TRUE"
#' is NOT necessary: the hidden variables are persistent through the
#' entire R session.  Also, send parameter "verbose = TRUE" to see
#' the *path2file location.
#' @param ... additional parameters
#' @return A call to getSymbols.AAII will load into the specified
#' environment one object for each \code{Symbol} specified,
#' with class defined by \code{return.class}.
#' @author Jeffrey A. Ryan
#' @author Andre Mikulec (adapted original code to work with AAII sentiment data)
#'
#' @references
#' Recent data is from
#' Univ. of Michigan Consumer Sentiment - Prelim:
#' \cite{Univ. of Michigan Consumer Sentiment - Prelim \url{https://www.briefing.com/investor/calendars/economic/releases/mich.htm}}
#'
#' @references
#' Original data is from here:
#' \cite{surveys of consumers UNIVERSITY OF MICHIGAN \url{https://data.sca.isr.umich.edu/tables.php}}
#'
#' @references
#' Historical data is from here.
#' At the request of the source, the data is delayed by 1 month:
#' \cite{University of Michigan: Consumer Sentiment \url{https://fred.stlouisfed.org/series/UMCSENT}}
#'
#' \cite{University of Michigan: Consumer Sentiment raw data \url{https://fred.stlouisfed.org/data/UMCSENT.txt}}
#' @seealso
#' \code{\link{getSymbols}}
#' \code{\link{setSymbolLookup}}
#' @keywords data
#' @examples
#' \dontrun{
#'
#' # common usage
#' if(!exists("UMCSENT")) getSymbols("UMCSENT", src = "UMich")
#'
#' getSymbols(c("UMCSENT"), src = "UMich")
#'
#' # force requery of data from the UMich site
#' # will collect one new FRED (historical obs) data and one new html (latest obs) file
#' getSymbols(c("UMCSENT"), src = "UMich", force = TRUE)
#'
#' # all columns in one xts object
#' getSymbols("UMichSentiment", src = "UMich")
#'
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom htmltab htmltab
#' @importFrom zoo as.Date
#' @importFrom stringr str_c
#' @export
getSymbols.UMich <- function(Symbols, env, return.class = "xts", ...) {
tryCatchLog::tryCatchLog({
initEnv(); on.exit({uninitEnv()})

    importDefaults("getSymbols.UMich")
    this.env <- environment()
    for (var in names(list(...))) {
        assign(var, list(...)[[var]], this.env)
    }
    if (!hasArg("verbose"))
        verbose <- FALSE
    if (!hasArg("auto.assign"))
        auto.assign <- TRUE

    if(!exists("force", envir = this.env, inherits = FALSE))
        force = FALSE

    # begin non-html area

    if(force) {
      rs <- symbolData(Symbol = "UMCSENT", src = "FRED", New = TRUE, NewMaxAge = "0 secs")
     if (verbose)
         cat("done.\n\n")
    } else {
      rs <- symbolData(Symbol = "UMCSENT", src = "FRED")
     if (verbose)
         cat("done.\n\n")
    }

    # end non-html area

    # begin html area

    if(exists(".UMichSentiment_path2file", envir = env, inherits = FALSE)) {
      assign("tmppage", get(".UMichSentiment_path2file", envir = env, inherits = FALSE), envir = this.env, inherits = FALSE)
    } else {
      tmppage <- NULL
    }
    if( !is.null(tmppage) &&
        !is.na(file.info(tmppage)$mtime) && # is.na - if the file does not exist
        !force
    ) {
    } else {

        # possible clean-up
        oldtmppage <- tmppage
        if(!is.null(oldtmppage) && !is.na(file.info(oldtmppage)$mtime)) on.exit(unlink(oldtmppage))

        UMICH.URL <- "https://www.briefing.com/investor/calendars/economic/releases/mich.htm"
        tmppage <- tempfile(fileext = ".html") # ( JAN 2019 )
        #
        # do not remove the 'tmppage' file
        # In this R session, keep the file around and available for the next query [if any]
        # the site https://www.briefing.com/investor/calendars/economic/releases/mich.htm is NOT engineered
        # to handle MANY data queries, NOR denial of service attacks

        if (verbose)
            cat("downloading ", "UMichSentiment", ".....\n\n")
        quantmod___try.download.file(UMICH.URL, destfile = tmppage, quiet = !verbose, mode = "wb", ...)
        assign(".UMichSentiment_path2file", tmppage, envir = env, inherits = FALSE)

    }
    if (verbose)
        cat("reading disk file ", tmppage, ".....\n\n")

    rt <- htmltab::htmltab(  # google chrome ( DEC 2018 )
            doc = tmppage
          , which = '//*[@id="InPlayEqContent"]/div[4]/div[1]/table'
          , rm_nodata_cols = F)

    rt <- rt[rt$Category == "Sentiment",][!colnames(rt) %in% "Category"] %>% unlist
    NowYear <-  DescTools::Year(Sys.Date())
    dataRecent  <- as.numeric(rt)
    # FRED historical UMCSENT dates are the "first of the month"
    # that datum means "about that entire month"
    indexRecent <- zoo::as.Date(stringr::str_c("01-", names(rt), "-", NowYear), format = "%d-%b-%Y")
    rt <- xts(dataRecent, indexRecent); colnames(rt) <- "UMCSENT"

    # end html area

    # prepare to splice (nothing to do)

    # splice

    rst <- rbind(rs,rt)
    # remove the first(earliest) duplicate index (if any)
    rst <- rst[!duplicated(index(rst), fromLast = TRUE),]
    fr <- rst
    fri <- fr # pass-throught on "UMichSentiment"

    # decompose [if any] into [many] Symbol(s), then return

    for (i in 1:length(Symbols)) {

        # User only wants an individual column
        if(Symbols[[i]] != "UMichSentiment") {
           if (verbose)
             cat("selecting ", Symbols[[i]], ".....\n\n")
          fri <- fr[, colnames(fr)[tolower(colnames(fr)) %in% tolower(Symbols[[i]])]]
        }

        fri <- quantmod___convert.time.series(fr = fri, return.class = return.class)
        if (auto.assign)
            assign(Symbols[[i]], fri, env)
    }
    if (auto.assign)
        return(Symbols)
    return(fri)

})}



#' end of month University of Michigan: Consumer Sentiment data
#'
#' @description
#' \preformatted{
#'
#' }
#'
#' @return xts object
#' @export
#' @importFrom tryCatchLog tryCatchLog
UMCSentimentEomData <- function() {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  # index adjust
  # last known University of Michigan: Consumer Sentiment : when I recieved it
  getSymbols("UMCSENT", src = "UMich", auto.assign = FALSE)  %>%
    eomIndex %>%
      trimLeadingNAGaps

})}



#' trim leading NAs and leading gaps of NAs
#'
#' @description
#' \preformatted{
#'
#' Some FRED early data was delivered less
#' frequently than later FRED data.
#' Remove that earlier wider-gapped data.
#'
#' Also, many functions in R CRAN package TTR
#' will return an error if leading NA values are found
#'
#' }
#'
#' @param xTs xts object
#' @return xts object
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom plyr llply
trimLeadingNAGaps <- function(xTs) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  xTs  <- initXts(xTs)
  #  for a single column matrix, plyr::aaply is broken
  as.matrix(as.data.frame(plyr::llply(as.data.frame(coredata(xTs)), function(x) {
           if(!any(is.na(x))) return(x)
           x[seq_len(max(which(is.na(x))))] <- NA_real_
           x
  }))) -> coredata(xTs)
  xTs

})}



#' add University of Michigan: Consumer Sentiment
#'
#' @description
#' \preformatted{
#'
#' NOT YET USED ANYWHERE
#'
#' }
#'
#' @param xTs xts object
#' @return xts object with merged data into xTs
#' @export
#' @importFrom tryCatchLog tryCatchLog
addUMCSentimentEomData <- function(xTs = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})
  xTs  <- initXts(xTs)

  UMCSentiment <- UMCSentimentEomData()

  combineXts(xTs, UMCSentiment)

})}



#' join two xts objects
#'
#' @description
#' \preformatted{
#'
#' Handles the edge case: if BOTH have no coredata
#' (then merge.xts produces ONE no-data no-index xts object)
#' then instead, preserve the indexes.
#'
#' Also, right side xts (xTs1) attributes are preserved that
#' contain the attribute name in the xtsAttribute
#' "carry.xts.attributes" character vector.
#' This means that right side xts (xTs1) attributes "add-to" and
#' overwrite left side xts (xTs) attributes.
#'
#' BADLY NEED AND EXAMPLE.
#'
#' FUTURE: make combineXts a replacement for xts:::merge.xts.
#'
#' }
#'
#' @param xTs xts object
#' @param xTs1 xts object to merge into xTs
#' @param carryAppend FALSE(Default) Instead of overwriting left side xts attributes,
#' append to the vector value to xtsAttribute vector
#' @return xts object
#' @examples
#' \dontrun{
#' > combineXts( initXts(NULL),xts(,zoo::as.Date(0)) )
#' Data:
#' numeric(0)
#'
#' Index:
#'  Date[1:1], format: "1970-01-01"
#' > combineXts( xts(,zoo::as.Date(1)), xts(,zoo::as.Date(0)) )
#' Data:
#' numeric(0)
#'
#' Index:
#'  Date[1:2], format: "1970-01-01" "1970-01-02"
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
combineXts <- function(xTs = NULL,xTs1 = NULL, carryAppend = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  xTs  <- initXts(xTs); xTs1 <- initXts(xTs1)

  if(is.null(carryAppend)) carryAppend <- FALSE

  xTsxtsAttr  <- xtsAttributes(xTs)
  xTs1xtsAttr <- xtsAttributes(xTs1)

  # edge case: if BOTH have no coredata (then merge.xts produces
  # ONE no-data no-index xts object) then instead, preserve the indexes
  if(is.null(Coredata(xTs)) && is.null(Coredata(xTs1))) {
    xTs <- xts(, sort(unique(c(index(xTs),index(xTs1)))))
  } else {
    xTs <- merge(xTs,xTs1)
  }

  # NOTE: these does NOT remove xts attributes
  # TO DO THAT, I would need my own function: `xtsAttributes[[<-.xts` to do that.

  if("carry.xts.attributes" %in% Names(xTsxtsAttr)) {
    for(carry.xts.attribute in xTsxtsAttr[["carry.xts.attributes"]]){
      if(carryAppend) {
        xtsAttributes(xTs)[[carry.xts.attribute]] <- unique(c(xtsAttributes(xTs)[[carry.xts.attribute]], xTsxtsAttr[[carry.xts.attribute]]))
      } else {
        xtsAttributes(xTs)[[carry.xts.attribute]] <- xTsxtsAttr[[carry.xts.attribute]]
      }
    }
  }

  if("carry.xts.attributes" %in% Names(xTs1xtsAttr)) {
    for(carry.xts.attribute in xTs1xtsAttr[["carry.xts.attributes"]]){
      if(carryAppend) {
        xtsAttributes(xTs)[[carry.xts.attribute]] <- unique(c(xtsAttributes(xTs)[[carry.xts.attribute]], xTs1xtsAttr[[carry.xts.attribute]]))
      } else {
        xtsAttributes(xTs)[[carry.xts.attribute]] <- xTs1xtsAttr[[carry.xts.attribute]]
      }

      # add
      xtsAttributes(xTs)[["carry.xts.attributes"]] <- unique(c(xtsAttributes(xTs)[["carry.xts.attributes"]], carry.xts.attribute))
    }
  }

  xTs

})}





#' cash APC returns (CASHapcrets)
#'
#' @description
#' \preformatted{
#'
#' all SYMBOLS must lead and be UPPERCASE
#'
#' }
#'
#' @param xTs xts object (only takes the index)
#' @param format default(NONE)(required) Choose "leading" or "current".
#' Required 'string of characters' in replacement column.
#' @return xts object with the same index as xTs
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_replace_all str_c str_replace
cashReturns <- function(xTs = NULL, format = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  xTs  <- initXts(xTs)

  if(is.null(format)) stop("cashReturns needs parameter 'format'")

  cashRets <- xts(rep(0,NROW(xTs)),index(xTs))
  # must keep HARD coded
  # so all SYMBOLS must lead and be UPPERCASE

  RegExpr <- stringr::str_replace_all(xtsAttributes(xTs)[["change"]], "[.]", "[.]")
  # multiple targets (multiple change values)
  RegExpr <- stringr::str_c(RegExpr, collapse = "|")
  RegExpr <- stringr::str_c(".+(?=", RegExpr,")")

  # colnames(cashRets)[1] <- stringr::str_replace(xtsAttributes(xTs)[["rettarget"]], RegExpr, "CASH")

  if(format == "leading") {
    if(xtsAttributes(xTs)[["rettarget"]] %Like% "current") {
      Leadingrettarget <- stringr::str_replace(xtsAttributes(xTs)[["rettarget"]], "current", "leading")
    } else {
      Leadingrettarget <- xtsAttributes(xTs)[["rettarget"]]
    }
    colnames(cashRets)[1] <- stringr::str_replace(Leadingrettarget, RegExpr, "CASH")
  }

  if(format == "current") {
    if(xtsAttributes(xTs)[["rettarget"]] %Like% "leading") {
      Currentrettarget <- stringr::str_replace(xtsAttributes(xTs)[["rettarget"]], "leading", "current")
    } else {
      Currentrettarget <- xtsAttributes(xTs)[["rettarget"]]
    }
    colnames(cashRets)[1] <- stringr::str_replace(Currentrettarget, RegExpr, "CASH")
  }

  cashRets

})}





#' leading cash APC returns (CASHapcrets)
#'
#' @description
#' \preformatted{
#'
#' }
#'
#' @param xTs xts object (only takes the index)
#' @return leading xts object
#' @export
leadingCashReturns <- function(xTs = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  xTs  <- initXts(xTs)

  cashReturns(xTs, format = "leading") %>%
    Leading(NoColNameChange = TRUE) -> xTs
  xTs

})}






#' current cash APC returns (CASHapcrets)
#'
#' @description
#' \preformatted{
#'
#' }
#'
#' @param xTs xts object (only takes the index)
#' @return leading xts object
#' @export
currentCashReturns <- function(xTs = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  xTs  <- initXts(xTs)

  cashReturns(xTs, format = "current") %>%
    Current(NoColNameChange = TRUE) -> xTs
  xTs

})}



#' add current and leading cash APC returns (CASHapcrets)
#'
#' @description
#' \preformatted{
#'
#' }
#'
#' @param xTs xts object
#' @param Symbol getSymbols Symbol
#' @param IsTarget is the objective
#' @param IsATarget is a member of many possible objectives
#' @return xts object with merged data into xTs
#' @export
addCurrLeadCashReturns <- function(xTs = NULL, IsTarget = NULL, IsATarget = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  InBndxTs <- as.character(substitute(xTs))
  initMktData(xTs, InBndxTs)

  # xTs  <- initXts(xTs)
  if(is.null(IsTarget))   IsTarget = FALSE
  if(is.null(IsATarget)) IsATarget = FALSE
  if(IsTarget)           IsATarget <- TRUE

  OrigClmns <- colnames(xTs)
                         # CASHapcrets
  xTs <- combineXts(xTs, leadingCashReturns(xTs))
  NewClmns  <- setdiff(colnames(xTs), OrigClmns)
  if(IsTarget) {
    xtsAttributes(xTs)[["rettarget"]] <- NewClmns
  }
  if(IsATarget) {
    xtsAttributes(xTs)[["rettargets"]] <- unique(c(NewClmns, xtsAttributes(xTs)[["rettargets"]]))
  }

  xTs <- combineXts(xTs, currentCashReturns(xTs))

  # xTs
  return(releaseMktData(xTs, InBndxTs, isInBndxTsMktData))

})}



#' add poor performance data (CRASHACML)
#'
#' @description
#' \preformatted{
#'
#' Of the 'target', find the two(2) worst performing months
#' over the next(future six(6)) months
#' and then sum those two(2) bad worst performing months together.
#'
#' calls
#'
#' Predictee <- xtsAttributes(mktdata)[["rettarget"]]
#' sumOrdersXts(mktdata[, Predictee], r =  -6:-1, nt = 2)
#'
#' see ? sumOrdersXts
#'
#' }
#'
#' @param xTs xts object
#' @param Predictee target. xtsAttributes(mktdat)[["rettarget"]](Default)
#' @return xts object with merged data into xTs
#' @export
addCrashData <- function(xTs = NULL, Predictee = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  InBndxTs <- as.character(substitute(xTs))
  initMktData(xTs, InBndxTs)

  # xTs  <- initXts(xTs)
  if(is.null(Predictee)) Predictee <- xtsAttributes(xTs)[["rettarget"]]

  # sumOrdersXts
  xTs1 <- sumOrdersXts(xTs[, Predictee], r =  -6:-1, nt = 2)
  colnames(xTs1) <- "CRASHACML"
  xTs <- combineXts(xTs, xTs1)

  # xTs
  return(releaseMktData(xTs, InBndxTs, isInBndxTsMktData))

})}


#' get the latest value in the month
#'
#' @description
#' \preformatted{
#'
#' }
#'
#' @param xTs xts object.
#' (currently) eomData only works on single column xtx objects.
#' @return xts object of the last observation with the
#' month date rounded up to the last day of the month
#' @examples
#' \dontrun{
#' # library(xts)
#' # > xTs <- xts(c(1,NA_real_,2), zoo::as.Date(c(1,11,21)))
#' # > xTs
#' #            [,1]
#' # 1970-01-02    1
#' # 1970-01-12   NA
#' # 1970-01-22    2
#' #
#' # > eomData(xTs = xTs)
#' #            [,1]
#' # 1970-01-31    2
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
toMonthlyData <- function(xTs = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  xTs  <- initXts(xTs)
  xTsOrig <- xTs

  if(NCOL(xTs) > 1) stop("(currently) eomData only works on single column xtx objects")
  xTs <- To.Monthly(xTs[!is.na(xTs)], OHLC = FALSE, indexAt = "lastof")
  # xTs <- xTs[index(xTs) <= tail(index(xTsOrig),1)]
  # last To.Monthly date is no greater than the last 'source data' date
  # or
  # last To.Monthly date is no greater than today's date ( as.Date in case the index type is 'not Date class')
  xTs <- xTs[index(xTs) <= tail(index(xTsOrig),1) | as.Date(index(xTs)) <= Sys.Date()]

  xTs

})}






#' getSymbol data from the target
#'
#' @description
#' \preformatted{
#'
#' }
#'
#' @param Symbol single target symbol as a string
#' @param src source of the data: Curently, "yahoo"(default).  Other: "FRED"
#' @param New TRUE(default) of the Symbol gets the data and stores the data
#' through the method of getNewSymbols (TRUE).  Otherwise,
#' get the data and store the data through the method of getSymbols (FALSE).
#' @param NewMaxAge default("4 hours"), if New = TRUE, then this
#' is the timeout period of the stored data. After NewMaxAge time has passed,
#' the the data will be re-queried from Yahoo corporation.
#' of the data
#' @param ... dots passed
#' @return xts object of results
#' @examples
#' \dontrun{
#'
#' # St. Louis FRED
#'
#' head(symbolData("GDP", src = "FRED"),1)
#'                           GDP
#' 1947-01-01 243.16399999999999
#'
#' Get all of the records from the "cache" or "pg".
#' If the cache and pg is older than 4 hours then
#' the data will be acquired anew from the source
#' and loaded into the "pg" and the "cache" and returned
#' to the user.
#'
#' default New == TRUE (default)
#' means use the function get\*New\*Symbols
#' checks the xts "updated" attribute
#'   checks (1)cache then (2)pg if data is older than NewMaxAge = "4 hours"
#'   checks "cache": manually check cache by ls(all.names = TRUE)
#'   checks "pg": manually check database by using: SELECT * FROM "Symbols"."Symbols"
#' then will get the new data from the "cache" or "pg".
#'
#' symbolData(Symbol = "UNRATE", src = "FRED")
#'
#' # Full test . . .
#' # 1. remove cache data: rm(.UNRATE) (if there)
#' # 2. drop database table "Symbols"."UNRATE"
#' # 3. remove corresponding record from "Symbols"."Symbols"
#' # 4. symbolData(Symbol = "UNRATE", src = "FRED")
#'
#' # Partial tests
#'
#' 2.1 Manually remove some (few) bottom records from the
#' database table "Symbols"."UNRATE"
#' Below just insert only-new (few) records
#' # placeNewRecords == "AddOnlyNew" (default)
#'
#' symbolData(Symbol = "UNRATE", src = "FRED", NewMaxAge = "1 secs")
#'
#' # or
#'
#' # 2.2
#' # placeNewRecords =="TruncateTable"
#'
#' symbolData(Symbol = "UNRATE", src = "FRED", NewMaxAge = "1 secs", placeNewRecords = "TruncateTable")
#'
#' # Yahoo
#'
#' symbolData(Symbol = "^GSPC"
#'
#' symbolData(Symbol = "^GSPC", NewMaxAge = "1 secs")
#'
#' symbolData(Symbol = "^GSPC", NewMaxAge = "1 secs", placeNewRecords = "TruncateTable")
#'
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_replace str_c
#' @importFrom stringr str_c
symbolData <- function(Symbol = NULL, src = NULL, New = NULL, NewMaxAge = NULL, ...) {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})
  if(is.null(Symbol)) stop("No symbolData was requested")

  if(is.null(src)) src <- "yahoo"

  if(is.null(New)) New <- TRUE
  if(New) {
    if(is.null(NewMaxAge)) {
    NewMaxAge <- "4 hours"
    } # else NewMaxAge <- NewMaxAge
  }

  if(length(Symbol) > 1) stop ("symbolData can only download one symbol at a time.")

  message(stringr::str_c("Begin symbolData - ",  src, " ", Symbol))

  from = "1900-01-01"

  if(New){
    xTs <- getNewSymbols(Symbol, src = src,
           from = from, auto.assign = FALSE, MaxAge = NewMaxAge, ...)
  } else {
    xTs <- getSymbols(Symbol, src = src,
           from = from, auto.assign = FALSE, ...)
  }

  if((NVAR(xTs) > 1) && any(colnames(xTs) %Like% "Close$")) {
    # since Yahoo 'often' does return an OHLC object
      # there will ONLY be one column 'like' Close
      xTs <- xTs[, colnames(xTs) %Like% "Close$"]
      if(NVAR(xTs) > 1) {
          stop(stringr::str_c("symbolData in Symbol: ",  src, " ", Symbol, "has detected TOO MANY columns like 'Close'"))
        }
      # a Yahoo [stock index] may(usually) has a caret(^)
      NewColName <- stringr::str_replace(Symbol, "\\^", "")
      colnames(xTs)[1] <- NewColName
  }

  message(stringr::str_c("End   symbolData - ",  src, " ", Symbol))

  xTs

})}



#' get the end of month data
#'
#' @description
#' \preformatted{
#'
#' }
#'
#' @param xTs xts object
#' @param Symbol getSymbols Symbol
#' @param src getSymbols source
#' @param SymplifyGeneratorFUN Function that Formats the output.
## #' @param ... further instructions to be passed in the form: Symbols_src_INSTRUCTION
#' @param CarryForward default(NULL) after simplification perform carry forward method(if any)
#' of data to replace future missing values
#' @return xts object with merged data into xTs
#' @export
#' @importFrom tryCatchLog tryCatchLog
## #' @importFrom stringr str_detect str_c
addEomData <- function(xTs = NULL, Symbol = NULL, src = NULL, SymplifyGeneratorFUN = NULL,  CarryForward = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  InBndxTs <- as.character(substitute(xTs))
  initMktData(xTs, InBndxTs)
  # xTs  <- initXts(xTs)

  if(is.null(Symbol)) stop("addEomData needs a Symbol")
  if(is.null(src))    stop("addEomData needs a src")

  ### if(is.null(NA.LOCF)) NA.LOCF <- FALSE

  ## # filter out dots
  ## Dots <- list(...)
  ## Dots <- Dots[stringr::str_detect(Names(Dots), stringr::str_c("^", Symbol, "_", src, "_"))]

  xTs1 <- eomData(Symbol = Symbol, src = src, SymplifyGeneratorFUN = SymplifyGeneratorFUN,  CarryForward = CarryForward)

  xTs  <- combineXts(xTs, xTs1)

  # xTs
  return(releaseMktData(xTs, InBndxTs, isInBndxTsMktData))

})}



#' get the end of month data
#'
#' @description
#' \preformatted{
#'
#' }
#'
#' @param Symbol getSymbols Symbol
#' @param src getSymbols source
#' @param SymplifyGeneratorFUN Function that Formats the output.
#' @param CarryForward default(NULL) after simplification perform
#' carry forward method(if any)
#' of data to replace future missing values.
#' Typical values may be the following. "NA.LOCF"; carry forward the last NA.
#' "NA.Shift.EOLM"; shift the entire series up (to the future) to the
#' last day of the previous month.
#' @return xts object
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom zoo na.locf
eomData <- function(Symbol = NULL, src = NULL, SymplifyGeneratorFUN = NULL,  CarryForward = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  if(is.null(Symbol)) stop("eomData needs a Symbol")
  if(is.null(src))    stop("eomData needs a src")

  if(!is.null(SymplifyGeneratorFUN)) {
    if(mode(SymplifyGeneratorFUN) == "function") {
      SymplifyGeneratorFUN = match.fun(SymplifyGeneratorFUN)
    } else {
      # character function
      SymplifyGeneratorFUN <- get(SymplifyGeneratorFUN)
    }
  }

  # index adjust
  xTs <- symbolData(Symbol = Symbol, src = src)
  if(!is.null(SymplifyGeneratorFUN)){
    xTs <- SymplifyGeneratorFUN(xTs)
  }

  # would have been better to for-loop through 'massagers.'
  if(!is.null(CarryForward)) {

    # a massager may chang the column name
    ColNamesXts <- colnames(xTs)

    if("NA.Shift.EOLM" %in% CarryForward) {
      IndexXts <- index(xTs)
      EOLMDate <- max(index(xTs[IndexXts <= max(IndexXts[IndexXts <= Sys.Date()])]))
      LagAmt   <- tail(delaySinceLastObs(xTs[IndexXts <= EOLMDate]),1)
      xTs <- LagXts(xTs,LagAmt)
      # think about
      # could have modified the column name to be ColnameDelay[LagXtsInEnglish]
    }

    if("NA.LOCF" %in% CarryForward) {
      xTs <- zoo::na.locf(xTs)
    }

    # change back the column name to the original
    colnames(xTs) <- ColNamesXts

  }



  return(xTs)

})}



## # NOT USED ANYWHERE
##
## #' get the latest end of month value in the month from the target
## #'
## #' @description
## #' \preformatted{
## #'
## #' }
## #'
## #' @param Symbol target symbol as a string
## #' @param src See ? symbolData
## #' @return xts object of the last observation with the
## #' month date rounded up to the last day of the month
## #' @examples
## #' \dontrun{
## #'
## #' # weekly
## #' ff <- head(symbolEomData("FF", src = "FRED"),1)
## #' # ff
## #' #              FF
## #' # 1954-07-31 0.63
## #'
## #' # weekly
## #' gspc <- head(symbolEomData("^GSPC"),1)
## #' # gspc
## #'
## #' }
## #' @export
## #' @importFrom tryCatchLog tryCatchLog
## symbolEomData <- function(Symbol = NULL, src = NULL) {
## tryCatchLog::tryCatchLog({
## initEnv();on.exit({uninitEnv()})
##
##   if(is.null(Symbol)) stop("No symbolsEomData was requested")
##   symbolData(Symbol = Symbol, src = src) %>%
##      eomData
##
## })}




#' symbols eom absolute proportional change from the previous month
#'
#' @export
#' @importFrom tryCatchLog tryCatchLog
symbolReturns <- function(Symbol = NULL, src = NULL
                        , SymplifyGeneratorFUN = NULL
                        , ReturnsGeneratorFUN =  NULL, ReturnsGeneratorFUNArgs =  NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  # Proportional change  seems more correct than log differenctes
  #
  # PerformanceAnalytics::Return.calculate
  #
  # AGGREGATE PORTFOLIO CONTRIBUTIONS THROUGH TIME
  # # [absolute] proportional change is CORRECT
  # Return.calculate(.) "Returns = pr/lag.xts(pr) - 1" ->  Return.portfolio
  # https://tradeblotter.wordpress.com/2014/09/25/aggregate-portfolio-contributions-through-time/
  # whereas
  # TTR::ROC "roc <- diff(log(x), n, na.pad = na.pad)"

  if(!is.null(SymplifyGeneratorFUN)) {
    if(mode(SymplifyGeneratorFUN) == "function") {
      SymplifyGeneratorFUN = match.fun(SymplifyGeneratorFUN)
    } else {
      # character function
      SymplifyGeneratorFUN <- get(SymplifyGeneratorFUN)
    }
  }

  if(!is.null(ReturnsGeneratorFUN)) {
    if(mode(ReturnsGeneratorFUN) == "function") {
      ReturnsGeneratorFUN = match.fun(ReturnsGeneratorFUN)
    } else {
      # character function
      ReturnsGeneratorFUN <- get(ReturnsGeneratorFUN)
    }
  }

  if(is.null(Symbol)) stop("No Symbol Data was requested")
  xTs <- symbolData(Symbol = Symbol, src = src)
  # xTs <- toMonthlyData(xTs)
  xTs <- SymplifyGeneratorFUN(xTs)
  if(NVAR(xTs) > 1) {
    xTs <- xTs[,1] # e.g. # SymplifyGeneratorFUN == fancifyXts
  }
  xTs <- ReturnsGeneratorFUN(xTs, ReturnsGeneratorFUNArgs = ReturnsGeneratorFUNArgs)
  return(xTs)

})}



#' get the Symbol current APC returns
#'
#' @description
#' \preformatted{
#'
#' }
#'
#' @return xts object
#' @export
#' @importFrom tryCatchLog tryCatchLog
currentSymbolReturns <- function(Symbol = NULL, src = NULL
                               , SymplifyGeneratorFUN = NULL
                               , ReturnsGeneratorFUN =  NULL, ReturnsGeneratorFUNArgs =  NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  symbolReturns(Symbol = Symbol, src = src, SymplifyGeneratorFUN = SymplifyGeneratorFUN, ReturnsGeneratorFUN =  ReturnsGeneratorFUN, ReturnsGeneratorFUNArgs = ReturnsGeneratorFUNArgs) %>% Current

})}



#' get the Symbol leading APC returns
#'
#' @description
#' \preformatted{
#'
#' }
#'
#' @return xts object
#' @export
#' @importFrom tryCatchLog tryCatchLog
leadingSymbolReturns <- function(Symbol = NULL, src = NULL
                               , SymplifyGeneratorFUN = NULL
                               , ReturnsGeneratorFUN =  NULL, ReturnsGeneratorFUNArgs =  NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  symbolReturns(Symbol = Symbol, src = src, SymplifyGeneratorFUN = SymplifyGeneratorFUN, ReturnsGeneratorFUN =  ReturnsGeneratorFUN, ReturnsGeneratorFUNArgs = ReturnsGeneratorFUNArgs) %>% Leading

})}



#' add leading and current Symbol APC returns (SYMBOLapcrets)
#'
#' @description
#' \preformatted{
#'
#' }
#'
#' @param xTs xts object
#' @param Symbol getSymbols Symbol
#' @param src getSymbols source
#' @param IsTarget is the objective
#' @param IsATarget is a member of many possible objectives
#' @param SymplifyGeneratorFUN Function that Formats the output
#' @return xts object with merged data into xTs
#' @examples
#' \dontrun{
#'
#'
#' addCurrLeadSymbolReturns(mktdata, Symbol = "^GSPC", src = "yahoo"
#'   , IsATarget = TRUE, SymplifyGeneratorFUN = "toMonthlyData"
#'   , ReturnsGeneratorFUN = "Returns"
#'   , ReturnsGeneratorFUNArgs = list(is.na.zero = TRUE,  Fun = list(Fun = "APC", lag = 1))
#' )
#'
#' addCurrLeadSymbolReturns(mktdata, Symbol = "GDP",  src = "FRED2"
#'   , IsTarget = TRUE,  SymplifyGeneratorFUN = "fancifyXts"
#'   , ReturnsGeneratorFUN = "Returns"
#'   , ReturnsGeneratorFUNArgs = list(is.na.zero = TRUE,  Fun = list(Fun = "APC", lag = 3))
#' )
#'
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
addCurrLeadSymbolReturns <- function(xTs = NULL, Symbol = NULL, src = NULL
                                      , IsTarget = NULL, IsATarget = NULL
                                      , SymplifyGeneratorFUN = NULL
                                      , ReturnsGeneratorFUN = NULL, ReturnsGeneratorFUNArgs =  NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  InBndxTs <- as.character(substitute(xTs))
  initMktData(xTs, InBndxTs)
  # xTs  <- initXts(xTs)

  if(is.null(IsTarget))   IsTarget = FALSE
  if(is.null(IsATarget)) IsATarget = FALSE
  if(IsTarget)           IsATarget <- TRUE

  OrigClmns <- colnames(xTs)
  xTs  <- combineXts(xTs, leadingSymbolReturns(Symbol = Symbol, src = src, SymplifyGeneratorFUN = SymplifyGeneratorFUN, ReturnsGeneratorFUN =  ReturnsGeneratorFUN, ReturnsGeneratorFUNArgs = ReturnsGeneratorFUNArgs), carryAppend = TRUE)
                                 # send to return.Portfolio and the calendar
                                 # SYMBOLapcrets
  NewClmns  <- setdiff(colnames(xTs), OrigClmns)
  if(IsTarget) {
    xtsAttributes(xTs)[["rettarget"]] <- NewClmns
  }
  if(IsATarget) {
    xtsAttributes(xTs)[["rettargets"]] <- unique(c(NewClmns, xtsAttributes(xTs)[["rettargets"]]))
  }

  xTs  <- combineXts(xTs, currentSymbolReturns(Symbol = Symbol, src = src, SymplifyGeneratorFUN = SymplifyGeneratorFUN, ReturnsGeneratorFUN =  ReturnsGeneratorFUN, ReturnsGeneratorFUNArgs = ReturnsGeneratorFUNArgs), carryAppend = TRUE)

  # xTs
  return(releaseMktData(xTs, InBndxTs, isInBndxTsMktData))

})}








#' Symbol weights using eyeball
#'
#' @description
#' \preformatted{
#'
#' This is the workhorse function. This is where the magic/logic happens.
#' Use any other columns (called indicators) that do not have the weights (_wts)
#' suffix and do not have the same root name compared to each and every
#' other *_wts column.
#'
#' }
#'
#' @param xTs xts object
#' @param Symbol name prefix given to the generated output columns
#' @return xts object with merged data into xTs
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_c str_replace
symbolEyeBallWts <- function(xTs = NULL, Symbol = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  xTs <- initXts(xTs)
  if(!is.null(xtsAttributes(xTs)[["rettarget"]])) {
    LeadingRetColName <- xtsAttributes(xTs)[["rettarget"]]
  } else {
    stop("symbolEyeBallWts needs xTs to have the attribute: rettarget")
  }

  if(!"UNRATE" %in% colnames(xTs)) stop("symbolEyeBallWts needs UNRATE")

  if(is.null(Symbol)) stop("symbolEyeBallWts needs a Symbol to give names to the output columns")

  unrate <- xTs[,"UNRATE"]
  # can't do math on leading NAs
  unrate <- unrate[!is.na(unrate),]

  # unrateleadingrets_wts <- ifelse( ((SMA(unrate,2)        - SMA(    unrate   ,6)) <= 0)              |
  #                                  ((SMA(lag(unrate),2)   - SMA(lag(unrate  ),6)) <= 0)              |
  #                                  ((SMA(lag(unrate,2),2) - SMA(lag(unrate,2),6)) <= 0), 1.00, 0.00)

  unrate1Indicator <- Less(SMA(    unrate   ,2), SMA(    unrate   ,6))
  colnames(unrate1Indicator) <- "unrate1"
  unrate2Indicator <- Less(SMA(lag(unrate)  ,2), SMA(lag(unrate  ),6))
  colnames(unrate2Indicator) <- "unrate2"
  unrate3Indicator <- Less(SMA(lag(unrate,2),2), SMA(lag(unrate,2),6))
  colnames(unrate3Indicator) <- "unrate3"

  unrateleadingrets_wts <-   ifelse( (unrate1Indicator <= 0)              |
                                     (unrate2Indicator <= 0)              |
                                     (unrate3Indicator <= 0), 1.00, 0.00)

  unrateleadingrets_wts[is.na(unrateleadingrets_wts)] <- 1 # 100% allocated

  LeadingRetWtsColName <-  stringr::str_c(LeadingRetColName, "_wts")
  colnames(unrateleadingrets_wts)[1] <- LeadingRetWtsColName

  message(stringr::str_c("End   buildModel - ", LeadingRetWtsColName, " ~ unrate1 + unrate2 + unrate3"))

  unratecurrentrets_wts <- lag.xts(unrateleadingrets_wts)

  CurrentRetWtsColName <-  stringr::str_replace(LeadingRetWtsColName, "leading", "current")
  colnames(unratecurrentrets_wts)[1] <- CurrentRetWtsColName

  unrate_wts <- combineXts(unrateleadingrets_wts, unratecurrentrets_wts)

  Fitted <- unrateleadingrets_wts
  colnames(Fitted) <- "Fitted"

  return(combineXts(unrate_wts, Fitted))
  # unrate_wts

})}



#' add Symbol Change weights returns using eyeball (SYMBOLCHANGErets)
#'
#' @description
#' \preformatted{
#'
#' }
#'
#' @param xTs xts object
#' @param Symbol See ? symbolEyeBallWts
#' @return xts object with merged data into xTs
#' @export
#' @importFrom tryCatchLog tryCatchLog
addSymbolEyeBallWts <- function(xTs = NULL, Symbol = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  InBndxTs <- as.character(substitute(xTs))
  initMktData(xTs, InBndxTs)
  # xTs  <- initXts(xTs)

  xTs <- combineXts(xTs, symbolEyeBallWts(xTs, Symbol = Symbol))

  # xTs
  return(releaseMktData(xTs, InBndxTs, isInBndxTsMktData))

})}



#' add Symbol Change weights using Machine learning
#'
#' @param xTs xts object
#' @param Predictee string of the name of the Predictee variable
#' @param Predictors characters vector of the require columns needed in the
#' indictor generator function: IndicatorGeneratorFUN
#' @param IndicatorGeneratorFUN string of the name of the function
#' or the function itself that generates more columns (to eventually
#' be used in the model.)
#' @param NumbReplicaCopiesMultiple passed to NumbReplicaCopies
#' This means how much more ( e.g. "focused" data ) is replicated
#' compared to "all" data.
#' @param  ModelTarget Exact column name that I am (eventually) trying
#' to 'financially' optimize
#' @param ... dots passed to the indicator generator function
#' @return xts object
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_c
symbolMachineWts <- function(xTs = NULL, Predictee = NULL, Predictors = NULL, IndicatorGeneratorFUN = NULL
                                    , NumbReplicaCopiesMultiple = NULL
                                    , ModelTarget = NULL
                                    , ...) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  Dots <- list(...)

  if(is.null(Predictee))             stop("symbolMachineWts needs a Predictee")
  if(is.null(Predictors))            stop("symbolMachineWts needs Predictors")
  if(is.null(IndicatorGeneratorFUN)) stop("symbolMachineWts needs an IndicatorGeneratorFUN")

  if(is.null(Dots[["timeSliceMatrix"]]))
    Dots <- c(list(), Dots, list(timeSliceMatrix = tis::nberDates()))

  DescTools::DoCall(prepAndDoMachineWtsData, c(list(),
                    list(xTs), Predictee = Predictee
                             , list(Predictors = Predictors)
                             , list(IndicatorGeneratorFUN = IndicatorGeneratorFUN)
                             , ModelTarget = ModelTarget
                             , Dots
  ))

})}



#' add Symbol Change weights returns using Machine learning (SYMBOLCHANGErets)
#'
#' @description
#' \preformatted{
#'
#' }
#'
#' @param xTs xts object
#' @param Predictee objective. Legacy. Modern: should instead be passed by
#' the following; xtsAttributes(mktdata)$rettarget.
#' @param Predictors Required fields. Passed to the 'weight determining function'
#' @param IndicatorGeneratorFUN Function that determines the wieghts.
#' Passed to 'weight determining function'.
#' @param NumbReplicaCopiesMultiple  Per each timeslice era.
#' Adjusts the number of copies of the focused area.
#' @param  ModelTarget Exact column name that I am (eventually) trying
#' to 'financially' optimize
#' @return xts object with merged data into xTs
#' @export
addSymbolMachineWts <- function(xTs = NULL, Predictee = NULL, Predictors = NULL, IndicatorGeneratorFUN = NULL
                                    , NumbReplicaCopiesMultiple = NULL
                                    , ModelTarget = NULL
                                    , ...) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  InBndxTs <- as.character(substitute(xTs))
  initMktData(xTs, InBndxTs)
  # xTs  <- initXts(xTs)

  if(is.null(Predictee) && !is.null(xtsAttributes(xTs)[["rettarget"]])) {
    Predictee <- xtsAttributes(xTs)[["rettarget"]]
  }

  symbolMachineWts(xTs, Predictee = Predictee, Predictors = Predictors
                 , IndicatorGeneratorFUN = IndicatorGeneratorFUN
                 , NumbReplicaCopiesMultiple = NumbReplicaCopiesMultiple
                 , ModelTarget = ModelTarget
                 , ...) -> xTs1


  xTs <- combineXts(xTs, xTs1)

  # xTs
  return(releaseMktData(xTs, InBndxTs, isInBndxTsMktData))

})}





#' collection of SMAs of the unrate Eyeball Indicator
#'
#' @description
#' \preformatted{
#'
#' }
#'
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom zoo na.trim
unrateEyeballIndicators <- function(unrate = NULL, ...) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  unrate <- initXts(unrate)

  # can not do math on leading NAs
  # (actually can not do any math on 'any' NAs)
  unrate <- zoo::na.trim(unrate)

  unrate1Indicator <- Less(SMA(    unrate   ,2), SMA(    unrate   ,6))
  colnames(unrate1Indicator) <- "unrate1"
  unrate2Indicator <- Less(SMA(lag(unrate)  ,2), SMA(lag(unrate  ),6))
  colnames(unrate2Indicator) <- "unrate2"
  unrate3Indicator <- Less(SMA(lag(unrate,2),2), SMA(lag(unrate,2),6))
  colnames(unrate3Indicator) <- "unrate3"

  merge(unrate1Indicator, unrate2Indicator, unrate3Indicator)

})}



#' collection of curves of the trends Indicator using average order
#'
#' @description
#' \preformatted{
#'
#' }
#' @param x xts object
#' @rdname trendsWithAOMIndicators
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom zoo na.trim
trendsWithAOMIndicators <- function(x = NULL, ...) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  x <- initXts(x)
  Dots <- list(...)

  if("UseAOMX" %in% names(Dots)) {
    UseAOMX <- Dots[["UseAOMX"]]
  } else {
    UseAOMX <- FALSE
  }
  if("UseAOMN" %in% names(Dots)) {
    UseAOMN <- Dots[["UseAOMN"]]
  } else {
    UseAOMN <- FALSE
  }

  # can not do math on leading NAs
  # (actually can not do any math on 'any' NAs)
  x <- zoo::na.trim(x)

  # xTs <- trends(x, col = "UNRATE", UseAOMX = TRUE)
  # 'col' WOULD HAVE already BEEN filtered by the caller of trendsWithAOMIndicators
  xTs <- trends(x, rep(TRUE, NVAR(x)), UseAOMX = UseAOMX, UseAOMN = UseAOMN)

  return(xTs)

})}
#' collection of curves of the trends Indicator using average order max
#'
#' @rdname trendsWithAOMIndicators
#' @export
trendsWithAOMXIndicators <- function(x = NULL, ...) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})
  trendsWithAOMIndicators(x = x, UseAOMX = TRUE, ...)
})}

#' collection of curves of the trends Indicator using average order min
#'
#' @rdname trendsWithAOMIndicators
#' @export
trendsWithAOMNIndicators <- function(x = NULL, ...) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})
  trendsWithAOMIndicators(x = x, UseAOMN = TRUE, ...)
})}


#' True Sortino Ratio "summary function"
#'
#' @description
#' \preformatted{
#'
#' for package caret trainControl parameter summaryFunction
#'
#' }
#'
#' @export
#' @importFrom PerformanceAnalytics SortinoRatio
SortinoRatioSummary <- function (data, lev = NULL, model = NULL) {

  R <- data[,"obs"] - data[,"pred"]
  # numeric vector of size NROW(data)
  #
  outOrig <- PerformanceAnalytics::SortinoRatio(R = R, MAR = 0)
  # marix if size 1x1
  #
  dimnames(outOrig) <- NULL
  outOrig <- as.vector(outOrig)
  out <- outOrig
  # numeric vector of size 1

  # attempt to extremize the results
  # (For now, just use) linear loss functions
  # ranges
  StrengthAboveMAR <- 1
  if(0 <= outOrig) out <- out * StrengthAboveMAR
  #
  # shut off multiply
  # (currently): does not seem to have a noticable effect
  # having the "right" data is !more important!
  #
  # StrengthBelowMAR <- 4
  StrengthBelowMAR <- 1
  if(outOrig < 0)  out <- out * StrengthBelowMAR

  names(out)[1] <- "ratio"
  return(out)

}



#' True Sortino Ratio 'loss function'
#'
#' @description
#' \preformatted{
#'
#' for iml FeatureImp$new
#'
#' }
#'
#' @param see ? iml::FeatureImp
#' @param see ? iml::FeatureImp
#' @param ... lev = NULL, model = NULL : passed to SortinoRatioSummary
#' @export
SortinoRatioLoss <- function (actual, predicted, ...) {

  SortinoRatioSummary(data = data.frame(obs = actual, pred = predicted, ...))

}



#' relative scaling
#'
#' @description
#' \preformatted{
#'
#' }
#'
#' @param x anything with methods for
#' Ops: "+","-","/", "*" and possibly methods for min/max
#' @param ... passed to DMwR ReScaling. Do not pass: x, t.mn, and t.mx.
#' @return re-scaled from low:mim/max*100' to max:100
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom DMwR ReScaling
relativeScale <- function(x, ...) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})
  DMwR::ReScaling(x, t.mn=min(x)/max(x)*100, t.mx=100, ...)
})}



#' relative scaling of 2 dimensions
#'
#' @description
#' \preformatted{
#'
#' }
#'
#' @param 2 dim object data.frame or two=dim non-matrix, non-array
#' with methods for
#' Ops: "+","-","/", "*" and possibly methods for min/max
#' @param cols columns to rescale
#' @param ... passed to DMwR ReScaling. Do not pass: x, t.mn, and t.mx.
#' @return re-scaled from low:mim/max*100' to max:100
#' @export
#' @importFrom tryCatchLog tryCatchLog
relativeScaleImportance <-function(x, cols = c("importance"), ... ) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  if(length(dim(x)) > 2) stop("relativeScaleImportance given x with too high dimensions")

  if(length(dim(x)) == 2) {
    for(col in cols){
      xi <- x[[col]]
      xs <- relativeScale(xi, ...)
      x[[col]] <- xs
    }
    res <- x
  } else {
    res <- relativeScale(x, ...)
  }
  return(res)
})}



#' formula.tools private as.character.formula
#'
#' @description
#' \preformatted{
#'
#' }
#'
#' @export
formula_tools___as_character_formula <- function (x, ...) {
    form <- paste(deparse(x), collapse = " ")
    form <- gsub("\\s+", " ", form, perl = FALSE)
    return(form)
}



#' column split an xts object into an environment of column-xts objects
#'
#' @description
#' \preformatted{
#'
#' uses xts private S3 function as.data.frame.xts
#'
#' }
#'
#' @param xTs xts object
#' @export
#' @importFrom plyr llply
#' @importFrom tryCatchLog tryCatchLog
xTsCols2SymbolsEnv <- function(xTs) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  # IF REMOVE ".fun = " then RStudio can debug
  # create an environment of xts objects
  plyr::llply(rlist::list.zip(xTs = as.data.frame(xTs), ColName = colnames(xTs)),
    function(x) {
      xx <- as.xts(x[["xTs"]], order.by = index(xTs))
      colnames(xx)[1] <- x[["ColName"]]
      # (+) non-core attributes (user) [if any]
      xtsAttributes(xx) <- xtsAttributes(xTs)
      xx
    }
  ) -> SymbolsOrig

  # reorders in alphabetical order
  Symbols <- list2env(SymbolsOrig)
  Symbols

})}


#' model weights
#'
#' @description
#' \preformatted{
#'
#' Using xgboost weights
#' (using objective(y hieght) value to determine 'how much I care'(weights))
#' the weights are then simply multiplied by the classification error
#' at each iteration of the learning process.
#'
#' Gradient boosting machines, a tutorial
#' Front Neurorobot. 2013; 7: 21.
#' Published online 2013 Dec 4. doi:  10.3389/fnbot.2013.00021
#' PMCID: PMC3885826l
#' http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3885826/
#'
#' }
#'
#' @param xTs data
#' @param target the column being predicted
#' @param TrainStart absolute training start date
#' @param TrainEnd absolute training end date
#' @param weightings passed to Hmisc::wtd.quantile weights
#' @param probs passed to Hmisc::wtd.quantile probs.
#' Default is seven(7) intervals of probs = c(0.00, 0.01, 0.10, 0.25, 0.75, 0.90, 0.99, 1.00).
#' @param CaseAdj new (re)values for values of the intervals 7:1 (from probs).
#' Must be passed to dplyr::case_when as a quoted list of formulas.
#' Passed as formula elements:  BareWeightRankings == <oldvalue> ~ <newvalue>.
#' See the default (in the code: tradeModel::AdjustedWeightRankings ).
#' @export
#' @importFrom Hmisc wtd.quantile
#' @importFrom dplyr case_when
#' @importFrom DescTools DoCall
#' @importFrom tryCatchLog tryCatchLog
weightRankings <- function(xTs, target, TrainStart, TrainEnd, weightings = NULL, probs = NULL, CaseAdj = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  Objectives <- as.vector(coredata(window(xTs, start = TrainStart, end = TrainEnd))[, target])
  # lower values have lower rank numbers
  if(is.null(probs)) {
    probs <- c(0.00, 0.01, 0.10, 0.25, 0.75, 0.90, 0.99, 1.00)
  } else {
    probs <- probs
  }
  findInterval(x =  Objectives,
    # SEE MY NOTES: CURRENTLY REVIEWING different weights determiners
    # https://cran.r-project.org/web/packages/freqweights/freqweights.pdf
    vec = Hmisc::wtd.quantile(
      x = Objectives,
      weights = if(is.null(weightings)) { rep(1, NROW(Objectives)) } else { weightings } , # SEE MY NOTES
      probs   = probs,
      na.rm = FALSE
    ),
  rightmost.closed = TRUE) %>%
    {.*(-1) } %>% { . + NROW(probs) } -> BareWeightRankings # (intervals(7)) values # { NROW(probs) - 1} # through 1
    # lower numbers 'now' have higher rank numbers
  if(is.null(CaseAdj)) {
    CaseAdj <- quote(list(
      BareWeightRankings == 7 ~ 30, # from 7  1%
      BareWeightRankings == 6 ~ 15, # from 6 10%
      BareWeightRankings == 5 ~  8, # from 5 25%
      TRUE                    ~ BareWeightRankings
    ))
  } else {
    CaseAdj <- CaseAdj
  }
  CaseAdj <- eval(CaseAdj)
  DescTools::DoCall(dplyr::case_when, CaseAdj) -> AdjustedWeightRankings
  # to be sent as to buildModel.train, as
  # weights = AdjustedWeightRankings
  # if the model is xgboost [xgbTree], then it does USE it
  AdjustedWeightRankings

})}



#' Rob Hyndman style time slices
#'
#' @description
#' \preformatted{
#'
#' Create time slice indexes to be passed to caret trainControl index
#' and indexOut. My visual observation is that this model
#' DOES \*worse\* than general cross validation.
#' This is because, per time slice, less observations
#' exist to TRAIN/TEST over.
#'
#' }
#'
#' @param xTs data
#' @param SlicesAllData list of index slice date vectors
#' @param TrainStart absolute training start date
#' @param TrainEnd absolute training end date
#' @return list of indexSlicesObs and indexSlicesOutObs position indexes
#' meant to be passed to caret::trainControl index and indexOut
#' @examples
#' \dontrun{
#' # determine slices of index and indexOut
#' indexSlices(
#'     # data.window = c(TrainingBegin, TrainingEnd)
#'     # Update currently specified or built model with most recent data
#'     # remove the last record(NO) (na.rm = FALSE)
#'     # "2007-01-31" (actual "2001-12-31")
#'     xTs = modelData(specifiedUnrateModel, exclude.training = TRUE) # not defined yet
#'   , SlicesAllData = NBERAllData
#'   , TrainStart = TrainingBegin
#'   , TrainEnd   = TrainingEnd
#' ) -> Sliced
#' # determine slices of index and indexOut to caret trainControl
#' # pass through
#' indexSlicesObs    <- Sliced[["indexSlicesObs"]]
#' indexSlicesOutObs <- Sliced[["indexSlicesOutObs"]]
#' }
#' @export
#' @importFrom plyr llply
#' @importFrom tryCatchLog tryCatchLog
indexSlices <- function(xTs, SlicesAllData, TrainStart, TrainEnd) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

                                                        # any UBL (or OTHER) functions that could have
                                                        # crept-in/created new observations that exist OUT-of-RANGE
                                                        # GARANTEED TO BE WITHIN c(TrainingBegin, TrainingEnd)
                              # ONLY NBER DATE RANGES                      # Re-defining Training* to be more date-restricte
  AllDataSliceTimeRanges <- plyr::llply(SlicesAllData, function(x)  c(start= max(head(x,1),TrainStart), end = min(TrainEnd,tail(x,1))) )
  # should be min(earliest)
  FirstLoop <- TRUE
  for(i in seq_along(AllDataSliceTimeRanges)) {
    if(FirstLoop) {
      TrainStart  <- AllDataSliceTimeRanges[[i]][["start"]]
      FirstLoop <- FALSE
    } else {
      TrainStart < min(TrainStart, AllDataSliceTimeRanges[[i]][["start"]])
    }
  }
  # should be max(latest)
  FirstLoop <- TRUE
  for(i in rev(seq_along(AllDataSliceTimeRanges))) {
    if(FirstLoop) {
      TrainEnd  <- AllDataSliceTimeRanges[[i]][["end"]]
      FirstLoop <- FALSE
    } else {
      TrainEnd < max(TrainStart, AllDataSliceTimeRanges[[i]][["end"]])
    }
  }

  Data <- xTs
   # determine timeSlices
  DataWobsid <- cbind(Data, obsid = seq_len(NROW(Data)))

  indexSlicesObs <- plyr::llply(AllDataSliceTimeRanges, function(x) { as.integer(coredata(window(DataWobsid, start = x[["start"]], end = x[["end"]])[,"obsid"])) })
  indexSlicesObsLastIndex <- length(indexSlicesObs)
  indexSlicesOutObs <- list()
  for(i in seq_along(indexSlicesObs)) {
    if(i != indexSlicesObsLastIndex) {
      indexSlicesOutObs[i] <- indexSlicesObs[i+1]
    } else {
      indexSlicesOutObs[i] <- indexSlicesObs[1] # gets the first set,
    }                                           # otherwise, if I choose the 4th set
  }                                             # then it(4th set) would be tested TWICE (and I do not want that)

  return(list(indexSlicesObs = indexSlicesObs, indexSlicesOutObs = indexSlicesOutObs))
})}



#' give more observations to the focused data
#'
#' @description
#' \preformatted{
#'
#' Produce more focused data duplicate observations
#' such that "focused data" is at least the same number of observations
#' as "all observations less focused observations"
#'
#' }
#'
#' @param xTs xts object of training data
#' @param NumbReplicaCopiesMultiple NULL(default) How much "more" to to
#' overbalance the "focused data" compareed to its corresponding "all data"
#' One(1) is the same as NULL(default).
#' Two(2) or any other fraction, e.g. 2.5 means multiply the internally derived
#' "number of replica copies" by this value.
#' @param SlicesAllData list of index slice date vectors
#' @param SlicesFocusedData list of index slice date vectors
#' @param TrainStart absolute training start date
#' @param TrainEnd absolute training end date
#' @export
#' @importFrom tryCatchLog tryCatchLog
balanceFocusedData <- function(
    xTs
  , NumbReplicaCopiesMultiple = NULL
  , SlicesFocusedData
  , SlicesAllData
  , TrainStart
  , TrainEnd
) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  if(is.null(NumbReplicaCopiesMultiple)) NumbReplicaCopiesMultiple <- 1.0

  if(length(SlicesAllData) != length(SlicesFocusedData)) {
    stop("In balanceFocusedData: need length(SlicesAllData) == length(SlicesFocusedData)")
  }
  NumbSlices <- length(SlicesFocusedData)
  # ANDRE balancing

  # determine the Focused timeslices to (replicate)copy
  Data <- xTs
  TrainingData <- window(Data, start = TrainStart, end = TrainEnd)
  for(slice in seq_len(NumbSlices)) {
    # x-num-ish it:(bind a 2nd,3rd,4th copy)
    # Torgo new 2018, 2017,2018 slides
    # To balance the data: how many replica copies do I need?
    # ANDRE DECISION
    # copy over enough (or more) so that the
    # SlicesFocusedData and and SlicesAllData numbers of records are balanced
    NumbReplicaCopies <- ceiling((length(SlicesAllData[[slice]]) - length(SlicesFocusedData[[slice]]))/length(SlicesFocusedData[[slice]]))
    NumbReplicaCopies <- NumbReplicaCopiesMultiple * NumbReplicaCopies
    FocusedDataOrigSliceData <- Data[SlicesFocusedData[[slice]]]
    for(copy in seq_len(NumbReplicaCopies)) {
      TrainingData <- rbind(TrainingData, FocusedDataOrigSliceData)
    }
  }
  # add back validation area data
  AdjData <- rbind(TrainingData, xTs[!index(xTs) %in% index(TrainingData), ])
  AdjData

})}




#' create/remove more or less observations determined by an UBL function
#'
#' @description
#' \preformatted{
#'
#' Produce more or less duplicate/near observations.
#'
#' }
#'
#' @param xTs xts object of training data
#' @param Fmla formula that is sent to the UBL function
#' @param TrainStart absolute training start date
#' @param TrainEnd absolute training end date
#' @param UBLFunction package UBL *Regress function ( default is ImpSampRegress ),
#' entered as enclosed in a "string" or bare
#' @param ... passed to the UBL function
#' if the UBLFunction is ImpSampRegress, then defaults are the following:
#' thr.rel = 0.5,  C.perc = list(1.0, 2.5),
#' rel: values less than zero are important, values greater than zero are not important
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_c
#' @importFrom UBL ImpSampRegress SmoteRegress RandOverRegress RandUnderRegress UtilOptimRegress
#' @importFrom DescTools DoCall
balanceByUBLData <- function(
    xTs
  , Fmla
  , TrainStart = NULL
  , TrainEnd = NULL
  , UBLFunction = NULL
  , ...
) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  Data <- xTs
  TrainingData <- window(Data, start = TrainStart, end = TrainEnd)
  # utility based learning
  UBLData <- cbind(as.data.frame(TrainingData), index = as.POSIXct(index(TrainingData)))
  row.names(UBLData) <- NULL
  UBLDataCompleteCases <- UBLData[complete.cases(UBLData),,drop = FALSE]

                                              # formula.tools:::as.character.formula
  UBLDataFormula <- as.formula(stringr::str_c(formula_tools___as_character_formula(Fmla), " + index")) # need the index to COPY

  if(!is.null(UBLFunction)) {
    if(mode(UBLFunction) == "function") {
      UBLFunction = match.fun(UBLFunction)
    } else {
      UBLFunction <- UBLFunction
    }
  } else {
    UBLFunction <- UBL::ImpSampRegress
  }

  Dots <- list(...)
  if("dat"   %in% names(Dots))
    DoData <- list(dat   = UBLDataCompleteCases)
  # UBL::UtilOptimRegress
  if("train" %in% names(Dots))
    DoData <- list(train = UBLDataCompleteCases)

  # values lhs of formula with values LESS than zero are MORE relevant (financial losses)
  #
  #
  # UBL functions;  or 'create new observations'
  #                x-axis   y-axis
  #                 y-val,  rel(height), slope at height(y-axis)
  if(!"rel" %in% names(Dots)){
    rel <- matrix(c(
                       -0.01, 1.0, 0.0, # negative y-values ( I care *much* about )
                        0.00, 0.5, 0.5,
                        0.01, 0.0, 0.0  # positive y-values ( I do not care *much* about )
                        )
                 , ncol = 3
                 , byrow = TRUE)
  } else {
    rel <- Dots[["rel"]]
  }
  if(!"thr.rel" %in% names(Dots)) {
    thr.rel = 0.5
  } else {
    thr.rel <- Dots[["thr.rel"]]
  }
  if(!"C.perc" %in% names(Dots)) {
    # I want new 150% percent MORE "financial loss data"
    # Keeping all of the financial profits
    # C.perc = list(1.0, 2.5))
    C.perc = list(1.0, 2.5)
  } else {
    C.perc <- Dots[["C.perc"]]
  }

  # UBL::ImpSampRegress: The relevance function is used to introduce
  # replicas of the most important examples and to remove the least important examples.
  # ? UBL::ImpSampRegress
  if(identical(UBLFunction, UBL::ImpSampRegress)) {
    # WERCS: WEighted Relevance-based Combination Strategy

    # filter out "junk" found in Dots
    NamesArgs <- Names(as.list(args(UBL::ImpSampRegress)))
    NamesArgs <- NamesArgs[NamesArgs != ""]

    UBLResults <- DescTools::DoCall(UBL::ImpSampRegress
      , c(list(), form = UBLDataFormula, dat = list(UBLDataCompleteCases)
      , rel = list(rel), thr.rel = thr.rel,  C.perc = list(C.perc)
      , Dots[Names(Dots) %in% setdiff(NamesArgs,  c("form","dat","train","rel","thr.rel","C.perc"))])
    )
    # I AM ending up LOOSING some 'UBLDataCompleteCases' data. WHY?
    # NOTE: no NEW index Values are created.
    # I CAN NOT garantee that all UBL functons do NOT do that
  } else {
    # anything else

    # filter out "junk" found in Dots
    NamesArgs <- Names(as.list(args(UBLFunction)))
    NamesArgs <- NamesArgs[NamesArgs != ""]

    UBLResults <- DescTools::DoCall(UBLFunction
      , c(list(), form = UBLDataFormula, DoData
      , Dots[Names(Dots) %in% setdiff(NamesArgs,  c("form","dat","train"))] ) )
  }

  UBLResultsIndex <- UBLResults[["index"]]
  UBLResults      <- UBLResults[, !colnames(UBLResults) %in% "index" , drop = FALSE]

  # redefine
  TrainingData <- as.xts(as.matrix(UBLResults), order.by = UBLResultsIndex)
  indexClass(TrainingData)  <- indexClass(xTs)
  indexFormat(TrainingData) <- indexFormat(xTs)
  # (+) non-core attributes (user) [if any]
  xtsAttributes(TrainingData) <- xtsAttributes(xTs)

  #
  # UBL functions will not leak data
  #   EXCEPT *GaussNoiseRegression* that can/will leak data.
  #
  # question about new/replicated UBL data and range of creation area #3
  # https://github.com/paobranco/UBL/issues/3
  #
  # prevent any leaking of 'new' [if any] UBL data into the validation area
  TrainingData <- window(TrainingData, start = TrainStart, end = TrainEnd)
  # add back validation area data
  AdjData <- rbind(TrainingData, xTs[!index(xTs) %in% index(TrainingData),])
  AdjData

})}



#' low-level set the values quantmod object slots
#'
#' @description
#' \preformatted{
#'
#' methods/html/slot.html
#'
#' }
#'
#' @param x quantmod object
#' @param ... list of name-value pairs
#' @export
`modelData<-` <- function(x, ...) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  # LATER: FIX [ ]:MAYBE CHECK FOR A quantmod object
  # LATER: FIX [ ]:MAYBE CHECK FOR A valid slot name
  Dots <-list(...)$value
  for(i in names(Dots)){
    slot(x, i) <- Dots[[i]]
  }
  x
})}










#' prep Machine weights data then call doMachineWts
#'
#' @param xTs xts object
#' @param ModelFormula NULL(default) "formula" (IF KNOWN in advance) to be based on
#' the Predictee string and the columns created by the IndicatorGeneratorFUN function.
#' If left NULL, then the formula be generated by the string Predictee and the column names returned
#' by after running the IndicatorGeneratorFUN function.
#' @param Predictee string of the name of the Predictee variable
#' @param Predictors characters vector of the require columns needed in the
#' indictor generator function: IndicatorGeneratorFUN
#' @param IndicatorGeneratorFUN string of the name of the function
#' or the function itself that generates more columns (to eventually
#' be used in the model.)
#' @param  ModelTarget Exact column name that I am (eventually) trying
#' to 'financially' optimize. xtsAttributes(xTs)[["rettarget"]](Default)
#' @param ExtremePct passed in dots to function doOneChoice
#' @param ... dots passed to the indicator generator function IndicatorGeneratorFUN(dots)
#' and to function doMachineWts(dots)
#' @return xts object
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_c
prepAndDoMachineWtsData <- function(xTs = NULL,  ModelFormula = NULL, Predictee = NULL
                                    , Predictors = NULL, IndicatorGeneratorFUN = NULL
                                    , ModelTarget = NULL
                                    , ...) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  xTs <- initXts(xTs)
  Dots <- list(...)

  if(is.null(Predictee) || (length(Predictee) > 1)) stop("prepAndDoMachineWtsData needs only ONE Predictee")
  if(is.null(Predictors)) stop("prepAndDoMachineWtsData needs some Predictors")
  if(is.null(IndicatorGeneratorFUN)) stop("prepAndDoMachineWtsData needs an IndicatorGeneratorFUN function")

  if(is.null(ModelTarget)) {
    ModelTarget_wts <- stringr::str_c(xtsAttributes(xTs)[["rettarget"]], "_wts")
  } else {
    # literally written derivation
    ModelTarget_wts <- stringr::str_c(ModelTarget, "_wts")
  }

  # if(mode(IndicatorGeneratorFUN) == "function") {
  #   IndicatorGeneratorFUN = match.fun(IndicatorGeneratorFUN)
  # } else {
  #   # character function
  #   IndicatorGeneratorFUN <- get(IndicatorGeneratorFUN)
  # }

  if(length(IndicatorGeneratorFUN) > 1) {
    IndicatorGeneratorFUN <- as.list(IndicatorGeneratorFUN)
    IndicatorGeneratorFUNLIST <- list()
    for(IndicatorGeneratorFUN1 in IndicatorGeneratorFUN) {
      if(mode(IndicatorGeneratorFUN1) == "function") {
        IndicatorGeneratorFUN1 = match.fun(IndicatorGeneratorFUN1)
      } else {
        # character function
        IndicatorGeneratorFUN1 <- get(IndicatorGeneratorFUN1)
      }
      # list of elements
      IndicatorGeneratorFUNLIST <- c(list(), IndicatorGeneratorFUNLIST, IndicatorGeneratorFUN1)
    }
    IndicatorGeneratorFUN <- IndicatorGeneratorFUNLIST
  } else {
    if(mode(IndicatorGeneratorFUN) == "function") {
      IndicatorGeneratorFUN = match.fun(IndicatorGeneratorFUN)
    } else {
      # character function
      IndicatorGeneratorFUN <- get(IndicatorGeneratorFUN)
    }
    # single element vector
  }


  # REM:ONE column of "valueLeadingRetsClms(xTs)" is what I am trying to predict
  InitialMachineWtsData <- xTs[ , c(valueLeadingRetsClms(xTs), valuePredictorClms(xTs))]

  if(!all(Predictors %in% colnames(InitialMachineWtsData))) {
    stop(stringr::str_c("prepAndDoMachineWtsData is missing Predictors:", stringr::str_c(Predictors[!Predictors %in% colnames(InitialMachineWtsData)], collapse = ", "), collapse = " "))
  }
  if(!Predictee %in% colnames(InitialMachineWtsData))
    stop(stringr::str_c("prepAndDoMachineWtsData needs Predictee: ", Predictee, collapse = " "))

  #               # unrateEyeballIndicators
  # Indicators <- IndicatorGeneratorFUN(InitialMachineWtsData[, Predictors], ...)

  if(length(Predictors) > 1) {
    Predictors <- as.list(Predictors)
    IndicatorsLIST <- list()
    Predictors_iter <- 0L
    for(Predictor in Predictors) {
      Predictors_iter <- Predictors_iter + 1L
      Indicator <-  IndicatorGeneratorFUN[[Predictors_iter]](InitialMachineWtsData[, Predictor], ...)
      # list of vectors
      # I did not bother to name each xts object(Indicator) (To do that I could have used Predictor)
      IndicatorsLIST <- c(list(), IndicatorsLIST, list(Indicator))
    }
    Indicators <- IndicatorsLIST
  } else {
    # xts columns
    Indicators <- IndicatorGeneratorFUN(InitialMachineWtsData[, Predictors], ...)
  }
  if(is.list(Indicators)) {Indicators <-  DescTools::DoCall(merge, Indicators) }

  ColNamesIndicators <- colnames(Indicators)
  # all + indicators
  xTs <- merge(xTs, Indicators)

  addedDummyColumn <- FALSE
  if(length(ColNamesIndicators) == 1) {
    # some models e.g. xgboost need at least two(2) indicators (predictors)
    xTs <- cbind(xTs, DUMMY = rep(0L, NROW(xTs)))
    addedDummyColumn <- TRUE
    ColNamesIndicators <- c(ColNamesIndicators, "DUMMY")
  }

  # traditionally the first column is the target variable # OLD: colnames(xTs)[1]
                                             # no longer match by positions

  if(is.null(ModelFormula)) {
    # ModelFormula <- as.formula(stringr::str_c(Predictee, " ~ ", stringr::str_c(colnames(Indicators), collapse = " + ")))
    ModelFormula <- as.formula(stringr::str_c(Predictee, " ~ ", stringr::str_c(ColNamesIndicators, collapse = " + ")))
  } else {
    # if the user arleady sen the ModelFormula
    # and if I added the DUMMY column, then just add it to the formula
    if(addedDummyColumn) ModelFormula <- stringr::str_c(ModelFormula, " + DUMMY")
  }

  # fitting
  Fitted <- doMachineWts(xTs, ModelFormula = ModelFormula, ...)

  # NO! Now, detected_in/set_using xtsAttributes(xTs/mktdata)[["rettarget"]] SEE ABOVE
  # ModelTarget_wts <-  stringr::str_c(formula.tools::lhs.vars(ModelFormula), "_wts")

  # deciding
  RetFitted <- doOneChoice(Fitted, ModelTarget_wts = ModelTarget_wts, ExtremePct = Dots[["ExtremePct"]])

  # return(RetFitted)
  return(combineXts(RetFitted, Fitted))

})}



#' determine weights using Machine learning
#'
#' @description
#' \preformatted{
#'
#' This is the workhorse function. This is where the magic/logic happens.
#' Use any other columns (called indicators) that do not have the weights (_wts)
#' suffix and do not have the same root name compared to each and every
#' other *_wts column.
#'
#' }
#'
#' @param xTs xts object
#' @param timeSliceMatrix NULL(default) passed through dots.
#' Input format is the same as "tis::nberDates()". See. ? tis::nberDates.
#' @param NumbReplicaCopiesMultiple in dots passed to NumbReplicaCopies
#' This means how much more ( e.g. "focused" data ) is replicated
#' compared to "all" data.
#' @param ... dots passed to further methods balanceFocusedData(parameters),
#' balanceByUBLData(dots), weightRankings(parameters)
#' @return xts object with merged data into xTs
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom zoo na.trim
#' @importFrom logging loginfo
#' @importFrom tis nberDates
#' @importFrom plyr llply
#' @importFrom stringr str_c
#' @importFrom dplyr arrange
#' @importFrom plyr llply
#' @importFrom DescTools DoCall
#' @importFrom UBL ImpSampRegress
#' @importFrom iml Predictor FeatureImp Interaction
#' @importFrom rlist list.zip
#' @importFrom caret trainControl
#' @importFrom formula.tools lhs.vars
#' @importFrom quantmod modelData
doMachineWts <- function(xTs = NULL,  ModelFormula = NULL, ...) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  Dots <- list(...)

  logging::loginfo("Begin: doMachineWts")

  xTs <- initXts(xTs)

  if(!complete.cases(xTs)[1]) {
    message("NOTE: in doMachineWts, xTs is missing some EARLY data.")
    message(stringr::str_c("OLD Earliest xTs index observation index time-date: ", min(index(xTs))))
    message("NOTE: NOW applying 'zoo::na.trim(xTs, sides = \"left\")': *** BUT YOU PROBABLY *WANT* TO FIX THIS!! ***")
    message("NOTE: incomplete cases busts caret.train (but does not? bust raw xgboost?)")
    # PERHAPS some 'early' data does not exist'
    xTs <- zoo::na.trim(xTs, sides = "left")
    message(stringr::str_c("NEW Earliest xTs index observation index time-date: ", min(index(xTs))))
  }

  # ordered R environment of Symbols
  Symbols <- xTsCols2SymbolsEnv(xTs)

  if(is.null(ModelFormula)) ModelFormula <- formula(xTs)

  specifyModel(formula = ModelFormula
            , na.rm = FALSE, source.envir = Symbols) ->
              # remove the last record(NO)
  specifiedUnrateModel

  # I can only train, test, validate where I have 'model target' predictee values
  ModelTarget          <- formula.tools::lhs.vars((formula(specifiedUnrateModel)))
  ModelTargetFirstDate <- head(index(na.trim(xTs[,ModelTarget])),1)
  ModelTargetTrainTestFirstDate <- ModelTargetFirstDate

  # NOTE: [ ] FIX: SHOULD NOT USE xTs HERE
  ModelTargetLastDate <- tail(index(na.trim(xTs[,ModelTarget])),1)
  # Later, I want to validate, so I save researve some dates(2007+)
  ModelTargetTrainTestLastDate <- min(as.Date("2006-12-31"), ModelTargetLastDate)

  #                                             I do not have any Predictee information earlier than this
  #                                             HARD-CODED(I just know this)        Desired end "2006-12-31", but actual end is "2001-11-30"
  #                                             as.Date("1970-12-31")
  if(!is.null(Dots[["timeSliceMatrix"]])) {
    SlicesAllData     <- timeSlices(timeSliceMatrix = Dots[["timeSliceMatrix"]], allSlicesStart = ModelTargetTrainTestFirstDate, allSlicesEnd = ModelTargetTrainTestLastDate, LongTimeSlices = TRUE, OmitSliceFirstDate = TRUE)
    SlicesFocusedData <- timeSlices(timeSliceMatrix = Dots[["timeSliceMatrix"]], allSlicesStart = ModelTargetTrainTestFirstDate, allSlicesEnd = ModelTargetTrainTestLastDate,                        OmitSliceFirstDate = TRUE)
  } else {
    stop("doMachineWts Dots[[\"timeSliceMatrix\"]] is NULL. I do not know what to do!")
  }      #FIGURE out something LATER

  # should be min(earliest),max(latest) Date of (SlicesAllData,SlicesFocusedData)
  TrainingBegin <- min(head(SlicesAllData[[1]],1), head(SlicesFocusedData[[1]],1))
  TrainingEnd   <- max(tail(SlicesAllData[[length(SlicesAllData)]],1), tail(SlicesAllData[[length(SlicesFocusedData)]],1))

  # give a more balanced observation count to the focused data
  # with respect to the non-focused data
  balanceFocusedData(
      xTs               = modelData(specifiedUnrateModel) # exclude.training not built/defined yet
                                # This means how much more ( e.g. "focused" data ) is replicated
                                # compared to "all" data
    , NumbReplicaCopiesMultiple = Dots[["NumbReplicaCopiesMultiple"]]
    , SlicesAllData     = SlicesAllData
    , SlicesFocusedData = SlicesFocusedData
    , TrainStart        = TrainingBegin
    , TrainEnd          = TrainingEnd
  ) -> TrainingData
  modelData(specifiedUnrateModel) <- list(model.data = TrainingData)

  # of the obserations that fall below (a number)
  # give them MORE like-observations.
  balanceByUBLData(
      xTs  = modelData(specifiedUnrateModel) # exclude.training not built/defined yet
    , Fmla = formula(specifiedUnrateModel)
    , TrainStart        = TrainingBegin
    , TrainEnd          = TrainingEnd
    , ...
  ) -> TrainingData
  modelData(specifiedUnrateModel) <- list(model.data = TrainingData)

  weightRankings(
      xTs = modelData(specifiedUnrateModel) # exclude.training not built/defined yet
    , target = ModelTarget
    , TrainStart = TrainingBegin
    , TrainEnd   = TrainingEnd
    , weightings = Dots[["weightings"]]
    , probs = Dots[["probs"]]
    , CaseAdj = Dots[["CaseAdj"]]
    # , CaseAdj    = quote(list(
    #     BareWeightRankings == 7 ~ 30, # from 7  1%
    #     BareWeightRankings == 6 ~ 15, # from 6 10%
    #     BareWeightRankings == 5 ~  8, # from 5 25%
    #     TRUE                    ~ BareWeightRankings
    #   ))
  ) -> AdjustedWeightRankings

  # fitting

  if(!"indexSlicesObs" %in% ls())    indexSlicesObs    <- NULL
  if(!"indexSlicesOutObs" %in% ls()) indexSlicesOutObs <- NULL
  trControl  <- caret::trainControl(method = "cv", number = if(!is.null(indexSlicesObs)) { length(indexSlicesObs) } else { 5 },
                             index    = if(!is.null(indexSlicesObs))    { indexSlicesObs }    else { NULL },
                             indexOut = if(!is.null(indexSlicesOutObs)) { indexSlicesOutObs } else { NULL },
                             summaryFunction = SortinoRatioSummary # formals(caret::trainControl) # to put back non-NULL args
                             )

  if(!"AdjustedWeightRankings" %in% ls()) AdjustedWeightRankings <- NULL
                                                    # first/last dates that the "predictee" dates are available
                                                    # "1970-12-31","2006-12-31"(actual "2001-11-30")
  message(stringr::str_c("Begin buildModel - ", as.character(formula(specifiedUnrateModel))), "")
  builtUnrateModel <- buildModel(specifiedUnrateModel,
                                 method="train",
                                 training.per=c(TrainingBegin, TrainingEnd),
                                 trControl = trControl,
                                 stage = "Test", # alternate  # "Production" "Test"
                                 weights = if(!is.null(AdjustedWeightRankings)) { AdjustedWeightRankings } else { NULL }, # weights
                                 maximize = TRUE,
                                 metric = "ratio"
                                 )

  message("Chosen Model")
  print(builtUnrateModel)
  message("Model Variable Importance of the Chosen Model")
  print(relativeScale(varImp(builtUnrateModel@fitted.model, scale = FALSE)$imp))

  # what makes the most sense is to use the
  # original (non-'added(removed) records) train/test data
  predictor = iml::Predictor$new(builtUnrateModel@fitted.model,
    data = as.data.frame(xTs[do.call(c,SlicesAllData),  colnames(xTs) %in% builtUnrateModel@model.inputs], stringsAsFactor = FALSE),
    y =       c(coredata(xTs[do.call(c,SlicesAllData),  colnames(xTs) %in% builtUnrateModel@model.target]))
  )
  message("iml: Feature Importance; Shuffling each feature and measuring how much the performance drops")
  imp = iml::FeatureImp$new(predictor, loss = SortinoRatioLoss)
  print(relativeScaleImportance(imp$results))

  message("iml: Measure Interactions; measure how strongly features interact with each other;")
  message("how much of the variance of f(x) is explained by the interaction of that feature")
  message("with any other feature")
  interact = iml::Interaction$new(predictor)
  print(relativeScaleImportance(dplyr::arrange(interact$results, desc(.interaction)), cols = ".interaction"))

  message(stringr::str_c("End   buildModel - ", as.character(formula(specifiedUnrateModel))),"")

  # prediction

  # Update currently specified or built model with most recent data
  UpdatedModelData <- getModelData(builtUnrateModel, na.rm = FALSE, source.envir = Symbols)
                                                     # remove the last record(NO)
                                                     # "2007-01-31" (actual "2001-12-31")

  # just after TrainTest
  # BETTER OFF deciding THIS(validation records) early AND HARCODING THE DATES
  ValidationPredictionBegin <- as.character(head(index(xTs[TrainingEnd < index(xTs)]),1))
  # lastest data, ModelTarget data can ( and in very  last data will ) be NA
  ValidationPredictionEnd   <- as.character(tail(index(xTs),1))

  # POSIX would consider the string "YYYY-MM-DD" to be ambiguous.
  # So make it not-ambiguous
  if(stringr::str_detect(ValidationPredictionBegin,"^\\d{4}-\\d{2}-\\d{2}$")) {
      ValidationPredictionBegin <- zoo::as.Date(ValidationPredictionBegin)
  }
  if(stringr::str_detect(ValidationPredictionEnd,"^\\d{4}-\\d{2}-\\d{2}$")) {
      ValidationPredictionEnd <- zoo::as.Date(ValidationPredictionEnd)
  }
  ValidationData <- quantmod::modelData(UpdatedModelData, data.window = c(ValidationPredictionBegin, ValidationPredictionEnd), exclude.training = TRUE)
  ValidationDataCompleteCases <- complete.cases(ValidationData[, builtUnrateModel@model.inputs])

  if(any(!ValidationDataCompleteCases)){

    # xgboost 'somtimes' drops sending a response to incomplete records
    # (a "dropped observation")
    # Also, even though my "data" is "not (recently) available" enough"
    # so that I "do not have a most recent end of month record."
    # I would STILL prefer to have a "most recent end of month record"
    # so, I KNOW what I can invest-in NOW.

    DateTimesIncompleteValidationData <- index(ValidationData)[!ValidationDataCompleteCases]
    message("")
    message(stringr::str_c("*** Not full complete cases exist for date/times: ", stringr::str_c(DateTimesIncompleteValidationData, collapse = ", "), ". ***"))
    message(               "*** Now will do a na.locf() to complete it/them. ***")
    ValidationData <- cbind(ValidationData[, builtUnrateModel@model.target], na.locf(ValidationData[, builtUnrateModel@model.inputs]))
  }

  RawFitted  <- predictModel(UpdatedModelData@fitted.model, ValidationData)

  Fitted  <- as.xts(RawFitted, index(ValidationData))
  colnames(Fitted) <- "Fitted"

  logging::loginfo("End:   doMachineWts")

  return(Fitted)

  # # uses S3 ifelse.xts
  # # strategy/rule weights
  # FittedOneSidedThreashold <- quantile(coredata(Fitted))["25%"]
  # message("\n75% of the time, I am 'IN' the market.")
  # message(stringr::str_c("FittedSignal OneSidedThreashold threashold is ", FittedOneSidedThreashold, "\n"))
  #
  # # leading
  # FittedSignal <- ifelse( Fitted > FittedOneSidedThreashold, rep(1,NROW(Fitted)), rep(0,NROW(Fitted)))
  # colnames(FittedSignal)[1] <- stringr::str_c(ModelTarget, "_wts")
  #
  # # current
  # Current <- lag(FittedSignal)
  # colnames(Current) <- stringr::str_replace(colnames(FittedSignal), "leading", "current")
  #
  # FittedSignalAndCurrent <- merge(FittedSignal, Current)
  # logging::loginfo("End:   doMachineWts")

  # FittedSignalAndCurrent

})}
# , error = function(e) { ErrorHandler(e = e) }



#' determine the investment choice from the results of a Machine learning
#'
#' @description
#' \preformatted{
#'
#' Given a Fitted model (xts object).  Determine
#' whether or not to buy in my favored asset (may by the stock market)
#' xor to buy in my less favored asset (may be cash).
#'
#' }
#'
#' @param Fitted previously fitted xts object
#' @param ModelTarget_wts NULL(default) character name
#' of the predictee.  Usually, expected to have its name
#' end in "_wts".
#' @param ExtremePct "25"(default) percent passed to quantile.
#' This represents the low percent of the time that the investor
#' is "not doing the (good) One Choice".
#' I.e. (bad) times. E.g., recession, or crashes.
#' @return xts object with merged data into xTs
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_c
doOneChoice <- function(Fitted, ModelTarget_wts = NULL, ExtremePct = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  if(is.null(ExtremePct)) ExtremePct <- "25"

  # uses S3 ifelse.xts
  # strategy/rule weights
  FittedOneSidedThreashold <- quantile(coredata(Fitted))[stringr::str_c(ExtremePct, "%")]
  message(stringr::str_c("\n", (1 - as.numeric(ExtremePct)/100)*100, "% of the time, I am 'IN' the market."))
  message(stringr::str_c("FittedSignal OneSidedThreashold threashold is ", FittedOneSidedThreashold, "\n"))

  # leading
  FittedSignal <- ifelse( Fitted > FittedOneSidedThreashold, rep(1,NROW(Fitted)), rep(0,NROW(Fitted)))
  colnames(FittedSignal)[1] <- ModelTarget_wts

  # current
  Current <- lag(FittedSignal)
  colnames(Current) <- stringr::str_replace(colnames(FittedSignal), "leading", "current")

  RetFittedSignalAndCurrent <- merge(FittedSignal, Current)

  return(RetFittedSignalAndCurrent)

})}



#' error handler function meant for tryCatchLog::tryCatchLog
#'
#' @description
#' \preformatted{
#'
#' }
#'
#' @param e input from
#' tryCatchLog::tryCatchLog(. . . , error = function(e) { ErrorHandler(e) })"
#' @param useENVI getOption("useENVI") option.
#' If NULL or TRUE, will use get("envi") to search for
#' the calling environment location that is stored in "envi".
#' Otherwise, will just get the calling environment from parent.frame(8).
#' @importFrom DescTools DoCall
#' @importFrom plyr llply
#' @export
ErrorHandler <- function(e, useENVI = getOption("useENVI")) {

  if(is.null(useENVI) || useENVI) {useENVI <- TRUE} else {useENVI <- FALSE}

  message("Look up into the summary(^) and detail(^) stacks")
  message("for the exact error location: FILE.R#LINE.")
  message("Error follows: . . .")
  writeLines("print(str(e))")
  print(str(e))
  writeLines("print(str(as.list(e$call)))")
  print(str(as.list(e$call)))
  message("IF as.list(e$call)[[1]] is visible, Rerun exact ERROR with")
  message("this:    DescTools::DoCall(as.list(e$call)[[1]],unlist(as.list(e$call)[-1]))")
  message("")

  if(useENVI && exists("envi")) {
    envi <- get("envi")
  } else {
    # envi <- parent.frame(6)
    # envi <- parent.frame(7) # should choose the outer ONE
    # [ ] FIX: INSTEAD: should have manually searched UP for the 2nd occurance of "envi"
    envi <- parent.frame(7)
  }
  eENames <- ls(envir = envi, all.names = TRUE, sort = F)
  eE <- plyr::llply(eENames, function(x) { get(x, envir = envi) } )
  Names(eE) <- eENames
  eE <- list2env(eE)
  message("Inspect the last know contents of the local (calling) environment")
  message("using:    ls.str(eE) or str(as.list(eE))")

  invisible()
}












#' cash APC weights
#'
#' @description
#' \preformatted{
#'
#' Currently, this number is just only all of the other weight
#' columns (*_wts) subtracted from one(1)
#'
#' }
#'
#' @param xTs xts object
#' @return xts object with merged data into xTs
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_c
cashWts <- function(xTs = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})
   xTs <- initXts(xTs)

  # excess left over
  Cashwts <- xts(rep(1,NROW(xTs)),index(xTs)) - rowSums(xTs[ ,wtsLeadingRetsClms(xTs)], na.rm = TRUE)
  colnames(Cashwts)[1] <- stringr::str_c(xtsAttributes(xTs)[["rettargets"]][xtsAttributes(xTs)[["rettargets"]] %Like% "^CASH.*"], "_wts")

  Cashwts

})}



#' add cash weights of apc returns (CASHapcrets)
#'
#' @description
#' \preformatted{
#'
#' }
#'
#' @param xTs xts object
#' @return xts object with merged data into xTs
#' @export
#' @importFrom tryCatchLog tryCatchLog
appendCashWts  <- function(xTs = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  InBndxTs <- as.character(substitute(xTs))
  initMktData(xTs, InBndxTs)
  # xTs  <- initXts(xTs)

  xTs <- combineXts(xTs, cashWts(xTs))

  # xTs
  return(releaseMktData(xTs, InBndxTs, isInBndxTsMktData))

})}



#' all other wts to make PerformanceAnalytics::Return.portfolio happy
#'
#' @description
#' \preformatted{
#'
#' }
#'
#' @param xTs xts object
#' @return xts object with merged data into xTs
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_c str_detect
allOtherWts <- function(xTs = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  # If the [machine] model did not add these.
  # then add these HERE
  # to make (future) PerformanceAnalytics::Return.portfolio happy.
  AllPossibleOtherWtsCols <- setdiff(
                                stringr::str_replace(xtsAttributes(xTs)[["rettargets"]], "leadingrets", "leadingrets_wts")
                              , wtsLeadingRetsClms(xTs)
                              )

  # I want the index Class
  xTsNew <- xTs[FALSE,FALSE]
  for(col in AllPossibleOtherWtsCols){

    xTs1 <- xts(rep(0,NROW(xTs)),index(xTs))
    colnames(xTs1)[1] <- col
    xTsNew <- DescTools::DoCall(cbind, c(list(), list(xTsNew), list(xTs1)))

  }
  return(xTsNew)

})}



#' add all other wts to make PerformanceAnalytics::Return.portfolio happy
#'
#' @description
#' \preformatted{
#'
#' }
#'
#' @param xTs xts object
#' @return xts object with merged data into xTs
#' @export
#' @importFrom tryCatchLog tryCatchLog
appendAllOtherWts  <- function(xTs = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  InBndxTs <- as.character(substitute(xTs))
  initMktData(xTs, InBndxTs)
  # xTs  <- initXts(xTs)

  xTs <- combineXts(xTs, allOtherWts(xTs))

  # xTs
  return(releaseMktData(xTs, InBndxTs, isInBndxTsMktData))

})}




#' show the last six(6) records
#'
#' @description
#' \preformatted{
#'
#' }
#'
#' @param xTs xts object
#' @param title heading
#' @param n number of rows to print
#' @return invisible xts object
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_c
printTail <- function(xTs = NULL, title = NULL, n = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  InBndxTs <- as.character(substitute(xTs))
  initMktData(xTs, InBndxTs)
  # xTs  <- initXts(xTs)

  message(stringr::str_c("      tail of ", title))
  if(is.null(n)) n = 6
  options(digits = 5L)
  message(stringr::str_c("      *** Should choose to invest OFTEN in One(1): ", xtsAttributes(xTs)[["rettarget"]], " ***      "))
  # print(tail(xTs[, setdiff(safeClms(xTs),  c(wtsCurrentRetsClms(xTs), CASHClms(xTs)))], n = n))
  # print(tail(xTs[, setdiff(safeClms(xTs),  c(valueLeadingRetsClms(xTs), wtsCurrentRetsClms(xTs), CASHClms(xTs)))], n = n))
  print(tail(xTs[, c(currentRetsClms(xTs), nonImportantClmns(xTs), leadingRetsWtsClms(xTs))], n = n))
                                         # "Fitted" and "predictor columns" maybe later I should/could split this up
  # invisible(xTs)
  return(releaseMktData(xTs, InBndxTs, isInBndxTsMktData, xTsInvisible = TRUE))

})}



#' initialize
#'
#' @description
#' \preformatted{
#'
#' }
#'
#' @param initVal portfolio starting value($$). If it is null
#' then the value is set to by 100000
#' @return set value
#' @examples
#' \dontrun{
#' # > initPorfVal(initVal = NULL)
#' # [1] 100000
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
initPorfVal <- function(initVal = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  if(is.null(initVal)) {
    initVal <- 100000
  } else if(initVal <= 0) {
    stop("initVal should be a large number.")
  } else { }

  initVal

})}



#' get the column names
#'
#' @description
#' \preformatted{
#'
#' This is used internally.
#'
#' }
#'
#' @param xTs xts object
#' @return zero length vector or column names
#' @export
#' @importFrom tryCatchLog tryCatchLog
safeClms  <- function(xTs = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})
  xTs  <- initXts(xTs)

  if(is.null(xTs)) {
    clms <- vector(mode = "character")
  } else {
    clms <- colnames(xTs)
  }
  clms
})}



# #' get the indicator columns
# #'
# #' @description
# #' \preformatted{
# #'
# #' These are columns that do not have a
# #' corresponding column having
# #' its ending in "_wts" and do not have a do not have the
# #' same root name compared to each and every other *_wts column.
# #'
# #' CURRENTLY NOT USED
# #'
# #' }
# #'
# #' @param xTs xts objectt
# #' @return column names
# #' @examples
# #' \dontrun{
# #' # > require(xts)
# #' # > xTs <- xts(matrix(1:3, ncol = 3, dimnames = list(NULL,c("a","b","b_wts"))),zoo::as.Date(0))[0]
# #' # > indClms(xTs)
# #' # [1] "a"
# #' }
# #' @export
# #' @importFrom tryCatchLog tryCatchLog
# indClms <- function(xTs = NULL) {
# tryCatchLog::tryCatchLog({
# initEnv();on.exit({uninitEnv()})
#   xTs  <- initXts(xTs)
#   clms <- safeClms(xTs)
#
#   setdiff(clms, c(valueClms(xTs),wtsClms(xTs)))
# })}


# #' get the values columns names
# #'
# #' @description
# #' \preformatted{
# #'
# #' Values column names have an associated column with the
# #' same root.  However the associated column always ends with
# #' the suffix "_wts"
# #'
# #' }
# #'
# #' @param xTs xts object
# #' @return column names
# #' @examples
# #' \dontrun{
# #' # > require(xts)
# #' # > xTs <- xts(matrix(1:3, ncol = 3, dimnames = list(NULL,c("a","b","b_wts"))),zoo::as.Date(0))[0]
# #' # > valueClms(xTs)
# #' # [1] "b"
# #' }
# #' @export
# #' @importFrom tryCatchLog tryCatchLog
# #' @importFrom stringr str_detect
# #' @importFrom stringr str_replace
# valueClms <- function(xTs = NULL) {
# tryCatchLog::tryCatchLog({
# initEnv();on.exit({uninitEnv()})
#   xTs  <- initXts(xTs)
#
#   clms <- safeClms(xTs)
#   clms <- sort(clms)
#
#   stringr::str_replace(clms, "_wts$", "")[stringr::str_detect(clms, "_wts$")]
#
# })}



# #' get the weights(_wts) columns
# #'
# #' @description
# #' \preformatted{
# #'
# #' Weights column names always ends in "_wts"
# #' THIS IS CURRENTLY NOT USED ANYWHERE
# #'
# #' }
# #'
# #' @param xTs xts object
# #' @return column names
# #' @examples
# #' \dontrun{
# #' # > require(xts)
# #' # > xTs <- xts(matrix(1:3, ncol = 3, dimnames = list(NULL,c("a","b","b_wts"))),zoo::as.Date(0))[0]
# #' # > wtsClms(xTs)
# #' # [1] "b_wts
# #' }
# #' @export
# #' @importFrom tryCatchLog tryCatchLog
# #' @importFrom stringr str_detect
# wtsClms  <- function(xTs = NULL) {
# tryCatchLog::tryCatchLog({
# initEnv();on.exit({uninitEnv()})
#   xTs  <- initXts(xTs)
#
#   clms <- safeClms(xTs)
#   clms <- sort(clms)
#
#   clms[stringr::str_detect(clms, "_wts$")]
#
# })}
# # clms <- c("b_wts","b","a_wts","a", "c")
# # stopifnot(valueClms(clms),  c("a",    "b"    ))
# # stopifnot(  wtsClms(clms),  c("a_wts","b_wts"))




#' get the currentrets_wts columns
#'
#' @description
#' \preformatted{
#'
#' Weights column names always end in "currentrets_wts"
#'
#' }
#'
#' @param xTs xts object
#' @return column names
#' @examples
#' \dontrun{
#' # xTs <- xts(matrix(1:3, ncol = 3, dimnames = list(NULL,c("a","b","b_currentrets_wts"))),zoo::as.Date(0))[0]
#' # wtsCurrentRetsClms(xTs)
#' # [1] "b_currentrets_wts"
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_detect
wtsCurrentRetsClms  <- function(xTs = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})
  xTs  <- initXts(xTs)

  clms <- safeClms(xTs)
  clms <- sort(clms)

  clms[stringr::str_detect(clms, "currentrets_wts$")]

})}
# clms <- c("b_currentrets_wts","b","a_currentrets_wts","a", "c")
# stopifnot(  wtsCurrentRetsClms(clms),  c("a_currentrets_wts","b_currentrets_wts"))



#' get the leadingrets_wts columns
#'
#' @description
#' \preformatted{
#'
#' Weights column names always end in "leadingrets_wts".
#'
#' }
#'
#' @param xTs xts object
#' @return column names
#' @examples
#' \dontrun{
#' # xTs <- xts(matrix(1:3, ncol = 3, dimnames = list(NULL,c("a","b","b_leadingrets_wts"))),zoo::as.Date(0))[0]
#' # wtsLeadingRetsClms(xTs)
#' # [1] "b_leadingrets_wts"
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_detect
wtsLeadingRetsClms  <- function(xTs = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})
  xTs  <- initXts(xTs)

  clms <- safeClms(xTs)
  clms <- sort(clms)

  clms[stringr::str_detect(clms, "leadingrets_wts$")]

})}
# clms <- c("b_leadingrets_wts","b","a_leadingrets_wts","a", "c")
# stopifnot(  wtsLeadingRetsClms(clms),  c("a_leadingrets_wts","b_leadingrets_wts"))



#' get the currentrets columns
#'
#' @description
#' \preformatted{
#'
#' Value column names end in "currentrets" without "_wts".
#'
#' }
#'
#' @param xTs xts object
#' @return column names
#' @examples
#' \dontrun{
#'
#' # clms <- c("b_currentrets","b","a_currentrets","a", "c", "b_currentrets_wts", a_currentrets_wts")
#' # stopifnot(valueCurrentRetsClms(clms),  c("a_currentrets","b_currentrets"))
#'
#' # xTs <- xts(matrix(1:3, ncol = 3, dimnames = list(NULL,c("a","b","b_currentrets"))),zoo::as.Date(0))[0]
#' # valueCurrentRetsClmsClms(xTs)
#' # [1] "b_currentrets"
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_detect
valueCurrentRetsClms  <- function(xTs = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})
  xTs  <- initXts(xTs)

  clms <- safeClms(xTs)
  clms <- sort(clms)

  clms[stringr::str_detect(clms, "currentrets$")]

})}




#' get the leadingrets columns
#'
#' @description
#' \preformatted{
#'
#' Value column names end in "leadingrets" without "_wts"
#'
#' }
#'
#' @param xTs xts object
#' @return column names
#' @examples
#' \dontrun{
#'
#' # clms <- c("b_leadingrets","b","a_leadingrets","a", "c", "b_leadingrets_wts", a_leadingrets_wts")
#' # stopifnot(valueLeadingRetsClms(clms),  c("a_leadingrets","b_leadingrets"))
#'
#' # xTs <- xts(matrix(1:3, ncol = 3, dimnames = list(NULL,c("a","b","b_leadingrets"))),zoo::as.Date(0))[0]
#' # valueLeadingRetsClmsClms(xTs)
#' # [1] "b_leadingrets"
#'
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_detect
valueLeadingRetsClms  <- function(xTs = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})
  xTs  <- initXts(xTs)

  clms <- safeClms(xTs)
  clms <- sort(clms)

  clms[stringr::str_detect(clms, "leadingrets$")]

})}




#' get the currentrets columns
#'
#' @description
#' \preformatted{
#'
#' }
#'
#' @param xTs xts object
#' @return column names
#' @export
#' @importFrom tryCatchLog tryCatchLog
currentRetsClms <- function(xTs = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})
  xTs  <- initXts(xTs)

  clms <- safeClms(xTs)
  clms <- sort(clms)

  clms[stringr::str_detect(clms, "currentrets$")]

})}



#' get the leadingrets_wts columns
#'
#' @description
#' \preformatted{
#'
#' }
#'
#' @param xTs xts object
#' @return column names
#' @export
#' @importFrom tryCatchLog tryCatchLog
leadingRetsWtsClms <- function(xTs = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})
  xTs  <- initXts(xTs)

  clms <- safeClms(xTs)
  clms <- sort(clms)

  clms[stringr::str_detect(clms, "leadingrets_wts$")]

})}



#' get the non-[current/leading]rets[_wts] columns
#'
#' @description
#' \preformatted{
#'
#' Fitted" and "predictor columns"
#'
#' }
#'
#' @param xTs xts object
#' @return column names
#' @export
#' @importFrom tryCatchLog tryCatchLog
nonImportantClmns <- function(xTs = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})
  xTs  <- initXts(xTs)

  clms <- safeClms(xTs)
  clms <- sort(clms)

  clms[!stringr::str_detect(clms, "currentrets$|currentrets_wts$|leadingrets$|leadingrets_wts$")]

})}



#' get the CASH columns
#'
#' @description
#' \preformatted{
#'
#' MAY NOT BE USED ANYMORE?
#'
#' }
#'
#' @param xTs xts object
#' @return column names
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_detect
CASHClms  <- function(xTs = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})
  xTs  <- initXts(xTs)

  clms <- safeClms(xTs)
  clms <- sort(clms)

  clms[stringr::str_detect(clms, "^CASH")]

})}



#' get the un-massaged original 'predictor' columns
#'
#' @description
#' \preformatted{
#'
#' }
#'
#' @param xTs xts object
#' @return column names
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_detect
valuePredictorClms  <- function(xTs = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})
  xTs  <- initXts(xTs)

  clms <- safeClms(xTs)
  clms <- sort(clms)

  setdiff(clms, c(wtsCurrentRetsClms(xTs), wtsLeadingRetsClms(xTs), valueCurrentRetsClms(xTs), valueLeadingRetsClms(xTs)))

})}




#' get the porfolio log returns
#'
#' @description
#' \preformatted{
#'
#' This is calculated by taking the proporation of weights(0-1),
#' but typically a fraction and then multiplying this proportion
#' by its corresponding value column.  The sum of weights(_wts)
#' columns sum to be one(1).
#'
#' }
#'
#' @param xTs xts object
#' @param initVal start value of the investor's porfolio
#' @return xts object of geometric returns
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom PerformanceAnalytics Return.portfolio
portfolioAPCReturns <- function(xTs = NULL, initVal = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  xTs  <- initXts(xTs)
  initVal <- initPorfVal(initVal)

  xTs <- xTs[,c(valueCurrentRetsClms(xTs), wtsLeadingRetsClms(xTs))]
  xTs <- xTs[complete.cases(xTs)]

  valuexTs <- xTs[, valueCurrentRetsClms(xTs)]
  wtsxTs   <- xTs[, wtsLeadingRetsClms(xTs)]

  # Return.portfolio
  # currently: it makes no difference: I tried both ways
  # I choose NOT to use it
  # index(wtsxTs) <- index(wtsxTs) + 1

  # avoid
  # Warning in PerformanceAnalytics::Return.portfolio(R = valuexTs, weights = wtsxTs,  :
  # number of assets in beginning_weights is less than number of columns in returns, so subsetting returns.
  #
  # temp solutions? ... better to handle this earlier in the CODE
  if(NVAR(valuexTs) != NVAR(wtsxTs)) {
    stop("PerformanceAnalytics::Return.portfolio SHOULD HAVE NVAR(valuexTs) == NVAR(wtsxTs)")
  }

  PerformanceAnalytics::Return.portfolio(
      R       = valuexTs
    , weights =   wtsxTs
    , value   = initVal
    , verbose = TRUE
  ) -> returns

  returns$returns

})}


#' get the porfolio non-log(arithmatic) returns
#'
#' @description
#' \preformatted{
#'
#' }
#'
#' @param xTs xts object
#' @param initVal start value of the investor's porfolio
#' @return xts object of arithmatic returns
#' @export
#' @importFrom tryCatchLog tryCatchLog
portfolioMonthlyReturns <- function(xTs = NULL, initVal = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  InBndxTs <- as.character(substitute(xTs))
  initMktData(xTs, InBndxTs)
  # xTs  <- initXts(xTs)

  initVal <- initPorfVal(initVal)

  # calls "Return.portfolio.geometric"
  # note: the datum of leading returns is never invested
  # the first record holds the decision of what to do next
  # then the "what to do next" is read "first", then "next"
  # the action is done

  # portLogRet1
  # portfolio.returns
  #
  xTs1 <- portfolioAPCReturns(xTs = xTs, initVal = initVal)
  xTs  <- combineXts(xTs, xTs1)

  # "monthlyReturn(exp(cumsum(portLogRet1)) * initVal)" produces the
  # same answer as "ret" in return.Portfolio.geometric
  # and here! I loose the '1st datum of "ret" (becomes zero(0))
  # so I can skip this ( and note the month1 == 0 messes with the accumulations
  # producing a small accumulation error)
  #
  # monthlyReturn(exp(cumsum(portLogRet1)) * initVal)
  #
  # portLogRet1

  # xTs
  return(releaseMktData(xTs, InBndxTs, isInBndxTsMktData))

})}



#' print calendar of month by month returns
#'
#' @description
#' \preformatted{
#'
#' The vericle left side axis is in "years".
#' The horizontal axis is in "months".
#'
#' }
#'
#' Geometric adding is done across months
#' An end of year(12 month summary) is in the verticle right side axis
#'
#' @param xTs xts object
#' @param title heading
#' @param PortFolioRetCol "portfolio.returns"(default). Column of the 'portfolio returns.'
#' @param MarginReturns TRUE(Default) Print PerformanceAnalytics::table.CalendarReturns aggregates
#' in the margin. If FALSE, is a simpleCalendar instead with no marginal data (because
#' it may not make sense.  E.g. printing the GDP)
#' @param start passed to window.xts
#' @param end passed to window.xts
#' @param DivBy divide by this value before printing.
#' Useful to divide Fitted by 100.
#' @return xts object of arithmatic returns
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom PerformanceAnalytics table.CalendarReturns
#' @importFrom stringr str_c
#' @importFrom zoo na.trim
printCalendar <- function(xTs = NULL, title = NULL, PortFolioRetCol = NULL, MarginReturns = NULL, start = NULL, end = NULL, DivBy = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  InBndxTs <- as.character(substitute(xTs))
  initMktData(xTs, InBndxTs)
  # xTs  <- initXts(xTs)

  if(is.null(PortFolioRetCol)) PortFolioRetCol <- "portfolio.returns"
  if(is.null(MarginReturns)) MarginReturns <- TRUE
  if(is.null(DivBy)) DivBy <- 1.0

  message(stringr::str_c("calendar of ", title))
  options(digits = 5L)
  # horizonal reduction
  if(!is.null(start) || !is.null(end)) {
    PrintingXts <- window(xTs[, PortFolioRetCol], start = start, end = end) / DivBy
  } else {
    PrintingXts <- xTs[, PortFolioRetCol] / DivBy
  }
  # verticle reduction: remove leading NAs
  PrintingXts <- zoo::na.trim(sides = "left", PrintingXts)
  #
  if(MarginReturns) {
    CalendarResult <- PerformanceAnalytics::table.CalendarReturns(PrintingXts, digits = 2, as.perc = TRUE, geometric = TRUE)
  } else {
    CalendarResult  <-                             simpleCalendar(PrintingXts, digits = 2, as.perc = TRUE, geometric = TRUE)
  }
  print(CalendarResult)
  # currently JUST for display.
  # Currently,  I am not adding data to xTs.

  # invisible(xTs)
  return(releaseMktData(xTs, InBndxTs, isInBndxTsMktData, xTsInvisible = TRUE))

})}



#' print a simple Calendar
#'
#' @description
#' \preformatted{
#'
#' Based on PerformanceAnalytics table.CalendarReturns.
#' It does not do aggregate column marginals
#' }
#' @param R	an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns
#' @param digits	number of digits to round results to for presentation
#' @param as.perc	TRUE/FALSE if TRUE, multiply simple returns by 100 to get %
#' @param geometric	utilize geometric chaining (TRUE) or simple/arithmetic chaining (FALSE)
#' to aggregate returns, default TRUE
#' @return xts object of arithmatic returns
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom PerformanceAnalytics checkData
simpleCalendar <- function (R, digits = 1, as.perc = TRUE, geometric = TRUE) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

    ri = PerformanceAnalytics::checkData(R, method = "zoo")
    columns = ncol(ri)
    columnnames = colnames(ri)
    rownames = rownames(ri)
    firstyear = as.numeric(format(strptime(as.POSIXct(time(ri)[1]), "%Y-%m-%d"), "%Y"))
    lastyear = as.numeric(format(strptime(as.POSIXct(time(ri)[length(ri[, 1])]), "%Y-%m-%d"), "%Y"))
    year = format(strptime(as.POSIXct(time(ri)), "%Y-%m-%d"), "%Y")
    month = format(strptime(as.POSIXct(time(ri)), "%Y-%m-%d"), "%b")
    monthlabels = strftime(seq.Date(as.Date("2000-01-01"), length.out = 12, by = "months"), format = "%b")
    rowlabels = (firstyear:lastyear)

    for (column in 1:columns) {
        target.df = as.data.frame(matrix(data = as.numeric(NA),
            length(rowlabels), length(monthlabels), dimnames = list(rowlabels, monthlabels)))
        for (i in 1:length(ri[, 1])) {
            if (!is.na(ri[i, column])) {
                target.df[year[i], month[i]] = ri[i, column]
            }
        }
        yearcol = as.data.frame(matrix(data = as.numeric(NA),
            length(rowlabels), 1, dimnames = list(rowlabels,
                columnnames[column])))
        # for (i in 1:length(yearcol[, 1])) {
        #     if (geometric)
        #         yearcol[i, columnnames[column]] = prod(1 + na.omit(as.numeric(target.df[i,]))) - 1
        #     else yearcol[i, columnnames[column]] = sum(as.numeric(target.df[i,
        #         ]), na.rm = TRUE)
        #     if (yearcol[i, columnnames[column]] == 0)
        #         yearcol[i, columnnames[column]] = NA
        # }
        target.df = cbind(target.df, yearcol)
        if (as.perc)
            multiplier = 100
        else multiplier = 1
        target.df = target.df * multiplier
        target.df = base::round(target.df, digits)
        if (column == 1)
            result.df = target.df
        else {
            result.df = cbind(result.df, target.df[, 13])
        }
    }
    colnames(result.df) = c(monthlabels, columnnames)
    result.df
})}





