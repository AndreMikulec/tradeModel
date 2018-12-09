

#' garantee a date is a date or and empty date
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


#' garantees return value value is a 'matrix of at least one dimension'or NULL
#'
#' handles the edge case of coredata(<vector of element of size 1 or size 0>)
#' meant specifically to input empty coredata data into xts(, index)
#' xts(,index) only accepts a non zero-dimension matrix or a NULL

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


#' number of variables
#'
#' currently NOT USED
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

  base::NCOL(x)

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


#' sets the enviroment
#'
#' space-saver - meant to be used at the beginning of a function
#'
#' @param init list with names entries of alernate/other options
#' @param envir where to return the options
#' @return environment is set
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
  options(warn=1L) # so, I can use BELOW: options(tryCatchLog.write.error.dump.file = TRUE)
  options(width=10000L) # LIMIT # Note: set Rterm(64 bit) as appropriate
  options(digits=if(is.null(init[["digits"]])) { 22L } else {init[["digits"]]})
  options(max.print=99999L)
  options(scipen=255L) # Try these = width

  # BECAUSE MY "WARNINGS ARE CONVERTED TO ERRORS" ( ABOVE: options(warn=2L) )
  # I can not directly use this feature
  # Error in save(list = names(.GlobalEnv), file = outfile, version = version,  :
  # (converted from warning) 'package:stats' may not be available when loading
  options(tryCatchLog.write.error.dump.file = TRUE)

  ops <- options()
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
#' space-saver - meant to be used at the beginning of a function
#'
#' @return environment is un-set
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

  envii <- rlang::caller_env()
  Sys.setenv(TZ=get("oldtz", envir = envii))
  options(get("ops",         envir = envii))
  invisible()

})}


#' geometric investment results
#'
#' xTs values less that zero will generate a numeric error
#'
#' @param xTs xts object of arithmatic returns
#' @return xts object of geometric returns
#' @examples
#' \dontrun{
#' # > require(xts)
#' # > xTs  <- xts(10:12,zoo::as.Date(0:2))
#' # > lr <- logReturns(xTs)
#' # > lr
#' #                         logrets
#' # 1970-01-01 0.000000000000000000
#' # 1970-01-02 0.095310179804324768
#' # 1970-01-03 0.087011376989629685
#' #
#' # > as.vector(coredata(exp(cumsum(lr)) * 10L))
#' # [1] 10.000000000000000 10.999999999999998 11.999999999999996
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_c
logReturns <- function(xTs = NULL)  {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  xTs  <- initXts(xTs)

  xTsLogRets <- ROC(xTs)             # which(is.na(xTsindLogRets)) # logrithmic
  xTsLogRets[is.na(xTsLogRets)] <- 0 # usually just the 1st observation
  colnames(xTsLogRets)[1] <- stringr::str_c(colnames(xTsLogRets)[1], "logrets")

  xTsLogRets

})}


#' get data from the St. Louis FRED
#'
#' @param Symbol FRED symbol as a string
#' @return xts object of results
#' @examples
#' \dontrun{
#' EXAMPLE
#' # > head(fredData("GDP"),1)
#' #                           gdp
#' # 1947-01-01 243.16399999999999
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_c
fredData <- function(Symbol = NULL, New = NULL, NewMaxAge = NULL) {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})
  if(is.null(Symbol)) stop("No fredData was requested")

  if(is.null(New)) New <- TRUE
  if(New) NewMaxAge <- "4 hours"

  message(stringr::str_c("Begin fredData - "), Symbol)

  src = "FRED"
  from = "1950-01-01"
  # NOTE, if Symbol == "WILL5000IND" # SINCE DEC 1970

  if(New){
    xTs <- getNewSymbols(Symbol, src = "FRED",
           from = from, auto.assign = FALSE, MaxAge = NewMaxAge)
  } else {
    xTs <- getSymbols(Symbol, src = "FRED",
           from = from, auto.assign = FALSE)
  }
  message(stringr::str_c("End   fredData - "), Symbol)

  colnames(xTs)[1] <- tolower(colnames(xTs))

  xTs

})}


#' get the latest value in the month
#'
#' @param xTs xts object
#' (currently) eomData only works on single column xtx objects
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
eomData <- function(xTs = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  xTs  <- initXts(xTs)
  xTsOrig <- xTs

  if(NCOL(xTs) > 1) stop("(currently) eomData only works on single column xtx objects")
  xTs <- to.monthly(xTs[!is.na(xTs)], OHLC = FALSE, indexAt = "lastof")
  xTs <- xTs[index(xTs) <= tail(index(xTsOrig),1)] # LEFT_OFF

  xTs

})}


#' lagging
#'
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
#' @importFrom lubridate %m+%
#' @importFrom DescTools Day
#' @importFrom DescTools LastDayOfMonth
Lagging <- function(xTs = NULL, Shift = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  `%m+%` <- lubridate::`%m+%`

  xTs <- initXts(xTs)
  if(is.null(Shift)) Shift = 1
  # compare to quantmod:::Lag.xts
  if(periodicity(xTs)[["scale"]] == "monthly") {
    if(DescTools::Day(head(index(xTs),1)) %in% c(28:31)) {
      refDates <- DescTools::LastDayOfMonth(tail(index(xTs),1) %m+% months( 1 * seq_len(Shift)) )
    }
    xTs <- merge(xTs, xts(, refDates) )
  }
  xTs %>% { lag(., 1 * Shift) } -> xTs
  if(stringr::str_detect(colnames(xTs)[1], "leadingrets$")) {
    colnames(xTs)[1] <- stringr::str_replace(colnames(xTs)[1], "leadingrets$", "rets")
  } else {
    colnames(xTs)[1] <- stringr::str_replace(colnames(xTs)[1], "rets$", "laggingrets")
  }
  xTs
})}



#' generate a good xts column name
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
#' works better on xts objects ( lapply or plyr::llply structure is held together )
#'
#' @param x1 data.frame or xts object
#' @param x2 data.frame or xts object
#' @return list of length two of two data.frames or xts objects
#' @examples
#' \dontrun{
#'#
#'# > list(iris[1:2,1:2], airquality[1:2,1:2])
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
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom plyr llply
pairWise <- function(x1, x2) {

  List <- c(list(),plyr::llply(x1, identity), plyr::llply(x2, identity))

  L1coord <- seq(from = 1, by = 2, length.out = 0.5*length(List))
  L2coord <- seq(from = 2, by = 2, length.out = 0.5*length(List))

  c(list(List[L1coord]),list(List[L2coord]))

}


#' Interleave two vectors of arbitrary length
#'
#' from R CRAN package rmngb
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
#' From a long data.frame, makes it into a wide data.frame.
#'
#' First, split-off into many data.frames using UniqueIDRegex
#' (and 100 percent correlated ConstColsRegex)
#'
#' Next, generates new columns using FactorColsRegex and then
#' repositions data items into those new columns ( see the example )
#' Data items are found in columns that are 'not members' of
#' UniqueIDRegex, ConstColsRegex, and FactorColsRegex
#'
#' Last, 'smart' merges all of the data.frames together while
#' adding new columns contributed by each splitted-off data.frame.
#' Results, will/should be (hopfully) 'less' rows with many more columns
#'
#' @param UniqueIDRegex unique IDs column names regular expression: determines the
#' character vector of the columns that compose of the "unique identifier" for the row.
#' This combination splits the input data.frame(x) into many data.frames(x - splitted).
#' @param ConstColsRegex constant column names regular expression: determines the
#' character vector of (1) the columns that compose of the "unique identifier" for the row.
#' (must include columns determined by parameter "UniqueIDRegex")
#' and (2) optionally its 100 percent correlated columns (with values that
#' do not vary with the values of the "unique identifer" columns.
#' An examples would be another datatype or alias or alternate name: eg. 17000 "2016-07-18"
#' @param FactorColsRegex reqular expression that determines the columns
#' to be flattened. It also the program "subtracts off" columns found in parameter "ConstColsRegex"
#' @param FactorColsNAReplace of columns that are the "result of FactorColsRegex",
#' replacement of the NA values.  Good options may be "None" or "Unknown" or "NotAppl"
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



#' (true) sortino ratio
#'
#' Sortino Ratio: Are you calculating it wrong?
#' https://www.rcmalternatives.com/2013/09/sortino-ratio-are-you-calculating-it-wrong/
#'
#' I CAN GET RID OF THIS FUNCTION
#'
#' # THIS WORKS CORRECTLY "AS IS"
#' Add feature zeroMAR to SortinoRatio ( actually DownsideDeviation ) #106
#' https://github.com/braverock/PerformanceAnalytics/issues/106
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
#' The best roller can roll anything with the most features
#' The idea is that I can control(change) the INTERIOR data OF NAs founds
#' within the middle of the data.
#' This data and middle of data is meant to be sent
#' to package TTR/PerformanceAnaltics functions.
#' TTR functions reject(error out) on NAs found in the interior.
#'
#' Here I can estimate or replace those NAs found in the middle of the data
#' before that data reaches TTR functions
#'
#' @param xTs xts object
#' @return modified xts object
#'
#' @examples
#' \dontrun{
#'#
#'#
#'# > require(TTR)
#'# > SMA(xts(c(0,1,2,3,NA), zoo::as.Date(0:4)), n = 2)
#'# Error in runSum(x, n) : Series contains non-leading NAs
#'#
#'# > SMA(xts(c(0,1,NA,3,4), zoo::as.Date(0:4)), n = 2)
#'# Error in runSum(x, n) : Series contains non-leading NAs
#'#
#'# > SMA(xts(c(NA,1,2,3,4), zoo::as.Date(0:4)), n = 2)
#'#            SMA
#'# 1970-01-01  NA
#'# 1970-01-02  NA
#'# 1970-01-03 1.5
#'# 1970-01-04 2.5
#'# 1970-01-05 3.5
#'#
#'# > rollApply(xts(c(0,-1,2,-3,4), zoo::as.Date(0:4))
#'#     , Fun = trueSortinoRatio, AltName = "SRTN", partial = TRUE, width = 2, MinRows = 2)
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
#'# 1970-01-04 2.0  # unless, I come up with a *better way* to re-organize 'around' the [NA] data
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
    reclass(xTsResult, xTsOrig)
  } -> xTsResult

  NewName <- "rollApply"
  if(!is.null(AltName)) NewName <- AltName
  if(is.null(FixedSep)) FixedSep <- "."

  colnames(xTsResult) <-  stringr::str_c( colnames(xTsResult), stringr::str_c(c(NewName, width), collapse = FixedSep), collapse = FixedSep)

  return(xTsResult)

})}





#' expland out xts
#'
#' from an xts function stub, create an ets object of derived columns
#'
#' Meant to create many package TTR technical trading column results
#' CURRENTLY NOT USED
#'
#' Idea was from
#'
#' Time series cross-validation 5
#' January 24, 2013
#' By Deane-Mayer
#' http://www.r-bloggers.com/time-series-cross-validation-5/
#' http://moderntoolmaking.blogspot.com/2013/01/time-series-cross-validation-5.html?utm_source=feedburner&utm_medium=feed&utm_campaign=Feed%3A+ModernToolMaking+%28Modern+Tool+Making%29
#'
#' NOTE: if any xTs2, then xTs1 and xTs2 are paired/matched column position to column position)
#'
#' @param xTs1 xts object
#' @param xTs2 xts object
#' @param Fun function name in the "bare" or in literal quotes("")
#' @param Whiches list of possible varying parameters that are expanded
#' to all possible combinations by expand.grid
#' @param AltName string alternate name for "Fun"
#' @param Prefix boolan default is FALSE.  TRUE would place the column meta before the column name
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
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom purrr transpose
#' @importFrom plyr llply
#' @importFrom DescTools DoCall
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
      xTs1 <- xTsColumnSet[[1]]; xTs2 <- xTsColumnSet[[2]]

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
#' pads beginning date as necessary
#'
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
#' @importFrom lubridate %m+%
#' @importFrom DescTools Day
#' @importFrom DescTools LastDayOfMonth
Leading <- function(xTs = NULL, Shift = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  `%m+%` <- lubridate::`%m+%`

  xTs <- initXts(xTs)
  if(is.null(Shift)) Shift = 1
  # compare to quantmod:::Lag.xts
  if(periodicity(xTs)[["scale"]] == "monthly") {
    if(DescTools::Day(head(index(xTs),1)) %in% c(28:31)) {
      refDates <- DescTools::LastDayOfMonth(head(index(xTs),1) %m+% months(-1 * seq_len(Shift)) )
    }
    xTs <- merge(xTs, xts(, refDates) )
  }
  xTs %>% { lag(.,-1 * Shift) } -> xTs
  if(stringr::str_detect(colnames(xTs)[1], "leadingrets$")) {
    colnames(xTs)[1] <- stringr::str_replace(colnames(xTs)[1], "laggingrets$", "rets")
  } else {
    colnames(xTs)[1] <- stringr::str_replace(colnames(xTs)[1], "rets$", "leadingrets")
  }
  xTs
})}


#' NBER timeslices
#'
#'@param allSlicesStart NULL(default), Date of the earlies possbile date.
#'This Date is the first day(1st) of a month. Note: this filter is applied LAST.
#'@param allSlicesEnd NULL(default), Date of the latest possbile date.
#'This Date is the last day(last) of a month. Note: this filter is applied late.
#'@param LongTimeSlices FALSE(default), if TRUE, include non-recession range that
#'is before this recession range.
#'@param LongestTimeSlice FALSE(default), if TRUE then the start value is the
#'beginning of "NBER dates" (and limited by allSlicesStart)
#'LongestTimeSlice = TRUE and LongTimeSlices = TRUE ARE mutually exclusive choices
#'of each other
#'@param OmitSliceFirstDate FALSE(default), All dates are end of month dates.
#'A previous timeslice tailing date is the the same date as the next timeslice heading date.
#'TRUE omits the heading date from each time slice.  This may be necessary to prevent
#'repetition of data, for example in sending timeslices to machine learning models.
#' @return a list of vectors of Dates of recession Ranges
#' @examples
#' \dontrun{
#' # str(timeSliceNBER())
#' # str(timeSliceNBER(allSlicesStart = zoo::as.Date("1969-12-31")))
#' # str(timeSliceNBER(allSlicesStart = zoo::as.Date("1969-12-31")
#' # , LongTimeSlices = TRUE)
#' # )
#' # str(timeSliceNBER(allSlicesStart = zoo::as.Date("1969-12-31")
#' #   , LongTimeSlices = TRUE, OmitSliceFirstDate = TRUE)
#' # )
#' # str(timeSliceNBER(allSlicesStart = zoo::as.Date("1969-12-31")
#' # , LongestTimeSlice = TRUE)
#' # )
#' # # now after seeing all of the date, THEN, choose the filter
#' # str(timeSliceNBER(allSlicesStart = zoo::as.Date("1969-12-31")
#' # , LongestTimeSlice = TRUE, allSlicesStart = ?, allSlicesEnd = ?)
#' # )
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom plyr llply
#' @importFrom tis nberDates
#' @importFrom zoo as.Date
timeSliceNBER <- function(allSlicesStart = NULL, allSlicesEnd = NULL, LongTimeSlices = NULL, LongestTimeSlice = NULL, OmitSliceFirstDate = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  if(!length(LongTimeSlices))   LongTimeSlices <- FALSE
  if(!length(LongestTimeSlice)) LongestTimeSlice <- FALSE

  NBERTISMatrix <-  tis::nberDates()
  datesFromTIS <-  function(x) zoo::as.Date(as.character(x), format = "%Y%m%d")
  beginNBERDates <- datesFromTIS(NBERTISMatrix[, "Start"])
    endNBERDates <- datesFromTIS(NBERTISMatrix[, "End"  ])
  xts(
      matrix(c(
          as.numeric(beginNBERDates)
        , as.numeric(endNBERDates)
        )
        , ncol = 2, dimnames = list(NULL, c("Start","End"))
      )
    , beginNBERDates
  ) -> NBERDates
  # previous recession's end
  LongStart <- lag(NBERDates[,"End"]) + 1; colnames(LongStart)[1] <- "LongStart"
  NBERDates <- merge(LongStart, NBERDates)

  # filter out
  if(length(allSlicesStart)) NBERDates <- NBERDates[allSlicesStart   <= index(NBERDates)]
    if(!NROW(NBERDates)) stop("timeSliceNBER allSlicesStart removed all data")
  if(length(allSlicesEnd))   NBERDates <- NBERDates[index(NBERDates) <= allSlicesEnd]
    if(!NROW(NBERDates)) stop("timeSliceNBER allSlicesEnd removed all data")

  # FUTURE: instead, could have detected periodicity and used split.xts
  # determine
  split(as.data.frame(NBERDates), seq_len(NROW(NBERDates))) %>%
     plyr::llply(function(x) {

       if(LongestTimeSlice) {
         ActualStart <- index(NBERDates[1,"Start"])
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

     }) -> ListOfNBERDateRanges

     ListOfNBERDateRanges

})}



#' get the latest value in the month from FRED
#'
#' ...
#' @param Symbol FRED symbol as a string
#' @return xts object of the last observation with the
#' month date rounded up to the last day of the month
#' @examples
#' \dontrun{
#' EXAMPLE
#' # # weekly
#' # > ff <- head(fredEomData("FF"),1)
#' # > ff,
#' #              ff
#' # 1954-07-31 0.63
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
fredEomData <- function(Symbol = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  if(is.null(Symbol)) stop("No fredData was requested")
  fredData(Symbol = Symbol) %>%
     eomData

})}





#' Wilshire 5000 Index price
#'
#' @return xts object of end of month returns
#' @export
#' @importFrom tryCatchLog tryCatchLog
wilshire5000indEomData <- function() {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  fredEomData(Symbol = "WILL5000IND")

})}


# NOTE: I may want to change to function(x, ...)

#' change the index date to the future
#'
#' @param x xts object or Date object
#' @return xts object or data object with the index moved up
#' @rdname nextMonthfromYesterday
#' @export
nextMonthfromYesterday <- function(x = NULL) {

  # tryCatchLog is not allowed here
  UseMethod("nextMonthfromYesterday")

}


#' @rdname nextMonthfromYesterday
#' @export
#' @importFrom tryCatchLog tryCatchLog
nextMonthfromYesterday.default <- function(x = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  stop("No nextMonthfromYesterday method for <input>")

})}


#' @param x date object
#' @rdname nextMonthfromYesterday
#' @examples
#' \dontrun{
#' # > require(xts)
#' # > nextMonthfromYesterday(zoo::as.Date("1970-01-12"))
#' # [1] "1970-01-31"
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom lubridate days
#' @importFrom lubridate %m+%
#' @importFrom Hmisc truncPOSIXt
#' @importFrom zoo as.Date
nextMonthfromYesterday.Date <- function(x = NULL) {
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


#' @rdname nextMonthfromYesterday
#' @examples
#' \dontrun{
#' # > require(xts)
#' # > xTs <- xts(, zoo::as.Date("1970-01-12"))
#' # > nextMonthfromYesterday(xTs)
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
nextMonthfromYesterday.xts <- function(x = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  xTs  <- initXts(x)
  plyr::llply(index(xTs), nextMonthfromYesterday) %>%
    { DescTools::DoCall(c,.) } %>%
      { xts(Coredata(xTs),.) }

})}


#' get the end of month UNRATE from FRED
#'
#' @return xts object
#' @export
#' @importFrom tryCatchLog tryCatchLog
unRateEomData <- function() {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  # index adjust
  # last known unemployment rate: when I recieved it; one month later
   fredData(Symbol = "UNRATE") %>%
    nextMonthfromYesterday

})}


#' add UNRATE (unrate)
#'
#' @param xTs xts object
#' @return xts object with merged data into xTs
#' @export
#' @importFrom tryCatchLog tryCatchLog
addUnRateEomData <- function(xTs = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})
  xTs  <- initXts(xTs)

  unRate <- unRateEomData()

  combineLogReturns(xTs, unRate)

})}


#' join two xts objects
#'
#' handles the edge case: if BOTH have no coredata (then merge.xts produces
#' ONE no-data no-index xts object) then instead, preserve the indexes
#'
#' @param xTs xts object
#' @param xTs1 xts object to merge into xTs
#' @return xts object
#' @examples
#' \dontrun{
#' #  > combineXts( initXts(NULL),xts(,zoo::as.Date(0)) )
#' # Data:
#' # numeric(0)
#' #
#' # Index:
#' #  Date[1:1], format: "1970-01-01"
#' # > combineXts( xts(,zoo::as.Date(1)), xts(,zoo::as.Date(0)) )
#' # Data:
#' # numeric(0)
#' #
#' # Index:
#' #  Date[1:2], format: "1970-01-01" "1970-01-02"
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
combineXts <- function(xTs = NULL,xTs1 = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  xTs  <- initXts(xTs); xTs1 <- initXts(xTs1)
  # edge case: if BOTH have no coredata (then merge.xts produces
  # ONE no-data no-index xts object) then instead, preserve the indexes
  if(is.null(Coredata(xTs)) && is.null(Coredata(xTs1))) {
    return(xts(, sort(unique(c(index(xTs),index(xTs1))))))
  } else {
    return(merge(xTs,xTs1))
  }

})}


#' join two Log Returned xts objects
#'
#' @param xTs xts object
#' @param xTs1 xts object to merge into xTs
#' @return xts object
#' @export
#' @importFrom tryCatchLog tryCatchLog
combineLogReturns <- function(xTs = NULL, xTs1 = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  xTs  <- initXts(xTs); xTs1 <- initXts(xTs1)

  combineXts(xTs, xTs1)

})}


#' cash log returns (cashlogrets)
#'
#' @param xTs xts object (only takes the index)
#' @return xts object with the same index as xTs
#' @export
#' @importFrom tryCatchLog tryCatchLog
cashLogReturns <- function(xTs = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  xTs  <- initXts(xTs)

  cashLogRets <- xts(rep(0,NROW(xTs)),index(xTs))
  colnames(cashLogRets)[1] <- "cashlogrets"

  cashLogRets

})}



#' add cash log returns (cashlogrets)
#'
#' currently not used anywhere
#'
#' @param xTs xts object (only takes the index)
#' @return xts object with merged data into xTs
#' @export
#' @importFrom tryCatchLog tryCatchLog
addCashLogReturns <- function(xTs = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  xTs  <- initXts(xTs)

                         # cashlogrets"
  combineLogReturns(xTs, cashLogReturns(xTs))

})}

#' leading cash log returns (cashlogrets)
#'
#' @param xTs xts object (only takes the index)
#' @return leading xts object
#' @export
leadingCashLogReturns <- function(xTs = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  xTs  <- initXts(xTs)

  cashLogReturns(xTs) %>%
    Leading -> xTs
  xTs

})}



#' add cash log returns (cashlogrets)
#'
#' @param xTs xts object
#' @return xts object with merged data into xTs
#' @export
addLeadingCashLogReturns <- function(xTs = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  xTs  <- initXts(xTs)

                         # cashlogrets"
  combineLogReturns(xTs, leadingCashLogReturns(xTs))

})}



#' get the Wilshare 5000 Index log returns
#'
#' @return xts object
#' @export
#' @importFrom tryCatchLog tryCatchLog
wilshire5000LogReturns <- function() {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  will5000idx <- wilshire5000indEomData()
  colnames(will5000idx)[1] <- "will5000idx"

  logReturns(xTs = will5000idx)

})}



#' add Willshire 5000 Index log returns (will5000idxlogrets)
#'
#' currently not used anywhere
#'
#' @param xTs xts object
#' @return xts object with merged data into xTs
#' @export
#' @importFrom tryCatchLog tryCatchLog
addWilshire5000LogReturns <- function(xTs = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  xTs  <- initXts(xTs)

                         # will5000idxlogrets
  combineLogReturns(xTs, wilshire5000LogReturns())

})}



#' get the Wilshare 5000 Index log returns
#'
#' @return xts object
#' @export
#' @importFrom tryCatchLog tryCatchLog
leadingWilshire5000LogReturns <- function() {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  wilshire5000LogReturns() %>% Leading

})}



#' add Willshire 5000 Index log returns (will5000idxlogrets)
#'
#' @param xTs xts object
#' @return xts object with merged data into xTs
#' @export
#' @importFrom tryCatchLog tryCatchLog
addLeadingWilshire5000LogReturns <- function(xTs = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  xTs  <- initXts(xTs)

                         # will5000idxlogrets
  combineLogReturns(xTs, leadingWilshire5000LogReturns())

})}



#' add weights (_wts)
#'
#' @param xTs target xts object
#' @param xTs1 source xts object of new weights
#' @return xts object with merged data into xTs
#' @export
#' @importFrom tryCatchLog tryCatchLog
addWts  <- function(xTs = NULL, xTs1 = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  xTs  <- initXts(xTs); xTs1 <- initXts(xTs1)

  combineXts(xTs, xTs1)

})}


#' add Willshire 5000 Index weights using eyeball
#'
#' This is the workhorse function. This is where the magic/logic happens.
#' Use any other columns (called indicators) that do not have the weights (_wts)
#' suffix and do not have the same root name compared to each and every
#' other *_wts column.
#'
#' @param xTs xts object
#' @return xts object with merged data into xTs
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_c
#' @importFrom stringr str_detect
willShire5000EyeBallWts <- function(xTs = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  xTs <- initXts(xTs)
  unrate <- xTs[,"unrate"]
  # can't do math on leading NAs
  unrate <- unrate[!is.na(unrate),]

  unrate_wts <- ifelse( ((SMA(unrate,2)        - SMA(    unrate   ,6)) <= 0)              |
                        ((SMA(lag(unrate),2)   - SMA(lag(unrate  ),6)) <= 0)              |
                        ((SMA(lag(unrate,2),2) - SMA(lag(unrate,2),6)) <= 0), 1.00, 0.00)
  unrate_wts[is.na(unrate_wts)] <- 1 # 100% allocated

  colnames(unrate_wts)[1] <- stringr::str_c(colnames(xTs)[stringr::str_detect(colnames(xTs), "^will5000idx.*rets$")], "_wts")

  unrate_wts

})}


#' SMAs of the unrate Eyeball Indicator
#'
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom zoo na.trim
unrateEyeballIndicators <- function(unrate = NULL) {
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



#' for package caret function trainControl parameter summaryFunction
#' True Sortino Ratio 'summary function'
#'
#' @export
SortinoRatioSummary <- function (data, lev = NULL, model = NULL) {

  require(PerformanceAnalytics)

  Power <- 16
  R <- data[,"obs"] - data[,"pred"]
  # numeric vector of size NROW(data)
  #
  outOrig <- SortinoRatio(R = R, MAR = 0)
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
  StrengthBelowMAR <- 4
  if(outOrig < 0)  out <- out * StrengthBelowMAR

  names(out)[1] <- "ratio"
  return(out)

}


#' for package iml function FeatureImp$new
#' True Sortino Ratio 'loss function'
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
#' @param x anything with methods for
#' Ops: "+","-","/", "*" and possibly methods for min/max
#' @param ... passed to DMwR ReScaling. Do not pass: x, t.mn, and t.mx
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
#' @param 2 dim object data.frame or two=dim non-matrix, non-array
#' with methods for
#' Ops: "+","-","/", "*" and possibly methods for min/max
#' @param cols columns to rescale
#' @param ... passed to DMwR ReScaling. Do not pass: x, t.mn, and t.mx
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



#' formula.tools:::as.character.formula
#'
#' @export
formula_tools___as_character_formula <- function (x, ...) {
    form <- paste(deparse(x), collapse = " ")
    form <- gsub("\\s+", " ", form, perl = FALSE)
    return(form)
}


#' column split an xts object into
#' an environment of column-xts objects
#'
#' uses S3 function: xts:::as.data.frame.xts
#'
#' @param xTs xts object
#' @export
#' @importFrom plyr llply
xTsCols2SymbolsEnv <- function(xTs) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  # IF REMOVE ".fun = " then RStudio can debug
  # create an environment of xts objects
  plyr::llply(as.data.frame(xTs),
    function(x) {
      xx <- as.xts(x, order.by = index(xTs))
      # (+) non-core attributes (user) [if any]
      xtsAttributes(xx) <- xtsAttributes(xTs)
      xx
    }
  ) -> SymbolsOrig

  # reorders in alphabetical order
  Symbols <- list2env(SymbolsOrig)
  Symbols

})}



#' add Willshire 5000 Index weights using Machine learning
#'
#' This is the workhorse function. This is where the magic/logic happens.
#' Use any other columns (called indicators) that do not have the weights (_wts)
#' suffix and do not have the same root name compared to each and every
#' other *_wts column.
#'
#' @param xTs xts object
#' @return xts object with merged data into xTs
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom logging loginfo
#' @importFrom plyr llply
#' @importFrom stringr str_c
#' @importFrom dplyr arrange
#' @importFrom plyr llply
#' @importFrom dplyr case_when
#' @importFrom Hmisc wtd.quantile
#' @importFrom DescTools DoCall
#' @importFrom UBL ImpSampRegress
#' @importFrom iml Predictor FeatureImp Interaction
#' @importFrom rlist list.zip
#' @importFrom caret trainControl
#' @importFrom formula.tools lhs.vars
willShire5000MachineWts <- function(xTs = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  logging::loginfo("Begin: willShire5000MachineWts")

  # received xTs( will5000idxlogrets, cashlogrets, unrate )
  # merge.xts target, indictors, and predictors into merged xTs

  unrate <- xTs[,"unrate"]
  unrateIndicators <- unrateEyeballIndicators(unrate)
  unrateIndicators <- initXts(unrateIndicators)

  # all indicators
  Indicators <- unrateIndicators
  xTs <- merge(xTs, Indicators)

  xTs <- initXts(xTs)

  # # IF REMOVE ".fun = " then RStudio can debug
  # # create an environment of xts objects
  # plyr::llply(as.data.frame(xTs),
  #   .fun =  function(x) {
  #     as.xts(x, order.by = index(xTs))
  #   }
  # ) -> SymbolsOrig
  #
  # # reorders in alphabetical order
  # Symbols <- list2env(SymbolsOrig)

  # ordered R environment of Symbols
  Symbols <- xTsCols2SymbolsEnv(xTs)

  # traditionally the first column is the target variable
  specifyModel(formula = as.formula(stringr::str_c( colnames(xTs)[1], " ~ ", stringr::str_c(colnames(Indicators), collapse = " + ")))
            , na.rm = FALSE, source.envir = Symbols) ->
              # remove the last record(NO)
  specifiedUnrateModel

  # I can only train, test, validate where I have 'model target' predictee values
  ModelTarget          <- formula.tools::lhs.vars((formula(specifiedUnrateModel)))
  ModelTargetFirstDate <- head(index(na.trim(xTs[,ModelTarget])),1)
  ModelTargetTrainTestFirstDate <- ModelTargetFirstDate

  ModelTargetLastDate <- tail(index(na.trim(xTs[,ModelTarget])),1)
  # Later, I want to validate, so I save researve some dates(2007+)
  ModelTargetTrainTestLastDate <- min(as.Date("2006-12-31"), ModelTargetLastDate)

  #                                             I do not have any Predictee information earlier than this
  #                                             HARD-CODED(I just know this)        Desired end "2006-12-31", but actual end is "2001-11-30"
  #                                             as.Date("1970-12-31")
  # [ ] FIX: SHOULD BE renamed NBERAllData, NBERFocusedData
  NBERAllData     <- timeSliceNBER(allSlicesStart = ModelTargetTrainTestFirstDate, allSlicesEnd = ModelTargetTrainTestLastDate, LongTimeSlices = TRUE, OmitSliceFirstDate = TRUE)
  NBERFocusedData <- timeSliceNBER(allSlicesStart = ModelTargetTrainTestFirstDate, allSlicesEnd = ModelTargetTrainTestLastDate,                        OmitSliceFirstDate = TRUE)

  # should be min(earliest),max(latest) Date of (NBERAllData,NBERFocusedData)
  TrainingBegin <- min(head(NBERAllData[[1]],1), head(NBERFocusedData[[1]],1))
  TrainingEnd   <- max(tail(NBERAllData[[length(NBERAllData)]],1), tail(NBERAllData[[length(NBERFocusedData)]],1))
  # FIX: validation (zone) and exact records and (timeindex) needs to be nown
  # [ ] BETTER(BELOW) use intersect(dates) and "duplicated.data.frame"
  #     to determine if any UBL created records leak INTO the validation AREA

  # prepare for caret timeslices index and indexOut
  trControl <- NULL
  if(length(NBERAllData) == length(NBERFocusedData)) {
    NumbSlices <- length(NBERFocusedData)

    # ANDRE balancing

    # determine the Focused timeslices to (replicate)copy
    Data <- modelData(specifiedUnrateModel)
    TrainingData <- window(Data, start = TrainingBegin, end = TrainingEnd)
    for(slice in seq_len(NumbSlices)) {
      # x-num-ish it:(bind a 2nd,3rd,4th copy)
      # Torgo new 2018, 2017,2018 slides
      # To balance the data: how many replica copies do I need?
      # ANDRE DECISION
      # copy over enough so that the NBERFocusedData and and NBERAllData numbers of records are balanced
      NumbReplicaCopies <- ceiling((length(NBERAllData[[slice]]) - length(NBERFocusedData[[slice]]))/length(NBERFocusedData[[slice]]))
      FocusedDataOrigSliceData <- Data[NBERFocusedData[[slice]]]
      for(copy in seq_len(NumbReplicaCopies)) {
        TrainingData <- rbind(TrainingData, FocusedDataOrigSliceData)
      }

      # UBL functionons;  or 'create new observations'
      #                x-axis   y-axis
      #                 y-val,  rel(height), slope at height(y-axis)
      Relevance <- matrix(c(
                         -0.01, 1.0, 0.0, # negative y-values ( I care *much* about )
                          0.00, 0.5, 0.5,
                          0.01, 0.0, 0.0  # positive y-values ( I do not care much about )
                          )
                   , ncol = 3
                   , byrow = TRUE)

      # utility based learning

      UBLData <- cbind(as.data.frame(TrainingData), index = as.POSIXct(index(TrainingData))); row.names(UBLData) <- NULL
      # ? UBL::ImpSampRegress example
      UBLDataCompleteCases <- UBLData[complete.cases(UBLData),,drop = FALSE]

      # WERCS: WEighted Relevance-based Combination Strategy
      # values lhs of formula with values LESS than zero are MORE relevant (financial losses)
      # Therefore, I want new 150% percent MORE "financial loss data"
      # Keeping all of the financial profits
      # C.perc = list(1.0, 2.5))                # formula.tools:::as.character.formula
      UBLDataFormula <- as.formula(stringr::str_c(formula_tools___as_character_formula(formula(specifiedUnrateModel)), " + index")) # need the index to COPY
      UBLResults     <- UBL::ImpSampRegress(UBLDataFormula, UBLDataCompleteCases, rel = Relevance, thr.rel = 0.5,  C.perc = list(1.0, 2.5))
      # I AM ending up LOOSING some 'UBLDataCompleteCases' data. WHY?
      # NOTE: no NEW index Values are created.
      # I CAN NOT garnantee that all UBL functons do NOT do that
      UBLResultsIndex <- UBLResults[["index"]]
      UBLResults      <- UBLResults[, !colnames(UBLResults) %in% "index" , drop = FALSE]

      # convert back
      # need the xts column names
      # IF REMOVE ".fun = " then RStudio can debug
      plyr::llply(rlist::list.zip(UBLResult = UBLResults, ColName = colnames(UBLResults)),
        .fun = function(x) {
          res <- as.xts(x[["UBLResult"]], order.by = UBLResultsIndex)
          indexClass(res)  <- indexClass(TrainingData)
          indexFormat(res) <- indexFormat(TrainingData)
          # (+) non-core attributes (user) [if any]
          xtsAttributes(res) <- xtsAttributes(TrainingData)
          res <- rbind(res, Data[,x[["ColName"]]][TrainingEnd < index(Data)])
          return(res)
        }
      ) -> UpDatedSymbols
      # reorders in alphabetical order
      # MASSIVE CHANGE HERE ... mostly ZEROS in Final CALENDAR
      # update Symbols with UBLResults
      Symbols <- list2env(UpDatedSymbols)
    }

  } else {
    stop("\"length(NBERAllData) == length(NBERFocusedData)\" is not TRUE")
  }
  # FIX: check if ANY NEW UBL records found its way into the VALIDATION area
  # [ ] BETER OFF
  # FIX: SINCE UBL/OTHER can ADD/REMOVE records
  # [ ] BETTER OFF deciding THIS(validation records) early AND HARCODING THE DATES

  # determine slices of index and indexOut
  # pass through
  indexSlicesObs    <- NULL
  indexSlicesOutObs <- NULL

  # MY VISUAL OBSERVATION: model DOES *worse*: option; less observation to TRAIN/TEST over

                                                        # any UBL (or OTHER) functions that could have
                                                        # crept-in/created new observations that exist OUT-of-RANGE
                                                        # GARANTEED TO BE WITHIN c(TrainingBegin, TrainingEnd)
                                 # ONLY NBER DATE RANGES                     # Re-defining Training* to be more date-restrictev
  AllDataSliceTimeRanges <- plyr::llply(NBERAllData, function(x)  c(start= max(head(x,1),TrainingBegin), end = min(TrainingEnd,tail(x,1))) )
  # should be min(earliest)
  FirstLoop <- TRUE
  for(i in seq_along(AllDataSliceTimeRanges)) {
    if(FirstLoop) {
      TrainingBegin  <- AllDataSliceTimeRanges[[i]][["start"]]
      FirstLoop <- FALSE
    } else {
      TrainingBegin < min(TrainingBegin, AllDataSliceTimeRanges[[i]][["start"]])
    }
  }
  # should be max(latest)
  FirstLoop <- TRUE
  for(i in rev(seq_along(AllDataSliceTimeRanges))) {
    if(FirstLoop) {
      TrainingEnd  <- AllDataSliceTimeRanges[[i]][["end"]]
      FirstLoop <- FALSE
    } else {
      TrainingEnd < max(TrainingBegin, AllDataSliceTimeRanges[[i]][["end"]])
    }
  }

  # Update currently specified or built model with most recent data
  specifiedUnrateModel <- getModelData(specifiedUnrateModel, na.rm = FALSE, source.envir = Symbols)
                                                            # remove the last record(NO)
                                                            # "2007-01-31" (actual "2001-12-31")

  Data <- modelData(specifiedUnrateModel) # , data.window = c(TrainingBegin, TrainingEnd)
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
  # lousy: so I turned off
  indexSlicesObs    <- NULL
  indexSlicesOutObs <- NULL


  # weights

  # I am passing ( also when I decide that I am not sending weights )
  # pass through
  AdjustedWeightRankings <- NULL
                            #rep(1,NROW(window(Data, start = TrainingBegin, end = TrainingEnd)))
  #
  # xgboost weights ( using objective(y hieght) value to determine 'how much I care'(weights))
  # The weights are then
  #
  #   simply multiplied by the classification error at each iteration of the learning process.
  #
  #   Front Neurorobot. 2013; 7: 21.
  #   Published online 2013 Dec 4. doi:  10.3389/fnbot.2013.00021
  #   PMCID: PMC3885826
  #   Gradient boosting machines, a tutorial
  #   http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3885826/
  #
  Objectives <- as.vector(coredata(window(Data, start = TrainingBegin, end = TrainingEnd))[, ModelTarget])
  # lower values have lower rank numbers
  findInterval(x =  Objectives,
    # SEE MY NOTES: CURRENTLY REVIEWING different weights determiners
    # https://cran.r-project.org/web/packages/freqweights/freqweights.pdf
               vec = Hmisc::wtd.quantile(
                 x = Objectives,
                 weights = rep(1, NROW(Objectives)), # SEE MY NOTES
                 probs = c(0.00, 0.01, 0.10, 0.25, 0.75, 0.90, 0.99, 1.00),
                 na.rm = FALSE ),
              rightmost.closed = TRUE) %>%
                {.*(-1) } %>% { . + 8 } -> BareWeightRankings # values 7 through 1
                # lower numbers 'now' have higher rank numbers
  dplyr::case_when(
    BareWeightRankings == 7 ~ 30, # from 7  1%
    BareWeightRankings == 6 ~ 15, # from 6 10%
    BareWeightRankings == 5 ~  8, # from 5 25%
    TRUE                    ~ BareWeightRankings
  ) -> AdjustedWeightRankings #
  # to be sent as to buildModel.train, as
  # weights = AdjustedWeightRankings
  # if the model is xgboost [xgbTree], then it does USE it

  # fitting

  trControl  <- caret::trainControl(method = "cv", number = if(!is.null(indexSlicesObs)) { length(indexSlicesObs) } else { 5 },
                             index    = if(!is.null(indexSlicesObs))    { indexSlicesObs }    else { NULL },
                             indexOut = if(!is.null(indexSlicesOutObs)) { indexSlicesOutObs } else { NULL },
                             summaryFunction = SortinoRatioSummary # formals(caret::trainControl) # to put back non-NULL args
                             )
                                                    # first/last dates that the "predictee" dates are available
                                                    # "1970-12-31","2006-12-31"(actual "2001-11-30")
  message(stringr::str_c("Begin buildModel - ", as.character(formula(specifiedUnrateModel))), "")
  builtUnrateModel <- buildModel(specifiedUnrateModel,
                                 method="train",
                                 training.per=c(TrainingBegin, TrainingEnd),
                                 trControl = trControl,
                                 stage = "Test", # alternate  # "Production" "Test"
                                 weights = AdjustedWeightRankings, # weights
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
    data = as.data.frame(xTs[DescTools::DoCall(c,NBERAllData),  colnames(xTs) %in% builtUnrateModel@model.inputs], stringsAsFactor = FALSE),
    y =       c(coredata(xTs[DescTools::DoCall(c,NBERAllData),  colnames(xTs) %in% builtUnrateModel@model.target]))
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

  # just after TrainTest                               # FIX: SINCE UBL/OTHER can ADD/REMOVE records
                                                       # [ ] BETTER OFF deciding THIS(validation records) early AND HARCODING THE DATES
  ValidationPredictionBegin <- as.character(head(index(xTs[TrainingEnd < index(xTs)]),1))
  # lastest data, ModelTarget data can ( and in very  last data will ) be NA
  ValidationPredictionEnd   <- as.character(tail(index(xTs),1))

  ValidationData <- modelData(UpdatedModelData, data.window = c(ValidationPredictionBegin, ValidationPredictionEnd), exclude.training = TRUE)
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
    message(      "*** Now will do a na.locf() to complete it/them. ***")
    ValidationData <- cbind(ValidationData[, builtUnrateModel@model.target], na.locf(ValidationData[, builtUnrateModel@model.inputs]))
  }
  Fitted  <- predictModel(UpdatedModelData@fitted.model, ValidationData)
  Fitted  <- as.xts(Fitted, index(ValidationData))

  # uses S3 ifelse.xts
  # strategy/rule weights
  FittedOneSidedThreashold <- quantile(coredata(Fitted))["25%"]
  message("\n75% of the time, I am 'IN' the market.")
  message(stringr::str_c("FittedSignal OneSidedThreashold threashold is ", FittedOneSidedThreashold, "\n"))
  FittedSignal <- ifelse( Fitted > FittedOneSidedThreashold, rep(1,NROW(Fitted)), rep(0,NROW(Fitted)))

  colnames(FittedSignal)[1] <- stringr::str_c(ModelTarget, "_wts")

  logging::loginfo("End:   willShire5000MachineWts")

  FittedSignal

}, error = function(e) { ErrorHandler(e = e) } ) }



#' error handler function meant for tryCatchLog::tryCatchLog
#'
#' @param e input from
#' tryCatchLog::tryCatchLog(. . . , error = function(e) { ErrorHandler(e) })"
#' @param useENVI getOption("useENVI") option.
#' If NULL or TRUE, will use get("envi") to search for
#' the calling environment location that is stored in "envi"
#' Otherwise, will just get the calling environment from parent.frame(8)
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

  browser()

  invisible()
}





#' add Willshire 5000 Index log weights returns using eyeball (will5000idxlogrets)
#'
#' @param xTs xts object
#' @return xts object with merged data into xTs
#' @export
#' @importFrom tryCatchLog tryCatchLog
addWillShire5000EyeBallWts <- function(xTs = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  xTs <- initXts(xTs)
  addWts(xTs,willShire5000EyeBallWts(xTs))

})}


#' add Willshire 5000 Index log weights returns using Machine learning (will5000idxlogrets)
#'
#' @param xTs xts object
#' @return xts object with merged data into xTs
#' @export
addWillShire5000MachineWts <- function(xTs = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  xTs <- initXts(xTs)
  addWts(xTs,willShire5000MachineWts(xTs))

})}


#' add cash weights
#'
#' Currently, this number is just only all of the other weight
#' columns (*_wts) subtracted from one(1)
#'
#' @param xTs xts object
#' @return xts object with merged data into xTs
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_c
#' @importFrom stringr str_detect
cashWts <- function(xTs = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})
   xTs <- initXts(xTs)

  # excess left over
  cash_wts <- xts(rep(1,NROW(xTs)),index(xTs)) - rowSums(xTs[,wtsClms(xTs)], na.rm = TRUE)
  colnames(cash_wts)[1] <- stringr::str_c(colnames(xTs)[stringr::str_detect(colnames(xTs), "^cash.*rets$")], "_wts")

  cash_wts

})}


#' add cash weights returns (cashlogrets)
#'
#' @param xTs xts object
#' @return xts object with merged data into xTs
#' @export
#' @importFrom tryCatchLog tryCatchLog
appendCashWts  <- function(xTs = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})
  xTs  <- initXts(xTs)

  addWts(xTs, cashWts(xTs))

})}


#' show the last six(6) records
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
  xTs  <- initXts(xTs)

  message(stringr::str_c("tail of ", title))
  if(is.null(n)) n = 6
  options(digits = 5L)
  print(tail(xTs, n = n))

  invisible(xTs)

})}


#' show the last six(6) records
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
#' used internally
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


#' get the indicator columns
#'
#' columns that do not have a corresponding column having
#' its ending in "_wts" and do not have a do not have the
#' same root name compared to each and every other *_wts column.
#'
#' CURRENTLYNOT USED
#'
#' @param xTs xts objectt
#' @return column names
#' @examples
#' \dontrun{
#' # > require(xts)
#' # > xTs <- xts(matrix(1:3, ncol = 3, dimnames = list(NULL,c("a","b","b_wts"))),zoo::as.Date(0))[0]
#' # > indClms(xTs)
#' # [1] "a"
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
indClms <- function(xTs = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})
  xTs  <- initXts(xTs)
  clms <- safeClms(xTs)

  setdiff(clms, c(valueClms(xTs),wtsClms(xTs)))
})}


#' get the values columns names
#'
#' Values column names have an associated column with the
#' same root.  However the associated column always ends with
#' the suffix "_wts"
#'
#' @param xTs xts object
#' @return column names
#' @examples
#' \dontrun{
#' # > require(xts)
#' # > xTs <- xts(matrix(1:3, ncol = 3, dimnames = list(NULL,c("a","b","b_wts"))),zoo::as.Date(0))[0]
#' # > valueClms(xTs)
#' # [1] "b"
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
valueClms <- function(xTs = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})
  xTs  <- initXts(xTs)

  clms <- safeClms(xTs)
  clms <- sort(clms)

  stringr::str_replace(clms, "_wts$", "")[stringr::str_detect(clms, "_wts$")]

})}


#' get the weights(_wts) columns
#'
#' Weights column names always ends in "_wts"
#' CURRENTLYNOT USED ANYWHERE
#'
#' @param xTs xts object
#' @return column names
#' @examples
#' \dontrun{
#' # > require(xts)
#' # > xTs <- xts(matrix(1:3, ncol = 3, dimnames = list(NULL,c("a","b","b_wts"))),zoo::as.Date(0))[0]
#' # > wtsClms(xTs)
#' # [1] "b_wts
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_detect
wtsClms  <- function(xTs = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})
  xTs  <- initXts(xTs)

  clms <- safeClms(xTs)
  clms <- sort(clms)

  clms[stringr::str_detect(clms, "_wts$")]

})}
# clms <- c("b_wts","b","a_wts","a", "c")
# stopifnot(valueClms(clms),  c("a",    "b"    ))
# stopifnot(  wtsClms(clms),  c("a_wts","b_wts"))


#' get the porfolio log returns
#'
#' This calculated by taking the proporation of weights(0-1),
#' but typically a fraction and then multiplying this proportion
#' by its corresponding value column.  The sum of weights(_wts)
#' columns sum to be one(1).
#'
#' @param xTs xts object
#' @param initVal start value of the investor's porfolio
#' @return xts object of geometric returns
#' @export
#' @importFrom tryCatchLog tryCatchLog
portfolioLogReturns <- function(xTs = NULL, initVal = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  xTs  <- initXts(xTs)
  initVal <- initPorfVal(initVal)

  xTs <- xTs[,c(valueClms(xTs),wtsClms(xTs))]
  xTs <- xTs[complete.cases(xTs)]

  valuexTs <- xTs[,valueClms(xTs)]

  wtsxTs   <- xTs[,wtsClms(xTs)]
  # tomorrow morning: Return.portfolio requirement
  index(wtsxTs) <- index(wtsxTs) + 1

  xTs <- xTs[complete.cases(xTs)]
  Return.portfolio(
      R       = valuexTs
    , weights =   wtsxTs
    , value   = initVal
    , verbose = TRUE
  ) -> returns

  returns$returns

})}


#' get the porfolio non-log(arithmatic) returns
#'
#' @param xTs xts object
#' @param initVal start value of the investor's porfolio
#' @return xts object of arithmatic returns
#' @export
#' @importFrom tryCatchLog tryCatchLog
portfolioMonthlyReturns <- function(xTs = NULL, initVal = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})
  xTs  <- initXts(xTs)
  initVal <- initPorfVal(initVal)

  portLogRet1 <- portfolioLogReturns(xTs = xTs, initVal = initVal)

  monthlyReturn(exp(cumsum(portLogRet1)) * initVal)

})}


#' print calendar of month by mont returns
#'
#' vericle left side axis is in years
#' horizonl axis is in months
#'
#' Geometric adding is done across months
#' An end of year(12 month summary) is in the verticle right side axis
#'
#' @param xTs xts object
#' @param title heading
#' @return xts object of arithmatic returns
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_c
printCalendar <- function(xTs = NULL, title = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})
  xTs  <- initXts(xTs)

  message(stringr::str_c("calendar of ", title))
  options(digits = 5L)
  print(table.CalendarReturns(xTs, digits = 1, as.perc = TRUE, geometric = TRUE))

  invisible(xTs)

})}

