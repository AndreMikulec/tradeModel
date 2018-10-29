

#' garentee a date is a date or and empty date
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
initDate <- function(date = NULL) {
  tryCatchLog::tryCatchLog({
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


#' garentees return value value is a 'matrix of at least one dimension'or NULL
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
initEnv <- function(init = NULL, envir = rlang::caller_env()) {
  tryCatchLog::tryCatchLog({

  # require(quantmod) # zoo, xts, TTR
  # require(PerformanceAnalytics)
  # so, I can use rstudio projects of packages
  if(!"quantmod" %in% search())             require(quantmod)
  if(!"PerformanceAnalytics" %in% search()) require(PerformanceAnalytics)

  # debugging
  # 1st round (where the error occurs)
  # tryCatchLog
  # 2nd round (env variables around where the error occurred)
  # options(error = quote(dump.frames("testdump", TRUE)))
  # utils/html/debugger.html
  # 3rd round
  # browser(expr = { <where condition> }

  # convenience
  assign("syms", rlang::syms, envir = envir)
  assign("!!!", rlang::`!!!`, envir = envir)
  assign("parse_expr", rlang::parse_expr, envir = environment())
  assign("eval_bare",  rlang::eval_bare,  envir = environment())
  assign("caller_env", rlang::caller_env, envir = environment())

  assign("%>%",  magrittr::`%>%` , envir = envir)
  assign("%m+%", lubridate::`%m+%`, envir = envir)
  assign("str_detect", stringr::str_detect, envir = envir)
  assign("str_replace", stringr::str_replace, envir = envir)
  assign("str_replace_all", stringr::str_replace_all, envir = envir)
  assign("str_c", stringr::str_c, envir = envir)
  assign("as_tibble", tibble::as_tibble,  envir = envir)
  assign("arrange", dplyr::arrange, envir = envir)
  assign("map", purrr::map, envir = envir)
  assign("transpose", purrr::transpose, envir = envir)
  # assign("invoke_map", purrr::invoke_map, envir = envir)
  assign("Day", DescTools::Day, envir = envir)
  assign("LastDayOfMonth", DescTools::LastDayOfMonth, envir = envir)
  assign("DoCall", DescTools::DoCall, envir = envir)

  assign("vars_select", tidyselect::vars_select, envir = envir)
  assign("matches", tidyselect::matches, envir = envir)

  assign("select", dplyr::select, envir = envir)

  assign("select_se", seplyr::select_se, envir = envir)
  assign("deselect", seplyr::deselect, envir = envir)
  assign("let", wrapr::let, envir = envir)

  assign("wrap", R.utils::wrap, envir = envir)

  assign("unite", tidyr::unite, envir = envir)

  action <- parse_expr("assign(\"env\", environment())")
  eval_bare(action, caller_env())

  ops <- options()

  # tryCatchLog: what level to activate
  futile.logger::flog.threshold(futile.logger::ERROR)

  options(warn=2L)
  options(width=10000L) # LIMIT # Note: set Rterm(64 bit) as appropriate
  options(digits=if(is.null(init[["digits"]])) { 22L } else {init[["digits"]]})
  options(max.print=99999L)
  options(scipen=255L) # Try these = width
  #
  assign("ops", ops, envir = envir)

  #correct for TZ
  oldtz <- Sys.getenv("TZ")
  if(oldtz!="UTC") {
    Sys.setenv(TZ="UTC")
  }
  #
  assign("oldtz", oldtz, envir = envir)

  invisible()

})}


#' sets the printing enviroment
#'
#' space-saver - meant to be used at the beginning of a function
#'
#' @return printing environment is set
#' @examples
#' \dontrun{
#' # initPrintEnv()
#' }
#' @export
initPrintEnv <- function() {
  tryCatchLog::tryCatchLog({
  init <- list()
  init[["digits"]] <- 5L
  initEnv(init = init)
  assign("ops"  , ops,   envir = parent.frame())
  assign("oldtz", oldtz, envir = parent.frame())

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
uninitEnv <- function() {
  tryCatchLog::tryCatchLog({
  Sys.setenv(TZ=get("oldtz", envir = parent.frame()))
  options(get("ops", envir = parent.frame()))

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
logReturns <- function(xTs = NULL)  {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})
  xTs  <- initXts(xTs)

  xTsLogRets <- ROC(xTs)             # which(is.na(xTsindLogRets)) # logrithmic
  xTsLogRets[is.na(xTsLogRets)] <- 0 # usually just the 1st observation
  colnames(xTsLogRets)[1] <- paste0(colnames(xTsLogRets)[1], "logrets")

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
fredData <- function(Symbol = NULL) {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})
  if(is.null(Symbol)) stop("No fredData was requested")

  xTs <- getSymbols(Symbol, src = "FRED", from = "1950-01-01", auto.assign = FALSE) # SINCE DEC 1970
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
Lagging <- function(xTs = NULL, Shift = NULL) {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})
  xTs <- initXts(xTs)
  if(is.null(Shift)) Shift = 1
  # compare to quantmod:::Lag.xts
  if(periodicity(xTs)[["scale"]] == "monthly") {
    if(Day(head(index(xTs),1)) %in% c(28:31)) {
      refDates <- LastDayOfMonth(tail(index(xTs),1) %m+% months( 1 * seq_len(Shift)) )
    }
    xTs <- merge(xTs, xts(, refDates) )
  }
  xTs %>% { lag(., 1 * Shift) } -> xTs
  if(str_detect(colnames(xTs)[1], "leadingrets$")) {
    colnames(xTs)[1] <- str_replace(colnames(xTs)[1], "leadingrets$", "rets")
  } else {
    colnames(xTs)[1] <- str_replace(colnames(xTs)[1], "rets$", "laggingrets")
  }
  xTs
})}



#' generate a good xts column name
#'
#' @param x single column xts with the old column name
#' @return single column xts with  the new column name
#' @examples
#' \dontrun{
#'
#' }
#' @export
newXtsColName <- function(xTs = NULL, Fun =  NULL, isCharFun = NULL, xTs1 = NULL, xTs2 = NULL, WhichCombo =  NULL, AltName = NULL, Prefix = NULL, FixedSep = NULL) {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})

  if(is.null(isCharFun)) stop("newXtsColName need actual paramter isCharFun")

  if(is.null(AltName)) {
    if(isCharFun) {
      NewName <- str_replace_all(Fun,"[.]|::",FixedSep)
    } else {
      NewName <- "anon"
    }
  } else {
    NewName <- AltName
  }

  str_c(
  c(
    if(!is.null(xTs1) && (NCOL(xTs1) > 0)) { colnames(xTs1)[1] } else { NULL },
    if(!is.null(xTs2) && (NCOL(xTs2) > 0)) { colnames(xTs2)[1] } else { NULL }
  ), collapse = FixedSep) -> Colnames

  if(length(WhichCombo)) {
    WhichCombo <-  str_c(c(interleave(names(WhichCombo), unlist(WhichCombo))), collapse = FixedSep)
  } else {
    WhichCombo <- NULL
  }

  PreName <- NULL; PostName <- NULL
  NewNameWhichCombo <- str_c(c(NewName, WhichCombo), collapse = FixedSep)
  if(is.null(Prefix) || (Prefix == FALSE)) {
    PostName <- NewNameWhichCombo
  } else {
    PreName  <- NewNameWhichCombo
  }
  NewName <- str_c(c(PreName, Colnames, PostName), collapse = FixedSep)
  colnames(xTs)[1] <-NewName

  xTs

})}



#' pairwise interleave of columns
#'
#' works better on xts objects ( lapply structure is held together )
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
pairWise <- function(x1, x2) {

  List <- c(list(),lapply(x1, identity), lapply(x2, identity))
  # also works and parallelizable
  # List <- c(list(), plyr::llply(x1, identity), plyr::llply(x2, identity))
  # xts FAILS . . .
  # List <- c(list(), purrr::map(x1, ~{identity(x = .x)}), purrr::map(x2, ~{identity(x = .x)}))

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

#' flatten a data.frame to (hopfully) ONE row
#'
#' NOTE: only WORKS with ONE unique primary key value
#' If I want to work with many primary key valuess,
#' then one would need to split the data upon those values
#' then next apply this function individually to each chunk
#' Last, 'smart' merge all of the data.frames together (while
#' adding new columns contributed by each data.frame).
#'
#' @examples
#' \dontrun{
#'#
#'# data.frame(
#'#   dateindexid = c(17000, 17000, 17000, 17000),
#'#   dateindexFact = c("2016-07-18", "2016-07-18", "2016-07-18", "2016-07-18"),
#'#   ActionFact = c("Morn", "Morn", "Night", "Night"),
#'#   ActionAtFact = c("Sell", "Buy", "Sell", "Buy"),
#'#   INSTR1 = c(8, 16, 32, 64),
#'#   INSTR2 = c(10008, 10016, 10032, 10064)
#'# , stringsAsFactors = FALSE
#'# )  -> DFS
#'#
#'# > DFS
#'#   dateindexid dateindexFact ActionFact ActionAtFact INSTR1 INSTR2
#'# 1       17000    2016-07-18       Morn         Sell      8  10008
#'# 2       17000    2016-07-18       Morn          Buy     16  10016
#'# 3       17000    2016-07-18      Night         Sell     32  10032
#'# 4       17000    2016-07-18      Night          Buy     64  10064
#'#
#'#
#'# > liquifyDF(DFS)
#'#   dateindexid dateindexFact Morn.Sell.INSTR1 Morn.Buy.INSTR1 Night.Sell.INSTR1 Night.Buy.INSTR1
#'# 1       17000    2016-07-18                8              16                32               64
#'#   Morn.Sell.INSTR2 Morn.Buy.INSTR2 Night.Sell.INSTR2 Night.Buy.INSTR2
#'# 1            10008           10016             10032            10064
#'#
#'#
#' }
#'@export
liquifyDF <- function(x, ConstColsRegex = "^dateindex", FactorColsRegex = "Fact$"
                       , FactorColsNAReplace = NULL
                       , FactorColsFixedSep = "."
                       , DetailColsFixedSep = "."
                       , SpaceFixedSep = "."
                       , AmperstandFixedSep = "And"
                      ) {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})

  if(NROW(x) == 0) { message("liquifyDF found zero rows"); return(data.frame()) }

  # typically "id" columns
  ConstCols <- vars_select(names(x), matches(ConstColsRegex))
                     # garantee no 'id' columns ( and the [rest of] factors )
  FCT_COLS_VECTOR <- setdiff(tidyselect::vars_select(names(x), matches(FactorColsRegex)), ConstCols)
  FCT_COLS_NAME   <- str_c(FCT_COLS_VECTOR, collapse = FactorColsFixedSep)
  FCT_COLS_SEP    <- str_c(FCT_COLS_VECTOR, collapse = ", ")

  if(!is.null(FactorColsNAReplace)) {
    for(coli in FCT_COLS_VECTOR) {
      x[is.na(x[, coli, drop = TRUE]),coli] <- FactorColsNAReplace
    }
  }

  LeftSideRow1  <-  select_se(x, ConstCols)[1, , drop = FALSE]
  NotLeftSide   <-  deselect(x, ConstCols)

  UNITE <- function(x) {
    let(list(FCT_COLS_NAME = FCT_COLS_NAME, FCT_COLS_SEP = FCT_COLS_SEP),
      unite(x, FCT_COLS_NAME, FCT_COLS_SEP, sep = FactorColsFixedSep)
    , subsMethod = "stringsubs", strict = FALSE)
    }

  # make ONE column to represent all factors
  NotLeftSide %>% UNITE %>%
    # change row.names to FCT_COLS_NAME, drop column 1
    `row.names<-`(.[[1]]) %>% select(-1) %>%
      # to one dimension : one BIG wide ROW
      as.matrix %>% wrap(sep = DetailColsFixedSep) -> NotLeftSide

  cbind(LeftSideRow1,as.data.frame(t(NotLeftSide))) -> results

  colnames(results) <- str_replace_all(colnames(results),"[ ]", SpaceFixedSep)
  colnames(results) <- str_replace_all(colnames(results),"&"  , AmperstandFixedSep)

  return(results)

})}



#' (true) sortino ratio
#'
#' Sortino Ratio: Are you calculating it wrong?
#' https://www.rcmalternatives.com/2013/09/sortino-ratio-are-you-calculating-it-wrong/
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
    (mean(x, na.rm = na.rm) - rf )/sd( local({x[x > 0] <- 0; x } ), na.rm = na.rm)
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
#'# > rollApply(xts(c(0,-1,2,-3,4), zoo::as.Date(0:4)), Fun = trueSortinoRatio, AltName = "SRTN", partial = TRUE, width = 2, MinRows = 2)
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
#'# rollApply(xts(c(0,1,NA,3,4), zoo::as.Date(0:4)), Fun = DSMA, AltName = "MATH", partial = FALSE, width = 2, n = 2, QTY = "Last")
#'#                                                                              # because I programmed around THIS
#'# rollApply(xts(c(0,1,NA,3,4), zoo::as.Date(0:4)), Fun = DSMA, AltName = "MATH", partial = TRUE, width = 2, n = 2,QTY = "Last")
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
rollApply <- function(
        xTs = NULL, width = NULL, partial = NULL, align = NULL, Fun = NULL
      , by = NULL, by.column = NULL, fill = NULL, coredata = NULL
      , AltName = NULL, FixedSep  = NULL
      , ...) {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})

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

  # would/should always be/been true else I may/have/never ever made it his far
  if(tryXtsSuccess) {
    reclass(xTsResult, xTsOrig)
  } -> xTsResult

  NewName <- "rollApply"
  if(!is.null(AltName)) NewName <- AltName
  if(is.null(FixedSep)) FixedSep <- "."

  colnames(xTsResult) <-  str_c( colnames(xTsResult), str_c(c(NewName, width), collapse = FixedSep), collapse = FixedSep)

  return(xTsResult)

})}





#' expland out xts
#'
#' from an xts function stub, create an ets object of derived columns
#'
#' Meant to create many package TTR technical trading column results
#' CURRENTLY NOT USED
#'
#' Ideas are from
#'
#' Time series cross-validation 5
#' January 24, 2013
#' By Deane-Mayer
#' http://www.r-bloggers.com/time-series-cross-validation-5/
#' http://moderntoolmaking.blogspot.com/2013/01/time-series-cross-validation-5.html?utm_source=feedburner&utm_medium=feed&utm_campaign=Feed%3A+ModernToolMaking+%28Modern+Tool+Making%29
#'
#' xTs1 (and xTs2 'if any' are paired/matched column position to column position)
#'
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

  DoCall(expand.grid, Whiches) %>%
      as.list %>%
        transpose -> WhichesCombinations
  if(!NCOL(WhichesCombinations)){ return(initXts()) }

  if(mode(Fun) == "function") {
    Fun = match.fun(Fun)
    isCharFun <- FALSE
  } else {
    isCharFun <- TRUE
  }

  xTs <- initXts()
  FunctionEnv <- environment()

  lapply(WhichesCombinations, function(WhichCombo) {

    lapply(pairWise(xTs1, xTs2), function(xTsColumnSet) {
      xTs1 <- xTsColumnSet[[1]]; xTs2 <- xTsColumnSet[[2]]

      if(NVAR(xTs2)) { xTs2List <- list(xTs2) } else { xTs2List <- NULL }
      Temp <- DoCall(Fun, args = c(list(), list(xTs1), xTs2List, WhichCombo, list(...)), quote = quote, envir = envir)

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
Leading <- function(xTs = NULL, Shift = NULL) {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})
  xTs <- initXts(xTs)
  if(is.null(Shift)) Shift = 1
  # compare to quantmod:::Lag.xts
  if(periodicity(xTs)[["scale"]] == "monthly") {
    if(Day(head(index(xTs),1)) %in% c(28:31)) {
      refDates <- LastDayOfMonth(head(index(xTs),1) %m+% months(-1 * seq_len(Shift)) )
    }
    xTs <- merge(xTs, xts(, refDates) )
  }
  xTs %>% { lag(.,-1 * Shift) } -> xTs
  if(str_detect(colnames(xTs)[1], "leadingrets$")) {
    colnames(xTs)[1] <- str_replace(colnames(xTs)[1], "laggingrets$", "rets")
  } else {
    colnames(xTs)[1] <- str_replace(colnames(xTs)[1], "rets$", "leadingrets")
  }
  xTs
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
wilshire5000indEomData <- function() {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})

  fredEomData(Symbol = "WILL5000IND")

})}


# NOTE: I may want to change to function(x, ...)

#' change the index date to the future
#'
#' @param xTs xts object or data object
#' @return xts object or data object with the index moved up
#' @rdname nextMonthfromYesterday
#' @export
nextMonthfromYesterday <- function(xTs = NULL) {
  # tryCatchLog is not allowed here
  UseMethod("nextMonthfromYesterday")

}


#' @rdname nextMonthfromYesterday
#' @export
nextMonthfromYesterday.default <- function(xTs = NULL) {
  tryCatchLog::tryCatchLog({
  stop("No nextMonthfromYesterday method for <input>")

})}


#' @param date date object
#' @rdname nextMonthfromYesterday
#' @examples
#' \dontrun{
#' # > require(xts)
#' # > nextMonthfromYesterday(zoo::as.Date("1970-01-12"))
#' # [1] "1970-01-31"
#' }
#' @export
nextMonthfromYesterday.Date <- function(date = NULL) {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})
  date <- initDate(date)

  Hmisc::truncPOSIXt(date, units = "months") %>%
    { zoo::as.Date(.)} %m+%
      months(1) %m+%
        lubridate::days(-1)

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
nextMonthfromYesterday.xts <- function(xTs = NULL) {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})
  xTs  <- initXts(xTs)

  lapply(index(xTs), nextMonthfromYesterday) %>%
    { do.call(c,.) } %>%
      { xts(Coredata(xTs),.) }

})}


#' get the end of month UNRATE from FRED
#'
#' @return xts object
#' @export
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

  colnames(unrate_wts)[1] <- str_c(colnames(xTs)[str_detect(colnames(xTs), "^will5000idx.*rets$")], "_wts")

  unrate_wts

})}


#' SMAs of the unrate Eyeball Indicator
#'
#' @export
unrateEyeballIndicators <- function(unrate = NULL) {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})

  unrate <- initXts(unrate)

  # can not do math on leading NAs
  # (actually can not do any math on 'any' NAs)
  unrate <- na.trim(unrate)

  unrate1Indicator <- Less(SMA(    unrate   ,2), SMA(    unrate   ,6))
  colnames(unrate1Indicator) <- "unrate1"
  unrate2Indicator <- Less(SMA(lag(unrate)  ,2), SMA(lag(unrate  ),6))
  colnames(unrate2Indicator) <- "unrate2"
  unrate3Indicator <- Less(SMA(lag(unrate,2),2), SMA(lag(unrate,2),6))
  colnames(unrate3Indicator) <- "unrate3"

  merge(unrate1Indicator, unrate2Indicator, unrate3Indicator)

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
willShire5000MachineWts <- function(xTs = NULL) {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})

  # received xTs( will5000idxlogrets, cashlogrets, unrate )
  # merge.xts target, indictors, and predictors into merged xTs

  unrate <- xTs[,"unrate"]
  unrateIndicators <- unrateEyeballIndicators(unrate)
  unrateIndicators <- initXts(unrateIndicators)

  # all indicators
  Indicators <- unrateIndicators
  xTs <- merge(xTs, Indicators)

  xTs <- initXts(xTs)

  # will5000idxlogrets late partial return is not useful
  # unrate NA at will5000idxlogrets late partial return is not useful
  # cashlogrets             obvious [future] return is not useful

  # create an environment of xts objects
  map(as.data.frame(xTs),
    ~ (function(x) {
        as.xts(x, order.by = index(xTs))
      })(x = .x)
  ) -> Symbols

  # reorders in alphabetical order
  Symbols <- list2env(Symbols)

  # traditionally the first column is the target variable
  specifyModel(formula = as.formula(str_c( colnames(xTs)[1], " ~ ",str_c(colnames(Indicators), collapse = " + ")))
            , na.rm = FALSE, source.envir = Symbols) ->
              # remove the last record(NO)
  specifiedUnrateModel

  builtUnrateModel <- buildModel(specifiedUnrateModel, method="train", training.per=c("1970-12-31","2006-12-31"))

  # Update currently specified or built model with most recent data
  UpdatedModelData <- getModelData(builtUnrateModel, na.rm = FALSE, source.envir = Symbols)
                                                     # remove the last record(NO)

  modelData <- modelData(UpdatedModelData, data.window = c("2007-01-31", as.character(tail(index(xTs),1))), exclude.training = TRUE)

  Fitted  <- predictModel(UpdatedModelData@fitted.model, modelData)
  Fitted  <- as.xts(Fitted, index(modelData))

  # uses S3 ifelse.xts
  # strategy/rule weights
  Fitted <- ifelse(Fitted > 0, rep(1,NROW(Fitted)), rep(0,NROW(Fitted)))

  colnames(Fitted)[1] <- str_c(colnames(xTs)[str_detect(colnames(xTs), "^will5000idx.*rets$")], "_wts")

  Fitted

})}



#' add Willshire 5000 Index log weights returns using eyeball (will5000idxlogrets)
#'
#' @param xTs xts object
#' @return xts object with merged data into xTs
#' @export
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
cashWts <- function(xTs = NULL) {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})
   xTs <- initXts(xTs)

  # excess left over
  cash_wts <- xts(rep(1,NROW(xTs)),index(xTs)) - rowSums(xTs[,wtsClms(xTs)], na.rm = TRUE)
  colnames(cash_wts)[1] <- str_c(colnames(xTs)[str_detect(colnames(xTs), "^cash.*rets$")], "_wts")

  cash_wts

})}


#' add cash weights returns (cashlogrets)
#'
#' @param xTs xts object
#' @return xts object with merged data into xTs
#' @export
addCashWts  <- function(xTs = NULL) {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})
  xTs  <- initXts(xTs)

  addWts(xTs, cashWts(xTs))

})}


#' show the last six(6) records
#'
#' @param xTs xts object
#' @param title heading
#' @return invisible xts object
#' @export
printTail <- function(xTs = NULL, title = NULL) {
  tryCatchLog::tryCatchLog({
  initPrintEnv();on.exit({uninitEnv()})
  xTs  <- initXts(xTs)

  message(paste0("tail of ", title))
  print(tail(xTs))

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
initPorfVal <- function(initVal = NULL) {
  tryCatchLog::tryCatchLog({
  if(is.null(initVal)) {
    initVal <- 100000
  } else if(initVal <= 0) {
    stop("initVal should be a large number.")
  } else { }

  initVal

})}


#' sort
#'
#' @export
Sort <- function(x) {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})

  vars <- syms("value")
  as_tibble(x) %>%
    arrange(!!!vars)  %>%
      list %>%
        { DoCall(c, .) } -> ret

   ret

})}


#' get the column names
#'
#' used internally
#'
#' @param xTs xts object
#' @return zero length vector or column names
#' @export
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
valueClms <- function(xTs = NULL) {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})
  xTs  <- initXts(xTs)

  clms <- safeClms(xTs)
  clms <- Sort(clms)

  str_replace(clms, "_wts$", "")[str_detect(clms, "_wts$")]

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
wtsClms  <- function(xTs = NULL) {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})
  xTs  <- initXts(xTs)

  clms <- safeClms(xTs)
  clms <- Sort(clms)

  clms[str_detect(clms, "_wts$")]

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
portfolioMonthlyReturns <- function(xTs = NULL, initVal = NULL) {
  tryCatchLog::tryCatchLog({
  env = environment()
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
printCalendar <- function(xTs = NULL, title = NULL) {
  tryCatchLog::tryCatchLog({
  env = environment()
  initPrintEnv();on.exit({uninitEnv()})
  xTs  <- initXts(xTs)

  message(paste0("calendar of ", title))
  print(table.CalendarReturns(xTs, digits = 1, as.perc = TRUE, geometric = TRUE))

  invisible(xTs)

})}

