

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
initEnv <- function(init = NULL, envir = parent.frame()) {
  tryCatchLog::tryCatchLog({

  require(quantmod) # zoo, xts, TTR
  require(PerformanceAnalytics)

  # debugging
  # 1st round (where the error occurs)
  # tryCatchLog
  # 2nd round (env variables around where the error occurred)
  # options(error = quote(dump.frames("testdump", TRUE)))
  # utils/html/debugger.html
  # 3rd round
  # browser(expr = { <where condition> }

  # convenience
  assign("%>%",  magrittr::`%>%` , envir = envir)
  assign("%m+%", lubridate::`%m+%`, envir = envir)

  assign("parse_expr", rlang::parse_expr, envir = environment())
  assign("eval_bare",  rlang::eval_bare,  envir = environment())
  assign("caller_env", rlang::caller_env, envir = environment())
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



#' leading
#'
#' @export
Leading <- function(xTs = NULL) {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})
  xTs %>% { lag(.,-1) }
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
#' @param xTs xts object
#' @return xts object with the index moved up
#' @rdname nextMonthfromYesterday
#' @export
nextMonthfromYesterday <- function(xTs = NULL) {
  # tryCatchLog is not allowed here
  UseMethod("nextMonthfromYesterday")

}


#' attempt to change the index date to the future
#'
#' @param xTs default object
#' @return stop
#' @rdname nextMonthfromYesterday
#' @export
nextMonthfromYesterday.default <- function(xTs = NULL) {
  tryCatchLog::tryCatchLog({
  stop("No nextMonthfromYesterday method for xTs")

})}


#' change the date to the future
#'
#' @param date date object
#' @rdname nextMonthfromYesterday
#' @return date object
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


#' change the date to the future
#'
#' @param xTs xts object
#' @rdname nextMonthfromYesterday
#' @return date object
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

  colnames(unrate_wts)[1] <- "will5000idxlogrets_wts"

  unrate_wts

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

  # can not do math on leading NAs
  # (actually can not do any math on 'any' NAs)
  unrate <- na.trim(unrate)

  unrate1Indicator <- Less(SMA(    unrate   ,2), SMA(    unrate   ,6))
  colnames(unrate1Indicator) <- "unrate1"
  unrate2Indicator <- Less(SMA(lag(unrate)  ,2), SMA(lag(unrate  ),6))
  colnames(unrate2Indicator) <- "unrate2"
  unrate3Indicator <- Less(SMA(lag(unrate,2),2), SMA(lag(unrate,2),6))
  colnames(unrate3Indicator) <- "unrate3"

  xTs <- merge(xTs, unrate1Indicator, unrate2Indicator, unrate3Indicator)
  xTs <- initXts(xTs)
  # will5000idxlogrets late partial return is not useful
  # unrate NA at will5000idxlogrets late partial return is not useful
  # cashlogrets             obvious [future] return is not useful
  ### xTs <- initXts(na.trim(xTs))
  ### xTs <- initXts(xTs[index(xTs) <= Sys.Date()])

  # modelData does not automatically
  #   add 'dates through now' (if they do not alread exist)
  # so I will do that here
  # seq(from = tail(index(xTs),1) + 1, to = Sys.Date(), by = "1 month") - 1 %>%
  #   initDate -> datesThroughNow
  # xTs <- initXts(merge(xTs, xts(, datesThroughNow)))
  # #   add 'future] dates'
  # datesNowThroughLater <- seq(from = last(index(xTs)) + 1, to = Sys.Date() + 134 , by = "1 month") - 1 %>%
  #   initDate -> datesNowThroughLater
  # # Date overlap is O.K.
  # xTs <- initXts(merge(xTs, xts(, datesNowThroughLater)))

  # create an environment of xts objects
  Symbols <- lapply(as.data.frame(xTs), function(x) {
    as.xts(x, order.by = index(xTs))
  })
  Symbols <- list2env(Symbols)

  specifiedUnrateModel <- specifyModel(formula = will5000idxlogrets ~ unrate1 + unrate2 + unrate3
                                     , na.rm = FALSE, source.envir = Symbols)
                                     # remove the last record
  tg <- expand.grid(
    nrounds   =  100,
    eta       =  c(0.1,0.01),
    max_depth =  c(4,6,8,10),
    gamma     =  0,
    colsample_bytree = c(1,0.5),
    min_child_weight = 1,
    subsample        = c(1,0.5)
  )
  tc <- caret::trainControl(method = "cv", number = 5)

  builtUnrateModel <- buildModel(specifiedUnrateModel,method="train", training.per=c("1970-12-31","2006-12-31")
                               , method_train = 'xgbTree', tuneGrid = tg, trControl = tc)

  # Update currently specified or built model with most recent data
  UpdatedModelData <- getModelData(builtUnrateModel, na.rm = FALSE, source.envir = Symbols)
                                                             # remove the last record

  modelData <- modelData(UpdatedModelData, data.window = c("2007-01-31", as.character(tail(index(xTs),1))), exclude.training = TRUE)

  Fitted  <- predictModel(UpdatedModelData@fitted.model, modelData)
  Fitted  <- as.xts(Fitted, index(modelData))

  # uses S3 ifelse.xts
  # strategy/rule weights
  Fitted <- ifelse(Fitted > 0, rep(1,NROW(Fitted)), rep(0,NROW(Fitted)))

  colnames(Fitted)[1] <- "will5000idxlogrets_wts"

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
  colnames(cash_wts)[1] <- "cashlogrets_wts"

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

  sub("_wts$", "", clms)[grepl("_wts$", clms)]

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

  clms[grepl("_wts$",clms)]

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

