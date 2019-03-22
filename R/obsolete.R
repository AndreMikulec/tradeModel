


#' get data from the St. Louis FRED
#'
#' @description
#' \preformatted{
#' RETIRED - NOT TO BE USED
#' }
#'
#' @param Symbol FRED symbol as a string
#' @param New TRUE(default) of the Symbol gets the data and stores the data
#' through the method of getNewSymbols (TRUE).  Otherwise,
#' get the data and store the data through the method of getNewSymbols (FALSE).
#' @param NewMaxAge default("4 hours"), if New = TRUE, then this
#' is the timeout period of the stored data. After NewMaxAge time has passed,
#' the the data will be re-queried from the St. Louis FRED.
#' of the data
#' @param ... dots passed
#' @return xts object of results
#' @examples
#' \dontrun{
#' EXAMPLE
#' # > head(symbolData("GDP", src = "FRED"),1)
#' #                           gdp
#' # 1947-01-01 243.16399999999999
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
#' # full test
#' # 1. remove cache data: rm(.UNRATE) (if there)
#' # 2. drop database table "Symbols"."UNRATE"
#' # 3. remove corresponding record from "Symbols"."Symbols"
#' # 4. symbolData(Symbol = "UNRATE", src = "FRED")
#'
#' # partial tests
#'
#' 2.1 remove some (few) bottom records from the
#' database table "Symbols"."UNRATE"
#' just insert only-new (few) records
#' # placeNewRecords == "AddOnlyNew" (default)
#'
#' symbolData(Symbol = "UNRATE", src = "FRED",NewMaxAge = "1 secs")
#'
#' # or
#'
#' # 2.2
#' # placeNewRecords =="TruncateTable"
#'
#' symbolData(Symbol = "UNRATE", src = "FRED", NewMaxAge = "1 secs", placeNewRecords = "TruncateTable")
#'
#' # "AddNewUpdateOld"
#'
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_c
fredData <- function(Symbol = NULL, New = NULL, NewMaxAge = NULL, ...) {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})
  if(is.null(Symbol)) stop("No fredData was requested")

  if(is.null(New)) New <- TRUE
  if(New) {
    if(is.null(NewMaxAge)) {
    NewMaxAge <- "4 hours"
    } # else NewMaxAge <- NewMaxAge
  }

  if(length(Symbol) > 1) stop ("fredData can only download one symbol at a time.")

  message(stringr::str_c("Begin fredData - ", Symbol))

  src = "FRED"
  from = "1950-01-01"
  # NOTE, if Symbol == "WILL5000IND" # SINCE DEC 1970

  if(New){
    xTs <- getNewSymbols(Symbol, src = "FRED",
           from = from, auto.assign = FALSE, MaxAge = NewMaxAge, ...)
  } else {
    xTs <- getSymbols(Symbol, src = "FRED",
           from = from, auto.assign = FALSE, ...)
  }
  message(stringr::str_c("End   fredData - ", Symbol))

  # colnames(xTs)[1] <- tolower(colnames(xTs))
  xTs

})}



#' get data from Yahoo corporation
#'
#' @description
#' \preformatted{
#'  RETIRED - NOT TO BE USED
#'  WORKS just like fredData
#' }
#'
#' @param Symbol FRED symbol as a string
#' @param New TRUE(default) of the Symbol gets the data and stores the data
#' through the method of getNewSymbols (TRUE).  Otherwise,
#' get the data and store the data through the method of getNewSymbols (FALSE).
#' @param NewMaxAge default("4 hours"), if New = TRUE, then this
#' is the timeout period of the stored data. After NewMaxAge time has passed,
#' the the data will be re-queried from Yahoo corporation.
#' of the data
#' @param ... dots passed
#' @return xts object of results
#' @examples
#' \dontrun{
#'
#' # WORKS just like fredData
#'
#' yahooData(Symbol = "^GSPC")
#'
#' yahooData(Symbol = "^GSPC", NewMaxAge = "1 secs")
#'
#' yahooData(Symbol = "^GSPC", NewMaxAge = "1 secs", placeNewRecords = "TruncateTable")
#'
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_replace str_c
#' @importFrom stringr str_c
yahooData <- function(Symbol = NULL, New = NULL, NewMaxAge = NULL, ...) {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})
  if(is.null(Symbol)) stop("No yahooData was requested")

  if(is.null(New)) New <- TRUE
  if(New) {
    if(is.null(NewMaxAge)) {
    NewMaxAge <- "4 hours"
    } # else NewMaxAge <- NewMaxAge
  }

  if(length(Symbol) > 1) stop ("yahooData can only download one symbol at a time.")

  message(stringr::str_c("Begin yahooData - ", Symbol))

  src = "yahoo"
  from = "1950-01-01"

  if(New){
    xTs <- getNewSymbols(Symbol, src = "yahoo",
           from = from, auto.assign = FALSE, MaxAge = NewMaxAge, ...)
  } else {
    xTs <- getSymbols(Symbol, src = "yahoo",
           from = from, auto.assign = FALSE, ...)
  }

  if((NVAR(xTs) > 1) && any(colnames(xTs) %Like% "Close$")) {
    # since Yahoo 'often' does return an OHLC object
      # there will ONLY be one column 'like' Close
      xTs <- xTs[, colnames(xTs) %Like% "Close$"]
      if(NVAR(xTs) > 1) {
          stop(stringr::str_c("yahooData in Symbol: ", Symbol, "has detected TOO MANY columns like 'Close'"))
        }
      # a Yahoo [stock index] may(usually) has a caret(^)
      NewColName <- stringr::str_replace(Symbol, "\\^", "")
      colnames(xTs)[1] <- NewColName
  }

  message(stringr::str_c("End   yahooData - ", Symbol))

  xTs

})}



#' get the latest value in the month from FRED
#'
#' @description
#' \preformatted{
#'
#' # Units: Index, Not Seasonally Adjusted
#' # Frequency: Daily, Close
#'
#' }
#'
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
  symbolData(Symbol = Symbol, src = "FRED") %>%
     eomData

})}



#' get the latest value in the month from FRED
#'
#' @description
#' \preformatted{
#'
#' # Units: Index, Not Seasonally Adjusted
#' # Frequency: Daily, Close
#'
#' }
#'
#' @param Symbol FRED symbol as a string
#' @return xts object of the last observation with the
#' month date rounded up to the last day of the month
#' @examples
#' \dontrun{
#' EXAMPLE
#' # # weekly
#' # > gspc <- head(yahooEomData("^GSPC"),1)
#' # > gspc
#' #
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
yahooEomData <- function(Symbol = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  if(is.null(Symbol)) stop("No yahooData was requested")
  yahooData(Symbol = Symbol) %>%
     eomData

})}



#' Get the Wilshire 5000 Index eom price from FRED
#'
#' @description
#' \preformatted{
#'
#' }
#'
#' @return xts object of end of month returns
#' @export
#' @importFrom tryCatchLog tryCatchLog
wilshire5000indEomData <- function() {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  # Units: Index, Not Seasonally Adjusted
  # Frequency: Daily, Close
  fredEomData(Symbol = "WILL5000IND")

})}


#' Get the S&P 500 Index eom price from Yahoo
#'
#' @description
#' \preformatted{
#'
#' }
#'
#' @return xts object of end of month returns
#' @export
#' @importFrom tryCatchLog tryCatchLog
SP500EomData <- function() {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  # Units: Index, Not Seasonally Adjusted
  # Frequency: Daily, Close
  yahooEomData(Symbol = "^GSPC")

})}



#' get the Willshire 5000 Index log returns
#'
#' @description
#' \preformatted{
#'
#' }
#'
#' @return xts object
#' @export
#' @importFrom tryCatchLog tryCatchLog
wilshire5000LogReturns <- function() {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  will5000ind <- wilshire5000indEomData()
  # colnames(will5000ind)[1] <- "WILL5000IND"

  logReturns(xTs = will5000ind)

})}



#' get the SP500 log returns
#'
#' @description
#' \preformatted{
#'
#' }
#'
#' @return xts object
#' @export
#' @importFrom tryCatchLog tryCatchLog
SP500LogReturns <- function() {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  sp500 <- SP500EomData()
  # WOULD prefer to add: 2% year for dividends
  # actually:
  #   for each element
  #     divide by 220 working days of the year
  #       continuously compound that value
  #         and SUM the elements along the way
  # I DO NOT know the math.
  # Skip for now. Add in the future.
  # Ask this questions in an online MATH/FINANCE forum

  logReturns(xTs = sp500)

})}



#' add Willshire 5000 Index log returns (WILL5000INDlogrets)
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
addWilshire5000LogReturns <- function(xTs = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  xTs  <- initXts(xTs)

                         # WILL5000INDlogrets
  combineXts(xTs, wilshire5000LogReturns())

})}



#' add SP500 log returns (GSPClogrets)
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
addSP500LogReturns <- function(xTs = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  xTs  <- initXts(xTs)

                  # GSPClogrets
  combineXts(xTs, SP500LogReturns())

})}



#' join two Log Returned xts objects
#'
#' @description
#' \preformatted{
#'
#' }
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





#' get the SP500 leading log returns
#'
#' @description
#' \preformatted{
#'
#' }
#'
#' @return xts object
#' @export
#' @importFrom tryCatchLog tryCatchLog
leadingSP500LogReturns <- function() {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  SP500LogReturns() %>% Leading

})}



#' get the Wilshire 5000 Index leading log returns
#'
#' @description
#' \preformatted{
#'
#' }
#'
#' @return xts object
#' @export
#' @importFrom tryCatchLog tryCatchLog
leadingWilshire5000LogReturns <- function() {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  wilshire5000LogReturns() %>% Leading

})}



#' get the SP500 Index current log returns
#'
#' @description
#' \preformatted{
#'
#' }
#'
#' @return xts object
#' @export
#' @importFrom tryCatchLog tryCatchLog
currentSP500LogReturns <- function() {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  SP500LogReturns() %>% Current

})}




#' get the Wilshire 5000 Index current log returns
#'
#' @description
#' \preformatted{
#'
#' }
#'
#' @return xts object
#' @export
#' @importFrom tryCatchLog tryCatchLog
currentWilshire5000LogReturns <- function() {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  wilshire5000LogReturns() %>% Current

})}



#' add current SP500 log returns (SP500logrets)
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
addCurrLeadSP500LogReturns <- function(xTs = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  xTs  <- initXts(xTs)
  xTs  <- combineXts(xTs, leadingSP500LogReturns())
                                 # send to return.Portfolio and the calendar
                                 # GSPClogrets
  xTs  <- combineXts(xTs, currentSP500LogReturns())

  xTs

})}



#' add current Willshire 5000 Index log returns (WILL5000INDlogrets)
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
addCurrLeadWilshire5000LogReturns <- function(xTs = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  xTs  <- initXts(xTs)
  xTs  <- combineXts(xTs, leadingWilshire5000LogReturns())
                                 # send to return.Portfolio and the calendar
                                 # WILL5000INDlogrets
  xTs  <- combineXts(xTs, currentWilshire5000LogReturns())

  xTs

})}



#' add weights (_wts)
#'
#' @description
#' \preformatted{
#'
#' }
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
#' @return xts object with merged data into xTs
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_c
#' @importFrom stringr str_detect
willShire5000EyeBallWts <- function(xTs = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  xTs <- initXts(xTs)
  unrate <- xTs[,"UNRATE"]
  # can't do math on leading NAs
  unrate <- unrate[!is.na(unrate),]

  unrateleadingrets_wts <- ifelse( ((SMA(unrate,2)        - SMA(    unrate   ,6)) <= 0)              |
                                   ((SMA(lag(unrate),2)   - SMA(lag(unrate  ),6)) <= 0)              |
                                   ((SMA(lag(unrate,2),2) - SMA(lag(unrate,2),6)) <= 0), 1.00, 0.00)
  unrateleadingrets_wts[is.na(unrateleadingrets_wts)] <- 1 # 100% allocated
  colnames(unrateleadingrets_wts)[1] <- "WILL5000INDlogleadingrets_wts"

  unratecurrentrets_wts <- lag.xts(unrateleadingrets_wts)
  colnames(unratecurrentrets_wts)[1] <- "WILL5000INDlogcurrentrets_wts"

  unrate_wts <- merge(unrateleadingrets_wts, unratecurrentrets_wts)
  unrate_wts

})}



#' add SP500 weights using eyeball
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
#' @return xts object with merged data into xTs
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_c
#' @importFrom stringr str_detect
SP500EyeBallWts <- function(xTs = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  xTs <- initXts(xTs)
  unrate <- xTs[,"UNRATE"]
  # can't do math on leading NAs
  unrate <- unrate[!is.na(unrate),]

  unrateleadingrets_wts <- ifelse( ((SMA(unrate,2)        - SMA(    unrate   ,6)) <= 0)              |
                                   ((SMA(lag(unrate),2)   - SMA(lag(unrate  ),6)) <= 0)              |
                                   ((SMA(lag(unrate,2),2) - SMA(lag(unrate,2),6)) <= 0), 1.00, 0.00)
  unrateleadingrets_wts[is.na(unrateleadingrets_wts)] <- 1 # 100% allocated
  colnames(unrateleadingrets_wts)[1] <- "GSPClogleadingrets_wts"

  unratecurrentrets_wts <- lag.xts(unrateleadingrets_wts)
  colnames(unratecurrentrets_wts)[1] <- "GSPClogcurrentrets_wts"

  unrate_wts <- merge(unrateleadingrets_wts, unratecurrentrets_wts)
  unrate_wts

})}



#' add Willshire 5000 Index log weights returns using eyeball (WILL5000INDlogrets)
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
addWillShire5000EyeBallWts <- function(xTs = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  xTs <- initXts(xTs)
  combineXts(xTs,willShire5000EyeBallWts(xTs))

})}



#' add SP500 log weights returns using eyeball (GSPClogrets)
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
addSP500EyeBallWts <- function(xTs = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  xTs <- initXts(xTs)
  combineXts(xTs,SP500EyeBallWts(xTs))

})}



#' add SP500 weights using Machine learning
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
#' @param ... dots passed to the indicator generator function
#' @return xts object
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_c
SP500MachineWts <- function(xTs = NULL, Predictee = "GSPClogleadingrets", Predictors = "UNRATE", IndicatorGeneratorFUN = "unrateEyeballIndicators"
                                    , NumbReplicaCopiesMultiple = NULL
                                    , ...) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  Dots <- list(...)

  if(is.null(Dots[["timeSliceMatrix"]]))
    Dots <- c(list(), Dots, list(timeSliceMatrix = tis::nberDates()))

  DescTools::DoCall(prepAndDoMachineWtsData, c(list(),
                    list(xTs), Predictee = "GSPClogleadingrets"
                             , Predictors = "UNRATE"
                             , IndicatorGeneratorFUN = "unrateEyeballIndicators", Dots
  ))

})}



#' add Willshire 5000 Index weights using Machine learning
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
#' @param ... dots passed to the indicator generator function
#' @return xts object
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_c
willShire5000MachineWts <- function(xTs = NULL, Predictee = "WILL5000INDlogleadingrets", Predictors = "UNRATE", IndicatorGeneratorFUN = "unrateEyeballIndicators"
                                    , NumbReplicaCopiesMultiple = NULL
                                    , ...) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  Dots <- list(...)

  if(is.null(Dots[["timeSliceMatrix"]]))
    Dots <- c(list(), Dots, list(timeSliceMatrix = tis::nberDates()))

  DescTools::DoCall(prepAndDoMachineWtsData, c(list(),
                    list(xTs), Predictee = "WILL5000INDlogleadingrets"
                             , Predictors = "UNRATE"
                             , IndicatorGeneratorFUN = "unrateEyeballIndicators", Dots
  ))

})}



#' add SP500 log weights returns using Machine learning (GSPClogrets)
#'
#' @description
#' \preformatted{
#'
#' }
#'
#' @param xTs xts object
#' @return xts object with merged data into xTs
#' @export
addSP500MachineWts <- function(xTs = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  xTs <- initXts(xTs)
  combineXts(xTs,SP500MachineWts(xTs))

})}


#' add Willshire 5000 Index log weights returns using Machine learning (WILL5000INDlogrets)
#'
#' @description
#' \preformatted{
#'
#' }
#'
#' @param xTs xts object
#' @return xts object with merged data into xTs
#' @export
addWillShire5000MachineWts <- function(xTs = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  xTs <- initXts(xTs)
  combineXts(xTs,willShire5000MachineWts(xTs))

})}




#' cash log returns (CASHlogrets)
#'
#' @description
#' \preformatted{
#'
#' }
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
  # must keep HARD coded
  colnames(cashLogRets)[1] <- "CASHlogrets"

  cashLogRets

})}




#' leading cash log returns (CASHlogrets)
#'
#' @description
#' \preformatted{
#'
#' }
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




#' current cash log returns (CASHlogrets)
#'
#' @description
#' \preformatted{
#'
#' }
#'
#' @param xTs xts object (only takes the index)
#' @return leading xts object
#' @export
currentCashLogReturns <- function(xTs = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  xTs  <- initXts(xTs)

  cashLogReturns(xTs) %>%
    Current -> xTs
  xTs

})}



#' add current cash log returns (CASHlogrets)
#'
#' @description
#' \preformatted{
#'
#' }
#'
#' @param xTs xts object
#' @return xts object with merged data into xTs
#' @export
addCurrLeadCashLogReturns <- function(xTs = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  xTs  <- initXts(xTs)
                                # CASHlogrets
  xTs <- combineXts(xTs, currentCashLogReturns(xTs))
  xTs <- combineXts(xTs, leadingCashLogReturns(xTs))
  xTs

})}


#' cash weights
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
#' @importFrom stringr str_detect
cashWts <- function(xTs = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})
   xTs <- initXts(xTs)

  # excess left over
  Cashwts <- xts(rep(1,NROW(xTs)),index(xTs)) - rowSums(xTs[ ,wtsLeadingRetsClms(xTs)], na.rm = TRUE)
  colnames(Cashwts)[1] <- "CASHlogleadingrets_wts"

  Cashwts

})}



#' add cash weights of log returns (CASHlogrets)
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
  xTs  <- initXts(xTs)

  combineXts(xTs, cashWts(xTs))

})}



#' add cash log returns (CASHlogrets)
#'
#' @description
#' \preformatted{
#'
#' NEVER USED - RETIRED - DO NOT USE
#'
#' }
#'
#' @param xTs xts object (only takes the index)
#' @return xts object with merged data into xTs
#' @export
#' @importFrom tryCatchLog tryCatchLog
addCashLogReturns <- function(xTs = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  xTs  <- initXts(xTs)

                         # CASHlogrets
  combineXts(xTs, cashLogReturns(xTs))

})}



#' Predicts the FRED WILL5000IND eom returns using UNRATE and the eyeball
#'
#' @export
#' @importFrom tryCatchLog tryCatchLog
UnRateEyeBalltradeModelWILL5000IND <- function() {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})

  # (1) data 'value' (try to optimize)
  addCurrLeadWilshire5000LogReturns()  %>%  #
  addCurrLeadCashLogReturns            %>%  #

  # (2) indicator(s)
  addUnRateEomData  %>% # unrate

  # (3) use indicator(s)(unrate) to make rules:signals(weights)
  addWillShire5000EyeBallWts %>%   #

  appendCashWts              %>%   # (excess)

  printTail("Exact Schedule of Leading of Eye Ball Returns and Decisions", n = 10) %>%

  # (4) apply in action
  portfolioMonthlyReturns %>%

  # (5) evaluate performance
  printCalendar("UnRateEyeBall Performance Returns")

})}
# UnRateEyeBalltradeModelWILL5000IND()


#' Predicts the Yahoo SP500 eom returns using UNRATE and the eyeball
#'
#' @export
#' @importFrom tryCatchLog tryCatchLog
UnRateEyeBalltradeModelGSPC <- function() {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})

  # (1) data 'value' (try to optimize)
  addCurrLeadSP500LogReturns() %>%  #
  addCurrLeadCashLogReturns    %>%  #

  # (2) indicator(s)
  addUnRateEomData %>% # unrate

  # (3) use indicator(s)(unrate) to make rules:signals(weights)
  addSP500EyeBallWts %>%   #

  appendCashWts      %>%   # (excess)

  printTail("Exact Schedule of Leading of Eye Ball Returns and Decisions", n = 10) %>%

  # (4) apply in action
  portfolioMonthlyReturns %>%

  # (5) evaluate performance
  printCalendar("UnRateEyeBall Performance Returns")

})}
# UnRateEyeBalltradeModelGSPC()


#' Predicts the FRED WILL5000IND eom returns using UNRATE and Machine learning
#'
#' @export
#' @importFrom tryCatchLog tryCatchLog
UnRateMachinetradeModelWILL5000IND <- function() {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})

  # (1) data 'value' (try to optimize)
  addCurrLeadWilshire5000LogReturns() %>%      #
  addCurrLeadCashLogReturns           %>%      #

  # (2) indicator(s)
  addUnRateEomData %>%                 # unrate

  # (3) use indicator(s)(unrate) to make rules:signals(weights)
  addWillShire5000MachineWts %>%       #
  appendCashWts              %>%       # (excess)

  printTail("Exact Schedule of Leading of UnRateMachine Returns and Decisions") %>%

  # (4) apply in action
  portfolioMonthlyReturns %>%

  # (5) evaluate performance
  printCalendar("UnRateMachine Performance Returns")

})}
# UnRateMachinetradeModelWILL5000IND()



#' Predicts the SP500 eom returns using UNRATE and Machine learning
#'
#' @export
#' @importFrom tryCatchLog tryCatchLog
UnRateMachinetradeModelGSPC <- function() {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})

  # (1) data 'value' (try to optimize)
  addCurrLeadSP500LogReturns() %>%  #
  addCurrLeadCashLogReturns    %>%  #

  # (2) indicator(s)
  addUnRateEomData %>%              # unrate

  # (3) use indicator(s)(unrate) to make rules:signals(weights)
  addSP500MachineWts %>%       #
  appendCashWts      %>%       # (excess)

  printTail("Exact Schedule of Leading of UnRateMachine Returns and Decisions") %>%

  # (4) apply in action
  portfolioMonthlyReturns %>% #

  # (5) evaluate performance
  printCalendar("UnRateMachine Performance Returns")

})}
# UnRateMachinetradeModelGSPC()









