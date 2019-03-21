


#' get data from the St. Louis FRED
#'
#' @description
#' \preformatted{
#'
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
#' # > head(fredData("GDP"),1)
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
#' fredData(Symbol = "UNRATE")
#'
#' # full test
#' # 1. remove cache data: rm(.UNRATE) (if there)
#' # 2. drop database table "Symbols"."UNRATE"
#' # 3. remove corresponding record from "Symbols"."Symbols"
#' # 4. fredData(Symbol = "UNRATE")
#'
#' # partial tests
#'
#' 2.1 remove some (few) bottom records from the
#' database table "Symbols"."UNRATE"
#' just insert only-new (few) records
#' # placeNewRecords == "AddOnlyNew" (default)
#'
#' fredData(Symbol = "UNRATE", NewMaxAge = "1 secs")
#'
#' # or
#'
#' # 2.2
#' # placeNewRecords =="TruncateTable"
#'
#' fredData(Symbol = "UNRATE", NewMaxAge = "1 secs", placeNewRecords = "TruncateTable")
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

  message(stringr::str_c("Begin fredData - "), Symbol)

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
  message(stringr::str_c("End   fredData - "), Symbol)

  # colnames(xTs)[1] <- tolower(colnames(xTs))
  xTs

})}



#' get data from Yahoo corporation
#'
#' @description
#' \preformatted{
#'
#'  WORKS just like fredData
#'
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
#' yahooData(Symbol = "^GSPC"
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

  message(stringr::str_c("Begin yahooData - "), Symbol)

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

  message(stringr::str_c("End   yahooData - "), Symbol)

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
  fredData(Symbol = Symbol) %>%
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


