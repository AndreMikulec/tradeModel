#' Predicts the Symbol eom returns using UNRATE and the eyeball
#'
#' @examples
#' \dontrun{
#'
#' # Predicts the FRED WILL5000IND / yahoo S&P500 eom returns using UNRATE and the eyeball
#'
#' UnRateEyeBalltradeModel(Symbol = "WILL5000IND", src = "FRED", Change = "apc")
#' UnRateEyeBalltradeModel(Symbol = "^GSPC", src = "yahoo", Change = "apc")
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
UnRateEyeBalltradeModel <- function(Symbol = NULL, src = NULL, Change = NULL) {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})

  # (1) data 'value' (try to optimize)
  addCurrLeadSymbolAPCReturns(mktdata, Symbol = Symbol, src = src)
  addCurrLeadCashAPCReturns(mktdata)

  # (2) indicator(s)
  addUnRateEomData(mktdata)

  # (3) use indicator(s)(unrate) to make rules:signals(weights)
  addSymbolEyeBallWts(mktdata, Symbol = Symbol, Change = Change)

  # appendCashAPCWts %>% # (excess)
  appendCashAPCWts(mktdata)

  # printTail("Exact Schedule of Leading of Eye Ball Returns and Decisions", n = 10)  %>%
  printTail(mktdata, "Exact Schedule of Leading of Eye Ball Returns and Decisions", n = 10)

  # (4) apply in action
  portfolioMonthlyReturns(mktdata)

  # (5) evaluate performance
  printCalendar(mktdata, "UnRateEyeBall Performance Returns")

})}



#' Predicts Symbol eom returns using UNRATE and Machine learning
#'
#' @examples
#' \dontrun{
#'
#' # Predicts the FRED WILL5000IND / yahoo S&P500 eom returns using UNRATE and the machine
#'
#' UnRateMachinetradeModel(Symbol = "WILL5000IND", src = "FRED", Change = "apc", Predictee = "WILL5000INDapcleadingrets")
#' UnRateMachinetradeModel(Symbol = "^GSPC"      , src = "yahoo", Change = "apc", Predictee = "GSPCapcleadingrets")
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
UnRateMachinetradeModel <- function(Symbol = NULL, src = NULL, Change = NULL, Predictee = NULL) {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})

  # (1) data 'value' (try to optimize)
  addCurrLeadSymbolAPCReturns(mktdata, Symbol = Symbol, src = src)
  addCurrLeadCashAPCReturns(mktdata)

  # (2) indicator(s)
  addUnRateEomData(mktdata)

  # (3) use indicator(s)(unrate) to make rules:signals(weights)
  addSymbolMachineWts(mktdata, Predictee = Predictee, Predictors = "UNRATE", IndicatorGeneratorFUN = "unrateEyeballIndicators")

  # appendCashAPCWts %>% # (excess)
  appendCashAPCWts(mktdata)

  # printTail("Exact Schedule of Leading of UnRateMachine Returns and Decisions") %>%
  printTail(mktdata, "Exact Schedule of Leading of UnRateMachine Returns and Decisions")

  # (4) apply in action
  portfolioMonthlyReturns(mktdata)

  # (5) evaluate performance
  printCalendar(mktdata, "UnRateMachine Performance Returns")

})}


