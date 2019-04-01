#' Predicts the Symbol eom returns using UNRATE and the eyeball
#'
#' @examples
#' \dontrun{
#'
#' # Predicts the FRED WILL5000IND / yahoo S&P500 eom returns using UNRATE and the eyeball
#'
#' UnRateEyeBalltradeModel(Symbol = "WILL5000IND", src = "FRED")
#' UnRateEyeBalltradeModel(Symbol = "^GSPC", src = "yahoo")
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
UnRateEyeBalltradeModel <- function(Symbol = NULL, src = NULL) {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})

  # (1) data 'value' (try to optimize)
  addCurrLeadSymbolAPCReturns(mktdata, Symbol = Symbol, src = src, IsTarget = TRUE, SymplifyGeneratorFUN = "toMonthlyData")
  addCurrLeadCashReturns(mktdata, IsATarget = TRUE)

  # (2) indicator(s)
  ## addUnRateEomData(mktdata)
  addEomData(mktdata, Symbol = "UNRATE", src = "FRED", SymplifyGeneratorFUN = "eomIndex")

  # fancifyXts(FRED2) requires extra FRED data: FRED2
  addEomData(mktdata, Symbol = "GDP",    src = "FRED2", SymplifyGeneratorFUN = "fancifyXts")

  # (3) use indicator(s)(unrate) to make rules:signals(weights)
  addSymbolEyeBallWts(mktdata, Symbol = Symbol)

  # excess
  appendCashWts(mktdata)
  # keep PerformanceAnalytics::Return.portfolio happy
  appendAllOtherWts(mktdata)

  # printTail("Exact Schedule of Leading of Eye Ball Returns and Decisions", n = 10)  %>%
  printTail(mktdata, "Exact Schedule of Leading of Eye Ball Returns and Decisions", n = 10)

  # (4) apply in action
  portfolioMonthlyReturns(mktdata)

  # (5) evaluate performance
  printCalendar(mktdata, "UnRateEyeBall Performance Returns")

  return(invisible())

})}



#' Predicts Symbol eom returns using UNRATE and Machine learning
#'
#' @examples
#' \dontrun{
#'
#' # Predicts the FRED WILL5000IND / yahoo S&P500 eom returns using UNRATE and the machine
#'
#' UnRateMachinetradeModel(Symbol = "WILL5000IND", src = "FRED")
#' UnRateMachinetradeModel(Symbol = "^GSPC"      , src = "yahoo")
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
UnRateMachinetradeModel <- function(Symbol = NULL, src = NULL) {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})

  # UnRateMachinetradeModel(Symbol = "GDP", src = "FRED2") # fancifyXts requires extra FRED data from FRED2
  # addCurrLeadSymbolAPCReturns(mktdata, Symbol = Symbol, src = src, IsTarget = TRUE, SymplifyGeneratorFUN = "fancifyXts")

  # (1) data 'value' (try to optimize)
  addCurrLeadSymbolAPCReturns(mktdata, Symbol = Symbol, src = src, IsTarget = TRUE, SymplifyGeneratorFUN = "toMonthlyData")

  # fancifyXts requires extra FRED data from FRED2
  addCurrLeadSymbolAPCReturns(mktdata, Symbol = "GDP", src = "FRED2", IsATarget = TRUE, SymplifyGeneratorFUN = "fancifyXts")
  addCurrLeadCashReturns(mktdata, IsATarget = TRUE)

  # (2) indicator(s)
  ## addUnRateEomData(mktdata)
  addEomData(mktdata, Symbol = "UNRATE", src = "FRED", SymplifyGeneratorFUN = "eomIndex")

  # fancifyXts(FRED2) # fancifyXts requires extra FRED data from FRED2
  addEomData(mktdata, Symbol = "GDP",    src = "FRED2", SymplifyGeneratorFUN = "fancifyXts")

  # (3) use indicator(s)(unrate) to make rules:signals(weights)
  addSymbolMachineWts(mktdata, Predictors = "UNRATE", IndicatorGeneratorFUN = "unrateEyeballIndicators")

  # excess
  appendCashWts(mktdata)
  # keep PerformanceAnalytics::Return.portfolio happy
  appendAllOtherWts(mktdata)

  # printTail("Exact Schedule of Leading of UnRateMachine Returns and Decisions") %>%
  printTail(mktdata, "Exact Schedule of Leading of UnRateMachine Returns and Decisions")

  # (4) apply in action
  portfolioMonthlyReturns(mktdata)

  # (5) evaluate performance
  printCalendar(mktdata, "UnRateMachine Performance Returns")

  return(invisible())

})}


