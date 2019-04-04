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
  addCurrLeadSymbolReturns(mktdata, Symbol = Symbol, src = src, IsTarget = TRUE, SymplifyGeneratorFUN = "toMonthlyData", ReturnsGeneratorFUN = "Returns", ReturnsGeneratorFUNArgs = list(is.na.zero = TRUE,  Fun = list(Fun = "APC", lag = 1)))
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
  addCurrLeadSymbolReturns(mktdata, Symbol = Symbol,  src = src,     IsTarget = TRUE,  SymplifyGeneratorFUN = "fancifyXts",    ReturnsGeneratorFUN = "Returns", ReturnsGeneratorFUNArgs = list(is.na.zero = TRUE,  Fun = list(Fun = "APC", lag = 3)))
  addCurrLeadSymbolReturns(mktdata, Symbol = "^GSPC", src = "yahoo", IsATarget = TRUE, SymplifyGeneratorFUN = "toMonthlyData", ReturnsGeneratorFUN = "Returns", ReturnsGeneratorFUNArgs = list(is.na.zero = TRUE,  Fun = list(Fun = "APC", lag = 1)))

  # (1) data 'value' (try to optimize)
  # addCurrLeadSymbolReturns(mktdata, Symbol = Symbol, src = src, IsTarget = TRUE, SymplifyGeneratorFUN = "toMonthlyData", ReturnsGeneratorFUN = "Returns", ReturnsGeneratorFUNArgs = list())




  # fancifyXts requires extra FRED data from FRED2
  ### addCurrLeadSymbolReturns(mktdata, Symbol = "GDP", src = "FRED2", IsATarget = TRUE, SymplifyGeneratorFUN = "fancifyXts", ReturnsGeneratorFUN = "Returns", ReturnsGeneratorFUNArgs = list())
  addCurrLeadCashReturns(mktdata, IsATarget = TRUE)

  # (2) indicator(s)
  ## addUnRateEomData(mktdata)
  addEomData(mktdata, Symbol = "UNRATE", src = "FRED", SymplifyGeneratorFUN = "eomIndex")

  # fancifyXts(FRED2) # fancifyXts requires extra FRED data from FRED2
  addEomData(mktdata, Symbol = "GDP",    src = "FRED2", SymplifyGeneratorFUN = "fancifyXts")

  browser()  # need xts attributes separating what I AM PREDICTING(target) v.s. (investment)(investments)
  # Note: STRONG POSSIBILITY GET RID OF currentrets_wts (NEVER IN PERFORMANCE ANALYTICS MATH)

  # LEFT_OFF # UnRateMachinetradeModel(Symbol = "GDP", src = "FRED2")
  # LEFT_OFF:           (1) choose the BEST 75% PREDICTED GDP.apc.3leadingrets
  #         #           (2) assign GSPC.apc.1leadingrets_wts <- 1: BEST 75% (ABOVE in the line above)
  #                     (3) print INSTEAD ... calendar USING GSPC.apc.1currentrets (INSTEAD OF GDP) (investment)
  #                     (4) print SECOND calendar USING GDP.apc.3currentrets (Discard Margins ... custom calendar)
  #

  # (3) use indicator(s)(unrate) to make rules:signals(weights)
  addSymbolMachineWts(mktdata, Predictors = "UNRATE", IndicatorGeneratorFUN = "unrateEyeballIndicators")

  browser()

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


