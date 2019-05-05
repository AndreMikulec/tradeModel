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
  printTail(mktdata, title = "Exact Schedule of Leading of Eye Ball Returns and Decisions", n = 10)

  # [ ] ANY future WORK_ON_THIS?
  message(" ")
  message("futuredate: WILL5000IND GSPC.apc.1leadingrets_wts is NA so WILL5000IND CASH.apc.1leadingrets_wts == 1, see function cashWts()")
  message("WILL5000IND GSPC.apc.1leadingrets_wts HAS NOTHING to calculate it")
  message("because ONLY doing (past and now): buildModel - WILL5000IND GSPC.apc.1leadingrets_wts ~ unrate1 + unrate2 + unrate3")
  message("ALTERNATIVE FUTURE CODE?: if ALL OF wtsLeadingRetsClms(xTs) are NA then Cashwts is ALSO NA")
  message(" ")
  message(" ")

  # (4) apply in action
  portfolioMonthlyReturns(mktdata)

  # (5) evaluate performance
  printCalendar(mktdata, title = "Fitted Performance Returns predictions", PortFolioRetCol = "Fitted", MarginReturns = FALSE, start = Sys.Date() - 366*20, DivBy = 100.00)
  printCalendar(mktdata, title = "UnRateEyeBall Performance Returns")

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

  # (1) data 'value' (try to optimize)
  addCurrLeadSymbolReturns(mktdata, Symbol = Symbol,  src = src, IsTarget = TRUE,  SymplifyGeneratorFUN = "toMonthlyData",    ReturnsGeneratorFUN = "Returns", ReturnsGeneratorFUNArgs = list(is.na.zero = TRUE,  Fun = list(Fun = "APC", lag = 1)))
  #
  # Symbol = "GDP", src = "FRED2" # fancifyXts requires extra FRED data from FRED2
  # fancifyXts need parameter lastUpdatedDate
  addCurrLeadSymbolReturns(mktdata, Symbol = "GDP",  src = "FRED2", IsATarget = TRUE,  SymplifyGeneratorFUN = "fancifyXts",    ReturnsGeneratorFUN = "Returns", ReturnsGeneratorFUNArgs = list(is.na.zero = TRUE,  Fun = list(Fun = "APC", lag = 3)))
  # SEEMS # XdepreciateX recorrect IsATarget usage.
  # MEANT for competive assets that have
  # both a "current" and "leading" value (and future _wts decision as an investment)
  # e.g. bonds v.s. stocks vs. real_estate
  # !!!"GDP" (is an example) but is improper USAGE!!

  # add poor performance data (CRASHACML)
  # Of the 'target', find the two(2) worst performing months
  # over the next(future six(6)) months
  # and then sum those two(2) bad worst performing months together.
  addCrashData(mktdata)

  addCurrLeadCashReturns(mktdata, IsATarget = TRUE)

  # (2) indicator(s)
  ## addUnRateEomData(mktdata)
  addEomData(mktdata, Symbol = "UNRATE", src = "FRED", SymplifyGeneratorFUN = "eomIndex")

  # fancifyXts(FRED2) # fancifyXts requires extra FRED data from FRED2
  addEomData(mktdata, Symbol = "GDP",    src = "FRED2", SymplifyGeneratorFUN = "fancifyXts")

  # need xts attributes separating what I AM PREDICTING(target) v.s. (investment)(investments)
  # Note: STRONG POSSIBILITY GET RID OF currentrets_wts (NEVER IN PERFORMANCE ANALYTICS MATH)

  # LEFT_OFF # UnRateMachinetradeModel
  #                     (1) DONE. decide the *next line* using 'poor performance data (CRASHACML)'
  #                     (2) DONE. choose the BEST 75% PREDICTED GSPC.apc.1leadingrets
  #                     (3) DONE. assign GSPC.apc.1leadingrets_wts <- 1: BEST 75% (ABOVE in the line above)
  #                     (4) DONE. buy GSPC.apc.1currentrets (INSTEAD OF GDP) investment
  #                     (5) TODO [ ] VERIFY/FIX: machine optimization function: TO BE COMPATIBLE WITH CRASHACML
  #                     (6) TODO [ ] ADD predictors: Predictors UNRATE, 'umich centiment', 'other recession indicators'

  #                                              "GeneralMathIndicators" (similar to RecessionSight)

  # (3) use indicator(s)(unrate) to make rules:signals(weights)
  # addSymbolMachineWts(mktdata, Predictors = "UNRATE", IndicatorGeneratorFUN = "unrateEyeballIndicators")

  browser()
  addSymbolMachineWts(mktdata, Predictee = "CRASHACML", Predictors = "UNRATE", IndicatorGeneratorFUN = "unrateEyeballIndicators")

  ## eventually(but not SOON)
  ## addSymbolMachineWts(mktdata, Predictors = c("UNRATE", "GDP", "GDP_DLY"), IndicatorGeneratorFUN = "unrateAndGDPIndicators")

  # excess
  appendCashWts(mktdata)
  # keep PerformanceAnalytics::Return.portfolio happy
  appendAllOtherWts(mktdata)

  # printTail("Exact Schedule of Leading of UnRateMachine Returns and Decisions") %>%
  printTail(mktdata, title = "Exact Schedule of Leading of UnRateMachine Returns and Decisions", n = 10)

  # (4) apply in action
  portfolioMonthlyReturns(mktdata)

  # (5) evaluate performance
  printCalendar(mktdata, title = "GDP results", PortFolioRetCol = "GDP.apc.3currentrets", MarginReturns = FALSE, start = Sys.Date() - 366*20)
  printCalendar(mktdata, title = "Fitted Performance Returns predictions", PortFolioRetCol = "Fitted", MarginReturns = FALSE, start = Sys.Date() - 366*20)
  printCalendar(mktdata, title = "UnRateMachine Performance Returns")

  return(invisible())

})}


