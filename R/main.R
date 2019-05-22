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
  addEomData(mktdata, Symbol = "UNRATE", src = "FRED", SymplifyGeneratorFUN = "eomIndex", NA.LOCF = FALSE)

  # fancifyXts(FRED2) requires extra FRED data: FRED2
  addEomData(mktdata, Symbol = "GDP",    src = "FRED2", SymplifyGeneratorFUN = "fancifyXts", NA.LOCF = FALSE)

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
#' # Predicts the FRED WILL5000IND / yahoo S&P500 eom returns using UNRATE (and/or 'others') and the machine
#'
#' # still works
#' UnRateMachinetradeModel(Symbol = "WILL5000IND", Predictors = "UNRATE", src = "FRED",  IndicatorGeneratorFUN = "unrateEyeballIndicators")
#' UnRateMachinetradeModel(Symbol = "^GSPC"      , Predictors = "UNRATE", src = "yahoo", IndicatorGeneratorFUN = "unrateEyeballIndicators")
#'
#' # alt derived data columns from 'trends'
#' UnRateMachinetradeModel(Symbol = "WILL5000IND", Predictors = "UNRATE", src = "FRED",  IndicatorGeneratorFUN = "trendsWithAOMXIndicators")
#' # technically works: but (Symbol = "^GSPC") gives UPSIDE DOWN predictions
#' UnRateMachinetradeModel(Symbol = "^GSPC"      , Predictors = "UNRATE", src = "yahoo", IndicatorGeneratorFUN = "trendsWithAOMXIndicators")
#'
#' # good
#'
#' UnRateMachinetradeModel(Symbol = "WILL5000IND", src = "FRED",  Predictors = c("UNRATE","UMCSENT"), IndicatorGeneratorFUN = c("trendsWithAOMXIndicators", "trendsWithAOMNIndicators"))
#' UnRateMachinetradeModel(Symbol = "^GSPC"      , src = "yahoo", Predictors = c("UNRATE","UMCSENT"), IndicatorGeneratorFUN = c("trendsWithAOMXIndicators", "trendsWithAOMNIndicators"))
#'
#' # NEW(WORK IN PROGRESS)
#'
#' UnRateMachinetradeModel(Symbol = "WILL5000IND", src = "FRED",  Predictors = c("UNRATE","UMCSENT","Earnings"), IndicatorGeneratorFUN = c("trendsWithAOMXIndicators", "trendsWithAOMNIndicators", "trendsWithAOMNIndicators"))
#' UnRateMachinetradeModel(Symbol = "^GSPC"      , src = "yahoo", Predictors = c("UNRATE","UMCSENT","Earnings"), IndicatorGeneratorFUN = c("trendsWithAOMXIndicators", "trendsWithAOMNIndicators", "trendsWithAOMNIndicators"))
#'
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
UnRateMachinetradeModel <- function(Symbol = NULL, src = NULL, Predictors = NULL, IndicatorGeneratorFUN = NULL) {
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
  if("UNRATE" %in% Predictors) {
    ## addUnRateEomData(mktdata)
    addEomData(mktdata, Symbol = "UNRATE", src = "FRED", SymplifyGeneratorFUN = "eomIndex", NA.LOCF = FALSE)
  }

  if("UMCSENT" %in% Predictors) {
    addEomData(mktdata, Symbol = "UMCSENT", src = "UMich", SymplifyGeneratorFUN = "eomIndex", NA.LOCF = TRUE)
  }

  # NEW(WORK IN PROGRESS)
  if("Earnings" %in% Predictors) {   # NEED: LEFT_OFF [ ] remove NA.LOCF and pull data forward to this months (or? last months) end date CarryForward = NULL
     addEomData(mktdata, Symbol = "Earnings", src = "YaleU", SymplifyGeneratorFUN = "eomIndex", NA.LOCF = TRUE)
  }

  # fancifyXts(FRED2) # fancifyXts requires extra FRED data from FRED2
  addEomData(mktdata, Symbol = "GDP",    src = "FRED2", SymplifyGeneratorFUN = "fancifyXts", NA.LOCF = FALSE)

  # need xts attributes separating what I AM PREDICTING(target) v.s. (investment)(investments)
  # Note: STRONG POSSIBILITY GET RID OF currentrets_wts (NEVER IN PERFORMANCE ANALYTICS MATH)

  # LEFT_OFF (but SEE: tradeModel_SCRATCH.txt)

  # (1) TODO: [ ] FIND/INTEGERATE: 'rank since last crash' (WHERE IS THAT?)
  # (2) TODO  [ ] ADD predictors: other recession indicators': WHAT DOES ** chavet/piger USE? **

  if(identical(Predictors, "UNRATE") && identical(IndicatorGeneratorFUN, "unrateEyeballIndicators")) {
    # still works
    addSymbolMachineWts(mktdata, Predictee = "CRASHACML", Predictors = Predictors, IndicatorGeneratorFUN = IndicatorGeneratorFUN)
  }

  # alt derived data columns from 'trends'
  # technically works
  # technically works: but (Symbol = "^GSPC") gives UPSIDE DOWN predictions
  if(identical(Predictors, "UNRATE") && identical(IndicatorGeneratorFUN, "trendsWithAOMXIndicators")) {
    addSymbolMachineWts(mktdata, Predictee = "CRASHACML", Predictors = Predictors, IndicatorGeneratorFUN = IndicatorGeneratorFUN)
  }

  # good
  if(
    identical(Predictors, c("UNRATE","UMCSENT"))                                                 &&
    identical(IndicatorGeneratorFUN, c("trendsWithAOMXIndicators", "trendsWithAOMNIndicators"))
  ) {
    addSymbolMachineWts(mktdata, Predictee = "CRASHACML", Predictors = Predictors, IndicatorGeneratorFUN = IndicatorGeneratorFUN)
  }

  # NEW(WORK IN PROGRESS)
  if(
    identical(Predictors, c("UNRATE","UMCSENT","Earnings"))                                                 &&
    identical(IndicatorGeneratorFUN, c("trendsWithAOMXIndicators", "trendsWithAOMNIndicators", "trendsWithAOMNIndicators"))
  ) {
    addSymbolMachineWts(mktdata, Predictee = "CRASHACML", Predictors = Predictors, IndicatorGeneratorFUN = IndicatorGeneratorFUN)
  }

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


