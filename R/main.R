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
#' # Predicts the FRED WILL5000IND / yahoo S&P500 eom returns using UNRATE (and/or 'others') and the machine
#'
#' # done: !!! misses entire 2008 recession !!!
#' UnRateMachinetradeModel(Symbol = "WILL5000IND", src = "FRED",  Predictors = "UNRATE", IndicatorGeneratorFUN = "unrateEyeballIndicators")
#' UnRateMachinetradeModel(Symbol = "^GSPC"      , src = "yahoo", Predictors = "UNRATE", IndicatorGeneratorFUN = "unrateEyeballIndicators")
#'
#'
#' # good captures entire 2008 recession 'and' early 2019
#' UnRateMachinetradeModel(Symbol = "WILL5000IND", src = "FRED", Predictors = "UNRATE",  IndicatorGeneratorFUN = "trendsWithAOMXIndicators")
#' #
#' # good captures entire 2008 recession 'and' early 2019
#' # technically works: but (Symbol = "^GSPC") gives UPSIDE DOWN predictions
#' UnRateMachinetradeModel(Symbol = "^GSPC"      , src = "yahoo", Predictors = "UNRATE", IndicatorGeneratorFUN = "trendsWithAOMXIndicators")
#'
#'
#' # good captures entire 2008 recession 'and' early 2019(current time data: just before edge)
#' UnRateMachinetradeModel(Symbol = "WILL5000IND", src = "FRED",  Predictors = c("UNRATE","UMCSENT"), IndicatorGeneratorFUN = c("trendsWithAOMXIndicators", "trendsWithAOMNIndicators"))
#' #
#' # good captures entire 2008 recession 'and' early 2019(current time data: just before edge)
#' UnRateMachinetradeModel(Symbol = "^GSPC"      , src = "yahoo", Predictors = c("UNRATE","UMCSENT"), IndicatorGeneratorFUN = c("trendsWithAOMXIndicators", "trendsWithAOMNIndicators"))
#'
#'
#' # good: captures entire 2008 recession 'and' early 2019(current time data: just before edge)
#' UnRateMachinetradeModel(Symbol = "WILL5000IND", src = "FRED",  Predictors = c("UNRATE","UMCSENT","Earnings"), IndicatorGeneratorFUN = c("trendsWithAOMXIndicators", "trendsWithAOMNIndicators", "trendsWithAOMNIndicators"))
#' #
#' # good: captures entire 2008 recession 'and' early 2019(current time data: just before edge)
#' UnRateMachinetradeModel(Symbol = "^GSPC"      , src = "yahoo", Predictors = c("UNRATE","UMCSENT","Earnings"), IndicatorGeneratorFUN = c("trendsWithAOMXIndicators", "trendsWithAOMNIndicators", "trendsWithAOMNIndicators"))
#'
#'
#' # done: extra SP500PriceEarningsRat !!! misses entire 2008 recession !!!
#' # SP500PriceEarningsRat (not as much impact as I originally thought or 'wrong' impact)
#' #
#' UnRateMachinetradeModel(Symbol = "WILL5000IND", src = "FRED",  Predictors = c("UNRATE","UMCSENT","Earnings", "SP500PriceEarningsRat"), IndicatorGeneratorFUN = c("trendsWithAOMXIndicators", "trendsWithAOMNIndicators", "trendsWithAOMNIndicators", "trendsWithAOMXIndicators"))
#' UnRateMachinetradeModel(Symbol = "^GSPC"      , src = "yahoo", Predictors = c("UNRATE","UMCSENT","Earnings", "SP500PriceEarningsRat"), IndicatorGeneratorFUN = c("trendsWithAOMXIndicators", "trendsWithAOMNIndicators", "trendsWithAOMNIndicators", "trendsWithAOMXIndicators"))
#'
#'
#' # add hoc (kept perfectly out of late 2007, 2008, and 1st half of 2009)
#' # but also kept out of 2013-2016
#' # therefore: SP500PriceEarningsRat NEEDS competition from BONDS: what is it? equivalent: Of Zimmermann Equity Premium: [ ] FIND & FIX
#' #
#' UnRateMachinetradeModel(Symbol = "^GSPC"      , src = "yahoo", Predictors = c("SP500PriceEarningsRat"), IndicatorGeneratorFUN = c("trendsWithAOMXIndicators"))
#'
#' # I WAS TOO MUCH IN-A-HURRY (JUNK OUTPUT)
#' # Year over year change in "Real Gross Private Domestic Investment: Fixed Investment: Nonresidential: Equipment"
#' # SEEMS 'STILL TOO NOISY':I REGRET trying; FRED Y033RL1Q225SBEA
#' UnRateMachinetradeModel(Symbol = "^GSPC"      , src = "yahoo", Predictors = c("UNRATE","YOYChangeInRGPDIFINRE"), IndicatorGeneratorFUN = c("trendsWithAOMXIndicators", "trendsWithAOMNIndicators"))
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
    addEomData(mktdata, Symbol = "UNRATE", src = "FRED", SymplifyGeneratorFUN = "eomIndex")
  }

  if("UMCSENT" %in% Predictors) {
    addEomData(mktdata, Symbol = "UMCSENT", src = "UMich", SymplifyGeneratorFUN = "eomIndex", CarryForward = "NA.LOCF")
  }

  if("Earnings" %in% Predictors) {
     addEomData(mktdata, Symbol = "Earnings", src = "YaleU", SymplifyGeneratorFUN = "eomIndex", CarryForward = "NA.Shift.EOLM")
  }

  if("SP500PriceEarningsRat" %in% Predictors) {
    addEomData(mktdata, Symbol = "SP500PriceEarningsRat", src = "YaleU", SymplifyGeneratorFUN = "eomIndex", CarryForward = "NA.Shift.EOLM")
  }

  # I WAS TOO MUCH IN-A-HURRY (JUNK OUTPUT)
  # Year over year change in "Real Gross Private Domestic Investment: Fixed Investment: Nonresidential: Equipment"
  # SEEMS 'STILL TOO NOISY':I REGRET trying; FRED Y033RL1Q225SBEA
  if("YOYChangeInRGPDIFINRE" %in% Predictors) {
    addEomData(mktdata, Symbol = c("YOYChangeInRGPDIFINRE", "YOYChangeInRGPDIFINRE_DLY"), Components = "Y033RL1Q225SBEA"
               # (or use MY relative/absolute change functions)
               , ComponentsFormula = c("(TTR::SMA(cumprod(1 + zoo::na.trim(Y033RL1Q225SBEA)/100),2) - xts::lag.xts(TTR::SMA(cumprod(1 + zoo::na.trim(Y033RL1Q225SBEA)/100),2), 4)) / xts::lag.xts(TTR::SMA(cumprod(1 + zoo::na.trim(Y033RL1Q225SBEA)/100),2), 4) * 100"
                                     , "Y033RL1Q225SBEA_DLY")
               , src = c("FRED2"), SymplifyGeneratorFUN = c("fancifyXts"))
  }

  # fancifyXts(FRED2) # fancifyXts requires extra FRED data from FRED2
  addEomData(mktdata, Symbol = "GDP",    src = "FRED2", SymplifyGeneratorFUN = "fancifyXts")

  # need xts attributes separating what I AM PREDICTING(target) v.s. (investment)(investments)
  # Note: STRONG POSSIBILITY GET RID OF currentrets_wts (NEVER IN PERFORMANCE ANALYTICS MATH)

  addSymbolMachineWtsDone <- FALSE

  if(identical(Predictors, "UNRATE") && identical(IndicatorGeneratorFUN, "unrateEyeballIndicators")) {
    # still works
    addSymbolMachineWts(mktdata, Predictee = "CRASHACML", Predictors = Predictors, IndicatorGeneratorFUN = IndicatorGeneratorFUN)
    addSymbolMachineWtsDone <- TRUE
  }

  # alt derived data columns from 'trends'
  # technically works
  # technically works: but (Symbol = "^GSPC") gives UPSIDE DOWN predictions
  if(identical(Predictors, "UNRATE") && identical(IndicatorGeneratorFUN, "trendsWithAOMXIndicators")) {
    addSymbolMachineWts(mktdata, Predictee = "CRASHACML", Predictors = Predictors, IndicatorGeneratorFUN = IndicatorGeneratorFUN)
    addSymbolMachineWtsDone <- TRUE
  }

  # tech working
  if(
    identical(Predictors, c("UNRATE","UMCSENT"))                                                 &&
    identical(IndicatorGeneratorFUN, c("trendsWithAOMXIndicators", "trendsWithAOMNIndicators"))
  ) {
    addSymbolMachineWts(mktdata, Predictee = "CRASHACML", Predictors = Predictors, IndicatorGeneratorFUN = IndicatorGeneratorFUN)
    addSymbolMachineWtsDone <- TRUE
  }

  # tech working
  if(
    identical(Predictors, c("UNRATE","UMCSENT","Earnings"))                                                 &&
    identical(IndicatorGeneratorFUN, c("trendsWithAOMXIndicators", "trendsWithAOMNIndicators", "trendsWithAOMNIndicators"))
  ) {
    addSymbolMachineWts(mktdata, Predictee = "CRASHACML", Predictors = Predictors, IndicatorGeneratorFUN = IndicatorGeneratorFUN)
    addSymbolMachineWtsDone <- TRUE
  }

  # tech working
  if(
    identical(Predictors, c("UNRATE","UMCSENT","Earnings", "SP500PriceEarningsRat"))                                                 &&
    identical(IndicatorGeneratorFUN, c("trendsWithAOMXIndicators", "trendsWithAOMNIndicators", "trendsWithAOMNIndicators", "trendsWithAOMXIndicators"))
  ) {
    addSymbolMachineWts(mktdata, Predictee = "CRASHACML", Predictors = Predictors, IndicatorGeneratorFUN = IndicatorGeneratorFUN)
    addSymbolMachineWtsDone <- TRUE
  }

  if(!addSymbolMachineWtsDone) {
    message("*** In UnRateMachinetradeModel LOVING MODEL not found. Using AD HOC model ***")
    addSymbolMachineWts(mktdata, Predictee = "CRASHACML", Predictors = Predictors, IndicatorGeneratorFUN = IndicatorGeneratorFUN)
    addSymbolMachineWtsDone <- TRUE
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


