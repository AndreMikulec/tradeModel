#' Predicts the Symbol eom returns using UNRATE and the eyeball
#'
#' @return xts object of monthly return results
#
#' @examples
#' \dontrun{
#' # Predicts the FRED WILL5000IND eom returns using UNRATE and the eyeball
#' # UnRateEyeBalltradeModel(Symbol = "WILL5000IND", src = "FRED", Change = "apc")
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
UnRateEyeBalltradeModel <- function(Symbol = NULL, src = NULL, Change = NULL) {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})

  # (1) data 'value' (try to optimize)
  addCurrLeadSymbolAPCReturns(Symbol = Symbol, src = src)
  # addCurrLeadCashAPCReturns %>%            #

  # (2) indicator(s)
  addUnRateEomData %>% # unrate

  # (3) use indicator(s)(unrate) to make rules:signals(weights)
  # addWillShire5000EyeBallWts %>%   #
  addSymbolEyeBallWts(Symbol = Symbol, Change = Change) %>%

  appendCashAPCWts      %>%      # (excess)

  printTail("Exact Schedule of Leading of Eye Ball Returns and Decisions", n = Inf) %>%

  # (4) apply in action
  portfolioMonthlyReturns  %>%

  # (5) evaluate performance
  printCalendar("UnRateEyeBall Performance Returns")

})}
# UnRateEyeBalltradeModel(Symbol = "WILL5000IND", src = "FRED", Change = "apc")



#' Predicts the Yahoo SP500 eom returns using UNRATE and the eyeball
#'
#' @return xts object of monthly return results
#' \describe{
#'   \item{One}{First item}
#'   \item{Two}{Second item}
#' }
#' @examples
#' \dontrun{
#' EXAMPLE
#' # EXAMPLE
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
UnRateEyeBalltradeModelGSPC <- function() {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})

  # (1) data 'value' (try to optimize)
  addCurrLeadSP500LogReturns() %>%  #
  addCurrLeadCashLogReturns %>%            #

  # (2) indicator(s)
  addUnRateEomData %>% # unrate

  # (3) use indicator(s)(unrate) to make rules:signals(weights)
  addSP500EyeBallWts %>%   #

  appendCashWts              %>%      # (excess)

  printTail("Exact Schedule of Leading of Eye Ball Returns and Decisions", n = Inf) %>%

  # (4) apply in action
  portfolioMonthlyReturns  %>%

  # (5) evaluate performance
  printCalendar("UnRateEyeBall Performance Returns")

})}
# UnRateEyeBalltradeModelGSPC()



#' Predicts the SP500 eom returns using UNRATE and Machine learning
#'
#' @return xts object of monthly return results
#' @examples
#' \dontrun{
#' EXAMPLE
#' # EXAMPLE
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
UnRateMachinetradeModelGSPC <- function() {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})

  # (1) data 'value' (try to optimize)
  addCurrLeadSP500LogReturns() %>%      #
  addCurrLeadCashLogReturns           %>%      #

  # (2) indicator(s)
  addUnRateEomData %>%                 # unrate

  # (3) use indicator(s)(unrate) to make rules:signals(weights)
  addSP500MachineWts %>%       #
  appendCashWts              %>%       # (excess)

  printTail("Exact Schedule of Leading of UnRateMachine Returns and Decisions") %>%

  # (4) apply in action
  portfolioMonthlyReturns %>%

  # (5) evaluate performance
  printCalendar("UnRateMachine Performance Returns")

})}
# UnRateMachinetradeModelGSPC()



#' Predicts the FRED WILL5000IND eom returns using UNRATE and Machine learning
#'
#' @return xts object of monthly return results
#' @examples
#' \dontrun{
#' EXAMPLE
#' # EXAMPLE
#' }
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

