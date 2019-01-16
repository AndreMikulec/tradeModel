#' Predicts the FRED WILL5000IND eom returns using UNRATE and the eyeball
#'
#' @return xts object of monthly return results
#' @examples
#' \dontrun{
#' EXAMPLE
#' # EXAMPLE
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
UnRateEyeBalltradeModel <- function() {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})

  # (1) data 'value' (try to optimize)
  addCurrLeadWilshire5000LogReturns() %>%  #
  addCurrLeadCashLogReturns %>%            #

  # (2) indicator(s)
  addUnRateEomData %>% # unrate

  # (3) use indicator(s)(unrate) to make rules:signals(weights)
  addWillShire5000EyeBallWts %>%   #

  appendCashWts              %>%      # (excess)

  printTail("Exact Schedule of Leading of Eye Ball Returns and Decisions", n = Inf) %>%

  # (4) apply in action
  portfolioMonthlyReturns %>%

  # (5) evaluate performance
  printCalendar("UnRateEyeBall Performance Returns")

})}
# UnRateEyeBalltradeModel()

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
UnRateMachinetradeModel <- function() {
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
# UnRateMachinetradeModel()

