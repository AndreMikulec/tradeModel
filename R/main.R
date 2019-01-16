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

  # How to backtest a strategy in R
  # https://www.r-bloggers.com/how-to-backtest-a-strategy-in-r/
  # http://blog.fosstrading.com/2011/03/how-to-backtest-strategy-in-r.html

  # (1) data 'value' (try to optimize)
  addLeadingWilshire5000LogReturns() %>%  #
  addLeadingCashLogReturns %>%          # PROBLEM IN HERE

  # (2) indicator(s)
  addUnRateEomData %>% # unrate


  # (3) use indicator(s)(unrate) to make rules:signals(weights)
  addWillShire5000EyeBallWts %>%   #

  appendCashWts              %>%      # (excess)

  printTail("Exact Schedule of Leading of Eye Ball returns and decisions") %>%

  # (4) apply in action
  portfolioMonthlyReturns %>%

  # (5) evaluate performance
  Lagging %>% printCalendar("Lagging Eye Ball returns")

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

  # How to backtest a strategy in R
  # https://www.r-bloggers.com/how-to-backtest-a-strategy-in-r/
  # http://blog.fosstrading.com/2011/03/how-to-backtest-strategy-in-r.html

  # (1) data 'value' (try to optimize)
  addLeadingWilshire5000LogReturns() %>%      #
  addLeadingCashLogReturns           %>%      #

  # (2) indicator(s)
  addUnRateEomData %>%                 # unrate

  # (3) use indicator(s)(unrate) to make rules:signals(weights)
  addWillShire5000MachineWts %>%       #
  appendCashWts              %>%       # (excess)

  printTail("Exact Schedule of Leading of UnRateMachine returns and decisions") %>%

  # (4) apply in action
  portfolioMonthlyReturns %>%

  # STRONG FEELING IT NEEDS PAST RETURNS ( AND NOT FUTURE RETURNS )
  # I NEED TO ASK ABOUT THIS
  # !!! CALENDAR SOMETIMES DOES NOT WORK!! - NEED FIXING?! - OFF BY 2-3 MONTHS !!!
  # (5) evaluate performance
  Lagging %>% printCalendar("Lagging UnRateMachine returns")

})}
# UnRateMachinetradeModel()

