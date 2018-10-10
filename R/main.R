#' runs the main
#'
#' space-saver - meant to be used at the beginning of a function
#'
#' @return xts object of monthly return results
#' @examples
#' \dontrun{
#' EXAMPLE
#' # EXAMPLE
#' }
#' @export
returnsUnRateEyeBall <- function() {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})

  # How to backtest a strategy in R
  # https://www.r-bloggers.com/how-to-backtest-a-strategy-in-r/
  # http://blog.fosstrading.com/2011/03/how-to-backtest-strategy-in-r.html

  # (1) data 'value' (try to optimize)
  addWilshire5000LogReturns() %>%      # will5000idxlogrets
  addCashLogReturns           %>%      # cashlogrets

  # (2) indicator(s)
  addUnRateEomData %>%                 # unrate

  # (3) use indicator(s)(unrate) to make rules:signals(weights)
  addWillShire5000EyeBallWts  %>%             # will5000logrets_wts
  addCashWts           %>%             # cashlogres_wts      (excess)
  # ret

  printTail("UnRateEyeBall") %>%

  # (4) apply in action
  portfolioMonthlyReturns %>%

  # (5) evaluate performance
  printCalendar("UnRateEyeBall")

})}


