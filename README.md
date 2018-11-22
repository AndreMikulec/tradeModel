# tradeModel

The goal of tradeModel is to determine an investment strategy that optimizes financial returns

## Installation

You can install tradeModel from github with:

``` r
# install.packages("devtools")
devtools::install_github("AndreMikulec/tradeModel")
```
Idea is based on  . . . 

How to backtest a strategy in R
https://www.r-bloggers.com/how-to-backtest-a-strategy-in-r/

http://blog.fosstrading.com/2011/03/how-to-backtest-strategy-in-r.html

``` r
# (1) data 'value' (try to optimize)
addWilshire5000LogReturns() %>%      # will5000idxlogrets
addCashLogReturns           %>%      # cashlogrets

# (2) indicator(s)
addUnRateEomData %>%                 # unrate

# (3) use indicator(s)(unrate) to make rules:signals(weights)
addWillShire5000Wts     %>%             # will5000logrets_wts
appendCashWts           %>%             # cashlogres_wts (excess)
printTail("UnRateEyeBall") %>%

# (4) apply in action
portfolioMonthlyReturns    %>%

# (5) evaluate performance
printCalendar("UnRateEyeBall")
```
