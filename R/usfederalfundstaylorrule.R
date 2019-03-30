

#' # NOTES, ADJUSTMENTS, FIXES TO BE INTEGRATED
#' # READ ( AND UNDERSTAND ) !ALL! FIRST BEFORE STARTING INTEGRATION
#'
#' [ ] Also add link to FED Funds Rate
#' FOMC will target the headline inflation rate
#' --------------------------------------------
#'
#' As St. Louis Fed President James Bullard detailed in a
#'
#'   2012 Regional Economist article,
#'   https://www.stlouisfed.org/publications/regional-economist/april-2012/recent-actions-increase-the-feds-transparency
#'
#' “The FOMC will target the
#'
#'   "headline inflation rate" as opposed to any other measure
#'   (e.g., core inflation, which excludes food and energy prices)
#'
#' because it makes sense to focus on the prices that U.S. households actually have to pay,” he wrote.
#'
#' Using Price Indexes to Measure the Inflation Rate
#' Wednesday, January 9, 2019
#' By Kristie Engemann, Public Affairs Staff
#' https://www.stlouisfed.org/open-vault/2019/january/price-indexes-measure-inflation
#' ORIG FROM
#' How do price indexes help economists track inflation?  http://ow.ly/RaRR50nR5EN
#' https://twitter.com/stlouisfed/status/1109273612471685120
#'
#' [ ] fix format abiguity (IF input can take in "Date" classed objects)
#' fix: input into the quantmod tradeModel function
#' if(format like "YYYY-MM-DD" ) { # "^\d{4}-[0-1][0-9]]-\d{2}$"
#'   d <- zoo::as.Date(d) ...
#' }
#'
#' [ ] fix see "Kevin L. Kliesen" email
#' #' y_t_less_y_ask: ... CHANGE NAME: see below
#' #'
#' #'     METHOD 1
#' FROM
#' #'     y_t_less_y_ask <- 100*(y_t - y_ask)/abs(y_ask)  # 100*(a-b)/b
#' #'     colnames(y_t_less_y_ask) <- "y_t_less_y_ask"
#' TO
#' #' 1993_TaylorRule_outputgap (JUST AS WRITTEN)
#' #'     y_t_less_y_ask <- y_t - y_ask
#' #'     colnames(y_t_less_y_ask) <- "y_t_less_y_ask"
#'
#' outputgap ... TO ....ouputgap_Taylor1993
#' y_t   ... TO ... y_t_Taylor1993
#' y_ask ... TO ... y_ask_Taylor1993
#'
#' outputgap ... TO ... modern_outputgap (less and more)
#'
#' y_t    ... TO ... y_t_modern
#' y_ask  ... TO ... y_ask_modern
#'
#' u_GAP_t # (Math is correct: just swap out: NROU ... TO ... NROUST
#' [ ] CHANGE . . .
#' UNRATE subtract off "CBO measure of the natural unemployment rate (NAIRU)"
#'
#'          CHANGE FROM
#'          Natural Rate of Unemployment (Long-Term) (NROU)
#'          https://fred.stlouisfed.org/series/NROU
#'            To (NOTE: CURRENLTY 2018-2019: THESE NUMBERS ARE THE SAME)
#'          Natural Rate of Unemployment (Short-Term)
#'          https://fred.stlouisfed.org/series/NROUST
#'
#'          [ ]
#'          CHANGE
#'            u_GAP_t <- unrate - nrou
#'          TO (SEE BELOW: Okun's law)
#'            u_GAP_t <- 2 * (unrate - nroust)
#'
#'            u_GAP_t
#'                Resource gap measures: gapt
#'                A commonly used rule of thumb called Okun's law posits that the
#'                  unemployment rate gap—the negative of the difference between the
#'                  unemployment rate and its natural rate—is typically
#'                    half as large as the output gap.
#'                    We implement Okun's law by allowing users to choose twice the unemployment gap as the resource gap.
#'
#'                Overview of Data tab
#'                Taylor Rule Utility
#'                https://www.frbatlanta.org/cqer/research/taylor-rule?panel=2
#'
#'        [ ] Add a comment / parameter
#'        0.30
#'        adjusted_5Y_BEI = TRUE
#'        Bullard's preferred measure: the 5Y BEI less 30 basis points (the adjusted 5Y BEI)
#'        https://research.stlouisfed.org/publications/economic-synopses/2019/01/15/is-the-fed-following-a-modernized-version-of-the-taylor-rule-part-2
#'
#'        Because the TIPS data are based on consumer price index (CPI) inflation,
#'        I subtract 30 basis points to make this expectations measure more closely
#'        correspond to the FOMC’s preferred measure of inflation, which is PCE inflation.
#'        . . .
#'        Incorporating real-time inflation expectations
#'        https://www.stlouisfed.org/~/media/files/pdfs/bullard/remarks/2018/bullard_memphis_economic_club_18_october_2018.pdf?la=en
#'
#'        Summary of Economic Projections (SEP)
#'        A modernized monetary policy rule
#'        https://www.stlouisfed.org/~/media/files/pdfs/bullard/remarks/2018/bullard_memphis_economic_club_18_october_2018.pdf?la=en
#'
#'        [ ]
#'        u_GAP_t passed parameter to i_t (seem to just NEED one function(i_t?))
#'        [ ] : True: i_t: just ONE function
#'              e.g. rho does not exist in "less modern version": So can be set to zero(0)
#'        i_t



#' less modernized FRB federal funds interest rate Taylor Rule
#'
#' @rdname lessModernFEDFundsTayorRule
#'
#' @author St. Louis Federal Board of Governors President James Bullard
#' @author Kevin L. Kliesen (articles author)
#' @author Andre Mikulec (adapted original code from the articles)
#' @references
#' \cite{Is the Fed Following a "Modernized" Version of the Taylor Rule? Part 1 - Posted 2019-01-15 \url{https://research.stlouisfed.org/publications/economic-synopses/2019/01/15/is-the-fed-following-a-modernized-version-of-the-taylor-rule-part-1}}
#' @references
#' \cite{Is the Fed Following a "Modernized" Version of the Taylor Rule? Part 2 - Posted 2019-01-15 \url{https://research.stlouisfed.org/publications/economic-synopses/2019/01/15/is-the-fed-following-a-modernized-version-of-the-taylor-rule-part-2}}
#' @references
#' \cite{Kevin L. Kliesen - Business Economist and Research Officer \url{https://research.stlouisfed.org/econ/kliesen/sel/}}
#' @references
#' \cite{Economic Synopses \url{https://research.stlouisfed.org/publications/economic-synopses/}}
#' @references
#' \cite{Taylor Rule Utility \url{https://www.frbatlanta.org/cqer/research/taylor-rule}}
#' @references
#' \cite{The source data used for the Taylor Rule Utility is available
#' \url{https://www.frbatlanta.org/-/media/documents/datafiles/cqer/research/taylor-rule/taylor-rule-data.xlsx}}
#'
#' @description
#' \preformatted{
#'
#' Taylor rule states that the monetary authority (e.g., the Federal Reserve)
#' should set its policy [federal funds interest] rate in the following manner.
#'
#' }
#' @details
#' \preformatted{
#'
#'
#' BEGIN UNORGANZIED/UN(RE)INVESTIGATED/UNINTEGRATED
#'
#' natural rate
#' OR
#' non-accelerating inflation rate of unemployment, or NAIRU
#' ---------------------------------------------------------
#'
#' The concept of the natural rate is related to what is sometimes termed the
#' non-accelerating inflation rate of unemployment, or NAIRU.
#'
#' In the long run, the two rates are expected to be equivalent; however, this need not be the
#' case over a near-term forecasting horizon.
#'
#' The gap between the actual unemployment rate and the natural rate is just
#' one of several factors CBO uses to make its near-term projections of inflation.
#'
#' Working Paper Series
#' 2007-06
#' Congressional Budget Office
#' Washington, D.C.
#' April 2007
#' The Natural Rate of Unemployment
#' David Brauer
#' (e-mail: davidb@cbo.gov)
#' https://cbo.gov/sites/default/files/cbofiles/ftpdocs/80xx/doc8008/2007-06.pdf
#'
#' https://fred.stlouisfed.org/series/NROU
#' https://fred.stlouisfed.org/series/NROU
#'
#' From the description, the case seems that (Short-Term)
#' is the better choice
#'
#' Natural Rate of Unemployment (Short-Term)
#' https://fred.stlouisfed.org/series/NROUST
#'
#' https://fred.stlouisfed.org/tags/series/?t=cbo%3Bnairu
#'
#'
#'
#' NAIRU theory
#' ------------
#'
#' According to the popular view,
#' once the actual unemployment rate falls to below the NAIRU, or the natural unemployment rate,
#'
#'   the rate of inflation tends to accelerate and economic activity becomes overheated.
#'   (This acceleration in the rate of inflation takes place through increases in the demand for goods and services.
#'    It also lifts the demand for workers and puts pressure on wages, reinforcing the growth in inflation).
#'
#' The Fed's Confusion Over the "Natural Rate" of Unemployment and Inflation
#' 07/20/2015
#' Frank Shostak
#' https://mises.org/library/fed%E2%80%99s-confusion-over-natural-rate-unemployment-and-inflation
#'
#'
#'
#' Determining the Federal Funds Rate
#' ----------------------------------
#'
#' Kliesen, Kevin L <kevin.l.kliesen@@stls.frb.org> clarification
#'
#' see HOTMAIL EMAIL (my "Sent" items box: I forwarded the reply to myself (for storage))
#' Subject: RE: TWO QUICK QUESTIONS: Is the Fed Following a "Modernized" Version of the Taylor Rule? Part 1/2
#'
#'
#'
#' END UNORGANZIED/UN(RE)INVESTIGATED/UNINTEGRATED
#'
#'
#' Authority (e.g., the Federal Reserve) should set its
#' policy rate (federal funds rate i(t)) in the following manner:
#'
#' GOAL
#'
#' i(t) <- r*  +  n(t) + alpha * (y(t) - y_ask)           + beta * (n(t) - n_ask)
#' i_t     r_ask  n_t             y_t                               n_t
#'
#' is actually calculated:
#'
#' METHOD 1
#'
#' y_t_less_y_ask <- 100 * (y_t - y_ask)/y_ask
#'
#' i_t <- r_ask +  n_t + alpha *  (y_t_less_y_ask) + beta * (n_t - n_ask)
#'
#' METHOD 2
#'
#' i_t <- r_ask +  n_t + alpha *  (output_gap    ) + beta * (n_t - n_ask)
#'
#' Three Key Principles
#' --------------------
#'
#' First
#' Fed should raise its federal funds target rate proportionally more
#' when inflation increases.  This is known as the Taylor principle.
#'
#' Second
#' The interest rate should be adjusted in response to the
#' output gap, a measure of "slack" in the economy.
#' This is known as the Phillips relationship, whereby inflation decreases (increases)
#' if real GDP decreases (increases) relative to real potential GDP.
#' (related to alpha and beta)
#'
#' Third
#' Taylor stipulated that the "equilibrium real interest rate" r_ask(r*),
#' should be fixed over time at 2 percent.
#'
#' Although Taylor believes that r_ask(r*) should remain invariant over time,
#' other policymakers have instead adopted the position that
#' r_ask(r*) is time varying and depends importantly on the following:
#'
#'   (1) underlying growth rate of the economy
#'     and
#'   (2) other factors, such as the
#'       demand for risk-free Treasury securities (i.e., "safe assets") ARTICLE_LINK
#'
#' Implemenaton
#' ------------
#'
#' GOAL
#'
#' i_t : nominal federal funds interest rate
#'
#' INPUT
#'
#' r_ask: equilibrium real interest rate
#'        should be fixed over time at 2 percent
#'
#'   r_ask <- 2.00
#'
#' n_t: current inflation rate measure from one year earlier
#'
#'     Measuring inflation trends
#'     "
#'     The Federal Open Market Committee (FOMC) has determined that
#'     "inflation at the rate of 2 percent, as measured by the
#'     annual change in the price index for personal consumption expenditures [PCE],
#'     is most consistent over the longer run with the Federal Reserves statutory mandate"
#'     for price stability.
#'     "
#'     How this graph was created:
#'     Search for “PCEPI,”
#'     check the three series, and click on “Add to Graph.”
#'     From the “Edit Graph” menu, change the units to “Percent Change from Year Ago.”
#'     Change the frequency to “Monthly” and the starting date to “2017-03-01.”
#'
#'     NOT THIS:
#'     Personal Consumption Expenditures (PCE)
#'     Seasonally Adjusted Annual Rate
#'     Frequency: Monthly
#'     # PLUS a three month delay to the reporting dta
#'     https://fred.stlouisfed.org/series/PCE
#'
#'     # PCEPI into "Percent Change from Year Ago" results in INFLATIONRATE
#'     THIS ONE:
#'     PCEPI "headline" (volitile)
#'     Personal Consumption Expenditures: Chain-type Price Index, Index 2012=100, Seasonally Adjusted (PCEPI)
#'     Last Updated: 2019-03-01 7:49 AM CST
#'     (last value): 2018-12-01  108.929
#'     Last Updated (publish date) is three months ago
#'     Personal Consumption Expenditures: Chain-type Price Index (PCEPI)
#'     https://fred.stlouisfed.org/series/PCEPI
#'
#'     # SO: inflation rate measured by the headline PCE
#'     PCEPI into "Percent Change from Year Ago" -> INFLATIONRATE
#'
#'     OTHERS:
#'
#'     "core PCE"
#'     PCEPILFE
#'     Personal Consumption Expenditures Excluding Food and Energy
#'     (Chain-Type Price Index), Monthly, Seasonally Adjusted
#'     Personal Consumption Expenditures Excluding Food and Energy (Chain-Type Price Index) (PCEPILFE)
#'     https://fred.stlouisfed.org/series/PCEPILFE
#'
#'     PCETRIM12M159SFRBDAL "trimmed" (Federal Reserve Bank of Dallas)
#'     Trimmed Mean PCE Inflation Rate, Seasonally Adjusted
#'     Trimmed Mean PCE Inflation Rate (PCETRIM12M159SFRBDAL)
#'     https://fred.stlouisfed.org/series/PCETRIM12M159SFRBDAL
#'
#'     Measuring inflation trends
#'     Posted on May 14, 2018
#'     David Wheelock
#'     Why use different inflation measures for policy analysis?
#'     https://fredblog.stlouisfed.org/2018/05/measuring-inflation-trends/
#'
#'     require(xts)
#'     PCEPI <- quantmod::getSymbols("PCEPI", src="FRED", auto.assign = F)
#'     index(PCEPI) <- DescTools::AddMonths(index(PCEPI),3)
#'     index(PCEPI) <- index(PCEPI) - 1 # now at the end of the current month
#'
#'  # percent change from a year ago
#'  n_t <- inflation_rate <- 100*(PCEPI - lag.xts(PCEPI,12))/abs(lag.xts(PCEPI,12))
#'  colnames(n_t) <- "n_t"
#'
#' y_t_less_y_ask OR output_gap (components) follow . . . :
#'
#' y_t: Real Gross Domestic Product (GDPC1)
#'
#'     Real Gross Domestic Product (GDPC1)
#'     Seasonally Adjusted Annual Rate
#'     Frequency: Quarterly
#'     E.g.
#'     As of MAR 15 2019:
#'     Last Updated: 2019-02-28 8:03 AM CST
#'     (last value): 2018-10-01  18784.632
#'     True as of date: 2018-12-31
#'     Last Updated (publish date) is slightly less than 2 months later
#'     So this is published 4x year: end of FEB, MAY, AUG, NOV.
#'     Real Gross Domestic Product (GDPC1)
#'     https://fred.stlouisfed.org/series/GDPC1
#'
#'     require(xts)
#'     GDPC1 <- quantmod::getSymbols("GDPC1", src="FRED", auto.assign = F)
#'     # "as of record date" to "last updated date"
#'     # (five months)
#'     index(GDPC1) <- DescTools::AddMonths(index(GDPC1),5)
#'     # first report
#'     From <- head(index(GDPC1),1)
#'     # last report
#'     To <-tail(index(GDPC1),1)
#'     # create intermediate monthly observations
#'     GDPC1 <- merge(GDPC1, xts(, seq(from = From, to = To, by = "months")) )
#'     # fill in intermediate months with last known data
#'     GDPC1 <- na.locf(GDPC1)
#'     # Change "beginning of month dates" to "end of month dates".
#'     index(GDPC1) <- index(GDPC1) - 1 # subtrace off one(1) day
#'
#'   y_t <- GDPC1; colnames(y_t) <- "y_t"
#'
#' y_ask: real potiential GDP
#'
#'     Real Potential Gross Domestic Product (GDPPOT)
#'     Not Seasonally Adjusted
#'     Frequency: Quarterly
#'     E.g.
#'     As of MAR 15 2019:
#'     Last Updated: 2019-02-06 9:01 AM CST
#'     (last value): 2029-10-01
#'     (last useful value): 2018-10-01
#'     True as of date: 2018-12-31
#'     Last Updated (publish date) is five(5) weeks later
#'     So this is published 4x year: just after the beginning of FEB, MAY, AUG, NOV.
#'     Real Potential Gross Domestic Product (GDPPOT)
#'     https://fred.stlouisfed.org/series/GDPPOT
#'
#'     This will use (almost) the same math as "Real Gross Domestic Product (GDPC1)"
#'
#'     require(xts)
#'     GDPPOT <- quantmod::getSymbols("GDPPOT", src="FRED", auto.assign = F)
#'     # "as of record date" to "last updated date"
#'     # (four months and one week: round up to the end of five(5) months)
#'     index(GDPPOT) <- DescTools::AddMonths(index(GDPPOT),5)
#'     # first report
#'     From <- head(index(GDPPOT),1)
#'     # last "useful"
#'     To <- tail(index(GDPPOT),1)
#'     # create intermediate monthly observations
#'     GDPPOT <- merge(GDPPOT, xts(, seq(from = From, to = To, by = "months")) )
#'     # fill in intermediate months with last known data
#'     GDPPOT <- na.locf(GDPPOT)
#'     # Change "beginning of month dates" to "end of month dates".
#'     index(GDPPOT) <- index(GDPPOT) - 1 # subtrace off one(1) day
#'
#'   y_ask <- GDPPOT; colnames(y_ask) <- "y_ask"
#'
#' y_t_less_y_ask:
#'
#'     METHOD 1
#'     Actually the math seems to not be a  "subtraction".
#'     Author's word "difference" may mean "log difference"
#'     The method "log difference" is sometimes used as an approximation of "percent change."
#'     See:
#'     100*(Real Gross Domestic Product-Real Potential Gross Domestic Product)/Real Potential Gross Domestic Product
#'     https://fred.stlouisfed.org/graph/?g=f1cZ#0
#'
#'     y_t_less_y_ask <- 100*(y_t - y_ask)/abs(y_ask)  # 100*(a-b)/b
#'     colnames(y_t_less_y_ask) <- "y_t_less_y_ask"
#'
#'     OR THE SAME . . .
#'
#'     100*(Real Gross Domestic Product-Real Potential Gross Domestic Product)/Real Potential Gross Domestic Product
#'     a <- GDPC1
#'     b <- GDPPOT
#'     Edit Graph: Formula; 100*(a-b)/b
#'     https://fred.stlouisfed.org/graph/?g=f1cZ#0
#'
#'     y_t_less_y_ask <- . . .  data from graph
#'
#'     word "difference" pending clarification
#'     From: Andre Mikulec <andre_mikulec@@hotmail.com>
#'     Sent: Sunday, March 17, 2019 11:29 PM
#'     To: kliesen@@stls.frb.org
#'     Subject: TWO QUICK QUESTIONS: Is the Fed Following a “Modernized” Version of the Taylor Rule? Part 1/2
#'
#'
#' output_gap:  a measure of "slack" in the economy
#'
#'     See URL web page footnotes #2:
#'     The difference between the two (GDP and potential GDP) being the  "output gap"
#'     percentage deviations—that is, the percent that real GDP is above or below real potential GDP.
#'     The original Taylor rule used the four-quarter percent change in the GDP price deflator.
#'
#'     Gross Domestic Product: Implicit Price Deflator (GDPDEF)
#'     Seasonally Adjusted
#'     Frequency: Quarterly
#'     E.g.
#'     As of MAR 15 2019:
#'     Last Updated: 2019-02-28
#'     (last value): 2018-10-01
#'     True as of date: 2018-12-31
#'     Last Updated (publish date) is slightly less than 2 months later.
#'     So this is published 4x year: end of FEB, MAY, AUG, NOV.
#'     Gross Domestic Product: Implicit Price Deflator (GDPDEF)
#'     https://fred.stlouisfed.org/series/GDPDEF
#'
#'     require(xts)
#'     GDPDEF <- quantmod::getSymbols("GDPDEF", src="FRED", auto.assign = F)
#'     # "as of record date" to "last updated date"
#'     # (five months)
#'     index(GDPDEF) <- DescTools::AddMonths(index(GDPDEF),5)
#'     # first report
#'     From <- head(index(GDPDEF),1)
#'     # last report
#'     To   <- tail(index(GDPDEF),1)
#'     # create intermediate monthly observations
#'     GDPDEF <- merge(GDPDEF, xts(, seq(from = From, to = To, by = "months")) )
#'     # fill in intermediate months with last known data
#'     GDPDEF <- na.locf(GDPDEF)
#'     # Change "beginning of month dates" to "end of month dates".
#'     index(GDPDEF) <- index(GDPDEF) - 1 # subtrace off one(1) day
#'
#'   METHOD 2 (original Taylor Rule)
#'
#'   output_gap <- 100*(GDPDEF - lag.xts(GDPDEF,12))/abs(lag.xts(GDPDEF,12))
#'   colnames(output_gap) <- "output_gap"
#'
#' n_ask: Feds inflation target
#'        which is currently 2 percent for the personal consumption expenditures price index.
#'
#'   n_ask <- 2.00
#'
#' alpha:
#' beta:
#'       In Taylors original specification,
#'       the coefficients on the output and inflation gaps,
#'       a and ß, respectively, were each 0.5.
#'
#'   alpha <- 0.5
#'   beta  <- 0.5
#'
#' GOAL
#'
#' i_t : nominal federal funds interest rate
#'
#' calculated:
#'
#' METHOD 1
#'
#' i_t_method1 <- r_ask +  n_t + alpha * (y_t_less_y_ask) + beta * (n_t - n_ask)
#' colnames(i_t_method1) <- "i_t_method1"
#' dygraphs::dygraph(i_t_method1, main = "Less Modernized method 1 Nominal Federal Funds Interest Rate")
#'
#' METHOD  2
#'
#' i_t_method2 <- r_ask +  n_t + alpha *  (output_gap    ) + beta * (n_t - n_ask)
#' colnames(i_t_method2) <- "i_t_method2"
#' dygraphs::dygraph(i_t_method2, main = "Less Modernized method 2 Nominal Federal Funds Interest Rate")
#'
#' }
"_PACKAGE"



#' @rdname lessModernFEDFundsTayorRule
#' @return n_t: xts object; less modernized current inflation rate measure from one year earlier
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom quantmod getSymbols
#' @importFrom DescTools AddMonths
n_t <- function() {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  # require(xts)
  # Personal Consumption Expenditures: Chain-type Price Index, Index 2012=100, Seasonally Adjusted (PCEPI)
  # https://fred.stlouisfed.org/series/PCEPI
  PCEPI <- quantmod::getSymbols("PCEPI", src="FRED", auto.assign = F)
  index(PCEPI) <- DescTools::AddMonths(index(PCEPI),3)
  index(PCEPI) <- index(PCEPI) - 1 # now at the end of the current month

  # percent change
  n_t <- inflation_rate <- 100*(PCEPI - lag.xts(PCEPI,12))/abs(lag.xts(PCEPI))
  colnames(n_t) <- "n_t"
  return(n_t)

})}
#' @rdname lessModernFEDFundsTayorRule
#' @return y_t: xts object; less modernized real gross domestic product (GDP)
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom quantmod getSymbols
#' @importFrom DescTools AddMonths LastDayOfMonth
y_t <- function() {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  # require(xts)
  # Real Gross Domestic Product (GDPC1)
  # https://fred.stlouisfed.org/series/GDPC1
  GDPC1 <- quantmod::getSymbols("GDPC1", src="FRED", auto.assign = F)
  # "as of record date" to "last updated date"
  # (five months)
  index(GDPC1) <- DescTools::AddMonths(index(GDPC1),5)
  # first report
  From <- head(index(GDPC1),1)
  # last "useful" report and I want a future date (so, I can predict))
  # To   <- max(tail(index(GDPPOT)[index(GDPPOT) <= Sys.Date()],1), (DescTools::LastDayOfMonth(Sys.Date()) + 1))
  # last report
  To  <- tail(index(GDPC1),1)
  # create intermediate monthly observations
  GDPC1 <- merge(GDPC1, xts(, seq(from = From, to = To, by = "months")) )
  # fill in intermediate months with last known data
  GDPC1 <- na.locf(GDPC1)
  # Change "beginning of month dates" to "end of month dates".
  index(GDPC1) <- index(GDPC1) - 1 # subtrace off one(1) day

  y_t <- GDPC1; colnames(y_t) <- "y_t"
  return(y_t)

})}
#' @rdname lessModernFEDFundsTayorRule
#' @return y_ask: xts object; less modernized real potiential GDP
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom quantmod getSymbols
#' @importFrom DescTools AddMonths LastDayOfMonth
y_ask <- function() {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  # require(xts)
  # Real Potential Gross Domestic Product (GDPPOT)
  # https://fred.stlouisfed.org/series/GDPPOT
  GDPPOT <- quantmod::getSymbols("GDPPOT", src="FRED", auto.assign = F)
  # "as of record date" to "last updated date"
  # (four months and one week: round up to the end of five(5) months)
  index(GDPPOT) <- DescTools::AddMonths(index(GDPPOT),5)
  # first report
  From <- head(index(GDPPOT),1)
  # last "useful"
  To <- tail(index(GDPPOT),1)
  # create intermediate monthly observations
  GDPPOT <- merge(GDPPOT, xts(, seq(from = From, to = To, by = "months")) )
  # fill in intermediate months with last known data
  GDPPOT <- na.locf(GDPPOT)
  # Change "beginning of month dates" to "end of month dates".
  index(GDPPOT) <- index(GDPPOT) - 1 # subtrace off one(1) day

  y_ask <- GDPPOT; colnames(y_ask) <- "y_ask"
  return(y_ask)

})}
#' @rdname lessModernFEDFundsTayorRule
#' @return y_t_less_y_ask: xts object; less modernized "Percent change difference from
#' "real potiential GDP" to "real gross domestic product"
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom quantmod getSymbols
#' @importFrom DescTools AddMonths LastDayOfMonth
y_t_less_y_ask  <- function() {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  # require(xts)
  # METHOD 1
  y_t   <- y_t()
  y_ask <- y_ask()

  y_t_less_y_ask <- 100*(y_t - y_ask)/abs(y_ask) # 100*(a-b)/b
  colnames(y_t_less_y_ask) <- "y_t_less_y_ask"
  return(y_t_less_y_ask)

})}
#' @rdname lessModernFEDFundsTayorRule
#' @return output_gap: xts object: less modernized "output gap"
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom quantmod getSymbols
#' @importFrom DescTools AddMonths LastDayOfMonth
output_gap  <- function() {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  # require(xts)
  # Gross Domestic Product: Implicit Price Deflator (GDPDEF)
  # https://fred.stlouisfed.org/series/GDPDEF
  GDPDEF <- quantmod::getSymbols("GDPDEF", src="FRED", auto.assign = F)
  # "as of record date" to "last updated date"
  # (five months)
  index(GDPDEF) <- DescTools::AddMonths(index(GDPDEF),5)
  # first report
  From <- head(index(GDPDEF),1)
  # last report
  To <- tail(index(GDPDEF),1)
  # create intermediate monthly observations
  GDPDEF <- merge(GDPDEF, xts(, seq(from = From, to = To, by = "months")) )
  # fill in intermediate months with last known data
  GDPDEF <- na.locf(GDPDEF)
  # Change "beginning of month dates" to "end of month dates".
  index(GDPDEF) <- index(GDPDEF) - 1 # subtrace off one(1) day

  # METHOD 2 (original Taylor Rule)
  output_gap <- 100*(GDPDEF - lag.xts(GDPDEF,12))/abs(lag.xts(GDPDEF,12))
  colnames(output_gap) <- "output_gap"
  return(output_gap)

})}
#' @rdname lessModernFEDFundsTayorRule
#' @return i_t_method1: xts object; version 1 of
#' less modernized federal funds nominal interest rate
#' @examples
#' \dontrun{
#' dygraphs::dygraph(i_t_method1(), main = "Less Modernized method 1 Nominal Federal Funds Interest Rate")
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
i_t_method1  <- function() {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  r_ask <- 2.00
  n_ask <- 2.00

  alpha <- 0.5
  beta  <- 0.5

  # METHOD 1

  i_t_method1 <- r_ask +  n_t() + alpha * (y_t_less_y_ask()) + beta * (n_t() - n_ask)
  colnames(i_t_method1) <- "i_t_method1"
  # dygraphs::dygraph(i_t_method1, main = "Nominal Federal Funds Interest Rate")
  return(i_t_method1)

})}
#' @rdname lessModernFEDFundsTayorRule
#' @return i_t_method2: xts object; version 2 of
#' less modernized federal funds nominal interest rate
#' @examples
#' \dontrun{
#' dygraphs::dygraph(i_t_method2(), main = "Less Modernized method 2 Nominal Federal Funds Interest Rate")
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
i_t_method2  <- function() {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  r_ask <- 2.00
  n_ask <- 2.00

  alpha <- 0.5
  beta  <- 0.5

  # METHOD 2

  i_t_method2 <- r_ask +  n_t() + alpha * (output_gap()) + beta * (n_t() - n_ask)
  colnames(i_t_method2) <- "i_t_method2"
  # dygraphs::dygraph(i_t_method2, main = "Nominal Federal Funds Interest Rate")
  return(i_t_method2)

})}
#' @rdname lessModernFEDFundsTayorRule
#' @return lessModernFEDFundsTayorRule: xts object; version 2 of
#' less modernized federal funds nominal interest rate
#' @examples
#' \dontrun{
#' # less(L) modern(M) Federal(F) Funds(F) Taylor(T) Rule(R): LMFTTR
#' dygraphs::dygraph(LMFFTR(), main = "Less Modernized method 1 Nominal Federal Funds Interest Rate")
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
LMFFTR  <- function() {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  # 1 instead of 2, because this #1 looks most like the graph in the article
  res <- i_t_method1()
  colnames(res) <- "LMFFTR"
  return(res)

})}



#' more modernized FRB federal funds interest rate Taylor Rule
#'
#' @rdname moreModernFEDFundsTayorRule
#'
#' @author St. Louis Federal Board of Governors President James Bullard
#' @author Kevin L. Kliesen (articles author)
#' @author Andre Mikulec (adapted original code from the articles)
#' @references
#' \cite{Is the Fed Following a “Modernized” Version of the Taylor Rule? Part 1 - Posted 2019-01-15 \url{https://research.stlouisfed.org/publications/economic-synopses/2019/01/15/is-the-fed-following-a-modernized-version-of-the-taylor-rule-part-1}}
#' @references
#' \cite{Is the Fed Following a “Modernized” Version of the Taylor Rule? Part 2 - Posted 2019-01-15 \url{https://research.stlouisfed.org/publications/economic-synopses/2019/01/15/is-the-fed-following-a-modernized-version-of-the-taylor-rule-part-2}}
#' @references
#' \cite{Kevin L. Kliesen - Business Economist and Research Officer \url{https://research.stlouisfed.org/econ/kliesen/sel/}}
#' @references
#' \cite{Economic Synopses \url{https://research.stlouisfed.org/publications/economic-synopses/}}
#' @references
#' \cite{Taylor Rule Utility \url{https://www.frbatlanta.org/cqer/research/taylor-rule}}
#' @references
#' \cite{The source data used for the Taylor Rule Utility is available
#' \url{https://www.frbatlanta.org/-/media/documents/datafiles/cqer/research/taylor-rule/taylor-rule-data.xlsx}}
#'
#' @description
#' \preformatted{
#' Bullard proposes an alternative, what he terms a
#' "modernized" version, of the Taylor rule
#' [to set its policy [federal funds interest] rate.]
#' }
#'
#' @details
#' \preformatted{
#'
#' In a recent speech,
#' Federal Reserve Bank of St. Louis President James Bullard
#' presented an alternative version of the Taylor rule
#' that reflects three developments that todays monetary policymakers
#' routinely confront.
#'
#' GOAL
#'
#' i(t) <- rho * i(t-1) + (1 - rho)(r_ask(t) + n_ask + phi(n) * n_GAP(t) + phi(u) * u_GAP(t))
#' i_t           i_tm1              r_ask_t            phi_n    n_GAP_t    phi_u    u_gap_t
#'
#' is actually calculated:
#'
#' METHOD 3
#'
#' i_t <- rho * i_tm1 + (1 - rho)(r_ask_t + n_ask + phi_n * n_GAP_t + phi_u * u_GAP_t)
#'
#' Three Key Principles
#' --------------------
#'
#' First,
#' the economy has entered an economic regime of low interest rates
#' that reflects, importantly, weak productivity growth and a
#' strong demand for safe assets.
#'
#' Second,
#' the Fed appears to have successfully engineered a regime of
#' low and relatively stable inflation expectations
#' that are anchored near the Feds inflation target.
#'
#' Third,
#' The Phillips relationship that posits a negative relationship between
#' inflation and the current level of the unemployment rate
#' and a measure of the "natural rate" has all but disappeared.
#' (Accordingly, this development means falling levels of the
#' unemployment rate relative to its natural rate will have a
#' very small effect on inflation.)
#'
#' Implemenaton
#' ------------
#'
#' GOAL
#'
#' i_t : nominal federal funds interest rate
#'
#' INPUT
#'
#' i_tm1: one quarter lag in the federal funds rate
#'
#'     Four series exist.
#'     I just take the the general one: the monthlies.
#'     Frequency: Monthly
#'     Notes:  Averages of daily figures.
#'     Effective Federal Funds Rate (FEDFUNDS)
#'     https://fred.stlouisfed.org/series/FEDFUNDS
#'
#'     require(xts)
#'     FEDFUNDS <- quantmod::getSymbols("FEDFUNDS", src = "FRED", auto.assign = F)
#'     index(FEDFUNDS) <- DescTools::AddMonths(index(FEDFUNDS),1)
#'     FEDFUNDS <- lag.xts(FEDFUNDS,3)
#'     # Change "beginning of month dates" to "end of month dates".
#'     index(FEDFUNDS) <- index(FEDFUNDS) - 1
#'
#'   i_tm1 <- FEDFUNDS; colnames(i_tm1) <- "i_tm1"
#'
#' rho:  fixed coefficient of "one quarter lag in the federal funds rate"
#'       This is a smoothing parameter with a value of 0.85.
#'       This means that the past periods policy rate is
#'       extraordinarily important for setting the current periods policy rate.
#'
#'   rho <- 0.85
#'
#' u_GAP_t: "[unemployment] output gap" measured as the
#'          difference between the
#'
#'     (1) current unemployment rate
#'       and the
#'     (2) Congressional Budget Offices natural rate of unemployment
#'
#'     Civilian Unemployment Rate (UNRATE)
#'     https://fred.stlouisfed.org/series/UNRATE/
#'
#'     Natural Rate of Unemployment (Long-Term) (NROU)
#'     U.S. Congressional Budget Office, Natural Rate of Unemployment (Long-Term) [NROU
#'     At the end of the quarter, the last useful value date
#'     and the Last Updated date are one month apart.
#'     Percent, Not Seasonally Adjusted
#'     Frequency: Quarterly
#'     E.g.
#'     As of MAR 15 2019
#'     Last Updated: 2019-02-06 9:01 AM CST
#'     (last value): 2029-10-01  4.455
#'     (last useful value): 2018-10-01  4.607
#'     True as of date: 2018-12-31
#'     Last Updated (publish date) is five(5) weeks later
#'     So this is published 4x year: just after the beginning of FEB, MAY, AUG, NOV.
#'     Natural Rate of Unemployment (Long-Term) (NROU)
#'     https://fred.stlouisfed.org/series/NROU
#'
#'     require(xts)
#'
#'     UNRATE <- quantmod::getSymbols("UNRATE", src = "FRED", auto.assign = F)
#'     index(UNRATE) <- DescTools::AddMonths(index(UNRATE),1)
#'     # Change "beginning of month dates" to "end of month dates".
#'     index(UNRATE) <- index(UNRATE) - 1
#'
#'     unrate <- UNRATE; colnames(unrate) <- "unrate"
#'
#'     Use (almost) the same math as "Real Gross Domestic Product (GDPC1)".
#'
#'     NROU <- quantmod::getSymbols("NROU", src="FRED", auto.assign = F)
#'     # "as of record date" to "last updated date"
#'     # (four months and one week: round up to the end of five(5) months)
#'     index(NROU) <- DescTools::AddMonths(index(NROU),5)
#'     # first report
#'     From <- head(index(NROU),1)
#'     # last "useful"
#'     To   <- tail(index(NROU),1)
#'     # create intermediate monthly observations
#'     NROU <- merge(NROU, xts(, seq(from = From, to = To, by = "months")) )
#'     # fill in intermediate months with last known data
#'     NROU <- na.locf(NROU)
#'     # Change "beginning of month dates" to "end of month dates".
#'     index(NROU) <- index(NROU) - 1 # subtrace off one(1) day
#'
#'     nrou <- NROU; colnames(nrou) <- "nrou"
#'
#'   u_GAP_t <- unrate - nrou
#'   colnames(u_GAP_t) <- "u_GAP_t"
#'
#' n_GAP_t: "inflation gap" measured as the
#'          difference between a
#'
#'     (1) market-based measure of "inflation expectations"
#'       and the
#'     (2) Feds inflation target.
#'
#'     Specifically, "inflation expectations" are measured as the
#'     difference between
#'
#'     (1) the nominal yield on a 5-year (5Y) Treasury security
#'       and
#'     (2) the yield on an inflation-adjusted (real) 5Y Treasury inflation-protected security (TIPS).
#'
#'     This difference is sometimes called the breakeven inflation (BEI) rate.
#'
#'     5-Year Breakeven Inflation Rate (T5YIE)
#'     Percent, Not Seasonally Adjusted, Daily
#'       a measure of expected inflation derived from
#'         5-Year Treasury Constant Maturity Securities
#'         (https://fred.stlouisfed.org/series/DGS5 ) - Daily
#'           and
#'         5-Year Treasury Inflation-Indexed Constant Maturity Securities
#'         (https://fred.stlouisfed.org/series/DFII5 ). - Daily
#'     The latest value implies what
#'     market participants expect inflation to be in the next 5 years, on average.
#'     Percent
#'     Not Seasonally Adjusted
#'     Daily
#'     5-Year Breakeven Inflation Rate (T5YIE)
#'     https://fred.stlouisfed.org/series/T5YIE
#'
#'     The Treasury uses the consumer price index to adjust the nominal price of the TIPS.
#'     Bullard subtracts 30 basis points from the 5Y BEI.
#'     (In article, this is called "the adjusted 5Y BEI".)
#'     Bullard argues this better accounts for the
#'
#'       (1) upward bias of the consumer price index
#'         relative to
#'       (2) inflation measured by the PCE inflation rate
#'
#'     require(xts)
#'
#'     # market inflation expectations
#'     T5YIE <- quantmod::getSymbols("T5YIE", src = "FRED", auto.assign = F)
#'     # acquire the last observation of the month
#'     # and re-date it to be the last day of the month
#'     mkt_inf_exp <- (To.Monthly(T5YIE, OHLC = FALSE, indexAt = "lastof") - 0.30)
#'     mkt_inf_exp <- mkt_inf_exp[index(mkt_inf_exp) <= Sys.Date()]
#'     n_ask <- 2.00
#'
#'   n_GAP_t <- mkt_inf_exp - n_ask
#'   colnames(n_GAP_t) <- "n_GAP_t"
#'
#' r_ask_t: "equilibrium real interest rate"
#'          This is different from the non-modern Taylor rule.
#'          This now varies over time (instead of being set at a fixed 2 percent).
#'
#'     This is measured as the trend interest rate estimated
#'     from a Hodrick-Prescott filter of the following:
#'
#'       (1) the 1-year nominal constant maturity Treasury yield
#'         less
#'       (2) the four-quarter change in the Federal Reserve Bank of Dallas
#'           trimmed mean measure of the personal consumption expenditures (PCE) inflation rate
#'
#'     1-Year Treasury Constant Maturity Rate (GS1)
#'     Three data series exist. I just take the monthlies.
#'     (last value) date and Last Updated date are the same.
#'     Percent
#'     Not Seasonally Adjusted
#'     Monthly
#'     1-Year Treasury Constant Maturity Rate (GS1)
#'     https://fred.stlouisfed.org/series/GS1
#'
#'     require(xts)
#'
#'     GS1 <- quantmod::getSymbols("GS1", src = "FRED", auto.assign = F)
#'     index(GS1) <- DescTools::AddMonths(index(GS1), 1)
#'     # Change "beginning of month dates" to "end of month dates".
#'     index(GS1) <-  index(GS1) - 1
#'
#'     # one year nominal constant maturity yield
#'     year_1_nom_cm_yield <- GS1
#'     colnames(year_1_nom_cm_yield) <- "year_1_nom_cm_yield"
#'
#'     # Less . . .
#'
#'     "four quarter change of" the Trimmed Mean PCE inflation rate
#'     produced by the Federal Reserve Bank of Dallas.
#'
#'     Trimmed Mean PCE Inflation Rate (PCETRIM1M158SFRBDAL)
#'     Percent Change at Annual Rate
#'     Seasonally Adjusted
#'     Frequency: Monthly
#'     E.g.
#'     As of MAR 15 2019:
#'     Last Updated: 2019-03-01 2:33 PM CST
#'     (last value): 2018-12-01   1.69
#'     Last Updated (publish date) is 3 months later.
#'     Trimmed Mean PCE Inflation Rate (PCETRIM1M158SFRBDAL)
#'     https://fred.stlouisfed.org/series/PCETRIM1M158SFRBDAL
#'
#'     require(xts)
#'     PCETRIM1M158SFRBDAL <- quantmod::getSymbols("PCETRIM1M158SFRBDAL", src="FRED", auto.assign = F)
#'     index(PCETRIM1M158SFRBDAL) <- DescTools::AddMonths(index(PCETRIM1M158SFRBDAL),3)
#'     index(PCETRIM1M158SFRBDAL) <- index(PCETRIM1M158SFRBDAL) - 1 # now at the end of the current month
#'
#'     # four quarter change of "trimmed mean PCE inflation rate"
#'     change_of_tm_PCE_infl_rate <- PCETRIM1M158SFRBDAL - lag.xts(PCETRIM1M158SFRBDAL,12)
#'     colnames(change_of_tm_PCE_infl_rate) <- "change_of_tm_PCE_infl_rate"
#'
#'     (I WAS TIRED AND I MAY HAVE ASKED THE WRONG QUESTION)
#'     word "difference" pending clarification
#'     From: Andre Mikulec <andre_mikulec@@hotmail.com>
#'     Sent: Sunday, March 17, 2019 11:29 PM
#'     To: kliesen@@stls.frb.org
#'     Subject: TWO QUICK QUESTIONS: Is the Fed Following a “Modernized” Version of the Taylor Rule? Part 1/2
#'
#'   r_ask_t <- year_1_nom_cm_yield - change_of_tm_PCE_infl_rate
#'   colnames(r_ask_t) <- "r_ask_t"
#'
#' n_ask: Federal Open Market Committees (FOMC) inflation target
#'        which is set at 2 percent for the personal consumption expenditures price index.
#'
#'     Note
#'     n_t: current inflation rate [ measured from one year earlier ]
#'     in the non-modernized version of the Taylor Rule
#'     is replaced by *this* n_ask: the Federal Open Market Committee's (FOMC's) inflation target
#'
#'     History (earliest to latest(first written to ammended)):
#'
#'     Federal Reserve issues FOMC statement of longer-run goals and policy strategy
#'     Last Update: January 25, 2012
#'     https://www.federalreserve.gov/newsevents/pressreleases/monetary20120125c.htm
#'
#'     What is the statement on longer-run goals and monetary policy strategy and why does the Federal Open Market Committee put it out?
#'     Last Update: May 15, 2017
#'     https://www.federalreserve.gov/faqs/statement-on-longer-run-goals-monetary-policy-strategy-fomc.htm
#'
#'     Statement on Longer-Run Goals and Monetary Policy Strategy (PDF)
#'     Amended January 29, 2019
#'     Adopted effective January 24, 2012; as amended effective January 29, 2019
#'
#'     Inflation at the rate of 2 percent, as measured by the
#'     "annual change in the price index for personal consumption expenditures"
#'     is most consistent over the longer run with the Federal Reserve’s statutory mandate
#'     https://www.federalreserve.gov/monetarypolicy/files/FOMC_LongerRunGoals.pdf
#'
#'     Federal Open Market Committee reaffirms its "Statement on Longer-Run Goals and Monetary Policy Strategy"
#'     January 30, 2019
#'     https://www.federalreserve.gov/newsevents/pressreleases/monetary20190130b.htm
#'
#'   n_ask <- 2.0
#'
#' phi_u: coefficient on the unemployment rate gap
#'        To reflect the flatness of the Phillips curve
#'        this value is set to 0.1.
#'
#'   phi_u <- 0.1
#'
#' phi_n: coefficient on the inflation gap
#'        This isequal to 1.5 and consistent with the 1993 Taylor rule.
#'
#'   phi_n <- 1.5
#'
#' GOAL
#'
#' i_t : nominal federal funds interest rate
#'
#' METHOD 3
#'
#' i_t_method3 <- rho * i_tm1 + (1 - rho) * (r_ask_t + n_ask + phi_n * n_GAP_t + phi_u * u_GAP_t)
#' colnames(i_t_method3) <- "i_t_method3"
#' dygraphs::dygraph(i_t_method3, main = ""More Modernized method 3 Nominal Federal Funds Interest Rate")
#'
#' }
"_PACKAGE"

#' @rdname moreModernFEDFundsTayorRule
#' @return i_tm1: xts object; more modernized one quarter lag in the federal funds rate
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom quantmod getSymbols
#' @importFrom DescTools AddMonths
i_tm1  <- function() {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  # require(xts)

  # Effective Federal Funds Rate (FEDFUNDS)
  # https://fred.stlouisfed.org/series/FEDFUNDS
  FEDFUNDS <- quantmod::getSymbols("FEDFUNDS", src = "FRED", auto.assign = F)
  index(FEDFUNDS) <- DescTools::AddMonths(index(FEDFUNDS),1)
  FEDFUNDS <- lag.xts(FEDFUNDS,3)
  # Change "beginning of month dates" to "end of month dates".
  index(FEDFUNDS) <- index(FEDFUNDS) - 1

  i_tm1 <- FEDFUNDS; colnames(i_tm1) <- "i_tm1"
  return(i_tm1)

})}
#' @rdname moreModernFEDFundsTayorRule
#' @return u_GAP_t: xts object; more modernized "[unemployment] output gap"
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom quantmod getSymbols
#' @importFrom DescTools AddMonths LastDayOfMonth
u_GAP_t  <- function() {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  # require(xts)

  # Civilian Unemployment Rate (UNRATE)
  # https://fred.stlouisfed.org/series/UNRATE/
  UNRATE <- quantmod::getSymbols("UNRATE", src = "FRED", auto.assign = F)
  index(UNRATE) <- DescTools::AddMonths(index(UNRATE),1)
  # Change "beginning of month dates" to "end of month dates".
  index(UNRATE) <- index(UNRATE) - 1

  unrate <- UNRATE; colnames(unrate) <- "unrate"

  # Use (almost) the same math as "Real Gross Domestic Product (GDPC1)"

  # Natural Rate of Unemployment (Long-Term) (NROU)
  # https://fred.stlouisfed.org/series/NROU
  NROU <- quantmod::getSymbols("NROU", src="FRED", auto.assign = F)
  # "as of record date" to "last updated date"
  # (four months and one week: round up to the end of five(5) months)
  index(NROU) <- DescTools::AddMonths(index(NROU),5)
  # first report
  From <- head(index(NROU),1)
  # last "useful"
  To <- tail(index(NROU),1)
  # create intermediate monthly observations
  NROU <- merge(NROU, xts(, seq(from = From, to = To, by = "months")) )
  # fill in intermediate months with last known data
  NROU <- na.locf(NROU)
  # Change "beginning of month dates" to "end of month dates".
  index(NROU) <- index(NROU) - 1 # subtrace off one(1) day

  nrou <- NROU; colnames(nrou) <- "nrou"

  u_GAP_t <- unrate - nrou
  colnames(u_GAP_t) <- "u_GAP_t"
  return(u_GAP_t)

})}
#' @rdname moreModernFEDFundsTayorRule
#' @return n_GAP_t: xts object; more modernized "inflation gap"
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom quantmod getSymbols
#' @importFrom DescTools AddMonths
n_GAP_t  <- function() {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  # require(xts)
  # market inflation expectations
  # 5-Year Breakeven Inflation Rate (T5YIE)
  # https://fred.stlouisfed.org/series/T5YIE
  T5YIE <- quantmod::getSymbols("T5YIE", src = "FRED", auto.assign = F)
  # acquire the last observation of the month
  # and re-date it to be the last day of the month
  mkt_inf_exp <- (To.Monthly(T5YIE, OHLC = FALSE, indexAt = "lastof") - 0.30)
  mkt_inf_exp <- mkt_inf_exp[index(mkt_inf_exp) <= Sys.Date()]

  n_ask <- 2.00

  n_GAP_t <- mkt_inf_exp - n_ask
  colnames(n_GAP_t) <- "n_GAP_t"
  return(n_GAP_t)

})}
#' @rdname moreModernFEDFundsTayorRule
#' @return r_ask_t: xts object; more modernized "equilibrium real interest rate"
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom quantmod getSymbols
#' @importFrom DescTools AddMonths
r_ask_t  <- function() {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  # require(xts)

  # 1-Year Treasury Constant Maturity Rate (GS1)
  # https://fred.stlouisfed.org/series/GS1
  GS1 <- quantmod::getSymbols("GS1", src = "FRED", auto.assign = F)
  index(GS1) <- DescTools::AddMonths(index(GS1), 1)
  # Change "beginning of month dates" to "end of month dates".
  index(GS1) <-  index(GS1) - 1

  # one year nominal constant maturity yield
  year_1_nom_cm_yield <- GS1
  colnames(year_1_nom_cm_yield) <- "year_1_nom_cm_yield"

  # Less . . .

  # Trimmed Mean PCE Inflation Rate (PCETRIM1M158SFRBDAL)
  # https://fred.stlouisfed.org/series/PCETRIM1M158SFRBDAL
  PCETRIM1M158SFRBDAL <- quantmod::getSymbols("PCETRIM1M158SFRBDAL", src="FRED", auto.assign = F)
  index(PCETRIM1M158SFRBDAL) <- DescTools::AddMonths(index(PCETRIM1M158SFRBDAL),3)
  index(PCETRIM1M158SFRBDAL) <- index(PCETRIM1M158SFRBDAL) - 1 # now at the end of the current month

  # four quarter change of "trimmed mean PCE inflation rate"
  change_of_tm_PCE_infl_rate <- PCETRIM1M158SFRBDAL - lag.xts(PCETRIM1M158SFRBDAL,12)
  # four quarter [percent] change of "trimmed mean PCE inflation rate"
  # change_of_tm_PCE_infl_rate <- (PCETRIM1M158SFRBDAL - lag.xts(PCETRIM1M158SFRBDAL,12))/abs(lag.xts(PCETRIM1M158SFRBDAL,12))
  colnames(change_of_tm_PCE_infl_rate) <- "change_of_tm_PCE_infl_rate"

  r_ask_t <- year_1_nom_cm_yield - change_of_tm_PCE_infl_rate
  colnames(r_ask_t) <- "r_ask_t"

  return(r_ask_t)

})}
#' @rdname moreModernFEDFundsTayorRule
#' @return t_i_method3: xts object; version 3 of more modernized federal funds nominal interest rate
#' @examples
#' \dontrun{
#' dygraphs::dygraph(i_t_method3(), main = "More Modernized method 3 Nominal Federal Funds Interest Rate")
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
i_t_method3  <- function() {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  rho <- 0.85

  n_ask <- 2.0

  phi_u <- 0.1
  phi_n <- 1.5

  # METHOD 3

  i_t_method3 <- rho * i_tm1() + (1 - rho) * (r_ask_t() + n_ask + phi_n * n_GAP_t() + phi_u * u_GAP_t())
  colnames(i_t_method3) <- "i_t_method3"
  # dygraphs::dygraph(i_t_method3, main = "More Modernized method 3 Nominal Federal Funds Interest Rate")
  return(i_t_method3)

})}
#' @rdname moreModernFEDFundsTayorRule
#' @return  MMFFTR: xts object; version 3 of more modernized federal funds nominal interest rate
#' @examples
#' \dontrun{
#' # more(M) modern(M) Federal(F) Funds(F) Taylor(T) Rule(R): MMFTTR
#' dygraphs::dygraph(MMFFTR(), main = "More Modernized method 3 Nominal Federal Funds Interest Rate")
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
MMFFTR  <- function() {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  res <- i_t_method3()
  colnames(res) <- "MMFFTR"
  return(res)

})}
