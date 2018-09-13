context("all")


test_that("safe object intitialization", {

  require(xts)

  expect_identical( initDate(date = NULL), structure(numeric(0), class = "Date"))



  expect_identical( initXts(xTs = NULL), structure(numeric(0),
                                                   index = structure(
                                                       numeric(0)
                                                     , tzone = "UTC"
                                                     , tclass = "Date")
                                                   , class = c("xts", "zoo"), .indexCLASS = "Date"
                                                   , tclass = "Date", .indexTZ = "UTC"
                                                   , tzone = "UTC")

  )

  expect_identical( initXts(xTs = zoo::as.Date(0)[0])
               , structure(numeric(0), index = structure(numeric(0)
               , tzone = "UTC", tclass = "Date")
               , class = c("xts", "zoo")
               , .indexCLASS = "Date", tclass = "Date"
               , .indexTZ = "UTC", tzone = "UTC")
               )

  expect_identical( Coredata(NULL),       NULL)
  expect_identical( Coredata(numeric(0)), NULL)
  expect_identical( Coredata(11:13),     structure(11:13, .Dim = c(3L, 1L)))

})


test_that("setup and unsetup env", {

  options(max.print=88888L)
  if(Sys.getenv("TZ") != "UTC") {tz <-Sys.getenv("TZ")} else {tz <- "US/Central"}
  Sys.setenv(TZ=tz)
  initEnv()
  expect_identical(getOption("max.print"), 99999L)
  expect_identical(Sys.getenv("TZ"), "UTC")

  uninitEnv()
  expect_identical(getOption("max.print"), 88888L)
  expect_identical(Sys.getenv("TZ"), tz)

  if(getOption("digits") != 22L) {ops <- options()} else {ops <- options(digits=12L)}
  options(ops)
  initPrintEnv()
  expect_identical(getOption("digits"), 5L)
  uninitEnv()
  expect_identical(options(), ops)

})


test_that("aquire and massage [FRED] data", {

  fd <- head(fredData("GDP"),1)
  xtsAttributes(fd) <- list(updated = NULL)

  expect_identical(fd, structure(243.164, class = c("xts", "zoo")
                        , .indexCLASS = "Date", tclass = "Date"
                        , .indexTZ = "UTC", tzone = "UTC"
                        , src = "FRED", index = structure(-725846400, tzone = "UTC", tclass = "Date")
                        , .Dim = c(1L, 1L), .Dimnames = list(NULL, "gdp")
                        )

  )
  
  require(xts)
  xTs <- xts(c(1,NA_real_,2), zoo::as.Date(c(1,11,21)))
  
  expect_identical( eomData(xTs = xTs), structure(2, class = c("xts", "zoo")
                                          , .indexCLASS = "Date", tclass = "Date"
                                          , .indexTZ = c(TZ = "UTC"), tzone = c(TZ = "UTC")
                                          , index = structure(2592000, tclass = "Date", tzone = "UTC")
                                          , .Dim = c(1L, 1L)
                                        )
  
  )
  
  # weekly
  ff <- head(fredEomData("FF"),1)
  xtsAttributes(ff) <- list(updated = NULL)
  
  expect_identical(ff, structure(0.63, class = c("xts", "zoo")
                         , .indexCLASS = "Date", tclass = "Date"
                         , .indexTZ = c(TZ = "UTC"), tzone = c(TZ = "UTC")
                         , src = "FRED", index = structure(-486691200, tclass = "Date", tzone = "UTC")
                         , .Dim = c(1L, 1L), .Dimnames = list(NULL, "ff"))
  
  
  )

})


test_that("date lubrication", {

  expect_identical("nextMonthfromYesterday.default", "nextMonthfromYesterday.default")
  
  require(xts)
  
  expect_identical(nextMonthfromYesterday(zoo::as.Date("1970-01-12")), structure(30, class = "Date"))
  
  xTs <- xts(, zoo::as.Date("1970-01-12"))

  expect_identical(nextMonthfromYesterday(xTs), structure(numeric(0), index = structure(2592000
                                                  , tzone = "UTC", tclass = "Date")
                                                  , class = c("xts", "zoo")
                                                  , .indexCLASS = "Date", tclass = "Date"
                                                  , .indexTZ = "UTC", tzone = "UTC"
                                                )

  )


})


test_that("instrument data", {

  expect_identical("wilshire5000indEomData", "wilshire5000indEomData")
  expect_identical("unRateEomData",                   "unRateEomData")

})


test_that("xTs manipulation", {

  # logReturns
  xTs  <- xts(10:12,zoo::as.Date(0:2))
  lr <- logReturns(xTs)          
  expect_equal(as.vector(coredata(xTs)) ,as.vector(coredata(exp(cumsum(lr)) * 10L)))

  expect_identical("wilshire5000LogReturns", "wilshire5000LogReturns")
  expect_identical("cashLogReturns",         "cashLogReturns")
  expect_identical("unRateEomData",          "unRateEomData")

})


test_that("predicton", {

  expect_identical("willShire5000Wts", "willShire5000Wts")
  expect_identical("cashWts", "cashWts")

})


test_that("add indicators and predicton", {

  expect_identical("addUnRateEomData",       "addUnRateEomData")

  expect_identical("addWts", "addWts")
  expect_identical("addWillShire5000Wts", "addWillShire5000Wts")
  expect_identical("addCashWts", "addCashWts")

})


test_that("combinations", {

  require(xts)

  expect_identical(combineXts( initXts(NULL),xts(,zoo::as.Date(0)) )
                 , structure(numeric(0), index = structure(0, tzone = "UTC", tclass = "Date")
                 , class = c("xts", "zoo")
                 , .indexCLASS = "Date", tclass = "Date"
                 , .indexTZ = "UTC", tzone = "UTC")
                 )

  expect_identical(combineXts( xts(,zoo::as.Date(1)), xts(,zoo::as.Date(0)) )
                 , structure(numeric(0), index = structure(c(0, 86400), tzone = "UTC", tclass = "Date")
                 , class = c("xts", "zoo")
                 , .indexCLASS = "Date", tclass = "Date"
                 , .indexTZ = "UTC", tzone = "UTC")
                 )
  
  expect_identical("combineLogReturns", "combineLogReturns")

  expect_identical("addWilshire5000LogReturns", "addWilshire5000LogReturns")
  expect_identical("addCashLogReturns",         "addCashLogReturns")

})


test_that("column roles", {

  require(xts)

  xTs <- xts(matrix(1:3, ncol = 3, dimnames = list(NULL,c("a","b","b_wts"))),zoo::as.Date(0))[0]

  expect_identical("safeClms",     "safeClms")
  expect_identical(indClms(xTs),   "a")
  expect_identical(valueClms(xTs), "b")
  expect_identical(wtsClms(xTs),   "b_wts")

})


test_that("porfolio", {

  expect_identical("portfolioLogReturns",      "portfolioLogReturns")
  expect_identical("portfolioMonthlyReturns",  "portfolioMonthlyReturns")
  expect_identical("returnsUnRateEyeBall",     "returnsUnRateEyeBall")

})


test_that("printing", {

  expect_identical("printTail",  "printTail")
  expect_identical("printCalendar", "printCalendar")

})



