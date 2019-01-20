

#' setdiff any dimension-less  object
#'
#' Unlike base::setdiff does not do "as.vector",
#' therefore vector classes are not lost
#' ( e.g. Date and POSIXt).
#'
#' To compare, it uses (same as setdiff.default) match
#'
#' @param x vector
#' @param y vector of elements to subtract off from x
#' @return vector from x, elements of y have been subtracted
#' from x
#' @export
setDiff <- function (x, y) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  if(!is.null(dim(x)) || !is.null(dim(y)))
    stop("setDiff needs dimension-less x and y")

  unique(if (length(x) || length(y))
      x[match(x, y, 0L) == 0L]
  else x)
})}



#' safely make a xts into a df and safely make df into an xts
#'
#' library(xts)
#' sample_xts <- as.xts(sample_matrix)
#' str(head(xtsize(dfize(sample_xts))))
#'
#' edge case tests
#'
#' xts1 <- xts(NA_real_, zoo::as.Date(0))
#' colnames(xts1) <- "col1"
#' xts1 <- xts1[0,]
#'
#' exiting from: dfize(xts1)
#' 'data.frame':   0 obs. of  2 variables:
#'  $ datetime:Class 'Date'  num(0)
#'  $ col1    : num
#'
#' > xtsize(dfize(xts1))
#'      col1
#' > str(xtsize(dfize(xts1)))
#' An 'xts' object of zero-width
#' > index(xtsize(dfize(xts1)))
#' [1] "Date of length 0"
#' @rdname converting
#' @export
#' @importFrom zoo as.Date
dfize <- function(xtso) {
  require(xts)
  df <- as.data.frame(xtso)  # zoo::as.data.frame.zoo # xts:::as.data.frame.xts
  # rn <- as.numeric( zoo::as.Date( row.names(df) ) )
  rn <- index(xtso)
  cb <- cbind(rn,df)
  colnames(cb)[1] <- "datetime"
  # attributes(cb) <- list(tindex = index)
  return(cb)
}


#' safely make df into an xts
#'
#' @rdname converting
#' @export
xtsize <- function(dfo) {
  require(xts)
  id <-  dfo[["datetime"]]
  # drop column
  dfo[,"datetime"] <- NULL
  cd <- coredata(as.matrix(dfo))
  xts(cd,id)  # could be 'zoo'
}


#' inside and exts environment
#'
#' unstable structure to debug inside: holds for 2 seconds then bounces out
#' within.xts IS TOO VOLITILE: CAN NOT browser/rstudio debug inside: SOMETHING IS NOT RIGHT
#'
#' @export
within.xts <- function (data, expr, ...) {
  data <- dfize(data)
  # tindex <- attrib(data, "tindex")
  parent <- environment()    # JUST CHANGED parent.frame() to environment()
  e <- evalq(environment(), data, parent)
  eval(substitute(expr), e)
  l <- as.list(e, all.names = TRUE)
  l <- l[!vapply(l, is.null, NA, USE.NAMES = FALSE)]
  nD <- length(del <- setdiff(names(data), (nl <- names(l))))
  data[nl] <- l
  if (nD)
    data[del] <- if (nD == 1)
      NULL
  else
    vector("list", nD)

  xtsize(data)
}



#' comparison of two objects
#'
#' @param test boolean test
#' @param yes result of pass
#' @param no result of no-pass
#' @return xts object with the same index as xTs1
#' @rdname ifelse
#' @export
ifelse         <- function(test, yes, no) UseMethod("ifelse")
#' @rdname ifelse
#' @export
ifelse.default <- function(test, yes, no) base::ifelse(test, yes, no )
#' @rdname ifelse
#' @export
ifelse.xts    <- function(test, yes, no) {

  require(xts)

  if(!is.xts(yes)) {
    if(NROW(yes) == 1) yes <- rep(yes,NROW(test))
    yes <- xts(yes, index(test))
  }
  if(!is.xts(no)) {
    if(NROW(no) == 1) no <- rep(no,NROW(test))
    no <- xts(no, index(test))
  }
  test.yes.no <- merge(test,yes,no)
  colnames(test.yes.no) <- c("test", "yes", "no" )
  res <- within( test.yes.no, { res <- ifelse(test,yes,no);rm(test,yes,no);return(res) } )
  colnames(res) <-"result"
  return(res)

}




#' difference of two xTs objects
#'
#' @param xTs1 leading xts object
#' @param xTs2 trailing xts object
#' @return xts object with the same index as xTs1
#' @export
#' @importFrom tryCatchLog tryCatchLog
Less <- function(xTs1 = NULL, xTs2 = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  xTs1  <- initXts(xTs1)
  xTs2  <- initXts(xTs2)

  xTs1 - xTs2

})}

#' NEED YET: e.g. initLag: index(X1) - #, Less: X2 - X1, LogDiff: log(X2/X1), FracChg: X2/X1 - 1, ArithDiff: X2/X1



#' quantmod getModelData with parameter source.envir
#'
#' get some Symbols from an environment (source.envir)
#'   will search first in (source.envir)
#'   if the Symbol is not found in the enviroment (source.envir),
#'   then get the Symbol from elsewhere
#' ...
#' passed to getSymbols
#'         useful: from, to
#'   maybe useful: src
#'   maybe useful: set per Symbol src using setSymbolLookup
#'
#' specifyModel(getModelData)
#' if na.rm == TRUE, then does 'na.exclude',
#'   BUT 'without rules' PUTS back (rbind) the last observation
#'
#' @export
#' @importFrom tryCatchLog tryCatchLog
getModelData <- function (x, na.rm = TRUE, source.envir = NULL, ...) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

    model <- x
    if (!is.quantmod(model))
        stop(sQuote("x"), "must be of class", dQuote("quantmod"),
            "\n")
    if (length(model@model.inputs) == 0) {
        build.vars <- c(model@model.target, model@build.inputs)
    }
    else {
        build.vars <- c(model@model.target, model@model.inputs)
    }
    model.symbols <- vars <- all.vars(model@model.spec)
    env <- new.env()
    lapply(vars, function(V) {
        if(is.null(source.envir)) {
            if(!exists(V)) {
                getSymbols(V, env = env, ...)
            }
            else {
                assign(V, get(V), env)
            }
        } else {
            if (!exists(V, envir = source.envir)) {
                getSymbols(V, env = env, ...)
            } else {
                assign(V, get(V, envir = source.envir), env)
            }
        }
    })
    target.data <- get(model.symbols[[1]], env)
    total.columns = NULL
    for (j in 1:length(model.symbols)) {
        if (j == 1) {
            m <- as.xts(target.data)
        }
        else {
            m <- merge(m, as.xts(get(model.symbols[[j]], env)),
                join = "inner")
        }
        total.columns[j] <- ncol(m)
    }
    fullIndex <- index(m)
    from.col = 1
    for (i in 1:length(model.symbols)) {
        assign(model.symbols[[i]], m[, from.col:(total.columns[i])],
            env)
        from.col = total.columns[i] + 1
    }
    mf <- xts(model.frame(model@model.spec, data = env, na.action = NULL),
        fullIndex)
    if (na.rm)
        mf <- rbind(na.exclude(mf[-nrow(mf), ]), mf[nrow(mf),
            ])
    colnames(mf) <- lapply(colnames(mf), function(x) {
        gsub("[) ]", "", gsub("[(,=^:'\"]", ".", x))
    })
    model@model.data <- mf
    model@build.inputs <- colnames(mf)[-1]
    model@model.formula = as.formula(paste(colnames(mf)[1], "~",
        paste(colnames(mf)[-1], collapse = "+"), sep = ""))
    return(model)
})}


#' try to download a file
#'
#' see pacakge quantmod quantmod:::convert.time.series
#'
#' @param url as quantmod:::try.download.file
#' @param destfile as quantmod:::try.download.file
#' @param method as quantmod:::try.download.file
#' @param quiet as quantmod:::try.download.file
#' @param mode as quantmod:::try.download.file
#' @param extra as quantmod:::try.download.file
#' @param ... as quantmod:::try.download.file
#' @export
#' @importFrom tryCatchLog tryCatchLog
quantmod___try.download.file <- function (url, destfile, method, quiet = FALSE, mode = "w", cacheOK = TRUE,
    extra = getOption("download.file.extra"), ...) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})
    if (missing(method))
        method <- getOption("download.file.method", default = "auto")
    try.download <- try({
        download.file(url, destfile, method, quiet, mode, cacheOK,
            extra)
    }, silent = TRUE)
    if (inherits(try.download, "try-error")) {
        if (requireNamespace("downloader", quietly = TRUE)) {
            downloader::download(url, destfile = destfile, quiet = quiet,
                mode = mode, cacheOK = cacheOK, extra = extra)
        }
        else {
            errcond <- attr(try.download, "condition")
            stop("Failed to download file. Error message:\n",
                errcond$message, "\n", "If this is related to https, possible solutions are:\n",
                "1. Explicitly pass method= via the getSymbols call (or via setDefaults)\n",
                "2. Install downloader, which may be able to automagically determine a method\n",
                "3. Set the download.file.method global option",
                call. = FALSE)
        }
    }
})}



#' convert time series
#'
#' see pacakge quantmod quantmod:::convert.time.series
#'
#' @param fr as quantmod:::convert.time.series
#' @param return.class as quantmod:::convert.time.series
#'
#' @export
#' @importFrom tryCatchLog tryCatchLog
quantmod___convert.time.series <- function(fr, return.class) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

    if ("quantmod.OHLC" %in% return.class) {
        class(fr) <- c("quantmod.OHLC", "zoo")
        return(fr)
    }
    else if ("xts" %in% return.class) {
        return(fr)
    }
    if ("zoo" %in% return.class) {
        return(as.zoo(fr))
    }
    else if ("ts" %in% return.class) {
        fr <- as.ts(fr)
        return(fr)
    }
    else if ("data.frame" %in% return.class) {
        fr <- as.data.frame(fr)
        return(fr)
    }
    else if ("matrix" %in% return.class) {
        fr <- as.data.frame(fr)
        return(fr)
    }
    else if ("timeSeries" %in% return.class) {
        if (requireNamespace("timeSeries", quietly = TRUE)) {
            fr <- timeSeries::timeSeries(coredata(fr), charvec = as.character(index(fr)))
            return(fr)
        }
        else {
            warning(paste("'timeSeries' from package 'timeSeries' could not be loaded:",
                " 'xts' class returned"))
        }
    }
})}



#' Create one data object from multiple sources,
#' applying transformations via standard R formula mechanism.
#'
#' UNUSED
#' see quantmod buildData
#' and this function can also acquire symbols that are stored in an environment
#'
#' @param formula as quantmod buildData
#' @param na.rm as quantmod buildData
#' @param return.class  as quantmod buildData
#' @param source.envir find xts Symbols in this environment
#' @return "return.class" as quantmod buildData
#' @export
#' @importFrom tryCatchLog tryCatchLog
buildData <- function(formula, na.rm = TRUE, return.class = "zoo", source.envir = NULL, ...) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

    if (is.quantmod(formula)) {
        fr <- modelData(formula)
    }
    else {
        fr <- modelData(specifyModel(formula, na.rm = na.rm, source.envir = NULL, ...))
    }
    fr <- quantmod___convert.time.series(fr = fr, return.class = return.class)
})}



#' quantmod specifyModel with parameter source.envir
#'
#' get some Symbols from an environment (source.envir)
#'   will search first in (source.envir)
#'   if the Symbol is not found in the enviroment (source.envir),
#'   then get the Symbol from elsewhere
#' ...
#' passed to getSymbols
#'         useful: from, to
#'   maybe useful: src
#'   maybe useful: set per Symbol src using setSymbolLookup
#'
#' specifyModel(getModelData) original code ( not changed by me )
#' if na.rm == TRUE,  then does 'na.exclude'
#'   BUT 'without rules' PUTS back (rbind) the last observation
#'
#' NOTE
#' TTR acceptable
#' avoid
#' specifyModel
#' Error in runSum(x, n) : Series contains non-leading NAs
#' zoo::na.trim(tail(x), sides = "right")
#'
#' example ( better: try a DescTools::DoCall )
#' Symbols <- unlist(plyr::llply( c("MSFT","AAPL","WMT","COST"), function(x) {
#'                           l <- list(); l[[x]] <- getSymbols(x, auto.assign = FALSE); l
#'                           }), recursive = FALSE)
#' Symbols <- list2env(Symbols)
#' ls.str(Symbols)
#'
#' getSymbols( c("AAPL","ORCL"), source.envir = Symbols)
#' rm("AAPL","ORCL")
#'
#' quantmod <- specifyModel(Next(ClCl(WMT)) ~ Lag(OpCl(AAPL)) + Lag(LoHi(COST),0:2)
#'   , source.envir = Symbols
#'   , from ="2007-01-01"
#'   , to   ="2011-12-31"
#'   )
#'
#' @export
#' @importFrom tryCatchLog tryCatchLog
specifyModel <- function (formula, na.rm = TRUE, source.envir = NULL, ...) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

    new.quantmod <- new("quantmod")
    formula <- as.formula(formula)
    dot.vars <- all.vars(formula)
    convert.vars <- function(vars) {
        v <- unlist(strsplit(vars, "[.]"))
        v <- paste(v[1], "(", v[2], if (length(v) > 2)
            paste(",", v[3], sep = ""), ")", sep = "")
        return(v)
    }
    new.quantmod@model.spec <- formula
    new.quantmod@model.formula <- as.formula(gsub("[) ]", "",
        gsub("[(,=:^'\"]", ".", deparse(formula))))
    new.quantmod@model.target <- as.character(new.quantmod@model.formula[[2]])
    new.quantmod@build.inputs <- as.character(attr(terms(new.quantmod@model.formula),
        "term.labels"))
    vars <- all.vars(formula)
    new.quantmod@symbols <- vars
    new.quantmod@product <- vars[1]
    new.quantmod <- getModelData(new.quantmod, na.rm = na.rm, source.envir = source.envir, ...)
    return(new.quantmod)
})}



#' column split a data.frame object into
#' an environment of column-xts objects
#'
#' @param x data.frame object
#' @param order.by xts compabile index
#' @param xts.attributes list of xts attributes to apply
#' @export
#' @importFrom plyr llply
#' @importFrom rlist list.zip
#' @importFrom plyr llply
#' @importFrom tryCatchLog tryCatchLog
DFCols2SymbolsEnv <- function(x, order.by, xts.attributes = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  # IF REMOVE ".fun = " then RStudio can debug
  # create an environment of xts objects
  plyr::llply(rlist::list.zip(df = x, ColName = colnames(x)),
    function(x) {
      xx <- as.xts(x[["df"]], order.by = order.by)
      colnames(xx)[1] <- x[["ColName"]]
      xtsAttributes(xx) <- xts.attributes
      xx
    }
  ) -> SymbolsOrig

  # reorders in alphabetical order
  Symbols <- list2env(SymbolsOrig)
  Symbols

})}



#' converts to a quantmod object
#'
#' @rdname as.quantmod
#' @export
as.quantmod.default <- function(x, outcomename, order.by, na.rm = TRUE, ...) { invisible() }
#' @rdname as.quantmod
#' @export
as.quantmod         <- function(x, outcomename, order.by, na.rm = TRUE, ...) { UseMethod("as.quantmod") }

# from a data.frame, covert to a quantmod object directly
#
# CURRENLY NOT USED
#
# if I have to do preprocessing ( e.g. a treatment )
#   therefore the situation may be cheaper to make a quantmod object
#     from a data.frame
#
# if na.rm == TRUE, then does 'na.exclude'
#   BUT 'without rules' PUTS back (rbind) the last observation
#
#' data(sample_matrix)
#' sample_xts <- as.xts(sample_matrix)
#' quantmodSample <- as.quantmod(as.data.frame(sample_xts), outcomename = "Close", order.by = index(sample_xts))
#'
#' @rdname as.quantmod
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom DataCombine MoveFront
#' @importFrom plyr llply
#' @importFrom stringr str_c
as.quantmod.data.frame  <- function(x, outcomename, order.by, na.rm = TRUE, ...) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  # TODO[] # instead of passing outcomename, pass instead, a formula
  x <- DataCombine::MoveFront(x, outcomename )

  # place single column where specifyModel ( getModelData ( exists ) ) can find

  # Little Inspired by
  # R: how do you merge/combine two environments?
  # https://stackoverflow.com/questions/26057400/r-how-do-you-merge-combine-two-environments
  #
  # NOTE: to merge environments: loop over "assign(get)"
  #
  # NOTE ALSO EXISTS: as.list.xts() ... , as.list.environment
  #                   USAGE c(as.list.xts(),as.list.xts())
  #

  Symbols <- DFCols2SymbolsEnv(x, order.by = order.by)

  # assign where specifyModel ( getModelData ( exists ) ) can find

  # if na.rm == TRUE, then does 'na.exclude',
  #   BUT 'without rules' PUTS back (rbind) the last observation

  # passed to getSymbols
  #
  #         useful: from, to
  #   maybe useful: src
  #   maybe useful: set per Symbol src using setSymbolLookup

  model <- specifyModel(stringr::str_c(outcomename, " ~ ", paste0(setdiff(colnames(x), outcomename), collapse = " + "))
             , na.rm = na.rm
             , source.envir = Symbols
             , ...)

  return(model)

})}


#' detect the number of computer cores
#'
#' Re: [Rd] Get Logical processor count correctly whether NUMA is enabled or disabled
#' Tomas Kalibera <tomas.kalibera@gmail.com>
#' Mon 9/3/2018, 8:07 AM
#' A summary for reference: the new detectCores() for Windows in R-devel
#' Get Logical processor count correctly whether NUMA is enabled or disabled
#' http://r.789695.n4.nabble.com/Get-Logical-processor-count-correctly-whether-NUMA-is-enabled-or-disabled-td4751774.html
#'
#' @return integer value of numer of cores
#' @examples
#' \dontrun{
#' detectTrueCores()
#' [1] 4
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
detectTrueCores <- function() {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  if (.Platform$OS.type == "windows") {
    out   <- system("wmic cpu get numberofcores", intern=TRUE)
    Cores <- as.integer(sum(as.numeric(gsub("([0-9]+).*", "\\1", grep("[0-9]+[ \t]*", out, value=TRUE)))))
  } else {
    Cores <- detectCores(logical = FALSE)
  }
  Cores

})}



#' S3-like method buildModel.train
#'
#' # tuneGrid ( production tester )
#' tg <- expand.grid(
#'   nrounds   =  10, # TEN TREES
#'   eta       =  c(0.1,0.01),
#'   max_depth =  c(4,6,8,10),
#'   gamma     =  0,
#'   colsample_bytree = c(1,0.5),
#'   min_child_weight = 1,
#'   subsample        = c(1,0.5)
#' )
#' # tuneGrid ( non-production tester )
#' tg <- expand.grid(
#'   nrounds   =  50, # TEST 10 trees - DEV 50 trees - OTHER 500 trees
#'   eta       =  c(0.1,0.01),
#'   max_depth =  c(4,7,10),
#'   gamma     =  0,
#'   colsample_bytree = 1,
#'   min_child_weight = 1,
#'   subsample        = 1
#' )
#'
#' 
#' cl <- parallel::makeCluster(parallel::detectCores(logical = FALSE)) 
#' doParallel::registerDoParallel(cl)
#' assign("cl", cl, envir = asNamespace("base")); rm(cl)
#' 
#' # default
#' # note: caret::trainControl(. . . , allowParallel = TRUE)
#' UnRateMachinetradeModel()
#' 
#' parallel::stopCluster(cl)
#' doParallel::stopImplicitCluster()
#' 
#' tc <- caret::trainControl(method = "cv", number = 5)
#'
#' TODO [ ] : fully WORKED EXAMPLE: combine WITH below
#' builtModel <- buildModel(specmodel, method="train", training.per=c("1970-12-31","2006-12-31")
#'   , method_train = "xgbTree", tuneGrid = tg, trControl = tc)
#'
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom caret trainControl train
#' @importFrom parallel makeCluster stopCluster
#' @importFrom doParallel registerDoParallel
#  # DescTools DoCall
buildModel.train <- function(quantmod,training.data, ...) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  if(is.method.available("train","caret")) {

    Dots <- list()
    Dots <- c(Dots, list(...))
    DotsOrigNames <- names(list(...))

    if(!"method_train" %in% DotsOrigNames) {
      Dots[["method"]] <- "xgbTree"
    } else {
      Dots[["method_train"]] <- NULL
      Dots[["method"]] <- list(...)[["method_train"]]
    }

    # https://xgboost.readthedocs.io/en/latest/parameter.html
    if((Dots[["method"]] == "xgbTree") & (!"tuneGrid" %in% DotsOrigNames)) {
      if(Dots[["stage"]] == "Production") {
        tuneGrid <- expand.grid(
          nrounds   =  c(500,200,100),
          eta       =  c(0.3,0.1,0.01,0.001), # default 0.3
          max_depth =  c(4,6,8,10),           # default 6
          gamma     =  0,                     # default 0
          colsample_bytree = c(1,0.5),        # default 1
          min_child_weight = 1,               # default 1
          subsample        = c(1,0.5)         # default 1
        )
      } else if(Dots[["stage"]] == "Test") {
        tuneGrid <- expand.grid(
          nrounds   =  c(100),
          eta       =  c(0.3, 0.1),
          max_depth =  c(4,8),
          gamma     =  0,
          colsample_bytree = c(1),
          min_child_weight = 1,
          subsample        = c(1)
        )
      } else {
        tuneGrid <- expand.grid(
          nrounds   =  100,
          eta       =  c(0.1,0.01),
          max_depth =  c(4,6,8,10),
          gamma     =  0,
          colsample_bytree = c(1,0.5),
          min_child_weight = 1,
          subsample        = c(1,0.5)
        )
      }
      Dots[["tuneGrid"]] <- tuneGrid
    }
    Dots[["stage"]] <- NULL

    if(!"trControl" %in% DotsOrigNames) {
      trControl <- caret::trainControl(method = "cv", number = 5)
      Dots[["trControl"]] <- trControl
    }

    # suppressWarnings
    # Error in nominalTrainWorkflow(x = x, y = y, wts = weights, info = trainInfo,  :
    # (converted from warning) There were missing values in resampled performance measures.
    # SOLVED:
    #   caret postResample.R#132
    #   if(length(unique(pred)) < 2 || length(unique(obs)) < 2)
    # for a sample (size 86) # xgboost ONLY made one(1) distinct numeric prediction
    # therefore, Rsquared could not be calculated therefore it is set to NA
    # This happens in ~ 30% of all samples sent
    if(!all(complete.cases(training.data))) print("NOTE: in buildModel.train, training.data is missing some data.")
    # sampling(index/indexOut) done in parallel ( default: allowParallel = T )
    #   for 'fast' modeling, the overhead is 'too slow'
    #   nominalTrainWorkflow
    #     info$loop

    ### ### commented out: if detected in a 'visible' environment e.g. base: see the instructions
    ### cl <- parallel::makeCluster(detectTrueCores())
    ### doParallel::registerDoParallel(cl)
    ### set.seed(2L)

    # DescTools::DoCall
    #   Error in model.frame.default(form = <formula> +  invalid type (closure) for variable '(weights)'
    # I do not know why this Error occurs: sometime, I will try to figure this out
    # (so for now, just revert back to "do.call")
    rp <- suppressWarnings(do.call(caret::train, base::append(c(list(), list(quantmod@model.formula),data=list(training.data)), Dots[!names(Dots) %in% "stage"]) ) )
    ### parallel::stopCluster(cl)

    return(list("fitted"=rp, "inputs"=attr(terms(rp),"term.labels")))
  }
})}

#' determine the future
#'
#' @rdname predictModel
#' @export
predictModel <- function(object, data, ...) {
    UseMethod("predictModel")
}

#' @rdname predictModel
#' @export
predictModel.default <- function (object, data, ...) {
    predict(object, data, ...)
}


#' check the .libPaths() to see a package has a training function
#'
#' extracted because quantmod does not export this
#'
#' @rdname is.method.available
#' @export
#' @importFrom tryCatchLog tryCatchLog
quantmod___is.method.available <- function (method, package) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

    if (!package %in% .packages()) {
        if (package %in% .packages(all.available = TRUE)) {
            cat(paste("loading required package:", package, "\n"))
            library(package, character.only = TRUE)
        }
        else {
            stop(paste("package", sQuote(package), "containing",
                sQuote(method), "unable to be located"))
        }
    }
    return(TRUE)
})}
#' @rdname is.method.available
#' @export
is.method.available <- function(method, package) quantmod___is.method.available(method = method, package = package)



# example (TODO [ ]) simplify
#

# from quantmod predictModel
#
# does caret train
#
#
predictModel.train <- function (object, data, ...) {
    if (quantmod___is.method.available('train','caret')) {
        predict(object, data, ...)
    }
}

