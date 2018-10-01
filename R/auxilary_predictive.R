


# quantmod getSymbols with source.envir

# get some Symbols from an environment (source.envir)
#   will search first in (source.envir)
#   if the Symbol is not found in the enviroment (source.envir),
#   then get the Symbol from elsewhere
# NOTE: do not do: "source.envir = e, "env = e"
#       when auto.assign = TRUE(default), .GetSymbols is placed where "env = e"
#
# e <- new.env()
# NOT the same as "env = e"
# assign("AAPL", getSymbols(list(AAPL = "yahoo"), auto.assign = F), envir = e)
# ls.str(e)
# AAPL : An 'xts' object on 2007-01-03/2018-04-24 containing:
#   Data: num [1:2847, 1:6] 12.3 12 12.3 12.3 12.3 ...
#  - attr(*, "dimnames")=List of 2
#   ..$ : NULL
#   ..$ : chr [1:6] "AAPL.Open" "AAPL.High" "AAPL.Low" "AAPL.Close" ...
#   Indexed by objects of class: [Date] TZ: UTC
#   xts Attributes:
# List of 2
#  $ src    : chr "yahoo"
#  $ updated: POSIXct[1:1], format: "2018-04-25 18:12:26"
#
# getSymbols(list(AAPL = "yahoo", MSFT = "yahoo"), source.envir = e)
# AAPLSymbol <- getSymbols(list(AAPL = "yahoo"), auto.assign = FALSE, source.envir = e)
#
getSymbols <- function (Symbols = NULL, env = parent.frame(), reload.Symbols = FALSE,
    verbose = FALSE, warnings = TRUE, src = "yahoo", symbol.lookup = TRUE,
    auto.assign = getOption("getSymbols.auto.assign", TRUE), source.envir = NULL, # BEGIN/END NEW CODE
    ...)                                                                        {
    if (getOption("getSymbols.warning4.0", TRUE)) {
        message(sQuote("getSymbols"), " currently uses auto.assign=TRUE by default, but will\n",
            "use auto.assign=FALSE in 0.5-0. You will still be able to use\n",
            sQuote("loadSymbols"), " to automatically load data. getOption(\"getSymbols.env\")\n",
            "and getOption(\"getSymbols.auto.assign\") will still be checked for\n",
            "alternate defaults.\n\n", "This message is shown once per session and may be disabled by setting \n",
            "options(\"getSymbols.warning4.0\"=FALSE). See ?getSymbols for details.\n")
        options(getSymbols.warning4.0 = FALSE)
    }
    importDefaults("getSymbols")
    if (missing(env) && !is.null(getOption("getSymbols.env")))
        env <- getOption("getSymbols.env")
    if (is.null(env))
        auto.assign <- FALSE
    if (!auto.assign && length(Symbols) > 1)
        stop("must use auto.assign=TRUE for multiple Symbols requests")
    force(Symbols)
    if (symbol.lookup && missing(src)) {
        symbols.src <- getOption("getSymbols.sources")
    }
    else {
        symbols.src <- src[1]
    }
    if (is.character(Symbols)) {
        Symbols <- unlist(strsplit(Symbols, ";"))
        tmp.Symbols <- vector("list")
        for (each.symbol in Symbols) {
            if (each.symbol %in% names(symbols.src)) {
                tmp.src <- symbols.src[[each.symbol]]$src[1]
                if (is.null(tmp.src)) {
                  tmp.Symbols[[each.symbol]] <- src[1]
                }
                else {
                  tmp.Symbols[[each.symbol]] <- tmp.src
                }
            }
            else {
                tmp.Symbols[[each.symbol]] <- src[1]
            }
        }
        Symbols <- tmp.Symbols
    }
    old.Symbols <- NULL
    if (auto.assign && exists(".getSymbols", env, inherits = FALSE)) {
        old.Symbols <- get(".getSymbols", env)
    }
    if (reload.Symbols) {
        Symbols <- c(Symbols, old.Symbols)[unique(names(c(Symbols,
            old.Symbols)))]
    }
    if (!auto.assign && length(Symbols) > 1)
        stop("must use auto.assign=TRUE when reloading multiple Symbols")
    if (!is.null(Symbols)) {
        Symbols <- as.list(unlist(lapply(unique(as.character(Symbols)),
            FUN = function(x) {
                Symbols[Symbols == x]
            })))
        all.symbols <- list()
        for (symbol.source in unique(as.character(Symbols))) {
            current.symbols <- names(Symbols[Symbols == symbol.source])
            # BEGIN NEW CODE #
            if(!is.null(source.envir)) {
                symbols.returned.from.envir <- character()
                for(current.symbols_i in current.symbols) {
                   if(exists(current.symbols_i, envir = source.envir)) {
                     symbols.returned_i <- list()
                     symbols.returned_i[[current.symbols_i]] <- get(current.symbols_i, envir = source.envir)
                     if (auto.assign) {
                        # WILL ONLY HAPPEN ONCE
                        assign(current.symbols_i, symbols.returned_i[[current.symbols_i]], env)
                        symbols.returned.from.envir <- append(symbols.returned.from.envir, current.symbols_i)
                     } else {
                       symbols.returned.from.envir <- symbols.returned_i[[current.symbols_i]]
                     }
                     current.symbols <- setdiff(current.symbols, current.symbols_i)
                   }
                }
                rm(current.symbols_i)
            }
            symbols.returned <- character()
            if(length(current.symbols)){
            # END NEW CODE
              symbols.returned <- do.call(paste("getSymbols.",
                  symbol.source, sep = ""), list(Symbols = current.symbols,
                  env = env, verbose = verbose, warnings = warnings,
                  auto.assign = auto.assign, ...))
            # BEGIN NEW CODE
            }
            if(exists("symbols.returned.from.envir") && length(symbols.returned.from.envir)) {
              if(auto.assign) {
                symbols.returned <- c(symbols.returned, symbols.returned.from.envir)
              } else {
                # WILL ONLY HAPPEN ONCE
                symbols.returned <- symbols.returned.from.envir
              }
            }
            # END NEW CODE
            if (!auto.assign)
                return(symbols.returned)
            for (each.symbol in symbols.returned) all.symbols[[each.symbol]] <- symbol.source
            # BEGIN NEW CODE
            # OVERWRITE
            if(exists("symbols.returned.from.envir") && length(symbols.returned.from.envir))
              for(each.symbol in symbols.returned.from.envir) all.symbols[[each.symbol]] <- attr(get(each.symbol, envir = source.envir), "src")
            # END NEW CODE
        }
        req.symbols <- names(all.symbols)
        all.symbols <- c(all.symbols, old.Symbols)[unique(names(c(all.symbols,
            old.Symbols)))]
        if (auto.assign) {
            assign(".getSymbols", all.symbols, env)
            return(req.symbols)
        }
    }
    else {
        warning("no Symbols specified")
    }
}




# quantmod getModelData with parameter source.envir
#
# get some Symbols from an environment (source.envir)
#   will search first in (source.envir)
#   if the Symbol is not found in the enviroment (source.envir),
#   then get the Symbol from elsewhere
# ...
# passed to getSymbols
#         useful: from, to
#   maybe useful: src
#   maybe useful: set per Symbol src using setSymbolLookup
#
# specifyModel(getModelData)
# if na.rm == TRUE, then does 'na.exclude',
#   BUT 'without rules' PUTS back (rbind) the last observation
#
getModelData <- function (x, na.rm = TRUE, source.envir = NULL, ...) {

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
}



# quantmod specifyModel with parameter source.envir
#
# get some Symbols from an environment (source.envir)
#   will search first in (source.envir)
#   if the Symbol is not found in the enviroment (source.envir),
#   then get the Symbol from elsewhere
# ...
# passed to getSymbols
#         useful: from, to
#   maybe useful: src
#   maybe useful: set per Symbol src using setSymbolLookup
#
# specifyModel(getModelData) original code ( not changed by me )
# if na.rm == TRUE,  then does 'na.exclude'
#   BUT 'without rules' PUTS back (rbind) the last observation
#
# NOTE
# TTR acceptable
# avoid
# specifyModel
# Error in runSum(x, n) : Series contains non-leading NAs
# zoo::na.trim(tail(x), sides = "right")
#
# example ( better: try a do.call )
# Symbols <- unlist(lapply( c("MSFT","AAPL","WMT","COST"), function(x) {
#                           l <- list(); l[[x]] <- getSymbols(x, auto.assign = FALSE); l
#                           }), recursive = FALSE)
# Symbols <- list2env(Symbols)
# ls.str(Symbols)
#
# getSymbols( c("AAPL","ORCL"), source.envir = Symbols)
# rm("AAPL","ORCL")
#
# quantmod <- specifyModel(Next(ClCl(WMT)) ~ Lag(OpCl(AAPL)) + Lag(LoHi(COST),0:2)
#   , source.envir = Symbols
#   , from ="2007-01-01"
#   , to   ="2011-12-31"
#   )
#
specifyModel <- function (formula, na.rm = TRUE, source.envir = NULL, ...) {

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
}




as.quantmod.default <- function(x, outcomename, order.by, na.rm = TRUE, ...) { invisible() }



as.quantmod         <- function(x, outcomename, order.by, na.rm = TRUE, ...) { UseMethod("as.quantmod") }



# from a data.frame, create a quantmod object directly
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
as.quantmod.data.frame  <- function(x, outcomename, order.by, na.rm = TRUE, ...) {

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
  Symbols <- lapply(x, function(x) {
    as.xts(x, order.by = order.by)
  })
  Symbols <- list2env(Symbols)

  # assign where specifyModel ( getModelData ( exists ) ) can find

  # if na.rm == TRUE, then does 'na.exclude',
  #   BUT 'without rules' PUTS back (rbind) the last observation

  # passed to getSymbols
  #
  #         useful: from, to
  #   maybe useful: src
  #   maybe useful: set per Symbol src using setSymbolLookup

  model <- specifyModel(paste0(outcomename, " ~ ", paste0(setdiff(colnames(x), outcomename), collapse = " + "))
             , na.rm = na.rm
             , source.envir = Symbols
             , ...)

  return(model)

}


# S3 method buildModel.train
#
# # tuneGrid ( production tester )
# tg <- expand.grid(
#   nrounds   =  10, # TEN TREES
#   eta       =  c(0.1,0.01),
#   max_depth =  c(4,6,8,10),
#   gamma     =  0,
#   colsample_bytree = c(1,0.5),
#   min_child_weight = 1,
#   subsample        = c(1,0.5)
# )
# # tuneGrid ( non-production tester )
# tg <- expand.grid(
#   nrounds   =  50, # TEST 10 trees - DEV 50 trees - OTHER 500 trees
#   eta       =  c(0.1,0.01),
#   max_depth =  c(4,7,10),
#   gamma     =  0,
#   colsample_bytree = 1,
#   min_child_weight = 1,
#   subsample        = 1
# )
#
# tc <- caret::trainControl(method = "cv", number = 5)
#
# TODO [ ] : fully WORKED EXAMPLE: combine WITH below
# builtModel <- buildModel(specmodel, method="train", training.per=c("1970-12-31","2006-12-31")
#   , method_train = 'xgbTree', tuneGrid = tg, trControl = tc)
#
buildModel.train <- function(quantmod,training.data,...) {

  if(is.method.available('train','caret')) {
    rp <- do.call(train,list(quantmod@model.formula,data=training.data,method = list(...)[["method_train"]], ...))
    return(list("fitted"=rp, "inputs"=attr(terms(rp),"term.labels")))
  }
}



predictModel.default <- function (object, data, ...) {
    predict(object, data, ...)
}


predictModel <- function(object, data, ...) {
    UseMethod("predictModel")
}

# extracted because this is *not public*
#
quantmod___is.method.available <- function(method, package) {
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
}

#
# example (TODO [ ]) simplify
#
#   all_possible_instrument_log_rets <- xts(, zoo::as.Date(0)[0])
#
#   will5000ind          <- get_fred_wilshire5000_eom_xts()
#   will5000ind_log_rets <- ROC(will5000ind)               # which(is.na(will5000ind_log_rets)) # logrithmic
#   will5000ind_log_rets[is.na(will5000ind_log_rets)] <- 0 # usually just the 1st observation
#
#   # will5000ind                       # 1st empty xts
#   all_possible_instrument_log_rets <- merge.xts(all_possible_instrument_log_rets, will5000ind_log_rets)
#
#   # "cash"             # no returns good/bad
#   cash_log_rets <- xts(rep(0,NROW(all_possible_instrument_log_rets)),index(all_possible_instrument_log_rets))
#   colnames(cash_log_rets) <- "cash"
#
#
#   # colnames                                   "cash"    +     "will5000ind"
#   all_possible_instrument_log_rets <- merge.xts(cash_log_rets, all_possible_instrument_log_rets)
#
#   unrate_indicator <- get_symbols_xts_eox("UNRATE", src ="FRED", returns = "monthly", pushback_fred_1st_days =  TRUE, month_delay = 1, OHLC = FALSE, indexAt = "lastof")
#   unrate_indicator <- unrate_indicator[["monthly"]]
#
#   #                                                             "unrate"
#   all_possible_indicators <- merge.xts(all_possible_indicators, unrate_indicator)
#
#   unrate <- all_possible_indicators[,"unrate"]
#
#   unrate1_indicator <- Less(SMA(    unrate   ,2), SMA(    unrate   ,6))
#   colnames(unrate1) <- "unrate1"
#
#   unrate2_indicator <- Less(SMA(lag(unrate)  ,2), SMA(lag(unrate  ),6))
#   colnames(unrate2) <- "unrate2"
#
#   unrate3_indicator <- Less(SMA(lag(unrate,2),2), SMA(lag(unrate,2),6))
#   colnames(unrate3) <- "unrate3"
#
#   # "unrate", "unrate1", "unrate2", "unrate3"
#   all_possible_indicators <- merge.xts(all_possible_indicators, unrate1_indicator, unrate2_indicator, unrate3_indicator)
#
#   merged <- merge(all_possible_instrument_log_rets, all_possible_indicators)
#
#   Symbols <- lapply(as.data.frame(merged), function(x) {
#     as.xts(x, order.by = index(merged))
#   })
#   Symbols <- list2env(Symbols)
#
#   specmodel <- specifyModel(will5000ind ~ unrate1 + unrate2 + unrate3, na.rm = TRUE, source.envir = Symbols)
#
#   tg <- expand.grid(
#     nrounds   =  100,
#     eta       =  c(0.1,0.01),
#     max_depth =  c(4,6,8,10),
#     gamma     =  0,
#     colsample_bytree = c(1,0.5),
#     min_child_weight = 1,
#     subsample        = c(1,0.5)
#   )
#   tc <- caret::trainControl(method = "cv", number = 5)
#
#   builtmodel <- buildModel(specmodel,method="train",training.per=c("1970-12-31","2006-12-31"),
#                   method_caret = 'xgbTree', tuneGrid = tg, trControl = tc)
#
#   gettedModelData <- getModelData(builtmodel, na.rm = TRUE, source.envir = Symbols)
#
#   modeldata <- modelData(getted_model_data, data.window = c("2007-01-31","2018-03-31"), exclude.training = TRUE)
#
#   #                       # dispatch on caret::train
#   fitted  <- predictModel(gettedModelData@fitted.model, modeldata)
#   fitted  <- as.xts(fitted, index(modeldata))
#
#
predictModel.train <- function (object, data, ...) {
    if (quantmod___is.method.available('train','caret')) {
        predict(object, data, ...)
    }
}

