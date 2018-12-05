




#' convert an OHLC xTs into aN OHLC data.frame MEANT to be loaded into a database
#'
#' @param xTs OHLC[V][A] object
#' @param con DBI database connection
#' @param field.names R column names
#' @param db.fields database column names
#' @export
#' @importFrom stringr str_c
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace_all
xTs2DBDF <- function(xTs, con, field.names, db.fields) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  df <- cbind(date = index(xTs), as.data.frame(xTs, stringsAsFactors = FALSE))

  # OHLC columns
  stringr::str_replace_all(colnames(df), "[.]", "_")  -> colnames(df)
  for(grouping in list.zip(field.names, db.fields)){
     colnames(df)[stringr::str_detect(tolower(colnames(df)), stringr::str_c("_", tolower(grouping[["field.names"]]),"$"))] <- grouping[["db.fields"]]
  }
  # all other column names "as is" (e.g. FRED)

  df

})}








#' from column names and datatypes, make a CREATE TABLE statement
#' needed to persistently store data
#'
#' also register its meta-data
#' (Note: if the meta-data table(Symbols) does not exist, then it will be created)
#'
#'# # create automatically by function: dfToCREATETable
#'
#'# CREATE TABLE "Symbols"
#'# (
#'#   "Symbols" text NOT NULL,
#'#   updated timestamp with time zone,
#'#   "updated_R_class" text,
#'#   src text,
#'#   CONSTRAINT "Symbols_pkey" PRIMARY KEY ("Symbols")
#'# )
#'# WITH (
#'#   OIDS=FALSE
#'# );
#'# ALTER TABLE "Symbols"
#'#   OWNER TO "Symbols";
#'
#' @param df data.frame (with column names)
#' @param con DBI database connection
#' @export
#' @importFrom plyr llply
#' @importFrom DBI dbQuoteIdentifier dbExecute dbQuoteString
dfToCREATETable <- function(df, con, Symbol, schname) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  # meta-data table
  if(!"Symbols" %in% pgListSchemaTables(con, "Symbols")) {
    ddl <- paste0("CREATE TABLE ", DBI::dbQuoteIdentifier(con, schname), ".", DBI::dbQuoteIdentifier(con, "Symbols"), "(",
                            DBI::dbQuoteIdentifier(con, "Symbols"),          " TEXT ", ", ",
                            DBI::dbQuoteIdentifier(con, "updated"),          " TIMESTAMP WITH TIMEZONE ", ", ",
                            DBI::dbQuoteIdentifier(con, "updated_R_class") , " TEXT ", ", ",
                            DBI::dbQuoteIdentifier(con, "src"),              " TEXT " ,
                          ");")
    DBI::dbExecute(con, ddl)

    ddl <- paste0("ALTER TABLE ", DBI::dbQuoteIdentifier(con, schname), ".", DBI::dbQuoteIdentifier(con, "Symbols"),
                          " ADD PRIMARY KEY ( ", DBI::dbQuoteIdentifier(con, "Symbols"), ")",
                          ";")
    DBI::dbExecute(con, ddl)
  }
  # upon creation, do Quote Once:  (1)schema, (2)table and (3)column names
  # the PostgreSQL storage will be: anything_capilized retains it's "" quotes.

  if(schname != "") { dotSchemaQuoted <- paste0(DBI::dbQuoteIdentifier(con, schname), ".") } else { dotSchemaQuoted <- "" }
  schemaSymbolsQuoted <-  paste0(dotSchemaQuoted, DBI::dbQuoteIdentifier(con, Symbol))

  # column datatypes
  colClasses  <- do.call(c,plyr::llply(df, function(x) {class(x)[1]}))
  colClasses[colClasses     == "numeric"]    <- "NUMERIC(14,3)"
  colClasses[colClasses %in%   "Date"]       <- "DATE"
  colClasses[colClasses %in%   "POSIXct"]    <- "TIMESTAMP WITH TIMEZONE"
  # ACTUALLY I HAVE NO EXPERIENCE ( THIS IS AN EDUCATED WILD GUESS: LATER, I WILL EXPERIMENT/TEST/FIX THIS )
  # xts OTHER supported index date/time classes
  colClasses[colClasses %in% c("chron", "yearmon", "yearqtr", "timeDate")] <- "TIMESTAMP WITH TIMEZONE"

  ddl <- paste0("CREATE TABLE ", schemaSymbolsQuoted ,"(", paste0( DBI::dbQuoteIdentifier(con, names(colClasses)), " ", colClasses, collapse = ", "), ");")
  DBI::dbExecute(con, ddl)
  ddl <- paste0("ALTER TABLE ", schemaSymbolsQuoted,
                " ADD PRIMARY KEY ( ", DBI::dbQuoteIdentifier(con, names(colClasses)[1]), ")",
                ";")
  DBI::dbExecute(con, ddl)
  dml <- paste0("INSERT INTO ", DBI::dbQuoteIdentifier(con, schname), ".", DBI::dbQuoteIdentifier(con, "Symbols"), 
                             "(", DBI::dbQuoteIdentifier(con, "Symbols"), ") VALUES (", DBI::dbQuoteString(con, Symbol), ");")
  DBI::dbExecute(con, dml)
  invisible()

})}



#' better Names
#'
#' if the argument to names is NULL or has zero(0) length
#' then instead for returning NULL, return character(0)
#'
#' @param x names
#' @export
Names <- function(x) {
  if(is.null(x) || !length(x)) return(character(0))
  names(x) -> res
  if(is.null(res)) return(character(0))
  return(res)
}

#' if x or value is null or has length zero(0) then
#' then that item is character(0)
#'
#' @param x names
#' @param value result
#' @export
`Names<-` <- function(x,value) {

   if(is.null(x)     || !length(x))         x <- character(0)
   if(is.null(value) || !length(value)) value <- character(0)
  `names<-`(x = x, value = value)

}


#' extract a list of R objects and assign to an environment
#'
#' mostly for 'space saving' and cleanliness
#'
#' @param List R list: a collection of R objects
#' @param nms vector of names of List object (default: everything)
#' @param environment to assign R objects
#' @return invisible
#' @examples
#' \dontrun{
#' List <- list(a=1, b=2)
#' # a, b
#' AssignEnv(List, c("a","b"))
#' }
#' @export
#' @importFrom stringr str_c
AssignEnv <- function(List, nms = NULL, envir = parent.frame()) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})
  # default everything
  if(!is.null(nms)) {
    nmsINnames <- nms %in% names(List)
    nms <-    nms[nmsINnames]
    if(any(!nmsINnames)) warning(stringr::str_c("AssignEnv is asking for R objects that are not found: ", nms[!nmsINnames], collapse = ", "))
  }
  for(nm in nms){
    assign(nm, List[[nm]], envir = envir)
  }
  invisible()
})}


#' smartly connect to PostgreSQL database
#'
#' @param user username(default "Symbols") to access database
#' @param password password(default "Symbols") to access database
#' @param dbname database name (default "Symbols")
#' @param schname schema name (default "Symbols")
#' @param host database host (default "localhost")
#' @param port database port (default 5432)
#' @param options pass extra parameters in aa string to the command line
#' @param forceISOdate TRUE(default)/FALSE if the communication of date (time stamp) from PostgreSQL
#' is forced to ISO style at conection
#' @return list of "con" DBI Connection object and "schname" final chosen schema name
#' @export
#' @importFrom RPostgreSQL PostgreSQL
#' @importFrom DBI dbConnect dbExecute dbQuoteIdentifier
pgConnect <- function(user=NULL,password=NULL,dbname=NULL,schname=NULL,
                      host=NULL,port=NULL,options=NULL,forceISOdate=TRUE) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  if(!requireNamespace("DBI", quietly=TRUE))
    stop("package:",dQuote("DBI"),"cannot be loaded.")
  if(!requireNamespace("RPostgreSQL", quietly=TRUE))
    stop("package:",dQuote("RPostgreSQL"),"cannot be loaded.")

  if(is.null(user))     user     <- "Symbols"
  if(is.null(password)) password <- "Symbols"
  if(is.null(dbname))   dbname   <- "Symbols"
  if(is.null(schname))  schname  <- "Symbols"
  if(is.null(user) || is.null(password) || is.null(dbname)) {
    stop(paste(
        'At least one connection argument (',sQuote('user'),
        sQuote('password'),sQuote('dbname'),
        ") is not set"))
  }
  con <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),user=user,password=password,dbname=dbname,
                                     host=host,port=port,options=options,forceISOdate=forceISOdate)

  if(schname != "") {
    schname <- schname
  } else {
    if(NROW(CurrentSearchPath)) {
      CurrentSearchPath <- pgCurrentSearchPath(con)
      schname <- CurrentSearchPath[1,1]
    } else {
      schname <- "public"
    }
  }
  DBI::dbExecute(con, "SET TIME ZONE 'UTC';")
  pgSetCurrentSearchPath(con, DBI::dbQuoteIdentifier(con, schname))
  list(con=con,user=user,password=password,dbname=dbname,schname=schname)

})}



#' of a specific PostgreSQL database schema, show its tables
#'
#' @param con PostgreSQL DBI connection
#' @param schname schema name
#' @return vector of characters of table names
#' The results do not have any order.
#' @export
#' @importFrom DBI dbGetQuery dbQuoteLiteral
pgListSchemaTables <- function(con, schname) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

    DBI::dbGetQuery(con,
      paste0(
       "
        SELECT
              table_catalog -- database
            , table_schema
            , table_name
        FROM
            information_schema.tables
        WHERE
            table_schema     IN (", DBI::dbQuoteLiteral(con, schname), ")
        ;
      "
      )
    ) -> db.Schemas.tables
    db.Schemas.tables[["table_name"]]

})}



#' of a specific PostgreSQL database schema table, show its columns
#'
#' @param con PostgreSQL DBI connection
#' @param schema name
#' @param table name
#' @return vector of characters of column names
#' The results are ordered.
#' @export
#' @importFrom DBI dbGetQuery dbQuoteLiteral
pgListSchemaTableColumns <- function(con, schname, tblname) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

    DBI::dbGetQuery(con,
      paste0(
       "
        SELECT
              table_catalog -- database
            , table_schema
            , table_name
            , column_name
            , udt_name -- human readable datatype
        FROM
            information_schema.columns
        WHERE
            table_schema NOT IN ('information_schema', 'pg_catalog') AND
            table_schema     IN (", DBI::dbQuoteLiteral(con, schname), ") AND
            table_name       IN (", DBI::dbQuoteLiteral(con, tblname), ")
        ORDER BY ordinal_position
        ;
      "
      )
    ) -> db.Schema.tbl
    db.Schema.tbl[["column_name"]]

})}



#' of a specific PostgreSQL database schema table, show its primary key columns
#'
#' @param con PostgreSQL DBI connection
#' @param schema name
#' @param table name
#' @return vector of characters of primary key column names
#' The results are ordered.
#' @export
#' @importFrom DBI dbGetQuery dbQuoteLiteral
pgListSchemaTablePrimaryKeyColumns <- function(con, schname, tblname) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

    # List primary keys for all tables - Postgresql
    #  https://dba.stackexchange.com/questions/11032/list-primary-keys-for-all-tables-postgresql

    DBI::dbGetQuery(con,

      paste0(
       "
        SELECT   tc.table_catalog -- database
               , tc.table_schema
               , tc.table_name
               , kc.column_name
        FROM
            information_schema.table_constraints tc,
            information_schema.key_column_usage kc
        WHERE
            tc.table_schema NOT IN ('information_schema', 'pg_catalog') AND
            tc.table_schema     IN (", DBI::dbQuoteLiteral(con, schname), ") AND
            tc.table_name       IN (", DBI::dbQuoteLiteral(con, tblname), ") AND
            tc.constraint_type = 'PRIMARY KEY' AND
            kc.table_name = tc.table_name and kc.table_schema = tc.table_schema AND
            kc.constraint_name = tc.constraint_name
        ORDER BY 1, 2
        ;
      "

      )
    ) -> db.Schema.tbl
    db.Schema.tbl[["column_name"]]

  })}





#' returns One column from a database (con)
#'
#' @param con DBI database connection
#' @param Query SQL expecting output to be a single value,
#' no value, or a single value with with comma separted internal parts
#' @param outName the new column name
#' @param unQuote (default: FALSE) strip beginning(") and end quotes(")
#' @examples
#' \dontrun{
#'
#' pgCurrentUser <- oneColumn(con, "SELECT CURRENT_USER;", "CurrentUser")
#'
#' pgCurrentSchema <- oneColumn(con, "SELECT current_schema();", "CurrentSchema")
#'
#' pgCurrentDB <- oneColumn(con, "SELECT current_database();", "CurrentDB")
#'
#' pgCurrentSearchPath <- oneColumn(con, "SHOW SEARCH_PATH;", "CurrentSearchPath")
#'
#' pgCurrentTempSchema <- oneColumn(con, 
#'   "SELECT nspname 
#'   FROM pg_namespace 
#'   WHERE oid = pg_my_temp_schema();", 
#' "CurrentTempSchema")
#'
#' }
#' @export
#' @importFrom plyr llply
#' @importFrom DBI dbGetQuery
oneColumn <- function(con, Query, outName, unQuote = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  if(is.null(unQuote)) unQuote = FALSE
  res <- DBI::dbGetQuery(con, Query)
  if(NROW(res)) {
    res <- res[[1]]
    res %>% strsplit(", ") %>%
      {identity(.)[[1]]} %>% as.data.frame(stringsAsFactors = F) %>%
        {colnames(.)[1] <- outName; .} -> ret
  } else {
    tempList <- list()
    tempList[[outName]] <- character()
    as.data.frame(L, stringsAsFactors = F) -> ret
  }
  if(unQuote) {
    split(ret,seq_len(NROW(ret))) %>%
      plyr::llply(function(x) { strsplit(x[[1]], "^\"|\"$")[[1]][2] }) %>% 
        do.call(c,.) %>%
          as.data.frame(., stringsAsFactors = FALSE) -> ret
  }
  colnames(ret) <- outName
  return(ret)
})}



#' get PostgreSQL current user name
#'
#' @rdname pgCurrent
#' @export
pgCurrentUser <- function(con) { oneColumn(con, "SELECT CURRENT_USER;", "CurrentUser") }

#' get PostgreSQL current user schema
#'
#' @rdname pgCurrent
#' @export
pgCurrentSchema <- function(con) { oneColumn(con, "SELECT current_schema();", "CurrentSchema") }

#' get PostgreSQL current user database name
#'
#' @rdname pgCurrent
#' @export
pgCurrentDB <- function(con) { oneColumn(con, "SELECT current_database();", "CurrentDB") }


#' get PostgreSQL current user search path
#'
#' @rdname pgCurrent
#' @export
pgCurrentSearchPath <- function(con) { oneColumn(con, "SHOW SEARCH_PATH;", "CurrentSearchPath", unQuote = TRUE) }


#' set PostgreSQL current user search path
#'
#' @rdname pgCurrent
#' @export
#' @importFrom DBI dbExecute
pgSetCurrentSearchPath <- function(con, path) { DBI::dbExecute(con, paste0("SET SEARCH_PATH TO ", path,";")) }



#' get PostgreSQL current user temp schema
#'
#' @rdname pgCurrent
#' @export
pgCurrentTempSchema <- function(con) { oneColumn(con, "SELECT nspname FROM pg_namespace WHERE oid = pg_my_temp_schema();", "CurrentTempSchema") }


#' get PostgreSQL current user time zone
#'
#' @rdname pgTimeZone
#' @export
pgCurrentTimeZone <- function(con) {  oneColumn(con, "SHOW TIMEZONE;", "CurrentTimeZone") }



#' custom sort a vector
#'
#' excess is appended to the end ( CI sort or CS sort )
#'
#' # Custom Sorting in R
#' # 2014
#' # https://stackoverflow.com/questions/23995285/custom-sorting-in-r
#' # AND
#' # Case insensitive sort of vector of string in R
#' # 2015
#' # https://stackoverflow.com/questions/29890303/case-insensitive-sort-of-vector-of-string-in-r
#'
#' @param Vector vector to be sorted
#' @param InitOrder starting custom sorting ( without the excess )
#' @param CI case insensitive
#' @examples
#' \dontrun{
#' customSorting( c("a","v", "E2", "c","l", "e3" ,"h","o","date"), 
#'    InitOrder = c("date", "o", "h", "l", "c", "v", "a"), CI = TRUE 
#'  )
#' [1] "date" "o"    "h"    "l"    "c"    "v"    "a"    "E2"   "e3"
#' }
#' @export
customSorting <- function(Vector, InitOrder, CI = FALSE) {

  # custom sorting
  VectorLevels <- InitOrder
  VectorExcess <- setdiff(Vector, VectorLevels)
  if(CI == FALSE) {
    VectorExcessCS <-   sort(VectorExcess)
    VectorExcessCaseDetermined <- VectorExcessCS
  } else {
    VectorExcessCI <-   VectorExcess[order(tolower(VectorExcess))]
    VectorExcessCaseDetermined <- VectorExcessCI
  }
  VectorExcessCaseDeterminedLevels <- c(VectorLevels, VectorExcessCaseDetermined)
  VectorFactor <- factor(Vector, levels = VectorExcessCaseDeterminedLevels)
  Vector <- Vector[order(VectorFactor)]
  Vector <- as.vector(Vector)
  Vector

}




#' quantmod getSymbols with source.envir
#'
#' Get some Symbols from an environment (source.envir)
#' It will search first in (source.envir)
#' If the Symbol is not found in the enviroment (source.envir),
#' then get the Symbol from elsewhere.
#'
#' NOTE: do not do: "source.envir = e, "env = e"
#' when auto.assign = TRUE(default), .GetSymbols is placed where "env = e"
#'
#' @param Symbols	a character vector specifying the names of each symbol to be loaded
#' @param env where to create objects. Setting env=NULL is equal to auto.assign=FALSE
#' @param reload.Symbols	boolean to reload current symbols in specified environment. (FALSE)
#' @param verbose boolean to turn on status of retrieval. (FALSE)
#' @param warnings	boolean to turn on warnings. (TRUE)
#' @param src character string specifying sourcing method. (yahoo)
#' @param symbol.lookup retrieve symbol's sourcing method from external lookup (TRUE)
#' @param auto.assign should results be loaded to env If FALSE, return results instead.
#' As of 0.4-0, this is the same as setting env=NULL. Defaults to TRUE
#' @param source.envir override: source environment to aquire the Symbols
#' @param ... pass other parameters
#' @examples
#' \dontrun{
#'
#' e <- new.env(parent = emptyenv())
#'
#' getSymbols(list(AAPL = "yahoo"), env = e) # bcomes INTERNALLY auto.assign = T
#' ls.str(envir = e)
#'
#' # INSTEAD OF "yahoo", get IT from the source.envir
#' AAPLSymbol <- getSymbols(list(AAPL = "yahoo"), auto.assign = FALSE, source.envir = e)
#' str(AAPLSymbol)
#'
#' }
#' @export
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
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})
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
})}



#' Downloads Symbols to specified env from a local R environment
#'
#' @param Symbols	a character vector specifying the names of each symbol to be loaded
#' @param env	where to create objects. (.GlobalEnv)
#' @param return.class	class of returned object
#' @param source.envir where to find xts objects ( location of cache )
#' @param ...	additional parameters
#' @return A call to getSymbols.csv will load into the
#' specified environment one object for each Symbol specified,
#' with class defined by return.class. Presently this may be
#' ts, zoo, xts, data.frame, or timeSeries
#' @examples
#' \dontrun{
#'
#' msft <- getSymbols("MSFT", src = "yahoo", auto.assign = FALSE)
#'
#' source.envir = list2env(list(MSFT = msft))
#' saveSymbols(trg = "cache", source.envir = source.envir)
#' msft2 <- getSymbols(Symbols = "MSFT", src = "cache", auto.assign = F)
#'
#' }
#' @export
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
getSymbols.cache <- function (Symbols = NULL, env, return.class = "xts", cache.envir = NULL,  ...) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})
    importDefaults("getSymbols.cache")

    if(is.null(cache.envir)) cache.envir <- .GlobalEnv

    SymbolsPassed <- Symbols

    this.env <- environment()
    for (var in names(list(...))) {
        assign(var, list(...)[[var]], this.env)
    }

    default.return.class <- return.class
    if (!hasArg("verbose"))
        verbose <- FALSE
    if (!hasArg("auto.assign"))
        auto.assign <- TRUE

    AllSymbols       <- ls(envir = cache.envir, all.names = TRUE)
    LessAllSymbols   <- AllSymbols[stringr::str_detect(AllSymbols,"^[.].+") &
                                  !stringr::str_detect(AllSymbols,"^[.]Random[.]seed$")]
    RetrievedSymbols <- stringr::str_replace(LessAllSymbols, "^[.]","")
    FoundSymbols     <- RetrievedSymbols %in% Symbols
    EnvSymbols       <- RetrievedSymbols[FoundSymbols]
    Symbols          <- EnvSymbols
    if(is.null(SymbolsPassed)) Symbols <- RetrievedSymbols

    # ORIGINAL quantmod-ism FOLLOWS ... [ ] could be cleaned up

    for (i in seq_along(Symbols)) {
        return.class <- default.return.class
        if (verbose)
            cat("loading ", Symbols[[i]], ".....")
        if (!Symbols[[i]] %in% RetrievedSymbols) {
            cat("\n", Symbols[[i]]," does not exist ", "....skipping\n")
            next
        }
        fr <- get(paste0(".", Symbols[[i]]), envir =  cache.envir)
        if (verbose)
            cat("done.\n")
        if (!is.xts(fr)) {
           # try HARDER to get ATTRIBUTES
           updated <- attributes(fr)[["updated"]]
           if(is.null(updated)) updated <- Sys.time()
           src <- attributes(fr)[["src"]]
           if(is.null(src))     src <- "cache"
           # finally ( HARD NOTE: conversion BELOW does not work if already an xts/zoo object)
           fr <- xts(fr[, -1], as.Date(fr[, 1], origin = "1970-01-01"),
                src = src, updated = updated)
        }
        # NO COLUMN NAME ADJUSTMENT/CONVERSTION BECAUSE NOT AN ORIGINAL SOURCE
        fr <- quantmod___convert.time.series(fr = fr, return.class = return.class)
        # NO SYMBOL NAME ADJUSTMENT/CONVERSTION BECAUSE NOT AN ORIGINAL SOURCE
        if (auto.assign)
            assign(Symbols[[i]], fr, env)
    }
    if (auto.assign)
        return(Symbols)
    return(fr)
})}



#' Retrieve Data from PostgreSQL Database
#'
#' @description
#' Fetch data from PostgreSQL database.  As with other
#' methods extending the \code{getSymbols} function,
#' this should \emph{NOT} be called directly.  Its
#' documentation is meant to highlight the formal
#' arguments, as well as provide a reference for
#' further user contributed data tools.
#'
#' @details
#' Meant to be called internally by \code{getSymbols} (see also)
#'
#' One of a few currently defined methods for loading data for
#' use with \pkg{quantmod}. Its use requires the packages
#' \pkg{DBI} and \pkg{RPostgreSQL}, along with a running
#' PostgreSQL database with tables corresponding to the
#' \code{Symbol} name.
#'
#' The purpose of this abstraction is to make transparent the
#' \sQuote{source} of the data, allowing instead the user to
#' concentrate on the data itself.
#'
#' The default configuration needs a table named
#' for the Symbol specified (e.g. MSFT), with
#' column names date,o,h,l,c,v,a. For table
#' layout changes it is best to use
#' \code{setDefaults(getSymbols.PostgreSQL,...)} with
#' the new db.fields values specified.
#' @param Symbols  a character vector specifying the names of each symbol to be loaded
#' @param con DBI connection
#' @param env where to create objects. (.GlobalEnv) CURRENLY BROKEN
#' @param return.class desirect class of returned object.
#' Can be xts, zoo, data.fram, or xts. (zoo)
#' @param db.fields character vector indicating
#' names of fields to retrieve
#' @param field.names names assigned to returned columns
#' @param user username (default "Symbols") to access database
#' @param password password (default "Symbols") to access database
#' @param dbname database name (default "Symbols")
#' @param schname schema name (default "Symbols")
#' @param host database host (default "localhost")
#' @param port database port (default 5432)
#' @param options pass extra parameters in a string to the command line
#' @param forceISOdate TRUE(default)/FALSE if the communication of date (time stamp) from PostgreSQL
#' is forced to ISO style at conection
#' @return A call to getSymbols.MySQL will load into the specified
#' environment one object for each \code{Symbol} specified,
#' with class defined by \code{return.class}.
#' @author Jeffrey A. Ryan
#' @author Andre Mikulec (adapted original code to work with PostgreSQL)
#' @references
#' \cite{PostgreSQL \url{https://www.postgresql.org}}
#' @references
#' \cite{David A. James and Saikat DebRoy (2006). R Interface to the MySQL databse. \url{www.omegahat.net}}
#' @references
#' \cite{R-SIG-DB. DBI: R Database Interface}
#' @seealso
#' \code{\link{getSymbols}}
#' \code{\link{setSymbolLookup}}
#' @keywords data
#' @examples
#' \dontrun{
#' # All 3 getSymbols calls return the same
#' # MSFT to the global environment
#' # The last example is what NOT to do!
#'
#' setDefaults(getSymbols.PostgreSQL,user='jdoe',password='secret',
#'             dbname='tradedata')
#'
#' ## Method #1
#' getSymbols('MSFT',src='PostgreSQL')
#' getSymbols('MSFT',src='pg')
#'
#' ## Method #2
#' setDefaults(getSymbols,src='PostgreSQL')
#' setDefaults(getSymbols,src='pg')
#'   # OR
#' setSymbolLookup(MSFT='PostgreSQL')
#' setSymbolLookup(MSFT='pg')
#'
#' getSymbols('MSFT')
#'
#' #########################################
#' ##  NOT RECOMMENDED!!!
#' #########################################
#' ## Method #3
#' getSymbols.PostgreSQL('MSFT',env=globalenv())
#' getSymbols.pg('MSFT',env=globalenv())
#'
#'#########################################
#' ## YEAR 2018 EXAMPLES
#'#########################################
#'
#' msft <- getSymbols("MSFT", src = "yahoo", auto.assign = FALSE)
#' source.envir = list2env(list(MSFT = msft))
#'
#' # save all of the .getSymbols Symbols and the source.envir Symbols
#' saveSymbols(trg = "pg", source.envir = source.envir)
#'
#' unrate <- getSymbols("UNRATE", src = "FRED", auto.assign =  F)
#' saveSymbols(trg = "pg", source.envir = list2env(list(UNRATE = unrate)))
#'
#' unrate.db <- getSymbols("UNRATE", src = "pg", auto.assign =  F)
#' unrate.db <- getSymbols(Symbols = "UNRATE", src = "pg", auto.assign = F)
#'
#' }
#' @export
#' @importFrom DBI dbSendQuery fetch  dbGetQuery dbDisconnect dbQuoteIdentifier dbQuoteString
#' @importFrom rlang eval_bare parse_expr
getSymbols.PostgreSQL <- function(Symbols = NULL, con = NULL, env, return.class = 'xts',
                               db.fields=c('o','h','l','c','v','a'),
                               field.names = c('Open','High','Low','Close','Volume','Adjusted'),
                               user=NULL,password=NULL,dbname=NULL,schname = NULL,host='localhost',port=5432,
                               options = NULL, forceISOdate = TRUE,
                               ...) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})
  importDefaults("getSymbols.PostgreSQL")

  schnamePassed <- schname
  SymbolsPassed <- Symbols

  this.env <- environment()
  # So no one messes with Date/date
  # BELOW data/Date meaningless ( only referenced by position(number) )
  db.fields   <- c('date', db.fields)
  field.names <- c('Date', field.names)

  for(var in names(list(...))) {
    # import all named elements that are NON formals
    assign(var, list(...)[[var]], this.env)
  }
  if(!hasArg("verbose")) verbose <- FALSE
  if(!hasArg("auto.assign")) auto.assign <- TRUE

  if(is.null(con)) {
    DBConMeta <- pgConnect(user=user,password=password,dbname=dbname,schname=schname,host=host,port=port,options=options,forceISOdate=forceISOdate)
    AssignEnv(DBConMeta, c("con"))
  }
  schname <- pgCurrentSchema(con)[[1]]
  if(!is.null(schnamePassed) && schnamePassed != schname) schname <- schnamePassed

  # ORIGINAL quantmod-ism FOLLOWS ... [ ] could be cleaned up

  AllSymbols <- pgListSchemaTables(con, schname)
  RetrievedSymbols <- AllSymbols[!AllSymbols %in% "Symbols"]
  FoundSymbols     <- RetrievedSymbols %in% Symbols
  db.Symbols       <- RetrievedSymbols[FoundSymbols]
  if(is.null(SymbolsPassed)) db.Symbols <- RetrievedSymbols

  if(length(Symbols) != sum(Symbols %in% db.Symbols)) {
    missing.db.symbol <- Symbols[!Symbols %in% db.Symbols]
    warning(paste('could not load symbol(s): ',paste(missing.db.symbol,collapse=', ')))
    Symbols <- Symbols[Symbols %in% db.Symbols]
  }

  if(schname != "") { dotSchemaQuoted <- paste0(DBI::dbQuoteIdentifier(con, schname), ".") } else { dotSchemaQuoted <- "" }

  schemaSymbolsQuoted <- list()
  Symbols.db.Cols <- list()

  for(i in seq_along(Symbols)) {

    if(verbose)
      cat(paste('Loading ',Symbols[[i]],paste(rep('.',10-nchar(Symbols[[i]])),collapse=''),sep=''))
    # PEEK AHEAD, SEE IF, VOLUME xor ADJUSTED, IS MISSING, SEE IF SINGLE COLUMN (FRED) DATA
    Symbols.db.Cols <- pgListSchemaTableColumns(con, schname, tblname = Symbols[[i]])

    # Goal
    # exist in the DB: 'date', 'o','h','l','c','v','a' ( along with FRED columns )
    # translate from those
    # to R             'Date', 'Open','High','Low','Close','Volume','Adjusted'( along with FRED columns "as is")

    # custom sorting
    db.fields    <- customSorting(Symbols.db.Cols, InitOrder = c("date", "o", "h", "l", "c", "v", "a"), CI = TRUE)
    field.names  <- customSorting(field.names,     InitOrder = c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted"), CI = TRUE)

    db.fieldsQuoted  <-  DBI::dbQuoteIdentifier(con, db.fields)
    schemaSymbolsQuoted[[i]] <-  paste0(dotSchemaQuoted, DBI::dbQuoteIdentifier(con, Symbols[[i]]))

    if(sum(c("o", "h", "l", "c") %in% db.fields) == 4) {
      Selection <- paste( db.fieldsQuoted , collapse=',')
      OHLCData <- TRUE
    } else {
      # NO MATCHES, THEN MAYBE A "SINGLE COLUMN (FRED) DATA"
      Selection <- "*"
      OHLCData <- FALSE
    }

    # ABOVE (^) ALREADY QUOTED
    query <- paste0("SELECT ", Selection," FROM ", schemaSymbolsQuoted[[i]]," ORDER BY ", DBI::dbQuoteIdentifier(con, "date"), ";")
    rs <- DBI::dbSendQuery(con, query)
    fr <- DBI::fetch(rs, n=-1)

    query <- paste0("SELECT ", " * ", " FROM ", DBI::dbQuoteIdentifier(con, schname), ".", DBI::dbQuoteIdentifier(con, "Symbols")," WHERE ", DBI::dbQuoteIdentifier(con, "Symbols"), " = ", DBI::dbQuoteString(con, Symbols[[i]]), ";")
    SymbolAttributes <- DBI::dbGetQuery(con, query)[,-1,drop =FALSE]

    updated <- NULL
    if("updated" %in% colnames(SymbolAttributes)) updated <- SymbolAttributes[["updated"]]

    updated_R_class <- NULL
    if("updated_R_class" %in% colnames(SymbolAttributes)) {
      updated_R_class <- SymbolAttributes[["updated_R_class"]]
      if(!updated_R_class %in% c("ts","data.frame")) {
        if(!isNamespaceLoaded(updated_R_class)) requireNamespace(updated_R_class, quietly = TRUE)
      }
      updated <- rlang::eval_bare(rlang::parse_expr(paste0("as.", updated_R_class,"(updated)")), environment())
    }
    src <- NULL
    if("src" %in% colnames(SymbolAttributes)) src <- SymbolAttributes[["src"]]

    DBI::dbDisconnect(con)

    order.by= do.call(c,fr[,1, drop = F])
    #fr <- data.frame(fr[,-1],row.names=fr[,1])
    fr <- xts(as.matrix(fr[,-1]),
              order.by=order.by,
              src=src,updated=updated)
    # MAY NOT HAVE VOLUME/ADJUSTED

    if(OHLCData) {
      colnames(fr) <- paste(Symbols[[i]],field.names[-1], sep='.')
    } else { # as-is # e.g. FRED
      colnames(fr) <- db.fields[-1]
    }
    fr <- quantmod___convert.time.series(fr=fr,return.class=return.class)
    if(auto.assign)
      assign(Symbols[[i]],fr,env)
    if(verbose) cat('done\n')
  }
  DBI::dbDisconnect(con)
  if(auto.assign)
    return(Symbols)
  return(fr)

})}
#' getSymbols pg
#'
#'@export
getSymbols.pg <- getSymbols.PostgreSQL


#' get a recent Symbol i(f I do not have one)
#'
#' @details
#'
#' first look in  nextsrc[1]
#' if not found look in  nextsrc[2], . . . etc
#' if ran out of  nextsrc, look in src
#'   then next back-updated all of  nextsrc (going backwards)
#'  alt: if DID NOT run out of nextsrc. A  nextsrc found the newer data.
#'    then backload the newer data into all previous nextsrcs (going backwards)
#'
#' @param Symbols as quantmod getSymbols: a character vector specifying the names of each symbol to be loaded
#' @param con DBI connection
#' @param envir.source source of Symbols
#' @param env as quantmod getSymbols: where to create objects. Setting env=NULL is equal to auto.assign=FALSE
#' @param reload.Symbols as quantmod getSymbols: boolean to reload current symbols in specified environment. (FALSE)
#' @param verbose as quantmod getSymbols: boolean to turn on status of retrieval. (FALSE)
#' @param warnings as quantmod getSymbols: boolean to turn on warnings. (TRUE)
#' @param src as quantmod getSymbols: character string specifying sourcing method. (yahoo)
#' @param symbol.lookup as quantmod getSymbols: retrieve symbol's sourcing method from external lookup (TRUE)
#' @param auto.assign as quantmod getSymbols: should results be loaded to env If FALSE, return results instead. As of 0.4-0, this is the same as setting env=NULL. Defaults to TRUE
#' @param file.path as quantmod getSymbols: character string of file location
#' @param nextsrc next alternative sourcing methods trying in order, if found data is refreshed in reverse order
#' @param MaxAge (default "4 hours") is longest age allowed, such the retrieving from
#' the "cache" will be done. If the MaxAge is exceeded then,
#' the Symbol(s) are refreshed anew from "src".
#' The format uses as.difftime: "# secs", "# mins", "# hours", "# days", "# weeks"
#' @param ... as quantmod getSymbols: additional parameters
#' @examples
#' \dontrun{
#'
#' # before, maybe, be sure,
#' # that "MSFT" is in the cache "cache" or database "pg"
#' # (example not shown)
#'
#' ls(all.names = TRUE)
#' msft <- getNewSymbols("MSFT", src = "yahoo", auto.assign = FALSE)
#' ls(all.names = TRUE)
#'
#' }
#' @export
getNewSymbols <- function (Symbols = NULL, env = parent.frame(), reload.Symbols = FALSE,
    verbose = FALSE, warnings = TRUE, src = "yahoo", symbol.lookup = TRUE,
    auto.assign = getOption("getSymbols.auto.assign", TRUE), source.envir = NULL,
    nextsrc = c("cache", "pg"), MaxAge = NULL, ...) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})
  importDefaults("getNewSymbols")

  if(is.null(MaxAge)) MaxAge <- "4 hours"
  MaxAgeValueUnits <- strsplit(MaxAge, " ")[[1]]
  MaxAgeValue <- as.integer(MaxAgeValueUnits[1])
  MaxAgeUnits <- MaxAgeValueUnits[2]

  nextrcSeqtoBeUpdated <- integer(0)

  AllUserSymbols <- unique(unlist(strsplit(Symbols, ";")))
  Symbols <- AllUserSymbols

  if (!auto.assign && length(Symbols) > 1)
  stop("must use auto.assign=TRUE when reloading multiple Symbols")

  for(i in seq_along(Symbols)) {

    for(nextsrc_forward in nextsrc) {

      FoundinNextSrc <- FALSE
      if(existSymbols(Symbols[[i]], src = nextsrc_forward)[[1]]) {
        updated <- updatedSymbols(Symbols[[i]], src = nextsrc_forward)[[1]]
        FoundinNextSrc <- TRUE
      }

      isTooOld <- "DIDNOTFIND"
      if(FoundinNextSrc) {
        AgeTestTooOld <- as.difftime(MaxAgeValue,  units = MaxAgeUnits) <  difftime(Sys.time(), updated)
        if(AgeTestTooOld) { isTooOld <- "YES"} else {  isTooOld <- "NO"}
      }

      if((isTooOld %in% c("DIDNOTFIND","YES"))) {
        next
      } else {
        break
      }

    } # nextsrc_forward in nextsrc
    if(isTooOld %in% c("DIDNOTFIND", "YES")) { # then get it from "src"
      # not in our "nextsrc",
      # therfore get it from the source "src"
      xTs <- getSymbols(Symbols = Symbols[[i]], env = env, reload.Symbols = reload.Symbols,
      verbose = verbose, warnings = warnings, src = src, symbol.lookup = symbol.lookup,
      auto.assign = FALSE, source.envir = source.envir,
      ...)

      # index(sequence) of (current(last) and) previous nextsrc values in 'reverse order' (SEE BELOW)
      nextrcSeqtoBeUpdated <- match(nextsrc_forward, nextsrc, 0) %>%
                             {seq_len(.) - 0} %>% {identity(.)[] } %>% rev
                              # creating a reverse order sequence
    }

    if(isTooOld %in% c("NO")) {  # then get it from nextsrc_forward
      # not in our "nextsrc",
      # therfore get it from the source "src"
      xTs <- getSymbols(Symbols = Symbols[[i]], env = env, reload.Symbols = reload.Symbols,
      verbose = verbose, warnings = warnings, src = nextsrc_forward, symbol.lookup = symbol.lookup,
      auto.assign = FALSE, source.envir = source.envir,
      ...)

      # index(sequence) of the previous nextsrc values in 'reverse order' (SEE BELOW)
      nextrcSeqtoBeUpdated <- match(nextsrc_forward, nextsrc, 0) %>%
                             {seq_len(.) - 1} %>% {identity(.)[-1] } %>% rev
                              # previous           # remove the value zero(0)

    }

    for(nextrcItoBeUpdated in nextrcSeqtoBeUpdated) {

      # write back to the previous nextsrc
      tempList <- list()
      tempList[[Symbols[[i]]]] <- xTs
      saveSymbols(Symbols[[i]], Gathering = "source.envir",
                   trg = nextsrc[nextrcItoBeUpdated], source.envir = list2env(tempList), ...)
      # extra TO DO on the INDEX(1)
      # index of 1, also "Get" it to be loaded into "env" ( conditoins: auto.assign = TRUE or !is.null(env) )
      # INHERITS (auto.assign)
      if(nextrcItoBeUpdated == 1) {

        # get it back from "where I saved from" ( not applicable: if auto.assign = TRUE )
        xTs2 <- getSymbols(Symbols = Symbols[[i]], env = env, reload.Symbols = reload.Symbols,
        verbose = verbose, warnings = warnings, src = nextsrc[nextrcItoBeUpdated], symbol.lookup = symbol.lookup,
        auto.assign = auto.assign, source.envir = NULL,...)
        if(!auto.assign){
          if(!identical(xTs,xTs2)) message("getNewSymbols: !auto.assign: !identical(xTs,xTs2)")
        }
      }

    } # nextrcItoBeUpdated in nextrcSeqtoBeUpdated

  } # i in seq_along(Symbols)

  if (!auto.assign && length(Symbols) == 1) {
    return(xTs)
  }

})}




#' saves xts object symbols to a persistent location (dispatcher)
#'
#' First, it will look for Symbols in the .getSymbols file and env and gather them
#' Next,  it will look for Symbols in source.envir and gather them
#'
#' If provided file.path, then object will be stored on disk(same as "RData")
#' If provided trg == "RData", "cache", or "PostgreSQL (or just "pg")
#' then the objectw be ALSO saved in this OTHER location
#'
#' Needs either/both "trg" or "file.path"
#'
#' @param Symbols	a character vector specifying the names of each symbol
#' @param source.envir source location of Symbols
#' @param env location of xts objects placed that had been aquired
#' with getSymbols("XXX", src = 'yahoo')
#' Gathering = "DotGetSymbols" and "file.path" look 'here' for Symbols
#' @param Gathering places to look for symbols to be collected
#' Can be "DotgetSymbols" and/or "source.envir"
#' Symbol names must be case-insenstive unique.
#' @param file.path if provided will save to disk
#' @param trg if provided will save to a target "cache"(Symbol name prepended with a ".")
#' or "pg" (PostgreSQL)
#' See ? saveSymbols.cache and ? saveSymbols.pg
#' @param target.envir if provided will save to to memory
#' Note: .getSymbols is * NOT UPDATED.*  A user is reponsible for 'self-tracking.'
#' @param ... passed to trg
#' @examples
#' \dontrun{
#'
#' # Symbols must be case-insenstive unique
#'
#' # Symbols names found in Symbols and the names of xts objects stored
#' # in source.envir, must be mutually exclusive ( case in-sensensitive match)
#' # (because list2env silently drops repeated members)
#'
#' getSymbols("IBM", src = "yahoo")
#'
#' msft <- getSymbols("MSFT", src = "yahoo", auto.assign = FALSE)
#' source.envir = list2env(list(MSFT = msft))
#'
#' # save only the "source.envir" Symbols
#' saveSymbols(source.envir = source.envir, Gathering = "source.envir", target.envir = e)
#'
#' # save only the DotgetSymbols" Symbols #
#' saveSymbols(source.envir = source.envir, Gathering = "DotgetSymbols", target.envir = e)
#'
#' # compatible with quantmod::saveSymbols
#' # when using "file.path"
#' # save the "DotgetSymbols"(required) to a file
#' saveSymbols(file.path = "C:\\Users\\Public")
#'
#' # compatible with quantmod::saveSymbols
#' # when using "file.path"
#' # save both "source.envir" and "DotgetSymbols"(required) to a file
#' saveSymbols(source.envir = source.envir,
#'   Gathering = "source.envir", file.path = "C:\\Users\\Public")
#'
#' # when using "file.path"
#' # save ONLY "source.envir" to a file
#' # SEE ? saveSymbols.RData
#' saveSymbols.RData(source.envir = source.envir, file.path = "C:\\Users\\Public")
#'
#' }
#' @export
#' @importFrom plyr llply
saveSymbols <- function(Symbols = NULL, env = parent.frame(),
                       source.envir = NULL, Gathering = "source.envir", ...) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})
  importDefaults("saveSymbols")

  source.envirPassed <- source.envir
  Dots <- list(...)

  # all init
  xTsGetSymbols <- list()

  xTsGetSymbolsDotgetSymbols <- list() # keep compatible with quantmod::saveSymbols
  if("DotgetSymbols" %in% Gathering || !is.null(Dots[["file.path"]])) {

    # gather from .getSymbols file and env
    DotgetSymbolsFound <- FALSE
    if (exists(".getSymbols", env, inherits = FALSE)) {
      DotgetSymbols <- get(".getSymbols", env, inherits = FALSE)
      Inherits <- FALSE
      DotgetSymbolsFound <- TRUE
    }

    if(DotgetSymbolsFound) {

      RetrievedSymbols      <- names(DotgetSymbols)
      FoundSymbols          <- RetrievedSymbols %in% Symbols
      DotgetUsingSymbols    <- RetrievedSymbols[FoundSymbols]
      if(is.null(Symbols))  DotgetUsingSymbols <- RetrievedSymbols

      for (each.symbol in DotgetUsingSymbols) {
        if(!each.symbol %in% names(xTsGetSymbols)){
          xTsGetSymbols <- c(xTsGetSymbols, list(get(each.symbol, envir = env, inherits = Inherits)))
          names(xTsGetSymbols)[length(xTsGetSymbols)] <- each.symbol
        }
      }

    }

  }
  xTsGetSymbolsDotgetSymbols <- xTsGetSymbols
  xTsGetSymbols <- list()

  xTsGetSymbolssource.envir <- list()
  if("source.envir" %in% Gathering) {

    # gather from source.envir

    runenv <- environment()
    # look in my custom environment
    if(is.environment(source.envir)) {

    RetrievedSymbols <- ls(envir = source.envir)
    FoundSymbols <- RetrievedSymbols %in% Symbols
    EnvSymbols   <- RetrievedSymbols[FoundSymbols]
    if(is.null(Symbols))  EnvSymbols <- RetrievedSymbols

    plyr::llply(EnvSymbols, function(x) {
      xx <- get(x, source.envir, inherits = FALSE)
      if((class(xx))[1] %in% c("zoo","xts", "data.frame","ts","timeSeries")) {
        if(!x %in% names(xTsGetSymbols )){
           xTsGetSymbols <- c(xTsGetSymbols, list(xx))
           names(xTsGetSymbols)[length(xTsGetSymbols)] <- x
           assign("xTsGetSymbols", xTsGetSymbols, envir = runenv)
         }
      }
    })}

  }
  xTsGetSymbolssource.envir <- xTsGetSymbols
  xTsGetSymbols <- list()

  # check for case-insenstive duplicates
  # ( because list2env SILENTYLY DELETES symbols of the same name AND in a DIFFERENT case)
  SamexTsGetSymbols <- intersect(tolower(names(xTsGetSymbolsDotgetSymbols)),tolower(names(xTsGetSymbolssource.envir)))
  if(length(SamexTsGetSymbols)) {
    stop(paste0("In saveSymbols, duplicate case-insenstive naames found in ", paste0(SamexTsGetSymbols, collapse = ", ")))
  }

                                       # keep compatible with quantmod::saveSymbols
  if("DotgetSymbols" %in% Gathering || !is.null(Dots[["file.path"]])) {
    xTsGetSymbols <- c(list(),xTsGetSymbolsDotgetSymbols)
  }
  # everything: either, may be empty
  xTsGetSymbols <- c(list(),xTsGetSymbols, xTsGetSymbolssource.envir)

  # save everything back to my custom environment (source.envir)
  # note list2env SILENTYLY DELETES symbols of the same name AND in a DIFFERENT case
  source.envir <- list2env(xTsGetSymbols, parent = emptyenv())

 # keep compatible with quantmod::saveSymbols
  if("file.path" %in% names(Dots) && is.null(source.envirPassed)) {
    do.call(saveSymbols.RData, c(list(), source.envir = list2env(xTsGetSymbols), Dots))
  }
  if("trg" %in% names(Dots)) {
    do.call(paste0("saveSymbols",".", Dots[["trg"]]), c(list(),
      Symbols = Symbols, source.envir = source.envir, Dots[!names(Dots) %in% "trg"]))
  }

  if(!is.null(Dots[["target.envir"]])) {
    for(var in names(xTsGetSymbols)) {
      assign(var, xTsGetSymbols[[var]], target.envir)
    }
  }

  invisible()

})}



#' saves xts object symbols to a persistent location (disk: Symbol.RData file)
#'
#' see quantmod SaveSymbols
#'
#' @param Symbols	a character vector specifying the names of each symbol
#' @param source.envir source location of Symbols
#' @param file.path  character string of file (Symbol.Rdata) location
#' @param ... pass through not used
#' @examples
#' \dontrun{
#'
#' msft <- getSymbols("MSFT", src = "yahoo", auto.assign = FALSE)
#' source.envir = list2env(list(MSFT = msft))
#' # save to a file
#' saveSymbols(source.envir = source.envir, file.path = "C:\\Users\\Public")
#'
#'}
#' @export
saveSymbols.RData <- function (Symbols = NULL,
  file.path = stop("must specify 'file.path'"), source.envir = NULL, ...) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})
  importDefaults("saveSymbols.RData")

  if(!is.null(source.envir)) {
    source.list <- as.list(source.envir)
  } else {
    stop("SaveSymbols.RData source.envir is NULL")
  }

  RetrievedSymbols <- Names(source.list)
  FoundSymbols <- RetrievedSymbols %in% Symbols
  EnvSymbols   <- RetrievedSymbols[FoundSymbols]
  if(is.null(Symbols))  EnvSymbols <- RetrievedSymbols

  for(each.symbol in EnvSymbols) {
     save(list = source.list[[each.symbol]], file = paste(file.path, "/", each.symbol, ".RData", sep = ""), envir = source.envir)
  }
  invisible()

})}



#' save xts objects as .xts in source.envir ( cache )
#'
#' @param Symbols	a character vector specifying the names of each symbol
#' @param source.envir location of xts objects
#' @param target.envir location of where to store xts objects ( location of cache )
#' @param ... not used
#' @examples
#' \dontrun{
#'
#' msft <- getSymbols("MSFT", src = "yahoo", auto.assign = FALSE)
#' source.envir = list2env(list(MSFT = msft))
#' saveSymbols(trg = "cache", source.envir = source.envir)
#' saveSymbols(trg = "cache", source.envir = source.envir, cache.envir = .GlobalEnv)
#'
#' }
#' @export
saveSymbols.cache <- function (Symbols = NULL, source.envir = NULL, cache.envir = NULL, ...) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})
    importDefaults("saveSymbols.cache")

    if(is.null(cache.envir)) cache.envir <- .GlobalEnv

    source.list <- as.list(source.envir)

    RetrievedSymbols <- Names(as.list(source.envir))
    FoundSymbols <- RetrievedSymbols %in% Symbols
    EnvSymbols   <- RetrievedSymbols[FoundSymbols]
    if(is.null(Symbols)) EnvSymbols <- RetrievedSymbols

    for(each.symbol in EnvSymbols) {
        assign(paste0(".", each.symbol), source.list[[each.symbol]], envir = cache.envir)
    }
    invisible()
})}




#' saves xts object symbols to a persistent location (database: PostgreSQL)
#'
#' # Once only, the end-user must pre-create the [database and [ user and]] schema
#'
#' CREATE ROLE "Symbols" LOGIN
#'   NOSUPERUSER INHERIT NOCREATEDB NOCREATEROLE NOREPLICATION;
#'
#' ALTER USER "Symbols" PASSWORD 'Symbols';
#'
#' CREATE DATABASE "Symbols"
#'   WITH OWNER = "Symbols"
#'        ENCODING = 'UTF8'
#'        TABLESPACE = pg_default
#'        LC_COLLATE = 'C'
#'        LC_CTYPE = 'C'
#'        CONNECTION LIMIT = -1;
#'
#' CREATE SCHEMA "Symbols"
#'   AUTHORIZATION "Symbols";
#'
#'# # create automatically by function: dfToCREATETable
#'# CREATE TABLE "Symbols"
#'# (
#'#   "Symbols" text NOT NULL,
#'#   updated timestamp with time zone,
#'#   "updated_R_class" text,
#'#   src text,
#'#   CONSTRAINT "Symbols_pkey" PRIMARY KEY ("Symbols")
#'# )
#'# WITH (
#'#   OIDS=FALSE
#'# );
#'# ALTER TABLE "Symbols"
#'#   OWNER TO "Symbols";
# #
#' @param Symbols	a character vector specifying the names of each symbol
#' @param sourc.envir location of xts objects
#' @param field.names names existing in starting columns
#' @param db.fields character vector indicating
#' names of fields to insert
#' @param user username(default "Symbols") to access database
#' @param password password(default "Symbols") to access database
#' @param dbname database name (default "Symbols")
#' @param schname schema name (default "Symbols")
#' @param host database host (default "localhost")
#' @param port database port (default 5432)
#' @param options pass extra parameters in aa string to the command line
#' @param forceISOdate TRUE(default)/FALSE if the communication of date (time stamp) from PostgreSQL
#' is forced to ISO style at conection
#' @param ... pass through parameters
#' @examples
#' \dontrun{
#'
#' msft <- getSymbols("MSFT", src = "yahoo", auto.assign = FALSE)
#' source.envir = list2env(list(MSFT = msft))
#'
#' # save all of the .getSymbols Symbols and the source.envir Symbols
#' saveSymbols(trg = "pg", source.envir = source.envir)
#'
#' unrate <- getSymbols("UNRATE", src = "FRED", auto.assign =  F)
#' saveSymbols(trg = "pg", source.envir = list2env(list(UNRATE = unrate)))
#'
#' unrate.db <- getSymbols("UNRATE", src = "pg", auto.assign =  F)
#' unrate.db <- getSymbols(Symbols = "UNRATE", src = "pg", auto.assign = F)
#'
#' }
#' @export
#' @importFrom DBI dbExecute dbWriteTable dbQuoteString dbQuoteIdentifier dbDisconnect
saveSymbols.PostgreSQL <- function(Symbols = NULL, con = NULL, source.envir = NULL,
  field.names = c('Open','High','Low','Close','Volume','Adjusted'),
  db.fields=c('o','h','l','c','v','a'),
  user=NULL,password=NULL,dbname=NULL,schname=NULL,host='localhost',port=5432,options=NULL, forceISOdate = TRUE,
  ...)  {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})
  importDefaults("saveSymbols.PostgreSQL")

  schnamePassed <- schname
  SymbolsPassed <- Symbols

  this.env <- environment()

  # HOPEFULLY no one messes with Date/date
  # HOPEFULLY BELOW data/Date meaningless ( only referenced by position(number[-1]) )
  field.names <- c('Date', field.names)
  db.fields   <- c('date', db.fields)

  for(var in names(list(...))) {
    # import all named elements that are NON formals
    assign(var, list(...)[[var]], this.env)
  }
  if(!hasArg("verbose")) verbose <- FALSE

  if(is.null(con)) {
    DBConMeta <- pgConnect(user=user,password=password,dbname=dbname,schname=schname,host=host,port=port,options=options,forceISOdate=forceISOdate)
    AssignEnv(DBConMeta, c("con"))
  }
  schname <- pgCurrentSchema(con)[[1]]
  if(!is.null(schnamePassed) && schnamePassed != schname) schname <- schnamePassed

  # now, I am only getting symbols from here
  ###### Symbols <- names(as.list(source.envir))[names(as.list(source.envir)) %in% Symbols]

  RetrievedSymbols  <- names(as.list(source.envir))
  FoundSymbols      <- RetrievedSymbols %in% Symbols
  EnvSymbols        <- RetrievedSymbols[FoundSymbols]
  Symbols           <- EnvSymbols
  if(is.null(SymbolsPassed)) Symbols <-RetrievedSymbols

  db.Symbols <- pgListSchemaTables(con, schname)
  db.Symbols <- db.Symbols[!db.Symbols %in% "Symbols"]

  # ORIGINAL quantmod-ism FOLLOWS ... [ ] could be cleaned up

  if(length(Symbols) != sum(Symbols %in% db.Symbols)) {
    missing.db.symbol <- Symbols[!Symbols %in% db.Symbols]
    # for the requested Symbol, if I do not have database table so, I have to create ONE

    # create *new* empty TABLEs
    dfs <- list()
    new.db.symbol <- c()
    # IMPORTANT!!
    # NOTE, IF THE STRUCTURE of THE incoming OBJECT is different from the STORED object
    # E.G.! NEW COLUMNS, DIFFERENT R/POSTGRESQL CLASS OF the index
    # then MAYBE
    # dbWriteTable WILL FAIL? and/or
    # DIFFERENT PROBLEM with MISSING other COLUMNS?
    # SEE caroline::dbWriteTable2
    # SEE RPostgre::dbWriteTable
    # SEE MY OWN NOTES
    # (I HAVE NOT HANDLED THESE 'change' CASES)
    # NOTE: the INDEX type probably will not change
    # BUT getting NEW added COLUMNS would be COMMON
    # so I WOULD (IN THE NEAR FUTURE) have to do:
    # (1) detect new column name (2) ADD COLUMN

    for (each.symbol in  missing.db.symbol) {

      xTs <- as.list(source.envir)[[each.symbol]]
      if(is.null(xTs)) { message(paste("Symbol ", each.symbol, " was not found, so skipping.")); next }
      df  <- xTs2DBDF(xTs = xTs, con = con, field.names = field.names[-1], db.fields = db.fields[-1])

      # create NEW CREATE TABLE statements"
      # column names
      dfToCREATETable(df = df, con = con, Symbol = each.symbol, schname = schname)

      new.db.symbol <- c(new.db.symbol, each.symbol)
      tempList <- list(); tempList[[each.symbol]] <- df
      dfs <- c(dfs, tempList)
    }

    missing.db.symbol <- setdiff(missing.db.symbol, new.db.symbol)
    if(length(missing.db.symbol))
      warning(paste('can not save symbol(s): ',paste(missing.db.symbol,collapse=', ')))
    db.Symbols <- unique(c(db.Symbols, new.db.symbol))

  }
  Symbols <- Symbols[Symbols %in% db.Symbols]

  # make empty TABLEs, then fill empty TABLEs
  for (each.symbol in Symbols) {

    xTs <- as.list(source.envir)[[each.symbol]]
    if(is.null(xTs)) { message(paste("Symbol ", each.symbol, " was not found s skipping.")); next }
    df  <- xTs2DBDF(xTs, con, field.names = field.names[-1], db.fields = db.fields[-1])

    if(schname != "") { dotSchemaQuoted <- paste0(DBI::dbQuoteIdentifier(con, schname), ".") } else { dotSchemaQuoted <- "" }
    DBI::dbExecute(con, paste0("TRUNCATE TABLE ", dotSchemaQuoted, DBI::dbQuoteIdentifier(con, each.symbol), ";"))

    # Goal
    # exist in  R       'Date', 'Open','High','Low','Close','Volume','Adjusted'( along with FRED columns )
    # translate from those
    # to DB: 'date', 'o','h','l','c','v','a' ( along with FRED columns "as is")

    # custom sorting
    db.fields    <- customSorting( colnames(df), InitOrder = db.fields, CI = TRUE)
    colnames(df) <- db.fields

    DBI::dbWriteTable(con, each.symbol, df, append = T, row.names = F)
    # if  each.symbol includes schema name + ".",
    # then dbWriteTable will return TRUE, but it will LIE

    updated <- NULL
    if("updated" %in% names(attributes(xTs))) {
      # OLD: "to_timestamp(", DBI::dbQuoteString(con, as.character(Sys.time())), " ,'YYYY-MM-DD HH24:MI:SS')"
      updated <- attributes(xTs)[["updated"]]
      # How to properly handle timezone when passing POSIXct objects between R and Postgres DBMS?
      # https://stackoverflow.com/questions/40524225/how-to-properly-handle-timezone-when-passing-posixct-objects-between-r-and-postg
      updated <- format(as.POSIXct(updated, tz = Sys.getenv("TZ")), "%Y-%m-%d %H:%M:%OS%z")
      DBI::dbExecute(con, paste0("UPDATE ", DBI::dbQuoteIdentifier(con, schname), ".", DBI::dbQuoteIdentifier(con, "Symbols"),
                            " SET ",
                            DBI::dbQuoteIdentifier(con, "updated"), " = ", DBI::dbQuoteString(con, updated),
                            " WHERE ",
                            DBI::dbQuoteIdentifier(con, "Symbols"), " = ", DBI::dbQuoteString(con, each.symbol),
                            ";"))
    }

    updated_R_class <- NULL
    if(inherits(xTs,"zoo")) {
      updated_R_class <- class(index(xTs))[1]
      DBI::dbExecute(con, paste0("UPDATE ", DBI::dbQuoteIdentifier(con, schname), ".", DBI::dbQuoteIdentifier(con, "Symbols"),
                            " SET ",
                            DBI::dbQuoteIdentifier(con, "updated_R_class")," = ", DBI::dbQuoteString(con, updated_R_class),
                            " WHERE ",
                            DBI::dbQuoteIdentifier(con, "Symbols"), " = ", DBI::dbQuoteString(con, each.symbol),
                            ";"))
    }
    src <- NULL
    if("src" %in% names(attributes(xTs))) {
      src <- as.character(attributes(xTs)[["src"]])
      DBI::dbExecute(con, paste0("UPDATE ", DBI::dbQuoteIdentifier(con, schname), ".", DBI::dbQuoteIdentifier(con, "Symbols"),
                            " SET ",
                            DBI::dbQuoteIdentifier(con, "src"), " = ", DBI::dbQuoteString(con, src),
                            " WHERE ",
                            DBI::dbQuoteIdentifier(con, "Symbols"), " = ", DBI::dbQuoteString(con, each.symbol),
                            ";"))
    }

  }
  DBI::dbDisconnect(con)
  invisible()

})}
#' saveSymbols pg
#'
#'@export
saveSymbols.pg <- saveSymbols.PostgreSQL



#' 'updated' date-ish/time-ish property
#' of an [xts] object symbols in a persistent location (dispatcher)
#'
#' looks in source.envir
#'
#' @param Symbols	a character vector specifying the names of each symbol
#' @param source.envir source location of Symbols
#' @param src source program to look for symbols
#' @param ... passed to src
#' @return a named list of 'updated' properties
#' @examples
#' \dontrun{
#'
#' # Symbols must be case-insenstive unique
#'
#' getSymbols("IBM", src = "yahoo") # auto.assign = TRUE
#'
#' # Symbols names found in Symbols and the names of xts objects stored
#' # in source.envir, must be mutually exclusive ( case in-sensensitive match)
#' # (because list2env uses a case-insenstive comparison and silently drops repeated members)
#'
#' # save just the Symbol "IBM"
#' saveSymbols("IBM", trg = "pg")
#'
#' msft <- getSymbols("MSFT", src = "yahoo", auto.assign = FALSE)
#' source.envir = list2env(list(MSFT = msft))
#' listSymbols(source.envir = source.envir)
#' updatedSymbols(Symbols  = "MSFT", source.envir = source.envir)
#' existSymbols(Symbols = "MSFT", source.envir = source.envir)
#'
#' saveSymbols(trg = "cache", source.envir = source.envir)
#' listSymbols(src = "cache")
#' updatedSymbols(Symbols  = "MSFT",src = "cache")
#' existSymbols(Symbols = "MSFT", src = "cache")
#'
#' # save all of the .getSymbols Symbols and the source.envir Symbols
#' saveSymbols(trg = "pg", source.envir = source.envir)
#' listSymbols(src = "pg")
#' updatedSymbols(Symbols  = "MSFT",src = "pg")
#' existSymbols(c("MSFT","IBM"), src = "pg")
#'
#' }
#' @export
#' @importFrom plyr llply
updatedSymbols <- function(Symbols = NULL, source.envir = NULL, ...) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})
  importDefaults("updatedSymbols")

  Dots <- list(...)

  EnvSymbols <- list()
  if(!"src" %in% names(Dots)) {
    runenv <- environment()
    # look in my custom environment
    if(is.environment(source.envir)) {

    AllSymbols <- ls(envir = source.envir, all.names = TRUE)
    FoundAllSymbols <- AllSymbols %in% Symbols
    AllFoundSymbols <- AllSymbols[FoundAllSymbols]
    if(is.null(Symbols)) AllFoundSymbols <- AllSymbols

    plyr::llply(AllFoundSymbols, function(x) {
      xx <- get(x, source.envir, inherits = FALSE)
      updated <- NULL
      if((class(xx)[1] == "xts")) {
        updated  <- xtsAttributes(xx)[["updated"]]
      } else if (!is.null(attributes(xx)[["updated"]])) {
        updated  <- attributes(xx)[["updated"]]
      }
      namesx <- x
      if(!is.null(updated)) { x <- updated } else { x <- NA_real_ }
      Names(x) <- namesx
      EnvSymbols <- c(EnvSymbols, as.list(x))
      assign("EnvSymbols", EnvSymbols, envir =  runenv)
      invisible()
    })}
  }

  SrcSymbols <- list()
  if("src" %in% names(Dots)) {
    SrcSymbols <- do.call(paste0("updatedSymbols",".", Dots[["src"]]), c(list(),
                    Symbols = Symbols, source.envir = source.envir, Dots[!names(Dots) %in% "src"]))
   }
  c(list(),EnvSymbols, SrcSymbols)

})}



#' 'updated' property of xts object symbols in the cache
#'
#' @param Symbols	a character vector specifying the names of each symbol
#' @param source.envir location of xts objects
#' @return a named list of 'updated' properties
#' @export
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
updatedSymbols.cache <- function(Symbols = NULL, cache.envir = NULL, ...) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})
  importDefaults("updatedSymbols.cache")

  if(is.null(cache.envir)) cache.envir <- .GlobalEnv

  AllSymbols <- updatedSymbols(source.envir = cache.envir)

  LessAllSymbols   <- AllSymbols[stringr::str_detect(Names(AllSymbols),"^[.].+") &
                                !stringr::str_detect(Names(AllSymbols),"^[.]Random[.]seed$")]
  Names(LessAllSymbols) <- stringr::str_replace(Names(LessAllSymbols), "^[.]","")
  RetrievedSymbols <- LessAllSymbols
  RetrievedSymbols

})}



#' 'updated' property of symbols in the PostgreSQL (pg) database
#'
#' @param Symbols	a character vector specifying the names of each symbol
#' @param con PostgreSQL DBI connection
#' @param user username (default "Symbols") to access database
#' @param password password (default "Symbols") to access database
#' @param dbname database name (default "Symbols")
#' @param schname schema name (default "Symbols")
#' @param host database host (default "localhost")
#' @param port database port (default 5432)
#' @param options pass extra parameters in a string to the command line
#' @param forceISOdate TRUE(default)/FALSE if the communication of date (time stamp) from PostgreSQL
#' is forced to ISO style at conection
#' @param ... passed unused
#' @return named list of the object 'updated' property
#' @export
#' @importFrom DBI dbGetQuery dbQuoteString dbQuoteIdentifier
updatedSymbols.PostgreSQL <- function(Symbols = NULL, con = NULL,
                               user=NULL,password=NULL,dbname=NULL,schname = NULL,host='localhost',port=5432,
                               options = NULL, forceISOdate = TRUE,
                               ...) {

tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})
  importDefaults("updatedSymbols.PostgreSQL")

  schnamePassed <- schname

  if(is.null(con)){
    DBConMeta <- pgConnect(user=user,password=password,dbname=dbname,schname=schname,host=host,port=port,options=options,forceISOdate=forceISOdate)
    AssignEnv(DBConMeta, c("con"))
  }
  schname <- pgCurrentSchema(con)[[1]]
  if(!is.null(schnamePassed) && schnamePassed != schname) schname <- schnamePassed

  RetrievedSymbols <- listSymbols(con, src = "pg")
  FoundSymbols <- RetrievedSymbols %in% Symbols
  DBSymbols    <- RetrievedSymbols[FoundSymbols]
  if(is.null(Symbols)) DBSymbols <- RetrievedSymbols

  Updateds <- list()
  for(Symbol in DBSymbols) {
    updated <- DBI::dbGetQuery(con, paste0(
      "SELECT ", DBI::dbQuoteIdentifier(con, "updated"),
      " FROM ", DBI::dbQuoteIdentifier(con, schname), ".", DBI::dbQuoteIdentifier(con, "Symbols"),
      " WHERE ", DBI::dbQuoteIdentifier(con, "Symbols"), " = ", DBI::dbQuoteString(con, Symbol), ";"))
    updated <- updated[[1]]
    Names(updated)[1] <- Symbol
    Updateds <- c(Updateds, list(updated))
  }
  Updateds

})}
#' updatedSymbols pg
#'
#'@export
updatedSymbols.pg <- updatedSymbols.PostgreSQL



#' object symbols in a persistent location (dispatcher)
#'
#' looks in source.envir xor "src" exclusively
#'
#' @param source.envir source location of Symbols
#' xts objects must have the attribute "src"
#' @param src source program to look for symbols
#' @param ... passed to src
#' @return character vector of Symbols
#' @examples
#' \dontrun{
#'
#' # Symbols must be case-insenstive unique
#'
#' getSymbols("IBM", src = "yahoo") # auto.assign = TRUE
#'
#' # Symbols names found in Symbols and the names of xts objects stored
#' # in source.envir, must be mutually exclusive ( case in-sensensitive match)
#' # (because list2env silently drops repeated members)
#'
#' # save just the Symbol "IBM"
#' saveSymbols("IBM", trg = "pg")
#'
#' msft <- getSymbols("MSFT", src = "yahoo", auto.assign = FALSE)
#' source.envir = list2env(list(MSFT = msft))
#' listSymbols(source.envir = source.envir)
#' existSymbols(Symbols = "MSFT", source.envir = source.envir)
#'
#' saveSymbols(trg = "cache", source.envir = source.envir)
#' listSymbols(src = "cache")
#' existSymbols(Symbols = "MSFT", src = "cache")
#'
#' # save all of the .getSymbols Symbols and the source.envir Symbols
#' saveSymbols(trg = "pg", source.envir = source.envir)
#' listSymbols(src = "pg")
#' existSymbols(c("MSFT","IBM"), src = "pg")
#'
#' }
#' @export
#' @importFrom plyr llply
listSymbols <- function(source.envir = NULL, ...) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})
  importDefaults("listSymbols")

  Dots <- list(...)

  EnvSymbols <- c()
  if(!"src" %in% names(Dots)) {
    runenv <- environment()
    # look in my custom environment
    if(is.environment(source.envir)) {
    plyr::llply(ls(envir = source.envir, all.names = TRUE), function(x) {
      xx <- get(x, source.envir, inherits = FALSE)
      if((class(xx))[1] %in% c("zoo","xts", "data.frame","ts","timeSeries")) {
        EnvSymbols <- c(EnvSymbols, x)
      }
    })}
  }

  SrcSymbols <- c()
  if("src" %in% names(Dots)) {
    SrcSymbols <- do.call(paste0("listSymbols",".", Dots[["src"]]), c(list(),
                    source.envir = source.envir, Dots[!names(Dots) %in% "src"]))
  }
  c(EnvSymbols, SrcSymbols)

})}



#' xts object symbols in the cache
#'
#' @param source.envir location of xts objects
#' @param ... passed unused
#' @return character vector of Symbols
#' @export
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
listSymbols.cache <- function(cache.envir = NULL, ...) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})
  importDefaults("listSymbols.cache")

  if(is.null(cache.envir)) cache.envir <- .GlobalEnv

  AllSymbols       <- ls(envir = cache.envir, all.names = TRUE)
  LessAllSymbols   <- AllSymbols[stringr::str_detect(AllSymbols,"^[.].+") &
                                !stringr::str_detect(AllSymbols,"^[.]Random[.]seed$")]
  RetrievedSymbols <- stringr::str_replace(LessAllSymbols, "^[.]","")
  RetrievedSymbols

})}



#' xts object symbols in the PostgreSQL (pg) database
#'
#' @param con PostgreSQL DBI connection
#' @param user username (default "Symbols") to access database
#' @param password password (default "Symbols") to access database
#' @param dbname database name (default "Symbols")
#' @param schname schema name (default "Symbols")
#' @param host database host (default "localhost")
#' @param port database port (default 5432)
#' @param options pass extra parameters in a string to the command line
#' @param forceISOdate TRUE(default)/FALSE if the communication of date (time stamp) from PostgreSQL
#' is forced to ISO style at conection
#' @param ... passed unused
#' @return character vector of Symbols
#' @export
listSymbols.PostgreSQL <- function(con = NULL,
                               user=NULL,password=NULL,dbname=NULL,schname = NULL,host='localhost',port=5432,
                               options = NULL, forceISOdate = TRUE,
                               ...) {

tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})
  importDefaults("listSymbols.PostgreSQL")

  schnamePassed <- schname

  if(is.null(con)) {
    DBConMeta <- pgConnect(user=user,password=password,dbname=dbname,schname=schname,host=host,port=port,options=options,forceISOdate=forceISOdate)
    AssignEnv(DBConMeta, c("con"))
  }
  schname <- pgCurrentSchema(con)[[1]]
  if(!is.null(schnamePassed) && schnamePassed != schname) schname <- schnamePassed

  SchemaTables <- pgListSchemaTables(con = con, schname = schname)
  SchemaTables <- SchemaTables[!SchemaTables %in% "Symbols"]
  SchemaTables

})}
#' listSymbols pg
#'
#'@export
listSymbols.pg <- listSymbols.PostgreSQL







#' xts object symbols
#'
#'looks in source.envir xor "src" exclusively
#'
#' @param Symbols	a character vector specifying the names of each symbol
#' @param source.envir location of xts objects
#' @param src source program to look for symbols
#' @param ... passed to src
#' @return R named list of Symbols and results (TRUE/FALSE)
#' @export
existSymbols <- function(Symbols = NULL, source.envir = NULL, ...) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})
  importDefaults("existSymbols")

  Dots <- list(...)

  EnvSymbols    <- character(0)
  NotEnvSymbols <- character(0)
  if(!"src" %in% names(Dots)) {
    if(is.environment(source.envir)) {

      RetrievedSymbols <- listSymbols(source.envir = source.envir)
      if(is.null(RetrievedSymbols))  RetrievedSymbols <- character(0)

      if(!is.null(Symbols)) {
        Results <-        Symbols %in% RetrievedSymbols
        Names(Results) <- Symbols
      } else {
        Results <- rep(TRUE, length(RetrievedSymbols))
        Names(Results) <- RetrievedSymbols
      }
      # because a named vector (of booleans) gets upgraded to character.
      return(as.list(Results))
    }
  }

  SrcSymbols <- character(0)
  if("src" %in% names(Dots)) {
    Results <- do.call(paste0("existSymbols",".", Dots[["src"]]), c(list(),
                    Symbols = Symbols, source.envir =  source.envir, Dots[!names(Dots) %in% "src"]))
    return(Results)
  }

})}



#' xts object symbols
#'
#' @param Symbols	a character vector specifying the names of each symbol
#' @param source.envir location of xts objects
#' @param ... passed unused
#' @return R named list of Symbols and results (TRUE/FALSE)
#' @export
existSymbols.cache <- function(Symbols = NULL, cache.envir = NULL,  ...) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})
  importDefaults("existSymbols.cache")

  if(is.null(cache.envir)) cache.envir <- .GlobalEnv

  if(is.environment(cache.envir)) {
    AllSymbols <- listSymbols(cache.envir = cache.envir, src = "cache")
    if(!is.null(AllSymbols)) {
      RetrievedSymbols <- AllSymbols
    } else {
      RetrievedSymbols <- character(0)
    }

    if(!is.null(Symbols)) {
      Results <-        Symbols %in% RetrievedSymbols
      Names(Results) <- Symbols
    } else {
      Results <- rep(TRUE, length(RetrievedSymbols))
      Names(Results) <- RetrievedSymbols
    }
    # because a named vector (of booleans) gets upgraded to character.
    return(as.list(Results))

  }

})}



#' xts object symbols exist in the PostgreSQL (pg) database
#' @param Symbols	a character vector specifying the names of each symbol
#' @param con PostgreSQL DBI connection
#' @param user username (default "Symbols") to access database
#' @param password password (default "Symbols") to access database
#' @param dbname database name (default "Symbols")
#' @param schname schema name (default "Symbols")
#' @param host database host (default "localhost")
#' @param port database port (default 5432)
#' @param options pass extra parameters in a string to the command line
#' @param forceISOdate TRUE(default)/FALSE if the communication of date (time stamp) from PostgreSQL
#' is forced to ISO style at conection
#' @param ... passed unused
#' @return R named list of Symbols and results (TRUE/FALSE)
#' @export
existSymbols.PostgreSQL <- function(Symbols = NULL, con = NULL,
                               user=NULL,password=NULL,dbname=NULL,schname = NULL,host='localhost',port=5432,
                               options = NULL, forceISOdate = TRUE,
                               ...) {

tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})
  importDefaults("existSymbols.PostgreSQL")

  schnamePassed <- schname

  if(is.null(con)) {
    DBConMeta <- pgConnect(user=user,password=password,dbname=dbname,
                           schname=schname,host=host,port=port,options=options,forceISOdate=forceISOdate)
    AssignEnv(DBConMeta, c("con"))
  }
  schname <- pgCurrentSchema(con)[[1]]
  if(!is.null(schnamePassed) && schnamePassed != schname) schname <- schnamePassed

  # src = "pg"
  RetrievedSymbols <- listSymbols(con, user=user,password=password,dbname=dbname,
                      schname=schname,host=host,port=port,options=options,
                      forceISOdate=forceISOdate, src = "pg")
  if(is.null(RetrievedSymbols)) RetrievedSymbols <- character(0)

  if(!is.null(Symbols)) {
    Results <-        Symbols %in% RetrievedSymbols
    Names(Results) <- Symbols
  } else {
    Results <- rep(TRUE, length(RetrievedSymbols))
    Names(Results) <- RetrievedSymbols
  }
  # because a named vector (of booleans) gets upgraded to character.
  return(as.list(Results))

})}
#' existSymbols pg
#'
#'@export
existSymbols.pg <- existSymbols.PostgreSQL



#' of a specific PostgreSQL database schema table show it's last date/time index value
#'
#' @param con PostgreSQL DBI connection
#' @param schname schema name
#' @param tblname table name
#' @return  table last date/time index value
#' The results do not have any order.
#' @export
#' @importFrom DBI dbGetQuery dbQuoteIdentifier
pgSchemaTableLastIndex <- function(con, schname = NULL, tblname) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  if(!is.null(con)) {
    schname <- pgCurrentSearchPath(con)[["CurrentSearchPath"]][1]
  }
  if(is.null(schname)) schname <- "Symbols"

    DBI::dbGetQuery(con,
      paste0(
       "
        SELECT -- NOTE if the primary key has multiple columns then take the leftmost column
              max(", DBI::dbQuoteIdentifier(con, pgListSchemaTablePrimaryKeyColumns(schname, tblname))[1], ")
        FROM
            ", DBI::dbQuoteIdentifier(con, schname),".", DBI::dbQuoteIdentifier(con, tblname), "
        ;
      "
      )
    ) -> db.Query.result
    as.POSIXct(db.Query.result[["max"]])

})}


