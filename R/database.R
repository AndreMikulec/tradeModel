



#' saves xts object symbols to a persistent location (dispatcher)
#'
#' @param Symbols	a character vector specifying the names of each symbol to be loaded
#' @param envi location of xts objects downloaded with getSymbols("XXX", src = 'yahoo')
#' @param source.envir source location of Symbols
#' @param ... pass through parameters
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
#' # save only the source.envir Symbols
#' saveSymbols(Symbols = "", trg = "pg", source.envir = source.envir)
#'
#' # save all of the .getSymbols Symbols and the source.envir Symbols
#' saveSymbols(trg = "pg", source.envir = source.envir)
#'
#' # save all to a file
#' # saveSymbols(source.envir = source.envir, file.path = "C:\\Users\\Public")
#'
#' }
#' @export
saveSymbols <- function(Symbols = NULL, envi = parent.frame(), source.envir = NULL, ...) {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})

  # case insensitive

  SymbolsInsource.envir <- Symbols[tolower(Symbols) %in% tolower(names(as.list(source.envir)))]
  if(length(SymbolsInsource.envir)) stop(paste0("Symbols ", paste0(SymbolsInsource.envir, collapse = ", "), " found in source.envir"))

  source.envirInSymbols <- names(as.list(source.envir))[tolower(names(as.list(source.envir))) %in% tolower(Symbols)]
  if(length(source.envirInSymbols)) stop(paste0("source.envir ", paste0(source.envirInSymbols, collapse = ", "), " found in Symbols"))

  xTsGetSymbols <- list()
  DotgetSymbolsFound <- FALSE
  if (exists(".getSymbols", envi, inherits = FALSE)) {
    DotgetSymbols <- get(".getSymbols", envi, inherits = FALSE)
    Inherits <- FALSE
    DotgetSymbolsFound <- TRUE
  }
  # XXX NOT NEEDED anymore 'env' was actually re-assigned by InitEnv XXX
  # tryCatchLog::tryCatchLog will give wrong "env = parent.frame()"
  # but: exists(".getSymbols", parent.frame(), inherits = FALSE) # is the correct parent.frame()
  else if (exists(".getSymbols", envi, inherits = TRUE)) {
    DotgetSymbols <- get(".getSymbols", envi, inherits = TRUE)
    Inherits <- TRUE
    DotgetSymbolsFound <- TRUE
  }

  if(DotgetSymbolsFound) {
    if (is.null(Symbols)) {
      # get all of the Symbols
      Symbols <- names(DotgetSymbols)
    }
    else {
      Symbols <- Symbols[Symbols %in% names(DotgetSymbols)]
    }
    for (each.symbol in Symbols) {
      if(!each.symbol %in% names(xTsGetSymbols )){
        xTsGetSymbols <- c(xTsGetSymbols, list(get(each.symbol, envir = envi, inherits = Inherits)))
        names(xTsGetSymbols)[length(xTsGetSymbols)] <- each.symbol
      }
    }
  }

  runenv <- environment()
  # look in my custom environment
  if(is.environment(source.envir)) {
  llply(ls(envir = source.envir), function(x) {
    xx <- get(x, source.envir, inherits = FALSE)
    if((class(xx)[1] == "xts") && ("src" %in% names(xtsAttributes(xx)))) {
      if(!x %in% names(xTsGetSymbols )){
         xTsGetSymbols <- c(xTsGetSymbols, list(xx))
         names(xTsGetSymbols)[length(xTsGetSymbols)] <- x
         assign("xTsGetSymbols", xTsGetSymbols, envir = runenv)
       }
    }
  })}

  # save everything in my custom environment
  if(is.environment(source.envir))
    # note list2env SILENTYLY a symbols of the same name AND in a DIFFERENT case
    source.envir <- list2env(xTsGetSymbols, parent = emptyenv())

  Dots <- list(...)
  # Symbols are no longer passed
  # because all 'Symbols' are in the source.envir

  if("file.path" %in% names(Dots))
    do.call(saveSymbols.RData, c(list(), source.envir = source.envir, Dots))

  if("trg" %in% names(Dots))
    do.call(paste0("saveSymbols",".", Dots[["trg"]]), c(list(), source.envir = source.envir, Dots[!names(Dots) %in% "trg"]))

  invisible()

})}



#' saves xts object symbols to a persistent location (disk: Symbol.RData file)
#'
#' see quantmod SaveSymbols
#'
#' @param file.path  character string of file (Symbol.Rdata) location
#' @param source.envir source location of Symbols
#' @param ... pass through parameters
#' @export
saveSymbols.RData <- function (file.path = stop("must specify 'file.path'"), source.envir = NULL, ...) {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})

  for(each.symbol in names(as.list(source.envir))) {
     save(list = each.symbol, file = paste(file.path, "/", each.symbol, ".RData", sep = ""), envir = source.envir)
  }
  invisible()

})}



#' convert an OHLC xTs into aN OHLC data.frame MEANT to be loaded into a database
#'
#' @param xTs OHLC[V][A] object
#' @param con DBI database connection
#' @param field.names R column names
#' @param db.fields database column names
#' @export
xTs2DBDF <- function(xTs, con, field.names, db.fields) {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})

  df <- cbind(date = index(xTs), as.data.frame(xTs, stringsAsFactors = FALSE))

  # OHLC columns
  str_replace_all(colnames(df), "[.]", "_")  -> colnames(df)
  for(grouping in list.zip(field.names, db.fields)){
     colnames(df)[str_detect(tolower(colnames(df)), str_c("_", tolower(grouping[["field.names"]]),"$"))] <- grouping[["db.fields"]]
  }
  # all other column names "as is" (e.g. FRED)

  df

})}



#' from column names and datatypes, make a CREATE TABLE statement
#'
#' @param df data.frame (with column names)
#' @param con DBI database connection
#' @export
DBDF2CREATETableStmt <- function(df, con, Symbol, schname) {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})

  # column datatypes
  colClasses  <- do.call(c,llply(df, function(x) class(x)))
  colClasses[colClasses == "numeric"] <- "NUMERIC(14,3)"

  # upon creation, do Quote Once:  (1)schema, (2)table and (3)column names
  # the PostgreSQL storage will be: anything_capilized retains it's "" quotes.

  if(schname != "") { dotSchemaQuoted <- paste0(dbQuoteIdentifier(con, schname), ".") } else { dotSchemaQuoted <- "" }
  schemaSymbolsQuoted <-  paste0(dotSchemaQuoted, dbQuoteIdentifier(con, Symbol))

  ddl <- paste0("CREATE TABLE ", schemaSymbolsQuoted ,"(", paste0( dbQuoteIdentifier(con, names(colClasses)), " ", colClasses, collapse = ", "), ");")
  ddl

})}


#' extract a list of R objects and assign to an environment
#'
#' mostly for 'space saving' and cleanliness
#'
#' @param List collection of R objects
#' @param nms names of List object (default: everything)
#' @param environment to assign R objects
#' @example
#' @return invisible
#' @examples
#' \dontrun{
#' List <- list(a=1, b=2)
#' # a, b
#' AssignEnv(List, c("a","b"))
#' }
#' @export
AssignEnv <- function(List, nms = NULL, envir = parent.frame()) {
tryCatchLog::tryCatchLog({
  for(nm in names(List)){
    # default everything
    if(!is.null(nms)) nm <- nm[nm %in% nms]
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
pgConnect <- function(user=NULL,password=NULL,dbname=NULL,schname=NULL,
                      host=NULL,port=NULL,options=NULL,forceISOdate=NULL) {
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
  pgSetCurrentSearchPath(con, dbQuoteIdentifier(con, schname))
  list(con=con,user=user,password=password,dbname=dbname,schname=schname)

})}



#' of a specific PostgreSQL database schema, show its tables
#'
#' @param con PostgreSQL DBI connection
#' @param schema name
#' @return vector of characters of table names
#' The results do not have any order.
#' @export
pgListSchemaTables <- function(con, schname) {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})

    dbGetQuery(con,
      paste0(
       "
        SELECT
              table_catalog -- database
            , table_schema
            , table_name
        FROM
            information_schema.tables
        WHERE
            table_schema     IN (", dbQuoteLiteral(con, schname), ")
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
pgListSchemaTableColumns <- function(con, schname, tblname) {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})

    dbGetQuery(con,
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
            table_schema     IN (", dbQuoteLiteral(con, schname), ") AND
            table_name       IN (", dbQuoteLiteral(con, tblname), ")
        ORDER BY ordinal_position
        ;
      "
      )
    ) -> db.Schema.tbl
    db.Schema.tbl[["column_name"]]

})}



#' saves xts object symbols to a persistent location (database: PostgreSQL)
#'
#' CREATE ROLE "Symbols" LOGIN
#'   NOSUPERUSER INHERIT NOCREATEDB NOCREATEROLE NOREPLICATION;
#'
#' ALTER USER Symbols PASSWORD 'Symbols';
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
#' }
#' @export
saveSymbols.PostgreSQL <- function(source.envir = NULL,
  field.names = c('Open','High','Low','Close','Volume','Adjusted'),
  db.fields=c('o','h','l','c','v','a'),
  user=NULL,password=NULL,dbname=NULL,schname=NULL,host='localhost',port=5432,options=NULL, forceISOdate = TRUE,
  ...)  {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})
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

  DBConMeta <- pgConnect(user=user,password=password,dbname=dbname,schname=schname,host=host,port=port,options=options,forceISOdate=forceISOdate)
  AssignEnv(DBConMeta, c("con", "user", "password", "dbname", "schname"))

  # now, I am only getting symbols from here
  Symbols <- names(as.list(source.envir))

  db.Symbols <- pgListSchemaTables(con, schname)

  if(length(Symbols) != sum(Symbols %in% db.Symbols)) {
    missing.db.symbol <- Symbols[!Symbols %in% db.Symbols]
    # for the requested Symbol, if I do not have database table so, I have to create ONE

    # create *new* empty TABLEs
    dfs <- list()
    new.db.symbol <- c()
    for (each.symbol in  missing.db.symbol) {

      xTs <- as.list(source.envir)[[each.symbol]]
      if(is.null(xTs)) { message(paste("Symbol ", each.symbol, " was not found, so skipping.")); next }
      df  <- xTs2DBDF(xTs, con, field.names = field.names[-1], db.fields = db.fields[-1])

      # create NEW CREATE TABLE statements"
      # column names
      ddl <- DBDF2CREATETableStmt(df, con, Symbol, schname)
      dbExecute(con, ddl)

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

    if(schname != "") { dotSchemaQuoted <- paste0(dbQuoteIdentifier(con, schname), ".") } else { dotSchemaQuoted <- "" }
    dbExecute(con, paste0("TRUNCATE TABLE ", dotSchemaQuoted, dbQuoteIdentifier(con, each.symbol), ";"))

    # Goal
    # exist in  R       'Date', 'Open','High','Low','Close','Volume','Adjusted'( along with FRED columns )
    # translate from those
    # to DB: 'date', 'o','h','l','c','v','a' ( along with FRED columns "as is")

    # custom sorting
    db.fields    <- customSorting( colnames(df), InitOrder = db.fields, CI = TRUE)
    colnames(df) <- db.fields

    dbWriteTable(con, each.symbol, df, append = T, row.names = F)
                     # if  each.symbol includes schema, will say TRUE, but will lie
    dbDisconnect(con)
  }
  invisible()

})}
#' saveSymbols pg
#'
#'@export
saveSymbols.pg <- saveSymbols.PostgreSQL



#' returns One column from a database (con)
#'
#' @param con DBI database connection
#' @param Query SQL expecting output to be a single value,
#' no value, or a single value with with comma separted internal parts
#' @param outName the new column name
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
#' pgCurrentTempSchema <- oneColumn(con, "SELECT nspname FROM pg_namespace WHERE oid = pg_my_temp_schema();", "CurrentTempSchema")
#'
#' }
#' @export
oneColumn <- function(con, Query, outName) {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})
  res <- dbGetQuery(con, Query)
  if(NROW(res)) {
    res <- res[[1]]
    res %>% strsplit(", ") %>%
      {identity(.)[[1]]} %>% as.data.frame(stringsAsFactors = F) %>%
        {colnames(.)[1] <- outName; .} -> ret
    return(ret)
  } else {
    L <- list()
    L[[outName]] <- character()
    ret <- as.data.frame(L, stringsAsFactors = F)
    return(ret)
  }

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
pgCurrentSearchPath <- function(con) { oneColumn(con, "SHOW SEARCH_PATH;", "CurrentSearchPath") }


#' set PostgreSQL current user search path
#'
#' @rdname pgCurrent
#' @export
pgSetCurrentSearchPath <- function(con, path) { dbExecute(con, paste0("SET SEARCH_PATH TO ", path,";")) }



#' get PostgreSQL current user temp schema
#'
#' @rdname pgCurrent
#' @export
pgCurrentTempSchema <- function(con) { oneColumn(con, "SELECT nspname FROM pg_namespace WHERE oid = pg_my_temp_schema();", "CurrentTempSchema") }

#' custom sort a vector
#'
#' excess is appended to the end ( CI sort or CS sort )
#'
#' @param Vector vector to be sorted
#' @param InitOrder starting custom sorting ( without the excess )
#' @param CI case insensitive
#' @examples
#' \dontrun {
#' customSorting( c("a","v", "E2", "c","l", "e3" ,"h","o","date"), InitOrder = c("date", "o", "h", "l", "c", "v", "a"), CI = TRUE )
#  # [1] "date" "o"    "h"    "l"    "c"    "v"    "a"    "E2"   "e3"
#' }
#' @export
customSorting <- function(Vector, InitOrder, CI = FALSE) {

  # Custom Sorting in R
  # 2014
  # https://stackoverflow.com/questions/23995285/custom-sorting-in-r
  # AND
  # Case insensitive sort of vector of string in R
  # 2015
  # https://stackoverflow.com/questions/29890303/case-insensitive-sort-of-vector-of-string-in-r

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
#' ## YEAR 2018 EXAMPLES
#'
#' ibm <- getSymbols("IBM", src = "pg", auto.assign =  F)
#'
#' unrate <- getSymbols("UNRATE", src = "FRED", auto.assign =  F)
#' saveSymbols(Symbols = "", trg = "pg", source.envir = list2env(list(UNRATE = unrate)))
#' unrate.db <- getSymbols("UNRATE", src = "pg", auto.assign =  F)
#' unrate.db <- getSymbols(Symbols = "UNRATE", src = "pg", auto.assign = F)
#'
#' }
#' @export
getSymbols.PostgreSQL <- function(Symbols = NULL, env, return.class = 'xts',
                               db.fields=c('o','h','l','c','v','a'),
                               field.names = c('Open','High','Low','Close','Volume','Adjusted'),
                               user=NULL,password=NULL,dbname=NULL,schname = NULL,host='localhost',port=5432,
                               options = NULL, forceISOdate = TRUE,
                               ...) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})
  # NOTE env IS BROKEN,because initEnv is overriding it
  # NOTE env IS BROKEN,because initEnv is overriding it
  importDefaults("getSymbols.PostgreSQL")
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

  DBConMeta <- pgConnect(user=user,password=password,dbname=dbname,schname=schname,host=host,port=port,options=options,forceISOdate=forceISOdate)
  AssignEnv(DBConMeta, c("con", "user", "password", "dbname", "schname"))

  db.Symbols <- pgListSchemaTables(con, schname)
  if(length(Symbols) != sum(Symbols %in% db.Symbols)) {
    missing.db.symbol <- Symbols[!Symbols %in% db.Symbols]
    warning(paste('could not load symbol(s): ',paste(missing.db.symbol,collapse=', ')))
    Symbols <- Symbols[Symbols %in% db.Symbols]
  }

  if(schname != "") { dotSchemaQuoted <- paste0(dbQuoteIdentifier(con, schname), ".") } else { dotSchemaQuoted <- "" }

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
    field.names  <- customSorting(field.names, InitOrder = c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted"), CI = TRUE)

    db.fieldsQuoted  <-  dbQuoteIdentifier(con, db.fields)
    schemaSymbolsQuoted[[i]] <-  paste0(dotSchemaQuoted, dbQuoteIdentifier(con, Symbols[[i]]))

    if(sum(c("o", "h", "l", "c") %in% db.fields) == 4) {
      Selection <- paste( db.fieldsQuoted , collapse=',')
      OHLCData <- TRUE
    } else {
      # NO MATCHES, THEN MAYBE A "SINGLE COLUMN (FRED) DATA"
      Selection <- "*"
      OHLCData <- FALSE
    }
    # ABOVE (^) ALREADY QUOTED
    query <- paste("SELECT ", Selection," FROM ", schemaSymbolsQuoted[[i]]," ORDER BY ",dbQuoteIdentifier(con, "date"))
    rs <- DBI::dbSendQuery(con, query)
    fr <- DBI::fetch(rs, n=-1)
    DBI::dbDisconnect(con)
    #fr <- data.frame(fr[,-1],row.names=fr[,1])
    fr <- xts(as.matrix(fr[,-1]),
              order.by=as.Date(fr[,1],origin='1970-01-01'),
              src=dbname,updated=Sys.time())
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
  dbDisconnect(con)
  if(auto.assign)
    return(Symbols)
  return(fr)
})}
#' getSymbols pg
#'
#'@export
getSymbols.pg <- getSymbols.PostgreSQL

