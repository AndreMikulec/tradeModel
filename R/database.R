




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
#' also register its meta-data
#'
#' @param df data.frame (with column names)
#' @param con DBI database connection
#' @export
dfToCREATETable <- function(df, con, Symbol, schname) {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})


  # meta-data table
  if(!"Symbols" %in% pgListSchemaTables(con, "Symbols")) {
    ddl <- paste0("CREATE TABLE ", dbQuoteIdentifier(con, schname), ".", dbQuoteIdentifier(con, "Symbols"), "(",
                            dbQuoteIdentifier(con, "Symbols"),          " TEXT ", ", ",
                            dbQuoteIdentifier(con, "updated"),          " TIMESTAMP WITH TIMEZONE ", ", ",
                            dbQuoteIdentifier(con, "updated_R_class") , " TEXT ", ", ",
                            dbQuoteIdentifier(con, "src"),              " TEXT " ,
                          ");")
    dbExecute(con, ddl)

    ddl <- paste0("ALTER TABLE ", dbQuoteIdentifier(con, schname), ".", dbQuoteIdentifier(con, "Symbols"),
                          " ADD PRIMARY KEY ( ", dbQuoteIdentifier(con, "Symbols"), ")",
                          ";")
    dbExecute(con, ddl)
  }
  # upon creation, do Quote Once:  (1)schema, (2)table and (3)column names
  # the PostgreSQL storage will be: anything_capilized retains it's "" quotes.

  if(schname != "") { dotSchemaQuoted <- paste0(dbQuoteIdentifier(con, schname), ".") } else { dotSchemaQuoted <- "" }
  schemaSymbolsQuoted <-  paste0(dotSchemaQuoted, dbQuoteIdentifier(con, Symbol))

  # column datatypes
  colClasses  <- do.call(llply(df, function(x) class(x)[1]))
  colClasses[colClasses     == "numeric"]    <- "NUMERIC(14,3)"
  colClasses[colClasses %in%   "Date"]       <- "DATE"
  colClasses[colClasses %in%   "POSIXct"]    <- "TIMESTAMP WITH TIMEZONE"
  # ACTUALLY I HAVE NO EXPERIENCE ( THIS IS AN EDUCATED WILD GUESS: LATER, I WILL EXPERIMENT/TEST/FIX THIS )
  # xts OTHER supported index date/time classes
  colClasses[colClasses %in% c("chron", "yearmon", "yearqtr", "timeDate")] <- "TIMESTAMP WITH TIMEZONE"

  ddl <- paste0("CREATE TABLE ", schemaSymbolsQuoted ,"(", paste0( dbQuoteIdentifier(con, names(colClasses)), " ", colClasses, collapse = ", "), ");")
  dbExecute(con, ddl)
  ddl <- paste0("ALTER TABLE ", schemaSymbolsQuoted,
                " ADD PRIMARY KEY ( ", dbQuoteIdentifier(con, names(colClasses)[1]), ")",
                ";")
  dbExecute(con, ddl)
  dml <- paste0("INSERT INTO ", dbQuoteIdentifier(con, schname), ".", dbQuoteIdentifier(con, "Symbols"), "(", dbQuoteIdentifier(con, "Symbols"), ") VALUES (", dbQuoteString(con, Symbol), ");")
  dbExecute(con, dml)
  invisible()

})}


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
AssignEnv <- function(List, nms = NULL, envir = parent.frame()) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})
  # default everything
  if(!is.null(nms)) {
    nmsINnames <- nms %in% names(List)
    nms <-    nms[nmsINnames]
    if(any(!nmsINnames)) warning(str_c("AssignEnv is asking for R objects that are not found: ", nms[!nmsINnames], collapse = ", "))
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
  dbExecute(con, "SET TIME ZONE 'UTC';")
  pgSetCurrentSearchPath(con, dbQuoteIdentifier(con, schname))
  list(con=con,user=user,password=password,dbname=dbname,schname=schname)

})}



#' of a specific PostgreSQL database schema, show its tables
#'
#' @param con PostgreSQL DBI connection
#' @param schname schema name
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



#' of a specific PostgreSQL database schema table, show its primary key columns
#'
#' @param con PostgreSQL DBI connection
#' @param schema name
#' @param table name
#' @return vector of characters of primary key column names
#' The results are ordered.
#' @export
pgListSchemaTablePrimaryKeyColumns <- function(con, schname, tblname) {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})

    # List primary keys for all tables - Postgresql
    #  https://dba.stackexchange.com/questions/11032/list-primary-keys-for-all-tables-postgresql

    dbGetQuery(con,

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
            tc.table_schema     IN (", dbQuoteLiteral(con, schname), ") AND
            tc.table_name       IN (", dbQuoteLiteral(con, tblname), ") AND
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
#' pgCurrentTempSchema <- oneColumn(con, "SELECT nspname FROM pg_namespace WHERE oid = pg_my_temp_schema();", "CurrentTempSchema")
#'
#' }
#' @export
oneColumn <- function(con, Query, outName, unQuote = NULL) {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})
  if(is.null(unQuote)) unQuote = FALSE
  res <- dbGetQuery(con, Query)
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
      llply(function(x) { strsplit(x[[1]], "^\"|\"$")[[1]][2] }) %>%
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
pgSetCurrentSearchPath <- function(con, path) { dbExecute(con, paste0("SET SEARCH_PATH TO ", path,";")) }



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
#' customSorting( c("a","v", "E2", "c","l", "e3" ,"h","o","date"), InitOrder = c("date", "o", "h", "l", "c", "v", "a"), CI = TRUE )
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



#' Downloads Symbols to specified env from a local R environment
#'
#' @param Symbols	a character vector specifying the names of each symbol to be loaded
#' @param env	where to create objects. (.GlobalEnv)
#' @param return.class	class of returned object
#' @param source.envir where to find xts objects ( location of cache )
#' @param ...	additional parameters
#' @return A call to getSymbols.csv will load into the specified environment one object for each Symbol specified, with class defined by return.class. Presently this may be ts, zoo, xts, data.frame, or timeSeries
#' @examples
#' \dontrun{
#' msft <- getSymbols("MSFT", src = "yahoo", auto.assign = FALSE)
#' source.envir = list2env(list(MSFT = msft))
#' saveSymbols(Symbols = "", trg = "cache", source.envir = source.envir)
#' res <- getSymbols(Symbols = "MSFT", src = "cache", auto.assign = F)
#' }
#' @export
getSymbols.cache <- function (Symbols = NULL, source.envir = NULL, env, return.class = "xts", ...) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})
    importDefaults("getSymbols.cache")
    this.env <- environment()
    for (var in names(list(...))) {
        assign(var, list(...)[[var]], this.env)
    }
    if(is.null(source.envir)) source.envir <- .GlobalEnv
    default.return.class <- return.class
    if (!hasArg("verbose"))
        verbose <- FALSE
    if (!hasArg("auto.assign"))
        auto.assign <- TRUE

    AllSymbols       <- ls(envir = source.envir, all.names = TRUE)
    RetrievedSymbols <- AllSymbols[str_detect(AllSymbols,"^[.].+")]
    FoundSymbols     <- RetrievedSymbols %in% paste0(".",Symbols)
    EnvSymbols       <- RetrievedSymbols[FoundSymbols]
    Symbols          <- EnvSymbols
    if(is.null(match.arg(Symbols))) Symbols <- RetrievedSymbols

    # ORIGINAL quantmod-ism FOLLOWS ... [ ] could be cleaned up

    for (i in seq_along(Symbols)) {
        return.class <- default.return.class
        if (verbose)
            cat("loading ", Symbols[[i]], ".....")
        if (!paste0(".", Symbols[[i]]) %in% RetrievedSymbols) {
            cat("\n", Symbols[[i]]," does not exist ", "....skipping\n")
            next
        }
        fr <- get(paste0(".", Symbols[[i]]), envir =  source.envir)
        if (verbose)
            cat("done.\n")
        if (!is.xts(fr))
            fr <- xts(fr[, -1], as.Date(fr[, 1], origin = "1970-01-01"),
                src = "cache", updated = Sys.time())
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
getSymbols.PostgreSQL <- function(Symbols = NULL, con = con, env, return.class = 'xts',
                               db.fields=c('o','h','l','c','v','a'),
                               field.names = c('Open','High','Low','Close','Volume','Adjusted'),
                               user=NULL,password=NULL,dbname=NULL,schname = NULL,host='localhost',port=5432,
                               options = NULL, forceISOdate = TRUE,
                               ...) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})
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

  if(is.null(con)) {
    DBConMeta <- pgConnect(user=user,password=password,dbname=dbname,schname=schname,host=host,port=port,options=options,forceISOdate=forceISOdate)
    AssignEnv(DBConMeta, c("con", "user", "password", "dbname", "schname"))
  }
  if(!is.null(match.arg(schname)) && match.arg(schname) != schname) schname <- match.arg(schname)

  # ORIGINAL quantmod-ism FOLLOWS ... [ ] could be cleaned up

  AllSymbols <- pgListSchemaTables(con, schname)
  RetrievedSymbols <- AllSymbols[!AllSymbols %in% "Symbols"]
  FoundSymbols     <- RetrievedSymbols %in% Symbols
  db.Symbols       <- RetrievedSymbols[FoundSymbols]
  if(is.null(match.arg(Symbols))) db.Symbols <- RetrievedSymbols

  if(length(Symbols) != sum(Symbols %in% db.Symbols)) {
    missing.db.symbol <- Symbols[!Symbols %in% db.Symbols]
    warning(paste('could not load symbol(s): ',paste(missing.db.symbol,collapse=', ')))
    Symbols <- Symbols[Symbols %in% db.Symbols]
  }

  if(schname != "") { dotSchemaQuoted <- paste0(dbQuoteIdentifier(con, schname), ".") } else { dotSchemaQuoted <- "" }

  schemaSymbolsQuoted <- list()
  Symbols.db.Cols <- list()
  for(i in se                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 +           q_along(Symbols)) {
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
    query <- paste0("SELECT ", Selection," FROM ", schemaSymbolsQuoted[[i]]," ORDER BY ",dbQuoteIdentifier(con, "date"), ";")
    rs <- DBI::dbSendQuery(con, query)
    fr <- DBI::fetch(rs, n=-1)

    query <- paste0("SELECT ", " * ", " FROM ", dbQuoteIdentifier(con, schname), ".", dbQuoteIdentifier(con, "Symbols")," WHERE ", dbQuoteIdentifier(con, "Symbols"), " = ", dbQuoteString(con, Symbols[[i]]), ";")
    SymbolAttributes <- DBI::dbGetQuery(con, query)[,-1,drop =FALSE]

    updated <- NULL
    if("updated" %in% colnames(SymbolAttributes)) updated <- SymbolAttributes[["updated"]]

    updated_R_class <- NULL
    if("updated_R_class" %in% colnames(SymbolAttributes)) {
      updated_R_class <- SymbolAttributes[["updated_R_class"]]
      updated <- eval_bare(parse_expr(paste0("as.", updated_R_class,"(updated)")), environment())
    }
    src <- NULL
    if("src" %in% colnames(SymbolAttributes)) src <- SymbolAttributes[["src"]]

    DBI::dbDisconnect(con)

    order.by= do.call(c,fr[,1, drop = F])
    #fr <- data.frame(fr[,-1],row.names=fr[,1])
    fr <- xts(as.matrix(fr[,-1]),
              order.by=order.by,        # ORIG: order.by=as.Date(fr[,1],origin='1970-01-01'),
              src=src,updated=updated)  # ORIG: src=dbname,updated=Sys.time())
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



#' saves xts object symbols to a persistent location (dispatcher)
#'
#' First, it will look for Symbols in the .getSymbols file and env and gather them
#' Next,  it will look for Symbols in source.envir and gather them
#'
#' If provided file.path, then object will be stored on disk(same as "RData")
#' If provided trg == "RData", "cache", or "PostgreSQL (or just "pg")
#' then the objectw be ALSO saved in this OTHER location
#'
#' @param Symbols	a character vector specifying the names of each symbol
#' @param source.envir source location of Symbols
#' @param env location of xts objects placed that had been aquired with getSymbols("XXX", src = 'yahoo')
#' @param Gathering places to look for symbols to be collected
#' xts objects must have the attribute "src"
#' @param file.path if provided will save to disk
#' @param trg if provided will savse to a target "cache" or "pg" (PostgreSQL)
#' @param ... passed to trg
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
saveSymbols <- function(Symbols = NULL, con = NULL, source.envir = NULL, env = parent.frame(),
                       Gathering = c("DotgetSymbols","source.envir"), ...) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  if("source.envir" %in% Gathering) {
    # case insensitive
    if(is.environment(source.envir)) {

      # note list2env SILENTYLY DELETES symbols of the same name AND in a DIFFERENT case
      SymbolsInsource.envir <- Symbols[tolower(Symbols) %in% tolower(names(as.list(source.envir)))]
      if(length(SymbolsInsource.envir)) stop(paste0("Symbols ", paste0(SymbolsInsource.envir, collapse = ", "), " found in source.envir"))

      # note list2env SILENTYLY DELETES symbols of the same name AND in a DIFFERENT case
      source.envirInSymbols <- names(as.list(source.envir))[tolower(names(as.list(source.envir))) %in% tolower(Symbols)]
      if(length(source.envirInSymbols)) stop(paste0("source.envir ", paste0(source.envirInSymbols, collapse = ", "), " found in Symbols"))

    }
  }


  xTsGetSymbols <- list()

  if("DotgetSymbols" %in% Gathering) {

    # gather from .getSymbols file and env
    DotgetSymbolsFound <- FALSE
    if (exists(".getSymbols", env, inherits = FALSE)) {
      DotgetSymbols <- get(".getSymbols", env, inherits = FALSE)
      Inherits <- FALSE
      DotgetSymbolsFound <- TRUE
    }

    if(DotgetSymbolsFound) {

      # if (is.null(Symbols)) {
      #   # get all of the Symbols
      #   Symbols <- names(DotgetSymbols)
      # }
      # else {
      #   Symbols <- Symbols[Symbols %in% names(DotgetSymbols)]
      # }

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

  if("source.envir" %in% Gathering) {

    # gather from source.envir

    runenv <- environment()
    # look in my custom environment
    if(is.environment(source.envir)) {

    RetrievedSymbols <- ls(envir = source.envir)
    FoundSymbols <- RetrievedSymbols %in% Symbols
    EnvSymbols   <- RetrievedSymbols[FoundSymbols]
    if(is.null(Symbols))  EnvSymbols <- RetrievedSymbols

    llply(EnvSymbols, function(x) {
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

  # save everything back to my custom environment (source.envir)
  # note list2env SILENTYLY DELETES symbols of the same name AND in a DIFFERENT case
  source.envir <- list2env(xTsGetSymbols, parent = emptyenv())

  Dots <- list(...)
  # Symbols are no longer passed
  # because all 'Symbols' are in the source.envir

  if("file.path" %in% names(Dots)) {
    do.call(saveSymbols.RData, c(list(), source.envir = source.envir, Dots))
  }
  if("trg" %in% names(Dots)) {
    do.call(paste0("saveSymbols",".", Dots[["trg"]]), c(list(),
      Symbols = Symbols, con = con, source.envir = source.envir, Dots[!names(Dots) %in% "trg"]))
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
#' @export
saveSymbols.RData <- function (Symbols = NULL, source.envir = NULL, file.path = stop("must specify 'file.path'"), ...) {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})

  source.list <- as.list(source.envir)

  RetrievedSymbols <- names(as.list(source.envir))
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
#' msft <- getSymbols("MSFT", src = "yahoo", auto.assign = FALSE)
#' source.envir = list2env(list(MSFT = msft))
#' saveSymbols(Symbols = "", trg = "cache", source.envir = source.envir)
#' }
#' @export
saveSymbols.cache <- function (Symbols = NULL, source.envir = NULL, target.envir = NULL, ...) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

    source.list <- as.list(source.envir)

    RetrievedSymbols <- names(as.list(source.envir))
    FoundSymbols <- RetrievedSymbols %in% Symbols
    EnvSymbols   <- RetrievedSymbols[FoundSymbols]
    if(is.null(Symbols)) EnvSymbols <- RetrievedSymbols

    if(is.null(target.envir)) target.envir <- .GlobalEnv

    for(each.symbol in EnvSymbols) {
        assign(paste0(".", each.symbol), source.list[[each.symbol]], envir = target.envir)
    }
    invisible()
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
#' }
#' @export
saveSymbols.PostgreSQL <- function(Symbols = NULL, con = con, source.envir = NULL,
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

  if(is.null(con)) {
    DBConMeta <- pgConnect(user=user,password=password,dbname=dbname,schname=schname,host=host,port=port,options=options,forceISOdate=forceISOdate)
    AssignEnv(DBConMeta, c("con", "user", "password", "dbname", "schname"))
  }
  if(!is.null(match.arg(schname)) && match.arg(schname) != schname) schname <- match.arg(schname)

  # now, I am only getting symbols from here
  ###### Symbols <- names(as.list(source.envir))[names(as.list(source.envir)) %in% Symbols]

  RetrievedSymbols  <- names(as.list(source.envir))
  FoundSymbols      <- RetrievedSymbols %in% Symbols
  EnvSymbols        <- RetrievedSymbols[FoundSymbols]
  Symbols           <- EnvSymbols
  if(is.null(match.arg(Symbols))) Symbols <-RetrievedSymbols

  db.Symbols <- pgListSchemaTables(con, schname)

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
    # then MAYBE dbWriteTable WILL FAIL? DIFFERENT PROBLEM with MISSING other COLUMNS?
    # SEE caroline::dbWriteTable2
    # SEE RPostgre::dbWRiteTable
    # SEE MY OWN NOTES
    # (I HAVE NOT HANDLED THESE 'change' CASES)
    # NOTE: the INDEX type probably will not change
    # BUT getting NEW added COLUMNS would be COMMON so I WOULD (IN THE NEAR FUTURE) have to do:
    # (1) detect (2) ADD COLUMN

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
    # if  each.symbol includes schema name + ".", then dbWriteTable will return TRUE, but it will LIE


    updated <- NULL
    if("updated" %in% names(attributes(xTs))) {
      # OLD: "to_timestamp(", dbQuoteString(con, as.character(Sys.time())), " ,'YYYY-MM-DD HH24:MI:SS')"
      updated <- attributes(xTs)[["updated"]]
      # How to properly handle timezone when passing POSIXct objects between R and Postgres DBMS?
      # https://stackoverflow.com/questions/40524225/how-to-properly-handle-timezone-when-passing-posixct-objects-between-r-and-postg
      updated <- format(as.POSIXct(updated, tz = Sys.getenv("TZ")), "%Y-%m-%d %H:%M:%OS%z")
      dbExecute(con, paste0("UPDATE ", dbQuoteIdentifier(con, schname), ".", dbQuoteIdentifier(con, "Symbols"),
                            " SET ",
                            dbQuoteIdentifier(con, "updated"), " = ", dbQuoteString(con, updated),
                            " WHERE ",
                            dbQuoteIdentifier(con, "Symbols"), " = ", dbQuoteString(con, each.symbol),
                            ";"))
    }

    updated_R_class <- NULL
    if(inherits(xTs,"zoo")) {
      updated_R_class <- class(index(xTs))[1]
      dbExecute(con, paste0("UPDATE ", dbQuoteIdentifier(con, schname), ".", dbQuoteIdentifier(con, "Symbols"),
                            " SET ",
                            dbQuoteIdentifier(con, "updated_R_class")," = ", dbQuoteString(con, updated_R_class),
                            " WHERE ",
                            dbQuoteIdentifier(con, "Symbols"), " = ", dbQuoteString(con, each.symbol),
                            ";"))
    }
    src <- NULL
    if("src" %in% names(attributes(xTs))) {
      src <- as.character(attributes(xTs)[["src"]])
      dbExecute(con, paste0("UPDATE ", dbQuoteIdentifier(con, schname), ".", dbQuoteIdentifier(con, "Symbols"),
                            " SET ",
                            dbQuoteIdentifier(con, "src"), " = ", dbQuoteString(con, src),
                            " WHERE ",
                            dbQuoteIdentifier(con, "Symbols"), " = ", dbQuoteString(con, each.symbol),
                            ";"))
    }

  }
  dbDisconnect(con)
  invisible()

})}
#' saveSymbols pg
#'
#'@export
saveSymbols.pg <- getSymbols.PostgreSQL



#' 'updated' property xts object symbols in a persistent location (dispatcher)
#'
#' looks in source.envir xor "src" exclusively
#'
#' @param Symbols	a character vector specifying the names of each symbol
#' @param source.envir source location of Symbols
#' xts objects must have the attribute "src"
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
updatedSymbols <- function(Symbols = NULL, con = NULL, source.envir = NULL, ...) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  EnvSymbols <- list()
  if(!"src" %in% names(Dots)) {
    runenv <- environment()
    # look in my custom environment
    if(is.environment(source.envir)) {

    AllSymbols <- ls(envir = source.envir, all.names = TRUE)
    FoundAllSymbols <- AllSymbols %in% Symbols
    AllFoundSymbols <- AllSymbols[FoundAllSymbols]
    if(is.null(Symbols)) AllFoundSymbols <- AllSymbols

    llply(AllFoundSymbols, function(x) {
      xx <- get(x, source.envir, inherits = FALSE)
      updated <- NULL
      if((class(xx)[1] == "xts")) {
        updated  <- xtsAttributes(xx)[["updated"]]
      } else if (!is.null(attributes(xx)[["updated"]])) {
        updated  <- attributes(xx)[["updated"]]
      }
      namesx <- names(x)
      if(!is.null(updated)) { x <- updated } else { x <- NA_real_ }
      names(x) <- namesx
      EnvSymbols <- c(EnvSymbols, x)
      assign("EnvSymbols", EnvSymbols, envir =  runenv)
      invisible()
    })}
  }

  Dots <- list(...)

  SrcSymbols <- list()
  if("src" %in% names(Dots)) {
    SrcSymbols <- do.call(paste0("updatedSymbols",".", Dots[["src"]]), c(list(),
                    Symbols = Symbols, con = con, source.envir = source.envir, Dots[!names(Dots) %in% "src"]))
   }
  c(list(),EnvSymbols, SrcSymbols)

})}



#' 'updated' property of xts object symbols in the cache
#'
#' @param Symbols	a character vector specifying the names of each symbol
#' @param source.envir location of xts objects
#' @return a named list of 'updated' properties
#' @export
updatedSymbols.cache <- function(Symbols = NULL, source.envir = NULL, ...) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  if(is.null(source.envir)) source.envir <- .Globalenv
  EnvSymbols <- updatedSymbols(Symbols = Symbols, source.envir = source.envir)
  EnvSymbols <- EnvSymbols[str_detect(names(EnvSymbols), "^[.].+")]
  EnvSymbols

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
updatedSymbols.PostgreSQL <- function(Symbols = NULL, con = NULL,
                               user=NULL,password=NULL,dbname=NULL,schname = NULL,host='localhost',port=5432,
                               options = NULL, forceISOdate = TRUE,
                               ...) {

tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  if(is.null(con)){
    DBConMeta <- pgConnect(user=user,password=password,dbname=dbname,schname=schname,host=host,port=port,options=options,forceISOdate=forceISOdate)
    AssignEnv(DBConMeta, c("con", "user", "password", "dbname", "schname"))
  }
  if(!is.null(match.arg(schname)) && match.arg(schname) != schname) schname <- match.arg(schname)


  RetrievedSymbols <- listSymbols(con, trg = "pg")
  FoundSymbols <- RetrievedSymbols %in% Symbols
  DBSymbols    <- RetrievedSymbols[FoundSymbols]
  if(is.null(Symbols)) DBSymbols <- RetrievedSymbols

  Updateds <- list()
  for(Symbol in DBSymbols) {
    updated <- dbGetQuery(con, paste0(
      "SELECT ", dbQuoteIdentifier(con, "updated"),
      " FROM ", dbQuoteIdentifier(con, schname), ".", dbQuoteIdentifier(con, "Symbols"),
      " WHERE ", dbQuoteIdentifier(con, "Symbols"), " = ", dbQuoteString(con, Symbol), ";"))
    updated <- undated[[1]]
    names(updated)[1] <- Symbol
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
#' saveSymbols(Symbols = "", trg = "cache", source.envir = source.envir)
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
listSymbols <- function(con = NULL, source.envir = NULL, ...) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  EnvSymbols <- c()
  if(!"src" %in% names(Dots)) {
    runenv <- environment()
    # look in my custom environment
    if(is.environment(source.envir)) {
    llply(ls(envir = source.envir, all.names = TRUE), function(x) {
      xx <- get(x, source.envir, inherits = FALSE)
      if((class(xx))[1] %in% c("zoo","xts", "data.frame","ts","timeSeries")) {
        EnvSymbols <- c(EnvSymbols, x)
      }
    })}
  }

  Dots <- list(...)

  SrcSymbols <- c()
  if("src" %in% names(Dots)) {
    SrcSymbols <- do.call(paste0("listSymbols",".", Dots[["src"]]), c(list(),
                    con = con, source.envir = source.envir, Dots[!names(Dots) %in% "src"]))
  }
  c(EnvSymbols, SrcSymbols)

})}



#' xts object symbols in the cache
#'
#' @param source.envir location of xts objects
#' @param ... passed unused
#' @return character vector of Symbols
#' @export
listSymbols.cache <- function(source.envir = NULL, ...) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  if(is.null(source.envir)) source.envir <- .Globalenv
  EnvSymbols <- listSymbols(source.envir = source.envir)
  EnvSymbols <- EnvSymbols[str_detect(names(EnvSymbols), "^[.].+")]
  EnvSymbols

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

  if(is.null(con)) {
    DBConMeta <- pgConnect(user=user,password=password,dbname=dbname,schname=schname,host=host,port=port,options=options,forceISOdate=forceISOdate)
    AssignEnv(DBConMeta, c("con", "user", "password", "dbname", "schname"))
  }
  if(!is.null(match.arg(schname)) && match.arg(schname) != schname) schname <- match.arg(schname)

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
#' @return character vector of Symbols
#' @export
existSymbols <- function(Symbols = NULL, con = NULL, source.envir = NULL, ...) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  EnvSymbols    <- character(0)
  NotEnvSymbols <- character(0)
  if(!"src" %in% names(Dots)) {
    if(is.environment(source.envir)) {

      RetrievedSymbols <- listSymbols(source.envir = source.envir)
      FoundSymbols <- RetrievedSymbols %in% Symbols

      EnvSymbols <- RetrievedSymbols[FoundSymbols]
      NamesEnvSymbols <-  names(EnvSymbols)
      EnvSymbols    <- rep(TRUE, length(EnvSymbols))
      names(EnvSymbols) <- NamesEnvSymbols

      NotEnvSymbols <- RetrievedSymbols[!FoundSymbols]
      NamesNotEnvSymbols <- names(NotEnvSymbols)
      NotEnvSymbols <- rep(FALSE, length(NotEnvSymbols))
      names(NotEnvSymbols) <- NamesEnvSymbols
    }
    EnvSymbols <-  c(EnvSymbols, NotEnvSymbols)[order(c(names(EnvSymbols), names(NotEnvSymbols)))]
  }

  Dots <- list(...)

  SrcSymbols < character(0)
  if("src" %in% names(Dots)) {
    SrcSymbols <- do.call(paste0("existSymbols",".", Dots[["src"]]), c(list(),
                    Symbols = Symbols, con = con, source.envir =  source.envir, Dots[!names(Dots) %in% "src"]))
  }

  c(EnvSymbols, SrcSymbols)


})}



#' xts object symbols
#'
#' @param Symbols	a character vector specifying the names of each symbol
#' @param source.envir location of xts objects
#' @param ... passed unused
#' @return named character vector of Symbols
#' @export
existSymbols.cache <- function(Symbols = NULL, source.envir = NULL, ...) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  EnvSymbols    <- character(0)
  NotEnvSymbols <- chaacter(0)
  if(is.environment(source.envir)) {
    AllSymbols <- listSymbols(source.envir = source.envir)
    RetrievedSymbols <- Symbols %in% llply(strsplit(AllSymbols, ""), function(x) {
    if(x[1] == "") {
      paste0(x[-1], collapse = "")
    } else {
      paste0(x, collapse = "")
    }
    })
    FoundSymbols <- RetrievedSymbols %in% Symbols
    EnvSymbols <- RetrievedSymbols[FoundSymbols]
    NamesEnvSymbols <-  names(EnvSymbols)
    EnvSymbols    <- rep(TRUE, length(EnvSymbols))
    names(EnvSymbols) <- NamesEnvSymbols

    NotEnvSymbols <- RetrievedSymbols[!FoundSymbols]
    NamesNotEnvSymbols <- names(NotEnvSymbols)
    NotEnvSymbols <- rep(FALSE, length(NotEnvSymbols))
    names(NotEnvSymbols) <- NamesEnvSymbols

  }
  c(EnvSymbols, NotEnvSymbols)[order(c(names(EnvSymbols), names(NotEnvSymbols)))]

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
#' @return character vector of Symbols
#' @export
existSymbols.PostgreSQL <- function(Symbols = NULL, con = NULL,
                               user=NULL,password=NULL,dbname=NULL,schname = NULL,host='localhost',port=5432,
                               options = NULL, forceISOdate = TRUE,
                               ...) {

tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  if(is.null(con)) {
    DBConMeta <- pgConnect(user=user,password=password,dbname=dbname,schname=schname,host=host,port=port,options=options,forceISOdate=forceISOdate)
    AssignEnv(DBConMeta, c("con", "user", "password", "dbname", "schname"))
  }
  if(!is.null(match.arg(schname)) && match.arg(schname) != schname) schname <- match.arg(schname)

  # trg = "pg"
  RetrievedSymbols <- listSymbols(con, user=user,password=password,dbname=dbname,schname=schname,host=host,port=port,options=options,forceISOdate=forceISOdate, trg = "pg")
  FoundSymbols <- RetrievedSymbols %in% Symbols

  DBSymbols <- RetrievedSymbols[FoundSymbols]
  NamesDBSymbols <-  names(DBSymbols)
  DBSymbols    <- rep(TRUE, length(DBSymbols))
  names(DBSymbols) <- NamesDBSymbols

  NotDBSymbols <- RetrievedSymbols[!FoundSymbols]
  NamesNotDBSymbols <- names(NotDBSymbols)
  NotDBSymbols <- rep(FALSE, length(NotDBSymbols))
  names(NotDBSymbols) <- NamesDBSymbols

  c(DBSymbols, NotDBSymbols)[order(c(names(DBSymbols), names(NotDBSymbols)))]

})}
#' listSymbols pg
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
pgSchemaTableLastIndex <- function(con, schname = NULL, tblname) {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})

  if(!is.null(con)) {
    schname <- pgCurrentSearchPath(con)[["CurrentSearchPath"]][1]
  }
  if(is.null(schname)) schname <- "Symbols"

    dbGetQuery(con,
      paste0(
       "
        SELECT -- NOTE if the primary key has multiple columns then take the leftmost column
              max(", dbQuoteIdentifier(con, pgListSchemaTablePrimaryKeyColumns(schname, tblname))[1], ")
        FROM
            ", dbQuoteIdentifier(con, schname),".", dbQuoteIdentifier(con, tblname), "
        ;
      "
      )
    ) -> db.Query.result
    as.POSIXct(db.Query.result[["max"]])

})}


