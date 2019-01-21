



#' convert an OHLC xTs into aN OHLC data.frame MEANT to be loaded into a database
#'
#' @param xTs OHLC[V][A] object
#' @param con DBI database connection
#' @param field.names R column names
#' @param db.fields database column names
#' @export
#' @importFrom tryCatchLog tryCatchLog
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


#' determine the column names of a data.frame's key columns
#'
#' friendly function with dfToCREATETable
#' This convenience function guesses the column names of the
#' table's PRIMARY KEY columns
#'
#' @param df data.frame sent to function dfToCREATETable
#' @param keys if passed NULL or 1 then the first column is returned.
#' if passed a numeric vector then those column by ordinal position are chosen.
#' if passed "NONE" then a zero length character vector is returned.
#' if passed a character vector then those are the primary key column.
#' names themselves
#' @return character vector of column names that make up the PRIMARY KEY
#' @export
dfGetPKEYNames <- function(df, keys) {

  # this does work
  # Re: how to find primary key field name?
  # https://www.postgresql.org/message-id/4E94DB9F.7090607@gmail.com
  # however,  (currently), I am sticking to the "keys" (passed parameter) "NULL is 1" logic

  if(is.null(keys)) keys <- 1
  if(keys == "NONE") return(vector(mode = "character"))
  if(length(keys) && length(colnames(df))) {
    if(  is.numeric(keys)) return(colnames(df)[keys])
    if(is.character(keys)) return(keys)
  }
  return(vector(mode = "character"))

}



#' from column names and datatypes, make a CREATE TABLE statement
#' needed to persistently store data
#'
#' also register its meta-data
#' (Note: if the meta-data table(Symbols) does not exist, then it will be created)
#'
#' friendly function with dfGetPKEYNames
#'
#'# # create automatically by function: dfToCREATETable
#'
#'# CREATE TABLE "Symbols"
#'# (
#'#   "Symbols" text NOT NULL,
#'#   updated timestamp with time zone,
#'#   "index_R_class" text,
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
#' @param Symbol new table name. Eg.g Could be a company TICKER or a FRED column.
#' @param schname schema name
#' @param keys column(s) that will be the PRIMARY KEY of the new table.
#' If left as NULL, the furthest left most column (lowest ordinal order) postion (1) will be chosen.
#' Othwerwise, a character vector of column names.
#' Otherwise, a numeric vector of column positions in the df ( from left to right )
#' Otherwise, "NONE", that means "do not add a PRIMARY KEY"
#' @param SymbolsTable c("CREATE", "UPDATE")(default) 'check and CREATE' a "Symbols" table.
#' Attempt to "UPDATE" a "Symbols" table. Can be one or the other or both.
#' Also, can be anything else, e.g. choose FALSE, to easily visually signal
#' to not do "Symbols" CREATE/UPDATE.
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_c
#' @importFrom DBI dbQuoteIdentifier dbExecute dbQuoteString
dfToCREATETable <- function(df, con, Symbol, schname, keys = NULL, SymbolsTable = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  # keys can also be "NONE" (see the if-then) below
  if(is.null(keys)) keys <- 1

  if(is.null(SymbolsTable)) SymbolsTable <- c("CREATE", "UPDATE")

  # meta-data table
  if("CREATE" %in% SymbolsTable) {
    if(!"Symbols" %in% pgListSchemaTables(con, "Symbols")) {
      ddl <- stringr::str_c("CREATE TABLE ", DBI::dbQuoteIdentifier(con, schname), ".", DBI::dbQuoteIdentifier(con, "Symbols"), "(",
                              DBI::dbQuoteIdentifier(con, "Symbols"),          " TEXT ", ", ",
                              DBI::dbQuoteIdentifier(con, "updated"),          " TIMESTAMP WITH TIMEZONE ", ", ",
                              DBI::dbQuoteIdentifier(con, "index_R_class"),    " TEXT ", ", ",
                              DBI::dbQuoteIdentifier(con, "src"),              " TEXT " ,
                            ");")
      DBI::dbExecute(con, ddl)

      ddl <- stringr::str_c("ALTER TABLE ", DBI::dbQuoteIdentifier(con, schname), ".", DBI::dbQuoteIdentifier(con, "Symbols"),
                            " ADD PRIMARY KEY ( ", DBI::dbQuoteIdentifier(con, "Symbols"), ")",
                            ";")
      DBI::dbExecute(con, ddl)
    }
  }

  # upon creation, do Quote Once:  (1)schema, (2)table and (3)column names
  # the PostgreSQL storage will be: anything_capilized retains it's "" quotes.

  if(schname != "") { dotSchemaQuoted <- paste0(DBI::dbQuoteIdentifier(con, schname), ".") } else { dotSchemaQuoted <- "" }
  schemaSymbolsQuoted <-  paste0(dotSchemaQuoted, DBI::dbQuoteIdentifier(con, Symbol))

  # column datatypes
  colClasses <- pgDFColClasses(df[, , drop = FALSE])

  if(is.numeric(keys))   { TablePrimKeyCols <- Names(colClasses)[keys] }
  if(is.character(keys)) { TablePrimKeyCols <- Names(colClasses)[names(colClasses) %in% keys] }

  # Note: PostgreSQL does allow zero column tables ( SQLite does not. )
  ddl <- stringr::str_c("CREATE TABLE ", schemaSymbolsQuoted ,"(", stringr::str_c( DBI::dbQuoteIdentifier(con, names(colClasses)), " ", colClasses, collapse = ", "), ");")
  DBI::dbExecute(con, ddl)

  if(length(colClasses) && (keys != "NONE")) {
    ddl <- stringr::str_c("ALTER TABLE ", schemaSymbolsQuoted,
                  " ADD PRIMARY KEY ( ", stringr::str_c(DBI::dbQuoteIdentifier(con, TablePrimKeyCols), collapse = ", "), ")",
                  ";")
    DBI::dbExecute(con, ddl)
  }

  if("UPDATE" %in% SymbolsTable) {
    dml <- stringr::str_c("INSERT INTO ", DBI::dbQuoteIdentifier(con, schname), ".", DBI::dbQuoteIdentifier(con, "Symbols"),
                               "(", DBI::dbQuoteIdentifier(con, "Symbols"), ") VALUES (", DBI::dbQuoteString(con, Symbol), ");")
    DBI::dbExecute(con, dml)
  }

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
#' @importFrom tryCatchLog tryCatchLog
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
#' @importFrom tryCatchLog tryCatchLog
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

  if(is.null(user))     { user     <- "Symbols"; Sys.setenv("PGUSER"     = "Symbols") }
  if(is.null(password)) { password <- "Symbols"; Sys.setenv("PGPASSWORD" = "Symbols") }
  if(is.null(dbname))   { dbname   <- "Symbols"; Sys.setenv("PGDATABASE" = "Symbols") }
  if(is.null(schname))  { schname  <- "Symbols" }
  if(is.null(user) || is.null(password) || is.null(dbname)) {
    stop(paste(
        'At least one connection argument (',sQuote('user'),
        sQuote('password'),sQuote('dbname'),
        ") is not set"))
  }
  con <- DBI::dbConnect(RPostgreSQL::PostgreSQL(max.con = 100),user=user,password=password,dbname=dbname,
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
  Sys.setenv("TRADEMODEL_SYMBOLS_SCHEMA" = schname)
  list(con=con,user=user,password=password,dbname=dbname,schname=schname)

})}



#' of a specific PostgreSQL database schema, show its tables
#'
#' @param con PostgreSQL DBI connection
#' @param schname schema name
#' @return vector of characters of table names
#' The results do not have any order.
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_c
#' @importFrom DBI dbGetQuery dbQuoteLiteral
pgListSchemaTables <- function(con, schname) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

    DBI::dbGetQuery(con,
      stringr::str_c(
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
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_c
#' @importFrom DBI dbGetQuery dbQuoteLiteral
pgListSchemaTableColumns <- function(con, schname, tblname) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

    DBI::dbGetQuery(con,
      stringr::str_c(
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



#' of a specific PostgreSQL database schema table, show its columns and data types
#'
#' @param con PostgreSQL DBI connection
#' @param schema name
#' @param table name
#' @return data.frame of characters of schema, table, column, and human readable type
#' ordered by ordinal position
#' The results are ordered.
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_c
#' @importFrom DBI dbGetQuery dbQuoteLiteral
pgSchemaTableColumnTypes <- function(con, schname, tblname) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

    DBI::dbGetQuery(con,
      stringr::str_c(
       "
        SELECT
        --   table_catalog -- database
        --  , table_schema
        --  ,
              table_name
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
    db.Schema.tbl

})}



#' of a specific PostgreSQL database schema table, show its primary key columns
#'
#' @param con PostgreSQL DBI connection
#' @param schema name
#' @param table name
#' @return vector of characters of primary key column names
#' The results are ordered.
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_c
#' @importFrom DBI dbGetQuery dbQuoteLiteral
pgListSchemaTablePrimaryKeyColumns <- function(con, schname, tblname) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

    # List primary keys for all tables - Postgresql
    #  https://dba.stackexchange.com/questions/11032/list-primary-keys-for-all-tables-postgresql

    DBI::dbGetQuery(con,

      stringr::str_c(
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
#' @return single column data.frame
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
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom plyr llply
#' @importFrom DBI dbGetQuery
#' @importFrom DescTools DoCall
oneColumn <- function(con, Query, outName, unQuote = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  if(is.null(unQuote)) unQuote = FALSE
  res <- DBI::dbGetQuery(con, Query)
  if(NROW(res)) {
    res <- res[[1]]
    res %>% { if(is.character(.)) { strsplit(., ", ") } else { . } } %>%
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
        { DescTools::DoCall(get("c"),.) } %>%
          as.data.frame(., stringsAsFactors = FALSE) -> ret
  }
  colnames(ret) <- outName
  return(ret)
})}



#' single quote
#'
#' NOT USED ANYWHERE
#'
#' @param x vector of strings
#' @importFrom stringr str_c
#' @importFrom plyr llply
#' @export
siQuote <- function(x) {
  unlist(plyr::llply(x, function(x) stringr::str_c("'", x, "'", collapse = "")))
}



#' addto/update a database with new information
#'
#' @param con DBI connection PostgreSQL
#' @param trgt remote server side string database table name of old data
#' @param keys trgt remote server side vector of strings of table
#' column names that make up a unique id for the row.
#' keys can not be zero length. keys can not be null.
#' @param schname schema name
#' @param df local client side data.frame of 'updated data'
#' if parameter oldData is not null, then df is not used.
#' @param varHint optional vector of character column names. Performance optimization
#' techique to limit the number of rows returned
#' from the database server to the client(R).
#' User must specify as paired position values.
#' e.g. varHint = "dateindex", valHint = "17000"
#' or e.g. varHint = c("dateindex", "ticker"), valHint = c("17000","'AAPL'")
#' Position matches one to one with valHint.
#' Parameter varHint is sent to pgOldData.
#' @param valHint
#' See varHint. Position matches one to one with varHint.
#' Parameter valHint is sent to pgOldData.
#' @param ... dots passed to pgUpdate
#' @examples
#' \dontrun{
#'
#' # setup
#' SuBmtcars <- mtcars[c(1,5),1:2]
#' oldData <- data.table::data.table(SuBmtcars, keep.rownames=TRUE, key="rn")
#' oldData[1,2] <- NA; oldData[2,3] <- NA
#'
#' con <- DBI::dbConnect(RPostgreSQL::PostgreSQL(max.con = 100), user = "postgres")
#' DBI::dbExecute(con, "DROP TABLE IF EXISTS public.mtcars")
#' DBI::dbWriteTable(con, "mtcars", oldData, row.names = FALSE)
#'
#' newData <- data.table::data.table(SuBmtcars, keep.rownames=TRUE, key="rn")
#' newData[2,2] <- NA; newData[1,3] <- NA
#'
#' newInsData <- data.table::data.table(mtcars[12, 2, drop = FALSE], keep.rownames=TRUE, key="rn")
#'
#' # new data
#' pgUpsize(con, trgt = "mtcars", keys = "rn", schname = "public",
#'   df = newInsData, varHint = "cyl", valHint = "8")
#' # current data
#' pgUpsize(con, trgt = "mtcars", keys = "rn", schname = "public", df = newData)
#'
#' DBI::dbDisconnect(con)
#'
#'}
#' @export
pgUpsize <- function(con, trgt = NULL, keys = NULL, schname = NULL, df = NULL, varHint = NULL, valHint = NULL, ...) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  df <- as.data.frame(df, stringsAsFactors = FALSE)

  Dots <- list(...)
  if(is.null(Dots[["SymbolsTable"]])) SymbolsTable <- FALSE

  # if the schema.table does not exist then add it.
  pgAddTable(con, trgt = trgt, keys = keys, schname = schname, df = df, SymbolsTable = SymbolsTable)
  # of any columns that exist in df but do not exist on the Server DB,
  # add those new columns to the Server DB
  pgAddColumnType(con, trgt = trgt, schname = schname, df = df)
  # new rows that do not exist on the server ( uniquely identified by keys )
  pgInsert(con, trgt = trgt, keys = keys, schname = schname, df = df, varHint = varHint, valHint = valHint)
  # current rows that already exist on the server ( uniquely identified by keys )
  pgUpdate(con, trgt = trgt, keys = keys, schname = schname, df = df, varHint = varHint, valHint = valHint, ... )

  invisible()

})}



#' add a table to a database
#'
#' @param con DBI database connection
#' @param Symbol new table name. E.g Could be a company TICKER or a FRED column.
#' @param keys trgt remote server side vector of strings of table
#' column names that make up a unique id for the row.
#' keys can not be zero length. keys can not be null.
#' @param schname schema name
#' @param df data.frame (with column names)
#' @param ... dots passed to dfToCREATETable
#' @export
pgAddTable <- function(con, trgt = NULL, keys = c("rn"), schname, df = NULL, ...) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  df <- as.data.frame(df, stringsAsFactors = FALSE)

  if(!DBI::dbExistsTable(con, c(schname, trgt))) {
    dfToCREATETable(df, con, Symbol = trgt, schname = schname, keys = keys, ...)
  } else {
    message("pgAddTable skipping table.  It is already there")
  }
  invisible()
})}



#' from R data.frame column types, determine Server DB data types
#'
#' @param df local client side data.frame. If df's class `[` method
#' drops single dimentions to vectors the call by using
#' pgDFColClasses(df[, "col1", drop = F])
#' @return named vector of names of column names and
#' values of Server DB column data types
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom plyr llply
#' @importFrom DescTools DoCall
#' @importFrom DBI dbExecute
#' @importFrom plyr llply
#' @examples
#' \dontrun{
#'
#' pgDFColClasses(mtcars[, "cyl", drop = F])
#'             cyl
#' "NUMERIC(14,3)"
#'
#' }
#' @export
pgDFColClasses <- function(df = NULL, ...) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  colClasses  <- DescTools::DoCall(get("c"),plyr::llply(as.data.frame(df, stringsAsFactors = FALSE), function(x) {class(x)[1]}))
  if(is.null(colClasses)) colClasses <- vector(mode = "character")

  # column datatypes
  colClasses[colClasses     == "logical"]    <- "BOOLEAN"
  colClasses[colClasses     == "character"]  <- "TEXT"
  colClasses[colClasses     == "integer"]    <- "INTEGER"
  colClasses[colClasses     == "numeric"]    <- "NUMERIC(14,3)"
  colClasses[colClasses %in%   "Date"]       <- "DATE"
  colClasses[colClasses %in%   "POSIXct"]    <- "TIMESTAMP WITH TIMEZONE"
  # ACTUALLY I HAVE NO EXPERIENCE ( THIS IS AN EDUCATED WILD GUESS: LATER, I WILL EXPERIMENT/TEST/FIX THIS )
  # xts OTHER supported index date/time classes
  colClasses[colClasses %in% c("chron", "yearmon", "yearqtr", "timeDate")] <- "TIMESTAMP WITH TIMEZONE"
  # NOTE: to represent a non-simple R class in a PostgreSQL datatype, one may need TWO [or more] columns
  #       PostgreSQL may need a 'non-simple' datatype (from CREATE TYPE)
  #       R/PostgreSQL may need datatype mappers
  #       FUTURE WORK?

  return(colClasses)

})}



#' add a column to a database table
#'
#' matches by colum name. If the column the does not
#' exist on the Server DB, then the column is added to the Server DB
#' by ALTER TABLE <trgt> ADD ( column ) called each per new column.

#' @param con DBI connection PostgreSQL
#' @param trgt remote server side string database table name of old data
#' @param schname schema name
#' @param df local client side data.frame of 'updated data'. Only the column
#' names are matched to the Server DB table
#'
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom DescTools DoCall
#' @importFrom stringr str_c
#' @importFrom plyr llply
#' @importFrom DBI dbQuoteIdentifier dbExecute
#' @examples
#' \dontrun{
#'
#'  con <- DBI::dbConnect(RPostgreSQL::PostgreSQL(max.con = 100), user = "postgres")
#'  DBI::dbWriteTable(con, "mtcars", oldData, row.names = FALSE)
#'  pgAddColumnType(con, trgt = "mtcars", schname = "public", df = data.frame(rnk = 0L))
#'
#' }
#' @export
pgAddColumnType <- function(con, trgt = NULL, schname = NULL, df = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  if(is.null(schname) || (nchar(schname) == 0)) stop("pgAddColumnType requires schname")
  schemaDotQuoted <- stringr::str_c( DBI::dbQuoteIdentifier(con, schname), ".")

  trgtQuoted <- DBI::dbQuoteIdentifier(con, trgt)
  schematrgtQuoted <- stringr::str_c(schemaDotQuoted, trgtQuoted)

  trgtColumns <- pgListSchemaTableColumns(con = con, schname = schname, tblname = trgt)
  NewColumns <- colnames(df)[!colnames(df) %in% trgtColumns]
  if(!length(NewColumns)) {
    message("pgAddColumnType did not find any new columns to add")
    return(invisible())
  }
  colClasses <- pgDFColClasses(df[, NewColumns, drop = F])

  ddl <- stringr::str_c("ALTER TABLE ", schematrgtQuoted ," ADD COLUMN ", stringr::str_c( DBI::dbQuoteIdentifier(con, names(colClasses)), " ", colClasses, collapse = ", "), ";")
  DBI::dbExecute(con, ddl)

  return(invisible())

})}


#' insert into a database table 'new' data
#'
#' The keys parameter determine distinct records.
#' Only 'new' records area added.
#' (See pgUpdate to add information to current(already existing) records)
#'
#' In internal feature
#' oldData previously collected limited server DB data used to
#' restrict what is inserted.
#' In oldData of data: server data and df data that is common by primary keys
#' is eliminatated from the attempted insert.
#' The keys are checked for 'no duplicates'
#'
#' @param con DBI connection PostgreSQL
#' @param trgt remote server side string database table name of old data
#' @param keys trgt remote server side vector of strings of table
#' column names that make up a unique id for the row.
#' keys can not be zero length. keys can not be null.
#' @param schname schema name
#' @param df local client side data.frame of limited data
#' that determines what data is to be returned from the Server DB
#' @param varHint optional vector of character column names. Performance optimization
#' techique to limit the number of rows returned
#' from the database server to the client(R).
#' User must specify as paired position values.
#' e.g. varHint = "dateindex", valHint = "17000"
#' or e.g. varHint = c("dateindex", "ticker"), valHint = c("17000","'AAPL'").
#' Position matches one to one with valHint.
#' @param valHint see above: varHint
#' @param ... dots passed currently not used
#' @examples
#' \dontrun{
#'
#' # setup
#' SuBmtcars <- mtcars[c(1,5),1:2]
#' oldData <- data.table::data.table(SuBmtcars, keep.rownames=TRUE, key="rn")
#' oldData[1,2] <- NA; oldData[2,3] <- NA
#'
#' con <- DBI::dbConnect(RPostgreSQL::PostgreSQL(max.con = 100), user = "postgres")
#' DBI::dbExecute(con, "DROP TABLE IF EXISTS public.mtcars")
#' DBI::dbWriteTable(con, "mtcars", oldData, row.names = FALSE)
#'
#' newInsData <- data.table::data.table(mtcars[12, 2, drop = FALSE], keep.rownames=TRUE, key="rn")
#' pgInsert(con, trgt = "mtcars", keys = "rn", schname = "public",
#'   df = newInsData, varHint = "cyl", valHint = "8")
#'
#' }
#' @importFrom data.table data.table rbindlist
#' @importFrom zoo as.Date
#' @importFrom stringr str_c
#' @importFrom DBI dbWriteTable
#' @export
pgInsert <- function(con, trgt = NULL, keys = c("rn"), schname, df = NULL, varHint = NULL, valHint = NULL, ...) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  df <- as.data.frame(df, stringsAsFactors = FALSE)

  if(is.null(trgt)) stop("pgInsert trgt can not be null")
  if(is.null(keys)) stop("pgInsert keys can not be null")
  #
  # MAYBE later I want SPECIFY a schname in
  # some other WAY (similar to what I have in other programs)
  if(is.null(schname)) stop("pgInsert schname can not be null")
  #
  if(is.null(df)) stop("pgInsert df can not be null")

  if(NROW(df) == 0) {
     message("pgInsert df has zero(0) rows.  Nothing is to do . . . ")
     return(invisible())
  }

  # IntentionFor == "INSERT" (best to limit data by varHint and valHint)
  # IntentionFor == "INSERT" (SELECT keys FROM )
    # limit local key columns to only just Server DB existing columns
  oldKeyData <- pgOldData(con, trgt = trgt, keys = keys, schname = schname, df = df, varHint = varHint, valHint = valHint, IntentionFor = "INSERT")

  ## BEGINNING OF PGINSERT/PGUPDATE
  # Server DB column names
  serverDBColumns <-  pgListSchemaTableColumns(con, schname = schname, tblname = trgt)

  # Note, this 'appends' heavily assumes that the
  # columns datatypes are the same (or compatiable) (UNCHECKED)

  # column names ORDER are the same (CHECKED)
  # RPostgreSQL::dbWriteTable C code, caroline::dbWriteTable2

  # garantee order of the df columns
  # from the original Server DB columns or oldKeyData
  # AND
  # chopVectorExcess = TRUE
  ColsAloneINoldKeyData <- setdiff(colnames(oldKeyData), serverDBColumns)
  if(length(ColsAloneINoldKeyData)) {
     warning("pgInsert found ColsAloneINoldKeyData that do not exist in the server\n")
     warning(stringr::str_c("  Will not trying to put them into the database here: ", ColsAloneINoldKeyData, "\n"))
  }                          # order garantee                                                             # not too many columns
  oldKeyData <- oldKeyData[, customSorting(colnames(oldKeyData), serverDBColumns, sortVectorExcess = FALSE, chopVectorExcess = TRUE), with=FALSE]

  newData <- data.table::data.table(df[, , drop = FALSE], key=keys)
  ## END OF PGINSERT/PGUPDATE
  ColsAloneINnewData <- setdiff(colnames(newData), serverDBColumns)
  if(length(ColsAloneINnewData)) {
     warning("pgInsert found ColsAloneINnewData that do not exist in the server\n")
     warning(stringr::str_c("  Will not trying to put them into the database here: ", ColsAloneINnewData, "\n"))
  }                          # order garantee                                                             # not too many columns
  newData <- newData[, customSorting(colnames(newData), serverDBColumns, sortVectorExcess = FALSE, chopVectorExcess = TRUE), with=FALSE]


  # fill in missing columns on the R client side that exist on the Server DB
  SchemaTableColumnTypes <- pgSchemaTableColumnTypes(con, schname = schname, tblname = trgt)
  SchemaTableColumnTypesSplitted <- split(SchemaTableColumnTypes, f = seq_len(NROW(SchemaTableColumnTypes)))

  # Maybe dbWriteTable does not need these

  # like package caroline::dbWriteTable2, if column exist in the Server DB
  # and and not in the df, then add an empty column to the
  # df of the correct Server DB colum datatype.
  for(Column in SchemaTableColumnTypesSplitted) {
    if(!Column[["column_name"]] %in% colnames(newData)) {
      newColData <- rep(NA_integer_, NROW(newData))
      switch(Column[["udt_name"]]
        , "int4"   = as.integer(newColData)
        , "text"   = as.character(newColData)
        , "float8" = as.numeric(newColData)
        , "bool"   = as.logical(newColData)
        , "numeric" = as.numeric(newColData)
        , "date"    = zoo::as.Date(newColData)
        , "timestamp" = as.as.POSIXct(newColData, tz = "UTC", origin = "1970-01-01")
        ) -> newColData
      newData[[Column[["column_name"]]]] <- newColData
    }                                      # CURRENLTLY NOT USED ANYWHERE
  }


  # Reorder the newData columns to match the order on the Server DB
  newData <- newData[, customSorting(colnames(newData), serverDBColumns, sortVectorExcess = FALSE), with=FALSE]

  # just select key columns
  newKeyData <- newData[, keys, with=FALSE]
  # correct, newKeyData is upper table data, see below, the clever programming: seq_len(NROW
  newKeyoldData <- data.table::rbindlist(list(newKeyData, oldKeyData))
  # choose rows with never duplicates           choose rows with never duplicates
  # choose rows with never duplicates  ******** choose rows with never duplicates
  # choose rows with never duplicates           choose rows with never duplicates
  NewDataIndex <- !duplicated(newKeyoldData) & !duplicated(newKeyoldData, fromLast = TRUE)

  NewDataIndex <- NewDataIndex[seq_len(NROW(newKeyData))]
  FinalNewData <- newData[NewDataIndex, ]

  if(NROW(FinalNewData)){
    # append record(s) up to the Server DB
    DBI::dbWriteTable(con, c(schname, trgt), FinalNewData, row.names = FALSE, overwrite = FALSE, append = TRUE)
  } else {
    message(stringr::str_c("pgInsert could not find records to write to ", schname, ".", trgt))
  }
  return(invisible())

})}



#' collect from the Server DB a limited amount of data restricted by df and VarHint
#'
#' Elegible Server DB data that is available for update/insert.
#' The idea is to get data from the server, so that one MAY want to update/insert.
#' For performance reasons,
#' the unique combinations of the keys of the data.frame df
#' are sent to the server, to limit the number of rows returned from the server.
#'
#' if a column is found in df and not found on the Server DB,
#' then, the function removes that column from df,
#' (because that column can not be selected from the Server DB),
#' and the user is warned about the column removal.
#' Hint, before running this function, run pgAddColumnType, to garantee(create)
#' columns from db to be new columns on the Server DB
#'
#' @param con DBI connection PostgreSQL
#' @param trgt remote server side string database table name of old data
#' @param keys trgt remote server side vector of strings of table
#' column names that make up a unique id for the row.
#' Currently, if keys != NULL and IntentionFor = "INSERT", then just the 'key' columns are returned.
#' If keys == NULL and IntentionFor == "INSERT" then all of the columns are returned
#' If IntentionFor == "UPDATE" then this parameter is ignored. (All of the columns are returned)
#' returned,
#' @param schname schema name
#' @param df local client side data.frame of limited data
#' that determines what data is to be returned from the Server DB
#' @param varHint optional vector of character column names. Performance optimization
#' techique to limit the number of rows returned
#' from the database server to the client(R).
#' User must specify as paired position values.
#' e.g. varHint = "dateindex", valHint = "17000"
#' or e.g. varHint = c("dateindex", "ticker"), valHint = c("17000","'AAPL'")
#' Position matches one to one with valHint
#' @param valHint
#' See varHint. Position matches one to one with varHint.
#' @param IntentionFor "UPDATE"(default), will collect 'key matching data.'
#' Otherwise "INSERT", will not collect 'key matching data.
#' About, "INSERT", the situation is best to use varHint and valHint to
#' generate some "INSERT" key to key comparison data.
#' @examples
#' \dontrun{
#'
#' # teardown and setup
#' SuBmtcars <- mtcars[c(1,5),1:2]
#' oldData <- data.table::data.table(SuBmtcars, keep.rownames=TRUE, key="rn")
#' oldData[1,2] <- NA; oldData[2,3] <- NA
#'
#' con <- DBI::dbConnect(RPostgreSQL::PostgreSQL(max.con = 100), user = "postgres")
#' DBI::dbExecute(con, "DROP TABLE IF EXISTS public.mtcars")
#' DBI::dbWriteTable(con, "mtcars", oldData, row.names = FALSE)
#'
#' newData <- data.table::data.table(SuBmtcars, keep.rownames=TRUE, key="rn")
#' newData[2,2] <- NA; newData[1,3] <- NA
#' # end of teardown and setup
#'
#' # default(IntentionFor == "UPDATE")
#' oldData <- pgOldData(con, trgt = "mtcars", keys = c("rn"), schname = "public", df = newData)
#'
#' # repeat teardown and setup
#' oldData <- pgOldData(con, trgt = "mtcars", keys = c("rn"), schname = "public", df = newData, varHint = "rn", valHint = "'Hornet Sportabout'")
#'
#' # repeat teardown and setup
#' oldData <- pgOldData(con, trgt = "mtcars", keys = NULL   , schname = "public", df = newData, IntentionFor = "INSERT")
#'
#' DBI::dbDisconnect(con)
#'
#'}
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_c
# non-exported S3 methods #  data.table:::split.data.table
#' @import data.table
#' @importFrom data.table data.table
#' @importFrom DBI dbGetQuery dbQuoteIdentifier dbQuoteString
#' @export
pgOldData <- function(con, trgt = NULL, keys = c("rn"), schname, df = NULL, varHint = NULL, valHint = NULL, IntentionFor = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  df <- as.data.frame(df, stringsAsFactors = FALSE)

  if(is.null(trgt)) stop("pgOldData trgt can not be null")
  # NOW ALLOWED "keys = NULL" call "pgOldData" with caution
  # if(is.null(keys)) stop("pgOldData keys can not be null")
  #
  # MAYBE later I want SPECIFY a schname in
  # some other WAY (similar to what I have in other programs)
  if(is.null(schname)) stop("pgOldData schname can not be null")
  #
  if(is.null(df))   stop("pgOldData df can not be null")

  if(NROW(df) == 0) {
     message("pgOldData df has zero(0) rows.  Nothing is to do . . . ")
     return(invisible())
  }

  if(is.null(IntentionFor)) IntentionFor <- "UPDATE"

  newData <- data.table::data.table(df, key=keys)

  # if a column is found in newData and not found on the Server DB,
  # then, now, remove that column from newData, (because that column can not be selected
  # from the Server DB), and send the user a warning about the column removal.
  serverDBColumns <-  pgListSchemaTableColumns(con, schname = schname, tblname = trgt)
  if(any(!colnames(newData) %in% serverDBColumns)) {
    newData <- newData[ , colnames(newData)[colnames(newData) %in% serverDBColumns], with = FALSE]
    warning("pgOldData found in df column(s) that do not exist on the Server DB.\nThose df column(s) have been removed.")
  }

  if(schname != "") { dotSchemaQuoted <- stringr::str_c(DBI::dbQuoteIdentifier(con, schname), ".") } else { dotSchemaQuoted <- "" }
  schemaTrgtTableQuoted <-  stringr::str_c(dotSchemaQuoted, DBI::dbQuoteIdentifier(con, trgt))

  SelectWhereExactlies <- vector(mode = "character")

  # from df, determine the verticle subset (WHERE) of data to collect from the server
  # initial subsetting for (UPDATEs)
  if(IntentionFor == "UPDATE") {

    if(any(keys %in% colnames(newData))) {
      if(length(keys)){
        Splits <-interaction(newData[, keys, with=FALSE], drop = TRUE)
        if(length(Splits)){
          Splitted <-  split(newData[, keys, with=FALSE], f = Splits)
        }
      }
    }

    if(!exists("Splitted", envir = environment(), inherits = FALSE))
      Splitted <- list()
    for(spl in Splitted) {
      keyvals <- do.call(c, spl)
      if(length(keyvals)) {
        keyvals <- if(!is.numeric(keyvals)) DBI::dbQuoteString(con, as.character(keyvals))
        SelectWhereExactly <- stringr::str_c("(", stringr::str_c(DBI::dbQuoteIdentifier(con, keys), " = ", keyvals, collapse = " AND "), ")", collapse = "")
        SelectWhereExactlies <- c(SelectWhereExactlies, SelectWhereExactly)
      }
    }
    if(length(SelectWhereExactlies)) {
      SelectWhereExactlies <- stringr::str_c("(" , stringr::str_c(SelectWhereExactlies, collapse = " OR "), ")", collapse = "")
    }

  }

  # extra subsetting (If any) # Heavily desired in the case of IntentionFor =="INSERT"
  if(!is.null(varHint) && !is.null(valHint)) {
    SelectWhereExactly <- stringr::str_c("(", stringr::str_c(DBI::dbQuoteIdentifier(con, varHint), " = ", valHint, collapse = " AND "), ")", collapse = "")
    SelectWhereExactlies <- c(SelectWhereExactlies, SelectWhereExactly)
  }

  # actually go to the Server DB then collect and bring down that server data to local R
  #  do not bring back too many columns
  if(IntentionFor == "UPDATE") {
    ColnamesAndCommas <- stringr::str_c(DBI::dbQuoteIdentifier(con, colnames(newData)), collapse = ", ")
  }
  if(IntentionFor == "INSERT") {
    if(length(keys))
      ColnamesAndCommas <- stringr::str_c(DBI::dbQuoteIdentifier(con, keys), collapse = ", ")
    if(!length(keys)){
      ColnamesAndCommas <- stringr::str_c(DBI::dbQuoteIdentifier(con, colnames(newData)), collapse = ", ")
      SelectWhereExactlies <- c( SelectWhereExactlies, " (1 = 0) ")
                              # no rows but I do get column names
    }
  }

  # combine subsetting
  if(length(SelectWhereExactlies)) {
    SelectWhereExactlies <- stringr::str_c(" WHERE " , stringr::str_c(SelectWhereExactlies, collapse = " AND "), collapse = "")
  }

  SQLSelection <- stringr::str_c("SELECT ", ColnamesAndCommas, " FROM ", schemaTrgtTableQuoted, SelectWhereExactlies)
  oldServerData <- DBI::dbGetQuery(con, SQLSelection)
  ### BIG SQL STATEMENT ### message(SQLSelection)
  oldServerData <- data.table::data.table(oldServerData, key=keys)
  oldServerData

})}



#' update a database table with 'updated' data
#'
#' For performance reasons,
#' the unique combinations of the keys of the data.frame df
#' are sent to the server, to limit the number of
#' rows returned from the server by pgOldData
#'
#' @param con DBI connection PostgreSQL
#' @param trgt remote server side string database table name of old data
#' @param keys trgt remote server side vector of strings of table
#' column names that make up a unique id for the row.
#' keys can not be zero length. keys can not be null.
#' @param schname schema name
#' @param df local client side data.frame of 'updated data'
#' if parameter oldData is not null, then df is not used.
#' @param varHint optional vector of character column names. Performance optimization
#' techique to limit the number of rows returned
#' from the database server to the client(R).
#' User must specify as paired position values.
#' e.g. varHint = "dateindex", valHint = "17000"
#' or e.g. varHint = c("dateindex", "ticker"), valHint = c("17000","'AAPL'")
#' Position matches one to one with valHint.
#' Parameter varHint is sent to pgOldData.
#' @param valHint
#' See varHint. Position matches one to one with varHint.
#' Parameter valHint is sent to pgOldData.
#' @param AppendConditions  create UPDATE statements rule specifics  c("LRvaluesDiff", "LnaRvalue")(default)
#' "LRvaluesDiff" is 'both sides (old/new) have a value and different from each other'
#' "LnaRvalue" is 'left side (old) does not have a value and right side (new) does have a value'
#' "Always" is 'both sides (old/new) values do not matter. Always append'
#' @param prepare.query FALSE(default) will use RPostgres PostgreSQLConnection
#' If TRUE, will use package RPostgre PqConnection
#' @param password (REQUIRED if prepare.query == TRUE) passed
#' to package RPostgres dbConnect to create a
#' class PqConnection connection.  This is needed to to a prepare.query
#' and create an intermediate TEMPORARY TABLE on the database server.
#' An alternative way to to specify the password is throught the environment
#' variable PGPASS.  E.g. Sys.setenv("PGPASSWORD" = "postgres")
#' @examples
#' \dontrun{
#'
#' # begin teardown and setup
#' SuBmtcars <- mtcars[c(1,5),1:2]
#' oldData <- data.table::data.table(SuBmtcars, keep.rownames=TRUE, key="rn")
#' oldData[1,2] <- NA; oldData[2,3] <- NA
#'
#' con <- DBI::dbConnect(RPostgreSQL::PostgreSQL(max.con = 100), user = "postgres")
#' DBI::dbExecute(con, "DROP TABLE IF EXISTS public.mtcars")
#' DBI::dbWriteTable(con, "mtcars", oldData, row.names = FALSE)
#'
#' newData <- data.table::data.table(SuBmtcars, keep.rownames=TRUE, key="rn")
#' newData[2,2] <- NA; newData[1,3] <- NA
#'
#' # not "prepare.query"
#' # end teardown and setup
#'
#' # this
#' pgUpdate(con, trgt = "mtcars", keys = c("rn"), schname = "public", df = newData)
#'
#' # begin teardown and setup
#' SuBmtcars <- mtcars[c(1,5),c(1:2,8)]
#' oldData <- data.table::data.table(SuBmtcars, keep.rownames=TRUE, key=c("rn","vs"))
#' oldData[1,2] <- NA; oldData[2,3] <- NA
#'
#' con <- DBI::dbConnect(RPostgreSQL::PostgreSQL(max.con = 100), user = "postgres")
#' DBI::dbExecute(con, "DROP TABLE IF EXISTS public.mtcars")
#' DBI::dbWriteTable(con, "mtcars", oldData, row.names = FALSE)
#'
#' newData <- data.table::data.table(SuBmtcars, keep.rownames=TRUE, key=c("rn","vs"))
#' newData[2,2] <- NA; newData[1,3] <- NA
#'
#' # not "prepare.query"
#' # end teardown and setup
#'
#' # this
#' pgUpdate(con, trgt = "mtcars", keys = c("rn","vs"), schname = "public", df = newData)
#'
#'
#'
#' # xor
#' # "prepare.query"
#' DBI::dbWriteTable(con, "mtcars", oldData, row.names = FALSE)
#' pgUpdate(con, trgt = "mtcars", keys = c("rn"), schname = "public",
#'   df = newData, prepare.query = TRUE, password = "postgres")
#'
#' DBI::dbDisconnect(con)
#'
#'}
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom plyr llply
#' @importFrom stringr str_c str_subset str_replace str_split
# non-exported S3 methods # data.table:::merge.data.table data.table:::split.data.table
#' @import data.table
#' @importFrom data.table data.table
#' @importFrom DescTools DoCall
#' @importFrom DBI dbWriteTable dbGetQuery dbQuoteIdentifier dbQuoteString dbBegin dbCommit dbExecute
#' @importFrom plyr llply
#' @importFrom RPostgres Postgres
#' @export
pgUpdate <- function(con, trgt = NULL, keys = c("rn"), schname, df = NULL, varHint = NULL, valHint = NULL, AppendConditions = NULL, ... ) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  df <- as.data.frame(df, stringsAsFactors = FALSE)

  Dots <- list(...)

  if(is.null(trgt)) stop("pgUpdate trgt can not be null")
  if(is.null(keys)) stop("pgUpdate keys can not be null")
  #
  # MAYBE later I want SPECIFY a schname in
  # some other WAY (similar to what I have in other programs)
  if(is.null(schname)) stop("pgUpdate schname can not be null")
  #
  if(is.null(df)) stop("pgUpdate df can not be null")

  if(is.null(AppendConditions)) AppendConditions <- c("LRvaluesDiff", "LnaRvalue")

  if(is.null(Dots[["prepare.query"]])) {
    prepare.query <- FALSE
  } else {
    prepare.query = Dots[["prepare.query"]]
  }

  if(NROW(df) == 0) {
     message("pgUpdate df has zero(0) rows.  Nothing is to do . . . ")
     return(invisible())
  }

  oldData <- pgOldData(con, trgt = trgt, keys = keys, schname = schname, df = df, varHint = varHint, valHint = valHint, IntentionFor = "UPDATE")


  ## BEGINNING OF PGINSERT/PGUPDATE
  # Server DB column names
  serverDBColumns <-  pgListSchemaTableColumns(con, schname = schname, tblname = trgt)

  # Note, this 'appends' heavily assumes that the
  # columns datatypes are the same (or compatiable) (UNCHECKED)

  # column names ORDER are the same (CHECKED)
  # RPostgreSQL::dbWriteTable C code, caroline::dbWriteTable2

  # garantee order of the df columns
  # from the original Server DB columns or oldData
  # AND
  # chopVectorExcess = TRUE
  ColsAloneINoldData <- setdiff(colnames(oldData), serverDBColumns)
  if(length(ColsAloneINoldData)) {
    warning("pgUpdate found ColsAloneINoldData that do not exist in the server\n")
    warning(stringr::str_c("  Will not trying to put them into the database here: ", ColsAloneINoldData, "\n"))
  }                          # order garantee                                                             # not too many columns
  oldData <- oldData[, customSorting(colnames(oldData), serverDBColumns, sortVectorExcess = FALSE, chopVectorExcess = TRUE), with=FALSE]

  newData <- data.table::data.table(df[, , drop = FALSE], key=keys)
  ## END OF PGINSERT/PGUPDATE
  ColsAloneINnewData <- setdiff(colnames(newData), serverDBColumns)
  if(length(ColsAloneINnewData)) {
     warning("pgUpdate found ColsAloneINnewData that do not exist in the server\n")
     warning(stringr::str_c("  Will not trying to put them into the database here: ", ColsAloneINnewData, "\n"))
  }                          # order garantee                                                             # not too many columns
  newData <- newData[, customSorting(colnames(newData), serverDBColumns, sortVectorExcess = FALSE, chopVectorExcess = TRUE), with=FALSE]



  if(schname != "") { dotSchemaQuoted <- stringr::str_c(DBI::dbQuoteIdentifier(con, schname), ".") } else { dotSchemaQuoted <- "" }
  schemaTrgtTableQuoted <-  stringr::str_c(dotSchemaQuoted, DBI::dbQuoteIdentifier(con, trgt))

  # match to the common key and
  # determine columns ( to later test for "changed" data)
  # keys can not be null (if keys are null then no join is produced)
  mergedData <- merge(oldData, newData, sort = FALSE) # , by = keys # defaults to the shared key
  if(NROW(mergedData) == 0) {
    message(stringr::str_c("pgUpdate targeting table: ", schemaTrgtTableQuoted, " merge, did not find any common values. Nothing to update . . . "))
    return(invisible())
  }
  # ignores key columns
  # ignores [extra] columns not both present in the
  #   'results of the SELECT from the DB server' and determined by the column names of data.frame (df)
  MatchingXCols     <- stringr::str_subset(colnames(mergedData), "[.]x$")
  MatchingColsRoots <- stringr::str_replace(MatchingXCols, "[.]x$", "")
  if(!length(MatchingColsRoots)){
    message(stringr::str_c("pgUpdate targeting table: ", schemaTrgtTableQuoted, " MatchingColsRoots, did not find any common columns. Nothing to update . . ."))
    return(invisible())
  }

  # prepare to process by row
  Splits   <- seq_len(NROW(mergedData))
  Splitted <- split(mergedData, f = Splits)

  # create UPDATE statements commons
  UpDateStmts <- vector(mode = "character")
  UpDateTarget <- stringr::str_c("UPDATE ", schemaTrgtTableQuoted, " trg ", " SET ")

  tempTable       <- stringr::str_c(trgt, "_src")
  tempTableQuoted <- DBI::dbQuoteIdentifier(con, tempTable)
  UpDateFrom  <- stringr::str_c(tempTableQuoted, " val ")
  UpDateWhere <-  vector(mode = "character")
  if(length(keys)) {
    UpDateWhere <- stringr::str_c(stringr::str_c("trg.",DBI::dbQuoteIdentifier(con, keys)), " = ", stringr::str_c("val.", DBI::dbQuoteIdentifier(con, keys)), collapse = " AND ")
  }

  # create UPDATE statements rule specifics
  UpDateStmts   <- vector(mode = "character")
  for(spl in Splitted) {
    UpDateSetColl <- vector(mode = "character")
    for(nm in MatchingColsRoots){
      LValue = stringr::str_c(nm,".x")
      RValue = stringr::str_c(nm,".y")
          # both sides have a value and different from each other
      if( (any(AppendConditions %in% "LRvaluesDiff") && ( !is.na(spl[[LValue]]) && !is.na(spl[[RValue]]) && (spl[[LValue]] != spl[[RValue]]) )) ||
          # left side does not have a value # right side does have a value
          (any(AppendConditions %in% "LnaRvalue") && (  is.na(spl[[LValue]]) && !is.na(spl[[RValue]]) )) ||
          # always append (ideal for small data that rarely changes (and may be hard to track) )
          # both sides values do not matter. Always append
          (any(AppendConditions %in% "Always")) # NOTE: FORGOT TO WRITE CODE: is.na(x) -> NULL
      ) {
        UpDateSet   <- stringr::str_c(DBI::dbQuoteIdentifier(con, nm), " = ", "val.", DBI::dbQuoteIdentifier(con, nm), " ")
        UpDateSetColl <- c(UpDateSetColl,UpDateSet)
      }
    }
    if(length(UpDateSetColl)) {
      keyvals <- do.call(c,plyr::llply(keys, function(x) { spl[[x]] })) # POSTGRESQL LIBERAL LETS US SELECT 'X' integer.
      names(keyvals) <- keys # names not used  # SHOULD BE A LIST BECAUSE OF TWO DATATYPES
      keyvals <- if(!is.numeric(keyvals)) DBI::dbQuoteString(con, as.character(keyvals))
      UpDateWhereExactly <- stringr::str_c(stringr::str_c("trg.", DBI::dbQuoteIdentifier(con, keys)), " = ", keyvals, collapse = " AND ")
      UpdateFromWhere <- stringr::str_c(" FROM ", UpDateFrom, " WHERE ", UpDateWhere, " AND ", UpDateWhereExactly)
      UpDateSets <- stringr::str_c(UpDateSetColl, collapse = ", ")
      UpDateStmt <- stringr::str_c(UpDateTarget, UpDateSets, UpdateFromWhere, "; ")
      UpDateStmts <- c(UpDateStmts, UpDateStmt)
    }
  }

  if(length(UpDateStmts)) {

    colClasses <- pgDFColClasses(newData)

    # would prefer less disk I/O so I would prefer to create a TEMPORARY table
    # R package Postgre can do that in dbWriteTable
    # but authose chose to have each statement prepared (so update statements may be slow )
    if(prepare.query) {
      if(inherits(con, "PostgreSQLConnection")) {
        con2 <- try( {DBI::dbConnect(RPostgres::Postgres(max.con = 100),
          user     = pgCurrentUser(con)[[1]],
          dbname   = pgCurrentDB(con)[[1]],
          port     = pgCurrentPort(con)[[1]],
          hostaddr = pgCurrentHostAddress(con)[[1]],
          password =       if(!is.null(Dots[["password"]]))    { Dots[["password"]] }
                      # would still! get this from the system environment, but I will be explicit
                      else if(nchar(Sys.getenv("PGPASSWORD"))) { Sys.getenv("PGPASSWORD") }
                      else { NULL } # elsewhere
        )}, silent = TRUE)
      }
      if(inherits(con2, "try-error")) {
        # doTemp <- ""
        rm(con2)
      } else if(inherits(con2, "PqConnection")) {
        # doTemp <- "TEMP"
        con <- con2; rm(con2)
        pgSetCurrentSearchPath(con, schname)
      } else {
        stop("pgUpdate could not make a new late connection")
      }
    }

    ddl <- stringr::str_c("CREATE TEMP TABLE ", tempTableQuoted ,"(", stringr::str_c( DBI::dbQuoteIdentifier(con, names(colClasses)), " ", colClasses, collapse = ", "), ");")
    DBI::dbExecute(con, ddl)

    if(any(names(colClasses) %in% keys)) {
      ddl <- stringr::str_c("ALTER TABLE ", tempTableQuoted,
                    " ADD PRIMARY KEY ( ", stringr::str_c(DBI::dbQuoteIdentifier(con, names(colClasses)[names(colClasses) %in% keys]), collapse = ", "), ")",
                    ";")
      DBI::dbExecute(con, ddl)
    }

    if(inherits(con, "PostgreSQLConnection")) {
      # so dbExistsTable, will return TRUE and NOT CREATE a foreground table
      # showMethods("dbExistsTable", includeDefs = TRUE, inherited = TRUE)
      # so PostgreSQLConnection
      writetempTable <- c(pgCurrentTempSchema(con)[[1]], tempTable)
    }
    if(inherits(con, "PqConnection")){
      writetempTable <- tempTable
    }

    # RPostgreSQL PostgreSQLConnection class connecion "temporary" parameter is ignored
    # RPostgre    PqConnection class connection        "temporary" is used
    # writetempTable is 'not quoted' because executed in the
    # string value part of dbExistsTable and in "C code"
    DBI::dbWriteTable(con, writetempTable, newData[, c(keys, MatchingColsRoots), with=FALSE] , row.names = FALSE, overwrite = FALSE, append = TRUE, temporary = TRUE)

    DBI::dbBegin(con)
    ### BIG UPDATE SQL HERE ### message(stringr::str_c(stringr::str_split(stringr::str_c(UpDateStmts, collapse = ""), ";\\s+")[[1]], ";\n"))
    # RPostgre PqConnection class connection SUCKS (What are they thinking?)
    # Error in result_create(conn@ptr, statement) :
    # Failed to prepare query: ERROR:  cannot insert multiple commands into a prepared statement
    if(inherits(con, "PqConnection")) {
      plyr::llply(UpDateStmts, function(x) { DBI::dbExecute(con, x) } )
    }
    if(inherits(con, "PostgreSQLConnection")) {
      UpDateStmts <- stringr::str_c(UpDateStmts, collapse = "")
      DBI::dbExecute(con, UpDateStmts)
    }
    DBI::dbCommit(con)
    DBI::dbExecute(con, stringr::str_c("DROP TABLE ", tempTableQuoted, ";"))
  } else {
    message(stringr::str_c("pgUpdate targeting table: ", schemaTrgtTableQuoted, " using df, did not find any eligible updates. Nothing to update . . ."))
  }
  invisible()

})}


#' get PostgreSQL host address
#'
#' Find the host name and port using PSQL commands
#' https://stackoverflow.com/questions/5598517/find-the-host-name-and-port-using-psql-commands
#'
#' @rdname pgCurrent
#' @export
pgCurrentHostAddress <- function(con) { oneColumn(con, "SELECT inet_server_addr();", "CurrentHostAddress") }

#' get PostgreSQL host address
#'
#' Find the host name and port using PSQL commands
#' https://stackoverflow.com/questions/5598517/find-the-host-name-and-port-using-psql-commands
#'
#' @rdname pgCurrent
#' @export
pgCurrentPort <- function(con) { oneColumn(con, "SELECT inet_server_port();", "CurrentPort") }


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
#' @importFrom stringr str_c
#' @importFrom DBI dbExecute
pgSetCurrentSearchPath <- function(con, path) { DBI::dbExecute(con, stringr::str_c("SET SEARCH_PATH TO ", path,";")) }



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



#' custom sorting a vector
#'
#' @rdname customSorting
#' @export
customSorting <- function(x, ...) UseMethod("customSorting")

#' custom sorting a vector
#'
#' @rdname customSorting
#' @export
customSorting.default <- function(x, ...) stop("No customSorting S3 method found")

#' custom sorting a vector
#'
#' excess Vector elements are appended to the end ( sort or not sort, CI sort or CS sort )
#' other elements found in InitOrder that are 'not found in Vector' are ignored
#'
#' @param x vector  to be sorted
#' @param InitOrder starting custom sorting ( without the excess )
#' @param CI FALSE(default) whether or not Vector excess items that are
#' not found in InitOrder  are sorted 'not case insensitive'(TRUE) or
#' ncase sensitive'(FALSE).  Sorting is done by "lower()"
#' @param sortVectorExcess TRUE(default) whether or not Vector excess columns
#' are attempted to be sorted (TRUE) or not attempted to be sorted (FALSE)
#' @param chopVectorExcess FALSE(default) whether or not excess (x) elements
#' not found in InitOrder are removed
#' @param ... dots passed
#' @return vector sorted by InitOrder
#' @references
#' \cite{Custom Sorting in R \url{https://stackoverflow.com/questions/23995285/custom-sorting-in-r}}
#' @references
#' \cite{Case insensitive sort of vector of string in R \url{https://stackoverflow.com/questions/29890303/case-insensitive-sort-of-vector-of-string-in-r}}
#' @rdname customSorting
#' @examples
#' \dontrun{
#'
#' customSorting( c("a","v", "E2", "c","l", "e3" ,"h","o","date"),
#'    InitOrder = c("date", "o", "h", "l", "c", "v", "a"), CI = TRUE
#' )
#' [1] "date" "o"    "h"    "l"    "c"    "v"    "a"    "E2"   "e3"
#'
#' customSorting(c("E","B","C","D","A"), c("D","B","C"), sortVectorExcess = FALSE)
#' [1] "D" "B" "C" "E" "A"
#'
#' # excess(Vector)  "G", "F"
#'
#' # customSorting(c("G", "D","B","C", "F"), c("E","B","C","D","A"))
#' [1]"B" "C" "D" "F" "G"
#'
#' customSorting(c("G", "D","B","C", "F"), c("E","B","C","D","A"), sortVectorExcess = FALSE)
#' [1] "B" "C" "D" "G" "F"
#'
#' # other(InitOrder) ignored "F"
#' customSorting(c("E","B","C","D","A"), c("F", "D","B","C"), sortVectorExcess = FALSE)
#' [1] "D" "B" "C" "E" "A"
#'
#' }
#' @export
customSorting.character <- function(x, InitOrder, CI = FALSE, sortVectorExcess = TRUE, chopVectorExcess = FALSE, ...) {

  Vector <- x
  # will reduce to vector
  Vector <- as.vector(Vector)
  InitOrder <- as.vector(InitOrder)
  # custom sorting
  VectorLevels <- InitOrder
  # will reduce to vector
  VectorExcess <- setdiff(Vector, VectorLevels)
  if(CI == FALSE) {
    if(sortVectorExcess) {
      VectorExcessCS <-   sort(VectorExcess)
    } else {
      VectorExcessCS <-        VectorExcess
    }
    VectorExcessCaseDetermined <- VectorExcessCS
  } else {
    if(sortVectorExcess) {
      VectorExcessCI <-   VectorExcess[order(tolower(VectorExcess))]
    } else {
      VectorExcessCI <-   VectorExcess
    }
    VectorExcessCaseDetermined <- VectorExcessCI
  }
  VectorExcessCaseDeterminedLevels <- c(VectorLevels, VectorExcessCaseDetermined)
  VectorFactor <- factor(Vector, levels = VectorExcessCaseDeterminedLevels)
  Vector <- Vector[order(VectorFactor)]
  Vector <- as.vector(Vector)
  if(chopVectorExcess)
    Vector <- Vector[!Vector %in% VectorExcessCaseDetermined]
  Vector

}
#' custom sorting a vector
#'
#' @rdname customSorting
#' @examples
#' \dontrun{
#' customSorting(c(7,4,2,3,6), c(5,2,3,4,1))
#' [1] 2 3 4 6 7
#' }
#' @export
customSorting.numeric <- customSorting.character

#' custom sorting a vector
#'
#' @rdname customSorting
#' @examples
#' \dontrun{
#' # Date examples
#'
#' customSorting(zoo::as.Date(6:3), zoo::as.Date(1:4))
#' [1] "1970-01-04" "1970-01-05" "1970-01-06" "1970-01-07"
#'
#' customSorting(zoo::as.Date(6:3), zoo::as.Date(1:4), sortVectorExcess = FALSE)
#' [1] "1970-01-04" "1970-01-05" "1970-01-07" "1970-01-06"
#'
#' customSorting(zoo::as.Date(6:3), zoo::as.Date(1:4), sortVectorExcess = FALSE, chopVectorExcess = TRUE)
#' [1] "1970-01-04" "1970-01-05"
#' }
#' @importFrom zoo as.Date
#' @export
customSorting.Date <- function(x, InitOrder, ...) {

  x <- as.numeric(x)
  InitOrder <- as.numeric(InitOrder)
  x <- customSorting(x, InitOrder = InitOrder, ...)
  zoo::as.Date(x, ...)
}

#' custom sorting a vector
#'
#' @rdname customSorting
#' @importFrom zoo yearmon
#' @export
customSorting.yearmon <- function(x, InitOrder, ...) {

  x <- as.numeric(x)
  InitOrder <- as.numeric(InitOrder)
  x <- customSorting(x, InitOrder = InitOrder, ...)
  zoo::as.yearmon(x, ...)
}

#' custom sorting a vector
#'
#' @rdname customSorting
#' @importFrom zoo yearqtr
#' @export
customSorting.yearqtr <- function(x, InitOrder, ...) {

  x <- as.numeric(x)
  InitOrder <- as.numeric(InitOrder)
  x <- customSorting(x, InitOrder = InitOrder, ...)
  zoo::as.yearqtr(x, ...)
}

#' custom sorting a vector
#'
#' @rdname customSorting
#' @export
customSorting.POSIXct <- function(x, InitOrder, ...) {

  x <- as.numeric(x)
  InitOrder <- as.numeric(InitOrder)
  x <- customSorting(x, InitOrder = InitOrder, ...)
  as.POSIXct(x, origin = "1970-01-01", ...)

}

#' custom sorting a vector
#'
#' @rdname customSorting
#' @importFrom chron as.chron
#' @export
customSorting.chron <- function(x, InitOrder, ...) {

  xA <- attributes(x)
  attributes(x) <- NULL
  x <- as.numeric(x)
  InitOrder <- as.numeric(InitOrder)
  x <- customSorting(x, InitOrder = InitOrder, ...)
  attributes(x) <- xA
  x
}

#' custom sorting a vector
#'
#' class 'timeSeries' does not have a customSorting implementation
#'
#' @rdname customSorting
#' @importFrom timeSeries as.timeSeries
#' @export
customSorting.timeSeries <- function(x, InitOrder, ...) {

  stop("timeSeries does not have a customSorting implementation")

}



#' Philadelphia Fed Survey of Professinal Forecasters Release Dates
#'
#' About the date/time index . . .
#'
#' Deadline and Release Dates for the Survey of Professional Forecasters
#' True deadline and news release dates for surveys prior to 1990:Q2 are not known.
#'
#' *The 1990Q2 survey was not taken in real time, because the Philadelphia Fed
#' had not yet taken over the survey. Forecasters were asked to provide dated
#' forecasts from May 1990.
#'
#' **The 1996Q1 survey was delayed because of the federal government shutdown,
#' which in turn delayed the release of government statistical data.
#'
#' ***The 2013Q4 survey was delayed because of the federal government shutdown,
#' which in turn delayed the release of government statistical data.
#'
#' @param env where to create objects. (.GlobalEnv)
#' @param MaxAge "4 hours"(default) is longest age allowed, such that the retrieving the
#' the MOST RECENTLY PUBLISHED FORECASTERS RELEASE DATES FILE from  the local fileystem
#' will be allowed to be done. If the MaxAge is exceeded then,
#' the most recent release dates file is refreshed anew from the Philadelphia Fed site.
#' The format uses as.difftime: "# secs", "# mins", "# hours", "# days", "# weeks"
#' @param force FALSE(default) re-download data from the Philadelphia Fed ( See the examples. )
#' Generally, using this parameter "force = TRUE"
#' is NOT necessary: after MaxAge has been exceeded and if a query needs the
#' MOST RECENTLY PUBLISHED FORECASTERS RELEASE DATES FILE then this file will be downloaded anew.
#' @param ... dots passed to quantmod___try.download.file
#' @return data.frame of YearQtr (yearqtr) TrueDeadlineDate (Date) ReleaseDate (Date)
#' @references
#' \cite{Survey of Professinal Forecasters Release Dates \url{https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/spf-release-dates.txt?la=en}}
#' @examples
#' \dontrun{
#' ForecastersReleaseDates()
#' ForecastersReleaseDates(force = TRUE)
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_replace_all str_detect str_c
#' @importFrom DataCombine FillDown VarDrop MoveFront
#' @importFrom zoo as.Date as.yearmon
#' @export
ForecastersReleaseDates <- function(env = parent.frame(), MaxAge = NULL, ...) {
tryCatchLog::tryCatchLog({
initEnv(); on.exit({uninitEnv()})

  this.env <- environment()
  for (var in names(list(...))) {
      assign(var, list(...)[[var]], this.env)
  }

  if(!hasArg("verbose"))
    verbose <- FALSE

  if(is.null(MaxAge)) MaxAge <- "4 hours"
  MaxAgeValueUnits <- strsplit(MaxAge, " ")[[1]]
  MaxAgeValue <- as.integer(MaxAgeValueUnits[1])
  MaxAgeUnits <- MaxAgeValueUnits[2]

  # force
  if(!exists("force", envir = this.env, inherits = FALSE))
      force = FALSE

  # maybe get from temp
  if(exists(".ForcastersReleaseDates_path2file", envir = env, inherits = FALSE) & !force) {
      assign("destfile", get(".ForcastersReleaseDates_path2file", envir = env, inherits = FALSE), envir = this.env, inherits = FALSE)
      if(file.exists(destfile)) {
        updated <- file.info(destfile)$mtime # is.na - if the file does not exist
        AgeTestTooOld <- as.difftime(MaxAgeValue,  units = MaxAgeUnits) <  difftime(Sys.time(), updated)
        if(!AgeTestTooOld) ReleaseDates <- readRDS(destfile)
          return(ReleaseDates)
      }
  }

  # get
  url <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/spf-release-dates.txt?la=en"
  tmp <- tempfile()
  quantmod___try.download.file(url, destfile = tmp, quiet = !verbose, mode = "wb", ...)
  RawData <- readLines(tmp)

  # do not remove the 'tmp' file
  # In this R session, keep the file around and available for the next query [if any]
  # the site https://www.philadelphiafed.org is NOT engineered
  # to handle MANY data queries, NOR denial of service attacks

  # format
  GoneAsk    <- stringr::str_replace_all(RawData, "\\*", " ")
  GoneTabs   <- stringr::str_replace_all(GoneAsk, "\t", "    ")
  GoneExcess <- GoneTabs[stringr::str_detect(GoneTabs, "^\\d{4} Q\\d|^     Q\\d")]
  ReadyRead <- stringr::str_replace_all(GoneExcess, "^    ", "0000")

  # extract
  read.table(textConnection(ReadyRead)
             , col.names  = c("Year"     ,"Quarter"  ,"TrueDeadlineDate","ReleaseDate")
             , colClasses = c("character","character","character"         , "character")
             , fill = TRUE # unequal length rows
             , stringsAsFactors = FALSE
  ) -> ReleaseDates

  # prepare for yearqtr
  ReleaseDates[ReleaseDates[["Year"]] == "0000", "Year"] <- NA_character_
  ReleaseDates <- DataCombine::FillDown(ReleaseDates, Var = "Year")

  # yearqtr
  ReleaseDates[["YearQtr"]] <- zoo::as.yearqtr(stringr::str_c( ReleaseDates[["Year"]], " ", ReleaseDates[["Quarter"]]), format = "%Y Q%q")
  ReleaseDates <- DataCombine::VarDrop(ReleaseDates, Var = c("Year","Quarter"))
  ReleaseDates <- DataCombine::MoveFront(ReleaseDates, Var = "YearQtr")

  # Dates
  ReleaseDates[["TrueDeadlineDate"]] <- zoo::as.Date(ReleaseDates[["TrueDeadlineDate"]], format = "%m/%d/%y")
  ReleaseDates[["ReleaseDate"]] <- zoo::as.Date(ReleaseDates[["ReleaseDate"]], format = "%m/%d/%y")

  # persistent
  destfile <- stringr::str_c(tempdir(),"/","ForcastersReleaseDates.rds")
  saveRDS(ReleaseDates, file = destfile)
  assign(".ForcastersReleaseDates_path2file", destfile, envir = env, inherits = FALSE)

  ReleaseDates

})}



#' apply data.table aggregator on subsets of rows
#'
#' @param x data.table to rung the aggrcation
#' @param Fun (default none: required) function to pass
#' @param By column to aggregate upon
#' @param ... dots passed to data.table aggregator
#' and to to Fun.  Also, FunStr may be passed a flag to signal
#' not to run match.call(Fun). match.call is only
#' ever allowed to run once on Fun. (R language)
#' Also, DescTools::DoCall only returns
#' as.character(substitute(Fun)) == "Fun"
#' @return data.table of aggregates
#' @references
#' \cite{period.apply on xts object takes about 50x longer than running it on it's core data and recasting. #278 \url{https://github.com/joshuaulrich/xts/issues/278}}
#' @references
#' \cite{[R-Forge #5754] GForce functions and row- + col-wise operations on .SD #523 \url{https://github.com/Rdatatable/data.table/issues/523}}
#' @examples
#' \dontrun{
#' MyFun <- function(x, Fun, By, ...) {
#'
#'   FunStr <- as.character(substitute(Fun))
#'   Fun    <- match.fun(Fun)
#'   Dots <- list(...)
#'   DescTools::DoCall(applyAggregateDT, c(list(), list(x), Fun = Fun, FunStr = FunStr, By = By, Dots))
#'
#' }
#' MyFun(as.data.table(mtcars[,c("cyl", "mpg", "disp")]), Fun = mean, By = "cyl", na.rm = TRUE)
#'
#'}
#' @importFrom tryCatchLog tryCatchLog
#' @import data.table
#' @importFrom data.table setDT
#' @importFrom plyr llply
#' @export
applyAggregateDT <- function(x, Fun, By, ...) {
tryCatchLog::tryCatchLog({
initEnv(); on.exit({uninitEnv()})

  Dots <- list(...)
  if(!is.null(Fun) && is.null(Dots[["FunStr"]])) {
     # I am not allowed to call this TWICE
     FunStr <- as.character(substitute(Fun))
     Fun    <- match.fun(Fun)
  }
  if(!is.null(Dots[["FunStr"]]) && !exists("FunStr", envir = environment(), inherits = FALSE))
    FunStr <- Dots[["FunStr"]]

  data.table::setDT(x, key = By)
  # mutithreaded data.table
  # I do not understand what is going on
  # May want to change back from plyr::llaply to lapply
  x <- x[, plyr::llply(.SD, .fun = Fun, ...), by = By]
  x
})}


#' Recodes values in a vector.
#'
#' R LANGUAGE INTERNAL CRASHING (I JUST GET NAs)
#'
#' Adapted from blockmodeling::recode.
#' Originally enhanced to be intermediary coding item
#' in the processes assignment of xts index time classes from one
#' class to another. If the oldcode and newcode classes
#' are not compatiable (see below)
#' then results are returned as elements in a a list.
#'
#' @param x vector or list
#' @param oldcode vector or list of old codes. Will be made into a vector.
#' @param new2oldcode intermediary vector or list belonging to the realm
#' of newcode but CAN be compared against/to values in oldcode.
#' Will be made into a vector.
#' @param newcode vector or list of new codes  Will be made into a vector.
#'  Will be made into a vector.
#' @param ComepareStrict FALSE(default) Method too choose the check compatibility.
#' if oldcode and new2oldcode are not [exactly] the same class.
#' Assume that if oldcode and new2oldcode inherit any of the other's classes
#' then the two classes are compatatible. Otherwise, a warning will be
#' returned to the user.
#' Strict(TRUE) both oldcode and new2oldcode must have the exact same class.
#' Otherwise, a warning will be returned to the user.
#' @param AssnStrict FALSE(default) Method too choose the check compatibility.
#' if x and newcode are not [exactly] the same class.
#' Assume that if x and newcode inherit any of the other's classes
#' then the two classes are compatatible. Results will be attempted
#' to be returned as a vector.
#' Strict(TRUE) both x and newcode must have the exact same class.
#' Results will be attempted to be returned as a vector.
#' Otherwise, the results will be attempted be returned as a list.
#' :NOTE: if not all the 'x' elements are attemped to be recoded then the
#' returned list may be composed of elements of different classes.
#' @examples
#' \dontrun{
#'}
#' @importFrom plyr llply
#' @export
ReCode <- function (x, oldcode = sort(unique(x)), new2oldcode = NULL, newcode, ComepareStrict = FALSE, AssnStrict = FALSE) {
tryCatchLog::tryCatchLog({
initEnv(); on.exit({uninitEnv()})

  if(is.null(new2oldcode) && (length(oldcode) != length(newcode)))
    stop("The number of old and new codes do not match")
  # pass through
  if(!length(oldcode)) {
    message("Recode: oldcode has no data, so nothing to do.")
    return(x)
  }
   # list with element of mixed classes
   # Note: R does NOT have sublist "list[i]" comparison methods
   # but elements could be compared with list[[i]] == list[[j]]
   # Coding could be cumberson: runtime could be long: check
   # each element for eaches class membership (however, this could/can be done.)

  if(is.list(oldcode)) {
     # peek
     OldClass <- class(oldcode[[1]])
     oldcode <-  do.call(c, oldcode)
     if(OldClass != class(oldcode)) stop("oldcode that is list can not have mixed class members")
  }

  ComepareStrictCompat  <- vector(mode = "logical")
  if(!is.null(new2oldcode)) {

    if(is.list(new2oldcode)) {
      # peek
      New2OldClass    <- class(new2oldcode[[1]])
      new2oldcode     <- do.call(c, new2oldcode)
    } else {
      New2OldClass    <- class(new2oldcode)
    }
    if(New2OldClass != class(new2oldcode)) stop("new2oldcode that is list can not have mixed class members")
    # is comparison allowed? ( a "better" check is "is there and Ops Method?" )
    if(!ComepareStrict) {
      if(
         any(do.call(c, plyr::llply(class(new2oldcode), function(x2) { inherits(oldcode, x2) }))) |
         any(do.call(c, plyr::llply(class(oldcode), function(x2)    { inherits(new2oldcode, x2) })))
       ) {
         ComepareStrictCompat <- TRUE
       } else {
         ComepareStrictCompat <- FALSE
         warning("new2oldcode and oldcode must inhertit from one of the classes frome each other")
       }
    }
    if(ComepareStrict) {
      if(class(new2oldcod) != class(oldcode)) {
        ComepareStrictCompat <- FALSE
        warning("new2oldcode and oldcode must have the same class")
      } else {
        ComepareStrictCompat <- TRUE
      }
    }
  }
  if(is.list(newcode)) {
    # peek
    NewClass <- class(newcode[[1]])
    newcode <-  do.call(c, newcode)
    if(NewClass != class(newcode)) stop("newcode that is list can not have mixed class members")
  }

  # the assignment to a vector
  AssnclassCompat <- vector(mode = "logical")

  # is assignment allowed?
  # inherit from any class membership
  if(!AssnStrict){
    if(
        any(do.call(c, plyr::llply(class(x), function(x2)       { inherits(newcode, x2) }))) |
        any(do.call(c, plyr::llply(class(newcode), function(x2) { inherits(x, x2) })))
    ) {
      AssnclassCompat <- TRUE
    } else {
      AssnclassCompat <- FALSE
    }
  }
  if(AssnStrict) {
    if(class(x) != class(newcode)) {
      AssnclassCompat <- FALSE
    } else {
      AssnclassCompat <- TRUE
    }
  }

  # can not assign to a vector (so instead assign to a list)
  if(!AssnclassCompat)
     x <- as.list(x)

   if(!is.null(new2oldcode)) {
    y <- new2oldcode
  }  else {
    y <- x
  }

  newx <- x
  if(is.list(y)) y <- do.call(c,y)
  #    y and newcode have the same numbers and are the source
  # newx and oldcode have the same numbers and are the target
  # R LANAGUAGE INTERNAL CRASHING: I JUST GET ALL "NA"s
  for (i in seq_along(newx)) {
    newx[oldcode[i] == y] <- newcode[i]
  }
  return(newx)
})}


#' number of days since Last Published
#'
#' The time between the "Last Updated Date" and the "index" date 'published date'
#' can be important in "model" building.
#' This amount of time can impact the quality of
#' the observations.  Therefore, the "model" building program needs
#' to know about the number of days since the "Last Updated Date."
#'
#' First:
#' Of the data of Yahoo finance, the "Last Updated Date" and
#' the index date 'published date' are the same day. quantmod::getSymbols.yahoo
#' needs only to return the data and index.
#' The St. Louis FRED's "Last Updated Date" and index date 'published date'
#' are different days.
#'
#' Second: Of the data of Yahoo finance, the Frequency
#' is "Daily".  The St. Louis FRED's "Frequency" can be"Quarterly",
#' "Monthly", "Weekly", or "Daily"
#'
#' From a timeseries returned by getSymbols.FRED2.
#' The xts attribute "Last_Updated" is used.
#'
#' Used is To.Monthly to convert the series to to a "Month-like" series.
#' This is typical in makeing long term predictions.
#'
#' The xts is returned with an extra column added..
#'
#' The author put some thought had been put into 'instead querying
#' ALFRED', but that path choice utimately was not taken.
#'
#' @param x xts object returned by getSymbols.FRED2 or
#' an xts object with an attribute of "Last_Updated" and "Frequency".
#' Or neither ( These values can be entered Manually).
#' @param AsOfToday Sys.Date() (default).
#' Otherwise overrides; POSIXct, Date or character("YYYY-MM-DD")
#' Date of refrence. Reality: today or date of run of the "modelling" program.
#' Note, choosing any earlier date does remove generated "Last Updated Dates"
#' However, choosing a later date does not add "Lst Updated Dates"
#' @param calcMonthlies TRUE(default) convert x (xts) object to an
#' end of month (eom) monthly (using To.Monthly) before
#' determining  the 'true last updated' dates.
#' Otherwise, FALSE; means 'x is already an eom monthly so do not apply To.Monthly.
#' @param Last_Updated xts attribute "Last_Updated"(default).
#' Otherwise overrides; POSIXct, Date or character("YYYY-MM-DD")
#' @param CalendarAdj "UnitedStates/GovernmentBond"(default).
#' To prevent 'last adjusted dates' from landing on an
#' unreasonable weekend, adjust the day forward to the next
#' US Government working day (e.g St. Louis FRED data).
#' Otherwise, "any other RQuantLib calender", e.g. "UnitedStates/NYSE".
#' The choice depends on the past schedule of the data. "NONE' meaning
#' no adjustment. See "?? RQuantLib::Calendars".
#' @param FixLastLastTrueUpDated TRUE(default). Of the current
#' month (determined by AsOfToday), the program calculates the
#' last day of the month then determines the its "Last Updated Date"
#' This 'date' may need custom adjustment.  For the hopefully one
#' Monthies date greater than today, adjust the last updated date,
#' to be calcualted using AsOfToday instead of the the last day of
#' the month. This is meant to corrrect the accuracy of the last
#' observation. This is meant to keep the usefulness of the
#' quantmod workflow of having the last observation be available to
#' be used to make a prediction.  Otherwise FALSE; useful
#' when no no new data is expected to be added between
#' now(AsOfDate) and month's end.
#' @param NaCol FALSE(Default). In for example, "Quarterly" data,
#' To.Monthy, will create gaps (observation rows with no data).
#' Create a new column and fill in the gaps with twos(2) and
#' the non-gaps with ones(1).
#' @param NaLOCF TRUE(Default). In for examle, "Quarterly" data,
#' To.Monthy, will create gaps (observation rows with no data).
#' Fill in the empty observations with the last(past.known data).
#' @param Frequency xts attribute "Frequency"(default) of Last_Updated.
#' Otherwise; character overrides.
#' Values can be "Quarterly". Currently, "Monthly", "Weekly", and "Daily".
#' Other Frequencies are not yet implmented.
#' @param mkAsOfToday2LastObs FALSE(default) Sets the hopefully the last
#' observation's xts index value to be today's date(AsOfToday)
#' instead of the end of month date. Generally,
#' mkAsOfToday2LastObs == TRUE is more trouble that it is worth.
#' @return xts object with a new column of show the delay since last update.
#' @examples
#' \dontrun{
#'
#' # "Quarterly" time series on the St. Louis FRED
#' GDP <- getSymbols("GDP", src = "FRED2", auto.assign = FALSE)
#' GDP_DLY <- fancifyXts(GDP)
#'
#' # "Monthly" time series on the St. Louis FRED
#' PAYEMS <- getSymbols("PAYEMS", src = "FRED2", auto.assign = FALSE)
#' PAYEMS_DLY <- fancifyXts(PAYEMS)
#'
#' # "Weekly" time series on the St. Louis FRED
#' TREAST <- getSymbols("TREAST", src = "FRED2", auto.assign = FALSE)
#' TREAST_DLY <- fancifyXts(TREAST)
#'
#' # "Daily" time series on the St. Louis FRED
#' DGS3MO <- getSymbols("DGS3MO", src = "FRED2", auto.assign = FALSE)
#' DGS3MO_DLY <- fancifyXts(DGS3MO)
#'
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom zoo as.Date
#' @importFrom DescTools AddMonths DoCall
#' @importFrom Hmisc truncPOSIXt
#' @importFrom xts lag.xts
#' @importFrom stringr str_c str_split
#' @importFrom RQuantLib adjust
#' @importFrom DataCombine MoveFront
#' @export
fancifyXts <- function(x, AsOfToday = NULL, calcMonthlies = NULL,
                          Last_Updated = NULL, CalendarAdj = NULL,
                          FixLastLastTrueUpDated = NULL, NaCol = NULL, NaLOCF = NULL,
                          Frequency = NULL, mkAsOfToday2LastObs = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  xTs <- initXts(x)

  if(NVAR(xTs) > 1) stop("fancifyXts only processes a single column xTs")

  if(is.null(AsOfToday)) { AsOfToday <- Sys.Date() } else { AsOfToday <- zoo::as.Date(AsOfToday) }

  if(is.null(calcMonthlies)) { calcMonthlies <- TRUE }

  if(is.null(Last_Updated)) Last_Updated <- xtsAttributes(xTs)[["Last_Updated"]]
  if(is.null(Last_Updated)) stop("fancifyXts need parameter lastUpdatedDate")

  if(is.null(Last_Updated)) Last_Updated <- xtsAttributes(xTs)[["Last_Updated"]]
  if(is.null(Last_Updated)) stop("fancifyXts need parameter lastUpdatedDate")

  if(is.null(CalendarAdj)) CalendarAdj <- "UnitedStates/GovernmentBond"

  if(is.null(FixLastLastTrueUpDated)) FixLastLastTrueUpDated <- TRUE

  if(is.null(NaCol)) NaCol <- FALSE

  if(is.null(NaLOCF)) NaLOCF <- TRUE

  if(is.null(Frequency)) Frequency <- xtsAttributes(xTs)[["Frequency"]]
  if(is.null(Frequency)) stop("fancifyXts: need parameter Frequency")

  if(is.null(mkAsOfToday2LastObs)) { mkAsOfToday2LastObs <- FALSE }

  if(Frequency == "Quarterly") OftenNess <- 3L # months
  if(Frequency == "Monthly"  ) OftenNess <- 1L

  if((!Frequency %in% c("Quarterly", "Monthly", "Daily")) &&
     (!Frequency %Like% "^Weekly.*$")
   )stop("fancifyXts: Frequency other than  \"Quarterly\", \"Monthly\", \"Weekly\"  and \"Daily\" is not yet implemented.")

  if(Frequency %in% c("Quarterly", "Monthly")) {
    AllPossibleLastUpdatedDates <- c(
      # To be used in "findInterval"
      # go back just "one more" to try to make sure that an early "Last_Updated" date exists.
      DescTools::AddMonths(zoo::as.Date(Last_Updated), rev(- OftenNess * c(seq_len(length(index(xTs)) + 1)))),
      zoo::as.Date(Last_Updated)
    )
  }

  # NOTE: St. Louis FRED's weeklies
  if(Frequency %Like% "^Weekly.*$") {
    AllPossibleLastUpdatedDates <- c(
      # To be used in "findInterval"
      # go back just "one more" to try to make sure that an early "Last_Updated" date exists.
      seq(zoo::as.Date(Last_Updated) - (NROW(xTs) + 5 + 5) * 7, to = zoo::as.Date(Last_Updated), by = "weeks")
      # produces one more 'back in time' observation.   # 5: extra_month_back
      # + 5 ( the month distance )
    )
  }

  # NOTE: St. Louis FRED's dailies
  if(Frequency == "Daily") {
    AllPossibleLastUpdatedDates <- c(
      # To be used in "findInterval"
      # go back just "one more" to try to make sure that an early "Last_Updated" date exists.
      # hack ( generally
      seq(head(index(xTs),1) - 2 - 31, to = zoo::as.Date(Last_Updated), by = "days")
      # produces one more 'back in time' observation.
      # + 1 ( the day distance )   # 31: extra_month_back
    )
  }
  # to not look into the future
  AllPossibleLastUpdatedDates <- AllPossibleLastUpdatedDates[AllPossibleLastUpdatedDates <= AsOfToday]

  Monthlies <- xTs
  if(calcMonthlies) {
    merge(
      To.Monthly(xTs, OHLC = FALSE, indexAt = "firstof"),
      xts( ,
        seq(from =  DescTools::AddMonths(index(To.Monthly(tail(xTs,1), OHLC = FALSE, indexAt = "firstof")),1),
                                                    # beginning of this month         # beginning of next month
            to   = zoo::as.Date(DescTools::AddMonths(Hmisc::truncPOSIXt(zoo::as.Date(AsOfToday), units = "months"),1)),
            by   = "months")
       )
    ) -> Monthlies # could be 'new'
  # end of a month and end of this current month
  index(Monthlies) <- index(Monthlies) - 1
  }
  if(head(index(Monthlies),1) < head(AllPossibleLastUpdatedDates,1))
    stop("fancifyXts: head(index(Monthlies),1) < head(AllPossibleLastUpdatedDates,1); so findInterval will FAIL(create an index of zero(0)")

  if(NaCol) {
    NaColumnlies <- c(coredata(Monthlies))
    NaColumnlies[!is.na(Monthlies)] <- 1
    NaColumnlies[ is.na(Monthlies)] <- 2
  }

  # sanity check
  if(1 < sum(AsOfToday < index(Monthlies))) {
    warning("fancifyXts found more than one(1) future Monthly. Be sure you know what you are doing.")
  }

  # "Daily": left.open = T is important
  True_Last_Updated <- AllPossibleLastUpdatedDates[findInterval(index(Monthlies), AllPossibleLastUpdatedDates, left.open = T, rightmost.closed =  T)]
  if(CalendarAdj != "NONE") {
    # to get the next government/business/other day
    True_Last_Updated <- RQuantLib::adjust(CalendarAdj, True_Last_Updated, 1)
  }
  # to not look into the future
  True_Last_Updated <- True_Last_Updated[True_Last_Updated <= AsOfToday]

  # difference
  DelaySinceLastUpdate <- index(Monthlies) - True_Last_Updated

  # True_Last_Updated date is NOT CORRECT for
  # the hopefully one(1) Monthles greater in time
  # than today(AsOfToday) [in this month]
  # I WANT TO PREDICT ON TODAY(AsOfToday)
  if(FixLastLastTrueUpDated) {
    # NOTE: if MULTIPLE MONTHS (1 < sum(AsOfToday < index(Monthlies)))
    # THEN I WILL have to REPROGRAM in some way.
    # Currently, the situation is not handled.  Only the above warning() is given.
    YearMonMonthliesIndexes <- zoo::as.yearmon(index(Monthlies))
    IndexOfCorrection <-       zoo::as.yearmon(AsOfToday) == YearMonMonthliesIndexes
    DelaySinceLastUpdate[IndexOfCorrection] <-   # subtract off too-last prediction days
      DelaySinceLastUpdate[IndexOfCorrection] - (index(Monthlies)[IndexOfCorrection] - AsOfToday)
  }

  # developed and debugged with "Quarterly" data
  # also works on "Monthly" data (but seems weird)
  # when the "Last Update" Occurred, pull forward Data values to be in the same observation(month)
  MonthsOfLastUpdate <- as.yearmon(index(Monthlies)) == as.yearmon(index(Monthlies) - DelaySinceLastUpdate)
  MonthsOfLastUpdateIndex <- seq_along(index(Monthlies))[MonthsOfLastUpdate]
  LatestMonthOfLastUpdateIndex <- max(MonthsOfLastUpdateIndex)
  #
  MonthOfDataIndex <- c(!is.na(Monthlies))
  MonthsToDataShiftForward <- LatestMonthOfLastUpdateIndex - max(seq_len(NROW(Monthlies))[MonthOfDataIndex])
  Monthlies <- lag.xts(Monthlies, MonthsToDataShiftForward)

  # if also creating this column, then makes sense to 'also' shift forward
  if(NaCol) {
    NaColumnlies <- lag.xts(NaColumnlies, MonthsToDataShiftForward)
  }

  if(NaLOCF) {
    Monthlies <- na.locf(Monthlies)
  }

  if(mkAsOfToday2LastObs) {
    # NOTE: if MULTIPLE MONTHS (1 < sum(AsOfToday < index(Monthlies)))
    # THEN I WILL have to REPROGRAM in some way.
    # Currently, the situation is not handled.  Only the above warning() is given.
    YearMonMonthliesIndexes <- zoo::as.yearmon(index(Monthlies))
    IndexOfCorrection <-       zoo::as.yearmon(AsOfToday) == YearMonMonthliesIndexes
    index(Monthlies)[IndexOfCorrection] <- AsOfToday
  }

  # some sort of smart naming choices
  # first column's characters before
  # the first underscore (or just the columns characters)
  # xTs ONLY USED to get/set column names
  # (I MAY WANT TO CHANGE THIS LATER)
  RootNm <- stringr::str_split(colnames(xTs)[1], "_")[[1]][1]
  newClmList <- list()

  # doing always (currently)
  NewDLYClm <- stringr::str_c(RootNm, "_DLY")
  newClmList[[NewDLYClm]] <- DelaySinceLastUpdate
  # optional
  if(NaCol) {
    NewNAClm <- stringr::str_c(RootNm, "_NA")
    newClmList[[NewNAClm]] <- NaColumnlies
  }
  xTs <- DescTools::DoCall(cbind, c(list(),list(Monthlies), newClmList))
  # xTs ONLY USED to get/set column names
  xTs <- DataCombine::MoveFront(xTs, Var = colnames(xTs)[!colnames(xTs) %in% names(newClmList)])
  xTs

})}



#' economic data from the St. Louis FRED
#'
#' Alternative to the quantmod::getSymbols.FRED
#'
#' This an en extension of quantmod::getSymbols.FRED.
#' Extra attibutes (e.g. Last_Updated, Frequency andothers)
#' are added to the xts object (if returned as an xts object
#' (recommended) and default).
#'
#' The user may sent 'from' and 'to' dates.
#'
#' Throught the 'Edit' parameter, the  user may pass
#' in data transformation formulas.
#'
#' For this apply either of the following must be true.
#' (1) The foruma query paramter formula(fml)
#' has just one veriable "a" and then this Edit transformation
#' is applied across all Symbols sent by the user. Or,
#' (2), if the number of Symbols is two(2) or greater then
#' then the number of symbols must equal to the number
#' of variables in the query paramter e.g. if the Symbols
#' sent are "WILL5000IND","BAMLC0A4CBBBEY" in
#' getSymbols(c("WILL5000IND","BAMLC0A4CBBBEY") then the
#' number of paraters must be two(2) and these are
#' letters of the aphabet starting with "a" and must be
#' lowercase. An example is 'fml="a-b"' (Se The Example.
#'
#' @param Symbols  a character vector specifying the names of each symbol
#' @param env where to create objects. (.GlobalEnv)
#' @param to data start date in format "YYYY-MM-DD"
#' @param from data end date in format  "YYYY-MM-DD"
#' @param Edit list of names items or named character vectors of Edit options.
#' (See the example).
#' @param NewName new name
#' if Symbol(s) are sent with an Edit formula
#' (so the the return result is is just one Symbol), then both the column name
#' and the (if auto.assign = TRUE), variable returned to the environement
#' is renamed to be the same as "NewName". If Edit is sent
#' and no NewName is provided then the default is "FREDQUERY."
#' Ignored otherwise.
#' @param transformReqExtraInfo two(2)+ series Edit transform requires
#' xts attributes of lastest Last_Updated and smalled time interval Frequency
#' @param return.class desired class of returned object.
#' Can be xts, zoo, data.frame, or xts (default)
#' @param ... additional parameters
#' @return A call to getSymbols.FRED2 will load into the specified
#' environment one object for each \code{Symbol} specified,
#' with class defined by \code{return.class}.
#' @author Jeffrey A. Ryan
#' @author Andre Mikulec (adapted original code to work with features)
#' @references
#' \cite{The equity premium \url{https://fredblog.stlouisfed.org/2016/07/the-equity-premium/}}
#' @references
#' \cite{customize the The equity premium [Edit Graph] \url{https://fred.stlouisfed.org/graph/?g=6LsS}}
#' @references
#' \cite{FRED ECONOMIC DATA: FEDERAL RESEARVE BANK OF ST. LOUIS \url{https://fred.stlouisfed.org/}}
#' @references
#' \cite{getSymbols('DTWEXB',src='FRED') broken #209 \url{https://github.com/joshuaulrich/quantmod/issues/209}}
#' @seealso
#' \code{\link{getSymbols}}
#' \code{\link{setSymbolLookup}}
#' @keywords data
#' @examples
#' \dontrun{
#'
#'
#' # reading text tables
#' Text2DF <- function(x) {
#'     read.csv(textConnection(x),
#'      sep = ":",
#'      strip.white = TRUE,
#'      colClasses = "character") -> x
#'      x
#' }
#'
#'
#' # Edit possible query parameters
#'
#' Units: transformation
#' Modify frequency: fq
#' Aggregation method: fam
#' Formula: fml
#' [Final] Units: fgst
#'
#' # <Time Series>: id
#'
#' # Settable 'per' each individual Time Series
#' # Settable when two or more Time Series are chosen
#'
#' # The first time series is (a).
#' # The second time series is (b). Etc. . . .
#' # The final time series the \*result\* of all of the individual
#' # time series and it is  \*not\* the last time series ( e.g. not "z")
#'
#' # Underscores separate each Time Series from each other
#'
#' Id <- vector(mode = "character")
#'
#' # examples
#' # &id=WILL5000IND
#' # &id=WILL5000IND_BAMLC0A4CBBBEY
#'
#' Id <- c("WILL5000IND","BAMLC0A4CBBBEY")
#'
#'
#'
#' # Units: transformation
#' # Settable 'per' each individual Time Series
#' # Underscores separate each Time Series from each other
#'
#' Units_transformation <-
#' "
#'   Code: Description
#'   lin: Index
#'   chg: Change, Index
#'   ch1: Change from Year Ago, Index
#'   pch: Percent Change
#'   pc1: Percent Change from Year Ago
#'   pca: Compounded Annual Rate of Change
#'   cch: Continuously Compounded Rate of Change
#'   cca: Continuously Compounded Annual Rate of Change
#'   nbd: Index (Scale value to 100 for chosen date)
#' "
#' Units_transformation <- Text2DF(Units_transformation)
#'
#' # examples
#' # &transformation=lin
#' # &transformation=cca_pc1
#'
#' Units_transformation <- c("cca","pc1")
#'
#'
#'
#' # Modify frequency: fq
#' # Settable in the First Time Series
#' # Settable in the Final Time Series (removed from the First Time Series)
#' # Note: When "Daily, Close" is chosen,  Aggregation method is not available.
#'
#' Modify_frequency_fq <-
#' "
#'   Code: Description
#'   Daily, Close: Daily, Close
#'   Weekly, Ending Friday: Weekly, Ending Friday
#'   Weekly, Ending Thursday: Weekly, Ending Thursday
#'   Weekly, Ending Wednesday: Weekly, Ending Wednesday
#'   Weekly, Ending Tuesday: Weekly, Ending Tuesday
#'   Weekly, Ending Monday: Weekly, Ending Monday
#'   Weekly, Ending Sunday: Weekly, Ending Sunday
#'   Weekly, Ending Saturday: Weekly, Ending Saturday
#'   Biweekly, Ending Wednesday: Biweekly, Ending Wednesday
#'   Biweekly, Ending Monday: Biweekly, Ending Monday
#'   Monthly: Monthly
#'   Quarterly: Quarterly
#'   Semiannual: Semiannual
#'   Annual: Annual
#' "
#' Text2DF(Modify_frequency_fq)
#'
#' # example
#' # &fq=Annual
#' # &&fq=Daily%2C%20Close
#'
#' Modify_frequency_fq <- "Annual"
#'
#'
#'
#' # Aggregation method: fam
#' # Settable in the First Time Series
#' # Settable in the Final Time Series (removed from the First Time Series)
#'
#' Aggregation_method_fam <-
#' "
#'   Code: Description
#'   avg: Average
#'   sum: Sum
#'   eop: End of Period
#' "
#' Aggregation_method_fam <- Text2DF(Aggregation_method_fam)
#'
#' # example
#' # &fam=eop
#'
#' Aggregation_method_fam <- "eop"
#'
#'
#'
#' # Formula: fml
#'
#' # Settable in the First Time Series
#' # Settable in the Final Time Series (removed from the First Time Series)
#'
#' Formula_fml <- vector(mode = "character")[1]
#'
#' # example
#' # &fml=a-b
#'
#' Formula <- "a-b"
#'
#'
#'
#' # [Final] Units: fgst
#'
#' # Settable in the First Time Series
#' # Settable in the Final Time Series (removed from the First Time Series)
#'
#' Final_Units_fgst <-
#' "
#'   lin:
#'   chg: Change
#'   ch1: Change from Year Ago
#'   pch: Percent Change
#'   pc1: Percent Change from Year Ago
#'   pca: Compounded Annual Rate of Change
#'   cch: Continuously Compounded Rate of Change
#'   cca: Continuously Compounded Annual Rate of Change
#'   log: Natural Log
#'   nbd: Index (Scale value to 100 for chosen date)
#' "
#' Final_Units_fgst <- Text2DF(Final_Units_fgst)
#'
#' # example
#' # &fgst=log
#'
#'
#'
#' # common usage
#' if(!exists("DGS3MO")) getSymbols("DGS3MO", src = "FRED2")
#'
#'# genernal useage
#' getSymbols(c("DGS3MO", "GDP"), src = "FRED2")
#'
#'  getSymbols("DGS3MO", src = "FRED2", NewName = "IGNORED")
#'
#'  getSymbols(c("DGS3MO", "GDP"), src = "FRED2", NewName = "IGNORED")
#'
#' # rare usage
#' # get the Equity Premium, see the references
#'
#' getSymbols(c(c("WILL5000IND","BAMLC0A4CBBBEY")), src = "FRED2",
#'   Edit = c(transformation = c("pc1","pch"), fq = "Daily, Close", fml="a-b")  )
#'
#' # also get the xts attributes of Last_Updated and Frequency
#' getSymbols(c(c("WILL5000IND","BAMLC0A4CBBBEY")), src = "FRED2",
#'   Edit = c(transformation = c("pc1","pch"), fq = "Daily, Close", fml="a-b"),
#'   transformReqExtraInfo = TRUE)
#'
#'  #  Equity Premium new name
#'
#' getSymbols(c(c("WILL5000IND","BAMLC0A4CBBBEY")), src = "FRED2",
#'   Edit = c(transformation = c("pc1","pch"), fq = "Daily, Close", fml="a-b"),
#'   NewName = "EQUITYPREMIUM")
#'
#' # single series with a transformation
#' # year-over-year percent change
#'
#' getSymbols(c(c("WILL5000IND")), src = "FRED2",
#'   Edit = c(transformation = "pc1", fq = "Daily, Close", fml="a"),
#'   NewName = "EQUITYYOYPCTCHG")
#'
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom rlang parse_expr
#' @importFrom DescTools DoCall
#' @importFrom zoo as.Date
#' @importFrom stringr str_c str_detect
#' @export
getSymbols.FRED2 <- function (Symbols, env, from = "1800-01-01", to = Sys.Date(),
                              Edit = NULL, NewName = NULL, transformReqExtraInfo = NULL,
                              return.class = "xts", ...) {
tryCatchLog::tryCatchLog({
initEnv(); on.exit({uninitEnv()})

    importDefaults("getSymbols.FRED2")
    this.env <- environment()
    for (var in names(list(...))) {
        assign(var, list(...)[[var]], this.env)
    }

    if (!hasArg("verbose"))
        verbose <- FALSE
    if (!hasArg("auto.assign"))
        auto.assign <- TRUE

    # concat with "&" between
    pasteAmp <- function(x,y) { stringr::str_c(c(x, y),  collapse = "&") }

    # LEFT_OFF: HOW MUCH AND WHAT TO RUN
    # ADD:  all.vars(parse(text="a + 3"))
    # [1] "a"

    SymbolsCount <- length(Symbols)
    UniqueFmlExprVars <- vector(mode = "character")
    EditUsingFredTransforms <- FALSE
    if(!is.null(Edit) && !is.na(Edit["fml"])) {
       FmlExprVars <- all.vars(rlang::parse_expr(Edit["fml"]))
       UniqueFmlExprVars <- unique(FmlExprVars)
       if((length(Symbols) == 1) &&
            (length(UniqueFmlExprVars == 1)) &&
            (UniqueFmlExprVars == "a")
       )
             EditUsingFredTransforms <- TRUE
       if(length(Symbols) == length(UniqueFmlExprVars))
             EditUsingFredTransforms <- TRUE
    }

    if(EditUsingFredTransforms) {

        # BEGIN KEEP

        # e.g.
        # Id <- c("WILL5000IND","BAMLC0A4CBBBEY")
        Id <- Symbols
        names(Id)[seq_along(Id)] <- "id"

        # e.g.
        # Edit <- c(transformation = c("pc1","pch"), fq = "Daily, Close", fml="a-b")
        names(Edit) <- stringr::str_remove_all(names(Edit),"\\d+$")

        Params <- c(Id,Edit)

        plyr::llply(Params, function(x) {
            if(is.list(x)) {
                plyr::llply(x, URLencode, reserved = TRUE)
            } else {
                URLencode(x, reserved = TRUE)
            }
        }) -> Encoded

        Ele <- vector(mode = "character")
        for(nm in unique(names(Encoded))){
            Ele[[nm]] <- stringr::str_c(Encoded[names(Encoded) %in% nm], collapse = "_")
        }
        QueryStringExtra <- stringr::str_c(names(Ele), "=" ,Ele, collapse = "&")
        # END KEEP

    }

    default.return.class <- return.class

    default.from <- from

    default.to <- to
    to <- if (is.null(to)) default.to
      else to

    # get the meta-data
    FRED2.URL1 <- "https://fred.stlouisfed.org/data/"
    # get the data
    FRED2.URL2 <- "https://fred.stlouisfed.org/graph/fredgraph.csv?"

    if(EditUsingFredTransforms) {
        if(length(Symbols) == 1)
            # because are still going to
            # https://fred.stlouisfed.org/data/EQUITYYOYPCTCHG.txt
            # to get the metadata
            OldSymbol <- Symbols
        if(!is.null(NewName)) {
             Symbols <- NewName
        } else {
             Symbols <- "FREDQUERY"
        }
    }
    tmp <- tempfile()
    on.exit(unlink(tmp))
    for (i in seq_along(Symbols)) {

        return.class <- getSymbolLookup()[[Symbols[[i]]]]$return.class
        return.class <- ifelse(is.null(return.class), default.return.class, return.class)

        from <- getSymbolLookup()[[Symbols[[i]]]]$from
        from <- if (is.null(from))
            default.from
        else from

        to <- getSymbolLookup()[[Symbols[[i]]]]$to
        to <- if (is.null(to))
            default.to
        else to

        # if using transforms and more than one series is involved.
        # then do not bother getting meta-data
        # (because the meta-data is not be true for both series (as a group))
        if(!(EditUsingFredTransforms && (1 < length(UniqueFmlExprVars)))) {

            # begin metadata

            if (verbose)
                cat("downloading ", Symbols[[i]], ".....\n\n")

            if(exists("OldSymbol", envir = this.env, inherits = FALSE)) {
                 # only in the case of using EditUsingFredTransforms
                 # AND length(Symbols) == 1, then meta-data is still useful
                 URL <- stringr::str_c(FRED2.URL1, OldSymbol, ".txt")
             } else {
                 URL <- stringr::str_c(FRED2.URL1, Symbols[[i]], ".txt")
             }

            quantmod___try.download.file(URL, destfile = tmp, quiet = !verbose, ...)

            if (verbose)
                cat("done.\n")

            fres <- readLines(tmp)

            # boundary splitter between header area and data area
            BoHeaderArea <- 1
            EoHeaderArea <- match(TRUE, stringr::str_detect(fres, "^DATE.*VALUE$")) - 1L
            BoDataArea   <- EoHeaderArea + 2L
            EoDataArea   <- length(fres)
            HeaderArea <- fres[seq(BoHeaderArea,EoHeaderArea,1)]
            DataArea   <- fres[seq(BoDataArea,EoDataArea ,1)]

            # separate dates and values
            DatesAndValues <- strsplit(DataArea, "[[:blank:]]+")
            #
            # idea from
            #
            # Select first element of nested list
            # MAR 2017
            # https://stackoverflow.com/questions/20428742/select-first-element-of-nested-list
            #
            DatesAndValues       <- unlist(DatesAndValues)
            DatesAndValuesLength <- length(DatesAndValues)
            # every other one
            Dates  <- DatesAndValues[seq(1,DatesAndValuesLength,2)]
            Values <- DatesAndValues[seq(2,DatesAndValuesLength,2)]
            Values[Values %in% "."] <- NA

            # read.dcf sometimes does not likes lines with blanks
            HeaderArea <- HeaderArea[!stringr::str_detect(HeaderArea,"^[[:blank:]]+$|^$")]

            # collect information about the series
            tcon <- textConnection(stringr::str_c(HeaderArea, collapse = "\n"))
            # try: keep.white = "Notes"

            # SeriesInfo Xts attributes
            SeriesInfo <- read.dcf(tcon, keep.white = "Notes") %>%
                as.data.frame(stringsAsFactors = FALSE) %>%
                  { colnames(.) <- stringr::str_replace(colnames(.), "\\s" , "_"); .}
                    as.list
            close(tcon)

            # end metadata

            # BEGIN THROW-AWAY (NOT USING)

            # create
            # not used: if someone whats here is the rest of the data
            fr0 <- DescTools::DoCall(xts, c(list(), list(x = as.numeric(Values)),
                                               list(order.by = zoo::as.Date(Dates), origin = "1970-01-01"),
                                               list(src = "FRED2"),
                                               list(updated = Sys.time()),
                                               SeriesInfo
                                            )
            )

            dim(fr0) <- c(NROW(fr0), 1)
            # colnames(fr) <- as.character(toupper(Symbols[[i]])) # not-toupper
            fr <- quantmod___convert.time.series(fr0, return.class = return.class)
            # Symbols[[i]] <- gsub("\\^", "", Symbols[[i]])

            # END THROW-AWAY (NOT USING)
        }

        # fr <- data

        # begin data

        # see
        # getSymbols('DTWEXB',src='FRED') broken #209
        # https://github.com/joshuaulrich/quantmod/issues/209
        #
        # example URL
        # https://fred.stlouisfed.org/graph/fredgraph.csv?cosd=2013-01-05&coed=2018-01-04&id=DGS3MO

        # generate the URL query

        StartEnd <- list(
            stringr::str_c("cosd=", from),
            stringr::str_c("coed=", to)
        )

        if(EditUsingFredTransforms) {
            # id is already in "QueryStringExtra" below
            QueryItems <- StartEnd
        } else {
            QueryItems <- c(list(), stringr::str_c("id=", Symbols[[i]]), StartEnd)
        }

        # collapse with an amperstand "&"
        QueryString <-
            Reduce(pasteAmp, QueryItems)

        if(exists("QueryStringExtra", envir = this.env, inherits = FALSE))
            QueryString <- stringr::str_c( QueryStringExtra, "&", QueryString)

        URL <- stringr::str_c(FRED2.URL2, QueryString)

        if (verbose)
            cat("downloading ", Symbols[[i]], ".....\n\n")

        quantmod___try.download.file(URL, destfile = tmp, quiet = !verbose, ...)
        fr <- read.csv(tmp, na.string = ".")

        if (verbose)
            cat("done.\n")

        # end data
        fr <- xts(as.matrix(fr[, -1]), zoo::as.Date(fr[, 1], origin = "1970-01-01"),
            src = "FRED2", updated = Sys.time())
        # "fml" and the number of "fml" symbols > 1 then NOT exist
        if(exists("SeriesInfo", envir = this.env, inherits = FALSE))
            xtsAttributes(fr) <- SeriesInfo

        dim(fr) <- c(NROW(fr), 1)

        if(EditUsingFredTransforms && (length(Id) > 1) && transformReqExtraInfo) {
            # go BACK to the text servers and go get them
            SeriesIdsXts <- plyr::llply(Id, function(x) { DescTools::DoCall(getSymbols, c(list(), c(x, src = "FRED2", env = env, from = from, to = to,
                              Edit = NULL, NewName = NULL, transformReqExtraInfo = NULL,
                              return.class = return.class, auto.assign = FALSE), list(...)[!names(list(...)) %in% "auto.assign"]))})
            names(SeriesIdsXts) <- Id
            SeriesIdsXtsAttribFrequency   <- plyr::llply( SeriesIdsXts, function(x) xtsAttributes(x)[["Frequency"]])
            SeriesIdsXtsAttribLastUpdated <- plyr::llply( SeriesIdsXts, function(x) xtsAttributes(x)[["Last_Updated"]])

            TwoSeriesSymbolLastUpdated <- vector(mode = "character")
            for(j in SeriesIdsXtsAttribLastUpdated) {
                ThisSymbolLastUpdated <- stringr::str_c(as.character(as.POSIXct(j)), " UTC")
                TwoSeriesSymbolLastUpdated <- unique(c(TwoSeriesSymbolLastUpdated, ThisSymbolLastUpdated))
            }
            # latest "Last_Updated"
            TwoSeriesSymbolLastUpdated <- max(TwoSeriesSymbolLastUpdated)
            xtsAttributes(fr) <- c(list(), xtsAttributes(fr), list(Last_Updated = TwoSeriesSymbolLastUpdated))

            TwoSeriesSymbolFrequency <- vector(mode = "character")
            DAILYFOUND <- WEEKLYFOUND <- MONTHLYFOUND <- QUARTERLYFOUND <- FALSE
            for(j in SeriesIdsXtsAttribFrequency) {
                ThisSymbolFrequency <- j
                if(ThisSymbolFrequency %Like% "^Daily") {
                    TwoSeriesSymbolFrequency <- ThisSymbolFrequency
                    DAILYFOUND <- TRUE
                  break
                } else if(ThisSymbolFrequency %Like% "^Weekly" && !DAILYFOUND) {
                    TwoSeriesSymbolFrequency <- ThisSymbolFrequency
                    WEEKLYFOUND <- TRUE
                } else if(ThisSymbolFrequency %Like% "^Monthly" && !WEEKLYFOUND && !DAILYFOUND) {
                    TwoSeriesSymbolFrequency <- ThisSymbolFrequency
                    MONTHLYFOUND <- TRUE
                } else if(ThisSymbolFrequency %Like% "^Quarterly" && !MONTHLYFOUND &&!WEEKLYFOUND && !DAILYFOUND) {
                    TwoSeriesSymbolFrequency <- ThisSymbolFrequency
                    QUARTERLYFOUND <- TRUE
                }

            }
            xtsAttributes(fr) <- c(list(), xtsAttributes(fr), list(Frequency = TwoSeriesSymbolFrequency))

        }

        if(EditUsingFredTransforms && is.null(NewName)) {
             NewName <- "FREDQUERY"
             colnames(fr)[1] <- NewName
      # } else if(!EditUsingFredTransforms && !is.null(NewName)) {
      #      colnames(fr)[1] <- NewName
        } else {
            colnames(fr)[1] <- as.character(Symbols[[i]]) # not-toupper
        }

        fr <- quantmod___convert.time.series(fr = fr, return.class = return.class)
        if (auto.assign)
            assign(Symbols[[i]], fr, env)
    }
    if (auto.assign)
        return(Symbols)
    return(fr)
})}





#' Survey of Professional Forecasters data from the Philadelphia FED
#'
#' About the data . . .
#'
#' File Structure: Column Header Nomenclature and Forecast Horizons
#'
#' “1” to “6” (quarterly forecasts) or
#' “A” and “B” (annual-average forecasts)
#'
#' The number “1” represents the “forecast” for the quarter prior to the quarter in
#' which the survey is conducted. The forecasters know the values of the variables for this
#' quarter at the time they submit their projections. . . . The forecasters are permitted
#' to forecast a revision
#'
#' The number “2” represents the forecast for the current quarter, defined as
#' the quarter in which the survey is conducted. The numbers “3” through “6”
#' represent the forecasts for the four quarters after the current quarter.
#' The letters “A” and “B” represent annual average forecasts
#' for the current year (the year in which the survey is conducted) and
#' the following year.
#'
#' About the date/time index . . .
#'
#' Deadline and Release Dates for the Survey of Professional Forecasters
#'
#' release dates
#' https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/spf-release-dates.txt?la=en
#'
#' True deadline and news release dates for surveys prior to 1990:Q2 are not known.
#'
#' NOTE: about "not known" (see below), for simplicity
#' xts time index of these "not known" periods is the middle date of the calendar quarter
#'
#' *The 1990Q2 survey was not taken in real time, because the Philadelphia Fed
#' had not yet taken over the survey. Forecasters were asked to provide dated
#' forecasts from May 1990.
#'
#' **The 1996Q1 survey was delayed because of the federal government shutdown,
#' which in turn delayed the release of government statistical data.
#'
#' ***The 2013Q4 survey was delayed because of the federal government shutdown,
#' which in turn delayed the release of government statistical data.
#'
#' # Popular start page
#' https://www.philadelphiafed.org/research-and-data/real-time-center/survey-of-professional-forecasters/data-files
#'
#' # all of the data (1968 to the present) in five(5) excel files
#' Individual Forecasts for the Survey of Professional Forecasters
#' https://www.philadelphiafed.org/research-and-data/real-time-center/survey-of-professional-forecasters/historical-data/individual-forecasts
#'
#' # Some, compare and constrasts may be
#' 'forcasters'(prediction) w.s. FRED (acutally what happened)
#'
#' # unemployment
#'
#' Civilian Unemployment Rate (UNEMP)
#' https://www.philadelphiafed.org/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/unemp
#'
#' Civilian Unemployment Rate (UNRATE)
#' https://fred.stlouisfed.org/series/UNRATE/
#'
#' # prices ( e.g. inflation )
#'
#' Price Index for Gross National Product/Gross Domestic Product (PGDP)
#' https://www.philadelphiafed.org/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/pgdp
#'
#' Gross National Product (chain-type price index) (A001RV1Q225SBEA)
#' https://fred.stlouisfed.org/series/A001RV1Q225SBEA
#'
#' # gross domestic product
#'
#' Nominal Gross National Product/Gross Domestic Product (NGDP)
#' https://www.philadelphiafed.org/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/ngdp
#'
#' Gross Domestic Product (GDP)
#' https://fred.stlouisfed.org/series/GDP
#'
#' # Corporate Profits
#'
#' Corporate Profits After Tax (CPROF)
#' https://www.philadelphiafed.org/research-and-data/real-time-center/survey-of-professional-forecasters/data-files/cprof
#'
#' Corporate Profits After Tax (without IVA and CCAdj) (CP)
#' https://fred.stlouisfed.org/series/CP
#'#'
#'
#' @param Symbols  a character vector specifying the names of each symbol to be loaded
#' To get all of the data use: "USFedPhilForecastingData".  Possible individual
#' Symbols are listed in the links on the Forcasters' main page
#' that reads (see below) ... (1) Historical Data Files for the Survey
#' of Professional Forecasters (2) Documentation: PDF Provides information on
#' all variables, transformations, and files in the survey.
#' E.g. "UNEMP2". Symbols will be returned
#' as (xts) data with names as excelcolumn+Fun"
#' E.g. "UNEMP2.mean"
#' @param env where to create objects. (.GlobalEnv)
#' @param return.class desired class of returned object.
#' Can be xts, zoo, data.frame, or xts (default)
#' @param SurveyDecades "All"(default). Vector of options include the following
#' "1970s", "1980s", "1990s", "2000s", and "2010s".
#' Multiple options can be chosen and  sent in a character
#' vector. e.g. c("2000s", "2010s")
#' @param DataPath "./USFedPhilForecastersData"(default). New/old location of the
#' new/old downloaded xls files from the Philadelphia Fed's site. Windows
#' users can also enter this path using instead two back slashes.
#' Abolute paths can also be used.
#' @param MaxAge "4 hours"(default) is longest age allowed, such that the retrieving the
#' the MOST RECENTLY PUBLISHED FORECASTERS EXCEL FILE from  the local fileystem
#' will be allowed to be done. If the MaxAge is exceeded then,
#' the most recent excel file is refreshed anew from the Philadelphia Fed site.
#' The format uses as.difftime: "# secs", "# mins", "# hours", "# days", "# weeks"
#' @param UseFST TRUE(default) use the "Excel + FST" files (from R package fst).
#' Initial creation of .fst files uses about 15 seconds of time per file (5 file).
#' Afterwards, queries are lightning fast.
#' Otherwiase FALSE, use "Excel Only"  Queries are slower (mostly because of the heavier disk I/O)
#' @param Fun mean(default) with na.rm = TRUE(default) Aggregate function to
#' apply to the period data. This becomes the symbol Suffix. E.g. "mean" will
#' produce the suffix "mean".
#' @param force FALSE(default) re-download data from USFedPhil ( See the examples. )
#' Generally, using this parameter "force = TRUE"
#' is NOT necessary: after MaxAge has been exceeded and if a query needs the
#' MOST RECENTLY PUBLISHED FORECASTERS EXCEL FILE then this file will be downloaded anew.
#' @param ... additional parameters pased to Fun. Also dots are passed to
#' quantmod___try.download.file
#' @return A call to getSymbols.USFedPhil will load into the specified
#' environment one object for each \code{Symbol} specified,
#' with class defined by \code{return.class}.
#' @author Jeffrey A. Ryan
#' @author Andre Mikulec (adapted original code to work with the
#' US Federal Researve Bank of Philadelphia's Survey of Professional Forecasters)
#' @references
#' \cite{Survey of Professinal Forecasters Release Dates \url{https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/spf-release-dates.txt?la=en}}
#' @references
#' \cite{Historical Data Files for the Survey of Professional Forecasters \url{https://www.philadelphiafed.org/research-and-data/real-time-center/survey-of-professional-forecasters/data-files}}
#' @references
#' \cite{Documentation: PDF Provides information on all variables, transformations, and files in the survey. \url{https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/spf-documentation.pdf?la=en}}
#' @references
#' \cite{Survey of Professional Forecasters \url{https://www.philadelphiafed.org/research-and-data/real-time-center/survey-of-professional-forecasters}}
#' @references
#' \cite{Federal Researve Bank of Philadelphia \url{https://www.philadelphiafed.org/}}
#' @seealso
#' \code{\link{getSymbols}}
#' \code{\link{setSymbolLookup}}
#' @keywords data
#' @examples
#' \dontrun{
#'
#' # common usage
#' if(!exists("UNEMP2_mean")) getSymbols("UNEMP2", src = "USFedPhil")
#'
#' getSymbols(c("UNEMP2", "PGDP2", "NGDP2","CPROF2"), src = "USFedPhil")
#' getSymbols(c("RR1_TBILL_PCE_3","RR3_TBILL_PCE_3"), src = "USFedPhil", Fun = mean, na.rm = TRUE)
#'
#' # standard deviation
#' getSymbols("UNEMP2", src = "USFedPhil", Fun = sd, na.rm = TRUE)
#'
#' # just recent data
#' getSymbols("UNEMP2", src = "USFedPhil", SurveyDecades = "2010s")
#'
#' # force requery of data from the Philladelphia site
#' # will collect new xls file(s)
#' getSymbols("UNEMP2", src = "USFedPhil", force = TRUE)
#'
#' # all columns in one VERY WIDE xts object
#' getSymbols("USFedPhilForecastingData", src = "USFedPhil")
#'
#'}
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom zoo as.Date as.yearqtr
#' @importFrom stringr  str_c str_remove
#' @importFrom readxl read_xls excel_sheets
#' @importFrom fst write.fst read.fst fst.metadata
#' @importFrom data.table data.table rbindlist
#' @importFrom RQuantLib adjust
#' @importFrom DataCombine MoveFront VarDrop
#' @importFrom plyr join
#' @export
getSymbols.USFedPhil <- function(Symbols, env, return.class = "xts",
           SurveyDecades = "All",
           DataPath = "./USFedPhilForecastersData", MaxAge = NULL, UseFST = TRUE,
           Fun = NULL, ...) {
tryCatchLog::tryCatchLog({
initEnv(); on.exit({uninitEnv()})

    importDefaults("getSymbols.USFedPhil")
    this.env <- environment()
    for (var in names(list(...))) {
        assign(var, list(...)[[var]], this.env)
    }
    if (!hasArg("verbose"))
        verbose <- FALSE
    if (!hasArg("auto.assign"))
        auto.assign <- TRUE

    Dots <- list(...)

    # SurveyDecades
    AllSurveyDecades <- c("1970s", "1980s", "1990s", "2000s", "2010s")
    if(is.null(SurveyDecades) || SurveyDecades == "All")
      SurveyDecades <-  AllSurveyDecades
    if(any(!SurveyDecades %in% c("All", AllSurveyDecades)))
      stop("getSymbols.USFedPhil can not find SurveyDecades member")

    # DataPath
    if(is.null(DataPath)) DataPath <- "./USFedPhilForecastersData"
    if(!dir.exists(DataPath)) dir.create(DataPath)
    # read ? dir.exists
    if(!dir.exists(DataPath)) stop("getSymbols.USFedPhil directory not created or created without a determinable name")
    DataPath <- normalizePath(path = DataPath, winslash = "/", mustWork = TRUE)

    if(is.null(MaxAge)) MaxAge <- "4 hours"
    MaxAgeValueUnits <- strsplit(MaxAge, " ")[[1]]
    MaxAgeValue <- as.integer(MaxAgeValueUnits[1])
    MaxAgeUnits <- MaxAgeValueUnits[2]

    # force
    if(!exists("force", envir = this.env, inherits = FALSE))
        force = FALSE

    USFedPhil.URL <- "https://www.philadelphiafed.org/-/media/research-and-data/real-time-center/survey-of-professional-forecasters/historical-data"

    AllSurveyDecadesIndex     <- seq_along(AllSurveyDecades)
    # needed to determine the universally highest index
    MaxAllSurveyDecadesIndex  <- max(AllSurveyDecadesIndex)

    SurveyDecadesIndex        <- match(SurveyDecades, AllSurveyDecades)
    DestFiles <- list()

    for(FileIndex in SurveyDecadesIndex) {
    FileExists <- FALSE
      # begin xls area

      # do not remove the 'destfile' files
      # keep the directory around and available for the next query [if any]
      # the site https://www.philadelphiafed.org/ is NOT engineered
      # to handle MANY data queries, NOR denial of service attacks

      # pairs of files
      File  <-  stringr::str_c("micro", FileIndex, ".xlsx")
      File2 <-  stringr::str_c("micro", FileIndex, ".fst")
      destfile  <- stringr::str_c(DataPath, "/", File)
      destfile2 <- stringr::str_c(DataPath, "/", File2)

      # work in pairs
      if((FileIndex  < MaxAllSurveyDecadesIndex) && file.exists(destfile) && !force) FileExists <- TRUE

      # limited use variables
      # last(youngest) .xlsx file should be refreshed often
      if(FileIndex == MaxAllSurveyDecadesIndex) {
        if(file.exists(destfile)) {
          updated <- file.info(destfile)$mtime # is.na - if the file does not exist
          AgeTestTooOld <- as.difftime(MaxAgeValue,  units = MaxAgeUnits) <  difftime(Sys.time(), updated)
          # wierd logic: if the file is "not too old" then I ALSO will say that the file EXISTS
          if(!AgeTestTooOld && !force) FileExists <- TRUE
        }
      }

      if(!FileExists) {
        USFedPhil.URL.file <- stringr::str_c(USFedPhil.URL, "/", File, "?la=en")
        if (verbose)
            cat("downloading ", destfile, ".....\n\n")
        message(stringr::str_c("getSymbols.USFedPhil Begin xlsx file download: ", destfile))
        quantmod___try.download.file(USFedPhil.URL.file, destfile = destfile, quiet = !verbose, mode = "wb", ...)
        message(stringr::str_c("getSymbols.USFedPhil End   xlsx file download: ", destfile))

      }

      # destfile2(fst) does not exist or destfile2(fst) is older than destfile(xls)
      if(( file.exists(destfile) && (   !file.exists(destfile2) ||
                                      (  file.exists(destfile2) && (file.info(destfile2)$mtime < file.info(destfile)$mtime) )
                                    )) && UseFST
      ) {

        Sheets <- readxl::excel_sheets(destfile)

        DestFileSheets <- data.frame()
        message(stringr::str_c("getSymbols.USFedPhil Begin fst file creation: ", destfile2))
        for(Sheet in Sheets) {
          # data.frame
          fr <- suppressWarnings(readxl::read_xlsx(path = destfile, sheet = Sheet,
                 col_names = TRUE,
                 col_types = "numeric")
          )

          if(!NCOL(DestFileSheets)) {
            DestFileSheets <- fr
          } else {
            # outer
            DestFileSheets <- plyr::join(DestFileSheets, fr, by = c("YEAR", "QUARTER", "ID", "INDUSTRY"), type = "full")
            # outer join cases where
            # forecaster("ID") that changed industry ("INDUSTRY")
            # "INDUSTRY is commonly NA
            DestFileSheets <- RemoveEmptyRows(DestFileSheets, Var = c("YEAR", "QUARTER", "ID"), Ele = "Any")
          }


        }
        message(stringr::str_c("getSymbols.USFedPhil End   fst file creation: ", destfile2))
        fst::write.fst(DestFileSheets, path = destfile2, compress = 0)

      }

      if(!exists("Sheets", envir = environment(), inherits = FALSE))
        Sheets <- readxl::excel_sheets(destfile)
      if(!"USFedPhilForecastingData" %in% Symbols) {
        # selection by "obscurity"
        Sheets1 <- Sheets[Sheets %in% stringr::str_remove(Symbols, "[A-Z1-9]$")]
        # _# sheets (on the far right of the Excel file)
        Sheets2 <- Sheets[Sheets %in% stringr::str_remove(Symbols, "_[A-Z1-9]$")]
        Sheets  <- union(Sheets1, Sheets2)
      }

      if((file.exists(destfile2)) && UseFST) {

          DestFile2Meta <- fst::fst.metadata(path = destfile2)

          # only the Symbols(columns of interest)
          if(!"USFedPhilForecastingData" %in% Symbols) {
            ColumnsToRead <- DestFile2Meta[["columnNames"]][DestFile2Meta[["columnNames"]] %in% Symbols]
            DestFileSheets <- fst::read.fst(path = destfile2, columns = c(c("YEAR", "QUARTER", "ID", "INDUSTRY"), ColumnsToRead))
          } else {
            DestFileSheets <- fst::read.fst(path = destfile2)
          }
          # industry of forecaster. (not important)
          DestFileSheets <- DataCombine::VarDrop(DestFileSheets, Var = "INDUSTRY")

      } else {  # UseFST == FALSE # excel read (should NOT HAVE made it this far )
        DestFileSheets <- data.frame()
        for(Sheet in Sheets) {
          # data.frame
          fr <- suppressWarnings(readxl::read_xlsx(path = destfile, sheet = Sheet,
                 col_names = TRUE,
                 col_types = "numeric")
          )
          # # FROM the source web sit, excel sheets are filled with these #N/A (appear as NA in R)
          # #   #N/A is the error value that means "no value is available."
          #     Use NA to mark empty cells. By entering #N/A in cells where you are missing information,
          #     you can avoid the problem of unintentionally including empty cells in your calculations.
          # # NA function
          # # Excel for Office 365 Excel for Office 365 for Mac Excel 2019 Excel 2016 More...
          # # JAN 2019
          # # https://support.office.com/en-us/article/na-function-5469c2d1-a90c-4fb5-9bbc-64bd9bb6b47c
          # avoid R math NaN results

          # only the Symbols(columns of interest)
          fr <- fr[, c( c("YEAR", "QUARTER", "ID", "INDUSTRY"), colnames(fr)[colnames(fr) %in% Symbols]), drop = F]
          # industry of forecaster. (not important)
          fr <- DataCombine::VarDrop(fr, Var = "INDUSTRY")

          if(!NCOL(DestFileSheets)) {
            DestFileSheets <- fr
          } else {
            # data.table group STILL has not made data.table:::merge.data.table public (export)ed
            DestFileSheets <- plyr::join(DestFileSheets, fr, by = c("YEAR", "QUARTER", "ID"), type = "full")
          }

        }
      }

      DestFileSheets <- data.table::data.table(DestFileSheets, key = c("YEAR","QUARTER","ID"))
      TheseSheets <- list()
      TheseSheets[[stringr::str_c("File_", FileIndex)]] <- DestFileSheets
      DestFiles <- c(list(), DestFiles, TheseSheets)

    }
    if (verbose)
        cat("reading disk directory ", tmp, ".....\n\n")

    # non-atomic "[[a]] <- b" assignments do not work on data.table
    # DataCombine::VarDrop does not work on data.table
    Res <- data.table::rbindlist(DestFiles, fill= TRUE) %>%
      as.data.frame

    # index

    # > frd <- ForecastersReleaseDates()
    # > str(frd)
    # 'data.frame':	115 obs. of  3 variables:
    #  $ YearQtr         : 'yearqtr' num  1990 Q2 1990 Q3 1990 Q4 1991 Q1 ...
    #  $ TrueDeadlineDate: Date, format: "1990-08-23" "1990-08-23" "1990-11-22" "1991-02-16" ...
    #  $ ReleaseDate     : Date, format: "1990-08-31" "1990-08-31" "1990-11-28" "1991-02-21" ...

    # update index
    ReleaseDates <- ForecastersReleaseDates()

    TimeDateYearQtr  <-
      stringr::str_c(Res[["YEAR"]]," ", Res[["QUARTER"]]) %>%
        { zoo::as.yearqtr(., format ="%Y %q") }

    # approximated dates
    TimeDateDateDate <- TimeDateYearQtr %>%
      { zoo::as.Date(.,frac = 0.5) } %>%
        { RQuantLib::adjust("UnitedStates/GovernmentBond", ., 1) }

    # override with correct dates
    # see # blockmodeling::recode

    NewTimeDateDateDate <- TimeDateDateDate
    for(i in seq_along(NewTimeDateDateDate)) {
      NewTimeDateDateDate[TimeDateYearQtr ==  ReleaseDates[["YearQtr"]][i]] <- ReleaseDates[["ReleaseDate"]][i]
     }
     TimeDateDateDate <- NewTimeDateDateDate

    Res[["TimeDate"]] <- TimeDateDateDate

    Res <- DataCombine::MoveFront(Res, Var = "TimeDate" )
    # ommiting "ID" (and "YEAR" AND "QUARTER")
    # The situation would be entertaining to find
    # the best forcaster(by "ID") of them all of "all time"
    if(!"USFedPhilForecastingData" %in% Symbols) {
      Res <- Res[, c("TimeDate", Symbols), drop = FALSE]
    } else { # "USFedPhilForecastingData"
      Res <- DataCombine::VarDrop(Res, Var = c("YEAR","QUARTER","ID"))
    }
    DT <- data.table::data.table(Res)

    # either called   directly getSymbols.USFedPhil(Fun = mean)
    # or     called indirectly getSymbols() called without "Fun = ??"
    #                       or wrong argument e.g. getSymbols(.fun = XX)
    if(!is.null(Fun) && is.null(Dots[["FunStr"]])) {
       # I am not allowed to call this TWICE
       FunStr <- as.character(substitute(Fun))
       Fun    <- match.fun(Fun)
    }
    if(!is.null(Dots[["FunStr"]]) && !exists("FunStr", envir = environment(), inherits = FALSE))
      FunStr <- Dots[["FunStr"]]

    # if the value has not bee sent
    if(is.null(Fun)) {
       Fun = mean
       FunStr = "mean"
       if(is.null(Dots[["is.na"]])) {
         Dots <- c(list(),Dots, na.rm = TRUE)
       }
    }

    # as.character(substitute(Fun)) == "Fun"
    # (inside of the function) called by DoCall
    DT <- DescTools::DoCall("applyAggregateDT", c(list(), x = list(DT), Fun = Fun, By = "TimeDate", FunStr = FunStr, Dots))
    # from aggregate(Fun), some aggregates remove, NaN
    DF <- as.data.frame(plyr::llply(DT,function(x) { x[is.nan(x)] <- NA; x }), stringsAsFactors = FALSE)
    colnames(DF) <- stringr::str_c(colnames(DF), "_", FunStr)
    fr <- DF

    if (verbose)
        cat("done.\n\n")
    fr <- xts(as.matrix(fr[, -1, drop = FALSE]), zoo::as.Date(fr[[1]], origin = "1970-01-01"),
              src = "USFedPhil", updated = Sys.time())
    fri <- fr
    rs <- fri

    # end xls area

    # prepare to splice ( nothing to do )

    # splice ( nothing to do )

    rst <- rs
    fr <- rst
    fri <- fr # pass-throught on !"USFedPhilForecastingData"

    # decompose [if any] into [many] Symbol(s), then return
    for (i in 1:length(Symbols)) {

        # User only wants an individual column
        if(Symbols[[i]] != "USFedPhilForecastingData") {
           if (verbose)
             cat("selecting ", Symbols[[i]], ".....\n\n")
          fri <- fr[, colnames(fr)[tolower(colnames(fr)) %in% tolower(stringr::str_c(Symbols[[i]], "_", FunStr))]]
        }

        fri <- quantmod___convert.time.series(fr = fri, return.class = return.class)
        if (auto.assign)
            assign(Symbols[[i]], fri, env)
    }
    if (auto.assign)
        return(Symbols)
    return(fri)

})}



#' American Association of Individual Investors (AAII) weekly sentiment survey
#'
#' @description
#' Data represents what direction members feel the
#' stock market will be in next 6 months.
#'
#' @details
#' The sentiment survey measures the percentage of individual investors who are
#' bullish, bearish, and neutral on the stock market short term;
#' individuals are polled from the AAII Web site on a weekly basis.
#' Only one vote per member is accepted in each weekly voting period.
#'
#' The latest published date/data is every Thursday.
#' The delivery time is during Friday (UNVERIFIED)
#'
#' @param Symbols  a character vector specifying the names of each symbol to be loaded
#' Possible Symbols are the following:
#' "AAIIsentiment" (means get all of the columns);
#' otherwise, get specific columns;
#' "Bullish", "Neutral", "Bearish",
#' "Bullish8WMA", "BullBearSpread",
#' "BullishAvg", "BullishAvgPStd", "BullishAvgNStd",
#' "SP500WHigh", "SP500WLow", and "SP500WClose"
#' @param env where to create objects. (.GlobalEnv)
#' @param return.class desired class of returned object.
#' Can be xts, zoo, data.frame, or xts (default)
#' @param force FALSE(default) re-download data from AAII ( See the examples. )
#' The hidden variables ".AAIIsentiment_path2file" and
#' ".AAIISentimentSurveyPastResults_path2file" located in the
#' environment of parameter env are the last know location of the "xls" file
#' and the "html" file.  Generally, using this parameter "force = TRUE"
#' is NOT necessary: the hidden variables are persistent through the
#' entire R session.  Also, send parameter "verbose = TRUE" to see
#' the *path2file locations.
#' @param ... additional parameters
#' @return A call to getSymbols.AAII will load into the specified
#' environment one object for each \code{Symbol} specified,
#' with class defined by \code{return.class}.
#' @author Jeffrey A. Ryan
#' @author Andre Mikulec (adapted original code to work with AAII sentiment data)
#' @references
#' \cite{AAII Investor Sentiment Survey \url{https://www.aaii.com/sentimentsurvey}}
#' @references
#' \cite{Sentiment Survey Past Results \url{https://www.aaii.com/sentimentsurvey/sent_results}}
#' @seealso
#' \code{\link{getSymbols}}
#' \code{\link{setSymbolLookup}}
#' @keywords data
#' @examples
#' \dontrun{
#'
#' # common usage
#' if(!exists("BullBearSpread")) getSymbols("BullBearSpread", src = "AAII")
#'
#' # During Fridays, if the user has a long R session that starts in the morning and
#' # continues through the evening, the user may want to get a new 'updated'
#' # Excel 'xls' file with the new results of this weeks vote.
#' # The Symbol name does not matter.  All symbols are from the same 'xls' file
#' # The dates are as of Thursday each week
#' getSymbols(c("Bullish", "Neutral", "Bearish"), src = "AAII")
#'
#' # force requery of data from the AAII site
#' # will collect one new xls (historical obs) file and one new html (latest obs) file
#' getSymbols(c("Bullish", "Neutral", "Bearish"), src = "AAII", force = TRUE)
#'
#' # all columns in one xts object
#' getSymbols("AAIIsentiment", src = "AAII")
#'
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom zoo as.Date
#' @importFrom plyr llply
#' @importFrom htmltab htmltab
#' @importFrom stringr str_replace
#' @importFrom readxl read_xls
#' @export
getSymbols.AAII <- function(Symbols, env, return.class = "xts", ...) {
tryCatchLog::tryCatchLog({
initEnv(); on.exit({uninitEnv()})

    importDefaults("getSymbols.AAII")
    this.env <- environment()
    for (var in names(list(...))) {
        assign(var, list(...)[[var]], this.env)
    }
    if (!hasArg("verbose"))
        verbose <- FALSE
    if (!hasArg("auto.assign"))
        auto.assign <- TRUE

    if(!exists("force", envir = this.env, inherits = FALSE))
        force = FALSE

    # begin xls area

    if(exists(".AAIIsentiment_path2file", envir = env, inherits = FALSE)) {
      assign("tmp", get(".AAIIsentiment_path2file", envir = env, inherits = FALSE), envir = this.env, inherits = FALSE)
    } else {
      tmp <- NULL
    }
    if( !is.null(tmp) &&
        !is.na(file.info(tmp)$mtime) && # is.na - if the file does not exist
        !force
    ) {
    } else {

        # possible clean-up
        oldtmp <- tmp
        if(!is.null(oldtmp) && !is.na(file.info(oldtmp)$mtime)) on.exit(unlink(oldtmp))

        AAII.URL <- "https://www.aaii.com/files/surveys/sentiment.xls"
        tmp <- tempfile(fileext = ".xls") # AAII still uses the OLD format ( DEC 2018 )
        #
        # do not remove the 'tmp' file
        # In this R session, keep the file around and available for the next query [if any]
        # the site https://www.aaii.com is NOT engineered
        # to handle MANY data queries, NOR denial of service attacks

        if (verbose)
            cat("downloading ", "AAIIsentiment", ".....\n\n")
        quantmod___try.download.file(AAII.URL, destfile = tmp, quiet = !verbose, mode = "wb", ...)
        assign(".AAIIsentiment_path2file", tmp, envir = env, inherits = FALSE)

    }
    if (verbose)
        cat("reading disk file ", tmp, ".....\n\n")

    # every Thursday
    Dates <- seq.Date(from = zoo::as.Date("1987-06-26"), to = Sys.Date(), by = "7 day")

    # Last verified: DEC 2018
    col_names = c("ReportedDate",
      "Bullish", "Neutral", "Bearish", "Total", # always 100%
      "Bullish8WMA", "BullBearSpread",
      "BullishAvg", "BullishAvgPStd", "BullishAvgNStd",
      "SP500WHigh", "SP500WLow", "SP500WClose")

    # data.frame
    fr <- readxl::read_xls(path = tmp, sheet = "SENTIMENT",
           col_names = col_names,
           col_types = c("date", rep("numeric",12)), skip = 5L,
           n_max = length(Dates))

    if (verbose)
        cat("done.\n\n")
    fr <- xts(as.matrix(fr[, -1]), zoo::as.Date(fr[[1]], origin = "1970-01-01"),
              src = "AAII", updated = Sys.time())
    fri <- fr
    rs <- fri

    # end xls area

    # begin html area

    if(exists(".AAIISentimentSurveyPastResults_path2file", envir = env, inherits = FALSE)) {
      assign("tmppage", get(".AAIISentimentSurveyPastResults_path2file", envir = env, inherits = FALSE), envir = this.env, inherits = FALSE)
    } else {
      tmppage <- NULL
    }
    if( !is.null(tmppage) &&
        !is.na(file.info(tmppage)$mtime) && # is.na - if the file does not exist
        !force
    ) {
    } else {

        # possible clean-up
        oldtmppage <- tmppage
        if(!is.null(oldtmppage) && !is.na(file.info(oldtmppage)$mtime)) on.exit(unlink(oldtmppage))

        AAII.URL <- "https://www.aaii.com/sentimentsurvey/sent_results"
        tmppage <- tempfile(fileext = ".html") # AAII still uses the OLD format ( DEC 2018 )
        #
        # do not remove the 'tmppage' file
        # In this R session, keep the file around and available for the next query [if any]
        # the site https://www.aaii.com is NOT engineered
        # to handle MANY data queries, NOR denial of service attacks

        if (verbose)
            cat("downloading ", "AAIISentimentSurveyPastResults", ".....\n\n")
        quantmod___try.download.file(AAII.URL, destfile = tmppage, quiet = !verbose, mode = "wb", ...)
        assign(".AAIISentimentSurveyPastResults_path2file", tmppage, envir = env, inherits = FALSE)

    }
    if (verbose)
        cat("reading disk file ", tmppage, ".....\n\n")

    rt <- htmltab::htmltab(  # google chrome ( DEC 2018 )
            doc = tmppage
          , which = '//*[@id="page_content"]/table[1]'
          , rm_nodata_cols = F)

    # I JUST NEED THE TOP ROW ( THE REST I GET FROM EXCEL )
    rt <- rt[1,,drop = FALSE]
    rt[["Reported Date"]] <- stringr::str_replace(rt[["Reported Date"]], ":", "")
    indexRecent <- zoo::as.Date(rt[["Reported Date"]], format = "%B %d")
    rt <- rt[,!colnames(rt) %in% "Reported Date", drop = FALSE]
    dataColnames <- colnames(rt)
    dataRecent <- as.numeric(stringr::str_replace(unlist(rt), "%", ""))/100.0
    rt <- xts(matrix(dataRecent, ncol = length(dataColnames)), indexRecent); colnames(rt) <- dataColnames

    # end html area

    # prepare to splice

    # rs coredata(c)
    rsc <- colnames(rs)[!colnames(rs) %in% colnames(rt)]
    rsc <- as.list(rsc)
    Names(rsc) <- unlist(rsc)
    NamesRSC <- Names(rsc)

    rsc <- plyr::llply(rsc, function(x) NA_real_)  %>%
           unlist %>% { matrix(., ncol = length(NamesRSC), dimnames = list(list(),NamesRSC)) }

    # rt coredata(c)
    rtc <- cbind(coredata(rt), rsc)
    rtc <- rtc[,customSorting(colnames(rtc), InitOrder = colnames(rs)), drop = FALSE]
    rt <- xts(rtc, index(rt))

    # splice

    rst <- rbind(rs,rt)
    # remove the first(earliest) [if any] duplicate index
    rst <- rst[!duplicated(index(rst), fromLast = TRUE),]
    fr <- rst
    fri <- fr # pass-throught on "AAIIsentiment"

    # decompose [if any] into [many] Symbol(s), then return

    for (i in 1:length(Symbols)) {

        # User only wants an individual column
        # Note: BAD user choices: ReportedDate and Total
        if(Symbols[[i]] != "AAIIsentiment") {
           if (verbose)
             cat("selecting ", Symbols[[i]], ".....\n\n")
          fri <- fr[, colnames(fr)[tolower(colnames(fr)) %in% tolower(Symbols[[i]])]]
        }

        fri <- quantmod___convert.time.series(fr = fri, return.class = return.class)
        if (auto.assign)
            assign(Symbols[[i]], fri, env)
    }
    if (auto.assign)
        return(Symbols)
    return(fri)

})}



#' stock market data from Yale University
#'
#' NOT USED ANYWHERE
#'
#' @param Symbols  a character vector specifying the names of each symbol to be loaded
#' Possible Symbols are the following:
#' "YaleUStockMarketData" (means get all of the columns);
#' otherwise, get specific columns;
#' "SP500Price", "Dividends", "Earnings",
#' "CPIAUCNSY" (Consumer Price Index for All Urban Consumers: All Items (of Yale)),
#  "DateFY" (Date Fraction of Yale), "GS10Y" (Long Interest Rate of Yale),
#  "RealPrice", "RealDividends", "RealEarnings", "CAPE"
#' @param env where to create objects. (.GlobalEnv)
#' @param return.class desired class of returned object.
#' Can be xts, zoo, data.frame, or xts (default)
#' @param force FALSE(default) re-download data from YaleU ( See the examples. )
#' The hidden variable ".YaleUstockMarketData_path2file" located in the
#' environment of parameter env is the last known location of the "xls" file.
#' Generally, using this parameter "force = TRUE"
#' is NOT necessary: the hidden variables are persistent through the
#' entire R session.  Also, send parameter "verbose = TRUE" to see
#' the *path2file location.
#' @param ... additional parameters
#' @return A call to getSymbols.YaleU will load into the specified
#' environment one object for each \code{Symbol} specified,
#' with class defined by \code{return.class}.
#' @author Jeffrey A. Ryan
#' @author Andre Mikulec (adapted original code to work with Yale University stock market data)
#' @references
#' \cite{ONLINE DATA ROBERT SHILLER \url{http://www.econ.yale.edu/~shiller/data.htm}}
#' @references
#' \cite{Home Page of Robert J. Shiller - Yale University \url{http://www.econ.yale.edu/~shiller/}}
#' @seealso
#' \code{\link{getSymbols}}
#' \code{\link{setSymbolLookup}}
#' @keywords data
#' @examples
#' \dontrun{
#'
#' # common usage
#' if(!exists("CAPE")) getSymbols("CAPE", src = "YaleU")
#'
#' getSymbols(c("CAPE", "Dividends", "Earnings"), src = "YaleU")
#'
#' # force requery of data from the Yale University site
#' # will collect one new xls (historical obs) file
#' getSymbols(c("CAPE", "Dividends", "Earnings"), src = "YaleU", force = TRUE)
#'
#' # all columns in one xts object
#' getSymbols("YaleUstockMarketData", src = "YaleU")
#'
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom zoo as.Date
#' @importFrom plyr llply
#' @importFrom htmltab htmltab
#' @importFrom stringr str_replace str_c
#' @importFrom readxl read_xls
#' @importFrom zoo na.trim
#' @importFrom Hmisc yearDays
#' @export
getSymbols.YaleU <- function(Symbols, env, return.class = "xts", ...) {
tryCatchLog::tryCatchLog({
initEnv(); on.exit({uninitEnv()})

    importDefaults("getSymbols.YaleU")
    this.env <- environment()
    for (var in names(list(...))) {
        assign(var, list(...)[[var]], this.env)
    }
    if (!hasArg("verbose"))
        verbose <- FALSE
    if (!hasArg("auto.assign"))
        auto.assign <- TRUE

    if(!exists("force", envir = this.env, inherits = FALSE))
        force = FALSE

    # begin xls area

    if(exists(".YaleUstockMarketData_path2file", envir = env, inherits = FALSE)) {
      assign("tmp", get(".YaleUstockMarketData_path2file", envir = env, inherits = FALSE), envir = this.env, inherits = FALSE)
    } else {
      tmp <- NULL
    }
    if( !is.null(tmp) &&
        !is.na(file.info(tmp)$mtime) && # is.na - if the file does not exist
        !force
    ) {
    } else {

        # possible clean-up
        oldtmp <- tmp
        if(!is.null(oldtmp) && !is.na(file.info(oldtmp)$mtime)) on.exit(unlink(oldtmp))

        YALEU.URL <- "http://www.econ.yale.edu/~shiller/data/ie_data.xls"
        tmp <- tempfile(fileext = ".xls") # ( JAN 2019 )
        #
        # do not remove the 'tmp' file
        # In this R session, keep the file around and available for the next query [if any]
        # the site http://www.econ.yale.edu/~shiller/data/ie_data.xls is NOT engineered
        # to handle MANY data queries, NOR denial of service attacks

        if (verbose)
            cat("downloading ", "YaleUstockMarketData", ".....\n\n")
        quantmod___try.download.file(YALEU.URL, destfile = tmp, quiet = !verbose, mode = "wb", ...)
        assign(".YaleUstockMarketData_path2file", tmp, envir = env, inherits = FALSE)

    }
    if (verbose)
        cat("reading disk file ", tmp, ".....\n\n")

    # Reports somewhere in the time range of the middle (15th,16th, or 17th)
    # of the current month
    Dates <- seq.Date(from = zoo::as.Date("1871-01-01"), to = Sys.Date(), by = "1 month")

    # Last verified: JAN 2019
    col_names = c("Date", "SP500Price", "Dividends", "Earnings", "CPIAUCNSY", "DateFY", "GS10Y", "RealPrice", "RealDividends", "RealEarnings", "CAPE" )

    # data.frame
    fr <- suppressWarnings(readxl::read_xls(path = tmp, sheet = "Data",
           col_names = col_names,
           col_types = c("numeric", rep("numeric",10)), skip = 8L,
           n_max = length(Dates)))

    # optimistic: but the last record
    # (current month 'not yet reported') may be all NAs
    fr <- zoo::na.trim( fr, sides = "right", is.na = "all")

    # FRED historical ... dates are the "first of the month"
    # that datum means "about that entire month"
    # need the Date
    fr[["Date"]] %>% round(2) %>% { . * 100 } %>%
      as.integer %>% as.character %>% { stringr::str_c(., "01") } %>%
        { zoo::as.Date(., format = "%Y%m%d") } -> fr[["Date"]]

    # Date Fraction of Yale (DateFY)
    DateFYYear          <- trunc(fr[["DateFY"]],0)
    DateFYYearFraction  <- fr[["DateFY"]] - DateFYYear
    #
    DateFYYearFirstDate <- zoo::as.Date(stringr::str_c(DateFYYear, "-01-01"))
    IntegerDateFYYearFirstDate <- as.integer(DateFYYearFirstDate)
    #
    DateFYNumbDaysInYear <- Hmisc::yearDays( DateFYYearFirstDate )
    #
    fr[["DateFY"]] <- IntegerDateFYYearFirstDate + DateFYNumbDaysInYear * DateFYYearFraction
    # Stored as the number of days since the UNIX Epoch(UTC time).
    # To get the date, do this.
    # zoo::as.Date(DateFY) # all 15th, 16th, or 17th of the month

    if (verbose)
        cat("done.\n\n")
    fr <- xts(as.matrix(fr[, -1]), zoo::as.Date(fr[[1]], origin = "1970-01-01"),
              src = "YaleU", updated = Sys.time())
    fri <- fr
    rs <- fri

    # end xls area

    # prepare to splice ( nothing to do )

    # splice ( nothing to do )

    rst <- rs
    fr <- rst
    fri <- fr # pass-throught on "YaleUstockMarketData"

    # decompose [if any] into [many] Symbol(s), then return

    for (i in 1:length(Symbols)) {

        # User only wants an individual column
        if(Symbols[[i]] != "YaleUstockMarketData") {
           if (verbose)
             cat("selecting ", Symbols[[i]], ".....\n\n")
          fri <- fr[, colnames(fr)[tolower(colnames(fr)) %in% tolower(Symbols[[i]])]]
        }

        fri <- quantmod___convert.time.series(fr = fri, return.class = return.class)
        if (auto.assign)
            assign(Symbols[[i]], fri, env)
    }
    if (auto.assign)
        return(Symbols)
    return(fri)

})}


#' detect rows with columns of non-NA vaues
#' and other columns of non-NA values
#'
#' detect columns(Var) where the values are
#' not-NA (determined by Ele) but
#' the rest of the columns are
#' NA everywhere else (determined by EleO)
#'
#' [ ] Fix?
#' NOTE: originally a domain-specific solution
#' API however is inconsitent about the
#' other Detect*Rows functions
#'
#' Function is written in DataCombine style
#'
#' @param x data.frame
#' @param Var columns determining complete cases.
#' Can be column names or positions
#' @param Ele "All"(default), of elements in Var, the test results
#' Other option is "Any"
#' @param EleO "All"(default), of elements in 'other than Var', the test results
#' Other option is "Any"
#' @return df with rows of all-NA removed
#' @examples
#' \dontrun{
#' res <- data.frame(A = c(1,NA,NA,NA,5,6), B = c(11,12,NA,NA,15,NA), C = c(101,102,103,NA,NA,NA))
#' res
#'    A  B   C
#' 1  1 11 101
#' 2 NA 12 102
#' 3 NA NA 103
#' 4 NA NA  NA
#' 5  5 15  NA
#' 6  6 NA  NA
#'
#' DetectOnlyNonEmptyVarsInRows(res, Var = c("A","B"))
#' [1] FALSE FALSE FALSE FALSE  TRUE FALSE
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom plyr llply
#' @export
DetectOnlyNonEmptyVarsInRows <- function(x, Var = NULL, Ele = NULL, EleO = NULL) {
tryCatchLog::tryCatchLog({
initEnv(); on.exit({uninitEnv()})

  if(is.null(Var)) Var <- colnames(x)
  if(is.null(Ele)) Ele  <- "All"
  if(is.null(Ele)) EleO <- "All"

  # non-NA in Var
  xVar <-  DetectFullRows(x, Var = Var, Ele = Ele)
  # NA in Var
  if(is.numeric(Var))   NotVar <- -Var
  if(is.character(Var)) NotVar <- !colnames(x) %in% Var # TRUE/FALSE
  if(is.logical(Var))   NotVar <- !Var
  xNotVar <- DetectEmptyRows(x, Var = NotVar, Ele = EleO)

  x <- xVar & xNotVar
  x

})}



#' detect rows with non-NA vaues
#'
#' complete.cases does not work on tibbles!
#' Function is written in DataCombine style
#'
#' @param x data.frame
#' @param Var columns determining complete cases
#' Can be column names or positions
#' Var is the determiner of full rows.
#' Other columns are ignored
#' @param Ele "All"(default), of elements in Var, the test results
#' Other option is "Any"
#' @return df with rows of all-NA removed
#' @examples
#' \dontrun{
#' data.frame(A = c(1,NA,NA), B = c(11,12,NA), C = c(101,102,103))
#'    A  B   C
#' 1  1 11 101
#' 2 NA 12 102
#' 3 NA NA 103
#'
#' DetectFullRows(data.frame(A = c(1,NA,NA), B = c(11,12,NA), C = c(101,102,103)),
#'   Var = c("A","B"))
#' [1]  TRUE FALSE FALSE
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom plyr llply
#' @export
DetectFullRows <- function(x, Var = NULL, Ele = NULL) {
tryCatchLog::tryCatchLog({
initEnv(); on.exit({uninitEnv()})

  if(is.null(Var)) Var <- colnames(x)
  if(is.null(Ele)) Ele <- "All"
  if(Ele == "All") Test <- `&`
  if(Ele == "Any") Test <- `|`

  x <- Reduce(Test, plyr::llply(x[, Var, drop = FALSE], function(x2) { !is.na(unlist(x2)) } ))
  x

})}



#' detect rows with NA vaues
#'
#' complete.cases does not work on tibbles!
#' Function is written in DataCombine style
#'
#' @param x data.frame
#' @param Var columns determining complete cases
#' Can be column names or positions
#' Var is the determiner of empty rows.
#' Other columns are ignored
#' @param Ele "All"(default), of elements in Var, the test results
#' Other option is "Any"
#' @examples
#' \dontrun{
#' data.frame(A = c(1,NA,NA), B = c(11,12,NA), C = c(101,102,103))
#'    A  B   C
#' 1  1 11 101
#' 2 NA 12 102
#' 3 NA NA 103
#'
#' DetectEmptyRows(data.frame(A = c(1,NA,NA), B = c(11,12,NA), C = c(101,102,103)),
#'   Var = c("A","B"))
#' [1] FALSE FALSE  TRUE
#' }
#' @return df with rows of all-NA removed
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom plyr llply
#' @export
DetectEmptyRows <- function(x, Var = NULL, Ele = NULL) {
tryCatchLog::tryCatchLog({
initEnv(); on.exit({uninitEnv()})

  if(is.null(Var)) Var <- colnames(x)
  if(is.null(Ele)) Ele <- "All"
  if(Ele == "All") Test <- `&`
  if(Ele == "Any") Test <- `|`

  x <- Reduce(Test, plyr::llply(x[, Var, drop = FALSE], function(x2) { is.na(unlist(x2)) } ))
  x

})}


#' remove rows with NA vaues
#'
#' complete.cases does not work on tibbles!
#' Function is written in DataCombine style
#'
#' @param x data.frame
#' @param Var columns determining complete cases
#' Can be column names or positions
#' Var is the determiner of empty rows.
#' Other columns are ignored
#' @param Ele "All"(default), of elements in Var, the test results
#' Other option is "Any"
#' @examples
#' \dontrun{
#' data.frame(A = c(1,NA,NA), B = c(11,12,NA), C = c(101,102,103))
#'    A  B   C
#' 1  1 11 101
#' 2 NA 12 102
#' 3 NA NA 103
#'
#' RemoveEmptyRows(data.frame(A = c(1,NA,NA), B = c(11,12,NA), C = c(101,102,103)),
#'   Var = c("A","B"))
#'    A  B   C
#' 1  1 11 101
#' 2 NA 12 102
#'
#' RemoveEmptyRows(data.frame(A = c(1,NA,NA), B = c(11,12,NA), C = c(101,102,103)),
#'    Var = c("A","B"), Ele = "Any")
#' A  B   C
#' 1 1 11 101
#' }
#' @return df with rows of all-NA removed
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom plyr llply
#' @export
RemoveEmptyRows <- function(x, Var = NULL, Ele = NULL) {
tryCatchLog::tryCatchLog({
initEnv(); on.exit({uninitEnv()})

  if(is.null(Var)) Var <- colnames(x)
  if(is.null(Ele)) Ele <- "All"

  # rows to keep
  x <- x[!DetectEmptyRows(x, Var = Var, Ele = Ele),,drop = FALSE]
  x

})}


#' Simfa bond and equity information
#'
#' Note only the monthly dates for the current year and the previous
#' year are available.  The remaining data is year-end-data.
#' The dates intermediate dates are filled in using
#' xts::to.monthly() and seq()
#' The monthly dates are interpolated using zoo::na.approx
#' that is stats::approx. (In R Code that is "approx1").
#'
#' # 1 (HAVE)
#' most valuable information
#' tab "Issuance" column "Corporate Debt" (Symbol is "CorporateDebt")
#' US Bond Market Issuance and Outstanding
#' https://www.sifma.org/wp-content/uploads/2017/06/cm-us-bond-market-sifma.xls
#' https://www.sifma.org/resources/research/us-bond-market-issuance-and-outstanding/
#' https://www.sifma.org/resources/research/bond-chart/
#' upper right hand corner, click on "download xls"
#' click on "explore"
#' SIFMA
#' https://www.sifma.org/
#'
#' same
#' Click on \[Explore\]
#' US Bond Market Issuance and Outstanding
#'   (Upper Right Corner)
#'   DOWNLOAD: JPG|CSV|XLS ( automation: need phantom js ) \*\*HERE\*\*
#'   https://www.sifma.org/wp-content/uploads/2017/06/cm-us-bond-market-sifma.xls
#'     XLS: "Issuance" tab ( has data through the previous month)
#' # XLS bottom ( SEE DEC 31 2018: EXACT SAME AS THE ARTICLE )
#'          Year	M	 Municipal 	 Treasury 	 Mortgage-Related 	 Corporate Debt 	 Federal Agency Securities 	 Asset-Backed 	 Total
#' YTD '17
#' YTD '18
#' % Change
#'
#' # 2 (HAVE)
#' most valuable information
#' tab "Average Maturity" column "Years" (Symbol is "BondsAveMaturity")
#' https://www.sifma.org/wp-content/uploads/2017/08/Corporate-US-Corporate-Issuance-SIFMA.xls
#' US Corporate Bond Issuance
#' https://www.sifma.org/resources/research/us-corporate-bond-issuance/
#' Securities Industry and Financial Markets Association
#' SIFMA
#' https://www.sifma.org/
#' #
#'
#' # 3 (HAVE)
#' most valuable information
#' tab "US Bond Market" column "Corporate Debt" (Symbol is "CorporateDebtTradeVol")
#' US Bond Market Trading Volume
#' https://www.sifma.org/wp-content/uploads/2017/06/cm-us-bond-market-trading-volume-sifma.xls
#' https://www.sifma.org/resources/research/us-bond-market-trading-volume/
#'
#' # 4 (DO NOT HAVE - PROBLEM - CAN NOT READ THE DATETIME COLUMN)
#' US Equity Issuance and Trading Volumes
#' most valuable information
#' tab "Underwriting" column "TOTAL EQUITY"
#' https://www.sifma.org/wp-content/uploads/2017/06/cm-us-equity-sifma1.xls
#' https://www.sifma.org/resources/research/us-equity-stats/
#'
#' main pages ( html list of all excel spreadsheets )
#' https://www.sifma.org/resources/archive/research/statistics/
#' # a.k.a.
#' https://www.sifma.org/resources/archive/research/statistics/?aq=&hPP=10&idx=prod_wp_searchable_posts&ap=0&fR%5Btaxonomies.research_type%5D%5B0%5D=Statistics&is_v=1
#' next two pages
#' https://www.sifma.org/resources/archive/research/statistics/?aq=&hPP=10&idx=prod_wp_searchable_posts&ap=1&fR%5Btaxonomies.research_type%5D%5B0%5D=Statistics&is_v=1
#' https://www.sifma.org/resources/archive/research/statistics/?aq=&hPP=10&idx=prod_wp_searchable_posts&ap=2&fR%5Btaxonomies.research_type%5D%5B0%5D=Statistics&is_v=1
#'
#' referred by Jeff Cox
#' AND
#' Christopher Whalen
#' twitter handle: rcwhalen
#'
#' When the Bid Goes to Zero |
#' https://goo.gl/2tUeJp  |
#' https://twitter.com/rcwhalen/status/1072619534819971072
#' AND
#' When the Bid Goes to Zero
#' December 11, 2018
#' By: R. Christopher Whalen
#' https://www.theinstitutionalriskanalyst.com/single-post/2018/12/11/When-the-Bid-Goes-to-Zero
#'
#' @param Symbols  a character vector specifying the names of each symbol to be loaded
#' Possible Symbols are the following:
#' "SimfaUSCorporate" (means get all of the columns);
#' otherwise, get specific columns;
#' BondsAveMaturity"
#' @param env where to create objects. (.GlobalEnv)
#' @param return.class desired class of returned object.
#' Can be xts, zoo, data.frame, or xts (default)
#' @param force FALSE(default) re-download data from Simfa ( See the examples. )
#' The hidden variable ".SimfaBondMarketData_path2file" located in the
#' environment of parameter env is the last known location of the "xls" file.
#' Generally, using this parameter "force = TRUE"
#' is NOT necessary: the hidden variables are persistent through the
#' entire R session.  Also, send parameter "verbose = TRUE" to see
#' the *path2file location.
#' @param ... additional parameters
#' @return A call to getSymbols.Simfa will load into the specified
#' environment one object for each \code{Symbol} specified,
#' with class defined by \code{return.class}.
#' @author Jeffrey A. Ryan
#' @author Andre Mikulec (adapted original code to work with Simfa Bond market data)
#' @references
#' \cite{US Corporate Bond Issuance \url{https://www.sifma.org/resources/research/us-corporate-bond-issuance/}}
#' @references
#' \cite{R. Christopher Whalen - When the Bid Goes to Zero \url{https://www.theinstitutionalriskanalyst.com/single-post/2018/12/11/When-the-Bid-Goes-to-Zero}}
#' @seealso
#' \code{\link{getSymbols}}
#' \code{\link{setSymbolLookup}}
#' @keywords data
#' @examples
#' \dontrun{
#'
#' # common usage
#' if(!exists("BondsAveMaturity")) getSymbols("BondsAveMaturity", src = "Simfa")
#'
#' getSymbols(c("CorporateDebt","BondsAveMaturity", "CorporateDebtTradeVol"),
#'   src = "Simfa")
#'
#' # force requery of data from the Simfa site
#' # will collect one new xls (historical obs) file
#' getSymbols(c("CorporateDebt","BondsAveMaturity", "CorporateDebtTradeVol"),
#'   src = "Simfa", force = TRUE)
#'
#' # all columns in one xts object
#' getSymbols("SimfaUSCorporate", src = "Simfa")
#' SimfaUSCorporate[, c("CorporateDebt","BondsAveMaturity", "CorporateDebtTradeVol")]
#'
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom zoo as.Date
#' @importFrom plyr llply
#' @importFrom htmltab htmltab
#' @importFrom stringr str_replace str_c str_pad
#' @importFrom readxl read_xls
#' @importFrom zoo na.trim
#' @importFrom DescTools LastDayOfMonth
#' @importFrom DescTools DoCall
#' @importFrom DataCombine FillDown
#' @importFrom hutils if_else
#' @importFrom DataCombine FillDown
#' @importFrom DataCombine MoveFront
#' @importFrom DataCombine VarDrop
# # importFrom Hmisc yearDays
#' @export
getSymbols.Simfa <- function(Symbols, env, return.class = "xts", ...) {
tryCatchLog::tryCatchLog({
initEnv(); on.exit({uninitEnv()})

    importDefaults("getSymbols.Simfa")
    this.env <- environment()
    for (var in names(list(...))) {
        assign(var, list(...)[[var]], this.env)
    }
    if (!hasArg("verbose"))
        verbose <- FALSE
    if (!hasArg("auto.assign"))
        auto.assign <- TRUE

    if(!exists("force", envir = this.env, inherits = FALSE))
        force = FALSE

    # begin xls area

    if(exists(".SimfaUSBondsOutStanding_path2file", envir = env, inherits = FALSE)) {
      assign("tmp", get(".SimfaUSBondsOutStanding_path2file", envir = env, inherits = FALSE), envir = this.env, inherits = FALSE)
    } else {
      tmp <- NULL
    }
    if( !is.null(tmp) &&
        !is.na(file.info(tmp)$mtime) && # is.na - if the file does not exist
        !force
    ) {
    } else {

        # possible clean-up
        oldtmp <- tmp
        if(!is.null(oldtmp) && !is.na(file.info(oldtmp)$mtime)) on.exit(unlink(oldtmp))

        Simfa.URL <- "https://www.sifma.org/wp-content/uploads/2017/06/cm-us-bond-market-sifma.xls"
        tmp <- tempfile(fileext = ".xls") # ( JAN 2019 )
        #
        # do not remove the 'tmp' file
        # In this R session, keep the file around and available for the next query [if any]
        # the site https://www.sifma.org is NOT engineered
        # to handle MANY data queries, NOR denial of service attacks

        if (verbose)
            cat("downloading ", ".SimfaUSBondsOutStanding_path2file", ".....\n\n")
        quantmod___try.download.file(Simfa.URL, destfile = tmp, quiet = !verbose, mode = "wb", ...)
        assign(".SimfaUSBondsOutStanding_path2file", tmp, envir = env, inherits = FALSE)

    }
    if (verbose)
        cat("reading disk file ", tmp, ".....\n\n")

    # Last verified: JAN 2019
    col_names <- c("Year", "Month", "MunicipalDebt", "TreasuryDebt", "MortgageDebt", "CorporateDebt", "FederalDebt", "AssetBackedDebt", "TotalDebt")

    # data.frame
    fr <- suppressWarnings(readxl::read_xls(path = tmp, sheet = "Issuance",
          col_names = col_names,
          col_types = c("text", "text", rep("numeric",7)), skip = 4L
                           )) # n_max = ???

    # remove end blank lines
    fr <- zoo::na.trim( fr, sides = "right", is.na = "all")
    # remove middle blank lines
    fr <- RemoveEmptyRows(fr)
    # fr <- fr[!(is.na(fr[["Year"]]) & !is.na(fr[["Month"]])), , drop = FALSE]
    # remove elements starting with these elements
    fr <- fr[!fr[["Year"]] %Like% "^YTD|Change",,drop = FALSE]
    # copy down elements
    fr <- DataCombine::FillDown(fr, Var = "Year")

    # create the dateindex
    # "Q" becomes the last month of the quarter
    within(fr, { hutils::if_else(Month == "Q1", "3", Month) -> Month;
                 hutils::if_else(Month == "Q2", "6", Month) -> Month;
                 hutils::if_else(Month == "Q3", "9", Month) -> Month;
                 hutils::if_else(Month == "Q4","12", Month) -> Month
    } ) -> fr
    fr[["Month"]][is.na(fr[["Month"]])] <- "12"
    # first of the month
    fr[["TimeDate"]] <- stringr::str_c(fr[["Year"]], "-",
        stringr::str_pad(fr[["Month"]], width = 2, side = "left", pad = "0"), "-01") %>%
          zoo::as.Date()
    # cleanup
    fr <- DataCombine::MoveFront(fr, Var = "TimeDate")
    fr <- DataCombine::VarDrop(fr, Var = c("Year","Month"))

    # FRED historical ... dates are the "first of the month"
    # that datum means "about that entire month"
    # need the Date

    if (verbose)
        cat("done.\n\n")
    fr <- xts(as.matrix(fr[, -1]), zoo::as.Date(fr[[1]], origin = "1970-01-01"),
              src = "Simfa", updated = Sys.time())
    # (prep for interpolation)
    # fill in missing first of the month dates
    fr <- To.Monthly(fr, indexAt = "lastof", OHLC = FALSE)
    # see Shiller
    # Uses stats::approx
    # (only useful because I want some false-ish historical data)
    fr <- zoo::na.approx(fr)
    # St. Louis FRED pattern of the 1st of the month repesents the "entire" month
    fr <- To.Monthly(fr, indexAt = "firstof", OHLC = FALSE)

    fri <- fr
    rs <- fri

    rs1 <- fri1 <- rs

    # end xls area

    # begin xls area

    if(exists(".SimfaUSCorporateBondIssuance_path2file", envir = env, inherits = FALSE)) {
      assign("tmp", get(".SimfaUSCorporateBondIssuance_path2file", envir = env, inherits = FALSE), envir = this.env, inherits = FALSE)
    } else {
      tmp <- NULL
    }
    if( !is.null(tmp) &&
        !is.na(file.info(tmp)$mtime) && # is.na - if the file does not exist
        !force
    ) {
    } else {

        # possible clean-up
        oldtmp <- tmp
        if(!is.null(oldtmp) && !is.na(file.info(oldtmp)$mtime)) on.exit(unlink(oldtmp))

        Simfa.URL <- "https://www.sifma.org/wp-content/uploads/2017/08/Corporate-US-Corporate-Issuance-SIFMA.xls"
        tmp <- tempfile(fileext = ".xls") # ( JAN 2019 )
        #
        # do not remove the 'tmp' file
        # In this R session, keep the file around and available for the next query [if any]
        # the site https://www.sifma.org is NOT engineered
        # to handle MANY data queries, NOR denial of service attacks

        if (verbose)
            cat("downloading ", ".SimfaUSCorporateBondIssuance_path2file", ".....\n\n")
        quantmod___try.download.file(Simfa.URL, destfile = tmp, quiet = !verbose, mode = "wb", ...)
        assign(".SimfaUSCorporateBondIssuance_path2file", tmp, envir = env, inherits = FALSE)

    }
    if (verbose)
        cat("reading disk file ", tmp, ".....\n\n")

    # Reports somewhere in the time range of the middle (15th,16th, or 17th)
    # of the current month
    # Dates <- seq.Date(from = zoo::as.Date("1871-01-01"), to = Sys.Date(), by = "1 month")

    # Last verified: JAN 2019
    col_names = c("TimeDate", "BondsAveMaturity")

    # data.frame
    fr <- suppressWarnings(readxl::read_xls(path = tmp, sheet = "Average Maturity",
          col_names = col_names,
          col_types = c("text", rep("numeric",1)), skip = 5L
                           )) # n_max = ???

    # optimistic: but the last record
    # (current month 'not yet reported') may be all NAs
    fr <- zoo::na.trim( fr, sides = "right", is.na = "all")

    # remove fully empty rows
    fr <- RemoveEmptyRows(fr)

    # Year headers
    LoneYearElementIndexes <- which(DetectOnlyNonEmptyVarsInRows(fr, Var = "TimeDate"), TRUE)
    LoneYearElementIndexes <- LoneYearElementIndexes[fr[["TimeDate"]][LoneYearElementIndexes + 1] == "Jan"]

    # range of years or year-months
    Splitted <- split(fr, findInterval(seq_len(NROW(fr)), LoneYearElementIndexes , left.open = FALSE))

    # convert column TimeDate to YYYY-Mon-01
    plyr::llply(Splitted, function(x) {

      TimeDate <- x[["TimeDate"]]

      # first element is always a year
      ThisYear <- TimeDate[1]

      # second element
      if(!is.na(TimeDate[2])) {
        # if the second element is not a year then it must be a month
        if(!TimeDate[2] %Like% "^\\d{4}$") {
          # convert Mon to YYYY-Mon-01
          NotYearElementIndexes <- 2:length(TimeDate)
          TimeDate[NotYearElementIndexes] <-
           stringr::str_c( ThisYear, "-", TimeDate[NotYearElementIndexes], "-01") %>%
             # for correct interpolation
             { DescTools::LastDayOfMonth(zoo::as.Date(., format = "%Y-%b-%d")) } %>% as.character
            # Year-alone element is no longer needed
            x <- x[-1, , drop = FALSE]
            TimeDate <- TimeDate[-1]
            x[["TimeDate"]] <- TimeDate
        } else { # second element ( and all col elements are years )
          x[["TimeDate"]] <- stringr::str_c(TimeDate, "-Dec-31") %>%
            { zoo::as.Date(., format = "%Y-%b-%d") }  %>% as.character # for correct interpolation
          # The next set contains the Month to Month
          # chop off the last value (it is already covered by the next set's month to month)
          x <- x[-NROW(x), , drop = FALSE]
        }
      } else {
        # just this lone-element is a year
        x[["TimeDate"]] <- stringr::str_c(TimeDate, "-Dec-31") %>%  # for correct interpolation
         { zoo::as.Date(., format = "%Y-%b-%d") } %>% as.character
        # chop off the last value (it is already covered by the next set's month to month)
        x <- x[0,,drop = FALSE]
      }

      x

    }) -> ListOfDFs
    DescTools::DoCall(rbind,ListOfDFs) -> fr
    fr[["TimeDate"]] <- zoo::as.Date(fr[["TimeDate"]])

    # FRED historical ... dates are the "first of the month"
    # that datum means "about that entire month"
    # need the Date

    # SAVED HERE ( EXAMPLE OF CALCULATING EXACT DATES FROM MATHEMATICAL YEARL FRACTIONS)
    # BUT WHAT DID I USE TO CALCULATE SHILLER'S FRACTIONS? (MIDDLE COLUMN?)
    #
    # fr[["Date"]] %>% round(2) %>% { . * 100 } %>%
    #   as.integer %>% as.character %>% { stringr::str_c(., "01") } %>%
    #     { zoo::as.Date(., format = "%Y%m%d") } -> fr[["Date"]]
    #
    # # Date Fraction of Yale (DateFY)
    # DateFYYear          <- trunc(fr[["DateFY"]],0)
    # DateFYYearFraction  <- fr[["DateFY"]] - DateFYYear
    # #
    # DateFYYearFirstDate <- zoo::as.Date(stringr::str_c(DateFYYear, "-01-01"))
    # IntegerDateFYYearFirstDate <- as.integer(DateFYYearFirstDate)
    # #
    # DateFYNumbDaysInYear <- Hmisc::yearDays( DateFYYearFirstDate )
    # #
    # fr[["DateFY"]] <- IntegerDateFYYearFirstDate + DateFYNumbDaysInYear * DateFYYearFraction
    # # Stored as the number of days since the UNIX Epoch(UTC time).
    # # To get the date, do this.
    # # zoo::as.Date(DateFY) # all 15th, 16th, or 17th of the month

    if (verbose)
        cat("done.\n\n")
    fr <- xts(as.matrix(fr[, -1]), zoo::as.Date(fr[[1]], origin = "1970-01-01"),
              src = "Simfa", updated = Sys.time())
    # (prep for interpolation)
    # fill in missing first of the month dates
    fr <- To.Monthly(fr, indexAt = "lastof", OHLC = FALSE)
    # see Shiller
    # Uses stats::approx
    # (only useful because I want some false-ish historical data)
    fr <- zoo::na.approx(fr)
    # St. Louis FRED pattern of the 1st of the month repesents the "entire" month
    fr <- To.Monthly(fr, indexAt = "firstof", OHLC = FALSE)

    fri <- fr
    rs <- fri
    rs2 <- fri2 <- rs

    # end xls area


    # begin xls area

    if(exists(".SimfaUSCorporateBondTradeVolume_path2file", envir = env, inherits = FALSE)) {
      assign("tmp", get(".SimfaUSCorporateBondTradeVolume_path2file", envir = env, inherits = FALSE), envir = this.env, inherits = FALSE)
    } else {
      tmp <- NULL
    }
    if( !is.null(tmp) &&
        !is.na(file.info(tmp)$mtime) && # is.na - if the file does not exist
        !force
    ) {
    } else {

        # possible clean-up
        oldtmp <- tmp
        if(!is.null(oldtmp) && !is.na(file.info(oldtmp)$mtime)) on.exit(unlink(oldtmp))

        Simfa.URL <- "https://www.sifma.org/wp-content/uploads/2017/06/cm-us-bond-market-trading-volume-sifma.xls"
        tmp <- tempfile(fileext = ".xls") # ( JAN 2019 )
        #
        # do not remove the 'tmp' file
        # In this R session, keep the file around and available for the next query [if any]
        # the site https://www.sifma.org is NOT engineered
        # to handle MANY data queries, NOR denial of service attacks

        if (verbose)
            cat("downloading ", ".SimfaUSCorporateBondTradeVolume_path2file", ".....\n\n")
        quantmod___try.download.file(Simfa.URL, destfile = tmp, quiet = !verbose, mode = "wb", ...)
        assign(".SimfaUSCorporateBondTradeVolume_path2file", tmp, envir = env, inherits = FALSE)

    }
    if (verbose)
        cat("reading disk file ", tmp, ".....\n\n")

    # Last verified: JAN 2019
    col_names = c("TimeDate",  "MunicipalTradeVol", "TreasureTradeVol",
                               "AgencyMBSTradeVol", "NonAgencyMBSTradeVol",
                               "ABSTradeVol",       "CorporateDebtTradeVol",
                               "FederalTradeVol")

    fr <- suppressWarnings(readxl::read_xls(path = tmp, sheet = "US Bond Market",
          col_names = col_names,
          col_types = c("text", rep("numeric",7)), skip = 5L
                           )) # n_max = ???

    # I am only interested
    fr <- fr[, c("TimeDate", "CorporateDebtTradeVol"), drop = FALSE]

    # optimistic: but the last record
    # (current month 'not yet reported') may be all NAs
    fr <- zoo::na.trim( fr, sides = "right", is.na = "all")

    # remove elements starting with these elements
    fr <- fr[!fr[["TimeDate"]] %Like% "^YTD|Change",,drop = FALSE]

    # last empty line detected
    PositionBeforeYearSummaries <- tail(which(DetectEmptyRows(fr)),1)
    # from here to the end
    RowsToRemove <- seq(PositionBeforeYearSummaries + 1, NROW(fr), by = 1)
    fr <- fr[-RowsToRemove,,drop = FALSE]

    # remove fully empty rows
    fr <- RemoveEmptyRows(fr)

    # Year headers
    LoneYearElementIndexes <- which(DetectOnlyNonEmptyVarsInRows(fr, Var = "TimeDate"), TRUE)
    LoneYearElementIndexes <- LoneYearElementIndexes[fr[["TimeDate"]][LoneYearElementIndexes + 1] == "Jan"]

    # range of years or year-months
    Splitted <- split(fr, findInterval(seq_len(NROW(fr)), LoneYearElementIndexes , left.open = FALSE))

    # convert column TimeDate to YYYY-Mon-01
    plyr::llply(Splitted, function(x) {

      TimeDate <- x[["TimeDate"]]

      # first element is always a year
      ThisYear <- TimeDate[1]

      # second element
      if(!is.na(TimeDate[2])) {
        # if the second element is not a year then it must be a month
        if(!TimeDate[2] %Like% "^\\d{4}$") {
          # convert Mon to YYYY-Mon-01
          NotYearElementIndexes <- 2:length(TimeDate)
          TimeDate[NotYearElementIndexes] <-
           stringr::str_c( ThisYear, "-", TimeDate[NotYearElementIndexes], "-01") %>%
             # for correct interpolation
             { DescTools::LastDayOfMonth(zoo::as.Date(., format = "%Y-%b-%d")) } %>% as.character
            # Year-alone element is no longer needed
            x <- x[-1, , drop = FALSE]
            TimeDate <- TimeDate[-1]
            x[["TimeDate"]] <- TimeDate
        } else { # second element ( and all col elements are years )
          x[["TimeDate"]] <- stringr::str_c(TimeDate, "-Dec-31") %>%
            { zoo::as.Date(., format = "%Y-%b-%d") }  %>% as.character # for correct interpolation
          # The next set contains the Month to Month
          # chop off the last value (it is already covered by the next set's month to month)
          x <- x[-NROW(x), , drop = FALSE]
          # chop off the next to last value (it is already covered by the next set's month to month)
          x <- x[-NROW(x), , drop = FALSE]
        }
      } else {
        # just this lone-element is a year
        x[["TimeDate"]] <- stringr::str_c(TimeDate, "-Dec-31") %>%  # for correct interpolation
           { zoo::as.Date(., format = "%Y-%b-%d") } %>% as.character
        # chop off the last value (it is already covered by the next set's month to month)
        x <- x[0,,drop = FALSE]
      }

      x

    }) -> ListOfDFs
    DescTools::DoCall(rbind,ListOfDFs) -> fr
    fr[["TimeDate"]] <- zoo::as.Date(fr[["TimeDate"]])

    # FRED historical ... dates are the "first of the month"
    # that datum means "about that entire month"
    # need the Date

    if (verbose)
        cat("done.\n\n")
    fr <- xts(as.matrix(fr[, -1]), zoo::as.Date(fr[[1]], origin = "1970-01-01"),
              src = "Simfa", updated = Sys.time())
    # (prep for interpolation)
    # fill in missing first of the month dates
    fr <- To.Monthly(fr, indexAt = "lastof", OHLC = FALSE)
    # see Shiller
    # Uses stats::approx
    # (only useful because I want some false-ish historical data)
    fr <- zoo::na.approx(fr)
    # St. Louis FRED pattern of the 1st of the month repesents the "entire" month
    fr <- To.Monthly(fr, indexAt = "firstof", OHLC = FALSE)

    fri <- fr
    rs <- fri
    rs3 <- fri3 <- rs

    # end xls area


    # # begin xls area
    #
    # if(exists(".SimfaUSEquityUnderwriting_path2file", envir = env, inherits = FALSE)) {
    #   assign("tmp", get(".SimfaUSEquityUnderwriting_path2file", envir = env, inherits = FALSE), envir = this.env, inherits = FALSE)
    # } else {
    #   tmp <- NULL
    # }
    # if( !is.null(tmp) &&
    #     !is.na(file.info(tmp)$mtime) && # is.na - if the file does not exist
    #     !force
    # ) {
    # } else {
    #
    #     # possible clean-up
    #     oldtmp <- tmp
    #     if(!is.null(oldtmp) && !is.na(file.info(oldtmp)$mtime)) on.exit(unlink(oldtmp))
    #
    #     Simfa.URL <- "https://www.sifma.org/wp-content/uploads/2017/06/cm-us-equity-sifma1.xls"
    #     tmp <- tempfile(fileext = ".xls") # ( JAN 2019 )
    #     #
    #     # do not remove the 'tmp' file
    #     # In this R session, keep the file around and available for the next query [if any]
    #     # the site https://www.sifma.org is NOT engineered
    #     # to handle MANY data queries, NOR denial of service attacks
    #
    #     if (verbose)
    #         cat("downloading ", ".SimfaUSEquityUnderwriting_path2file", ".....\n\n")
    #     quantmod___try.download.file(Simfa.URL, destfile = tmp, quiet = !verbose, mode = "wb", ...)
    #     assign(".SimfaUSEquityUnderwriting_path2file", tmp, envir = env, inherits = FALSE)
    #
    # }
    # if (verbose)
    #     cat("reading disk file ", tmp, ".....\n\n")
    #
    # # Last verified: JAN 2019
    #
    # col_names = c("TimeDate", "CommonStockUnderAct", "PreferredStockUnderAct", "TOTALEQUITYUnderAct",
    #                           "AllIPOsUnderAct", "TrueIPOsUnderAct", "SecondaryUnderAct")
    #
    # # PROBLEM TIMEDATE COLUMN IS NOT READABLE HALF-READ (HALF CONVERTED TO INTEGERS)
    # # data.frame
    # fr <- suppressWarnings(readxl::read_xls(path = tmp, sheet = "Underwriting",
    #       col_names = col_names,
    #       col_types = c("text", rep("numeric",6)), skip = 6L
    #                        )) # n_max = ???
    #
    # # only one of Interest
    # fr <- fr[, c("TimeDate","TOTALEQUITYUnderAct"), drop = FALSE]
    #
    # # optimistic: but the last record
    # # (current month 'not yet reported') may be all NAs
    # fr <- zoo::na.trim( fr, sides = "right", is.na = "all")
    #
    # # remove fully empty rows (should be a function)
    # fr <- RemoveEmptyRows(fr)
    #
    # # dateindex
    # fr[["TimeDate"]][fr[["TimeDate"]] %Like% "^\\d{4}$"] <-
    #   stringr::str_c(fr[["TimeDate"]][fr[["TimeDate"]] %Like% "^\\d{4}$"], "-12-31")
    #
    # fr[["TimeDate"]][!fr[["TimeDate"]] %Like% "^\\d{4}$"] <-
    #   zoo::as.Date(stringr::str_c("01-", fr[["TimeDate"]][!fr[["TimeDate"]] %Like% "^\\d{4}$"]), format = "%d-%b-%y") %>%
    #     { DescTools::LastDayOfMonth } %>% as.character
    #
    # fr[["TimeDate"]] <- zoo::as.Date(fr[["TimeDate"]])
    #
    # # FRED historical ... dates are the "first of the month"
    # # that datum means "about that entire month"
    # # need the Date
    #
    # if (verbose)
    #     cat("done.\n\n")
    # fr <- xts(as.matrix(fr[, -1]), zoo::as.Date(fr[[1]], origin = "1970-01-01"),
    #           src = "Simfa", updated = Sys.time())
    # # (prep for interpolation)
    # # fill in missing first of the month dates
    # fr <- To.Monthly(fr, indexAt = "lastof", OHLC = FALSE)
    # # see Shiller
    # # Uses stats::approx
    # # (only useful because I want some false-ish historical data)
    # fr <- zoo::na.approx(fr)
    # # St. Louis FRED pattern of the 1st of the month repesents the "entire" month
    # fr <- To.Monthly(fr, indexAt = "firstof", OHLC = FALSE)
    #
    # fri <- fr
    # rs <- fri
    # rs4 <- fri4 <- rs

    # end xls area

    # prepare to splice ( nothing to do )

    # splice
    # S3 dispatch
    ### rs <- merge(rs1,rs2,rs3,rs4)
    rs <- merge(rs1,rs2,rs3)

    rst <- rs
    fr <- rst
    fri <- fr # pass-throught on "SimfaBondMarketData"

    # decompose [if any] into [many] Symbol(s), then return

    for (i in 1:length(Symbols)) {

        # User only wants an individual column
        if(Symbols[[i]] != "SimfaUSCorporate") {
           if (verbose)
             cat("selecting ", Symbols[[i]], ".....\n\n")
          fri <- fr[, colnames(fr)[tolower(colnames(fr)) %in% tolower(Symbols[[i]])]]
        }

        fri <- quantmod___convert.time.series(fr = fri, return.class = return.class)
        if (auto.assign)
            assign(Symbols[[i]], fri, env)
    }
    if (auto.assign)
        return(Symbols)
    return(fri)

})}




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
#' @importFrom plyr llply
#' @importFrom tryCatchLog tryCatchLog
getSymbols <- function (Symbols = NULL, env = parent.frame(), reload.Symbols = FALSE,
    verbose = FALSE, warnings = TRUE, src = "yahoo", symbol.lookup = TRUE,
    auto.assign = getOption("getSymbols.auto.assign", TRUE), source.envir = NULL, # BEGIN/END NEW CODE
    Fun = NULL, ...)                                                                        {
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
              FunStr <- NULL
              Dots <- list(...)
              if(!is.null(Fun) && is.null(Dots[["FunStr"]])) {
                FunStr <- as.character(substitute(Fun))
                Fun <- match.fun(Fun)
              }
            if(!is.null(Dots[["FunStr"]]) && !exists("FunStr", envir = environment(), inherits = FALSE))
              FunStr <- Dots[["FunStr"]]
            # END NEW CODE
            symbols.returned <- do.call(paste("getSymbols.",
                symbol.source, sep = ""), list(Symbols = current.symbols,
                env = env, verbose = verbose, warnings = warnings,
                auto.assign = auto.assign, Fun = Fun, FunStr = FunStr, ...)) # NEW CODE , Fun = Fun, FunStr = FunStr
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
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
#' @importFrom stringr str_c
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
        fr <- get(stringr::str_c(".", Symbols[[i]]), envir =  cache.envir)
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
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom DBI dbSendQuery fetch  dbGetQuery dbDisconnect dbQuoteIdentifier dbQuoteString
#' @importFrom stringr str_c
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

  conWasOrigNULL <- FALSE
  if(is.null(con)) {
    DBConMeta <- pgConnect(user=user,password=password,dbname=dbname,schname=schname,host=host,port=port,options=options,forceISOdate=forceISOdate)
    AssignEnv(DBConMeta, c("con"))
    conWasOrigNULL <- TRUE
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

  if(schname != "") { dotSchemaQuoted <- stringr::str_c(DBI::dbQuoteIdentifier(con, schname), ".") } else { dotSchemaQuoted <- "" }

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
    schemaSymbolsQuoted[[i]] <-  stringr::str_c(dotSchemaQuoted, DBI::dbQuoteIdentifier(con, Symbols[[i]]))

    if(sum(c("o", "h", "l", "c") %in% db.fields) == 4) {
      Selection <- paste( db.fieldsQuoted , collapse=',')
      OHLCData <- TRUE
    } else {
      # NO MATCHES, THEN MAYBE A "SINGLE COLUMN (FRED) DATA"
      Selection <- "*"
      OHLCData <- FALSE
    }

    # ABOVE (^) ALREADY QUOTED
    query <- stringr::str_c("SELECT ", Selection," FROM ", schemaSymbolsQuoted[[i]]," ORDER BY ", DBI::dbQuoteIdentifier(con, "date"), ";")
    rs <- DBI::dbSendQuery(con, query)
    fr <- DBI::fetch(rs, n=-1)

    query <- stringr::str_c("SELECT ", " * ", " FROM ", DBI::dbQuoteIdentifier(con, schname), ".", DBI::dbQuoteIdentifier(con, "Symbols")," WHERE ", DBI::dbQuoteIdentifier(con, "Symbols"), " = ", DBI::dbQuoteString(con, Symbols[[i]]), ";")
    SymbolAttributes <- DBI::dbGetQuery(con, query)[,-1,drop =FALSE]

    updated <- NULL
    if("updated" %in% colnames(SymbolAttributes)) updated <- SymbolAttributes[["updated"]]

    index_R_class <- NULL
    if("index_R_class" %in% colnames(SymbolAttributes)) {
      index_R_class <- SymbolAttributes[["index_R_class"]]
      if(!index_R_class %in% c("ts","data.frame")) {
        if(!isNamespaceLoaded(index_R_class)) requireNamespace(index_R_class, quietly = TRUE)
      }
      updated <- rlang::eval_bare(rlang::parse_expr(stringr::str_c("as.", index_R_class,"(updated)")), environment())
    }
    src <- NULL
    if("src" %in% colnames(SymbolAttributes)) src <- SymbolAttributes[["src"]]

    # SHOULD I HAVE THIS HERE?  I DO NOT ANYWHERE ELSE
    # [ ] REMOVE ME
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
  if(conWasOrigNULL)
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
#' @importFrom tryCatchLog tryCatchLog
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
      auto.assign = FALSE, source.envir = source.envir, ...)

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
      auto.assign = FALSE, source.envir = source.envir, ...)

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
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom plyr llply
#' @importFrom stringr str_c
#' @importFrom DescTools DoCall
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
    stop(stringr::str_c("In saveSymbols, duplicate case-insenstive names found in ", paste0(SamexTsGetSymbols, collapse = ", ")))
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
    DescTools::DoCall(saveSymbols.RData, c(list(), source.envir = list2env(xTsGetSymbols), Dots))
  }
  if("trg" %in% names(Dots)) {
    DescTools::DoCall(stringr::str_c("saveSymbols",".", Dots[["trg"]]), c(list(),
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
#' @importFrom tryCatchLog tryCatchLog
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
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_c
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
        assign(stringr::str_c(".", each.symbol), source.list[[each.symbol]], envir = cache.envir)
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
#'#   "index_R_class" text,
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
#' @param keys passed to dfToCREATETable and dfGetPKEYNames
#' @param placeNewRecords "AddOnlyNew"(default)
#' Append 'new' records determined by 'new' inbound key values
#' to the table using pgInsert.
#' The other choice is "TruncateTable". TRUNCATE TABLE the table contents.
#' Follow by running "pgInsert"
#' NOTE, user choice is specific to the nature of the changing source
#' data.  For example, FRED historical data is typically static: e.g. UNRATE
#' (but maybe not all historic data in FRED is static.  Therefore, in the
#' case of most FRED data, a good choice may be, "AddOnlyNew". Differently,
#' Yahoo finanical stock data is very often re-adjuste to account
#' for split and dividends, therefore yesterday's query on data may
#' be useless.  Therefore, in the case of Yahoo financial stock data, a
#' good choice may be "TruncateTable"
#' @param varHint if placeNewRecords = "AddOnlyNew", then passed to pgInsert. See ? pgInsert
#' @param valHint if placeNewRecords = "AddOnlyNew", then passed to pgInsert. See ? pgInsert
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
#' # maybe appropriate for Yahoo data
#' # same placeNewRecords = "TruncateTable" (default)
#' saveSymbols(trg = "pg", source.envir = source.envir,
#'   placeNewRecords = "TruncateTable")
#'
#' unrate <- getSymbols("UNRATE", src = "FRED", auto.assign =  F)
#' saveSymbols(trg = "pg", source.envir = list2env(list(UNRATE = unrate)))
#'
#' # maybe appropriate for FRED data
#' saveSymbols(trg = "pg", source.envir = list2env(list(UNRATE = unrate)),
#'   placeNewRecords = "AddOnlyNew")
#'
#' unrate.db <- getSymbols("UNRATE", src = "pg", auto.assign =  F)
#' unrate.db <- getSymbols(Symbols = "UNRATE", src = "pg", auto.assign = F)
#'
#' }
#' @export
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_c
#' @importFrom DBI dbExecute dbWriteTable dbQuoteString dbQuoteIdentifier dbDisconnect
saveSymbols.PostgreSQL <- function(Symbols = NULL, con = NULL, source.envir = NULL,
  field.names = c('Open','High','Low','Close','Volume','Adjusted'),
  db.fields=c('o','h','l','c','v','a'),
  keys = NULL, placeNewRecords = NULL, varHint = NULL, valHint = NULL,
  user=NULL,password=NULL,dbname=NULL,schname=NULL,host='localhost',port=5432,
  options=NULL, forceISOdate = TRUE,
  ...)  {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})
  importDefaults("saveSymbols.PostgreSQL")

  if(is.null(placeNewRecords)) placeNewRecords <- "AddOnlyNew" # "TruncateTable"

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

  conWasOrigNULL <- FALSE
  if(is.null(con)) {
    DBConMeta <- pgConnect(user=user,password=password,dbname=dbname,schname=schname,host=host,port=port,options=options,forceISOdate=forceISOdate)
    AssignEnv(DBConMeta, c("con"))
    conWasOrigNULL <- TRUE
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
  newCREATEdTables <- vector(mode = "character")

  # ORIGINAL quantmod-ism FOLLOWS ... [ ] could be cleaned up

  if(length(Symbols) != sum(Symbols %in% db.Symbols)) {
    missing.db.symbol <- Symbols[!Symbols %in% db.Symbols]
    # for the requested Symbol, if I do not have database table so, I have to create ONE

    # create *new* empty TABLEs
    dfs <- list()
    new.db.symbol <- c()

    for (each.symbol in  missing.db.symbol) {

      xTs <- as.list(source.envir)[[each.symbol]]
      if(is.null(xTs)) { message(paste("Symbol ", each.symbol, " was not found, so skipping.")); next }
      df  <- xTs2DBDF(xTs = xTs, con = con, field.names = field.names[-1], db.fields = db.fields[-1])

      # create NEW CREATE TABLE statements"
      # column names
      #
      # dfToCREATETable(df = df, con = con, Symbol = each.symbol, schname = schname, keys = keys)
      pgAddTable(con, trgt = each.symbol, keys = keys, schname = schname, df = df)
      #
      newCREATEdTables <- c(newCREATEdTables, each.symbol)

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

    # IMPORTANT!!
    # NOTE, IF THE STRUCTURE of THE incoming OBJECT is different from the STORED object
    # E.G.! NEW COLUMNS, DIFFERENT R/POSTGRESQL CLASS OF the index
    # then MAYBE dbWriteTable WILL FAIL? and/or
    # (I HAVE NOT HANDLED THESE 'change' CASES)
    # NOTE: the INDEX type probably will not change
    # BUT getting NEW added COLUMNS would be COMMON
    # so I WOULD  have to do:
    # (1) detect new column name (2) ADD COLUMN

    # of any columns that exist in df but do not exist on the Server DB,
    # add those new columns to the Server DB
    pgAddColumnType(con, trgt = each.symbol, schname = schname, df = df)

    # Goal
    # exist in  R       'Date', 'Open','High','Low','Close','Volume','Adjusted'( along with FRED columns )
    # translate from those
    # to DB:            'date', 'o','h','l','c','v','a' ( along with FRED columns "as is")

    # custom sorting
    db.fields    <- customSorting( colnames(df), InitOrder = db.fields, CI = TRUE)
    colnames(df) <- db.fields

    if(schname != "") {
      dotSchemaQuoted <- stringr::str_c(DBI::dbQuoteIdentifier(con, schname), ".")
    } else {
      dotSchemaQuoted <- ""
    }

    # see ?PrimaryKeyCols on how NULL 'keys' is interpreted
    PrimaryKeyCols <- dfGetPKEYNames(df = df, keys = keys)

    if(placeNewRecords == "TruncateTable") {
      message("saveSymbols.PostgreSQL is doing TruncateTable")
      DBI::dbExecute(con, stringr::str_c("TRUNCATE TABLE ", dotSchemaQuoted, DBI::dbQuoteIdentifier(con, each.symbol), ";"))

      # DBI::dbWriteTable(con, c(schname ,each.symbol), df, append = T, row.names = F)
      # if  each.symbol includes schema name + ".",
      # then dbWriteTable will return TRUE, but it will LIE
      #
      # NOTE: pgInsert calls DBI::dbWriteTable that appends data
      pgInsert(con, trgt = each.symbol, keys = PrimaryKeyCols, schname = schname, df = df, varHint = varHint, valHint = valHint, ...)

      # pgAdmin3 LTS (BigSQL) mentioned that I should do
      DBI::dbExecute(con, stringr::str_c("VACUUM (VERBOSE, ANALYZE) ", dotSchemaQuoted, DBI::dbQuoteIdentifier(con, each.symbol), ";"))
    }
    if((placeNewRecords == "AddOnlyNew") || (placeNewRecords == "AddNewUpdateOld")) {
      message("saveSymbols.PostgreSQL is doing AddOnlyNew")

      # NOTE: pgInsert calls DBI::dbWriteTable that appends data
      pgInsert(con, trgt = each.symbol, keys = PrimaryKeyCols, schname = schname, df = df, varHint = varHint, valHint = valHint, ...)

      if(placeNewRecords == "AddNewUpdateOld") {
        message("saveSymbols.PostgreSQL is doing AddNewUpdateOld")

        # current rows that already exist on the server ( uniquely identified by keys )
        pgUpdate(con, trgt = each.symbol, keys = PrimaryKeyCols, schname = schname, df = df, varHint = varHint, valHint = valHint, ... )

      }

      if(each.symbol %in% newCREATEdTables) {
        # assume a MASS amount of data has been uploaded
        # (this is typically the case if a tables has been newly created)

        # pgAdmin3 LTS (BigSQL) mentioned that I should do
        DBI::dbExecute(con, stringr::str_c("VACUUM (VERBOSE, ANALYZE) ", dotSchemaQuoted, DBI::dbQuoteIdentifier(con, each.symbol), ";"))
      }

    }

    updated <- NULL
    if("updated" %in% names(attributes(xTs))) {
      # OLD: "to_timestamp(", DBI::dbQuoteString(con, as.character(Sys.time())), " ,'YYYY-MM-DD HH24:MI:SS')"
      updated <- attributes(xTs)[["updated"]]
      # How to properly handle timezone when passing POSIXct objects between R and Postgres DBMS?
      # https://stackoverflow.com/questions/40524225/how-to-properly-handle-timezone-when-passing-posixct-objects-between-r-and-postg
      updated <- format(as.POSIXct(updated, tz = Sys.getenv("TZ")), "%Y-%m-%d %H:%M:%OS%z")
      DBI::dbExecute(con, stringr::str_c("UPDATE ", DBI::dbQuoteIdentifier(con, schname), ".", DBI::dbQuoteIdentifier(con, "Symbols"),
                            " SET ",
                            DBI::dbQuoteIdentifier(con, "updated"), " = ", DBI::dbQuoteString(con, updated),
                            " WHERE ",
                            DBI::dbQuoteIdentifier(con, "Symbols"), " = ", DBI::dbQuoteString(con, each.symbol),
                            ";"))
    }

    index_R_class <- NULL
    if(inherits(xTs,"zoo")) {
      index_R_class <- class(index(xTs))[1]
      DBI::dbExecute(con, stringr::str_c("UPDATE ", DBI::dbQuoteIdentifier(con, schname), ".", DBI::dbQuoteIdentifier(con, "Symbols"),
                            " SET ",
                            DBI::dbQuoteIdentifier(con, "index_R_class")," = ", DBI::dbQuoteString(con, index_R_class),
                            " WHERE ",
                            DBI::dbQuoteIdentifier(con, "Symbols"), " = ", DBI::dbQuoteString(con, each.symbol),
                            ";"))
    }
    src <- NULL
    if("src" %in% names(attributes(xTs))) {
      src <- as.character(attributes(xTs)[["src"]])
      DBI::dbExecute(con, stringr::str_c("UPDATE ", DBI::dbQuoteIdentifier(con, schname), ".", DBI::dbQuoteIdentifier(con, "Symbols"),
                            " SET ",
                            DBI::dbQuoteIdentifier(con, "src"), " = ", DBI::dbQuoteString(con, src),
                            " WHERE ",
                            DBI::dbQuoteIdentifier(con, "Symbols"), " = ", DBI::dbQuoteString(con, each.symbol),
                            ";"))
    }

  }
  if(conWasOrigNULL)
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
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom plyr llply
#' @importFrom stringr str_c
#' @importFrom DescTools DoCall
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
    SrcSymbols <- DescTools::DoCall(stringr::str_c("updatedSymbols",".", Dots[["src"]]), c(list(),
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
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
#' @importFrom plyr llply
updatedSymbols.cache <- function(Symbols = NULL, cache.envir = NULL, ...) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})
  importDefaults("updatedSymbols.cache")

  if(is.null(cache.envir)) cache.envir <- .GlobalEnv

  AllSymbols <- updatedSymbols(source.envir = cache.envir)

  # LessAllSymbols   <- AllSymbols[stringr::str_detect(Names(AllSymbols),"^[.].+") &
  #                               !stringr::str_detect(Names(AllSymbols),"^[.]Random[.]seed$")]
  # cache stored symbols require .UPPPECASE ( so I can disinquish from other NON-symbols)
  ## LessAllSymbols   <- AllSymbols[stringr::str_detect(Names(AllSymbols),"^[.][A-Z]+")]
  ## Names(LessAllSymbols) <- stringr::str_replace(Names(LessAllSymbols), "^[.]","")
  ## RetrievedSymbols <- LessAllSymbols
  ## RetrievedSymbols

  if(!is.null(Symbols)) {

    # remove the beginning dot (if any)
    Names(AllSymbols) <- stringr::str_replace(Names(AllSymbols), "^[.]","")
    return(AllSymbols[Names(AllSymbols) %in% Symbols])

  } else { # take a guess (1) EVERYTHING that begins with a dot(.) anded_with
           #              (2) EVERYTHING that updatedSymbols(FROM ABOVE) returned non-NA data

    LessAllSymbols   <- AllSymbols[stringr::str_detect(Names(AllSymbols),"^[.].+")]
    Names(LessAllSymbols) <- stringr::str_replace(Names(LessAllSymbols), "^[.]","")
    NamesLessAllSymbols <- Names(LessAllSymbols)
                                     # (2): could be more robust: check for xts allowable index classes
    LessAllSymbols <- LessAllSymbols[unlist(plyr::llply(LessAllSymbols, function(x) { !is.na(x) }))]
    return(LessAllSymbols)

  }

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
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_c
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
    updated <- DBI::dbGetQuery(con, stringr::str_c(
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
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom plyr llply
#' @importFrom stringr str_c
#' @importFrom DescTools DoCall
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
    SrcSymbols <- DescTools::DoCall(stringr::str_c("listSymbols",".", Dots[["src"]]), c(list(),
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
#' @importFrom tryCatchLog tryCatchLog
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
#' @importFrom tryCatchLog tryCatchLog
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
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom stringr str_c
#' @importFrom DescTools DoCall
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
    Results <- DescTools::DoCall(stringr::str_c("existSymbols",".", Dots[["src"]]), c(list(),
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
#' @importFrom tryCatchLog tryCatchLog
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
#' @importFrom tryCatchLog tryCatchLog
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
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom DescTools DoCall
#' @importFrom DBI dbGetQuery dbQuoteIdentifier
pgSchemaTableLastIndex <- function(con, schname = NULL, tblname) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  if(!is.null(con)) {
    schname <- pgCurrentSearchPath(con)[["CurrentSearchPath"]][1]
  }
  if(is.null(schname)) schname <- "Symbols"

    DBI::dbGetQuery(con,
      stringr::str_c(
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


