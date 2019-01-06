



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
#' con <- DBI::dbConnect(RPostgreSQL::PostgreSQL(), user = "postgres")
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

  # if the schema.table does not exist then add it.
  pgAddTable(con, trgt = trgt, keys = keys, schname = schname, df = df)
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
#' @param df data.frame (with column names)
#' @param con DBI database connection
#' @param Symbol new table name. E.g Could be a company TICKER or a FRED column.
#' @param schname schema name
#' @param keys trgt remote server side vector of strings of table
#' column names that make up a unique id for the row.
#' keys can not be zero length. keys can not be null.
#' @export
pgAddTable <- function(con, trgt = NULL, keys = c("rn"), schname, df = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

  if(!DBI::dbExistsTable(con, c(schname, trgt))) {
    dfToCREATETable(df, con, Symbol = trgt, schname = schname, keys = keys, SymbolsTable = FALSE)
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
#'  con <- DBI::dbConnect(RPostgreSQL::PostgreSQL(), user = "postgres")
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
#' @param oldData previously collected limited server DB data used to
#' restrict what is updated/inserted.  If not provide by the user then
#' the oldData will be (re)generated from df.
#' Here oldData is used to CHECK that the key columns that I
#' what to insert DO NOT EXISTS on the Server DB
#' Here, only the keys are checked for 'no duplicates'
#' @examples
#' \dontrun{
#'
#' # setup
#' SuBmtcars <- mtcars[c(1,5),1:2]
#' oldData <- data.table::data.table(SuBmtcars, keep.rownames=TRUE, key="rn")
#' oldData[1,2] <- NA; oldData[2,3] <- NA
#'
#' con <- DBI::dbConnect(RPostgreSQL::PostgreSQL(), user = "postgres")
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
#' @importFrom DBI dbWriteTable
#' @export
pgInsert <- function(con, trgt = NULL, keys = c("rn"), schname, df = NULL, varHint = NULL, valHint = NULL) {
tryCatchLog::tryCatchLog({
initEnv();on.exit({uninitEnv()})

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

  # Server DB column names
  serverDBColumns <-  pgListSchemaTableColumns(con, schname = schname, tblname = trgt)

  # IntentionFor == "INSERT" (best to limit data by varHint and valHint)
  # IntentionFor == "INSERT" (SELECT keys FROM )
    # limit local key columns to only just Server DB existing columns
  oldKeyData <- pgOldData(con, trgt = trgt, keys = keys, schname = schname, df = df, varHint = varHint, valHint = valHint, IntentionFor = "INSERT")
  # garantee order of the Server DB columns
  oldKeyData <- oldKeyData[, customSorting(colnames(oldKeyData), serverDBColumns, sortVectorExcess = FALSE), with=FALSE]

  newData <- data.table::data.table(df[, , drop = FALSE], key=keys)
  # fill in missing columns on the R client side that exist on the Server DB
  SchemaTableColumnTypes <- pgSchemaTableColumnTypes(con, schname = schname, tblname = trgt)
  SchemaTableColumnTypesSplitted <- split(SchemaTableColumnTypes, f = seq_len(NROW(SchemaTableColumnTypes)))

  # Maybe dbWriteTable does not need these
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
    }
  }

  # Reorder the newData columns to match the order on the Server DB
  newData <- newData[, customSorting(colnames(newData), serverDBColumns, sortVectorExcess = FALSE), with=FALSE]

  # just select key columns
  newKeyData <- newData[, keys, with=FALSE]
  # correct, newKeyData is upper table data, see below, the clever programming: seq_len(NROW
  newKeyoldData <- rbindlist(list(newKeyData, oldKeyData))
  # choose rows with never duplicates
  NewDataIndex <- !duplicated(newKeyoldData) & !duplicated(newKeyoldData, fromLast = TRUE)

  # append up to the Server DB
  DBI::dbWriteTable(con, c(schname, trgt),newData[NewDataIndex[seq_len(NROW(newKeyData))], ], row.names = FALSE, overwrite = FALSE, append = TRUE)
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
#' keys can not be zero length. keys can not be null.
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
#' # setup
#' SuBmtcars <- mtcars[c(1,5),1:2]
#' oldData <- data.table::data.table(SuBmtcars, keep.rownames=TRUE, key="rn")
#' oldData[1,2] <- NA; oldData[2,3] <- NA
#'
#' con <- DBI::dbConnect(RPostgreSQL::PostgreSQL(), user = "postgres")
#' DBI::dbExecute(con, "DROP TABLE IF EXISTS public.mtcars")
#' DBI::dbWriteTable(con, "mtcars", oldData, row.names = FALSE)
#'
#' newData <- data.table::data.table(SuBmtcars, keep.rownames=TRUE, key="rn")
#' newData[2,2] <- NA; newData[1,3] <- NA
#'
#' oldData <- pgOldData(con, trgt = "mtcars", keys = c("rn"), schname = "public", df = newData)
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

  if(is.null(trgt)) stop("pgOldData trgt can not be null")
  if(is.null(keys)) stop("pgOldData keys can not be null")
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
      keyvals <-  unlist(spl)
      if(length(keyvals)) {
        keyvals <- if(!is.numeric(keyvals)) DBI::dbQuoteString(con, keyvals)
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
  # combine subsetting
  if(length(SelectWhereExactlies)) {
    SelectWhereExactlies <- stringr::str_c(" WHERE " , stringr::str_c(SelectWhereExactlies, collapse = " AND "), collapse = "")
  }

  if(schname != "") { dotSchemaQuoted <- stringr::str_c(DBI::dbQuoteIdentifier(con, schname), ".") } else { dotSchemaQuoted <- "" }
  schemaTrgtTableQuoted <-  stringr::str_c(dotSchemaQuoted, DBI::dbQuoteIdentifier(con, trgt))

  # actually go to the Server DB then collect and bring down that server data to local R
  #  do not bring back too many columns
  if(IntentionFor == "UPDATE") {
    ColnamesAndCommas <- stringr::str_c(DBI::dbQuoteIdentifier(con, colnames(newData)), collapse = ", ")
  }
  if(IntentionFor == "INSERT") {
    if(length(keys)){
      ColnamesAndCommas <- stringr::str_c(DBI::dbQuoteIdentifier(con, keys), collapse = ", ")
    } else {
      ColnamesAndCommas <- "'t'"
    }
  }

  SQLSelection <- stringr::str_c("SELECT ", ColnamesAndCommas, " FROM ", schemaTrgtTableQuoted, SelectWhereExactlies)
  oldServerData <- DBI::dbGetQuery(con, SQLSelection)
  message(SQLSelection)
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
#' # setup
#' SuBmtcars <- mtcars[c(1,5),1:2]
#' oldData <- data.table::data.table(SuBmtcars, keep.rownames=TRUE, key="rn")
#' oldData[1,2] <- NA; oldData[2,3] <- NA
#'
#' con <- DBI::dbConnect(RPostgreSQL::PostgreSQL(), user = "postgres")
#' DBI::dbExecute(con, "DROP TABLE IF EXISTS public.mtcars")
#' DBI::dbWriteTable(con, "mtcars", oldData, row.names = FALSE)
#'
#' newData <- data.table::data.table(SuBmtcars, keep.rownames=TRUE, key="rn")
#' newData[2,2] <- NA; newData[1,3] <- NA
#'
#' # not "prepare.query"
#'
#' # this
#' pgUpdate(con, trgt = "mtcars", keys = c("rn"), schname = "public", df = newData)
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
          (any(AppendConditions %in% "Always"))
      ) {
        UpDateSet   <- stringr::str_c(DBI::dbQuoteIdentifier(con, nm), " = ", "val.", DBI::dbQuoteIdentifier(con, nm), " ")
        UpDateSetColl <- c(UpDateSetColl,UpDateSet)
      }
    }
    if(length(UpDateSetColl)) {
      keyvals <- unlist(plyr::llply(keys, function(x) { spl[[x]] }))
      names(keyvals) <- keys # names not used
      keyvals <- if(!is.numeric(keyvals)) DBI::dbQuoteString(con, keyvals)
      UpDateWhereExactly <- stringr::str_c(stringr::str_c("trg.", DBI::dbQuoteIdentifier(con, keys)), " = ", keyvals, collapse = " AND ")
      UpdateFromWhere <- stringr::str_c(" FROM ", UpDateFrom, " WHERE ", UpDateWhere, " AND ", UpDateWhereExactly)
      UpDateSets <- stringr::str_c(UpDateSetColl, collapse = ", ")
      UpDateStmt <- stringr::str_c(UpDateTarget, UpDateSets, UpdateFromWhere, "; ")
      UpDateStmts <- c(UpDateStmts, UpDateStmt)
    }
  }
  if(length(UpDateStmts)) {

    colClasses <- pgDFColClasses(newData[, , drop = FALSE])

    # would prefer less disk I/O so I would prefer to create a TEMPORARY table
    # R package Postgre can do that in dbWriteTable
    # but authose chose to have each statement prepared (so update statements may be slow )
    if(prepare.query) {
      if(inherits(con, "PostgreSQLConnection")) {
        con2 <- try( {DBI::dbConnect(RPostgres::Postgres(),
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
    message(stringr::str_c(stringr::str_split(stringr::str_c(UpDateStmts, collapse = ""), ";\\s+")[[1]], ";\n"))
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



#' custom sort a vector
#'
#' excess Vector elements are appended to the end ( sort or not sort, CI sort or CS sort )
#' other elements found in InitOrder that are 'not found in Vector' are ignored
#'
#' @param Vector vector to be sorted
#' @param InitOrder starting custom sorting ( without the excess )
#' @param CI FALSE(default) whether or not Vector excess columns that are
#' not found in InitOrder  are sorted 'not case insensitive'(TRUE) or
#' ncase sensitive'(FALSE)
#' @param sortVectorExcess TRUE(default) weather or not Vector excess columns
#' are attempted to be sorted (TRUE) or not attempted to be sorted (FALSE)
#' @return vector Vector sorted by InitOrder
#' @references
#' \cite{Custom Sorting in R \url{https://stackoverflow.com/questions/23995285/custom-sorting-in-r}}
#' @references
#' \cite{Case insensitive sort of vector of string in R \url{https://stackoverflow.com/questions/29890303/case-insensitive-sort-of-vector-of-string-in-r}}
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
#' customSorting(c("G", "D","B","C", "F"), c("E","B","C","D","A"), sortVectorExcess = FALSE)
#' [1] "B" "C" "D" "G" "F"
#'
#' # other(InitOrder) ignored "F"
#' customSorting(c("E","B","C","D","A"), c("F", "D","B","C"), sortVectorExcess = FALSE)
#' [1] "D" "B" "C" "E" "A"
#'
#' }
#' @export
customSorting <- function(Vector, InitOrder, CI = FALSE, sortVectorExcess = TRUE) {

  # custom sorting
  VectorLevels <- InitOrder
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
  Vector

}



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
#' getSymbols("SENTIMENT", src = "AAII")
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
#'
#' #' getSymbols("AAIIsentiment", src = "AAII")
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
        !is.na(file.info(tmp)$mtime) &&
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
        !is.na(file.info(tmppage)$mtime) &&
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
    rtc <- rtc[,customSorting(Vector = colnames(rtc), InitOrder = colnames(rs)), drop = FALSE]
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
          fri <- fr[, Symbols[[i]]]
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
    stop(stringr::str_c("In saveSymbols, duplicate case-insenstive naames found in ", paste0(SamexTsGetSymbols, collapse = ", ")))
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
#' @param placeNewRecords "TruncateTable" (default).
#' TRUNCATE TABLE the table contents ("TruncateTable").
#' Append 'new' records determined by 'new' inbound key values
#' to the table via pgInsert ("AddOnlyNew").
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

  if(is.null(placeNewRecords)) placeNewRecords <- "TruncateTable"

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
    # IMPORTANT!!
    # NOTE, IF THE STRUCTURE of THE incoming OBJECT is different from the STORED object
    # E.G.! NEW COLUMNS, DIFFERENT R/POSTGRESQL CLASS OF the index
    # then MAYBE
    # dbWriteTable WILL FAIL? and/or
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
      dfToCREATETable(df = df, con = con, Symbol = each.symbol, schname = schname, keys = keys)
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

    if(placeNewRecords == "TruncateTable") {
      DBI::dbExecute(con, stringr::str_c("TRUNCATE TABLE ", dotSchemaQuoted, DBI::dbQuoteIdentifier(con, each.symbol), ";"))

      DBI::dbWriteTable(con, c(schname ,each.symbol), df, append = T, row.names = F)
      # if  each.symbol includes schema name + ".",
      # then dbWriteTable will return TRUE, but it will LIE

      # pgAdmin3 LTS (BigSQL) mentioned that I should do
      DBI::dbExecute(con, stringr::str_c("VACUUM (VERBOSE, ANALYZE) ", dotSchemaQuoted, DBI::dbQuoteIdentifier(con, each.symbol), ";"))
    }
    if(placeNewRecords == "AddOnlyNew") {

      # see ?PrimaryKeyCols on how NULL 'keys' is interpreted
      PrimaryKeyCols <- dfGetPKEYNames(df = df, keys = keys)

      # Note, this 'append' heavily assumes that the
      # columns names are the same
      # and may be
      # column names ORDER are the same (UNVERIFIED)
      #   see RPostgreSQL::dbWriteTable C code, caroline::dbWriteTable2
      # and
      # column types are the samme
      # I have not (yet) handled that cases
      # in such that any of the above "assumptions" differ
      #
      # NOTE: pgInsert calls DBI::dbWriteTable that appends data
      #
      pgInsert(con, trgt = each.symbol, keys = PrimaryKeyCols, schname = schname, df = df, varHint = varHint, valHint = valHint)
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


