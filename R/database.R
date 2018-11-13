



#' saves xts object symbols to a persistent location (dispatcher)
#'
#' @param Symbols	a character vector specifying the names of each symbol to be loaded
#' @param env	where to create(save) objects. Setting env=NULL is equal to auto.assign=FALSE
#' @param source.envir source location of Symbols
#' @param ... pass through parameters
#' @export
#' @examples
#' \dontrun{
#'
#' saveSymbols("IBM", trg = "pg", schname = "money",
#'   source.envir = list2env(list(IBM = getSymbols("IBM", auto.assign = F))))
#'
#' }
saveSymbols <- function(Symbols = NULL, env = parent.frame(), source.envir = NULL, ...) {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})

  browser()
  xTsGetSymbols <- list()
  # traditional quantmod env # this does NOT work with tryCatchLog::tryCatchLog
  # Th .GlobalEnv xTsGetSymbols are not being recored in .getSymbols
  if (exists(".getSymbols", env, inherits = FALSE)) {
      DotgetSymbols <- get(".getSymbols", env, inherits = FALSE)
      if (is.null(Symbols)) {
          Symbols <- names(DotgetSymbols)
      }
      else {
          Symbols <- Symbols[Symbols %in% names(DotgetSymbols)]
      }
      for (each.symbol in Symbols) {
          if(!each.symbol %in% names(xTsGetSymbols )){
            xTsGetSymbols <- c(xTsGetSymbols, get(each.symbol, envir = env, inherits = FALSE))
            names(xTsGetSymbols)[length(xTsGetSymbols)] < each.symbol
          }
      }
  }

   runenv <- environment()


  # look in my custom environment ONLY

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
   })
   }

   # save everything in my custom environment
   if(!is.environment(source.envir))
     source.envir <- list2env(xTsGetSymbols, parent = emptyenv())

  Dots <- list(...)
  if("file.path" %in% names(Dots))
    do.call(saveSymbols.RData, c(list(), Symbols = Symbols, env = env, source.envir = source.envir, Dots))

  if("trg" %in% names(Dots)) {
     do.call(paste0("saveSymbols",".", Dots[["trg"]]), c(list(),Symbols = Symbols, env = env, source.envir = source.envir, Dots))
  }


  invisible()

})}



#' saves xts object symbols to a persistent location (disk: Symbol.RData file)
#'
#' see quantmod SaveSymbols
#'
#' @param Symbols	a character vector specifying the names of each symbol to be loaded
#' @param file.path  character string of file (Symbol.Rdata) location
#' @param env	where to create(save) objects. Setting env=NULL is equal to auto.assign=FALSE
#' @param source.envir source location of Symbols
#' @param ... pass through parameters
#' @export
saveSymbols.RData <- function (Symbols = NULL, file.path = stop("must specify 'file.path'"),
  env = parent.frame(), source.envir = NULL, ...) {
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
#' @export
xTs2DBDF <- function(xTs, con) {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})

  df <- cbind(date = index(xTs), as.data.frame(xTs, stringsAsFactors = FALSE))

  # OHLC columns
  str_replace_all(colnames(df), "[.]", "_")  -> colnames(df)
  colnames(df)[str_detect(tolower(colnames(df)), "_open$")] <- "o"
  colnames(df)[str_detect(tolower(colnames(df)), "_high$")] <- "h"
  colnames(df)[str_detect(tolower(colnames(df)), "_low$")] <- "l"
  colnames(df)[str_detect(tolower(colnames(df)), "_close$")] <- "c"
  colnames(df)[str_detect(tolower(colnames(df)), "_volume$")] <- "v"
  colnames(df)[str_detect(tolower(colnames(df)), "_adjusted$")] <- "a"
  # all other column names "as is" (e.g. FRED)

  # colnames(df) <- dbQuoteIdentifier(con, colnames(df))
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

  if(schname != "") { dotSchemaQuoted <- paste0(dbQuoteIdentifier(con, schname), ".") } else { dotSchemaQuoted <- "" }
  schemaSymbolsQuoted <-  paste0(dotSchemaQuoted, dbQuoteIdentifier(con, Symbol))

    ddl <- paste0("CREATE TABLE ", schemaSymbolsQuoted ,"(", paste0( dbQuoteIdentifier(con, names(colClasses)), " ", colClasses, collapse = ", "), ");")
  # ddl <- paste0("CREATE TABLE ", schname ,".", Symbol ,"(", paste0( names(colClasses), " ", colClasses, collapse = ", "), ");")
  # ddl <- paste0("CREATE TABLE ", schemaSymbolsQuoted ," (", paste0( names(colClasses), " ", colClasses, collapse = ", "), ");")

  ddl

})}



#' saves xts object symbols to a persistent location (database: PostgreSQL)
#'
#' @param Symbols	a character vector specifying the names of each symbol to be loaded
#' @param env	where to create(save) objects. Setting env=NULL is equal to auto.assign=FALSE
#' @param field.names names existing in starting columns
#' @param db.fields character vector indicating
#' names of fields to insert
#' @param user username to access database
#' @param password password to access database
#' @param dbname database name
#' @param schname schema name
#' @param host database host
#' @param port database port
#' @param options pass extra parameters in aa string to the command line
#' @param forceISOdate FALSE/TRUE if the communication of date (time stamp) from PostgreSQL
#' is forced to ISO style at conection
#' @param ... pass through parameters
#' @export
saveSymbols.PostgreSQL <- function(Symbols = NULL, env = NULL,
  field.names = c('Open','High','Low','Close','Volume','Adjusted'),
  db.fields=c('o','h','l','c','v','a'),
  source.envir = NULL,
  user=NULL,password=NULL,dbname=NULL,schname=NULL,host='localhost',port=5432,options=NULL, forceISOdate = TRUE,
  ...)  {
  tryCatchLog::tryCatchLog({
  initEnv();on.exit({uninitEnv()})
  this.env <- environment()

   # HOPEFULLY no one messes with Date/date
   # HOPEFULLY BELOW data/Date meaningless ( only referenced by position(number) )
   field.names <- c('Date', field.names)
   db.fields   <- c('date', db.fields)

  for(var in names(list(...))) {
    # import all named elements that are NON formals
    assign(var, list(...)[[var]], this.env)
  }
  if(!hasArg("verbose")) verbose <- FALSE

  if(!requireNamespace("DBI", quietly=TRUE))
    stop("package:",dQuote("DBI"),"cannot be loaded.")
  if(!requireNamespace("RPostgreSQL", quietly=TRUE))
    stop("package:",dQuote("RPostgreSQL"),"cannot be loaded.")

  if(is.null(user))     user     <- "postgres"
  if(is.null(password)) password <- "postgres"
  if(is.null(dbname))   dbname   <- "postgres"
  if(is.null(schname))  schname  <- ""
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

  db.Symbols <- DBI::dbListTables(con)
  if(length(Symbols) != sum(Symbols %in% db.Symbols)) {
    missing.db.symbol <- Symbols[!Symbols %in% db.Symbols]
    # for the requested Symbol, I do not have adatabae table so, I have to create ONE

    dfs <- list()
    new.db.symbol <- c()
    for (each.symbol in  missing.db.symbol) {

      xTs <- as.list(source.envir)[[each.symbol]]
      if(is.null(xTs)) { message(paste("Symbol ", each.symbol, " was not found s skipping.")); next }
      df  <- xTs2DBDF(xTs, con)

      # create NEW CREATE TABLE statements"
      # column names
      ddl <- DBDF2CREATETableStmt(df, con, each.symbol, schname)
      dbExecute(con, ddl)

      new.db.symbol <- c(new.db.symbol, each.symbol)
      L <- list(); L[[each.symbol]] <- df
      dfs <- c(dfs, L)
    }

    missing.db.symbol <- setdiff(missing.db.symbol, new.db.symbol)
    if(length(missing.db.symbol))
      warning(paste('can not save symbol(s): ',paste(missing.db.symbol,collapse=', ')))
    db.Symbols <- unique(c(db.Symbols, new.db.symbol))

  }
  Symbols <- Symbols[Symbols %in% db.Symbols]

  for (each.symbol in Symbols) {

    # if(each.symbol %in% names(dfs)) {
    #   df <- dfs[[each.symbol]]
    # } else {
    #   xTs <- getSymbols(each.symbol, source.envir = Symbols.envir, auto.assign = F)
    #   df  <- xTs2DBDF(xTs)
    # }

    xTs <- as.list(source.envir)[[each.symbol]]
    if(is.null(xTs)) { message(paste("Symbol ", each.symbol, " was not found s skipping.")); next }
    df  <- xTs2DBDF(xTs)

    if(schname != "") { dotSchemaQuoted <- paste0(dbQuoteIdentifier(con, schname), ".") } else { dotSchemaQuoted <- "" }
    dbExecute(con, paste0("TRUNCATE TABLE ", dotSchemaQuoted, dbQuoteIdentifier(con, each.symbol), ";"))

    # Goal
    # exist in  R       'Date', 'Open','High','Low','Close','Volume','Adjusted'( along with FRED columns )
    # translate from those
    # to DB: 'date', 'o','h','l','c','v','a' ( along with FRED columns "as is")

    # custom sorting
    # field.names  <- customSorting(field.names , InitOrder = field.names, CI = TRUE)
    # df <- df[, field.names, drop = FALSE]

    db.fields    <- customSorting(db.fields, InitOrder = db.fields, CI = TRUE)
    colnames(df) <- db.fields

    # upon creation, do Quote Once:  (1)schema, (2)table and (3)column names
    # the storage will be that anything_capilized retains it's "" quotes.

    dbWriteTable(con, each.symbol, df, append = T, row.names = F)
                    # if with schema, will say TRUE, but will lie
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
pgCurrentUser <- function(con) oneColumn(con, "SELECT CURRENT_USER;", "CurrentUser")

#' get PostgreSQL current user schema
#'
#' @rdname pgCurrent
#' @export
pgCurrentSchema <- function(con) oneColumn(con, "SELECT current_schema();", "CurrentSchema")

#' get PostgreSQL current user database name
#'
#' @rdname pgCurrent
#' @export
pgCurrentDB <- function(con) oneColumn(con, "SELECT current_database();", "CurrentDB")


#' get PostgreSQL current user search path
#'
#' @rdname pgCurrent
#' @export
pgCurrentSearchPath <- function(con) oneColumn(con, "SHOW SEARCH_PATH;", "CurrentSearchPath")


#' set PostgreSQL current user search path
#'
#' @rdname pgCurrent
#' @export
pgSetCurrentSearchPath <- function(con, path) dbExecute(con, paste0("SET SEARCH_PATH TO ", path,";"))



#' get PostgreSQL current user temp schema
#'
#' @rdname pgCurrent
#' @export
pgCurrentTempSchema <- function(con) oneColumn(con, "SELECT nspname FROM pg_namespace WHERE oid = pg_my_temp_schema();", "CurrentTempSchema")

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
#'
#' @param Symbols a charcter vector specifying
#' the names of each symbol to be laoded
#' @param env where to create objects (.GlobalEnv)
#' @param return.class desirect class of returned object.
#' Can be xts, zoo, data.fram, or xts. (zoo)
#' @param db.fields character vector indicating
#' names of fields to retrieve
#' @param field.names names assigned to returned columns
#' @param user username to access database
#' @param password password to access database
#' @param dbname database name
#' @param schname schema name
#' @param host database host
#' @param port database port
#' @param options pass extra parameters in a string to the command line
#' @param forceISOdate FALSE/TRUE if the communication of date (time stamp) from PostgreSQL
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
#'
#'
#' ## Method #2
#' setDefaults(getSymbols,src='PostgreSQL')
#'   # OR
#' setSymbolLookup(MSFT='PostgreSQL')
#'
#' getSymbols('MSFT')
#'
#' #########################################
#' ##  NOT RECOMMENDED!!!
#' #########################################
#' ## Method #3
#' getSymbols.PostgreSQL('MSFT',env=globalenv())
#'
#' ## YEAR 2018 EXAMPLE
#'
#' ibm <- getSymbols.PostgreSQL("IBM", schname ="money", auto.assign =  F)
#'
#' }
#' @export
getSymbols.PostgreSQL <- function(Symbols,env,return.class='xts',
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

     if(!requireNamespace("DBI", quietly=TRUE))
       stop("package:",dQuote("DBI"),"cannot be loaded.")
     if(!requireNamespace("RPostgreSQL", quietly=TRUE))
       stop("package:",dQuote("RPostgreSQL"),"cannot be loaded.")

        if(is.null(user))     user     <- "postgres"
        if(is.null(password)) password <- "postgres"
        if(is.null(dbname))   dbname   <- "postgres"
        if(is.null(schname))  schname  <- ""
        if(is.null(user) || is.null(password) || is.null(dbname)) {
          stop(paste(
              'At least one connection argument (',sQuote('user'),
              sQuote('password'),sQuote('dbname'),
              ") is not set"))
        }
        con <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),user=user,password=password,dbname=dbname,
                                           host=host,port=port,options=options,forceISOdate=forceISOdate)
        db.Symbols <- DBI::dbListTables(con)
        if(length(Symbols) != sum(Symbols %in% db.Symbols)) {
          missing.db.symbol <- Symbols[!Symbols %in% db.Symbols]
                warning(paste('could not load symbol(s): ',paste(missing.db.symbol,collapse=', ')))
                Symbols <- Symbols[Symbols %in% db.Symbols]
        }

        if(schname != "") { dotSchemaQuoted <- paste0(dbQuoteIdentifier(con, schname), ".") } else { dotSchemaQuoted <- "" }

        schemaSymbolsQuoted <- list()
        Symbols.db.Cols <- list()
        for(i in seq_along(Symbols)) {
            if(verbose) {
                cat(paste('Loading ',Symbols[[i]],paste(rep('.',10-nchar(Symbols[[i]])),collapse=''),sep=''))
            }

            # PEEK AHEAD, SEE IF, VOLUME xor ADJUSTED, IS MISSING, SEE IF SINGLE COLUMN (FRED) DATA
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
                    table_name       IN (", dbQuoteLiteral(con, Symbols[[i]]),")
                ;
              "
              )
            ) -> Symbols.db.Cols[[i]]

            # Goal
            # exist in the DB: 'date', 'o','h','l','c','v','a' ( along with FRED columns )
            # translate from those
            # to R             'Date', 'Open','High','Low','Close','Volume','Adjusted'( along with FRED columns "as is")

            # Symbols.db.Columns <-  do.call(c,Symbols.db.Cols[[i]][,"column_name", drop = F])
            # Symbols.db.ColumnsColumnOne <- Symbols.db.Columns[1] # date # NOT USED
            #
            # MatchedPositions <- match( db.fields[-1], do.call(c,llply(strsplit(tolower(Symbols.db.Columns[-1]),""), function(x) x[1] ))  )
            # Symbols.db.Columns <- Symbols.db.Columns[-1][MatchedPositions]
            # Symbols.db.Columns <- c(Symbols.db.ColumnsColumnOne, Symbols.db.Columns)
            # # (can be character())
            #
            # field.namesColumnOne <- field.names[1]
            # MatchedPositions <- match( db.fields[-1], do.call(c,llply(strsplit(tolower(field.names[-1]),""), function(x) x[1] ))  )
            # # (can be character())
            # field.names <- field.names[-1][MatchedPositions]
            # field.names <- c(field.namesColumnOne, field.names)

            # custom sorting
            db.fields    <- customSorting(db.fields, InitOrder = c("date", "o", "h", "l", "c", "v", "a"), CI = TRUE)
            field.names  <- customSorting(db.fields, InitOrder = c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted"), CI = TRUE)

            db.fieldsQuoted  <-  dbQuoteIdentifier(con, db.fields)
            schemaSymbolsQuoted[[i]] <-  paste0(dotSchemaQuoted, dbQuoteIdentifier(con, Symbols[[i]]))

            if(length(db.fields) > 1) {
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
            #fr <- data.frame(fr[,-1],row.names=fr[,1])
            fr <- xts(as.matrix(fr[,-1]),
                      order.by=as.Date(fr[,1],origin='1970-01-01'),
                      src=dbname,updated=Sys.time())
            # MAY NOT HAVE VOLUME/ADJUSTED
            if(OHLCData) {
              colnames(fr) <- paste(Symbols[[i]],field.names[-1], sep='.')
            } else { # as-is # e.g. FRED
              colnames(fr) <- Symbols.db.Columns[-1]
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
#' "getSymbols pg
#'
#'@export
getSymbols.pg <- getSymbols.PostgreSQL

