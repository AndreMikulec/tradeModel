
foo1 <- function(Symbol = NULL) {
initEnv();on.exit({uninitEnv()})
  message(stringr::str_c("Begin - ", Symbol))
}
# foo1(Symbol = "IBM")

foo2 <- function(Symbol = NULL) {
initEnv();on.exit({uninitEnv()})
  tryCatchLog::tryCatchLog({
  message(stringr::str_c("Begin - ", Symbol))
})}
# foo2(Symbol = "IBM")

foo3 <- function(Symbol = NULL) {
initEnv();on.exit({uninitEnv()})
  message("Begin - ")
}
# foo3(Symbol = "IBM")

foo4 <- function(Symbol = NULL) {
initEnv();on.exit({uninitEnv()})
  tryCatchLog::tryCatchLog({
  message(base::paste0("Begin - ", Symbol))
})}
# foo4(Symbol = "IBM")

foo5 <- function(Symbol = NULL) {
initEnv();on.exit({uninitEnv()})
  tryCatchLog::tryCatchLog({
  message(paste0("Begin - ", Symbol))
})}
# foo5(Symbol = "IBM")


