#' Converts MySQL dump to SQLite file
#'
#' @param in_file MySQL dump file path
#' @param out_file SQLite file path
#' @param ... Additional parameters for reading dump (e.g. encoding)
#' @export
dump2sqlite_old <- function(in_file, out_file = paste0(in_file, "ite"), ...){
  dump <- readLines(in_file, ...) |>
    trimws()
  tables <- get_tables(dump)
  create_statements <- purrr::map_chr(tables, ~get_create(dump, .x))
  insert_statements <- dump[stringr::str_starts(dump, stringr::regex("INSERT", ignore_case = TRUE))] |>
    stringr::str_remove_all("\\\\'")
  con <- RSQLite::dbConnect(RSQLite::SQLite(), out_file)
  purrr::walk(create_statements, ~RSQLite::dbExecute(con, .x))
  purrr::walk(insert_statements, ~RSQLite::dbExecute(con, .x))
  RSQLite::dbDisconnect(con)
}
#' Converts MySQL dump to SQLite file
#'
#' @param in_file MySQL dump file path
#' @param out_file SQLite file path
#' @param ... Additional parameters for reading dump (e.g. encoding)
#' @export
dump2sqlite <- function(in_file, out_file = paste0(in_file, "ite"), ...){
  dump <- readLines(in_file, ...) |>
    trimws()
  tables <- get_tables(dump)
  create_statements <- purrr::map_chr(tables, ~get_create(dump, .x))
  insert_statements <- get_inserts(dump)
  con <- RSQLite::dbConnect(RSQLite::SQLite(), out_file)
  purrr::walk(create_statements, ~RSQLite::dbExecute(con, .x))
  purrr::walk(insert_statements, ~RSQLite::dbExecute(con, .x))
  RSQLite::dbDisconnect(con)
}
