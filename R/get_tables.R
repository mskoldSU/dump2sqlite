#' Extracts table names from MySQL dump
#'
#' @param dump A character vector containing lines of MySQL dump file
#' @return A character vector of table names
get_tables <- function(dump){
  dump[stringr::str_starts(dump, stringr::regex("CREATE TABLE", ignore_case = TRUE))] |>
    stringr::str_split("`") |>
    purrr::map_chr(2)
}
