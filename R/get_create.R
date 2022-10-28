#' Extracts CREATE statement from MySQL dump
#'
#' @param dump A character vector containing lines of MySQL dump file
#' @param table Name of table to be created
#' @return An SQL CREATE query
get_create <- function(dump, table){
  start <- which(stringr::str_starts(dump, stringr::regex(paste0("CREATE TABLE `", table, "`"), ignore_case = TRUE)))
  entries <- which(stringr::str_starts(dump, "`"))
  entries <- entries[entries > start]
  sequence <- (start + 1):(length(entries) + start)
  entry_code <- dump[entries[which(entries == sequence)]] |>
    stringr::str_split(" ") |>
    purrr::map_chr(~paste(.x[1], .x[2]))
  paste0(paste0("CREATE TABLE `", table, "` (", paste(entry_code, collapse = ","), ")")) |>
    stringr::str_replace_all(",,", ",") |>
    stringr::str_replace_all(",\\)", "\\)")
}
