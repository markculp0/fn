#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr arrange
#' @importFrom readr read_csv
#' @importFrom readr write_csv
#' @importFrom readr col_character
#' @importFrom readxl read_xlsx
#' @importFrom writexl write_xlsx

#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#' @export
col2txt <- function(df, cnum) {
  f <- file("text.txt", "w")
  for (i in 1:dim(df)[1]) {
    writeLines(as.character(df[i, cnum]), f)
  }
  close(f)
}

#' @export
desc_cnt <- function(df, c){

  df2 <- dplyr::select(df, .data[[c]])
  df2 <- dplyr::group_by(df2, .data[[c]])
  df2 <- dplyr::summarise(df2, cnt=dplyr::n())
  df2 <- dplyr::arrange(df2, desc(cnt))
  View(df2)

  df2
}

#' @export
desc_cnt2 <- function(df, c1, c2) {
  df2 <- dplyr::select(df, .data[[c1]], .data[[c2]])
  df2 <- dplyr::group_by(df2, .data[[c1]], .data[[c2]])
  df2 <- dplyr::summarise(df2, cnt=dplyr::n())
  df2 <- dplyr::arrange(df2, desc(cnt))
  View(df2)

  df2
}

#' @export
dt_range <- function(df, coldate, colname){
  df2 <- dplyr::select(df, .data[[coldate]], .data[[colname]])
  df2 <- dplyr::group_by(df2, .data[[colname]])
  date_range <- dplyr::summarise(df2, minDate=min(.data[[coldate]]), maxDate=max(.data[[coldate]]))
  date_range <- dplyr::arrange(date_range, desc(maxDate))

  date_range
}

#' @export
dt_range2 <- function(df, coldate, colname, colname2){
  df2 <- dplyr::select(df, .data[[coldate]], .data[[colname]], .data[[colname2]])
  df2 <- dplyr::group_by(df2, .data[[colname]], .data[[colname2]])
  date_range2 <- dplyr::summarise(df2, minDate=min(.data[[coldate]]), maxDate=max(.data[[coldate]]))

  date_range2
}

#' @export
rcsv <- function(f){
  df <- readr::read_csv(f, col_types = list(
    .default = readr::col_character()
  ))
}

#' @export
rxl <- function(f){
  df <- readxl::read_xlsx(f)

  df
}

#' @export
wxl <- function(f, fname) {
  writexl::write_xlsx(f, fname)
}

#' @export
wcsv <- function(df, nm) {
  readr::write_csv(df, nm)
}



