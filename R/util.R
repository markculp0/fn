#' @importFrom dplyr arrange
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 scale_x_date
#' @importFrom ggplot2 theme
#' @importFrom lubridate as_date
#' @importFrom lubridate hour
#' @importFrom lubridate ymd_hms
#' @importFrom readr read_csv
#' @importFrom readr read_delim
#' @importFrom readr write_csv
#' @importFrom readr col_character
#' @importFrom readxl read_xlsx
#' @importFrom writexl write_xlsx

#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'
#   Load All                   'Ctrl + Shift + L'
#   Document                 devtools::document()
#   Use data       usethis::use_data(my_pkg_data)
#   Build Binary Pkg     Build > Build Binary Pkg

#' @export
col2txt <- function(df, cnum) {
  f <- file("text.txt", "w")
  for (i in 1:dim(df)[1]) {
    writeLines(as.character(df[i, cnum]), f)
  }
  close(f)
}

# Util function to add date time columns
dttm_col <- function(df, dfScol) {

  # Create datetime column
  df$dttm <- lubridate::ymd_hms(dfScol)

  # Create date column
  df$dt <- lubridate::as_date(df$dttm)

  # Create hour column
  df$hr <- lubridate::hour(df$dttm)

  df
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
evt_id_join <- function(df) {
  # Add data
  event_ids <- fn::event_ids

  # Change EventId column name to EventID
  nms <- names(df)
  nms <- gsub("EventId", "EventID", nms)
  names(df) <- nms

  # Join event_ids db
  df2 <- dplyr::left_join(df, event_ids, by = c("EventID" = "EventID"), copy = TRUE)
  df2
}

#' @export
gplot_by_day <- function(df, dfScol) {
  rg <- rg_title(dfScol)
  df <- dttm_col(df, dfScol)
  ggplot2::ggplot(df, ggplot2::aes(x=dt)) +
    ggplot2::scale_x_date(date_labels = "%a,%m/%d", date_breaks = "day") +
    ggplot2::theme(axis.text.x=ggplot2::element_text(angle=60, hjust=1)) +
    ggtitle(rg) +
    ggplot2::geom_bar()
}

#' @export
gplot_by_hour <- function(df, dfScol, dt) {
  # Add date & hour columns
  df <- dttm_col(df, dfScol)

  # Bar chart hourly counts
  ggplot2::ggplot(df, ggplot2::aes(x=hr)) +
    ggplot2::ggtitle(dt) +
    ggplot2::geom_bar()
}

#' @export
rautoruns <- function(f){

  # Read in autoruns.csv
  df <- readr::read_delim(f, delim = '\t', col_types =  list(
    .default = readr::col_character()
  ))

  # Remove category headers
  hitL <- is.na(df$Entry)
  df <- df[!hitL,]

  # Run registry key
  hitL <- grepl("Run", df$`Entry Location`)
  run_key <<- df[hitL,]
  View(run_key)

  # Blank timestamps
  hitL <- is.na(df$Time)
  blank_dttm <<- df[hitL,]

  # Not verified
  hitL <- grepl("^\\(Verified", df$Signer)
  not_verify <<- df[!hitL,]
  verify <- df[hitL,]
  View(not_verify)

  # Signers
  signer <- dplyr::select(verify, Signer)
  signer <- dplyr::group_by(signer, Signer)
  signer <- dplyr::summarise(signer, cnt=dplyr::n())
  signer <<- dplyr::arrange(signer, desc(cnt))

}

#' @export
rcports <- function(f){

  cports_header <- fn::cports_header
  df <- rcsvn(f)
  names(df) <- c(cports_header)

  # Established
  established <<- dplyr::filter(df, State == "Established")
  View(established)

  # Remote connections
  remote <- dplyr::select(df, RemoteHostName, RemoteAddress, RemotePort, State, Company, ProcessName, ProcessPath)
  hitL <- is.na(remote$RemoteAddress); remote <- remote[!hitL,]; rm(hitL)
  hitL <- remote$RemoteAddress %in% c("0.0.0.0", "::", "127.0.0.1")
  remote <<- remote[!hitL,]

  # All
  cports <<- df

}

#' @export
rcsv <- function(f){
  df <- readr::read_csv(f, col_types = list(
    .default = readr::col_character()
  ))
}

#' @export
rcsvn <- function(f){
  df <- readr::read_csv(f, col_names = FALSE,
    col_types = list(
    .default = readr::col_character()
  ))
}

#' @export
rtsv <- function(f){
  df <- readr::read_delim(f, delim = '\t', col_types =  list(
    .default = readr::col_character()
  ))
  df
}

# Create date range string
rg_title <- function(dfScol){
  rg <- paste(range(dfScol)[1], range(dfScol)[2], sep = " to ")
  rg
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

#' @export
wheq <- function(df, cnam, value){
  df2 <- dplyr::filter(df, .data[[cnam]] == value)
  View(df2)
  df2
}

#' @export
whgt <- function(df, cnam, value){
  df2 <- dplyr::filter(df, .data[[cnam]] > value)
  View(df2)
  df2
}

#' @export
whlt <- function(df, cnam, value){
  df2 <- dplyr::filter(df, .data[[cnam]] < value)
  View(df2)
  df2
}
