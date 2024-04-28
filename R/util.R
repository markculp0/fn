#' @importFrom dplyr arrange
#' @importFrom dplyr bind_cols
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
#' @importFrom lubridate ymd_hm
#' @importFrom lubridate ymd_hms
#' @importFrom stringr str_extract
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_replace
#' @importFrom stringr str_split
#' @importFrom readr read_csv
#' @importFrom readr read_delim
#' @importFrom readr write_csv
#' @importFrom readr col_character
#' @importFrom readxl read_xlsx
#' @importFrom tibble as_tibble
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
doc2txt <- function(doc){
  # Temp directory
  dir.create("_tmp")

  # Get docname and docpath
  docname <- gsub(".docx","", doc)
  docpath <- stringr::str_extract(docname, "^.+/")
  docname <- gsub("^.+/","",docname)

  # Unzip docx, extract text
  unzip(doc, exdir = "_tmp")
  txt <- readLines("_tmp/word/document.xml")

  # Filter text
  l <- stringr::str_extract_all(txt[2], "<w:t.*</w:t")
  txt2 <- l[[1]]

  # Remove text
  txt2 <- gsub("<w:[^>]+>", "", txt2)
  txt2 <- gsub("</w:[^>]+>", "", txt2)
  txt2 <- gsub("</w:t", "", txt2)

  # Write output
  docout <- paste0(docpath, docname, ".txt")
  writeLines(txt2, docout)

  # Clean up
  rm(l, txt, txt2, doc, docname, docpath, docout)
  unlink("_tmp", recursive = T, force = T)

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
dttm_mdy_hm <- function(df, dfSc){
  dttm <- lubridate::mdy_hm(dfSc)
  dttm <- as.character(dttm)
  dttm <- data.frame(dttm)
  df2 <- dplyr::bind_cols(dttm, df)
  df2
}

#' @export
dttm_mdy_hms <- function(df, dfSc){
  dttm <- lubridate::mdy_hms(dfSc)
  dttm <- as.character(dttm)
  dttm <- data.frame(dttm)
  df2 <- dplyr::bind_cols(dttm, df)
  df2
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

# wevtutil functions ------------------------------------------------------

# Create dataframe from logfile entries
# a=log, h=map, col1=1st column name
wevt_add_columns <- function(a, h, col1){
  # Subset a
  a2 <- a[h]

  # Remove tabs, spaces
  a2 <- gsub("[\t ,]", "", a2)

  # Count sets of log entries & columns
  hitL <- grepl(col1, a2)
  n <- sum(hitL)
  c <- (length(a2) / n)

  # Replace 1st : in each string
  a2 <- stringr::str_replace(a2, ":", "|")

  # Return list of 2 char vectors
  l <- stringr::str_split(a2, "\\|")

  # Get column names from 1 set
  colnames <- sapply(l,"[[",1)
  colnames <- colnames[1:c]

  # Get column values
  colvalue <- sapply(l,"[[",2)
  m2 <- matrix(colvalue, nrow = n, ncol = c, byrow = T)
  df2 <- tibble::as_tibble(m2,  .name_repair = ~ colnames)
  df2
}

# Add a key-value lookup column
wevt_add_lookup_column <- function(key, newcolname){

  n <- length(key)
  keyvalue <- vector("list", n)

  i <- 1
  for (k in key){
    keyvalue[[i]] <- fn::logontype[[k]]
    i <- i + 1
  }
  keyvalue <- unlist(keyvalue)

  # Convert to data frame
  m3 <- matrix(keyvalue)
  df3 <- tibble::as_tibble(m3,  .name_repair = ~ newcolname)

  df3
}

# Return boolean map for section matches
wevt_section_map <- function(a, section, offsets){
  # offsets <- c(1,2,3)

  h <- grepl(section, a)

  i <- 1
  l <- vector("list", length(offsets))
  for (i in offsets) {
    l[[i]] <- which(h) + i
    i <- i + 1
  }
  v <- unlist(l)
  h <- xor(h,h)
  h[v] <- TRUE

  h
}

# Get Windows event log 4624 events
#' @export
wevt_logon_4624 <- function(n=10){

  cmd <- paste0("wevtutil qe \"Security.evtx\" /q:\"Event/System/EventID=4624\" /lf:true /f:text /rd:true /c:", n)
  a <- system(cmd, intern = T)

  # Subset header columns
  h1 <- grepl("Date:|Event ID:| Task:|Keyword:| User:|User Name:|Computer:|Logon Type:", a)
  df <- wevt_add_columns(a, h1, "^Date")

  # Add key-value column
  key <- df$LogonType
  df2 <- wevt_add_lookup_column(key, "TypeName")

  # Add Subject subsection columns
  h2 <- wevt_section_map(a, "Subject:", c(1,2,3))
  a[h2] <- gsub("Security ID:", "SubSecID:", a[h2])
  a[h2] <- gsub("Account Name:", "SubAcctName:", a[h2])
  a[h2] <- gsub("Account Domain:", "SubAcctDomain:", a[h2])
  df3 <- wevt_add_columns(a, h2, "SubSecID")

  # Add New Logon subsection columns
  h3 <- wevt_section_map(a, "New Logon:", c(1,2,3))
  a[h3] <- gsub("Security ID:", "NewSecID:", a[h3])
  a[h3] <- gsub("Account Name:", "NewAcctName:", a[h3])
  a[h3] <- gsub("Account Domain:", "NewAcctDomain:", a[h3])
  df4 <- wevt_add_columns(a, h3, "NewSecID")

  # Add Process Information subsection columns
  h4 <- wevt_section_map(a, "Process Information:", c(1,2))
  df5 <- wevt_add_columns(a, h4, "ProcessID")

  # Add Network Information subsection columns
  h5 <- wevt_section_map(a, "Network Information:", c(1,2,3))
  a[h5] <- gsub("Workstation Name:", "WkstaName:", a[h5])
  a[h5] <- gsub("Source Network Address:", "SrcNetwkAddr:", a[h5])
  a[h5] <- gsub("Source Port:", "SrcPort:", a[h5])
  df6 <- wevt_add_columns(a, h5, "WkstaName")

  df <- dplyr::bind_cols(df, df2, df3, df4, df5, df6)

  View(df)
  df
}

# Get 4624 Interactive events
#' @export
wevt_logoni_4624 <- function(n=10){

  cmd <- paste0("wevtutil qe \"Security.evtx\" /q:\"Event/System/EventID=4624 and (Event/EventData/Data=2 or Event/EventData/Data=10)\" /lf:true /f:text /rd:true /c:", n)
  a <- system(cmd, intern = T)

  # Subset header columns
  h1 <- grepl("Date:|Event ID:| Task:|Keyword:| User:|User Name:|Computer:|Logon Type:", a)
  df <- wevt_add_columns(a, h1, "^Date")

  # Add key-value column
  key <- df$LogonType
  df2 <- wevt_add_lookup_column(key, "TypeName")

  # Add Subject subsection columns
  h2 <- wevt_section_map(a, "Subject:", c(1,2,3,4))
  a[h2] <- gsub("Security ID:", "SubSecID:", a[h2])
  a[h2] <- gsub("Account Name:", "SubAcctName:", a[h2])
  a[h2] <- gsub("Account Domain:", "SubAcctDomain:", a[h2])
  a[h2] <- gsub("Logon ID:", "SubLogonID:", a[h2])
  df3 <- wevt_add_columns(a, h2, "SubSecID")

  # Add New Logon subsection columns
  h3 <- wevt_section_map(a, "New Logon:", c(1,2,3,4))
  a[h3] <- gsub("Security ID:", "NewSecID:", a[h3])
  a[h3] <- gsub("Account Name:", "NewAcctName:", a[h3])
  a[h3] <- gsub("Account Domain:", "NewAcctDomain:", a[h3])
  a[h3] <- gsub("Logon ID:", "NewLogonID:", a[h3])
  df4 <- wevt_add_columns(a, h3, "NewSecID")

  # Add Process Information subsection columns
  h4 <- wevt_section_map(a, "Process Information:", c(1,2))
  df5 <- wevt_add_columns(a, h4, "ProcessID")

  # Add Network Information subsection columns
  h5 <- wevt_section_map(a, "Network Information:", c(1,2,3))
  a[h5] <- gsub("Workstation Name:", "WkstaName:", a[h5])
  a[h5] <- gsub("Source Network Address:", "SrcNetwkAddr:", a[h5])
  a[h5] <- gsub("Source Port:", "SrcPort:", a[h5])
  df6 <- wevt_add_columns(a, h5, "WkstaName")

  df <- dplyr::bind_cols(df, df2, df3, df4, df5, df6)

  View(df)
  df
}

# Get 4625 events
#' @export
wevt_logonfail_4625 <- function(n=10){

  cmd <- paste0("wevtutil qe \"Security.evtx\" /q:\"Event/System/EventID=4625\" /lf:true /f:text /rd:true /c:", n)
  a <- system(cmd, intern = T)

  # Subset header columns
  h1 <- grepl("Date:|Event ID:| Task:|Keyword:| User:|User Name:|Computer:|Logon Type:", a)
  df <- wevt_add_columns(a, h1, "^Date")

  # Add key-value column
  key <- df$LogonType
  df2 <- wevt_add_lookup_column(key, "TypeName")

  # Add Subject subsection columns
  h2 <- wevt_section_map(a, "Subject:", c(1,2,3,4))
  a[h2] <- gsub("Security ID:", "SubSecID:", a[h2])
  a[h2] <- gsub("Account Name:", "SubAcctName:", a[h2])
  a[h2] <- gsub("Account Domain:", "SubAcctDomain:", a[h2])
  a[h2] <- gsub("Logon ID:", "SubLogonID:", a[h2])
  df3 <- wevt_add_columns(a, h2, "SubSecID")

  # Add Account for which ... subsection columns
  h3 <- wevt_section_map(a, "Account For Which Logon Failed:", c(1,2,3))
  a[h3] <- gsub("Security ID:", "TrgtSecID:", a[h3])
  a[h3] <- gsub("Account Name:", "TrgtAcctName:", a[h3])
  a[h3] <- gsub("Account Domain:", "TrgtAcctDomain:", a[h3])
  df4 <- wevt_add_columns(a, h3, "TrgtSecID")

  # Add Process Information subsection columns
  h4 <- wevt_section_map(a, "Process Information:", c(1,2))
  a[h4] <- gsub("Caller Process ID:", "CallerProcID:", a[h4])
  a[h4] <- gsub("Caller Process Name:", "CallerProcName:", a[h4])
  df5 <- wevt_add_columns(a, h4, "CallerProcID")

  # Add Network Information subsection columns
  h5 <- wevt_section_map(a, "Network Information:", c(1,2,3))
  a[h5] <- gsub("Workstation Name:", "WkstaName:", a[h5])
  a[h5] <- gsub("Source Network Address:", "SrcNetwkAddr:", a[h5])
  a[h5] <- gsub("Source Port:", "SrcPort:", a[h5])
  df6 <- wevt_add_columns(a, h5, "WkstaName")

  df <- dplyr::bind_cols(df, df2, df3, df4, df5, df6)

  View(df)
  df
}

# Get 4634 events
#' @export
wevt_logoff_4634 <- function(n=10){

  cmd <- paste0("wevtutil qe \"Security.evtx\" /q:\"Event/System/EventID=4634\" /lf:true /f:text /rd:true /c:", n)
  a <- system(cmd, intern = T)

  # Subset header columns
  h1 <- grepl("Date:|Event ID:| Task:|Keyword:| User:|User Name:|Computer:|Logon Type:", a)
  df <- wevt_add_columns(a, h1, "^Date")

  # Add key-value column
  key <- df$LogonType
  df2 <- wevt_add_lookup_column(key, "TypeName")

  # Add Subject subsection columns
  h2 <- wevt_section_map(a, "Subject:", c(1,2,3,4))
  a[h2] <- gsub("Security ID:", "SubSecID:", a[h2])
  a[h2] <- gsub("Account Name:", "SubAcctName:", a[h2])
  a[h2] <- gsub("Account Domain:", "SubAcctDomain:", a[h2])
  a[h2] <- gsub("Logon ID:", "SubLogonID:", a[h2])
  df3 <- wevt_add_columns(a, h2, "SubSecID")

  df <- dplyr::bind_cols(df, df2, df3)

  View(df)
  df
}

# Get 4647 events
#' @export
wevt_logoff_4647 <- function(n=10){

  cmd <- paste0("wevtutil qe \"Security.evtx\" /q:\"Event/System/EventID=4647\" /lf:true /f:text /rd:true /c:", n)
  a <- system(cmd, intern = T)

  # Subset header columns
  h1 <- grepl("Date:|Event ID:| Task:|Keyword:| User:|User Name:|Computer:", a)
  df <- wevt_add_columns(a, h1, "^Date")

  # Add Subject subsection columns
  h2 <- wevt_section_map(a, "Subject:", c(1,2,3,4))
  a[h2] <- gsub("Security ID:", "SubSecID:", a[h2])
  a[h2] <- gsub("Account Name:", "SubAcctName:", a[h2])
  a[h2] <- gsub("Account Domain:", "SubAcctDomain:", a[h2])
  a[h2] <- gsub("Logon ID:", "SubLogonID:", a[h2])
  df3 <- wevt_add_columns(a, h2, "SubSecID")

  df <- dplyr::bind_cols(df, df3)

  View(df)
  df
}

# Get 4648 events
#' @export
wevt_logon_runas_4648 <- function(n=10){

  cmd <- paste0("wevtutil qe \"Security.evtx\" /q:\"Event/System/EventID=4648\" /lf:true /f:text /rd:true /c:", n)
  a <- system(cmd, intern = T)

  # Subset header columns
  h1 <- grepl("Date:|Event ID:| Task:|Keyword:| User:|User Name:|Computer:", a)
  df <- wevt_add_columns(a, h1, "^Date")

  # Add Subject subsection columns
  h2 <- wevt_section_map(a, "Subject:", c(1,2,3,4))
  a[h2] <- gsub("Security ID:", "SubSecID:", a[h2])
  a[h2] <- gsub("Account Name:", "SubAcctName:", a[h2])
  a[h2] <- gsub("Account Domain:", "SubAcctDomain:", a[h2])
  a[h2] <- gsub("Logon ID:", "SubLogonID:", a[h2])
  df3 <- wevt_add_columns(a, h2, "SubSecID")

  # Add Account Whose Credentials ... subsection columns
  h3 <- wevt_section_map(a, "Account Whose Credentials", c(1,2))
  a[h3] <- gsub("Account Name:", "CredAcctName:", a[h3])
  a[h3] <- gsub("Account Domain:", "CredAcctDomain:", a[h3])
  df4 <- wevt_add_columns(a, h3, "CredAcctName")

  # Add Target Server subsection columns
  h6 <- wevt_section_map(a, "Target Server:", c(1,2))
  a[h6] <- gsub("Target Server Name:", "TrgtSrvrName:", a[h6])
  a[h6] <- gsub("Additional Information:", "TrgtAddlInfo:", a[h6])
  df2 <- wevt_add_columns(a, h6, "TrgtSrvrName")

  # Add Process Information subsection columns
  h4 <- wevt_section_map(a, "Process Information:", c(1,2))
  a[h4] <- gsub("Process ID:", "ProcID:", a[h4])
  a[h4] <- gsub("Process Name:", "ProcName:", a[h4])
  df5 <- wevt_add_columns(a, h4, "ProcID")

  # Add Network Information subsection columns
  h5 <- wevt_section_map(a, "Network Information:", c(1,2))
  a[h5] <- gsub("Network Address:", "NetwkAddr:", a[h5])
  a[h5] <- gsub("Port:", "NetwkPort:", a[h5])
  df6 <- wevt_add_columns(a, h5, "NetwkAddr")

  df <- dplyr::bind_cols(df, df3, df2, df4, df5, df6)

  View(df)
  df
}

# Get 4672 events
#' @export
wevt_logon_admin_4672 <- function(n=10){

  cmd <- paste0("wevtutil qe \"Security.evtx\" /q:\"Event/System/EventID=4672\" /lf:true /f:text /rd:true /c:", n)
  a <- system(cmd, intern = T)

  # Subset header columns
  h1 <- grepl("Date:|Event ID:| Task:|Keyword:| User:|User Name:|Computer:", a)
  df <- wevt_add_columns(a, h1, "^Date")

  # Add Subject subsection columns
  h2 <- wevt_section_map(a, "Subject:", c(1,2,3,4))
  a[h2] <- gsub("Security ID:", "SubSecID:", a[h2])
  a[h2] <- gsub("Account Name:", "SubAcctName:", a[h2])
  a[h2] <- gsub("Account Domain:", "SubAcctDomain:", a[h2])
  a[h2] <- gsub("Logon ID:", "SubLogonID:", a[h2])
  df3 <- wevt_add_columns(a, h2, "SubSecID")

  df <- dplyr::bind_cols(df, df3)

  View(df)
  df
}

#' @export
yarascan <- function(raw_t) {
  cmd <- paste0("yara64 -w c:\\yara\\rules\\index.yar ", raw_t)
  system(cmd)
}
