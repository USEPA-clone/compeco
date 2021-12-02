#' Deal with buoy data
#' 
#' This function fetches buoy data from a remote url (currently EPA's newftp), 
#' downloads it, cleans it, and merges it with existing data.
#' 
#'  @param remote_data URL for the remote data location, currently using 
#'                     newftp.epa.gov
#'  @param user User name for accessing the remote-data URL, defaults to local 
#'              environment variable, NEWFTPU
#'  @param pass Password for accessing the remote-data URL, defaults to local 
#'              environmnetal variable, NEWFTPP.  Ok to use interactively with 
#'              user supplied password, but be careful with passwords stored in
#'              scripts using this function.  Instead use enviornment variables.
#'  @param output Name of output .rda file.  Using rda as its compression is 
#'                quite good and saves space of csv.
#'  @return Tibble of merged data of all buoy data.
#'  @export
ce_process_buoy_data <- function(remote_data = c("sftp://newftp.epa.gov/buoys/"), user = Sys.getenv("NEWFTPU"), pass = Sys.getenv("NEWFTPP"), output){
  ce_pull_buoy_data(remote_data, user, pass)
  ce_merge_buoy_data(output)
} 

#' Pull data
#' 
#' @keywords internal
ce_pull_buoy_data <- function(...){
  args <- list(...)
  remote_data <- args[[1]]
  user <- args[[2]]
  pass <- args[[3]]
  up <- paste0(user,":", pass)
  
  h <- new_handle()
  handle_setopt(handle = h, httpauth = 1, userpwd = up)
  res <- curl_fetch_memory(url = remote_data, handle = h)
  
  file_string <- unlist(str_split(rawToChar(res$content), "\n"))
  files <- str_extract(file_string, "data.*csv")
  files <- files[!is.na(files)]
  new_files <- files[!files %in% list.files(here("data/buoys"))]
  
  for(i in new_files){
    file_url <- paste0("sftp://newftp.epa.gov/buoys/",i)
    file_path <- paste0(here("data/buoys"), "/", i)
    tryCatch({curl_download(file_url, i, handle = h)},
             error = function(e) e, warning = function(w) w
    )
  }
  
}

#' Merge data
#' 
#' @keywords internal
ce_merge_buoy_data <- function(...){
  files <- list.files(here("data/buoys"), full.names = TRUE)
  
  if(file.exists(here("data/buoys/merged_buoy_data.rda"))){
    # Get rda time
    last_rda <- file.info(here("data/merged_buoy_data.rda"))$mtime
    # file times
    files <- files[file.info(files)$mtime >= last_rda]
    load(here("data/merged_buoy_data.rda"))
  } else {
    merged_buoy_data <- data.frame()
  }
  
  merged_buoy_data_new <- map_df(files, clean_buoy_data) %>%
    unique()
  
  merged_buoy_data <- rbind(merged_buoy_data, merged_buoy_data_new) %>%
    unique() %>%
    # Time Zero for both ponds
    filter((waterbody == "shubael" & date_time >= "2021-06-10 12:00:00") |
             (waterbody == "hamblin" & date_time >= "2021-06-10 14:15"))
  
  save(merged_buoy_data, 
       file = here("data/merged_buoy_data.rda"),
       compress = "xz")
}

#' clean buoy data
#' 
#' This is Cape Code specific now.  Will work until we add other buoys.  Need 
#' to think about how to make this more universal.  Might be on the buoy to 
#' web side of things.
#' 
#' @keywords internal
clean_buoy_data <- function(csv_file){
  buoy_data_header <- read_csv(csv_file, n_max = 3, col_names = FALSE)
  buoy_data_raw <- read_csv(csv_file, skip = 3, col_names = FALSE)
  shubael_cols <- which(str_ends(buoy_data_header[1,], "Shubael Pond"))
  hamblin_cols <- which(str_ends(buoy_data_header[1,], "Hamblin Pond"))
  shubael_col_header <- make.unique(tolower(c("date_time", 
                                              as.character(buoy_data_header[2,shubael_cols]))))
  hamblin_col_header <- make.unique(tolower(c("date_time", 
                                              as.character(buoy_data_header[2,hamblin_cols]))))
  shubael_data <- buoy_data_raw[,c(1,shubael_cols)]
  hamblin_data <- buoy_data_raw[,c(1,hamblin_cols)]
  names(shubael_data) <- shubael_col_header
  names(hamblin_data) <- hamblin_col_header
  shubael_data <- mutate(shubael_data, waterbody = "shubael")
  hamblin_data <- mutate(hamblin_data, waterbody = "hamblin")
  buoy_data <- rbind(shubael_data, hamblin_data)
  buoy_data <- mutate(buoy_data, device = "cb150")
  buoy_data <- select(buoy_data, waterbody, date_time, device, everything())
  buoy_data <- pivot_longer(buoy_data, cols = 'processor power':'roll')
  buoy_data <- mutate(buoy_data, date_time = mdy_hms(date_time, 
                                                     tz = "America/New_York"))
  buoy_data
}