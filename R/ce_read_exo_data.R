#' Reads in a EXO data file
#' 
#' Data files pulled from a YSI EXO sonde come from either a YSI handheld or 
#' via KorEXO.  This function will read those files and convert them into a 
#' data frame.
#' @param exo_file Path and file name for an exo file to read
#' @param source How were the data retrieved from the sensor.  Current options 
#'               include "handheld" or "korexo"
#' @export
#' @examples 
#' exo_file_path <- system.file("extdata/210915-112052-_.csv", package = "compeco")
#' ce_read_exo_data(exo_file_path)
ce_read_exo_data <- function(exo_file, source = c("handheld", "korexo")){
  browser()
  source <- match.arg(source)
  # Reading In Files
  if(readr::guess_encoding(exo_file)[1,1] == "UTF-16LE" & readr::guess_encoding(exo_file)[1,2] == 1){
    x_utf16 <- readBin(exo_file, "raw", n = file.size(exo_file))
    x_utf8 <- iconv(list(x_utf16), from = "UTF-16LE", to = "UTF-8", toRaw = TRUE)[[1]]
    exo <- readr::read_csv(x_utf8, col_names = FALSE)
    exo_begin <- which(str_detect(exo[[1]], "Date"))
    exo <- readr::read_csv(x_utf8, skip = exo_begin)
    # Could pull header data here that **should** eventuall have waterbody/site
  } else {
    exo <- readr::read_csv(exo_file, col_names = FALSE)
    exo_begin <- which(str_detect(exo[[1]], "Date"))
    exo <- readr::read_csv(x_utf8, skip = exo_begin)
  }
    
  
  # Pull out metadata portion and save
  
  
} 