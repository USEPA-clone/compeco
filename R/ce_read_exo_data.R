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
  # Deal with Metadata file
  if(sensor_meta & !dir.exists(dirname(sensor_meta_file))){
    dir.create(dirname(sensor_meta_file))
  } else if(sensor_meta){
    if(file.exists(sensor_meta_file)){
      message(paste0("The ", basename(sensor_meta_file), " file exists.  Creating new file with incremented ID added."))
      existing_files <- list.files(dirname(sensor_meta_file), 
                                   pattern = gsub(".csv", "", 
                                                  basename(sensor_meta_file)))
      existing_files <- gsub(".csv", "", existing_files)
      if(length(existing_files) > 1){
        number <- stringr::str_extract(existing_files, "[0-9]*$")
        number <- as.numeric(number)
        number <- as.character(number + 1)
        if(nchar(number) == 1){number <- paste0("0", number)}
        sensor_meta_file <- paste0(tools::file_path_sans_ext(sensor_meta_file), 
                                   number, ".",
                                   tools::file_ext(sensor_meta_file))
      } else {
        sensor_meta_file <- paste0(tools::file_path_sans_ext(sensor_meta_file), 
                                   "01.", tools::file_ext(sensor_meta_file))
      }
    }
    meta <- readLines(exo_file, skipNul = TRUE)
    idx <- which(stringr::str_detect(meta, "^Date")) - 1
    writeLines(meta[1:idx], sensor_meta_file,sep = ",")
    
  }
  
  
  # Pull out metadata portion and save
  
  
} 