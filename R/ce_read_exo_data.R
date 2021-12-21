#' Reads in a EXO data file
#' 
#' Data files pulled from a YSI EXO sonde come from either a YSI handheld or 
#' via KorEXO.  This function will read those files and convert them into a 
#' data frame.
#' @param exo_file Path and file name for an exo file to read
#' @param sensor_meta Logical to save sensor metadata included with input EXO 
#'                    file. Defaults to TRUE.
#' @param sensor_meta_file Path where sensor metadat is stored.  Defaults to a 
#'                         metadata folder in the same folder as the 
#'                         \code{exo_file}.                    
#' @export
#' @examples 
#' exo_file_path <- system.file("extdata/210915-112052-_.csv", package = "compeco")
#' ce_read_exo_data(exo_file_path)
ce_read_exo_data <- function(exo_file, sensor_meta = TRUE, 
                             sensor_meta_file = paste0(dirname(exo_file),
                                                       "/metadata/meta_",
                                                       basename(exo_file))){
  
  # Deal with Metadata file
  if(sensor_meta & !dir.exists(dirname(sensor_meta_file))){
    dir.create(dirname(sensor_meta_file))
  } else if(T){
    browser()
    #TODO Check for file existence, if exists message but make sure to change
    #TODO names and add (e.g. _01, _02 etc.)
  }
  
  # Pull out metadata portion and save
  
  
} 