#' Reads in a EXO data file
#' 
#' Data files pulled from a YSI EXO sonde come from either a YSI handheld or 
#' via KorEXO.  This function will read those files and convert them into a 
#' data frame.
#' @param exo_file Path and file name for an exo file to read
#' @export
#' @examples 
#' exo_file_path <- system.file("extdata/210915-112052-_.csv", package = "compeco")
#' ce_read_exo_data(exo_file_path)
ce_read_exo_data <- function(exo_file){
  
} 