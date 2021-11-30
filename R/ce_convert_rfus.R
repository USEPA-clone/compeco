#' Converts RFUs to concentration in μg/L
#' 
#' This functions needs input data from a fluorometer of RFU's and then
#' uses a standard curve to make the conversion.  The standard curves are stored
#' internally in this package and may be updated.  The module, fluorometer, and 
#' date arguments will specify which curve data file to use.
#' 
#' @param rfu_in A .csv file with sample metadata and RFU.  See ADDEXAMPLEFILE
#'                for an example of the expected file format.
#' @param module One of two options: "ext_chla", "invivo_chla", or "phyco", 
#'                default is "ext_chla".
#' @param fluorometer One of two options: "ours" or "theirs", default is 
#'                     "ours". These represent the two Turner Triology's at
#'                     ACESD.  The "ours" one was purchased by the compeco lab, 
#'                     the "theirs", has been in use for sometime and currently 
#'                     resides in M07. The default is "ours".
#' @param year Year of the standard curve.  If more than one curve caluclated in
#'             a year add "a", "b", ...  Acceptable values are:  "2021"
#' @param output An options output path and csv file name for converted values.
#' @return returns a tibble with proper metadata, input RFUs and converted
#'          concentrations                     
#' @note While it is possible to mix and match modules and fluorometers, don't 
#'        do this.  Keep the "ours" modules with the "ours" fluorometer 
#'        and vice-versa. 
#' @importFrom readr read_csv write_csv
#' @export
#' @examples
#' examp_data <- system.file("extdata/chla_2021_7_29.csv", package = "compeco")
#' ce_convert_rfus(rfu_in = examp_data)
ce_convert_rfus <- function(rfu_in, 
                            module = c("ext_chla", "invivo_chla", "phyco"),
                            year = c("2021"),
                            fluorometer = c("ours", "theirs"),
                            output = NULL){
  module <- match.arg(module)
  year <- match.arg(year)
  fluorometer <- match.arg(fluorometer)
  std_curve <- ce_create_std_curve(module, year, fluorometer)
  
  rfus <- suppressMessages(read_csv(rfu_in))
  browser()
  
  
  
  if(!is.null(output)) write_csv(out, output)
  conc
}

#' Create a standard curve
#'
#' This function creates a standard curve from input fluorometer and 
#' spectrophotometer .csv files.  Curve is used to convert RFUs to µg/L.
#' 
#' @param ... Arguments for module, fluorometer and date of input standard curve
#'            csv files.  Passed from \code{\link{ce_convert_rfus}}
#' @keywords internal
ce_create_std_curve <- function(...){
  browser()
  
}