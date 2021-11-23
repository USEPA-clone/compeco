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
#' @param fluorometer One of two options: "compeco" or "acesd", default is 
#'                     "compeco". These represent the two Turner Triology's at
#'                     ACESD.  The "compeco" one was purchased by our lab, the
#'                     "acesd", has been in use for sometime and currently 
#'                     resides in M07. The default is "compeco".
#' @param date Date of the standard curve.  Acceptable values are:  "NEED THESE"
#' @param output An options output path and csv file name for converted values.
#' @return returns a tibble with proper metadata, input RFUs and converted
#'          concentrations                     
#' @note While it is possible to mix and match modules and fluormeters, don't 
#'        do this.  Keep the "compeco" modules with the "compeco" fluorometer 
#'        and vice-versa. 
#' @importFrom readr read_csv write_csv
#' @export
#' @examples
#' examp_data <- system.file("extdata/chla_2021_7_29.csv", package = "compeco")
#' ce_convert_rfus(rfu_in = examp_data)
ce_convert_rfus <- function(rfu_in, 
                            module = c("ext_chla", "invivo_chla", "phyco"),
                            fluorometer = c("compeco", "acesd"),
                            date = c("2021-11-16"),
                            output = NULL){
  module <- match.arg(module)
  fluorometer <- match.arg(fluorometer)
  date <- match.arg(date)
  rfus <- suppressMessages(read_csv(rfu_in))
  browser()
  
  
  
  if(!is.null(output)) write_csv(out, output)
  conc
}
