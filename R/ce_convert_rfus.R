#' Converts RFUs to concentration in μg/L
#' 
#' This functions needs input data from a fluorometer of RFU's and then
#' uses a standard curve to make the conversion.  The standard curves are are
#' calculated and stored internally in this package and may be updated.  
#' Fluorometer and spec files needed for the standard curves are detailed in 
#' ADDSOPHERE and will work on both blanked and unblanked spec measurements.  
#' Note that the ADDSOPHERE SOP sepcifies blanked mesaurements.  The unblanked 
#' measurements are include for older standard curve methods but should not be 
#' used going forward. The module, fluorometer, and date arguments will specify 
#' which curve data file to use.  
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
#' examp_data <- system.file("extdata/chla_2021_7_14.csv", package = "compeco")
#' # Chla and Ours - Blanked spec
#' ce_convert_rfus(rfu_in = examp_data, module = "ext_chla", fluorometer = "ours")
#' # Chla and Theirs - Unblanked spec
#' ce_convert_rfus(rfu_in = examp_data, module = "ext_chla", fluorometer = "theirs")                
ce_convert_rfus <- function(rfu_in, 
                            module = c("ext_chla", "invivo_chla", "phyco"),
                            year = years,
                            fluorometer = c("ours", "theirs"),
                            output = NULL){
  
  module <- match.arg(module)
  year <- match.arg(year)
  fluorometer <- match.arg(fluorometer)
  std_curve <- ce_create_std_curve(module, year, fluorometer)
  rfus <- suppressMessages(read_csv(rfu_in))
  sample_solid_std <- mean(rfus$value[rfus$site=="solid std"])
  
  # Check solid standard drift
  perc_diff <- abs(1-(sample_solid_std/std_curve$solid_std))
  if(perc_diff >= 0.1){
    stop("Sample solid standard is more than 10% from standard curve solid standard.")
  } else if(perc_diff >= 0.05){
    warning("Sample solid standard is more than 5% from standard curve solid standard.  A new standard curve may be required.")
  }
  
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
  args <- list(...)
  module <- args[[1]]
  year <- args[[2]]
  fluorom <- args[[3]]
  ffile <- paste0(system.file("extdata", package = "compeco"), "/", 
                  module, "_",  year, "_", fluorom, "_fluorometer.csv")
  sfile <-  paste0(system.file("extdata", package = "compeco"), "/", 
                   module, "_",  year, "_", fluorom, "_spec.xlsx")
  fluoro <- suppressMessages(read_csv(ffile))
  fluoro <- dplyr::group_by(fluoro, standard)
  fluoro <- dplyr::summarize(fluoro, avg_value = mean(value))
  fluoro <- dplyr::ungroup(fluoro)
  blank <- fluoro[fluoro$standard == "blank",]$avg_value
  fluoro <- dplyr::mutate(fluoro, blanked_rfus = 
                            dplyr::case_when(standard != "solid" ~ 
                                               avg_value - blank,
                                             TRUE ~ avg_value),
                          standard = tolower(standard))
  fluoro <- dplyr::filter(fluoro, .data$standard != "blank")
  
  if(module == "ext_chla"){
    sheets <- readxl::excel_sheets(sfile)
    specs <- purrr::map(sheets, function(x) {
      spec <- readxl::read_excel(sfile, sheet = x, skip = 4)
      spec <- dplyr::mutate(spec, standard = x)})
    specs <- do.call(rbind, specs)
    specs <- dplyr::filter(specs, nm == 750 | nm == 664)
    
    # Blank correction
    blank750 <- specs[specs$standard == "Blank.Sample" & specs$nm == 750,]$A
    blank664 <- specs[specs$standard == "Blank.Sample" & specs$nm == 664,]$A
    blanked <- dplyr::near(0, blank750, tol = 0.001) | 
                             dplyr::near(0, blank664, tol = 0.001)
    if(!blanked){
      stop("Find Jeff.  The code to deal with un-blanked spec data has not yet been written!")
    } else {
      specs <- tidyr::pivot_wider(specs,standard,names_from = nm, 
                                  names_prefix = "nm", values_from = A)
      specs <- dplyr::mutate(specs, corrected_abs = nm664 - nm750)
    }    
  } else if(module == "phyco"){
    spec <- read_excel(sfile, sheet = 1, skip = 4)
  } else if(module == "invivo_chla"){
    #Who knows???
  }
  specs <- dplyr::mutate(specs, conc = (11.4062*(.data$corrected_abs/10))*1000,
                         standard = gsub(".sample", "", 
                                          tolower(.data$standard)))
  specs <- dplyr::filter(specs, .data$standard != "blank")
  spec_fluor <- dplyr::left_join(fluoro, specs, by = "standard")
  std_curve <- lm(conc ~ blanked_rfus, 
                  data = spec_fluor[spec_fluor$standard != "solid",])
  list(std_curve = std_curve, solid_std = spec_fluor$blanked_rfus[spec_fluor$standard == "solid"])
}

#' Read and clean spec
#' 
#' @keywords internal
ce_read_spec_files <- function(){
  
}
