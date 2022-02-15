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
#' @param output An optional output path and csv file name for converted values.
#' @param std_check Logical to determine if a solid standard check should be 
#'                  done.  Defualts to TRUE.
#' @return returns a tibble with proper metadata, input RFUs and converted
#'          concentrations                     
#' @note While it is possible to mix and match modules and fluorometers, don't 
#'        do this.  Keep the "ours" modules with the "ours" fluorometer 
#'        and vice-versa. 
#' @importFrom readr read_csv write_csv
#' @export
#' @examples
#' examp_chla_data <- system.file("extdata/chla_2021_7_14.csv", package = "compeco")
#' examp_phyco_data <- system.file("extdata/phyco_2021_06_28.csv", package = "compeco")
#' ce_convert_rfus(rfu_in = examp_chla_data, module = "ext_chla", fluorometer = "ours")
#' ce_convert_rfus(rfu_in = examp_phyco_data, module = "phyco", fluorometer = "ours")
ce_convert_rfus <- function(rfu_in, 
                            module = c("ext_chla", "invivo_chla", "phyco"),
                            year = years,
                            fluorometer = c("ours", "theirs"),
                            output = NULL,
                            std_check = TRUE){
  
  module <- match.arg(module)
  year <- match.arg(year)
  fluorometer <- match.arg(fluorometer)
  std_curve <- ce_create_std_curve(module, year, fluorometer)
  rfus <- suppressMessages(readr::read_csv(rfu_in, na = c("","NA","na")))
  sample_solid_std <- mean(rfus$value[rfus$site=="solid std"])
  
  # Check solid standard drift
  perc_diff <- abs(1-(sample_solid_std/std_curve$solid_std))
  if(perc_diff >= 0.1 & std_check){
    stop("Sample solid standard is more than 10% from standard curve solid standard.")
  } else if(perc_diff >= 0.05 & std_check){
    warning("Sample solid standard is more than 5% from standard curve solid standard.  A new standard curve may be required.")
  } else {
    message(paste("Solid standard is off by ", round(perc_diff, 1), "%."))
  }
  
  # Convert RFU to Concentration
  conc <- ce_convert_to_conc(rfus, std_curve)
  
  # Add missing columns
  names_to_check <- c("waterbody", "site", "depth", "dups", "reps")
  miss_names <- setdiff(names_to_check, names(conc))
  conc[miss_names] <- NA
  
  # Clean up output concentrations
  conc1 <- dplyr::select(conc, date, waterbody, site, depth, dups, reps, variable, 
                         units, value = value_cor_dilute_correct)
  conc1 <- dplyr::mutate(conc1, variable = module)
  conc2 <- dplyr::select(conc, date, waterbody, site, depth, dups, reps, variable, 
                         units, value = value_field_conc_dilute_correct)
  conc2 <- dplyr::mutate(conc2, variable = module, units = "µg/L")
  conc <- dplyr::bind_rows(conc1, conc2)
  
  
  if(!is.null(output)) write_csv(conc, output)
  message(paste0("Std Curve - year: ", year, ", fluorometer: ", fluorometer, 
                 ", slope: ", round(coef(std_curve$std_curve), 4)))
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
  fluoro <- suppressMessages(readr::read_csv(ffile,
                                      na = c("","NA","na")))
  if(module == "ext_chla"){
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
    sheets <- readxl::excel_sheets(sfile)
    specs <- purrr::map(sheets, function(x) {
      spec <- readxl::read_excel(sfile, sheet = x, skip = 4, 
                                 na = c("","NA","na"))
      spec <- dplyr::mutate(spec, standard = x)})
    specs <- do.call(rbind, specs)
    specs <- dplyr::filter(specs, nm == 750 | nm == 664)
    
    # Blank correction
    blank750 <- specs[specs$standard == "Blank.Sample" & specs$nm == 750,]$A
    blank664 <- specs[specs$standard == "Blank.Sample" & specs$nm == 664,]$A
    blanked <- dplyr::near(0, blank750, tol = 0.001) | 
                             dplyr::near(0, blank664, tol = 0.001)
    if(!blanked){
      stop("Find Jeff and Stephen.  The code to deal with un-blanked spec data has not yet been written!")
    } else {
      specs <- tidyr::pivot_wider(specs,standard,names_from = nm, 
                                  names_prefix = "nm", values_from = A)
      specs <- dplyr::mutate(specs, corrected_abs = nm664 - nm750)
    }
    specs <- dplyr::mutate(specs, conc = (11.4062*(.data$corrected_abs/10))*1000,
                           standard = gsub(".sample", "", 
                                           tolower(.data$standard)))
    specs <- dplyr::filter(specs, .data$standard != "blank")
    fluoro <- dplyr::left_join(fluoro, specs, by = "standard")
    std_curve <- lm(conc ~ 0 + blanked_rfus, 
                    data = fluoro[fluoro$standard != "solid",])
  } else if(module == "phyco"){
    fluoro <- dplyr::group_by(fluoro, standard)
    fluoro <- dplyr::summarize(fluoro, avg_value = mean(value), 
                               conc = mean(concentration))
    fluoro <- dplyr::ungroup(fluoro)
    blank <- fluoro[fluoro$standard == "blank",]$avg_value
    fluoro <- dplyr::mutate(fluoro, blanked_rfus = 
                              dplyr::case_when(standard != "solid" ~ 
                                                 avg_value - blank,
                                               TRUE ~ avg_value),
                            standard = tolower(standard))
    fluoro <- dplyr::filter(fluoro, .data$standard != "blank")
    std_curve <- lm(conc ~ 0 + blanked_rfus, 
                    data = fluoro[fluoro$standard != "solid",])
    
    #Need to create standard curve for phyco
  } else if(module == "invivo_chla"){
    #Who knows???
  }
  
  list(std_curve = std_curve, solid_std = fluoro$blanked_rfus[fluoro$standard == "solid"])
}


#' Convert RFUS to concentration
#'
#' This function creates takes an compeco input rfu object and standard curve 
#' object and converts the rfus to concentration
#' 
#' @param rfus rfu object from \code{ce_convert_rfus}
#' @param std_curve std_curve object from \code{ce_convert_rfus}
#' @keywords internal
ce_convert_to_conc <- function(rfus, std_curve){
  rfus <- dplyr::mutate(rfus, date = lubridate::ymd(paste(year, month, day)))
  blanks <- dplyr::filter(rfus, site == "blank")
  blanks <- dplyr::group_by(blanks, waterbody, site, date)
  blanks <- dplyr::summarize(blanks , blank_cor = mean(value))
  blanks <- dplyr::ungroup(blanks)
  blanks <- dplyr::select(blanks, waterbody, date, blank_cor)
  conc <- dplyr::left_join(rfus, blanks)
  conc <- dplyr::filter(conc, site != "blank")
  conc <- dplyr::mutate(conc, value_cor = value - blank_cor)
  std_curve_slope <- coef(std_curve$std_curve)
  conc <- dplyr::mutate(conc, value_cuvette_conc = value_cor * std_curve_slope)
  conc <- dplyr::mutate(conc, value_field_conc = value_cuvette_conc * (0.01/(filter_vol/1000)))
  conc <- dplyr::filter(conc, site != "solid std")

  # Add dilution column if missing the correct for dilutions
  dilution_col <- c("dilution")
  miss_names <- setdiff(dilution_col, names(conc))
  conc[miss_names] <- 1
  conc <- dplyr::mutate(conc,
                        dilution = dplyr::case_when(is.na(dilution) ~ 1,
                                             TRUE ~ dilution),
                        value_cor_dilute_correct = value_cor * dilution,
                        value_field_conc_dilute_correct = value_field_conc * 
                          dilution)
  conc
}