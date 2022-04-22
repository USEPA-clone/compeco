#' Reads and cleans an EXO2 data file
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
  
  source <- match.arg(source)
  # Reading In Files
  if(readr::guess_encoding(exo_file)[1,1] == "UTF-16LE" & 
     readr::guess_encoding(exo_file)[1,2] == 1){
    x_utf16 <- readBin(exo_file, "raw", n = file.size(exo_file))
    x_utf8 <- iconv(list(x_utf16), from = "UTF-16LE", to = "UTF-8", 
                    toRaw = TRUE)[[1]]
    exo <- readr::read_csv(x_utf8, col_names = FALSE)
    waterbody <- as.character(exo[exo$X1 == "Site:",2])
    exo_begin <- which(str_detect(exo[[1]], "Date"))
    exo <- readr::read_csv(x_utf8, skip = exo_begin)
    exo <- dplyr::mutate(exo, waterbody = waterbody)
    # Could pull header data here that **should** eventually have waterbody/site
  } else {
    exo <- readr::read_csv(exo_file, col_names = FALSE)
    waterbody <- exo[exo$X1 == "Site:",2]
    exo_begin <- which(stringr::str_detect(exo[[1]], "Date"))
    exo <- readr::read_csv(x_utf8, skip = exo_begin)
  }
  # Need to figure out site stuff from EXO2 as well
  exo <- dplyr::mutate(exo, waterbody = waterbody, field_dups = NA_real_, 
                       lab_reps = NA_real_, site = NA_character_, 
                       notes = NA_character_)
  exo <- dplyr::select(exo, date = Date, time = Time, waterbody, site, depth = `DEP m`, 
                field_dups, lab_reps, temp = `°C`, do_perc = `DO %`, 
                cond = `SPC-uS/cm`, pH, nitrate = `UVNOx mg/L`, turbidity = FNU, 
                tss = `TSS mg/L`, phyco_rfu = `BGA-PC RFU`, 
                phyco_conc = `BGA-PC ug/L`, chl_rfu = `Chl RFU`, 
                chl_conc = `Chl ug/L`, fdom_rfu = `fDOM RFU`, 
                fdom_qsu = `fDOM QSU`, pressure = mmHg, notes)
  exo <- tidyr::pivot_longer(exo, temp:pressure, names_to = "variable", values_to = "value")
  exo <- dplyr::mutate(exo, units = case_when(variable == "temp" ~
                                                "°C", 
                                              variable == "do_perc" ~
                                                "%",
                                              variable == "cond" ~
                                                "µS/cm",
                                              variable == "pH" ~
                                                "pH",
                                              variable == "nitrate" ~
                                                "mg/L",
                                              variable == "turbidity" ~
                                                "FNU",
                                              variable == "tss" ~
                                                "mg/L",
                                              variable == "phyco_rfu" ~
                                                "RFU",
                                              variable == "phyco_conc" ~
                                                "µg/L",
                                              variable == "chl_rfu" ~
                                                "RFU",
                                              variable == "chl_conc" ~
                                                "µg/L",
                                              variable == "fdom_rfu" ~
                                                "RFU",
                                              variable == "fdom_qsu" ~
                                                "QSU",
                                              variable == "pressure" ~
                                                "mmHG",
                                              TRUE ~ NA_character_),
                       variable = case_when(variable == "phyco_rfu" ~
                                              "phyco",
                                            variable == "phyco_conc" ~
                                              "phyco",
                                            variable == "chl_rfu" ~
                                              "chl",
                                            variable == "chl_conc" ~
                                              "chl",
                                            variable == "fdom_rfu" ~
                                              "fdom",
                                            variable == "fdom_qsu" ~
                                              "fdom",
                                            variable == "do_perc" ~
                                              "do",
                                            TRUE ~ variable))
  exo <- dplyr::select(exo, date, time, waterbody, site, depth, field_dups, lab_reps, variable, 
                       units, value, notes)
  exo
} 