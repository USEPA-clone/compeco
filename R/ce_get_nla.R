#' Function to get 2012 NLA Site data
#' 
#' Grabs data from EPA site, switches all columns to lower case, removes 
#' unsampled sites.
#' 
#' @return A tibble
get_nla_2012_site <- function(){
  site_url <- "https://www.epa.gov/sites/production/files/2016-12/nla2012_wide_siteinfo_08232016.csv"
  site <- readr::read_csv(site_url, na = c("", "NA", "N/A"))
  site <- dplyr::rename_all(site, tolower)
  site <- dplyr::filter(site, !is.na(.data$date_col))
  site
}
#' Function to get 2012 NLA water chemistry data
#' 
#' Grabs data from EPA site, switches all columns to lower case.
#' 
#' @return A tibble
get_nla_2012_wc <- function(){
  wc_url <- "https://www.epa.gov/sites/production/files/2016-12/nla2012_waterchem_wide.csv"
  wc <- readr::read_csv(wc_url, na = c("", "NA", "N/A", "CALC"))
  wc <- dplyr::rename_all(wc, tolower)
  wc
}

#' Function to get 2012 NLA water profile data
#' 
#' Grabs data from EPA site, switches all columns to lower case.
#' 
#' @return A tibble
get_nla_2012_profile <- function(){
  profile_url <- "https://www.epa.gov/sites/production/files/2016-12/nla2012_wide_profile_08232016.csv"
  profile <- readr::read_csv(profile_url, na = c("", "NA", "N/A"))
  profile <- dplyr::rename_all(profile, tolower)
  profile
}

#' Function to get 2012 NLA toxin data
#' 
#' Grabs data from EPA site, switches all columns to lower case.
#' 
#' @return A tibble
get_nla_2012_toxin <- function(){
  toxin_url <- "https://www.epa.gov/sites/production/files/2016-11/nla2012_algaltoxins_08192016.csv"
  toxin <- readr::read_csv(toxin_url, na = c("", "NA", "N/A"))
  toxin <- dplyr::rename_all(toxin, tolower)
  toxin
}

#' Function to get 2012 NLA chlorophyll a data
#' 
#' Grabs data from EPA site, switches all columns to lower case.
#' 
#' @return A tibble
get_nla_2012_chla <- function(){
  
  chla_url <- "https://www.epa.gov/sites/production/files/2016-11/nla2012_chla_wide.csv"
  chla <- readr::read_csv(chla_url, na = c("", "NA", "N/A"))
  chla <- dplyr::rename_all(chla, tolower)
  chla
}

#' Function to get 2007 All Data Zip
#' 
#' Downloads and extracts all 2007 NLA data
#' 
#' @param data_dir Location to download and save data to. Defaults to a "data" 
#'                 folder in the current working directory.  Will create if it
#'                 does not current exist.
#' @param zip_dir location, within data_dir to extract files.  Defaults to 
#'                "nla2007"
#' @return Character path for unzipped data
get_nla_2007_all <- function(data_dir = "data", zip_dir = "nla2007"){
  data_dir <- normalizePath(data_dir)
  zip_file <- normalizePath(paste0(data_dir, "/nla2007_alldata.zip"),
                            mustWork = FALSE)
  file_dir <- normalizePath(paste0(data_dir, paste0("/", zip_dir)), 
                            mustWork = FALSE) 
  if(!dir.exists(data_dir)){dir.create(data_dir)}
  if(!file.exists(zip_file)){
    zip_url <- "https://www.epa.gov/sites/default/files/2017-02/nla2007_alldata.zip"
    resp <- httr::GET(zip_url, 
                      httr::write_disk(zip_file))
  }
  if(!dir.exists(file_dir)){
    message("Unzipping files.")
    unzip(zip_file, exdir = file_dir)
  }
  file_dir
}

#' Function to get 2007 NLA Site data
#' 
#' Grabs data from a path, switches all columns to lower case.
#' @param nla_site_file path to where nla data exist.  Defaults to locally 
#'                     downloaded files in "data/nla2007"
#' 
#' @return A tibble
get_nla_2007_site <- function(nla_site_file = 
                                "data/nla2007/NLA2007_SampledLakeInformation_20091113.csv"){
  nla_site_file <- normalizePath(nla_site_file)
  if(!file.exists(nla_site_file)){
    stop("Site information file does not exist.  Download with get_nla_2007_all.")
  }
  site <- readr::read_csv(nla_site_file, na = c("", "NA", "N/A"))
  site <- dplyr::rename_all(site, tolower)
  site
}

#' Function to get 2007 NLA water quality data
#' 
#' Grabs data from a path, switches all columns to lower case.
#' @param nla_wq_file path to where nla data exist.  Defaults to locally 
#'                     downloaded files in "data/nla2007"
#' 
#' @return A tibble
get_nla_2007_wc_chla <- function(nla_wq_file = 
                                   "data/nla2007/NLA2007_WaterQuality_20091123.csv"){
  
  nla_wq_file <- normalizePath(nla_wq_file)
  if(!file.exists(nla_wq_file)){
    stop("Water quality file does not exist.  Download with get_nla_2007_all.")
  }
  wc_chla <- readr::read_csv(nla_wq_file, na = c("", "NA", "N/A"))
  wc_chla <- dplyr::rename_all(wc_chla, tolower)
  wc_chla
}

#' Function to get 2007 NLA toxin data
#' 
#' Grabs data from a path, switches all columns to lower case.  The recreational 
#' condition data includes toxins and various other measures.
#' 
#' @param nla_rec_cond_file path to where nla data exist.  Defaults to locally 
#'                     downloaded files in "data/nla2007"
#' 
#' @return A tibble
get_nla_2007_toxin <- function(nla_rec_cond_file = 
                                 "data/nla2007/NLA2007_Recreational_ConditionEstimates_20091123.csv"){
  
  nla_rec_cond_file <- normalizePath(nla_rec_cond_file)
  toxin <- readr::read_csv(nla_rec_cond_file, na = c("", "NA", "N/A"))
  toxin <- dplyr::rename_all(toxin, tolower)
  toxin
}

################################################################################
# Cleaning/QA Flag functions

#' Clean 2012 NLA Site Data
#' @param site_df A data frame of NLA site data, from 
#'                \code{get_nla_2012_site()}.
#' @return A tibble of cleaned 2012 NLA site data
clean_nla_2012_site <- function(site_df){
  site_df_out <- dplyr::select(site_df, site_id, siteid_07, uid, date_col, 
                               visit_no, state, cntyname, gnis_id, gnis_name, 
                               nars_name, comid2007, comid2012, comids2007, 
                               area_ha, lon_dd83, lat_dd83, xcoord, ycoord,
                               im_comment)
  site_df_out
}

#' Clean 2012 NLA Water Chemistry Data
#' @param wc_df A data frame of NLA water chemistry data, from 
#'              \code{get_nla_2012_waterchem()}.
#' @param site_df A data frame of 2012 NLA sites, from 
#'              \code{clean_nla_2012_site()}.
#' @param chla_df A data frame of 2012 NLA chlorophyll data, from 
#'              \code{get_nla_2012_chla()}.
#' @param tox_df A data frame of 2012 NLA cyanotoxin data, from 
#'              \code{get_nla_2012_toxin()}.                         
#' @return A tibble of cleaned 2012 NLA site data
clean_nla_2012 <- function(wc_df, site_df, chla_df, tox_df){
  # double check that flag fields don't have different names than variables
  # comments?
  # compare units to 2007 - see Jeff's handwritten notes
  site_df<- dplyr::select(site_df, uid, date_col, site_id, visit_no,
                          site_id_07 = siteid_07)
  chla_df <- dplyr::select(chla_df, uid, chlx_result, chlx_units, chlx_lab_flag,
                           chlx_qa_flag, chlx_mdl, chlx_rl)
  tox_df <- dplyr::select(tox_df, uid, micx_result, micx_units, micx_qa_flag, 
                          micx_mdl, micx_rl)
  
  unique_id <- readr::read_csv(here::here("data/NARS_FullSiteList_through2019.csv"))
  unique_id <- dplyr::rename_all(unique_id, tolower)
  unique_id <- dplyr::filter(unique_id, study == "NLA", dsgn_cycle == "2012")
  unique_id <- dplyr::select(unique_id, unique_id, site_id, lon_dd83, lat_dd83)
  
  wc_df <- dplyr::left_join(wc_df, site_df, by = "uid")
  wc_df <- dplyr::full_join(wc_df, chla_df, by = "uid")
  wc_df <- dplyr::full_join(wc_df, tox_df, by = "uid")
  wc_df <- dplyr::left_join(wc_df, unique_id, by = "site_id")
  
  enlongen <- function(df, col_type, col_out){
    df <- dplyr::select(df, unique_id, site_id, date_col, visit_no, 
                        lon_dd83, lat_dd83, contains(col_type))
    df <- tidyr::pivot_longer(df, cols = ends_with(col_type),
                              names_to = "variable", values_to = col_out)
    df <- dplyr::mutate(df, variable = stringr::str_replace(.data$variable, 
                                                            col_type, ""))
    df
  }
  
  #wc_df_lab_flag <- enlongen(wc_df, "_lab_flag", "lab_flag")
  wc_df_qa_flag <- enlongen(wc_df, "_qa_flag", "qa_flag")
  wc_df_units <- enlongen(wc_df, "_units", "units")
  wc_df_result <- enlongen(wc_df, "_result", "value")
  wc_df_mdl <- enlongen(wc_df, "_mdl", "min_detect_limit")
  wc_df_rl <- enlongen(wc_df, "_rl", "rep_limit")
  
  suppressMessages({
    #wc_df_j <- dplyr::full_join(wc_df_lab_flag, wc_df_qa_flag)
    wc_df_j <- dplyr::full_join(wc_df_qa_flag, wc_df_units)
    wc_df_j <- dplyr::full_join(wc_df_j, wc_df_result)
    wc_df_j <- dplyr::full_join(wc_df_j, wc_df_mdl)
    wc_df_j <- dplyr::full_join(wc_df_j, wc_df_rl)
  })
  
  wc_df_out <- dplyr::select(wc_df_j, unique_id, site_id, date_col, visit_no, 
                             lon_dd83, lat_dd83, variable, value, units, 
                             min_detect_limit, rep_limit, qa_flag)
  wc_df_out <- dplyr::mutate(wc_df_out, date_col = lubridate::mdy(date_col))
  wc_df_out
}



#' Clean 2007 NLA Water Chemistry Data
#' @param wc_chla_df A data frame of 2007 NLA water chemistry and chlorphyll 
#'                   data, from \code{get_nla_2007_waterchem()}.
#' @param tox_df A data frame of 2007 NLA cyanotoxin data from 
#'               \code{get_nla_2007_toxin()}.               
#' @return A tibble of cleaned 2007 NLA site data
clean_nla_2007 <- function(wc_chla_df, tox_df){
  
  # TODO Some questions on correct units/limits - These are as is from docs.
  #      Deal with changes in other cleaning functions.
  #   analyte mdl   rl    units   mdl from QAPP only
  #   cond    na    2.0   uS/cm
  #   turb    1     2.0   NTU
  #   ph      na    na    ph  
  #   anc     na    na    ueq/L
  #   toc/doc 0.10  0.20  mg C/L
  #   ammonia 0.01  0.02  mg N/L
  #   n03_n02 0.01  0.02  mg N/L
  #   tn      0.01  20    ug/L *should be ug N/L?* mdl from qapp in mg/L
  #   tp      2     4     ug P/L
  #   po4     2     4     ug P/L in QAPP, not in dataset?
  #   so4     5     10.41 ueq/L  slightly lower than QAPP
  #   cl      3     5.64  ueq/L  slightly lower than QAPP
  #   no3     1     1.43  ueq/L  this looks wrong - 4 in QAPP, 1.43 is nh3
  #   ca      2.5   4.99  ueq/L
  #   mg      4     8.23  ueq/L
  #   na      2     4.35  ueq/L
  #   k       1     2.56  ueq/L
  #   sio2    0.05  0.10  mg sio2/L
  #   tss     1     2     mg/L  in QAPP, not in dataset?
  #   color   na    5     pcu   if defining rl as 2x mdl - how is this possible?
  #   chla          0.1   ?g/L  this is all wrong...should be ug/L and using wrong rl 3.0 in QAPP
  #   micx   0.05   0.1   µg/L
  
  unique_id <- readr::read_csv(here::here("data/NARS_FullSiteList_through2019.csv"))
  unique_id <- dplyr::rename_all(unique_id, tolower)
  unique_id <- dplyr::filter(unique_id, study == "NLA", dsgn_cycle == "2007")
  unique_id <- dplyr::select(unique_id, unique_id, site_id, lon_dd83, lat_dd83)
  
  tox_df <- dplyr::select(tox_df, site_id, visit_no, micx = mcyst_tl_ugl, 
                          micx_flag = flag_info)
  
  # Rename phlab_flag to ph_lab_flag and no3no2_flag to no3_no2_flag
  # Rename and merge (merge might be unnecessary) chla fld/lab flag and comment
  # Pull in comment fields too
  wc_chla_df <- dplyr::left_join(wc_chla_df, tox_df, by = c("site_id", "visit_no"))
  wc_chla_df <- dplyr::left_join(wc_chla_df, unique_id, by = "site_id")
  
  enlongen <- function(df, col_type, col_out){
    df <- dplyr::select(df, "unique_id", "site_id", "date_col", "visit_no", "lon_dd83", 
                        "lat_dd83", contains(col_type))
    df <- tidyr::pivot_longer(df, cols = ends_with(col_type),
                              names_to = "variable", values_to = col_out)
    df <- dplyr::mutate(df, variable = stringr::str_replace(.data$variable, 
                                                            col_type, ""))
    df
  }
  
  
  
  wc_chla_df_flag <- enlongen(wc_chla_df, "_flag", "qa_flag")
  # second flag not needed?  wc_chla_df_qa_flag <- enlongen(wc_chla_df, "_qa_flag", "qa_flag")
  
  wc_chla_df_result <- dplyr::select(wc_chla_df, unique_id, site_id, date_col, 
                                     visit_no, lon_dd83, lat_dd83, anc, turb, 
                                     toc, doc, nh4, no3_no2, ntl, ptl, cl, no3, 
                                     so4, ca, mg, na, k, color, sio2, micx, 
                                     chla)
  #error below - Error: Can't combine `site_id` <character> and `anc` <double>.
  wc_chla_df_result <- tidyr::pivot_longer(wc_chla_df_result,cols=anc:chla,
                                           names_to = "variable", 
                                           values_to = "value")
  wc_chla_df_units_limits <- dplyr::select(wc_chla_df_result, unique_id, 
                                           site_id, date_col, visit_no, 
                                           lon_dd83, lat_dd83, variable, value)
  wc_chla_df_units_limits <- dplyr::mutate(wc_chla_df_units_limits, units = 
                                             dplyr::case_when(variable == "micx" ~
                                                                "µg/L",
                                                              variable == "ntl" ~
                                                                "µg/L",
                                                              variable == "ptl" ~
                                                                "ug P/L",
                                                              variable == "anc" ~
                                                                "ueq/L",
                                                              variable == "cond" ~
                                                                "uS/cm",
                                                              variable == "turb" ~
                                                                "ntu",
                                                              variable == "ph" ~
                                                                "ph",
                                                              variable == "toc" ~
                                                                "mg C/L",
                                                              variable == "doc" ~
                                                                "mg C/L",
                                                              variable == "nh4" ~
                                                                "mg N/L",
                                                              variable == "no3_no2" ~
                                                                "mg N/L",
                                                              variable == "tn" ~
                                                                "ug/L",
                                                              variable == "tp" ~
                                                                "ug P/L",
                                                              variable == "so4" ~
                                                                "ueq/L",
                                                              variable == "cl" ~
                                                                "ueq/L",
                                                              variable == "no3" ~
                                                                "ueq/L",
                                                              variable == "ca" ~
                                                                "ueq/L",
                                                              variable == "mg" ~
                                                                "ueq/L",
                                                              variable == "na" ~
                                                                "ueq/L",
                                                              variable == "k" ~
                                                                "ueq/L",
                                                              variable == "sio2" ~
                                                                "mg sio2/L",
                                                              variable == "color" ~
                                                                "pcu",
                                                              variable == "chla" ~
                                                                "?g/L",
                                                              TRUE ~ NA_character_))
  wc_chla_df_units_limits <- dplyr::mutate(wc_chla_df_units_limits, mdl = 
                                             dplyr::case_when(variable == "micx" ~
                                                                0.05,
                                                              variable == "ntl" ~
                                                                0.01,
                                                              variable == "ptl" ~
                                                                2,
                                                              variable == "anc" ~
                                                                NA_real_,
                                                              variable == "cond" ~
                                                                NA_real_,
                                                              variable == "turb" ~
                                                                1,
                                                              variable == "ph" ~
                                                                NA_real_,
                                                              variable == "toc" ~
                                                                0.1,
                                                              variable == "doc" ~
                                                                0.1,
                                                              variable == "nh4" ~
                                                                0.01,
                                                              variable == "no3_no2" ~
                                                                0.01,
                                                              variable == "tn" ~
                                                                0.01,
                                                              variable == "tp" ~
                                                                2,
                                                              variable == "so4" ~
                                                                5,
                                                              variable == "cl" ~
                                                                3,
                                                              variable == "no3" ~
                                                                1,
                                                              variable == "ca" ~
                                                                2.5,
                                                              variable == "mg" ~
                                                                4,
                                                              variable == "na" ~
                                                                2,
                                                              variable == "k" ~
                                                                1,
                                                              variable == "sio2" ~
                                                                0.05,
                                                              variable == "color" ~
                                                                NA_real_,
                                                              variable == "chla" ~
                                                                NA_real_,
                                                              TRUE ~ NA_real_))
  wc_chla_df_units_limits <- dplyr::mutate(wc_chla_df_units_limits, rl = 
                                             dplyr::case_when(variable == "micx" ~
                                                                0.1,
                                                              variable == "ntl" ~
                                                                20,
                                                              variable == "ptl" ~
                                                                4,
                                                              variable == "anc" ~
                                                                NA_real_,
                                                              variable == "cond" ~
                                                                2.0,
                                                              variable == "turb" ~
                                                                2.0,
                                                              variable == "ph" ~
                                                                NA_real_,
                                                              variable == "toc" ~
                                                                0.2,
                                                              variable == "doc" ~
                                                                0.2,
                                                              variable == "nh4" ~
                                                                0.02,
                                                              variable == "no3_no2" ~
                                                                0.02,
                                                              variable == "tn" ~
                                                                0.20,
                                                              variable == "tp" ~
                                                                4,
                                                              variable == "so4" ~
                                                                10.41,
                                                              variable == "cl" ~
                                                                5.64,
                                                              variable == "no3" ~
                                                                1.43,
                                                              variable == "ca" ~
                                                                4.99,
                                                              variable == "mg" ~
                                                                8.23,
                                                              variable == "na" ~
                                                                4.35,
                                                              variable == "k" ~
                                                                2.56,
                                                              variable == "sio2" ~
                                                                0.1,
                                                              variable == "color" ~
                                                                5,
                                                              variable == "chla" ~
                                                                0.1,
                                                              TRUE ~ NA_real_))
  
  
  suppressMessages({
    wc_df_j <- dplyr::full_join(wc_chla_df_units_limits, wc_chla_df_flag)
  })
  
  wc_df_out <- dplyr::select(wc_df_j, unique_id, site_id, date_col, visit_no,
                             lon_dd83, lat_dd83, variable, value, units, 
                             min_detect_limit = mdl, rep_limit = rl, qa_flag)
  wc_df_out <- dplyr::mutate(wc_df_out, date_col = lubridate::mdy(date_col))
  
  #    year variable variable_07to12 year2 variable2
  #    2007 anc      anc             2012 anc 
  #    2007 ca       calcium         2012 calcium 
  #    2007 chla     chlx            2012 chlx
  #    2007 cl       chloride        2012 chloride
  #    2007 color    color           2012 color 
  #    2007 cond     cond            2012 cond  
  #    2007 doc      doc             2012 doc
  #    2007 k        potassium       2012 potassium
  #    2007 mg       magnesium       2012 magnesium
  #    2007 micx     micx            2012 micx 
  #    2007 na       sodium          2012 sodium  
  #    2007 nh4      ammonia_n       2012 ammonia_n
  #    2007 no3      nitrate_n       2012 nitrate_n 
  #    2007 no3_no2  nitrate_nitrite_n 2012 nitrate_nitrite_n
  #    2007 NA       NA              2012 nitrite_n ???
  #    2007 ntl      ntl             2012 ntl
  #    2007 phlab    ph              2012 ph
  #    2007 ptl      ptl             2012 ptl
  #    2007 sio2     silica          2012 silica
  #    2007 so4      sulfate         2012 sulfate
  #    2007 toc      toc             2012 NA   
  #    2007 turb     turb            2012 turb
  #    2007 NA       NA              2012 aluminum
  #    2007 NA       NA              2012 tss 
  
  wc_df_out <- dplyr::mutate(wc_df_out, 
                             variable = 
                               dplyr::case_when(variable == "ca" ~ "calcium",
                                                variable == "chla" ~ "chlx",
                                                variable == "cl" ~ "chloride",
                                                variable == "k" ~ "potassium",
                                                variable == "mg" ~ "magnesium",
                                                variable == "na" ~ "sodium",
                                                variable == "nh4" ~ "ammonia_n",
                                                variable == "no3" ~ "nitrate_n",
                                                variable == "no3_no2" ~ "nitrate_nitrite_n",
                                                variable == "phlab" ~ "ph",
                                                variable == "sio2" ~ "silica",
                                                variable == "so4" ~ "sulfate",
                                                TRUE ~ variable))
  wc_df_out
}


#' Merge multiple NLAs
#' 
#' This function takes cleaned up NLA datasets and merges them into a single
#' master NLA data frame
#' 
#' @param ... NLA data frames to merge.  These are output from 
#'            \code{clea_nla_xxx()}.
#' @return A merged tibble of cleaned NLA data
merge_nla <- function(...){
  
  nla_dfs <- list(...)
  nla_out <- do.call(rbind,  nla_dfs)
  
  nla_out
}
