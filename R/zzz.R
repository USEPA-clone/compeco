.onLoad <- function(libname, pkgname){
  # Builds acceptable year variables for the spec and fluoro standard curve
  # files
  years <- stringr::str_split(list.files(system.file("extdata", 
                                                     package = "compeco"), 
                                "*fluorometer*"), "_")
  years_idx <- purrr::map(years, function(x) stringr::str_detect(x, "\\d"))
  years <- unique(unlist(purrr::map2(years, years_idx, 
                                       function(x, y) unique(x[y]))))
  assign("years", years, envir = parent.env(environment()))
}