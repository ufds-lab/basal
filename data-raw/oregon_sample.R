load("data-raw/oregon_sample.rda")

unique_counties = unique(oregon_sample$countyfips)

county_lookup = lapply(unique_counties, function (code) {
  code <- substring(code, 3)
  return (
    gsub(" County", "", x = tidycensus::fips_codes[
      (tidycensus::fips_codes$county_code == code) & 
        (tidycensus::fips_codes$state == "OR"),
    ]$county
    
    ))
})

names(county_lookup) <- unique_counties

county_imputation_function <- function (county_list) {
  sapply(county_list, function (x) {county_lookup[[x]]})
}

oregon_sample = oregon_sample |>
  dplyr::select(biomass,
                countyfips,
                evt_class_cd,
                lfevc,
                elev,
                ppt) |>
  dplyr::rename(county = countyfips,
                evt = evt_class_cd,
                evc = lfevc)
oregon_sample$county = county_imputation_function(oregon_sample$county)


usethis::use_data(oregon_sample, overwrite = TRUE)
