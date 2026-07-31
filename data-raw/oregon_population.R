load("data-raw/oregon_population.rda")

unique_counties = unique(oregon_population$countyfips)

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

oregon_population = oregon_population |>
  dplyr::select(countyfips,
                evt_class_cd,
                lfevc,
                elev,
                ppt) |>
  dplyr::rename(county = countyfips,
                evt = evt_class_cd,
                evc = lfevc)

oregon_population$county = county_imputation_function(oregon_population$county)


usethis::use_data(oregon_population, overwrite = TRUE)
