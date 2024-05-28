library(tidyverse)
library(tidycensus)
library(tigris)
library(sf)

census_api_key(key = "d1c04badd9944b4e12216a494e3ed5fa8547a582", overwrite = T, install = T)
options(tigris_use_cache = TRUE)


v = tidycensus::load_variables(2022, "acs5",cache = T)
concepts = data.frame(bye = unique(v$concept))

View(v %>% dplyr::filter(concept == "Ratio of Income to Poverty Level in the Past 12 Months of Families by Family Type by Presence of Related Children Under 18 Years by Age of Related Children"))

writexl::write_xlsx(concepts, path = "concepts.xlsx")



suffixes = c("Under 5 years", "5 years", "6 to 11 years", "12 to 14 years", "15 years", "16 and 17 years")

tables_to_gather =
   map(suffixes,
            function(x){v %>%
                dplyr::filter(stringr::str_starts(concept,"Poverty Status in the Past 12 Months by Sex by Age ")) %>%
                dplyr::filter(endsWith(label,x))
            }) %>%
    dplyr::bind_rows()



zctas = c("80016","80111")

foo <-
  suppressMessages(
    get_acs(
      geography = "zcta",
      year = 2022,
      variables = tables_to_gather$name[x],
      zcta = zctas,
      geometry = F,
      survey = "acs5",
      cache_table = T,
      show_call = F)
  )

dat_zcta_ = tables_to_gather %>% dplyr::left_join()

zcta_ = "80016"

dat_zcta_ <- pbapply::pblapply(1:nrow(tables_to_gather), function(x){

    dat_zcta_ = tables_to_gather[x, ] %>% dplyr::mutate(zcta = zcta_ , estimate = NA, moe = NA)

    foo <-
      suppressMessages(
          get_acs(
              geography = "zcta",
              year = 2022,
              variables = tables_to_gather$name[x],
              zcta = "80016",
              geometry = F,
              survey = "acs5",
              cb = F,
              cache_table = T,
              show_call = F)
      )

  dat_zcta_$estimate = foo$estimate
  dat_zcta_$moe = foo$moe

  return(dat_zcta_)
}) %>% dplyr::bind_rows()


suffixes = c("With related children of the householder under 18 years:!!Under 5 years only", "With related children of the householder under 18 years:!!Under 5 years and 5 to 17 year", "With related children of the householder under 18 years:!!5 to 17 years only")


tables_to_gather2 = map(suffixes,
                        function(x){v %>%
                            dplyr::filter(concept == "Ratio of Income to Poverty Level in the Past 12 Months of Families by Family Type by Presence of Related Children Under 18 Years by Age of Related Children") %>%
                            dplyr::filter(str_ends(label, x))
                          }) %>%
  dplyr::bind_rows()





