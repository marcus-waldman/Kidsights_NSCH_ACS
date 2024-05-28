extract_vars_cahmi<-function(source,...,vars=NULL){

  extracted_df = NULL
  if(source == "2021-2022"){
    extracted_df = cahmi_2021_2022 %>% dplyr::mutate(fwc = fwc_2122) %>%
      dplyr::select(YEAR,FIPSST,STRATUM,HHID, fwc, all_of(vars))
    names(extracted_df) = tolower(names(extracted_df))
    extracted_df = extracted_df %>%
      dplyr::mutate(stratum = as.character(stratum %>% zap_attributes()),
                    hhid = as.character(hhid %>% zap_attributes()),
                    fipsst = zap_attributes(fipsst)
      )
  }
  if(source == "2019-2020"){
    extracted_df = cahmi_2019_2020 %>% dplyr::mutate(fwc = fwc_1920) %>%
      dplyr::select(year,fipsst,stratum,hhid, fwc, all_of(vars)) %>%
      dplyr::mutate(fwc = fwc) %>%
      dplyr::mutate(hhid = as.character(hhid)) %>%
      dplyr::mutate(fipsst = zap_attributes(fipsst))
  }
  if(source == "2017-2018"){
    extracted_df = cahmi_2017_2018 %>% dplyr::mutate(fwc = FWC_1718) %>%
      mutate(fwc = fwc) %>%
      dplyr::select(YEAR,FIPSST,STRATUM,HHID, fwc, all_of(vars))
    names(extracted_df) = tolower(names(extracted_df))
    extracted_df = extracted_df %>%
      dplyr::mutate(stratum = as.character(stratum),
                    hhid = as.character(hhid),
                    fipsst = zap_attributes(fipsst)
                    )
  }
  if (source == "2016"){
    extracted_df= cahmi_2016 %>%
      dplyr::mutate(fwc = FWC) %>%
      dplyr::select(YEAR,FIPSST,STRATUM,HHID, fwc, all_of(vars))
    names(extracted_df) = tolower(names(extracted_df))
    extracted_df = extracted_df %>%
      dplyr::mutate(fipsst = zap_attributes(fipsst))
  }

  return(extracted_df)
}
