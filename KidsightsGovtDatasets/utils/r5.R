r5<-function(raw_datasets, dprior){ # r5-TEMPER
  #      TEMPER (2017-2021): How often does this child lose control of his or her temper when things do not go his or her way?
                  # value                   label
                  # 1                  Always
                  # 2        Most of the time
                  # 3     About half the time
                  # 4               Sometimes
                  # 5                   Never
  #      TEMPER_R (2022): How often does this child lose their temper?
                  # value                   label
                  # 1                  Always
                  # 2        Most of the time
                  # 3     About half the time
                  # 4               Sometimes
                  # 5                   Never

  
  #Recode 
  df_r5_1721 = lapply(2017:2021, function(x){
    var = paste0("temper_",x-2000)
    recode_it(rawdat = raw_datasets[[as.character(x)]], 
              year = x, 
              lex = "r5_1721", 
              var_cahmi = var, 
              reverse=F) 
  })  %>% dplyr::bind_rows()
  
  #Recode 
  df_r5_22 =  raw_datasets[["2022"]] %>% 
    recode_it(rawdat = ., 
              year = 2022, 
              lex = "r5_22", 
              var_cahmi = "temperR_22", 
              reverse=F) 
  
  #Bind the recoded item response data
  df_r5 = df_r5_1721 %>% 
    dplyr::bind_rows(df_r5_22) %>% 
    dplyr::mutate(across(everything(), zap_all)) %>% 
    as.data.frame() %>% 
    dplyr::rename_all(tolower)
  
  #Construct Mplus syntax
  syntax_r5 = NULL
  

  # Create a plot to look at differnces in cumulative item percentages
  plot_r5 =weighted_twoway(df_r5, var = "r5_1721")%>% 
    bind_rows(weighted_twoway(df_r5, var = 'r5_22')) %>% 
    dplyr::arrange(sc_age_years) %>% 
    tau_plot(item="r5", what = "cumulative", syntax = syntax_r5)
  
  
  return(list(data = df_r5 %>% dplyr::select(year,hhid,starts_with("r5")), syntax = syntax_r5, plot = plot_r5))
  
}


