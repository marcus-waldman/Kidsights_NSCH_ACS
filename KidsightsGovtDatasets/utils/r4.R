r4<-function(raw_datasets, dprior){ # 4-DISTRACTED
  
  #FIX MODEL PRIOR (SEE E.G. E1)  
  
  #Recode 
  df_r4 = lapply(2017:2022, function(x){
    var = paste0("distracted_",x-2000)
    recode_it(rawdat = raw_datasets[[as.character(x)]], 
              year = x, 
              lex = "r4_1722", 
              var_cahmi = var, 
              reverse=F) 
  })  %>% dplyr::bind_rows()
  

  
  #Bind the recoded item response data
  df_r4 = df_r4 %>% 
    dplyr::mutate(across(everything(), zap_all)) %>% 
    as.data.frame() %>% 
    dplyr::rename_all(tolower)
  
  #Construct Mplus syntax
  syntax_r4 = NULL

  # Create a plot to look at differnces in cumulative item percentages
  plot_r4 = weighted_twoway(df_r4, var = "r4_1722") %>% 
    dplyr::arrange(sc_age_years) %>% 
    tau_plot(item="r4_1722")
  
  return(list(data = df_r4 %>% dplyr::select(year,hhid,starts_with("r4")), syntax = syntax_r4, plot = plot_r4))
  
}


