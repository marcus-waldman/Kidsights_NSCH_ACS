r1<-function(raw_datasets, dprior){ 
  
  #FIX MODEL PRIOR (SEE E.G. E1)  (This one may be correct already)
  
  
  # r1-NEWACTIVITY (2017-21): How often does this child become angry or anxious when going from one activity to another?
  #    STARTNEWACT (2022): How often does this child have difficulty when asked to end one activity and start a new activity?
  
  require(tidyverse)

  #Recode 
  df_r1_1721 = lapply(2017:2021, function(x){
    var = paste0("NewActivity_",x-2000)
    recode_it(rawdat = raw_datasets[[as.character(x)]], 
              year = x, 
              lex = "r1_1721", 
              var_cahmi = var, 
              reverse=F) 
  })  %>% dplyr::bind_rows()
  
  #Recode 
  df_r1_22 =  raw_datasets[["2022"]] %>% 
    recode_it(rawdat = ., 
              year = 2022, 
              lex = "r1_22", 
              var_cahmi = "StartNewAct_22", 
              reverse=F) 
  
  #Bind the recoded item response data
  df_r1 = df_r1_1721 %>% 
    dplyr::bind_rows(df_r1_22) %>% 
    dplyr::mutate(across(everything(), zap_all)) %>% 
    as.data.frame() %>% 
    dplyr::rename_all(tolower)
  
  #Construct Mplus syntax
  syntax_r1 = list(
    TITLE = NULL,
    VARIABLE = list(USEV = c("r1_1721, r1_22"), 
                    CATEGORICAL = c("r1_1721", "r1_22")
    ),
    MODEL= NULL,
    `MODEL PRIORS` = NULL
  )
  

  # Create a plot to look at differnces in cumulative item percentages
  plot_r1 = weighted_twoway(df_r1, var = "r1_1721") %>% 
    bind_rows(weighted_twoway(df_r1, var = "r1_22")) %>% 
    dplyr::arrange(sc_age_years) %>% 
    tau_plot(item="r1", what = "cumulative", syntax = syntax_r1)
  
  
  return(list(data = df_r1 %>% dplyr::select(year,hhid,starts_with("r1")), syntax = syntax_r1, plot = plot_r1))
  
}


