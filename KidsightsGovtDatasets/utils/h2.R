h2<-function(raw_datasets, dprior){ # 2-K2Q01_D
  
  require(tidyverse)

  # #
  # 1                                    Excellent
  # 2                                    Very Good
  # 3                                         Good
  # 4                                         Fair
  # 5                                         Poor
  
  # Fix the fact that there are no value labels in 2016
  labels_16 = sjlabelled::get_values(raw_datasets[["2017"]]$K2Q01_D)
  names(labels_16) = sjlabelled::get_labels(raw_datasets[["2017"]]$K2Q01_D)
  raw_datasets[["2016"]]$K2Q01_D = sjlabelled::add_labels(raw_datasets[["2016"]]$K2Q01_D, labels = labels_16)
  
  
  #Recode 2017_2022: K2Q01_D
  df_h2 = lapply(2017:2022, function(x){
    
    recode_it(rawdat = raw_datasets[[as.character(x)]], 
              year = x, 
              lex = "h2_1722", 
              var_cahmi = "K2Q01_D", 
              reverse=T, 
              force_value_missing = c(6)) 
  })  %>% 
    dplyr::bind_rows() %>% 
    dplyr::mutate(across(everything(), zap_all)) %>% 
    as.data.frame() %>% 
    dplyr::rename_all(tolower)
  
  
  
  #Construct Mplus syntax
  syntax_h2 = list(
    TITLE = "!h2_1722 (K2Q01_D): How would you describe the condition of this child’s teeth?",
    VARIABLE = list(USEV = c("h2_1722"), 
                    CATEGORICAL = c("h2_1722")
    ),
    MODEL= c("!h2_1722 (K2Q01_D)",
             " HE by h2_1722*1 (ah2)",
             " [h2_1722$1*] (t1h2)", 
             " [h2_1722$2*] (t2h2)", 
             " [h2_1722$3*] (t3h2)",
             " [h2_1722$4*] (t4h2)"
    ),
    `MODEL PRIORS` = NULL
  )
  

  # Create a plot to look at differnces in cumulative item percentages
  plot_h2 = weighted_twoway(df = df_h2, var = "h2_1722") %>% 
    dplyr::arrange(sc_age_years) %>% 
    tau_plot(item="h2_1722")
  
  return(list(data = df_h2 %>% dplyr::select(year,hhid,starts_with("h2")), syntax = syntax_h2, plot = plot_h2))
  
}


