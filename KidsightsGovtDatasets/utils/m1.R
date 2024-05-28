m1<-function(raw_datasets, dprior){ # m1-DRAWACIRCLE
  
  require(tidyverse)
  
  
  #Recode 2016-2022: K2Q01_D
  df_m1 = lapply(2022, function(x){
    
    var_cahmi = "DRAWACIRCLE"
    recode_it(rawdat = raw_datasets[[as.character(x)]], 
              year = x, 
              lex = "m1_22", 
              var_cahmi = var_cahmi, 
              reverse=F) 
  })  %>% 
    dplyr::bind_rows() %>% 
    dplyr::mutate(across(everything(), zap_all)) %>% 
    as.data.frame() %>% 
    dplyr::rename_all(tolower)

  #Construct Mplus syntax
  syntax_m1 = list(
    TITLE = "!m1_22 (DRAWACIRCLE): How well can this child draw a circle?",
    VARIABLE = list(USEV = c("m1_22"), 
                    CATEGORICAL = c("m1_22")
                    )
    ,
    MODEL= c("!m1_22 (DRAWACIRCLE)",
             " MO by m1_22*1 (am1)",
             " [m1_22$1*] (t1m1);", 
             " [m1_22$2*] (t2m1);", 
             " [m1_22$3*] (t3m1);"
            ),
    `MODEL PRIORS` = "!m1_22 (DRAWACIRCLE)"
  )
  

  # Create a plot to look at differnces in cumulative item percentages
  plot_m1 = weighted_twoway(df = df_m1, var = "m1_22") %>% 
    dplyr::arrange(sc_age_years) %>% 
    tau_plot(item="m1_22")
  
  return(list(data = df_m1 %>% dplyr::select(year,hhid,m1_22), syntax = syntax_m1, plot = plot_m1))
  
}


