o4<-function(raw_datasets, dprior){ # 4-PLAYWELL
  #FIX MODEL PRIOR (SEE E.G. E1)  
  
  require(tidyverse)
  
  
  #Recode 
  df_o4 = lapply(2017:2022, function(x){
    var = paste0("PlayWell_",x-2000)
    recode_it(rawdat = raw_datasets[[as.character(x)]], 
              year = x, 
              lex = "o4_1722", 
              var_cahmi = var, 
              reverse=T) 
  })  %>% dplyr::bind_rows()
  
  
  #Bind the recoded item response data
  df_o4 = df_o4 %>% 
    dplyr::mutate(across(everything(), zap_all)) %>% 
    as.data.frame() %>% 
    dplyr::rename_all(tolower)
  
  #Construct Mplus syntax
  syntax_o4 = list(
    TITLE = c("!o4 (PlayWell): How often can this child explain things he or she has seen or done so that you get a very good idea what happened?"),
    VARIABLE = list(NAMES = c("o4_1722"), 
                    USEV = c("o4_1722"), 
                    CATEGORICAL = c("o4_1722")
    ),
    MODEL= c("\n!o4_1722 (PlayWell)",
             "   EM by o4_1722*1 (lo4)",
             "   [o4_1722$1*] (t1o4)", 
             "   [o4_1722$2*] (t2o4)", 
             "   [o4_1722$3*] (t3o4)",
             "   [o4_1722$4*] (t4o4)"
    ),
    `MODEL PRIORS` = NULL, 
    `MODEL CONSTRAINT` = NULL
  )
  

  return(list(data = df_o4 %>% dplyr::select(year,hhid,starts_with("o4")), syntax = syntax_o4))
  
}