o5<-function(raw_datasets, dprior){ # 5-HURTSAD
  
  #FIX MODEL PRIOR (SEE E.G. E1)  
  
  
  #Recode 
  df_o5 = lapply(2017:2022, function(x){
    var = paste0("HurtSad_",x-2000)
    recode_it(rawdat = raw_datasets[[as.character(x)]], 
              year = x, 
              lex = "o5_1722", 
              var_cahmi = var, 
              reverse=T) 
  })  %>% dplyr::bind_rows()
  
    # value                   label
    # 1                  Always
    # 2        Most of the time
    # 3     About half the time
    # 4               Sometimes
    # 5                   Never
    
  #Bind the recoded item response data
  df_o5 = df_o5%>% 
    dplyr::mutate(across(everything(), zap_all)) %>% 
    as.data.frame() %>% 
    dplyr::rename_all(tolower)
  
  #Construct Mplus syntax
  syntax_o5 = list(
    TITLE = c("!o5_1722 (HurtSad): How often can this child explain things he or she has seen or done so that you get a very good idea what happened?"),
    VARIABLE = list(NAMES = c("o5_1722"), 
                    USEV = c("o5_1722"), 
                    CATEGORICAL = c("o5_1722")
    ),
    MODEL= c("\n!o5_1722 (HurtSad)",
             "   EM by o5_1722*1 (lo5)",
             "   [o5_1722$1*] (t1o5)", 
             "   [o5_1722$2*] (t2o5)", 
             "   [o5_1722$3*] (t3o5)",
             "   [o5_1722$4*] (t4o5)"
    ),
    `MODEL PRIORS` = NULL, 
    `MODEL CONSTRAINT` = NULL
  )
  
  
  
  return(list(data = df_o5 %>% dplyr::select(year,hhid,starts_with("o5")), syntax = syntax_o5))
  
}


