o1<-function(raw_datasets, dprior){ # 1-CLEAREXP

  # ClearExp: How often can this child explain things he or she has seen or done so that you get a very good idea what happened?
    #(2017-22)
      # 1                  Always
      # 2        Most of the time
      # 3     About half the time
      # 4               Sometimes
      # 5                   Never

  
  #Recode 
  df_o1 = lapply(2017:2022, function(x){
    var = paste0("ClearExp_",x-2000)
    recode_it(rawdat = raw_datasets[[as.character(x)]], 
              year = x, 
              lex = "o1_1722", 
              var_cahmi = var, 
              reverse=T) 
  })  %>% dplyr::bind_rows()
  
  
  #Bind the recoded item response data
  df_o1 = df_o1 %>% 
    dplyr::mutate(across(everything(), zap_all)) %>% 
    as.data.frame() %>% 
    dplyr::rename_all(tolower)
  
  #Construct Mplus syntax
  syntax_o1 = list(
    TITLE = c("!o1_1722 (ClearExp): How often can this child explain things he or she has seen or done so that you get a very good idea what happened?"),
    VARIABLE = list(NAMES = c("o1_1722"), 
                    USEV = c("o1_1722"), 
                    CATEGORICAL = c("o1_1722")
    ),
    MODEL= c("\n! o1_1722 (ClearExp)",
             "   EM by o1_1722*1 (lo1)",
             "   [o1_1722$1*] (t1o1)", 
             "   [o1_1722$2*] (t2o1)", 
             "   [o1_1722$3*] (t3o1)",
             "   [o1_1722$4*] (t4o1)"
    ),
    `MODEL PRIORS` = NULL,
    `MODEL CONSTRAINT` = NULL
  )
  

  
  
  return(list(data = df_o1 %>% dplyr::select(year,hhid,starts_with("o1")), syntax = syntax_o1))
  
}


