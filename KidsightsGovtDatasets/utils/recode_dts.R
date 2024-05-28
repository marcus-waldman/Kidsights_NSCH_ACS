recode_dts<-function(dat,vars){
  
  out = dat
  for(v in vars){
    print(v)
    tmp = dat %>% purrr::pluck(v)
    K = max(tmp, na.rm = T)
    if(K>1){
      foo = mat.or.vec(nr=length(tmp), nc = K) + NA
      for(k in 0:(K-1)){
        foo[tmp<k, k+1] = NA
        foo[tmp==k, k+1] = 0
        foo[tmp>k, k+1] = 1
      }
      foo = data.frame(foo)
      names(foo) =  paste0(v, letters[1:K])
    } else {
      foo = data.frame(foo = tmp)
      names(foo) = paste0(v,"a")
    }
    out = out %>% dplyr::bind_cols(foo)
  }
  
  return(out)
  
}