rm(list = ls())

library(tidyverse)
library(haven)
library(sjlabelled)
library(stringr)
library(readxl)
library(mirt)


# Change repo directory
repo_wd = "C:/cloned-directories/HRTL-2016-2022/HRTL-2016-2022"
#repo_wd = "C:/repos/HRTL-2016-2022"
setwd(repo_wd)

# Initalize functions
for(x in list.files("utils/", full.names = T)){cat(paste0("\n",x, " successfully sourced")); source(x)}


raw_datasets = read_rds(file = "data/raw_datasets.rds")

# Construct skeleton of analytic dataset
dat = lapply(2017:2022, function(x){
  raw_datasets[[as.character(x)]] %>%
    dplyr::select(FIPSST,STRATUM,HHID,FWC,SC_AGE_YEARS, SC_SEX) %>%
    dplyr::filter(SC_AGE_YEARS == 3 | SC_AGE_YEARS == 4 | SC_AGE_YEARS == 5) %>%
    dplyr::rename(AGE = SC_AGE_YEARS) %>%
    dplyr::mutate(SC_SEX=as.integer(SC_SEX==1)) %>%
    dplyr::rename(MALE = SC_SEX) %>%
    dplyr::rename_all(tolower) %>%
    mutate(across(everything(), zap_all)) %>%
    mutate(across(where(is.character), as.numeric))%>%
    mutate(across(everything(), zap_all)) %>%
    dplyr::mutate(year = x) %>%
    dplyr::relocate(year)
}) %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(recnum = 1:nrow(.)) %>%
  dplyr::relocate(recnum) %>%
  as.data.frame() %>%
  dplyr::relocate(recnum,hhid,year,stratum,fipsst,fwc,age,male)

dprior = "NULL"

# ##### Early Learning Skills ####
  #e1-RECOGBEGIN: How often can this child recognize the beginning sound of a word? For example, can this child tell you that the word 'ball' starts with the 'buh' sound?
  e1_list = e1(raw_datasets,dprior)
  dat = dat %>% safe_left_join(e1_list$data, by = c("year","hhid"))
  #e2-SAMESOUND
  e2_list = e2(raw_datasets,dprior)
  dat = dat %>% safe_left_join(e2_list$data, by = c("year","hhid"))
  #e3-RHYMEWORD: How well can this child come up with words that rhyme (e.g., "cat" and "mat")
  e3_list = e3(raw_datasets,dprior)
  dat = dat %>% safe_left_join(e3_list$data, by = c("year","hhid"))
  #e4-RECOGABC
  e4_list = e4(raw_datasets,dprior)
  dat = dat %>% safe_left_join(e4_list$data, by = c("year","hhid"))
  #e5-WRITENAME: How often can this child write their first name, even if some of the letters aren't quite right or are backwards?
  e5_list = e5(raw_datasets,dprior)
  dat = dat %>% safe_left_join(e5_list$data, by = c("year","hhid"))
  #e6-READONEDIGIT
  e6_list = e6(raw_datasets,dprior)
  dat = dat %>% safe_left_join(e6_list$data, by = c("year","hhid"))
  #e7-COUNTTO
  e7_list = e7(raw_datasets,dprior)
  dat = dat %>% safe_left_join(e7_list$data, by = c("year","hhid"))
  #e8-GROUPOFOBJECTS
  e8_list = e8(raw_datasets,dprior)
  dat = dat %>% safe_left_join(e8_list$data, by = c("year","hhid"))
  #e9-SIMPLEADDITION
  e9_list = e9(raw_datasets,dprior)
  dat = dat %>% safe_left_join(e9_list$data, by = c("year","hhid"))



# #### Social Emotional Development ####
  # !o1-CLEAREXP How often can this child explain things he or she has seen or done so that you get a very good idea what happened?
  o1_list = o1(raw_datasets,dprior)
  dat = dat %>% safe_left_join(o1_list$data, by = c("year","hhid"))
  # o2-NAMEEMOTIONS: How often can this child recognize and name their own emotions?
  o2_list = o2(raw_datasets,dprior)
  dat = dat %>% safe_left_join(o2_list$data, by = c("year","hhid"))
  # # o3-SHARETOYS
  o3_list = o3(raw_datasets,dprior)
  dat = dat %>% safe_left_join(o3_list$data, by = c("year","hhid"))
  # # !o4-PLAYWELL: How often does this child play well with others?
  o4_list = o4(raw_datasets,dprior)
  dat = dat %>% safe_left_join(o4_list$data, by = c("year","hhid"))
  # # !o5-HURTSAD
  o5_list = o5(raw_datasets,dprior)
  dat = dat %>% safe_left_join(o5_list$data, by = c("year","hhid"))
  # o6-FOCUSON: How often can this child focus on a task you give them for at least a few minutes? For example, can this child focus on simple chores?
  o6_list = o6(raw_datasets,dprior)
  dat = dat %>% safe_left_join(o6_list$data, by = c("year","hhid"))


#### Health ####
  # h1-K2Q01
  h1_list=h1(raw_datasets,dprior)
  dat = dat %>% safe_left_join(h1_list$data, by = c("year","hhid"))
  # h2-K2Q01_D
  h2_list=h2(raw_datasets,dprior)
  dat = dat %>% safe_left_join(h2_list$data, by = c("year","hhid"))
  # h3-DailyAct
  h3_list=h3(raw_datasets,dprior)
  dat = dat %>% safe_left_join(h3_list$data, by = c("year","hhid"))

# #### Self-Regulation ####
  # !r1-NEWACTIVITY (2016-21): How often does this child have difficulty when asked to end one activity and start a new activity
  #    STARTNEWACT (2022): How often does this child have difficulty when asked to end one activity and start a new activity?
  r1_list = r1(raw_datasets,dprior)
  dat = dat %>% safe_left_join(r1_list$data, by = c("year","hhid"))
  # r2-: CALMDOWN: How often does this child have trouble calming down?
  r2_list = r2(raw_datasets,dprior)
  dat = dat %>% safe_left_join(r2_list$data, by = c("year","hhid"))
  # r3-WAITFORTURN
  r3_list = r3(raw_datasets,dprior)
  dat = dat %>% safe_left_join(r3_list$data, by = c("year","hhid"))
  # !r4-DISTRACTED: How often is this child easily distracted?
  r4_list = r4(raw_datasets,dprior)
  dat = dat %>% safe_left_join(r4_list$data, by = c("year","hhid"))
  # r5- :How often does this child lose their temper?
  r5_list_list = r5(raw_datasets,dprior)
  dat = dat %>% safe_left_join(r5_list_list$data, by = c("year","hhid"))

  # #### Motor Development ####
  #m1-DRAWACIRCLE
  m1_list=m1(raw_datasets,dprior)
  dat = dat %>% safe_left_join(m1_list$data, by = c("year","hhid"))
  #m2-DRAWAFACE
  m2_list=m2(raw_datasets,dprior)
  dat = dat %>% safe_left_join(m2_list$data, by = c("year","hhid"))
  #m3-DRAWAPERSON
  m3_list=m3(raw_datasets,dprior)
  dat = dat %>% safe_left_join(m3_list$data, by = c("year","hhid"))
  #m4-BOUNCEABALL
  m4_list=m4(raw_datasets,dprior)
  dat = dat %>% safe_left_join(m4_list$data, by = c("year","hhid"))
  #m5-USEPENCIL
  m5_list=m5(raw_datasets,dprior)
  dat = dat %>% safe_left_join(m5_list$data, by = c("year","hhid"))


#dat = dat %>% dplyr::select(-dplyr::ends_with("a"), -dplyr::ends_with("b"), -dplyr::ends_with("c"), -dplyr::ends_with("d"))



gpcmitems = dat %>% dplyr::select(e1_1722:m5_1721)
eds = which(startsWith(names(gpcmitems),"e"))
ods = which(startsWith(names(gpcmitems),"o"))
hds = which(startsWith(names(gpcmitems),"h"))
rds = which(startsWith(names(gpcmitems),"r"))
mds = which(startsWith(names(gpcmitems),"m"))

# specific_model_vec = rep(NA, length(names(gpcmitems)))
# specific_model_vec[eds]=1
# specific_model_vec[ods]=2
# specific_model_vec[hds]=3
# specific_model_vec[rds]=4
# specific_model_vec[mds]=5
#
# mirt::mirtCluster(4)
# bfit_gpcm = mirt::bfactor(data = gpcmitems,
#                      model = specific_model_vec,
#                      itemtype = "gpcm")
# mirt::mirtCluster(remove = T)
#
# mirt::mirtCluster(4)
# theta = mirt::fscores(bfit_gpcm, method = "plausible", plausible.type = "MH", technical = list(MHcand = NULL))
# theta2 = mirt::fscores(bfit_gpcm, method = "plausible", plausible.type = "MH", technical = list(MHcand = NULL))
#
#
# hi = data.frame(g1 = theta[,1], g2 = theta2[,1])
#
# mirt::mirtCluster(remove = T)
#
# theta = data.frame(theta)
# names(theta) = c("gen","e","o","h","r","m")
# dat = dat %>% dplyr::bind_cols(theta)
#
# dat$year = as.factor(dat$year)
#
# ggplot(dat, aes(h1_1722, col=year, fill = year=="2020")) + geom_bar(aes(y=after_stat(prop)), position = "dodge")
# ggplot(dat, aes(h2_1722, col=year, fill = year=="2020")) + geom_bar(aes(y=after_stat(prop)), position = "dodge")
# ggplot(dat, aes(h3_1722, col=year, fill = year=="2020")) + geom_bar(aes(y=after_stat(prop)), position = "dodge")
#
#
# dat = dat %>% dplyr::group_by(year) %>% dplyr::summarise()
#
# lm(cbind(gen+e,gen+o,gen+h,gen+r,gen+m)~as.factor(year) + age*male, data = dat, weights = dat$fwc)
#
# mirt::mirtCluster(4)
# dat_imp = mirt::imputeMissing(x = bfit_gpcm, Theta = as.matrix(theta))
# mirt::mirtCluster(remove = T)


G_txt = paste0("G = 1-", ncol(gpcmitems))
E_txt = paste0("E = ",min(eds), "-",max(eds))
O_txt= paste0("O = ",min(ods), "-",max(ods))
H_txt= paste0("H = ",min(hds), "-",max(hds))
R_txt = paste0("R = ",min(rds), "-",max(rds))
M_txt = paste0("M = ",min(mds), "-",max(mds))

Q = mat.or.vec(nr = ncol(gpcmitems), nc = 6)
Q[,1] = 1
Q[eds,2]=1; Q[ods,3]=1; Q[hds,4]=1; Q[rds,5]=1; Q[mds,6]=1

COV = mat.or.vec(6,6) + 1
COV[1,2:6] <- COV[2:6,1] <- 0
mod = mirt::mirt.model(Q, COV=COV)

pars0  = mirt::mirt(data = gpcmitems,
                    mod = mod,
                    itemtype = "gpcm",
                    pars = "values",
                    method = "MHRM",
                    formula = list(
                      ~ 0 + male + I(age-4) + I(year==2017) + I(year==2018) + I(year==2019) + I(year==2020) + I(year==2021),
                      ~ 0 + male + I(age-4) + I(year==2017) + I(year==2018) + I(year==2019) + I(year==2020) + I(year==2021),
                      ~ 0 + male + I(age-4) + I(year==2017) + I(year==2018) + I(year==2019) + I(year==2020) + I(year==2021),
                      ~ 0 + male + I(age-4) + I(year==2017) + I(year==2018) + I(year==2019) + I(year==2020) + I(year==2021),
                      ~ 0 + male + I(age-4) + I(year==2017) + I(year==2018) + I(year==2019) + I(year==2020) + I(year==2021),
                      ~ 0 + male + I(age-4) + I(year==2017) + I(year==2018) + I(year==2019) + I(year==2020) + I(year==2021)
                      )
                    )
pars0$lbound[pars0$name %in% c("a1","a2","a3","a4","a5","a6")] = 0


t1 = proc.time()
fit_gpcm_satbf  = mirt::mirt(data = gpcmitems,
                             mod = mod,
                             itemtype = "gpcm",
                             pars = pars0,
                             method = "MHRM",
                             formula = list(
                               ~ 0 + male + I(age-4) + I(year==2017) + I(year==2018) + I(year==2019) + I(year==2020) + I(year==2021),
                               ~ 0 + male + I(age-4) + I(year==2017) + I(year==2018) + I(year==2019) + I(year==2020) + I(year==2021),
                               ~ 0 + male + I(age-4) + I(year==2017) + I(year==2018) + I(year==2019) + I(year==2020) + I(year==2021),
                               ~ 0 + male + I(age-4) + I(year==2017) + I(year==2018) + I(year==2019) + I(year==2020) + I(year==2021),
                               ~ 0 + male + I(age-4) + I(year==2017) + I(year==2018) + I(year==2019) + I(year==2020) + I(year==2021),
                               ~ 0 + male + I(age-4) + I(year==2017) + I(year==2018) + I(year==2019) + I(year==2020) + I(year==2021)
                             ),
                             control = list(
                               info_if_converged = F,
                               logLik_if_converged = F

                             )
)
proc.time()-t1
