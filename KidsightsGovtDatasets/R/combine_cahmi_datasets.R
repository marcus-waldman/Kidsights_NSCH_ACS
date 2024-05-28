combine_cahmi_datasets<-function(...){

  require(tidyverse)
  require(haven)
  require(pollster)

  ############### year, fipsst, stratum, hhid, fwc #####################

  cahmi_2016_2022 = list(extract_vars_cahmi(source = "2021-2022", cahmi_2021_2022),
                         extract_vars_cahmi(source="2019-2020", cahmi_2019_2020),
                         extract_vars_cahmi(source="2017-2018", cahmi_2017_2018),
                         extract_vars_cahmi(source="2016", cahmi_2016)) %>%
    dplyr::bind_rows() # Combined dataset


  ########  a1_grade: Caregiver 1 Educational Attainment #############
  #2021-2022
  tmp_2122 = extract_vars_cahmi(source = "2021-2022", cahmi_2021_2022, vars = "A1_GRADE") %>%
    dplyr::mutate(a1_grade = ifelse(a1_grade>=90, NA,a1_grade))

  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_202, vars = "a1_grade")

  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2017_2018, vars = "A1_GRADE") %>%
    dplyr::mutate(a1_grade = ifelse(a1_grade>=90, NA,a1_grade))

  # 2016:
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2016, vars = "A1_GRADE") %>%
    dplyr::mutate(a1_grade = ifelse(a1_grade>=90, NA,a1_grade))


  tmp_16_22 = list(tmp_2122, tmp_1920,tmp_1718,tmp_16) %>% dplyr::bind_rows()


  plt = weighted_bar(dat = tmp_16_22, var = "a1_grade"); print(plt)


  nrow(cahmi_2016_2022)==nrow(tmp_16_22)
  nrow(cahmi_2016_2022)
  cahmi_2016_2022 = cahmi_2016_2022 %>%
    left_join(tmp_16_22, by = c("year","fipsst","stratum","hhid", "fwc"))
  nrow(cahmi_2016_2022)
  rm(list = ls()[startsWith(ls(),"tmp")])


  ########  a2_grade: Caregiver 2 Educational Attainment #############
  #2021-2022
  tmp_2122 = extract_vars_cahmi(source = "2021-2022", cahmi_2021_2022, vars = "A2_GRADE") %>%
    dplyr::mutate(a2_grade = ifelse(a2_grade>=90, NA,a2_grade))

  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "a2_grade")
  #unique(tmp_1920$a2_grade)

  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "A2_GRADE")
  #unique(tmp_1718$a2_grade)
  tmp_1718 = tmp_1718 %>%
    dplyr::mutate(a2_grade = ifelse(a2_grade>=90, NA,a2_grade))

  # 2016:
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "A2_GRADE")
  #unique(tmp_16$a2_grade)


  tmp_16_22 = list(tmp_2122, tmp_1920,tmp_1718,tmp_16) %>% dplyr::bind_rows()


  plt = weighted_bar(dat = tmp_16_22, var = "a2_grade"); print(plt)


  nrow(cahmi_2016_2022)==nrow(tmp_16_22)
  nrow(cahmi_2016_2022)
  cahmi_2016_2022 = cahmi_2016_2022 %>%
    left_join(tmp_16_22, by = c("year","fipsst","stratum","hhid", "fwc"))
  nrow(cahmi_2016_2022)
  rm(list = ls()[startsWith(ls(),"tmp")])

  ###### adulteduc ##########
  #2021-2022
  tmp_2122 = extract_vars_cahmi(source = "2021-2022", cahmi_2021_2022, vars = "AdultEduc_2122") %>%
    dplyr::mutate(adulteduc = adulteduc_2122)  %>% dplyr::select(-adulteduc_2122)
  unique(tmp_2122$adulteduc)

  #2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "AdultEduc_1920") %>%
    dplyr::mutate(adulteduc = AdultEduc_1920) %>% dplyr::select(-AdultEduc_1920)
  unique(tmp_1920$adulteduc)

  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "AdultEduc_1718") %>%
    dplyr::mutate(adulteduc = adulteduc_1718) %>% dplyr::select(-adulteduc_1718)
  unique(tmp_1718$adulteduc)

  # 2016:
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "AdultEduc_16") %>%
    dplyr::mutate(adulteduc = adulteduc_16) %>% dplyr::select(-adulteduc_16)
  unique(tmp_16$adulteduc)


  tmp_16_22 = list(tmp_2122, tmp_1920,tmp_1718,tmp_16) %>% dplyr::bind_rows()
  tmp_16_22$adulteduc[tmp_16_22$adulteduc>=90] = NA

  plt = weighted_bar(dat = tmp_16_22, var = "adulteduc"); print(plt)



  nrow(cahmi_2016_2022)==nrow(tmp_16_22)
  nrow(cahmi_2016_2022)
  cahmi_2016_2022 = cahmi_2016_2022 %>%
    left_join(tmp_16_22, by = c("year","fipsst","stratum","hhid", "fwc"))
  nrow(cahmi_2016_2022)
  rm(list = ls()[startsWith(ls(),"tmp")])



  ############### Percent Above Poverty Line #####################
  #2021-22
  tmp_2122 = extract_vars_cahmi(source = "2021-2022", cahmi_2021_2022, vars = "FPL_I1")

  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, vars = "fpl_i1")

  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2017_2018, vars = "FPL_I1")

  # 2016:
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2016, vars = "FPL")
  tmp_16 = tmp_16 %>% dplyr::mutate(fpl_i1 = fpl) %>%
    dplyr::select(-all_of("fpl"))


  identical(names(tmp_2122), names(tmp_1920))
  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_22 = list(tmp_2122, tmp_1920,tmp_1718,tmp_16) %>% dplyr::bind_rows()


  plt <- weighted_ecdf(dat = tmp_16_22, var = "fpl_i1")
  print(plt)
  nrow(cahmi_2016_2022)==nrow(tmp_16_22)
  nrow(cahmi_2016_2022)
  cahmi_2016_2022 = cahmi_2016_2022 %>%
    left_join(tmp_16_22, by = c("year","fipsst","stratum","hhid", "fwc"))
  nrow(cahmi_2016_2022)
  rm(list = ls()[startsWith(ls(),"tmp")])


  ########  WIC Benefits - Past 12 Months #############
  # 2021-2022
  tmp_2122 = extract_vars_cahmi(source = "2021-2022", cahmi_2021_2022, vars = "WIC_2122") %>%
    dplyr::mutate(wic = wic_2122) %>%
    dplyr::select(-wic_2122)


  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "WIC_1920") %>%
    dplyr::mutate(wic = WIC_1920) %>%
    dplyr::select(-WIC_1920)


  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "WIC_1718") %>%
    dplyr::mutate(wic = wic_1718) %>%
    dplyr::select(-wic_1718)

  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "WIC_16") %>%
    dplyr::mutate(wic = wic_16) %>%
    dplyr::select(-wic_16)

  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_22 = list(tmp_2122,tmp_1920,tmp_1718,tmp_16) %>% dplyr::bind_rows()
  tmp_16_22 = tmp_16_22 %>% dplyr::mutate(wic = ifelse(wic==99,NA,wic))

  tmp_16_22 = tmp_16_22 %>%
    mutate(
      wic2 = zap_attributes(wic),
      wic = haven::labelled(wic2,
                            labels = c(Yes=1, No=2),
                            label = "Received benefits from the WIC Program at any time during the past 12 months")
    ) %>%
    dplyr::select(-wic2)


  plt = weighted_bar(tmp_16_22, var = "wic"); print(plt)

  nrow(cahmi_2016_2022)==nrow(tmp_16_22)
  nrow(cahmi_2016_2022)
  cahmi_2016_2022 = cahmi_2016_2022 %>%
    left_join(tmp_16_22, by = c("year","fipsst","stratum","hhid", "fwc"))
  nrow(cahmi_2016_2022)
  rm(list = ls()[startsWith(ls(),"tmp")])



  ########  Cash Assistance - Past 12 Months #############
  #2021-2022
  tmp_2122 = extract_vars_cahmi(source = "2021-2022", cahmi_2021_2022, vars = "CashAss_2122") %>%
    dplyr::mutate(cashass = cashass_2122) %>%
    dplyr::select(-cashass_2122)



  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "CashAss_1920") %>%
    dplyr::mutate(cashass = CashAss_1920) %>%
    dplyr::select(-CashAss_1920)


  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "CashAss_1718") %>%
    dplyr::mutate(cashass = cashass_1718) %>%
    dplyr::select(-cashass_1718)

  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "CashAss_16") %>%
    dplyr::mutate(cashass = cashass_16) %>%
    dplyr::select(-cashass_16)

  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_22 = list(tmp_2122,tmp_1920,tmp_1718,tmp_16) %>% dplyr::bind_rows()
  tmp_16_22 = tmp_16_22 %>% dplyr::mutate(cashass = ifelse(cashass==99,NA,cashass))


  tmp_16_22 = tmp_16_22 %>%
    dplyr::mutate(cashass2 = zap_attributes(cashass),
           cashass = haven::labelled(cashass2,
                                     labels = c(Yes=1,No = 2),
                                     label = "Received cash assistance from government at any time during the past 12 months")
    ) %>%
    dplyr::select(-cashass2)


  plt = weighted_bar(tmp_16_22, var = "cashass"); print(plt)


  nrow(cahmi_2016_2022)==nrow(tmp_16_22)
  nrow(cahmi_2016_2022)
  cahmi_2016_2022 = cahmi_2016_2022 %>%
    left_join(tmp_16_22, by = c("year","fipsst","stratum","hhid", "fwc"))
  nrow(cahmi_2016_2022)
  rm(list = ls()[startsWith(ls(),"tmp")])


  ########  Food Stamp or SNAP - Past 12 Months #############

  #2021-2022
  tmp_2122 = extract_vars_cahmi(source = "2021-2022", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "FoodStamp_2122") %>%
    dplyr::mutate(foodstamp = foodstamp_2122) %>%
    dplyr::select(-foodstamp_2122)

  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "FoodStamp_1920") %>%
    dplyr::mutate(foodstamp = FoodStamp_1920) %>%
    dplyr::select(-FoodStamp_1920)


  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "FoodStamp_1718") %>%
    dplyr::mutate(foodstamp = foodstamp_1718) %>%
    dplyr::select(-foodstamp_1718)

  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "FoodStamp_16") %>%
    dplyr::mutate(foodstamp = foodstamp_16) %>%
    dplyr::select(-foodstamp_16)

  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_22 = list(tmp_2122, tmp_1920,tmp_1718,tmp_16) %>% dplyr::bind_rows()
  tmp_16_22 = tmp_16_22 %>% dplyr::mutate(foodstamp = ifelse(foodstamp==99,NA,foodstamp))

  tmp_16_22 = tmp_16_22 %>%
    dplyr::mutate(foodstamp = zap_attributes(foodstamp),
           foodstamp = haven::labelled(foodstamp,
                                       labels = c(Yes=1,No = 2),
                                       label = "Received Food Stamps or Supplemental Nutrition Assistance Program benefits")
    )

  plt = weighted_bar(tmp_16_22, var = "foodstamp"); print(plt)

  nrow(cahmi_2016_2022)==nrow(tmp_16_22)
  nrow(cahmi_2016_2022)
  cahmi_2016_2022 = cahmi_2016_2022 %>%
    left_join(tmp_16_22, by = c("year","fipsst","stratum","hhid", "fwc"))
  nrow(cahmi_2016_2022)
  rm(list = ls()[startsWith(ls(),"tmp")])


  ########  Free or Reduced Price Breakfast or Lunch - Past 12 Months #############
  #2021-2022
  tmp_2122 = extract_vars_cahmi(source = "2021-2022", cahmi_2021_2022, vars = "MealFree_2122") %>%
    dplyr::mutate(mealfree = mealfree_2122) %>%
    dplyr::select(-mealfree_2122)

  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "MealFree_1920") %>%
    dplyr::mutate(mealfree = MealFree_1920) %>%
    dplyr::select(-MealFree_1920)


  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "MealFree_1718") %>%
    dplyr::mutate(mealfree = mealfree_1718) %>%
    dplyr::select(-mealfree_1718)

  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "MealFree_16") %>%
    dplyr::mutate(mealfree = mealfree_16) %>%
    dplyr::select(-mealfree_16)

  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_22 = list(tmp_2122, tmp_1920,tmp_1718,tmp_16) %>% dplyr::bind_rows()
  tmp_16_22 = tmp_16_22 %>% dplyr::mutate(mealfree = ifelse(mealfree==99,NA,mealfree))

  tmp_16_22 = tmp_16_22 %>%
    dplyr::mutate(mealfree2 = zap_attributes(mealfree),
           mealfree = haven::labelled(mealfree,
                                      labels = c(Yes=1,No = 2),
                                      label = "Received free or reduced-cost breakfasts or lunches at school")
    ) %>%
    dplyr::select(-mealfree2)

  plt  = weighted_bar(tmp_16_22, var = "mealfree"); print(plt)

  nrow(cahmi_2016_2022)==nrow(tmp_16_22)
  nrow(cahmi_2016_2022)
  cahmi_2016_2022 = cahmi_2016_2022 %>%
    left_join(tmp_16_22, by = c("year","fipsst","stratum","hhid", "fwc"))
  nrow(cahmi_2016_2022)
  rm(list = ls()[startsWith(ls(),"tmp")])

  ######## Public or government provided healthcare #####
  #2021-2022
  tmp_2122 = extract_vars_cahmi(source = "2021-2022", cahmi_2021_2022, vars = "K12Q12") %>%
    dplyr::mutate(govhealthc = k12q12) %>%
    dplyr::select(-k12q12)

  cahmi_2016$K12Q12 %>% unique()

  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "k12q12") %>%
    dplyr::mutate(govhealthc = k12q12) %>%
    dplyr::select(-k12q12)


  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "K12Q12") %>%
    dplyr::mutate(govhealthc = k12q12) %>%
    dplyr::select(-k12q12)

  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "K12Q12") %>%
    dplyr::mutate(govhealthc = k12q12) %>%
    dplyr::select(-k12q12)

  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_22 = list(tmp_2122, tmp_1920,tmp_1718,tmp_16) %>% dplyr::bind_rows()
  tmp_16_22 = tmp_16_22 %>% dplyr::mutate(govhealthc = ifelse(govhealthc>2,NA,govhealthc))


  tmp_16_22 = tmp_16_22 %>%
    dplyr::mutate(govhealthc2 = zap_attributes(govhealthc),
           govhealthc = haven::labelled(govhealthc2,
                                        labels = c(Yes=1,No = 2),
                                        label = "Government assisted health care")
    ) %>%
    dplyr::select(-govhealthc2)

  plt = weighted_bar(tmp_16_22, var = "govhealthc"); print(plt)

  nrow(cahmi_2016_2022)==nrow(tmp_16_22)
  nrow(cahmi_2016_2022)
  cahmi_2016_2022 = cahmi_2016_2022 %>%
    left_join(tmp_16_22, by = c("year","fipsst","stratum","hhid", "fwc"))
  nrow(cahmi_2016_2022)
  rm(list = ls()[startsWith(ls(),"tmp")])




  ########  foodcash (Derived): Indicator 6.27: Received food or cash assistance during the past 12 months #############
  cahmi_2016_2022 = cahmi_2016_2022 %>%
    dplyr::mutate(foodcash= as.integer((wic==1) + (cashass==1) + (foodstamp==1) + (mealfree==1))) %>%
    dplyr::mutate(foodcash = plyr::mapvalues(foodcash,from = c(0,1,2,3,4), to = c(3,2,2,1,1))) %>%
    dplyr::mutate(foodcash = zap_attributes(foodcash),
           foodcash = haven::labelled(foodcash,
                                      labels= c("3-4 types"=1,"1-2 types"= 2, "None"=3),
                                      label = "Received free or reduced-cost breakfasts or lunches at school")
    )

  plt = weighted_bar(cahmi_2016_2022 %>% dplyr::select(year, fwc, foodcash), var = "foodcash"); print(plt)



  ########  Adult 1 Relationship to Child #############
  #2021-2022
  tmp_2122 = extract_vars_cahmi(source = "2021-2022", cahmi_2021_2022, vars = "A1_RELATION")

  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "a1_relation")

  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "A1_RELATION") %>%
    dplyr::mutate(a1_relation = ifelse(a1_relation==5, 6, a1_relation) ) # THIS CATEGORY APPEARS IN THE DATA SET BUT NOT IN ANY VARIABLE LABELS, MUST BE CAHMI MISTAKE. Collapse `5 - Aunt or Uncle` with `6 - Other: Relative` becasue Aunt or Uncle not a category in this year


  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "A1_RELATION") %>%
    dplyr::mutate(a1_relation = ifelse(a1_relation==5, 6, a1_relation) ) # Collapse `5 - Aunt or Uncle` with `6 - Other: Relative` becasue Aunt or Uncle not a category in this year

  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_22 = list(tmp_2122,tmp_1920,tmp_1718,tmp_16) %>% dplyr::bind_rows()


  tmp_16_22 = tmp_16_22 %>%
    dplyr::mutate(a1_relation = ifelse(a1_relation==99,NA,a1_relation))  %>%
    dplyr::mutate(a1_relation = zap_attributes(a1_relation) %>%
             haven::labelled(labels = c("Biological or Adoptive Parent"=1, "Step-parent"=2, "Grandparent"=3, "Foster Parent"=4, "Other: Relative"=6, "Other: Non-Relative"=7),
                             label = "Adult 1 - How Related to Child")
    )


  plt = weighted_bar(tmp_16_22 %>% dplyr::select(year, fwc, a1_relation), var = "a1_relation"); print(plt)

  nrow(cahmi_2016_2022)==nrow(tmp_16_22)
  nrow(cahmi_2016_2022)
  cahmi_2016_2022 = cahmi_2016_2022 %>%
    left_join(tmp_16_22, by = c("year","fipsst","stratum","hhid", "fwc"))
  nrow(cahmi_2016_2022)
  rm(list = ls()[startsWith(ls(),"tmp")])


  ########  Adult 2 Relationship to Child #############
  #2021-2022
  tmp_2122 = extract_vars_cahmi(source = "2021-2022", cahmi_2021_2022, vars = "A2_RELATION")

  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "a2_relation")

  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "A2_RELATION") %>%
    dplyr::mutate(a2_relation = ifelse(a2_relation==5, 6, a2_relation) )

  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "A2_RELATION") %>%
    dplyr::mutate(a2_relation = ifelse(a2_relation==5, 6, a2_relation) )


  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_22 = list(tmp_2122,tmp_1920,tmp_1718,tmp_16) %>% dplyr::bind_rows()

  tmp_16_22 = tmp_16_22 %>%
    dplyr::mutate(a2_relation = ifelse(a2_relation==99 | a2_relation==8,NA,a2_relation))  %>% #Note that 8 is the child only has 1 caregiver. Set to missing
    dplyr::mutate(a2_relation = zap_attributes(a2_relation) %>%
             haven::labelled(labels = c("Biological or Adoptive Parent"=1, "Step-parent"=2, "Grandparent"=3, "Foster Parent"=4, "Other: Relative"=6, "Other: Non-Relative"=7),
                             label = "Adult 2 - How Related to Child")
    )

  plt = weighted_bar(tmp_16_22 %>% dplyr::select(year, fwc, a2_relation), var = "a2_relation"); print(plt)

  nrow(cahmi_2016_2022)==nrow(tmp_16_22)
  nrow(cahmi_2016_2022)
  cahmi_2016_2022 = cahmi_2016_2022 %>%
    left_join(tmp_16_22, by = c("year","fipsst","stratum","hhid", "fwc"))
  nrow(cahmi_2016_2022)
  rm(list = ls()[startsWith(ls(),"tmp")])


  ########  sex: What is this child's sex #############
  #2021-2022
  tmp_2122 = extract_vars_cahmi(source = "2021-2022", cahmi_2021_2022, vars = "sex_2122") %>%
    dplyr::mutate(sex = sex_2122) %>% dplyr::select(-sex_2122)

  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "sex_1920") %>%
    dplyr::mutate(sex = sex_1920) %>% dplyr::select(-sex_1920)

  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "sex_1718") %>%
    dplyr::mutate(sex = sex_1718) %>% dplyr::select(-sex_1718)

  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "sex_16") %>%
    dplyr::mutate(sex = sex_16) %>% dplyr::select(-sex_16)

  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_22 = list(tmp_2122,tmp_1920,tmp_1718,tmp_16) %>% dplyr::bind_rows()
  tmp_16_22 = tmp_16_22 %>% dplyr::mutate(sex = ifelse(sex==99,NA,sex))

  tmp_16_22 = tmp_16_22 %>%
    dplyr::mutate(sex = sex %>% zap_attributes() %>%
             haven::labelled(labels= c("Male"=1,"Female" = 2), label = "Child's sex")
    )

  plt = weighted_bar(tmp_16_22 %>% dplyr::select(year, fwc, sex), var = "sex"); print(plt)


  nrow(cahmi_2016_2022)==nrow(tmp_16_22)
  nrow(cahmi_2016_2022)
  cahmi_2016_2022 = cahmi_2016_2022 %>%
    left_join(tmp_16_22, by = c("year","fipsst","stratum","hhid", "fwc"))
  nrow(cahmi_2016_2022)
  rm(list = ls()[startsWith(ls(),"tmp")])


  ########  sc_hispanic_r: Hispanic Origin of Selected Child, Recode #############
  #2021-2022
  tmp_2122 = extract_vars_cahmi(source = "2021-2022", cahmi_2021_2022, vars = "SC_HISPANIC_R") %>%
    dplyr::mutate(sc_hispanic_r = ifelse(sc_hispanic_r>=90,NA,sc_hispanic_r))

  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "sc_hispanic_r")

  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "SC_HISPANIC_R") %>%
    dplyr::mutate(sc_hispanic_r = ifelse(sc_hispanic_r>=90,NA,sc_hispanic_r))

  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "SC_HISPANIC_R")

  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_22 = list(tmp_2122,tmp_1920,tmp_1718,tmp_16) %>% dplyr::bind_rows()

  tmp_16_22 = tmp_16_22 %>%
    dplyr::mutate(sc_hispanic_r =
             sc_hispanic_r %>% zap_attributes() %>%
             haven::labelled(labels = c("Hispanic or Latino Origin"=1, "Not Hispanic or Latino Origin"=2),
                             label = "Hispanic Origin of Selected Child")
    )


  plt = weighted_bar(tmp_16_22 %>% dplyr::select(year, fwc, sc_hispanic_r), var = "sc_hispanic_r"); print(plt)

  nrow(cahmi_2016_2022)==nrow(tmp_16_22)
  nrow(cahmi_2016_2022)
  cahmi_2016_2022 = cahmi_2016_2022 %>%
    left_join(tmp_16_22, by = c("year","fipsst","stratum","hhid", "fwc"))
  nrow(cahmi_2016_2022)
  rm(list = ls()[startsWith(ls(),"tmp")])

  ########  race4: Race and ethnicity distribution of the child population #############
  #2021-2022
  tmp_2122 = extract_vars_cahmi(source = "2021-2022", cahmi_2021_2022, vars = "race4_2122")  %>%
    dplyr::mutate(race4 = race4_2122) %>% dplyr::select(-race4_2122)

  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "race4_1920") %>%
    dplyr::mutate(race4 = race4_1920) %>% dplyr::select(-race4_1920)

  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "race4_1718")  %>%
    dplyr::mutate(race4 = race4_1718) %>% dplyr::select(-race4_1718)

  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "race4_16") %>%
    dplyr::mutate(race4 = race4_16) %>% dplyr::select(-race4_16)


  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_22 = list(tmp_2122,tmp_1920,tmp_1718,tmp_16) %>% dplyr::bind_rows()
  tmp_16_22$race4[tmp_16_22$race4==99]=NA

  tmp_16_22 = tmp_16_22 %>%
    dplyr::mutate(race4 = race4 %>% zap_attributes() %>%
             haven::labelled(label = "Race and ethnicity distribution of the child population",
                             labels = c("Hispanic"=1, "White, non-Hispanic"=2, "Black, non-Hispanic"=3, "Other/Multi-racial, non-Hispanic"=4))
    )

  plt = weighted_bar(tmp_16_22 %>% dplyr::select(year, fwc, race4), var = "race4"); print(plt)

  nrow(cahmi_2016_2022)==nrow(tmp_16_22)
  nrow(cahmi_2016_2022)
  cahmi_2016_2022 = cahmi_2016_2022 %>%
    left_join(tmp_16_22, by = c("year","fipsst","stratum","hhid", "fwc"))
  nrow(cahmi_2016_2022)
  rm(list = ls()[startsWith(ls(),"tmp")])


  ########  rxmeds: Children qualifying on the CSHCN Screener prescription medication criteria #############
  #2021-2022
  tmp_2122 = extract_vars_cahmi(source = "2021-2022", cahmi_2021_2022, vars = "SC_K2Q10")  %>%  dplyr::mutate(scrxmeds = sc_k2q10) %>% dplyr::select(-sc_k2q10)
  unique(tmp_2122$scrxmeds)
  tmp_2122$scrxmeds[tmp_2122$scrxmeds>=90] = NA

  unique(cahmi_2019_2020$sc_k2q10)
  table(cahmi_2019_2020$sc_k2q10, useNA = "always")

  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "sc_k2q10") %>% dplyr::mutate(scrxmeds = sc_k2q10) %>% dplyr::select(-sc_k2q10)
  unique(tmp_1920$scrxmeds)


  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "SC_K2Q10")  %>%  dplyr::mutate(scrxmeds = sc_k2q10) %>% dplyr::select(-sc_k2q10)
  unique(tmp_1718$scrxmeds)
  tmp_1718$scrxmeds[tmp_1718$scrxmeds>=90] = NA


  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "SC_K2Q10")  %>%  dplyr::mutate(scrxmeds = sc_k2q10) %>% dplyr::select(-sc_k2q10)
  unique(tmp_16$scrxmeds)


  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_22 = list(tmp_2122,tmp_1920,tmp_1718,tmp_16) %>% dplyr::bind_rows()


  with(tmp_16_22, unique(scrxmeds))
  tmp_16_22 = tmp_16_22 %>%
    dplyr::mutate(scrxmeds = scrxmeds %>% zap_attributes() %>%
             haven::labelled(label = "SC Needs or Uses Medication Currently",
                             labels = c("Yes"=1,"No"=2))
    )


  plt = weighted_bar(tmp_16_22 %>% dplyr::select(year, fwc, scrxmeds), var = "scrxmeds"); print(plt)

  nrow(cahmi_2016_2022)==nrow(tmp_16_22)
  nrow(cahmi_2016_2022)
  cahmi_2016_2022 = cahmi_2016_2022 %>%
    left_join(tmp_16_22, by = c("year","fipsst","stratum","hhid", "fwc"))
  nrow(cahmi_2016_2022)
  rm(list = ls()[startsWith(ls(),"tmp")])

  ########  scserve:  SC Needs or Uses More Medical Care than Others#############
  unique(cahmi_2021_2022$SC_K2Q13)
  table(cahmi_2021_2022$SC_K2Q13, useNA = "always")
  #2021-2022
  tmp_2122 = extract_vars_cahmi(source = "2021-2022", cahmi_2021_2022, vars = "SC_K2Q13")  %>%  dplyr::mutate(scserve = sc_k2q13) %>% dplyr::select(-sc_k2q13)
  unique(tmp_2122$scserve)
  tmp_2122$scserve[tmp_2122$scserve>=90] = NA

  unique(cahmi_2019_2020$sc_k2q13)
  table(cahmi_2019_2020$sc_k2q13, useNA = "always")

  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "sc_k2q13") %>% dplyr::mutate(scserve = sc_k2q13) %>% dplyr::select(-sc_k2q13)
  unique(tmp_1920$scserve)


  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "SC_K2Q13")  %>%  dplyr::mutate(scserve = sc_k2q13) %>% dplyr::select(-sc_k2q13)
  unique(tmp_1718$scserve)
  tmp_1718$scserve[tmp_1718$scserve>=90] = NA

  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "SC_K2Q13")  %>%  dplyr::mutate(scserve = sc_k2q13) %>% dplyr::select(-sc_k2q13)
  unique(tmp_16$scserve)

  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_22 = list(tmp_2122,tmp_1920,tmp_1718,tmp_16) %>% dplyr::bind_rows()
  with(tmp_16_22, table(year,scserve))

  with(tmp_16_22, unique(scserve))
  tmp_16_22 = tmp_16_22 %>%
    dplyr::mutate(scserve = scserve %>% zap_attributes() %>%
             haven::labelled(label = "SC Needs or Uses More Medical Care than Other",
                             labels = c("Yes"=1,"No"=2))
    )

  plt = weighted_bar(tmp_16_22 %>% dplyr::select(year, fwc, scserve), var = "scserve"); print(plt)


  nrow(cahmi_2016_2022)==nrow(tmp_16_22)
  nrow(cahmi_2016_2022)
  cahmi_2016_2022 = cahmi_2016_2022 %>%
    left_join(tmp_16_22, by = c("year","fipsst","stratum","hhid", "fwc"))
  nrow(cahmi_2016_2022)
  rm(list = ls()[startsWith(ls(),"tmp")])


  ########  scfunc: SC Limited Ability  #############
  unique(cahmi_2021_2022$SC_K2Q16)
  table(cahmi_2021_2022$SC_K2Q16, useNA = "always")
  #2021-2022
  tmp_2122 = extract_vars_cahmi(source = "2021-2022", cahmi_2021_2022, vars = "SC_K2Q16")  %>%  dplyr::mutate(scfunc = sc_k2q16) %>% dplyr::select(-sc_k2q16)
  unique(tmp_2122$scfunc)
  tmp_2122$scfunc[tmp_2122$scfunc>=90] = NA

  unique(cahmi_2019_2020$sc_k2q16)
  table(cahmi_2019_2020$sc_k2q16, useNA = "always")

  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "sc_k2q16") %>% dplyr::mutate(scfunc = sc_k2q16) %>% dplyr::select(-sc_k2q16)
  unique(tmp_1920$scfunc)


  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "SC_K2Q16")  %>%  dplyr::mutate(scfunc = sc_k2q16) %>% dplyr::select(-sc_k2q16)
  unique(tmp_1718$scfunc)
  tmp_1718$scfunc[tmp_1718$scfunc>=90] = NA

  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "SC_K2Q16")  %>%  dplyr::mutate(scfunc = sc_k2q16) %>% dplyr::select(-sc_k2q16)
  unique(tmp_16$scfunc)

  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_22 = list(tmp_2122,tmp_1920,tmp_1718,tmp_16) %>% dplyr::bind_rows()
  with(tmp_16_22, table(year,scfunc))




  with(tmp_16_22, unique(scfunc))
  tmp_16_22 = tmp_16_22 %>%
    dplyr::mutate(scfunc = scfunc %>% zap_attributes() %>%
             haven::labelled(label = "SC Limited Ability Currently",
                             labels = c("Yes"=1,"No"=2))
    )

  plt = weighted_bar(tmp_16_22 %>% dplyr::select(year, fwc, scfunc), var = "scfunc"); print(plt)



  nrow(cahmi_2016_2022)==nrow(tmp_16_22)
  nrow(cahmi_2016_2022)
  cahmi_2016_2022 = cahmi_2016_2022 %>%
    left_join(tmp_16_22, by = c("year","fipsst","stratum","hhid", "fwc"))
  nrow(cahmi_2016_2022)
  rm(list = ls()[startsWith(ls(),"tmp")])


  ########  sctherapy:  #############
  unique(cahmi_2021_2022$SC_K2Q19)
  table(cahmi_2021_2022$SC_K2Q19, useNA = "always")
  #2021-2022
  tmp_2122 = extract_vars_cahmi(source = "2021-2022", cahmi_2021_2022, vars = "SC_K2Q19")  %>%  dplyr::mutate(sctherapy = sc_k2q19) %>% dplyr::select(-sc_k2q19)
  unique(tmp_2122$sctherapy)
  tmp_2122$sctherapy[tmp_2122$sctherapy>=90] = NA

  unique(cahmi_2019_2020$sc_k2q19)
  table(cahmi_2019_2020$sc_k2q19, useNA = "always")

  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "sc_k2q19") %>% dplyr::mutate(sctherapy = sc_k2q19) %>% dplyr::select(-sc_k2q19)
  unique(tmp_1920$sctherapy)


  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "SC_K2Q19")  %>%  dplyr::mutate(sctherapy = sc_k2q19) %>% dplyr::select(-sc_k2q19)
  unique(tmp_1718$sctherapy)
  tmp_1718$sctherapy[tmp_1718$sctherapy>=90] = NA

  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "SC_K2Q19")  %>%  dplyr::mutate(sctherapy = sc_k2q19) %>% dplyr::select(-sc_k2q19)
  unique(tmp_16$sctherapy)


  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_22 = list(tmp_2122,tmp_1920,tmp_1718,tmp_16) %>% dplyr::bind_rows()
  with(tmp_16_22, table(year,sctherapy))

  with(tmp_16_22, unique(sctherapy))
  tmp_16_22 = tmp_16_22 %>%
    dplyr::mutate(sctherapy = sctherapy %>% zap_attributes() %>%
             haven::labelled(label = "SC Special Therapy",
                             labels = c("Yes"=1,"No"=2))
    )
  with(tmp_16_22, unique(sctherapy))

  plt = weighted_bar(tmp_16_22 %>% dplyr::select(year, fwc, sctherapy), var = "sctherapy"); print(plt)


  nrow(cahmi_2016_2022)==nrow(tmp_16_22)
  nrow(cahmi_2016_2022)
  cahmi_2016_2022 = cahmi_2016_2022 %>%
    left_join(tmp_16_22, by = c("year","fipsst","stratum","hhid", "fwc"))
  nrow(cahmi_2016_2022)
  rm(list = ls()[startsWith(ls(),"tmp")])


  ########  scmhealth:  #############
  unique(cahmi_2021_2022$SC_K2Q22)
  table(cahmi_2021_2022$SC_K2Q22, useNA = "always")
  #2021-2022
  tmp_2122 = extract_vars_cahmi(source = "2021-2022", cahmi_2021_2022, vars = "SC_K2Q22")  %>%  dplyr::mutate(scmhealth = sc_k2q22) %>% dplyr::select(-sc_k2q22)
  unique(tmp_2122$scmhealth)
  tmp_2122$scmhealth[tmp_2122$scmhealth>=90] = NA

  unique(cahmi_2019_2020$sc_k2q22)
  table(cahmi_2019_2020$sc_k2q22, useNA = "always")

  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "sc_k2q22") %>% dplyr::mutate(scmhealth = sc_k2q22) %>% dplyr::select(-sc_k2q22)
  unique(tmp_1920$scmhealth)


  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "SC_K2Q22")  %>%  dplyr::mutate(scmhealth = sc_k2q22) %>% dplyr::select(-sc_k2q22)
  unique(tmp_1718$scmhealth)
  tmp_1718$scmhealth[tmp_1718$scmhealth>=90] = NA

  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "SC_K2Q22")  %>%  dplyr::mutate(scmhealth = sc_k2q22) %>% dplyr::select(-sc_k2q22)
  unique(tmp_16$scmhealth)


  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_22 = list(tmp_2122,tmp_1920,tmp_1718,tmp_16) %>% dplyr::bind_rows()
  with(tmp_16_22, table(year,scmhealth))


  with(tmp_16_22, unique(scmhealth))
  tmp_16_22 = tmp_16_22 %>%
    dplyr::mutate(scmhealth = scmhealth %>% zap_attributes() %>%
             haven::labelled(label = "SC Emotion Develop Behave Treatment",
                             labels = c("Yes"=1,"No"=2))
    )
  with(tmp_16_22, unique(scmhealth))

  plt = weighted_bar(tmp_16_22 %>% dplyr::select(year, fwc, scmhealth), var = "scmhealth"); print(plt)


  nrow(cahmi_2016_2022)==nrow(tmp_16_22)
  nrow(cahmi_2016_2022)
  cahmi_2016_2022 = cahmi_2016_2022 %>%
    left_join(tmp_16_22, by = c("year","fipsst","stratum","hhid", "fwc"))
  nrow(cahmi_2016_2022)
  rm(list = ls()[startsWith(ls(),"tmp")])


  ########  Indicator 1.11 Children with speical needs care #############
  unique(cahmi_2021_2022$CSHCN_2122)
  table(cahmi_2021_2022$CSHCN_2122, useNA = "always")
  #2021-2022
  tmp_2122 = extract_vars_cahmi(source = "2021-2022", cahmi_2021_2022, vars = "CSHCN_2122") %>% dplyr::mutate(cshcn = cshcn_2122) %>% dplyr::select(-cshcn_2122)
  unique(tmp_2122$cshcn)

  unique(cahmi_2019_2020$CSHCN_1920)
  table(cahmi_2019_2020$CSHCN_1920, useNA = "always")

  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "CSHCN_1920") %>% dplyr::mutate(cshcn = CSHCN_1920) %>% dplyr::select(-CSHCN_1920)
  unique(tmp_1920$cshcn)


  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "CSHCN_1718") %>% dplyr::mutate(cshcn = cshcn_1718) %>% dplyr::select(-cshcn_1718)
  unique(tmp_1718$cshcn)

  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "CSHCN_16")  %>%  dplyr::mutate(cshcn = cshcn_16) %>% dplyr::select(-cshcn_16)
  unique(tmp_16$cshcn)


  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_22 = list(tmp_2122,tmp_1920,tmp_1718,tmp_16) %>% dplyr::bind_rows()
  with(tmp_16_22, table(year,cshcn, useNA = "always"))


  with(tmp_16_22, unique(cshcn, useNA = "always"))
  tmp_16_22 = tmp_16_22 %>%
    dplyr::mutate(cshcn = cshcn %>% zap_attributes() %>%
             haven::labelled(label = "Indicator 1.11: Children with special health care needs",
                             labels = c("Meet screening criteria"=1,"Do not meet screening criteria"=2))
    )
  with(tmp_16_22, unique(cshcn))
  with(tmp_16_22, table(year,cshcn, useNA = "always"))

  plt = weighted_bar(tmp_16_22 %>% dplyr::select(year, fwc, cshcn), var = "cshcn"); print(plt)


  nrow(cahmi_2016_2022)==nrow(tmp_16_22)
  nrow(cahmi_2016_2022)
  cahmi_2016_2022 = cahmi_2016_2022 %>%
    left_join(tmp_16_22, by = c("year","fipsst","stratum","hhid", "fwc"))
  nrow(cahmi_2016_2022)
  rm(list = ls()[startsWith(ls(),"tmp")])


  ########  acedivorce: Divorced or separated  #############
  unique(cahmi_2021_2022$ACE3)
  table(cahmi_2021_2022$ACE3, useNA = "always")

  #2021-2022
  tmp_2122 = extract_vars_cahmi(source = "2021-2022", cahmi_2021_2022, vars = "ACE3")  %>%  dplyr::mutate(acedivorce = ace3) %>% dplyr::select(-ace3)
  unique(tmp_2122$acedivorce)
  tmp_2122$acedivorce[tmp_2122$acedivorce>=90] = NA

  unique(cahmi_2019_2020$ace3)
  table(cahmi_2019_2020$ace3, useNA = "always")

  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "ace3") %>% dplyr::mutate(acedivorce = ace3) %>% dplyr::select(-ace3)
  unique(tmp_1920$acedivorce)


  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "ACE3")  %>%  dplyr::mutate(acedivorce = ace3) %>% dplyr::select(-ace3)
  unique(tmp_1718$acedivorce)
  tmp_1718$acedivorce[tmp_1718$acedivorce>=90] = NA

  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "ACE3")  %>%  dplyr::mutate(acedivorce = ace3) %>% dplyr::select(-ace3)
  unique(tmp_16$acedivorce)


  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_22 = list(tmp_2122,tmp_1920,tmp_1718,tmp_16) %>% dplyr::bind_rows()
  with(tmp_16_22, table(year,acedivorce))

  with(tmp_16_22, unique(acedivorce))
  tmp_16_22 = tmp_16_22 %>%
    dplyr::mutate(acedivorce = acedivorce %>% zap_attributes() %>%
             haven::labelled(label = "Child Experienced - Parent or Guardian Divorced",
                             labels = c("Yes"=1,"No"=2))
    )
  with(tmp_16_22, unique(acedivorce))

  plt = weighted_bar(tmp_16_22 %>% dplyr::select(year, fwc, acedivorce), var = "acedivorce"); print(plt)


  nrow(cahmi_2016_2022)==nrow(tmp_16_22)
  nrow(cahmi_2016_2022)
  cahmi_2016_2022 = cahmi_2016_2022 %>%
    left_join(tmp_16_22, by = c("year","fipsst","stratum","hhid", "fwc"))
  nrow(cahmi_2016_2022)
  rm(list = ls()[startsWith(ls(),"tmp")])


  ########  acedeath: Parent or guardian died  #############
  unique(cahmi_2021_2022$ACE4)
  table(cahmi_2021_2022$ACE4, useNA = "always")
  #2021-2022
  tmp_2122 = extract_vars_cahmi(source = "2021-2022", cahmi_2021_2022, vars = "ACE4")  %>%  dplyr::mutate(acedeath = ace4) %>% dplyr::select(-ace4)
  unique(tmp_2122$acedeath)
  tmp_2122$acedeath[tmp_2122$acedeath>=90] = NA

  unique(cahmi_2019_2020$ace4)
  table(cahmi_2019_2020$ace4, useNA = "always")

  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "ace4") %>% dplyr::mutate(acedeath = ace4) %>% dplyr::select(-ace4)
  unique(tmp_1920$acedeath)


  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "ACE4")  %>%  dplyr::mutate(acedeath = ace4) %>% dplyr::select(-ace4)
  unique(tmp_1718$acedeath)
  tmp_1718$acedeath[tmp_1718$acedeath>=90] = NA

  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "ACE4")  %>%  dplyr::mutate(acedeath = ace4) %>% dplyr::select(-ace4)
  unique(tmp_16$acedeath)


  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_22 = list(tmp_2122,tmp_1920,tmp_1718,tmp_16) %>% dplyr::bind_rows()
  with(tmp_16_22, table(year,acedeath))


  with(tmp_16_22, unique(acedeath))
  tmp_16_22 = tmp_16_22 %>%
    dplyr::mutate(acedeath = acedeath %>% zap_attributes() %>%
             haven::labelled(label = "Child Experienced - Parent or Guardian Died",
                             labels = c("Yes"=1,"No"=2))
    )
  with(tmp_16_22, unique(acedeath))


  plt = weighted_bar(tmp_16_22 %>% dplyr::select(year, fwc, acedeath), var = "acedeath"); print(plt)


  nrow(cahmi_2016_2022)==nrow(tmp_16_22)
  nrow(cahmi_2016_2022)
  cahmi_2016_2022 = cahmi_2016_2022 %>%
    left_join(tmp_16_22, by = c("year","fipsst","stratum","hhid", "fwc"))
  nrow(cahmi_2016_2022)
  rm(list = ls()[startsWith(ls(),"tmp")])



  ########  acejail: Parent or guardian in jail  #############
  unique(cahmi_2021_2022$ACE5)
  table(cahmi_2021_2022$ACE5, useNA = "always")
  #2021-2022
  tmp_2122 = extract_vars_cahmi(source = "2021-2022", cahmi_2021_2022, vars = "ACE5")  %>%  dplyr::mutate(acejail = ace5) %>% dplyr::select(-ace5)
  unique(tmp_2122$acejail)
  tmp_2122$acejail[tmp_2122$acejail>=90] = NA

  unique(cahmi_2019_2020$ace5)
  table(cahmi_2019_2020$ace5, useNA = "always")

  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "ace5") %>% dplyr::mutate(acejail = ace5) %>% dplyr::select(-ace5)
  unique(tmp_1920$acejail)


  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "ACE5")  %>%  dplyr::mutate(acejail = ace5) %>% dplyr::select(-ace5)
  unique(tmp_1718$acejail)
  tmp_1718$acejail[tmp_1718$acejail>=90] = NA

  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "ACE5")  %>%  dplyr::mutate(acejail = ace5) %>% dplyr::select(-ace5)
  unique(tmp_16$acejail)



  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_22 = list(tmp_2122,tmp_1920,tmp_1718,tmp_16) %>% dplyr::bind_rows()
  with(tmp_16_22, table(year,acejail))

  with(tmp_16_22, unique(acejail))
  tmp_16_22 = tmp_16_22 %>%
    dplyr::mutate(acejail = acejail %>% zap_attributes() %>%
             haven::labelled(label = "Child Experienced - Parent or Guardian Time in Jail",
                             labels = c("Yes"=1,"No"=2))
    )
  with(tmp_16_22, unique(acejail))

  plt = weighted_bar(tmp_16_22 %>% dplyr::select(year, fwc, acejail), var = "acejail"); print(plt)


  nrow(cahmi_2016_2022)==nrow(tmp_16_22)
  nrow(cahmi_2016_2022)
  cahmi_2016_2022 = cahmi_2016_2022 %>%
    left_join(tmp_16_22, by = c("year","fipsst","stratum","hhid", "fwc"))
  nrow(cahmi_2016_2022)
  rm(list = ls()[startsWith(ls(),"tmp")])

  ########  acedv: Saw or herd parents or adults slap, hit, kick, punch one aother  #############
  unique(cahmi_2021_2022$ACE6)
  table(cahmi_2021_2022$ACE6, useNA = "always")
  #2021-2022
  tmp_2122 = extract_vars_cahmi(source = "2021-2022", cahmi_2021_2022, vars = "ACE6")  %>%  dplyr::mutate(acedv = ace6) %>% dplyr::select(-ace6)
  unique(tmp_2122$acedv)
  tmp_2122$acedv[tmp_2122$acedv>=90] = NA

  unique(cahmi_2019_2020$ace6)
  table(cahmi_2019_2020$ace6, useNA = "always")

  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "ace6") %>% dplyr::mutate(acedv = ace6) %>% dplyr::select(-ace6)
  unique(tmp_1920$acedv)


  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "ACE6")  %>%  dplyr::mutate(acedv = ace6) %>% dplyr::select(-ace6)
  unique(tmp_1718$acedv)
  tmp_1718$acedv[tmp_1718$acedv>=90] = NA

  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "ACE6")  %>%  dplyr::mutate(acedv = ace6) %>% dplyr::select(-ace6)
  unique(tmp_16$acedv)

  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_22 = list(tmp_2122,tmp_1920,tmp_1718,tmp_16) %>% dplyr::bind_rows()
  with(tmp_16_22, table(year,acedv))

  with(tmp_16_22, unique(acedv))
  tmp_16_22 = tmp_16_22 %>%
    dplyr::mutate(acedv = acedv %>% zap_attributes() %>%
             haven::labelled(label = "Child Experienced - Adults Slap, Hit, Kick, Punch Others",
                             labels = c("Yes"=1,"No"=2))
    )
  with(tmp_16_22, unique(acedv))

  plt = weighted_bar(tmp_16_22 %>% dplyr::select(year, fwc, acedv), var = "acedv"); print(plt)


  nrow(cahmi_2016_2022)==nrow(tmp_16_22)
  nrow(cahmi_2016_2022)
  cahmi_2016_2022 = cahmi_2016_2022 %>%
    left_join(tmp_16_22, by = c("year","fipsst","stratum","hhid", "fwc"))
  nrow(cahmi_2016_2022)
  rm(list = ls()[startsWith(ls(),"tmp")])


  ########  aceviolence: Was a victim or witnessed violence  #############
  unique(cahmi_2021_2022$ACE7)
  table(cahmi_2021_2022$ACE7, useNA = "always")
  #2021-2022
  tmp_2122 = extract_vars_cahmi(source = "2021-2022", cahmi_2021_2022, vars = "ACE7")  %>%  dplyr::mutate(aceviolence = ace7) %>% dplyr::select(-ace7)
  unique(tmp_2122$aceviolence)
  tmp_2122$aceviolence[tmp_2122$aceviolence>=90] = NA

  unique(cahmi_2019_2020$ace7)
  table(cahmi_2019_2020$ace7, useNA = "always")

  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "ace7") %>% dplyr::mutate(aceviolence = ace7) %>% dplyr::select(-ace7)
  unique(tmp_1920$aceviolence)


  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "ACE7")  %>%  dplyr::mutate(aceviolence = ace7) %>% dplyr::select(-ace7)
  unique(tmp_1718$aceviolence)
  tmp_1718$aceviolence[tmp_1718$aceviolence>=90] = NA

  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "ACE7")  %>%  dplyr::mutate(aceviolence = ace7) %>% dplyr::select(-ace7)
  unique(tmp_16$aceviolence)


  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_22 = list(tmp_2122,tmp_1920,tmp_1718,tmp_16) %>% dplyr::bind_rows()
  with(tmp_16_22, table(year,aceviolence))


  with(tmp_16_22, unique(aceviolence))
  tmp_16_22 = tmp_16_22 %>%
    dplyr::mutate(aceviolence = aceviolence %>% zap_attributes() %>%
             haven::labelled(label = "Child Experienced - Victim of Violence",
                             labels = c("Yes"=1,"No"=2))
    )
  with(tmp_16_22, unique(aceviolence))


  plt = weighted_bar(tmp_16_22 %>% dplyr::select(year, fwc, aceviolence), var = "aceviolence"); print(plt)


  nrow(cahmi_2016_2022)==nrow(tmp_16_22)
  nrow(cahmi_2016_2022)
  cahmi_2016_2022 = cahmi_2016_2022 %>%
    left_join(tmp_16_22, by = c("year","fipsst","stratum","hhid", "fwc"))
  nrow(cahmi_2016_2022)
  rm(list = ls()[startsWith(ls(),"tmp")])

  ########  acementalill: Lived with anyone mentally ill, suicidal, or severly depressed  #############
  unique(cahmi_2021_2022$ACE8)
  table(cahmi_2021_2022$ACE8, useNA = "always")
  #2021-2022
  tmp_2122 = extract_vars_cahmi(source = "2021-2022", cahmi_2021_2022, vars = "ACE8")  %>%  dplyr::mutate(acementalill = ace8) %>% dplyr::select(-ace8)
  unique(tmp_2122$acementalill)
  tmp_2122$acementalill[tmp_2122$acementalill>=90] = NA

  unique(cahmi_2019_2020$ace8)
  table(cahmi_2019_2020$ace8, useNA = "always")

  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "ace8") %>% dplyr::mutate(acementalill = ace8) %>% dplyr::select(-ace8)
  unique(tmp_1920$acementalill)


  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "ACE8")  %>%  dplyr::mutate(acementalill = ace8) %>% dplyr::select(-ace8)
  unique(tmp_1718$acementalill)
  tmp_1718$acementalill[tmp_1718$acementalill>=90] = NA

  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "ACE8")  %>%  dplyr::mutate(acementalill = ace8) %>% dplyr::select(-ace8)
  unique(tmp_16$acementalill)


  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_22 = list(tmp_2122,tmp_1920,tmp_1718,tmp_16) %>% dplyr::bind_rows()
  with(tmp_16_22, table(year,acementalill))

  with(tmp_16_22, unique(acementalill))
  tmp_16_22 = tmp_16_22 %>%
    dplyr::mutate(acementalill = acementalill %>% zap_attributes() %>%
             haven::labelled(label = "Child Experienced - Lived with Mentally Ill",
                             labels = c("Yes"=1,"No"=2))
    )
  with(tmp_16_22, unique(acementalill))

  plt = weighted_bar(tmp_16_22 %>% dplyr::select(year, fwc, acementalill), var = "acementalill"); print(plt)


  nrow(cahmi_2016_2022)==nrow(tmp_16_22)
  nrow(cahmi_2016_2022)
  cahmi_2016_2022 = cahmi_2016_2022 %>%
    left_join(tmp_16_22, by = c("year","fipsst","stratum","hhid", "fwc"))
  nrow(cahmi_2016_2022)
  rm(list = ls()[startsWith(ls(),"tmp")])


  ########  aceaddict: #############
  unique(cahmi_2021_2022$ACE9)
  table(cahmi_2021_2022$ACE9, useNA = "always")

  #2021-2022
  tmp_2122 = extract_vars_cahmi(source = "2021-2022", cahmi_2021_2022, vars = "ACE9")  %>%  dplyr::mutate(aceaddict = ace9) %>% dplyr::select(-ace9)
  unique(tmp_2122$aceaddict)
  tmp_2122$aceaddict[tmp_2122$aceaddict>=90] = NA
  #
  unique(cahmi_2019_2020$ace9)
  table(cahmi_2019_2020$ace9, useNA = "always")

  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "ace9") %>% dplyr::mutate(aceaddict = ace9) %>% dplyr::select(-ace9)
  unique(tmp_1920$aceaddict)

  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "ACE9")  %>%  dplyr::mutate(aceaddict = ace9) %>% dplyr::select(-ace9)
  unique(tmp_1718$aceaddict)
  tmp_1718$aceaddict[tmp_1718$aceaddict>=90] = NA

  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "ACE9")  %>%  dplyr::mutate(aceaddict = ace9) %>% dplyr::select(-ace9)
  unique(tmp_16$aceaddict)


  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_22 = list(tmp_2122,tmp_1920,tmp_1718,tmp_16) %>% dplyr::bind_rows()
  with(tmp_16_22, table(year,aceaddict))

  with(tmp_16_22, unique(aceaddict))
  tmp_16_22 = tmp_16_22 %>%
    dplyr::mutate(aceaddict = aceaddict %>% zap_attributes() %>%
             haven::labelled(label = "Child Experienced - Lived with Person with Alcohol/Drug Problem",
                             labels = c("Yes"=1,"No"=2))
    )
  with(tmp_16_22, unique(aceaddict))

  plt = weighted_bar(tmp_16_22 %>% dplyr::select(year, fwc, aceaddict), var = "aceaddict"); print(plt)


  nrow(cahmi_2016_2022)==nrow(tmp_16_22)
  nrow(cahmi_2016_2022)
  cahmi_2016_2022 = cahmi_2016_2022 %>%
    left_join(tmp_16_22, by = c("year","fipsst","stratum","hhid", "fwc"))
  nrow(cahmi_2016_2022)
  rm(list = ls()[startsWith(ls(),"tmp")])

  ########  aceracism: Treated Unfairly because of his or her race or ethnic group #############
  unique(cahmi_2021_2022$ACE10)
  table(cahmi_2021_2022$ACE10, useNA = "always")
  #2021-2022
  tmp_2122 = extract_vars_cahmi(source = "2021-2022", cahmi_2021_2022, vars = "ACE10")  %>%  dplyr::mutate(aceracism = ace10) %>% dplyr::select(-ace10)
  unique(tmp_2122$aceracism)
  tmp_2122$aceracism[tmp_2122$aceracism>=90] = NA

  unique(cahmi_2019_2020$ace10)
  table(cahmi_2019_2020$ace10, useNA = "always")

  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "ace10") %>% dplyr::mutate(aceracism = ace10) %>% dplyr::select(-ace10)
  unique(tmp_1920$aceracism)


  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "ACE10")  %>%  dplyr::mutate(aceracism = ace10) %>% dplyr::select(-ace10)
  unique(tmp_1718$aceracism)
  tmp_1718$aceracism[tmp_1718$aceracism>=90] = NA

  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "ACE10")  %>%  dplyr::mutate(aceracism = ace10) %>% dplyr::select(-ace10)
  unique(tmp_16$aceracism)


  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_22 = list(tmp_2122,tmp_1920,tmp_1718,tmp_16) %>% dplyr::bind_rows()
  with(tmp_16_22, table(year,aceracism))

  with(tmp_16_22, unique(aceracism))
  tmp_16_22 = tmp_16_22 %>%
    dplyr::mutate(aceracism = aceracism %>% zap_attributes() %>%
             haven::labelled(label = "Child Experienced - Treated Unfairly Because of Race",
                             labels = c("Yes"=1,"No"=2))
    )
  with(tmp_16_22, unique(aceracism))

  plt = weighted_bar(tmp_16_22 %>% dplyr::select(year, fwc, aceracism), var = "aceracism"); print(plt)


  nrow(cahmi_2016_2022)==nrow(tmp_16_22)
  nrow(cahmi_2016_2022)
  cahmi_2016_2022 = cahmi_2016_2022 %>%
    left_join(tmp_16_22, by = c("year","fipsst","stratum","hhid", "fwc"))
  nrow(cahmi_2016_2022)
  rm(list = ls()[startsWith(ls(),"tmp")])


  ########  a1_sex:  #############
  #2021-2022
  tmp_2122 = extract_vars_cahmi(source = "2021-2022", cahmi_2021_2022, vars = "A1_SEX")
  unique(tmp_2122$a1_sex)
  tmp_2122$a1_sex[tmp_2122$a1_sex>=90] = NA

  unique(cahmi_2019_2020$a1_sex)
  table(cahmi_2019_2020$a1_sex, useNA = "always")

  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "a1_sex")
  unique(tmp_1920$a1_sex)


  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "A1_SEX")
  unique(tmp_1718$a1_sex)
  tmp_1718$a1_sex[tmp_1718$a1_sex>=90] = NA

  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "A1_SEX")
  unique(tmp_16$a1_sex)


  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_22 = list(tmp_2122,tmp_1920,tmp_1718,tmp_16) %>% dplyr::bind_rows()
  with(tmp_16_22, table(year,a1_sex))

  with(tmp_16_22, unique(a1_sex))
  tmp_16_22 = tmp_16_22 %>%
    dplyr::mutate(a1_sex = a1_sex %>% zap_attributes() %>%
             haven::labelled(label = "Adult 1 - Sex",
                             labels = c("Male"=1,"Female"=2))
    )
  with(tmp_16_22, unique(a1_sex))


  plt = weighted_bar(tmp_16_22 %>% dplyr::select(year, fwc, a1_sex), var = "a1_sex"); print(plt)


  nrow(cahmi_2016_2022)==nrow(tmp_16_22)
  nrow(cahmi_2016_2022)
  cahmi_2016_2022 = cahmi_2016_2022 %>%
    left_join(tmp_16_22, by = c("year","fipsst","stratum","hhid", "fwc"))
  nrow(cahmi_2016_2022)
  rm(list = ls()[startsWith(ls(),"tmp")])


  ########  a2_sex:  #############
  #2021-2022
  tmp_2122 = extract_vars_cahmi(source = "2021-2022", cahmi_2021_2022, vars = "A2_SEX")
  unique(tmp_2122$a2_sex)
  tmp_2122$a2_sex[tmp_2122$a2_sex>=90] = NA

  unique(cahmi_2019_2020$a2_sex)
  table(cahmi_2019_2020$a2_sex, useNA = "always")

  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "a2_sex")
  unique(tmp_1920$a2_sex)


  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "A2_SEX")
  unique(tmp_1718$a2_sex)
  tmp_1718$a2_sex[tmp_1718$a2_sex>=90] = NA

  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "A2_SEX")
  unique(tmp_16$a2_sex)


  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_22 = list(tmp_2122,tmp_1920,tmp_1718,tmp_16) %>% dplyr::bind_rows()
  with(tmp_16_22, table(year,a2_sex))


  with(tmp_16_22, unique(a2_sex))
  tmp_16_22 = tmp_16_22 %>%
    dplyr::mutate(a2_sex = a2_sex %>% zap_attributes() %>%
             haven::labelled(label = "Adult 2 - Sex",
                             labels = c("Male"=1,"Female"=2))
    )
  with(tmp_16_22, unique(a2_sex))

  plt = weighted_bar(tmp_16_22 %>% dplyr::select(year, fwc, a2_sex), var = "a2_sex"); print(plt)

  nrow(cahmi_2016_2022)==nrow(tmp_16_22)
  nrow(cahmi_2016_2022)
  cahmi_2016_2022 = cahmi_2016_2022 %>%
    left_join(tmp_16_22, by = c("year","fipsst","stratum","hhid", "fwc"))
  nrow(cahmi_2016_2022)
  rm(list = ls()[startsWith(ls(),"tmp")])

  ########  a1_age:  #############
  #2021-2022
  tmp_2122 = extract_vars_cahmi(source = "2021-2022", cahmi_2021_2022, vars = "A1_AGE")
  unique(tmp_2122$a1_age)
  max(tmp_2122$a1_age,na.rm=T)
  tmp_2122$a1_age[tmp_2122$a1_age>=90] = NA

  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "a1_age")
  unique(tmp_1920$a1_age)
  max(tmp_1920$a1_age,na.rm=T)

  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "A1_AGE")
  unique(tmp_1718$a1_age)
  max(tmp_1718$a1_age,na.rm=T)
  tmp_1718$a1_age[tmp_1718$a1_age>=90] = NA

  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "A1_AGE")
  unique(tmp_16$a1_age)
  with(tmp_16, sort(unique(a1_age)))
  with(tmp_16, table(a1_age))


  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_22 = list(tmp_2122,tmp_1920,tmp_1718,tmp_16) %>% dplyr::bind_rows()
  with(tmp_16_22, table(year,a1_age))

  with(tmp_16_22, unique(a1_age))
  tmp_16_22 = tmp_16_22 %>%
    dplyr::mutate(a1_age = a1_age %>% zap_attributes() %>%
                    haven::labelled(labels = c("75 or older" = 75),
                                    label = "Adult 1 - Age in Years"))
  with(tmp_16_22, unique(a1_age))

  weighted_ecdf(dat=tmp_16_22, var = "a1_age")

  nrow(cahmi_2016_2022)==nrow(tmp_16_22)
  nrow(cahmi_2016_2022)
  cahmi_2016_2022 = cahmi_2016_2022 %>%
    left_join(tmp_16_22, by = c("year","fipsst","stratum","hhid", "fwc"))
  nrow(cahmi_2016_2022)
  rm(list = ls()[startsWith(ls(),"tmp")])


  ########  a2_age:  #############
  #2021-2022
  tmp_2122 = extract_vars_cahmi(source = "2021-2022", cahmi_2021_2022, vars = "A2_AGE")
  unique(tmp_2122$a2_age)
  tmp_2122$a2_age[tmp_2122$a2_age>=90] = NA

  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "a2_age")
  unique(tmp_1920$a2_age)


  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "A2_AGE")
  unique(tmp_1718$a2_age)
  tmp_1718$a2_age[tmp_1718$a2_age>=90] = NA

  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "A2_AGE")
  unique(tmp_16$a2_age)


  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_22 = list(tmp_2122,tmp_1920,tmp_1718,tmp_16) %>% dplyr::bind_rows()
  with(tmp_16_22, table(year,a2_age))

  with(tmp_16_22, unique(a2_age))
  tmp_16_22 = tmp_16_22 %>%
    dplyr::mutate(a2_age = a2_age %>% zap_attributes() %>%
                    haven::labelled(labels = c("75 or older" = 75),
                                    label = "Adult 2 - Age in Years"))
  with(tmp_16_22, unique(a2_age))


  plt = weighted_ecdf(tmp_16_22 %>% dplyr::select(year, fwc, a2_age), var = "a2_age"); print(plt)

  nrow(cahmi_2016_2022)==nrow(tmp_16_22)
  nrow(cahmi_2016_2022)
  cahmi_2016_2022 = cahmi_2016_2022 %>%
    left_join(tmp_16_22, by = c("year","fipsst","stratum","hhid", "fwc"))
  nrow(cahmi_2016_2022)
  rm(list = ls()[startsWith(ls(),"tmp")])


  ########  a1_physhealth:  #############
  #2021-2022
  tmp_2122 = extract_vars_cahmi(source = "2021-2022", cahmi_2021_2022, vars = "A1_PHYSHEALTH")
  unique(tmp_2122$a1_physhealth)
  tmp_2122$a1_physhealth[tmp_2122$a1_physhealth>=90] = NA

  unique(cahmi_2019_2020$a1_physhealth)
  table(cahmi_2019_2020$a1_physhealth, useNA = "always")

  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "a1_physhealth")
  unique(tmp_1920$a1_physhealth)


  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "A1_PHYSHEALTH")
  unique(tmp_1718$a1_physhealth)
  tmp_1718$a1_physhealth[tmp_1718$a1_physhealth>=90] = NA

  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "A1_PHYSHEALTH")
  unique(tmp_16$a1_physhealth)

  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_22 = list(tmp_2122,tmp_1920,tmp_1718,tmp_16) %>% dplyr::bind_rows()
  with(tmp_16_22, table(year,a1_physhealth))


  unique(tmp_16_22$a1_physhealth)
  tmp_16_22 = tmp_16_22 %>%
    dplyr::mutate(a1_physhealth = a1_physhealth %>% zap_attributes() %>%
                    haven::labelled(labels = c("Excellent"=1, "Very Good"=2, "Good"=3, "Fair"=4, "Poor"=5),
                                    label = "Adult 1 - Physical Health"))
  with(tmp_16_22, unique(a1_physhealth))


  plt = weighted_bar(tmp_16_22 %>% dplyr::select(year, fwc, a1_physhealth), var = "a1_physhealth"); print(plt)


  nrow(cahmi_2016_2022)==nrow(tmp_16_22)
  nrow(cahmi_2016_2022)
  cahmi_2016_2022 = cahmi_2016_2022 %>%
    left_join(tmp_16_22, by = c("year","fipsst","stratum","hhid", "fwc"))
  nrow(cahmi_2016_2022)
  rm(list = ls()[startsWith(ls(),"tmp")])


  ########  a2_physhealth:  #############
  #2021-2022
  tmp_2122 = extract_vars_cahmi(source = "2021-2022", cahmi_2021_2022, vars = "A2_PHYSHEALTH")
  unique(tmp_2122$a2_physhealth)
  tmp_2122$a2_physhealth[tmp_2122$a2_physhealth>=90] = NA

  unique(cahmi_2019_2020$a2_physhealth)
  table(cahmi_2019_2020$a2_physhealth, useNA = "always")

  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "a2_physhealth")
  unique(tmp_1920$a2_physhealth)


  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "A2_PHYSHEALTH")
  unique(tmp_1718$a2_physhealth)
  tmp_1718$a2_physhealth[tmp_1718$a2_physhealth>=90] = NA

  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "A2_PHYSHEALTH")
  unique(tmp_16$a2_physhealth)


  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_22 = list(tmp_2122,tmp_1920,tmp_1718,tmp_16) %>% dplyr::bind_rows()
  with(tmp_16_22, table(year,a2_physhealth))

  unique(tmp_16_22$a2_physhealth)
  tmp_16_22 = tmp_16_22 %>%
    dplyr::mutate(a2_physhealth = a2_physhealth %>% zap_attributes() %>%
                    haven::labelled(labels = c("Excellent"=1, "Very Good"=2, "Good"=3, "Fair"=4, "Poor"=5),
                                    label = "Adult 2 - Physical Health"))
  with(tmp_16_22, unique(a2_physhealth))

  plt = weighted_bar(tmp_16_22 %>% dplyr::select(year, fwc, a2_physhealth), var = "a2_physhealth"); print(plt)


  nrow(cahmi_2016_2022)==nrow(tmp_16_22)
  nrow(cahmi_2016_2022)
  cahmi_2016_2022 = cahmi_2016_2022 %>%
    left_join(tmp_16_22, by = c("year","fipsst","stratum","hhid", "fwc"))
  nrow(cahmi_2016_2022)
  rm(list = ls()[startsWith(ls(),"tmp")])


  ########  a1_menthealth:  #############

  #2021-2022
  tmp_2122 = extract_vars_cahmi(source = "2021-2022", cahmi_2021_2022, vars = "A1_MENTHEALTH")
  unique(tmp_2122$a1_menthealth)
  tmp_2122$a1_menthealth[tmp_2122$a1_menthealth>=90] = NA

  unique(cahmi_2019_2020$a1_menthealth)
  table(cahmi_2019_2020$a1_menthealth, useNA = "always")

  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "a1_menthealth")
  unique(tmp_1920$a1_menthealth)


  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "A1_MENTHEALTH")
  unique(tmp_1718$a1_menthealth)
  tmp_1718$a1_menthealth[tmp_1718$a1_menthealth>=90] = NA

  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "A1_MENTHEALTH")
  unique(tmp_16$a1_menthealth)


  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_22 = list(tmp_2122,tmp_1920,tmp_1718,tmp_16) %>% dplyr::bind_rows()
  with(tmp_16_22, table(year,a1_menthealth))


  unique(tmp_16_22$a1_menthealth)
  tmp_16_22 = tmp_16_22 %>%
    dplyr::mutate(a1_menthealth = a1_menthealth %>% zap_attributes() %>%
                    haven::labelled(labels = c("Excellent"=1, "Very Good"=2, "Good"=3, "Fair"=4, "Poor"=5),
                                    label = "Adult 1 - Mental Health"))
  with(tmp_16_22, unique(a1_menthealth))

  plt = weighted_bar(tmp_16_22 %>% dplyr::select(year, fwc, a1_menthealth), var = "a1_menthealth"); print(plt)


  nrow(cahmi_2016_2022)==nrow(tmp_16_22)
  nrow(cahmi_2016_2022)
  cahmi_2016_2022 = cahmi_2016_2022 %>%
    left_join(tmp_16_22, by = c("year","fipsst","stratum","hhid", "fwc"))
  nrow(cahmi_2016_2022)
  rm(list = ls()[startsWith(ls(),"tmp")])

  ########  a2_menthealth  #############
  #2021-2022
  tmp_2122 = extract_vars_cahmi(source = "2021-2022", cahmi_2021_2022, vars = "A2_MENTHEALTH")
  unique(tmp_2122$a2_menthealth)
  tmp_2122$a2_menthealth[tmp_2122$a2_menthealth>=90] = NA

  unique(cahmi_2019_2020$a2_menthealth)
  table(cahmi_2019_2020$a2_menthealth, useNA = "always")

  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "a2_menthealth")
  unique(tmp_1920$a2_menthealth)


  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "A2_MENTHEALTH")
  unique(tmp_1718$a2_menthealth)
  tmp_1718$a2_menthealth[tmp_1718$a2_menthealth>=90] = NA

  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "A2_MENTHEALTH")
  unique(tmp_16$a2_menthealth)


  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_22 = list(tmp_2122,tmp_1920,tmp_1718,tmp_16) %>% dplyr::bind_rows()
  with(tmp_16_22, table(year,a2_menthealth))


  unique(tmp_16_22$a2_menthealth)
  tmp_16_22 = tmp_16_22 %>%
    dplyr::mutate(a2_menthealth = a2_menthealth %>% zap_attributes() %>%
                    haven::labelled(labels = c("Excellent"=1, "Very Good"=2, "Good"=3, "Fair"=4, "Poor"=5),
                                    label = "Adult 1 - Mental Health"))
  with(tmp_16_22, unique(a2_menthealth))


  plt = weighted_bar(tmp_16_22 %>% dplyr::select(year, fwc, a2_menthealth), var = "a2_menthealth"); print(plt)

  nrow(cahmi_2016_2022)==nrow(tmp_16_22)
  nrow(cahmi_2016_2022)
  cahmi_2016_2022 = cahmi_2016_2022 %>%
    left_join(tmp_16_22, by = c("year","fipsst","stratum","hhid", "fwc"))
  nrow(cahmi_2016_2022)
  rm(list = ls()[startsWith(ls(),"tmp")])

  ########  a1_marital  #############
  #2021-2022
  tmp_2122 = extract_vars_cahmi(source = "2021-2022", cahmi_2021_2022, vars = "A1_MARITAL")
  unique(tmp_2122$a1_marital)
  tmp_2122$a1_marital[tmp_2122$a1_marital>=90] = NA

  unique(cahmi_2019_2020$a1_marital)
  table(cahmi_2019_2020$a1_marital, useNA = "always")

  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "a1_marital")
  unique(tmp_1920$a1_marital)


  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "A1_MARITAL")
  unique(tmp_1718$a1_marital)
  tmp_1718$a1_marital[tmp_1718$a1_marital>=90] = NA

  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "A1_MARITAL")
  unique(tmp_16$a1_marital)


  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_22 = list(tmp_2122,tmp_1920,tmp_1718,tmp_16) %>% dplyr::bind_rows()
  with(tmp_16_22, table(year,a1_marital, useNA = "always"))

  with(tmp_16_22, unique(a1_marital))
  tmp_16_22 = tmp_16_22 %>%
    dplyr::mutate(a1_marital = a1_marital %>% zap_attributes() %>%
                    haven::labelled(labels = c("Married"=1, "Not married, but living with a partner"=2, "Never Married"=3, "Divorced"=4, "Separated"=5, "Widowed"=6),
                                    label = "Adult 1 - Marital Status"))
  with(tmp_16_22, unique(a1_marital))

  plt = weighted_bar(tmp_16_22 %>% dplyr::select(year, fwc, a1_marital), var = "a1_marital"); print(plt)


  nrow(cahmi_2016_2022)==nrow(tmp_16_22)
  nrow(cahmi_2016_2022)
  cahmi_2016_2022 = cahmi_2016_2022 %>%
    left_join(tmp_16_22, by = c("year","fipsst","stratum","hhid", "fwc"))
  nrow(cahmi_2016_2022)
  rm(list = ls()[startsWith(ls(),"tmp")])


  ########  a2_marital  #############
  #2021-2022
  tmp_2122 = extract_vars_cahmi(source = "2021-2022", cahmi_2021_2022, vars = "A2_MARITAL")
  unique(tmp_2122$a2_marital)
  tmp_2122$a2_marital[tmp_2122$a2_marital>=90] = NA

  unique(cahmi_2019_2020$a2_marital)
  table(cahmi_2019_2020$a2_marital, useNA = "always")

  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "a2_marital")
  unique(tmp_1920$a2_marital)


  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "A2_MARITAL")
  unique(tmp_1718$a2_marital)
  tmp_1718$a2_marital[tmp_1718$a2_marital>=90] = NA

  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "A2_MARITAL")
  unique(tmp_16$a2_marital)

  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_22 = list(tmp_2122,tmp_1920,tmp_1718,tmp_16) %>% dplyr::bind_rows()
  with(tmp_16_22, table(year,a2_marital, useNA = "always"))

  with(tmp_16_22, unique(a2_marital))
  tmp_16_22 = tmp_16_22 %>%
    dplyr::mutate(a2_marital = a2_marital %>% zap_attributes() %>%
                    haven::labelled(labels = c("Married"=1, "Not married, but living with a partner"=2, "Never Married"=3, "Divorced"=4, "Separated"=5, "Widowed"=6),
                                    label = "Adult 2 - Marital Status"))
  with(tmp_16_22, unique(a2_marital))

  plt = weighted_bar(tmp_16_22 %>% dplyr::select(year, fwc, a2_marital), var = "a2_marital"); print(plt)


  nrow(cahmi_2016_2022)==nrow(tmp_16_22)
  nrow(cahmi_2016_2022)
  cahmi_2016_2022 = cahmi_2016_2022 %>%
    left_join(tmp_16_22, by = c("year","fipsst","stratum","hhid", "fwc"))
  nrow(cahmi_2016_2022)>
    rm(list = ls()[startsWith(ls(),"tmp")])


  ########  sc_age_years: Child's age in years  #############
  #2021-2022
  tmp_2122 = extract_vars_cahmi(source = "2021-2022", cahmi_2021_2022, vars = "SC_AGE_YEARS")
  unique(tmp_2122$sc_age_years)
  tmp_2122$sc_age_years[tmp_2122$sc_age_years>=90] = NA

  unique(cahmi_2019_2020$sc_age_years)
  table(cahmi_2019_2020$sc_age_years, useNA = "always")

  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "sc_age_years")
  unique(tmp_1920$sc_age_years)


  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "SC_AGE_YEARS")
  unique(tmp_1718$sc_age_years)
  tmp_1718$sc_age_years[tmp_1718$sc_age_years>=90] = NA

  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "SC_AGE_YEARS")
  unique(tmp_16$sc_age_years)


  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_22 = list(tmp_2122,tmp_1920,tmp_1718,tmp_16) %>% dplyr::bind_rows()
  with(tmp_16_22, table(year,sc_age_years))


  with(tmp_16_22, unique(sc_age_years))
  tmp_16_22 = tmp_16_22 %>%
    dplyr::mutate(sc_age_years = sc_age_years %>% zap_attributes() %>%
                    haven::labelled(label = "Child's Age in Years"))
  with(tmp_16_22, unique(sc_age_years))

  plt = weighted_ecdf(tmp_16_22 %>% dplyr::select(year, fwc, sc_age_years), var = "sc_age_years"); print(plt)


  nrow(cahmi_2016_2022)==nrow(tmp_16_22)
  nrow(cahmi_2016_2022)
  cahmi_2016_2022 = cahmi_2016_2022 %>%
    left_join(tmp_16_22, by = c("year","fipsst","stratum","hhid", "fwc"))
  nrow(cahmi_2016_2022)
  rm(list = ls()[startsWith(ls(),"tmp")])


  ########  care10hrs: Indicator 6.21: Received care from others at least 10 hours/week, age 0-5  #############
  #2021-2022
  tmp_2122 = extract_vars_cahmi(source = "2021-2022", cahmi_2021_2022, vars = "K6Q20") %>%
    dplyr::mutate(care10hrs = k6q20) %>% dplyr::select(-k6q20)
  unique(tmp_2122$care10hrs)
  tmp_2122$care10hrs[tmp_2122$care10hrs>=90] = NA

  unique(cahmi_2019_2020$k6q20)
  table(cahmi_2019_2020$k6q20, useNA = "always")

  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "k6q20") %>%
    dplyr::mutate(care10hrs = k6q20) %>% dplyr::select(-k6q20)
  unique(tmp_1920$care10hrs)


  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "K6Q20") %>%
    dplyr::mutate(care10hrs = k6q20) %>% dplyr::select(-k6q20)
  unique(tmp_1718$care10hrs)
  tmp_1718$care10hrs[tmp_1718$care10hrs>=90] = NA

  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "K6Q20") %>%
    dplyr::mutate(care10hrs = k6q20) %>% dplyr::select(-k6q20)
  unique(tmp_16$care10hrs)


  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_22 = list(tmp_2122,tmp_1920,tmp_1718,tmp_16) %>% dplyr::bind_rows()
  with(tmp_16_22, table(year,care10hrs))


  with(tmp_16_22, unique(care10hrs))
  tmp_16_22 = tmp_16_22 %>%
    dplyr::mutate(care10hrs = care10hrs %>% zap_attributes() %>%
                    haven::labelled(labels = c("Yes"=1, "No"=2),
                                    label = "Receive Care From Others at Least 10 Hours Per Week"))
  with(tmp_16_22, unique(care10hrs))


  plt = weighted_bar(tmp_16_22 %>% dplyr::select(year, fwc, care10hrs), var = "care10hrs"); print(plt)


  nrow(cahmi_2016_2022)==nrow(tmp_16_22)
  nrow(cahmi_2016_2022)
  cahmi_2016_2022 = cahmi_2016_2022 %>%
    left_join(tmp_16_22, by = c("year","fipsst","stratum","hhid", "fwc"))
  nrow(cahmi_2016_2022)
  rm(list = ls()[startsWith(ls(),"tmp")])
  with(cahmi_2016_2022, table(sc_age_years,care10hrs, useNA="always"))


  ########## chhealth: Child's General Health #####
  #2021-2022
  tmp_2122 = extract_vars_cahmi(source = "2021-2022", cahmi_2021_2022, vars = "K2Q01") %>%
    dplyr::mutate(chhealth = k2q01) %>% dplyr::select(-k2q01)
  unique(tmp_2122$chhealth)
  tmp_2122$chhealth[tmp_2122$chhealth>=90] = NA

  unique(cahmi_2019_2020$k2q01)
  table(cahmi_2019_2020$k2q01, useNA = "always")

  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "k2q01") %>%
    dplyr::mutate(chhealth = k2q01) %>% dplyr::select(-k2q01)
  unique(tmp_1920$chhealth)


  #2017-2018
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "K2Q01") %>%
    dplyr::mutate(chhealth = k2q01) %>% dplyr::select(-k2q01)
  unique(tmp_1718$chhealth)
  tmp_1718$chhealth[tmp_1718$chhealth>=90] = NA

  # 2016
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "K2Q01") %>%
    dplyr::mutate(chhealth = k2q01) %>% dplyr::select(-k2q01)
  unique(tmp_16$chhealth)


  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_22 = list(tmp_2122,tmp_1920,tmp_1718,tmp_16) %>% dplyr::bind_rows()
  with(tmp_16_22, table(year,chhealth))


  with(tmp_16_22, unique(chhealth))
  tmp_16_22 = tmp_16_22 %>%
    dplyr::mutate(chhealth = chhealth %>% zap_attributes() %>%
                    haven::labelled(labels = c("Excellent"=1, "Very Good"=2, "Good"=3, "Fair"=4, "Poor"=5),
                                    label = "Child's General Health"))
  with(tmp_16_22, unique(chhealth))

  plt = weighted_bar(tmp_16_22 %>% dplyr::select(year, fwc, chhealth), var = "chhealth"); print(plt)


  nrow(cahmi_2016_2022)==nrow(tmp_16_22)
  nrow(cahmi_2016_2022)
  cahmi_2016_2022 = cahmi_2016_2022 %>%
    left_join(tmp_16_22, by = c("year","fipsst","stratum","hhid", "fwc"))
  nrow(cahmi_2016_2022)
  rm(list = ls()[startsWith(ls(),"tmp")])


  # ###### (Derived) scalewgt: Scaled child weight #####
  tmp_agg = cahmi_2016_2022 %>% dplyr::group_by(year) %>% dplyr::summarise(fwc_bar = mean(fwc))
  cahmi_2016_2022 = cahmi_2016_2022 %>%
    dplyr::left_join(tmp_agg, by = "year")  %>%
    dplyr::mutate(fwc_bar = fwc_bar %>% haven::labelled(label = "Average NSCH Interview Child Weights for Given Year.")) %>%
    dplyr::mutate(log2s_fwc = log(fwc/fwc_bar,2)) %>%
    dplyr::mutate(log2s_fwc = log2s_fwc %>%
             zap_attributes() %>%
             haven::labelled(label = "Log_2 of 2016-2020 Normalized NSCH Interview Child Weights"))

  rm(tmp_agg)

#### NEED TO UPDATE!!! ###
  # ######## Metropolitan status #####
  # #2021-2022
  # unique(cahmi_2021_2022$METRO_YN, useNA = "always")
  # tmp_2122 = extract_vars_cahmi(source = "2021-2022", cahmi_2021_2022, vars = "METRO_YN") %>%
  #   dplyr::mutate(rural = ifelse(metro_yn==98,NA, as.integer(metro_yn==2))) %>% dplyr::select(-metro_yn)
  # unique(tmp_2122$rural)
  # table(tmp_2122$rural, useNA = "always")
  #
  #
  # # 2019-2020
  # tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "metro_yn") %>%
  #   dplyr::mutate(rural = as.integer(metro_yn==2)) %>% dplyr::select(-metro_yn)
  # unique(tmp_1920$rural)
  #
  #
  # #2017-2018
  # unique(cahmi_2017_2018$METRO_YN, useNA = "always")
  # tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "METRO_YN") %>%
  #   dplyr::mutate(rural = ifelse(metro_yn==98,NA, as.integer(metro_yn==2))) %>% dplyr::select(-metro_yn)
  # unique(tmp_1718$rural)
  # table(tmp_1718$rural, useNA = "always")
  #
  #
  # # 2016
  # unique(cahmi_2016$METRO_YN, useNA = "always")
  # tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "METRO_YN") %>%
  #   dplyr::mutate(rural = as.integer(metro_yn==2)) %>% dplyr::select(-metro_yn)
  # unique(tmp_16$rural)
  #
  #
  # identical(names(tmp_1920),names(tmp_1718))
  # identical(names(tmp_1920),names(tmp_16))
  # tmp_16_22 = list(tmp_2122,tmp_1920,tmp_1718,tmp_16) %>% dplyr::bind_rows()
  # with(tmp_16_22, table(year,rural, useNA = "always"))
  #
  #
  # with(tmp_16_22, unique(rural))
  # tmp_16_22 = tmp_16_22 %>%
  #   dplyr::mutate(rural = rural %>% zap_attributes() %>%
  #                   haven::labelled(labels = c("In Metropolitan Statistical Area" =0, "Not In Metropolitan Statistical Area "=1),
  #                                   label = "Rural Status (derived; based on Metropolitan Statistical Area Status)"))
  # with(tmp_16_22, unique(rural))
  #
  # nrow(cahmi_2016_2022)==nrow(tmp_16_22)
  # nrow(cahmi_2016_2022)
  # cahmi_2016_2022 = cahmi_2016_2022 %>%
  #   left_join(tmp_16_22, by = c("year","fipsst","stratum","hhid", "fwc"))
  # nrow(cahmi_2016_2022)
  # rm(list = ls()[startsWith(ls(),"tmp")])

  ######## Number of family members #####
  #2021-2022
  unique(cahmi_2021_2022$FAMCOUNT, useNA = "always")
  tmp_2122 = extract_vars_cahmi(source = "2021-2022", cahmi_2021_2022, vars = "FAMCOUNT")
  unique(tmp_2122$famcount)
  table(tmp_2122$famcount, useNA = "always")
  tmp_2122$famcount[tmp_2122$famcount>=90] = NA

  unique(cahmi_2019_2020$famcount)
  table(cahmi_2019_2020$famcount, useNA = "always")

  # 2019-2020
  tmp_1920 = extract_vars_cahmi(source = "2019-2020", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "famcount")
  unique(tmp_1920$famcount)


  #2017-2018
  unique(cahmi_2017_2018$FAMCOUNT, useNA = "always")
  tmp_1718 = extract_vars_cahmi(source = "2017-2018", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "FAMCOUNT")
  unique(tmp_1718$famcount)
  table(tmp_1718$famcount, useNA = "always")
  tmp_1718$famcount[tmp_1718$famcount>=90] = NA


  # 2016
  unique(cahmi_2016$FAMCOUNT, useNA = "always")
  tmp_16 = extract_vars_cahmi(source = "2016", cahmi_2019_2020, cahmi_2017_2018, cahmi_2016, vars = "FAMCOUNT")
  unique(tmp_16$famcount)


  identical(names(tmp_1920),names(tmp_1718))
  identical(names(tmp_1920),names(tmp_16))
  tmp_16_22 = list(tmp_2122,tmp_1920,tmp_1718,tmp_16) %>% dplyr::bind_rows()
  with(tmp_16_22, table(year,famcount, useNA = "always"))


  with(tmp_16_22, unique(famcount))
  tmp_16_22 = tmp_16_22 %>%
    dplyr::mutate(famcount = famcount %>% zap_attributes() %>%
                    haven::labelled(labels = c("8 or more"=8),
                                    label = "Number of family members in household"))
  with(tmp_16_22, unique(famcount))

  plt = weighted_bar(tmp_16_22 %>% dplyr::select(year, fwc, famcount), var = "famcount"); print(plt)



  nrow(cahmi_2016_2022)==nrow(tmp_16_22)
  nrow(cahmi_2016_2022)
  cahmi_2016_2022 = cahmi_2016_2022 %>%
    left_join(tmp_16_22, by = c("year","fipsst","stratum","hhid", "fwc"))
  nrow(cahmi_2016_2022)
  rm(list = ls()[startsWith(ls(),"tmp")])



####### Additional variables:
  #Parental nativity (PrntNativity_2122)


  return(cahmi_2016_2022)
}

