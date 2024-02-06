# flag51: 原則上，「留職停薪原因」與「借調類別」填寫應相符:1.	借調公民營事業機構留職停薪?借調至公民營事業機構 2.	借調行政法人機關留職停薪?借調至行政法人機關 3.借調法定實驗學校留職停薪?借調至法定實驗學校-------------------------------------------------------------------
flag_person <- drev_person_1

#「留職停薪原因」與「借調類別」不合理處
flag_person$err_lev <- 0
flag_person$err_lev <-
  if_else(
    flag_person$brtype == "借調至公民營事業機構"  &
      flag_person$levpay != "借調公民營事業機構留職停薪",
    1,
    flag_person$err_lev
  )
flag_person$err_lev <-
  if_else(
    flag_person$brtype == "借調至行政法人機關"  &
      flag_person$levpay != "借調行政法人機關留職停薪",
    1,
    flag_person$err_lev
  )
flag_person$err_lev <-
  if_else(
    flag_person$brtype == "借調至法定實驗學校"  &
      flag_person$levpay != "借調法定實驗學校留職停薪",
    1,
    flag_person$err_lev
  )

flag_person$err_lev <-
  if_else(
    flag_person$brtype != "借調至公民營事業機構"  &
      flag_person$levpay == "借調公民營事業機構留職停薪",
    1,
    flag_person$err_lev
  )
flag_person$err_lev <-
  if_else(
    flag_person$brtype != "借調至行政法人機關"  &
      flag_person$levpay == "借調行政法人機關留職停薪",
    1,
    flag_person$err_lev
  )
flag_person$err_lev <-
  if_else(
    flag_person$brtype != "借調至法定實驗學校"  &
      flag_person$levpay == "借調法定實驗學校留職停薪",
    1,
    flag_person$err_lev
  )

#呈現姓名
flag_person$err_flag_txt <- ""
flag_person$err_flag_txt <-
  case_when(flag_person$err_lev == 1 ~ flag_person$name,
            TRUE ~ flag_person$err_flag_txt)

if (dim(flag_person %>% subset(err_lev == 1))[1] != 0) {
  #根據organization_id + source，展開成寬資料(wide)
  flag_person_wide_flag51 <- flag_person %>%
    subset(select = c(
      organization_id,
      idnumber,
      err_flag_txt,
      edu_name2,
      source,
      err_lev
    )) %>%
    subset(err_lev == 1) %>%
    dcast(organization_id + source ~ err_flag_txt, value.var = "err_flag_txt")
  
  #合併所有name
  temp <-
    colnames(flag_person_wide_flag51)[3:length(colnames(flag_person_wide_flag51))]
  flag_person_wide_flag51$flag51_r <- NA
  for (i in temp) {
    flag_person_wide_flag51$flag51_r <-
      paste(flag_person_wide_flag51$flag51_r,
            flag_person_wide_flag51[[i]],
            sep = " ")
  }
  flag_person_wide_flag51$flag51_r <-
    gsub("NA ",
         replacement = "",
         flag_person_wide_flag51$flag51_r)
  flag_person_wide_flag51$flag51_r <-
    gsub(" NA",
         replacement = "",
         flag_person_wide_flag51$flag51_r)
  
  #產生檢誤報告文字
  flag51_temp <- flag_person_wide_flag51 %>%
    group_by(organization_id) %>%
    mutate(flag51_txt = paste(source, "需核對「留職停薪原因」與「借調類別」：", flag51_r, sep = ""),
           "") %>%
    subset(select = c(organization_id, flag51_txt)) %>%
    distinct(organization_id, flag51_txt)
  
  #根據organization_id，展開成寬資料(wide)
  flag51 <- flag51_temp %>%
    dcast(organization_id ~ flag51_txt, value.var = "flag51_txt")
  
  #合併教員資料表及職員(工)資料表報告文字
  temp <- colnames(flag51)[2:length(colnames(flag51))]
  flag51$flag51 <- NA
  for (i in temp) {
    flag51$flag51 <- paste(flag51$flag51, flag51[[i]], sep = "； ")
  }
  flag51$flag51 <- gsub("NA； ", replacement = "", flag51$flag51)
  flag51$flag51 <- gsub("； NA", replacement = "", flag51$flag51)
  
  #產生檢誤報告文字
  flag51 <- flag51 %>%
    subset(select = c(organization_id, flag51)) %>%
    distinct(organization_id, flag51) %>%
    mutate(flag51 = paste(flag51, "", sep = ""))
} else{
  #偵測flag51是否存在。若不存在，則產生NA行
  if ('flag51' %in% ls()) {
    print("flag51")
  } else{
    flag51 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    flag51$flag51 <- ""
  }
}
