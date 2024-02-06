# flag52: 留職停薪原因若填寫「應徵入伍留職停薪」、「奉派協助友邦留職停薪」、「育嬰留職停薪」、「侍親留職停薪」、「依親留職停薪」、「出國進修或研究留職停薪」、「易服勞役留職停薪」、「延長留職停薪」、「照護配偶或子女留職停薪」、「國內外進修期滿延長留職停薪」、「延長病假期滿留職停薪」、「因公傷病公假期滿留職停薪」、「留職停薪/停聘」、「其他情事留職停薪」、「借調公務機關留職停薪」、「借調公民營事業機構留職停薪」、「借調行政法人機關留職停薪」、「借調法定實驗學校留職停薪」，在商借類別應填寫N。 -------------------------------------------------------------------
flag_person <- drev_person_1

flag_person$levpay <-
  if_else(flag_person$levpay == "NA", "N", flag_person$levpay)
flag_person$negle <-
  if_else(flag_person$negle == "NA", "N", flag_person$negle)

#「留職停薪原因」、「商借類別」不合理處
flag_person$err_lev <- 0
flag_person$err_lev <-
  if_else(
    flag_person$levpay != "N"  &
      flag_person$negle != "N" &
      flag_person$source == "教員資料表",
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
  flag_person_wide_flag52 <- flag_person %>%
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
    colnames(flag_person_wide_flag52)[3:length(colnames(flag_person_wide_flag52))]
  flag_person_wide_flag52$flag52_r <- NA
  for (i in temp) {
    flag_person_wide_flag52$flag52_r <-
      paste(flag_person_wide_flag52$flag52_r,
            flag_person_wide_flag52[[i]],
            sep = " ")
  }
  flag_person_wide_flag52$flag52_r <-
    gsub("NA ",
         replacement = "",
         flag_person_wide_flag52$flag52_r)
  flag_person_wide_flag52$flag52_r <-
    gsub(" NA",
         replacement = "",
         flag_person_wide_flag52$flag52_r)
  
  #產生檢誤報告文字
  flag52_temp <- flag_person_wide_flag52 %>%
    group_by(organization_id) %>%
    mutate(flag52_txt = paste(source, "需核對「留職停薪原因」與「商借類別」：", flag52_r, sep = ""),
           "") %>%
    subset(select = c(organization_id, flag52_txt)) %>%
    distinct(organization_id, flag52_txt)
  
  #根據organization_id，展開成寬資料(wide)
  flag52 <- flag52_temp %>%
    dcast(organization_id ~ flag52_txt, value.var = "flag52_txt")
  
  #合併教員資料表及職員(工)資料表報告文字
  temp <- colnames(flag52)[2:length(colnames(flag52))]
  flag52$flag52 <- NA
  for (i in temp) {
    flag52$flag52 <- paste(flag52$flag52, flag52[[i]], sep = "； ")
  }
  flag52$flag52 <- gsub("NA； ", replacement = "", flag52$flag52)
  flag52$flag52 <- gsub("； NA", replacement = "", flag52$flag52)
  
  #產生檢誤報告文字
  flag52 <- flag52 %>%
    subset(select = c(organization_id, flag52)) %>%
    distinct(organization_id, flag52) %>%
    mutate(flag52 = paste(flag52, "", sep = ""))
} else{
  #偵測flag52是否存在。若不存在，則產生NA行
  if ('flag52' %in% ls()) {
    print("flag52")
  } else{
    flag52 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    flag52$flag52 <- ""
  }
}
