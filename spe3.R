# spe3: 本校到職日期晚於填報基準日。 -------------------------------------------------------------------
flag_person <- drev_person_1

#本校到職日期晚於填報基準日。
flag_person$survey_year <- 113
flag_person$survey_mon <- 3

flag_person$arvy <-
  substr(flag_person$onbodat, 1, 3) %>% as.numeric()
flag_person$arvm <-
  substr(flag_person$onbodat, 4, 5) %>% as.numeric()

flag_person$err_spe <-
  if_else((flag_person$arvy * 12 + flag_person$arvm) > (flag_person$survey_year * 12 + flag_person$survey_mon),
          1,
          0
  )

#加註
flag_person$name <-
  paste(flag_person$name, "（", flag_person$onbodat, "）", sep = "")
flag_person$name <- gsub("；）", replacement = "）", flag_person$name)
flag_person$name <- gsub("（）", replacement = "", flag_person$name)

#呈現姓名
flag_person$err_flag_txt <- ""
flag_person$err_flag_txt <-
  case_when(flag_person$err_spe == 1 ~ flag_person$name,
            TRUE ~ flag_person$err_flag_txt)

if (dim(flag_person %>% subset(err_spe == 1))[1] != 0) {
  #根據organization_id + source，展開成寬資料(wide)
  flag_person_wide_spe3 <- flag_person %>%
    subset(select = c(
      organization_id,
      idnumber,
      err_flag_txt,
      edu_name2,
      source,
      err_spe
    )) %>%
    subset(err_spe == 1) %>%
    dcast(organization_id + source ~ err_flag_txt, value.var = "err_flag_txt")
  
  #合併所有name
  temp <-
    colnames(flag_person_wide_spe3)[3:length(colnames(flag_person_wide_spe3))]
  flag_person_wide_spe3$spe3_r <- NA
  for (i in temp) {
    flag_person_wide_spe3$spe3_r <-
      paste(flag_person_wide_spe3$spe3_r,
            flag_person_wide_spe3[[i]],
            sep = " ")
  }
  flag_person_wide_spe3$spe3_r <-
    gsub("NA ", replacement = "", flag_person_wide_spe3$spe3_r)
  flag_person_wide_spe3$spe3_r <-
    gsub(" NA", replacement = "", flag_person_wide_spe3$spe3_r)
  
  #產生檢誤報告文字
  spe3_temp <- flag_person_wide_spe3 %>%
    group_by(organization_id) %>%
    mutate(spe3_txt = paste(source, "：", spe3_r, sep = ""), "") %>%
    subset(select = c(organization_id, spe3_txt)) %>%
    distinct(organization_id, spe3_txt)
  
  #根據organization_id，展開成寬資料(wide)
  spe3 <- spe3_temp %>%
    dcast(organization_id ~ spe3_txt, value.var = "spe3_txt")
  
  #合併教員資料表及職員(工)資料表報告文字
  temp <- colnames(spe3)[2:length(colnames(spe3))]
  spe3$spe3 <- NA
  for (i in temp) {
    spe3$spe3 <- paste(spe3$spe3, spe3[[i]], sep = "； ")
  }
  spe3$spe3 <- gsub("NA； ", replacement = "", spe3$spe3)
  spe3$spe3 <- gsub("； NA", replacement = "", spe3$spe3)
  
  #產生檢誤報告文字
  spe3 <- spe3 %>%
    subset(select = c(organization_id, spe3)) %>%
    distinct(organization_id, spe3) %>%
    mutate(spe3 = paste(spe3, "（請確認修正到職日期，並請以資料基準日113年3月29日當時情況為準）", sep = ""))
} else{
  #偵測spe3是否存在。若不存在，則產生NA行
  if ('spe3' %in% ls()) {
    print("spe3")
  } else{
    spe3 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    spe3$spe3 <- ""
  }
}
