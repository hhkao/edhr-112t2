# flag82: 若請假類別填寫「留職停薪」，則留職停薪原因須填寫內容。 -------------------------------------------------------------------
flag_person <- drev_person_1

#抓出:請假類別填"留職停薪"，但留職停薪原因填N
flag_person$err_flag <- 0
flag_person$err_flag <-
  if_else(grepl("留職停薪", flag_person$leave) &
            flag_person$levpay == "N",
          1,
          flag_person$err_flag)
flag_person$err_flag <-
  if_else(grepl("留職停薪", flag_person$leave) &
            flag_person$levpay == "Ｎ",
          1,
          flag_person$err_flag)
flag_person$err_flag <-
  if_else(grepl("留停", flag_person$leave) &
            flag_person$levpay == "N",
          1,
          flag_person$err_flag)
flag_person$err_flag <-
  if_else(grepl("留停", flag_person$leave) &
            flag_person$levpay == "Ｎ",
          1,
          flag_person$err_flag)

#加註
flag_person$name <-
  paste(
    flag_person$name,
    "（",
    "請假類別：",
    flag_person$leave,
    "；留職停薪原因：",
    flag_person$levpay,
    "）",
    sep = ""
  )
flag_person$name <- gsub("；）", replacement = "）", flag_person$name)
flag_person$name <- gsub("（）", replacement = "", flag_person$name)

#呈現姓名
flag_person$err_flag_txt <- ""
flag_person$err_flag_txt <-
  case_when(flag_person$err_flag == 1 ~ flag_person$name,
            TRUE ~ flag_person$err_flag_txt)

if (dim(flag_person %>% subset(err_flag == 1))[1] != 0) {
  #根據organization_id + source，展開成寬資料(wide)
  flag_person_wide_flag82 <- flag_person %>%
    subset(select = c(
      organization_id,
      idnumber,
      err_flag_txt,
      edu_name2,
      source,
      err_flag
    )) %>%
    subset(err_flag == 1) %>%
    dcast(organization_id + source ~ err_flag_txt, value.var = "err_flag_txt")
  
  #合併所有name
  temp <-
    colnames(flag_person_wide_flag82)[3:length(colnames(flag_person_wide_flag82))]
  flag_person_wide_flag82$flag82_r <- NA
  for (i in temp) {
    flag_person_wide_flag82$flag82_r <-
      paste(flag_person_wide_flag82$flag82_r,
            flag_person_wide_flag82[[i]],
            sep = " ")
  }
  flag_person_wide_flag82$flag82_r <-
    gsub("NA ",
         replacement = "",
         flag_person_wide_flag82$flag82_r)
  flag_person_wide_flag82$flag82_r <-
    gsub(" NA",
         replacement = "",
         flag_person_wide_flag82$flag82_r)
  
  #產生檢誤報告文字
  flag82_temp <- flag_person_wide_flag82 %>%
    group_by(organization_id) %>%
    mutate(flag82_txt = paste(source, "需修改請假類別、留職停薪原因：", flag82_r, sep = ""),
           "") %>%
    subset(select = c(organization_id, flag82_txt)) %>%
    distinct(organization_id, flag82_txt)
  
  #根據organization_id，展開成寬資料(wide)
  flag82 <- flag82_temp %>%
    dcast(organization_id ~ flag82_txt, value.var = "flag82_txt")
  
  #合併教員資料表及職員(工)資料表報告文字
  temp <- colnames(flag82)[2:length(colnames(flag82))]
  flag82$flag82 <- NA
  for (i in temp) {
    flag82$flag82 <- paste(flag82$flag82, flag82[[i]], sep = "； ")
  }
  flag82$flag82 <- gsub("NA； ", replacement = "", flag82$flag82)
  flag82$flag82 <- gsub("； NA", replacement = "", flag82$flag82)
  
  #產生檢誤報告文字
  flag82 <- flag82 %>%
    subset(select = c(organization_id, flag82)) %>%
    distinct(organization_id, flag82) %>%
    mutate(flag82 = paste(flag82, "", sep = ""))
} else{
  #偵測flag82是否存在。若不存在，則產生NA行
  if ('flag82' %in% ls()) {
    print("flag82")
  } else{
    flag82 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    flag82$flag82 <- ""
  }
}
