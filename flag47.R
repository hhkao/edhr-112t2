# flag47: 兼任行政職職稱(一)若填寫“校長"，代表服務身分別填答有誤，故應核對服務身分別與兼任行政職職稱(一)。 -------------------------------------------------------------------
flag_person <- drev_person_1

#標記服務身分別不為"校長，且兼任行政職稱為"校長"
flag_person$err_flag <- 0
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      flag_person$sertype != "校長" &
      flag_person$admintitle1 == "校長",
    1,
    flag_person$err_flag
  )

#加註
flag_person$err_flag_txt <- ""
flag_person$err_flag_txt <-
  case_when(flag_person$err_flag == 1 ~ flag_person$name,
            TRUE ~ flag_person$err_flag_txt)

if (dim(flag_person %>% subset(err_flag == 1))[1] != 0) {
  #根據organization_id + source，展開成寬資料(wide)
  flag_person_wide_flag47 <- flag_person %>%
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
    colnames(flag_person_wide_flag47)[3:length(colnames(flag_person_wide_flag47))]
  flag_person_wide_flag47$flag47_r <- NA
  for (i in temp) {
    flag_person_wide_flag47$flag47_r <-
      paste(flag_person_wide_flag47$flag47_r,
            flag_person_wide_flag47[[i]],
            sep = " ")
  }
  flag_person_wide_flag47$flag47_r <-
    gsub("NA ",
         replacement = "",
         flag_person_wide_flag47$flag47_r)
  flag_person_wide_flag47$flag47_r <-
    gsub(" NA",
         replacement = "",
         flag_person_wide_flag47$flag47_r)
  
  #產生檢誤報告文字
  flag47_temp <- flag_person_wide_flag47 %>%
    group_by(organization_id) %>%
    mutate(flag47_txt = paste(source, "需核對「服務身分別」：", flag47_r, sep = ""),
           "") %>%
    subset(select = c(organization_id, flag47_txt)) %>%
    distinct(organization_id, flag47_txt)
  
  #根據organization_id，展開成寬資料(wide)
  flag47 <- flag47_temp %>%
    dcast(organization_id ~ flag47_txt, value.var = "flag47_txt")
  
  #合併教員資料表及職員(工)資料表報告文字
  temp <- colnames(flag47)[2:length(colnames(flag47))]
  flag47$flag47 <- NA
  for (i in temp) {
    flag47$flag47 <- paste(flag47$flag47, flag47[[i]], sep = "； ")
  }
  flag47$flag47 <- gsub("NA； ", replacement = "", flag47$flag47)
  flag47$flag47 <- gsub("； NA", replacement = "", flag47$flag47)
  
  #產生檢誤報告文字
  flag47 <- flag47 %>%
    subset(select = c(organization_id, flag47)) %>%
    distinct(organization_id, flag47) %>%
    mutate(flag47 = paste(flag47, "（請依實際情況並按欄位說明修正）", sep = ""))
} else{
  #偵測flag47是否存在。若不存在，則產生NA行
  if ('flag47' %in% ls()) {
    print("flag47")
  } else{
    flag47 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    flag47$flag47 <- ""
  }
}
