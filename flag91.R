# flag91: 本學期離退教職員（工）資料表比對上一學年（期）教員/職員（工）資料表，同一身分識別碼所對應的姓名不同。 -------------------------------------------------------------------
flag_person <- drev_P_retire_merge_pre %>%
  rename(
    name = name.x,
    name_pre = name.y,
    name_retire = name,
    edu_name2 = edu_name2.x
  )

#本學期離退教職員（工）資料表比對上一學年（期）教員/職員（工）資料表，同一身分識別碼所對應的姓名不同。
flag_person$err_flag <- 0
flag_person$err_flag <-
  if_else(
    flag_person$pre == 1 &
      is.na(flag_person$now) &
      flag_person$retire == 1 &
      flag_person$name_pre != flag_person$name_retire,
    1,
    flag_person$err_flag
  )

#加註
flag_person$name <-
  paste(flag_person$name_pre,
        "/",
        flag_person$name_retire,
        sep = "")
flag_person$name <- gsub("；）", replacement = "）", flag_person$name)
flag_person$name <- gsub("（）", replacement = "", flag_person$name)

#呈現姓名
flag_person$err_flag_txt <- ""
flag_person$err_flag_txt <-
  case_when(flag_person$err_flag == 1 ~ flag_person$name,
            TRUE ~ flag_person$err_flag_txt)

if (dim(flag_person %>% subset(err_flag == 1))[1] != 0) {
  #根據organization_id，展開成寬資料(wide)
  flag_person_wide_flag91 <- flag_person %>%
    subset(select = c(organization_id,
                      idnumber,
                      err_flag_txt,
                      edu_name2,
                      err_flag)) %>%
    subset(err_flag == 1) %>%
    dcast(organization_id ~ err_flag_txt, value.var = "err_flag_txt")
  
  #合併所有name
  temp <-
    colnames(flag_person_wide_flag91)[2:length(colnames(flag_person_wide_flag91))]
  flag_person_wide_flag91$flag91_r <- NA
  for (i in temp) {
    flag_person_wide_flag91$flag91_r <-
      paste(flag_person_wide_flag91$flag91_r,
            flag_person_wide_flag91[[i]],
            sep = " ")
  }
  flag_person_wide_flag91$flag91_r <-
    gsub("NA ",
         replacement = "",
         flag_person_wide_flag91$flag91_r)
  flag_person_wide_flag91$flag91_r <-
    gsub(" NA",
         replacement = "",
         flag_person_wide_flag91$flag91_r)
  
  #產生檢誤報告文字
  flag91_temp <- flag_person_wide_flag91 %>%
    group_by(organization_id) %>%
    mutate(flag91_txt = paste("請確認：", flag91_r, sep = ""), "") %>%
    subset(select = c(organization_id, flag91_txt)) %>%
    distinct(organization_id, flag91_txt)
  
  #根據organization_id，展開成寬資料(wide)
  flag91 <- flag91_temp %>%
    dcast(organization_id ~ flag91_txt, value.var = "flag91_txt")
  
  #合併教員資料表及職員(工)資料表報告文字
  temp <- colnames(flag91)[2:length(colnames(flag91))]
  flag91$flag91 <- NA
  for (i in temp) {
    flag91$flag91 <- paste(flag91$flag91, flag91[[i]], sep = "； ")
  }
  flag91$flag91 <- gsub("NA； ", replacement = "", flag91$flag91)
  flag91$flag91 <- gsub("； NA", replacement = "", flag91$flag91)
  
  #產生檢誤報告文字
  flag91 <- flag91 %>%
    subset(select = c(organization_id, flag91)) %>%
    distinct(organization_id, flag91) %>%
    mutate(flag91 = paste(
      flag91,
      "（離退人員於上一期資料填報姓名與本次資料不相同。請務必確認修正，如已更名，請來電告知。）",
      sep = ""
    ))
} else{
  #偵測flag91是否存在。若不存在，則產生NA行
  if ('flag91' %in% ls()) {
    print("flag91")
  } else{
    flag91 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    flag91$flag91 <- ""
  }
}
