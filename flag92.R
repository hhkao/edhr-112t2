# flag92: 教員/職員（工）資料表及離退教職員(工)資料表，同一身分識別碼所對應的姓名不一致，請確認各該身分識別碼所屬正確人員。 -------------------------------------------------------------------
flag_person <- drev_P_retire %>%
  rename(name = name.x, name_retire = name.y)

#若drev_P_retire無資料，建立物件
if (dim(drev_P_retire)[1] == 0) {
  temp <-
    matrix("", nrow = 1, ncol = ncol(flag_person)) %>% data.frame()
  names(temp) <- names(flag_person)
  flag_person <- temp
} else{
  print("flag92: drev_P_retire is already exists.")
}

#本次離退教職員(工)資料表所列人員，若有出現本次教員資料表或職員（工）資料表(已存在錯誤情形)，姓名不一致
flag_person$err_flag <- 0
flag_person$err_flag <-
  if_else(flag_person$name != flag_person$name_retire,
          1,
          flag_person$err_flag)

#加註
flag_person$name <-
  paste(flag_person$name, "/", flag_person$name_retire, sep = "")
flag_person$name <- gsub("；）", replacement = "）", flag_person$name)
flag_person$name <- gsub("（）", replacement = "", flag_person$name)

#呈現姓名
flag_person$err_flag_txt <- ""
flag_person$err_flag_txt <-
  case_when(flag_person$err_flag == 1 ~ flag_person$name,
            TRUE ~ flag_person$err_flag_txt)

if (dim(flag_person %>% subset(err_flag == 1))[1] != 0) {
  #根據organization_id + source，展開成寬資料(wide)
  flag_person_wide_flag92 <- flag_person %>%
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
    colnames(flag_person_wide_flag92)[3:length(colnames(flag_person_wide_flag92))]
  flag_person_wide_flag92$flag92_r <- NA
  for (i in temp) {
    flag_person_wide_flag92$flag92_r <-
      paste(flag_person_wide_flag92$flag92_r,
            flag_person_wide_flag92[[i]],
            sep = " ")
  }
  flag_person_wide_flag92$flag92_r <-
    gsub("NA ",
         replacement = "",
         flag_person_wide_flag92$flag92_r)
  flag_person_wide_flag92$flag92_r <-
    gsub(" NA",
         replacement = "",
         flag_person_wide_flag92$flag92_r)
  
  #產生檢誤報告文字
  flag92_temp <- flag_person_wide_flag92 %>%
    group_by(organization_id) %>%
    mutate(flag92_txt = paste("請確認：", flag92_r, sep = ""), "") %>%
    subset(select = c(organization_id, flag92_txt)) %>%
    distinct(organization_id, flag92_txt)
  
  #根據organization_id，展開成寬資料(wide)
  flag92 <- flag92_temp %>%
    dcast(organization_id ~ flag92_txt, value.var = "flag92_txt")
  
  #合併教員資料表及職員(工)資料表報告文字
  temp <- colnames(flag92)[2:length(colnames(flag92))]
  flag92$flag92 <- NA
  for (i in temp) {
    flag92$flag92 <- paste(flag92$flag92, flag92[[i]], sep = "； ")
  }
  flag92$flag92 <- gsub("NA； ", replacement = "", flag92$flag92)
  flag92$flag92 <- gsub("； NA", replacement = "", flag92$flag92)
  
  #產生檢誤報告文字
  flag92 <- flag92 %>%
    subset(select = c(organization_id, flag92)) %>%
    distinct(organization_id, flag92) %>%
    mutate(
      flag92 = paste(
        flag92,
        "（教員/職員（工）資料表及離退教職員(工)資料表，同一身分識別碼所對應的姓名不一致，請確認各該身分識別碼所屬正確人員。）",
        sep = ""
      )
    )
} else{
  #偵測flag92是否存在。若不存在，則產生NA行
  if ('flag92' %in% ls()) {
    print("flag92")
  } else{
    flag92 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    flag92$flag92 <- ""
  }
}
