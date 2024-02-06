# flag84: 離退教職員（工）資料表所列人員，應為上一學年（期）專任教職員（工）。 -------------------------------------------------------------------
flag_person <- drev_P_retire_pre_inner %>%
  rename(name = name.x, name_retire = name.y) %>%
  left_join(edu_name2, by = c("organization_id"))

#若drev_P_retire_pre_inner無資料，建立物件
if (dim(drev_P_retire_pre_inner)[1] == 0) {
  temp <-
    matrix("", nrow = 1, ncol = ncol(flag_person)) %>% data.frame()
  names(temp) <- names(flag_person)
  flag_person <- temp
} else{
  print("flag84: drev_P_retire_pre_inner is already exists.")
}

#填寫在「離退教職員(工)資料表」之人員，聘任類別需為「專任」。(與上一期資料比對)
flag_person$err_flag <- 0
flag_person$err_flag <-
  if_else(flag_person$emptype != "專任" &
            flag_person$emptype != "",
          1,
          flag_person$err_flag)

#呈現姓名
flag_person$err_flag_txt <- ""
flag_person$err_flag_txt <-
  case_when(flag_person$err_flag == 1 ~ flag_person$name,
            TRUE ~ flag_person$err_flag_txt)

if (dim(flag_person %>% subset(err_flag == 1))[1] != 0) {
  #根據organization_id + source，展開成寬資料(wide)
  flag_person_wide_flag84 <- flag_person %>%
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
    colnames(flag_person_wide_flag84)[3:length(colnames(flag_person_wide_flag84))]
  flag_person_wide_flag84$flag84_r <- NA
  for (i in temp) {
    flag_person_wide_flag84$flag84_r <-
      paste(flag_person_wide_flag84$flag84_r,
            flag_person_wide_flag84[[i]],
            sep = " ")
  }
  flag_person_wide_flag84$flag84_r <-
    gsub("NA ",
         replacement = "",
         flag_person_wide_flag84$flag84_r)
  flag_person_wide_flag84$flag84_r <-
    gsub(" NA",
         replacement = "",
         flag_person_wide_flag84$flag84_r)
  
  flag_person_wide_flag84$source <-
    gsub("資料表",
         replacement = "",
         flag_person_wide_flag84$source)
  
  #產生檢誤報告文字
  flag84_temp <- flag_person_wide_flag84 %>%
    group_by(organization_id) %>%
    mutate(flag84_txt = paste(source, "：", flag84_r, sep = ""), "") %>%
    subset(select = c(organization_id, flag84_txt)) %>%
    distinct(organization_id, flag84_txt)
  
  #根據organization_id，展開成寬資料(wide)
  flag84 <- flag84_temp %>%
    dcast(organization_id ~ flag84_txt, value.var = "flag84_txt")
  
  #合併教員資料表及職員(工)資料表報告文字
  temp <- colnames(flag84)[2:length(colnames(flag84))]
  flag84$flag84 <- NA
  for (i in temp) {
    flag84$flag84 <- paste(flag84$flag84, flag84[[i]], sep = "； ")
  }
  flag84$flag84 <- gsub("NA； ", replacement = "", flag84$flag84)
  flag84$flag84 <- gsub("； NA", replacement = "", flag84$flag84)
  
  #產生檢誤報告文字
  flag84 <- flag84 %>%
    subset(select = c(organization_id, flag84)) %>%
    distinct(organization_id, flag84) %>%
    mutate(
      flag84 = paste(
        flag84,
        "（查貴校上一學年所填資料，上述人員聘任類別非屬『專任』。依欄位說明，非專任教職員(工)之退休或離職者，不須填列離退教職員(工)資料表，請務必再確認。）",
        sep = ""
      )
    )
} else{
  #偵測flag84是否存在。若不存在，則產生NA行
  if ('flag84' %in% ls()) {
    print("flag84")
  } else{
    flag84 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    flag84$flag84 <- ""
  }
}
