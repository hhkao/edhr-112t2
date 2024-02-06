# flag90: 校內行政職務，原則由專任或代理教師兼任，請再確認實際情況及所填資料。 -------------------------------------------------------------------
flag_person <- drev_person_1

#兼任、長期代課、專職族語老師、鐘點教師、約聘僱、約用教師，不應有兼任行政職務，也不可擔任導師
flag_person$err_flag <- 0
flag_person$err_flag <- if_else(
  flag_person$emptype %in% c("兼任",
                             "長期代課",
                             "專職族語老師",
                             "鐘點教師",
                             "約聘僱",
                             "約用")
  & flag_person$sertype == "教師"
  &
    flag_person$admintitle1 != "N" ,
  1,
  flag_person$err_flag
)

#加註
flag_person$name <-
  paste(flag_person$name, "（", flag_person$emptype, "）", sep = "")
flag_person$name <- gsub("；）", replacement = "）", flag_person$name)
flag_person$name <- gsub("（）", replacement = "", flag_person$name)

#呈現姓名
flag_person$err_flag_txt <- ""
flag_person$err_flag_txt <-
  case_when(flag_person$err_flag == 1 ~ flag_person$name,
            TRUE ~ flag_person$err_flag_txt)

if (dim(flag_person %>% subset(err_flag == 1))[1] != 0) {
  #根據organization_id + source，展開成寬資料(wide)
  flag_person_wide_flag90 <- flag_person %>%
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
    colnames(flag_person_wide_flag90)[3:length(colnames(flag_person_wide_flag90))]
  flag_person_wide_flag90$flag90_r <- NA
  for (i in temp) {
    flag_person_wide_flag90$flag90_r <-
      paste(flag_person_wide_flag90$flag90_r,
            flag_person_wide_flag90[[i]],
            sep = " ")
  }
  flag_person_wide_flag90$flag90_r <-
    gsub("NA ",
         replacement = "",
         flag_person_wide_flag90$flag90_r)
  flag_person_wide_flag90$flag90_r <-
    gsub(" NA",
         replacement = "",
         flag_person_wide_flag90$flag90_r)
  
  #產生檢誤報告文字
  flag90_temp <- flag_person_wide_flag90 %>%
    group_by(organization_id) %>%
    mutate(flag90_txt = paste("姓名：", flag90_r, sep = ""), "") %>%
    subset(select = c(organization_id, flag90_txt)) %>%
    distinct(organization_id, flag90_txt)
  
  #根據organization_id，展開成寬資料(wide)
  flag90 <- flag90_temp %>%
    dcast(organization_id ~ flag90_txt, value.var = "flag90_txt")
  
  #合併教員資料表及職員(工)資料表報告文字
  temp <- colnames(flag90)[2:length(colnames(flag90))]
  flag90$flag90 <- NA
  for (i in temp) {
    flag90$flag90 <- paste(flag90$flag90, flag90[[i]], sep = "； ")
  }
  flag90$flag90 <- gsub("NA； ", replacement = "", flag90$flag90)
  flag90$flag90 <- gsub("； NA", replacement = "", flag90$flag90)
  
  #產生檢誤報告文字
  flag90 <- flag90 %>%
    subset(select = c(organization_id, flag90)) %>%
    distinct(organization_id, flag90) %>%
    mutate(
      flag90 = paste(
        flag90,
        "（人事資料顯示該教師兼任行政職務）\n",
        "（校內行政職務原則由專任教師兼任，請協助再確認上述教師是否兼任行政職，或協助再確認上述教師之聘任類別）",
        sep = ""
      )
    )
} else{
  #偵測flag90是否存在。若不存在，則產生NA行
  if ('flag90' %in% ls()) {
    print("flag90")
  } else{
    flag90 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    flag90$flag90 <- ""
  }
}
