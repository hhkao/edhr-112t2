# flag7: 出生年月日換算成年齡後，偏高或偏低。 -------------------------------------------------------------------
flag_person <- drev_person_1

flag_person$survey_year <- 113
flag_person$birthy <-
  substr(flag_person$birthdate, 1, 3) %>% as.numeric()

flag_person$age <- flag_person$survey_year - flag_person$birthy

#錯誤標記
flag_person$irr_year <- 0
flag_person$irr_year <-
  if_else(flag_person$age < 18, 1, flag_person$irr_year)
flag_person$irr_year <-
  if_else(flag_person$age > 75, 1, flag_person$irr_year)
flag_person$irr_year <-
  if_else(
    flag_person$age > 75 &
      (
        flag_person$emptype == "兼任" |
          flag_person$emptype == "長期代課" |
          flag_person$emptype == "專職族語老師" |
          flag_person$emptype == "鐘點教師" |
          flag_person$emptype == "約聘僱" |
          flag_person$emptype == "約用" |
          flag_person$emptype == "派遣"
      ),
    0,
    flag_person$irr_year
  )
flag_person$irr_year <-
  if_else(
    flag_person$age > 85 &
      (
        flag_person$emptype == "兼任" |
          flag_person$emptype == "長期代課" |
          flag_person$emptype == "專職族語老師" |
          flag_person$emptype == "鐘點教師" |
          flag_person$emptype == "約聘僱" |
          flag_person$emptype == "約用" |
          flag_person$emptype == "派遣"
      ),
    1,
    flag_person$irr_year
  )

#姓名加註出生年月日
flag_person$name <- case_when(
  flag_person$irr_year == 1 ~ paste(flag_person$name,
                                    "（",
                                    flag_person$birthdate,
                                    "）",
                                    sep = ""),
  TRUE ~ flag_person$name
)

if (dim(flag_person %>% subset(irr_year == 1))[1] != 0) {
  #根據organization_id + source，展開成寬資料(wide)
  flag_person_flag7 <- flag_person %>%
    subset(select = c(organization_id,
                      idnumber,
                      name,
                      edu_name2,
                      source,
                      irr_year)) %>%
    subset(irr_year == 1) %>%
    dcast(organization_id + source ~ name, value.var = "name")
  
  #合併所有name
  temp <-
    colnames(flag_person_flag7)[3:length(colnames(flag_person_flag7))]
  flag_person_flag7$flag7_r <- NA
  for (i in temp) {
    flag_person_flag7$flag7_r <-
      paste(flag_person_flag7$flag7_r,
            flag_person_flag7[[i]],
            sep = " ")
  }
  flag_person_flag7$flag7_r <-
    gsub("NA ", replacement = "", flag_person_flag7$flag7_r)
  flag_person_flag7$flag7_r <-
    gsub(" NA", replacement = "", flag_person_flag7$flag7_r)
  
  #產生檢誤報告文字
  flag7_temp <- flag_person_flag7 %>%
    group_by(organization_id) %>%
    mutate(flag7_txt = paste(source, "：", flag7_r, sep = ""), "") %>%
    subset(select = c(organization_id, flag7_txt)) %>%
    distinct(organization_id, flag7_txt)
  
  #根據organization_id，展開成寬資料(wide)
  flag7 <- flag7_temp %>%
    dcast(organization_id ~ flag7_txt, value.var = "flag7_txt")
  
  #合併教員資料表及職員(工)資料表報告文字
  temp <- colnames(flag7)[2:length(colnames(flag7))]
  flag7$flag7 <- NA
  for (i in temp) {
    flag7$flag7 <- paste(flag7$flag7, flag7[[i]], sep = "； ")
  }
  flag7$flag7 <- gsub("NA； ", replacement = "", flag7$flag7)
  flag7$flag7 <- gsub("； NA", replacement = "", flag7$flag7)
  
  #產生檢誤報告文字
  flag7 <- flag7 %>%
    subset(select = c(organization_id, flag7)) %>%
    distinct(organization_id, flag7) %>%
    mutate(flag7 = paste(flag7, "（請確認出生年月日是否正確）", sep = ""))
} else{
  #偵測flag7是否存在。若不存在，則產生NA行
  if ('flag7' %in% ls()) {
    print("flag7")
  } else{
    flag7 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    flag7$flag7 <- ""
  }
}
