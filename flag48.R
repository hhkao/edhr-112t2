# flag48: 1. 職務名稱與兼任行政職職稱(一)，兩者不應填相同職稱。2. 兼任行政職職稱(一)~(三)，三者不應填相同職稱。-------------------------------------------------------------------
flag_person <- drev_person_1

flag_person$err_admintitle0  <-
  paste(flag_person$adminunit0,
        flag_person$admintitle0,
        sep = "")
flag_person$err_admintitle1  <-
  paste(flag_person$adminunit1,
        flag_person$admintitle1,
        sep = "")
flag_person$err_admintitle2  <-
  paste(flag_person$adminunit2,
        flag_person$admintitle2,
        sep = "")
flag_person$err_admintitle3  <-
  paste(flag_person$adminunit3,
        flag_person$admintitle3,
        sep = "")

#職務名稱與兼任行政職職稱不合理處
flag_person$err_flag <- 0
flag_person$err_flag <-
  if_else(
    flag_person$err_admintitle0 == flag_person$err_admintitle1,
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$err_admintitle0 == "NN" &
      flag_person$err_admintitle1 == "NN",
    0,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$err_admintitle0 == flag_person$err_admintitle2,
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$err_admintitle0 == "NN" &
      flag_person$err_admintitle2 == "NN",
    0,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$err_admintitle0 == flag_person$err_admintitle3,
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$err_admintitle0 == "NN" &
      flag_person$err_admintitle3 == "NN",
    0,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else((flag_person$err_admintitle1 == flag_person$err_admintitle2) &
            flag_person$err_admintitle1 != "NN",
          1,
          flag_person$err_flag
  )
flag_person$err_flag <-
  if_else((flag_person$err_admintitle1 == flag_person$err_admintitle3) &
            flag_person$err_admintitle1 != "NN",
          1,
          flag_person$err_flag
  )
flag_person$err_flag <-
  if_else((flag_person$err_admintitle2 == flag_person$err_admintitle3) &
            flag_person$err_admintitle2 != "NN",
          1,
          flag_person$err_flag
  )

#加註姓名
flag_person$err_flag_txt <- ""
flag_person$err_flag_txt <-
  case_when(flag_person$err_flag == 1 ~ flag_person$name,
            TRUE ~ flag_person$err_flag_txt)

if (dim(flag_person %>% subset(err_flag == 1))[1] != 0) {
  #根據organization_id + source，展開成寬資料(wide)
  flag_person_wide_flag48 <- flag_person %>%
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
    colnames(flag_person_wide_flag48)[3:length(colnames(flag_person_wide_flag48))]
  flag_person_wide_flag48$flag48_r <- NA
  for (i in temp) {
    flag_person_wide_flag48$flag48_r <-
      paste(flag_person_wide_flag48$flag48_r,
            flag_person_wide_flag48[[i]],
            sep = " ")
  }
  flag_person_wide_flag48$flag48_r <-
    gsub("NA ",
         replacement = "",
         flag_person_wide_flag48$flag48_r)
  flag_person_wide_flag48$flag48_r <-
    gsub(" NA",
         replacement = "",
         flag_person_wide_flag48$flag48_r)
  
  #產生檢誤報告文字
  flag48_temp <- flag_person_wide_flag48 %>%
    mutate(flag48_txt =
             case_when(
               source == "教員資料表" ~ paste(source, "需核對「服務身分別」與「兼任行政職職稱(一)」：", flag48_r, sep = ""),
               source == "職員(工)資料表" ~ paste(source, "「職務名稱」與「兼任行政職職稱」重複：", flag48_r, sep = "")
             )) %>%
    group_by(organization_id) %>%
    subset(select = c(organization_id, flag48_txt)) %>%
    distinct(organization_id, flag48_txt)
  
  #根據organization_id，展開成寬資料(wide)
  flag48 <- flag48_temp %>%
    dcast(organization_id ~ flag48_txt, value.var = "flag48_txt")
  
  #合併教員資料表及職員(工)資料表報告文字
  temp <- colnames(flag48)[2:length(colnames(flag48))]
  flag48$flag48 <- NA
  for (i in temp) {
    flag48$flag48 <- paste(flag48$flag48, flag48[[i]], sep = "； ")
  }
  flag48$flag48 <- gsub("NA； ", replacement = "", flag48$flag48)
  flag48$flag48 <- gsub("； NA", replacement = "", flag48$flag48)
  
  #產生檢誤報告文字
  flag48 <- flag48 %>%
    subset(select = c(organization_id, flag48)) %>%
    distinct(organization_id, flag48) %>%
    mutate(
      flag48 = paste(
        flag48,
        "（以上人員之專職工作職稱請填入『職務名稱』，非『兼任行政職職稱』。併請確認以上人員除本職職務外，是否再兼任其他職務）",
        sep = ""
      )
    )
} else{
  #偵測flag48是否存在。若不存在，則產生NA行
  if ('flag48' %in% ls()) {
    print("flag48")
  } else{
    flag48 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    flag48$flag48 <- ""
  }
}
