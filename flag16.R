# flag16: 請假類別應依《教師請假規則》、《公務人員請假規則》以及人事行政主管機關公教員工請假給假一覽表相關規定填列。 -------------------------------------------------------------------
flag_person <- drev_person_1

#標記各種假別為1
flag_person$err_flag <- 1
flag_person$err_flag <-
  if_else(flag_person$leave == "事假", 0, flag_person$err_flag)
flag_person$err_flag <-
  if_else(flag_person$leave == "家庭照顧假", 0, flag_person$err_flag)
flag_person$err_flag <-
  if_else(flag_person$leave == "病假", 0, flag_person$err_flag)
flag_person$err_flag <-
  if_else(flag_person$leave == "延長病假", 0, flag_person$err_flag)
flag_person$err_flag <-
  if_else(flag_person$leave == "普通傷病假", 0, flag_person$err_flag)
flag_person$err_flag <-
  if_else(flag_person$leave == "生理假", 0, flag_person$err_flag)
flag_person$err_flag <-
  if_else(flag_person$leave == "婚假", 0, flag_person$err_flag)
flag_person$err_flag <-
  if_else(flag_person$leave == "娩假", 0, flag_person$err_flag)
flag_person$err_flag <-
  if_else(flag_person$leave == "分娩假", 0, flag_person$err_flag)
flag_person$err_flag <-
  if_else(flag_person$leave == "產前假", 0, flag_person$err_flag)
flag_person$err_flag <-
  if_else(flag_person$leave == "陪產假", 0, flag_person$err_flag)
flag_person$err_flag <-
  if_else(flag_person$leave == "流產假", 0, flag_person$err_flag)
flag_person$err_flag <-
  if_else(flag_person$leave == "安胎假", 0, flag_person$err_flag)
flag_person$err_flag <-
  if_else(flag_person$leave == "喪假", 0, flag_person$err_flag)
flag_person$err_flag <-
  if_else(flag_person$leave == "休假", 0, flag_person$err_flag)
flag_person$err_flag <-
  if_else(flag_person$leave == "捐贈器官或骨髓假",
          0,
          flag_person$err_flag)
flag_person$err_flag <-
  if_else(flag_person$leave == "公假", 0, flag_person$err_flag)
flag_person$err_flag <-
  if_else(flag_person$leave == "特別休假", 0, flag_person$err_flag)
flag_person$err_flag <-
  if_else(flag_person$leave == "特休", 0, flag_person$err_flag)
#衛理女中確實核給"延長重病假"
flag_person$err_flag <-
  if_else(
    flag_person$leave == "延長重病假" &
      flag_person$organization_id == "411302",
    0,
    flag_person$err_flag
  )

flag_person$err_flag <-
  if_else(flag_person$leave == "延長病假(安胎)",
          0,
          flag_person$err_flag)
flag_person$err_flag <-
  if_else(flag_person$leave == "公假(公傷假)",
          0,
          flag_person$err_flag)
flag_person$err_flag <-
  if_else(flag_person$leave == "公(傷)假", 0, flag_person$err_flag)
flag_person$err_flag <-
  if_else(flag_person$leave == "安胎病假、產前假、娩假",
          0,
          flag_person$err_flag)
flag_person$err_flag <-
  if_else(flag_person$leave == "安胎病假及延長病假",
          0,
          flag_person$err_flag)

flag_person$err_flag <-
  if_else(
    grepl("留職停薪$", flag_person$leave) |
      grepl("留停$", flag_person$leave),
    0,
    flag_person$err_flag
  )

flag_person$err_flag <-
  if_else(
    grepl("^育嬰假$", flag_person$leave) &
      flag_person$levpay == "育嬰留職停薪",
    0,
    flag_person$err_flag
  )

flag_person$err_flag <-
  if_else(flag_person$leave == "N", 0, flag_person$err_flag)


#加註請假類別
flag_person$err_flag_txt <- ""
flag_person$err_flag_txt <- case_when(
  flag_person$err_flag == 1 ~ paste(flag_person$name, "（", flag_person$leave, "）", sep = ""),
  TRUE ~ flag_person$err_flag_txt
)

if (dim(flag_person %>% subset(err_flag == 1))[1] != 0) {
  #根據organization_id + source，展開成寬資料(wide)
  flag_person_wide_flag16 <- flag_person %>%
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
    colnames(flag_person_wide_flag16)[3:length(colnames(flag_person_wide_flag16))]
  flag_person_wide_flag16$flag16_r <- NA
  for (i in temp) {
    flag_person_wide_flag16$flag16_r <-
      paste(flag_person_wide_flag16$flag16_r,
            flag_person_wide_flag16[[i]],
            sep = " ")
  }
  flag_person_wide_flag16$flag16_r <-
    gsub("NA ",
         replacement = "",
         flag_person_wide_flag16$flag16_r)
  flag_person_wide_flag16$flag16_r <-
    gsub(" NA",
         replacement = "",
         flag_person_wide_flag16$flag16_r)
  
  #產生檢誤報告文字
  flag16_temp <- flag_person_wide_flag16 %>%
    group_by(organization_id) %>%
    mutate(flag16_txt = paste(source, "需修改請假類別：", flag16_r, sep = ""),
           "") %>%
    subset(select = c(organization_id, flag16_txt)) %>%
    distinct(organization_id, flag16_txt)
  
  #根據organization_id，展開成寬資料(wide)
  flag16 <- flag16_temp %>%
    dcast(organization_id ~ flag16_txt, value.var = "flag16_txt")
  
  #合併教員資料表及職員(工)資料表報告文字
  temp <- colnames(flag16)[2:length(colnames(flag16))]
  flag16$flag16 <- NA
  for (i in temp) {
    flag16$flag16 <- paste(flag16$flag16, flag16[[i]], sep = "； ")
  }
  flag16$flag16 <- gsub("NA； ", replacement = "", flag16$flag16)
  flag16$flag16 <- gsub("； NA", replacement = "", flag16$flag16)
  
  #產生檢誤報告文字
  flag16 <- flag16 %>%
    subset(select = c(organization_id, flag16)) %>%
    distinct(organization_id, flag16) %>%
    mutate(flag16 = paste(
      flag16,
      "（請依規定確認或修正請假類別，或是否屬於請假，若以上人員未有請假情事，請填寫半型大寫『N』）",
      sep = ""
    ))
} else{
  #偵測flag16是否存在。若不存在，則產生NA行
  if ('flag16' %in% ls()) {
    print("flag16")
  } else{
    flag16 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    flag16$flag16 <- ""
  }
}
