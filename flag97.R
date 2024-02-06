# flag97: 專任和代理是否同時存在兩校以上。 -------------------------------------------------------------------
#這裡是撈所有學校的資料來比對
flag_person <- drev_person %>%
  select(
    c(
      "organization_id",
      "edu_name2",
      "idnumber",
      "name",
      "sertype",
      "emptype",
      "emsub",
      "source"
    )
  ) %>%
  subset(emptype %in% c("專任", "代理", "代理(連)")) %>%
  group_by(idnumber) %>%
  mutate(index = n()) %>%
  filter(index > 1) %>%
  ungroup() %>%
  mutate(err_flag = 1)

flag_person$sertype <-
  if_else(is.na(flag_person$sertype),
          "職員(工)",
          flag_person$sertype)

#加註
flag_person$name <-
  paste(flag_person$name,
        "（",
        flag_person$emptype,
        flag_person$sertype,
        "）",
        sep = "")
flag_person$name <- gsub("；）", replacement = "）", flag_person$name)
flag_person$name <- gsub("（）", replacement = "", flag_person$name)

#呈現姓名
flag_person$err_flag_txt <- ""
flag_person$err_flag_txt <-
  case_when(flag_person$err_flag == 1 ~ flag_person$name,
            TRUE ~ flag_person$err_flag_txt)

if (dim(flag_person %>% subset(err_flag == 1))[1] != 0) {
  #根據organization_id + source，展開成寬資料(wide)
  flag_person_wide_flag97 <- flag_person %>%
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
    colnames(flag_person_wide_flag97)[3:length(colnames(flag_person_wide_flag97))]
  flag_person_wide_flag97$flag97_r <- NA
  for (i in temp) {
    flag_person_wide_flag97$flag97_r <-
      paste(flag_person_wide_flag97$flag97_r,
            flag_person_wide_flag97[[i]],
            sep = " ")
  }
  flag_person_wide_flag97$flag97_r <-
    gsub("NA ",
         replacement = "",
         flag_person_wide_flag97$flag97_r)
  flag_person_wide_flag97$flag97_r <-
    gsub(" NA",
         replacement = "",
         flag_person_wide_flag97$flag97_r)
  
  #產生檢誤報告文字
  flag97_temp <- flag_person_wide_flag97 %>%
    group_by(organization_id) %>%
    mutate(flag97_txt = paste(source, "：請確認該員本學期是否在職：", flag97_r, sep = ""),
           "") %>%
    subset(select = c(organization_id, flag97_txt)) %>%
    distinct(organization_id, flag97_txt)
  
  #根據organization_id，展開成寬資料(wide)
  flag97 <- flag97_temp %>%
    dcast(organization_id ~ flag97_txt, value.var = "flag97_txt")
  
  #合併教員資料表及職員(工)資料表報告文字
  temp <- colnames(flag97)[2:length(colnames(flag97))]
  flag97$flag97 <- NA
  for (i in temp) {
    flag97$flag97 <- paste(flag97$flag97, flag97[[i]], sep = "； ")
  }
  flag97$flag97 <- gsub("NA； ", replacement = "", flag97$flag97)
  flag97$flag97 <- gsub("； NA", replacement = "", flag97$flag97)
  
  #產生檢誤報告文字
  flag97 <- flag97 %>%
    subset(select = c(organization_id, flag97)) %>%
    distinct(organization_id, flag97) %>%
    mutate(flag97 = paste(flag97, "", sep = ""))
} else{
  #偵測flag97是否存在。若不存在，則產生NA行
  if ('flag97' %in% ls()) {
    print("flag97")
  } else{
    flag97 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    flag97$flag97 <- ""
  }
}
