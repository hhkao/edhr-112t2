# flag69: 「是否擔任學（群）科教學研究委員會召集人」，原則上同科召集人不會有兩人以上。 -------------------------------------------------------------------
#*若填N、NA、Y而致使重複，則改為不重複(填NA或Y由Flag68處理)
flag_person <- drev_P_load %>%
  subset(load == 1)

flag_person <- flag_person %>%
  subset(!(mitleader %in% c("N", "NA", "Y"))) %>%
  select(organization_id,
         edu_name2,
         idnumber,
         name,
         mitleader) %>%
  group_by(mitleader) %>%
  mutate(index = n()) %>%
  filter(index > 1) %>%
  ungroup() %>%
  mutate(err_flag = 1)

#加註
flag_person$name <-
  paste(flag_person$name,
        "（",
        flag_person$mitleader,
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
  flag_person_wide_flag69 <- flag_person %>%
    subset(select = c(organization_id,
                      idnumber,
                      err_flag_txt,
                      edu_name2,
                      err_flag)) %>%
    subset(err_flag == 1) %>%
    dcast(organization_id ~ err_flag_txt, value.var = "err_flag_txt")
  
  #合併所有name
  temp <-
    colnames(flag_person_wide_flag69)[2:length(colnames(flag_person_wide_flag69))]
  flag_person_wide_flag69$flag69_r <- NA
  for (i in temp) {
    flag_person_wide_flag69$flag69_r <-
      paste(flag_person_wide_flag69$flag69_r,
            flag_person_wide_flag69[[i]],
            sep = " ")
  }
  flag_person_wide_flag69$flag69_r <-
    gsub("NA ",
         replacement = "",
         flag_person_wide_flag69$flag69_r)
  flag_person_wide_flag69$flag69_r <-
    gsub(" NA",
         replacement = "",
         flag_person_wide_flag69$flag69_r)
  
  #產生檢誤報告文字
  flag69_temp <- flag_person_wide_flag69 %>%
    group_by(organization_id) %>%
    mutate(
      flag69_txt = paste(
        "請確認以下人員於教學資料表的「是否擔任學（群）科教學研究委員會召集人」，原則上同科召集人不應有兩人以上：",
        flag69_r,
        sep = ""
      ),
      ""
    ) %>%
    subset(select = c(organization_id, flag69_txt)) %>%
    distinct(organization_id, flag69_txt)
  
  #根據organization_id，展開成寬資料(wide)
  flag69 <- flag69_temp %>%
    dcast(organization_id ~ flag69_txt, value.var = "flag69_txt")
  
  #合併教員資料表及職員(工)資料表報告文字
  temp <- colnames(flag69)[2:length(colnames(flag69))]
  flag69$flag69 <- NA
  for (i in temp) {
    flag69$flag69 <- paste(flag69$flag69, flag69[[i]], sep = "； ")
  }
  flag69$flag69 <- gsub("NA； ", replacement = "", flag69$flag69)
  flag69$flag69 <- gsub("； NA", replacement = "", flag69$flag69)
  
  #產生檢誤報告文字
  flag69 <- flag69 %>%
    subset(select = c(organization_id, flag69)) %>%
    distinct(organization_id, flag69) %>%
    mutate(flag69 = paste(flag69, "", sep = ""))
} else{
  #偵測flag69是否存在。若不存在，則產生NA行
  if ('flag69' %in% ls()) {
    print("flag69")
  } else{
    flag69 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    flag69$flag69 <- ""
  }
}
