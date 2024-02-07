# flag74: 「是否擔任各類藝術才能班、體育班、學術性向資賦優異班之召集人」，原則上同班別召集人不會有兩人以上。 -------------------------------------------------------------------
#*若填N、NA、Y而致使重複，則改為不重複(填NA或Y由Flag68處理)
flag_person <- drev_P_load %>%
  subset(load == 1)

flag_person <- flag_person %>%
  subset(!(classleader %in% c("N", "NA", "Y"))) %>%
  select(organization_id,
         edu_name2,
         idnumber,
         name,
         classleader) %>%
  group_by(classleader) %>%
  mutate(index = n()) %>%
  filter(index > 1) %>%
  ungroup() %>%
  mutate(err_flag = 1)

#加註
flag_person$name <-
  paste(flag_person$name,
        "（",
        flag_person$classleader,
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
  flag_person_wide_flag74 <- flag_person %>%
    subset(select = c(organization_id,
                      idnumber,
                      err_flag_txt,
                      edu_name2,
                      err_flag)) %>%
    subset(err_flag == 1) %>%
    dcast(organization_id ~ err_flag_txt, value.var = "err_flag_txt")
  
  #合併所有name
  temp <-
    colnames(flag_person_wide_flag74)[2:length(colnames(flag_person_wide_flag74))]
  flag_person_wide_flag74$flag74_r <- NA
  for (i in temp) {
    flag_person_wide_flag74$flag74_r <-
      paste(flag_person_wide_flag74$flag74_r,
            flag_person_wide_flag74[[i]],
            sep = " ")
  }
  flag_person_wide_flag74$flag74_r <-
    gsub("NA ",
         replacement = "",
         flag_person_wide_flag74$flag74_r)
  flag_person_wide_flag74$flag74_r <-
    gsub(" NA",
         replacement = "",
         flag_person_wide_flag74$flag74_r)
  
  #產生檢誤報告文字
  flag74_temp <- flag_person_wide_flag74 %>%
    group_by(organization_id) %>%
    mutate(
      flag74_txt = paste(
        "請確認以下人員於教學資料表的「是否擔任各類藝術才能班、體育班、學術性向資賦優異班之召集人」，原則上同班別主任（召集人）不應有兩人以上，但請先確認：",
        flag74_r,
        sep = ""
      ),
      ""
    ) %>%
    subset(select = c(organization_id, flag74_txt)) %>%
    distinct(organization_id, flag74_txt)
  
  #根據organization_id，展開成寬資料(wide)
  flag74 <- flag74_temp %>%
    dcast(organization_id ~ flag74_txt, value.var = "flag74_txt")
  
  #合併教員資料表及職員(工)資料表報告文字
  temp <- colnames(flag74)[2:length(colnames(flag74))]
  flag74$flag74 <- NA
  for (i in temp) {
    flag74$flag74 <- paste(flag74$flag74, flag74[[i]], sep = "； ")
  }
  flag74$flag74 <- gsub("NA； ", replacement = "", flag74$flag74)
  flag74$flag74 <- gsub("； NA", replacement = "", flag74$flag74)
  
  #產生檢誤報告文字
  flag74 <- flag74 %>%
    subset(select = c(organization_id, flag74)) %>%
    distinct(organization_id, flag74) %>%
    mutate(flag74 = paste(flag74, "", sep = ""))
} else{
  #偵測flag74是否存在。若不存在，則產生NA行
  if ('flag74' %in% ls()) {
    print("flag74")
  } else{
    flag74 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    flag74$flag74 <- ""
  }
}
