# flag8: 國籍別應填入「本國籍」或者外交部網站之世界各國名稱一覽表的國家名稱（或者至少須足以辨識國家）。 -------------------------------------------------------------------
#flag8_append-------------------------------------------------------------------
flag_person <- drev_person_1

#檢視國籍別欄位字元數不為3
view_flag8 <- distinct(flag_person, nation, .keep_all = TRUE) %>%
  subset(nchar(nation) != 3) %>%
  subset(select = c(organization_id, idnumber, nation, edu_name2, source))

#不合理的情形標記為1
flag_person$err_flag <- case_when(
  flag_person$nation == "外籍" ~ 1,
  flag_person$nation == "外國籍" ~ 1,
  flag_person$nation == "國外" ~ 1,
  flag_person$nation == "外國" ~ 1,
  flag_person$nation == "國內" ~ 1,
  grepl("雙重", flag_person$nation) ~ 1,
  flag_person$nation == "N" ~ 1,
  TRUE ~ 0
)

#flag98比對用
flag_person_flag8 <- flag_person %>%
  subset(err_flag == 1) %>%
  select("idnumber", "err_flag")

#加註
flag_person$name <-
  paste(flag_person$name, "（", flag_person$nation, "）", sep = "")
flag_person$name <- gsub("；）", replacement = "）", flag_person$name)
flag_person$name <- gsub("（）", replacement = "", flag_person$name)


if (dim(flag_person %>% subset(err_flag == 1))[1] != 0) {
  #根據organization_id + source，展開成寬資料(wide)
  flag_person_wide_flag8 <- flag_person %>%
    subset(select = c(organization_id,
                      idnumber,
                      name,
                      edu_name2,
                      source,
                      err_flag)) %>%
    subset(err_flag == 1) %>%
    dcast(organization_id + source ~ name, value.var = "name")
  
  #合併所有name
  temp <-
    colnames(flag_person_wide_flag8)[3:length(colnames(flag_person_wide_flag8))]
  flag_person_wide_flag8$flag8_r <- NA
  for (i in temp) {
    flag_person_wide_flag8$flag8_r <-
      paste(flag_person_wide_flag8$flag8_r,
            flag_person_wide_flag8[[i]],
            sep = " ")
  }
  flag_person_wide_flag8$flag8_r <-
    gsub("NA ", replacement = "", flag_person_wide_flag8$flag8_r)
  flag_person_wide_flag8$flag8_r <-
    gsub(" NA", replacement = "", flag_person_wide_flag8$flag8_r)
  
  #產生檢誤報告文字
  flag8_temp <- flag_person_wide_flag8 %>%
    group_by(organization_id) %>%
    mutate(flag8_txt = paste(source, "需修改「國籍別」：", flag8_r, sep = ""), "") %>%
    subset(select = c(organization_id, flag8_txt)) %>%
    distinct(organization_id, flag8_txt)
  
  #根據organization_id，展開成寬資料(wide)
  flag8 <- flag8_temp %>%
    dcast(organization_id ~ flag8_txt, value.var = "flag8_txt")
  
  #合併教員資料表及職員(工)資料表報告文字
  temp <- colnames(flag8)[2:length(colnames(flag8))]
  flag8$flag8 <- NA
  for (i in temp) {
    flag8$flag8 <- paste(flag8$flag8, flag8[[i]], sep = "； ")
  }
  flag8$flag8 <- gsub("NA； ", replacement = "", flag8$flag8)
  flag8$flag8 <- gsub("； NA", replacement = "", flag8$flag8)
  
  #產生檢誤報告文字
  flag8 <- flag8 %>%
    subset(select = c(organization_id, flag8)) %>%
    distinct(organization_id, flag8) %>%
    mutate(flag8 = paste(flag8,
                         "（請填寫正確國家名稱）",
                         sep = ""))
} else{
  #偵測flag8是否存在。若不存在，則產生NA行
  if ('flag8' %in% ls()) {
    print("flag8")
  } else{
    flag8 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    flag8$flag8 <- ""
  }
}
