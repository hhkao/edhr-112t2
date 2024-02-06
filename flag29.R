# flag29: 教學資料表的姓名應為中文或英文，不得有亂碼。 -------------------------------------------------------------------
#flag29_append-------------------------------------------------------------------
flag_person <- drev_P_load %>%
  subset(load == 1)

#檢視姓名欄位字元數不為3
view_flag29 <- distinct(flag_person, name, .keep_all = TRUE) %>%
  subset(nchar(name) != 3) %>%
  subset(select = c(organization_id, idnumber, name, edu_name2, source))

#數字、特殊符號標記為1(不包含．)
flag_person$err_flag <-
  grepl("\\d|[[:punct:]&&[^.]]", flag_person$name) %>% as.integer()

if (dim(flag_person %>% subset(err_flag == 1))[1] != 0) {
  #根據organization_id + source，展開成寬資料(wide)
  flag_person_wide_flag29 <- flag_person %>%
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
    colnames(flag_person_wide_flag29)[3:length(colnames(flag_person_wide_flag29))]
  flag_person_wide_flag29$flag29_r <- NA
  for (i in temp) {
    flag_person_wide_flag29$flag29_r <-
      paste(flag_person_wide_flag29$flag29_r,
            flag_person_wide_flag29[[i]],
            sep = " ")
  }
  flag_person_wide_flag29$flag29_r <-
    gsub("NA ", replacement = "", flag_person_wide_flag29$flag29_r)
  flag_person_wide_flag29$flag29_r <-
    gsub(" NA", replacement = "", flag_person_wide_flag29$flag29_r)
  
  #產生檢誤報告文字
  flag29_temp <- flag_person_wide_flag29 %>%
    group_by(organization_id) %>%
    mutate(flag29_txt = paste(source, "需修改姓名處：", flag29_r, sep = ""), "") %>%
    subset(select = c(organization_id, flag29_txt)) %>%
    distinct(organization_id, flag29_txt)
  
  #根據organization_id，展開成寬資料(wide)
  flag29 <- flag29_temp %>%
    dcast(organization_id ~ flag29_txt, value.var = "flag29_txt")
  
  #合併教員資料表及職員(工)資料表報告文字
  temp <- colnames(flag29)[2:length(colnames(flag29))]
  flag29$flag29 <- NA
  for (i in temp) {
    flag29$flag29 <- paste(flag29$flag29, flag29[[i]], sep = "； ")
  }
  flag29$flag29 <- gsub("NA； ", replacement = "", flag29$flag29)
  flag29$flag29 <- gsub("； NA", replacement = "", flag29$flag29)
  
  #產生檢誤報告文字
  flag29 <- flag29 %>%
    subset(select = c(organization_id, flag29)) %>%
    distinct(organization_id, flag29)
} else{
  #偵測flag29是否存在。若不存在，則產生NA行
  if ('flag29' %in% ls()) {
    print("flag29")
  } else{
    flag29 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    flag29$flag29 <- ""
  }
}
