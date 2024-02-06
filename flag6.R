# flag6: 人事資料表的姓名應為中文或英文，不得有亂碼。 -------------------------------------------------------------------
#flag6_append-------------------------------------------------------------------
flag_person <- drev_person_1

#檢視姓名欄位字元數不為3
view_flag6 <- distinct(flag_person, name, .keep_all = TRUE) %>%
  subset(nchar(name) != 3) %>%
  subset(select = c(organization_id, idnumber, name, edu_name2, source))

#數字、特殊符號標記為1(不包含．)
flag_person$err_flag <-
  grepl("\\d|[[:punct:]&&[^.]]", flag_person$name) %>% as.integer()

if (dim(flag_person %>% subset(err_flag == 1))[1] != 0) {
  #根據organization_id + source，展開成寬資料(wide)
  flag_person_wide_flag6 <- flag_person %>%
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
    colnames(flag_person_wide_flag6)[3:length(colnames(flag_person_wide_flag6))]
  flag_person_wide_flag6$flag6_r <- NA
  for (i in temp) {
    flag_person_wide_flag6$flag6_r <-
      paste(flag_person_wide_flag6$flag6_r,
            flag_person_wide_flag6[[i]],
            sep = " ")
  }
  flag_person_wide_flag6$flag6_r <-
    gsub("NA ", replacement = "", flag_person_wide_flag6$flag6_r)
  flag_person_wide_flag6$flag6_r <-
    gsub(" NA", replacement = "", flag_person_wide_flag6$flag6_r)
  
  #產生檢誤報告文字
  flag6_temp <- flag_person_wide_flag6 %>%
    group_by(organization_id) %>%
    mutate(flag6_txt = paste(source, "需修改姓名處：", flag6_r, sep = ""), "") %>%
    subset(select = c(organization_id, flag6_txt)) %>%
    distinct(organization_id, flag6_txt)
  
  #根據organization_id，展開成寬資料(wide)
  flag6 <- flag6_temp %>%
    dcast(organization_id ~ flag6_txt, value.var = "flag6_txt")
  
  #合併教員資料表及職員(工)資料表報告文字
  temp <- colnames(flag6)[2:length(colnames(flag6))]
  flag6$flag6 <- NA
  for (i in temp) {
    flag6$flag6 <- paste(flag6$flag6, flag6[[i]], sep = "； ")
  }
  flag6$flag6 <- gsub("NA； ", replacement = "", flag6$flag6)
  flag6$flag6 <- gsub("； NA", replacement = "", flag6$flag6)
  
  #產生檢誤報告文字
  flag6 <- flag6 %>%
    subset(select = c(organization_id, flag6)) %>%
    distinct(organization_id, flag6)
} else{
  #偵測flag6是否存在。若不存在，則產生NA行
  if ('flag6' %in% ls()) {
    print("flag6")
  } else{
    flag6 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    flag6$flag6 <- ""
  }
}
