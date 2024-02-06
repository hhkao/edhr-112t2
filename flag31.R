# flag31: 本學期實際教授高級中等學校課程之「教師」、「主任教官」、「教官」或「校長」，主要任教科別不得為「NA」。 -------------------------------------------------------------------
flag_person <- drev_P_load %>%
  subset(load == 1)

flag_person$err_flag <- 0
flag_person$err_flag <- if_else(
  flag_person$sertype %in% c("校長", "教師", "主任教官", "教官") &
    flag_person$mainsub %in% c("NA", "ＮＡ"),
  
  1,
  flag_person$err_flag
)

#加註
flag_person$err_flag_txt <- ""
flag_person$err_flag_txt <- case_when(
  flag_person$err_flag == 1 ~ paste(flag_person$name,
                                    sep = ""),
  TRUE ~ flag_person$err_flag_txt
)

if (dim(flag_person %>% subset(err_flag == 1))[1] != 0) {
  #根據organization_id + source，展開成寬資料(wide)
  flag_person_wide_flag31 <- flag_person %>%
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
    colnames(flag_person_wide_flag31)[3:length(colnames(flag_person_wide_flag31))]
  flag_person_wide_flag31$flag31_r <- NA
  for (i in temp) {
    flag_person_wide_flag31$flag31_r <-
      paste(flag_person_wide_flag31$flag31_r,
            flag_person_wide_flag31[[i]],
            sep = " ")
  }
  flag_person_wide_flag31$flag31_r <-
    gsub("NA ",
         replacement = "",
         flag_person_wide_flag31$flag31_r)
  flag_person_wide_flag31$flag31_r <-
    gsub(" NA",
         replacement = "",
         flag_person_wide_flag31$flag31_r)
  
  #產生檢誤報告文字
  flag31_temp <- flag_person_wide_flag31 %>%
    group_by(organization_id) %>%
    mutate(flag31_txt = paste(flag31_r, sep = ""), "") %>%
    subset(select = c(organization_id, flag31_txt)) %>%
    distinct(organization_id, flag31_txt)
  
  #根據organization_id，展開成寬資料(wide)
  flag31 <- flag31_temp %>%
    dcast(organization_id ~ flag31_txt, value.var = "flag31_txt")
  
  #合併教員資料表及職員(工)資料表報告文字
  temp <- colnames(flag31)[2:length(colnames(flag31))]
  flag31$flag31 <- NA
  for (i in temp) {
    flag31$flag31 <- paste(flag31$flag31, flag31[[i]], sep = "； ")
  }
  flag31$flag31 <- gsub("NA； ", replacement = "", flag31$flag31)
  flag31$flag31 <- gsub("； NA", replacement = "", flag31$flag31)
  
  #產生檢誤報告文字
  flag31 <- flag31 %>%
    subset(select = c(organization_id, flag31)) %>%
    distinct(organization_id, flag31) %>%
    mutate(flag31 = paste("姓名：",
                          flag31,
                          sep = ""))
} else{
  #偵測flag31是否存在。若不存在，則產生NA行
  if ('flag31' %in% ls()) {
    print("flag31")
  } else{
    flag31 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    flag31$flag31 <- ""
  }
}
