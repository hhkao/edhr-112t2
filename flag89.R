# flag89: 專任教師、代理教師原則須具大專以上學歷，請再確認實際情況及所填資料。 -------------------------------------------------------------------
flag_person <- drev_person_1

#專任、代理教師最高學歷是否為大專以上不應為N
flag_person$err_flag <- 0
flag_person$err_flag <- if_else(
  flag_person$degree == "N"
  &
    flag_person$emptype %in% c("專任", "代理", "代理(連)")
  &
    flag_person$sertype == "教師",
  1,
  flag_person$err_flag
)

#呈現姓名
flag_person$err_flag_txt <- ""
flag_person$err_flag_txt <-
  case_when(flag_person$err_flag == 1 ~ flag_person$name,
            TRUE ~ flag_person$err_flag_txt)

if (dim(flag_person %>% subset(err_flag == 1))[1] != 0) {
  #根據organization_id + source，展開成寬資料(wide)
  flag_person_wide_flag89 <- flag_person %>%
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
    colnames(flag_person_wide_flag89)[3:length(colnames(flag_person_wide_flag89))]
  flag_person_wide_flag89$flag89_r <- NA
  for (i in temp) {
    flag_person_wide_flag89$flag89_r <-
      paste(flag_person_wide_flag89$flag89_r,
            flag_person_wide_flag89[[i]],
            sep = " ")
  }
  flag_person_wide_flag89$flag89_r <-
    gsub("NA ",
         replacement = "",
         flag_person_wide_flag89$flag89_r)
  flag_person_wide_flag89$flag89_r <-
    gsub(" NA",
         replacement = "",
         flag_person_wide_flag89$flag89_r)
  
  #產生檢誤報告文字
  flag89_temp <- flag_person_wide_flag89 %>%
    group_by(organization_id) %>%
    mutate(flag89_txt = paste(source, "：", flag89_r, sep = ""), "") %>%
    subset(select = c(organization_id, flag89_txt)) %>%
    distinct(organization_id, flag89_txt)
  
  #根據organization_id，展開成寬資料(wide)
  flag89 <- flag89_temp %>%
    dcast(organization_id ~ flag89_txt, value.var = "flag89_txt")
  
  #合併教員資料表及職員(工)資料表報告文字
  temp <- colnames(flag89)[2:length(colnames(flag89))]
  flag89$flag89 <- NA
  for (i in temp) {
    flag89$flag89 <- paste(flag89$flag89, flag89[[i]], sep = "； ")
  }
  flag89$flag89 <- gsub("NA； ", replacement = "", flag89$flag89)
  flag89$flag89 <- gsub("； NA", replacement = "", flag89$flag89)
  
  #產生檢誤報告文字
  flag89 <- flag89 %>%
    subset(select = c(organization_id, flag89)) %>%
    distinct(organization_id, flag89) %>%
    mutate(flag89 = paste(flag89, "（請再協助確認渠等人員畢業學歷）", sep = ""))
} else{
  #偵測flag89是否存在。若不存在，則產生NA行
  if ('flag89' %in% ls()) {
    print("flag89")
  } else{
    flag89 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    flag89$flag89 <- ""
  }
}
