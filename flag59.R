# flag59: 校長之聘任類別需為「專任」。-------------------------------------------------------------------
flag_person <- drev_person_1

#校長之聘任類別不為專任 不合理
flag_person$err_flag <- 0
flag_person$err_flag <-
  if_else(flag_person$sertype == "校長" &
            flag_person$emptype != "專任",
          1,
          flag_person$err_flag)
#師大附中及高師大附中校長為兼任
flag_person$err_flag <-
  if_else(
    flag_person$organization_id == "330301" |
      flag_person$organization_id == "580301",
    0,
    flag_person$err_flag
  )

#呈現姓名
flag_person$err_flag_txt <- ""
flag_person$err_flag_txt <-
  case_when(flag_person$err_flag == 1 ~ flag_person$name,
            TRUE ~ flag_person$err_flag_txt)

if (dim(flag_person %>% subset(err_flag == 1))[1] != 0) {
  #根據organization_id + source，展開成寬資料(wide)
  flag_person_wide_flag59 <- flag_person %>%
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
    colnames(flag_person_wide_flag59)[3:length(colnames(flag_person_wide_flag59))]
  flag_person_wide_flag59$flag59_r <- NA
  for (i in temp) {
    flag_person_wide_flag59$flag59_r <-
      paste(flag_person_wide_flag59$flag59_r,
            flag_person_wide_flag59[[i]],
            sep = " ")
  }
  flag_person_wide_flag59$flag59_r <-
    gsub("NA ",
         replacement = "",
         flag_person_wide_flag59$flag59_r)
  flag_person_wide_flag59$flag59_r <-
    gsub(" NA",
         replacement = "",
         flag_person_wide_flag59$flag59_r)
  
  #產生檢誤報告文字
  flag59_temp <- flag_person_wide_flag59 %>%
    group_by(organization_id) %>%
    mutate(flag59_txt = paste("校長之聘任類別需為「專任」。", sep = ""), "") %>%
    subset(select = c(organization_id, flag59_txt)) %>%
    distinct(organization_id, flag59_txt)
  
  #根據organization_id，展開成寬資料(wide)
  flag59 <- flag59_temp %>%
    dcast(organization_id ~ flag59_txt, value.var = "flag59_txt")
  
  #合併教員資料表及職員(工)資料表報告文字
  temp <- colnames(flag59)[2:length(colnames(flag59))]
  flag59$flag59 <- NA
  for (i in temp) {
    flag59$flag59 <- paste(flag59$flag59, flag59[[i]], sep = "； ")
  }
  flag59$flag59 <- gsub("NA； ", replacement = "", flag59$flag59)
  flag59$flag59 <- gsub("； NA", replacement = "", flag59$flag59)
  
  #產生檢誤報告文字
  flag59 <- flag59 %>%
    subset(select = c(organization_id, flag59)) %>%
    distinct(organization_id, flag59) %>%
    mutate(flag59 = paste(flag59, "", sep = ""))
} else{
  #偵測flag59是否存在。若不存在，則產生NA行
  if ('flag59' %in% ls()) {
    print("flag59")
  } else{
    flag59 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    flag59$flag59 <- ""
  }
}
