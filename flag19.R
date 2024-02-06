# flag19: 填寫外來人口統一證號者，國籍別應非「本國籍」。 -------------------------------------------------------------------
flag_person <- drev_person_1

#外來人口統一證號：第二碼為A B C D 8 9
flag_person$err_flag <- 0
flag_person$err_flag <-
  if_else(
    grepl("^[^\\s][ABCD89][^\\s]+$", flag_person$idnumber) &
      (flag_person$nation %in% c("本國籍", "本國", "臺灣", "台灣", "中華民國")),
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
  flag_person_wide_flag19 <- flag_person %>%
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
    colnames(flag_person_wide_flag19)[3:length(colnames(flag_person_wide_flag19))]
  flag_person_wide_flag19$flag19_r <- NA
  for (i in temp) {
    flag_person_wide_flag19$flag19_r <-
      paste(flag_person_wide_flag19$flag19_r,
            flag_person_wide_flag19[[i]],
            sep = " ")
  }
  flag_person_wide_flag19$flag19_r <-
    gsub("NA ",
         replacement = "",
         flag_person_wide_flag19$flag19_r)
  flag_person_wide_flag19$flag19_r <-
    gsub(" NA",
         replacement = "",
         flag_person_wide_flag19$flag19_r)
  
  #產生檢誤報告文字
  flag19_temp <- flag_person_wide_flag19 %>%
    group_by(organization_id) %>%
    mutate(flag19_txt = paste(source, "：", flag19_r, sep = ""), "") %>%
    subset(select = c(organization_id, flag19_txt)) %>%
    distinct(organization_id, flag19_txt)
  
  #根據organization_id，展開成寬資料(wide)
  flag19 <- flag19_temp %>%
    dcast(organization_id ~ flag19_txt, value.var = "flag19_txt")
  
  #合併教員資料表及職員(工)資料表報告文字
  temp <- colnames(flag19)[2:length(colnames(flag19))]
  flag19$flag19 <- NA
  for (i in temp) {
    flag19$flag19 <- paste(flag19$flag19, flag19[[i]], sep = "； ")
  }
  flag19$flag19 <- gsub("NA； ", replacement = "", flag19$flag19)
  flag19$flag19 <- gsub("； NA", replacement = "", flag19$flag19)
  
  #產生檢誤報告文字
  flag19 <- flag19 %>%
    subset(select = c(organization_id, flag19)) %>%
    distinct(organization_id, flag19) %>%
    mutate(flag19 = paste(flag19, "（請確認且修正該員所屬國籍別）", sep = ""))
} else{
  #偵測flag19是否存在。若不存在，則產生NA行
  if ('flag19' %in% ls()) {
    print("flag19")
  } else{
    flag19 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    flag19$flag19 <- ""
  }
}
