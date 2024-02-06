# flag68: 「是否擔任學（群）科教學研究委員會召集人」、「是否擔任各類藝術才能班、體育班、學術性向資賦優異班之召集人」，須填入其擔任召集人之學科完整名稱或N。 -------------------------------------------------------------------
flag_person <- drev_P_load %>%
  subset(load == 1)

flag_person$err_flag1 <- 0
flag_person$err_flag1 <-
  if_else(grepl("Y", flag_person$mitleader), 1, flag_person$err_flag1)
flag_person$err_flag1 <-
  if_else(grepl("NA", flag_person$mitleader), 1, flag_person$err_flag1)
flag_person$err_flag1 <-
  if_else(grepl("無", flag_person$mitleader), 1, flag_person$err_flag1)
flag_person$err_flag1 <-
  if_else(grepl("有", flag_person$mitleader), 1, flag_person$err_flag1)
flag_person$err_flag1 <-
  if_else(grepl("沒有", flag_person$mitleader), 1, flag_person$err_flag1)
flag_person$err_flag2 <- 0
flag_person$err_flag2 <-
  if_else(grepl("Y", flag_person$classleader), 1, flag_person$err_flag2)
flag_person$err_flag2 <-
  if_else(grepl("NA", flag_person$classleader),
          1,
          flag_person$err_flag2)
flag_person$err_flag2 <-
  if_else(grepl("無", flag_person$classleader), 1, flag_person$err_flag2)
flag_person$err_flag2 <-
  if_else(grepl("有", flag_person$classleader), 1, flag_person$err_flag2)
flag_person$err_flag2 <-
  if_else(grepl("沒有", flag_person$classleader),
          1,
          flag_person$err_flag2)

flag_person$err_flag_68 <-
  flag_person$err_flag1 + flag_person$err_flag2

flag_person$err_flag <- 0
flag_person$err_flag <-
  if_else(flag_person$err_flag_68 != 0, 1, flag_person$err_flag)

#加註
flag_person$name <- paste(flag_person$name, "（", sep = "")
flag_person$name <-
  if_else(
    flag_person$err_flag1 != 0,
    paste(
      flag_person$name,
      "是否擔任學（群）科教學研究委員會召集人：",
      flag_person$mitleader,
      "；",
      sep = ""
    ),
    flag_person$name
  )
flag_person$name <-
  if_else(
    flag_person$err_flag2 != 0,
    paste(
      flag_person$name,
      "是否擔任各類藝術才能班、體育班、學術性向資賦優異班之召集人：",
      flag_person$classleader,
      "；",
      sep = ""
    ),
    flag_person$name
  )
flag_person$name <- paste(flag_person$name, "）", sep = "")
flag_person$name <- gsub("；）", replacement = "）", flag_person$name)
flag_person$name <- gsub("（）", replacement = "", flag_person$name)

#呈現姓名
flag_person$err_flag_txt <- ""
flag_person$err_flag_txt <-
  case_when(flag_person$err_flag == 1 ~ flag_person$name,
            TRUE ~ flag_person$err_flag_txt)

if (dim(flag_person %>% subset(err_flag == 1))[1] != 0) {
  #根據organization_id + source，展開成寬資料(wide)
  flag_person_wide_flag68 <- flag_person %>%
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
    colnames(flag_person_wide_flag68)[3:length(colnames(flag_person_wide_flag68))]
  flag_person_wide_flag68$flag68_r <- NA
  for (i in temp) {
    flag_person_wide_flag68$flag68_r <-
      paste(flag_person_wide_flag68$flag68_r,
            flag_person_wide_flag68[[i]],
            sep = " ")
  }
  flag_person_wide_flag68$flag68_r <-
    gsub("NA ",
         replacement = "",
         flag_person_wide_flag68$flag68_r)
  flag_person_wide_flag68$flag68_r <-
    gsub(" NA",
         replacement = "",
         flag_person_wide_flag68$flag68_r)
  
  #產生檢誤報告文字
  flag68_temp <- flag_person_wide_flag68 %>%
    group_by(organization_id) %>%
    mutate(flag68_txt = paste(flag68_r, sep = ""), "") %>%
    subset(select = c(organization_id, flag68_txt)) %>%
    distinct(organization_id, flag68_txt)
  
  #根據organization_id，展開成寬資料(wide)
  flag68 <- flag68_temp %>%
    dcast(organization_id ~ flag68_txt, value.var = "flag68_txt")
  
  #合併教員資料表及職員(工)資料表報告文字
  temp <- colnames(flag68)[2:length(colnames(flag68))]
  flag68$flag68 <- NA
  for (i in temp) {
    flag68$flag68 <- paste(flag68$flag68, flag68[[i]], sep = "； ")
  }
  flag68$flag68 <- gsub("NA； ", replacement = "", flag68$flag68)
  flag68$flag68 <- gsub("； NA", replacement = "", flag68$flag68)
  
  #產生檢誤報告文字
  flag68 <- flag68 %>%
    subset(select = c(organization_id, flag68)) %>%
    distinct(organization_id, flag68) %>%
    mutate(flag68 = paste("姓名：",
                          flag68,
                          "（請依欄位說明填寫學科或班別名稱）",
                          sep = ""))
} else{
  #偵測flag68是否存在。若不存在，則產生NA行
  if ('flag68' %in% ls()) {
    print("flag68")
  } else{
    flag68 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    flag68$flag68 <- ""
  }
}
