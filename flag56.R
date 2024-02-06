# flag56: 教師、主任教官、教官的「每週基本教學節數」、「減授節數」不可填NA。 -------------------------------------------------------------------
flag_person <- drev_P_load %>%
  subset(load == 1)

flag_person$err_basic <- 0
flag_person$err_basic <- if_else(
  flag_person$sertype %in% c("教師", "主任教官", "教官") &
    flag_person$basic %>% is.na(),
  1,
  flag_person$err_basic
)

flag_person$err_cut <- 0
flag_person$err_cut <- if_else(
  flag_person$sertype %in% c("教師", "主任教官", "教官") &
    flag_person$cut %>% is.na(),
  1,
  flag_person$err_cut
)

flag_person$err_othertime <- 0
flag_person$err_othertime <- if_else(
  flag_person$sertype %in% c("教師", "主任教官", "教官") &
    flag_person$othertime %>% is.na(),
  1,
  flag_person$err_othertime
)

flag_person$err_flag_56 <-
  flag_person$err_basic + flag_person$err_cut + flag_person$err_othertime

flag_person$err_flag <- 0
flag_person$err_flag <-
  if_else(flag_person$err_flag_56 != 0, 1, flag_person$err_flag)


#加註
flag_person$name <- paste(flag_person$name, "（", sep = "")
flag_person$name <-
  if_else(
    flag_person$err_basic != 0,
    paste(flag_person$name,
          "每週基本教學節數：NA",
          "；",
          sep = ""),
    flag_person$name
  )
flag_person$name <-
  if_else(
    flag_person$err_cut != 0,
    paste(flag_person$name,
          "減授節數：NA",
          "；",
          sep = ""),
    flag_person$name
  )
flag_person$name <-
  if_else(
    flag_person$err_othertime != 0,
    paste(flag_person$name,
          "校外兼課總節數：NA",
          "；",
          sep = ""),
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
  flag_person_wide_flag56 <- flag_person %>%
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
    colnames(flag_person_wide_flag56)[3:length(colnames(flag_person_wide_flag56))]
  flag_person_wide_flag56$flag56_r <- NA
  for (i in temp) {
    flag_person_wide_flag56$flag56_r <-
      paste(flag_person_wide_flag56$flag56_r,
            flag_person_wide_flag56[[i]],
            sep = " ")
  }
  flag_person_wide_flag56$flag56_r <-
    gsub("NA ",
         replacement = "",
         flag_person_wide_flag56$flag56_r)
  flag_person_wide_flag56$flag56_r <-
    gsub(" NA",
         replacement = "",
         flag_person_wide_flag56$flag56_r)
  
  #產生檢誤報告文字
  flag56_temp <- flag_person_wide_flag56 %>%
    group_by(organization_id) %>%
    mutate(flag56_txt = paste(flag56_r, sep = ""), "") %>%
    subset(select = c(organization_id, flag56_txt)) %>%
    distinct(organization_id, flag56_txt)
  
  #根據organization_id，展開成寬資料(wide)
  flag56 <- flag56_temp %>%
    dcast(organization_id ~ flag56_txt, value.var = "flag56_txt")
  
  #合併教員資料表及職員(工)資料表報告文字
  temp <- colnames(flag56)[2:length(colnames(flag56))]
  flag56$flag56 <- NA
  for (i in temp) {
    flag56$flag56 <- paste(flag56$flag56, flag56[[i]], sep = "； ")
  }
  flag56$flag56 <- gsub("NA； ", replacement = "", flag56$flag56)
  flag56$flag56 <- gsub("； NA", replacement = "", flag56$flag56)
  
  #產生檢誤報告文字
  flag56 <- flag56 %>%
    subset(select = c(organization_id, flag56)) %>%
    distinct(organization_id, flag56) %>%
    mutate(flag56 = paste(flag56,
                          "（或併請人事單位確認）",
                          sep = ""))
} else{
  #偵測flag56是否存在。若不存在，則產生NA行
  if ('flag56' %in% ls()) {
    print("flag56")
  } else{
    flag56 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    flag56$flag56 <- ""
  }
}
