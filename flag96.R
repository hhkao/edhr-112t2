# flag96: 校內一級主管（主任）原則由專任教職員擔（兼）任。 -------------------------------------------------------------------
flag_person <- drev_person_1

#職稱為"主任"且聘任類別不為專任(這學期都抓出來，再審酌是否請學校改)
flag_person$err_flag <- 0
flag_person$err_flag <-
  if_else((
    grepl("主任$", flag_person$admintitle0) |
      grepl("主任$", flag_person$admintitle1) |
      grepl("主任$", flag_person$admintitle2) |
      grepl("主任$", flag_person$admintitle3)
  )
  &
    (flag_person$emptype != "專任") ,
  1,
  flag_person$err_flag
  )
#科主任非一級主管
flag_person$err_flag <-
  if_else((
    grepl("科主任$", flag_person$admintitle0) |
      grepl("科主任$", flag_person$admintitle1) |
      grepl("科主任$", flag_person$admintitle2) |
      grepl("科主任$", flag_person$admintitle3)
  ),
  0,
  flag_person$err_flag)

#加註
flag_person$name <-
  if_else(
    grepl("主任$", flag_person$admintitle0) &
      flag_person$emptype != "專任",
    paste(
      flag_person$name,
      "（",
      flag_person$emptype,
      " ",
      flag_person$adminunit0,
      flag_person$admintitle0,
      "）",
      sep = ""
    ),
    flag_person$name
  )
flag_person$name <-
  if_else(
    grepl("主任$", flag_person$admintitle1) &
      flag_person$emptype != "專任",
    paste(
      flag_person$name,
      "（",
      flag_person$emptype,
      " ",
      flag_person$adminunit1,
      flag_person$admintitle1,
      "）",
      sep = ""
    ),
    flag_person$name
  )
flag_person$name <-
  if_else(
    grepl("主任$", flag_person$admintitle2) &
      flag_person$emptype != "專任",
    paste(
      flag_person$name,
      "（",
      flag_person$emptype,
      " ",
      flag_person$adminunit2,
      flag_person$admintitle2,
      "）",
      sep = ""
    ),
    flag_person$name
  )
flag_person$name <-
  if_else(
    grepl("主任$", flag_person$admintitle3) &
      flag_person$emptype != "專任",
    paste(
      flag_person$name,
      "（",
      flag_person$emptype,
      " ",
      flag_person$adminunit3,
      flag_person$admintitle3,
      "）",
      sep = ""
    ),
    flag_person$name
  )
flag_person$name <- gsub("；）", replacement = "）", flag_person$name)
flag_person$name <- gsub("（）", replacement = "", flag_person$name)

#呈現姓名
flag_person$err_flag_txt <- ""
flag_person$err_flag_txt <-
  case_when(flag_person$err_flag == 1 ~ flag_person$name,
            TRUE ~ flag_person$err_flag_txt)

if (dim(flag_person %>% subset(err_flag == 1))[1] != 0) {
  #根據organization_id + source，展開成寬資料(wide)
  flag_person_wide_flag96 <- flag_person %>%
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
    colnames(flag_person_wide_flag96)[3:length(colnames(flag_person_wide_flag96))]
  flag_person_wide_flag96$flag96_r <- NA
  for (i in temp) {
    flag_person_wide_flag96$flag96_r <-
      paste(flag_person_wide_flag96$flag96_r,
            flag_person_wide_flag96[[i]],
            sep = " ")
  }
  flag_person_wide_flag96$flag96_r <-
    gsub("NA ",
         replacement = "",
         flag_person_wide_flag96$flag96_r)
  flag_person_wide_flag96$flag96_r <-
    gsub(" NA",
         replacement = "",
         flag_person_wide_flag96$flag96_r)
  
  #產生檢誤報告文字
  flag96_temp <- flag_person_wide_flag96 %>%
    group_by(organization_id) %>%
    mutate(flag96_txt = paste(source, "：", flag96_r, sep = ""), "") %>%
    subset(select = c(organization_id, flag96_txt)) %>%
    distinct(organization_id, flag96_txt)
  
  #根據organization_id，展開成寬資料(wide)
  flag96 <- flag96_temp %>%
    dcast(organization_id ~ flag96_txt, value.var = "flag96_txt")
  
  #合併教員資料表及職員(工)資料表報告文字
  temp <- colnames(flag96)[2:length(colnames(flag96))]
  flag96$flag96 <- NA
  for (i in temp) {
    flag96$flag96 <- paste(flag96$flag96, flag96[[i]], sep = "； ")
  }
  flag96$flag96 <- gsub("NA； ", replacement = "", flag96$flag96)
  flag96$flag96 <- gsub("； NA", replacement = "", flag96$flag96)
  
  #產生檢誤報告文字
  flag96 <- flag96 %>%
    subset(select = c(organization_id, flag96)) %>%
    distinct(organization_id, flag96) %>%
    mutate(
      flag96 = paste(
        flag96,
        "（校內一級主管（主任）原則由專任教職員擔（兼）任，請協助再確認上述教職員是否擔（兼）任校內一級主管（主任），或協助再確認上述教職員之聘任類別）",
        sep = ""
      )
    )
} else{
  #偵測flag96是否存在。若不存在，則產生NA行
  if ('flag96' %in% ls()) {
    print("flag96")
  } else{
    flag96 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    flag96$flag96 <- ""
  }
}
