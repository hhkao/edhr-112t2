# spe5: 教職員工畢業學校若為(科技)大學或(技術)學院，學歷資訊原則於「學士」、「碩士」或「博士」學歷欄位填列，而非「副學士」。 -------------------------------------------------------------------
flag_person <- drev_person_1

#副學士學位畢業學校名稱不可出現"大學"及"學院"，可出現"專科學校"
flag_person$err_flag_adegreeu1 <- 0
flag_person$err_flag_adegreeu2 <- 0
flag_person$err_flag_adegreeu1 <-
  if_else(grepl("大學", flag_person$adegreeu1),
          1,
          flag_person$err_flag_adegreeu1)
flag_person$err_flag_adegreeu1 <-
  if_else(grepl("學院", flag_person$adegreeu1),
          1,
          flag_person$err_flag_adegreeu1)
flag_person$err_flag_adegreeu1 <-
  if_else(grepl("科大", flag_person$adegreeu1),
          1,
          flag_person$err_flag_adegreeu1)
flag_person$err_flag_adegreeu2 <-
  if_else(grepl("大學", flag_person$adegreeu2),
          1,
          flag_person$err_flag_adegreeu2)
flag_person$err_flag_adegreeu2 <-
  if_else(grepl("學院", flag_person$adegreeu2),
          1,
          flag_person$err_flag_adegreeu2)
flag_person$err_flag_adegreeu2 <-
  if_else(grepl("科大", flag_person$adegreeu2),
          1,
          flag_person$err_flag_adegreeu2)
#達姆施塔特工業大學（德語：Technische Universitat Darmstadt），是德國歷史悠久的理工大學
flag_person$err_flag_adegreeu2 <-
  if_else(grepl("Darmstadt$", flag_person$adegreeu2),
          1,
          flag_person$err_flag_adegreeu2)
#副學士的情況
flag_person$err_flag_adegreeu1 <-
  if_else(grepl("專科", flag_person$adegreeu1),
          0,
          flag_person$err_flag_adegreeu1)
flag_person$err_flag_adegreeu1 <-
  if_else(grepl("二專", flag_person$adegreeu1),
          0,
          flag_person$err_flag_adegreeu1)
flag_person$err_flag_adegreeu1 <-
  if_else(grepl("二年制", flag_person$adegreeu1),
          0,
          flag_person$err_flag_adegreeu1)
flag_person$err_flag_adegreeu1 <-
  if_else(grepl("五專", flag_person$adegreeu1),
          0,
          flag_person$err_flag_adegreeu1)
flag_person$err_flag_adegreeu1 <-
  if_else(grepl("五年制", flag_person$adegreeu1),
          0,
          flag_person$err_flag_adegreeu1)
flag_person$err_flag_adegreeu1 <-
  if_else(grepl("商專", flag_person$adegreeu1),
          0,
          flag_person$err_flag_adegreeu1)
flag_person$err_flag_adegreeu1 <-
  if_else(grepl("農專", flag_person$adegreeu1),
          0,
          flag_person$err_flag_adegreeu1)
flag_person$err_flag_adegreeu1 <-
  if_else(grepl("空專", flag_person$adegreeu1),
          0,
          flag_person$err_flag_adegreeu1)
flag_person$err_flag_adegreeu1 <-
  if_else(grepl("三專", flag_person$adegreeu1),
          0,
          flag_person$err_flag_adegreeu1)
flag_person$err_flag_adegreeu1 <-
  if_else(grepl("護專", flag_person$adegreeu1),
          0,
          flag_person$err_flag_adegreeu1)
flag_person$err_flag_adegreeu2 <-
  if_else(grepl("專科", flag_person$adegreeu2),
          0,
          flag_person$err_flag_adegreeu2)
flag_person$err_flag_adegreeu2 <-
  if_else(grepl("二專", flag_person$adegreeu2),
          0,
          flag_person$err_flag_adegreeu2)
flag_person$err_flag_adegreeu2 <-
  if_else(grepl("二年制", flag_person$adegreeu2),
          0,
          flag_person$err_flag_adegreeu2)
flag_person$err_flag_adegreeu2 <-
  if_else(grepl("五專", flag_person$adegreeu2),
          0,
          flag_person$err_flag_adegreeu2)
flag_person$err_flag_adegreeu2 <-
  if_else(grepl("五年制", flag_person$adegreeu2),
          0,
          flag_person$err_flag_adegreeu2)
flag_person$err_flag_adegreeu2 <-
  if_else(grepl("商專", flag_person$adegreeu2),
          0,
          flag_person$err_flag_adegreeu2)
flag_person$err_flag_adegreeu2 <-
  if_else(grepl("農專", flag_person$adegreeu2),
          0,
          flag_person$err_flag_adegreeu2)
flag_person$err_flag_adegreeu2 <-
  if_else(grepl("空專", flag_person$adegreeu2),
          0,
          flag_person$err_flag_adegreeu2)
flag_person$err_flag_adegreeu2 <-
  if_else(grepl("三專", flag_person$adegreeu2),
          0,
          flag_person$err_flag_adegreeu2)
flag_person$err_flag_adegreeu2 <-
  if_else(grepl("護專", flag_person$adegreeu2),
          0,
          flag_person$err_flag_adegreeu2)

flag_person$err_flag <-
  flag_person$err_flag_adegreeu1 + flag_person$err_flag_adegreeu2

#加註學士學位畢業學校名稱
flag_person$err_flag_txt <- ""
flag_person$err_flag_txt <- case_when(
  flag_person$err_flag_adegreeu1 == 1 ~ paste(
    flag_person$name,
    "（副學士學位畢業學校（一）：",
    flag_person$adegreeu1,
    "）",
    sep = ""
  ),
  flag_person$err_flag_adegreeu2 == 1 ~ paste(
    flag_person$name,
    "（副學士學位畢業學校（二）：",
    flag_person$adegreeu2,
    "）",
    sep = ""
  ),
  TRUE ~ flag_person$err_flag_txt
)

if (dim(flag_person %>% subset(err_flag == 1))[1] != 0) {
  #根據organization_id + source，展開成寬資料(wide)
  flag_person_wide_spe5 <- flag_person %>%
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
    colnames(flag_person_wide_spe5)[3:length(colnames(flag_person_wide_spe5))]
  flag_person_wide_spe5$spe5_r <- NA
  for (i in temp) {
    flag_person_wide_spe5$spe5_r <-
      paste(flag_person_wide_spe5$spe5_r,
            flag_person_wide_spe5[[i]],
            sep = " ")
  }
  flag_person_wide_spe5$spe5_r <-
    gsub("NA ", replacement = "", flag_person_wide_spe5$spe5_r)
  flag_person_wide_spe5$spe5_r <-
    gsub(" NA", replacement = "", flag_person_wide_spe5$spe5_r)
  
  #產生檢誤報告文字
  spe5_temp <- flag_person_wide_spe5 %>%
    group_by(organization_id) %>%
    mutate(spe5_txt = paste(source, "：", spe5_r, sep = ""), "") %>%
    subset(select = c(organization_id, spe5_txt)) %>%
    distinct(organization_id, spe5_txt)
  
  #根據organization_id，展開成寬資料(wide)
  spe5 <- spe5_temp %>%
    dcast(organization_id ~ spe5_txt, value.var = "spe5_txt")
  
  #合併教員資料表及職員(工)資料表報告文字
  temp <- colnames(spe5)[2:length(colnames(spe5))]
  spe5$spe5 <- NA
  for (i in temp) {
    spe5$spe5 <- paste(spe5$spe5, spe5[[i]], sep = "； ")
  }
  spe5$spe5 <- gsub("NA； ", replacement = "", spe5$spe5)
  spe5$spe5 <- gsub("； NA", replacement = "", spe5$spe5)
  
  #產生檢誤報告文字
  spe5 <- spe5 %>%
    subset(select = c(organization_id, spe5)) %>%
    distinct(organization_id, spe5) %>%
    mutate(
      spe5 = paste(
        spe5,
        "（請務必確認以上人員畢業證書所載學位別。若副學士或專科畢業學校為(科技/空中)大學、(技術)學院或其他技職校院，且確認為專科學制，請於「副學士或專科畢業學校」欄位中在校名後註記專科學制或專科部）",
        sep = ""
      )
    )
} else{
  #偵測spe5是否存在。若不存在，則產生NA行
  if ('spe5' %in% ls()) {
    print("spe5")
  } else{
    spe5 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    spe5$spe5 <- ""
  }
}
