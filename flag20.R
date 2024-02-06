# flag20: 教職員工畢業學校若為專科學校，學歷資訊應於「副學士」畢業學校欄位填列。 -------------------------------------------------------------------
flag_person <- drev_person_1

#學士學位畢業學校名稱不可出現"專科學校"
flag_person$err_flag_bdegreeu1 <- 0
flag_person$err_flag_bdegreeu1 <-
  if_else(grepl("專科", flag_person$bdegreeu1),
          1,
          flag_person$err_flag_bdegreeu1)
flag_person$err_flag_bdegreeu1 <-
  if_else(grepl("二專", flag_person$bdegreeu1),
          1,
          flag_person$err_flag_bdegreeu1)
flag_person$err_flag_bdegreeu1 <-
  if_else(grepl("五專", flag_person$bdegreeu1),
          1,
          flag_person$err_flag_bdegreeu1)
flag_person$err_flag_bdegreeu1 <-
  if_else(grepl("海專", flag_person$bdegreeu1),
          1,
          flag_person$err_flag_bdegreeu1)
flag_person$err_flag_bdegreeu1 <-
  if_else(grepl("工專", flag_person$bdegreeu1),
          1,
          flag_person$err_flag_bdegreeu1)
flag_person$err_flag_bdegreeu1 <-
  if_else(grepl("商專", flag_person$bdegreeu1),
          1,
          flag_person$err_flag_bdegreeu1)
flag_person$err_flag_bdegreeu1 <-
  if_else(grepl("藝專", flag_person$bdegreeu1),
          1,
          flag_person$err_flag_bdegreeu1)
flag_person$err_flag_bdegreeu1 <-
  if_else(grepl("農專", flag_person$bdegreeu1),
          1,
          flag_person$err_flag_bdegreeu1)
flag_person$err_flag_bdegreeu1 <-
  if_else(grepl("護專", flag_person$bdegreeu1),
          1,
          flag_person$err_flag_bdegreeu1)
flag_person$err_flag_bdegreeu1 <-
  if_else(grepl("家專", flag_person$bdegreeu1),
          1,
          flag_person$err_flag_bdegreeu1)
flag_person$err_flag_bdegreeu1 <-
  if_else(grepl("行專", flag_person$bdegreeu1),
          1,
          flag_person$err_flag_bdegreeu1)
flag_person$err_flag_bdegreeu1 <-
  if_else(grepl("師專", flag_person$bdegreeu1),
          1,
          flag_person$err_flag_bdegreeu1)
flag_person$err_flag_bdegreeu1 <-
  if_else(grepl("藥專", flag_person$bdegreeu1),
          1,
          flag_person$err_flag_bdegreeu1)
flag_person$err_flag_bdegreeu1 <-
  if_else(
    grepl("^台南家專學校財團法人台南應用科技大學$", flag_person$bdegreeu1),
    0,
    flag_person$err_flag_bdegreeu1
  )
#陸軍官校專科班為學士學位
flag_person$err_flag_bdegreeu1 <-
  if_else(grepl("^陸軍官校專科班$", flag_person$bdegreeu1),
          0,
          flag_person$err_flag_bdegreeu1)
flag_person$err_flag_bdegreeu1 <-
  if_else(grepl("^陸軍官校大學部$", flag_person$bdegreeu1),
          0,
          flag_person$err_flag_bdegreeu1)

flag_person$err_flag_bdegreeu2 <- 0
flag_person$err_flag_bdegreeu2 <-
  if_else(grepl("專科", flag_person$bdegreeu2),
          1,
          flag_person$err_flag_bdegreeu2)
flag_person$err_flag_bdegreeu2 <-
  if_else(grepl("二專", flag_person$bdegreeu2),
          1,
          flag_person$err_flag_bdegreeu2)
flag_person$err_flag_bdegreeu2 <-
  if_else(grepl("五專", flag_person$bdegreeu2),
          1,
          flag_person$err_flag_bdegreeu2)
flag_person$err_flag_bdegreeu2 <-
  if_else(grepl("海專", flag_person$bdegreeu2),
          1,
          flag_person$err_flag_bdegreeu2)
flag_person$err_flag_bdegreeu2 <-
  if_else(grepl("工專", flag_person$bdegreeu2),
          1,
          flag_person$err_flag_bdegreeu2)
flag_person$err_flag_bdegreeu2 <-
  if_else(grepl("商專", flag_person$bdegreeu2),
          1,
          flag_person$err_flag_bdegreeu2)
flag_person$err_flag_bdegreeu2 <-
  if_else(grepl("藝專", flag_person$bdegreeu2),
          1,
          flag_person$err_flag_bdegreeu2)
flag_person$err_flag_bdegreeu2 <-
  if_else(grepl("農專", flag_person$bdegreeu2),
          1,
          flag_person$err_flag_bdegreeu2)
flag_person$err_flag_bdegreeu2 <-
  if_else(grepl("護專", flag_person$bdegreeu2),
          1,
          flag_person$err_flag_bdegreeu2)
flag_person$err_flag_bdegreeu2 <-
  if_else(grepl("家專", flag_person$bdegreeu2),
          1,
          flag_person$err_flag_bdegreeu2)
flag_person$err_flag_bdegreeu2 <-
  if_else(grepl("行專", flag_person$bdegreeu2),
          1,
          flag_person$err_flag_bdegreeu2)
flag_person$err_flag_bdegreeu2 <-
  if_else(grepl("師專", flag_person$bdegreeu2),
          1,
          flag_person$err_flag_bdegreeu2)
flag_person$err_flag_bdegreeu2 <-
  if_else(grepl("藥專", flag_person$bdegreeu2),
          1,
          flag_person$err_flag_bdegreeu2)
flag_person$err_flag_bdegreeu2 <-
  if_else(
    grepl("^台南家專學校財團法人台南應用科技大學$", flag_person$bdegreeu2),
    0,
    flag_person$err_flag_bdegreeu2
  )
#陸軍官校專科班為學士學位
flag_person$err_flag_bdegreeu2 <-
  if_else(grepl("^陸軍官校專科班$", flag_person$bdegreeu2),
          0,
          flag_person$err_flag_bdegreeu2)
flag_person$err_flag_bdegreeu2 <-
  if_else(grepl("^陸軍官校大學部$", flag_person$bdegreeu2),
          0,
          flag_person$err_flag_bdegreeu2)

flag_person$err_flag <-
  flag_person$err_flag_bdegreeu1 + flag_person$err_flag_bdegreeu2

#加註學士學位畢業學校名稱
flag_person$err_flag_txt <- ""
flag_person$err_flag_txt <- case_when(
  flag_person$err_flag_bdegreeu1 == 1 ~ paste(
    flag_person$name,
    "（學士學位畢業學校（一）：",
    flag_person$bdegreeu1,
    "）",
    sep = ""
  ),
  flag_person$err_flag_bdegreeu2 == 1 ~ paste(
    flag_person$name,
    "（學士學位畢業學校（二）：",
    flag_person$bdegreeu2,
    "）",
    sep = ""
  ),
  TRUE ~ flag_person$err_flag_txt
)

if (dim(flag_person %>% subset(err_flag == 1))[1] != 0) {
  #根據organization_id + source，展開成寬資料(wide)
  flag_person_wide_flag20 <- flag_person %>%
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
    colnames(flag_person_wide_flag20)[3:length(colnames(flag_person_wide_flag20))]
  flag_person_wide_flag20$flag20_r <- NA
  for (i in temp) {
    flag_person_wide_flag20$flag20_r <-
      paste(flag_person_wide_flag20$flag20_r,
            flag_person_wide_flag20[[i]],
            sep = " ")
  }
  flag_person_wide_flag20$flag20_r <-
    gsub("NA ",
         replacement = "",
         flag_person_wide_flag20$flag20_r)
  flag_person_wide_flag20$flag20_r <-
    gsub(" NA",
         replacement = "",
         flag_person_wide_flag20$flag20_r)
  
  #產生檢誤報告文字
  flag20_temp <- flag_person_wide_flag20 %>%
    group_by(organization_id) %>%
    mutate(flag20_txt = paste(source, "：", flag20_r, sep = ""), "") %>%
    subset(select = c(organization_id, flag20_txt)) %>%
    distinct(organization_id, flag20_txt)
  
  #根據organization_id，展開成寬資料(wide)
  flag20 <- flag20_temp %>%
    dcast(organization_id ~ flag20_txt, value.var = "flag20_txt")
  
  #合併教員資料表及職員(工)資料表報告文字
  temp <- colnames(flag20)[2:length(colnames(flag20))]
  flag20$flag20 <- NA
  for (i in temp) {
    flag20$flag20 <- paste(flag20$flag20, flag20[[i]], sep = "； ")
  }
  flag20$flag20 <- gsub("NA； ", replacement = "", flag20$flag20)
  flag20$flag20 <- gsub("； NA", replacement = "", flag20$flag20)
  
  #產生檢誤報告文字
  flag20 <- flag20 %>%
    subset(select = c(organization_id, flag20)) %>%
    distinct(organization_id, flag20)
} else{
  #偵測flag20是否存在。若不存在，則產生NA行
  if ('flag20' %in% ls()) {
    print("flag20")
  } else{
    flag20 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    flag20$flag20 <- ""
  }
}
