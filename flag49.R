# flag49: 1.	職員(工)的「職務名稱」不應填N（全型或半型皆不行）。-------------------------------------------------------------------
# 2. 職員(工)的「服務單位」不應填N（全型或半型皆不行），且應入填入對應職稱的學校內部單位。
flag_person <- drev_person_1

#標記職務名稱、服務單位為N或非學校內部單位
flag_person$err_adm1 <- 1
flag_person$err_adm1 <-
  if_else(grepl("組", flag_person$adminunit0),
          0,
          flag_person$err_adm1)
flag_person$err_adm1 <-
  if_else(grepl("室", flag_person$adminunit0),
          0,
          flag_person$err_adm1)
flag_person$err_adm1 <-
  if_else(grepl("科", flag_person$adminunit0),
          0,
          flag_person$err_adm1)
flag_person$err_adm1 <-
  if_else(grepl("中心", flag_person$adminunit0),
          0,
          flag_person$err_adm1)
flag_person$err_adm1 <-
  if_else(grepl("部", flag_person$adminunit0),
          0,
          flag_person$err_adm1)
flag_person$err_adm1 <-
  if_else(grepl("辦公室", flag_person$adminunit0),
          0,
          flag_person$err_adm1)
flag_person$err_adm1 <-
  if_else(grepl("館", flag_person$adminunit0),
          0,
          flag_person$err_adm1)
flag_person$err_adm1 <-
  if_else(grepl("處", flag_person$adminunit0),
          0,
          flag_person$err_adm1)
flag_person$err_adm1 <-
  if_else(grepl("部", flag_person$adminunit0),
          0,
          flag_person$err_adm1)
flag_person$err_adm1 <-
  if_else(grepl("^社區大學$", flag_person$adminunit0),
          0,
          flag_person$err_adm1)
flag_person$err_adm1 <-
  if_else(flag_person$adminunit0 == "", 0, flag_person$err_adm1)
flag_person$err_adm1 <-
  if_else(flag_person$adminunit0 == "董事會",
          0,
          flag_person$err_adm1)
flag_person$err_adm1 <-
  if_else(flag_person$adminunit0 == "實習農場",
          0,
          flag_person$err_adm1)
flag_person$err_adm1 <-
  if_else(flag_person$adminunit0 == "NA",
          1,
          flag_person$err_adm1)
flag_person$err_adm1 <-
  if_else(flag_person$adminunit0 == "n", 1, flag_person$err_adm1)
flag_person$err_adm1 <-
  if_else(flag_person$adminunit0 == "N", 1, flag_person$err_adm1)
flag_person$err_adm1 <-
  if_else(flag_person$adminunit0 == "Ｎ", 1, flag_person$err_adm1)
flag_person$err_adm1 <-
  if_else(flag_person$adminunit0 == "國教署",
          1,
          flag_person$err_adm1)
flag_person$err_adm1 <-
  if_else(flag_person$adminunit0 == "y", 1, flag_person$err_adm1)
flag_person$err_adm1 <-
  if_else(flag_person$adminunit0 == "Y", 1, flag_person$err_adm1)

flag_person$err_adm2 <- 0
flag_person$err_adm2 <-
  if_else(flag_person$admintitle0 == "NA",
          1,
          flag_person$err_adm2)
flag_person$err_adm2 <-
  if_else(flag_person$admintitle0 == "N",
          1,
          flag_person$err_adm2)
flag_person$err_adm2 <-
  if_else(flag_person$admintitle0 == "Ｎ",
          1,
          flag_person$err_adm2)
flag_person$err_adm2 <-
  if_else(flag_person$admintitle0 == "n",
          1,
          flag_person$err_adm2)
flag_person$err_adm2 <-
  if_else(flag_person$admintitle0 == "Y",
          1,
          flag_person$err_adm2)
flag_person$err_adm2 <-
  if_else(flag_person$admintitle0 == "y",
          1,
          flag_person$err_adm2)
flag_person$err_adm2 <-
  if_else(grepl("Ｎ", flag_person$admintitle0),
          1,
          flag_person$err_adm2)

flag_person$err_flag <- flag_person$err_adm1 + flag_person$err_adm2
flag_person$err_adm <- 0
flag_person$err_adm <-
  if_else(flag_person$err_flag != 0 &
            flag_person$source == "職員(工)資料表",
          1,
          flag_person$err_adm)

#加註
flag_person$name <- paste(flag_person$name, "（", sep = "")
flag_person$name <-
  if_else(
    flag_person$err_adm2 != 0,
    paste(flag_person$name,
          "職務名稱：",
          flag_person$admintitle0,
          "；",
          sep = ""),
    flag_person$name
  )
flag_person$name <-
  if_else(
    flag_person$err_adm1 != 0,
    paste(flag_person$name,
          "服務單位：",
          flag_person$adminunit0,
          "；",
          sep = ""),
    flag_person$name
  )
flag_person$name <- paste(flag_person$name, "）", sep = "")
flag_person$name <- gsub("；）", replacement = "）", flag_person$name)
flag_person$name <- gsub("（）", replacement = "", flag_person$name)


flag_person$err_flag_txt <- ""
flag_person$err_flag_txt <-
  case_when(flag_person$err_adm == 1 ~ flag_person$name,
            TRUE ~ flag_person$err_flag_txt)

if (dim(flag_person %>% subset(err_adm == 1))[1] != 0) {
  #根據organization_id + source，展開成寬資料(wide)
  flag_person_wide_flag49 <- flag_person %>%
    subset(select = c(
      organization_id,
      idnumber,
      err_flag_txt,
      edu_name2,
      source,
      err_adm
    )) %>%
    subset(err_adm == 1) %>%
    dcast(organization_id + source ~ err_flag_txt, value.var = "err_flag_txt")
  
  #合併所有name
  temp <-
    colnames(flag_person_wide_flag49)[3:length(colnames(flag_person_wide_flag49))]
  flag_person_wide_flag49$flag49_r <- NA
  for (i in temp) {
    flag_person_wide_flag49$flag49_r <-
      paste(flag_person_wide_flag49$flag49_r,
            flag_person_wide_flag49[[i]],
            sep = " ")
  }
  flag_person_wide_flag49$flag49_r <-
    gsub("NA ",
         replacement = "",
         flag_person_wide_flag49$flag49_r)
  flag_person_wide_flag49$flag49_r <-
    gsub(" NA",
         replacement = "",
         flag_person_wide_flag49$flag49_r)
  
  #產生檢誤報告文字
  flag49_temp <- flag_person_wide_flag49 %>%
    group_by(organization_id) %>%
    mutate(flag49_txt = paste(source, "：", flag49_r, sep = ""), "") %>%
    subset(select = c(organization_id, flag49_txt)) %>%
    distinct(organization_id, flag49_txt)
  
  #根據organization_id，展開成寬資料(wide)
  flag49 <- flag49_temp %>%
    dcast(organization_id ~ flag49_txt, value.var = "flag49_txt")
  
  #合併教員資料表及職員(工)資料表報告文字
  temp <- colnames(flag49)[2:length(colnames(flag49))]
  flag49$flag49 <- NA
  for (i in temp) {
    flag49$flag49 <- paste(flag49$flag49, flag49[[i]], sep = "； ")
  }
  flag49$flag49 <- gsub("NA； ", replacement = "", flag49$flag49)
  flag49$flag49 <- gsub("； NA", replacement = "", flag49$flag49)
  
  #產生檢誤報告文字
  flag49 <- flag49 %>%
    subset(select = c(organization_id, flag49)) %>%
    distinct(organization_id, flag49) %>%
    mutate(flag49 = paste(flag49, "（請確認『職務名稱』、『服務單位』）", sep = ""))
} else{
  #偵測flag49是否存在。若不存在，則產生NA行
  if ('flag49' %in% ls()) {
    print("flag49")
  } else{
    flag49 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    flag49$flag49 <- ""
  }
}
