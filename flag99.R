# flag99: 教員聘任類別、技術教師、專任輔導教師與業界專家（業師）之檢查-------------------------------------------------------------------
#1.	技術教師之聘任類別不應為「兼任」或「鐘點教師」。
#2.	專任輔導教師之聘任類別不應為「兼任」或「鐘點教師」。
#3.	業界專家（業師）之聘任類別不應為「專任」或「代理」

flag_person <- drev_person_1

#技術教師不可為兼任或鐘點教師
flag_person$err_flag1 <- 0
flag_person$err_flag1 <-
  if_else(
    flag_person$skillteacher == "Y" &
      flag_person$emptype %in% c("兼任", "鐘點教師") &
      flag_person$source == "教員資料表",
    1,
    flag_person$err_flag1
  )

#專輔教師不可為兼任或鐘點教師
flag_person$err_flag2 <- 0
flag_person$err_flag2 <-
  if_else(
    flag_person$counselor == "Y" &
      flag_person$emptype %in% c("兼任", "鐘點教師") &
      flag_person$source == "教員資料表",
    1,
    flag_person$err_flag2
  )

#業師不可為專任或代理
flag_person$err_flag3 <- 0
flag_person$err_flag3 <-
  if_else(
    flag_person$expecter == "Y" &
      flag_person$emptype %in% c("專任", "代理", "代理(連)") &
      flag_person$source == "教員資料表",
    1,
    flag_person$err_flag3
  )

flag_person$err_flag_99 <-
  flag_person$err_flag1 + flag_person$err_flag2 + flag_person$err_flag3

flag_person$err_flag <- 0
flag_person$err_flag <-
  if_else(flag_person$err_flag_99 != 0, 1, flag_person$err_flag)

#加註
flag_person$name <- paste(flag_person$name, "（", " ", sep = "")
flag_person$name <-
  if_else(
    flag_person$err_flag1 != 0,
    paste(flag_person$name, "技術教師 ", "、", sep = ""),
    flag_person$name
  )
flag_person$name <-
  if_else(
    flag_person$err_flag2 != 0,
    paste(flag_person$name, "專任輔導教師 ", "、", sep = ""),
    flag_person$name
  )
flag_person$name <-
  if_else(
    flag_person$err_flag3 != 0,
    paste(flag_person$name, "業界專家", sep = ""),
    flag_person$name
  )
flag_person$name <-
  paste(flag_person$name, "（", flag_person$emptype, "））", sep = "")
flag_person$name <- gsub("、）", replacement = "）", flag_person$name)
flag_person$name <-
  gsub("技術教師 、（", replacement = "技術教師（", flag_person$name)
flag_person$name <-
  gsub("專任輔導教師 、（", replacement = "專任輔導教師（", flag_person$name)
flag_person$name <-
  gsub("業界專家 、（", replacement = "業界專家（", flag_person$name)
flag_person$name <- gsub("（）", replacement = "", flag_person$name)

#呈現姓名
flag_person$err_flag_txt <- ""
flag_person$err_flag_txt <- case_when(flag_person$err_flag == 1 ~ flag_person$name,
                                      TRUE ~ flag_person$err_flag_txt)

if (dim(flag_person %>% subset(err_flag == 1))[1] != 0) {
  #根據organization_id + source，展開成寬資料(wide)
  flag_person_wide_flag99 <- flag_person %>%
    subset(
      select = c(
        organization_id,
        idnumber,
        err_flag_txt,
        edu_name2,
        source,
        err_flag
      )
    ) %>%
    subset(err_flag == 1) %>%
    dcast(organization_id + source ~ err_flag_txt, value.var = "err_flag_txt")
  
  #合併所有name
  temp <-
    colnames(flag_person_wide_flag99)[3:length(colnames(flag_person_wide_flag99))]
  flag_person_wide_flag99$flag99_r <- NA
  for (i in temp) {
    flag_person_wide_flag99$flag99_r <-
      paste(flag_person_wide_flag99$flag99_r,
            flag_person_wide_flag99[[i]],
            sep = " ")
  }
  flag_person_wide_flag99$flag99_r <-
    gsub("NA ",
         replacement = "",
         flag_person_wide_flag99$flag99_r)
  flag_person_wide_flag99$flag99_r <-
    gsub(" NA",
         replacement = "",
         flag_person_wide_flag99$flag99_r)
  
  #產生檢誤報告文字
  flag99_temp <- flag_person_wide_flag99 %>%
    group_by(organization_id) %>%
    mutate(flag99_txt = paste(source, "姓名：", flag99_r, sep = ""), "") %>%
    subset(select = c(organization_id, flag99_txt)) %>%
    distinct(organization_id, flag99_txt)
  
  #根據organization_id，展開成寬資料(wide)
  flag99 <- flag99_temp %>%
    dcast(organization_id ~ flag99_txt, value.var = "flag99_txt")
  
  #合併教員資料表及職員(工)資料表報告文字
  temp <- colnames(flag99)[2:length(colnames(flag99))]
  flag99$flag99 <- NA
  for (i in temp) {
    flag99$flag99 <- paste(flag99$flag99, flag99[[i]], sep = "； ")
  }
  flag99$flag99 <- gsub("NA； ", replacement = "", flag99$flag99)
  flag99$flag99 <- gsub("； NA", replacement = "", flag99$flag99)
  
  #（請協助確認上開人員之『聘任類別』，或是否確為業師或技術教師。）
  #（請協助確認上開人員之『聘任類別』，或是否依規定聘為專任輔導教師具備相關身分資格。）
  
  #產生檢誤報告文字
  flag99 <- flag99 %>%
    subset(select = c(organization_id, flag99)) %>%
    distinct(organization_id, flag99) %>%
    mutate(flag99 = paste(flag99, "（請確認上開人員之『聘任類別』，或是否確為業師或技術教師。）", sep = ""))
} else{
  #偵測flag99是否存在。若不存在，則產生NA行
  if ('flag99' %in% ls()) {
    print("flag99")
  } else{
    flag99 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    flag99$flag99 <- ""
  }
}

