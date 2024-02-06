# flag94: 職員（工）的「職務名稱」與「聘任類別」不相符應。 -------------------------------------------------------------------
flag_person <- drev_person_1

#職員工若為專任，職務名稱不可出現"約僱"、"約聘雇"、"約雇"、"約聘雇"、"約聘"之關鍵字
#私立學校flag94還是檢查，但屬於確認項目
flag_person$err_flag <- 0
flag_person$err_flag <-
  if_else(
    grepl("約僱", flag_person$admintitle0) &
      flag_person$emptype == "專任" &
      flag_person$source == "職員(工)資料表",
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    grepl("約聘僱", flag_person$admintitle0) &
      flag_person$emptype == "專任" &
      flag_person$source == "職員(工)資料表",
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    grepl("約雇", flag_person$admintitle0) &
      flag_person$emptype == "專任" &
      flag_person$source == "職員(工)資料表",
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    grepl("約聘雇", flag_person$admintitle0) &
      flag_person$emptype == "專任" &
      flag_person$source == "職員(工)資料表",
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    grepl("約聘", flag_person$admintitle0) &
      flag_person$emptype == "專任" &
      flag_person$source == "職員(工)資料表",
    1,
    flag_person$err_flag
  )

#加註
flag_person$name <-
  paste(flag_person$name,
        "（職務名稱：",
        flag_person$admintitle0,
        "；）",
        sep = "")
flag_person$name <- gsub("；）", replacement = "）", flag_person$name)
flag_person$name <- gsub("（）", replacement = "", flag_person$name)

#呈現姓名
flag_person$err_flag_txt <- ""
flag_person$err_flag_txt <- case_when(flag_person$err_flag == 1 ~ flag_person$name,
                                      TRUE ~ flag_person$err_flag_txt)

if (dim(flag_person %>% subset(err_flag == 1))[1] != 0) {
  #根據organization_id + source，展開成寬資料(wide)
  flag_person_wide_flag94 <- flag_person %>%
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
    colnames(flag_person_wide_flag94)[3:length(colnames(flag_person_wide_flag94))]
  flag_person_wide_flag94$flag94_r <- NA
  for (i in temp) {
    flag_person_wide_flag94$flag94_r <-
      paste(flag_person_wide_flag94$flag94_r,
            flag_person_wide_flag94[[i]],
            sep = " ")
  }
  flag_person_wide_flag94$flag94_r <-
    gsub("NA ",
         replacement = "",
         flag_person_wide_flag94$flag94_r)
  flag_person_wide_flag94$flag94_r <-
    gsub(" NA",
         replacement = "",
         flag_person_wide_flag94$flag94_r)
  
  #產生檢誤報告文字
  flag94_temp <- flag_person_wide_flag94 %>%
    group_by(organization_id) %>%
    mutate(flag94_txt = paste(source, "：", flag94_r, sep = ""), "") %>%
    subset(select = c(organization_id, flag94_txt)) %>%
    distinct(organization_id, flag94_txt)
  
  #根據organization_id，展開成寬資料(wide)
  flag94 <- flag94_temp %>%
    dcast(organization_id ~ flag94_txt, value.var = "flag94_txt")
  
  #合併教員資料表及職員(工)資料表報告文字
  temp <- colnames(flag94)[2:length(colnames(flag94))]
  flag94$flag94 <- NA
  for (i in temp) {
    flag94$flag94 <- paste(flag94$flag94, flag94[[i]], sep = "； ")
  }
  flag94$flag94 <- gsub("NA； ", replacement = "", flag94$flag94)
  flag94$flag94 <- gsub("； NA", replacement = "", flag94$flag94)
  
  #產生檢誤報告文字
  flag94 <- flag94 %>%
    subset(select = c(organization_id, flag94)) %>%
    distinct(organization_id, flag94) %>%
    mutate(
      flag94 = paste(
        flag94,
        "（請確認上開職員(工)之『聘任類別』及『職務名稱』。凡以簽訂契約方式任用之人員，無論是否為編制內員額，其『聘任類別』原則為『約聘僱』或『約用』。並請再協助確認上開職員(工)『職務名稱』是否正確。惟貴校職員(工)如具正式公務人員身分者，則其『聘任類別』原則應是『專任』。）",
        sep = ""
      )
    )
} else{
  #偵測flag94是否存在。若不存在，則產生NA行
  if ('flag94' %in% ls()) {
    print("flag94")
  } else{
    flag94 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    flag94$flag94 <- ""
  }
}
