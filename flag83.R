# flag83: 離退教職員（工）資料表所列人員，不應填列為本次教員資料表或職員（工）資料表之專任或代理人員。 -------------------------------------------------------------------
flag_person <- drev_P_retire %>%
  rename(name = name.x, name_retire = name.y)

#若drev_P_retire無資料，建立物件
if (dim(drev_P_retire)[1] == 0) {
  temp <-
    matrix("", nrow = 1, ncol = ncol(flag_person)) %>% data.frame()
  names(temp) <- names(flag_person)
  flag_person <- temp
} else{
  print("flag83: drev_P_retire is already exists.")
}

#離退教職員(工)資料表所列人員，不應填列為本次教員資料表或職員（工）資料表之專任或代理人員。(與本期資料比對)
#抓出:離退人員有出現在教員資料表、職員工資料表，且為專任或代理
flag_person$err_flag <- 0
flag_person$err_flag <- if_else(
  !is.na(flag_person$name_retire)
  &
    flag_person$emptype %in% c("專任", "代理", "代理(連)"),
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
  flag_person_wide_flag83 <- flag_person %>%
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
    colnames(flag_person_wide_flag83)[3:length(colnames(flag_person_wide_flag83))]
  flag_person_wide_flag83$flag83_r <- NA
  for (i in temp) {
    flag_person_wide_flag83$flag83_r <-
      paste(flag_person_wide_flag83$flag83_r,
            flag_person_wide_flag83[[i]],
            sep = " ")
  }
  flag_person_wide_flag83$flag83_r <-
    gsub("NA ",
         replacement = "",
         flag_person_wide_flag83$flag83_r)
  flag_person_wide_flag83$flag83_r <-
    gsub(" NA",
         replacement = "",
         flag_person_wide_flag83$flag83_r)
  
  #產生檢誤報告文字
  flag83_temp <- flag_person_wide_flag83 %>%
    group_by(organization_id) %>%
    mutate(flag83_txt = paste(source, "：", flag83_r, sep = ""), "") %>%
    subset(select = c(organization_id, flag83_txt)) %>%
    distinct(organization_id, flag83_txt)
  
  #根據organization_id，展開成寬資料(wide)
  flag83 <- flag83_temp %>%
    dcast(organization_id ~ flag83_txt, value.var = "flag83_txt")
  
  #合併教員資料表及職員(工)資料表報告文字
  temp <- colnames(flag83)[2:length(colnames(flag83))]
  flag83$flag83 <- NA
  for (i in temp) {
    flag83$flag83 <- paste(flag83$flag83, flag83[[i]], sep = "； ")
  }
  flag83$flag83 <- gsub("NA； ", replacement = "", flag83$flag83)
  flag83$flag83 <- gsub("； NA", replacement = "", flag83$flag83)
  
  #產生檢誤報告文字
  flag83 <- flag83 %>%
    subset(select = c(organization_id, flag83)) %>%
    distinct(organization_id, flag83) %>%
    mutate(
      flag83 = paste(
        flag83,
        "（請確認上述人員是否退休、退伍或因故離職，若是，則不需填列至本次教員資料表或職員（工）資料表，並請依欄位說明確認離退職類別）",
        sep = ""
      )
    )
} else{
  #偵測flag83是否存在。若不存在，則產生NA行
  if ('flag83' %in% ls()) {
    print("flag83")
  } else{
    flag83 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    flag83$flag83 <- ""
  }
}
