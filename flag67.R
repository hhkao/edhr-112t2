# flag67: 「擔任其他教學相關職務名稱」應填入本欄定義之職務名稱或內容。 -------------------------------------------------------------------
flag_person <- drev_P_load %>%
  subset(load == 1)

#「擔任其他教學相關職務名稱」應填入本欄為之定義職務名稱或內容(有學校填行政職稱、教學資料表前列的教學職務名稱、NA、Y等)
#注意:每年須檢視是否有新的錯誤必須抓出
flag_person$err_flag <- 0
flag_person$err_flag <-
  if_else(grepl("主任$", flag_person$otherteacher),
          1,
          flag_person$err_flag)
flag_person$err_flag <-
  if_else(grepl("組長$", flag_person$otherteacher),
          1,
          flag_person$err_flag)
flag_person$err_flag <-
  if_else(grepl("校長$", flag_person$otherteacher),
          1,
          flag_person$err_flag)
flag_person$err_flag <-
  if_else(grepl("^協助行政$", flag_person$otherteacher),
          1,
          flag_person$err_flag)
flag_person$err_flag <-
  if_else(grepl("^協行$", flag_person$otherteacher),
          1,
          flag_person$err_flag)
flag_person$err_flag <-
  if_else(grepl("課程諮詢教師$", flag_person$otherteacher),
          1,
          flag_person$err_flag)
flag_person$err_flag <-
  if_else(grepl("^課諮教師$", flag_person$otherteacher),
          1,
          flag_person$err_flag)
flag_person$err_flag <-
  if_else(grepl("^兼任教師$", flag_person$otherteacher),
          1,
          flag_person$err_flag)
flag_person$err_flag <-
  if_else(grepl("^兼任$", flag_person$otherteacher),
          1,
          flag_person$err_flag)
flag_person$err_flag <-
  if_else(grepl("^導師$", flag_person$otherteacher),
          1,
          flag_person$err_flag)
flag_person$err_flag <-
  if_else(grepl("召集人$", flag_person$otherteacher),
          1,
          flag_person$err_flag)
flag_person$err_flag <-
  if_else(grepl("領召$", flag_person$otherteacher),
          1,
          flag_person$err_flag)
flag_person$err_flag <-
  if_else(grepl("^Y$", flag_person$otherteacher),
          1,
          flag_person$err_flag)
flag_person$err_flag <-
  if_else(grepl("^NA$", flag_person$otherteacher),
          1,
          flag_person$err_flag)
flag_person$err_flag <-
  if_else(grepl("^秘書$", flag_person$otherteacher),
          1,
          flag_person$err_flag)
flag_person$err_flag <-
  if_else(grepl("^祕書$", flag_person$otherteacher),
          1,
          flag_person$err_flag)
#前導主任或組長、均優質化(均優)是其他教學相關職務名稱，故不能標記為錯誤
flag_person$err_flag <-
  if_else(grepl("前導", flag_person$otherteacher),
          0,
          flag_person$err_flag)
flag_person$err_flag <-
  if_else(grepl("均優", flag_person$otherteacher),
          0,
          flag_person$err_flag)
flag_person$err_flag <-
  if_else(grepl("均質", flag_person$otherteacher),
          0,
          flag_person$err_flag)
flag_person$err_flag <-
  if_else(grepl("^探究與實作課程推動中心(北區)執行秘書$", flag_person$otherteacher),
          0,
          flag_person$err_flag)
flag_person$err_flag <-
  if_else(grepl("^藝術生活學科中心執行秘書$", flag_person$otherteacher),
          0,
          flag_person$err_flag)
flag_person$err_flag <-
  if_else(grepl("^技術教學中心教學組長$", flag_person$otherteacher),
          0,
          flag_person$err_flag)
flag_person$err_flag <-
  if_else(grepl("^技術教學中心秘書$", flag_person$otherteacher),
          0,
          flag_person$err_flag)
flag_person$err_flag <-
  if_else(grepl("^科學班班主任$", flag_person$otherteacher),
          0,
          flag_person$err_flag)

#加註
flag_person$err_flag_txt <- ""
flag_person$err_flag_txt <- case_when(
  flag_person$err_flag == 1 ~ paste(flag_person$name,
                                    "（",
                                    flag_person$otherteacher,
                                    "）",
                                    sep = ""),
  TRUE ~ flag_person$err_flag_txt
)

if (dim(flag_person %>% subset(err_flag == 1))[1] != 0) {
  #根據organization_id + source，展開成寬資料(wide)
  flag_person_wide_flag67 <- flag_person %>%
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
    colnames(flag_person_wide_flag67)[3:length(colnames(flag_person_wide_flag67))]
  flag_person_wide_flag67$flag67_r <- NA
  for (i in temp) {
    flag_person_wide_flag67$flag67_r <-
      paste(flag_person_wide_flag67$flag67_r,
            flag_person_wide_flag67[[i]],
            sep = " ")
  }
  flag_person_wide_flag67$flag67_r <-
    gsub("NA ",
         replacement = "",
         flag_person_wide_flag67$flag67_r)
  flag_person_wide_flag67$flag67_r <-
    gsub(" NA",
         replacement = "",
         flag_person_wide_flag67$flag67_r)
  
  #產生檢誤報告文字
  flag67_temp <- flag_person_wide_flag67 %>%
    group_by(organization_id) %>%
    mutate(flag67_txt = paste(flag67_r, sep = ""), "") %>%
    subset(select = c(organization_id, flag67_txt)) %>%
    distinct(organization_id, flag67_txt)
  
  #根據organization_id，展開成寬資料(wide)
  flag67 <- flag67_temp %>%
    dcast(organization_id ~ flag67_txt, value.var = "flag67_txt")
  
  #合併教員資料表及職員(工)資料表報告文字
  temp <- colnames(flag67)[2:length(colnames(flag67))]
  flag67$flag67 <- NA
  for (i in temp) {
    flag67$flag67 <- paste(flag67$flag67, flag67[[i]], sep = "； ")
  }
  flag67$flag67 <- gsub("NA； ", replacement = "", flag67$flag67)
  flag67$flag67 <- gsub("； NA", replacement = "", flag67$flag67)
  
  #產生檢誤報告文字
  flag67 <- flag67 %>%
    subset(select = c(organization_id, flag67)) %>%
    distinct(organization_id, flag67) %>%
    mutate(
      flag67 = paste(
        "請確認以下人員於教學資料表的「擔任其他教學相關職務名稱」：",
        flag67,
        "（本欄所稱其他教學相關職務不包括行政處室主任、組長、科主任、學程主任等行政職務，爰請修正，並請將行政職務請填至兼任行政職務及其服務單位等欄）",
        sep = ""
      )
    )
} else{
  #偵測flag67是否存在。若不存在，則產生NA行
  if ('flag67' %in% ls()) {
    print("flag67")
  } else{
    flag67 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    flag67$flag67 <- ""
  }
}
