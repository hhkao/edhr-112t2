# flag18: 人事資料表各欄位是否有資料分布異常的情形。 -------------------------------------------------------------------
flag_person <- drev_person_1

flag_person$count_emptype <-
  if_else(flag_person$emptype == "專任" &
            flag_person$source == "教員資料表",
          1,
          0)
flag_person$count_emptype2 <-
  if_else(flag_person$emptype == "專任" &
            flag_person$source == "職員(工)資料表",
          1,
          0)
flag_person$count_empunit <-
  if_else((
    flag_person$empunit == "高中部日間部" |
      flag_person$empunit == "國中部日間部" |
      flag_person$empunit == "中學部"
  ) & flag_person$source == "教員資料表",
  1,
  0
  )
flag_person$count_empunit2 <-
  if_else((
    flag_person$empunit == "高中部日間部" |
      flag_person$empunit == "國中部日間部" |
      flag_person$empunit == "中學部"
  ) & flag_person$source == "職員(工)資料表",
  1,
  0
  )
flag_person$count_sertype <-
  if_else(flag_person$sertype == "教師", 1, 0)
flag_person$count_sertype2 <-
  if_else(flag_person$sertype == "校長", 1, 0)
flag_person$count_skillteacher <-
  if_else(flag_person$skillteacher == "N", 1, 0)
flag_person$count_counselor <-
  if_else(flag_person$counselor == "N", 1, 0)
flag_person$count_speteacher <-
  if_else(flag_person$speteacher == "N", 1, 0)
flag_person$count_joiteacher <-
  if_else(flag_person$joiteacher %in% c("1", "2"), 1, 0)
flag_person$count_joiteacher2 <-
  if_else(flag_person$joiteacher %in% c("3", "4"), 1, 0)
flag_person$count_joiteacher3 <-
  if_else(flag_person$joiteacher == "N", 1, 0)
flag_person$count_expecter <-
  if_else(flag_person$expecter == "N", 1, 0)
flag_person$count_workexp <-
  if_else(flag_person$workexp == "N", 1, 0)
flag_person$count_study <- if_else(flag_person$study == "N", 1, 0)

flag_person <- flag_person %>%
  mutate(
    count_admin2 = 0,
    count_admin3 = 0,
    count_admin4 = 0,
    count_admin5 = 0,
    count_admin6 = 0,
    count_admin8 = 0,
    count_admin9 = 0
  )

temp <- c("0", "1", "2", "3")
for (x in temp) {
  flag_person$count_admin2 <- case_when(
    grepl("教務", flag_person[[paste("adminunit", x, sep = "")]])                                                                                                                                     &
      (grepl("主任$", flag_person[[paste("admintitle", x, sep = "")]]) |
         grepl("主任1$", flag_person[[paste("admintitle", x, sep = "")]])) &
      !grepl("主任教官", flag_person[[paste("admintitle", x, sep = "")]]) &
      !grepl("科主任", flag_person[[paste("admintitle", x, sep = "")]]) &
      !grepl("學程主任", flag_person[[paste("admintitle", x, sep = "")]]) &
      !grepl("國中部主任", flag_person[[paste("admintitle", x, sep = "")]]) ~ 1,
    TRUE ~ flag_person$count_admin2
  )
}
for (x in temp) {
  flag_person$count_admin3 <- case_when(
    (grepl("學務", flag_person[[paste("adminunit", x, sep = "")]]) |
       grepl("學生事務", flag_person[[paste("adminunit", x, sep = "")]]))                                                                &
      (grepl("主任$", flag_person[[paste("admintitle", x, sep = "")]]) |
         grepl("主任1$", flag_person[[paste("admintitle", x, sep = "")]])) &
      !grepl("主任教官", flag_person[[paste("admintitle", x, sep = "")]]) &
      !grepl("科主任", flag_person[[paste("admintitle", x, sep = "")]]) &
      !grepl("學程主任", flag_person[[paste("admintitle", x, sep = "")]]) ~ 1,
    TRUE ~ flag_person$count_admin3
  )
}
for (x in temp) {
  flag_person$count_admin4 <- case_when(
    grepl("總務", flag_person[[paste("adminunit", x, sep = "")]])                                                                                                                                     &
      (grepl("主任$", flag_person[[paste("admintitle", x, sep = "")]]) |
         grepl("主任1$", flag_person[[paste("admintitle", x, sep = "")]])) &
      !grepl("主任教官", flag_person[[paste("admintitle", x, sep = "")]]) &
      !grepl("科主任", flag_person[[paste("admintitle", x, sep = "")]]) &
      !grepl("學程主任", flag_person[[paste("admintitle", x, sep = "")]]) ~ 1,
    TRUE ~ flag_person$count_admin4
  )
}
for (x in temp) {
  flag_person$count_admin5 <- case_when(
    grepl("輔導", flag_person[[paste("adminunit", x, sep = "")]])                                                                                                                                     &
      (grepl("主任$", flag_person[[paste("admintitle", x, sep = "")]]) |
         grepl("主任1$", flag_person[[paste("admintitle", x, sep = "")]])) &
      !grepl("主任輔導教師$", flag_person[[paste("admintitle", x, sep = "")]]) &
      !grepl("主任教官", flag_person[[paste("admintitle", x, sep = "")]]) &
      !grepl("科主任", flag_person[[paste("admintitle", x, sep = "")]]) &
      !grepl("學程主任", flag_person[[paste("admintitle", x, sep = "")]]) ~ 1,
    TRUE ~ flag_person$count_admin5
  )
}
for (x in temp) {
  flag_person$count_admin6 <- case_when(
    (
      grepl("圖書", flag_person[[paste("adminunit", x, sep = "")]]) |
        grepl("圖資", flag_person[[paste("adminunit", x, sep = "")]]) |
        grepl("圖書資訊", flag_person[[paste("adminunit", x, sep = "")]])
    ) &
      ((grepl("主任$", flag_person[[paste("admintitle", x, sep = "")]]) |
          grepl("主任1$", flag_person[[paste("admintitle", x, sep = "")]])) &
         !grepl("主任教官", flag_person[[paste("admintitle", x, sep = "")]]) &
         !grepl("科主任", flag_person[[paste("admintitle", x, sep = "")]]) &
         !grepl("學程主任", flag_person[[paste("admintitle", x, sep = "")]])
      ) |
      grepl("^館長$", flag_person[[paste("admintitle", x, sep = "")]]) ~ 1,
    TRUE ~ flag_person$count_admin6
  )
}
for (x in temp) {
  flag_person$count_admin8 <- case_when(
    grepl("人事", flag_person[[paste("adminunit", x, sep = "")]])                                                                                                                                      &
      ((grepl("主任$", flag_person[[paste("admintitle", x, sep = "")]]) |
          grepl("主任1$", flag_person[[paste("admintitle", x, sep = "")]])) &
         !grepl("主任教官", flag_person[[paste("admintitle", x, sep = "")]]) &
         !grepl("科主任", flag_person[[paste("admintitle", x, sep = "")]]) &
         !grepl("學程主任", flag_person[[paste("admintitle", x, sep = "")]])
      ) |
      grepl("^人事管理員$", flag_person[[paste("admintitle", x, sep = "")]]) ~ 1,
    TRUE ~ flag_person$count_admin8
  )
}
for (x in temp) {
  flag_person$count_admin9 <- case_when(
    (grepl("會計", flag_person[[paste("adminunit", x, sep = "")]]) |
       grepl("主計", flag_person[[paste("adminunit", x, sep = "")]]))                                                                    &
      ((grepl("主任$", flag_person[[paste("admintitle", x, sep = "")]]) |
          grepl("主任1$", flag_person[[paste("admintitle", x, sep = "")]])) &
         !grepl("主任教官", flag_person[[paste("admintitle", x, sep = "")]]) &
         !grepl("科主任", flag_person[[paste("admintitle", x, sep = "")]]) &
         !grepl("學程主任", flag_person[[paste("admintitle", x, sep = "")]])
      ) |
      grepl("^主計員$", flag_person[[paste("admintitle", x, sep = "")]]) |
      grepl("^主計員$", flag_person[[paste("admintitle", x, sep = "")]]) ~ 1,
    TRUE ~ flag_person$count_admin9
  )
}

#調整NA
temp <-
  c(
    "count_emptype",
    "count_empunit",
    "count_sertype",
    "count_sertype2",
    "count_skillteacher",
    "count_counselor",
    "count_speteacher",
    "count_counselor",
    "count_speteacher",
    "count_joiteacher",
    "count_joiteacher2",
    "count_joiteacher3",
    "count_expecter",
    "count_workexp",
    "count_study",
    "count_admin2",
    "count_admin3",
    "count_admin4",
    "count_admin5",
    "count_admin6",
    "count_admin8",
    "count_admin9"
  )
for (x in temp) {
  flag_person[[x]][is.na(flag_person[[x]])] <- 0
}

flag_person$jj <- 1

flag_person_wide_flag18 <-
  aggregate(
    cbind(
      count_emptype,
      count_emptype2,
      count_empunit,
      count_empunit2,
      count_sertype,
      count_sertype2,
      count_skillteacher,
      count_counselor,
      count_speteacher,
      count_joiteacher,
      count_joiteacher2,
      count_joiteacher3,
      count_expecter,
      count_workexp,
      count_study,
      count_admin2,
      count_admin3,
      count_admin4,
      count_admin5,
      count_admin6,
      count_admin8,
      count_admin9,
      jj
    ) ~ organization_id + source,
    flag_person,
    sum
  )

flag_person_wide_flag18$flag_err <- 0
flag_person_wide_flag18$err_emptype <-
  if_else(
    flag_person_wide_flag18$count_emptype / flag_person_wide_flag18$jj < 0.5 &
      flag_person_wide_flag18$source == "教員資料表",
    "教員資料表專任教學人員人數偏低，請再協助確認實際聘任情況，或請確認是否填報完整教員名單資料。",
    ""
  )
flag_person_wide_flag18$err_emptype2 <-
  if_else(
    flag_person_wide_flag18$count_emptype2 / flag_person_wide_flag18$jj < 0.5 &
      flag_person_wide_flag18$source == "職員(工)資料表",
    "職員(工)資料表專任人員人數偏低，請再協助確認實際聘任情況，或請確認是否填報完整職員(工)名單資料。",
    ""
  )
flag_person_wide_flag18$err_empunit <-
  if_else(
    flag_person_wide_flag18$count_empunit / flag_person_wide_flag18$jj < 0.5 &
      flag_person_wide_flag18$source == "教員資料表",
    "教員資料表主聘單位各類別人數分布異常，請再協助確認實際聘任情況。",
    ""
  )
flag_person_wide_flag18$err_empunit2 <-
  if_else(
    flag_person_wide_flag18$count_empunit2 / flag_person_wide_flag18$jj < 0.5 &
      flag_person_wide_flag18$source == "職員(工)資料表",
    "職員(工)資料表主聘單位各類別人數分布異常，請再協助確認實際聘任情況。",
    ""
  )
flag_person_wide_flag18$err_sertype <-
  if_else(
    flag_person_wide_flag18$count_sertype / flag_person_wide_flag18$jj < 0.5 &
      flag_person_wide_flag18$source == "教員資料表",
    "教師人數偏低，請再協助確認實際聘任情況。",
    ""
  )
flag_person_wide_flag18$err_sertype2 <-
  if_else(
    flag_person_wide_flag18$count_sertype2 > 1 &
      flag_person_wide_flag18$source == "教員資料表",
    "校長人數超過一位，請再協助確認實際聘任情況。",
    ""
  )
flag_person_wide_flag18$err_skillteacher <-
  if_else(
    flag_person_wide_flag18$count_skillteacher / flag_person_wide_flag18$jj < 0.5 &
      flag_person_wide_flag18$source == "教員資料表",
    "專業及技術教師人數偏多，請再協助確認實際聘任情況。",
    ""
  )
flag_person_wide_flag18$err_counselor <-
  if_else(
    flag_person_wide_flag18$count_counselor / flag_person_wide_flag18$jj < 0.5 &
      flag_person_wide_flag18$source == "教員資料表",
    "專任輔導教師人數偏多，請再協助確認實際聘任情況。",
    ""
  )
flag_person_wide_flag18$err_speteacher <-
  if_else(
    flag_person_wide_flag18$count_speteacher / flag_person_wide_flag18$jj < 0.5 &
      flag_person_wide_flag18$source == "教員資料表",
    "特教班專職教師人數偏多，請再協助確認實際聘任情況，並依欄位說明修正資料。",
    ""
  )
flag_person_wide_flag18$err_joiteacher <-
  if_else(
    flag_person_wide_flag18$count_joiteacher / flag_person_wide_flag18$jj > 0.1 &
      flag_person_wide_flag18$source == "教員資料表",
    "合聘教師人數偏多（請確認校內教師是否與他校合聘：如有與他校合聘者，本校又為『主聘學校』，再請於『是否為合聘教師』一欄填入『1』，若以本校為『從聘學校』請於『是否為合聘教師』一欄填入『2』；若沒有與他校合聘，則『是否為合聘教師』一欄請填『N』）",
    ""
  )
flag_person_wide_flag18$err_joiteacher2 <-
  if_else(
    flag_person_wide_flag18$count_joiteacher2 / flag_person_wide_flag18$jj > 0.2 &
      flag_person_wide_flag18$source == "教員資料表",
    "巡迴教師人數偏多（請確認校內巡迴教師人數：如有巡迴教師，以本校又為『中心學校』，再請於『是否為合聘教師』一欄填入『3』，若以本校為『從屬學校』請於『是否為合聘教師』一欄填入『4』；若沒有巡迴教師，則『是否為合聘教師』一欄請填『N』）",
    ""
  )
flag_person_wide_flag18$err_joiteacher3 <-
  if_else(
    flag_person_wide_flag18$count_joiteacher3 / flag_person_wide_flag18$jj < 0.5 &
      flag_person_wide_flag18$source == "教員資料表",
    "合聘教師與巡迴教師人數偏多（請確認校內合聘教師、巡迴教師情形）",
    ""
  )
flag_person_wide_flag18$err_expecter <-
  if_else(
    flag_person_wide_flag18$count_expecter / flag_person_wide_flag18$jj < 0.5 &
      flag_person_wide_flag18$source == "教員資料表",
    "業界專家人數偏多，請再協助確認實際聘任情況，或請確認是否將專業及技術教師誤填為業界專家。",
    ""
  )
flag_person_wide_flag18$err_workexp <-
  if_else(
    flag_person_wide_flag18$count_workexp / flag_person_wide_flag18$jj < 0.5 &
      flag_person_wide_flag18$source == "教員資料表",
    "一年以上與任教領域相關之業界實務工作經驗人數偏多（請再協助確認，『是否具備一年以上與任教領域相關之業界實務工作經驗』填寫『Y』之教員，是否確依欄位說明具備此經驗）",
    ""
  )
flag_person_wide_flag18$err_study <-
  if_else(
    flag_person_wide_flag18$count_study / flag_person_wide_flag18$jj < 0.5 &
      flag_person_wide_flag18$source == "教員資料表",
    "近六年內進行與專業或技術有關之研習或研究的人數偏多，請再協助確認實際聘任情況。",
    ""
  )

#如果err_joiteacher、err_joiteacher2、err_joiteacher3同時皆被抓出的調整
idx <-
  which(
    flag_person_wide_flag18$err_joiteacher != "" &
      flag_person_wide_flag18$err_joiteacher2 != "" &
      flag_person_wide_flag18$err_joiteacher3 != ""
  )
flag_person_wide_flag18[idx, c("err_joiteacher", "err_joiteacher2")] <-
  ""

idx <-
  which(
    flag_person_wide_flag18$err_joiteacher != "" &
      flag_person_wide_flag18$err_joiteacher2 != "" &
      flag_person_wide_flag18$err_joiteacher3 == ""
  )
flag_person_wide_flag18[idx, c("err_joiteacher", "err_joiteacher2")] <-
  ""
flag_person_wide_flag18[idx, c("err_joiteacher3")] <-
  "合聘教師與巡迴教師人數偏多（請確認校內合聘教師、巡迴教師情形）"

idx <-
  which(
    flag_person_wide_flag18$err_joiteacher != "" &
      flag_person_wide_flag18$err_joiteacher2 == "" &
      flag_person_wide_flag18$err_joiteacher3 != ""
  )
flag_person_wide_flag18[idx, c("err_joiteacher")] <- ""

idx <-
  which(
    flag_person_wide_flag18$err_joiteacher == "" &
      flag_person_wide_flag18$err_joiteacher2 != "" &
      flag_person_wide_flag18$err_joiteacher3 != ""
  )
flag_person_wide_flag18[idx, c("err_joiteacher2")] <- ""

flag_person_wide_flag18$err_admin2 <-
  if_else(flag_person_wide_flag18$count_admin2 > 1,
          "教務處主管（主任）人數超過一位，請再協助確認實際聘任情況。",
          "")
flag_person_wide_flag18$err_admin3 <-
  if_else(flag_person_wide_flag18$count_admin3 > 1,
          "學務處主管（主任）人數超過一位，請再協助確認實際聘任情況。",
          "")
flag_person_wide_flag18$err_admin4 <-
  if_else(flag_person_wide_flag18$count_admin4 > 1,
          "總務處主管（主任）人數超過一位，請再協助確認實際聘任情況。",
          "")
flag_person_wide_flag18$err_admin5 <-
  if_else(flag_person_wide_flag18$count_admin5 > 1,
          "輔導室主管（主任）人數超過一位，請再協助確認實際聘任情況。",
          "")
flag_person_wide_flag18$err_admin6 <-
  if_else(flag_person_wide_flag18$count_admin6 > 1,
          "圖書館主管（主任）人數超過一位，請再協助確認實際聘任情況。",
          "")
flag_person_wide_flag18$err_admin8 <-
  if_else(flag_person_wide_flag18$count_admin8 > 1,
          "人事室主管（主任）人數超過一位，請再協助確認實際聘任情況。",
          "")
flag_person_wide_flag18$err_admin9 <-
  if_else(flag_person_wide_flag18$count_admin9 > 1,
          "主（會）計室主管（主任）人數超過一位，請再協助確認實際聘任情況。",
          "")


flag_person_wide_flag18$err_flag_txt <-
  paste(
    flag_person_wide_flag18$err_emptype,
    flag_person_wide_flag18$err_emptype2,
    flag_person_wide_flag18$err_empunit,
    flag_person_wide_flag18$err_empunit2,
    flag_person_wide_flag18$err_sertype,
    flag_person_wide_flag18$err_sertype2,
    flag_person_wide_flag18$err_admin2,
    flag_person_wide_flag18$err_admin3,
    flag_person_wide_flag18$err_admin4,
    flag_person_wide_flag18$err_admin5,
    flag_person_wide_flag18$err_admin6,
    flag_person_wide_flag18$err_admin8,
    flag_person_wide_flag18$err_admin9,
    flag_person_wide_flag18$err_skillteacher,
    flag_person_wide_flag18$err_counselor,
    flag_person_wide_flag18$err_speteacher,
    flag_person_wide_flag18$err_joiteacher,
    flag_person_wide_flag18$err_joiteacher2,
    flag_person_wide_flag18$err_joiteacher3,
    flag_person_wide_flag18$err_expecter,
    flag_person_wide_flag18$err_workexp,
    flag_person_wide_flag18$err_study,
    sep = " "
  )

# #產生檢誤報告文字
# flag18_temp <- flag_person_wide_flag18 %>%
#   group_by(organization_id) %>%
#   mutate(flag18_txt = paste(source, "需修改請假類別：", flag18_r, sep = ""), "") %>%
#   subset(select = c(organization_id, flag18_txt)) %>%
#   distinct(organization_id, flag18_txt)

if (dim(flag_person %>% subset(grepl(
  "\\S", flag_person_wide_flag18$err_flag_txt
)))[1] != 0) {
  #根據organization_id，展開成寬資料(wide)
  flag18 <- flag_person_wide_flag18 %>%
    subset(grepl("\\S", flag_person_wide_flag18$err_flag_txt)) %>%
    dcast(organization_id ~ err_flag_txt, value.var = "err_flag_txt")
  
  #合併教員資料表及職員(工)資料表報告文字
  temp <- colnames(flag18)[2:length(colnames(flag18))]
  flag18$flag18 <- NA
  for (i in temp) {
    flag18$flag18 <- paste(flag18$flag18, flag18[[i]], sep = "； ")
  }
  flag18$flag18 <- gsub("NA； ", replacement = "", flag18$flag18)
  flag18$flag18 <- gsub("； NA", replacement = "", flag18$flag18)
  
  #產生檢誤報告文字
  flag18 <- flag18 %>%
    subset(select = c(organization_id, flag18)) %>%
    distinct(organization_id, flag18)
  
  #刪除字串最後異常空格
  trim_t <- function (x) {
    gsub("\\s+|\\s+$", "", x)
  }
  
  flag18$flag18 <- trim_t(flag18$flag18) ##test
} else{
  #偵測flag18是否存在。若不存在，則產生NA行
  if ('flag18' %in% ls()) {
    print("flag18")
  } else{
    flag18 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    flag18$flag18 <- ""
  }
}
