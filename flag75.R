# flag75: 若擔(兼)任正副校長、處室主任（組長）、秘書等行政職務，則其教學資料表「是否擔任協助行政教師」一欄不應填Y。 -------------------------------------------------------------------
flag_person <- drev_P_load %>%
  subset(load == 1)

flag_person$flag_admin0 <- 0  #校長、副校長
temp <- c("0", "1", "2", "3")
for (x in temp) {
  flag_person$flag_admin0 <- case_when(
    flag_person$sertype == "校長" |
      grepl("校長1", flag_person[[paste("adminunit", x, sep = "")]]) |
      grepl("副校長", flag_person[[paste("adminunit", x, sep = "")]]) ~ 1,
    TRUE ~ flag_person$flag_admin0
  )
}

flag_person$flag_admin1 <- 0  #一級主管
temp <- c("0", "1", "2", "3")
for (x in temp) {
  flag_person$flag_admin1 <-
    if_else((
      grepl("祕書", flag_person[[paste0("admintitle", x)]]) |
        grepl("秘書", flag_person[[paste0("admintitle", x)]]) |
        grepl("主任", flag_person[[paste0("admintitle", x)]])
    ) &
      grepl("校長", flag_person[[paste0("adminunit", x)]]),
    1,
    flag_person$flag_admin1)
  flag_person$flag_admin1 <-
    if_else(
      (grepl("主任", flag_person[[paste0("admintitle", x)]]) |
         grepl("部主任", flag_person[[paste0("admintitle", x)]])) &
        !grepl("主任教官", flag_person[[paste0("admintitle", x)]]) &
        !grepl("學程主任", flag_person[[paste0("admintitle", x)]]) &
        (
          grepl("教務", flag_person[[paste0("adminunit", x)]]) |
            grepl("學務", flag_person[[paste0("adminunit", x)]]) |
            grepl("學生事務", flag_person[[paste0("adminunit", x)]]) |
            grepl("總務", flag_person[[paste0("adminunit", x)]]) |
            grepl("輔導", flag_person[[paste0("adminunit", x)]]) |
            grepl("會計", flag_person[[paste0("adminunit", x)]]) |
            grepl("人事", flag_person[[paste0("adminunit", x)]]) |
            grepl("圖書", flag_person[[paste0("adminunit", x)]]) |
            grepl("實習", flag_person[[paste0("adminunit", x)]]) |
            grepl("進修", flag_person[[paste0("adminunit", x)]]) |
            grepl("資訊", flag_person[[paste0("adminunit", x)]]) |
            grepl("研究發展", flag_person[[paste0("adminunit", x)]]) |
            grepl("研發", flag_person[[paste0("adminunit", x)]]) |
            grepl("特殊教育", flag_person[[paste0("adminunit", x)]]) |
            grepl("特教", flag_person[[paste0("adminunit", x)]]) |
            grepl("建教", flag_person[[paste0("adminunit", x)]]) |
            grepl("技術交流", flag_person[[paste0("adminunit", x)]]) |
            grepl("國中部", flag_person[[paste0("adminunit", x)]]) |
            grepl("幼兒", flag_person[[paste0("adminunit", x)]]) |
            grepl("幼稚", flag_person[[paste0("adminunit", x)]]) |
            grepl("主計", flag_person[[paste0("adminunit", x)]]) |
            grepl("雙語部", flag_person[[paste0("adminunit", x)]]) |
            grepl("國小部", flag_person[[paste0("adminunit", x)]]) |
            grepl("藝文中心", flag_person[[paste0("adminunit", x)]]) |
            grepl("圖資中心", flag_person[[paste0("adminunit", x)]]) |
            grepl("國際部", flag_person[[paste0("adminunit", x)]]) |
            grepl("中心", flag_person[[paste0("adminunit", x)]])
        ),
      1,
      flag_person$flag_admin1
    )
}



flag_person$flag_admin2 <- 0  #二級主管
temp <- c("0", "1", "2", "3")
for (x in temp) {
  flag_person$flag_admin2 <- if_else(
    (
      grepl("組長", flag_person[[paste0("admintitle", x)]]) |
        grepl("副組長", flag_person[[paste0("admintitle", x)]]) |
        grepl("學程主任", flag_person[[paste0("admintitle", x)]])
    ) &
      (
        grepl("教務", flag_person[[paste0("adminunit", x)]]) |
          grepl("教學", flag_person[[paste0("adminunit", x)]]) |
          grepl("註冊", flag_person[[paste0("adminunit", x)]]) |
          grepl("設備", flag_person[[paste0("adminunit", x)]]) |
          grepl("試務", flag_person[[paste0("adminunit", x)]]) |
          grepl("課務", flag_person[[paste0("adminunit", x)]]) |
          grepl("實習", flag_person[[paste0("adminunit", x)]]) |
          grepl("就業輔導", flag_person[[paste0("adminunit", x)]]) |
          grepl("就輔", flag_person[[paste0("adminunit", x)]]) |
          grepl("實驗研究", flag_person[[paste0("adminunit", x)]]) |
          grepl("實研", flag_person[[paste0("adminunit", x)]]) |
          grepl("學務", flag_person[[paste0("adminunit", x)]]) |
          grepl("學生事務", flag_person[[paste0("adminunit", x)]]) |
          grepl("訓育", flag_person[[paste0("adminunit", x)]]) |
          grepl("生活輔導", flag_person[[paste0("adminunit", x)]]) |
          grepl("生輔", flag_person[[paste0("adminunit", x)]]) |
          grepl("體育", flag_person[[paste0("adminunit", x)]]) |
          grepl("衛生", flag_person[[paste0("adminunit", x)]]) |
          grepl("社團活動", flag_person[[paste0("adminunit", x)]]) |
          grepl("總務", flag_person[[paste0("adminunit", x)]]) |
          grepl("文書", flag_person[[paste0("adminunit", x)]]) |
          grepl("庶務", flag_person[[paste0("adminunit", x)]]) |
          grepl("出納", flag_person[[paste0("adminunit", x)]]) |
          grepl("輔導", flag_person[[paste0("adminunit", x)]]) |
          grepl("資料", flag_person[[paste0("adminunit", x)]]) |
          grepl("資訊", flag_person[[paste0("adminunit", x)]]) |
          grepl("會計", flag_person[[paste0("adminunit", x)]]) |
          grepl("人事", flag_person[[paste0("adminunit", x)]]) |
          grepl("圖書", flag_person[[paste0("adminunit", x)]]) |
          grepl("技術服務", flag_person[[paste0("adminunit", x)]]) |
          grepl("讀者服務", flag_person[[paste0("adminunit", x)]]) |
          grepl("資訊媒體", flag_person[[paste0("adminunit", x)]]) |
          grepl("實習", flag_person[[paste0("adminunit", x)]]) |
          grepl("技能檢定", flag_person[[paste0("adminunit", x)]]) |
          grepl("建教", flag_person[[paste0("adminunit", x)]]) |
          grepl("建合", flag_person[[paste0("adminunit", x)]]) |
          grepl("實用技能", flag_person[[paste0("adminunit", x)]]) |
          grepl("實技", flag_person[[paste0("adminunit", x)]]) |
          grepl("特殊教育", flag_person[[paste0("adminunit", x)]]) |
          grepl("特教", flag_person[[paste0("adminunit", x)]]) |
          grepl("衛生", flag_person[[paste0("adminunit", x)]]) |
          grepl("進修", flag_person[[paste0("adminunit", x)]]) |
          grepl("主計", flag_person[[paste0("adminunit", x)]]) |
          grepl("研究發展", flag_person[[paste0("adminunit", x)]]) |
          grepl("研發", flag_person[[paste0("adminunit", x)]]) |
          grepl("技術交流", flag_person[[paste0("adminunit", x)]]) |
          grepl("科", flag_person[[paste0("adminunit", x)]]) |
          grepl("國際處", flag_person[[paste0("adminunit", x)]]) |
          grepl("招生處", flag_person[[paste0("adminunit", x)]]) |
          grepl("國中部", flag_person[[paste0("adminunit", x)]]) |
          grepl("幼兒", flag_person[[paste0("adminunit", x)]]) |
          grepl("幼稚", flag_person[[paste0("adminunit", x)]]) |
          grepl("教官室", flag_person[[paste0("adminunit", x)]]) |
          grepl("董事會", flag_person[[paste0("adminunit", x)]]) |
          grepl("中心", flag_person[[paste0("adminunit", x)]]) |
          grepl("國際部", flag_person[[paste0("adminunit", x)]]) |
          grepl("國小部", flag_person[[paste0("adminunit", x)]])
      )
    ,
    1,
    flag_person$flag_admin2
  )
}

flag_person$flag_admin3 <- 0  #其他行政職
temp <- c("0", "1", "2", "3")
for (x in temp) {
  flag_person$flag_admin3 <- if_else(
    (
      flag_person[[paste0("admintitle", x)]] != "N" &
        flag_person[[paste0("admintitle", x)]] != "董事長" &
        flag_person$flag_admin1 == 0 &
        flag_person$flag_admin2 == 0
    )
    ,
    1,
    flag_person$flag_admin3
  )
}

#取最高職位的主管，一人僅算一次
flag_person$flag_admin1 <-
  if_else(flag_person$flag_admin0 == 1, 0, flag_person$flag_admin1)
flag_person$flag_admin2 <-
  if_else(
    flag_person$flag_admin0 == 1 |
      flag_person$flag_admin1 == 1,
    0,
    flag_person$flag_admin2
  )
flag_person$flag_admin3 <-
  if_else(
    flag_person$flag_admin0 == 1 |
      flag_person$flag_admin1 == 1 |
      flag_person$flag_admin2 == 1,
    0,
    flag_person$flag_admin3
  )

#正副校長、一級主管、二級主管、非主管級行政人員四種身分歸類在同一變數
flag_person$admin <- 0
flag_person$admin <-
  if_else(flag_person$flag_admin0 == 1, 1, flag_person$admin)
flag_person$admin <-
  if_else(flag_person$flag_admin1 == 1, 2, flag_person$admin)
flag_person$admin <-
  if_else(flag_person$flag_admin2 == 1, 3, flag_person$admin)
flag_person$admin <-
  if_else(flag_person$flag_admin3 == 1, 4, flag_person$admin)


#若擔(兼)任正副校長、處室主任（組長）、秘書等行政職務，「是否擔任協助行政教師」填Y則抓出
flag_person$err_flag <- 0
flag_person$err_flag <-
  if_else(
    (
      flag_person$admin == 1 |
        flag_person$admin == 2 |
        flag_person$admin == 3
    ) & flag_person$adminteacher == "Y",
    1,
    flag_person$err_flag
  )

flag_person$admintitle0 <-
  if_else(
    flag_person$admintitle0 == "N" &
      flag_person$err_flag == 1,
    "",
    flag_person$admintitle0
  )
flag_person$adminunit0 <-
  if_else(flag_person$adminunit0 == "N" &
            flag_person$err_flag == 1,
          "",
          flag_person$adminunit0)
flag_person$admintitle1 <-
  if_else(
    flag_person$admintitle1 == "N" &
      flag_person$err_flag == 1,
    "",
    flag_person$admintitle1
  )
flag_person$adminunit1 <-
  if_else(flag_person$adminunit1 == "N" &
            flag_person$err_flag == 1,
          "",
          flag_person$adminunit1)
flag_person$admintitle2 <-
  if_else(
    flag_person$admintitle2 == "N" &
      flag_person$err_flag == 1,
    "",
    flag_person$admintitle2
  )
flag_person$adminunit2 <-
  if_else(flag_person$adminunit2 == "N" &
            flag_person$err_flag == 1,
          "",
          flag_person$adminunit2)
flag_person$admintitle3 <-
  if_else(
    flag_person$admintitle3 == "N" &
      flag_person$err_flag == 1,
    "",
    flag_person$admintitle3
  )
flag_person$adminunit3 <-
  if_else(flag_person$adminunit3 == "N" &
            flag_person$err_flag == 1,
          "",
          flag_person$adminunit3)

flag_person$admin_all <-
  paste(
    flag_person$adminunit0,
    flag_person$admintitle0,
    ";",
    flag_person$adminunit1,
    flag_person$admintitle1,
    ";",
    flag_person$adminunit2,
    flag_person$admintitle2,
    ";",
    flag_person$adminunit3,
    flag_person$admintitle3,
    sep = ""
  )

flag_person$name <-
  if_else(
    flag_person$err_flag == 1 &
      flag_person$admin ==  1 &
      flag_person$sertype == "校長",
    paste0(flag_person$name, "（", flag_person$sertype, "）"),
    flag_person$name
  )
flag_person$name <-
  if_else(
    flag_person$err_flag == 1 &
      flag_person$admin ==  1 &
      flag_person$source == "職員(工)資料表",
    paste0(flag_person$name, "（", flag_person$admin_all, "）"),
    flag_person$name
  )
flag_person$name <-
  if_else(
    flag_person$err_flag == 1 &
      flag_person$admin ==  2,
    paste0(flag_person$name, "（", flag_person$admin_all, "）"),
    flag_person$name
  )
flag_person$name <-
  if_else(
    flag_person$err_flag == 1 &
      flag_person$admin ==  3,
    paste0(flag_person$name, "（", flag_person$admin_all, "）"),
    flag_person$name
  )

flag_person$name <-
  gsub(";;;）", replacement = "）", flag_person$name)
flag_person$name <- gsub(";;）", replacement = "）", flag_person$name)
flag_person$name <- gsub(";）", replacement = "）", flag_person$name)
flag_person$name <-
  gsub("（;;;", replacement = "（", flag_person$name)
flag_person$name <- gsub("（;;", replacement = "（", flag_person$name)
flag_person$name <- gsub("（;", replacement = "（", flag_person$name)

#加註
flag_person$err_flag_txt <- ""
flag_person$err_flag_txt <- case_when(
  flag_person$err_flag == 1 ~ paste(flag_person$name,
                                    sep = ""),
  TRUE ~ flag_person$err_flag_txt
)

if (dim(flag_person %>% subset(err_flag == 1))[1] != 0) {
  #根據organization_id + source，展開成寬資料(wide)
  flag_person_wide_flag75 <- flag_person %>%
    subset(select = c(
      organization_id,
      idnumber,
      err_flag_txt,
      edu_name2,
      source,
      err_flag
    )) %>%
    subset(err_flag == 1) %>%
    dcast(organization_id ~ err_flag_txt, value.var = "err_flag_txt")
  
  #合併所有name
  temp <-
    colnames(flag_person_wide_flag75)[2:length(colnames(flag_person_wide_flag75))]
  flag_person_wide_flag75$flag75_r <- NA
  for (i in temp) {
    flag_person_wide_flag75$flag75_r <-
      paste(flag_person_wide_flag75$flag75_r,
            flag_person_wide_flag75[[i]],
            sep = " ")
  }
  flag_person_wide_flag75$flag75_r <-
    gsub("NA ",
         replacement = "",
         flag_person_wide_flag75$flag75_r)
  flag_person_wide_flag75$flag75_r <-
    gsub(" NA",
         replacement = "",
         flag_person_wide_flag75$flag75_r)
  
  #產生檢誤報告文字
  flag75_temp <- flag_person_wide_flag75 %>%
    group_by(organization_id) %>%
    mutate(flag75_txt = paste(flag75_r, sep = ""), "") %>%
    subset(select = c(organization_id, flag75_txt)) %>%
    distinct(organization_id, flag75_txt)
  
  #根據organization_id，展開成寬資料(wide)
  flag75 <- flag75_temp %>%
    dcast(organization_id ~ flag75_txt, value.var = "flag75_txt")
  
  #合併教員資料表及職員(工)資料表報告文字
  temp <- colnames(flag75)[2:length(colnames(flag75))]
  flag75$flag75 <- NA
  for (i in temp) {
    flag75$flag75 <- paste(flag75$flag75, flag75[[i]], sep = "； ")
  }
  flag75$flag75 <- gsub("NA； ", replacement = "", flag75$flag75)
  flag75$flag75 <- gsub("； NA", replacement = "", flag75$flag75)
  
  #產生檢誤報告文字
  flag75 <- flag75 %>%
    subset(select = c(organization_id, flag75)) %>%
    distinct(organization_id, flag75) %>%
    mutate(flag75 = paste("請確認以下人員擔(兼)任行政或教學相關職務情況：",
                          flag75,
                          sep = ""))
} else{
  #偵測flag75是否存在。若不存在，則產生NA行
  if ('flag75' %in% ls()) {
    print("flag75")
  } else{
    flag75 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    flag75$flag75 <- ""
  }
}
