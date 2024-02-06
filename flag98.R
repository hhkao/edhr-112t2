# flag98: 右欄所列人員身分識別碼與其他學校重複，且姓名、出生年月日、國籍別、最高學歷資訊與填列資料不同。 -------------------------------------------------------------------
#這裡是撈所有學校的資料來比對
flag_person <- drev_person %>%
  mutate(nation_recode = nation)

#最高學歷
flag_person$elv1 <- ""
flag_person$elv1 <- if_else(flag_person$ddegreen1 != "" &
                              flag_person$ddegreen1 != "N",
                            "博士",
                            flag_person$elv1)
flag_person$elv1 <- if_else(
  flag_person$ddegreen1 == "N" &
    flag_person$ddegreen2 == "N" &
    flag_person$mdegreen1 != "" &
    flag_person$mdegreen1 != "N",
  "碩士",
  flag_person$elv1
)
flag_person$elv1 <- if_else(
  flag_person$ddegreen1 == "N" &
    flag_person$ddegreen2 == "N" &
    flag_person$mdegreen1 == "N" &
    flag_person$mdegreen2 == "N" &
    flag_person$bdegreen1 != "" &
    flag_person$bdegreen1 != "N",
  "學士",
  flag_person$elv1
)
flag_person$elv1 <- if_else(
  flag_person$ddegreen1 == "N" &
    flag_person$ddegreen2 == "N" &
    flag_person$mdegreen1 == "N" &
    flag_person$mdegreen2 == "N" &
    flag_person$bdegreen1 == "N" &
    flag_person$bdegreen2 == "N" &
    flag_person$adegreen1 != "" &
    flag_person$adegreen1 != "N",
  "副學士",
  flag_person$elv1
)
flag_person$elv1 <-
  if_else(flag_person$degree == "N", "高中職以下", flag_person$elv1)


#刪除"籍" "藉"
flag_person$nation_recode <-
  gsub("籍", replacement = "", flag_person$nation_recode)
flag_person$nation_recode <-
  gsub("藉", replacement = "", flag_person$nation_recode)

#名詞統一
flag_person <- flag_person %>%
  mutate(
    nation_recode = recode(
      nation_recode,
      "TWN" = "本國",
      "中華民國" = "本國",
      "台灣" = "本國",
      "臺灣" = "本國",
      "大韓民國" = "韓國",
      "SocialistRepublicofVietnam" = "越南",
      "中國大陸" = "中國"
    )
  )

#姓名檢查
flag_person_name <- flag_person %>%
  select(c("organization_id",
           "edu_name2",
           "idnumber",
           "name",
           "source")) %>%
  group_by(idnumber) %>%
  filter(n_distinct(name) > 1) %>%
  ungroup() %>%
  mutate(err_flag_name = 1) %>%
  select("organization_id", "idnumber", "err_flag_name")

#出生年月日檢查
flag_person_birthdate <- flag_person %>%
  select(c(
    "organization_id",
    "edu_name2",
    "idnumber",
    "name",
    "birthdate",
    "source"
  )) %>%
  group_by(idnumber) %>%
  filter(n_distinct(birthdate) > 1) %>%
  ungroup() %>%
  mutate(err_flag_birthdate = 1) %>%
  select("organization_id", "idnumber", "err_flag_birthdate")

#國籍別檢查
flag_person_nation <- flag_person %>%
  select(
    c(
      "organization_id",
      "edu_name2",
      "idnumber",
      "name",
      "nation",
      "nation_recode",
      "source"
    )
  ) %>%
  group_by(idnumber) %>%
  filter(n_distinct(nation_recode) > 1) %>%
  ungroup() %>%
  mutate(err_flag_nation = 1)

#"國籍別"不合理的情況在flag8處理
flag_person_nation <- flag_person_nation %>%
  left_join(flag_person_flag8, by = c("idnumber")) %>%
  subset(err_flag_nation == 1 & is.na(err_flag)) %>%
  select("organization_id", "idnumber", "err_flag_nation")

#最高學位檢查
flag_person_elv1 <- flag_person %>%
  select(
    c(
      "organization_id",
      "edu_name2",
      "idnumber",
      "name",
      "emptype",
      "sertype",
      "elv1",
      "ddegreen1",
      "ddegreeu1",
      "ddegreeg1",
      "ddegreen2",
      "ddegreeu2",
      "ddegreeg2",
      "mdegreen1",
      "mdegreeu1",
      "mdegreeg1",
      "mdegreen2",
      "mdegreeu2",
      "mdegreeg2",
      "bdegreen1",
      "bdegreeu1",
      "bdegreeg1",
      "bdegreen2",
      "bdegreeu2",
      "bdegreeg2",
      "adegreen1",
      "adegreeu1",
      "adegreeg1",
      "adegreen2",
      "adegreeu2",
      "adegreeg2",
      "source"
    )
  ) %>%
  group_by(idnumber) %>%
  filter(n_distinct(elv1) > 1) %>%
  ungroup() %>%
  mutate(err_flag_elv1 = 1) %>%
  select("organization_id", "idnumber", "err_flag_elv1")

#合併檢查結果
flag_person <- flag_person %>%
  left_join(flag_person_name, by = c("organization_id", "idnumber")) %>%
  left_join(flag_person_birthdate,
            by = c("organization_id", "idnumber")) %>%
  left_join(flag_person_nation,
            by = c("organization_id", "idnumber")) %>%
  left_join(flag_person_elv1, by = c("organization_id", "idnumber"))

flag_person$err_flag_name[is.na(flag_person$err_flag_name)] <- 0
flag_person$err_flag_birthdate[is.na(flag_person$err_flag_birthdate)] <-
  0
flag_person$err_flag_nation[is.na(flag_person$err_flag_nation)] <- 0
flag_person$err_flag_elv1[is.na(flag_person$err_flag_elv1)] <- 0

flag_person$err_flag98 <-
  flag_person$err_flag_name + flag_person$err_flag_birthdate + flag_person$err_flag_nation + flag_person$err_flag_elv1

flag_person$err_flag <- 0
flag_person$err_flag <-
  if_else(flag_person$err_flag98 != 0, 1, flag_person$err_flag)

#加註
flag_person$name <- paste(flag_person$name, "（", sep = "")
flag_person$name <-
  if_else(
    flag_person$err_flag_name != 0,
    paste(flag_person$name, "姓名", "；", sep = ""),
    flag_person$name
  )
flag_person$name <-
  if_else(
    flag_person$err_flag_birthdate != 0,
    paste(flag_person$name,
          "出生年月日：",
          flag_person$birthdate,
          "；",
          sep = ""),
    flag_person$name
  )
flag_person$name <-
  if_else(
    flag_person$err_flag_nation != 0,
    paste(flag_person$name,
          "國籍別：",
          flag_person$nation,
          "；",
          sep = ""),
    flag_person$name
  )
flag_person$name <-
  if_else(
    flag_person$err_flag_elv1 != 0,
    paste(flag_person$name, "最高學歷：", flag_person$elv1, sep = ""),
    flag_person$name
  )
flag_person$name <- paste(flag_person$name, "）", sep = "")
flag_person$name <- gsub("；）", replacement = "）", flag_person$name)
flag_person$name <- gsub("（）", replacement = "", flag_person$name)

#呈現姓名
flag_person$err_flag_txt <- ""
flag_person$err_flag_txt <-
  case_when(flag_person$err_flag == 1 ~ flag_person$name,
            TRUE ~ flag_person$err_flag_txt)

if (dim(flag_person %>% subset(err_flag == 1))[1] != 0) {
  #根據organization_id + source，展開成寬資料(wide)
  flag_person_wide_flag98 <- flag_person %>%
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
    colnames(flag_person_wide_flag98)[3:length(colnames(flag_person_wide_flag98))]
  flag_person_wide_flag98$flag98_r <- NA
  for (i in temp) {
    flag_person_wide_flag98$flag98_r <-
      paste(flag_person_wide_flag98$flag98_r,
            flag_person_wide_flag98[[i]],
            sep = " ")
  }
  flag_person_wide_flag98$flag98_r <-
    gsub("NA ",
         replacement = "",
         flag_person_wide_flag98$flag98_r)
  flag_person_wide_flag98$flag98_r <-
    gsub(" NA",
         replacement = "",
         flag_person_wide_flag98$flag98_r)
  
  #（請確認上述人員最高學歷資訊，包括畢業學校及系所之全稱，如有錯誤請務必修正）
  #（請確認*員出生年月日及最高學歷資訊，如有錯誤請務必修正）
  #（請確認上述人員最高學歷資訊，如有錯誤併請修正）
  #（請確認*員出生年月日，如有錯誤併請修正）
  
  #（請確認並修正*員最高學歷資訊）
  #（請確認並修正*員出生年月日）
  #（請確認*員姓氏正確寫法）
  
  #產生檢誤報告文字
  flag98_temp <- flag_person_wide_flag98 %>%
    group_by(organization_id) %>%
    mutate(flag98_txt = paste(source, "：請確認該員基本資料：", flag98_r, sep = ""),
           "") %>%
    subset(select = c(organization_id, flag98_txt)) %>%
    distinct(organization_id, flag98_txt)
  
  #根據organization_id，展開成寬資料(wide)
  flag98 <- flag98_temp %>%
    dcast(organization_id ~ flag98_txt, value.var = "flag98_txt")
  
  #合併教員資料表及職員(工)資料表報告文字
  temp <- colnames(flag98)[2:length(colnames(flag98))]
  flag98$flag98 <- NA
  for (i in temp) {
    flag98$flag98 <- paste(flag98$flag98, flag98[[i]], sep = "； ")
  }
  flag98$flag98 <- gsub("NA； ", replacement = "", flag98$flag98)
  flag98$flag98 <- gsub("； NA", replacement = "", flag98$flag98)
  
  #產生檢誤報告文字
  flag98 <- flag98 %>%
    subset(select = c(organization_id, flag98)) %>%
    distinct(organization_id, flag98) %>%
    mutate(flag98 = paste(flag98, "", sep = ""))
} else{
  #偵測flag98是否存在。若不存在，則產生NA行
  if ('flag98' %in% ls()) {
    print("flag98")
  } else{
    flag98 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    flag98$flag98 <- ""
  }
}
