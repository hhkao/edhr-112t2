# flag95: 請確認專任教師名單是否完整。 (內參)-------------------------------------------------------------------
flag_person <- drev_person_1

#教育部統計處公布專任教師/兼任教師/職員人數
#跟統計處比較的分析，先內部參閱，暫不納入檢核，但我認為也許可以看不同波次學校填的人數比較，尤其是專任，應該不會差太多，若像跟統計處比較一樣差異太大，就是有問題

filename <- "./112_base0_revise.xlsx"

# 讀取檔案
moe_111_base0 <- read_excel(filename)

#統計處"專任教師"定義：以實際現有(編制內)人數計算，包括校長(大專附設除外)、超額分發教師、專任輔導教師、長期代理教師、特教班專任教師、原住民專任教師及教官，不含運動教練。服兵役及留職停薪教師，以占實缺之長期代理教師資料計列。
flag_person$count_emptype1 <- if_else(
  flag_person$sertype == "教師" &
    (
      flag_person$emptype == "專任" |
        flag_person$emptype == "代理" | flag_person$emptype == "代理(連)"
    ) |
    flag_person$sertype == "校長" |
    flag_person$sertype == "教官" |
    flag_person$sertype == "主任教官"
  ,
  1,
  0
)

#統計處"兼任教師"定義：係指以部分時間擔任學校編制內教師依規定排課後尚餘之課務或特殊類科之課務者，已計列本表專任教師者除外。
flag_person$count_emptype2 <-
  if_else(flag_person$sertype == "教師" &
            flag_person$emptype == "兼任",
          1,
          0)

#統計處"職員"定義：依據「高級中等學校組織設置及員額編制標準」第8條，以實際現職(編制內)人數計算，包括辦理行政工作及一般技術工作之專任人員(含技士、技佐、營養師、護理師(或護士)、專任運動教練、救生員或運動傷害防護員、管理員及實習指導員等)。
flag_person$count_staff <-
  if_else(flag_person$source == "職員(工)資料表" &
            flag_person$emptype == "專任",
          1,
          0)

flag_person$count_emptype1 <-
  if_else(is.na(flag_person$count_emptype1),
          0,
          flag_person$count_emptype1)
flag_person$count_emptype2 <-
  if_else(is.na(flag_person$count_emptype2),
          0,
          flag_person$count_emptype2)

flag_person_wide_flag95 <-
  aggregate(
    cbind(count_emptype1, count_emptype2, count_staff) ~ organization_id,
    flag_person,
    sum
  ) %>%
  left_join(moe_111_base0, by = "organization_id")

flag_person_wide_flag95$count_emptype1_1 <-
  as.numeric(flag_person_wide_flag95$count_emptype1_1)
flag_person_wide_flag95$count_emptype2_1 <-
  as.numeric(flag_person_wide_flag95$count_emptype2_1)
flag_person_wide_flag95$count_staff_1 <-
  as.numeric(flag_person_wide_flag95$count_staff_1)


flag_person_wide_flag95$flag_err <- 0
flag_person_wide_flag95$err_emptype1 <-
  (
    flag_person_wide_flag95$count_emptype1 - flag_person_wide_flag95$count_emptype1_1
  ) / flag_person_wide_flag95$count_emptype1
flag_person_wide_flag95$err_emptype2 <-
  (
    flag_person_wide_flag95$count_emptype2 - flag_person_wide_flag95$count_emptype2_1
  ) / flag_person_wide_flag95$count_emptype2
flag_person_wide_flag95$err_staff <-
  (flag_person_wide_flag95$count_staff - flag_person_wide_flag95$count_staff_1) / flag_person_wide_flag95$count_staff

flag_person_wide_flag95$err_emptype1 <-
  scales::percent(flag_person_wide_flag95$err_emptype1, accuracy = 0.1)
flag_person_wide_flag95$err_emptype2 <-
  scales::percent(flag_person_wide_flag95$err_emptype2, accuracy = 0.1)
flag_person_wide_flag95$err_staff <-
  scales::percent(flag_person_wide_flag95$err_staff, accuracy = 0.1)

#請確認貴校所填專任教師、代理教師、校長、教官、主任教官名單資料是否完整。
#請確認是否完整填報專任教員名單資料。

flag_person_wide_flag95$err_flag_txt <- paste0(
  "統計處專任教師人數：",
  flag_person_wide_flag95$count_emptype1_1,
  "人；",
  "本資料庫專任教師、代理教師、校長、教官、主任教官人數：",
  flag_person_wide_flag95$count_emptype1,
  "；差異百分比",
  flag_person_wide_flag95$err_emptype1
)

#產生檢誤報告文字
flag95 <- flag_person_wide_flag95 %>%
  subset(select = c(organization_id, err_flag_txt)) %>%
  rename(flag95 = err_flag_txt) %>%
  distinct(organization_id, flag95)
