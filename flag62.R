# flag62: 職務名稱及兼任行政職職稱（一）～（三）資料內容是否完整正確。-------------------------------------------------------------------
#如：1.	職務名稱及兼任行政職職稱（一）～（三）填入非職稱內容。
#2.	服務單位及兼任行政職服務單位（一）～（三）填入非服務單位內容。
#3.	校長、教官、主任教官屬於教員，故應填至教員資料表。
#職員工的「職務名稱」不應有教師、老師等非行政工作之名稱。
flag_person <- drev_person_1

# 檢查admintitle adminunit admintitle1 adminunit1 admintitle2 adminunit2 admintitle3 adminunit3是否有任一個值包含全形"Ｎ"
if ("Ｎ" %in% flag_person$admintitle0  ||
    "Ｎ" %in% flag_person$adminunit0   ||
    "Ｎ" %in% flag_person$admintitle1 ||
    "Ｎ" %in% flag_person$adminunit1  ||
    "Ｎ" %in% flag_person$admintitle2 ||
    "Ｎ" %in% flag_person$adminunit2  ||
    "Ｎ" %in% flag_person$admintitle3 ||
    "Ｎ" %in% flag_person$adminunit3) {
  # 如果包含"Ｎ"，則篩選organization_id對應的值
  flag62_detectN <- flag_person %>% filter(
    admintitle0 == "Ｎ" |
      adminunit0 == "Ｎ" |
      admintitle1 == "Ｎ" |
      adminunit1 == "Ｎ" |
      admintitle2 == "Ｎ" |
      adminunit2 == "Ｎ" |
      admintitle3 == "Ｎ" |
      adminunit3 == "Ｎ"
  ) %>% distinct(organization_id)
  cat("Ｎ exists.\n")
  flag_person$admintitle0[flag_person$admintitle0 == "Ｎ"] <- "N"
  flag_person$adminunit0[flag_person$adminunit0 == "Ｎ"] <- "N"
  flag_person$admintitle1[flag_person$admintitle1 == "Ｎ"] <- "N"
  flag_person$adminunit1[flag_person$adminunit1 == "Ｎ"] <- "N"
  flag_person$admintitle2[flag_person$admintitle2 == "Ｎ"] <- "N"
  flag_person$adminunit2[flag_person$adminunit2 == "Ｎ"] <- "N"
  flag_person$admintitle3[flag_person$admintitle3 == "Ｎ"] <- "N"
  flag_person$adminunit3[flag_person$adminunit3 == "Ｎ"] <- "N"
  
  flag62_detectN$detectN <- "1"
} else {
  flag62_detectN <-
    data.frame(organization_id = "", detectN = "")  # 創建一個空的DataFrame
  cat("Ｎ does not exist.\n")
}


#職務名稱
flag_person$err_admintitle0 <- 1
flag_person$err_admintitle0 <-
  if_else(grepl("主任$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("佐理員$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("助理$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("人員$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("助理員$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("技士", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("技工", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("技佐", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("防護員$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("組長$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("組員$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("管理員$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("管理師$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("輔導員$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("工友$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("職工$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("^約僱", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("^約聘", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
#不可只填約僱 約雇 約聘 約聘僱 約聘
flag_person$err_admintitle0 <-
  if_else(grepl("^約僱$", flag_person$admintitle0),
          1,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("^約雇$", flag_person$admintitle0),
          1,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("^約聘$", flag_person$admintitle0),
          1,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("^約聘僱$", flag_person$admintitle0),
          1,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("^約聘$", flag_person$admintitle0),
          1,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("書記$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("幹事", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("^學務創新", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("創新人力", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("營養師$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("^職輔員$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("護士$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("護理師$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("^均質化承辦人$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("^均職化承辦人$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("^優質化協辦人$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("校安$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("心理師$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("技術員$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("職輔員$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("廚工$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("編制外行政人力$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("司機$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("秘書$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("祕書$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("???書$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("舍監$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("辦事員$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("事務員$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("職務代理$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("職代$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("職務代理人$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("救生員$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("值機員$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("監督$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("三副$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("社工師$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("校護$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("專員$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("雇員$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("僱員$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("^充實行政人力$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("1", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("工讀生$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("工讀$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("警衛$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("^實習餐廳經理$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("清潔員$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("^清潔$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("佐理$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("^會計員$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("^水電$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("^總機$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("^園藝$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("電工$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("^木工$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("^守衛$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("校長$", flag_person$admintitle0),
          1,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("^副校長$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("館員$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("^出納$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("庶務$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("環保$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("體衛$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("^書院Coach$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("執行長$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("顧問$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("^助教$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("^督導$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("教師$", flag_person$admintitle0),
          1,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("老師$", flag_person$admintitle0),
          1,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("導師$", flag_person$admintitle0),
          1,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("教學支援人員$", flag_person$admintitle0),
          1,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("^董事長$", flag_person$admintitle0),
          1,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("指導員$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("補充行政人力$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("^學創人力$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(
    grepl("書記\\(控障-公務人員\\)$", flag_person$admintitle0),
    0,
    flag_person$err_admintitle0
  )
flag_person$err_admintitle0 <-
  if_else(grepl("^校警$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("^專任行政人力$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("安心上工", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("護理員", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("工程師$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("設計師$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("駕駛", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("^網管$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("守衛$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("廚師$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("經理$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("技術士$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("校工$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("門衛$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("^廚房幫廚$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("保全", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("人事員$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("主廚$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("教練$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("館長$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("^臨時工$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("^監廚$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("^牧師$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
#光禾華德福才可有以下職稱：總務、學務、教務、輔導、人事、國中部行政、高中部行政、會計
flag_person$err_admintitle0 <-
  if_else(
    grepl("^國中部行政$", flag_person$admintitle0) &
      flag_person$organization_id == "121302",
    0,
    flag_person$err_admintitle0
  )
flag_person$err_admintitle0 <-
  if_else(
    grepl("^高中部行政$", flag_person$admintitle0) &
      flag_person$organization_id == "121302",
    0,
    flag_person$err_admintitle0
  )
flag_person$err_admintitle0 <-
  if_else(
    grepl("^總務$", flag_person$admintitle0) &
      flag_person$organization_id == "121302",
    0,
    flag_person$err_admintitle0
  )
flag_person$err_admintitle0 <-
  if_else(
    grepl("^學務$", flag_person$admintitle0) &
      flag_person$organization_id == "121302",
    0,
    flag_person$err_admintitle0
  )
flag_person$err_admintitle0 <-
  if_else(
    grepl("^教務$", flag_person$admintitle0) &
      flag_person$organization_id == "121302",
    0,
    flag_person$err_admintitle0
  )
flag_person$err_admintitle0 <-
  if_else(
    grepl("^輔導$", flag_person$admintitle0) &
      flag_person$organization_id == "121302",
    0,
    flag_person$err_admintitle0
  )
flag_person$err_admintitle0 <-
  if_else(
    grepl("^人事$", flag_person$admintitle0) &
      flag_person$organization_id == "121302",
    0,
    flag_person$err_admintitle0
  )
flag_person$err_admintitle0 <-
  if_else(
    grepl("^會計$", flag_person$admintitle0) &
      flag_person$organization_id == "121302",
    0,
    flag_person$err_admintitle0
  )
#東方工商才可有以下職稱：職員
flag_person$err_admintitle0 <-
  if_else(
    grepl("^職員$", flag_person$admintitle0) &
      flag_person$organization_id == "331402",
    0,
    flag_person$err_admintitle0
  )
#仁義高中才可有以下職稱：會計
flag_person$err_admintitle0 <-
  if_else(
    grepl("^會計$", flag_person$admintitle0) &
      flag_person$organization_id == "201309",
    0,
    flag_person$err_admintitle0
  )
flag_person$err_admintitle0 <-
  if_else(flag_person$source == 1,
          0,
          flag_person$err_admintitle0)
#運動教練已在flag34檢查
flag_person$err_admintitle0 <-
  if_else(grepl("教練$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
#教官已在flag15檢查
flag_person$err_admintitle0 <-
  if_else(grepl("教官$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
#N或NA已在flag49檢查
flag_person$err_admintitle0 <-
  if_else(grepl("^N$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
flag_person$err_admintitle0 <-
  if_else(grepl("^NA$", flag_person$admintitle0),
          0,
          flag_person$err_admintitle0)
#私立光復高中(181305)才可有以下職稱：駐廠(學校稱呼他為"駐廠老師"，但他沒有授課，也不算老師，所以職稱就改為"駐廠")
flag_person$err_admintitle0 <-
  if_else(
    grepl("^駐廠$", flag_person$admintitle0) &
      flag_person$organization_id == "181305",
    0,
    flag_person$err_admintitle0
  )


#服務單位
flag_person$err_adminunit0 <- 1
flag_person$err_adminunit0 <-
  if_else(grepl("^人事室$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^主計室$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^校長室$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^副校長室$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^特教辦公室$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("秘書室$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("祕書室$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("秘書處$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("祕書處$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^小學部$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^國小部$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^國中部$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^中學部$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^國教署$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^教官室$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^教官室\\(軍訓室\\)$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^教務處", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^進修部$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^進修學校$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^圖書館$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^圖書室$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^實習處", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^實習室$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^實習農場$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^實習輔導處$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^輔導室$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("輔導處$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^學生事務處", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^學務處", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^總務處", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^雙語部$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^專案辦公室$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^軍訓室$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^會計室$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^會計部$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^資訊室$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^國際部$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^董事會$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("中心$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("研究發展處$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^保健室$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^招生處$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^公關室$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^農場經營$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^藝文中心", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("科$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("1", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(
    grepl("組$", flag_person$adminunit0) &
      (
        grepl("*處*", flag_person$adminunit0) |
          grepl("*中心*", flag_person$adminunit0) |
          grepl("*部*", flag_person$adminunit0) |
          grepl("*室*", flag_person$adminunit0) |
          grepl("*館*", flag_person$adminunit0)
      ),
    0,
    flag_person$err_adminunit0
  )
flag_person$err_adminunit0 <-
  if_else(grepl("^國中部教務處$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^高中部教務處$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^中學部教務處$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^國際部教務處$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^中學部學務處$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("校區校長室$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^圖資室$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^圖資處$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^實習就業處$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^總務室$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^社區大學$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^國際教育處$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^研發處$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^資訊處$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^實輔處$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^住校處$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^發展事務處$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^家具設計發展處$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^國際處$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^研發室$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^圖資室兼技術交流處$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^教育推廣處$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^學輔處$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^顧問室$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^研究發展室$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^小學部籌備處$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^綜合高中$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^技術交流處$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("實驗室$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^資源班$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^外語部$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^外語處$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^研發部$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^國際事務處$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^招生辦公室$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("國際處$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^電腦室$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^招生部$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^宿舍處$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^人文室$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("服務處$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^員生社$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^教學資源中心處$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^校務發展室$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^校牧室$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^宗輔室$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^油印室$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^公共事務室$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("辦事處$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^生命教育室$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^國際室$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^教學研究室$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^國際暨建教處$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^外語中心處$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("語文中心$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^國小部總務處$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^外語教學處$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
#立志高中才有"高國中部"
flag_person$err_adminunit0 <-
  if_else(
    grepl("^高國中部$", flag_person$adminunit0) &
      flag_person$organization_id == "551301",
    0,
    flag_person$err_adminunit0
  )
#光禾華德福才可有以下服務單位：國中部日間部、高中部日間部
flag_person$err_adminunit0 <-
  if_else(
    grepl("^國中部日間部$", flag_person$adminunit0) &
      flag_person$organization_id == "121302",
    0,
    flag_person$err_adminunit0
  )
flag_person$err_adminunit0 <-
  if_else(
    grepl("^高中部日間部$", flag_person$adminunit0) &
      flag_person$organization_id == "121302",
    0,
    flag_person$err_adminunit0
  )
#N或NA已在flag49檢查
flag_person$err_adminunit0 <-
  if_else(grepl("^N$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(grepl("^NA$", flag_person$adminunit0),
          0,
          flag_person$err_adminunit0)
flag_person$err_adminunit0 <-
  if_else(flag_person$source == 1, 0, flag_person$err_adminunit0)
#台北市育達高中 雲林縣維多利亞實驗高中有"教導處"
flag_person$err_adminunit0 <-
  if_else(
    grepl("^教導處$", flag_person$adminunit0) &
      flag_person$organization_id == "311401",
    0,
    flag_person$err_adminunit0
  )
flag_person$err_adminunit0 <-
  if_else(
    grepl("^教導處$", flag_person$adminunit0) &
      flag_person$organization_id == "091320",
    0,
    flag_person$err_adminunit0
  )
#磐石高中有"國中部雙語班"
flag_person$err_adminunit0 <-
  if_else(
    grepl("^國中部雙語班$", flag_person$adminunit0) &
      flag_person$organization_id == "181307",
    0,
    flag_person$err_adminunit0
  )
#私立光復高中(181305)有"完全中學部"
flag_person$err_adminunit0 <-
  if_else(
    grepl("^完全中學部$", flag_person$adminunit0) &
      flag_person$organization_id == "181305",
    0,
    flag_person$err_adminunit0
  )

#兼任行政職職稱（一）
flag_person$err_admintitle1 <- 1
flag_person$err_admintitle1 <-
  if_else(grepl("主任$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("秘書$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("組長$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("組員$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("^副校長$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("^均質化承辦人$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("^均職化承辦人$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("^優質化協辦人$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("校安$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("心理師$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("技術員$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("防護員$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("人員$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("職輔員$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("廚工$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("營養師$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("編制外行政人力$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("司機$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("秘書$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("祕書$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("舍監$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("辦事員$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("幹事$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("職務代理$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("職代$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("職務代理人$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("救生員$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("值機員$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("監督$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("三副$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("社工師$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("助理$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("專員$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("政風$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("1", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("^N$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("輔導員$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("^警衛$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("^實習餐廳經理$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("管理師$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("清潔員$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("佐理$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("技佐$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("^會計員$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("管理員$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("^書記$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("佐理員$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("館員$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("科主席$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("護理師$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("助理員$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("^助教$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("庶務$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("文書$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("^電競專案教練$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("^出納$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("^助教$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("^督導$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("訓育業務$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("指導員$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("駕駛", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("^網管$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("守衛$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("廚師$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("經理$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("技術士$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("校工$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("門衛$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("^廚房幫廚$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("^臨時約聘助理(計時)$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("保全", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("人事員$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("主廚$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("教練$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("館長$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("技士$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("技工$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("^監廚$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
#光禾華德福才可有以下職稱：總務、學務、教務、輔導、人事、國中部行政、高中部行政、會計
flag_person$err_admintitle1 <-
  if_else(
    grepl("^國中部行政$", flag_person$admintitle1) &
      flag_person$organization_id == "121302",
    0,
    flag_person$err_admintitle1
  )
flag_person$err_admintitle1 <-
  if_else(
    grepl("^高中部行政$", flag_person$admintitle1) &
      flag_person$organization_id == "121302",
    0,
    flag_person$err_admintitle1
  )
flag_person$err_admintitle1 <-
  if_else(
    grepl("^總務$", flag_person$admintitle1) &
      flag_person$organization_id == "121302",
    0,
    flag_person$err_admintitle1
  )
flag_person$err_admintitle1 <-
  if_else(
    grepl("^學務$", flag_person$admintitle1) &
      flag_person$organization_id == "121302",
    0,
    flag_person$err_admintitle1
  )
flag_person$err_admintitle1 <-
  if_else(
    grepl("^教務$", flag_person$admintitle1) &
      flag_person$organization_id == "121302",
    0,
    flag_person$err_admintitle1
  )
flag_person$err_admintitle1 <-
  if_else(
    grepl("^輔導$", flag_person$admintitle1) &
      flag_person$organization_id == "121302",
    0,
    flag_person$err_admintitle1
  )
flag_person$err_admintitle1 <-
  if_else(
    grepl("^人事$", flag_person$admintitle1) &
      flag_person$organization_id == "121302",
    0,
    flag_person$err_admintitle1
  )
flag_person$err_admintitle1 <-
  if_else(
    grepl("^會計$", flag_person$admintitle1) &
      flag_person$organization_id == "121302",
    0,
    flag_person$err_admintitle1
  )
flag_person$err_admintitle1 <-
  if_else(grepl("^校長$", flag_person$admintitle1),
          1,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("教官$", flag_person$admintitle1),
          1,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("教師$", flag_person$admintitle1),
          1,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("運動教練$", flag_person$admintitle1),
          1,
          flag_person$err_admintitle1)
flag_person$err_admintitle1 <-
  if_else(grepl("^董事長$", flag_person$admintitle1),
          1,
          flag_person$err_admintitle1)
#校長已在flag15檢查
flag_person$err_admintitle1 <-
  if_else(grepl("^校長$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
#導師已在flag15檢查
flag_person$err_admintitle1 <-
  if_else(grepl("導師$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
#教師已在flag15檢查
flag_person$err_admintitle1 <-
  if_else(grepl("教師$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
#教官已在flag15檢查
flag_person$err_admintitle1 <-
  if_else(grepl("教官$", flag_person$admintitle1),
          0,
          flag_person$err_admintitle1)
#私立立仁高中(201314)才可有以下職稱：人事
flag_person$err_admintitle1 <-
  if_else(
    grepl("^人事$", flag_person$admintitle1) &
      flag_person$organization_id == "201314",
    0,
    flag_person$err_admintitle1
  )


#兼任行政職服務單位（一）
flag_person$err_adminunit1 <- 1
flag_person$err_adminunit1 <-
  if_else(grepl("^校長室$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^副校長室$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^秘書室$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^國小部", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^小學部$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(
    grepl("^國中部$", flag_person$adminunit1) |
      grepl("^國民中學部$", flag_person$adminunit1),
    0,
    flag_person$err_adminunit1
  )
flag_person$err_adminunit1 <-
  if_else(grepl("^中學部$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^教官室$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^教務處", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^進修部", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^進修學校$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^資訊室$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^圖書館", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^圖書室$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^實習處", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^實習室$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^輔導室", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("輔導處", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^學生事務處", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^學務處", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^總務處", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^雙語部", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^軍訓室$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^會計室$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^主計室$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^資訊室$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^國際部$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^董事會$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("中心$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("研究發展處$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("人事室$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^招生處$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^公關室$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^農場經營$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^藝文中心", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("科$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("1", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(
    grepl("組$", flag_person$adminunit1) &
      (
        grepl("*處*", flag_person$adminunit1) |
          grepl("*中心*", flag_person$adminunit1) |
          grepl("*部*", flag_person$adminunit1) |
          grepl("*室*", flag_person$adminunit1) |
          grepl("*館*", flag_person$adminunit1)
      ),
    0,
    flag_person$err_adminunit1
  )
flag_person$err_adminunit1 <-
  if_else(grepl("^N$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^國中部教務處$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^高中部教務處$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^中學部教務處$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^國際部教務處$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^中學部學務處$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("校區校長室$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^圖資室$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^圖資處$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^國際教育處$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^研發處$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^資訊處$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^實輔處$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^家具設計發展處$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^國際處$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^研發室$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^圖資室兼技術交流處$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^教育推廣處$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^學輔處$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^綜合高中$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^技術交流處$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^補校$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^國際交流處$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^專案研究室$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("分校$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^創發處$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^補校教學組$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^補校教務組$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^補校訓育組$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^實驗研究組$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^外語部$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^外語處$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^研發部$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^國際事務處$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^招生辦公室$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("國際處$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^電腦室$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^招生部$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^宿舍處$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^人文室$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("服務處$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^員生社$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^教學資源中心處$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^校務發展室$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^校牧室$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^宗輔室$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^油印室$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^公共事務室$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("辦事處$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^生命教育室$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^國際室$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^教學研究室$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^國際暨建教處$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^外語中心處$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("語文中心$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^國小部總務處$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
flag_person$err_adminunit1 <-
  if_else(grepl("^外語教學處$", flag_person$adminunit1),
          0,
          flag_person$err_adminunit1)
#立志高中才有"高國中部"
flag_person$err_adminunit1 <-
  if_else(
    grepl("^高國中部$", flag_person$adminunit1) &
      flag_person$organization_id == "551301",
    0,
    flag_person$err_adminunit1
  )
#光禾華德福才可有以下服務單位：國中部日間部、高中部日間部
flag_person$err_adminunit1 <-
  if_else(
    grepl("^國中部日間部$", flag_person$adminunit1) &
      flag_person$organization_id == "121302",
    0,
    flag_person$err_adminunit1
  )
flag_person$err_adminunit1 <-
  if_else(
    grepl("^高中部日間部$", flag_person$adminunit1) &
      flag_person$organization_id == "121302",
    0,
    flag_person$err_adminunit1
  )
#磐石高中才有"高中部"
flag_person$err_adminunit1 <-
  if_else(
    grepl("^高中部$", flag_person$adminunit1) &
      flag_person$organization_id == "181307",
    0,
    flag_person$err_adminunit1
  )
#私立義大國際高中(121320)才有"發展事務處"
flag_person$err_adminunit1 <-
  if_else(
    grepl("^發展事務處$", flag_person$adminunit1) &
      flag_person$organization_id == "121320",
    0,
    flag_person$err_adminunit1
  )
#天主教道明中學(581302)才有"劍橋國際事務部"
flag_person$err_adminunit1 <-
  if_else(
    grepl("^劍橋國際事務部$", flag_person$adminunit1) &
      flag_person$organization_id == "581302",
    0,
    flag_person$err_adminunit1
  )
#台北市育達高中 雲林縣維多利亞實驗高中有"教導處"
flag_person$err_adminunit1 <-
  if_else(
    grepl("^教導處$", flag_person$adminunit1) &
      flag_person$organization_id == "311401",
    0,
    flag_person$err_adminunit1
  )
flag_person$err_adminunit1 <-
  if_else(
    grepl("^教導處$", flag_person$adminunit1) &
      flag_person$organization_id == "091320",
    0,
    flag_person$err_adminunit1
  )

#兼任行政職職稱（二）
flag_person$err_admintitle2 <- 1
flag_person$err_admintitle2 <-
  if_else(grepl("主任$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("秘書$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("組長$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("組員$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("^副校長$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("^均質化承辦人$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("^均職化承辦人$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("^優質化協辦人$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("校安$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("心理師$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("技術員$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("人員$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("職輔員$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("廚工$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("營養師$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("編制外行政人力$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("司機$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("秘書$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("祕書$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("舍監$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("辦事員$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("幹事$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("職務代理$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("職代$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("職務代理人$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("救生員$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("值機員$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("監督$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("三副$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("社工師$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("助理$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("專員$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("政風$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("1", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("^N$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("輔導員$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("^警衛$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("^實習餐廳經理$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("管理師$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("清潔員$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("佐理$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("技佐$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("^會計員$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("管理員$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("^書記$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("佐理員$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("館員$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("科主席$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("護理師$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("助理員$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("^助教$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("庶務$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("文書$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("^電競專案教練$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("^出納$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("^助教$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("^督導$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("訓育業務$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("指導員$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("駕駛", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("^網管$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("守衛$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("廚師$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("經理$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("技術士$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("校工$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("門衛$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("^廚房幫廚$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("^臨時約聘助理(計時)$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("保全", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("人事員$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("主廚$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("教練$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("館長$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("技士$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("技工$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("^監廚$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
#光禾華德福才可有以下職稱：總務、學務、教務、輔導、人事、國中部行政、高中部行政、會計
flag_person$err_admintitle2 <-
  if_else(
    grepl("^國中部行政$", flag_person$admintitle2) &
      flag_person$organization_id == "121302",
    0,
    flag_person$err_admintitle2
  )
flag_person$err_admintitle2 <-
  if_else(
    grepl("^高中部行政$", flag_person$admintitle2) &
      flag_person$organization_id == "121302",
    0,
    flag_person$err_admintitle2
  )
flag_person$err_admintitle2 <-
  if_else(
    grepl("^總務$", flag_person$admintitle2) &
      flag_person$organization_id == "121302",
    0,
    flag_person$err_admintitle2
  )
flag_person$err_admintitle2 <-
  if_else(
    grepl("^學務$", flag_person$admintitle2) &
      flag_person$organization_id == "121302",
    0,
    flag_person$err_admintitle2
  )
flag_person$err_admintitle2 <-
  if_else(
    grepl("^教務$", flag_person$admintitle2) &
      flag_person$organization_id == "121302",
    0,
    flag_person$err_admintitle2
  )
flag_person$err_admintitle2 <-
  if_else(
    grepl("^輔導$", flag_person$admintitle2) &
      flag_person$organization_id == "121302",
    0,
    flag_person$err_admintitle2
  )
flag_person$err_admintitle2 <-
  if_else(
    grepl("^人事$", flag_person$admintitle2) &
      flag_person$organization_id == "121302",
    0,
    flag_person$err_admintitle2
  )
flag_person$err_admintitle2 <-
  if_else(
    grepl("^會計$", flag_person$admintitle2) &
      flag_person$organization_id == "121302",
    0,
    flag_person$err_admintitle2
  )
flag_person$err_admintitle2 <-
  if_else(grepl("^校長$", flag_person$admintitle2),
          1,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("教官$", flag_person$admintitle2),
          1,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("教師$", flag_person$admintitle2),
          1,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("運動教練$", flag_person$admintitle2),
          1,
          flag_person$err_admintitle2)
flag_person$err_admintitle2 <-
  if_else(grepl("^董事長$", flag_person$admintitle2),
          1,
          flag_person$err_admintitle2)
#校長已在flag15檢查
flag_person$err_admintitle2 <-
  if_else(grepl("^校長$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
#導師已在flag15檢查
flag_person$err_admintitle2 <-
  if_else(grepl("導師$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
#教師已在flag15檢查
flag_person$err_admintitle2 <-
  if_else(grepl("教師$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)
#教官已在flag15檢查
flag_person$err_admintitle2 <-
  if_else(grepl("教官$", flag_person$admintitle2),
          0,
          flag_person$err_admintitle2)


#兼任行政職服務單位（二）
flag_person$err_adminunit2 <- 1
flag_person$err_adminunit2 <-
  if_else(grepl("^校長室$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^副校長室$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^秘書室$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^國小部", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^小學部$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(
    grepl("^國中部$", flag_person$adminunit2) |
      grepl("^國民中學部$", flag_person$adminunit2),
    0,
    flag_person$err_adminunit2
  )
flag_person$err_adminunit2 <-
  if_else(grepl("^中學部$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^教官室$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^教務處", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^進修部", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^進修學校$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^資訊室$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^圖書館", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^圖書室$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^實習處", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^實習室$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^輔導室", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("輔導處", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^學生事務處", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^學務處", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^總務處", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^雙語部", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^軍訓室$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^會計室$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^主計室$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^資訊室$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^國際部$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^董事會$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("中心$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("研究發展處$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("人事室$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^招生處$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^公關室$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^農場經營$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^藝文中心", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("科$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("1", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(
    grepl("組$", flag_person$adminunit2) &
      (
        grepl("*處*", flag_person$adminunit2) |
          grepl("*中心*", flag_person$adminunit2) |
          grepl("*部*", flag_person$adminunit2) |
          grepl("*室*", flag_person$adminunit2) |
          grepl("*館*", flag_person$adminunit2)
      ),
    0,
    flag_person$err_adminunit2
  )
flag_person$err_adminunit2 <-
  if_else(grepl("^N$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^國中部教務處$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^高中部教務處$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^中學部教務處$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^國際部教務處$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^中學部學務處$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("校區校長室$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^圖資室$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^圖資處$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^國際教育處$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^研發處$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^資訊處$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^實輔處$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^家具設計發展處$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^國際處$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^研發室$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^圖資室兼技術交流處$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^教育推廣處$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^學輔處$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^綜合高中$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^技術交流處$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^補校$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^國際交流處$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^專案研究室$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("分校$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^創發處$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^補校教學組$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^補校教務組$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^補校訓育組$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^實驗研究組$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^外語部$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^外語處$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^研發部$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^國際事務處$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^招生辦公室$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("國際處$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^電腦室$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^招生部$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^宿舍處$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^人文室$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("服務處$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^員生社$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^教學資源中心處$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^校務發展室$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^校牧室$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^宗輔室$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^油印室$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^公共事務室$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("辦事處$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^生命教育室$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^國際室$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^教學研究室$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^國際暨建教處$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^外語中心處$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("語文中心$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^國小部總務處$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
flag_person$err_adminunit2 <-
  if_else(grepl("^外語教學處$", flag_person$adminunit2),
          0,
          flag_person$err_adminunit2)
#立志高中才有"高國中部"
flag_person$err_adminunit2 <-
  if_else(
    grepl("^高國中部$", flag_person$adminunit2) &
      flag_person$organization_id == "551301",
    0,
    flag_person$err_adminunit2
  )
#光禾華德福才可有以下服務單位：國中部日間部、高中部日間部
flag_person$err_adminunit2 <-
  if_else(
    grepl("^國中部日間部$", flag_person$adminunit2) &
      flag_person$organization_id == "121302",
    0,
    flag_person$err_adminunit2
  )
flag_person$err_adminunit2 <-
  if_else(
    grepl("^高中部日間部$", flag_person$adminunit2) &
      flag_person$organization_id == "121302",
    0,
    flag_person$err_adminunit2
  )
#磐石高中才有"高中部"
flag_person$err_adminunit2 <-
  if_else(
    grepl("^高中部$", flag_person$adminunit2) &
      flag_person$organization_id == "181307",
    0,
    flag_person$err_adminunit2
  )
#台北市育達高中 雲林縣維多利亞實驗高中有"教導處"
flag_person$err_adminunit2 <-
  if_else(
    grepl("^教導處$", flag_person$adminunit2) &
      flag_person$organization_id == "311401",
    0,
    flag_person$err_adminunit2
  )
flag_person$err_adminunit2 <-
  if_else(
    grepl("^教導處$", flag_person$adminunit2) &
      flag_person$organization_id == "091320",
    0,
    flag_person$err_adminunit2
  )


#兼任行政職職稱（三）
flag_person$err_admintitle3 <- 1
flag_person$err_admintitle3 <-
  if_else(grepl("主任$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("秘書$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("組長$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("組員$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("^副校長$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("^均質化承辦人$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("^均職化承辦人$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("^優質化協辦人$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("校安$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("心理師$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("技術員$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("人員$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("職輔員$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("廚工$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("營養師$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("編制外行政人力$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("司機$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("秘書$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("祕書$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("舍監$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("辦事員$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("幹事$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("職務代理$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("職代$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("職務代理人$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("救生員$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("值機員$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("監督$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("三副$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("社工師$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("助理$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("專員$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("政風$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("1", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("^N$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("輔導員$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("^警衛$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("^實習餐廳經理$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("管理師$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("清潔員$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("佐理$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("技佐$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("^會計員$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("管理員$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("^書記$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("佐理員$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("館員$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("科主席$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("護理師$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("助理員$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("^助教$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("庶務$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("文書$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("^電競專案教練$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("^出納$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("^助教$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("^督導$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("訓育業務$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("指導員$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("駕駛", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("^網管$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("守衛$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("廚師$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("經理$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("技術士$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("校工$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("門衛$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("^廚房幫廚$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("^臨時約聘助理(計時)$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("保全", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("人事員$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("主廚$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("教練$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("館長$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("技士$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("技工$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("^監廚$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
#光禾華德福才可有以下職稱：總務、學務、教務、輔導、人事、國中部行政、高中部行政、會計
flag_person$err_admintitle3 <-
  if_else(
    grepl("^國中部行政$", flag_person$admintitle3) &
      flag_person$organization_id == "121302",
    0,
    flag_person$err_admintitle3
  )
flag_person$err_admintitle3 <-
  if_else(
    grepl("^高中部行政$", flag_person$admintitle3) &
      flag_person$organization_id == "121302",
    0,
    flag_person$err_admintitle3
  )
flag_person$err_admintitle3 <-
  if_else(
    grepl("^總務$", flag_person$admintitle3) &
      flag_person$organization_id == "121302",
    0,
    flag_person$err_admintitle3
  )
flag_person$err_admintitle3 <-
  if_else(
    grepl("^學務$", flag_person$admintitle3) &
      flag_person$organization_id == "121302",
    0,
    flag_person$err_admintitle3
  )
flag_person$err_admintitle3 <-
  if_else(
    grepl("^教務$", flag_person$admintitle3) &
      flag_person$organization_id == "121302",
    0,
    flag_person$err_admintitle3
  )
flag_person$err_admintitle3 <-
  if_else(
    grepl("^輔導$", flag_person$admintitle3) &
      flag_person$organization_id == "121302",
    0,
    flag_person$err_admintitle3
  )
flag_person$err_admintitle3 <-
  if_else(
    grepl("^人事$", flag_person$admintitle3) &
      flag_person$organization_id == "121302",
    0,
    flag_person$err_admintitle3
  )
flag_person$err_admintitle3 <-
  if_else(
    grepl("^會計$", flag_person$admintitle3) &
      flag_person$organization_id == "121302",
    0,
    flag_person$err_admintitle3
  )
flag_person$err_admintitle3 <-
  if_else(grepl("^校長$", flag_person$admintitle3),
          1,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("教官$", flag_person$admintitle3),
          1,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("教師$", flag_person$admintitle3),
          1,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("運動教練$", flag_person$admintitle3),
          1,
          flag_person$err_admintitle3)
flag_person$err_admintitle3 <-
  if_else(grepl("^董事長$", flag_person$admintitle3),
          1,
          flag_person$err_admintitle3)
#校長已在flag15檢查
flag_person$err_admintitle3 <-
  if_else(grepl("^校長$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
#導師已在flag15檢查
flag_person$err_admintitle3 <-
  if_else(grepl("導師$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
#教師已在flag15檢查
flag_person$err_admintitle3 <-
  if_else(grepl("教師$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)
#教官已在flag15檢查
flag_person$err_admintitle3 <-
  if_else(grepl("教官$", flag_person$admintitle3),
          0,
          flag_person$err_admintitle3)


#兼任行政職服務單位（三）
flag_person$err_adminunit3 <- 1
flag_person$err_adminunit3 <-
  if_else(grepl("^校長室$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^副校長室$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^秘書室$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^國小部", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^小學部$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(
    grepl("^國中部$", flag_person$adminunit3) |
      grepl("^國民中學部$", flag_person$adminunit3),
    0,
    flag_person$err_adminunit3
  )
flag_person$err_adminunit3 <-
  if_else(grepl("^中學部$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^教官室$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^教務處", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^進修部", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^進修學校$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^資訊室$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^圖書館", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^圖書室$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^實習處", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^實習室$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^輔導室", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("輔導處", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^學生事務處", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^學務處", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^總務處", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^雙語部", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^軍訓室$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^會計室$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^主計室$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^資訊室$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^國際部$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^董事會$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("中心$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("研究發展處$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("人事室$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^招生處$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^公關室$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^農場經營$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^藝文中心", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("科$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("1", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(
    grepl("組$", flag_person$adminunit3) &
      (
        grepl("*處*", flag_person$adminunit3) |
          grepl("*中心*", flag_person$adminunit3) |
          grepl("*部*", flag_person$adminunit3) |
          grepl("*室*", flag_person$adminunit3) |
          grepl("*館*", flag_person$adminunit3)
      ),
    0,
    flag_person$err_adminunit3
  )
flag_person$err_adminunit3 <-
  if_else(grepl("^N$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^國中部教務處$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^高中部教務處$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^中學部教務處$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^國際部教務處$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^中學部學務處$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("校區校長室$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^圖資室$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^圖資處$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^國際教育處$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^研發處$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^資訊處$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^實輔處$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^家具設計發展處$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^國際處$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^研發室$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^圖資室兼技術交流處$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^教育推廣處$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^學輔處$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^綜合高中$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^技術交流處$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^補校$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^國際交流處$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^專案研究室$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("分校$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^創發處$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^補校教學組$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^補校教務組$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^補校訓育組$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^實驗研究組$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^外語部$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^外語處$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^研發部$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^國際事務處$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^招生辦公室$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("國際處$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^電腦室$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^招生部$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^宿舍處$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^人文室$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("服務處$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^員生社$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^教學資源中心處$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^校務發展室$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^校牧室$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^宗輔室$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^油印室$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^公共事務室$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("辦事處$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^生命教育室$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^國際室$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^教學研究室$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^國際暨建教處$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^外語中心處$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("語文中心$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^國小部總務處$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
flag_person$err_adminunit3 <-
  if_else(grepl("^外語教學處$", flag_person$adminunit3),
          0,
          flag_person$err_adminunit3)
#立志高中才有"高國中部"
flag_person$err_adminunit3 <-
  if_else(
    grepl("^高國中部$", flag_person$adminunit3) &
      flag_person$organization_id == "551301",
    0,
    flag_person$err_adminunit3
  )
#光禾華德福才可有以下服務單位：國中部日間部、高中部日間部
flag_person$err_adminunit3 <-
  if_else(
    grepl("^國中部日間部$", flag_person$adminunit3) &
      flag_person$organization_id == "121302",
    0,
    flag_person$err_adminunit3
  )
flag_person$err_adminunit3 <-
  if_else(
    grepl("^高中部日間部$", flag_person$adminunit3) &
      flag_person$organization_id == "121302",
    0,
    flag_person$err_adminunit3
  )
#磐石高中才有"高中部"
flag_person$err_adminunit3 <-
  if_else(
    grepl("^高中部$", flag_person$adminunit3) &
      flag_person$organization_id == "181307",
    0,
    flag_person$err_adminunit3
  )
#台北市育達高中 雲林縣維多利亞實驗高中有"教導處"
flag_person$err_adminunit3 <-
  if_else(
    grepl("^教導處$", flag_person$adminunit3) &
      flag_person$organization_id == "311401",
    0,
    flag_person$err_adminunit3
  )
flag_person$err_adminunit3 <-
  if_else(
    grepl("^教導處$", flag_person$adminunit3) &
      flag_person$organization_id == "091320",
    0,
    flag_person$err_adminunit3
  )

#以下為參考文字
#教員資料表之各兼任行政職資料不完整或不正確：請依欄位說明確認並正確填列行政單位名稱，如為二級單位主管，請敘明一級與二級單位名稱。如教務處教學組，學生事務處生活輔導組。
#教員資料表之兼任行政職資料不完整或不正確：請依欄位說明確認並正確填列行政單位名稱，如為二級單位主管，請敘明一級與二級單位名稱。如教務處教學組，學生事務處生活輔導組。
#職員(工)資料表之各服務單位資料不完整或不正確：請依欄位說明確認並正確填列服務單位名稱，如為二級單位主管，請敘明一級與二級單位名稱。如總務處出納組，學生事務處生活輔導組。
#教員資料表及職員(工)資料表之(兼任)職稱或服務單位資料不完整或不正確：請依欄位說明確認並正確填列行政單位名稱，如為二級單位主管，請敘明一級與二級單位名稱。如教務處教學組，總務處出納組。
#上述職員(工)資料表中人員，若未再兼任或代理行政職務者，僅須填寫「職務名稱」與「服務單位」，且二級單位名稱請於「服務單位」所填一級單位名稱後面加註。
#請確認*員之職稱或服務身分別，若確為教師，請將資料填至教員資料表。
#（職員(工)資料表之服務單位資料不完整或不正確：請依欄位說明確認並正確填列行政單位名稱，如為二級單位，請敘明一級與二級單位名稱，如學務處體育組，總務處出納組。另請再確認資源班是否為行政單位名稱。）
#（請確認並正確填列『兼任行政職服務單位』名稱，此欄位不需填入職務名稱。）
#（請確認並正確填列『兼任行政職服務單位』名稱，此欄位不需填入職務名稱(該欄請刪除『長』字)。）
#（請確認並正確填列『兼任行政職服務單位』名稱，此欄位不需填入職務名稱(該欄請刪除『長』字；如為二級單位主管，請同時敘明一級與二級單位名稱，如教務處教學組、學生事務處生活輔導組。）
#（若確認*員因故代理校長，請於所代理之行政職職稱、行政職服務單位註記「1」，填報方式如下：
#兼任行政職職稱(一)：校長1
#兼任行政職服務單位(一)：校長室1）
#（請再協助確認*員職務正確完整名稱，職稱與服務單位請依不同欄位分別填寫）
#（請再協助確認並修正*員兼任職務及其服務單位之正確完整名稱）
#（請依欄位說明，填列*員於校內任職之正確職務名稱及服務單位名稱於『職務名稱』及『服務單位』欄位（如屬二級單位者，請敘明一與二級單位名稱）。
#若蔡員於校內兼任多項行政職務，請分別填列於『兼任行政職職稱』、『兼任行政職服務單位』（一）～（三）欄位。
#蔡員於本學期若代理行政職務，所代理之行政職稱及其服務單位亦請填寫於 本 兼任行政職職稱及兼任行政職服務單位 欄，並於代理職稱與單位後加註「1」。）
#（請再協助確認上述人員服務單位正確完整名稱）
#（請再協助確認上述人員職務正確完整名稱，或修正錯字）
#（『兼任行政職職稱』應填入職稱，『兼任行政職服務單位』應填入單位名稱，請修正資料；另，『校長』為『服務身分別』，惟如由教師代理，請依欄位說明填寫註明）
#（請再協助確認上述人員職務正確完整名稱，並請務必按欄位說明正確填報資料；如為隸屬於二級單位，請同時敘明一級與二級單位名稱，如總務處出納組，學生事務處生活輔導組。若編制並未設組，請來電告知。）
#（教員資料表及職員(工)資料表之(兼任)職稱或服務單位資料不完整或不正確：請依欄位說明確認並正確填列各級行政單位名稱；如為二級單位主管，請同時敘明一級與二級單位名稱，如教務處教學組，總務處出納組。若編制並未設組，請來電告知。另，上述人員如於本職之外並未兼任其他職務，則兼任行政職職務及服務單位名稱，請填『N』）
#（上述職員(工)資料表中人員，若未再兼任或代理行政職務者，僅須填寫「職務名稱」與「服務單位」，且如為二級單位主管，請於「服務單位」同時敘明一級與二級單位名稱，如：總務處出納組，實習處實習就業輔導組等。）

#人事、會計僅設組長
#（職員(工)資料表之各服務單位資料不完整或不正確：請依欄位說明確認並正確填列服務單位名稱，如為二級單位主管，請敘明一級與二級單位名稱。如總務處出納組，學生事務處生活輔導組。若編制並未設組，請來電告知）

err_title0 <- data.frame(
  admintitle0 = flag_person$admintitle0,
  adminunit0 = flag_person$adminunit0,
  organization_id = flag_person$organization_id
)
flag_person$err_title0 <-
  if_else(!err_title0$admintitle0 %in% "N" &
            err_title0$admintitle0 %in% "組長" &
            ((
              !grepl("組$", err_title0$adminunit0) &
                !grepl("^N$", err_title0$adminunit0)
            ) |
              (
                grepl("組$", err_title0$adminunit0) &
                  nchar(err_title0$adminunit0) <= 5
              )),
          1,
          0)

err_title1 <- data.frame(
  admintitle1 = flag_person$admintitle1,
  adminunit1 = flag_person$adminunit1,
  organization_id = flag_person$organization_id
)
flag_person$err_title1 <-
  if_else(!err_title1$admintitle1 %in% "N" &
            err_title1$admintitle1 %in% "組長" &
            ((
              !grepl("組$", err_title1$adminunit1) &
                !grepl("^N$", err_title1$adminunit1)
            ) |
              (
                grepl("組$", err_title1$adminunit1) &
                  nchar(err_title1$adminunit1) <= 5
              )),
          1,
          0)

err_title2 <- data.frame(
  admintitle2 = flag_person$admintitle2,
  adminunit2 = flag_person$adminunit2,
  organization_id = flag_person$organization_id
)
flag_person$err_title2 <-
  if_else(!err_title2$admintitle2 %in% "N" &
            err_title2$admintitle2 %in% "組長" &
            ((
              !grepl("組$", err_title2$adminunit2) &
                !grepl("^N$", err_title2$adminunit2)
            ) |
              (
                grepl("組$", err_title2$adminunit2) &
                  nchar(err_title2$adminunit2) <= 5
              )),
          1,
          0)

err_title3 <- data.frame(
  admintitle3 = flag_person$admintitle3,
  adminunit3 = flag_person$adminunit3,
  organization_id = flag_person$organization_id
)
flag_person$err_title3 <-
  if_else(!err_title3$admintitle3 %in% "N" &
            err_title3$admintitle3 %in% "組長" &
            ((
              !grepl("組$", err_title3$adminunit3) &
                !grepl("^N$", err_title3$adminunit3)
            ) |
              (
                grepl("組$", err_title3$adminunit3) &
                  nchar(err_title3$adminunit3) <= 5
              )),
          1,
          0)

flag_person$err_flag_62 <-
  flag_person$err_admintitle0 + flag_person$err_adminunit0 + flag_person$err_admintitle1 + flag_person$err_adminunit1 + flag_person$err_admintitle2 + flag_person$err_adminunit2 + flag_person$err_admintitle3 + flag_person$err_adminunit3 + flag_person$err_title0 + flag_person$err_title1 + flag_person$err_title2 + flag_person$err_title3

flag_person$err_flag <- if_else(flag_person$err_flag_62 != 0, 1, 0)


#備註文字用
#err_flag_1: 職稱或服務單位不合理
flag_person$err_flag_1 <-
  if_else(
    flag_person$err_admintitle0 != 0 |
      flag_person$err_adminunit0 != 0 |
      flag_person$err_admintitle1 != 0 |
      flag_person$err_adminunit1 != 0 |
      flag_person$err_admintitle2 != 0 |
      flag_person$err_adminunit2 != 0 |
      flag_person$err_admintitle3 != 0 |
      flag_person$err_adminunit3 != 0,
    1,
    0
  )
#err_flag_2: 職稱為組長，且未填二級單位
flag_person$err_flag_2 <-
  if_else(
    flag_person$err_title0 != 0 |
      flag_person$err_title1 != 0 |
      
      flag_person$err_title2 != 0 |
      flag_person$err_title3 != 0,
    1,
    0
  )
#err_flag_2_1: 職員工資料表出現err_flag_2
flag_person$err_flag2_1 <- if_else((
  flag_person$err_title0 != 0 |
    flag_person$err_title1 != 0 |
    flag_person$err_title2 != 0 |
    flag_person$err_title3 != 0
) &
  flag_person$source == "職員(工)資料表",
1,
0
)
#err_flag_2_2: 教員資料表出現err_flag_2
flag_person$err_flag2_2 <- if_else((
  flag_person$err_title1 != 0 |
    flag_person$err_title2 != 0 |
    flag_person$err_title3 != 0
) &
  flag_person$source == "教員資料表",
1,
0
)

#aggregate該校err_flag2_1及err_flag2_2 -> 該校err_flag_2(職稱為組長，且未填二級單位)出現在教員or職員(工)資料表
flag_person_err_flag_detect <-
  aggregate(cbind(err_flag2_1, err_flag2_2) ~ organization_id,
            flag_person,
            sum) %>%
  rename(err_flag2_1_detect = err_flag2_1,
         err_flag2_2_detect = err_flag2_2)

flag_person <- flag_person %>%
  left_join(flag_person_err_flag_detect, by = "organization_id")

#加註
flag_person$name <- paste(flag_person$name, "（", sep = "")
flag_person$name <-
  if_else(
    flag_person$err_admintitle0 != 0,
    paste(flag_person$name,
          "職務名稱：",
          flag_person$admintitle0,
          "；",
          sep = ""),
    flag_person$name
  )
flag_person$name <-
  if_else(
    flag_person$err_adminunit0 != 0,
    paste(flag_person$name,
          "服務單位：",
          flag_person$adminunit0,
          "；",
          sep = ""),
    flag_person$name
  )
flag_person$name <-
  if_else(
    flag_person$err_admintitle1 != 0,
    paste(
      flag_person$name,
      "兼任行政職職稱(一)：",
      flag_person$admintitle1,
      "；",
      sep = ""
    ),
    flag_person$name
  )
flag_person$name <-
  if_else(
    flag_person$err_adminunit1 != 0,
    paste(
      flag_person$name,
      "兼任行政職服務單位(一)：",
      flag_person$adminunit1,
      "；",
      sep = ""
    ),
    flag_person$name
  )
flag_person$name <-
  if_else(
    flag_person$err_admintitle2 != 0,
    paste(
      flag_person$name,
      "兼任行政職職稱(二)：",
      flag_person$admintitle2,
      "；",
      sep = ""
    ),
    flag_person$name
  )
flag_person$name <-
  if_else(
    flag_person$err_adminunit2 != 0,
    paste(
      flag_person$name,
      "兼任行政職服務單位(二)：",
      flag_person$adminunit2,
      "；",
      sep = ""
    ),
    flag_person$name
  )
flag_person$name <-
  if_else(
    flag_person$err_admintitle3 != 0,
    paste(
      flag_person$name,
      "兼任行政職職稱(三)：",
      flag_person$admintitle3,
      "；",
      sep = ""
    ),
    flag_person$name
  )
flag_person$name <-
  if_else(
    flag_person$err_adminunit3 != 0,
    paste(
      flag_person$name,
      "兼任行政職服務單位(三)：",
      flag_person$adminunit3,
      "；",
      sep = ""
    ),
    flag_person$name
  )
flag_person$name <-
  if_else(
    flag_person$err_title0 != 0,
    paste(
      flag_person$name,
      "服務單位：",
      flag_person$adminunit0,
      " ",
      "職務名稱：",
      flag_person$admintitle0,
      "；",
      sep = ""
    ),
    flag_person$name
  )
flag_person$name <-
  if_else(
    flag_person$err_title1 != 0,
    paste(
      flag_person$name,
      "兼任行政職服務單位(一)：",
      flag_person$adminunit1,
      " ",
      "兼任行政職職稱(一)：",
      flag_person$admintitle1,
      "；",
      sep = ""
    ),
    flag_person$name
  )
flag_person$name <-
  if_else(
    flag_person$err_title2 != 0,
    paste(
      flag_person$name,
      "兼任行政職服務單位(二)：",
      flag_person$adminunit2,
      " ",
      "兼任行政職職稱(二)：",
      flag_person$admintitle2,
      "；",
      sep = ""
    ),
    flag_person$name
  )
flag_person$name <-
  if_else(
    flag_person$err_title3 != 0,
    paste(
      flag_person$name,
      "兼任行政職服務單位(三)：",
      flag_person$adminunit3,
      " ",
      "兼任行政職職稱(三)：",
      flag_person$admintitle3,
      "；",
      sep = ""
    ),
    flag_person$name
  )
flag_person$name <- paste(flag_person$name, "）", sep = "")
flag_person$name <- gsub("；）", replacement = "）", flag_person$name)
flag_person$name <- gsub("（）", replacement = "", flag_person$name)

flag_person$err_flag_txt <- ""
flag_person$err_flag_txt <-
  case_when(flag_person$err_flag == 1 ~ flag_person$name,
            TRUE ~ flag_person$err_flag_txt)

if (dim(flag_person %>% subset(err_flag == 1))[1] != 0) {
  #根據organization_id + source，展開成寬資料(wide)
  flag_person_wide_flag62_1 <- tryCatch({
    flag_person %>%
      subset(
        select = c(
          organization_id,
          idnumber,
          err_flag_txt,
          edu_name2,
          source,
          err_flag,
          err_flag_1,
          err_flag2_1,
          err_flag2_2,
          err_flag2_1_detect,
          err_flag2_2_detect
        )
      ) %>%
      subset(err_flag_1 == 1) %>%
      dcast(organization_id + source ~ err_flag_txt, value.var = "err_flag_txt")
  }, error = function(e) {
    NULL
  })
  
  if (!is.null(flag_person_wide_flag62_1)) {
    #合併所有name
    temp <-
      colnames(flag_person_wide_flag62_1)[3:length(colnames(flag_person_wide_flag62_1))]
    flag_person_wide_flag62_1$flag62_1_r <- NA
    for (i in temp) {
      flag_person_wide_flag62_1$flag62_1_r <-
        paste(flag_person_wide_flag62_1$flag62_1_r,
              flag_person_wide_flag62_1[[i]],
              sep = " ")
    }
    flag_person_wide_flag62_1$flag62_1_r <-
      gsub("NA ",
           replacement = "",
           flag_person_wide_flag62_1$flag62_1_r)
    flag_person_wide_flag62_1$flag62_1_r <-
      gsub(" NA",
           replacement = "",
           flag_person_wide_flag62_1$flag62_1_r)
    flag_person_wide_flag62_1$flag62_1_r <-
      paste0(flag_person_wide_flag62_1$flag62_1_r,
             "\n（請再協助確認上述人員職務正確完整名稱）") #若#err_flag_1: 職稱或服務單位不合理，則加註
  } else{
    print("flag_person_wide_flag62_1 not exists.")
    rm(flag_person_wide_flag62_1)
  }
  
  #根據organization_id + source，展開成寬資料(wide)
  flag_person_wide_flag62_2_1 <- tryCatch({
    flag_person %>%
      subset(
        select = c(
          organization_id,
          idnumber,
          err_flag_txt,
          edu_name2,
          source,
          err_flag,
          err_flag_1,
          err_flag2_1,
          err_flag2_2,
          err_flag2_1_detect,
          err_flag2_2_detect
        )
      ) %>%
      subset(err_flag2_1 == 1 & err_flag2_2_detect == 0) %>%
      dcast(organization_id + source ~ err_flag_txt, value.var = "err_flag_txt")
  }, error = function(e) {
    NULL
  })
  
  if (!is.null(flag_person_wide_flag62_2_1)) {
    #合併所有name
    temp <-
      colnames(flag_person_wide_flag62_2_1)[3:length(colnames(flag_person_wide_flag62_2_1))]
    flag_person_wide_flag62_2_1$flag62_2_1_r <- NA
    for (i in temp) {
      flag_person_wide_flag62_2_1$flag62_2_1_r <-
        paste(
          flag_person_wide_flag62_2_1$flag62_2_1_r,
          flag_person_wide_flag62_2_1[[i]],
          sep = " "
        )
    }
    flag_person_wide_flag62_2_1$flag62_2_1_r <-
      gsub("NA ",
           replacement = "",
           flag_person_wide_flag62_2_1$flag62_2_1_r)
    flag_person_wide_flag62_2_1$flag62_2_1_r <-
      gsub(" NA",
           replacement = "",
           flag_person_wide_flag62_2_1$flag62_2_1_r)
    flag_person_wide_flag62_2_1$flag62_2_1_r <-
      paste0(
        flag_person_wide_flag62_2_1$flag62_2_1_r,
        "\n（職員(工)資料表之各服務單位資料不完整或不正確：請依欄位說明確認並正確填列各級服務單位名稱；如為二級單位主管，請同時敘明一級與二級單位名稱，如總務處出納組，學生事務處生活輔導組。若編制並未設組，請來電告知。）"
      ) #若#err_flag_2_1: 職員工資料表出現err_flag_2，則加註
  } else{
    print("flag_person_wide_flag62_2_1 not exists.")
    rm(flag_person_wide_flag62_2_1)
  }
  
  #根據organization_id + source，展開成寬資料(wide)
  flag_person_wide_flag62_2_2 <- tryCatch({
    flag_person %>%
      subset(
        select = c(
          organization_id,
          idnumber,
          err_flag_txt,
          edu_name2,
          source,
          err_flag,
          err_flag_1,
          err_flag2_1,
          err_flag2_2,
          err_flag2_1_detect,
          err_flag2_2_detect
        )
      ) %>%
      subset(err_flag2_2 == 1 & err_flag2_1_detect == 0) %>%
      dcast(organization_id + source ~ err_flag_txt, value.var = "err_flag_txt")
  }, error = function(e) {
    NULL
  })
  
  if (!is.null(flag_person_wide_flag62_2_2)) {
    #合併所有name
    temp <-
      colnames(flag_person_wide_flag62_2_2)[3:length(colnames(flag_person_wide_flag62_2_2))]
    flag_person_wide_flag62_2_2$flag62_2_2_r <- NA
    for (i in temp) {
      flag_person_wide_flag62_2_2$flag62_2_2_r <-
        paste(
          flag_person_wide_flag62_2_2$flag62_2_2_r,
          flag_person_wide_flag62_2_2[[i]],
          sep = " "
        )
    }
    flag_person_wide_flag62_2_2$flag62_2_2_r <-
      gsub("NA ",
           replacement = "",
           flag_person_wide_flag62_2_2$flag62_2_2_r)
    flag_person_wide_flag62_2_2$flag62_2_2_r <-
      gsub(" NA",
           replacement = "",
           flag_person_wide_flag62_2_2$flag62_2_2_r)
    flag_person_wide_flag62_2_2$flag62_2_2_r <-
      paste0(
        flag_person_wide_flag62_2_2$flag62_2_2_r,
        "\n（教員資料表之各兼任行政職資料不完整或不正確：請依欄位說明確認並正確填列各級行政單位名稱；如為二級單位主管，請同時敘明一級與二級單位名稱，如教務處教學組，學生事務處生活輔導組。若編制並未設組，請來電告知。）"
      ) #若err_flag_2_2: 教員資料表出現err_flag_2，則加註
  } else{
    print("flag_person_wide_flag62_2_2 not exists.")
    rm(flag_person_wide_flag62_2_2)
  }
  
  #根據organization_id + source，展開成寬資料(wide)
  flag_person_wide_flag62_2_3 <- tryCatch({
    flag_person %>%
      subset(
        select = c(
          organization_id,
          idnumber,
          err_flag_txt,
          edu_name2,
          source,
          err_flag,
          err_flag_1,
          err_flag2_1,
          err_flag2_2,
          err_flag2_1_detect,
          err_flag2_2_detect
        )
      ) %>%
      subset((err_flag2_1 == 1 &
                err_flag2_2_detect != 0) |
               (err_flag2_1_detect != 0 & err_flag2_2 == 1)
      ) %>%
      dcast(organization_id + source ~ err_flag_txt, value.var = "err_flag_txt")
  }, error = function(e) {
    NULL
  })
  
  if (!is.null(flag_person_wide_flag62_2_3)) {
    #合併所有name
    temp <-
      colnames(flag_person_wide_flag62_2_3)[3:length(colnames(flag_person_wide_flag62_2_3))]
    flag_person_wide_flag62_2_3$flag62_2_3_r <- NA
    for (i in temp) {
      flag_person_wide_flag62_2_3$flag62_2_3_r <-
        paste(
          flag_person_wide_flag62_2_3$flag62_2_3_r,
          flag_person_wide_flag62_2_3[[i]],
          sep = " "
        )
    }
    flag_person_wide_flag62_2_3$flag62_2_3_r <-
      gsub("NA ",
           replacement = "",
           flag_person_wide_flag62_2_3$flag62_2_3_r)
    flag_person_wide_flag62_2_3$flag62_2_3_r <-
      gsub(" NA",
           replacement = "",
           flag_person_wide_flag62_2_3$flag62_2_3_r)
    flag_person_wide_flag62_2_3$flag62_2_3_r <-
      if_else(
        flag_person_wide_flag62_2_3$source == "職員(工)資料表",
        paste0(
          flag_person_wide_flag62_2_3$flag62_2_3_r,
          "\n（教員資料表及職員(工)資料表之(兼任)職稱或服務單位資料不完整或不正確：請依欄位說明確認並正確填列各級行政單位名稱；如為二級單位主管，請同時敘明一級與二級單位名稱，如教務處教學組，總務處出納組。若編制並未設組，請來電告知。）"
        ),
        #若err_flag_2_3: 教員及職員工資料表同時出現err_flag_2，則加註
        flag_person_wide_flag62_2_3$flag62_2_3_r
      )
  } else{
    print("flag_person_wide_flag62_2_3 not exists.")
    rm(flag_person_wide_flag62_2_3)
  }
  
  if ('flag_person_wide_flag62_1' %in% ls()) {
    #如果flag_person_wide_flag62_1有建立成功
    print("flag_person_wide_flag62_1 exists.")
  } else{
    #如果未建立成功，建立空白物件
    flag_person_wide_flag62_1 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    data_source <- c("教員資料表", "職員(工)資料表")
    
    flag_person_wide_flag62_1 <-
      expand.grid(
        organization_id = flag_person_wide_flag62_1$organization_id %>% as.character(),
        source = data_source
      )
    flag_person_wide_flag62_1$flag62_1_r <-  ""
  }
  
  if ('flag_person_wide_flag62_2_1' %in% ls()) {
    #如果flag_person_wide_flag62_2_1有建立成功
    print("flag_person_wide_flag62_2_1 exists.")
  } else{
    #如果未建立成功，建立空白物件
    flag_person_wide_flag62_2_1 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    data_source <- c("教員資料表", "職員(工)資料表")
    
    flag_person_wide_flag62_2_1 <-
      expand.grid(
        organization_id = flag_person_wide_flag62_2_1$organization_id %>% as.character(),
        source = data_source
      )
    flag_person_wide_flag62_2_1$flag62_2_1_r <-  ""
  }
  
  if ('flag_person_wide_flag62_2_2' %in% ls()) {
    #如果flag_person_wide_flag62_2_2有建立成功
    print("flag_person_wide_flag62_2_2 exists.")
  } else{
    #如果未建立成功，建立空白物件
    flag_person_wide_flag62_2_2 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    data_source <- c("教員資料表", "職員(工)資料表")
    
    flag_person_wide_flag62_2_2 <-
      expand.grid(
        organization_id = flag_person_wide_flag62_2_2$organization_id %>% as.character(),
        source = data_source
      )
    flag_person_wide_flag62_2_2$flag62_2_2_r <-  ""
  }
  
  if ('flag_person_wide_flag62_2_3' %in% ls()) {
    #如果flag_person_wide_flag62_2_3有建立成功
    print("flag_person_wide_flag62_2_3 exists.")
  } else{
    #如果未建立成功，建立空白物件
    flag_person_wide_flag62_2_3 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    data_source <- c("教員資料表", "職員(工)資料表")
    
    flag_person_wide_flag62_2_3 <-
      expand.grid(
        organization_id = flag_person_wide_flag62_2_3$organization_id %>% as.character(),
        source = data_source
      )
    flag_person_wide_flag62_2_3$flag62_2_3_r <-  ""
  }
  
  flag_person_wide_flag62 <- flag_person_wide_flag62_1 %>%
    full_join(flag_person_wide_flag62_2_1,
              by = c("organization_id", "source")) %>%
    full_join(flag_person_wide_flag62_2_2,
              by = c("organization_id", "source")) %>%
    full_join(flag_person_wide_flag62_2_3,
              by = c("organization_id", "source")) %>%
    select(
      c(
        "organization_id",
        "source",
        "flag62_1_r",
        "flag62_2_1_r",
        "flag62_2_2_r",
        "flag62_2_3_r"
      )
    ) %>%
    mutate(flag62_r = paste(flag62_1_r,
                            flag62_2_1_r,
                            flag62_2_2_r,
                            flag62_2_3_r,
                            sep = "\n"))
  flag_person_wide_flag62$flag62_r <-
    gsub("NA\n+",
         replacement = "",
         flag_person_wide_flag62$flag62_r)
  flag_person_wide_flag62$flag62_r <-
    gsub("\nNA+",
         replacement = "",
         flag_person_wide_flag62$flag62_r)
  flag_person_wide_flag62$flag62_r <-
    gsub("NA",
         replacement = "",
         flag_person_wide_flag62$flag62_r)
  
  
  
  #產生檢誤報告文字
  flag62_temp <- flag_person_wide_flag62 %>%
    subset(flag62_r != "" & flag62_r != "\n") %>%
    group_by(organization_id) %>%
    mutate(flag62_txt = paste(source, "之行政職資料不完整或不正確：", flag62_r, sep = ""),
           "") %>%
    subset(select = c(organization_id, flag62_txt)) %>%
    distinct(organization_id, flag62_txt)
  
  #根據organization_id，展開成寬資料(wide)
  flag62 <- flag62_temp %>%
    dcast(organization_id ~ flag62_txt, value.var = "flag62_txt")
  
  #合併教員資料表及職員(工)資料表報告文字
  temp <- colnames(flag62)[2:length(colnames(flag62))]
  flag62$flag62 <- NA
  for (i in temp) {
    flag62$flag62 <- paste(flag62$flag62, flag62[[i]], sep = "； ")
  }
  flag62$flag62 <- gsub("NA； ", replacement = "", flag62$flag62)
  flag62$flag62 <- gsub("； NA", replacement = "", flag62$flag62)
  
  #產生檢誤報告文字
  flag62 <- flag62 %>%
    subset(select = c(organization_id, flag62)) %>%
    distinct(organization_id, flag62) %>%
    mutate(flag62 = paste(flag62, "", sep = ""))
  
  #全形"Ｎ"的提示
  flag62 <- flag62 %>%
    full_join(flag62_detectN, by = "organization_id") %>%
    subset(!is.na(flag62))
  flag62$detectN <-
    if_else(is.na(flag62$detectN), "", flag62$detectN)
  flag62$flag62 <-
    if_else(
      flag62$detectN == "1",
      paste0(
        "請將教員資料表及職員(工)資料表之「兼任行政職職稱（一）～（三）」、「兼任行政職服務單位（一）～（三）」、「職務名稱」或「服務單位」之「全形Ｎ」修正為「半形N」。",
        flag62$flag62
      ),
      flag62$flag62
    )
  flag62 <- flag62 %>%
    select(-c("detectN"))
  
} else{
  #偵測flag62是否存在。若不存在，則產生NA行
  if ('flag62' %in% ls()) {
    print("flag62")
  } else{
    flag62 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    flag62$flag62 <- ""
    
    #全形"Ｎ"的提示
    flag62 <- flag62 %>%
      full_join(flag62_detectN, by = "organization_id")
    flag62$detectN <-
      if_else(is.na(flag62$detectN), "", flag62$detectN)
    flag62$flag62 <-
      if_else(
        flag62$detectN == "1",
        "請將教員資料表及職員(工)資料表之「兼任行政職職稱（一）～（三）」、「兼任行政職服務單位（一）～（三）」、「職務名稱」或「服務單位」之「全形Ｎ」修正為「半形N」。",
        flag62$flag62
      )
    flag62 <- flag62 %>%
      select(-c("detectN"))
    
  }
}
