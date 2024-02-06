# spe6: 各教育階段學歷資料內容是否完整正確。-------------------------------------------------------------------
# 例如：
# 1.	各學歷階段「國別」非填入「本國」或者外交部網站之世界各國名稱一覽表的國家名稱（或者至少須足以辨識國家）。
# 2.	各學歷階段「學校」填入非學校名稱。
# 3.	各學歷階段「系所」填入非系所名稱。
# 4.	需有專科學歷，才能報考碩士研究所（若為逕讀碩士，副學士不得為N）。
# 5.	需有學士學歷，才能報考博士研究所（若為逕讀博士，學士不得為N或填逕讀博士）。
# 6.	學士學位欄位若填列「逕讀碩士」，應填列碩士學位（不應為N）。
# 7.	碩士學位欄位若填列「逕讀博士」，應填列博士學位（不應為N）。

flag_person <- drev_person_1

#博士學位畢業學校國別（一）
flag_person$err_ddegreen1 <- 0
flag_person$err_ddegreen1 <-
  if_else(grepl("博士", flag_person$ddegreen1),
          1,
          flag_person$err_ddegreen1)
flag_person$err_ddegreen1 <-
  if_else(grepl("碩士", flag_person$ddegreen1),
          1,
          flag_person$err_ddegreen1)
flag_person$err_ddegreen1 <-
  if_else(grepl("學士", flag_person$ddegreen1),
          1,
          flag_person$err_ddegreen1)
flag_person$err_ddegreen1 <-
  if_else(grepl("副學士", flag_person$ddegreen1),
          1,
          flag_person$err_ddegreen1)
flag_person$err_ddegreen1 <-
  if_else(grepl("大學", flag_person$ddegreen1),
          1,
          flag_person$err_ddegreen1)
flag_person$err_ddegreen1 <-
  if_else(grepl("分校", flag_person$ddegreen1),
          1,
          flag_person$err_ddegreen1)
flag_person$err_ddegreen1 <-
  if_else(grepl("學院", flag_person$ddegreen1),
          1,
          flag_person$err_ddegreen1)
flag_person$err_ddegreen1 <-
  if_else(grepl("科大", flag_person$ddegreen1),
          1,
          flag_person$err_ddegreen1)
flag_person$err_ddegreen1 <-
  if_else(grepl("學校", flag_person$ddegreen1),
          1,
          flag_person$err_ddegreen1)
flag_person$err_ddegreen1 <-
  if_else(grepl("官校", flag_person$ddegreen1),
          1,
          flag_person$err_ddegreen1)
flag_person$err_ddegreen1 <-
  if_else(grepl("預校", flag_person$ddegreen1),
          1,
          flag_person$err_ddegreen1)
flag_person$err_ddegreen1 <-
  if_else(grepl("書院", flag_person$ddegreen1),
          1,
          flag_person$err_ddegreen1)
flag_person$err_ddegreen1 <-
  if_else(grepl("專科", flag_person$ddegreen1),
          1,
          flag_person$err_ddegreen1)
flag_person$err_ddegreen1 <-
  if_else(grepl("藝專", flag_person$ddegreen1),
          1,
          flag_person$err_ddegreen1)
flag_person$err_ddegreen1 <-
  if_else(grepl("海專", flag_person$ddegreen1),
          1,
          flag_person$err_ddegreen1)
flag_person$err_ddegreen1 <-
  if_else(grepl("工專", flag_person$ddegreen1),
          1,
          flag_person$err_ddegreen1)
flag_person$err_ddegreen1 <-
  if_else(grepl("護專", flag_person$ddegreen1),
          1,
          flag_person$err_ddegreen1)
flag_person$err_ddegreen1 <-
  if_else(grepl("家專", flag_person$ddegreen1),
          1,
          flag_person$err_ddegreen1)
flag_person$err_ddegreen1 <-
  if_else(grepl("商專", flag_person$ddegreen1),
          1,
          flag_person$err_ddegreen1)
flag_person$err_ddegreen1 <-
  if_else(grepl("行專", flag_person$ddegreen1),
          1,
          flag_person$err_ddegreen1)
flag_person$err_ddegreen1 <-
  if_else(grepl("農專", flag_person$ddegreen1),
          1,
          flag_person$err_ddegreen1)
flag_person$err_ddegreen1 <-
  if_else(grepl("體專", flag_person$ddegreen1),
          1,
          flag_person$err_ddegreen1)
flag_person$err_ddegreen1 <-
  if_else(grepl("藥專", flag_person$ddegreen1),
          1,
          flag_person$err_ddegreen1)
flag_person$err_ddegreen1 <-
  if_else(grepl("師專", flag_person$ddegreen1),
          1,
          flag_person$err_ddegreen1)
flag_person$err_ddegreen1 <-
  if_else(grepl("醫專", flag_person$ddegreen1),
          1,
          flag_person$err_ddegreen1)
flag_person$err_ddegreen1 <-
  if_else(grepl("語專", flag_person$ddegreen1),
          1,
          flag_person$err_ddegreen1)
flag_person$err_ddegreen1 <-
  if_else(grepl("university", flag_person$ddegreen1),
          1,
          flag_person$err_ddegreen1)
flag_person$err_ddegreen1 <-
  if_else(grepl("University", flag_person$ddegreen1),
          1,
          flag_person$err_ddegreen1)
flag_person$err_ddegreen1 <-
  if_else(grepl("UNIVERSITY", flag_person$ddegreen1),
          1,
          flag_person$err_ddegreen1)
flag_person$err_ddegreen1 <-
  if_else(grepl("college", flag_person$ddegreen1),
          1,
          flag_person$err_ddegreen1)
flag_person$err_ddegreen1 <-
  if_else(grepl("College", flag_person$ddegreen1),
          1,
          flag_person$err_ddegreen1)
flag_person$err_ddegreen1 <-
  if_else(grepl("COLLEGE", flag_person$ddegreen1),
          1,
          flag_person$err_ddegreen1)
flag_person$err_ddegreen1 <-
  if_else(grepl("系", flag_person$ddegreen1),
          1,
          flag_person$err_ddegreen1)
flag_person$err_ddegreen1 <-
  if_else(grepl("所", flag_person$ddegreen1),
          1,
          flag_person$err_ddegreen1)
flag_person$err_ddegreen1 <-
  if_else(grepl("班$", flag_person$ddegreen1),
          1,
          flag_person$err_ddegreen1)
flag_person$err_ddegreen1 <-
  if_else(grepl("不分科系", flag_person$ddegreen1),
          1,
          flag_person$err_ddegreen1)
flag_person$err_ddegreen1 <-
  if_else(grepl("不分系", flag_person$ddegreen1),
          1,
          flag_person$err_ddegreen1)
flag_person$err_ddegreen1 <-
  if_else(grepl("department", flag_person$ddegreen1),
          1,
          flag_person$err_ddegreen1)
flag_person$err_ddegreen1 <-
  if_else(grepl("Department", flag_person$ddegreen1),
          1,
          flag_person$err_ddegreen1)
flag_person$err_ddegreen1 <-
  if_else(grepl("DEPARTMENT", flag_person$ddegreen1),
          1,
          flag_person$err_ddegreen1)
flag_person$err_ddegreen1 <-
  if_else(grepl("兼課", flag_person$ddegreen1),
          1,
          flag_person$err_ddegreen1)
flag_person$err_ddegreen1 <-
  if_else(grepl("最高學歷", flag_person$ddegreen1),
          1,
          flag_person$err_ddegreen1)
flag_person$err_ddegreen1 <-
  if_else(grepl("^Y$", flag_person$ddegreen1),
          1,
          flag_person$err_ddegreen1)
flag_person$err_ddegreen1 <-
  if_else(grepl("^待查詢$", flag_person$ddegreen1),
          1,
          flag_person$err_ddegreen1)
flag_person$err_ddegreen1 <-
  if_else(grepl("^無$", flag_person$ddegreen1),
          1,
          flag_person$err_ddegreen1)
flag_person$err_ddegreen1 <-
  if_else(grepl("^外國$", flag_person$ddegreen1),
          1,
          flag_person$err_ddegreen1)
flag_person$err_ddegreen1 <-
  if_else(grepl("^國立$", flag_person$ddegreen1),
          1,
          flag_person$err_ddegreen1)
flag_person$err_ddegreen1 <-
  if_else(grepl("^歐洲$", flag_person$ddegreen1),
          1,
          flag_person$err_ddegreen1)
flag_person$err_ddegreen1 <-
  if_else(grepl("^美洲$", flag_person$ddegreen1),
          1,
          flag_person$err_ddegreen1)
flag_person$err_ddegreen1 <-
  if_else(grepl("^亞洲$", flag_person$ddegreen1),
          1,
          flag_person$err_ddegreen1)
flag_person$err_ddegreen1 <-
  if_else(grepl("^非洲$", flag_person$ddegreen1),
          1,
          flag_person$err_ddegreen1)
flag_person$err_ddegreen1 <-
  if_else(grepl("^大洋洲$", flag_person$ddegreen1),
          1,
          flag_person$err_ddegreen1)
flag_person$err_ddegreen1 <-
  if_else(grepl("肄業", flag_person$ddegreen1),
          1,
          flag_person$err_ddegreen1)
flag_person$err_ddegreen1 <-
  if_else(grepl("學分班", flag_person$ddegreen1),
          1,
          flag_person$err_ddegreen1)
flag_person$err_ddegreen1 <-
  if_else(grepl("結業", flag_person$ddegreen1),
          1,
          flag_person$err_ddegreen1)

#博士學位畢業學校（一）
flag_person$err_ddegreeu1 <- 1
flag_person$err_ddegreeu1 <-
  if_else(grepl("大學", flag_person$ddegreeu1),
          0,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(grepl("分校", flag_person$ddegreeu1),
          0,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(grepl("學院", flag_person$ddegreeu1),
          0,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(grepl("師大", flag_person$ddegreeu1),
          0,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(grepl("科大", flag_person$ddegreeu1),
          0,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(grepl("學校", flag_person$ddegreeu1),
          0,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(grepl("官校", flag_person$ddegreeu1),
          0,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(grepl("預校", flag_person$ddegreeu1),
          0,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(grepl("書院", flag_person$ddegreeu1),
          0,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(grepl("體院", flag_person$ddegreeu1),
          0,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(grepl("專科", flag_person$ddegreeu1),
          0,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(grepl("藝專", flag_person$ddegreeu1),
          0,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(grepl("海專", flag_person$ddegreeu1),
          0,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(grepl("工專", flag_person$ddegreeu1),
          0,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(grepl("護專", flag_person$ddegreeu1),
          0,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(grepl("家專", flag_person$ddegreeu1),
          0,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(grepl("商專", flag_person$ddegreeu1),
          0,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(grepl("行專", flag_person$ddegreeu1),
          0,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(grepl("農專", flag_person$ddegreeu1),
          0,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(grepl("體專", flag_person$ddegreeu1),
          0,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(grepl("師專", flag_person$ddegreeu1),
          0,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(grepl("藥專", flag_person$ddegreeu1),
          0,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(grepl("醫專", flag_person$ddegreeu1),
          0,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(grepl("語專", flag_person$ddegreeu1),
          0,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(grepl("士校", flag_person$ddegreeu1),
          0,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(grepl("專校$", flag_person$ddegreeu1),
          0,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(grepl("university", flag_person$ddegreeu1),
          0,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(grepl("University", flag_person$ddegreeu1),
          0,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(grepl("UNIVERSITY", flag_person$ddegreeu1),
          0,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(grepl("Uni$", flag_person$ddegreeu1),
          0,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(grepl("college", flag_person$ddegreeu1),
          0,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(grepl("College", flag_person$ddegreeu1),
          0,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(grepl("COLLEGE", flag_person$ddegreeu1),
          0,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(grepl("Universidad", flag_person$ddegreeu1),
          0,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(grepl("UNIVERSIDAD", flag_person$ddegreeu1),
          0,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(grepl("Conservatory", flag_person$ddegreeu1),
          0,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(grepl("CRD", flag_person$ddegreeu1),
          0,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(grepl("ENM", flag_person$ddegreeu1),
          0,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(grepl("CRC", flag_person$ddegreeu1),
          0,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(grepl("EMMA", flag_person$ddegreeu1),
          0,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(grepl("CRR", flag_person$ddegreeu1),
          0,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(grepl("CNR", flag_person$ddegreeu1),
          0,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(grepl("TheNewSchool", flag_person$ddegreeu1),
          0,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(grepl("Hochschule", flag_person$ddegreeu1),
          0,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(
    grepl("BergenSchoolofArchitecture", flag_person$ddegreeu1),
    0,
    flag_person$err_ddegreeu1
  )
flag_person$err_ddegreeu1 <-
  if_else(grepl("Universitat", flag_person$ddegreeu1),
          0,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(grepl("Institute$", flag_person$ddegreeu1),
          0,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(grepl("StellenboschUni$", flag_person$ddegreeu1),
          0,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(grepl("^TUDarmstadt$", flag_person$ddegreeu1),
          0,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(grepl("^N$", flag_person$ddegreeu1),
          0,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(grepl("^莫斯科柴可夫斯基$", flag_person$ddegreeu1),
          0,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(grepl("音樂院$", flag_person$ddegreeu1),
          0,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(grepl("音樂研究所$", flag_person$ddegreeu1),
          0,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(grepl("大?$", flag_person$ddegreeu1),
          0,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(grepl("研究所博士班$", flag_person$ddegreeu1),
          0,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(grepl("^中興法商$", flag_person$ddegreeu1),
          0,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(grepl("^待查詢$", flag_person$ddegreeu1),
          0,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(grepl("本國", flag_person$ddegreeu1),
          1,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(grepl("^日本國立岡山大學$", flag_person$ddegreeu1),
          0,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(grepl("肄業", flag_person$ddegreeu1),
          1,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(grepl("學分班", flag_person$ddegreeu1),
          1,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(grepl("結業", flag_person$ddegreeu1),
          1,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(grepl("籌備處$", flag_person$ddegreeu1),
          1,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(grepl("Academy", flag_person$ddegreeu1),
          0,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(grepl("academy", flag_person$ddegreeu1),
          0,
          flag_person$err_ddegreeu1)
flag_person$err_ddegreeu1 <-
  if_else(grepl("ACADEMY", flag_person$ddegreeu1),
          0,
          flag_person$err_ddegreeu1)

#博士學位畢業系所（一）
flag_person$err_ddegreeg1 <- 0
flag_person$err_ddegreeg1 <-
  if_else(grepl("^博士$", flag_person$ddegreeg1),
          1,
          flag_person$err_ddegreeg1)
flag_person$err_ddegreeg1 <-
  if_else(grepl("碩士", flag_person$ddegreeg1),
          1,
          flag_person$err_ddegreeg1)
flag_person$err_ddegreeg1 <-
  if_else(grepl("學士", flag_person$ddegreeg1),
          1,
          flag_person$err_ddegreeg1)
flag_person$err_ddegreeg1 <-
  if_else(grepl("副學士", flag_person$ddegreeg1),
          1,
          flag_person$err_ddegreeg1)
flag_person$err_ddegreeg1 <-
  if_else(grepl("大學", flag_person$ddegreeg1),
          1,
          flag_person$err_ddegreeg1)
flag_person$err_ddegreeg1 <-
  if_else(grepl("分校", flag_person$ddegreeg1),
          1,
          flag_person$err_ddegreeg1)
flag_person$err_ddegreeg1 <-
  if_else(grepl("^學院$", flag_person$ddegreeg1),
          1,
          flag_person$err_ddegreeg1)
flag_person$err_ddegreeg1 <-
  if_else(grepl("科大", flag_person$ddegreeg1),
          1,
          flag_person$err_ddegreeg1)
flag_person$err_ddegreeg1 <-
  if_else(grepl("學校", flag_person$ddegreeg1),
          1,
          flag_person$err_ddegreeg1)
flag_person$err_ddegreeg1 <-
  if_else(grepl("官校", flag_person$ddegreeg1),
          1,
          flag_person$err_ddegreeg1)
flag_person$err_ddegreeg1 <-
  if_else(grepl("預校", flag_person$ddegreeg1),
          1,
          flag_person$err_ddegreeg1)
flag_person$err_ddegreeg1 <-
  if_else(grepl("書院", flag_person$ddegreeg1),
          1,
          flag_person$err_ddegreeg1)
flag_person$err_ddegreeg1 <-
  if_else(grepl("專科", flag_person$ddegreeg1),
          1,
          flag_person$err_ddegreeg1)
flag_person$err_ddegreeg1 <-
  if_else(grepl("藝專", flag_person$ddegreeg1),
          1,
          flag_person$err_ddegreeg1)
flag_person$err_ddegreeg1 <-
  if_else(grepl("海專", flag_person$ddegreeg1),
          1,
          flag_person$err_ddegreeg1)
flag_person$err_ddegreeg1 <-
  if_else(grepl("工專", flag_person$ddegreeg1),
          1,
          flag_person$err_ddegreeg1)
flag_person$err_ddegreeg1 <-
  if_else(grepl("護專", flag_person$ddegreeg1),
          1,
          flag_person$err_ddegreeg1)
flag_person$err_ddegreeg1 <-
  if_else(grepl("家專", flag_person$ddegreeg1),
          1,
          flag_person$err_ddegreeg1)
flag_person$err_ddegreeg1 <-
  if_else(grepl("商專", flag_person$ddegreeg1),
          1,
          flag_person$err_ddegreeg1)
flag_person$err_ddegreeg1 <-
  if_else(grepl("行專", flag_person$ddegreeg1),
          1,
          flag_person$err_ddegreeg1)
flag_person$err_ddegreeg1 <-
  if_else(grepl("農專", flag_person$ddegreeg1),
          1,
          flag_person$err_ddegreeg1)
flag_person$err_ddegreeg1 <-
  if_else(grepl("體專", flag_person$ddegreeg1),
          1,
          flag_person$err_ddegreeg1)
flag_person$err_ddegreeg1 <-
  if_else(grepl("藥專", flag_person$ddegreeg1),
          1,
          flag_person$err_ddegreeg1)
flag_person$err_ddegreeg1 <-
  if_else(grepl("師專", flag_person$ddegreeg1),
          1,
          flag_person$err_ddegreeg1)
flag_person$err_ddegreeg1 <-
  if_else(grepl("醫專", flag_person$ddegreeg1),
          1,
          flag_person$err_ddegreeg1)
flag_person$err_ddegreeg1 <-
  if_else(grepl("語專", flag_person$ddegreeg1),
          1,
          flag_person$err_ddegreeg1)
flag_person$err_ddegreeg1 <-
  if_else(grepl("university", flag_person$ddegreeg1),
          1,
          flag_person$err_ddegreeg1)
flag_person$err_ddegreeg1 <-
  if_else(grepl("University", flag_person$ddegreeg1),
          1,
          flag_person$err_ddegreeg1)
flag_person$err_ddegreeg1 <-
  if_else(grepl("UNIVERSITY", flag_person$ddegreeg1),
          1,
          flag_person$err_ddegreeg1)
flag_person$err_ddegreeg1 <-
  if_else(grepl("college", flag_person$ddegreeg1),
          1,
          flag_person$err_ddegreeg1)
flag_person$err_ddegreeg1 <-
  if_else(grepl("College", flag_person$ddegreeg1),
          1,
          flag_person$err_ddegreeg1)
flag_person$err_ddegreeg1 <-
  if_else(grepl("COLLEGE", flag_person$ddegreeg1),
          1,
          flag_person$err_ddegreeg1)
flag_person$err_ddegreeg1 <-
  if_else(grepl("兼課", flag_person$ddegreeg1),
          1,
          flag_person$err_ddegreeg1)
flag_person$err_ddegreeg1 <-
  if_else(grepl("最高學歷", flag_person$ddegreeg1),
          1,
          flag_person$err_ddegreeg1)
flag_person$err_ddegreeg1 <-
  if_else(grepl("^Y$", flag_person$ddegreeg1),
          1,
          flag_person$err_ddegreeg1)
flag_person$err_ddegreeg1 <-
  if_else(grepl("^待查詢$", flag_person$ddegreeg1),
          1,
          flag_person$err_ddegreeg1)
flag_person$err_ddegreeg1 <-
  if_else(grepl("^無$", flag_person$ddegreeg1),
          1,
          flag_person$err_ddegreeg1)
flag_person$err_ddegreeg1 <-
  if_else(grepl("肄業", flag_person$ddegreeg1),
          1,
          flag_person$err_ddegreeg1)
flag_person$err_ddegreeg1 <-
  if_else(grepl("學分班", flag_person$ddegreeg1),
          1,
          flag_person$err_ddegreeg1)
flag_person$err_ddegreeg1 <-
  if_else(grepl("結業", flag_person$ddegreeg1),
          1,
          flag_person$err_ddegreeg1)
flag_person$err_ddegreeg1 <-
  if_else(grepl("行政$", flag_person$ddegreeg1),
          1,
          flag_person$err_ddegreeg1)

#博士學位畢業學校國別（二）
flag_person$err_ddegreen2 <- 0
flag_person$err_ddegreen2 <-
  if_else(grepl("博士", flag_person$ddegreen2),
          1,
          flag_person$err_ddegreen2)
flag_person$err_ddegreen2 <-
  if_else(grepl("碩士", flag_person$ddegreen2),
          1,
          flag_person$err_ddegreen2)
flag_person$err_ddegreen2 <-
  if_else(grepl("學士", flag_person$ddegreen2),
          1,
          flag_person$err_ddegreen2)
flag_person$err_ddegreen2 <-
  if_else(grepl("副學士", flag_person$ddegreen2),
          1,
          flag_person$err_ddegreen2)
flag_person$err_ddegreen2 <-
  if_else(grepl("大學", flag_person$ddegreen2),
          1,
          flag_person$err_ddegreen2)
flag_person$err_ddegreen2 <-
  if_else(grepl("分校", flag_person$ddegreen2),
          1,
          flag_person$err_ddegreen2)
flag_person$err_ddegreen2 <-
  if_else(grepl("學院", flag_person$ddegreen2),
          1,
          flag_person$err_ddegreen2)
flag_person$err_ddegreen2 <-
  if_else(grepl("科大", flag_person$ddegreen2),
          1,
          flag_person$err_ddegreen2)
flag_person$err_ddegreen2 <-
  if_else(grepl("學校", flag_person$ddegreen2),
          1,
          flag_person$err_ddegreen2)
flag_person$err_ddegreen2 <-
  if_else(grepl("官校", flag_person$ddegreen2),
          1,
          flag_person$err_ddegreen2)
flag_person$err_ddegreen2 <-
  if_else(grepl("預校", flag_person$ddegreen2),
          1,
          flag_person$err_ddegreen2)
flag_person$err_ddegreen2 <-
  if_else(grepl("書院", flag_person$ddegreen2),
          1,
          flag_person$err_ddegreen2)
flag_person$err_ddegreen2 <-
  if_else(grepl("專科", flag_person$ddegreen2),
          1,
          flag_person$err_ddegreen2)
flag_person$err_ddegreen2 <-
  if_else(grepl("藝專", flag_person$ddegreen2),
          1,
          flag_person$err_ddegreen2)
flag_person$err_ddegreen2 <-
  if_else(grepl("海專", flag_person$ddegreen2),
          1,
          flag_person$err_ddegreen2)
flag_person$err_ddegreen2 <-
  if_else(grepl("工專", flag_person$ddegreen2),
          1,
          flag_person$err_ddegreen2)
flag_person$err_ddegreen2 <-
  if_else(grepl("護專", flag_person$ddegreen2),
          1,
          flag_person$err_ddegreen2)
flag_person$err_ddegreen2 <-
  if_else(grepl("家專", flag_person$ddegreen2),
          1,
          flag_person$err_ddegreen2)
flag_person$err_ddegreen2 <-
  if_else(grepl("商專", flag_person$ddegreen2),
          1,
          flag_person$err_ddegreen2)
flag_person$err_ddegreen2 <-
  if_else(grepl("行專", flag_person$ddegreen2),
          1,
          flag_person$err_ddegreen2)
flag_person$err_ddegreen2 <-
  if_else(grepl("農專", flag_person$ddegreen2),
          1,
          flag_person$err_ddegreen2)
flag_person$err_ddegreen2 <-
  if_else(grepl("體專", flag_person$ddegreen2),
          1,
          flag_person$err_ddegreen2)
flag_person$err_ddegreen2 <-
  if_else(grepl("藥專", flag_person$ddegreen2),
          1,
          flag_person$err_ddegreen2)
flag_person$err_ddegreen2 <-
  if_else(grepl("師專", flag_person$ddegreen2),
          1,
          flag_person$err_ddegreen2)
flag_person$err_ddegreen2 <-
  if_else(grepl("醫專", flag_person$ddegreen2),
          1,
          flag_person$err_ddegreen2)
flag_person$err_ddegreen2 <-
  if_else(grepl("語專", flag_person$ddegreen2),
          1,
          flag_person$err_ddegreen2)
flag_person$err_ddegreen2 <-
  if_else(grepl("university", flag_person$ddegreen2),
          1,
          flag_person$err_ddegreen2)
flag_person$err_ddegreen2 <-
  if_else(grepl("University", flag_person$ddegreen2),
          1,
          flag_person$err_ddegreen2)
flag_person$err_ddegreen2 <-
  if_else(grepl("UNIVERSITY", flag_person$ddegreen2),
          1,
          flag_person$err_ddegreen2)
flag_person$err_ddegreen2 <-
  if_else(grepl("college", flag_person$ddegreen2),
          1,
          flag_person$err_ddegreen2)
flag_person$err_ddegreen2 <-
  if_else(grepl("College", flag_person$ddegreen2),
          1,
          flag_person$err_ddegreen2)
flag_person$err_ddegreen2 <-
  if_else(grepl("COLLEGE", flag_person$ddegreen2),
          1,
          flag_person$err_ddegreen2)
flag_person$err_ddegreen2 <-
  if_else(grepl("系", flag_person$ddegreen2),
          1,
          flag_person$err_ddegreen2)
flag_person$err_ddegreen2 <-
  if_else(grepl("所", flag_person$ddegreen2),
          1,
          flag_person$err_ddegreen2)
flag_person$err_ddegreen2 <-
  if_else(grepl("班$", flag_person$ddegreen2),
          1,
          flag_person$err_ddegreen2)
flag_person$err_ddegreen2 <-
  if_else(grepl("不分科系", flag_person$ddegreen2),
          1,
          flag_person$err_ddegreen2)
flag_person$err_ddegreen2 <-
  if_else(grepl("不分系", flag_person$ddegreen2),
          1,
          flag_person$err_ddegreen2)
flag_person$err_ddegreen2 <-
  if_else(grepl("department", flag_person$ddegreen2),
          1,
          flag_person$err_ddegreen2)
flag_person$err_ddegreen2 <-
  if_else(grepl("Department", flag_person$ddegreen2),
          1,
          flag_person$err_ddegreen2)
flag_person$err_ddegreen2 <-
  if_else(grepl("DEPARTMENT", flag_person$ddegreen2),
          1,
          flag_person$err_ddegreen2)
flag_person$err_ddegreen2 <-
  if_else(grepl("兼課", flag_person$ddegreen2),
          1,
          flag_person$err_ddegreen2)
flag_person$err_ddegreen2 <-
  if_else(grepl("最高學歷", flag_person$ddegreen2),
          1,
          flag_person$err_ddegreen2)
flag_person$err_ddegreen2 <-
  if_else(grepl("^Y$", flag_person$ddegreen2),
          1,
          flag_person$err_ddegreen2)
flag_person$err_ddegreen2 <-
  if_else(grepl("^待查詢$", flag_person$ddegreen2),
          1,
          flag_person$err_ddegreen2)
flag_person$err_ddegreen2 <-
  if_else(grepl("^無$", flag_person$ddegreen2),
          1,
          flag_person$err_ddegreen2)
flag_person$err_ddegreen2 <-
  if_else(grepl("^外國$", flag_person$ddegreen2),
          1,
          flag_person$err_ddegreen2)
flag_person$err_ddegreen2 <-
  if_else(grepl("^國立$", flag_person$ddegreen2),
          1,
          flag_person$err_ddegreen2)
flag_person$err_ddegreen2 <-
  if_else(grepl("^歐洲$", flag_person$ddegreen2),
          1,
          flag_person$err_ddegreen2)
flag_person$err_ddegreen2 <-
  if_else(grepl("^美洲$", flag_person$ddegreen2),
          1,
          flag_person$err_ddegreen2)
flag_person$err_ddegreen2 <-
  if_else(grepl("^亞洲$", flag_person$ddegreen2),
          1,
          flag_person$err_ddegreen2)
flag_person$err_ddegreen2 <-
  if_else(grepl("^非洲$", flag_person$ddegreen2),
          1,
          flag_person$err_ddegreen2)
flag_person$err_ddegreen2 <-
  if_else(grepl("^大洋洲$", flag_person$ddegreen2),
          1,
          flag_person$err_ddegreen2)
flag_person$err_ddegreen2 <-
  if_else(grepl("肄業", flag_person$ddegreen2),
          1,
          flag_person$err_ddegreen2)
flag_person$err_ddegreen2 <-
  if_else(grepl("學分班", flag_person$ddegreen2),
          1,
          flag_person$err_ddegreen2)
flag_person$err_ddegreen2 <-
  if_else(grepl("結業", flag_person$ddegreen2),
          1,
          flag_person$err_ddegreen2)

#博士學位畢業學校（二）
flag_person$err_ddegreeu2 <- 1
flag_person$err_ddegreeu2 <-
  if_else(grepl("大學", flag_person$ddegreeu2),
          0,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(grepl("分校", flag_person$ddegreeu2),
          0,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(grepl("學院", flag_person$ddegreeu2),
          0,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(grepl("師大", flag_person$ddegreeu2),
          0,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(grepl("科大", flag_person$ddegreeu2),
          0,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(grepl("學校", flag_person$ddegreeu2),
          0,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(grepl("官校", flag_person$ddegreeu2),
          0,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(grepl("預校", flag_person$ddegreeu2),
          0,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(grepl("書院", flag_person$ddegreeu2),
          0,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(grepl("體院", flag_person$ddegreeu2),
          0,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(grepl("專科", flag_person$ddegreeu2),
          0,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(grepl("藝專", flag_person$ddegreeu2),
          0,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(grepl("海專", flag_person$ddegreeu2),
          0,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(grepl("工專", flag_person$ddegreeu2),
          0,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(grepl("護專", flag_person$ddegreeu2),
          0,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(grepl("家專", flag_person$ddegreeu2),
          0,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(grepl("商專", flag_person$ddegreeu2),
          0,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(grepl("行專", flag_person$ddegreeu2),
          0,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(grepl("農專", flag_person$ddegreeu2),
          0,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(grepl("體專", flag_person$ddegreeu2),
          0,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(grepl("師專", flag_person$ddegreeu2),
          0,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(grepl("藥專", flag_person$ddegreeu2),
          0,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(grepl("醫專", flag_person$ddegreeu2),
          0,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(grepl("語專", flag_person$ddegreeu2),
          0,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(grepl("士校", flag_person$ddegreeu2),
          0,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(grepl("專校$", flag_person$ddegreeu2),
          0,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(grepl("university", flag_person$ddegreeu2),
          0,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(grepl("University", flag_person$ddegreeu2),
          0,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(grepl("UNIVERSITY", flag_person$ddegreeu2),
          0,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(grepl("Uni$", flag_person$ddegreeu2),
          0,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(grepl("college", flag_person$ddegreeu2),
          0,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(grepl("College", flag_person$ddegreeu2),
          0,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(grepl("COLLEGE", flag_person$ddegreeu2),
          0,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(grepl("Universidad", flag_person$ddegreeu2),
          0,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(grepl("UNIVERSIDAD", flag_person$ddegreeu2),
          0,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(grepl("Conservatory", flag_person$ddegreeu2),
          0,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(grepl("CRD", flag_person$ddegreeu2),
          0,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(grepl("ENM", flag_person$ddegreeu2),
          0,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(grepl("CRC", flag_person$ddegreeu2),
          0,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(grepl("EMMA", flag_person$ddegreeu2),
          0,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(grepl("CRR", flag_person$ddegreeu2),
          0,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(grepl("CNR", flag_person$ddegreeu2),
          0,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(grepl("TheNewSchool", flag_person$ddegreeu2),
          0,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(grepl("Hochschule", flag_person$ddegreeu2),
          0,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(
    grepl("BergenSchoolofArchitecture", flag_person$ddegreeu2),
    0,
    flag_person$err_ddegreeu2
  )
flag_person$err_ddegreeu2 <-
  if_else(grepl("Universitat", flag_person$ddegreeu2),
          0,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(grepl("Institute$", flag_person$ddegreeu2),
          0,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(grepl("StellenboschUni$", flag_person$ddegreeu2),
          0,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(grepl("^TUDarmstadt$", flag_person$ddegreeu2),
          0,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(grepl("^N$", flag_person$ddegreeu2),
          0,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(grepl("^莫斯科柴可夫斯基$", flag_person$ddegreeu2),
          0,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(grepl("音樂院$", flag_person$ddegreeu2),
          0,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(grepl("音樂研究所$", flag_person$ddegreeu2),
          0,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(grepl("大?$", flag_person$ddegreeu2),
          0,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(grepl("研究所博士班$", flag_person$ddegreeu2),
          0,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(grepl("^中興法商$", flag_person$ddegreeu2),
          0,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(grepl("^待查詢$", flag_person$ddegreeu2),
          0,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(grepl("本國", flag_person$ddegreeu2),
          1,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(grepl("^日本國立岡山大學$", flag_person$ddegreeu2),
          0,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(grepl("肄業", flag_person$ddegreeu2),
          1,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(grepl("學分班", flag_person$ddegreeu2),
          1,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(grepl("結業", flag_person$ddegreeu2),
          1,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(grepl("籌備處$", flag_person$ddegreeu2),
          1,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(grepl("Academy", flag_person$ddegreeu2),
          0,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(grepl("academy", flag_person$ddegreeu2),
          0,
          flag_person$err_ddegreeu2)
flag_person$err_ddegreeu2 <-
  if_else(grepl("ACADEMY", flag_person$ddegreeu2),
          0,
          flag_person$err_ddegreeu2)

#博士學位畢業系所（二）
flag_person$err_ddegreeg2 <- 0
flag_person$err_ddegreeg2 <-
  if_else(grepl("^博士$", flag_person$ddegreeg2),
          1,
          flag_person$err_ddegreeg2)
flag_person$err_ddegreeg2 <-
  if_else(grepl("碩士", flag_person$ddegreeg2),
          1,
          flag_person$err_ddegreeg2)
flag_person$err_ddegreeg2 <-
  if_else(grepl("學士", flag_person$ddegreeg2),
          1,
          flag_person$err_ddegreeg2)
flag_person$err_ddegreeg2 <-
  if_else(grepl("副學士", flag_person$ddegreeg2),
          1,
          flag_person$err_ddegreeg2)
flag_person$err_ddegreeg2 <-
  if_else(grepl("大學", flag_person$ddegreeg2),
          1,
          flag_person$err_ddegreeg2)
flag_person$err_ddegreeg2 <-
  if_else(grepl("分校", flag_person$ddegreeg2),
          1,
          flag_person$err_ddegreeg2)
flag_person$err_ddegreeg2 <-
  if_else(grepl("^學院$", flag_person$ddegreeg2),
          1,
          flag_person$err_ddegreeg2)
flag_person$err_ddegreeg2 <-
  if_else(grepl("科大", flag_person$ddegreeg2),
          1,
          flag_person$err_ddegreeg2)
flag_person$err_ddegreeg2 <-
  if_else(grepl("學校", flag_person$ddegreeg2),
          1,
          flag_person$err_ddegreeg2)
flag_person$err_ddegreeg2 <-
  if_else(grepl("官校", flag_person$ddegreeg2),
          1,
          flag_person$err_ddegreeg2)
flag_person$err_ddegreeg2 <-
  if_else(grepl("預校", flag_person$ddegreeg2),
          1,
          flag_person$err_ddegreeg2)
flag_person$err_ddegreeg2 <-
  if_else(grepl("書院", flag_person$ddegreeg2),
          1,
          flag_person$err_ddegreeg2)
flag_person$err_ddegreeg2 <-
  if_else(grepl("專科", flag_person$ddegreeg2),
          1,
          flag_person$err_ddegreeg2)
flag_person$err_ddegreeg2 <-
  if_else(grepl("藝專", flag_person$ddegreeg2),
          1,
          flag_person$err_ddegreeg2)
flag_person$err_ddegreeg2 <-
  if_else(grepl("海專", flag_person$ddegreeg2),
          1,
          flag_person$err_ddegreeg2)
flag_person$err_ddegreeg2 <-
  if_else(grepl("工專", flag_person$ddegreeg2),
          1,
          flag_person$err_ddegreeg2)
flag_person$err_ddegreeg2 <-
  if_else(grepl("護專", flag_person$ddegreeg2),
          1,
          flag_person$err_ddegreeg2)
flag_person$err_ddegreeg2 <-
  if_else(grepl("家專", flag_person$ddegreeg2),
          1,
          flag_person$err_ddegreeg2)
flag_person$err_ddegreeg2 <-
  if_else(grepl("商專", flag_person$ddegreeg2),
          1,
          flag_person$err_ddegreeg2)
flag_person$err_ddegreeg2 <-
  if_else(grepl("行專", flag_person$ddegreeg2),
          1,
          flag_person$err_ddegreeg2)
flag_person$err_ddegreeg2 <-
  if_else(grepl("農專", flag_person$ddegreeg2),
          1,
          flag_person$err_ddegreeg2)
flag_person$err_ddegreeg2 <-
  if_else(grepl("體專", flag_person$ddegreeg2),
          1,
          flag_person$err_ddegreeg2)
flag_person$err_ddegreeg2 <-
  if_else(grepl("藥專", flag_person$ddegreeg2),
          1,
          flag_person$err_ddegreeg2)
flag_person$err_ddegreeg2 <-
  if_else(grepl("師專", flag_person$ddegreeg2),
          1,
          flag_person$err_ddegreeg2)
flag_person$err_ddegreeg2 <-
  if_else(grepl("醫專", flag_person$ddegreeg2),
          1,
          flag_person$err_ddegreeg2)
flag_person$err_ddegreeg2 <-
  if_else(grepl("語專", flag_person$ddegreeg2),
          1,
          flag_person$err_ddegreeg2)
flag_person$err_ddegreeg2 <-
  if_else(grepl("university", flag_person$ddegreeg2),
          1,
          flag_person$err_ddegreeg2)
flag_person$err_ddegreeg2 <-
  if_else(grepl("University", flag_person$ddegreeg2),
          1,
          flag_person$err_ddegreeg2)
flag_person$err_ddegreeg2 <-
  if_else(grepl("UNIVERSITY", flag_person$ddegreeg2),
          1,
          flag_person$err_ddegreeg2)
flag_person$err_ddegreeg2 <-
  if_else(grepl("college", flag_person$ddegreeg2),
          1,
          flag_person$err_ddegreeg2)
flag_person$err_ddegreeg2 <-
  if_else(grepl("College", flag_person$ddegreeg2),
          1,
          flag_person$err_ddegreeg2)
flag_person$err_ddegreeg2 <-
  if_else(grepl("COLLEGE", flag_person$ddegreeg2),
          1,
          flag_person$err_ddegreeg2)
flag_person$err_ddegreeg2 <-
  if_else(grepl("兼課", flag_person$ddegreeg2),
          1,
          flag_person$err_ddegreeg2)
flag_person$err_ddegreeg2 <-
  if_else(grepl("最高學歷", flag_person$ddegreeg2),
          1,
          flag_person$err_ddegreeg2)
flag_person$err_ddegreeg2 <-
  if_else(grepl("^Y$", flag_person$ddegreeg2),
          1,
          flag_person$err_ddegreeg2)
flag_person$err_ddegreeg2 <-
  if_else(grepl("^待查詢$", flag_person$ddegreeg2),
          1,
          flag_person$err_ddegreeg2)
flag_person$err_ddegreeg2 <-
  if_else(grepl("^無$", flag_person$ddegreeg2),
          1,
          flag_person$err_ddegreeg2)
flag_person$err_ddegreeg2 <-
  if_else(grepl("肄業", flag_person$ddegreeg2),
          1,
          flag_person$err_ddegreeg2)
flag_person$err_ddegreeg2 <-
  if_else(grepl("學分班", flag_person$ddegreeg2),
          1,
          flag_person$err_ddegreeg2)
flag_person$err_ddegreeg2 <-
  if_else(grepl("結業", flag_person$ddegreeg2),
          1,
          flag_person$err_ddegreeg2)
flag_person$err_ddegreeg2 <-
  if_else(grepl("行政$", flag_person$ddegreeg2),
          1,
          flag_person$err_ddegreeg2)

#碩士學位畢業學校國別（一）
flag_person$err_mdegreen1 <- 0
flag_person$err_mdegreen1 <-
  if_else(grepl("博士", flag_person$mdegreen1),
          1,
          flag_person$err_mdegreen1)
flag_person$err_mdegreen1 <-
  if_else(grepl("碩士", flag_person$mdegreen1),
          1,
          flag_person$err_mdegreen1)
flag_person$err_mdegreen1 <-
  if_else(grepl("學士", flag_person$mdegreen1),
          1,
          flag_person$err_mdegreen1)
flag_person$err_mdegreen1 <-
  if_else(grepl("副學士", flag_person$mdegreen1),
          1,
          flag_person$err_mdegreen1)
flag_person$err_mdegreen1 <-
  if_else(grepl("大學", flag_person$mdegreen1),
          1,
          flag_person$err_mdegreen1)
flag_person$err_mdegreen1 <-
  if_else(grepl("分校", flag_person$mdegreen1),
          1,
          flag_person$err_mdegreen1)
flag_person$err_mdegreen1 <-
  if_else(grepl("學院", flag_person$mdegreen1),
          1,
          flag_person$err_mdegreen1)
flag_person$err_mdegreen1 <-
  if_else(grepl("科大", flag_person$mdegreen1),
          1,
          flag_person$err_mdegreen1)
flag_person$err_mdegreen1 <-
  if_else(grepl("學校", flag_person$mdegreen1),
          1,
          flag_person$err_mdegreen1)
flag_person$err_mdegreen1 <-
  if_else(grepl("官校", flag_person$mdegreen1),
          1,
          flag_person$err_mdegreen1)
flag_person$err_mdegreen1 <-
  if_else(grepl("預校", flag_person$mdegreen1),
          1,
          flag_person$err_mdegreen1)
flag_person$err_mdegreen1 <-
  if_else(grepl("書院", flag_person$mdegreen1),
          1,
          flag_person$err_mdegreen1)
flag_person$err_mdegreen1 <-
  if_else(grepl("專科", flag_person$mdegreen1),
          1,
          flag_person$err_mdegreen1)
flag_person$err_mdegreen1 <-
  if_else(grepl("藝專", flag_person$mdegreen1),
          1,
          flag_person$err_mdegreen1)
flag_person$err_mdegreen1 <-
  if_else(grepl("海專", flag_person$mdegreen1),
          1,
          flag_person$err_mdegreen1)
flag_person$err_mdegreen1 <-
  if_else(grepl("工專", flag_person$mdegreen1),
          1,
          flag_person$err_mdegreen1)
flag_person$err_mdegreen1 <-
  if_else(grepl("護專", flag_person$mdegreen1),
          1,
          flag_person$err_mdegreen1)
flag_person$err_mdegreen1 <-
  if_else(grepl("家專", flag_person$mdegreen1),
          1,
          flag_person$err_mdegreen1)
flag_person$err_mdegreen1 <-
  if_else(grepl("商專", flag_person$mdegreen1),
          1,
          flag_person$err_mdegreen1)
flag_person$err_mdegreen1 <-
  if_else(grepl("行專", flag_person$mdegreen1),
          1,
          flag_person$err_mdegreen1)
flag_person$err_mdegreen1 <-
  if_else(grepl("農專", flag_person$mdegreen1),
          1,
          flag_person$err_mdegreen1)
flag_person$err_mdegreen1 <-
  if_else(grepl("體專", flag_person$mdegreen1),
          1,
          flag_person$err_mdegreen1)
flag_person$err_mdegreen1 <-
  if_else(grepl("藥專", flag_person$mdegreen1),
          1,
          flag_person$err_mdegreen1)
flag_person$err_mdegreen1 <-
  if_else(grepl("師專", flag_person$mdegreen1),
          1,
          flag_person$err_mdegreen1)
flag_person$err_mdegreen1 <-
  if_else(grepl("醫專", flag_person$mdegreen1),
          1,
          flag_person$err_mdegreen1)
flag_person$err_mdegreen1 <-
  if_else(grepl("語專", flag_person$mdegreen1),
          1,
          flag_person$err_mdegreen1)
flag_person$err_mdegreen1 <-
  if_else(grepl("university", flag_person$mdegreen1),
          1,
          flag_person$err_mdegreen1)
flag_person$err_mdegreen1 <-
  if_else(grepl("University", flag_person$mdegreen1),
          1,
          flag_person$err_mdegreen1)
flag_person$err_mdegreen1 <-
  if_else(grepl("UNIVERSITY", flag_person$mdegreen1),
          1,
          flag_person$err_mdegreen1)
flag_person$err_mdegreen1 <-
  if_else(grepl("college", flag_person$mdegreen1),
          1,
          flag_person$err_mdegreen1)
flag_person$err_mdegreen1 <-
  if_else(grepl("College", flag_person$mdegreen1),
          1,
          flag_person$err_mdegreen1)
flag_person$err_mdegreen1 <-
  if_else(grepl("COLLEGE", flag_person$mdegreen1),
          1,
          flag_person$err_mdegreen1)
flag_person$err_mdegreen1 <-
  if_else(grepl("系", flag_person$mdegreen1),
          1,
          flag_person$err_mdegreen1)
flag_person$err_mdegreen1 <-
  if_else(grepl("所", flag_person$mdegreen1),
          1,
          flag_person$err_mdegreen1)
flag_person$err_mdegreen1 <-
  if_else(grepl("班$", flag_person$mdegreen1),
          1,
          flag_person$err_mdegreen1)
flag_person$err_mdegreen1 <-
  if_else(grepl("不分科系", flag_person$mdegreen1),
          1,
          flag_person$err_mdegreen1)
flag_person$err_mdegreen1 <-
  if_else(grepl("不分系", flag_person$mdegreen1),
          1,
          flag_person$err_mdegreen1)
flag_person$err_mdegreen1 <-
  if_else(grepl("department", flag_person$mdegreen1),
          1,
          flag_person$err_mdegreen1)
flag_person$err_mdegreen1 <-
  if_else(grepl("Department", flag_person$mdegreen1),
          1,
          flag_person$err_mdegreen1)
flag_person$err_mdegreen1 <-
  if_else(grepl("DEPARTMENT", flag_person$mdegreen1),
          1,
          flag_person$err_mdegreen1)
flag_person$err_mdegreen1 <-
  if_else(grepl("^逕讀", flag_person$mdegreen1),
          0,
          flag_person$err_mdegreen1)
flag_person$err_mdegreen1 <-
  if_else(grepl("^逕讀碩士$", flag_person$mdegreen1),
          1,
          flag_person$err_mdegreen1)
flag_person$err_mdegreen1 <-
  if_else(grepl("^兼課", flag_person$mdegreen1),
          1,
          flag_person$err_mdegreen1)
flag_person$err_mdegreen1 <-
  if_else(grepl("^最高學歷", flag_person$mdegreen1),
          1,
          flag_person$err_mdegreen1)
flag_person$err_mdegreen1 <-
  if_else(grepl("^Y$", flag_person$mdegreen1),
          1,
          flag_person$err_mdegreen1)
flag_person$err_mdegreen1 <-
  if_else(grepl("^待查詢$", flag_person$mdegreen1),
          1,
          flag_person$err_mdegreen1)
flag_person$err_mdegreen1 <-
  if_else(grepl("^無$", flag_person$mdegreen1),
          1,
          flag_person$err_mdegreen1)
flag_person$err_mdegreen1 <-
  if_else(grepl("^外國$", flag_person$mdegreen1),
          1,
          flag_person$err_mdegreen1)
flag_person$err_mdegreen1 <-
  if_else(grepl("^國立$", flag_person$mdegreen1),
          1,
          flag_person$err_mdegreen1)
flag_person$err_mdegreen1 <-
  if_else(grepl("^歐洲$", flag_person$mdegreen1),
          1,
          flag_person$err_mdegreen1)
flag_person$err_mdegreen1 <-
  if_else(grepl("^亞洲$", flag_person$mdegreen1),
          1,
          flag_person$err_mdegreen1)
flag_person$err_mdegreen1 <-
  if_else(grepl("^美洲$", flag_person$mdegreen1),
          1,
          flag_person$err_mdegreen1)
flag_person$err_mdegreen1 <-
  if_else(grepl("^非洲$", flag_person$mdegreen1),
          1,
          flag_person$err_mdegreen1)
flag_person$err_mdegreen1 <-
  if_else(grepl("^大洋洲$", flag_person$mdegreen1),
          1,
          flag_person$err_mdegreen1)
flag_person$err_mdegreen1 <-
  if_else(grepl("肄業", flag_person$mdegreen1),
          1,
          flag_person$err_mdegreen1)
flag_person$err_mdegreen1 <-
  if_else(grepl("學分班", flag_person$mdegreen1),
          1,
          flag_person$err_mdegreen1)
flag_person$err_mdegreen1 <-
  if_else(grepl("結業", flag_person$mdegreen1),
          1,
          flag_person$err_mdegreen1)

#碩士學位畢業學校學校（一）
flag_person$err_mdegreeu1 <- 1
flag_person$err_mdegreeu1 <-
  if_else(grepl("大學", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("分校", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("學院", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("師院", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("師大", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("科大", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("學校", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("官校", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("預校", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("書院", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("體院", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("專科", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("藝專", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("海專", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("工專", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("護專", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("家專", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("商專", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("行專", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("農專", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("體專", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("師專", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("藥專", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("醫專", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("語專", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("士校", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("專校$", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("逕讀", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("音樂院$", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("音樂研究所$", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("university", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("University", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("UNIVERSITY", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("Uni$", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("UNIVERSIT", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("college", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("College", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("COLLEGE", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("Universidad$", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("UNIVERSIDAD$", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("Conservatory$", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("CRD$", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("ENM$", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("CRC$", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("EMMA$", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("CRR$", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("CNR$", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("TheNewSchool$", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("Hochschule$", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(
    grepl("BergenSchoolofArchitecture$",
          flag_person$mdegreeu1),
    0,
    flag_person$err_mdegreeu1
  )
flag_person$err_mdegreeu1 <-
  if_else(grepl("Universitat$", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("Institute$", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("StellenboschUni$", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("^TUDarmstadt$", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("^大?$", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("^研究所博士班$", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("^中興法商$", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("^N$", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("^待查詢$", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("^無$", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("^離職$", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("^因故$", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(
    grepl("^NavalPostgraduateSchool$", flag_person$mdegreeu1),
    0,
    flag_person$err_mdegreeu1
  )
flag_person$err_mdegreeu1 <-
  if_else(
    grepl(
      "^BiblicalInterpretationLondonSchoolofTheology$",
      flag_person$mdegreeu1
    ),
    0,
    flag_person$err_mdegreeu1
  )
flag_person$err_mdegreeu1 <-
  if_else(grepl("^DallasBaptistUniv$", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(
    grepl("^NewYorkFilmAcademy$", flag_person$mdegreeu1),
    0,
    flag_person$err_mdegreeu1
  )
flag_person$err_mdegreeu1 <-
  if_else(
    grepl(
      "^ConservatorioStatalediMilano“GiuseppeVerdi”Italia$",
      flag_person$mdegreeu1
    ),
    0,
    flag_person$err_mdegreeu1
  )
flag_person$err_mdegreeu1 <-
  if_else(grepl("^衛理神學研究院$", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(
    grepl("^KnowledgeSystemInstitute$", flag_person$mdegreeu1),
    0,
    flag_person$err_mdegreeu1
  )
flag_person$err_mdegreeu1 <-
  if_else(
    grepl(
      "^ColumbiaBiblicalSeminaryandSchoolofMissions$",
      flag_person$mdegreeu1
    ),
    0,
    flag_person$err_mdegreeu1
  )
flag_person$err_mdegreeu1 <-
  if_else(
    grepl("^UnitecInstituteofTechnology$",
          flag_person$mdegreeu1),
    0,
    flag_person$err_mdegreeu1
  )
flag_person$err_mdegreeu1 <-
  if_else(grepl("^巴拉圭高等戰略研究院$", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("^本國$", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("^肄業$", flag_person$mdegreeu1),
          1,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("^學分班$", flag_person$mdegreeu1),
          1,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("^籌備處$", flag_person$mdegreeu1),
          1,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("^高雄餐旅$", flag_person$mdegreeu1),
          1,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(
    grepl("^universiteJeanMoulinLyon3$",
          flag_person$mdegreeu1),
    0,
    flag_person$err_mdegreeu1
  )
flag_person$err_mdegreeu1 <-
  if_else(
    grepl("^UniversityCollegeLondon$", flag_person$mdegreeu1),
    0,
    flag_person$err_mdegreeu1
  )
flag_person$err_mdegreeu1 <-
  if_else(
    grepl("^BirminghamUiversity$", flag_person$mdegreeu1),
    0,
    flag_person$err_mdegreeu1
  )
flag_person$err_mdegreeu1 <-
  if_else(
    grepl(
      "^ColumbiaUniversity哥倫比亞大學MathematicsEducation$",
      flag_person$mdegreeu1
    ),
    0,
    flag_person$err_mdegreeu1
  )
flag_person$err_mdegreeu1 <-
  if_else(
    grepl(
      "^StevensInstituteofTechnology，NJ，USA$",
      flag_person$mdegreeu1
    ),
    0,
    flag_person$err_mdegreeu1
  )
flag_person$err_mdegreeu1 <-
  if_else(
    grepl("^UNITEDSTATESSPORTSACADEMY$",
          flag_person$mdegreeu1),
    0,
    flag_person$err_mdegreeu1
  )
flag_person$err_mdegreeu1 <-
  if_else(
    grepl("^KnowledgeSystemsInstitute$",
          flag_person$mdegreeu1),
    0,
    flag_person$err_mdegreeu1
  )
flag_person$err_mdegreeu1 <-
  if_else(grepl("DomusAcademy", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(grepl("DOMUSACADEMY", flag_person$mdegreeu1),
          0,
          flag_person$err_mdegreeu1)
flag_person$err_mdegreeu1 <-
  if_else(
    grepl("^UniversiteStendhalGrenobleIII$",
          flag_person$mdegreeu1),
    0,
    flag_person$err_mdegreeu1
  )

#碩士學位畢業系所（一）
flag_person$err_mdegreeg1 <- 0
flag_person$err_mdegreeg1 <-
  if_else(grepl("博士", flag_person$mdegreeg1),
          1,
          flag_person$err_mdegreeg1)
flag_person$err_mdegreeg1 <-
  if_else(grepl("^逕讀博士$", flag_person$mdegreeg1),
          0,
          flag_person$err_mdegreeg1)
flag_person$err_mdegreeg1 <-
  if_else(grepl("^碩士$", flag_person$mdegreeg1),
          1,
          flag_person$err_mdegreeg1)
flag_person$err_mdegreeg1 <-
  if_else(grepl("學士", flag_person$mdegreeg1),
          1,
          flag_person$err_mdegreeg1)
flag_person$err_mdegreeg1 <-
  if_else(grepl("副學士", flag_person$mdegreeg1),
          1,
          flag_person$err_mdegreeg1)
flag_person$err_mdegreeg1 <-
  if_else(grepl("大學", flag_person$mdegreeg1),
          1,
          flag_person$err_mdegreeg1)
flag_person$err_mdegreeg1 <-
  if_else(grepl("分校", flag_person$mdegreeg1),
          1,
          flag_person$err_mdegreeg1)
flag_person$err_mdegreeg1 <-
  if_else(grepl("^學院$", flag_person$mdegreeg1),
          1,
          flag_person$err_mdegreeg1)
flag_person$err_mdegreeg1 <-
  if_else(grepl("科大", flag_person$mdegreeg1),
          1,
          flag_person$err_mdegreeg1)
flag_person$err_mdegreeg1 <-
  if_else(grepl("學校", flag_person$mdegreeg1),
          1,
          flag_person$err_mdegreeg1)
flag_person$err_mdegreeg1 <-
  if_else(grepl("官校", flag_person$mdegreeg1),
          1,
          flag_person$err_mdegreeg1)
flag_person$err_mdegreeg1 <-
  if_else(grepl("預校", flag_person$mdegreeg1),
          1,
          flag_person$err_mdegreeg1)
flag_person$err_mdegreeg1 <-
  if_else(grepl("書院", flag_person$mdegreeg1),
          1,
          flag_person$err_mdegreeg1)
flag_person$err_mdegreeg1 <-
  if_else(grepl("專科", flag_person$mdegreeg1),
          1,
          flag_person$err_mdegreeg1)
flag_person$err_mdegreeg1 <-
  if_else(grepl("藝專", flag_person$mdegreeg1),
          1,
          flag_person$err_mdegreeg1)
flag_person$err_mdegreeg1 <-
  if_else(grepl("海專", flag_person$mdegreeg1),
          1,
          flag_person$err_mdegreeg1)
flag_person$err_mdegreeg1 <-
  if_else(grepl("工專", flag_person$mdegreeg1),
          1,
          flag_person$err_mdegreeg1)
flag_person$err_mdegreeg1 <-
  if_else(grepl("護專", flag_person$mdegreeg1),
          1,
          flag_person$err_mdegreeg1)
flag_person$err_mdegreeg1 <-
  if_else(grepl("家專", flag_person$mdegreeg1),
          1,
          flag_person$err_mdegreeg1)
flag_person$err_mdegreeg1 <-
  if_else(grepl("商專", flag_person$mdegreeg1),
          1,
          flag_person$err_mdegreeg1)
flag_person$err_mdegreeg1 <-
  if_else(grepl("行專", flag_person$mdegreeg1),
          1,
          flag_person$err_mdegreeg1)
flag_person$err_mdegreeg1 <-
  if_else(grepl("農專", flag_person$mdegreeg1),
          1,
          flag_person$err_mdegreeg1)
flag_person$err_mdegreeg1 <-
  if_else(grepl("體專", flag_person$mdegreeg1),
          1,
          flag_person$err_mdegreeg1)
flag_person$err_mdegreeg1 <-
  if_else(grepl("藥專", flag_person$mdegreeg1),
          1,
          flag_person$err_mdegreeg1)
flag_person$err_mdegreeg1 <-
  if_else(grepl("師專", flag_person$mdegreeg1),
          1,
          flag_person$err_mdegreeg1)
flag_person$err_mdegreeg1 <-
  if_else(grepl("醫專", flag_person$mdegreeg1),
          1,
          flag_person$err_mdegreeg1)
flag_person$err_mdegreeg1 <-
  if_else(grepl("語專", flag_person$mdegreeg1),
          1,
          flag_person$err_mdegreeg1)
flag_person$err_mdegreeg1 <-
  if_else(grepl("university", flag_person$mdegreeg1),
          1,
          flag_person$err_mdegreeg1)
flag_person$err_mdegreeg1 <-
  if_else(grepl("University", flag_person$mdegreeg1),
          1,
          flag_person$err_mdegreeg1)
flag_person$err_mdegreeg1 <-
  if_else(grepl("UNIVERSITY", flag_person$mdegreeg1),
          1,
          flag_person$err_mdegreeg1)
flag_person$err_mdegreeg1 <-
  if_else(grepl("college", flag_person$mdegreeg1),
          1,
          flag_person$err_mdegreeg1)
flag_person$err_mdegreeg1 <-
  if_else(grepl("College", flag_person$mdegreeg1),
          1,
          flag_person$err_mdegreeg1)
flag_person$err_mdegreeg1 <-
  if_else(grepl("COLLEGE", flag_person$mdegreeg1),
          1,
          flag_person$err_mdegreeg1)
flag_person$err_mdegreeg1 <-
  if_else(grepl("兼課", flag_person$mdegreeg1),
          1,
          flag_person$err_mdegreeg1)
flag_person$err_mdegreeg1 <-
  if_else(grepl("最高學歷", flag_person$mdegreeg1),
          1,
          flag_person$err_mdegreeg1)
flag_person$err_mdegreeg1 <-
  if_else(grepl("^Y$", flag_person$mdegreeg1),
          1,
          flag_person$err_mdegreeg1)
flag_person$err_mdegreeg1 <-
  if_else(grepl("^待查詢$", flag_person$mdegreeg1),
          1,
          flag_person$err_mdegreeg1)
flag_person$err_mdegreeg1 <-
  if_else(grepl("^無$", flag_person$mdegreeg1),
          1,
          flag_person$err_mdegreeg1)
flag_person$err_mdegreeg1 <-
  if_else(grepl("肄業", flag_person$mdegreeg1),
          1,
          flag_person$err_mdegreeg1)
flag_person$err_mdegreeg1 <-
  if_else(grepl("學分班", flag_person$mdegreeg1),
          1,
          flag_person$err_mdegreeg1)
flag_person$err_mdegreeg1 <-
  if_else(grepl("行政$", flag_person$mdegreeg1),
          1,
          flag_person$err_mdegreeg1)
flag_person$err_mdegreeg1 <-
  if_else(
    grepl("^教育政策與行政$", flag_person$mdegreeg1) &
      (
        grepl("國立臺灣師範大學", flag_person$mdegreeu1) |
          grepl("國立台灣師範大學", flag_person$mdegreeu1)
      ),
    0,
    flag_person$err_mdegreeg1
  )
flag_person$err_mdegreeg1 <-
  if_else(
    grepl("^社會教育學系學校圖書行政$", flag_person$mdegreeg1) &
      (
        grepl("國立臺灣師範大學", flag_person$mdegreeu1) |
          grepl("國立台灣師範大學", flag_person$mdegreeu1)
      ),
    0,
    flag_person$err_mdegreeg1
  )
flag_person$err_mdegreeg1 <-
  if_else(
    grepl("^工業教育學系技職教育行政$", flag_person$mdegreeg1) &
      (
        grepl("國立臺灣師範大學", flag_person$mdegreeu1) |
          grepl("國立台灣師範大學", flag_person$mdegreeu1)
      ),
    0,
    flag_person$err_mdegreeg1
  )
flag_person$err_mdegreeg1 <-
  if_else(
    grepl("^教師在職進修教學及學校行政碩士學位班$", flag_person$mdegreeg1) &
      grepl("中山大學", flag_person$mdegreeu1),
    0,
    flag_person$err_mdegreeg1
  )


#碩士學位畢業學校國別（二）
flag_person$err_mdegreen2 <- 0
flag_person$err_mdegreen2 <-
  if_else(grepl("博士", flag_person$mdegreen2),
          1,
          flag_person$err_mdegreen2)
flag_person$err_mdegreen2 <-
  if_else(grepl("碩士", flag_person$mdegreen2),
          1,
          flag_person$err_mdegreen2)
flag_person$err_mdegreen2 <-
  if_else(grepl("學士", flag_person$mdegreen2),
          1,
          flag_person$err_mdegreen2)
flag_person$err_mdegreen2 <-
  if_else(grepl("副學士", flag_person$mdegreen2),
          1,
          flag_person$err_mdegreen2)
flag_person$err_mdegreen2 <-
  if_else(grepl("大學", flag_person$mdegreen2),
          1,
          flag_person$err_mdegreen2)
flag_person$err_mdegreen2 <-
  if_else(grepl("分校", flag_person$mdegreen2),
          1,
          flag_person$err_mdegreen2)
flag_person$err_mdegreen2 <-
  if_else(grepl("學院", flag_person$mdegreen2),
          1,
          flag_person$err_mdegreen2)
flag_person$err_mdegreen2 <-
  if_else(grepl("科大", flag_person$mdegreen2),
          1,
          flag_person$err_mdegreen2)
flag_person$err_mdegreen2 <-
  if_else(grepl("學校", flag_person$mdegreen2),
          1,
          flag_person$err_mdegreen2)
flag_person$err_mdegreen2 <-
  if_else(grepl("官校", flag_person$mdegreen2),
          1,
          flag_person$err_mdegreen2)
flag_person$err_mdegreen2 <-
  if_else(grepl("預校", flag_person$mdegreen2),
          1,
          flag_person$err_mdegreen2)
flag_person$err_mdegreen2 <-
  if_else(grepl("書院", flag_person$mdegreen2),
          1,
          flag_person$err_mdegreen2)
flag_person$err_mdegreen2 <-
  if_else(grepl("專科", flag_person$mdegreen2),
          1,
          flag_person$err_mdegreen2)
flag_person$err_mdegreen2 <-
  if_else(grepl("藝專", flag_person$mdegreen2),
          1,
          flag_person$err_mdegreen2)
flag_person$err_mdegreen2 <-
  if_else(grepl("海專", flag_person$mdegreen2),
          1,
          flag_person$err_mdegreen2)
flag_person$err_mdegreen2 <-
  if_else(grepl("工專", flag_person$mdegreen2),
          1,
          flag_person$err_mdegreen2)
flag_person$err_mdegreen2 <-
  if_else(grepl("護專", flag_person$mdegreen2),
          1,
          flag_person$err_mdegreen2)
flag_person$err_mdegreen2 <-
  if_else(grepl("家專", flag_person$mdegreen2),
          1,
          flag_person$err_mdegreen2)
flag_person$err_mdegreen2 <-
  if_else(grepl("商專", flag_person$mdegreen2),
          1,
          flag_person$err_mdegreen2)
flag_person$err_mdegreen2 <-
  if_else(grepl("行專", flag_person$mdegreen2),
          1,
          flag_person$err_mdegreen2)
flag_person$err_mdegreen2 <-
  if_else(grepl("農專", flag_person$mdegreen2),
          1,
          flag_person$err_mdegreen2)
flag_person$err_mdegreen2 <-
  if_else(grepl("體專", flag_person$mdegreen2),
          1,
          flag_person$err_mdegreen2)
flag_person$err_mdegreen2 <-
  if_else(grepl("藥專", flag_person$mdegreen2),
          1,
          flag_person$err_mdegreen2)
flag_person$err_mdegreen2 <-
  if_else(grepl("師專", flag_person$mdegreen2),
          1,
          flag_person$err_mdegreen2)
flag_person$err_mdegreen2 <-
  if_else(grepl("醫專", flag_person$mdegreen2),
          1,
          flag_person$err_mdegreen2)
flag_person$err_mdegreen2 <-
  if_else(grepl("語專", flag_person$mdegreen2),
          1,
          flag_person$err_mdegreen2)
flag_person$err_mdegreen2 <-
  if_else(grepl("university", flag_person$mdegreen2),
          1,
          flag_person$err_mdegreen2)
flag_person$err_mdegreen2 <-
  if_else(grepl("University", flag_person$mdegreen2),
          1,
          flag_person$err_mdegreen2)
flag_person$err_mdegreen2 <-
  if_else(grepl("UNIVERSITY", flag_person$mdegreen2),
          1,
          flag_person$err_mdegreen2)
flag_person$err_mdegreen2 <-
  if_else(grepl("college", flag_person$mdegreen2),
          1,
          flag_person$err_mdegreen2)
flag_person$err_mdegreen2 <-
  if_else(grepl("College", flag_person$mdegreen2),
          1,
          flag_person$err_mdegreen2)
flag_person$err_mdegreen2 <-
  if_else(grepl("COLLEGE", flag_person$mdegreen2),
          1,
          flag_person$err_mdegreen2)
flag_person$err_mdegreen2 <-
  if_else(grepl("系", flag_person$mdegreen2),
          1,
          flag_person$err_mdegreen2)
flag_person$err_mdegreen2 <-
  if_else(grepl("所", flag_person$mdegreen2),
          1,
          flag_person$err_mdegreen2)
flag_person$err_mdegreen2 <-
  if_else(grepl("班$", flag_person$mdegreen2),
          1,
          flag_person$err_mdegreen2)
flag_person$err_mdegreen2 <-
  if_else(grepl("不分科系", flag_person$mdegreen2),
          1,
          flag_person$err_mdegreen2)
flag_person$err_mdegreen2 <-
  if_else(grepl("不分系", flag_person$mdegreen2),
          1,
          flag_person$err_mdegreen2)
flag_person$err_mdegreen2 <-
  if_else(grepl("department", flag_person$mdegreen2),
          1,
          flag_person$err_mdegreen2)
flag_person$err_mdegreen2 <-
  if_else(grepl("Department", flag_person$mdegreen2),
          1,
          flag_person$err_mdegreen2)
flag_person$err_mdegreen2 <-
  if_else(grepl("DEPARTMENT", flag_person$mdegreen2),
          1,
          flag_person$err_mdegreen2)
flag_person$err_mdegreen2 <-
  if_else(grepl("^逕讀", flag_person$mdegreen2),
          0,
          flag_person$err_mdegreen2)
flag_person$err_mdegreen2 <-
  if_else(grepl("^逕讀碩士$", flag_person$mdegreen2),
          1,
          flag_person$err_mdegreen2)
flag_person$err_mdegreen2 <-
  if_else(grepl("^兼課", flag_person$mdegreen2),
          1,
          flag_person$err_mdegreen2)
flag_person$err_mdegreen2 <-
  if_else(grepl("^最高學歷", flag_person$mdegreen2),
          1,
          flag_person$err_mdegreen2)
flag_person$err_mdegreen2 <-
  if_else(grepl("^Y$", flag_person$mdegreen2),
          1,
          flag_person$err_mdegreen2)
flag_person$err_mdegreen2 <-
  if_else(grepl("^待查詢$", flag_person$mdegreen2),
          1,
          flag_person$err_mdegreen2)
flag_person$err_mdegreen2 <-
  if_else(grepl("^無$", flag_person$mdegreen2),
          1,
          flag_person$err_mdegreen2)
flag_person$err_mdegreen2 <-
  if_else(grepl("^外國$", flag_person$mdegreen2),
          1,
          flag_person$err_mdegreen2)
flag_person$err_mdegreen2 <-
  if_else(grepl("^國立$", flag_person$mdegreen2),
          1,
          flag_person$err_mdegreen2)
flag_person$err_mdegreen2 <-
  if_else(grepl("^歐洲$", flag_person$mdegreen2),
          1,
          flag_person$err_mdegreen2)
flag_person$err_mdegreen2 <-
  if_else(grepl("^亞洲$", flag_person$mdegreen2),
          1,
          flag_person$err_mdegreen2)
flag_person$err_mdegreen2 <-
  if_else(grepl("^美洲$", flag_person$mdegreen2),
          1,
          flag_person$err_mdegreen2)
flag_person$err_mdegreen2 <-
  if_else(grepl("^非洲$", flag_person$mdegreen2),
          1,
          flag_person$err_mdegreen2)
flag_person$err_mdegreen2 <-
  if_else(grepl("^大洋洲$", flag_person$mdegreen2),
          1,
          flag_person$err_mdegreen2)
flag_person$err_mdegreen2 <-
  if_else(grepl("肄業", flag_person$mdegreen2),
          1,
          flag_person$err_mdegreen2)
flag_person$err_mdegreen2 <-
  if_else(grepl("學分班", flag_person$mdegreen2),
          1,
          flag_person$err_mdegreen2)

#碩士學位畢業學校學校（二）
flag_person$err_mdegreeu2 <- 1
flag_person$err_mdegreeu2 <-
  if_else(grepl("大學", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("分校", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("學院", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("師院", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("師大", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("科大", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("學校", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("官校", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("預校", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("書院", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("體院", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("專科", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("藝專", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("海專", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("工專", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("護專", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("家專", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("商專", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("行專", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("農專", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("體專", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("師專", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("藥專", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("醫專", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("語專", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("士校", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("專校$", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("逕讀$", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("音樂院$", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("音樂研究所$", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("university$", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("University$", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("UNIVERSITY$", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("Uni$", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("college", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("College", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("COLLEGE", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("Universidad$", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("UNIVERSIDAD$", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("Conservatory$", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("CRD$", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("ENM$", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("CRC$", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("EMMA$", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("CRR$", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("CNR$", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("TheNewSchool$", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("Hochschule$", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(
    grepl("BergenSchoolofArchitecture$",
          flag_person$mdegreeu2),
    0,
    flag_person$err_mdegreeu2
  )
flag_person$err_mdegreeu2 <-
  if_else(grepl("Universitat$", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("Institute$", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("StellenboschUni$", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("^TUDarmstadt$", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("^大?$", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("^研究所博士班$", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("^中興法商$", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("^N$", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("^待查詢$", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("^無$", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("^離職$", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("^因故$", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(
    grepl("^NavalPostgraduateSchool$", flag_person$mdegreeu2),
    0,
    flag_person$err_mdegreeu2
  )
flag_person$err_mdegreeu2 <-
  if_else(
    grepl(
      "^BiblicalInterpretationLondonSchoolofTheology$",
      flag_person$mdegreeu2
    ),
    0,
    flag_person$err_mdegreeu2
  )
flag_person$err_mdegreeu2 <-
  if_else(grepl("^DallasBaptistUniv$", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(
    grepl("^NewYorkFilmAcademy$", flag_person$mdegreeu2),
    0,
    flag_person$err_mdegreeu2
  )
flag_person$err_mdegreeu2 <-
  if_else(
    grepl(
      "^ConservatorioStatalediMilano“GiuseppeVerdi”Italia$",
      flag_person$mdegreeu2
    ),
    0,
    flag_person$err_mdegreeu2
  )
flag_person$err_mdegreeu2 <-
  if_else(grepl("^衛理神學研究院$", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(
    grepl("^KnowledgeSystemInstitute$", flag_person$mdegreeu2),
    0,
    flag_person$err_mdegreeu2
  )
flag_person$err_mdegreeu2 <-
  if_else(
    grepl(
      "^ColumbiaBiblicalSeminaryandSchoolofMissions$",
      flag_person$mdegreeu2
    ),
    0,
    flag_person$err_mdegreeu2
  )
flag_person$err_mdegreeu2 <-
  if_else(
    grepl("^UnitecInstituteofTechnology$",
          flag_person$mdegreeu2),
    0,
    flag_person$err_mdegreeu2
  )
flag_person$err_mdegreeu2 <-
  if_else(grepl("^巴拉圭高等戰略研究院$", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("^本國$", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("^肄業$", flag_person$mdegreeu2),
          1,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("^學分班$", flag_person$mdegreeu2),
          1,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("^籌備處$", flag_person$mdegreeu2),
          1,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("^高雄餐旅$", flag_person$mdegreeu2),
          1,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(
    grepl("^universiteJeanMoulinLyon3$",
          flag_person$mdegreeu2),
    0,
    flag_person$err_mdegreeu2
  )
flag_person$err_mdegreeu2 <-
  if_else(
    grepl("^UniversityCollegeLondon$", flag_person$mdegreeu2),
    0,
    flag_person$err_mdegreeu2
  )
flag_person$err_mdegreeu2 <-
  if_else(
    grepl("^BirminghamUiversity$", flag_person$mdegreeu2),
    0,
    flag_person$err_mdegreeu2
  )
flag_person$err_mdegreeu2 <-
  if_else(
    grepl(
      "^ColumbiaUniversity哥倫比亞大學MathematicsEducation$",
      flag_person$mdegreeu2
    ),
    0,
    flag_person$err_mdegreeu2
  )
flag_person$err_mdegreeu2 <-
  if_else(
    grepl(
      "^StevensInstituteofTechnology，NJ，USA$",
      flag_person$mdegreeu2
    ),
    0,
    flag_person$err_mdegreeu2
  )
flag_person$err_mdegreeu2 <-
  if_else(
    grepl("^UNITEDSTATESSPORTSACADEMY$",
          flag_person$mdegreeu2),
    0,
    flag_person$err_mdegreeu2
  )
flag_person$err_mdegreeu2 <-
  if_else(
    grepl("^KnowledgeSystemsInstitute$",
          flag_person$mdegreeu2),
    0,
    flag_person$err_mdegreeu2
  )
flag_person$err_mdegreeu2 <-
  if_else(grepl("DomusAcademy", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(grepl("DOMUSACADEMY", flag_person$mdegreeu2),
          0,
          flag_person$err_mdegreeu2)
flag_person$err_mdegreeu2 <-
  if_else(
    grepl("^UniversiteStendhalGrenobleIII$",
          flag_person$mdegreeu2),
    0,
    flag_person$err_mdegreeu2
  )

#碩士學位畢業系所（二）
flag_person$err_mdegreeg2 <- 0
flag_person$err_mdegreeg2 <-
  if_else(grepl("博士", flag_person$mdegreeg2),
          1,
          flag_person$err_mdegreeg2)
flag_person$err_mdegreeg2 <-
  if_else(grepl("^逕讀博士$", flag_person$mdegreeg2),
          0,
          flag_person$err_mdegreeg2)
flag_person$err_mdegreeg2 <-
  if_else(grepl("^碩士$", flag_person$mdegreeg2),
          1,
          flag_person$err_mdegreeg2)
flag_person$err_mdegreeg2 <-
  if_else(grepl("學士", flag_person$mdegreeg2),
          1,
          flag_person$err_mdegreeg2)
flag_person$err_mdegreeg2 <-
  if_else(grepl("副學士", flag_person$mdegreeg2),
          1,
          flag_person$err_mdegreeg2)
flag_person$err_mdegreeg2 <-
  if_else(grepl("大學", flag_person$mdegreeg2),
          1,
          flag_person$err_mdegreeg2)
flag_person$err_mdegreeg2 <-
  if_else(grepl("分校", flag_person$mdegreeg2),
          1,
          flag_person$err_mdegreeg2)
flag_person$err_mdegreeg2 <-
  if_else(grepl("^學院$", flag_person$mdegreeg2),
          1,
          flag_person$err_mdegreeg2)
flag_person$err_mdegreeg2 <-
  if_else(grepl("科大", flag_person$mdegreeg2),
          1,
          flag_person$err_mdegreeg2)
flag_person$err_mdegreeg2 <-
  if_else(grepl("學校", flag_person$mdegreeg2),
          1,
          flag_person$err_mdegreeg2)
flag_person$err_mdegreeg2 <-
  if_else(grepl("官校", flag_person$mdegreeg2),
          1,
          flag_person$err_mdegreeg2)
flag_person$err_mdegreeg2 <-
  if_else(grepl("預校", flag_person$mdegreeg2),
          1,
          flag_person$err_mdegreeg2)
flag_person$err_mdegreeg2 <-
  if_else(grepl("書院", flag_person$mdegreeg2),
          1,
          flag_person$err_mdegreeg2)
flag_person$err_mdegreeg2 <-
  if_else(grepl("專科", flag_person$mdegreeg2),
          1,
          flag_person$err_mdegreeg2)
flag_person$err_mdegreeg2 <-
  if_else(grepl("藝專", flag_person$mdegreeg2),
          1,
          flag_person$err_mdegreeg2)
flag_person$err_mdegreeg2 <-
  if_else(grepl("海專", flag_person$mdegreeg2),
          1,
          flag_person$err_mdegreeg2)
flag_person$err_mdegreeg2 <-
  if_else(grepl("工專", flag_person$mdegreeg2),
          1,
          flag_person$err_mdegreeg2)
flag_person$err_mdegreeg2 <-
  if_else(grepl("護專", flag_person$mdegreeg2),
          1,
          flag_person$err_mdegreeg2)
flag_person$err_mdegreeg2 <-
  if_else(grepl("家專", flag_person$mdegreeg2),
          1,
          flag_person$err_mdegreeg2)
flag_person$err_mdegreeg2 <-
  if_else(grepl("商專", flag_person$mdegreeg2),
          1,
          flag_person$err_mdegreeg2)
flag_person$err_mdegreeg2 <-
  if_else(grepl("行專", flag_person$mdegreeg2),
          1,
          flag_person$err_mdegreeg2)
flag_person$err_mdegreeg2 <-
  if_else(grepl("農專", flag_person$mdegreeg2),
          1,
          flag_person$err_mdegreeg2)
flag_person$err_mdegreeg2 <-
  if_else(grepl("體專", flag_person$mdegreeg2),
          1,
          flag_person$err_mdegreeg2)
flag_person$err_mdegreeg2 <-
  if_else(grepl("藥專", flag_person$mdegreeg2),
          1,
          flag_person$err_mdegreeg2)
flag_person$err_mdegreeg2 <-
  if_else(grepl("師專", flag_person$mdegreeg2),
          1,
          flag_person$err_mdegreeg2)
flag_person$err_mdegreeg2 <-
  if_else(grepl("醫專", flag_person$mdegreeg2),
          1,
          flag_person$err_mdegreeg2)
flag_person$err_mdegreeg2 <-
  if_else(grepl("語專", flag_person$mdegreeg2),
          1,
          flag_person$err_mdegreeg2)
flag_person$err_mdegreeg2 <-
  if_else(grepl("university", flag_person$mdegreeg2),
          1,
          flag_person$err_mdegreeg2)
flag_person$err_mdegreeg2 <-
  if_else(grepl("University", flag_person$mdegreeg2),
          1,
          flag_person$err_mdegreeg2)
flag_person$err_mdegreeg2 <-
  if_else(grepl("UNIVERSITY", flag_person$mdegreeg2),
          1,
          flag_person$err_mdegreeg2)
flag_person$err_mdegreeg2 <-
  if_else(grepl("college", flag_person$mdegreeg2),
          1,
          flag_person$err_mdegreeg2)
flag_person$err_mdegreeg2 <-
  if_else(grepl("College", flag_person$mdegreeg2),
          1,
          flag_person$err_mdegreeg2)
flag_person$err_mdegreeg2 <-
  if_else(grepl("COLLEGE", flag_person$mdegreeg2),
          1,
          flag_person$err_mdegreeg2)
flag_person$err_mdegreeg2 <-
  if_else(grepl("兼課", flag_person$mdegreeg2),
          1,
          flag_person$err_mdegreeg2)
flag_person$err_mdegreeg2 <-
  if_else(grepl("最高學歷", flag_person$mdegreeg2),
          1,
          flag_person$err_mdegreeg2)
flag_person$err_mdegreeg2 <-
  if_else(grepl("^Y$", flag_person$mdegreeg2),
          1,
          flag_person$err_mdegreeg2)
flag_person$err_mdegreeg2 <-
  if_else(grepl("^待查詢$", flag_person$mdegreeg2),
          1,
          flag_person$err_mdegreeg2)
flag_person$err_mdegreeg2 <-
  if_else(grepl("^無$", flag_person$mdegreeg2),
          1,
          flag_person$err_mdegreeg2)
flag_person$err_mdegreeg2 <-
  if_else(grepl("肄業", flag_person$mdegreeg2),
          1,
          flag_person$err_mdegreeg2)
flag_person$err_mdegreeg2 <-
  if_else(grepl("學分班", flag_person$mdegreeg2),
          1,
          flag_person$err_mdegreeg2)
flag_person$err_mdegreeg2 <-
  if_else(grepl("行政$", flag_person$mdegreeg2),
          1,
          flag_person$err_mdegreeg2)
flag_person$err_mdegreeg2 <-
  if_else(
    grepl("^教育政策與行政$", flag_person$mdegreeg2) &
      (
        grepl("國立臺灣師範大學", flag_person$mdegreeu1) |
          grepl("國立台灣師範大學", flag_person$mdegreeu1)
      ),
    0,
    flag_person$err_mdegreeg2
  )
flag_person$err_mdegreeg2 <-
  if_else(
    grepl("^社會教育學系學校圖書行政$", flag_person$mdegreeg2) &
      (
        grepl("國立臺灣師範大學", flag_person$mdegreeu1) |
          grepl("國立台灣師範大學", flag_person$mdegreeu1)
      ),
    0,
    flag_person$err_mdegreeg2
  )
flag_person$err_mdegreeg2 <-
  if_else(
    grepl("^工業教育學系技職教育行政$", flag_person$mdegreeg2) &
      (
        grepl("國立臺灣師範大學", flag_person$mdegreeu1) |
          grepl("國立台灣師範大學", flag_person$mdegreeu1)
      ),
    0,
    flag_person$err_mdegreeg2
  )
flag_person$err_mdegreeg2 <-
  if_else(
    grepl("^教師在職進修教學及學校行政碩士學位班$", flag_person$mdegreeg2) &
      grepl("中山大學", flag_person$mdegreeu2),
    0,
    flag_person$err_mdegreeg2
  )

#學士學位畢業學校國別（一）
flag_person$err_bdegreen1 <- 0
flag_person$err_bdegreen1 <-
  if_else(grepl("博士", flag_person$bdegreen1),
          1,
          flag_person$err_bdegreen1)
flag_person$err_bdegreen1 <-
  if_else(grepl("碩士", flag_person$bdegreen1),
          1,
          flag_person$err_bdegreen1)
flag_person$err_bdegreen1 <-
  if_else(grepl("學士", flag_person$bdegreen1),
          1,
          flag_person$err_bdegreen1)
flag_person$err_bdegreen1 <-
  if_else(grepl("副學士", flag_person$bdegreen1),
          1,
          flag_person$err_bdegreen1)
flag_person$err_bdegreen1 <-
  if_else(grepl("大學", flag_person$bdegreen1),
          1,
          flag_person$err_bdegreen1)
flag_person$err_bdegreen1 <-
  if_else(grepl("分校", flag_person$bdegreen1),
          1,
          flag_person$err_bdegreen1)
flag_person$err_bdegreen1 <-
  if_else(grepl("學院", flag_person$bdegreen1),
          1,
          flag_person$err_bdegreen1)
flag_person$err_bdegreen1 <-
  if_else(grepl("科大", flag_person$bdegreen1),
          1,
          flag_person$err_bdegreen1)
flag_person$err_bdegreen1 <-
  if_else(grepl("學校", flag_person$bdegreen1),
          1,
          flag_person$err_bdegreen1)
flag_person$err_bdegreen1 <-
  if_else(grepl("官校", flag_person$bdegreen1),
          1,
          flag_person$err_bdegreen1)
flag_person$err_bdegreen1 <-
  if_else(grepl("預校", flag_person$bdegreen1),
          1,
          flag_person$err_bdegreen1)
flag_person$err_bdegreen1 <-
  if_else(grepl("書院", flag_person$bdegreen1),
          1,
          flag_person$err_bdegreen1)
flag_person$err_bdegreen1 <-
  if_else(grepl("專科", flag_person$bdegreen1),
          1,
          flag_person$err_bdegreen1)
flag_person$err_bdegreen1 <-
  if_else(grepl("藝專", flag_person$bdegreen1),
          1,
          flag_person$err_bdegreen1)
flag_person$err_bdegreen1 <-
  if_else(grepl("海專", flag_person$bdegreen1),
          1,
          flag_person$err_bdegreen1)
flag_person$err_bdegreen1 <-
  if_else(grepl("工專", flag_person$bdegreen1),
          1,
          flag_person$err_bdegreen1)
flag_person$err_bdegreen1 <-
  if_else(grepl("護專", flag_person$bdegreen1),
          1,
          flag_person$err_bdegreen1)
flag_person$err_bdegreen1 <-
  if_else(grepl("家專", flag_person$bdegreen1),
          1,
          flag_person$err_bdegreen1)
flag_person$err_bdegreen1 <-
  if_else(grepl("商專", flag_person$bdegreen1),
          1,
          flag_person$err_bdegreen1)
flag_person$err_bdegreen1 <-
  if_else(grepl("行專", flag_person$bdegreen1),
          1,
          flag_person$err_bdegreen1)
flag_person$err_bdegreen1 <-
  if_else(grepl("農專", flag_person$bdegreen1),
          1,
          flag_person$err_bdegreen1)
flag_person$err_bdegreen1 <-
  if_else(grepl("體專", flag_person$bdegreen1),
          1,
          flag_person$err_bdegreen1)
flag_person$err_bdegreen1 <-
  if_else(grepl("藥專", flag_person$bdegreen1),
          1,
          flag_person$err_bdegreen1)
flag_person$err_bdegreen1 <-
  if_else(grepl("師專", flag_person$bdegreen1),
          1,
          flag_person$err_bdegreen1)
flag_person$err_bdegreen1 <-
  if_else(grepl("醫專", flag_person$bdegreen1),
          1,
          flag_person$err_bdegreen1)
flag_person$err_bdegreen1 <-
  if_else(grepl("語專", flag_person$bdegreen1),
          1,
          flag_person$err_bdegreen1)
flag_person$err_bdegreen1 <-
  if_else(grepl("university", flag_person$bdegreen1),
          1,
          flag_person$err_bdegreen1)
flag_person$err_bdegreen1 <-
  if_else(grepl("University", flag_person$bdegreen1),
          1,
          flag_person$err_bdegreen1)
flag_person$err_bdegreen1 <-
  if_else(grepl("UNIVERSITY", flag_person$bdegreen1),
          1,
          flag_person$err_bdegreen1)
flag_person$err_bdegreen1 <-
  if_else(grepl("college", flag_person$bdegreen1),
          1,
          flag_person$err_bdegreen1)
flag_person$err_bdegreen1 <-
  if_else(grepl("College", flag_person$bdegreen1),
          1,
          flag_person$err_bdegreen1)
flag_person$err_bdegreen1 <-
  if_else(grepl("COLLEGE", flag_person$bdegreen1),
          1,
          flag_person$err_bdegreen1)
flag_person$err_bdegreen1 <-
  if_else(grepl("系", flag_person$bdegreen1),
          1,
          flag_person$err_bdegreen1)
flag_person$err_bdegreen1 <-
  if_else(grepl("所", flag_person$bdegreen1),
          1,
          flag_person$err_bdegreen1)
flag_person$err_bdegreen1 <-
  if_else(grepl("班$", flag_person$bdegreen1),
          1,
          flag_person$err_bdegreen1)
flag_person$err_bdegreen1 <-
  if_else(grepl("不分科系", flag_person$bdegreen1),
          1,
          flag_person$err_bdegreen1)
flag_person$err_bdegreen1 <-
  if_else(grepl("不分系", flag_person$bdegreen1),
          1,
          flag_person$err_bdegreen1)
flag_person$err_bdegreen1 <-
  if_else(grepl("department", flag_person$bdegreen1),
          1,
          flag_person$err_bdegreen1)
flag_person$err_bdegreen1 <-
  if_else(grepl("Department", flag_person$bdegreen1),
          1,
          flag_person$err_bdegreen1)
flag_person$err_bdegreen1 <-
  if_else(grepl("DEPARTMENT", flag_person$bdegreen1),
          1,
          flag_person$err_bdegreen1)
flag_person$err_bdegreen1 <-
  if_else(grepl("^逕讀博士$", flag_person$bdegreen1),
          0,
          flag_person$err_bdegreen1)
flag_person$err_bdegreen1 <-
  if_else(grepl("^逕讀碩士$", flag_person$bdegreen1),
          0,
          flag_person$err_bdegreen1)
flag_person$err_bdegreen1 <-
  if_else(grepl("^逕行修讀碩士$", flag_person$bdegreen1),
          0,
          flag_person$err_bdegreen1)
flag_person$err_bdegreen1 <-
  if_else(grepl("兼課", flag_person$bdegreen1),
          1,
          flag_person$err_bdegreen1)
flag_person$err_bdegreen1 <-
  if_else(grepl("最高學歷", flag_person$bdegreen1),
          1,
          flag_person$err_bdegreen1)
flag_person$err_bdegreen1 <-
  if_else(grepl("^Y$", flag_person$bdegreen1),
          1,
          flag_person$err_bdegreen1)
flag_person$err_bdegreen1 <-
  if_else(grepl("^待查詢$", flag_person$bdegreen1),
          1,
          flag_person$err_bdegreen1)
flag_person$err_bdegreen1 <-
  if_else(grepl("^無$", flag_person$bdegreen1),
          1,
          flag_person$err_bdegreen1)
flag_person$err_bdegreen1 <-
  if_else(grepl("^外國$", flag_person$bdegreen1),
          1,
          flag_person$err_bdegreen1)
flag_person$err_bdegreen1 <-
  if_else(grepl("^國立$", flag_person$bdegreen1),
          1,
          flag_person$err_bdegreen1)
flag_person$err_bdegreen1 <-
  if_else(grepl("^歐洲$", flag_person$bdegreen1),
          1,
          flag_person$err_bdegreen1)
flag_person$err_bdegreen1 <-
  if_else(grepl("^亞洲$", flag_person$bdegreen1),
          1,
          flag_person$err_bdegreen1)
flag_person$err_bdegreen1 <-
  if_else(grepl("^美洲$", flag_person$bdegreen1),
          1,
          flag_person$err_bdegreen1)
flag_person$err_bdegreen1 <-
  if_else(grepl("^非洲$", flag_person$bdegreen1),
          1,
          flag_person$err_bdegreen1)
flag_person$err_bdegreen1 <-
  if_else(grepl("^大洋洲$", flag_person$bdegreen1),
          1,
          flag_person$err_bdegreen1)
flag_person$err_bdegreen1 <-
  if_else(grepl("肄業", flag_person$bdegreen1),
          1,
          flag_person$err_bdegreen1)
flag_person$err_bdegreen1 <-
  if_else(grepl("學分班", flag_person$bdegreen1),
          1,
          flag_person$err_bdegreen1)
flag_person$err_bdegreen1 <-
  if_else(grepl("結業", flag_person$bdegreen1),
          1,
          flag_person$err_bdegreen1)

#學士學位畢業學校（一）
flag_person$err_bdegreeu1 <- 1
flag_person$err_bdegreeu1 <-
  if_else(grepl("大學", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("分校", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("學院", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("師大", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("科大", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("教大", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("學校", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("官校", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("預校", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("書院", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("體院", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("師院", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("專科", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("藝專", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("海專", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("工專", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("護專", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("家專", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("商專", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("行專", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("農專", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("體專", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("師專", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("藥專", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("醫專", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("語專", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("士校", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("專校$", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("逕讀", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("音樂院$", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
#flag_person$err_bdegreeu1 <- if_else(grepl("音樂研究所$", flag_person$bdegreeu1), 0, flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("university", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("University", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("UNIVERSITY", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("Uni$", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("college", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("College", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("COLLEGE", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("Universidad", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("UNIVERSIDAD", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("Conservatory", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("CRD", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("ENM", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("CRC", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("EMMA", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("CRR", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("CNR", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("TheNewSchool", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("Hochschule", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(
    grepl("BergenSchoolofArchitecture", flag_person$bdegreeu1),
    0,
    flag_person$err_bdegreeu1
  )
flag_person$err_bdegreeu1 <-
  if_else(grepl("Universitat", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("StellenboschUni$", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("^TUDarmstadt$", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("大?$", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("^中興法商$", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("^N$", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("^待查詢$", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("^無$", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("離職", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("因故", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(
    grepl("VirginiaMilitaryInstitute", flag_person$bdegreeu1),
    0,
    flag_person$err_bdegreeu1
  )
flag_person$err_bdegreeu1 <-
  if_else(grepl("^LISAA$", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("^IstitutoSecoli$", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("^輔大$", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(
    grepl("^UnivstersityOFDelaware$", flag_person$bdegreeu1),
    0,
    flag_person$err_bdegreeu1
  )
flag_person$err_bdegreeu1 <-
  if_else(
    grepl("^UinvofCentralOklahoma$", flag_person$bdegreeu1),
    0,
    flag_person$err_bdegreeu1
  )
flag_person$err_bdegreeu1 <-
  if_else(grepl("^赫拉德茨克拉洛韋$", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(
    grepl(
      "^ConservatorioStatalediMilano“GiuseppeVerdi”Italia$",
      flag_person$bdegreeu1
    ),
    0,
    flag_person$err_bdegreeu1
  )
flag_person$err_bdegreeu1 <-
  if_else(grepl("^陸軍官校專科班$", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("中興法商", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("國立體院", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("逕獨碩士", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("UNISA", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("Univerity", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("山口?立大?", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(
    grepl(
      "ColumbiaBiblicalSeminaryandSchoolofMissions",
      flag_person$bdegreeu1
    ),
    0,
    flag_person$err_bdegreeu1
  )
flag_person$err_bdegreeu1 <-
  if_else(grepl("本國", flag_person$bdegreeu1),
          1,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("肄業", flag_person$bdegreeu1),
          1,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("學分班", flag_person$bdegreeu1),
          1,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("結業", flag_person$bdegreeu1),
          1,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("籌備處$", flag_person$bdegreeu1),
          1,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("^教育學院$", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("^高雄餐旅$", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("^台灣體大$", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("^國立空大$", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("^台灣體院$", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(
    grepl("^CityandGuildsofLondonArtSchool$",
          flag_person$bdegreeu1),
    0,
    flag_person$err_bdegreeu1
  )
flag_person$err_bdegreeu1 <-
  if_else(
    grepl("^BirminghamUiversity$", flag_person$bdegreeu1),
    0,
    flag_person$err_bdegreeu1
  )
flag_person$err_bdegreeu1 <-
  if_else(
    grepl("^NewJerseyInstituteofTechnology$",
          flag_person$bdegreeu1),
    0,
    flag_person$err_bdegreeu1
  )
flag_person$err_bdegreeu1 <-
  if_else(
    grepl(
      "^CollegeofEducationPotchefstroomSA$",
      flag_person$bdegreeu1
    ),
    0,
    flag_person$err_bdegreeu1
  )
flag_person$err_bdegreeu1 <-
  if_else(
    grepl("^?校法人????園?????????????西$", flag_person$bdegreeu1),
    0,
    flag_person$err_bdegreeu1
  )
flag_person$err_bdegreeu1 <-
  if_else(grepl("^同等學力$", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("^政戰正規班$", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("^日本國立埼玉大學$", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("^日本國立埼玉大學教養學部$", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("^日本國立奈良教育大學（學院）$", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("^日本國立熊本大學$", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(
    grepl(
      "^ConservatoriostatalediMilano“GiuseppeVerdi”Italia$",
      flag_person$bdegreeu1
    ),
    0,
    flag_person$err_bdegreeu1
  )
flag_person$err_bdegreeu1 <-
  if_else(grepl("所", flag_person$bdegreeu1),
          1,
          flag_person$err_bdegreeu1)
flag_person$err_bdegreeu1 <-
  if_else(grepl("逕行修讀", flag_person$bdegreeu1),
          0,
          flag_person$err_bdegreeu1)

#學士學位畢業系所（一）
flag_person$err_bdegreeg1 <- 0
flag_person$err_bdegreeg1 <-
  if_else(grepl("博士", flag_person$bdegreeg1),
          1,
          flag_person$err_bdegreeg1)
flag_person$err_bdegreeg1 <-
  if_else(grepl("碩士", flag_person$bdegreeg1),
          1,
          flag_person$err_bdegreeg1)
flag_person$err_bdegreeg1 <-
  if_else(grepl("^逕讀碩士$", flag_person$bdegreeg1),
          0,
          flag_person$err_bdegreeg1)
flag_person$err_bdegreeg1 <-
  if_else(grepl("^學士$", flag_person$bdegreeg1),
          1,
          flag_person$err_bdegreeg1)
flag_person$err_bdegreeg1 <-
  if_else(grepl("副學士", flag_person$bdegreeg1),
          1,
          flag_person$err_bdegreeg1)
flag_person$err_bdegreeg1 <-
  if_else(grepl("大學", flag_person$bdegreeg1),
          1,
          flag_person$err_bdegreeg1)
flag_person$err_bdegreeg1 <-
  if_else(grepl("分校", flag_person$bdegreeg1),
          1,
          flag_person$err_bdegreeg1)
flag_person$err_bdegreeg1 <-
  if_else(grepl("^學院$", flag_person$bdegreeg1),
          1,
          flag_person$err_bdegreeg1)
flag_person$err_bdegreeg1 <-
  if_else(grepl("科大", flag_person$bdegreeg1),
          1,
          flag_person$err_bdegreeg1)
flag_person$err_bdegreeg1 <-
  if_else(grepl("學校", flag_person$bdegreeg1),
          1,
          flag_person$err_bdegreeg1)
flag_person$err_bdegreeg1 <-
  if_else(grepl("官校", flag_person$bdegreeg1),
          1,
          flag_person$err_bdegreeg1)
flag_person$err_bdegreeg1 <-
  if_else(grepl("預校", flag_person$bdegreeg1),
          1,
          flag_person$err_bdegreeg1)
flag_person$err_bdegreeg1 <-
  if_else(grepl("書院", flag_person$bdegreeg1),
          1,
          flag_person$err_bdegreeg1)
flag_person$err_bdegreeg1 <-
  if_else(grepl("專科", flag_person$bdegreeg1),
          1,
          flag_person$err_bdegreeg1)
flag_person$err_bdegreeg1 <-
  if_else(grepl("藝專", flag_person$bdegreeg1),
          1,
          flag_person$err_bdegreeg1)
flag_person$err_bdegreeg1 <-
  if_else(grepl("海專", flag_person$bdegreeg1),
          1,
          flag_person$err_bdegreeg1)
flag_person$err_bdegreeg1 <-
  if_else(grepl("工專", flag_person$bdegreeg1),
          1,
          flag_person$err_bdegreeg1)
flag_person$err_bdegreeg1 <-
  if_else(grepl("護專", flag_person$bdegreeg1),
          1,
          flag_person$err_bdegreeg1)
flag_person$err_bdegreeg1 <-
  if_else(grepl("家專", flag_person$bdegreeg1),
          1,
          flag_person$err_bdegreeg1)
flag_person$err_bdegreeg1 <-
  if_else(grepl("商專", flag_person$bdegreeg1),
          1,
          flag_person$err_bdegreeg1)
flag_person$err_bdegreeg1 <-
  if_else(grepl("行專", flag_person$bdegreeg1),
          1,
          flag_person$err_bdegreeg1)
flag_person$err_bdegreeg1 <-
  if_else(grepl("農專", flag_person$bdegreeg1),
          1,
          flag_person$err_bdegreeg1)
flag_person$err_bdegreeg1 <-
  if_else(grepl("體專", flag_person$bdegreeg1),
          1,
          flag_person$err_bdegreeg1)
flag_person$err_bdegreeg1 <-
  if_else(grepl("藥專", flag_person$bdegreeg1),
          1,
          flag_person$err_bdegreeg1)
flag_person$err_bdegreeg1 <-
  if_else(grepl("師專", flag_person$bdegreeg1),
          1,
          flag_person$err_bdegreeg1)
flag_person$err_bdegreeg1 <-
  if_else(grepl("醫專", flag_person$bdegreeg1),
          1,
          flag_person$err_bdegreeg1)
flag_person$err_bdegreeg1 <-
  if_else(grepl("語專", flag_person$bdegreeg1),
          1,
          flag_person$err_bdegreeg1)
flag_person$err_bdegreeg1 <-
  if_else(grepl("university", flag_person$bdegreeg1),
          1,
          flag_person$err_bdegreeg1)
flag_person$err_bdegreeg1 <-
  if_else(grepl("University", flag_person$bdegreeg1),
          1,
          flag_person$err_bdegreeg1)
flag_person$err_bdegreeg1 <-
  if_else(grepl("UNIVERSITY", flag_person$bdegreeg1),
          1,
          flag_person$err_bdegreeg1)
flag_person$err_bdegreeg1 <-
  if_else(grepl("college", flag_person$bdegreeg1),
          1,
          flag_person$err_bdegreeg1)
flag_person$err_bdegreeg1 <-
  if_else(grepl("College", flag_person$bdegreeg1),
          1,
          flag_person$err_bdegreeg1)
flag_person$err_bdegreeg1 <-
  if_else(grepl("COLLEGE", flag_person$bdegreeg1),
          1,
          flag_person$err_bdegreeg1)
flag_person$err_bdegreeg1 <-
  if_else(grepl("兼課", flag_person$bdegreeg1),
          1,
          flag_person$err_bdegreeg1)
flag_person$err_bdegreeg1 <-
  if_else(grepl("最高學歷", flag_person$bdegreeg1),
          1,
          flag_person$err_bdegreeg1)
flag_person$err_bdegreeg1 <-
  if_else(grepl("^Y$", flag_person$bdegreeg1),
          1,
          flag_person$err_bdegreeg1)
flag_person$err_bdegreeg1 <-
  if_else(grepl("^待查詢$", flag_person$bdegreeg1),
          1,
          flag_person$err_bdegreeg1)
flag_person$err_bdegreeg1 <-
  if_else(grepl("^無$", flag_person$bdegreeg1),
          1,
          flag_person$err_bdegreeg1)
flag_person$err_bdegreeg1 <-
  if_else(grepl("^逕獨碩士$", flag_person$bdegreeg1),
          0,
          flag_person$err_bdegreeg1)
flag_person$err_bdegreeg1 <-
  if_else(grepl("^逕行修讀碩士$", flag_person$bdegreeg1),
          0,
          flag_person$err_bdegreeg1)
flag_person$err_bdegreeg1 <-
  if_else(grepl("肄業", flag_person$bdegreeg1),
          1,
          flag_person$err_bdegreeg1)
flag_person$err_bdegreeg1 <-
  if_else(grepl("學分班", flag_person$bdegreeg1),
          1,
          flag_person$err_bdegreeg1)
flag_person$err_bdegreeg1 <-
  if_else(grepl("結業", flag_person$bdegreeg1),
          1,
          flag_person$err_bdegreeg1)
flag_person$err_bdegreeg1 <-
  if_else(grepl("行政$", flag_person$bdegreeg1),
          1,
          flag_person$err_bdegreeg1)
flag_person$err_bdegreeg1 <-
  if_else(grepl("所", flag_person$bdegreeg1),
          1,
          flag_person$err_bdegreeg1)
flag_person$err_bdegreeg1 <-
  if_else(grepl("^逕讀碩士$", flag_person$bdegreeg1),
          0,
          flag_person$err_bdegreeg1)

#學士學位畢業學校國別（二）
flag_person$err_bdegreen2 <- 0
flag_person$err_bdegreen2 <-
  if_else(grepl("博士", flag_person$bdegreen2),
          1,
          flag_person$err_bdegreen2)
flag_person$err_bdegreen2 <-
  if_else(grepl("碩士", flag_person$bdegreen2),
          1,
          flag_person$err_bdegreen2)
flag_person$err_bdegreen2 <-
  if_else(grepl("學士", flag_person$bdegreen2),
          1,
          flag_person$err_bdegreen2)
flag_person$err_bdegreen2 <-
  if_else(grepl("副學士", flag_person$bdegreen2),
          1,
          flag_person$err_bdegreen2)
flag_person$err_bdegreen2 <-
  if_else(grepl("大學", flag_person$bdegreen2),
          1,
          flag_person$err_bdegreen2)
flag_person$err_bdegreen2 <-
  if_else(grepl("分校", flag_person$bdegreen2),
          1,
          flag_person$err_bdegreen2)
flag_person$err_bdegreen2 <-
  if_else(grepl("學院", flag_person$bdegreen2),
          1,
          flag_person$err_bdegreen2)
flag_person$err_bdegreen2 <-
  if_else(grepl("科大", flag_person$bdegreen2),
          1,
          flag_person$err_bdegreen2)
flag_person$err_bdegreen2 <-
  if_else(grepl("學校", flag_person$bdegreen2),
          1,
          flag_person$err_bdegreen2)
flag_person$err_bdegreen2 <-
  if_else(grepl("官校", flag_person$bdegreen2),
          1,
          flag_person$err_bdegreen2)
flag_person$err_bdegreen2 <-
  if_else(grepl("預校", flag_person$bdegreen2),
          1,
          flag_person$err_bdegreen2)
flag_person$err_bdegreen2 <-
  if_else(grepl("書院", flag_person$bdegreen2),
          1,
          flag_person$err_bdegreen2)
flag_person$err_bdegreen2 <-
  if_else(grepl("專科", flag_person$bdegreen2),
          1,
          flag_person$err_bdegreen2)
flag_person$err_bdegreen2 <-
  if_else(grepl("藝專", flag_person$bdegreen2),
          1,
          flag_person$err_bdegreen2)
flag_person$err_bdegreen2 <-
  if_else(grepl("海專", flag_person$bdegreen2),
          1,
          flag_person$err_bdegreen2)
flag_person$err_bdegreen2 <-
  if_else(grepl("工專", flag_person$bdegreen2),
          1,
          flag_person$err_bdegreen2)
flag_person$err_bdegreen2 <-
  if_else(grepl("護專", flag_person$bdegreen2),
          1,
          flag_person$err_bdegreen2)
flag_person$err_bdegreen2 <-
  if_else(grepl("家專", flag_person$bdegreen2),
          1,
          flag_person$err_bdegreen2)
flag_person$err_bdegreen2 <-
  if_else(grepl("商專", flag_person$bdegreen2),
          1,
          flag_person$err_bdegreen2)
flag_person$err_bdegreen2 <-
  if_else(grepl("行專", flag_person$bdegreen2),
          1,
          flag_person$err_bdegreen2)
flag_person$err_bdegreen2 <-
  if_else(grepl("農專", flag_person$bdegreen2),
          1,
          flag_person$err_bdegreen2)
flag_person$err_bdegreen2 <-
  if_else(grepl("體專", flag_person$bdegreen2),
          1,
          flag_person$err_bdegreen2)
flag_person$err_bdegreen2 <-
  if_else(grepl("藥專", flag_person$bdegreen2),
          1,
          flag_person$err_bdegreen2)
flag_person$err_bdegreen2 <-
  if_else(grepl("師專", flag_person$bdegreen2),
          1,
          flag_person$err_bdegreen2)
flag_person$err_bdegreen2 <-
  if_else(grepl("醫專", flag_person$bdegreen2),
          1,
          flag_person$err_bdegreen2)
flag_person$err_bdegreen2 <-
  if_else(grepl("語專", flag_person$bdegreen2),
          1,
          flag_person$err_bdegreen2)
flag_person$err_bdegreen2 <-
  if_else(grepl("university", flag_person$bdegreen2),
          1,
          flag_person$err_bdegreen2)
flag_person$err_bdegreen2 <-
  if_else(grepl("University", flag_person$bdegreen2),
          1,
          flag_person$err_bdegreen2)
flag_person$err_bdegreen2 <-
  if_else(grepl("UNIVERSITY", flag_person$bdegreen2),
          1,
          flag_person$err_bdegreen2)
flag_person$err_bdegreen2 <-
  if_else(grepl("college", flag_person$bdegreen2),
          1,
          flag_person$err_bdegreen2)
flag_person$err_bdegreen2 <-
  if_else(grepl("College", flag_person$bdegreen2),
          1,
          flag_person$err_bdegreen2)
flag_person$err_bdegreen2 <-
  if_else(grepl("COLLEGE", flag_person$bdegreen2),
          1,
          flag_person$err_bdegreen2)
flag_person$err_bdegreen2 <-
  if_else(grepl("系", flag_person$bdegreen2),
          1,
          flag_person$err_bdegreen2)
flag_person$err_bdegreen2 <-
  if_else(grepl("所", flag_person$bdegreen2),
          1,
          flag_person$err_bdegreen2)
flag_person$err_bdegreen2 <-
  if_else(grepl("班$", flag_person$bdegreen2),
          1,
          flag_person$err_bdegreen2)
flag_person$err_bdegreen2 <-
  if_else(grepl("不分科系", flag_person$bdegreen2),
          1,
          flag_person$err_bdegreen2)
flag_person$err_bdegreen2 <-
  if_else(grepl("不分系", flag_person$bdegreen2),
          1,
          flag_person$err_bdegreen2)
flag_person$err_bdegreen2 <-
  if_else(grepl("department", flag_person$bdegreen2),
          1,
          flag_person$err_bdegreen2)
flag_person$err_bdegreen2 <-
  if_else(grepl("Department", flag_person$bdegreen2),
          1,
          flag_person$err_bdegreen2)
flag_person$err_bdegreen2 <-
  if_else(grepl("DEPARTMENT", flag_person$bdegreen2),
          1,
          flag_person$err_bdegreen2)
flag_person$err_bdegreen2 <-
  if_else(grepl("^逕讀博士$", flag_person$bdegreen2),
          0,
          flag_person$err_bdegreen2)
flag_person$err_bdegreen2 <-
  if_else(grepl("^逕讀碩士$", flag_person$bdegreen2),
          0,
          flag_person$err_bdegreen2)
flag_person$err_bdegreen2 <-
  if_else(grepl("兼課", flag_person$bdegreen2),
          1,
          flag_person$err_bdegreen2)
flag_person$err_bdegreen2 <-
  if_else(grepl("最高學歷", flag_person$bdegreen2),
          1,
          flag_person$err_bdegreen2)
flag_person$err_bdegreen2 <-
  if_else(grepl("^Y$", flag_person$bdegreen2),
          1,
          flag_person$err_bdegreen2)
flag_person$err_bdegreen2 <-
  if_else(grepl("^待查詢$", flag_person$bdegreen2),
          1,
          flag_person$err_bdegreen2)
flag_person$err_bdegreen2 <-
  if_else(grepl("^無$", flag_person$bdegreen2),
          1,
          flag_person$err_bdegreen2)
flag_person$err_bdegreen2 <-
  if_else(grepl("^外國$", flag_person$bdegreen2),
          1,
          flag_person$err_bdegreen2)
flag_person$err_bdegreen2 <-
  if_else(grepl("^國立$", flag_person$bdegreen2),
          1,
          flag_person$err_bdegreen2)
flag_person$err_bdegreen2 <-
  if_else(grepl("^歐洲$", flag_person$bdegreen2),
          1,
          flag_person$err_bdegreen2)
flag_person$err_bdegreen2 <-
  if_else(grepl("^亞洲$", flag_person$bdegreen2),
          1,
          flag_person$err_bdegreen2)
flag_person$err_bdegreen2 <-
  if_else(grepl("^美洲$", flag_person$bdegreen2),
          1,
          flag_person$err_bdegreen2)
flag_person$err_bdegreen2 <-
  if_else(grepl("^非洲$", flag_person$bdegreen2),
          1,
          flag_person$err_bdegreen2)
flag_person$err_bdegreen2 <-
  if_else(grepl("^大洋洲$", flag_person$bdegreen2),
          1,
          flag_person$err_bdegreen2)
flag_person$err_bdegreen2 <-
  if_else(grepl("肄業", flag_person$bdegreen2),
          1,
          flag_person$err_bdegreen2)
flag_person$err_bdegreen2 <-
  if_else(grepl("學分班", flag_person$bdegreen2),
          1,
          flag_person$err_bdegreen2)
flag_person$err_bdegreen2 <-
  if_else(grepl("結業", flag_person$bdegreen2),
          1,
          flag_person$err_bdegreen2)

#學士學位畢業學校（二）
flag_person$err_bdegreeu2 <- 1
flag_person$err_bdegreeu2 <-
  if_else(grepl("大學", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("分校", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("學院", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("師大", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("科大", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("教大", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("學校", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("官校", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("預校", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("書院", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("體院", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("師院", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("專科", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("藝專", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("海專", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("工專", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("護專", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("家專", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("商專", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("行專", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("農專", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("體專", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("師專", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("藥專", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("醫專", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("語專", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("士校", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("專校$", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("逕讀", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("音樂院$", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
#flag_person$err_bdegreeu2 <- if_else(grepl("音樂研究所$", flag_person$bdegreeu2), 0, flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("university", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("University", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("UNIVERSITY", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("Uni$", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("college", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("College", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("COLLEGE", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("Universidad", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("UNIVERSIDAD", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("Conservatory", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("CRD", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("ENM", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("CRC", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("EMMA", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("CRR", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("CNR", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("TheNewSchool", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("Hochschule", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(
    grepl("BergenSchoolofArchitecture", flag_person$bdegreeu2),
    0,
    flag_person$err_bdegreeu2
  )
flag_person$err_bdegreeu2 <-
  if_else(grepl("Universitat", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("StellenboschUni$", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("^TUDarmstadt$", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("大?$", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("^中興法商$", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("^N$", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("^待查詢$", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("^無$", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("離職", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("因故", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(
    grepl("VirginiaMilitaryInstitute", flag_person$bdegreeu2),
    0,
    flag_person$err_bdegreeu2
  )
flag_person$err_bdegreeu2 <-
  if_else(grepl("^LISAA$", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("^IstitutoSecoli$", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("^輔大$", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(
    grepl("^UnivstersityOFDelaware$", flag_person$bdegreeu2),
    0,
    flag_person$err_bdegreeu2
  )
flag_person$err_bdegreeu2 <-
  if_else(
    grepl("^UinvofCentralOklahoma$", flag_person$bdegreeu2),
    0,
    flag_person$err_bdegreeu2
  )
flag_person$err_bdegreeu2 <-
  if_else(grepl("^赫拉德茨克拉洛韋$", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(
    grepl(
      "^ConservatorioStatalediMilano“GiuseppeVerdi”Italia$",
      flag_person$bdegreeu2
    ),
    0,
    flag_person$err_bdegreeu2
  )
flag_person$err_bdegreeu2 <-
  if_else(grepl("^陸軍官校專科班$", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("中興法商", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("國立體院", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("逕獨碩士", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("UNISA", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("Univerity", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("山口?立大?", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(
    grepl(
      "ColumbiaBiblicalSeminaryandSchoolofMissions",
      flag_person$bdegreeu2
    ),
    0,
    flag_person$err_bdegreeu2
  )
flag_person$err_bdegreeu2 <-
  if_else(grepl("本國", flag_person$bdegreeu2),
          1,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("肄業", flag_person$bdegreeu2),
          1,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("學分班", flag_person$bdegreeu2),
          1,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("結業", flag_person$bdegreeu2),
          1,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("籌備處$", flag_person$bdegreeu2),
          1,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("^教育學院$", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("^高雄餐旅$", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("^台灣體大$", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("^國立空大$", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("^台灣體院$", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(
    grepl("^CityandGuildsofLondonArtSchool$",
          flag_person$bdegreeu2),
    0,
    flag_person$err_bdegreeu2
  )
flag_person$err_bdegreeu2 <-
  if_else(
    grepl("^BirminghamUiversity$", flag_person$bdegreeu2),
    0,
    flag_person$err_bdegreeu2
  )
flag_person$err_bdegreeu2 <-
  if_else(
    grepl("^NewJerseyInstituteofTechnology$",
          flag_person$bdegreeu2),
    0,
    flag_person$err_bdegreeu2
  )
flag_person$err_bdegreeu2 <-
  if_else(
    grepl(
      "^CollegeofEducationPotchefstroomSA$",
      flag_person$bdegreeu2
    ),
    0,
    flag_person$err_bdegreeu2
  )
flag_person$err_bdegreeu2 <-
  if_else(
    grepl("^?校法人????園?????????????西$", flag_person$bdegreeu2),
    0,
    flag_person$err_bdegreeu2
  )
flag_person$err_bdegreeu2 <-
  if_else(grepl("^同等學力$", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("^政戰正規班$", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("^日本國立埼玉大學$", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("^日本國立埼玉大學教養學部$", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("^日本國立奈良教育大學（學院）$", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(grepl("^日本國立熊本大學$", flag_person$bdegreeu2),
          0,
          flag_person$err_bdegreeu2)
flag_person$err_bdegreeu2 <-
  if_else(
    grepl(
      "^ConservatoriostatalediMilano“GiuseppeVerdi”Italia$",
      flag_person$bdegreeu2
    ),
    0,
    flag_person$err_bdegreeu2
  )
flag_person$err_bdegreeu2 <-
  if_else(grepl("所", flag_person$bdegreeu2),
          1,
          flag_person$err_bdegreeu2)

#學士學位畢業系所（二）
flag_person$err_bdegreeg2 <- 0
flag_person$err_bdegreeg2 <-
  if_else(grepl("博士", flag_person$bdegreeg2),
          1,
          flag_person$err_bdegreeg2)
flag_person$err_bdegreeg2 <-
  if_else(grepl("碩士", flag_person$bdegreeg2),
          1,
          flag_person$err_bdegreeg2)
flag_person$err_bdegreeg2 <-
  if_else(grepl("^逕讀碩士$", flag_person$bdegreeg2),
          0,
          flag_person$err_bdegreeg2)
flag_person$err_bdegreeg2 <-
  if_else(grepl("^學士$", flag_person$bdegreeg2),
          1,
          flag_person$err_bdegreeg2)
flag_person$err_bdegreeg2 <-
  if_else(grepl("副學士", flag_person$bdegreeg2),
          1,
          flag_person$err_bdegreeg2)
flag_person$err_bdegreeg2 <-
  if_else(grepl("大學", flag_person$bdegreeg2),
          1,
          flag_person$err_bdegreeg2)
flag_person$err_bdegreeg2 <-
  if_else(grepl("分校", flag_person$bdegreeg2),
          1,
          flag_person$err_bdegreeg2)
flag_person$err_bdegreeg2 <-
  if_else(grepl("^學院$", flag_person$bdegreeg2),
          1,
          flag_person$err_bdegreeg2)
flag_person$err_bdegreeg2 <-
  if_else(grepl("科大", flag_person$bdegreeg2),
          1,
          flag_person$err_bdegreeg2)
flag_person$err_bdegreeg2 <-
  if_else(grepl("學校", flag_person$bdegreeg2),
          1,
          flag_person$err_bdegreeg2)
flag_person$err_bdegreeg2 <-
  if_else(grepl("官校", flag_person$bdegreeg2),
          1,
          flag_person$err_bdegreeg2)
flag_person$err_bdegreeg2 <-
  if_else(grepl("預校", flag_person$bdegreeg2),
          1,
          flag_person$err_bdegreeg2)
flag_person$err_bdegreeg2 <-
  if_else(grepl("書院", flag_person$bdegreeg2),
          1,
          flag_person$err_bdegreeg2)
flag_person$err_bdegreeg2 <-
  if_else(grepl("專科", flag_person$bdegreeg2),
          1,
          flag_person$err_bdegreeg2)
flag_person$err_bdegreeg2 <-
  if_else(grepl("藝專", flag_person$bdegreeg2),
          1,
          flag_person$err_bdegreeg2)
flag_person$err_bdegreeg2 <-
  if_else(grepl("海專", flag_person$bdegreeg2),
          1,
          flag_person$err_bdegreeg2)
flag_person$err_bdegreeg2 <-
  if_else(grepl("工專", flag_person$bdegreeg2),
          1,
          flag_person$err_bdegreeg2)
flag_person$err_bdegreeg2 <-
  if_else(grepl("護專", flag_person$bdegreeg2),
          1,
          flag_person$err_bdegreeg2)
flag_person$err_bdegreeg2 <-
  if_else(grepl("家專", flag_person$bdegreeg2),
          1,
          flag_person$err_bdegreeg2)
flag_person$err_bdegreeg2 <-
  if_else(grepl("商專", flag_person$bdegreeg2),
          1,
          flag_person$err_bdegreeg2)
flag_person$err_bdegreeg2 <-
  if_else(grepl("行專", flag_person$bdegreeg2),
          1,
          flag_person$err_bdegreeg2)
flag_person$err_bdegreeg2 <-
  if_else(grepl("農專", flag_person$bdegreeg2),
          1,
          flag_person$err_bdegreeg2)
flag_person$err_bdegreeg2 <-
  if_else(grepl("體專", flag_person$bdegreeg2),
          1,
          flag_person$err_bdegreeg2)
flag_person$err_bdegreeg2 <-
  if_else(grepl("藥專", flag_person$bdegreeg2),
          1,
          flag_person$err_bdegreeg2)
flag_person$err_bdegreeg2 <-
  if_else(grepl("師專", flag_person$bdegreeg2),
          1,
          flag_person$err_bdegreeg2)
flag_person$err_bdegreeg2 <-
  if_else(grepl("醫專", flag_person$bdegreeg2),
          1,
          flag_person$err_bdegreeg2)
flag_person$err_bdegreeg2 <-
  if_else(grepl("語專", flag_person$bdegreeg2),
          1,
          flag_person$err_bdegreeg2)
flag_person$err_bdegreeg2 <-
  if_else(grepl("university", flag_person$bdegreeg2),
          1,
          flag_person$err_bdegreeg2)
flag_person$err_bdegreeg2 <-
  if_else(grepl("University", flag_person$bdegreeg2),
          1,
          flag_person$err_bdegreeg2)
flag_person$err_bdegreeg2 <-
  if_else(grepl("UNIVERSITY", flag_person$bdegreeg2),
          1,
          flag_person$err_bdegreeg2)
flag_person$err_bdegreeg2 <-
  if_else(grepl("college", flag_person$bdegreeg2),
          1,
          flag_person$err_bdegreeg2)
flag_person$err_bdegreeg2 <-
  if_else(grepl("College", flag_person$bdegreeg2),
          1,
          flag_person$err_bdegreeg2)
flag_person$err_bdegreeg2 <-
  if_else(grepl("COLLEGE", flag_person$bdegreeg2),
          1,
          flag_person$err_bdegreeg2)
flag_person$err_bdegreeg2 <-
  if_else(grepl("兼課", flag_person$bdegreeg2),
          1,
          flag_person$err_bdegreeg2)
flag_person$err_bdegreeg2 <-
  if_else(grepl("最高學歷", flag_person$bdegreeg2),
          1,
          flag_person$err_bdegreeg2)
flag_person$err_bdegreeg2 <-
  if_else(grepl("^Y$", flag_person$bdegreeg2),
          1,
          flag_person$err_bdegreeg2)
flag_person$err_bdegreeg2 <-
  if_else(grepl("^待查詢$", flag_person$bdegreeg2),
          1,
          flag_person$err_bdegreeg2)
flag_person$err_bdegreeg2 <-
  if_else(grepl("^無$", flag_person$bdegreeg2),
          1,
          flag_person$err_bdegreeg2)
flag_person$err_bdegreeg2 <-
  if_else(grepl("^逕獨碩士$", flag_person$bdegreeg2),
          0,
          flag_person$err_bdegreeg2)
flag_person$err_bdegreeg2 <-
  if_else(grepl("^逕行修讀碩士$", flag_person$bdegreeg2),
          0,
          flag_person$err_bdegreeg2)
flag_person$err_bdegreeg2 <-
  if_else(grepl("肄業", flag_person$bdegreeg2),
          1,
          flag_person$err_bdegreeg2)
flag_person$err_bdegreeg2 <-
  if_else(grepl("學分班", flag_person$bdegreeg2),
          1,
          flag_person$err_bdegreeg2)
flag_person$err_bdegreeg2 <-
  if_else(grepl("結業", flag_person$bdegreeg2),
          1,
          flag_person$err_bdegreeg2)
flag_person$err_bdegreeg2 <-
  if_else(grepl("行政$", flag_person$bdegreeg2),
          1,
          flag_person$err_bdegreeg2)
flag_person$err_bdegreeg2 <-
  if_else(grepl("所", flag_person$bdegreeg2),
          1,
          flag_person$err_bdegreeg2)
flag_person$err_bdegreeg2 <-
  if_else(grepl("^逕讀碩士$", flag_person$bdegreeg2),
          0,
          flag_person$err_bdegreeg2)

#副學士學位畢業學校國別（一）
flag_person$err_adegreen1 <- 0
flag_person$err_adegreen1 <-
  if_else(grepl("博士", flag_person$adegreen1),
          1,
          flag_person$err_adegreen1)
flag_person$err_adegreen1 <-
  if_else(grepl("碩士", flag_person$adegreen1),
          1,
          flag_person$err_adegreen1)
flag_person$err_adegreen1 <-
  if_else(grepl("學士", flag_person$adegreen1),
          1,
          flag_person$err_adegreen1)
flag_person$err_adegreen1 <-
  if_else(grepl("副學士", flag_person$adegreen1),
          1,
          flag_person$err_adegreen1)
flag_person$err_adegreen1 <-
  if_else(grepl("大學", flag_person$adegreen1),
          1,
          flag_person$err_adegreen1)
flag_person$err_adegreen1 <-
  if_else(grepl("分校", flag_person$adegreen1),
          1,
          flag_person$err_adegreen1)
flag_person$err_adegreen1 <-
  if_else(grepl("學院", flag_person$adegreen1),
          1,
          flag_person$err_adegreen1)
flag_person$err_adegreen1 <-
  if_else(grepl("科大", flag_person$adegreen1),
          1,
          flag_person$err_adegreen1)
flag_person$err_adegreen1 <-
  if_else(grepl("學校", flag_person$adegreen1),
          1,
          flag_person$err_adegreen1)
flag_person$err_adegreen1 <-
  if_else(grepl("官校", flag_person$adegreen1),
          1,
          flag_person$err_adegreen1)
flag_person$err_adegreen1 <-
  if_else(grepl("預校", flag_person$adegreen1),
          1,
          flag_person$err_adegreen1)
flag_person$err_adegreen1 <-
  if_else(grepl("書院", flag_person$adegreen1),
          1,
          flag_person$err_adegreen1)
flag_person$err_adegreen1 <-
  if_else(grepl("專科", flag_person$adegreen1),
          1,
          flag_person$err_adegreen1)
flag_person$err_adegreen1 <-
  if_else(grepl("藝專", flag_person$adegreen1),
          1,
          flag_person$err_adegreen1)
flag_person$err_adegreen1 <-
  if_else(grepl("海專", flag_person$adegreen1),
          1,
          flag_person$err_adegreen1)
flag_person$err_adegreen1 <-
  if_else(grepl("工專", flag_person$adegreen1),
          1,
          flag_person$err_adegreen1)
flag_person$err_adegreen1 <-
  if_else(grepl("護專", flag_person$adegreen1),
          1,
          flag_person$err_adegreen1)
flag_person$err_adegreen1 <-
  if_else(grepl("家專", flag_person$adegreen1),
          1,
          flag_person$err_adegreen1)
flag_person$err_adegreen1 <-
  if_else(grepl("商專", flag_person$adegreen1),
          1,
          flag_person$err_adegreen1)
flag_person$err_adegreen1 <-
  if_else(grepl("行專", flag_person$adegreen1),
          1,
          flag_person$err_adegreen1)
flag_person$err_adegreen1 <-
  if_else(grepl("農專", flag_person$adegreen1),
          1,
          flag_person$err_adegreen1)
flag_person$err_adegreen1 <-
  if_else(grepl("體專", flag_person$adegreen1),
          1,
          flag_person$err_adegreen1)
flag_person$err_adegreen1 <-
  if_else(grepl("藥專", flag_person$adegreen1),
          1,
          flag_person$err_adegreen1)
flag_person$err_adegreen1 <-
  if_else(grepl("師專", flag_person$adegreen1),
          1,
          flag_person$err_adegreen1)
flag_person$err_adegreen1 <-
  if_else(grepl("醫專", flag_person$adegreen1),
          1,
          flag_person$err_adegreen1)
flag_person$err_adegreen1 <-
  if_else(grepl("語專", flag_person$adegreen1),
          1,
          flag_person$err_adegreen1)
flag_person$err_adegreen1 <-
  if_else(grepl("企專", flag_person$adegreen1),
          1,
          flag_person$err_adegreen1)
flag_person$err_adegreen1 <-
  if_else(grepl("university", flag_person$adegreen1),
          1,
          flag_person$err_adegreen1)
flag_person$err_adegreen1 <-
  if_else(grepl("University", flag_person$adegreen1),
          1,
          flag_person$err_adegreen1)
flag_person$err_adegreen1 <-
  if_else(grepl("UNIVERSITY", flag_person$adegreen1),
          1,
          flag_person$err_adegreen1)
flag_person$err_adegreen1 <-
  if_else(grepl("college", flag_person$adegreen1),
          1,
          flag_person$err_adegreen1)
flag_person$err_adegreen1 <-
  if_else(grepl("College", flag_person$adegreen1),
          1,
          flag_person$err_adegreen1)
flag_person$err_adegreen1 <-
  if_else(grepl("COLLEGE", flag_person$adegreen1),
          1,
          flag_person$err_adegreen1)
flag_person$err_adegreen1 <-
  if_else(grepl("系", flag_person$adegreen1),
          1,
          flag_person$err_adegreen1)
flag_person$err_adegreen1 <-
  if_else(grepl("所", flag_person$adegreen1),
          1,
          flag_person$err_adegreen1)
flag_person$err_adegreen1 <-
  if_else(grepl("班$", flag_person$adegreen1),
          1,
          flag_person$err_adegreen1)
flag_person$err_adegreen1 <-
  if_else(grepl("不分科系", flag_person$adegreen1),
          1,
          flag_person$err_adegreen1)
flag_person$err_adegreen1 <-
  if_else(grepl("不分系", flag_person$adegreen1),
          1,
          flag_person$err_adegreen1)
flag_person$err_adegreen1 <-
  if_else(grepl("department", flag_person$adegreen1),
          1,
          flag_person$err_adegreen1)
flag_person$err_adegreen1 <-
  if_else(grepl("Department", flag_person$adegreen1),
          1,
          flag_person$err_adegreen1)
flag_person$err_adegreen1 <-
  if_else(grepl("DEPARTMENT", flag_person$adegreen1),
          1,
          flag_person$err_adegreen1)
flag_person$err_adegreen1 <-
  if_else(grepl("兼課", flag_person$adegreen1),
          1,
          flag_person$err_adegreen1)
flag_person$err_adegreen1 <-
  if_else(grepl("最高學歷", flag_person$adegreen1),
          1,
          flag_person$err_adegreen1)
flag_person$err_adegreen1 <-
  if_else(grepl("逕讀", flag_person$adegreen1),
          1,
          flag_person$err_adegreen1)
flag_person$err_adegreen1 <-
  if_else(grepl("^Y$", flag_person$adegreen1),
          1,
          flag_person$err_adegreen1)
flag_person$err_adegreen1 <-
  if_else(grepl("^待查詢$", flag_person$adegreen1),
          1,
          flag_person$err_adegreen1)
flag_person$err_adegreen1 <-
  if_else(grepl("^無$", flag_person$adegreen1),
          1,
          flag_person$err_adegreen1)
flag_person$err_adegreen1 <-
  if_else(grepl("^外國$", flag_person$adegreen1),
          1,
          flag_person$err_adegreen1)
flag_person$err_adegreen1 <-
  if_else(grepl("^國立$", flag_person$adegreen1),
          1,
          flag_person$err_adegreen1)
flag_person$err_adegreen1 <-
  if_else(grepl("^歐洲$", flag_person$adegreen1),
          1,
          flag_person$err_adegreen1)
flag_person$err_adegreen1 <-
  if_else(grepl("^亞洲$", flag_person$adegreen1),
          1,
          flag_person$err_adegreen1)
flag_person$err_adegreen1 <-
  if_else(grepl("^同等學力$", flag_person$adegreen1),
          0,
          flag_person$err_adegreen1)
flag_person$err_adegreen1 <-
  if_else(grepl("肄業", flag_person$adegreen1),
          1,
          flag_person$err_adegreen1)
flag_person$err_adegreen1 <-
  if_else(grepl("學分班", flag_person$adegreen1),
          1,
          flag_person$err_adegreen1)
flag_person$err_adegreen1 <-
  if_else(grepl("結業", flag_person$adegreen1),
          1,
          flag_person$err_adegreen1)

#副學士學位畢業學校（一）
flag_person$err_adegreeu1 <- 1
flag_person$err_adegreeu1 <-
  if_else(grepl("大學", flag_person$adegreeu1),
          0,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("分校", flag_person$adegreeu1),
          0,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("學院", flag_person$adegreeu1),
          0,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("師大", flag_person$adegreeu1),
          0,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("科大", flag_person$adegreeu1),
          0,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("學校", flag_person$adegreeu1),
          0,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("官校", flag_person$adegreeu1),
          0,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("預校", flag_person$adegreeu1),
          0,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("書院", flag_person$adegreeu1),
          0,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("體院", flag_person$adegreeu1),
          0,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("專科", flag_person$adegreeu1),
          0,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("藝專", flag_person$adegreeu1),
          0,
          flag_person$err_adegreeu1)

flag_person$err_adegreeu1 <-
  if_else(grepl("海事", flag_person$adegreeu1),
          1,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("海專", flag_person$adegreeu1),
          0,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("海事專科", flag_person$adegreeu1),
          0,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("海事商業專科", flag_person$adegreeu1),
          0,
          flag_person$err_adegreeu1)

flag_person$err_adegreeu1 <-
  if_else(grepl("工商", flag_person$adegreeu1),
          1,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("工專", flag_person$adegreeu1),
          0,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("工商管理專科", flag_person$adegreeu1),
          0,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("工商專校", flag_person$adegreeu1),
          0,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("工商專科學校", flag_person$adegreeu1),
          0,
          flag_person$err_adegreeu1)

flag_person$err_adegreeu1 <-
  if_else(grepl("護專", flag_person$adegreeu1),
          0,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("家專", flag_person$adegreeu1),
          0,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("商專", flag_person$adegreeu1),
          0,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("行專", flag_person$adegreeu1),
          0,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("農專", flag_person$adegreeu1),
          0,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("體專", flag_person$adegreeu1),
          0,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("藥專", flag_person$adegreeu1),
          0,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("師專", flag_person$adegreeu1),
          0,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("醫專", flag_person$adegreeu1),
          0,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("語專", flag_person$adegreeu1),
          0,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("企專", flag_person$adegreeu1),
          0,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("士校", flag_person$adegreeu1),
          0,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("專校$", flag_person$adegreeu1),
          0,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("專校(二專)$", flag_person$adegreeu1),
          0,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("逕讀", flag_person$adegreeu1),
          0,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("音樂院$", flag_person$adegreeu1),
          0,
          flag_person$err_adegreeu1)
#flag_person$err_adegreeu1 <- if_else(grepl("音樂研究所$", flag_person$adegreeu1), 0, flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("university", flag_person$adegreeu1),
          0,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("University", flag_person$adegreeu1),
          0,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("UNIVERSITY", flag_person$adegreeu1),
          0,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("Uni$", flag_person$adegreeu1),
          0,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("college", flag_person$adegreeu1),
          0,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("College", flag_person$adegreeu1),
          0,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("COLLEGE", flag_person$adegreeu1),
          0,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("Universidad", flag_person$adegreeu1),
          0,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("UNIVERSIDAD", flag_person$adegreeu1),
          0,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("Conservatory", flag_person$adegreeu1),
          0,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("CRD", flag_person$adegreeu1),
          0,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("ENM", flag_person$adegreeu1),
          0,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("CRC", flag_person$adegreeu1),
          0,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("EMMA", flag_person$adegreeu1),
          0,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("CRR", flag_person$adegreeu1),
          0,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("CNR", flag_person$adegreeu1),
          0,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("TheNewSchool", flag_person$adegreeu1),
          0,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("Hochschule", flag_person$adegreeu1),
          0,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(
    grepl("BergenSchoolofArchitecture", flag_person$adegreeu1),
    0,
    flag_person$err_adegreeu1
  )
flag_person$err_adegreeu1 <-
  if_else(grepl("Universitat", flag_person$adegreeu1),
          0,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("^TUDarmstadt$", flag_person$adegreeu1),
          0,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("^N$", flag_person$adegreeu1),
          0,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("^待查詢$", flag_person$adegreeu1),
          0,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("職業學校", flag_person$adegreeu1),
          1,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("職校", flag_person$adegreeu1),
          1,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("高級", flag_person$adegreeu1),
          1,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("高中", flag_person$adegreeu1),
          1,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("高職", flag_person$adegreeu1),
          1,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("高工", flag_person$adegreeu1),
          1,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("高商", flag_person$adegreeu1),
          1,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("高農", flag_person$adegreeu1),
          1,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("商工", flag_person$adegreeu1),
          1,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("工家", flag_person$adegreeu1),
          1,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("農工", flag_person$adegreeu1),
          1,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("工農", flag_person$adegreeu1),
          1,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("家商", flag_person$adegreeu1),
          1,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("商海", flag_person$adegreeu1),
          1,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("護家", flag_person$adegreeu1),
          1,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("藝校", flag_person$adegreeu1),
          1,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("附工", flag_person$adegreeu1),
          1,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("附中", flag_person$adegreeu1),
          1,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("中學", flag_person$adegreeu1),
          1,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("一中", flag_person$adegreeu1),
          1,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("二中", flag_person$adegreeu1),
          1,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("女中", flag_person$adegreeu1),
          1,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("實中", flag_person$adegreeu1),
          1,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("實驗學校", flag_person$adegreeu1),
          1,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("特殊學校", flag_person$adegreeu1),
          1,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("亞洲餐旅", flag_person$adegreeu1),
          1,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("^高雄餐旅$", flag_person$adegreeu1),
          1,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("^珠海學校$", flag_person$adegreeu1),
          1,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("本國", flag_person$adegreeu1),
          1,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("肄業", flag_person$adegreeu1),
          1,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("學分班", flag_person$adegreeu1),
          1,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("結業", flag_person$adegreeu1),
          1,
          flag_person$err_adegreeu1)
#"仁德醫護管理專科學校"的前身
flag_person$err_adegreeu1 <-
  if_else(grepl("^仁德高級醫事職業學校$", flag_person$adegreeu1),
          0,
          flag_person$err_adegreeu1)
#"慈惠醫護管理專科學校"的前身
flag_person$err_adegreeu1 <-
  if_else(grepl("^私立慈惠謢理助產學校$", flag_person$adegreeu1),
          0,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("籌備處$", flag_person$adegreeu1),
          1,
          flag_person$err_adegreeu1)
flag_person$err_adegreeu1 <-
  if_else(grepl("所", flag_person$adegreeu1),
          1,
          flag_person$err_adegreeu1)

#副學士學位畢業系所（一）
flag_person$err_adegreeg1 <- 0
flag_person$err_adegreeg1 <-
  if_else(grepl("博士", flag_person$adegreeg1),
          1,
          flag_person$err_adegreeg1)
flag_person$err_adegreeg1 <-
  if_else(grepl("碩士", flag_person$adegreeg1),
          1,
          flag_person$err_adegreeg1)
flag_person$err_adegreeg1 <-
  if_else(grepl("學士", flag_person$adegreeg1),
          1,
          flag_person$err_adegreeg1)
flag_person$err_adegreeg1 <-
  if_else(grepl("^副學士$", flag_person$adegreeg1),
          1,
          flag_person$err_adegreeg1)
flag_person$err_adegreeg1 <-
  if_else(grepl("大學", flag_person$adegreeg1),
          1,
          flag_person$err_adegreeg1)
flag_person$err_adegreeg1 <-
  if_else(grepl("分校", flag_person$adegreeg1),
          1,
          flag_person$err_adegreeg1)
flag_person$err_adegreeg1 <-
  if_else(grepl("^學院$", flag_person$adegreeg1),
          1,
          flag_person$err_adegreeg1)
flag_person$err_adegreeg1 <-
  if_else(grepl("科大", flag_person$adegreeg1),
          1,
          flag_person$err_adegreeg1)
flag_person$err_adegreeg1 <-
  if_else(grepl("學校", flag_person$adegreeg1),
          1,
          flag_person$err_adegreeg1)
flag_person$err_adegreeg1 <-
  if_else(grepl("官校", flag_person$adegreeg1),
          1,
          flag_person$err_adegreeg1)
flag_person$err_adegreeg1 <-
  if_else(grepl("預校", flag_person$adegreeg1),
          1,
          flag_person$err_adegreeg1)
flag_person$err_adegreeg1 <-
  if_else(grepl("書院", flag_person$adegreeg1),
          1,
          flag_person$err_adegreeg1)
flag_person$err_adegreeg1 <-
  if_else(grepl("專科", flag_person$adegreeg1),
          1,
          flag_person$err_adegreeg1)
#陸軍官校有"專科班"
flag_person$err_adegreeg1 <-
  if_else((
    grepl("專科", flag_person$adegreeg1) |
      grepl("專科班", flag_person$adegreeg1)
  ) &
    grepl("陸軍官校", flag_person$adegreeu1),
  0,
  flag_person$err_adegreeg1
  )
flag_person$err_adegreeg1 <-
  if_else(grepl("藝專", flag_person$adegreeg1),
          1,
          flag_person$err_adegreeg1)
flag_person$err_adegreeg1 <-
  if_else(grepl("海專", flag_person$adegreeg1),
          1,
          flag_person$err_adegreeg1)
flag_person$err_adegreeg1 <-
  if_else(grepl("工專", flag_person$adegreeg1),
          1,
          flag_person$err_adegreeg1)
flag_person$err_adegreeg1 <-
  if_else(grepl("護專", flag_person$adegreeg1),
          1,
          flag_person$err_adegreeg1)
flag_person$err_adegreeg1 <-
  if_else(grepl("家專", flag_person$adegreeg1),
          1,
          flag_person$err_adegreeg1)
flag_person$err_adegreeg1 <-
  if_else(grepl("商專", flag_person$adegreeg1),
          1,
          flag_person$err_adegreeg1)
flag_person$err_adegreeg1 <-
  if_else(grepl("行專", flag_person$adegreeg1),
          1,
          flag_person$err_adegreeg1)
flag_person$err_adegreeg1 <-
  if_else(grepl("農專", flag_person$adegreeg1),
          1,
          flag_person$err_adegreeg1)
flag_person$err_adegreeg1 <-
  if_else(grepl("體專", flag_person$adegreeg1),
          1,
          flag_person$err_adegreeg1)
flag_person$err_adegreeg1 <-
  if_else(grepl("藥專", flag_person$adegreeg1),
          1,
          flag_person$err_adegreeg1)
flag_person$err_adegreeg1 <-
  if_else(grepl("師專", flag_person$adegreeg1),
          1,
          flag_person$err_adegreeg1)
flag_person$err_adegreeg1 <-
  if_else(grepl("醫專", flag_person$adegreeg1),
          1,
          flag_person$err_adegreeg1)
flag_person$err_adegreeg1 <-
  if_else(grepl("語專", flag_person$adegreeg1),
          1,
          flag_person$err_adegreeg1)
flag_person$err_adegreeg1 <-
  if_else(grepl("企專", flag_person$adegreeg1),
          1,
          flag_person$err_adegreeg1)
flag_person$err_adegreeg1 <-
  if_else(grepl("university", flag_person$adegreeg1),
          1,
          flag_person$err_adegreeg1)
flag_person$err_adegreeg1 <-
  if_else(grepl("University", flag_person$adegreeg1),
          1,
          flag_person$err_adegreeg1)
flag_person$err_adegreeg1 <-
  if_else(grepl("UNIVERSITY", flag_person$adegreeg1),
          1,
          flag_person$err_adegreeg1)
flag_person$err_adegreeg1 <-
  if_else(grepl("college", flag_person$adegreeg1),
          1,
          flag_person$err_adegreeg1)
flag_person$err_adegreeg1 <-
  if_else(grepl("College", flag_person$adegreeg1),
          1,
          flag_person$err_adegreeg1)
flag_person$err_adegreeg1 <-
  if_else(grepl("COLLEGE", flag_person$adegreeg1),
          1,
          flag_person$err_adegreeg1)
flag_person$err_adegreeg1 <-
  if_else(grepl("兼課", flag_person$adegreeg1),
          1,
          flag_person$err_adegreeg1)
flag_person$err_adegreeg1 <-
  if_else(grepl("最高學歷", flag_person$adegreeg1),
          1,
          flag_person$err_adegreeg1)
flag_person$err_adegreeg1 <-
  if_else(grepl("逕讀", flag_person$adegreeg1),
          1,
          flag_person$err_adegreeg1)
flag_person$err_adegreeg1 <-
  if_else(grepl("^Y$", flag_person$adegreeg1),
          1,
          flag_person$err_adegreeg1)
flag_person$err_adegreeg1 <-
  if_else(grepl("^待查詢$", flag_person$adegreeg1),
          1,
          flag_person$err_adegreeg1)
flag_person$err_adegreeg1 <-
  if_else(grepl("^無$", flag_person$adegreeg1),
          1,
          flag_person$err_adegreeg1)
flag_person$err_adegreeg1 <-
  if_else(grepl("^同等學力$", flag_person$adegreeg1),
          0,
          flag_person$err_adegreeg1)
flag_person$err_adegreeg1 <-
  if_else(grepl("肄業", flag_person$adegreeg1),
          1,
          flag_person$err_adegreeg1)
flag_person$err_adegreeg1 <-
  if_else(grepl("學分班", flag_person$adegreeg1),
          1,
          flag_person$err_adegreeg1)
flag_person$err_adegreeg1 <-
  if_else(grepl("結業", flag_person$adegreeg1),
          1,
          flag_person$err_adegreeg1)
flag_person$err_adegreeg1 <-
  if_else(grepl("行政$", flag_person$adegreeg1),
          1,
          flag_person$err_adegreeg1)
flag_person$err_adegreeg1 <-
  if_else(grepl("所", flag_person$adegreeg1),
          1,
          flag_person$err_adegreeg1)

#副學士學位畢業學校國別（二）
flag_person$err_adegreen2 <- 0
flag_person$err_adegreen2 <-
  if_else(grepl("博士", flag_person$adegreen2),
          1,
          flag_person$err_adegreen2)
flag_person$err_adegreen2 <-
  if_else(grepl("碩士", flag_person$adegreen2),
          1,
          flag_person$err_adegreen2)
flag_person$err_adegreen2 <-
  if_else(grepl("學士", flag_person$adegreen2),
          1,
          flag_person$err_adegreen2)
flag_person$err_adegreen2 <-
  if_else(grepl("副學士", flag_person$adegreen2),
          1,
          flag_person$err_adegreen2)
flag_person$err_adegreen2 <-
  if_else(grepl("大學", flag_person$adegreen2),
          1,
          flag_person$err_adegreen2)
flag_person$err_adegreen2 <-
  if_else(grepl("分校", flag_person$adegreen2),
          1,
          flag_person$err_adegreen2)
flag_person$err_adegreen2 <-
  if_else(grepl("學院", flag_person$adegreen2),
          1,
          flag_person$err_adegreen2)
flag_person$err_adegreen2 <-
  if_else(grepl("科大", flag_person$adegreen2),
          1,
          flag_person$err_adegreen2)
flag_person$err_adegreen2 <-
  if_else(grepl("學校", flag_person$adegreen2),
          1,
          flag_person$err_adegreen2)
flag_person$err_adegreen2 <-
  if_else(grepl("官校", flag_person$adegreen2),
          1,
          flag_person$err_adegreen2)
flag_person$err_adegreen2 <-
  if_else(grepl("預校", flag_person$adegreen2),
          1,
          flag_person$err_adegreen2)
flag_person$err_adegreen2 <-
  if_else(grepl("書院", flag_person$adegreen2),
          1,
          flag_person$err_adegreen2)
flag_person$err_adegreen2 <-
  if_else(grepl("專科", flag_person$adegreen2),
          1,
          flag_person$err_adegreen2)
flag_person$err_adegreen2 <-
  if_else(grepl("藝專", flag_person$adegreen2),
          1,
          flag_person$err_adegreen2)
flag_person$err_adegreen2 <-
  if_else(grepl("海專", flag_person$adegreen2),
          1,
          flag_person$err_adegreen2)
flag_person$err_adegreen2 <-
  if_else(grepl("工專", flag_person$adegreen2),
          1,
          flag_person$err_adegreen2)
flag_person$err_adegreen2 <-
  if_else(grepl("護專", flag_person$adegreen2),
          1,
          flag_person$err_adegreen2)
flag_person$err_adegreen2 <-
  if_else(grepl("家專", flag_person$adegreen2),
          1,
          flag_person$err_adegreen2)
flag_person$err_adegreen2 <-
  if_else(grepl("商專", flag_person$adegreen2),
          1,
          flag_person$err_adegreen2)
flag_person$err_adegreen2 <-
  if_else(grepl("行專", flag_person$adegreen2),
          1,
          flag_person$err_adegreen2)
flag_person$err_adegreen2 <-
  if_else(grepl("農專", flag_person$adegreen2),
          1,
          flag_person$err_adegreen2)
flag_person$err_adegreen2 <-
  if_else(grepl("體專", flag_person$adegreen2),
          1,
          flag_person$err_adegreen2)
flag_person$err_adegreen2 <-
  if_else(grepl("藥專", flag_person$adegreen2),
          1,
          flag_person$err_adegreen2)
flag_person$err_adegreen2 <-
  if_else(grepl("師專", flag_person$adegreen2),
          1,
          flag_person$err_adegreen2)
flag_person$err_adegreen2 <-
  if_else(grepl("醫專", flag_person$adegreen2),
          1,
          flag_person$err_adegreen2)
flag_person$err_adegreen2 <-
  if_else(grepl("語專", flag_person$adegreen2),
          1,
          flag_person$err_adegreen2)
flag_person$err_adegreen2 <-
  if_else(grepl("企專", flag_person$adegreen2),
          1,
          flag_person$err_adegreen2)
flag_person$err_adegreen2 <-
  if_else(grepl("university", flag_person$adegreen2),
          1,
          flag_person$err_adegreen2)
flag_person$err_adegreen2 <-
  if_else(grepl("University", flag_person$adegreen2),
          1,
          flag_person$err_adegreen2)
flag_person$err_adegreen2 <-
  if_else(grepl("UNIVERSITY", flag_person$adegreen2),
          1,
          flag_person$err_adegreen2)
flag_person$err_adegreen2 <-
  if_else(grepl("college", flag_person$adegreen2),
          1,
          flag_person$err_adegreen2)
flag_person$err_adegreen2 <-
  if_else(grepl("College", flag_person$adegreen2),
          1,
          flag_person$err_adegreen2)
flag_person$err_adegreen2 <-
  if_else(grepl("COLLEGE", flag_person$adegreen2),
          1,
          flag_person$err_adegreen2)
flag_person$err_adegreen2 <-
  if_else(grepl("系", flag_person$adegreen2),
          1,
          flag_person$err_adegreen2)
flag_person$err_adegreen2 <-
  if_else(grepl("所", flag_person$adegreen2),
          1,
          flag_person$err_adegreen2)
flag_person$err_adegreen2 <-
  if_else(grepl("班$", flag_person$adegreen2),
          1,
          flag_person$err_adegreen2)
flag_person$err_adegreen2 <-
  if_else(grepl("不分科系", flag_person$adegreen2),
          1,
          flag_person$err_adegreen2)
flag_person$err_adegreen2 <-
  if_else(grepl("不分系", flag_person$adegreen2),
          1,
          flag_person$err_adegreen2)
flag_person$err_adegreen2 <-
  if_else(grepl("department", flag_person$adegreen2),
          1,
          flag_person$err_adegreen2)
flag_person$err_adegreen2 <-
  if_else(grepl("Department", flag_person$adegreen2),
          1,
          flag_person$err_adegreen2)
flag_person$err_adegreen2 <-
  if_else(grepl("DEPARTMENT", flag_person$adegreen2),
          1,
          flag_person$err_adegreen2)
flag_person$err_adegreen2 <-
  if_else(grepl("兼課", flag_person$adegreen2),
          1,
          flag_person$err_adegreen2)
flag_person$err_adegreen2 <-
  if_else(grepl("最高學歷", flag_person$adegreen2),
          1,
          flag_person$err_adegreen2)
flag_person$err_adegreen2 <-
  if_else(grepl("逕讀", flag_person$adegreen2),
          1,
          flag_person$err_adegreen2)
flag_person$err_adegreen2 <-
  if_else(grepl("^Y$", flag_person$adegreen2),
          1,
          flag_person$err_adegreen2)
flag_person$err_adegreen2 <-
  if_else(grepl("^待查詢$", flag_person$adegreen2),
          1,
          flag_person$err_adegreen2)
flag_person$err_adegreen2 <-
  if_else(grepl("^無$", flag_person$adegreen2),
          1,
          flag_person$err_adegreen2)
flag_person$err_adegreen2 <-
  if_else(grepl("^外國$", flag_person$adegreen2),
          1,
          flag_person$err_adegreen2)
flag_person$err_adegreen2 <-
  if_else(grepl("^國立$", flag_person$adegreen2),
          1,
          flag_person$err_adegreen2)
flag_person$err_adegreen2 <-
  if_else(grepl("^歐洲$", flag_person$adegreen2),
          1,
          flag_person$err_adegreen2)
flag_person$err_adegreen2 <-
  if_else(grepl("^亞洲$", flag_person$adegreen2),
          1,
          flag_person$err_adegreen2)
flag_person$err_adegreen2 <-
  if_else(grepl("^同等學力$", flag_person$adegreen2),
          0,
          flag_person$err_adegreen2)
flag_person$err_adegreen2 <-
  if_else(grepl("肄業", flag_person$adegreen2),
          1,
          flag_person$err_adegreen2)
flag_person$err_adegreen2 <-
  if_else(grepl("學分班", flag_person$adegreen2),
          1,
          flag_person$err_adegreen2)
flag_person$err_adegreen2 <-
  if_else(grepl("結業", flag_person$adegreen2),
          1,
          flag_person$err_adegreen2)

#副學士學位畢業學校（二）
flag_person$err_adegreeu2 <- 1
flag_person$err_adegreeu2 <-
  if_else(grepl("大學", flag_person$adegreeu2),
          0,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("分校", flag_person$adegreeu2),
          0,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("學院", flag_person$adegreeu2),
          0,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("師大", flag_person$adegreeu2),
          0,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("科大", flag_person$adegreeu2),
          0,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("學校", flag_person$adegreeu2),
          0,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("官校", flag_person$adegreeu2),
          0,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("預校", flag_person$adegreeu2),
          0,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("書院", flag_person$adegreeu2),
          0,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("體院", flag_person$adegreeu2),
          0,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("專科", flag_person$adegreeu2),
          0,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("藝專", flag_person$adegreeu2),
          0,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("海專", flag_person$adegreeu2),
          0,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("工專", flag_person$adegreeu2),
          0,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("護專", flag_person$adegreeu2),
          0,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("家專", flag_person$adegreeu2),
          0,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("商專", flag_person$adegreeu2),
          0,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("行專", flag_person$adegreeu2),
          0,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("農專", flag_person$adegreeu2),
          0,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("體專", flag_person$adegreeu2),
          0,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("藥專", flag_person$adegreeu2),
          0,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("師專", flag_person$adegreeu2),
          0,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("醫專", flag_person$adegreeu2),
          0,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("語專", flag_person$adegreeu2),
          0,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("企專", flag_person$adegreeu2),
          0,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("士校", flag_person$adegreeu2),
          0,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("專校$", flag_person$adegreeu2),
          0,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("專校(二專)$", flag_person$adegreeu2),
          0,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("逕讀", flag_person$adegreeu2),
          0,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("音樂院$", flag_person$adegreeu2),
          0,
          flag_person$err_adegreeu2)
#flag_person$err_adegreeu2 <- if_else(grepl("音樂研究所$", flag_person$adegreeu2), 0, flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("university", flag_person$adegreeu2),
          0,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("University", flag_person$adegreeu2),
          0,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("UNIVERSITY", flag_person$adegreeu2),
          0,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("Uni$", flag_person$adegreeu2),
          0,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("college", flag_person$adegreeu2),
          0,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("College", flag_person$adegreeu2),
          0,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("COLLEGE", flag_person$adegreeu2),
          0,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("Universidad", flag_person$adegreeu2),
          0,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("UNIVERSIDAD", flag_person$adegreeu2),
          0,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("Conservatory", flag_person$adegreeu2),
          0,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("CRD", flag_person$adegreeu2),
          0,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("ENM", flag_person$adegreeu2),
          0,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("CRC", flag_person$adegreeu2),
          0,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("EMMA", flag_person$adegreeu2),
          0,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("CRR", flag_person$adegreeu2),
          0,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("CNR", flag_person$adegreeu2),
          0,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("TheNewSchool", flag_person$adegreeu2),
          0,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("Hochschule", flag_person$adegreeu2),
          0,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(
    grepl("BergenSchoolofArchitecture", flag_person$adegreeu2),
    0,
    flag_person$err_adegreeu2
  )
flag_person$err_adegreeu2 <-
  if_else(grepl("Universitat", flag_person$adegreeu2),
          0,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("^TUDarmstadt$", flag_person$adegreeu2),
          0,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("^N$", flag_person$adegreeu2),
          0,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("^待查詢$", flag_person$adegreeu2),
          0,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("職業學校", flag_person$adegreeu2),
          1,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("職校", flag_person$adegreeu2),
          1,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("高級", flag_person$adegreeu2),
          1,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("高中", flag_person$adegreeu2),
          1,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("高職", flag_person$adegreeu2),
          1,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("高工", flag_person$adegreeu2),
          1,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("高商", flag_person$adegreeu2),
          1,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("高農", flag_person$adegreeu2),
          1,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("商工", flag_person$adegreeu2),
          1,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("工商", flag_person$adegreeu2),
          1,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("工家", flag_person$adegreeu2),
          1,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("農工", flag_person$adegreeu2),
          1,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("工農", flag_person$adegreeu2),
          1,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("家商", flag_person$adegreeu2),
          1,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("商海", flag_person$adegreeu2),
          1,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("海事", flag_person$adegreeu2),
          1,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("護家", flag_person$adegreeu2),
          1,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("藝校", flag_person$adegreeu2),
          1,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("附工", flag_person$adegreeu2),
          1,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("附中", flag_person$adegreeu2),
          1,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("中學", flag_person$adegreeu2),
          1,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("一中", flag_person$adegreeu2),
          1,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("二中", flag_person$adegreeu2),
          1,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("女中", flag_person$adegreeu2),
          1,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("實中", flag_person$adegreeu2),
          1,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("實驗學校", flag_person$adegreeu2),
          1,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("特殊學校", flag_person$adegreeu2),
          1,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("亞洲餐旅", flag_person$adegreeu2),
          1,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("^高雄餐旅$", flag_person$adegreeu2),
          1,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("^珠海學校$", flag_person$adegreeu2),
          1,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("本國", flag_person$adegreeu2),
          1,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("肄業", flag_person$adegreeu2),
          1,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("學分班", flag_person$adegreeu2),
          1,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("結業", flag_person$adegreeu2),
          1,
          flag_person$err_adegreeu2)
#"仁德醫護管理專科學校"的前身
flag_person$err_adegreeu2 <-
  if_else(grepl("^仁德高級醫事職業學校$", flag_person$adegreeu2),
          0,
          flag_person$err_adegreeu2)
#"慈惠醫護管理專科學校"的前身
flag_person$err_adegreeu2 <-
  if_else(grepl("^私立慈惠謢理助產學校$", flag_person$adegreeu2),
          0,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("籌備處$", flag_person$adegreeu2),
          1,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("所", flag_person$adegreeu2),
          1,
          flag_person$err_adegreeu2)
#"工商專校"、"工商專科學校"正確
flag_person$err_adegreeu2 <-
  if_else(grepl("工商專校", flag_person$adegreeu2),
          0,
          flag_person$err_adegreeu2)
flag_person$err_adegreeu2 <-
  if_else(grepl("工商專科學校", flag_person$adegreeu2),
          0,
          flag_person$err_adegreeu2)

#副學士學位畢業系所（二）
flag_person$err_adegreeg2 <- 0
flag_person$err_adegreeg2 <-
  if_else(grepl("博士", flag_person$adegreeg2),
          1,
          flag_person$err_adegreeg2)
flag_person$err_adegreeg2 <-
  if_else(grepl("碩士", flag_person$adegreeg2),
          1,
          flag_person$err_adegreeg2)
flag_person$err_adegreeg2 <-
  if_else(grepl("學士", flag_person$adegreeg2),
          1,
          flag_person$err_adegreeg2)
flag_person$err_adegreeg2 <-
  if_else(grepl("^副學士$", flag_person$adegreeg2),
          1,
          flag_person$err_adegreeg2)
flag_person$err_adegreeg2 <-
  if_else(grepl("大學", flag_person$adegreeg2),
          1,
          flag_person$err_adegreeg2)
flag_person$err_adegreeg2 <-
  if_else(grepl("分校", flag_person$adegreeg2),
          1,
          flag_person$err_adegreeg2)
flag_person$err_adegreeg2 <-
  if_else(grepl("^學院$", flag_person$adegreeg2),
          1,
          flag_person$err_adegreeg2)
flag_person$err_adegreeg2 <-
  if_else(grepl("科大", flag_person$adegreeg2),
          1,
          flag_person$err_adegreeg2)
flag_person$err_adegreeg2 <-
  if_else(grepl("學校", flag_person$adegreeg2),
          1,
          flag_person$err_adegreeg2)
flag_person$err_adegreeg2 <-
  if_else(grepl("官校", flag_person$adegreeg2),
          1,
          flag_person$err_adegreeg2)
flag_person$err_adegreeg2 <-
  if_else(grepl("預校", flag_person$adegreeg2),
          1,
          flag_person$err_adegreeg2)
flag_person$err_adegreeg2 <-
  if_else(grepl("書院", flag_person$adegreeg2),
          1,
          flag_person$err_adegreeg2)
flag_person$err_adegreeg2 <-
  if_else(grepl("專科", flag_person$adegreeg2),
          1,
          flag_person$err_adegreeg2)
#陸軍官校有"專科班"
flag_person$err_adegreeg2 <-
  if_else((
    grepl("專科", flag_person$adegreeg2) |
      grepl("專科班", flag_person$adegreeg2)
  ) &
    grepl("陸軍官校", flag_person$adegreeu1),
  0,
  flag_person$err_adegreeg2
  )
flag_person$err_adegreeg2 <-
  if_else(grepl("藝專", flag_person$adegreeg2),
          1,
          flag_person$err_adegreeg2)
flag_person$err_adegreeg2 <-
  if_else(grepl("海專", flag_person$adegreeg2),
          1,
          flag_person$err_adegreeg2)
flag_person$err_adegreeg2 <-
  if_else(grepl("工專", flag_person$adegreeg2),
          1,
          flag_person$err_adegreeg2)
flag_person$err_adegreeg2 <-
  if_else(grepl("護專", flag_person$adegreeg2),
          1,
          flag_person$err_adegreeg2)
flag_person$err_adegreeg2 <-
  if_else(grepl("家專", flag_person$adegreeg2),
          1,
          flag_person$err_adegreeg2)
flag_person$err_adegreeg2 <-
  if_else(grepl("商專", flag_person$adegreeg2),
          1,
          flag_person$err_adegreeg2)
flag_person$err_adegreeg2 <-
  if_else(grepl("行專", flag_person$adegreeg2),
          1,
          flag_person$err_adegreeg2)
flag_person$err_adegreeg2 <-
  if_else(grepl("農專", flag_person$adegreeg2),
          1,
          flag_person$err_adegreeg2)
flag_person$err_adegreeg2 <-
  if_else(grepl("體專", flag_person$adegreeg2),
          1,
          flag_person$err_adegreeg2)
flag_person$err_adegreeg2 <-
  if_else(grepl("藥專", flag_person$adegreeg2),
          1,
          flag_person$err_adegreeg2)
flag_person$err_adegreeg2 <-
  if_else(grepl("師專", flag_person$adegreeg2),
          1,
          flag_person$err_adegreeg2)
flag_person$err_adegreeg2 <-
  if_else(grepl("醫專", flag_person$adegreeg2),
          1,
          flag_person$err_adegreeg2)
flag_person$err_adegreeg2 <-
  if_else(grepl("語專", flag_person$adegreeg2),
          1,
          flag_person$err_adegreeg2)
flag_person$err_adegreeg2 <-
  if_else(grepl("企專", flag_person$adegreeg2),
          1,
          flag_person$err_adegreeg2)
flag_person$err_adegreeg2 <-
  if_else(grepl("university", flag_person$adegreeg2),
          1,
          flag_person$err_adegreeg2)
flag_person$err_adegreeg2 <-
  if_else(grepl("University", flag_person$adegreeg2),
          1,
          flag_person$err_adegreeg2)
flag_person$err_adegreeg2 <-
  if_else(grepl("UNIVERSITY", flag_person$adegreeg2),
          1,
          flag_person$err_adegreeg2)
flag_person$err_adegreeg2 <-
  if_else(grepl("college", flag_person$adegreeg2),
          1,
          flag_person$err_adegreeg2)
flag_person$err_adegreeg2 <-
  if_else(grepl("College", flag_person$adegreeg2),
          1,
          flag_person$err_adegreeg2)
flag_person$err_adegreeg2 <-
  if_else(grepl("COLLEGE", flag_person$adegreeg2),
          1,
          flag_person$err_adegreeg2)
flag_person$err_adegreeg2 <-
  if_else(grepl("兼課", flag_person$adegreeg2),
          1,
          flag_person$err_adegreeg2)
flag_person$err_adegreeg2 <-
  if_else(grepl("最高學歷", flag_person$adegreeg2),
          1,
          flag_person$err_adegreeg2)
flag_person$err_adegreeg2 <-
  if_else(grepl("逕讀", flag_person$adegreeg2),
          1,
          flag_person$err_adegreeg2)
flag_person$err_adegreeg2 <-
  if_else(grepl("^Y$", flag_person$adegreeg2),
          1,
          flag_person$err_adegreeg2)
flag_person$err_adegreeg2 <-
  if_else(grepl("^待查詢$", flag_person$adegreeg2),
          1,
          flag_person$err_adegreeg2)
flag_person$err_adegreeg2 <-
  if_else(grepl("^無$", flag_person$adegreeg2),
          1,
          flag_person$err_adegreeg2)
flag_person$err_adegreeg2 <-
  if_else(grepl("^同等學力$", flag_person$adegreeg2),
          0,
          flag_person$err_adegreeg2)
flag_person$err_adegreeg2 <-
  if_else(grepl("肄業", flag_person$adegreeg2),
          1,
          flag_person$err_adegreeg2)
flag_person$err_adegreeg2 <-
  if_else(grepl("學分班", flag_person$adegreeg2),
          1,
          flag_person$err_adegreeg2)
flag_person$err_adegreeg2 <-
  if_else(grepl("結業", flag_person$adegreeg2),
          1,
          flag_person$err_adegreeg2)
flag_person$err_adegreeg2 <-
  if_else(grepl("行政$", flag_person$adegreeg2),
          1,
          flag_person$err_adegreeg2)
flag_person$err_adegreeg2 <-
  if_else(grepl("所", flag_person$adegreeg2),
          1,
          flag_person$err_adegreeg2)

#學校名稱與科系名稱相同之情形
flag_person$err_ddegreeg1 <-
  if_else(
    flag_person$ddegreeu1 == flag_person$ddegreeg1 &
      (
        flag_person$ddegreeu1 != "N" &
          flag_person$ddegreeu1 != "逕讀碩士" &
          flag_person$ddegreeu1 != "逕讀博士"
      ),
    1,
    flag_person$err_ddegreeg1
  )
flag_person$err_ddegreeg2 <-
  if_else(
    flag_person$ddegreeu2 == flag_person$ddegreeg2 &
      (
        flag_person$ddegreeu2 != "N" &
          flag_person$ddegreeu2 != "逕讀碩士" &
          flag_person$ddegreeu2 != "逕讀博士"
      ),
    1,
    flag_person$err_ddegreeg2
  )
flag_person$err_mdegreeg1 <-
  if_else(
    flag_person$mdegreeu1 == flag_person$mdegreeg1 &
      (
        flag_person$mdegreeu1 != "N" &
          flag_person$mdegreeu1 != "逕讀碩士" &
          flag_person$mdegreeu1 != "逕讀博士"
      ),
    1,
    flag_person$err_mdegreeg1
  )
flag_person$err_mdegreeg2 <-
  if_else(
    flag_person$mdegreeu2 == flag_person$mdegreeg2 &
      (
        flag_person$mdegreeu2 != "N" &
          flag_person$mdegreeu2 != "逕讀碩士" &
          flag_person$mdegreeu2 != "逕讀博士"
      ),
    1,
    flag_person$err_mdegreeg2
  )
flag_person$err_bdegreeg1 <-
  if_else(
    flag_person$bdegreeu1 == flag_person$bdegreeg1 &
      (
        flag_person$bdegreeu1 != "N" &
          flag_person$bdegreeu1 != "逕讀碩士" &
          flag_person$bdegreeu1 != "逕行修讀碩士" &
          flag_person$bdegreeu1 != "逕讀博士"
      ),
    1,
    flag_person$err_bdegreeg1
  )
flag_person$err_bdegreeg2 <-
  if_else(
    flag_person$bdegreeu2 == flag_person$bdegreeg2 &
      (
        flag_person$bdegreeu2 != "N" &
          flag_person$bdegreeu2 != "逕讀碩士" &
          flag_person$bdegreeu2 != "逕讀博士"
      ),
    1,
    flag_person$err_bdegreeg2
  )
flag_person$err_adegreeg1 <-
  if_else(
    flag_person$adegreeu1 == flag_person$adegreeg1 &
      (
        flag_person$adegreeu1 != "N" &
          flag_person$adegreeu1 != "逕讀碩士" &
          flag_person$adegreeu1 != "逕讀博士"
      ),
    1,
    flag_person$err_adegreeg1
  )
flag_person$err_adegreeg2 <-
  if_else(
    flag_person$adegreeu2 == flag_person$adegreeg2 &
      (
        flag_person$adegreeu2 != "N" &
          flag_person$adegreeu2 != "逕讀碩士" &
          flag_person$adegreeu2 != "逕讀博士"
      ),
    1,
    flag_person$err_adegreeg2
  )

#軍校當時沒有區分系所
flag_person$err_ddegreeg1 <-
  if_else(
    flag_person$ddegreeu1 == flag_person$ddegreeg1 &
      flag_person$ddegreeg1 == "海軍軍官學校",
    0,
    flag_person$err_ddegreeg1
  )
flag_person$err_ddegreeg1 <-
  if_else(
    flag_person$ddegreeu1 == flag_person$ddegreeg1 &
      flag_person$ddegreeg1 == "空軍航空技術學院",
    0,
    flag_person$err_ddegreeg1
  )
flag_person$err_ddegreeg2 <-
  if_else(
    flag_person$ddegreeu1 == flag_person$ddegreeg2 &
      flag_person$ddegreeg2 == "海軍軍官學校",
    0,
    flag_person$err_ddegreeg2
  )
flag_person$err_ddegreeg2 <-
  if_else(
    flag_person$ddegreeu1 == flag_person$ddegreeg2 &
      flag_person$ddegreeg2 == "空軍航空技術學院",
    0,
    flag_person$err_ddegreeg2
  )
flag_person$err_mdegreeg1 <-
  if_else(
    flag_person$mdegreeu1 == flag_person$mdegreeg1 &
      flag_person$mdegreeg1 == "海軍軍官學校",
    0,
    flag_person$err_mdegreeg1
  )
flag_person$err_mdegreeg1 <-
  if_else(
    flag_person$mdegreeu1 == flag_person$mdegreeg1 &
      flag_person$mdegreeg1 == "空軍航空技術學院",
    0,
    flag_person$err_mdegreeg1
  )
flag_person$err_mdegreeg2 <-
  if_else(
    flag_person$mdegreeu1 == flag_person$mdegreeg2 &
      flag_person$mdegreeg2 == "海軍軍官學校",
    0,
    flag_person$err_mdegreeg2
  )
flag_person$err_mdegreeg2 <-
  if_else(
    flag_person$mdegreeu1 == flag_person$mdegreeg2 &
      flag_person$mdegreeg2 == "空軍航空技術學院",
    0,
    flag_person$err_mdegreeg2
  )
flag_person$err_bdegreeg1 <-
  if_else(
    flag_person$bdegreeu1 == flag_person$bdegreeg1 &
      flag_person$bdegreeg1 == "海軍軍官學校",
    0,
    flag_person$err_bdegreeg1
  )
flag_person$err_bdegreeg1 <-
  if_else(
    flag_person$bdegreeu1 == flag_person$bdegreeg1 &
      flag_person$bdegreeg1 == "空軍航空技術學院",
    0,
    flag_person$err_bdegreeg1
  )
flag_person$err_bdegreeg2 <-
  if_else(
    flag_person$bdegreeu1 == flag_person$bdegreeg2 &
      flag_person$bdegreeg2 == "海軍軍官學校",
    0,
    flag_person$err_bdegreeg2
  )
flag_person$err_bdegreeg2 <-
  if_else(
    flag_person$bdegreeu1 == flag_person$bdegreeg2 &
      flag_person$bdegreeg2 == "空軍航空技術學院",
    0,
    flag_person$err_bdegreeg2
  )
flag_person$err_adegreeg1 <-
  if_else(
    flag_person$adegreeu1 == flag_person$adegreeg1 &
      flag_person$adegreeg1 == "海軍軍官學校",
    0,
    flag_person$err_adegreeg1
  )
flag_person$err_adegreeg1 <-
  if_else(
    flag_person$adegreeu1 == flag_person$adegreeg1 &
      flag_person$adegreeg1 == "空軍航空技術學院",
    0,
    flag_person$err_adegreeg1
  )
flag_person$err_adegreeg2 <-
  if_else(
    flag_person$adegreeu1 == flag_person$adegreeg2 &
      flag_person$adegreeg2 == "海軍軍官學校",
    0,
    flag_person$err_adegreeg2
  )
flag_person$err_adegreeg2 <-
  if_else(
    flag_person$adegreeu1 == flag_person$adegreeg2 &
      flag_person$adegreeg2 == "空軍航空技術學院",
    0,
    flag_person$err_adegreeg2
  )

#學士逕讀碩士，但副學士為N
flag_person$err_bdeade <- 0
flag_person$err_bdeade <-
  if_else(
    flag_person$bdegreeu1 == "逕讀碩士" &
      flag_person$adegreeu1 == "N",
    1,
    flag_person$err_bdeade
  )
flag_person$err_bdeade <-
  if_else(
    flag_person$bdegreeu1 == "逕讀" &
      flag_person$adegreeu1 == "N",
    1,
    flag_person$err_bdeade
  )
flag_person$err_bdeade <-
  if_else(
    grepl("讀", flag_person$bdegreeu1) &
      flag_person$adegreeu1 == "N",
    1,
    flag_person$err_bdeade
  )

#碩士逕讀碩士，但學士為N
flag_person$err_bdeade2 <- 0
flag_person$err_bdeade2 <-
  if_else(
    flag_person$mdegreeu1 == "逕讀博士" &
      flag_person$bdegreeu1 == "N",
    1,
    flag_person$err_bdeade2
  )
flag_person$err_bdeade2 <-
  if_else(
    flag_person$mdegreeu1 == "逕讀" &
      flag_person$bdegreeu1 == "N",
    1,
    flag_person$err_bdeade2
  )

#碩士、學士逕讀博士，直接有博士
flag_person$err_bdeade3 <- 0
flag_person$err_bdeade3 <-
  if_else(
    flag_person$mdegreeu1 == "逕讀博士" &
      flag_person$bdegreeu1 == "逕讀博士",
    1,
    flag_person$err_bdeade3
  )
flag_person$err_bdeade3 <-
  if_else(
    flag_person$mdegreeu1 == "逕讀" &
      flag_person$bdegreeu1 == "逕讀",
    1,
    flag_person$err_bdeade3
  )

flag_person$err_bdeade <-
  if_else(flag_person$err_bdeade3 == 1 &
            flag_person$err_bdeade == 1,
          0,
          flag_person$err_bdeade)

#有副學士，學士填逕讀碩士，但碩士為N
flag_person$err_bdeade6 <- 0
flag_person$err_bdeade6 <-
  if_else(
    flag_person$adegreeu1 != "N" &
      grepl("讀", flag_person$bdegreeu1) &
      flag_person$mdegreeu1 == "N",
    1,
    flag_person$err_bdeade6
  )

#有學士，碩士填逕讀博士，但博士為N
flag_person$err_bdeade7 <- 0
flag_person$err_bdeade7 <-
  if_else(
    flag_person$bdegreeu1 != "N" &
      grepl("讀", flag_person$mdegreeu1) &
      flag_person$ddegreeu1 == "N",
    1,
    flag_person$err_bdeade7
  )


#學校為國外學校，但國別卻填本國
flag_person$err_bdeade4 <- 0
#博士(一)
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen1 == "本國" &
      grepl("A", flag_person$ddegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen1 == "本國" &
      grepl("E", flag_person$ddegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen1 == "本國" &
      grepl("I", flag_person$ddegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen1 == "本國" &
      grepl("O", flag_person$ddegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen1 == "本國" &
      grepl("U", flag_person$ddegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen1 == "本國" &
      grepl("a", flag_person$ddegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen1 == "本國" &
      grepl("e", flag_person$ddegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen1 == "本國" &
      grepl("i", flag_person$ddegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen1 == "本國" &
      grepl("o", flag_person$ddegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen1 == "本國" &
      grepl("u", flag_person$ddegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen1 == "本國" &
      grepl("^英國", flag_person$ddegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen1 == "本國" &
      grepl("^美國", flag_person$ddegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen1 == "本國" &
      grepl("^加拿大", flag_person$ddegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen1 == "本國" &
      grepl("^日本", flag_person$ddegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen1 == "本國" &
      grepl("^韓國", flag_person$ddegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen1 == "本國" &
      grepl("^菲律賓", flag_person$ddegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen1 == "本國" &
      grepl("^南非", flag_person$ddegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen1 == "本國" &
      grepl("^西班牙", flag_person$ddegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen1 == "本國" &
      grepl("^法國", flag_person$ddegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen1 == "本國" &
      grepl("^俄羅斯", flag_person$ddegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen1 == "本國" &
      grepl("^德國", flag_person$ddegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen1 == "本國" &
      grepl("^澳洲", flag_person$ddegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen1 == "本國" &
      grepl("^紐西蘭", flag_person$ddegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen1 == "本國" &
      grepl("^義大利", flag_person$ddegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen1 == "本國" &
      grepl("^比利時", flag_person$ddegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen1 == "本國" &
      grepl("^芬蘭", flag_person$ddegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen1 == "本國" &
      grepl("^澳大利亞", flag_person$ddegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen1 == "本國" &
      grepl("^泰國", flag_person$ddegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen1 == "本國" &
      grepl("^美利堅合眾國", flag_person$ddegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen1 == "本國" &
      grepl("^印尼", flag_person$ddegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen1 == "本國" &
      grepl("^阿根廷", flag_person$ddegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen1 == "本國" &
      grepl("^越南", flag_person$ddegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen1 == "本國" &
      grepl("^香港", flag_person$ddegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen1 == "英國" &
      grepl("^彰化", flag_person$ddegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen1 == "本國" &
      grepl("^澳大利亞", flag_person$ddegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen1 == "本國" &
      grepl("^荷蘭", flag_person$ddegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen1 == "本國" &
      grepl("紐約", flag_person$ddegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen1 == "本國" &
      grepl("州立", flag_person$ddegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen1 == "本國" &
      grepl("東京", flag_person$ddegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen1 == "本國" &
      grepl("波士頓", flag_person$ddegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen1 == "本國" &
      grepl("路易安納", flag_person$ddegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen1 == "本國" &
      grepl("關西", flag_person$ddegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen1 == "本國" &
      grepl("專修大學", flag_person$ddegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen1 == "本國" &
      grepl("匹茲堡", flag_person$ddegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen1 == "本國" &
      grepl("俄克拉荷馬", flag_person$ddegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen1 == "本國" &
      grepl("雪菲爾", flag_person$ddegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen1 == "本國" &
      grepl("胡志明", flag_person$ddegreeu1),
    1,
    flag_person$err_bdeade4
  )
#博士(二)
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen2 == "本國" &
      grepl("A", flag_person$ddegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen2 == "本國" &
      grepl("E", flag_person$ddegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen2 == "本國" &
      grepl("I", flag_person$ddegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen2 == "本國" &
      grepl("O", flag_person$ddegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen2 == "本國" &
      grepl("U", flag_person$ddegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen2 == "本國" &
      grepl("a", flag_person$ddegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen2 == "本國" &
      grepl("e", flag_person$ddegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen2 == "本國" &
      grepl("i", flag_person$ddegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen2 == "本國" &
      grepl("o", flag_person$ddegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen2 == "本國" &
      grepl("u", flag_person$ddegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen2 == "本國" &
      grepl("^英國", flag_person$ddegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen2 == "本國" &
      grepl("^美國", flag_person$ddegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen2 == "本國" &
      grepl("^加拿大", flag_person$ddegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen2 == "本國" &
      grepl("^日本", flag_person$ddegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen2 == "本國" &
      grepl("^韓國", flag_person$ddegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen2 == "本國" &
      grepl("^菲律賓", flag_person$ddegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen2 == "本國" &
      grepl("^南非", flag_person$ddegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen2 == "本國" &
      grepl("^西班牙", flag_person$ddegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen2 == "本國" &
      grepl("^法國", flag_person$ddegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen2 == "本國" &
      grepl("^俄羅斯", flag_person$ddegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen2 == "本國" &
      grepl("^德國", flag_person$ddegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen2 == "本國" &
      grepl("^澳洲", flag_person$ddegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen2 == "本國" &
      grepl("^紐西蘭", flag_person$ddegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen2 == "本國" &
      grepl("^義大利", flag_person$ddegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen2 == "本國" &
      grepl("^比利時", flag_person$ddegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen2 == "本國" &
      grepl("^芬蘭", flag_person$ddegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen2 == "本國" &
      grepl("^澳大利亞", flag_person$ddegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen2 == "本國" &
      grepl("^泰國", flag_person$ddegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen2 == "本國" &
      grepl("^美利堅合眾國", flag_person$ddegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen2 == "本國" &
      grepl("^印尼", flag_person$ddegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen2 == "本國" &
      grepl("^阿根廷", flag_person$ddegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen2 == "本國" &
      grepl("^越南", flag_person$ddegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen2 == "本國" &
      grepl("^香港", flag_person$ddegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen2 == "英國" &
      grepl("^彰化", flag_person$ddegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen2 == "本國" &
      grepl("^澳大利亞", flag_person$ddegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen2 == "本國" &
      grepl("^荷蘭", flag_person$ddegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen2 == "本國" &
      grepl("紐約", flag_person$ddegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen2 == "本國" &
      grepl("州立", flag_person$ddegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen2 == "本國" &
      grepl("東京", flag_person$ddegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen2 == "本國" &
      grepl("波士頓", flag_person$ddegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen2 == "本國" &
      grepl("路易安納", flag_person$ddegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen2 == "本國" &
      grepl("關西", flag_person$ddegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen2 == "本國" &
      grepl("專修大學", flag_person$ddegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen2 == "本國" &
      grepl("匹茲堡", flag_person$ddegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen2 == "本國" &
      grepl("俄克拉荷馬", flag_person$ddegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen2 == "本國" &
      grepl("雪菲爾", flag_person$ddegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$ddegreen2 == "本國" &
      grepl("胡志明", flag_person$ddegreeu2),
    1,
    flag_person$err_bdeade4
  )
#碩士(一)
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen1 == "本國" &
      grepl("A", flag_person$mdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen1 == "本國" &
      grepl("E", flag_person$mdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen1 == "本國" &
      grepl("I", flag_person$mdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen1 == "本國" &
      grepl("O", flag_person$mdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen1 == "本國" &
      grepl("U", flag_person$mdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen1 == "本國" &
      grepl("a", flag_person$mdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen1 == "本國" &
      grepl("e", flag_person$mdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen1 == "本國" &
      grepl("i", flag_person$mdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen1 == "本國" &
      grepl("o", flag_person$mdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen1 == "本國" &
      grepl("u", flag_person$mdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen1 == "本國" &
      grepl("^英國", flag_person$mdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen1 == "本國" &
      grepl("^美國", flag_person$mdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen1 == "本國" &
      grepl("^加拿大", flag_person$mdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen1 == "本國" &
      grepl("^日本", flag_person$mdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen1 == "本國" &
      grepl("^韓國", flag_person$mdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen1 == "本國" &
      grepl("^菲律賓", flag_person$mdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen1 == "本國" &
      grepl("^南非", flag_person$mdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen1 == "本國" &
      grepl("^西班牙", flag_person$mdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen1 == "本國" &
      grepl("^法國", flag_person$mdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen1 == "本國" &
      grepl("^俄羅斯", flag_person$mdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen1 == "本國" &
      grepl("^德國", flag_person$mdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen1 == "本國" &
      grepl("^澳洲", flag_person$mdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen1 == "本國" &
      grepl("^紐西蘭", flag_person$mdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen1 == "本國" &
      grepl("^義大利", flag_person$mdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen1 == "本國" &
      grepl("^比利時", flag_person$mdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen1 == "本國" &
      grepl("^芬蘭", flag_person$mdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen1 == "本國" &
      grepl("^澳大利亞", flag_person$mdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen1 == "本國" &
      grepl("^泰國", flag_person$mdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen1 == "本國" &
      grepl("^美利堅合眾國", flag_person$mdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen1 == "本國" &
      grepl("^印尼", flag_person$mdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen1 == "本國" &
      grepl("^阿根廷", flag_person$mdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen1 == "本國" &
      grepl("^越南", flag_person$mdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen1 == "本國" &
      grepl("^香港", flag_person$mdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen1 == "英國" &
      grepl("^彰化", flag_person$mdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen1 == "本國" &
      grepl("^澳大利亞", flag_person$mdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen1 == "本國" &
      grepl("^荷蘭", flag_person$mdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen1 == "本國" &
      grepl("紐約", flag_person$mdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen1 == "本國" &
      grepl("州立", flag_person$mdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen1 == "本國" &
      grepl("東京", flag_person$mdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen1 == "本國" &
      grepl("波士頓", flag_person$mdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen1 == "本國" &
      grepl("路易安納", flag_person$mdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen1 == "本國" &
      grepl("關西", flag_person$mdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen1 == "本國" &
      grepl("專修大學", flag_person$mdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen1 == "本國" &
      grepl("匹茲堡", flag_person$mdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen1 == "本國" &
      grepl("俄克拉荷馬", flag_person$mdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen1 == "本國" &
      grepl("雪菲爾", flag_person$mdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen1 == "本國" &
      grepl("胡志明", flag_person$mdegreeu1),
    1,
    flag_person$err_bdeade4
  )
#碩士(二)
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen2 == "本國" &
      grepl("A", flag_person$mdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen2 == "本國" &
      grepl("E", flag_person$mdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen2 == "本國" &
      grepl("I", flag_person$mdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen2 == "本國" &
      grepl("O", flag_person$mdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen2 == "本國" &
      grepl("U", flag_person$mdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen2 == "本國" &
      grepl("a", flag_person$mdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen2 == "本國" &
      grepl("e", flag_person$mdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen2 == "本國" &
      grepl("i", flag_person$mdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen2 == "本國" &
      grepl("o", flag_person$mdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen2 == "本國" &
      grepl("u", flag_person$mdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen2 == "本國" &
      grepl("^英國", flag_person$mdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen2 == "本國" &
      grepl("^美國", flag_person$mdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen2 == "本國" &
      grepl("^加拿大", flag_person$mdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen2 == "本國" &
      grepl("^日本", flag_person$mdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen2 == "本國" &
      grepl("^韓國", flag_person$mdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen2 == "本國" &
      grepl("^菲律賓", flag_person$mdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen2 == "本國" &
      grepl("^南非", flag_person$mdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen2 == "本國" &
      grepl("^西班牙", flag_person$mdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen2 == "本國" &
      grepl("^法國", flag_person$mdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen2 == "本國" &
      grepl("^俄羅斯", flag_person$mdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen2 == "本國" &
      grepl("^德國", flag_person$mdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen2 == "本國" &
      grepl("^澳洲", flag_person$mdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen2 == "本國" &
      grepl("^紐西蘭", flag_person$mdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen2 == "本國" &
      grepl("^義大利", flag_person$mdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen2 == "本國" &
      grepl("^比利時", flag_person$mdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen2 == "本國" &
      grepl("^芬蘭", flag_person$mdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen2 == "本國" &
      grepl("^澳大利亞", flag_person$mdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen2 == "本國" &
      grepl("^泰國", flag_person$mdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen2 == "本國" &
      grepl("^美利堅合眾國", flag_person$mdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen2 == "本國" &
      grepl("^印尼", flag_person$mdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen2 == "本國" &
      grepl("^阿根廷", flag_person$mdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen2 == "本國" &
      grepl("^越南", flag_person$mdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen2 == "本國" &
      grepl("^香港", flag_person$mdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen2 == "英國" &
      grepl("^彰化", flag_person$mdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen2 == "本國" &
      grepl("^澳大利亞", flag_person$mdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen2 == "本國" &
      grepl("^荷蘭", flag_person$mdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen2 == "本國" &
      grepl("紐約", flag_person$mdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen2 == "本國" &
      grepl("州立", flag_person$mdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen2 == "本國" &
      grepl("東京", flag_person$mdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen2 == "本國" &
      grepl("波士頓", flag_person$mdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen2 == "本國" &
      grepl("路易安納", flag_person$mdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen2 == "本國" &
      grepl("關西", flag_person$mdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen2 == "本國" &
      grepl("專修大學", flag_person$mdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen2 == "本國" &
      grepl("匹茲堡", flag_person$mdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen2 == "本國" &
      grepl("俄克拉荷馬", flag_person$mdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen2 == "本國" &
      grepl("雪菲爾", flag_person$mdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$mdegreen2 == "本國" &
      grepl("胡志明", flag_person$mdegreeu2),
    1,
    flag_person$err_bdeade4
  )
#學士(一)
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen1 == "本國" &
      grepl("A", flag_person$bdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen1 == "本國" &
      grepl("E", flag_person$bdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen1 == "本國" &
      grepl("I", flag_person$bdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen1 == "本國" &
      grepl("O", flag_person$bdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen1 == "本國" &
      grepl("U", flag_person$bdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen1 == "本國" &
      grepl("a", flag_person$bdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen1 == "本國" &
      grepl("e", flag_person$bdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen1 == "本國" &
      grepl("i", flag_person$bdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen1 == "本國" &
      grepl("o", flag_person$bdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen1 == "本國" &
      grepl("u", flag_person$bdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen1 == "本國" &
      grepl("^英國", flag_person$bdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen1 == "本國" &
      grepl("^美國", flag_person$bdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen1 == "本國" &
      grepl("^加拿大", flag_person$bdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen1 == "本國" &
      grepl("^日本", flag_person$bdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen1 == "本國" &
      grepl("^韓國", flag_person$bdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen1 == "本國" &
      grepl("^菲律賓", flag_person$bdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen1 == "本國" &
      grepl("^南非", flag_person$bdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen1 == "本國" &
      grepl("^西班牙", flag_person$bdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen1 == "本國" &
      grepl("^法國", flag_person$bdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen1 == "本國" &
      grepl("^俄羅斯", flag_person$bdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen1 == "本國" &
      grepl("^德國", flag_person$bdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen1 == "本國" &
      grepl("^澳洲", flag_person$bdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen1 == "本國" &
      grepl("^紐西蘭", flag_person$bdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen1 == "本國" &
      grepl("^義大利", flag_person$bdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen1 == "本國" &
      grepl("^比利時", flag_person$bdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen1 == "本國" &
      grepl("^芬蘭", flag_person$bdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen1 == "本國" &
      grepl("^澳大利亞", flag_person$bdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen1 == "本國" &
      grepl("^泰國", flag_person$bdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen1 == "本國" &
      grepl("^美利堅合眾國", flag_person$bdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen1 == "本國" &
      grepl("^印尼", flag_person$bdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen1 == "本國" &
      grepl("^阿根廷", flag_person$bdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen1 == "本國" &
      grepl("^越南", flag_person$bdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen1 == "本國" &
      grepl("^香港", flag_person$bdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen1 == "英國" &
      grepl("^彰化", flag_person$bdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen1 == "本國" &
      grepl("^澳大利亞", flag_person$bdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen1 == "本國" &
      grepl("^荷蘭", flag_person$bdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen1 == "本國" &
      grepl("紐約", flag_person$bdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen1 == "本國" &
      grepl("州立", flag_person$bdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen1 == "本國" &
      grepl("東京", flag_person$bdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen1 == "本國" &
      grepl("波士頓", flag_person$bdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen1 == "本國" &
      grepl("路易安納", flag_person$bdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen1 == "本國" &
      grepl("關西", flag_person$bdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen1 == "本國" &
      grepl("專修大學", flag_person$bdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen1 == "本國" &
      grepl("匹茲堡", flag_person$bdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen1 == "本國" &
      grepl("俄克拉荷馬", flag_person$bdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen1 == "本國" &
      grepl("雪菲爾", flag_person$bdegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen1 == "本國" &
      grepl("胡志明", flag_person$bdegreeu1),
    1,
    flag_person$err_bdeade4
  )
#學士(二)
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen2 == "本國" &
      grepl("A", flag_person$bdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen2 == "本國" &
      grepl("E", flag_person$bdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen2 == "本國" &
      grepl("I", flag_person$bdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen2 == "本國" &
      grepl("O", flag_person$bdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen2 == "本國" &
      grepl("U", flag_person$bdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen2 == "本國" &
      grepl("a", flag_person$bdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen2 == "本國" &
      grepl("e", flag_person$bdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen2 == "本國" &
      grepl("i", flag_person$bdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen2 == "本國" &
      grepl("o", flag_person$bdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen2 == "本國" &
      grepl("u", flag_person$bdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen2 == "本國" &
      grepl("^英國", flag_person$bdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen2 == "本國" &
      grepl("^美國", flag_person$bdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen2 == "本國" &
      grepl("^加拿大", flag_person$bdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen2 == "本國" &
      grepl("^日本", flag_person$bdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen2 == "本國" &
      grepl("^韓國", flag_person$bdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen2 == "本國" &
      grepl("^菲律賓", flag_person$bdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen2 == "本國" &
      grepl("^南非", flag_person$bdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen2 == "本國" &
      grepl("^西班牙", flag_person$bdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen2 == "本國" &
      grepl("^法國", flag_person$bdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen2 == "本國" &
      grepl("^俄羅斯", flag_person$bdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen2 == "本國" &
      grepl("^德國", flag_person$bdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen2 == "本國" &
      grepl("^澳洲", flag_person$bdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen2 == "本國" &
      grepl("^紐西蘭", flag_person$bdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen2 == "本國" &
      grepl("^義大利", flag_person$bdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen2 == "本國" &
      grepl("^比利時", flag_person$bdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen2 == "本國" &
      grepl("^芬蘭", flag_person$bdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen2 == "本國" &
      grepl("^澳大利亞", flag_person$bdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen2 == "本國" &
      grepl("^泰國", flag_person$bdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen2 == "本國" &
      grepl("^美利堅合眾國", flag_person$bdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen2 == "本國" &
      grepl("^印尼", flag_person$bdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen2 == "本國" &
      grepl("^阿根廷", flag_person$bdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen2 == "本國" &
      grepl("^越南", flag_person$bdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen2 == "本國" &
      grepl("^香港", flag_person$bdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen2 == "英國" &
      grepl("^彰化", flag_person$bdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen2 == "本國" &
      grepl("^澳大利亞", flag_person$bdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen2 == "本國" &
      grepl("^荷蘭", flag_person$bdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen2 == "本國" &
      grepl("紐約", flag_person$bdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen2 == "本國" &
      grepl("州立", flag_person$bdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen2 == "本國" &
      grepl("東京", flag_person$bdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen2 == "本國" &
      grepl("波士頓", flag_person$bdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen2 == "本國" &
      grepl("路易安納", flag_person$bdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen2 == "本國" &
      grepl("關西", flag_person$bdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen2 == "本國" &
      grepl("專修大學", flag_person$bdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen2 == "本國" &
      grepl("匹茲堡", flag_person$bdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen2 == "本國" &
      grepl("俄克拉荷馬", flag_person$bdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen2 == "本國" &
      grepl("雪菲爾", flag_person$bdegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$bdegreen2 == "本國" &
      grepl("胡志明", flag_person$bdegreeu2),
    1,
    flag_person$err_bdeade4
  )
#副學士(一)
flag_person$err_bdeade4 <-
  if_else(
    flag_person$adegreen1 == "本國" &
      grepl("A", flag_person$adegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$adegreen1 == "本國" &
      grepl("E", flag_person$adegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$adegreen1 == "本國" &
      grepl("I", flag_person$adegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$adegreen1 == "本國" &
      grepl("O", flag_person$adegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$adegreen1 == "本國" &
      grepl("U", flag_person$adegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$adegreen1 == "本國" &
      grepl("a", flag_person$adegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$adegreen1 == "本國" &
      grepl("e", flag_person$adegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$adegreen1 == "本國" &
      grepl("i", flag_person$adegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$adegreen1 == "本國" &
      grepl("o", flag_person$adegreeu1),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$adegreen1 == "本國" &
      grepl("u", flag_person$adegreeu1),
    1,
    flag_person$err_bdeade4
  )
#副學士(二)
flag_person$err_bdeade4 <-
  if_else(
    flag_person$adegreen2 == "本國" &
      grepl("A", flag_person$adegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$adegreen2 == "本國" &
      grepl("E", flag_person$adegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$adegreen2 == "本國" &
      grepl("I", flag_person$adegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$adegreen2 == "本國" &
      grepl("O", flag_person$adegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$adegreen2 == "本國" &
      grepl("U", flag_person$adegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$adegreen2 == "本國" &
      grepl("a", flag_person$adegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$adegreen2 == "本國" &
      grepl("e", flag_person$adegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$adegreen2 == "本國" &
      grepl("i", flag_person$adegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$adegreen2 == "本國" &
      grepl("o", flag_person$adegreeu2),
    1,
    flag_person$err_bdeade4
  )
flag_person$err_bdeade4 <-
  if_else(
    flag_person$adegreen2 == "本國" &
      grepl("u", flag_person$adegreeu2),
    1,
    flag_person$err_bdeade4
  )

#逕讀碩士國別填本國
flag_person$err_bdeade5 <- 0
flag_person$err_bdeade5 <-
  if_else(
    flag_person$bdegreeu1 == "逕讀碩士" &
      grepl("國", flag_person$bdegreen1),
    1,
    flag_person$err_bdeade5
  )

flag_person$err_flag_sp6 <-
  flag_person$err_ddegreen1 + flag_person$err_ddegreeu1 + flag_person$err_ddegreeg1 + flag_person$err_ddegreen2 + flag_person$err_ddegreeu2 + flag_person$err_ddegreeg2 + flag_person$err_mdegreen1 + flag_person$err_mdegreeu1 + flag_person$err_mdegreeg1 + flag_person$err_mdegreen2 + flag_person$err_mdegreeu2 + flag_person$err_mdegreeg2 + flag_person$err_bdegreen1 + flag_person$err_bdegreeu1 + flag_person$err_bdegreeg1 + flag_person$err_bdegreen2 + flag_person$err_bdegreeu2 + flag_person$err_bdegreeg2 + flag_person$err_adegreen1 + flag_person$err_adegreeu1 + flag_person$err_adegreeg1 + flag_person$err_adegreen2 + flag_person$err_adegreeu2 + flag_person$err_adegreeg2 + flag_person$err_bdeade + flag_person$err_bdeade2 + flag_person$err_bdeade3 + flag_person$err_bdeade4 + flag_person$err_bdeade5 + flag_person$err_bdeade6 + flag_person$err_bdeade7

flag_person$err_flag <- 0
flag_person$err_flag <-
  if_else(flag_person$err_flag_sp6 != 0, 1, flag_person$err_flag)

#兼任或鐘點教師，次高學歷可不填

#最高學歷為碩士，但沒填學士學歷
flag_person$err_flag <-
  if_else(
    flag_person$source == 1 &
      (
        flag_person$emptype == "長期代課" |
          flag_person$emptype == "兼任" |
          flag_person$emptype == "鐘點教師"
      ) &
      flag_person$mdegreen1 != "" &
      (
        flag_person$bdegreen1 == "NA" |
          flag_person$bdegreen1 == "無法取得資料" |
          flag_person$bdegreen1 == "待查詢"
      ),
    0,
    flag_person$err_flag
  )
#最高學歷為博士，但沒填學士學歷
flag_person$err_flag <-
  if_else(
    flag_person$source == 1 &
      (
        flag_person$emptype == "長期代課" |
          flag_person$emptype == "兼任" |
          flag_person$emptype == "鐘點教師"
      ) &
      flag_person$ddegreen1 != "" &
      (
        flag_person$bdegreen1 == "NA" |
          flag_person$bdegreen1 == "無法取得資料" |
          flag_person$bdegreen1 == "待查詢"
      ),
    0,
    flag_person$err_flag
  )
#最高學歷為博士，但沒填碩士學歷
flag_person$err_flag <-
  if_else(
    flag_person$source == 1 &
      (
        flag_person$emptype == "長期代課" |
          flag_person$emptype == "兼任" |
          flag_person$emptype == "鐘點教師"
      ) &
      flag_person$ddegreen1 != "" &
      (
        flag_person$mdegreen1 == "NA" |
          flag_person$mdegreen1 == "無法取得資料" |
          flag_person$mdegreen1 == "待查詢"
      ),
    0,
    flag_person$err_flag
  )

#加註
flag_person$name <- paste(flag_person$name, "（", sep = "")
flag_person$name <-
  if_else(
    flag_person$err_ddegreen1 != 0,
    paste(
      flag_person$name,
      "博士學位畢業學校國別（一）：",
      flag_person$ddegreen1,
      "；",
      sep = ""
    ),
    flag_person$name
  )
flag_person$name <-
  if_else(
    flag_person$err_ddegreeu1 != 0,
    paste(
      flag_person$name,
      "博士學位畢業學校（一）：",
      flag_person$ddegreeu1,
      "；",
      sep = ""
    ),
    flag_person$name
  )
flag_person$name <-
  if_else(
    flag_person$err_ddegreeg1 != 0,
    paste(
      flag_person$name,
      "博士學位畢業系所（一）：",
      flag_person$ddegreeg1,
      "；",
      sep = ""
    ),
    flag_person$name
  )
flag_person$name <-
  if_else(
    flag_person$err_ddegreen2 != 0,
    paste(
      flag_person$name,
      "博士學位畢業學校國別（二）：",
      flag_person$ddegreen2,
      "；",
      sep = ""
    ),
    flag_person$name
  )
flag_person$name <-
  if_else(
    flag_person$err_ddegreeu2 != 0,
    paste(
      flag_person$name,
      "博士學位畢業學校（二）：",
      flag_person$ddegreeu2,
      "；",
      sep = ""
    ),
    flag_person$name
  )
flag_person$name <-
  if_else(
    flag_person$err_ddegreeg2 != 0,
    paste(
      flag_person$name,
      "博士學位畢業系所（二）：",
      flag_person$ddegreeg2,
      "；",
      sep = ""
    ),
    flag_person$name
  )
flag_person$name <-
  if_else(
    flag_person$err_mdegreen1 != 0,
    paste(
      flag_person$name,
      "碩士學位畢業學校國別（一）：",
      flag_person$mdegreen1,
      "；",
      sep = ""
    ),
    flag_person$name
  )
flag_person$name <-
  if_else(
    flag_person$err_mdegreeu1 != 0,
    paste(
      flag_person$name,
      "碩士學位畢業學校（一）：",
      flag_person$mdegreeu1,
      "；",
      sep = ""
    ),
    flag_person$name
  )
flag_person$name <-
  if_else(
    flag_person$err_mdegreeg1 != 0,
    paste(
      flag_person$name,
      "碩士學位畢業系所（一）：",
      flag_person$mdegreeg1,
      "；",
      sep = ""
    ),
    flag_person$name
  )
flag_person$name <-
  if_else(
    flag_person$err_mdegreen2 != 0,
    paste(
      flag_person$name,
      "碩士學位畢業學校國別（二）：",
      flag_person$mdegreen2,
      "；",
      sep = ""
    ),
    flag_person$name
  )
flag_person$name <-
  if_else(
    flag_person$err_mdegreeu2 != 0,
    paste(
      flag_person$name,
      "碩士學位畢業學校（二）：",
      flag_person$mdegreeu2,
      "；",
      sep = ""
    ),
    flag_person$name
  )
flag_person$name <-
  if_else(
    flag_person$err_mdegreeg2 != 0,
    paste(
      flag_person$name,
      "碩士學位畢業系所（二）：",
      flag_person$mdegreeg2,
      "；",
      sep = ""
    ),
    flag_person$name
  )
flag_person$name <-
  if_else(
    flag_person$err_bdegreen1 != 0,
    paste(
      flag_person$name,
      "學士學位畢業學校國別（一）：",
      flag_person$bdegreen1,
      "；",
      sep = ""
    ),
    flag_person$name
  )
flag_person$name <-
  if_else(
    flag_person$err_bdegreeu1 != 0,
    paste(
      flag_person$name,
      "學士學位畢業學校（一）：",
      flag_person$bdegreeu1,
      "；",
      sep = ""
    ),
    flag_person$name
  )
flag_person$name <-
  if_else(
    flag_person$err_bdegreeg1 != 0,
    paste(
      flag_person$name,
      "學士學位畢業科系（一）：",
      flag_person$bdegreeg1,
      "；",
      sep = ""
    ),
    flag_person$name
  )
flag_person$name <-
  if_else(
    flag_person$err_bdegreen2 != 0,
    paste(
      flag_person$name,
      "學士學位畢業學校國別（二）：",
      flag_person$bdegreen2,
      "；",
      sep = ""
    ),
    flag_person$name
  )
flag_person$name <-
  if_else(
    flag_person$err_bdegreeu2 != 0,
    paste(
      flag_person$name,
      "學士學位畢業學校（二）：",
      flag_person$bdegreeu2,
      "；",
      sep = ""
    ),
    flag_person$name
  )
flag_person$name <-
  if_else(
    flag_person$err_bdegreeg2 != 0,
    paste(
      flag_person$name,
      "學士學位畢業科系（二）：",
      flag_person$bdegreeg2,
      "；",
      sep = ""
    ),
    flag_person$name
  )
flag_person$name <-
  if_else(
    flag_person$err_adegreen1 != 0,
    paste(
      flag_person$name,
      "副學士或專科畢業學校國別（一）：",
      flag_person$adegreen1,
      "；",
      sep = ""
    ),
    flag_person$name
  )
flag_person$name <-
  if_else(
    flag_person$err_adegreeu1 != 0,
    paste(
      flag_person$name,
      "副學士或專科畢業學校（一）：",
      flag_person$adegreeu1,
      "；",
      sep = ""
    ),
    flag_person$name
  )
flag_person$name <-
  if_else(
    flag_person$err_adegreeg1 != 0,
    paste(
      flag_person$name,
      "副學士或專科畢業科系（一）：",
      flag_person$adegreeg1,
      "；",
      sep = ""
    ),
    flag_person$name
  )
flag_person$name <-
  if_else(
    flag_person$err_adegreen2 != 0,
    paste(
      flag_person$name,
      "副學士或專科畢業學校國別（二）：",
      flag_person$adegreen2,
      "；",
      sep = ""
    ),
    flag_person$name
  )
flag_person$name <-
  if_else(
    flag_person$err_adegreeu2 != 0,
    paste(
      flag_person$name,
      "副學士或專科畢業學校（二）：",
      flag_person$adegreeu2,
      "；",
      sep = ""
    ),
    flag_person$name
  )
flag_person$name <-
  if_else(
    flag_person$err_adegreeg2 != 0,
    paste(
      flag_person$name,
      "副學士或專科畢業科系（二）：",
      flag_person$adegreeg2,
      "；",
      sep = ""
    ),
    flag_person$name
  )
flag_person$name <-
  if_else(
    flag_person$err_bdeade != 0,
    paste(
      flag_person$name,
      "副學士或專科畢業學校國別（一）：",
      flag_person$adegreen1,
      "、副學士或專科畢業學校（一）：",
      flag_person$adegreeu1,
      "、副學士或專科畢業科系（一）：",
      flag_person$adegreeg1,
      " (若逕讀碩士，副學士或專科畢業資訊應不為N)",
      sep = ""
    ),
    flag_person$name
  )
flag_person$name <-
  if_else(
    flag_person$err_bdeade2 != 0,
    paste(flag_person$name, "（若逕讀博士，學士畢業資訊應不為N）", sep = ""),
    flag_person$name
  )
flag_person$name <-
  if_else(
    flag_person$err_bdeade3 != 0,
    paste(flag_person$name, "（若逕讀博士，學士或專科畢業資訊應不為逕讀博士）", sep = ""),
    flag_person$name
  )
flag_person$name <-
  if_else(
    flag_person$err_bdeade4 != 0,
    paste(flag_person$name, "若於外國學校取得學位，其學位畢業學校國別不應為本國", sep = ""),
    flag_person$name
  )
flag_person$name <-
  if_else(
    flag_person$err_bdeade5 != 0,
    paste(flag_person$name, "若為逕讀碩士，相關欄位請依欄位說明填寫", sep = ""),
    flag_person$name
  )
flag_person$name <-
  if_else(
    flag_person$err_bdeade6 != 0,
    paste(flag_person$name, "若為逕讀碩士，應填列碩士學歷相關資訊", sep = ""),
    flag_person$name
  )
flag_person$name <-
  if_else(
    flag_person$err_bdeade7 != 0,
    paste(flag_person$name, "若為逕讀博士，應填列博士學歷相關資訊", sep = ""),
    flag_person$name
  )
flag_person$name <- paste(flag_person$name, "）", sep = "")
flag_person$name <- gsub("；）", replacement = "）", flag_person$name)
flag_person$name <- gsub("（）", replacement = "", flag_person$name)

# （請再協助確認上述人員畢業學校正確完整名稱，或修正錯字）
# （請確認畢業科系正確名稱）
# （請確認並修正填寫畢業科系正確全稱）
# （請確認畢業系所正確名稱）
# （請確認並修正填寫畢業系所與班別正確全稱）
# （請確認畢業系所正確全稱，且學分班結業非取得學位）
# （請確認*員『*士學位畢業科系』正確全稱，如為學分班結業則非取得學位）
# （請確認畢業科系所正確全稱，若未取得畢業/學位證書，則不認定取得該學位。另，各學位別學歷資訊請依欄位說明，按各學位別欄位分別填寫）
# （請確認畢業學校正確名稱）
# （請確認畢業學校所在國家正確名稱）
# （請依*員畢業證書確認畢業學校正確名稱）
# （請確認並修正填寫畢業學校正確名稱。且若副學士或專科畢業學校為(科技/空中)大學、(技術)學院或其他技職校院，且確認為專科學制，請於「副學士或專科畢業學校」欄位中在校名後註記專科學制或專科部）
# （請依照*員『*士』學位畢業證書，修正填寫正確之學校名稱。）
# （請依照*員『*士』學位畢業證書，修正填寫正確之系所名稱。）
# （請依照*員『*士』學位畢業證書，修正填寫正確之科系名稱。）
# （請依照*員『*士』學位畢業證書，修正填寫正確之科系名稱，或刪除贅字。）
# （請確認*員畢業學校正確名稱及拼字是否正確）
# （請依照上開人員碩士學位畢業證書，修正填寫正確之系所名稱全稱。）
# （若該名教員似具二個以上學士學位，如確取得二個學士學位，則請按取得學位年月日，由近而遠(由最近取得至次近取得)依序分別填寫『學士學位畢業科系(一) 』(最近)、『學士學位畢業科系(二) 』(次近)，倘無法區分時間先後順序，如雙主修，亦請皆填列。）
#
#（請確認並正確填列『*士學位畢業科系』名稱，此欄位不需填入學校名稱。）
#（請依照*員『*士』學位畢業證書，修正填寫正確之學校名稱，且學校與系所之名稱應依欄位說明按欄位分別填寫，或刪除贅字。）
#（請務必依照上述人員學位/畢業證書，並按欄位說明修正填寫正確之畢業學校國家名稱、學校名稱、科系所名稱，或刪除贅字。）

# 40學分班之文字
# （學分班非屬『學位授予法』規定之學位別，且依該法規定，須修業期滿、修滿應修學分並符畢業條件，始能獲頒學位。若*員經確認未獲碩士學位，請於碩士學位畢業學校國別、畢業學校、畢業系所三欄填『N』）
# （學分班非屬『學位授予法』規定之學位別，且依該法規定，須修業期滿、修滿應修學分並符畢業條件，始能獲頒學位。若*員經確認未獲學士學位，請於學士學位畢業學校國別、畢業學校、畢業科系三欄填『N』）

#高中學歷
# （請確認*員最高學歷，若*員最高學歷不為大專以上，「最高學歷是否為大專以上」及各級學歷資訊欄位請皆填「N」。）

#逕讀碩士
#（請確認並修正張員之學士學位各欄位資訊，若張員以副學士學位或專科學歷，就讀研究所取得碩士學位，則學士學位相關欄位資料，請直接填寫「逕讀碩士」）


#呈現姓名
flag_person$err_flag_txt <- ""
flag_person$err_flag_txt <-
  case_when(flag_person$err_flag == 1 ~ flag_person$name,
            TRUE ~ flag_person$err_flag_txt)

if (dim(flag_person %>% subset(err_flag == 1))[1] != 0) {
  #根據organization_id + source，展開成寬資料(wide)
  flag_person_wide_spe6 <- flag_person %>%
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
    colnames(flag_person_wide_spe6)[3:length(colnames(flag_person_wide_spe6))]
  flag_person_wide_spe6$spe6_r <- NA
  for (i in temp) {
    flag_person_wide_spe6$spe6_r <-
      paste(flag_person_wide_spe6$spe6_r,
            flag_person_wide_spe6[[i]],
            sep = " ")
  }
  flag_person_wide_spe6$spe6_r <-
    gsub("NA ", replacement = "", flag_person_wide_spe6$spe6_r)
  flag_person_wide_spe6$spe6_r <-
    gsub(" NA", replacement = "", flag_person_wide_spe6$spe6_r)
  
  #產生檢誤報告文字
  spe6_temp <- flag_person_wide_spe6 %>%
    group_by(organization_id) %>%
    mutate(spe6_txt = paste(source, "之大學（學士）以上各教育階段學歷資料不完整或不正確：", spe6_r, sep = ""),
           "") %>%
    subset(select = c(organization_id, spe6_txt)) %>%
    distinct(organization_id, spe6_txt)
  
  #根據organization_id，展開成寬資料(wide)
  spe6 <- spe6_temp %>%
    dcast(organization_id ~ spe6_txt, value.var = "spe6_txt")
  
  #合併教員資料表及職員(工)資料表報告文字
  temp <- colnames(spe6)[2:length(colnames(spe6))]
  spe6$spe6 <- NA
  for (i in temp) {
    spe6$spe6 <- paste(spe6$spe6, spe6[[i]], sep = "； ")
  }
  spe6$spe6 <- gsub("NA； ", replacement = "", spe6$spe6)
  spe6$spe6 <- gsub("； NA", replacement = "", spe6$spe6)
  
  #產生檢誤報告文字
  spe6 <- spe6 %>%
    subset(select = c(organization_id, spe6)) %>%
    distinct(organization_id, spe6)
} else{
  #偵測spe6是否存在。若不存在，則產生NA行
  if ('spe6' %in% ls()) {
    print("spe6")
  } else{
    spe6 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    spe6$spe6 <- ""
  }
}
