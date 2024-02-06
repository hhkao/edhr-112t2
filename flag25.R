# flag25: 請確認並依欄位說明填寫高中主要任教科別之全稱。如為共同學科，請分科填寫，例如：國文科、歷史科、生物科等；如為專業群科，不需填寫群別，但亦請分科填寫，例如：鑄造科、汽車科、烘焙科等。 -------------------------------------------------------------------
flag_person <- drev_P_load %>%
  subset(load == 1)

# 主要任教科別 ------------------------------------------------------------------
flag_person$depcode_p <- ""

#科
flag_person$depcode_p <-
  if_else((
    grepl("機械", flag_person$mainsub) |
      grepl("機工", flag_person$mainsub)
  ) &
    (
      flag_person$typeM == 1 |
        flag_person$typeV == 1 |
        flag_person$typeC == 1
    ) ,
  "301",
  flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else((
    grepl("資訊", flag_person$mainsub) |
      grepl("電腦", flag_person$mainsub) |
      grepl("計算機", flag_person$mainsub) |
      grepl("計概", flag_person$mainsub) |
      grepl("電算", flag_person$mainsub) |
      grepl("電子計算", flag_person$mainsub)
  ) &
    (
      flag_person$typeM == 1 |
        flag_person$typeV == 1 |
        flag_person$typeC == 1
    ) ,
  "305",
  flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("電機", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "308",
    flag_person$depcode_p
  )

flag_person$depcode_p <-
  if_else(
    grepl("農場經營", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "201",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("園藝", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "202",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("森林", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "204",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("農業機械", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "205",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("食品加工", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "206",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else((
    grepl("野生", flag_person$mainsub) |
      grepl("動物", flag_person$mainsub)
  ) &
    (
      flag_person$typeM == 1 |
        flag_person$typeV == 1 |
        flag_person$typeC == 1
    ) ,
  "214",
  flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("農產", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "215",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("造園", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "216",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else((
    grepl("畜產", flag_person$mainsub) |
      grepl("畜保", flag_person$mainsub)
  ) &
    (
      flag_person$typeM == 1 |
        flag_person$typeV == 1 |
        flag_person$typeC == 1
    ) ,
  "217",
  flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else((
    grepl("鑄造", flag_person$mainsub) |
      grepl("鑄（造）工科", flag_person$mainsub)
  ) &
    (
      flag_person$typeM == 1 |
        flag_person$typeV == 1 |
        flag_person$typeC == 1
    ) ,
  "302",
  flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("汽車", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "303",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("板金", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "304",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("電子", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "306",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("控制", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "307",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("冷凍", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "309",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("建築", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "311",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("木工", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "312",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("化工", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "315",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("美工", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "316",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("美術工藝", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "318",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("紡織", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "319",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("電機空調", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "321",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("木模", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "332",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("配管", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "337",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("模具", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "338",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("染整", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "352",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("^機電", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "360",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else((
    grepl("陶瓷", flag_person$mainsub) |
      grepl("陶工", flag_person$mainsub)
  ) &
    (
      flag_person$typeM == 1 |
        flag_person$typeV == 1 |
        flag_person$typeC == 1
    ) ,
  "361",
  flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("製圖", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "363",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("重機", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "364",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("土木", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "365",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("室內空間", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "366",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("環境檢驗", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "367",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("生物產業機電", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "372",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else((
    grepl("圖文", flag_person$mainsub) |
      grepl("傳播", flag_person$mainsub)
  ) &
    (
      flag_person$typeM == 1 |
        flag_person$typeV == 1 |
        flag_person$typeC == 1
    ) ,
  "373",
  flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("電腦機械製圖", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "374",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else((
    grepl("軌道", flag_person$mainsub) |
      grepl("車輛", flag_person$mainsub)
  ) &
    (
      flag_person$typeM == 1 |
        flag_person$typeV == 1 |
        flag_person$typeC == 1
    ) ,
  "380",
  flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else((
    grepl("飛機修護", flag_person$mainsub) |
      grepl("飛修", flag_person$mainsub) |
      grepl("飛機", flag_person$mainsub)
  ) &
    (
      flag_person$typeM == 1 |
        flag_person$typeV == 1 |
        flag_person$typeC == 1
    ) ,
  "381",
  flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("航空", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "384",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("動力", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "392",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("消防", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "397",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else((
    grepl("測繪", flag_person$mainsub) |
      grepl("測量", flag_person$mainsub)
  ) &
    (
      flag_person$typeM == 1 |
        flag_person$typeV == 1 |
        flag_person$typeC == 1
    ) ,
  "398",
  flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("家具設計", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "399",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else((
    grepl("商業", flag_person$mainsub) |
      grepl("商經", flag_person$mainsub)
  ) &
    (
      flag_person$typeM == 1 |
        flag_person$typeV == 1 |
        flag_person$typeC == 1
    ) ,
  "401",
  flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else((
    grepl("國際貿易", flag_person$mainsub) |
      grepl("國貿", flag_person$mainsub)
  ) &
    (
      flag_person$typeM == 1 |
        flag_person$typeV == 1 |
        flag_person$typeC == 1
    ) ,
  "402",
  flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("會計", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "403",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else((
    grepl("資料處理", flag_person$mainsub) |
      grepl("資處", flag_person$mainsub)
  ) &
    (
      flag_person$typeM == 1 |
        flag_person$typeV == 1 |
        flag_person$typeC == 1
    ) ,
  "404",
  flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else((
    grepl("廣告", flag_person$mainsub) |
      grepl("廣設", flag_person$mainsub)
  ) &
    (
      flag_person$typeM == 1 |
        flag_person$typeV == 1 |
        flag_person$typeC == 1
    ) ,
  "406",
  flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else((
    grepl("觀光", flag_person$mainsub) |
      grepl("顴光", flag_person$mainsub)
  ) &
    (
      flag_person$typeM == 1 |
        flag_person$typeV == 1 |
        flag_person$typeC == 1
    ) ,
  "407",
  flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else((
    grepl("餐飲", flag_person$mainsub) |
      grepl("餐管", flag_person$mainsub)
  ) &
    (
      flag_person$typeM == 1 |
        flag_person$typeV == 1 |
        flag_person$typeC == 1
    ) ,
  "408",
  flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("不動產", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "418",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else((
    grepl("電子商務", flag_person$mainsub) |
      grepl("電商", flag_person$mainsub)
  ) &
    (
      flag_person$typeM == 1 |
        flag_person$typeV == 1 |
        flag_person$typeC == 1
    ) ,
  "425",
  flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("流通", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "426",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("多媒體設計", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "430",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("多媒體應用", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "431",
    flag_person$depcode_p
  )

flag_person$depcode_p <-
  if_else(
    grepl("英語文", flag_person$mainsub) |
      grepl("英語", flag_person$mainsub) |
      grepl("英文", flag_person$mainsub) |
      grepl("英文科", flag_person$mainsub),
    "02",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(((
    grepl("應用英語", flag_person$mainsub) |
      grepl("應英", flag_person$mainsub)
  ) |
    ((
      grepl("應用外語", flag_person$mainsub) |
        grepl("應外", flag_person$mainsub)
    ) &
      (
        grepl("英語", flag_person$mainsub) |
          grepl("英文", flag_person$mainsub)
      ))) &
    (
      flag_person$typeM == 1 |
        flag_person$typeV == 1 |
        flag_person$typeC == 1
    ) ,
  "433",
  flag_person$depcode_p
  )
#flag_person$depcode_p <- if_else(((grepl("應用英語", flag_person$mainsub) | grepl("應英", flag_person$mainsub)) | ((grepl("應用外語", flag_person$mainsub) | grepl("應外", flag_person$mainsub)) & (grepl("英語", flag_person$mainsub) | grepl("英文", flag_person$mainsub)))) & (flag_person$typeM == 0 & flag_person$typeV == 0 & flag_person$typeC == 0) , "02", flag_person$depcode_p)

flag_person$depcode_p <-
  if_else((grepl("應用日語", flag_person$mainsub) |
             (
               grepl("應用外語", flag_person$mainsub) &
                 (
                   grepl("日語", flag_person$mainsub) |
                     grepl("日文", flag_person$mainsub)
                 )
             )) &
            (
              flag_person$typeM == 1 |
                flag_person$typeV == 1 |
                flag_person$typeC == 1
            ) ,
          "434",
          flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else((
    grepl("日語", flag_person$mainsub) |
      grepl("日文", flag_person$mainsub) |
      grepl("應日", flag_person$mainsub)
  ) &
    (
      flag_person$typeM == 1 |
        flag_person$typeV == 1 |
        flag_person$typeC == 1
    ) ,
  "434",
  flag_person$depcode_p
  )

flag_person$depcode_p <-
  if_else(
    grepl("家政科", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "501",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("服裝", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "502",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else((
    grepl("幼兒", flag_person$mainsub) |
      grepl("幼保", flag_person$mainsub)
  ) &
    (
      flag_person$typeM == 1 |
        flag_person$typeV == 1 |
        flag_person$typeC == 1
    ) ,
  "503",
  flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("美容", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "504",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("^食品科$", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "505",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else((
    grepl("室內設計", flag_person$mainsub) |
      grepl("室設", flag_person$mainsub)
  ) &
    (
      flag_person$typeM == 1 |
        flag_person$typeV == 1 |
        flag_person$typeC == 1
    ) ,
  "512",
  flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("時尚模特兒", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "513",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else((
    grepl("流行", flag_person$mainsub) |
      grepl("服飾", flag_person$mainsub)
  ) &
    (
      flag_person$typeM == 1 |
        flag_person$typeV == 1 |
        flag_person$typeC == 1
    ) ,
  "515",
  flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("時尚造型", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "516",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("烘焙", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "517",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("漁業", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "701",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("輪機", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "702",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("通信", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "703",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("養殖", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "705",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("水產經營", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "706",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("航海", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "708",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("航運", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "717",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("水產食品", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "718",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("戲劇", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "801",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("音樂科", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "802",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("舞蹈", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "803",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("美術科", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "804",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("影劇", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "806",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("西樂", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "807",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("國樂", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "808",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("劇場", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "813",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else((
    grepl("電影", flag_person$mainsub) |
      grepl("電視", flag_person$mainsub)
  ) &
    (
      flag_person$typeM == 1 |
        flag_person$typeV == 1 |
        flag_person$typeC == 1
    ) ,
  "816",
  flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else((
    grepl("表演", flag_person$mainsub) |
      grepl("表藝", flag_person$mainsub)
  ) &
    (
      flag_person$typeM == 1 |
        flag_person$typeV == 1 |
        flag_person$typeC == 1
    ) ,
  "817",
  flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("動畫", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "820",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("時尚工藝", flag_person$mainsub) &
      (
        flag_person$typeM == 1 |
          flag_person$typeV == 1 |
          flag_person$typeC == 1
      ) ,
    "822",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    (flag_person$mainsub == "資訊應用學程" |
       flag_person$mainsub == "資訊應用科") &
      flag_person$typeM == 1 ,
    "F01",
    flag_person$depcode_p
  )

#一般科目
flag_person$depcode_p <-
  if_else(grepl("國語文", flag_person$mainsub),
          "01",
          flag_person$depcode_p)

#第二外國語文/第二外語：不可以僅填第二外國語文/第二外國語/第二外語，後方要填語文種類
flag_person$depcode_p <-
  if_else((
    grepl("日語", flag_person$mainsub) |
      grepl("日文", flag_person$mainsub)
  ) &
    (
      flag_person$typeM == 0 &
        flag_person$typeV == 0 &
        flag_person$typeC == 0
    ) ,
  "24",
  flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else((
    grepl("韓語", flag_person$mainsub) |
      grepl("韓文", flag_person$mainsub)
  ), "24", flag_person$depcode_p)
flag_person$depcode_p <-
  if_else((
    grepl("法語", flag_person$mainsub) |
      grepl("法文", flag_person$mainsub)
  ), "24", flag_person$depcode_p)
flag_person$depcode_p <-
  if_else((
    grepl("西班牙語", flag_person$mainsub) |
      grepl("西班牙文", flag_person$mainsub)
  ), "24", flag_person$depcode_p)
flag_person$depcode_p <-
  if_else((
    grepl("德語", flag_person$mainsub) |
      grepl("德文", flag_person$mainsub)
  ), "24", flag_person$depcode_p)
flag_person$depcode_p <-
  if_else((
    grepl("西語", flag_person$mainsub) |
      grepl("西文", flag_person$mainsub)
  ), "24", flag_person$depcode_p)
flag_person$depcode_p <-
  if_else((
    grepl("義大利語", flag_person$mainsub) |
      grepl("義大利文", flag_person$mainsub)
  ), "24", flag_person$depcode_p)
flag_person$depcode_p <-
  if_else((
    grepl("俄語", flag_person$mainsub) |
      grepl("俄文", flag_person$mainsub)
  ), "24", flag_person$depcode_p)
flag_person$depcode_p <-
  if_else((
    grepl("拉丁語", flag_person$mainsub) |
      grepl("拉丁文", flag_person$mainsub)
  ), "24", flag_person$depcode_p)

#臺灣手語
flag_person$depcode_p <-
  if_else(
    grepl("臺灣手語", flag_person$mainsub) |
      grepl("台灣手語", flag_person$mainsub),
    "23",
    flag_person$depcode_p
  )

#新住民語：越南、印尼、泰國、緬甸、柬埔寨、菲律賓、馬來西亞
flag_person$depcode_p <-
  if_else(grepl("新住民", flag_person$mainsub),
          "22",
          flag_person$depcode_p)
flag_person$depcode_p <-
  if_else(grepl("越", flag_person$mainsub), "22", flag_person$depcode_p)
flag_person$depcode_p <-
  if_else(grepl("印", flag_person$mainsub), "22", flag_person$depcode_p)
flag_person$depcode_p <-
  if_else(grepl("泰", flag_person$mainsub), "22", flag_person$depcode_p)
flag_person$depcode_p <-
  if_else(grepl("緬", flag_person$mainsub), "22", flag_person$depcode_p)
flag_person$depcode_p <-
  if_else(grepl("柬", flag_person$mainsub), "22", flag_person$depcode_p)
flag_person$depcode_p <-
  if_else(grepl("菲", flag_person$mainsub), "22", flag_person$depcode_p)
flag_person$depcode_p <-
  if_else(grepl("馬來", flag_person$mainsub), "22", flag_person$depcode_p)

#本土語：不可以僅填本土語文/本土語，後方要填語文種類
flag_person$depcode_p <-
  if_else(grepl("閩南", flag_person$mainsub), "21", flag_person$depcode_p)
flag_person$depcode_p <-
  if_else(grepl("閔南", flag_person$mainsub), "21", flag_person$depcode_p)
flag_person$depcode_p <-
  if_else(grepl("閩東語", flag_person$mainsub),
          "21",
          flag_person$depcode_p)
flag_person$depcode_p <-
  if_else((
    grepl("客語", flag_person$mainsub) |
      grepl("客家", flag_person$mainsub)
  ), "21", flag_person$depcode_p)
flag_person$depcode_p <-
  if_else(grepl("阿美", flag_person$mainsub), "21", flag_person$depcode_p)
flag_person$depcode_p <-
  if_else(grepl("排灣", flag_person$mainsub), "21", flag_person$depcode_p)
flag_person$depcode_p <-
  if_else(grepl("泰雅", flag_person$mainsub), "21", flag_person$depcode_p)
flag_person$depcode_p <-
  if_else(grepl("布農", flag_person$mainsub), "21", flag_person$depcode_p)
flag_person$depcode_p <-
  if_else(grepl("卑南", flag_person$mainsub), "21", flag_person$depcode_p)
flag_person$depcode_p <-
  if_else(grepl("魯凱", flag_person$mainsub), "21", flag_person$depcode_p)
flag_person$depcode_p <-
  if_else(grepl("賽夏", flag_person$mainsub), "21", flag_person$depcode_p)
flag_person$depcode_p <-
  if_else(grepl("鄒族", flag_person$mainsub), "21", flag_person$depcode_p)
flag_person$depcode_p <-
  if_else(grepl("達悟", flag_person$mainsub), "21", flag_person$depcode_p)
flag_person$depcode_p <-
  if_else(grepl("雅美", flag_person$mainsub), "21", flag_person$depcode_p)
flag_person$depcode_p <-
  if_else(grepl("邵族", flag_person$mainsub), "21", flag_person$depcode_p)
flag_person$depcode_p <-
  if_else(grepl("噶瑪蘭", flag_person$mainsub),
          "21",
          flag_person$depcode_p)
flag_person$depcode_p <-
  if_else(grepl("太魯閣", flag_person$mainsub),
          "21",
          flag_person$depcode_p)
flag_person$depcode_p <-
  if_else(grepl("撒奇萊雅", flag_person$mainsub),
          "21",
          flag_person$depcode_p)
flag_person$depcode_p <-
  if_else(grepl("賽德克", flag_person$mainsub),
          "21",
          flag_person$depcode_p)
flag_person$depcode_p <-
  if_else(grepl("拉阿魯哇", flag_person$mainsub),
          "21",
          flag_person$depcode_p)
flag_person$depcode_p <-
  if_else(grepl("卡那卡那富", flag_person$mainsub),
          "21",
          flag_person$depcode_p)
flag_person$depcode_p <-
  if_else(grepl("賽考利克", flag_person$mainsub),
          "21",
          flag_person$depcode_p)

flag_person$depcode_p <-
  if_else(grepl("國文", flag_person$mainsub), "01", flag_person$depcode_p)
flag_person$depcode_p <-
  if_else(grepl("數學", flag_person$mainsub), "03", flag_person$depcode_p)
flag_person$depcode_p <-
  if_else(grepl("歷史", flag_person$mainsub), "09", flag_person$depcode_p)
flag_person$depcode_p <-
  if_else(grepl("地理", flag_person$mainsub), "0A", flag_person$depcode_p)
flag_person$depcode_p <-
  if_else(grepl("公民", flag_person$mainsub), "0B", flag_person$depcode_p)

flag_person$depcode_p <-
  if_else(grepl("物理", flag_person$mainsub), "0C", flag_person$depcode_p)
flag_person$depcode_p <-
  if_else(grepl("化學", flag_person$mainsub), "0D", flag_person$depcode_p)
flag_person$depcode_p <-
  if_else(grepl("生物", flag_person$mainsub), "0E", flag_person$depcode_p)
flag_person$depcode_p <-
  if_else((
    grepl("地球科學", flag_person$mainsub) |
      grepl("地科", flag_person$mainsub)
  ), "0F", flag_person$depcode_p)

flag_person$depcode_p <-
  if_else(
    grepl("^音樂$", flag_person$mainsub) &
      (
        flag_person$typeM == 0 &
          flag_person$typeV == 0 &
          flag_person$typeC == 0
      ),
    "0U",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("美術", flag_person$mainsub) &
      (
        flag_person$typeM == 0 &
          flag_person$typeV == 0 &
          flag_person$typeC == 0
      ),
    "0V",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("藝術生活", flag_person$mainsub) |
      grepl("藝術與生活", flag_person$mainsub) |
      grepl("藝術生活科", flag_person$mainsub) |
      grepl("藝術科", flag_person$mainsub) |
      grepl("視覺應用", flag_person$mainsub) |
      grepl("音樂應用", flag_person$mainsub) |
      grepl("表演藝術", flag_person$mainsub) |
      grepl("表藝", flag_person$mainsub),
    "0W",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(grepl("生命教育", flag_person$mainsub) ,
          "0X",
          flag_person$depcode_p)
flag_person$depcode_p <-
  if_else(grepl("生涯", flag_person$mainsub) , "0Y", flag_person$depcode_p)
flag_person$depcode_p <-
  if_else(
    grepl("家政", flag_person$mainsub) &
      (
        flag_person$typeM == 0 &
          flag_person$typeV == 0 &
          flag_person$typeC == 0
      ),
    "0Z",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(grepl("法律與生活", flag_person$mainsub),
          "10",
          flag_person$depcode_p)
flag_person$depcode_p <-
  if_else(grepl("環境科學", flag_person$mainsub),
          "11",
          flag_person$depcode_p)
flag_person$depcode_p <-
  if_else(
    grepl("生活科技", flag_person$mainsub) |
      grepl("生科", flag_person$mainsub),
    "12",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else((grepl("資訊", flag_person$mainsub)) &
            (
              flag_person$typeM == 0 &
                flag_person$typeV == 0 &
                flag_person$typeC == 0
            ) ,
          "13",
          flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("健康", flag_person$mainsub) |
      grepl("護理", flag_person$mainsub) |
      grepl("健護", flag_person$mainsub),
    "14",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else((grepl("體育", flag_person$mainsub)), "15", flag_person$depcode_p)
flag_person$depcode_p <-
  if_else(
    grepl("國防", flag_person$mainsub) |
      grepl("軍訓", flag_person$mainsub),
    "16",
    flag_person$depcode_p
  )

#新增"特殊教育"領域，科目別又分「身心障礙」(929)、「資賦優異」(930)、「特殊教育-其他」(931)
flag_person$depcode_p <-
  if_else(
    grepl("特殊教育", flag_person$mainsub) |
      grepl("特教", flag_person$mainsub),
    "931",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("身心障礙", flag_person$mainsub) |
      grepl("身障", flag_person$mainsub) |
      grepl("障礙", flag_person$mainsub),
    "929",
    flag_person$depcode_p
  )
flag_person$depcode_p <-
  if_else(
    grepl("資賦優異", flag_person$mainsub) |
      grepl("資優", flag_person$mainsub) ,
    "930",
    flag_person$depcode_p
  )

flag_person$depcode_p <-
  if_else((
    grepl("輔導", flag_person$mainsub) |
      grepl("專輔", flag_person$mainsub)
  ), "0AA", flag_person$depcode_p)

flag_person$depcode_p <-
  if_else(grepl("不分科", flag_person$mainsub),
          "XY",
          flag_person$depcode_p)



#轉中文
flag_person$dep <- ""
flag_person$dep <-
  if_else(flag_person$depcode_p == "01", "國語文", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "02", "英語文", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "21", "本土語文", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "22", "新住民語文", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "23", "臺灣手語", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "24", "第二外國語文", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "14", "健康與護理", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "15", "體育", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "03", "數學", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "12", "生活科技", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "13", "資訊科技", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "0X", "生命教育", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "0Y", "生涯規劃", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "0Z", "家政", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "0AA", "輔導", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "0AB", "童軍", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "929", "身心障礙", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "930", "資賦優異", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "931", "特殊教育-其他", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "10", "法律與生活", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "11", "環境科學概論", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "0U", "音樂", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "0V", "美術", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "0W", "藝術生活", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "09", "歷史", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "0A", "地理", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "0B", "公民與社會", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "0C", "物理", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "0D", "化學", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "0CD", "理化", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "0E", "生物", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "0F", "地球科學", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "0G", "自然科學-其他", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "16", "全民國防教育", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "301", "機械科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "302", "鑄造科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "304", "板金科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "332", "機械木模科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "337", "配管科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "338", "模具科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "360", "機電科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "363", "製圖科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "372", "生物產業機電科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "374", "電腦機械製圖科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "303", "汽車科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "364", "重機科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "381", "飛機修護科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "392", "動力機械科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "205", "農業機械科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "380", "軌道車輛科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "315", "化工科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "319", "紡織科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "352", "染整科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "367", "環境檢驗科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "305", "資訊科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "306", "電子科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "307", "控制科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "308", "電機科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "309", "冷凍空調科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "384", "航空電子科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "703", "電子通信科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "321", "電機空調科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "311", "建築科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "365", "土木科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "397", "消防工程科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "398", "空間測繪科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "401", "商業經營科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "402", "國際貿易科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "403", "會計事務科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "404", "資料處理科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "425", "電子商務科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "426", "流通管理科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "215", "農產行銷科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "717", "航運管理科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "706", "水產經營科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "418", "不動產事務科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "433", "應用英語科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "434", "應用日語科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "312", "家具木工科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "316", "美工科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "361", "陶瓷工程科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "366", "室內空間設計科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "373", "圖文傳播科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "394", "金屬工藝科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "399", "家具設計科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "406", "廣告設計科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "430", "多媒體設計科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "512", "室內設計科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "431", "多媒體應用科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "318", "美術工藝科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "801", "戲劇科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "802", "音樂科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "803", "舞蹈科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "804", "美術科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "806", "影劇科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "807", "西樂科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "808", "國樂科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "816", "電影電視科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "817", "表演藝術科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "820", "多媒體動畫科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "822", "時尚工藝科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "813", "劇場藝術科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "201", "農場經營科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "202", "園藝科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "204", "森林科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "214", "野生動物保育科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "216", "造園科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "217", "畜產保健科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "206", "食品加工科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "505", "食品科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "718", "水產食品科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "517", "烘焙科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "407", "觀光事業科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "408", "餐飲管理科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "501", "家政科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "502", "服裝科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "503", "幼兒保育科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "504", "美容科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "513", "時尚模特兒科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "515", "流行服飾科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "516", "時尚造型科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "702", "輪機科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "708", "航海科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "701", "漁業科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "705", "水產養殖科", flag_person$dep)
flag_person$dep <-
  if_else(flag_person$depcode_p == "XY", "不分科", flag_person$dep)

#---------------

flag_person$err_flag <- 0
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      flag_person$load == 1 &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ) & flag_person$depcode_p == "",
    1,
    flag_person$err_flag
  )

#可以填綜合職能科
flag_person$err_flag <-
  if_else(grepl("綜合職能", flag_person$mainsub), 0, flag_person$err_flag)

#不可以填混搭名稱
flag_person$err_flag <-
  if_else((
    grepl("第二外", flag_person$mainsub) |
      grepl("日", flag_person$mainsub) |
      grepl("韓", flag_person$mainsub) |
      grepl("法", flag_person$mainsub) |
      grepl("西", flag_person$mainsub) |
      grepl("德", flag_person$mainsub) |
      grepl("義", flag_person$mainsub) |
      grepl("俄", flag_person$mainsub) |
      grepl("拉丁", flag_person$mainsub)
  ) & grepl("手語", flag_person$mainsub),
  1,
  flag_person$err_flag
  )
flag_person$err_flag <-
  if_else((
    grepl("第二外", flag_person$mainsub) |
      grepl("日", flag_person$mainsub) |
      grepl("韓", flag_person$mainsub) |
      grepl("法", flag_person$mainsub) |
      grepl("西", flag_person$mainsub) |
      grepl("德", flag_person$mainsub) |
      grepl("義", flag_person$mainsub) |
      grepl("俄", flag_person$mainsub) |
      grepl("拉丁", flag_person$mainsub)
  ) &
    (
      grepl("新住民", flag_person$mainsub) |
        grepl("越", flag_person$mainsub) |
        grepl("印", flag_person$mainsub) |
        grepl("泰", flag_person$mainsub) |
        grepl("緬", flag_person$mainsub) |
        grepl("柬", flag_person$mainsub) |
        grepl("菲", flag_person$mainsub) |
        grepl("馬", flag_person$mainsub)
    ),
  1,
  flag_person$err_flag
  )
flag_person$err_flag <-
  if_else((
    grepl("第二外", flag_person$mainsub) |
      grepl("日", flag_person$mainsub) |
      grepl("韓", flag_person$mainsub) |
      grepl("法", flag_person$mainsub) |
      grepl("西", flag_person$mainsub) |
      grepl("德", flag_person$mainsub) |
      grepl("義", flag_person$mainsub) |
      grepl("俄", flag_person$mainsub) |
      grepl("拉丁", flag_person$mainsub)
  ) & (
    grepl("本土", flag_person$mainsub) |
      grepl("閩", flag_person$mainsub) |
      grepl("客", flag_person$mainsub) |
      grepl("阿美", flag_person$mainsub) |
      grepl("排灣", flag_person$mainsub) |
      grepl("泰雅", flag_person$mainsub) |
      grepl("布農", flag_person$mainsub) |
      grepl("卑南", flag_person$mainsub) |
      grepl("魯凱", flag_person$mainsub) |
      grepl("賽夏", flag_person$mainsub) |
      grepl("鄒", flag_person$mainsub) |
      grepl("達悟", flag_person$mainsub) |
      grepl("雅美", flag_person$mainsub) |
      grepl("邵", flag_person$mainsub) |
      grepl("噶瑪蘭", flag_person$mainsub) |
      grepl("太魯閣", flag_person$mainsub) |
      grepl("薩奇萊雅", flag_person$mainsub) |
      grepl("賽德克", flag_person$mainsub) |
      grepl("拉阿魯哇", flag_person$mainsub) |
      grepl("卡那卡那富", flag_person$mainsub) |
      grepl("賽考利克", flag_person$mainsub)
  ),
  1,
  flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    grepl("手語", flag_person$mainsub) &
      (
        grepl("新住民", flag_person$mainsub) |
          grepl("越", flag_person$mainsub) |
          grepl("印", flag_person$mainsub) |
          grepl("泰", flag_person$mainsub) |
          grepl("緬", flag_person$mainsub) |
          grepl("柬", flag_person$mainsub) |
          grepl("菲", flag_person$mainsub) |
          grepl("馬", flag_person$mainsub)
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    grepl("手語", flag_person$mainsub) &
      (
        grepl("本土", flag_person$mainsub) |
          grepl("閩", flag_person$mainsub) |
          grepl("客", flag_person$mainsub) |
          grepl("阿美", flag_person$mainsub) |
          grepl("排灣", flag_person$mainsub) |
          grepl("泰雅", flag_person$mainsub) |
          grepl("布農", flag_person$mainsub) |
          grepl("卑南", flag_person$mainsub) |
          grepl("魯凱", flag_person$mainsub) |
          grepl("賽夏", flag_person$mainsub) |
          grepl("鄒", flag_person$mainsub) |
          grepl("達悟", flag_person$mainsub) |
          grepl("雅美", flag_person$mainsub) |
          grepl("邵", flag_person$mainsub) |
          grepl("噶瑪蘭", flag_person$mainsub) |
          grepl("太魯閣", flag_person$mainsub) |
          grepl("薩奇萊雅", flag_person$mainsub) |
          grepl("賽德克", flag_person$mainsub) |
          grepl("拉阿魯哇", flag_person$mainsub) |
          grepl("卡那卡那富", flag_person$mainsub) |
          grepl("賽考利克", flag_person$mainsub)
      ),
    1,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else((
    grepl("新住民", flag_person$mainsub) |
      grepl("越", flag_person$mainsub) |
      grepl("印", flag_person$mainsub) |
      grepl("泰", flag_person$mainsub) |
      grepl("緬", flag_person$mainsub) |
      grepl("柬", flag_person$mainsub) |
      grepl("菲", flag_person$mainsub) |
      grepl("馬", flag_person$mainsub)
  ) & (
    grepl("本土", flag_person$mainsub) |
      grepl("閩", flag_person$mainsub) |
      grepl("客", flag_person$mainsub) |
      grepl("阿美", flag_person$mainsub) |
      grepl("排灣", flag_person$mainsub) |
      grepl("泰雅", flag_person$mainsub) |
      grepl("布農", flag_person$mainsub) |
      grepl("卑南", flag_person$mainsub) |
      grepl("魯凱", flag_person$mainsub) |
      grepl("賽夏", flag_person$mainsub) |
      grepl("鄒", flag_person$mainsub) |
      grepl("達悟", flag_person$mainsub) |
      grepl("雅美", flag_person$mainsub) |
      grepl("邵", flag_person$mainsub) |
      grepl("噶瑪蘭", flag_person$mainsub) |
      grepl("太魯閣", flag_person$mainsub) |
      grepl("薩奇萊雅", flag_person$mainsub) |
      grepl("賽德克", flag_person$mainsub) |
      grepl("拉阿魯哇", flag_person$mainsub) |
      grepl("卡那卡那富", flag_person$mainsub) |
      grepl("賽考利克", flag_person$mainsub)
  ),
  1,
  flag_person$err_flag
  )

#NA改成flag31處理
flag_person$err_flag <-
  if_else(
    flag_person$source == "教員資料表" &
      (
        flag_person$sertype == "教師" |
          flag_person$sertype == "主任教官" |
          flag_person$sertype == "教官"
      ) & flag_person$mainsub == "NA",
    0,
    flag_person$err_flag
  )

flag_person$err_flag <-
  if_else(is.na(flag_person$err_flag), 0, flag_person$err_flag)

#加註
flag_person$err_flag_txt <- ""
flag_person$err_flag_txt <- case_when(
  flag_person$err_flag == 1 ~ paste(flag_person$name,
                                    "（",
                                    flag_person$mainsub,
                                    "）",
                                    sep = ""),
  TRUE ~ flag_person$err_flag_txt
)

if (dim(flag_person %>% subset(err_flag == 1))[1] != 0) {
  #根據organization_id + source，展開成寬資料(wide)
  flag_person_wide_flag25 <- flag_person %>%
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
    colnames(flag_person_wide_flag25)[3:length(colnames(flag_person_wide_flag25))]
  flag_person_wide_flag25$flag25_r <- NA
  for (i in temp) {
    flag_person_wide_flag25$flag25_r <-
      paste(flag_person_wide_flag25$flag25_r,
            flag_person_wide_flag25[[i]],
            sep = " ")
  }
  flag_person_wide_flag25$flag25_r <-
    gsub("NA ",
         replacement = "",
         flag_person_wide_flag25$flag25_r)
  flag_person_wide_flag25$flag25_r <-
    gsub(" NA",
         replacement = "",
         flag_person_wide_flag25$flag25_r)
  
  #產生檢誤報告文字
  flag25_temp <- flag_person_wide_flag25 %>%
    group_by(organization_id) %>%
    mutate(flag25_txt = paste(flag25_r, sep = ""), "") %>%
    subset(select = c(organization_id, flag25_txt)) %>%
    distinct(organization_id, flag25_txt)
  
  #根據organization_id，展開成寬資料(wide)
  flag25 <- flag25_temp %>%
    dcast(organization_id ~ flag25_txt, value.var = "flag25_txt")
  
  #合併教員資料表及職員(工)資料表報告文字
  temp <- colnames(flag25)[2:length(colnames(flag25))]
  flag25$flag25 <- NA
  for (i in temp) {
    flag25$flag25 <- paste(flag25$flag25, flag25[[i]], sep = "； ")
  }
  flag25$flag25 <- gsub("NA； ", replacement = "", flag25$flag25)
  flag25$flag25 <- gsub("； NA", replacement = "", flag25$flag25)
  
  #產生檢誤報告文字
  flag25 <- flag25 %>%
    subset(select = c(organization_id, flag25)) %>%
    distinct(organization_id, flag25) %>%
    mutate(flag25 = paste("請確認該員之科別名稱(非授課科目)或所歸屬之領域/群別（可參閱欄位說明及其附件）：",
                          flag25,
                          sep = ""))
} else{
  #偵測flag25是否存在。若不存在，則產生NA行
  if ('flag25' %in% ls()) {
    print("flag25")
  } else{
    flag25 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    flag25$flag25 <- ""
  }
}
