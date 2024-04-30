# flag80: 代理教師、兼任教師、鐘點教師、長期代課教師、約用教師、約聘僱教師的「本校到職日期」非屬本學期，請再確認。-------------------------------------------------------------------
flag_person <- drev_person_1

#代理教師、兼任教師、長期代課教師、鐘點教師、約聘僱教師、約用教師到職日過早

#兼任教師、長期代課教師、鐘點教師、約聘僱教師、約用教師到職日應為上一個學期開學日(每學期(年)需修改emp_year、emp_mon的時間)
flag_person$emp_year1 <- 112
flag_person$emp_mon1 <- 8

flag_person$arvy1 <-
  substr(flag_person$onbodat, 1, 3) %>% as.numeric()
flag_person$arvm1 <-
  substr(flag_person$onbodat, 4, 5) %>% as.numeric()

flag_person$err_emp1 <- if_else((
  flag_person$emptype %in% c("兼任",
                             "長期代課",
                             "鐘點教師",
                             "約聘僱",
                             "約用")
  & flag_person$sertype == "教師"
)
&
  (flag_person$arvy1 * 12 + flag_person$arvm1) < (flag_person$emp_year1 * 12 + flag_person$emp_mon1),
1,
0
)

#代理教師到職日應為上一個學期開學日-2年，依法規代理教師得續聘2次(每學期(年)需修改emp_year、emp_mon的時間)

#代理教師到職日應為上一個學期開學日-2年以上，聘任類別卻填"代理"，要請學校改為"代理(連)"
#若聘任類別直接填了"代理(連)"，視為學校已確認，不檢查
#仲賢：可能要改為上一個學期開學日-1年以上，因為法定可以連續聘任也算連續聘任
flag_person$emp_year2 <- flag_person$emp_year1 - 1
flag_person$emp_mon2 <- flag_person$emp_mon1

flag_person$arvy2 <-
  substr(flag_person$onbodat, 1, 3) %>% as.numeric()
flag_person$arvm2 <-
  substr(flag_person$onbodat, 4, 5) %>% as.numeric()

flag_person$err_emp2 <-
  if_else((flag_person$emptype == "代理" &
             flag_person$sertype == "教師") &
            (flag_person$arvy2 * 12 + flag_person$arvm2) < (flag_person$emp_year2 * 12 + flag_person$emp_mon2),
          1,
          0
  )

flag_person$emptypesertype <-
  paste(flag_person$emptype, flag_person$sertype, sep = "")
flag_person$emptypesertype <-
  if_else(flag_person$emptypesertype == "鐘點教師教師",
          "鐘點教師",
          flag_person$emptypesertype)

flag_person$err_flag <-
  if_else(flag_person$err_emp1 == 1 |
            flag_person$err_emp2 == 1,
          1,
          0)

#備註文字用
#err_emp1: 兼任教師、長期代課教師、鐘點教師、約聘僱教師、約用教師到職日過早
#err_emp2: 代理教師到職日過早
#aggregate該校err_emp1及err_emp2 -> 該校err_emp出現在err_emp1 or err_emp2
flag_person_err_emp_detect <-
  aggregate(cbind(err_emp1, err_emp2) ~ organization_id,
            flag_person,
            sum) %>%
  rename(err_emp1_detect = err_emp1,
         err_emp2_detect = err_emp2)

flag_person <- flag_person %>%
  left_join(flag_person_err_emp_detect, by = "organization_id")

#加註
flag_person$name <-
  paste(
    flag_person$name,
    "（",
    flag_person$emptypesertype,
    " 到職日:",
    flag_person$onbodat,
    "）",
    sep = ""
  )
flag_person$name <- gsub("；）", replacement = "）", flag_person$name)
flag_person$name <- gsub("（）", replacement = "", flag_person$name)

#呈現姓名
flag_person$err_flag_txt <- ""
flag_person$err_flag_txt <-
  case_when(flag_person$err_flag == 1 ~ flag_person$name,
            TRUE ~ flag_person$err_flag_txt)

if (dim(flag_person %>% subset(err_flag == 1))[1] != 0) {
  #根據organization_id，展開成寬資料(wide)
  flag_person_wide_flag80_1 <- tryCatch({
    flag_person %>%
      subset(
        select = c(
          organization_id,
          idnumber,
          err_flag_txt,
          edu_name2,
          err_flag,
          err_emp1,
          err_emp2,
          err_emp1_detect,
          err_emp2_detect
        )
      ) %>%
      subset(err_emp1 == 1) %>%
      dcast(organization_id ~ err_flag_txt,
            value.var = "err_flag_txt",
            fun.aggregate = first)
  }, error = function(e) {
    NULL
  })
  
  if (!is.null(flag_person_wide_flag80_1)) {
    #合併所有name
    temp <-
      colnames(flag_person_wide_flag80_1)[2:length(colnames(flag_person_wide_flag80_1))]
    flag_person_wide_flag80_1$flag80_1_r <- NA
    for (i in temp) {
      flag_person_wide_flag80_1$flag80_1_r <-
        paste(flag_person_wide_flag80_1$flag80_1_r,
              flag_person_wide_flag80_1[[i]],
              sep = " ")
    }
    flag_person_wide_flag80_1$flag80_1_r <-
      gsub("NA ",
           replacement = "",
           flag_person_wide_flag80_1$flag80_1_r)
    flag_person_wide_flag80_1$flag80_1_r <-
      gsub(" NA",
           replacement = "",
           flag_person_wide_flag80_1$flag80_1_r)
    flag_person_wide_flag80_1$flag80_1_r <-
      paste0(flag_person_wide_flag80_1$flag80_1_r,
             "\n（請依欄位說明，再協助確認是否為本次任職聘書/聘約之到職日期。）") #若#err_flag_1: 職稱或服務單位不合理，則加註
  } else{
    print("flag_person_wide_flag80_1 not exists.")
    rm(flag_person_wide_flag80_1)
  }
  
  #根據organization_id，展開成寬資料(wide)
  flag_person_wide_flag80_2 <- tryCatch({
    flag_person %>%
      subset(
        select = c(
          organization_id,
          idnumber,
          err_flag_txt,
          edu_name2,
          err_flag,
          err_emp1,
          err_emp2,
          err_emp1_detect,
          err_emp2_detect
        )
      ) %>%
      subset(err_emp2 == 1) %>%
      dcast(organization_id ~ err_flag_txt, value.var = "err_flag_txt")
  }, error = function(e) {
    NULL
  })
  
  if (!is.null(flag_person_wide_flag80_2)) {
    #合併所有name
    temp <-
      colnames(flag_person_wide_flag80_2)[2:length(colnames(flag_person_wide_flag80_2))]
    flag_person_wide_flag80_2$flag80_2_r <- NA
    for (i in temp) {
      flag_person_wide_flag80_2$flag80_2_r <-
        paste(flag_person_wide_flag80_2$flag80_2_r,
              flag_person_wide_flag80_2[[i]],
              sep = " ")
    }
    flag_person_wide_flag80_2$flag80_2_r <-
      gsub("NA ",
           replacement = "",
           flag_person_wide_flag80_2$flag80_2_r)
    flag_person_wide_flag80_2$flag80_2_r <-
      gsub(" NA",
           replacement = "",
           flag_person_wide_flag80_2$flag80_2_r)
    flag_person_wide_flag80_2$flag80_2_r <-
      paste0(
        flag_person_wide_flag80_2$flag80_2_r,
        "\n（請依欄位說明及簡報，再協助確認各學年(學期)聘任期間是否中斷超過一個月以上，若否，則認定為聘任未中斷（即『連續聘任』），該代理教師之「聘任類別」請填寫「代理(連)」。）"
      ) #若#err_flag_1: 職稱或服務單位不合理，則加註
  } else{
    print("flag_person_wide_flag80_2 not exists.")
    rm(flag_person_wide_flag80_2)
  }
  
  if ('flag_person_wide_flag80_1' %in% ls()) {
    #如果flag_person_wide_flag80_1有建立成功
    print("flag_person_wide_flag80_1 exists.")
  } else{
    #如果未建立成功，建立空白物件
    flag_person_wide_flag80_1 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    
    flag_person_wide_flag80_1$flag80_1_r <-  ""
  }
  
  if ('flag_person_wide_flag80_2' %in% ls()) {
    #如果flag_person_wide_flag80_2有建立成功
    print("flag_person_wide_flag80_2 exists.")
  } else{
    #如果未建立成功，建立空白物件
    flag_person_wide_flag80_2 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    
    flag_person_wide_flag80_2$flag80_2_r <-  ""
  }
  
  flag_person_wide_flag80 <- flag_person_wide_flag80_1 %>%
    full_join(flag_person_wide_flag80_2, by = c("organization_id")) %>%
    select(c("organization_id", "flag80_1_r", "flag80_2_r")) %>%
    mutate(flag80_r = paste(flag80_1_r, flag80_2_r, sep = "\n"))
  flag_person_wide_flag80$flag80_r <-
    gsub("NA\n+",
         replacement = "",
         flag_person_wide_flag80$flag80_r)
  flag_person_wide_flag80$flag80_r <-
    gsub("\nNA+",
         replacement = "",
         flag_person_wide_flag80$flag80_r)
  
  
  #產生檢誤報告文字
  flag80 <- flag_person_wide_flag80 %>%
    subset(flag80_r != "") %>%
    group_by(organization_id) %>%
    mutate(flag80 = paste("教員資料表需核對「本校到職日期」：", flag80_r, sep = ""), "") %>%
    subset(select = c(organization_id, flag80)) %>%
    distinct(organization_id, flag80)
} else{
  #偵測flag80是否存在。若不存在，則產生NA行
  if ('flag80' %in% ls()) {
    print("flag80")
  } else{
    flag80 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    flag80$flag80 <- ""
  }
}
