# flag39: 學校工作總年資（本校服務年資+本校到職前學校服務總年資），與年齡之差距過大或過小。 -------------------------------------------------------------------
flag_person <- drev_person_1

#年齡
#創設變項出生年月日：birthy birthm birthd
flag_person$birthy <- ""
flag_person$birthm <- ""
flag_person$birthd <- ""

flag_person$birthy <-
  if_else(
    nchar(flag_person$birthdate) == 6,
    substr(flag_person$birthdate, 1, 2),
    flag_person$birthy
  )
flag_person$birthm <-
  if_else(
    nchar(flag_person$birthdate) == 6,
    substr(flag_person$birthdate, 3, 4),
    flag_person$birthm
  )
flag_person$birthd <-
  if_else(
    nchar(flag_person$birthdate) == 6,
    substr(flag_person$birthdate, 5, 6),
    flag_person$birthd
  )
flag_person$birthy <-
  if_else(
    nchar(flag_person$birthdate) == 7,
    substr(flag_person$birthdate, 1, 3),
    flag_person$birthy
  )
flag_person$birthm <-
  if_else(
    nchar(flag_person$birthdate) == 7,
    substr(flag_person$birthdate, 4, 5),
    flag_person$birthm
  )
flag_person$birthd <-
  if_else(
    nchar(flag_person$birthdate) == 7,
    substr(flag_person$birthdate, 6, 7),
    flag_person$birthd
  )

flag_person$birthy <- as.numeric(flag_person$birthy)
flag_person$birthm <- as.numeric(flag_person$birthm)
flag_person$birthd <- as.numeric(flag_person$birthd)

flag_person$survey_year <- 2024

#創設變項年齡（以年為單位）：age
flag_person$age <- 0
flag_person$age <-
  if_else(
    flag_person$survey_year %% 4 != 0,
    ((flag_person$survey_year - 1911) + 3 / 12 + 29 / 365) - (
      flag_person$birthy + (flag_person$birthm / 12) + (flag_person$birthd / 365)
    ),
    flag_person$age
  )
flag_person$age <-
  if_else(
    flag_person$survey_year %% 4 == 0,
    ((flag_person$survey_year - 1911) + 3 / 12 + 29 / 366) - (
      flag_person$birthy + (flag_person$birthm / 12) + (flag_person$birthd / 366)
    ),
    flag_person$age
  )

flag_person$onbodaty <- ""
flag_person$onbodatm <- ""
flag_person$onbodatd <- ""
flag_person$onbodatd <- ""

flag_person$onbodaty <-
  if_else(
    nchar(flag_person$onbodat) == 6,
    substr(flag_person$onbodat, 1, 2),
    flag_person$onbodaty
  )
flag_person$onbodatm <-
  if_else(
    nchar(flag_person$onbodat) == 6,
    substr(flag_person$onbodat, 3, 4),
    flag_person$onbodatm
  )
flag_person$onbodatd <-
  if_else(
    nchar(flag_person$onbodat) == 6,
    substr(flag_person$onbodat, 5, 6),
    flag_person$onbodatd
  )
flag_person$onbodaty <-
  if_else(
    nchar(flag_person$onbodat) == 7,
    substr(flag_person$onbodat, 1, 3),
    flag_person$onbodaty
  )
flag_person$onbodatm <-
  if_else(
    nchar(flag_person$onbodat) == 7,
    substr(flag_person$onbodat, 4, 5),
    flag_person$onbodatm
  )
flag_person$onbodatd <-
  if_else(
    nchar(flag_person$onbodat) == 7,
    substr(flag_person$onbodat, 6, 7),
    flag_person$onbodatd
  )

flag_person$onbodaty <- as.numeric(flag_person$onbodaty)
flag_person$onbodatm <- as.numeric(flag_person$onbodatm)
flag_person$onbodatd <- as.numeric(flag_person$onbodatd)

#本校服務年資
flag_person$tser <- 0
flag_person$tser <-
  if_else(
    flag_person$survey_year %% 4 != 0,
    ((flag_person$survey_year - 1911) + 3 / 12 + 29 / 365) - (
      flag_person$onbodaty + (flag_person$onbodatm / 12) + (flag_person$onbodatd /
                                                              365)
    ),
    flag_person$tser
  )
flag_person$tser <-
  if_else(
    flag_person$survey_year %% 4 == 0,
    ((flag_person$survey_year - 1911) + 3 / 12 + 29 / 366) - (
      flag_person$onbodaty + (flag_person$onbodatm / 12) + (flag_person$onbodatd /
                                                              366)
    ),
    flag_person$tser
  )

#本次本校任職需扣除之年資
flag_person$desey <-
  substr(flag_person$desedym, 1, 2) %>% as.numeric()
flag_person$desem <-
  substr(flag_person$desedym, 3, 4) %>% as.numeric()

flag_person$dese <- (flag_person$desey + (flag_person$desem / 12))

#本校服務年資-本校任職需扣除資年資 才是實際在本校的服務年資
flag_person$tser <- flag_person$tser - flag_person$dese

#避免掉年資小於零的情況（因本校到職日期+本次本校任職需扣除之年資可能為8/1的情況）
flag_person$tser <-
  if_else(flag_person$tser < 0, 0, flag_person$tser)

#本校到職前學校服務總年資
flag_person$beoby <-
  substr(flag_person$beobdym, 1, 2) %>% as.numeric
flag_person$beobm <-
  substr(flag_person$beobdym, 3, 4) %>% as.numeric

flag_person$beob <- (flag_person$beoby + (flag_person$beobm / 12))

#學校教學工作總年資
flag_person$tsch <- flag_person$tser + flag_person$beob

flag_person$err_flag <- 0
flag_person$err_flag <-
  if_else(flag_person$age - flag_person$tsch <= 17,
          1,
          flag_person$err_flag)
flag_person$err_flag <-
  if_else(flag_person$age - flag_person$tsch > 75,
          1,
          flag_person$err_flag)
flag_person$err_flag <-
  if_else(
    flag_person$age - flag_person$tsch > 75 &
      (
        flag_person$emptype == "兼任" |
          flag_person$emptype == "長期代課" |
          flag_person$emptype == "專職族語老師" |
          flag_person$emptype == "鐘點教師" |
          flag_person$emptype == "約聘僱" |
          flag_person$emptype == "約用" |
          flag_person$emptype == "派遣"
      ),
    0,
    flag_person$err_flag
  )
flag_person$err_flag <-
  if_else(
    flag_person$age - flag_person$tsch > 85 &
      (
        flag_person$emptype == "兼任" |
          flag_person$emptype == "長期代課" |
          flag_person$emptype == "專職族語老師" |
          flag_person$emptype == "鐘點教師" |
          flag_person$emptype == "約聘僱" |
          flag_person$emptype == "約用" |
          flag_person$emptype == "派遣"
      ),
    1,
    flag_person$err_flag
  )

flag_person$age <- floor(flag_person$age)
flag_person$tsch <- floor(flag_person$tsch)
flag_person$gowork <- flag_person$age - flag_person$tsch

temp <- c("age", "tsch", "gowork")
for (x in temp) {
  flag_person[[x]] <- flag_person[[x]] %>% as.character()
}

#加註學校工作總年資及工作起始歲數
flag_person$err_flag_txt <- ""
flag_person$err_flag_txt <- case_when(
  flag_person$err_flag == 1 ~ paste(
    flag_person$name,
    flag_person$age,
    "歲，但學校工作總年資有",
    flag_person$tsch,
    "年（約",
    flag_person$gowork,
    "歲開始工作）",
    sep = ""
  ),
  TRUE ~ flag_person$err_flag_txt
)

if (dim(flag_person %>% subset(err_flag == 1))[1] != 0) {
  #根據organization_id + source，展開成寬資料(wide)
  flag_person_wide_flag39 <- flag_person %>%
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
    colnames(flag_person_wide_flag39)[3:length(colnames(flag_person_wide_flag39))]
  flag_person_wide_flag39$flag39_r <- NA
  for (i in temp) {
    flag_person_wide_flag39$flag39_r <-
      paste(flag_person_wide_flag39$flag39_r,
            flag_person_wide_flag39[[i]],
            sep = " ")
  }
  flag_person_wide_flag39$flag39_r <-
    gsub("NA ",
         replacement = "",
         flag_person_wide_flag39$flag39_r)
  flag_person_wide_flag39$flag39_r <-
    gsub(" NA",
         replacement = "",
         flag_person_wide_flag39$flag39_r)
  
  #產生檢誤報告文字
  flag39_temp <- flag_person_wide_flag39 %>%
    group_by(organization_id) %>%
    mutate(flag39_txt = paste(source, "：", flag39_r, sep = ""), "") %>%
    subset(select = c(organization_id, flag39_txt)) %>%
    distinct(organization_id, flag39_txt)
  
  #根據organization_id，展開成寬資料(wide)
  flag39 <- flag39_temp %>%
    dcast(organization_id ~ flag39_txt, value.var = "flag39_txt")
  
  #合併教員資料表及職員(工)資料表報告文字
  temp <- colnames(flag39)[2:length(colnames(flag39))]
  flag39$flag39 <- NA
  for (i in temp) {
    flag39$flag39 <- paste(flag39$flag39, flag39[[i]], sep = "； ")
  }
  flag39$flag39 <- gsub("NA； ", replacement = "", flag39$flag39)
  flag39$flag39 <- gsub("； NA", replacement = "", flag39$flag39)
  
  #產生檢誤報告文字
  flag39 <- flag39 %>%
    subset(select = c(organization_id, flag39)) %>%
    distinct(organization_id, flag39) %>%
    mutate(flag39 = paste(
      "請確認該員之「本校到職日期」、「本校任職需扣除之年資」、「本校到職前學校服務總年資」，",
      flag39,
      sep = ""
    ))
} else{
  #偵測flag39是否存在。若不存在，則產生NA行
  if ('flag39' %in% ls()) {
    print("flag39")
  } else{
    flag39 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    flag39$flag39 <- ""
  }
}
