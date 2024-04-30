# flag24: 本校到職日期與填報基準日的差距，不應小於本校任職需扣除年資。 -------------------------------------------------------------------
flag_person <- drev_person_1

flag_person$survey_year <- 2024
flag_person$survey_mon <- 3
flag_person$onbodaty <- ""
flag_person$onbodatm <- ""
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

#本校服務年資-本校任職需扣除之年資 才是實際在本校的服務年資
flag_person$tser <- flag_person$tser - flag_person$dese

#本校到職前學校服務總年資
flag_person$beoby <-
  substr(flag_person$beobdym, 1, 2) %>% as.numeric
flag_person$beobm <-
  substr(flag_person$beobdym, 3, 4) %>% as.numeric

flag_person$beob <- (flag_person$beoby + (flag_person$beobm / 12))

#學校教學工作總年資
flag_person$tsch <- flag_person$tser + flag_person$beob

#tser要小於-0.00137而不是0的原因：本校到職日期+本次本校任職需扣除之年資可能為4月1日，剛好超過資料基準日3月31日一天
#tser改成要小於-0.0041而不是-0.00137的原因：本校到職日期+本次本校任職需扣除之年資可能為10月1日，剛好超過資料基準日9月30日一天
#因扣除年資未滿一個月以一個月計，下學期基準日為2/28，可能造成誤差

flag_person$err_flag <- 0
flag_person$err_flag <-
  if_else(flag_person$tser < -.0806, 1, flag_person$err_flag)

#若spe3錯 則不應出現在flag24
flag_person$survey_year <- flag_person$survey_year - 1911

flag_person$arvy <-
  substr(flag_person$onbodat, 1, 3) %>% as.numeric()
flag_person$arvm <-
  substr(flag_person$onbodat, 4, 5) %>% as.numeric()

flag_person$err_spe = if_else((flag_person$arvy * 12 + flag_person$arvm) > (flag_person$survey_year * 12 + flag_person$survey_mon),
                              1,
                              0
)

flag_person$err_flag <-
  if_else(flag_person$err_spe == 1, 0, flag_person$err_flag)

#換算到職至資料基準日的日期
flag_person$tser_ndese <- flag_person$tser + flag_person$dese
flag_person$tser_ndesey <- floor(flag_person$tser_ndese)
flag_person$tser_ndesem <-
  ceiling((flag_person$tser_ndese - floor(flag_person$tser_ndese)) * 12)
flag_person$ndesey <- floor(flag_person$dese)
flag_person$ndesem <-
  ceiling((flag_person$dese - floor(flag_person$dese)) * 12)

temp <- c("tser_ndesey", "tser_ndesem", "ndesey", "ndesem")
for (x in temp) {
  flag_person[[x]] <- flag_person[[x]] %>% as.character()
}

#加註到職至資料基準日的時間，和扣除年資
flag_person$err_flag_txt <- ""
flag_person$err_flag_txt <- case_when(
  flag_person$err_flag == 1 ~ paste(
    flag_person$name,
    flag_person$onbodat,
    "到職（到職至資料基準日為",
    flag_person$tser_ndesey,
    "年",
    flag_person$tser_ndesem,
    "個月，但扣除年資為",
    flag_person$ndesey,
    "年",
    flag_person$ndesem,
    "個月",
    "）",
    sep = ""
  ),
  TRUE ~ flag_person$err_flag_txt
)

if (dim(flag_person %>% subset(err_flag == 1))[1] != 0) {
  #根據organization_id + source，展開成寬資料(wide)
  flag_person_wide_flag24 <- flag_person %>%
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
    colnames(flag_person_wide_flag24)[3:length(colnames(flag_person_wide_flag24))]
  flag_person_wide_flag24$flag24_r <- NA
  for (i in temp) {
    flag_person_wide_flag24$flag24_r <-
      paste(flag_person_wide_flag24$flag24_r,
            flag_person_wide_flag24[[i]],
            sep = " ")
  }
  flag_person_wide_flag24$flag24_r <-
    gsub("NA ",
         replacement = "",
         flag_person_wide_flag24$flag24_r)
  flag_person_wide_flag24$flag24_r <-
    gsub(" NA",
         replacement = "",
         flag_person_wide_flag24$flag24_r)
  
  #產生檢誤報告文字
  flag24_temp <- flag_person_wide_flag24 %>%
    group_by(organization_id) %>%
    mutate(flag24_txt = paste(source, "：", flag24_r, sep = ""), "") %>%
    subset(select = c(organization_id, flag24_txt)) %>%
    distinct(organization_id, flag24_txt)
  
  #根據organization_id，展開成寬資料(wide)
  flag24 <- flag24_temp %>%
    dcast(organization_id ~ flag24_txt, value.var = "flag24_txt")
  
  #合併教員資料表及職員(工)資料表報告文字
  temp <- colnames(flag24)[2:length(colnames(flag24))]
  flag24$flag24 <- NA
  for (i in temp) {
    flag24$flag24 <- paste(flag24$flag24, flag24[[i]], sep = "； ")
  }
  flag24$flag24 <- gsub("NA； ", replacement = "", flag24$flag24)
  flag24$flag24 <- gsub("； NA", replacement = "", flag24$flag24)
  
  #產生檢誤報告文字
  flag24 <- flag24 %>%
    subset(select = c(organization_id, flag24)) %>%
    distinct(organization_id, flag24) %>%
    mutate(flag24 = paste("請確認該員之「本校到職日期」、「本校任職需扣除之年資」，", flag24, sep = ""))
} else{
  #偵測flag24是否存在。若不存在，則產生NA行
  if ('flag24' %in% ls()) {
    print("flag24")
  } else{
    flag24 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    flag24$flag24 <- ""
  }
}
