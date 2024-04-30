# flag85: 離退教職員（工）資料表中，離職退休情況註記為「退休」人員之年齡偏小。 -------------------------------------------------------------------
flag_person <- drev_P_retire_pre_inner %>%
  rename(name = name.x, name_retire = name.y) %>%
  left_join(edu_name2, by = c("organization_id"))

#若drev_P_retire_pre_inner無資料，建立物件
if (dim(drev_P_retire_pre_inner)[1] == 0) {
  temp <-
    matrix("", nrow = 1, ncol = ncol(flag_person)) %>% data.frame()
  names(temp) <- names(flag_person)
  flag_person <- temp
} else{
  print("flag85: drev_P_retire_pre_inner is already exists.")
}

#離職退休情況為「退休」之人員年齡低於42歲。(與上一期資料比對)
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


flag_person$err_flag <- 0
flag_person$err_flag <-
  if_else(flag_person$resitu == "R" &
            flag_person$age < 42,
          1,
          flag_person$err_flag)

#呈現姓名
flag_person$err_flag_txt <- ""
flag_person$err_flag_txt <-
  case_when(flag_person$err_flag == 1 ~ flag_person$name,
            TRUE ~ flag_person$err_flag_txt)

if (dim(flag_person %>% subset(err_flag == 1))[1] != 0) {
  #根據organization_id + source，展開成寬資料(wide)
  flag_person_wide_flag85 <- flag_person %>%
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
    colnames(flag_person_wide_flag85)[3:length(colnames(flag_person_wide_flag85))]
  flag_person_wide_flag85$flag85_r <- NA
  for (i in temp) {
    flag_person_wide_flag85$flag85_r <-
      paste(flag_person_wide_flag85$flag85_r,
            flag_person_wide_flag85[[i]],
            sep = " ")
  }
  flag_person_wide_flag85$flag85_r <-
    gsub("NA ",
         replacement = "",
         flag_person_wide_flag85$flag85_r)
  flag_person_wide_flag85$flag85_r <-
    gsub(" NA",
         replacement = "",
         flag_person_wide_flag85$flag85_r)
  
  #產生檢誤報告文字
  flag85_temp <- flag_person_wide_flag85 %>%
    group_by(organization_id) %>%
    mutate(flag85_txt = paste(source, "：", flag85_r, sep = ""), "") %>%
    subset(select = c(organization_id, flag85_txt)) %>%
    distinct(organization_id, flag85_txt)
  
  #根據organization_id，展開成寬資料(wide)
  flag85 <- flag85_temp %>%
    dcast(organization_id ~ flag85_txt, value.var = "flag85_txt")
  
  #合併教員資料表及職員(工)資料表報告文字
  temp <- colnames(flag85)[2:length(colnames(flag85))]
  flag85$flag85 <- NA
  for (i in temp) {
    flag85$flag85 <- paste(flag85$flag85, flag85[[i]], sep = "； ")
  }
  flag85$flag85 <- gsub("NA； ", replacement = "", flag85$flag85)
  flag85$flag85 <- gsub("； NA", replacement = "", flag85$flag85)
  
  #產生檢誤報告文字
  flag85 <- flag85 %>%
    subset(select = c(organization_id, flag85)) %>%
    distinct(organization_id, flag85) %>%
    mutate(flag85 = paste(flag85, "（該員年齡似低於最低法定退休年齡，敬請再協助確認）", sep = ""))
} else{
  #偵測flag85是否存在。若不存在，則產生NA行
  if ('flag85' %in% ls()) {
    print("flag85")
  } else{
    flag85 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    flag85$flag85 <- ""
  }
}
