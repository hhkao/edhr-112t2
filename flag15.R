# flag15: 兼任行政職職稱（一）～（三）不應填入校長或非行政職職稱，例如老師、教師、運動教練等。 -------------------------------------------------------------------
#flag15_append-------------------------------------------------------------------
flag_person <- drev_person_1

#"本國美國"標記為1
flag_person$err_flag_admintitle1 <- 0
flag_person$err_flag_admintitle2 <- 0
flag_person$err_flag_admintitle3 <- 0
flag_person$err_flag_admintitle1 <- case_when(
  grepl("教學支援工作人員$", flag_person$admintitle1) |
    grepl("教學支援人員$", flag_person$admintitle1) |
    grepl("老師$", flag_person$admintitle1) |
    grepl("教師$", flag_person$admintitle1) |
    grepl("導師", flag_person$admintitle1) |
    grepl("運動教練", flag_person$admintitle1) |
    grepl("^校長$", flag_person$admintitle1) |
    grepl("教官$", flag_person$admintitle1) ~ 1,
  TRUE ~ flag_person$err_flag_admintitle1
)
flag_person$err_flag_admintitle2 <- case_when(
  grepl("教學支援工作人員$", flag_person$admintitle2) |
    grepl("教學支援人員$", flag_person$admintitle2) |
    grepl("老師$", flag_person$admintitle2) |
    grepl("教師$", flag_person$admintitle2) |
    grepl("導師", flag_person$admintitle2) |
    grepl("運動教練", flag_person$admintitle2) |
    grepl("^校長$", flag_person$admintitle2) |
    grepl("教官$", flag_person$admintitle2) ~ 1,
  TRUE ~ flag_person$err_flag_admintitle2
)
flag_person$err_flag_admintitle3 <- case_when(
  grepl("教學支援工作人員$", flag_person$admintitle3) |
    grepl("教學支援人員$", flag_person$admintitle3) |
    grepl("老師$", flag_person$admintitle3) |
    grepl("教師$", flag_person$admintitle3) |
    grepl("導師", flag_person$admintitle3) |
    grepl("運動教練", flag_person$admintitle3) |
    grepl("^校長$", flag_person$admintitle3) |
    grepl("教官$", flag_person$admintitle3) ~ 1,
  TRUE ~ flag_person$err_flag_admintitle3
)

flag_person$err_flag <-
  flag_person$err_flag_admintitle1 + flag_person$err_flag_admintitle2 + flag_person$err_flag_admintitle3

#加註職稱
flag_person$err_flag_txt <- ""
flag_person$err_flag_txt <- case_when(
  flag_person$err_flag_admintitle1 == 1 ~ paste(flag_person$name,
                                                "（",
                                                flag_person$admintitle1,
                                                "）",
                                                sep = ""),
  TRUE ~ flag_person$err_flag_txt
)
flag_person$err_flag_txt <- case_when(
  flag_person$err_flag_admintitle2 == 1 ~ paste(flag_person$name,
                                                "（",
                                                flag_person$admintitle2,
                                                "）",
                                                sep = ""),
  TRUE ~ flag_person$err_flag_txt
)
flag_person$err_flag_txt <- case_when(
  flag_person$err_flag_admintitle3 == 1 ~ paste(flag_person$name,
                                                "（",
                                                flag_person$admintitle3,
                                                "）",
                                                sep = ""),
  TRUE ~ flag_person$err_flag_txt
)
#---

if (dim(flag_person %>% subset(err_flag == 1))[1] != 0) {
  #根據organization_id + source，展開成寬資料(wide)
  flag_person_wide_flag15 <- flag_person %>%
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
    colnames(flag_person_wide_flag15)[3:length(colnames(flag_person_wide_flag15))]
  flag_person_wide_flag15$flag15_r <- NA
  for (i in temp) {
    flag_person_wide_flag15$flag15_r <-
      paste(flag_person_wide_flag15$flag15_r,
            flag_person_wide_flag15[[i]],
            sep = " ")
  }
  flag_person_wide_flag15$flag15_r <-
    gsub("NA ",
         replacement = "",
         flag_person_wide_flag15$flag15_r)
  flag_person_wide_flag15$flag15_r <-
    gsub(" NA",
         replacement = "",
         flag_person_wide_flag15$flag15_r)
  
  #產生檢誤報告文字
  flag15_temp <- flag_person_wide_flag15 %>%
    group_by(organization_id) %>%
    mutate(flag15_txt = paste(source, "需修改兼任行政職職稱：", flag15_r, sep = ""),
           "") %>%
    subset(select = c(organization_id, flag15_txt)) %>%
    distinct(organization_id, flag15_txt)
  
  #根據organization_id，展開成寬資料(wide)
  flag15 <- flag15_temp %>%
    dcast(organization_id ~ flag15_txt, value.var = "flag15_txt")
  
  #合併教員資料表及職員(工)資料表報告文字
  temp <- colnames(flag15)[2:length(colnames(flag15))]
  flag15$flag15 <- NA
  for (i in temp) {
    flag15$flag15 <- paste(flag15$flag15, flag15[[i]], sep = "； ")
  }
  flag15$flag15 <- gsub("NA； ", replacement = "", flag15$flag15)
  flag15$flag15 <- gsub("； NA", replacement = "", flag15$flag15)
  
  #產生檢誤報告文字
  flag15 <- flag15 %>%
    subset(select = c(organization_id, flag15)) %>%
    distinct(organization_id, flag15) %>%
    mutate(
      flag15 = paste(
        flag15,
        "（校長、教師、教官、主任教官、族語老師、教學支援人員屬於服務身分別，若渠等教員未再兼任行政職務，如秘書、學務主任、生活輔導組組長等，請於兼任行政職職稱(單位)填“N” ）",
        sep = ""
      )
    )
} else{
  #偵測flag15是否存在。若不存在，則產生NA行
  if ('flag15' %in% ls()) {
    print("flag15")
  } else{
    flag15 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    flag15$flag15 <- ""
  }
}
