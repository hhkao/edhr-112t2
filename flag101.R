# flag101: 教師、主任教官及教官若未擔任學（群）科教學研究委員會召集人、各類藝術才能班、體育班、學術性向資賦優異班之召集人、課程諮詢教師、協助行政教師或其他教學相關職務，其減授節數原則為0。 -------------------------------------------------------------------
flag_person <- drev_P_load %>%
  subset(load == 1)

flag_person$err_flag <- 0
flag_person$err_flag <- if_else(
  flag_person$sertype %in% c("教師", "主任教官", "教官") &
    flag_person$emptype %in% c("專任", "代理", "代理(連)") &
    flag_person$cut > 0 &
    flag_person$mitleader == "N" &
    flag_person$classleader == "N" &
    flag_person$ccounselor == "N" &
    flag_person$adminteacher == "N" &
    flag_person$otherteacher %in% c("N", "科學班班主任", "科學班主任", "科學班"),
  1,
  flag_person$err_flag
)

#加註
flag_person$err_flag_txt <- ""
flag_person$err_flag_txt <- case_when(
  flag_person$err_flag == 1 ~ paste(flag_person$name,
                                    sep = ""),
  TRUE ~ flag_person$err_flag_txt
)

if (dim(flag_person %>% subset(err_flag == 1))[1] != 0) {
  #根據organization_id + source，展開成寬資料(wide)
  flag_person_wide_flag101 <- flag_person %>%
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
    colnames(flag_person_wide_flag101)[3:length(colnames(flag_person_wide_flag101))]
  flag_person_wide_flag101$flag101_r <- NA
  for (i in temp) {
    flag_person_wide_flag101$flag101_r <-
      paste(flag_person_wide_flag101$flag101_r,
            flag_person_wide_flag101[[i]],
            sep = " ")
  }
  flag_person_wide_flag101$flag101_r <-
    gsub("NA ",
         replacement = "",
         flag_person_wide_flag101$flag101_r)
  flag_person_wide_flag101$flag101_r <-
    gsub(" NA",
         replacement = "",
         flag_person_wide_flag101$flag101_r)
  
  #產生檢誤報告文字
  flag101_temp <- flag_person_wide_flag101 %>%
    group_by(organization_id) %>%
    mutate(flag101_txt = paste(flag101_r, sep = ""), "") %>%
    subset(select = c(organization_id, flag101_txt)) %>%
    distinct(organization_id, flag101_txt)
  
  #根據organization_id，展開成寬資料(wide)
  flag101 <- flag101_temp %>%
    dcast(organization_id ~ flag101_txt, value.var = "flag101_txt")
  
  #合併教員資料表及職員(工)資料表報告文字
  temp <- colnames(flag101)[2:length(colnames(flag101))]
  flag101$flag101 <- NA
  for (i in temp) {
    flag101$flag101 <- paste(flag101$flag101, flag101[[i]], sep = "； ")
  }
  flag101$flag101 <- gsub("NA； ", replacement = "", flag101$flag101)
  flag101$flag101 <- gsub("； NA", replacement = "", flag101$flag101)
  
  #產生檢誤報告文字
  flag101 <- flag101 %>%
    subset(select = c(organization_id, flag101)) %>%
    distinct(organization_id, flag101) %>%
    mutate(flag101 = paste("教師姓名：",
                           flag101,
                           sep = ""))
} else{
  #偵測flag101是否存在。若不存在，則產生NA行
  if ('flag101' %in% ls()) {
    print("flag101")
  } else{
    flag101 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    flag101$flag101 <- ""
  }
}
