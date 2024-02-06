# flag26: 減授節數不為0，教學相關職務各欄位，包括「是否擔任學（群）科之教學研究委員會召集人」、「是否擔任各類藝術才能班、體育班、學術性向資賦優異班之召集人」、「是否擔任課程諮詢教師」、「是否擔任協助行政教師」、「擔任其他教學相關職務名稱」，應按欄位說明，至少有一個欄位填入「Y」、「C」或職務名稱。 -------------------------------------------------------------------
flag_person <- drev_P_load %>%
  subset(load == 1)

flag_person$err_flag <- 0
flag_person$err_flag <- if_else(
  flag_person$cut == 0 & (
    flag_person$mitleader != "N" |
      flag_person$classleader != "N" |
      flag_person$ccounselor != "N" |
      flag_person$adminteacher != "N" |
      !flag_person$otherteacher %in% c("N", "科學班班主任", "科學班主任", "科學班")
  ),
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
  flag_person_wide_flag26 <- flag_person %>%
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
    colnames(flag_person_wide_flag26)[3:length(colnames(flag_person_wide_flag26))]
  flag_person_wide_flag26$flag26_r <- NA
  for (i in temp) {
    flag_person_wide_flag26$flag26_r <-
      paste(flag_person_wide_flag26$flag26_r,
            flag_person_wide_flag26[[i]],
            sep = " ")
  }
  flag_person_wide_flag26$flag26_r <-
    gsub("NA ",
         replacement = "",
         flag_person_wide_flag26$flag26_r)
  flag_person_wide_flag26$flag26_r <-
    gsub(" NA",
         replacement = "",
         flag_person_wide_flag26$flag26_r)
  
  #產生檢誤報告文字
  flag26_temp <- flag_person_wide_flag26 %>%
    group_by(organization_id) %>%
    mutate(flag26_txt = paste(flag26_r, sep = ""), "") %>%
    subset(select = c(organization_id, flag26_txt)) %>%
    distinct(organization_id, flag26_txt)
  
  #根據organization_id，展開成寬資料(wide)
  flag26 <- flag26_temp %>%
    dcast(organization_id ~ flag26_txt, value.var = "flag26_txt")
  
  #合併教員資料表及職員(工)資料表報告文字
  temp <- colnames(flag26)[2:length(colnames(flag26))]
  flag26$flag26 <- NA
  for (i in temp) {
    flag26$flag26 <- paste(flag26$flag26, flag26[[i]], sep = "； ")
  }
  flag26$flag26 <- gsub("NA； ", replacement = "", flag26$flag26)
  flag26$flag26 <- gsub("； NA", replacement = "", flag26$flag26)
  
  #產生檢誤報告文字
  flag26 <- flag26 %>%
    subset(select = c(organization_id, flag26)) %>%
    distinct(organization_id, flag26) %>%
    mutate(flag26 = paste("教師姓名：",
                          flag26,
                          sep = ""))
} else{
  #偵測flag26是否存在。若不存在，則產生NA行
  if ('flag26' %in% ls()) {
    print("flag26")
  } else{
    flag26 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    flag26$flag26 <- ""
  }
}
