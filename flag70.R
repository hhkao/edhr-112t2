# flag70: 本學期調離本校、商借至其他學校單位、留職停薪、請假或停職(停聘)者，其資料原則不需填報本表。 -------------------------------------------------------------------
flag_person <- drev_P_load %>%
  subset(load == 1)

#有請假、留停、借調、商借或停聘的情形，且每週實際教學節數任一欄位不為0(加總 > 0)，則抓出
flag_person$err_flag <- 0
flag_person$err_flag <- if_else(
  flag_person$source == "教員資料表" &
    (
      flag_person$leave != "N" |
        flag_person$levpay != "N" |
        grepl("借調至", flag_person$brtype) |
        grepl("商借至", flag_person$negle) |
        flag_person$suspend != "N"
    ),
  1,
  flag_person$err_flag
)
flag_person$err_flag <- if_else(
  flag_person$source == "職員(工)資料表" &
    (
      flag_person$leave != "N" |
        flag_person$levpay != "N" |
        grepl("借調至", flag_person$brtype) |
        flag_person$suspend != "N"
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
  flag_person_wide_flag70 <- flag_person %>%
    subset(select = c(
      organization_id,
      idnumber,
      err_flag_txt,
      edu_name2,
      source,
      err_flag
    )) %>%
    subset(err_flag == 1) %>%
    dcast(organization_id ~ err_flag_txt, value.var = "err_flag_txt")
  
  #合併所有name
  temp <-
    colnames(flag_person_wide_flag70)[2:length(colnames(flag_person_wide_flag70))]
  flag_person_wide_flag70$flag70_r <- NA
  for (i in temp) {
    flag_person_wide_flag70$flag70_r <-
      paste(flag_person_wide_flag70$flag70_r,
            flag_person_wide_flag70[[i]],
            sep = " ")
  }
  flag_person_wide_flag70$flag70_r <-
    gsub("NA ",
         replacement = "",
         flag_person_wide_flag70$flag70_r)
  flag_person_wide_flag70$flag70_r <-
    gsub(" NA",
         replacement = "",
         flag_person_wide_flag70$flag70_r)
  
  #產生檢誤報告文字
  flag70_temp <- flag_person_wide_flag70 %>%
    group_by(organization_id) %>%
    mutate(flag70_txt = paste(flag70_r, sep = ""), "") %>%
    subset(select = c(organization_id, flag70_txt)) %>%
    distinct(organization_id, flag70_txt)
  
  #根據organization_id，展開成寬資料(wide)
  flag70 <- flag70_temp %>%
    dcast(organization_id ~ flag70_txt, value.var = "flag70_txt")
  
  #合併教員資料表及職員(工)資料表報告文字
  temp <- colnames(flag70)[2:length(colnames(flag70))]
  flag70$flag70 <- NA
  for (i in temp) {
    flag70$flag70 <- paste(flag70$flag70, flag70[[i]], sep = "； ")
  }
  flag70$flag70 <- gsub("NA； ", replacement = "", flag70$flag70)
  flag70$flag70 <- gsub("； NA", replacement = "", flag70$flag70)
  
  #產生檢誤報告文字
  flag70 <- flag70 %>%
    subset(select = c(organization_id, flag70)) %>%
    distinct(organization_id, flag70) %>%
    mutate(
      flag70 = paste(
        "以下人員有調離本校、留職停薪、請假、停職或停聘情形，請確認「高中日間部課程全學期授課之每週實際教學節數」、「高中進修部課程全學期授課之每週實際教學節數」、「高中課程非全學期授課之實際教學總節數」、「國中課程全學期授課之每週實際教學節數」：",
        flag70,
        sep = ""
      )
    )
} else{
  #偵測flag70是否存在。若不存在，則產生NA行
  if ('flag70' %in% ls()) {
    print("flag70")
  } else{
    flag70 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    flag70$flag70 <- ""
  }
}
