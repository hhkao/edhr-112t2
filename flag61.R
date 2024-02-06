# flag61: 本學期高中部及中學部未調離本校、商借至其他學校單位、留職停薪、請假或停職(停聘)之專任教師及代理教師，如未兼任行政職務或教學職務，且非專任輔導教師，其至少於高中日間部、高中進修部或國中部有教學節數。 -------------------------------------------------------------------
flag_person <- drev_P_load %>%
  subset(load == 1) %>%
  subset(!is.na(basic) | !is.na(cut) | !is.na(othertime)) #NA在這不處理

flag_person$err_flag <- 0
flag_person$err_flag <- if_else(
  (
    flag_person$hschftime + flag_person$hschcftime + (flag_person$hschptime / 20) + flag_person$jhschftime
  ) == 0 &
    flag_person$empunit %in% c("高中部日間部", "高中部進修部", "中學部", "中學部進修部") &
    flag_person$empunit %in% c("專任", "代理", "代理(連)") &
    flag_person$sertype == "教師" &
    flag_person$leave == "N" &
    flag_person$levpay == "N" &
    !grepl("借調至", flag_person$brtype) &
    !grepl("商借至", flag_person$negle) &
    flag_person$suspend == "N" &
    flag_person$admintitle1 == "N" &
    flag_person$counselor == "N" &
    flag_person$tutor == "N" &
    flag_person$mitleader == "N" &
    flag_person$classleader == "N" &
    flag_person$adminteacher == "N" &
    flag_person$otherteacher %in% c("N", "NA"),
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
  flag_person_wide_flag61 <- flag_person %>%
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
    colnames(flag_person_wide_flag61)[3:length(colnames(flag_person_wide_flag61))]
  flag_person_wide_flag61$flag61_r <- NA
  for (i in temp) {
    flag_person_wide_flag61$flag61_r <-
      paste(flag_person_wide_flag61$flag61_r,
            flag_person_wide_flag61[[i]],
            sep = " ")
  }
  flag_person_wide_flag61$flag61_r <-
    gsub("NA ",
         replacement = "",
         flag_person_wide_flag61$flag61_r)
  flag_person_wide_flag61$flag61_r <-
    gsub(" NA",
         replacement = "",
         flag_person_wide_flag61$flag61_r)
  
  #產生檢誤報告文字
  flag61_temp <- flag_person_wide_flag61 %>%
    group_by(organization_id) %>%
    mutate(flag61_txt = paste(flag61_r, sep = ""), "") %>%
    subset(select = c(organization_id, flag61_txt)) %>%
    distinct(organization_id, flag61_txt)
  
  #根據organization_id，展開成寬資料(wide)
  flag61 <- flag61_temp %>%
    dcast(organization_id ~ flag61_txt, value.var = "flag61_txt")
  
  #合併教員資料表及職員(工)資料表報告文字
  temp <- colnames(flag61)[2:length(colnames(flag61))]
  flag61$flag61 <- NA
  for (i in temp) {
    flag61$flag61 <- paste(flag61$flag61, flag61[[i]], sep = "； ")
  }
  flag61$flag61 <- gsub("NA； ", replacement = "", flag61$flag61)
  flag61$flag61 <- gsub("； NA", replacement = "", flag61$flag61)
  
  #產生檢誤報告文字
  flag61 <- flag61 %>%
    subset(select = c(organization_id, flag61)) %>%
    distinct(organization_id, flag61) %>%
    mutate(flag61 = paste("姓名：",
                          flag61,
                          sep = ""))
} else{
  #偵測flag61是否存在。若不存在，則產生NA行
  if ('flag61' %in% ls()) {
    print("flag61")
  } else{
    flag61 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    flag61$flag61 <- ""
  }
}
