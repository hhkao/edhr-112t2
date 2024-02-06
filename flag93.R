# flag93: 離退教職員（工）資料表所列人員，應為上一學年（期）之教員或職員（工）。 -------------------------------------------------------------------
flag_person <- drev_P_retire_pre_right %>%
  rename(name = name.x, name_retire = name.y) %>%
  left_join(edu_name2, by = c("organization_id"))

#若drev_P_retire_pre_inner無資料，建立物件
if (dim(drev_P_retire_pre_inner)[1] == 0) {
  temp <-
    matrix("", nrow = 1, ncol = ncol(flag_person)) %>% data.frame()
  names(temp) <- names(flag_person)
  flag_person <- temp
} else{
  print("flag93: drev_P_retire_pre_inner is already exists.")
}

#填寫在「離退教職員(工)資料表」之人員，聘任類別需為「專任」。(與上一期資料比對)
#抓出:離退人員在上一期聘任類別非專任(專任的人員才能填到離退表)
flag_person$err_flag <- 0
flag_person$err_flag <-
  if_else(is.na(flag_person$pre), 1, flag_person$err_flag)

#若該校上一期未上傳人事資料，此檢誤不檢查
drev_person_pre_list <- drev_person_pre %>%
  select("organization_id") %>%
  distinct(organization_id, .keep_all = TRUE) %>%
  mutate(pre_list = 1)
flag_person <- flag_person %>%
  left_join(drev_person_pre_list, by = "organization_id")

flag_person$err_flag <-
  if_else(flag_person$err_flag == 1 &
            is.na(flag_person$pre_list),
          0,
          flag_person$err_flag)

#呈現姓名
flag_person$err_flag_txt <- ""
flag_person$err_flag_txt <- case_when(flag_person$err_flag == 1 ~ flag_person$name_retire,
                                      TRUE ~ flag_person$err_flag_txt)

if (dim(flag_person %>% subset(err_flag == 1))[1] != 0) {
  #根據organization_id + source，展開成寬資料(wide)
  flag_person_wide_flag93 <- flag_person %>%
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
    colnames(flag_person_wide_flag93)[3:length(colnames(flag_person_wide_flag93))]
  flag_person_wide_flag93$flag93_r <- NA
  for (i in temp) {
    flag_person_wide_flag93$flag93_r <-
      paste(flag_person_wide_flag93$flag93_r,
            flag_person_wide_flag93[[i]],
            sep = " ")
  }
  flag_person_wide_flag93$flag93_r <-
    gsub("NA ",
         replacement = "",
         flag_person_wide_flag93$flag93_r)
  flag_person_wide_flag93$flag93_r <-
    gsub(" NA",
         replacement = "",
         flag_person_wide_flag93$flag93_r)
  
  #產生檢誤報告文字
  flag93_temp <- flag_person_wide_flag93 %>%
    group_by(organization_id) %>%
    mutate(flag93_txt = paste("離退教職員(工)資料表：", flag93_r, sep = ""), "") %>%
    subset(select = c(organization_id, flag93_txt)) %>%
    distinct(organization_id, flag93_txt)
  
  #根據organization_id，展開成寬資料(wide)
  flag93 <- flag93_temp %>%
    dcast(organization_id ~ flag93_txt, value.var = "flag93_txt")
  
  #合併教員資料表及職員(工)資料表報告文字
  temp <- colnames(flag93)[2:length(colnames(flag93))]
  flag93$flag93 <- NA
  for (i in temp) {
    flag93$flag93 <- paste(flag93$flag93, flag93[[i]], sep = "； ")
  }
  flag93$flag93 <- gsub("NA； ", replacement = "", flag93$flag93)
  flag93$flag93 <- gsub("； NA", replacement = "", flag93$flag93)
  
  #產生檢誤報告文字
  flag93 <- flag93 %>%
    subset(select = c(organization_id, flag93)) %>%
    distinct(organization_id, flag93) %>%
    mutate(flag93 = if_else(
      substr(organization_id, 3, 3) == "1",
      paste0(
        flag93,
        "（查貴校上一學年所填資料，上述人員未在貴校教職員(工)資料中，請確認上述人員是否於112年2月1日-112年7月31日有退休或因故離職之情形，或是否屬於貴校教職員(工)，併請確認貴校教職員工名單是否完整正確。）"
      ),
      #私立
      paste0(
        flag93,
        "（查貴校上一學年所填資料，上述人員未在貴校教職員(工)資料中，請確認上述人員是否於111年10月1日-112年7月31日有退休或因故離職之情形，或是否屬於貴校教職員(工)，併請確認貴校教職員工名單是否完整正確。）"
      )
    )) #公立
} else{
  #偵測flag93是否存在。若不存在，則產生NA行
  if ('flag93' %in% ls()) {
    print("flag93")
  } else{
    flag93 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    flag93$flag93 <- ""
  }
}
