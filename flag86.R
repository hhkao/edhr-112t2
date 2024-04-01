# flag86: 上一學年（期）專任教職員（工）若未於本學年（期）教職員（工）資料表內，則應填列於離退教職員（工）資料表。 -------------------------------------------------------------------
flag_person <- drev_P_retire_merge_pre %>%
  rename(name = name.x,
         name_retire = name,
         edu_name2 = edu_name2.x)

#抓出:有出現在上一期資料，但在本次填報已被刪除，但退休資料表沒有出現的專任人員
#只出現在上一期(pre = 1 & now = NA)   且為專任    但本次離退表卻沒有出現(空白)(retire = NA)
flag_person$err_flag <- 0
flag_person$err_flag <-
  if_else(
    flag_person$pre == 1 &
      is.na(flag_person$now) &
      is.na(flag_person$retire) &
      flag_person$emptype.y == "專任",
    1,
    flag_person$err_flag
  )

#呈現姓名
flag_person$err_flag_txt <- ""
flag_person$err_flag_txt <- case_when(flag_person$err_flag == 1 ~ flag_person$name.y,
                                      TRUE ~ flag_person$err_flag_txt)

if (dim(flag_person %>% subset(err_flag == 1))[1] != 0) {
  #根據organization_id，展開成寬資料(wide)
  flag_person_wide_flag86 <- flag_person %>%
    subset(select = c(organization_id,
                      idnumber,
                      err_flag_txt,
                      edu_name2,
                      err_flag)) %>%
    subset(err_flag == 1) %>%
    dcast(organization_id ~ err_flag_txt,
          value.var = "err_flag_txt",
          fun.aggregate = first)
  
  #合併所有name
  temp <-
    colnames(flag_person_wide_flag86)[2:length(colnames(flag_person_wide_flag86))]
  flag_person_wide_flag86$flag86_r <- NA
  for (i in temp) {
    flag_person_wide_flag86$flag86_r <-
      paste(flag_person_wide_flag86$flag86_r,
            flag_person_wide_flag86[[i]],
            sep = " ")
  }
  flag_person_wide_flag86$flag86_r <-
    gsub("NA ",
         replacement = "",
         flag_person_wide_flag86$flag86_r)
  flag_person_wide_flag86$flag86_r <-
    gsub(" NA",
         replacement = "",
         flag_person_wide_flag86$flag86_r)
  
  #產生檢誤報告文字
  flag86_temp <- flag_person_wide_flag86 %>%
    group_by(organization_id) %>%
    mutate(flag86_txt = paste("姓名：", flag86_r, sep = ""), "") %>%
    subset(select = c(organization_id, flag86_txt)) %>%
    distinct(organization_id, flag86_txt)
  
  #根據organization_id，展開成寬資料(wide)
  flag86 <- flag86_temp %>%
    dcast(organization_id ~ flag86_txt, value.var = "flag86_txt")
  
  #合併教員資料表及職員(工)資料表報告文字
  temp <- colnames(flag86)[2:length(colnames(flag86))]
  flag86$flag86 <- NA
  for (i in temp) {
    flag86$flag86 <- paste(flag86$flag86, flag86[[i]], sep = "； ")
  }
  flag86$flag86 <- gsub("NA； ", replacement = "", flag86$flag86)
  flag86$flag86 <- gsub("； NA", replacement = "", flag86$flag86)
  
  #產生檢誤報告文字
  flag86 <- flag86 %>%
    subset(select = c(organization_id, flag86)) %>%
    distinct(organization_id, flag86) %>%
    mutate(flag86 = if_else(
      substr(organization_id, 3, 3) == "1",
      paste0(
        flag86,
        "（經比對貴校上一學年所填資料，上述人員並未出現於本學期的教員資料表或職員(工)資料表，請確認渠等是否於112學年度第一學期（112年8月1日(含)至113年1月31日(含)）退休或因故離職等，若於該期間退休或因故離職等，應於離退教職員(工)資料表填寫資料。如非於該期間退休或因故離職，或已介聘、調至他校，請來電告知。）"
      ),
      #私立
      paste0(
        flag86,
        "（經比對貴校上一學年所填資料，上述人員並未出現於本學期的教員資料表或職員(工)資料表，請確認渠等是否於112學年度第一學期（112年8月1日(含)至113年1月31日(含)）退休或因故離職等，若於該期間退休或因故離職等，應於離退教職員(工)資料表填寫資料。如非於該期間退休或因故離職，或已介聘、調至他校，請來電告知。）"
      )
    )) #公立
  
  
} else{
  #偵測flag86是否存在。若不存在，則產生NA行
  if ('flag86' %in% ls()) {
    print("flag86")
  } else{
    flag86 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    flag86$flag86 <- ""
  }
}
