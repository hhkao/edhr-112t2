# flag57: 學歷資料各學位別畢業學校國別/校名/科系所之（一）與（二）內容不應相同，請依學位取得實際情況修正。-------------------------------------------------------------------
flag_person <- drev_person_1

#「留職停薪原因」與「借調類別」不合理處
flag_person$err_degree <- 0
flag_person$err_degree <-
  if_else((
    flag_person$ddegreen1 == flag_person$ddegreen2 &
      flag_person$ddegreen1 != "N"
  ) &
    (
      flag_person$ddegreeu1 == flag_person$ddegreeu2 &
        flag_person$ddegreeu1 != "N"
    ) &
    (
      flag_person$ddegreeg1 == flag_person$ddegreeg2 &
        flag_person$ddegreeg1 != "N"
    ),
  1,
  flag_person$err_degree
  )
flag_person$err_degree <-
  if_else((
    flag_person$mdegreen1 == flag_person$mdegreen2 &
      flag_person$mdegreen1 != "N"
  ) &
    (
      flag_person$mdegreeu1 == flag_person$ddegreeu2 &
        flag_person$mdegreeu1 != "N"
    ) &
    (
      flag_person$mdegreeg1 == flag_person$ddegreeg2 &
        flag_person$mdegreeg1 != "N"
    ),
  1,
  flag_person$err_degree
  )
flag_person$err_degree <-
  if_else((
    flag_person$bdegreen1 == flag_person$bdegreen2 &
      flag_person$bdegreen1 != "N"
  ) &
    (
      flag_person$bdegreeu1 == flag_person$bdegreeu2 &
        flag_person$bdegreeu1 != "N"
    ) &
    (
      flag_person$bdegreeg1 == flag_person$bdegreeg2 &
        flag_person$bdegreeg1 != "N"
    ),
  1,
  flag_person$err_degree
  )
flag_person$err_degree <-
  if_else((
    flag_person$adegreen1 == flag_person$adegreen2 &
      flag_person$adegreen1 != "N"
  ) &
    (
      flag_person$adegreeu1 == flag_person$adegreeu2 &
        flag_person$adegreeu1 != "N"
    ) &
    (
      flag_person$adegreeg1 == flag_person$adegreeg2 &
        flag_person$adegreeg1 != "N"
    ),
  1,
  flag_person$err_degree
  )

#呈現姓名
flag_person$err_flag_txt <- ""
flag_person$err_flag_txt <- case_when(flag_person$err_degree == 1 ~ flag_person$name,
                                      TRUE ~ flag_person$err_flag_txt)

if (dim(flag_person %>% subset(err_degree == 1))[1] != 0) {
  #根據organization_id + source，展開成寬資料(wide)
  flag_person_wide_flag57 <- flag_person %>%
    subset(select = c(
      organization_id,
      idnumber,
      err_flag_txt,
      edu_name2,
      source,
      err_degree
    )) %>%
    subset(err_degree == 1) %>%
    dcast(organization_id + source ~ err_flag_txt, value.var = "err_flag_txt")
  
  #合併所有name
  temp <-
    colnames(flag_person_wide_flag57)[3:length(colnames(flag_person_wide_flag57))]
  flag_person_wide_flag57$flag57_r <- NA
  for (i in temp) {
    flag_person_wide_flag57$flag57_r <-
      paste(flag_person_wide_flag57$flag57_r,
            flag_person_wide_flag57[[i]],
            sep = " ")
  }
  flag_person_wide_flag57$flag57_r <-
    gsub("NA ",
         replacement = "",
         flag_person_wide_flag57$flag57_r)
  flag_person_wide_flag57$flag57_r <-
    gsub(" NA",
         replacement = "",
         flag_person_wide_flag57$flag57_r)
  
  #產生檢誤報告文字
  flag57_temp <- flag_person_wide_flag57 %>%
    group_by(organization_id) %>%
    mutate(flag57_txt = paste("請檢視修正學歷資訊內容：", source, "：", flag57_r, sep = ""),
           "") %>%
    subset(select = c(organization_id, flag57_txt)) %>%
    distinct(organization_id, flag57_txt)
  
  #根據organization_id，展開成寬資料(wide)
  flag57 <- flag57_temp %>%
    dcast(organization_id ~ flag57_txt, value.var = "flag57_txt")
  
  #合併教員資料表及職員(工)資料表報告文字
  temp <- colnames(flag57)[2:length(colnames(flag57))]
  flag57$flag57 <- NA
  for (i in temp) {
    flag57$flag57 <- paste(flag57$flag57, flag57[[i]], sep = "； ")
  }
  flag57$flag57 <- gsub("NA； ", replacement = "", flag57$flag57)
  flag57$flag57 <- gsub("； NA", replacement = "", flag57$flag57)
  
  #產生檢誤報告文字
  flag57 <- flag57 %>%
    subset(select = c(organization_id, flag57)) %>%
    distinct(organization_id, flag57) %>%
    mutate(
      flag57 = paste(
        flag57,
        "（請確認該員是否具有二個以上相同科系之*士學位，若無，則*學士學位畢業學校國別、學校、科系（二）請填寫『N』。）",
        sep = ""
      )
    )
} else{
  #偵測flag57是否存在。若不存在，則產生NA行
  if ('flag57' %in% ls()) {
    print("flag57")
  } else{
    flag57 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    flag57$flag57 <- ""
  }
}
