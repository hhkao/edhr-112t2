# flag9: 博士、碩士、學士、副學士畢業學校國別（一）～（二）應填入「本國」或者外交部網站之世界各國名稱一覽表的國家名稱（或者至少須足以辨識國家）。 -------------------------------------------------------------------
#flag9_append-------------------------------------------------------------------
flag_person <- drev_person_1

#檢視畢業學校國別欄位字元數不為3
view_flag9 <-
  distinct(
    flag_person,
    ddegreen1,
    ddegreen2,
    mdegreen1,
    mdegreen2,
    bdegreen1,
    bdegreen2,
    adegreen1,
    adegreen2,
    .keep_all = TRUE
  ) %>%
  subset(
    nchar(ddegreen1) != 3 |
      nchar(ddegreen2) != 3 |
      nchar(mdegreen1) != 3 |
      nchar(mdegreen2) != 3 |
      nchar(bdegreen1) != 3 |
      nchar(bdegreen2) != 3 |
      nchar(adegreen1) != 3 | nchar(adegreen2) != 3
  ) %>%
  subset(
    select = c(
      organization_id,
      idnumber,
      ddegreen1,
      ddegreen2,
      mdegreen1,
      mdegreen2,
      bdegreen1,
      bdegreen2,
      adegreen1,
      adegreen2,
      edu_name2,
      source
    )
  )

#"本國美國"標記為1
flag_person$err_flag <- case_when(
  flag_person$ddegreen1 == "本國美國" |
    flag_person$ddegreen2 == "本國美國" |
    flag_person$mdegreen1 == "本國美國" |
    flag_person$mdegreen2 == "本國美國" |
    flag_person$bdegreen1 == "本國美國" |
    flag_person$bdegreen2 == "本國美國" |
    flag_person$adegreen1 == "本國美國" |
    flag_person$adegreen2 == "本國美國" ~ 1,
  TRUE ~ 0
)
#---

if (dim(flag_person %>% subset(err_flag == 1))[1] != 0) {
  #根據organization_id + source，展開成寬資料(wide)
  flag_person_wide_flag9 <- flag_person %>%
    subset(select = c(organization_id,
                      idnumber,
                      name,
                      edu_name2,
                      source,
                      err_flag)) %>%
    subset(err_flag == 1) %>%
    dcast(organization_id + source ~ name, value.var = "name")
  
  #合併所有name
  temp <-
    colnames(flag_person_wide_flag9)[3:length(colnames(flag_person_wide_flag9))]
  flag_person_wide_flag9$flag9_r <- NA
  for (i in temp) {
    flag_person_wide_flag9$flag9_r <-
      paste(flag_person_wide_flag9$flag9_r,
            flag_person_wide_flag9[[i]],
            sep = " ")
  }
  flag_person_wide_flag9$flag9_r <-
    gsub("NA ", replacement = "", flag_person_wide_flag9$flag9_r)
  flag_person_wide_flag9$flag9_r <-
    gsub(" NA", replacement = "", flag_person_wide_flag9$flag9_r)
  
  #產生檢誤報告文字
  flag9_temp <- flag_person_wide_flag9 %>%
    group_by(organization_id) %>%
    mutate(flag9_txt = paste(source, "需修改畢業學校國別者：", flag9_r, sep = ""),
           "") %>%
    subset(select = c(organization_id, flag9_txt)) %>%
    distinct(organization_id, flag9_txt)
  
  #根據organization_id，展開成寬資料(wide)
  flag9 <- flag9_temp %>%
    dcast(organization_id ~ flag9_txt, value.var = "flag9_txt")
  
  #合併教員資料表及職員(工)資料表報告文字
  temp <- colnames(flag9)[2:length(colnames(flag9))]
  flag9$flag9 <- NA
  for (i in temp) {
    flag9$flag9 <- paste(flag9$flag9, flag9[[i]], sep = "； ")
  }
  flag9$flag9 <- gsub("NA； ", replacement = "", flag9$flag9)
  flag9$flag9 <- gsub("； NA", replacement = "", flag9$flag9)
  
  #產生檢誤報告文字
  flag9 <- flag9 %>%
    subset(select = c(organization_id, flag9)) %>%
    distinct(organization_id, flag9)
} else{
  #偵測flag9是否存在。若不存在，則產生NA行
  if ('flag9' %in% ls()) {
    print("flag9")
  } else{
    flag9 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    flag9$flag9 <- ""
  }
}
