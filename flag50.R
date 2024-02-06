# flag50: 留職停薪原因為「應徵入伍留職停薪」、「奉派協助友邦留職停薪」、「育嬰留職停薪」、「侍親留職停薪」、「依親留職停薪」、「出國進修或研究留職停薪」、「易服勞役留職停薪」、「延長留職停薪」、「照護配偶或子女留職停薪」、「國內外進修期滿延長留職停薪」、「延長病假期滿留職停薪」、「因公傷病公假期滿留職停薪」、「留職停薪/停聘」、「其他情事留職停薪」，在借調類別應填寫N。 -------------------------------------------------------------------
flag_person <- drev_person_1

flag_person$brtype <-
  if_else(flag_person$brtype == "NA", "N", flag_person$brtype)

#留職停薪原因不合理處
flag_person$err_lev <- 0
flag_person$err_lev <-
  if_else(
    flag_person$levpay %in% c(
      "應徵入伍留職停薪",
      "奉派協助友邦留職停薪",
      "育嬰留職停薪",
      "侍親留職停薪",
      "依親留職停薪",
      "出國進修或研究留職停薪",
      "易服勞役留職停薪",
      "延長留職停薪",
      "易服勞役留職停薪",
      "照護配偶或子女留職停薪",
      "國內外進修期滿延長留職停薪",
      "延長病假期滿留職停薪",
      "留職停薪/停聘",
      "其他情事留職停薪"
    )
    &
      flag_person$brtype != "N",
    1,
    flag_person$err_lev
  )

#呈現姓名
flag_person$err_flag_txt <- ""
flag_person$err_flag_txt <-
  case_when(flag_person$err_lev == 1 ~ flag_person$name,
            TRUE ~ flag_person$err_flag_txt)

if (dim(flag_person %>% subset(err_lev == 1))[1] != 0) {
  #根據organization_id + source，展開成寬資料(wide)
  flag_person_wide_flag50 <- flag_person %>%
    subset(select = c(
      organization_id,
      idnumber,
      err_flag_txt,
      edu_name2,
      source,
      err_lev
    )) %>%
    subset(err_lev == 1) %>%
    dcast(organization_id + source ~ err_flag_txt, value.var = "err_flag_txt")
  
  #合併所有name
  temp <-
    colnames(flag_person_wide_flag50)[3:length(colnames(flag_person_wide_flag50))]
  flag_person_wide_flag50$flag50_r <- NA
  for (i in temp) {
    flag_person_wide_flag50$flag50_r <-
      paste(flag_person_wide_flag50$flag50_r,
            flag_person_wide_flag50[[i]],
            sep = " ")
  }
  flag_person_wide_flag50$flag50_r <-
    gsub("NA ",
         replacement = "",
         flag_person_wide_flag50$flag50_r)
  flag_person_wide_flag50$flag50_r <-
    gsub(" NA",
         replacement = "",
         flag_person_wide_flag50$flag50_r)
  
  #產生檢誤報告文字
  flag50_temp <- flag_person_wide_flag50 %>%
    group_by(organization_id) %>%
    mutate(flag50_txt = paste(source, "需核對「留職停薪原因」與「借調類別」：", flag50_r, sep = ""),
           "") %>%
    subset(select = c(organization_id, flag50_txt)) %>%
    distinct(organization_id, flag50_txt)
  
  #根據organization_id，展開成寬資料(wide)
  flag50 <- flag50_temp %>%
    dcast(organization_id ~ flag50_txt, value.var = "flag50_txt")
  
  #合併教員資料表及職員(工)資料表報告文字
  temp <- colnames(flag50)[2:length(colnames(flag50))]
  flag50$flag50 <- NA
  for (i in temp) {
    flag50$flag50 <- paste(flag50$flag50, flag50[[i]], sep = "； ")
  }
  flag50$flag50 <- gsub("NA； ", replacement = "", flag50$flag50)
  flag50$flag50 <- gsub("； NA", replacement = "", flag50$flag50)
  
  #產生檢誤報告文字
  flag50 <- flag50 %>%
    subset(select = c(organization_id, flag50)) %>%
    distinct(organization_id, flag50) %>%
    mutate(flag50 = paste(flag50, "", sep = ""))
} else{
  #偵測flag50是否存在。若不存在，則產生NA行
  if ('flag50' %in% ls()) {
    print("flag50")
  } else{
    flag50 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    flag50$flag50 <- ""
  }
}
