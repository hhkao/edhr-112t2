# flag64: 本校任職需扣除年資非0000者分布偏高。 -------------------------------------------------------------------
flag_person <- drev_person_1

flag_person$dese <- 0
flag_person$dese <-
  if_else(flag_person$desedym != "0000", 1, flag_person$dese)


flag_person$jj <- 1

flag_person_wide_flag64 <-
  aggregate(cbind(dese, jj) ~ organization_id, flag_person, sum)

flag_person_wide_flag64$flag_err <- 0
flag_person_wide_flag64$err_flag_txt <-
  if_else(
    flag_person_wide_flag64$dese / flag_person_wide_flag64$jj > 0.25,
    "扣除年資不為零的人數似偏高，請再依欄位說明確認。",
    ""
  )

if (dim(flag_person_wide_flag64 %>% subset(err_flag_txt != ""))[1] != 0) {
  #根據organization_id，展開成寬資料(wide)
  flag64 <- flag_person_wide_flag64 %>%
    subset(err_flag_txt != "") %>%
    dcast(organization_id ~ err_flag_txt, value.var = "err_flag_txt")
  
  #合併教員資料表及職員(工)資料表報告文字
  temp <- colnames(flag64)[2:length(colnames(flag64))]
  flag64$flag64 <- NA
  for (i in temp) {
    flag64$flag64 <- paste(flag64$flag64, flag64[[i]], sep = "； ")
  }
  flag64$flag64 <- gsub("NA； ", replacement = "", flag64$flag64)
  flag64$flag64 <- gsub("； NA", replacement = "", flag64$flag64)
  
  #產生檢誤報告文字
  flag64 <- flag64 %>%
    subset(select = c(organization_id, flag64)) %>%
    distinct(organization_id, flag64)
} else{
  #偵測flag64是否存在。若不存在，則產生NA行
  if ('flag64' %in% ls()) {
    print("flag64")
  } else{
    flag64 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    flag64$flag64 <- ""
  }
}
