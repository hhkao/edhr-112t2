# flag58: 授課者的實際教學節數原則需滿足基本鐘點。請確認教學資料表「高中日間部課程全學期授課之每週實際教學節數」、「高中進修部課程全學期授課之每週實際教學節數」、「高中課程非全學期授課之實際教學總節數」、「國中課程全學期授課之每週實際教學節數」、「每週基本教學節數」、「減授節數」六欄位之值是否正確且符合實際狀況。 -------------------------------------------------------------------
flag_person <- drev_P_load %>%
  subset(load == 1) %>%
  subset(!is.na(basic) | !is.na(cut) | !is.na(othertime)) #NA在這不處理

#超鐘點 =
# (高中日間部課程全學期授課之每週實際教學節數
#   +
#     高中進修部課程全學期授課之每週實際教學節數
#   +
#     高中課程非全學期授課之實際教學總節數 /  20
#   +
#     國中課程全學期授課之每週實際教學節數
#
#   – (每週基本教學節數 – 減授節數)
flag_person <- flag_person %>%
  mutate(overtime = (hschftime + hschcftime + (hschptime / 20) + jhschftime) - (basic - cut))


flag_person$err_flag <- 0
flag_person$err_flag <- if_else(flag_person$overtime < 0,
                                1,
                                flag_person$err_flag)
#排除教官、主任教官、專輔教師
flag_person$err_flag <-
  if_else(flag_person$sertype %in% c("主任教官", "教官"),
          0,
          flag_person$err_flag)
flag_person$err_flag <- if_else(flag_person$counselor == "Y",
                                0,
                                flag_person$err_flag)

#加註
flag_person$err_flag_txt <- ""
flag_person$err_flag_txt <- case_when(
  flag_person$err_flag == 1 ~ paste(flag_person$name,
                                    sep = ""),
  TRUE ~ flag_person$err_flag_txt
)

if (dim(flag_person %>% subset(err_flag == 1))[1] != 0) {
  #根據organization_id + source，展開成寬資料(wide)
  flag_person_wide_flag58 <- flag_person %>%
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
    colnames(flag_person_wide_flag58)[3:length(colnames(flag_person_wide_flag58))]
  flag_person_wide_flag58$flag58_r <- NA
  for (i in temp) {
    flag_person_wide_flag58$flag58_r <-
      paste(flag_person_wide_flag58$flag58_r,
            flag_person_wide_flag58[[i]],
            sep = " ")
  }
  flag_person_wide_flag58$flag58_r <-
    gsub("NA ",
         replacement = "",
         flag_person_wide_flag58$flag58_r)
  flag_person_wide_flag58$flag58_r <-
    gsub(" NA",
         replacement = "",
         flag_person_wide_flag58$flag58_r)
  
  #產生檢誤報告文字
  flag58_temp <- flag_person_wide_flag58 %>%
    group_by(organization_id) %>%
    mutate(flag58_txt = paste(flag58_r, sep = ""), "") %>%
    subset(select = c(organization_id, flag58_txt)) %>%
    distinct(organization_id, flag58_txt)
  
  #根據organization_id，展開成寬資料(wide)
  flag58 <- flag58_temp %>%
    dcast(organization_id ~ flag58_txt, value.var = "flag58_txt")
  
  #合併教員資料表及職員(工)資料表報告文字
  temp <- colnames(flag58)[2:length(colnames(flag58))]
  flag58$flag58 <- NA
  for (i in temp) {
    flag58$flag58 <- paste(flag58$flag58, flag58[[i]], sep = "； ")
  }
  flag58$flag58 <- gsub("NA； ", replacement = "", flag58$flag58)
  flag58$flag58 <- gsub("； NA", replacement = "", flag58$flag58)
  
  #產生檢誤報告文字
  flag58 <- flag58 %>%
    subset(select = c(organization_id, flag58)) %>%
    distinct(organization_id, flag58) %>%
    mutate(
      flag58 = paste(
        "請確認該名教員之「每週基本教學節數」、「減授節數」、「高中日間部課程全學期授課之每週實際教學節數」、「高中進修部課程全學期授課之每週實際教學節數」、「高中課程非全學期授課之實際教學總節數」、「國中課程全學期授課之每週實際教學節數」：",
        flag58,
        "（請依欄位說明確認填報內容，並請注意同一職務所減授之鐘點，勿重複計算）",
        sep = ""
      )
    )
} else{
  #偵測flag58是否存在。若不存在，則產生NA行
  if ('flag58' %in% ls()) {
    print("flag58")
  } else{
    flag58 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    flag58$flag58 <- ""
  }
}
