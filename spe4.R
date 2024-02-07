# spe4: 校長、以及高中部與中學部之專任、代理教師、主任教官、教官，若未調離本校、商借至其他學校單位、留職停薪、請假或停職(停聘)者，於教學資料表應有資料。 -------------------------------------------------------------------
flag_person <- drev_P_load
#包含"未填在教學資料表"者

#有請假、留停、借調、商借或停聘的情形，且每週實際教學節數任一欄位不為0(加總 > 0)，則抓出
flag_person$err_flag <- 0
flag_person$err_flag <- if_else(
  flag_person$load == "" &
    flag_person$source == "教員資料表" &
    flag_person$empunit %in% c("高中部日間部", "高中部進修部", "中學部", "中學部進修部") &
    flag_person$emptype %in% c("專任", "代理", "代理(連)") &
    flag_person$sertype %in% c("校長", "教師", "主任教官", "教官") &
    (
      flag_person$leave == "N" |
        flag_person$levpay == "N" |
        !grepl("借調至", flag_person$brtype) |
        !grepl("商借至", flag_person$levpay) |
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
  flag_person_wide_spe4 <- flag_person %>%
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
    colnames(flag_person_wide_spe4)[2:length(colnames(flag_person_wide_spe4))]
  flag_person_wide_spe4$spe4_r <- NA
  for (i in temp) {
    flag_person_wide_spe4$spe4_r <-
      paste(flag_person_wide_spe4$spe4_r,
            flag_person_wide_spe4[[i]],
            sep = " ")
  }
  flag_person_wide_spe4$spe4_r <-
    gsub("NA ",
         replacement = "",
         flag_person_wide_spe4$spe4_r)
  flag_person_wide_spe4$spe4_r <-
    gsub(" NA",
         replacement = "",
         flag_person_wide_spe4$spe4_r)
  
  #產生檢誤報告文字
  spe4_temp <- flag_person_wide_spe4 %>%
    group_by(organization_id) %>%
    mutate(spe4_txt = paste(spe4_r, sep = ""), "") %>%
    subset(select = c(organization_id, spe4_txt)) %>%
    distinct(organization_id, spe4_txt)
  
  #根據organization_id，展開成寬資料(wide)
  spe4 <- spe4_temp %>%
    dcast(organization_id ~ spe4_txt, value.var = "spe4_txt")
  
  #合併教員資料表及職員(工)資料表報告文字
  temp <- colnames(spe4)[2:length(colnames(spe4))]
  spe4$spe4 <- NA
  for (i in temp) {
    spe4$spe4 <- paste(spe4$spe4, spe4[[i]], sep = "； ")
  }
  spe4$spe4 <- gsub("NA； ", replacement = "", spe4$spe4)
  spe4$spe4 <- gsub("； NA", replacement = "", spe4$spe4)
  
  #產生檢誤報告文字
  spe4 <- spe4 %>%
    subset(select = c(organization_id, spe4)) %>%
    distinct(organization_id, spe4) %>%
    mutate(spe4 = paste("姓名：",
                        spe4,
                        sep = ""))
} else{
  #偵測spe4是否存在。若不存在，則產生NA行
  if ('spe4' %in% ls()) {
    print("spe4")
  } else{
    spe4 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    spe4$spe4 <- ""
  }
}
