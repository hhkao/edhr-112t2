# flag100: 校長「本校到職前學校服務總年資」偏小。 -------------------------------------------------------------------
flag_person <- drev_person_1 %>%
  subset(source == "教員資料表")

#本校到職前學校服務總年資
flag_person$beoby <-
  substr(flag_person$beobdym, 1, 2) %>% as.numeric
flag_person$beobm <-
  substr(flag_person$beobdym, 3, 4) %>% as.numeric

flag_person$beob <- (flag_person$beoby + (flag_person$beobm / 12))

flag_person$err_flag <- 0
flag_person$err_flag <-
  if_else(flag_person$sertype == "校長" &
            flag_person$beob < 10,
          1,
          flag_person$err_flag)

#加註
flag_person$err_flag_txt <- ""
flag_person$err_flag_txt <- case_when(
  flag_person$err_flag == 1 ~ paste(
    flag_person$name,
    "（本校到職前學校服務總年資：",
    flag_person$beobdym,
    "）",
    sep = ""
  ),
  TRUE ~ flag_person$err_flag_txt
)

if (dim(flag_person %>% subset(err_flag == 1))[1] != 0) {
  #根據organization_id + source，展開成寬資料(wide)
  flag_person_wide_flag100 <- flag_person %>%
    subset(
      select = c(
        organization_id,
        idnumber,
        err_flag_txt,
        edu_name2,
        source,
        err_flag
      )
    ) %>%
    subset(err_flag == 1) %>%
    dcast(organization_id + source ~ err_flag_txt, value.var = "err_flag_txt")
  
  #合併所有name
  temp <-
    colnames(flag_person_wide_flag100)[3:length(colnames(flag_person_wide_flag100))]
  flag_person_wide_flag100$flag100_r <- NA
  for (i in temp) {
    flag_person_wide_flag100$flag100_r <-
      paste(flag_person_wide_flag100$flag100_r,
            flag_person_wide_flag100[[i]],
            sep = " ")
  }
  flag_person_wide_flag100$flag100_r <-
    gsub("NA ",
         replacement = "",
         flag_person_wide_flag100$flag100_r)
  flag_person_wide_flag100$flag100_r <-
    gsub(" NA",
         replacement = "",
         flag_person_wide_flag100$flag100_r)
  
  #產生檢誤報告文字
  flag100_temp <- flag_person_wide_flag100 %>%
    group_by(organization_id) %>%
    mutate(flag100_txt = paste(flag100_r, sep = ""), "") %>%
    subset(select = c(organization_id, flag100_txt)) %>%
    distinct(organization_id, flag100_txt)
  
  #根據organization_id，展開成寬資料(wide)
  flag100 <- flag100_temp %>%
    dcast(organization_id ~ flag100_txt, value.var = "flag100_txt")
  
  #合併教員資料表及職員(工)資料表報告文字
  temp <- colnames(flag100)[2:length(colnames(flag100))]
  flag100$flag100 <- NA
  for (i in temp) {
    flag100$flag100 <- paste(flag100$flag100, flag100[[i]], sep = "； ")
  }
  flag100$flag100 <- gsub("NA； ", replacement = "", flag100$flag100)
  flag100$flag100 <- gsub("； NA", replacement = "", flag100$flag100)
  
  #產生檢誤報告文字
  flag100 <- flag100 %>%
    subset(select = c(organization_id, flag100)) %>%
    distinct(organization_id, flag100) %>%
    mutate(
      flag100 = paste(
        flag100,
        "（校長『本校到職前學校服務總年資』似偏少，請確認校長以『校長身分』就任之日期，此日期請填在『本校到職日期』；校長在就任日期前，在本校及他校擔任教師與主任等全職工作之年資，請填在『本校到職前學校服務總年資』。）",
        sep = ""
      )
    )
} else{
  #偵測flag100是否存在。若不存在，則產生NA行
  if ('flag100' %in% ls()) {
    print("flag100")
  } else{
    flag100 <- drev_person_1 %>%
      distinct(organization_id, .keep_all = TRUE) %>%
      subset(select = c(organization_id))
    flag100$flag100 <- ""
  }
}

