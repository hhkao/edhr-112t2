# # 人事資料表與教學資料表合併 -----------------------------------------------------------
# data_load <- data_load %>%
#   mutate(load = 1)
# drev_P_load <-
#   merge(
#     x = drev_person_1,
#     y = data_load,
#     by = c("organization_id", "idnumber"),
#     all.x = TRUE,
#     all.y = TRUE
#   )
# #name欄位重複處理
# drev_P_load$name <- drev_P_load$name.x
# drev_P_load <- drev_P_load %>%
#   select(-c("name.x", "name.y"))
# #將每週基本教學節數、減授節數、校外兼課總節數的"NA"改為空值，以利後續分析
# drev_P_load$basic <-
#   if_else(drev_P_load$basic == "NA", "", drev_P_load$basic)
# drev_P_load$cut <-
#   if_else(drev_P_load$cut == "NA", "", drev_P_load$cut)
# drev_P_load$othertime <-
#   if_else(drev_P_load$othertime == "NA", "", drev_P_load$othertime)
# 
# #將merge後出現NA的欄位改為空值，避免後續出錯
# drev_P_load[is.na(drev_P_load)] <- ""
# 
# drev_P_load$basic <- drev_P_load$basic %>% as.numeric()
# drev_P_load$cut <- drev_P_load$basic %>% as.numeric()
# drev_P_load$hschftime <- drev_P_load$hschftime %>% as.numeric()
# drev_P_load$hschcftime <- drev_P_load$hschcftime %>% as.numeric()
# drev_P_load$hschptime <- drev_P_load$hschptime %>% as.numeric()
# drev_P_load$jhschftime <- drev_P_load$jhschftime %>% as.numeric()
# drev_P_load$othertime <- drev_P_load$basic %>% as.numeric()

# 人事資料表與離退教職員(工)資料表合併 (inner join) -----------------------------------------------------------
drev_P_retire <-
  merge(
    x = drev_person_1,
    y = data_retire,
    by = c("organization_id", "idnumber")
  )

# 前一期人事資料表與離退教職員(工)資料表合併 (inner join) -----------------------------------------------------------
drev_P_retire_pre_inner <-
  merge(
    x = drev_person_pre,
    y = data_retire,
    by = c("organization_id", "idnumber")
  )
# 前一期人事資料表與離退教職員(工)資料表合併 (right join) -----------------------------------------------------------
drev_person_pre_1 <- drev_person_pre %>%
  mutate(pre = 1)
drev_P_retire_pre_right <-
  merge(
    x = drev_person_pre_1,
    y = data_retire,
    by = c("organization_id", "idnumber"),
    all.y = TRUE
  )

#學校名稱 (本次已上傳學校)
edu_name2 <- data.frame("organization_id" = drev_person$organization_id,
                        "edu_name2" = drev_person$edu_name2) %>%
  distinct(organization_id, .keep_all = TRUE)
#本期人事資料表與前一期人事資料表合併 -----------------------------------------------------------
drev_person_2 <- drev_person %>%
  mutate(now = 1)
drev_P_retire_merge_pre <-
  merge(
    x = drev_person_2,
    y = drev_person_pre_1,
    by = c("organization_id", "idnumber"),
    all.x = TRUE,
    all.y = TRUE
  )
#只留已上傳學校
drev_P_retire_merge_pre <-
  merge(x = drev_P_retire_merge_pre,
        y = edu_name2,
        by = c("organization_id"))
#再與本次離退表合併
data_retire_1 <- data_retire %>%
  mutate(retire = 1)
drev_P_retire_merge_pre <-
  merge(
    x = drev_P_retire_merge_pre,
    y = data_retire_1,
    by = c("organization_id", "idnumber"),
    all.x = TRUE,
    all.y = TRUE
  )