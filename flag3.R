# flag3: 設有專門學程總班級數四班以上的高級中等學校，本校應設有學程主任。 -------------------------------------------------------------------
#flag3_append-------------------------------------------------------------------
data_schtype_wide_flag3 <- data_schtype_wide %>%
  subset(dep1_code != "11")

data_schtype_wide_flag3 <-
  aggregate(nclass ~ organization_id + type_code,
            data_schtype_wide_flag3,
            sum) %>%
  rename(flag_nclass = nclass) %>%
  subset(type_code == "M" & flag_nclass >= 4) %>%
  distinct(organization_id, .keep_all = TRUE)

flag_person <- drev_person_1
flag_person <-
  merge(
    x = flag_person,
    y = data_schtype_wide_flag3,
    by = c("organization_id"),
    all.x = TRUE
  )

flag_person$err_flag <- case_when((flag_person$type_code == "M" &
                                     flag_person$flag_nclass >= 4) ~ 1,
                                  TRUE ~ 0)
#---

temp <- c("0", "1", "2", "3")
for (x in temp) {
  flag_person$err_flag <- case_when(
    grepl("學程主任", flag_person[[paste("admintitle", x, sep = "")]]) &
      (flag_person$type_code == "M" & flag_person$flag_nclass >= 4) ~ 0,
    TRUE ~ flag_person$err_flag
  )
}

flag3 <- flag_person %>%
  group_by(organization_id) %>%
  mutate(err_flag = min(err_flag)) %>%
  mutate(flag3 = if_else(err_flag == 1, "請學校確認是否設置學程主任", "")) %>%
  subset(select = c(organization_id, flag3)) %>%
  distinct(organization_id, flag3)

#偵測flag3是否存在。若不存在，則產生NA行
if ('flag3' %in% ls()) {
  print("flag3")
} else{
  flag3 <- drev_person_1 %>%
    distinct(organization_id, .keep_all = TRUE) %>%
    subset(select = c(organization_id))
  flag3$flag3 <- ""
}
