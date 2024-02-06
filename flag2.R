# flag2: 設有專業類科二科以上的高級中等學校，本校應設有科主任（或有同類學程時應設有學程主任）。 -------------------------------------------------------------------
#flag2_append-------------------------------------------------------------------
data_schtype_wide_flag2 <-
  aggregate(typeV ~ organization_id + type_code,
            data_schtype_wide,
            sum) %>%
  subset(type_code == "V" & typeV >= 2) %>%
  rename(flag_typeV = typeV)

flag_person <- drev_person_1
flag_person <-
  merge(
    x = flag_person,
    y = data_schtype_wide_flag2,
    by = c("organization_id"),
    all.x = TRUE
  )

flag_person$err_flag <- case_when(flag_person$flag_typeV >= 2 ~ 1,
                                  TRUE ~ 0)
#---

temp <- c("0", "1", "2", "3")
for (x in temp) {
  flag_person$err_flag <-
    case_when((grepl("科主任", flag_person[[paste("admintitle", x, sep = "")]]) |
                 (
                   grepl("主任", flag_person[[paste("admintitle", x, sep = "")]]) &
                     grepl("科$", flag_person[[paste("adminunit", x, sep = "")]])
                 )) &
                flag_person$flag_typeV >= 2 ~ 0,
              TRUE ~ flag_person$err_flag)
}

flag2 <- flag_person %>%
  group_by(organization_id) %>%
  mutate(err_flag = min(err_flag)) %>%
  mutate(flag2 = if_else(err_flag == 1, "請學校確認是否設置科主任或學程主任", "")) %>%
  subset(select = c(organization_id, flag2)) %>%
  distinct(organization_id, flag2)

#偵測flag2是否存在。若不存在，則產生NA行
if ('flag2' %in% ls()) {
  print("flag2")
} else{
  flag2 <- drev_person_1 %>%
    distinct(organization_id, .keep_all = TRUE) %>%
    subset(select = c(organization_id))
  flag2$flag2 <- ""
}
