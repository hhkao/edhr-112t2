# flag1: 學校（副）校長、一級單位主管名單的完整度 -------------------------------------------------------------------
flag_person <- drev_person_1 %>%
  mutate(
    admin1 = 0,
    admin2 = 0,
    admin3 = 0,
    admin4 = 0,
    admin5 = 0,
    admin6 = 0,
    admin7 = 0,
    admin8 = 0,
    admin9 = 0
  )

flag_person$admin1 <- case_when(
  flag_person$sertype == "校長" |
    flag_person$admintitle1 == "校長" |
    flag_person$admintitle1 == "校長1" |
    flag_person$admintitle2 == "校長1" |
    flag_person$admintitle3 == "校長1" |
    grepl("/校長1", flag_person$admintitle1) |
    grepl("校長1/", flag_person$admintitle1) |
    grepl("/校長1", flag_person$admintitle2) |
    grepl("校長1/", flag_person$admintitle2) |
    grepl("/校長1", flag_person$admintitle3) |
    grepl("校長1/", flag_person$admintitle3) ~ 1,
  TRUE ~ flag_person$admin1
)

flag_person$admin7 <- case_when(flag_person$typeV == 0 ~ 1,
                                TRUE ~ flag_person$admin7)

temp <- c("0", "1", "2", "3")
for (x in temp) {
  flag_person$admin2 <- case_when(
    grepl("教務", flag_person[[paste("adminunit", x, sep = "")]])                                                                                                                                     &
      (grepl("主任$", flag_person[[paste("admintitle", x, sep = "")]]) |
         grepl("主任1$", flag_person[[paste("admintitle", x, sep = "")]])) &
      !grepl("主任教官", flag_person[[paste("admintitle", x, sep = "")]]) &
      !grepl("科主任", flag_person[[paste("admintitle", x, sep = "")]]) &
      !grepl("學程主任", flag_person[[paste("admintitle", x, sep = "")]]) ~ 1,
    TRUE ~ flag_person$admin2
  )
}
for (x in temp) {
  flag_person$admin3 <- case_when(
    (grepl("學務", flag_person[[paste("adminunit", x, sep = "")]]) |
       grepl("學生事務", flag_person[[paste("adminunit", x, sep = "")]]))                                                                &
      (grepl("主任$", flag_person[[paste("admintitle", x, sep = "")]]) |
         grepl("主任1$", flag_person[[paste("admintitle", x, sep = "")]])) &
      !grepl("主任教官", flag_person[[paste("admintitle", x, sep = "")]]) &
      !grepl("科主任", flag_person[[paste("admintitle", x, sep = "")]]) &
      !grepl("學程主任", flag_person[[paste("admintitle", x, sep = "")]]) ~ 1,
    TRUE ~ flag_person$admin3
  )
}
for (x in temp) {
  flag_person$admin4 <- case_when(
    grepl("總務", flag_person[[paste("adminunit", x, sep = "")]])                                                                                                                                     &
      (grepl("主任$", flag_person[[paste("admintitle", x, sep = "")]]) |
         grepl("主任1$", flag_person[[paste("admintitle", x, sep = "")]])) &
      !grepl("主任教官", flag_person[[paste("admintitle", x, sep = "")]]) &
      !grepl("科主任", flag_person[[paste("admintitle", x, sep = "")]]) &
      !grepl("學程主任", flag_person[[paste("admintitle", x, sep = "")]]) ~ 1,
    TRUE ~ flag_person$admin4
  )
}
for (x in temp) {
  flag_person$admin5 <- case_when(
    grepl("輔導", flag_person[[paste("adminunit", x, sep = "")]])                                                                                                                                     &
      (grepl("主任$", flag_person[[paste("admintitle", x, sep = "")]]) |
         grepl("主任1$", flag_person[[paste("admintitle", x, sep = "")]])) &
      !grepl("主任輔導教師$", flag_person[[paste("admintitle", x, sep = "")]]) &
      !grepl("主任教官", flag_person[[paste("admintitle", x, sep = "")]]) &
      !grepl("科主任", flag_person[[paste("admintitle", x, sep = "")]]) &
      !grepl("學程主任", flag_person[[paste("admintitle", x, sep = "")]]) ~ 1,
    TRUE ~ flag_person$admin5
  )
}
for (x in temp) {
  flag_person$admin6 <- case_when(
    (
      grepl("圖書", flag_person[[paste("adminunit", x, sep = "")]]) |
        grepl("圖資", flag_person[[paste("adminunit", x, sep = "")]]) |
        grepl("圖書資訊", flag_person[[paste("adminunit", x, sep = "")]])
    ) &
      ((grepl("主任$", flag_person[[paste("admintitle", x, sep = "")]]) |
          grepl("主任1$", flag_person[[paste("admintitle", x, sep = "")]])) &
         !grepl("主任教官", flag_person[[paste("admintitle", x, sep = "")]]) &
         !grepl("科主任", flag_person[[paste("admintitle", x, sep = "")]]) &
         !grepl("學程主任", flag_person[[paste("admintitle", x, sep = "")]])
      ) |
      grepl("^館長$", flag_person[[paste("admintitle", x, sep = "")]]) ~ 1,
    TRUE ~ flag_person$admin6
  )
}
for (x in temp) {
  flag_person$admin7 <- case_when(
    grepl("實習", flag_person[[paste("adminunit", x, sep = "")]]) &
      flag_person$typeV == 1                                                                                                            &
      (grepl("主任$", flag_person[[paste("admintitle", x, sep = "")]]) |
         grepl("主任1$", flag_person[[paste("admintitle", x, sep = "")]])) &
      !grepl("主任教官", flag_person[[paste("admintitle", x, sep = "")]]) &
      !grepl("科主任", flag_person[[paste("admintitle", x, sep = "")]]) &
      !grepl("學程主任", flag_person[[paste("admintitle", x, sep = "")]]) ~ 1,
    TRUE ~ flag_person$admin7
  )
}
for (x in temp) {
  flag_person$admin8 <- case_when(
    grepl("人事", flag_person[[paste("adminunit", x, sep = "")]])                                                                                                                                      &
      ((grepl("主任$", flag_person[[paste("admintitle", x, sep = "")]]) |
          grepl("主任1$", flag_person[[paste("admintitle", x, sep = "")]])) &
         !grepl("主任教官", flag_person[[paste("admintitle", x, sep = "")]]) &
         !grepl("科主任", flag_person[[paste("admintitle", x, sep = "")]]) &
         !grepl("學程主任", flag_person[[paste("admintitle", x, sep = "")]])
      ) |
      grepl("^人事管理員$", flag_person[[paste("admintitle", x, sep = "")]]) ~ 1,
    TRUE ~ flag_person$admin8
  )
}
for (x in temp) {
  flag_person$admin9 <- case_when(
    (grepl("會計", flag_person[[paste("adminunit", x, sep = "")]]) |
       grepl("主計", flag_person[[paste("adminunit", x, sep = "")]]))                                                                    &
      ((grepl("主任$", flag_person[[paste("admintitle", x, sep = "")]]) |
          grepl("主任1$", flag_person[[paste("admintitle", x, sep = "")]])) &
         !grepl("主任教官", flag_person[[paste("admintitle", x, sep = "")]]) &
         !grepl("科主任", flag_person[[paste("admintitle", x, sep = "")]]) &
         !grepl("學程主任", flag_person[[paste("admintitle", x, sep = "")]])
      ) |
      grepl("^主計員$", flag_person[[paste("admintitle", x, sep = "")]]) |
      grepl("^主計員$", flag_person[[paste("admintitle", x, sep = "")]]) ~ 1,
    TRUE ~ flag_person$admin9
  )
}

#NOTE:實習處主任設置條件為：1.只有專業群科 or 2.只有專業群科和綜合高中 or 3.只有專業群科和實用技能學程
#NOTE:2021/04/20討論：只要有專業群科的學校，都需檢查有無實習處，其他類別不管

flag1 <- flag_person %>%
  group_by(organization_id) %>%
  mutate(
    admin1 = max(admin1),
    admin2 = max(admin2),
    admin3 = max(admin3),
    admin4 = max(admin4),
    admin5 = max(admin5),
    admin6 = max(admin6),
    admin7 = max(admin7),
    admin8 = max(admin8),
    admin9 = max(admin9)
  ) %>%
  mutate(
    admin1_txt = if_else(admin1 == 0, "校長", ""),
    admin2_txt = if_else(admin2 == 0, "教務處主管", ""),
    admin3_txt = if_else(admin3 == 0, "學務處主管", ""),
    admin4_txt = if_else(admin4 == 0, "總務處主管", ""),
    admin5_txt = if_else(admin5 == 0, "輔導室主管", ""),
    admin6_txt = if_else(admin6 == 0, "圖書館主管", ""),
    admin7_txt = if_else(admin7 == 0, "實習處主管", ""),
    admin8_txt = if_else(admin8 == 0, "人事室主管", ""),
    admin9_txt = if_else(admin9 == 0, "主（會）計室主管", "")
  ) %>%
  mutate(
    flag1 = paste(
      "尚待增補之學校主管：",
      admin1_txt,
      admin2_txt,
      admin3_txt,
      admin4_txt,
      admin5_txt,
      admin6_txt,
      admin7_txt,
      admin8_txt,
      admin9_txt,
      sep = " "
    )
  ) %>%
  mutate(flag1 = recode(gsub("\\s+", " ", flag1), `尚待增補之學校主管： ` = "")) %>%
  mutate(flag1 = if_else(
    flag1 != "",
    paste(flag1, "（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）", sep = ""),
    flag1
  )) %>%
  subset(select = c(organization_id, flag1)) %>%
  distinct(organization_id, flag1)

flag1$flag1 <- gsub("： ", replacement = "：", flag1$flag1)
flag1$flag1 <- gsub(" （", replacement = "（", flag1$flag1)


#偵測flag1是否存在。若不存在，則產生NA行
if ('flag1' %in% ls()) {
  print("flag1")
} else{
  flag1 <- drev_person_1 %>%
    distinct(organization_id, .keep_all = TRUE) %>%
    subset(select = c(organization_id))
  flag1$flag1 <- ""
}
