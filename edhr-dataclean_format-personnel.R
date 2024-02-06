#調整英文字母大小寫
temp <-
  c(
    "idnumber",
    "implcls",
    "skillteacher",
    "counselor",
    "speteacher",
    "joiteacher",
    "expecter",
    "study",
    "ddegreen1",
    "ddegreeu1",
    "ddegreeg1",
    "ddegreen2",
    "ddegreeu2",
    "ddegreeg2",
    "mdegreen1",
    "mdegreeu1",
    "mdegreeg1",
    "mdegreen2",
    "mdegreeu2",
    "mdegreeg2",
    "bdegreen1",
    "bdegreeu1",
    "bdegreeg1",
    "bdegreen2",
    "bdegreeu2",
    "bdegreeg2",
    "adegreen1",
    "adegreeu1",
    "adegreeg1",
    "adegreen2",
    "adegreeu2",
    "adegreeg2",
    "leave",
    "admintitle",
    "adminunit",
    "admintitle1",
    "adminunit1",
    "admintitle2",
    "adminunit2",
    "admintitle3",
    "adminunit3"
  )
for (x in temp) {
  drev_person_1[[x]] <- drev_person_1[[x]] %>% toupper()
}

#刪除空格的方程式
trim <- function (x) {
  gsub("\\s+", "", x)
}

#刪除空格 "\\s+"
#刪除字串前面或者字串後面的空格 "^\\s+|\\s+$"
temp <- names(drev_person)
temp1 <-
  c(
    "name",
    "ddegreen1",
    "ddegreeu1",
    "ddegreeg1",
    "ddegreen2",
    "ddegreeu2",
    "ddegreeg2",
    "mdegreen1",
    "mdegreeu1",
    "mdegreeg1",
    "mdegreen2",
    "mdegreeu2",
    "mdegreeg2",
    "bdegreen1",
    "bdegreeu1",
    "bdegreeg1",
    "bdegreen2",
    "bdegreeu2",
    "bdegreeg2",
    "adegreen1",
    "adegreeu1",
    "adegreeg1",
    "adegreen2",
    "adegreeu2",
    "adegreeg2",
    "source",
    "edu_name2"
  )
for (i in temp1) {
  temp <- temp[-which(temp == i)]
}
for (x in temp) {
  drev_person_1[[x]] <- trim(drev_person_1[[x]])
}

#職務名稱、服務單位 欄位名稱更名 方便分析
names(drev_person_1)[which(names(drev_person_1) == "admintitle")] <-
  "admintitle0"
names(drev_person_1)[which(names(drev_person_1) == "adminunit")] <-
  "adminunit0"

temp <-
  c(
    "admintitle0",
    "adminunit0",
    "admintitle1",
    "adminunit1",
    "admintitle2",
    "adminunit2",
    "admintitle3",
    "adminunit3"
  )
for (x in temp) {
  drev_person_1[[x]][grep("^祕書$", drev_person_1[[x]])] <- "秘書"
  drev_person_1[[x]][grep("^圖書管主任$", drev_person_1[[x]])] <- "圖書館主任"
}

#教員資料表無專任行政職欄位 調整成N 方便分析
temp <- c("admintitle0", "adminunit0")
for (x in temp) {
  drev_person_1[[x]][is.na(drev_person_1[[x]])] <- "N"
}

#國籍別、畢業學校國別
temp <-
  c(
    "nation",
    "ddegreen1",
    "ddegreen2",
    "mdegreen1",
    "mdegreen2",
    "bdegreen1",
    "bdegreen2",
    "adegreen1",
    "adegreen2"
  )
for (x in temp) {
  drev_person_1[[x]][grep("^本國籍$", drev_person_1[[x]])] <- "本國"
  drev_person_1[[x]][grep("^澳洲$", drev_person_1[[x]])] <- "澳大利亞"
}

#亂碼
drev_person_1[["mdegreeu1"]][grep("^國立台灣藝術?學$", drev_person_1[["mdegreeu1"]])] <-
  "國立台灣藝術大學"
drev_person_1[["mdegreeu1"]][grep("^私立中華科技?學$", drev_person_1[["mdegreeu1"]])] <-
  "私立中華科技大學"

drev_person_1[["mdegreeg1"]][grep("^?物科技系$", drev_person_1[["mdegreeg1"]])] <-
  "生物科技系"

drev_person_1[["bdegreeu1"]][grep("^國立台北科技?學$", drev_person_1[["bdegreeu1"]])] <-
  "國立台北科技大學"
drev_person_1[["bdegreeu1"]][grep("^?雄餐旅?學$", drev_person_1[["bdegreeu1"]])] <-
  "高雄餐旅大學"
drev_person_1[["bdegreeu1"]][grep("^銘傳?學$", drev_person_1[["bdegreeu1"]])] <-
  "銘傳大學"
drev_person_1[["bdegreeu1"]][grep("^德明財經科技?學$", drev_person_1[["bdegreeu1"]])] <-
  "德明財經科技大學"
drev_person_1[["bdegreeu1"]][grep("^國立台灣藝術?學$", drev_person_1[["bdegreeu1"]])] <-
  "國立台灣藝術大學"
drev_person_1[["bdegreeu1"]][grep("^吳鳳科技?學$", drev_person_1[["bdegreeu1"]])] <-
  "吳鳳科技大學"
drev_person_1[["bdegreeu1"]][grep("^弘光科技?學$", drev_person_1[["bdegreeu1"]])] <-
  "弘光科技大學"

drev_person_1[["bdegreeg1"]][grep("^?輛?程系$", drev_person_1[["bdegreeg1"]])] <-
  "車輛工程系"
drev_person_1[["bdegreeg1"]][grep("^?餐廚藝系$", drev_person_1[["bdegreeg1"]])] <-
  "西餐廚藝系"
drev_person_1[["bdegreeg1"]][grep("^化妝品應?管理系$", drev_person_1[["bdegreeg1"]])] <-
  "化妝品應用管理系"



#將gender由字串轉成數字
drev_person_1 <- drev_person_1 %>%
  transform(gender = as.numeric(gender))