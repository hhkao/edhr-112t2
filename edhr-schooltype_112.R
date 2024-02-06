# 統計處高級中等學校科別資料 -----------------------------------------------------------
filename <- "./112_base2_revise.xlsx"

# 讀取檔案
data_schtype <- read_excel(filename)

data_schtype <-
  c(
    "學校代碼",
    "學校名稱",
    "學程(等級)別",
    "學程(等級)名稱",
    "日夜別",
    "日夜別名稱",
    "群別代碼",
    "群別名稱",
    "科系代碼",
    "科系名稱",
    "班級數",
    "學生數"
  ) %>%
  data_schtype[, .]
# 改變變項名稱與形態
data_schtype <-
  plyr::rename(
    data_schtype,
    c(
      "學校代碼"       = "organization_id"
      ,
      "學校名稱"       = "edu_name"
      ,
      "學程(等級)別"   = "type_code"
      ,
      "學程(等級)名稱" = "type_name"
      ,
      "日夜別"         = "dn_code"
      ,
      "日夜別名稱"     = "dn_name"
      ,
      "群別代碼"       = "dep1_code"
      ,
      "群別名稱"       = "dep1_name"
      ,
      "科系代碼"       = "depcode"
      ,
      "科系名稱"       = "dep2_name"
      ,
      "班級數"         = "nclass"
      ,
      "學生數"         = "nstudent"
    )
  )
data_schtype$nclass[data_schtype$nclass == "-"]     <- NA
data_schtype$nstudent[data_schtype$nstudent == "-"] <- NA
data_schtype$nclass   <- as.numeric(data_schtype$nclass)
data_schtype$nstudent <- as.numeric(data_schtype$nstudent)

data_schtype$organization_id <-
  recode_factor(data_schtype$organization_id,
                "140222" = "140401"
                ,
                "400144" = "400419")
data_schtype$try1 <- NA
data_schtype$try1 <- 1
# 主管機關
data_schtype$authority <- NA
data_schtype$authority[substr(data_schtype$organization_id, 3, 3) == "0"] <-
  "國立"
data_schtype$authority[substr(data_schtype$organization_id, 3, 3) == "1"] <-
  "私立"
data_schtype$authority[substr(data_schtype$organization_id, 3, 3) == "3" |
                         substr(data_schtype$organization_id, 3, 3) == "4"] <-
  "縣市立"

data_schtype$authority[data_schtype$authority == "國立" &
                         (data_schtype$organization_id == "140401" |
                            data_schtype$organization_id == "400419")]                                     <-
  "技職司管轄國立"
data_schtype$authority[data_schtype$authority == "國立" &
                         (
                           data_schtype$organization_id == "110328" |
                             data_schtype$organization_id == "180301" |
                             data_schtype$organization_id == "060323"
                         )] <-  "國教署與科技部共管"


data_schtype$authority[data_schtype$authority == "縣市立" &
                         as.numeric(substr(data_schtype$organization_id, 1, 2)) == 1]  <-
  "新北市市立"
data_schtype$authority[data_schtype$authority ==   "私立" &
                         as.numeric(substr(data_schtype$organization_id, 1, 2)) == 1]  <-
  "新北市私立"
data_schtype$authority[data_schtype$authority == "縣市立" &
                         as.numeric(substr(data_schtype$organization_id, 1, 2)) == 2]  <-
  "宜蘭縣縣立"
data_schtype$authority[data_schtype$authority ==   "私立" &
                         as.numeric(substr(data_schtype$organization_id, 1, 2)) == 3]  <-
  "桃園市私立"
data_schtype$authority[data_schtype$authority == "縣市立" &
                         as.numeric(substr(data_schtype$organization_id, 1, 2)) == 3]  <-
  "桃園市市立"
data_schtype$authority[data_schtype$authority == "縣市立" &
                         as.numeric(substr(data_schtype$organization_id, 1, 2)) == 4]  <-
  "新竹縣縣立"
data_schtype$authority[data_schtype$authority == "縣市立" &
                         as.numeric(substr(data_schtype$organization_id, 1, 2)) == 5]  <-
  "苗栗縣縣立"
data_schtype$authority[data_schtype$authority == "縣市立" &
                         as.numeric(substr(data_schtype$organization_id, 1, 2)) == 6]  <-
  "臺中市市立"
data_schtype$authority[data_schtype$authority ==   "私立" &
                         as.numeric(substr(data_schtype$organization_id, 1, 2)) == 6]  <-
  "臺中市私立"
data_schtype$authority[data_schtype$authority == "縣市立" &
                         as.numeric(substr(data_schtype$organization_id, 1, 2)) == 7]  <-
  "彰化縣縣立"
data_schtype$authority[data_schtype$authority == "縣市立" &
                         as.numeric(substr(data_schtype$organization_id, 1, 2)) == 8]  <-
  "南投縣縣立"
data_schtype$authority[data_schtype$authority == "縣市立" &
                         as.numeric(substr(data_schtype$organization_id, 1, 2)) == 9]  <-
  "雲林縣縣立"
data_schtype$authority[data_schtype$authority == "縣市立" &
                         as.numeric(substr(data_schtype$organization_id, 1, 2)) == 10] <-
  "嘉義縣縣立"
data_schtype$authority[data_schtype$authority == "縣市立" &
                         as.numeric(substr(data_schtype$organization_id, 1, 2)) == 11] <-
  "臺南市市立"
data_schtype$authority[data_schtype$authority == "縣市立" &
                         as.numeric(substr(data_schtype$organization_id, 1, 2)) == 12] <-
  "高雄市市立"
data_schtype$authority[data_schtype$authority == "縣市立" &
                         as.numeric(substr(data_schtype$organization_id, 1, 2)) == 13] <-
  "屏東縣縣立"
data_schtype$authority[data_schtype$authority == "縣市立" &
                         as.numeric(substr(data_schtype$organization_id, 1, 2)) == 14] <-
  "臺東縣縣立"
data_schtype$authority[data_schtype$authority == "縣市立" &
                         as.numeric(substr(data_schtype$organization_id, 1, 2)) == 15] <-
  "花蓮縣縣立"
data_schtype$authority[data_schtype$authority == "縣市立" &
                         as.numeric(substr(data_schtype$organization_id, 1, 2)) == 16] <-
  "澎湖縣縣立"
data_schtype$authority[data_schtype$authority == "縣市立" &
                         as.numeric(substr(data_schtype$organization_id, 1, 2)) == 17] <-
  "基隆市市立"
data_schtype$authority[data_schtype$authority == "縣市立" &
                         as.numeric(substr(data_schtype$organization_id, 1, 2)) == 18] <-
  "新竹市市立"
data_schtype$authority[data_schtype$authority == "縣市立" &
                         as.numeric(substr(data_schtype$organization_id, 1, 2)) == 19] <-
  "臺中市市立"
data_schtype$authority[data_schtype$authority ==   "私立" &
                         as.numeric(substr(data_schtype$organization_id, 1, 2)) == 19] <-
  "臺中市私立"
data_schtype$authority[data_schtype$authority == "縣市立" &
                         as.numeric(substr(data_schtype$organization_id, 1, 2)) == 20] <-
  "嘉義市市立"
data_schtype$authority[data_schtype$authority == "縣市立" &
                         as.numeric(substr(data_schtype$organization_id, 1, 2)) == 21] <-
  "臺南市市立"

for (x in 30:42) {
  data_schtype$authority[data_schtype$authority == "縣市立" &
                           as.numeric(substr(data_schtype$organization_id, 1, 2)) == x]  <-
    "臺北市市立"
  data_schtype$authority[data_schtype$authority ==   "私立" &
                           as.numeric(substr(data_schtype$organization_id, 1, 2)) == x]  <-
    "臺北市私立"
}

for (x in 50:61) {
  data_schtype$authority[data_schtype$authority == "縣市立" &
                           as.numeric(substr(data_schtype$organization_id, 1, 2)) == x]  <-
    "高雄市市立"
  data_schtype$authority[data_schtype$authority ==   "私立" &
                           as.numeric(substr(data_schtype$organization_id, 1, 2)) == x]  <-
    "高雄市私立"
}
data_schtype$authority[data_schtype$authority == "縣市立" &
                         as.numeric(substr(data_schtype$organization_id, 1, 2)) == 64] <-
  "高雄市市立"
data_schtype$authority[data_schtype$authority == "縣市立" &
                         as.numeric(substr(data_schtype$organization_id, 1, 2)) == 66] <-
  "臺中市市立"
data_schtype$authority[data_schtype$authority == "縣市立" &
                         as.numeric(substr(data_schtype$organization_id, 1, 2)) == 67] <-
  "臺南市市立"
data_schtype$authority[data_schtype$authority == "縣市立" &
                         as.numeric(substr(data_schtype$organization_id, 1, 2)) == 71] <-
  "金門縣縣立"
data_schtype$authority[data_schtype$authority == "縣市立" &
                         as.numeric(substr(data_schtype$organization_id, 1, 2)) == 72] <-
  "連江縣縣立"
data_schtype$authority[data_schtype$authority == "國立"]                                                           <-
  "國教署管轄國立"
data_schtype$authority[data_schtype$authority == "私立"]                                                           <-
  "國教署管轄私立"


# type_code
data_schtype$type_code <- recode_factor(data_schtype$type_code,
                                        "A" = "H"
                                        ,
                                        "B" = "V"
                                        ,
                                        "O" = "M")
data_schtype$type_code <-
  factor(data_schtype$type_code,
         levels = c("C", "E", "F", "H", "V", "M", "J", "U"))
data_schtype$type_code[data_schtype$dn_code == "D" &
                         data_schtype$type_code == "U"] <- "E"
data_schtype$type_code[data_schtype$dn_code == "N" &
                         data_schtype$type_code == "U"] <- "F"

# 各校班級的開課單位
data_schtype_wide <- data_schtype %>%
  mutate(schtype = paste("type", type_code, sep = "")) %>%
  spread(key = schtype, value = nstudent)

data_schtype_wide <- data_schtype_wide %>%
  group_by(organization_id) %>%
  mutate(typeC = sum(typeC, na.rm = TRUE)) %>%
  group_by(organization_id) %>%
  mutate(typeH = sum(typeH, na.rm = TRUE)) %>%
  group_by(organization_id) %>%
  mutate(typeJ = sum(typeJ, na.rm = TRUE)) %>%
  group_by(organization_id) %>%
  mutate(typeV = sum(typeV, na.rm = TRUE)) %>%
  group_by(organization_id) %>%
  mutate(typeM = sum(typeM, na.rm = TRUE)) %>%
  group_by(organization_id) %>%
  mutate(typeF = sum(typeF, na.rm = TRUE)) %>%
  group_by(organization_id) %>%
  mutate(typeE = sum(typeE, na.rm = TRUE))

# 國際部與雙語部的名單會逐年變動。


temp <- data_schtype %>%
  filter(dep1_code == "11", dep2_name == "雙語部") %>%
  distinct(organization_id) %>%
  `[[`(1) %>%
  as.character()

data_schtype_wide$typeD <- 0
for (x in temp) {
  data_schtype_wide <- data_schtype_wide %>%
    mutate(typeD = if_else((organization_id == x |
                              typeD == 1), 1 , 0))
}

data_schtype_wide <- data_schtype_wide %>%
  mutate(
    typeH = if_else(as.numeric(typeH) > 1, 1, 0, missing = NULL),
    typeJ = if_else(as.numeric(typeJ) > 1, 1, 0, missing = NULL),
    typeV = if_else(as.numeric(typeV) > 1, 1, 0, missing = NULL),
    typeC = if_else(as.numeric(typeC) > 1, 1, 0, missing = NULL),
    typeM = if_else(as.numeric(typeM) > 1, 1, 0, missing = NULL),
    typeF = if_else(as.numeric(typeF) > 1, 1, 0, missing = NULL),
    typeE = if_else(as.numeric(typeE) > 1, 1, 0, missing = NULL),
    typeD = if_else(as.numeric(typeD) > 1, 1, 0, missing = 0)
  )