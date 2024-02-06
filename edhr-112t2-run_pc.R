#請搜尋[每次填報更改]來更改內容

rm(list=ls())

# 載入所需套件
#隱藏警告訊息
suppressWarnings({
  suppressPackageStartupMessages({
  library(DBI)
  library(odbc)
  library(magrittr)
  library(dplyr)
  library(readxl)
  library(stringr)
  library(openxlsx)
  library(tidyr)
  library(reshape2)
  })
})

time_now <- Sys.time()

#[每次填報更改]請輸入本次填報設定檔標題(字串需與標題完全相符，否則會找不到)
title <- "112學年度下學期高級中等學校教育人力資源資料庫（國立學校人事及教務）"

#[每次填報更改]請更改自己管區的學校代碼
dis <- c(
  "330301", 
  "380301", 
  "030305", 
  "030403", 
  "070301", 
  "070304", 
  "070307", 
  "070316", 
  "070319", 
  "070401", 
  "070402", 
  "070403", 
  "070405", 
  "070406", 
  "070408", 
  "070409", 
  "070410", 
  "070415", 
  "080302", 
  "080305", 
  "080307", 
  "080308", 
  "080401", 
  "080403", 
  "080404", 
  "080406", 
  "080410", 
  "170301", 
  "170302", 
  "170403", 
  "170404", 
  "140301", 
  "140302", 
  "140303", 
  "140404", 
  "140405", 
  "140408", 
  "720301", 
  "200302", 
  "200303", 
  "200401", 
  "200405", 
  "200406", 
  "200407"
)

checkfile_server <- "\\\\192.168.110.245\\Plan_edhr\\教育部高級中等學校教育人力資源資料庫建置第7期計畫(1120201_1130731)\\檢核語法檔\\R\\自動化資料檢核結果\\\\edhr-112t2-check_print-人事.xlsx" #[每次填報更改]請更改本次server匯出的檢核結果檔之路徑
check02_server <- readxl :: read_excel(checkfile_server)

#審核同意的名單 = check02_server subset自己管區學校的名單
list_agree <- check02_server %>% 
  select("organization_id") %>% 
  subset(organization_id %in% dis) %>% 
  mutate(agree = 1)

#載入server端產出的檢核結果excel檔
check02 <- check02_server %>%
  subset(organization_id %in% dis)

#[每次填報更改]以下個案處理請依自己實際需求修改
# 計畫端個案處理 -------------------------------------------------------------------

# #國立中央大學附屬中壢高中(030305)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：153人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：154；差異百分比0.6%" & check02$organization_id == "030305", "", check02$flag95)
#   #日本國立奈良教育大學（學院）	中等學校教育系 正確
# check02$spe6 <- if_else(check02$spe6 == "教員資料表之大學（學士）以上各教育階段學歷資料不完整或不正確：范清美（學士學位畢業科系（一）：中等學校教育系）" & check02$organization_id == "030305", "", check02$spe6)
# 
# #國立北科大附屬桃園農工(030403)
#   #資源班 服務單位正確
# check02$flag49 <- if_else(check02$flag49 != "" & check02$organization_id == "030403", "", check02$flag49)
#   #劉怡君 劉智暉 張晏瑜 林秀澄 詹淑媛 陳勝利 皆非本學期退休或因故離職人員
# check02$flag86 <- if_else(check02$flag86 != "" & check02$organization_id == "030403", "", check02$flag86)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：213人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：212；差異百分比-0.5%" & check02$organization_id == "030403", "", check02$flag95)
#   #同等學力
# check02$spe6 <- if_else(check02$spe6 == "職員(工)資料表之大學（學士）以上各教育階段學歷資料不完整或不正確：歐奕廷（副學士或專科畢業學校（一）：同等學歷）" & check02$organization_id == "030403", "", check02$spe6)
# 
# #市立龍潭高中(033302)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：117人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：124；差異百分比5.6%" & check02$organization_id == "033302", "", check02$flag95)
# 
# #市立桃園高中(033304)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：161人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：168；差異百分比4.2%" & check02$organization_id == "033304", "", check02$flag95)
#   #憲兵專科 正確
# check02$spe6 <- if_else(check02$spe6 == "職員(工)資料表之大學（學士）以上各教育階段學歷資料不完整或不正確：周芷恆（副學士或專科畢業科系（一）：憲兵專科）" & check02$organization_id == "033304", "", check02$spe6)
# 
# #市立武陵高中(033306)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：143人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：163；差異百分比12.3%" & check02$organization_id == "033306", "", check02$flag95)
# 
# #市立楊梅高中(033316)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：127人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：124；差異百分比-2.4%" & check02$organization_id == "033316", "", check02$flag95)
# 
# #市立陽明高中(033325)
#   #劉素妏 吳岱融 張育偉 楊美華 盧聖真 許芳菁 陳麗如 皆非本學期退休或因故離職人員
# check02$flag86 <- if_else(check02$flag86 != "" & check02$organization_id == "033325", "", check02$flag86)
#   #陳碧瑩 112年1月3日到職，112年7月1日離職
# check02$flag93 <- if_else(check02$flag93 == "離退教職員(工)資料表：陳碧瑩（查貴校上一學年所填資料，上述人員未在貴校教職員(工)資料中，請確認上述人員是否於111年10月1日-112年7月31日有退休或因故離職之情形，或是否屬於貴校教職員(工)，併請確認貴校教職員工名單是否完整正確。）" & check02$organization_id == "033325", "", check02$flag93)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：149人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：155；差異百分比3.9%" & check02$organization_id == "033325", "", check02$flag95)
# 
# #市立內壢高中(033327)
#   #王炎川 皆非本學期退休或因故離職人員
# check02$flag86 <- if_else(check02$flag86 != "" & check02$organization_id == "033327", "", check02$flag86)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：146人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：145；差異百分比-0.7%" & check02$organization_id == "033327", "", check02$flag95)
#   #國立政治大學 學校行政碩士在職專班
# check02$spe6 <- if_else(check02$spe6 == "教員資料表之大學（學士）以上各教育階段學歷資料不完整或不正確：王炎川（碩士學位畢業系所（一）：學校行政碩士在職專班）" & check02$organization_id == "033327", "", check02$spe6)
# 
# #市立中壢高商(033407)
#   #賴正倫 教官退休 正確
# check02$flag85 <- if_else(check02$flag85 == "教員資料表：賴正倫（該員年齡似低於最低法定退休年齡，敬請再協助確認）" & check02$organization_id == "033407", "", check02$flag85)
#   #劉素玲 周秀英 孫喬新 林均桓 林月霞 湯孟瑜 蕭永福 蘇鴻銘 鄭書季 陳柏臻 陳錦昌 皆非本學期退休或因故離職人員
# check02$flag86 <- if_else(check02$flag86 != "" & check02$organization_id == "033407", "", check02$flag86)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：157人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：153；差異百分比-2.6%" & check02$organization_id == "033407", "", check02$flag95)
# 
# #市立中壢家商(033408)
#   #陳昱希 16歲 正確
# check02$flag7 <- if_else(check02$flag7 == "職員(工)資料表：陳昱希（0951004）（請確認出生年月日是否正確）" & check02$organization_id == "033408", "", check02$flag7)
#   #陳昱希16歲，但學校工作總年資有0年（約16歲開始工作） 正確
# check02$flag39 <- if_else(check02$flag39 == "請確認該員之「本校到職日期」、「本校任職需扣除之年資」、「本校到職前學校服務總年資」，職員(工)資料表：陳昱希16歲，但學校工作總年資有0年（約16歲開始工作）" & check02$organization_id == "033408", "", check02$flag39)
#   #謝鈺釵 鄭旭助 皆非本學期退休或因故離職人員
# check02$flag86 <- if_else(check02$flag86 != "" & check02$organization_id == "033408", "", check02$flag86)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：77人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：82；差異百分比6.1%" & check02$organization_id == "033408", "", check02$flag95)
#   #國立中山大學 教育研究所教師在職進修教學及學校行政碩士學位班、國立政治大學	學校行政碩士在職專班 正確
# check02$spe6 <- if_else(check02$spe6 == "教員資料表之大學（學士）以上各教育階段學歷資料不完整或不正確：伍展儀（碩士學位畢業系所（一）：教育研究所教師在職進修教學及學校行政碩士學位班） 黃挹芬（碩士學位畢業系所（一）：學校行政碩士在職專班）" & check02$organization_id == "033408", "", check02$spe6)
# 
# #市立南崁高中(034306)
#   #呂靜葉 周禹辰 方竟曉 洪淑琪 黃思嘉 皆非本學期退休或因故離職人員
# check02$flag86 <- if_else(check02$flag86 != "" & check02$organization_id == "034306", "", check02$flag86)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：109人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：113；差異百分比3.5%" & check02$organization_id == "034306", "", check02$flag95)
#   #國立彰化師範大學	輔導與諮商學系學校輔導與諮商組、國立臺灣師範大學	教育學系學校行政班 正確
# check02$spe6 <- if_else(check02$spe6 == "教員資料表之大學（學士）以上各教育階段學歷資料不完整或不正確：何思穎（學士學位畢業科系（一）：輔導與諮商學系學校輔導與諮商組） 陳家祥（碩士學位畢業系所（一）：教育學系學校行政班）" & check02$organization_id == "034306", "", check02$spe6)
# 
# #市立大溪高中(034312)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：83人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：84；差異百分比1.2%" & check02$organization_id == "034312", "", check02$flag95)
#   #國立政治大學	學校行政碩士在職專班 正確
# check02$spe6 <- if_else(check02$spe6 == "教員資料表之大學（學士）以上各教育階段學歷資料不完整或不正確：蕭英全（碩士學位畢業系所（一）：學校行政碩士在職專班）" & check02$organization_id == "034312", "", check02$spe6)
# 
# #市立壽山高中(034314)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：144人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：148；差異百分比2.7%" & check02$organization_id == "034314", "", check02$flag95)
#   #黃麗娥 黄麗娥
# check02$flag98 <- if_else(check02$flag98 == "教員資料表：請確認該員基本資料：黃麗娥（姓名）" & check02$organization_id == "034314", "", check02$flag98)
#   #國立政治大學	學校行政碩士、明尼蘇達州立大學	教育政策與行政 正確
# check02$spe6 <- if_else(check02$spe6 == "教員資料表之大學（學士）以上各教育階段學歷資料不完整或不正確：呂雅惠（碩士學位畢業系所（一）：學校行政碩士） 連維志（博士學位畢業系所（一）：教育政策與行政）" & check02$organization_id == "034314", "", check02$spe6)
# 
# #市立平鎮高中(034319)
#   #何玉雲 劉得琪 周樂正 洪郁發 游正祥 許秋瑩 黃思蕾 皆非本學期退休或因故離職人員
# check02$flag86 <- if_else(check02$flag86 != "" & check02$organization_id == "034319", "", check02$flag86)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：107人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：124；差異百分比13.7%" & check02$organization_id == "034319", "", check02$flag95)
# 
# #市立觀音高中(034332)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：139人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：142；差異百分比2.1%" & check02$organization_id == "034332", "", check02$flag95)
# 
# #市立新屋高級中等學校(034335)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：129人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：119；差異百分比-8.4%" & check02$organization_id == "034335", "", check02$flag95)
# 
# #市立永豐高中(034347)
#   #伍慧美 林益儒 陳銘軍 黃郁文 皆非本學期退休或因故離職人員
# check02$flag86 <- if_else(check02$flag86 != "" & check02$organization_id == "034347", "", check02$flag86)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：191人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：194；差異百分比1.5%" & check02$organization_id == "034347", "", check02$flag95)
#   #政治作戰學校	美術專科 正確
# check02$spe6 <- if_else(check02$spe6 == "職員(工)資料表之大學（學士）以上各教育階段學歷資料不完整或不正確：陳慶明（副學士或專科畢業科系（一）：美術專科）" & check02$organization_id == "034347", "", check02$spe6)
# 
# #市立大園國際高中(034399)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：114人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：122；差異百分比6.6%" & check02$organization_id == "034399", "", check02$flag95)
# 
# #國立彰化女中(070301)
#   #唐國詩 張盈惠 朱美惠 皆非本學期退休或因故離職人員
# check02$flag86 <- if_else(check02$flag86 != "" & check02$organization_id == "070301", "", check02$flag86)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：108人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：111；差異百分比2.7%" & check02$organization_id == "070301", "", check02$flag95)
# 
# #國立員林高中(070304)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：119人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：119；差異百分比0.0%" & check02$organization_id == "070304", "", check02$flag95)
# 
# #國立彰化高中(070307)
#   #姜志忠 朱芳寅 柯鐙鐊 楊雅妃 白夕芬 鐘慧卿 顏麗眞 黎思岑 皆非本學期退休或因故離職人員
# check02$flag86 <- if_else(check02$flag86 != "" & check02$organization_id == "070307", "", check02$flag86)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：149人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：150；差異百分比0.7%" & check02$organization_id == "070307", "", check02$flag95)
# 
# #國立鹿港高中(070316)
#   #蔡奉育55歲，但學校工作總年資有39年（約16歲開始工作） 正確
# check02$flag39 <- if_else(check02$flag39 == "請確認該員之「本校到職日期」、「本校任職需扣除之年資」、「本校到職前學校服務總年資」，職員(工)資料表：蔡奉育55歲，但學校工作總年資有39年（約16歲開始工作）" & check02$organization_id == "070316", "", check02$flag39)
#   #施佳礽 王詩云 鄭綜聖 黃釋賢 皆非本學期退休或因故離職人員
# check02$flag86 <- if_else(check02$flag86 != "" & check02$organization_id == "070316", "", check02$flag86)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：116人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：114；差異百分比-1.8%" & check02$organization_id == "070316", "", check02$flag95)
# 
# #國立溪湖高中(070319)
#   #沒有設置學程主任
# check02$flag3 <- if_else(check02$flag3 == "請學校確認是否設置學程主任" & check02$organization_id == "070319", "", check02$flag3)
#   #吳雅鈴 巫正成 林曉倩 蔡淇茂 皆非本學期退休或因故離職人員
# check02$flag86 <- if_else(check02$flag86 != "" & check02$organization_id == "070319", "", check02$flag86)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：110人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：113；差異百分比2.7%" & check02$organization_id == "070319", "", check02$flag95)
# 
# #國立彰師附工(070401)
#   #人事室主管 暫缺
# check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：人事室主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "070401", "", check02$flag1)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：180人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：179；差異百分比-0.6%" & check02$organization_id == "070401", "", check02$flag95)
# 
# #國立永靖高工(070402)
#   #張雅媚 曾國揚 李鵬吉 林再呈 陳姜維 陳毓芬 皆非本學期退休或因故離職人員
# check02$flag86 <- if_else(check02$flag86 != "" & check02$organization_id == "070402", "", check02$flag86)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：109人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：106；差異百分比-2.8%" & check02$organization_id == "070402", "", check02$flag95)
#   #國立彰化師範大學	教育學院學校行政研究所 正確
# check02$spe6 <- if_else(check02$spe6 == "教員資料表之大學（學士）以上各教育階段學歷資料不完整或不正確：邱子耀（碩士學位畢業系所（一）：教育學院學校行政研究所）" & check02$organization_id == "070402", "", check02$spe6)
# 
# #國立二林工商(070403)
#   #人事室主管 暫缺
# check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：人事室主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "070403", "", check02$flag1)
#   #無設置學程主任
# check02$flag3 <- if_else(check02$flag3 == "請學校確認是否設置學程主任" & check02$organization_id == "070403", "", check02$flag3)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：136人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：130；差異百分比-4.6%" & check02$organization_id == "070403", "", check02$flag95)
# 
# #國立秀水高工(070405)
#   #劉溢桐 林錫輝 蘇錦洲 陳松洲 皆非本學期退休或因故離職人員
# check02$flag86 <- if_else(check02$flag86 != "" & check02$organization_id == "070405", "", check02$flag86)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：128人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：131；差異百分比2.3%" & check02$organization_id == "070405", "", check02$flag95)
# 
# #國立彰化高商(070406)
#   #吳幼婕 吳靜儀 呂鈞恩 張彥肜 曾冠陸 王金淑 簡伯倫 皆非本學期退休或因故離職人員
# check02$flag86 <- if_else(check02$flag86 != "" & check02$organization_id == "070406", "", check02$flag86)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：159人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：157；差異百分比-1.3%" & check02$organization_id == "070406", "", check02$flag95)
# 
# #國立員林農工(070408)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：124人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：133；差異百分比6.8%" & check02$organization_id == "070408", "", check02$flag95)
#   #同等學力
# check02$spe6 <- if_else(check02$spe6 == "職員(工)資料表之大學（學士）以上各教育階段學歷資料不完整或不正確：王如亮（副學士或專科畢業學校（一）：同等學力）" & check02$organization_id == "070408", "", check02$spe6)
# 
# #國立員林崇實高工(070409)
#   #王麗粧約15歲開始工作無誤
# check02$flag39 <- if_else(check02$flag39 == "請確認該員之「本校到職日期」、「本校任職需扣除之年資」、「本校到職前學校服務總年資」，職員(工)資料表：王麗粧56歲，但學校工作總年資有41年（約15歲開始工作）" & check02$organization_id == "070409", "", check02$flag39)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：99人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：103；差異百分比3.9%" & check02$organization_id == "070409", "", check02$flag95)
#   #國立高雄海事專科學校 正確
# check02$spe6 <- if_else(check02$spe6 == "職員(工)資料表之大學（學士）以上各教育階段學歷資料不完整或不正確：陳志偉（副學士或專科畢業學校（一）：國立高雄海事專科學校）" & check02$organization_id == "070409", "", check02$spe6)
# 
# #國立員林家商(070410)
#   #劉彩緞 柯詠純 陳柏頴 皆非本學期退休或因故離職人員
# check02$flag86 <- if_else(check02$flag86 != "" & check02$organization_id == "070410", "", check02$flag86)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：108人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：111；差異百分比2.7%" & check02$organization_id == "070410", "", check02$flag95)
# 
# #國立北斗家商(070415)
#   #楊世圳 許順傑 皆非本學期退休或因故離職人員
# check02$flag86 <- if_else(check02$flag86 != "" & check02$organization_id == "070415", "", check02$flag86)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：120人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：121；差異百分比0.8%" & check02$organization_id == "070415", "", check02$flag95)
# 
# #縣立彰化藝術高中(074308)
#   #主（會）計室主管 暫缺
# check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：主（會）計室主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "074308", "", check02$flag1)
#   #職員(工)資料表：賈慶貞57歲，但學校工作總年資有42年（約16歲開始工作）這是正確的
# check02$flag39 <- if_else(check02$flag39 == "請確認該員之「本校到職日期」、「本校任職需扣除之年資」、「本校到職前學校服務總年資」，職員(工)資料表：賈慶貞58歲，但學校工作總年資有42年（約16歲開始工作）" & check02$organization_id == "074308", "", check02$flag39)
#   #謝瓊慧 皆非本學期退休或因故離職人員
# check02$flag86 <- if_else(check02$flag86 != "" & check02$organization_id == "074308", "", check02$flag86)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：118人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：125；差異百分比5.6%" & check02$organization_id == "074308", "", check02$flag95)
# 
# #縣立二林高中(074313)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：118人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：113；差異百分比-4.4%" & check02$organization_id == "074313", "", check02$flag95)
# 
# #縣立和美高中(074323)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：140人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：140；差異百分比0.0%" & check02$organization_id == "074323", "", check02$flag95)
#   #國立彰化師範大學	教育研究所學校行政碩士班 正確
# check02$spe6 <- if_else(check02$spe6 == "教員資料表之大學（學士）以上各教育階段學歷資料不完整或不正確：常方舜（碩士學位畢業系所（一）：教育研究所學校行政碩士班）" & check02$organization_id == "074323", "", check02$spe6)
# 
# #縣立田中高中(074328)
#   #周經芝 李昆奮 林志青 鄧冠明 鄭如琬 皆非本學期退休或因故離職人員
# check02$flag86 <- if_else(check02$flag86 != "" & check02$organization_id == "074328", "", check02$flag86)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：101人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：97；差異百分比-4.1%" & check02$organization_id == "074328", "", check02$flag95)
# 
# #縣立成功高中(074339)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：108人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：106；差異百分比-1.9%" & check02$organization_id == "074339", "", check02$flag95)
# 
# #國立南投高中(080302)
#   #林昭璿 皆非本學期退休或因故離職人員
# check02$flag86 <- if_else(check02$flag86 != "" & check02$organization_id == "080302", "", check02$flag86)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：121人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：127；差異百分比4.7%" & check02$organization_id == "080302", "", check02$flag95)
#   #私立淡水工商管理專科學校 正確
# check02$spe6 <- if_else(check02$spe6 == "職員(工)資料表之大學（學士）以上各教育階段學歷資料不完整或不正確：祝祥峪（副學士或專科畢業學校（一）：私立淡水工商管理專科學校）" & check02$organization_id == "080302", "", check02$spe6)
# 
# #國立中興高中(080305)
#   #職員(工)資料表：李美紅58歲，但學校工作總年資有42年（約17歲開始工作） 這是正確的
# check02$flag39 <- if_else(check02$flag39 == "請確認該員之「本校到職日期」、「本校任職需扣除之年資」、「本校到職前學校服務總年資」，職員(工)資料表：李美紅59歲，但學校工作總年資有42年（約17歲開始工作）" & check02$organization_id == "080305", "", check02$flag39)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：100人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：99；差異百分比-1.0%" & check02$organization_id == "080305", "", check02$flag95)
#   #國立臺灣師範大學	教育研究所學校行政碩士班 正確
# check02$spe6 <- if_else(check02$spe6 == "教員資料表之大學（學士）以上各教育階段學歷資料不完整或不正確：簡茆汯（碩士學位畢業系所（一）：教育研究所學校行政碩士班）" & check02$organization_id == "080305", "", check02$spe6)
# 
# #國立竹山高中(080307)
#   #余念纁 張宏名 張繼祖 洪鈴惠 石仲哲 陳昌慶 陳漢銘 皆非本學期退休或因故離職人員
# check02$flag86 <- if_else(check02$flag86 != "" & check02$organization_id == "080307", "", check02$flag86)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：111人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：112；差異百分比0.9%" & check02$organization_id == "080307", "", check02$flag95)
#   #資訊管理科副學士(二專) 正確
# check02$spe6 <- if_else(check02$spe6 == "職員(工)資料表之大學（學士）以上各教育階段學歷資料不完整或不正確：簡尚華（副學士或專科畢業科系（一）：資訊管理科副學士(二專)）" & check02$organization_id == "080307", "", check02$spe6)
# 
# #國立暨大附中(080308)
#   #扣除年資不為零的人數偏高 屬實
# check02$flag64 <- if_else(check02$flag64 == "扣除年資不為零的人數似偏高，請再依欄位說明確認。" & check02$organization_id == "080308", "", check02$flag64)
#   #康靖坤 柯智馨 王雅瑩 皆非本學期退休或因故離職人員
# check02$flag86 <- if_else(check02$flag86 != "" & check02$organization_id == "080308", "", check02$flag86)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：95人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：92；差異百分比-3.3%" & check02$organization_id == "080308", "", check02$flag95)
# 
# #國立仁愛高農(080401)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：51人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：53；差異百分比3.8%" & check02$organization_id == "080401", "", check02$flag95)
# 
# #國立埔里高工(080403)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：103人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：104；差異百分比1.0%" & check02$organization_id == "080403", "", check02$flag95)
#   #歐陽正華 鍾佳諺 皆非本學期退休或因故離職人員
# check02$flag86 <- if_else(check02$flag86 != "" & check02$organization_id == "080403", "", check02$flag86)
# 
# #國立南投高商(080404)
#   #吳宛璇 吳玫杏 方慧玲 林玎璣 皆非本學期退休或因故離職人員
# check02$flag86 <- if_else(check02$flag86 != "" & check02$organization_id == "080404", "", check02$flag86)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：91人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：89；差異百分比-2.2%" & check02$organization_id == "080404", "", check02$flag95)
# 
# #國立草屯商工(080406)
#  #洪汾埴 胡瑋倫 黃莉棉 皆非本學期退休或因故離職人員
# check02$flag86 <- if_else(check02$flag86 != "" & check02$organization_id == "080406", "", check02$flag86)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：132人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：132；差異百分比0.0%" & check02$organization_id == "080406", "", check02$flag95)
#   #國立彰化師範大學 教育研究所學校行政碩士班，蕭思文（副學士或專科畢業學校（一）：國立臺中商業專科學校附設高商補校） 學校表示無誤
# check02$spe6 <- if_else(check02$spe6 == "教員資料表之大學（學士）以上各教育階段學歷資料不完整或不正確：鄭玄倫（碩士學位畢業系所（一）：教育研究所學校行政碩士班）； 職員(工)資料表之大學（學士）以上各教育階段學歷資料不完整或不正確：蕭思文（副學士或專科畢業學校（一）：國立臺中商業專科學校附設高商補校）" & check02$organization_id == "080406", "", check02$spe6)
# 
# #國立水里商工(080410)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：87人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：89；差異百分比2.2%" & check02$organization_id == "080410", "", check02$flag95)
# 
# #縣立旭光高中(084309)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：141人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：144；差異百分比2.1%" & check02$organization_id == "084309", "", check02$flag95)
#   #國立彰化師範大學	教育研究所學校行政碩士 正確
# check02$spe6 <- if_else(check02$spe6 == "教員資料表之大學（學士）以上各教育階段學歷資料不完整或不正確：劉宗明（碩士學位畢業系所（一）：教育研究所學校行政碩士）" & check02$organization_id == "084309", "", check02$spe6)
# 
# #私立協同高中(101304)
#   #兼任教師連續聘任不中斷無誤
# check02$flag80 <- if_else(check02$flag80 != "" & check02$organization_id == "101304", "", check02$flag80)
#   #林志修 林慶驊 郭慶生 謝嘉惠 皆非本學期退休或因故離職人員
# check02$flag86 <- if_else(check02$flag86 != "" & check02$organization_id == "101304", "", check02$flag86)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：99人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：97；差異百分比-2.1%" & check02$organization_id == "101304", "", check02$flag95)
# 
# #私立萬能工商(101406)
#  #林怡安（服務單位：實習處海外招生處 職務名稱：組長）實習處海外招生處 正確，主管為組長 正確
# check02$flag62 <- if_else(check02$flag62 != "" & check02$organization_id == "101406", "", check02$flag62)
#  #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：49人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：40；差異百分比-22.5%" & check02$organization_id == "101406", "", check02$flag95)
# 
# # #私立光禾華德福實驗學校(121302)
# #   #確實沒有圖書館主管，有教務處主管 學務處主管 總務處主管 輔導室主管 人事室主管 主（會）計室主管，但各處室分別僅一人管理，職稱不是主管
# # check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：教務處主管 學務處主管 總務處主管 輔導室主管 圖書館主管 人事室主管 主（會）計室主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "121302", "", check02$flag1)
# #   #陳淑市 皆非本學期退休或因故離職人員
# # check02$flag86 <- if_else(check02$flag86 == "姓名：陳淑市（經比對貴校上一學年所填資料，上述人員並未出現於本學期的教員資料表或職員(工)資料表，請確認渠等是否於111學年度第一學期（111年8月1日-112年1月31日）退休或因故離職等，若於該學期退休或因故離職等，應於離退教職員(工)資料表填寫資料。如非於該學期退休或因故離職，或已介聘、調至他校，請來電告知。）" & check02$organization_id == "121302", "", check02$flag86)
# #   #本項目不需請學校修正
# # check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：7人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：12；差異百分比41.7%" & check02$organization_id == "121302", "", check02$flag95)
# 
# #財團法人新光高中(121306)
#  #確實沒有總務處主管 圖書館主管 實習處主管
# check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：總務處主管 圖書館主管 實習處主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "121306", "", check02$flag1)
#  #沒有設置科主任或學程主任
# check02$flag2 <- if_else(check02$flag2 == "請學校確認是否設置科主任或學程主任" & check02$organization_id == "121306", "", check02$flag2)
#   #顏正偉 上期資料沒填
# check02$flag93 <- if_else(check02$flag93 == "離退教職員(工)資料表：顏正偉（查貴校上一學年所填資料，上述人員未在貴校教職員(工)資料中，請確認上述人員是否於112年2月1日-112年7月31日有退休或因故離職之情形，或是否屬於貴校教職員(工)，併請確認貴校教職員工名單是否完整正確。）" & check02$organization_id == "121306", "", check02$flag93)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：12人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：5；差異百分比-140.0%" & check02$organization_id == "121306", "", check02$flag95)
# 
# #財團法人普門中學(121307)
#  #確實沒有實習處主管 人事室主管(不為該校教職員 且不支薪)
# check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：實習處主管 人事室主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "121307", "", check02$flag1)
#  #放過學校 職員(工)資料表專任人員人數偏低
# check02$flag18 <- if_else(check02$flag18 == "職員(工)資料表專任人員人數偏低，請再協助確認實際聘任情況，或請確認是否填報完整職員(工)名單資料。" & check02$organization_id == "121307", "", check02$flag18)
#  #兼任教師連續聘任不中斷無誤
# check02$flag80 <- if_else(check02$flag80 != "" & check02$organization_id == "121307", "", check02$flag80)
#  #莊惠雯 皆非本學期退休或因故離職人員
# check02$flag86 <- if_else(check02$flag86 != "" & check02$organization_id == "121307", "", check02$flag86)
#  #邱淑貞 上期基準日後離職
# check02$flag93 <- if_else(check02$flag93 == "離退教職員(工)資料表：邱淑貞（查貴校上一學年所填資料，上述人員未在貴校教職員(工)資料中，請確認上述人員是否於112年2月1日-112年7月31日有退休或因故離職之情形，或是否屬於貴校教職員(工)，併請確認貴校教職員工名單是否完整正確。）" & check02$organization_id == "121307", "", check02$flag93)
#  #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：46人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：51；差異百分比9.8%" & check02$organization_id == "121307", "", check02$flag95)
#   #總務處主任確實由代理教師兼任。約聘僱可算全職，可暫不請學校修正
# check02$flag96 <- if_else(check02$flag96 == "教員資料表：石德文（代理 總務處主任）（校內一級主管（主任）原則由專任教職員擔（兼）任，請協助再確認上述教職員是否擔（兼）任校內一級主管（主任），或協助再確認上述教職員之聘任類別）" & check02$organization_id == "121307", "", check02$flag96)
#  
# # #私立正義高中(121318)
#  #確實沒有圖書館主管，實際上於教務處會有人去管理圖書館
# check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：圖書館主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "121318", "", check02$flag1)
#  #放過學校 職員(工)資料表專任人員人數偏低
# check02$flag18 <- if_else(check02$flag18 == "職員(工)資料表專任人員人數偏低，請再協助確認實際聘任情況，或請確認是否填報完整職員(工)名單資料。" & check02$organization_id == "121318", "", check02$flag18)
#  #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：23人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：27；差異百分比14.8%" & check02$organization_id == "121318", "", check02$flag95)
#  #約聘僱可算全職，可暫不請學校修正
# check02$flag96 <- if_else(check02$flag96 == "職員(工)資料表：呂時傑（約聘僱 總務處主任）（約聘僱 學務處主任）（校內一級主管（主任）原則由專任教職員擔（兼）任，請協助再確認上述教職員是否擔（兼）任校內一級主管（主任），或協助再確認上述教職員之聘任類別）" & check02$organization_id == "121318", "", check02$flag96)
# 
# #私立義大國際高中(121320)
#  #確實沒有設置圖書館主管
# check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：圖書館主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "121320", "", check02$flag1)
# #   #學務處設有主任 副主任
# # check02$flag18 <- if_else(check02$flag18 == "學務處主管（主任）人數超過一位，請再協助確認實際聘任情況。" & check02$organization_id == "121320", "", check02$flag18)
#  #人事室、校長室的主管為組長，職稱無誤
# check02$flag62 <- if_else(check02$flag62 != "" & check02$organization_id == "121320", "", check02$flag62)
#  #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：33人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：37；差異百分比10.8%" & check02$organization_id == "121320", "", check02$flag95)
#  #柯瓊琪（碩士學位畢業學校（一）：VNIVERSITAS PENNSYLVANIENSIS）正確
# check02$spe6 <- if_else(check02$spe6 == "教員資料表之大學（學士）以上各教育階段學歷資料不完整或不正確：柯瓊琪（碩士學位畢業學校（一）：VNIVERSITASPENNSYLVANIENSIS）" & check02$organization_id == "121320", "", check02$spe6)
# 
# #私立中山工商(121405)
#  #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：316人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：302；差異百分比-4.6%" & check02$organization_id == "121405", "", check02$flag95)
# 
# #私立旗美商工(121410)
#  #確實沒有設置學務處主管 輔導室主管
# check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：學務處主管 輔導室主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "121410", "", check02$flag1)
#  #沒有設置科主任或學程主任
# check02$flag2 <- if_else(check02$flag2 == "請學校確認是否設置科主任或學程主任" & check02$organization_id == "121410", "", check02$flag2)
#  #放過學校 職員(工)資料表專任人員人數偏低、教員資料表專任教學人員人數偏低、教員資料表主聘單位各類別人數分布異常、一年以上與任教領域相關之業界實務工作經驗人數偏多。
# check02$flag18 <- if_else(check02$flag18 == "職員(工)資料表專任人員人數偏低，請再協助確認實際聘任情況，或請確認是否填報完整職員(工)名單資料。；教員資料表專任教學人員人數偏低，請再協助確認實際聘任情況，或請確認是否填報完整教員名單資料。教員資料表主聘單位各類別人數分布異常，請再協助確認實際聘任情況。一年以上與任教領域相關之業界實務工作經驗人數偏多（請再協助確認，『是否具備一年以上與任教領域相關之業界實務工作經驗』填寫『Y』之教員，是否確依欄位說明具備此經驗）" & check02$organization_id == "121410", "", check02$flag18)
#  #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：3人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：3；差異百分比0.0%" & check02$organization_id == "121410", "", check02$flag95)
#  #約聘僱可算全職，可暫不請學校修正
# check02$flag96 <- if_else(check02$flag96 == "職員(工)資料表：尹素月（約聘僱 會計室會計主任）（校內一級主管（主任）原則由專任教職員擔（兼）任，請協助再確認上述教職員是否擔（兼）任校內一級主管（主任），或協助再確認上述教職員之聘任類別）" & check02$organization_id == "121410", "", check02$flag96)
#  #林春貴 國立屏東大學	教育行政
# check02$spe6 <- if_else(check02$spe6 == "教員資料表之大學（學士）以上各教育階段學歷資料不完整或不正確：林春貴（博士學位畢業系所（一）：教育行政）" & check02$organization_id == "121410", "", check02$spe6)
# 
# #私立高英工商(121413)
#  #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：61人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：60；差異百分比-1.7%" & check02$organization_id == "121413", "", check02$flag95)
# 
# #私立華德工家(121415)
#  #有填 教務處圖書室主任
# check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：圖書館主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "121415", "", check02$flag1)
# #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：22人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：24；差異百分比8.3%" & check02$organization_id == "121415", "", check02$flag95)
# 
# #私立高苑工商(121417)
#  #確實沒有總務處主管 圖書館主管
# check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：總務處主管 圖書館主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "121417", "", check02$flag1)
#  #沒有設置科主任或學程主任
# check02$flag3 <- if_else(check02$flag3 == "請學校確認是否設置學程主任" & check02$organization_id == "121417", "", check02$flag3)
#  #放過學校 教員資料表專任教學人員人數偏低，請再協助確認實際聘任情況，或請確認是否填報完整教員名單資料。一年以上與任教領域相關之業界實務工作經驗人數偏多。
# check02$flag18 <- if_else(check02$flag18 == "教員資料表專任教學人員人數偏低，請再協助確認實際聘任情況，或請確認是否填報完整教員名單資料。一年以上與任教領域相關之業界實務工作經驗人數偏多（請再協助確認，『是否具備一年以上與任教領域相關之業界實務工作經驗』填寫『Y』之教員，是否確依欄位說明具備此經驗）" & check02$organization_id == "121417", "", check02$flag18)
#  #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：53人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：56；差異百分比5.4%" & check02$organization_id == "121417", "", check02$flag95)
#   #科主任不算一級主管 陳建宏（代理(連) 電機科主任） 陳慕寧（代理(連) 餐飲管理科主任）
# check02$flag96 <- if_else(check02$flag96 != "" & check02$organization_id == "121417", "", check02$flag96)
# 
# #國立臺東大學附屬體育高中(140301)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：45人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：66；差異百分比31.8%" & check02$organization_id == "140301", "", check02$flag95)
# 
# #國立臺東女中(140302)
#   #蘇靜芬 鄭華鈞 皆非本學期退休或因故離職人員
# check02$flag86 <- if_else(check02$flag86 != "" & check02$organization_id == "140302", "", check02$flag86)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：68人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：73；差異百分比6.8%" & check02$organization_id == "140302", "", check02$flag95)
#   #美國	楊百翰大學	大學學習系 正確 學校已求證
# check02$spe6 <- if_else(check02$spe6 == "職員(工)資料表之大學（學士）以上各教育階段學歷資料不完整或不正確：田玫琪（學士學位畢業科系（一）：大學學習系）" & check02$organization_id == "140302", "", check02$spe6)
# 
# #國立臺東高中(140303)
#   #主（會）計室主管暫缺
# check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：主（會）計室主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "140303", "", check02$flag1)
#  #廖于舜 曾振華 林利嘉 林查理 白國梅 許肇文 諶惠貞 陳建昇 陳蕙英 馬鉅強 皆非本學期退休或因故離職人員
# check02$flag86 <- if_else(check02$flag86 != "" & check02$organization_id == "140303", "", check02$flag86)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：81人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：83；差異百分比2.4%" & check02$organization_id == "140303", "", check02$flag95)
# 
# #國立關山工商(140404)
#  #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：56人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：56；差異百分比0.0%" & check02$organization_id == "140404", "", check02$flag95)
# 
# #國立臺東高商(140405)
#   #人事室主管暫缺
# check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：人事室主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "140405", "", check02$flag1)
#   #詹玟璇 鍾佩珊 皆非本學期退休或因故離職人員
# check02$flag86 <- if_else(check02$flag86 != "" & check02$organization_id == "140405", "", check02$flag86)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：95人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：95；差異百分比0.0%" & check02$organization_id == "140405", "", check02$flag95)
#   #吳欣怡代理教師，實習處會計事務科科主任
# check02$flag96 <- if_else(check02$flag96 == "教員資料表：吳欣怡（代理(連) 實習處會計事務科科主任）（校內一級主管（主任）原則由專任教職員擔（兼）任，請協助再確認上述教職員是否擔（兼）任校內一級主管（主任），或協助再確認上述教職員之聘任類別）" & check02$organization_id == "140405", "", check02$flag96)
# 
# #國立成功商水(140408)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：35人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：35；差異百分比0.0%" & check02$organization_id == "140408", "", check02$flag95)
# 
# #臺東縣均一高中(141301)
#  #確實沒有設置圖書館主管
# check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：圖書館主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "141301", "", check02$flag1)
#  #放過學校 教員資料表專任教學人員人數偏低
# check02$flag18 <- if_else(check02$flag18 == "教員資料表專任教學人員人數偏低，請再協助確認實際聘任情況，或請確認是否填報完整教員名單資料。" & check02$organization_id == "141301", "", check02$flag18)
#  #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：25人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：30；差異百分比16.7%" & check02$organization_id == "141301", "", check02$flag95)
#   #白珮璇代理教師，確實兼任國際部主任、學務處主任、並代理校長職務
# check02$flag96 <- if_else(check02$flag96 == "教員資料表：白珮璇（代理(連) 國際部主任）（代理(連) 學務處主任）（校內一級主管（主任）原則由專任教職員擔（兼）任，請協助再確認上述教職員是否擔（兼）任校內一級主管（主任），或協助再確認上述教職員之聘任類別）" & check02$organization_id == "141301", "", check02$flag96)
# 
# #私立育仁高中(141307)
#  #確實沒有設置輔導室主管(網站有 已去電確認確實沒有) 圖書館主管 實習處主管
# check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：輔導室主管 圖書館主管 實習處主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "141307", "", check02$flag1)
#  #沒有設置科主任
# check02$flag2 <- if_else(check02$flag2 == "請學校確認是否設置科主任或學程主任" & check02$organization_id == "141307", "", check02$flag2)
#  #放過學校 教員資料表專任人員人數偏低
# check02$flag18 <- if_else(check02$flag18 == "教員資料表專任教學人員人數偏低，請再協助確認實際聘任情況，或請確認是否填報完整教員名單資料。" & check02$organization_id == "141307", "", check02$flag18)
#  #兼任教師連續聘任不中斷無誤
# check02$flag80 <- if_else(check02$flag80 != "" & check02$organization_id == "141307", "", check02$flag80)
#  #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：13人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：11；差異百分比-18.2%" & check02$organization_id == "141307", "", check02$flag95)
# 
# #私立公東高工(141406)
# #確實沒有設置圖書館主管 僅有組長(屬於教務處)
# check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：圖書館主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "141406", "", check02$flag1)
# #輔導室主任、實習輔導處主任
# check02$flag18 <- if_else(check02$flag18 == "輔導室主管（主任）人數超過一位，請再協助確認實際聘任情況。" & check02$organization_id == "141406", "", check02$flag18)
#   #職務名稱：修女 職稱無誤
# check02$flag62 <- if_else(check02$flag62 != "" & check02$organization_id == "141406", "", check02$flag62)
#   #林承均 賴玉柱 專任教師退休後以代理方式回聘
# check02$flag83 <- if_else(check02$flag83 != "" & check02$organization_id == "141406", "", check02$flag83)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：39人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：41；差異百分比4.9%" & check02$organization_id == "141406", "", check02$flag95)
# 
# #縣立蘭嶼高中(144322)
#   #確實沒有圖書館主管
# check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：圖書館主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "144322", "", check02$flag1)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：31人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：33；差異百分比6.1%" & check02$organization_id == "144322", "", check02$flag95)
# 
# #國立基隆女中(170301)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：110人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：108；差異百分比-1.9%" & check02$organization_id == "170301", "", check02$flag95)
# 
# #國立基隆高中(170302)
#   #毛淑娟 江木發 鄭義翰 皆非本學期退休或因故離職人員
# check02$flag86 <- if_else(check02$flag86 != "" & check02$organization_id == "170302", "", check02$flag86)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：79人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：84；差異百分比6.0%" & check02$organization_id == "170302", "", check02$flag95)
# 
# #國立基隆商工(170404)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：148人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：148；差異百分比0.0%" & check02$organization_id == "170404", "", check02$flag95)
#   #國立政治大學	教育學院學校行政碩士在職專班 正確
# check02$spe6 <- if_else(check02$spe6 == "教員資料表之大學（學士）以上各教育階段學歷資料不完整或不正確：江巨材（碩士學位畢業系所（一）：教育學院學校行政碩士在職專班）" & check02$organization_id == "170404", "", check02$spe6)
# 
# #市立中山高中(173304)
#   #主（會）計室主管 暫缺
# check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：主（會）計室主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "173304", "", check02$flag1)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：78人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：76；差異百分比-2.6%" & check02$organization_id == "173304", "", check02$flag95)
# 
# #市立安樂高中(173306)
#   #安胎病假及延長病假 正確
# check02$flag16 <- if_else(check02$flag16 == "教員資料表需修改請假類別：陳丹玲（安胎病假及延長病假）（請確認或修正請假類別，或是否屬於請假，若以上人員未有請假情事，請填寫半型大寫『N』）" & check02$organization_id == "173306", "", check02$flag16)
#   #方保社 石清杉 陳美靜 皆非本學期退休或因故離職人員
# check02$flag86 <- if_else(check02$flag86 != "" & check02$organization_id == "173306", "", check02$flag86)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：86人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：93；差異百分比7.5%" & check02$organization_id == "173306", "", check02$flag95)
# 
# #市立暖暖高中(173307)
#   #闕淑煜60歲，但學校工作總年資有45年（約15歲開始工作） 正確
# check02$flag39 <- if_else(check02$flag39 == "請確認該員之「本校到職日期」、「本校任職需扣除之年資」、「本校到職前學校服務總年資」，職員(工)資料表：闕淑煜60歲，但學校工作總年資有45年（約15歲開始工作）" & check02$organization_id == "173307", "", check02$flag39)
#   #朱雅雯 皆非本學期退休或因故離職人員
# check02$flag86 <- if_else(check02$flag86 != "" & check02$organization_id == "173307", "", check02$flag86)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：60人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：61；差異百分比1.6%" & check02$organization_id == "173307", "", check02$flag95)
# 
# #市立八斗高中(173314)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：64人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：64；差異百分比0.0%" & check02$organization_id == "173314", "", check02$flag95)
# 
# #國立海洋大學附屬基隆海事(170403)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：124人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：123；差異百分比-0.8%" & check02$organization_id == "170403", "", check02$flag95)
# 
# #私立光復高中(181305)
#  #放過學校 職員(工)資料表專任人員人數偏低
# check02$flag18 <- if_else(check02$flag18 == "職員(工)資料表專任人員人數偏低，請再協助確認實際聘任情況，或請確認是否填報完整職員(工)名單資料。" & check02$organization_id == "181305", "", check02$flag18)
#   #陳麗娟48歲，但學校工作總年資有31年（約17歲開始工作） 無誤
# check02$flag39 <- if_else(check02$flag39 == "請確認該員之「本校到職日期」、「本校任職需扣除之年資」、「本校到職前學校服務總年資」，職員(工)資料表：陳麗娟48歲，但學校工作總年資有31年（約17歲開始工作）" & check02$organization_id == "181305", "", check02$flag39)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：179人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：176；差異百分比-1.7%" & check02$organization_id == "181305", "", check02$flag95)
#  #謝馥霞（碩士學位畢業學校（一）：NEW ENGLAND CONSERVATORY OF MUSIC）正確
# check02$spe6 <- if_else(check02$spe6 == "教員資料表之大學（學士）以上各教育階段學歷資料不完整或不正確：謝馥霞（碩士學位畢業學校（一）：NEWENGLANDCONSERVATORYOFMUSIC）" & check02$organization_id == "181305", "", check02$spe6)
# 
# #私立曙光女中(181306)
#  #確實沒有設置圖書館主管 實習處主管
# check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：圖書館主管 實習處主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "181306", "", check02$flag1)
#  #沒有設置科主任
# check02$flag2 <- if_else(check02$flag2 == "請學校確認是否設置科主任或學程主任" & check02$organization_id == "181306", "", check02$flag2)
#  #高中部教務主任、國中部教務主任
# check02$flag18 <- if_else(check02$flag18 == "教務處主管（主任）人數超過一位，請再協助確認實際聘任情況。" & check02$organization_id == "181306", "", check02$flag18)
#  #邱婉渟 皆非本學期退休或因故離職人員
# check02$flag86 <- if_else(check02$flag86 != "" & check02$organization_id == "181306", "", check02$flag86)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：98人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：100；差異百分比2.0%" & check02$organization_id == "181306", "", check02$flag95)
#  #魯和鳳，國立政治大學 教育學院學校行政
# check02$spe6 <- if_else(check02$spe6 == "教員資料表之大學（學士）以上各教育階段學歷資料不完整或不正確：魯和鳳（碩士學位畢業系所（一）：教育學院學校行政）" & check02$organization_id == "181306", "", check02$spe6)
# 
# #私立磐石高中(181307)
#  #確實沒有設置圖書館主管
# check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：圖書館主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "181307", "", check02$flag1)
#  #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：102人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：102；差異百分比0.0%" & check02$organization_id == "181307", "", check02$flag95)
# 
# #私立世界高中(181308)
#  #確實沒有輔導室主管 實習處主管(有實習處(實輔處)) 圖書館主任(教務處有人會管理，實際上沒有相關職稱)
# check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：輔導室主管 圖書館主管 實習處主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "181308", "", check02$flag1)
#  #放過學校 教員資料表專任人員人數偏低
# check02$flag18 <- if_else(check02$flag18 == "教員資料表專任教學人員人數偏低，請再協助確認實際聘任情況，或請確認是否填報完整教員名單資料。" & check02$organization_id == "181308", "", check02$flag18)
#  #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：19人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：19；差異百分比0.0%" & check02$organization_id == "181308", "", check02$flag95)
#   #國籍別正確
# check02$flag98 <- if_else(check02$flag98 == "教員資料表：請確認該員基本資料：金鈱昊（國籍別：韓國、南韓）" & check02$organization_id == "181308", "", check02$flag98)
# 
# # #私立興華高中(201304)
#  #確實沒有設置主（會）計室主管
# check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：主（會）計室主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "201304", "", check02$flag1)
#  #放過學校 職員(工)資料表專任人員人數偏低
# check02$flag18 <- if_else(check02$flag18 == "職員(工)資料表專任人員人數偏低，請再協助確認實際聘任情況，或請確認是否填報完整職員(工)名單資料。" & check02$organization_id == "201304", "", check02$flag18)
#  #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：46人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：44；差異百分比-4.5%" & check02$organization_id == "201304", "", check02$flag95)
#  #科主任不屬於一級主管、約聘僱算全職 給通過
# check02$flag96 <- if_else(check02$flag96 == "職員(工)資料表：江政達（約聘僱 總務處主任）（校內一級主管（主任）原則由專任教職員擔（兼）任，請協助再確認上述教職員是否擔（兼）任校內一級主管（主任），或協助再確認上述教職員之聘任類別）" & check02$organization_id == "201304", "", check02$flag96)
# 
# #私立仁義高中(201309)
#  #確實沒有設置人事室主管
# check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：人事室主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "201309", "", check02$flag1)
#  #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：0人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：2；差異百分比100.0%" & check02$organization_id == "201309", "", check02$flag95)
# 
# #私立嘉華高中(201310)
#  #確實沒有圖書館主管
# check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：圖書館主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "201310", "", check02$flag1)
#  #放過學校 職員(工)資料表專任人員人數偏低、教員資料表專任教學人員人數偏低
# check02$flag18 <- if_else(check02$flag18 == "職員(工)資料表專任人員人數偏低，請再協助確認實際聘任情況，或請確認是否填報完整職員(工)名單資料。" & check02$organization_id == "201310", "", check02$flag18)
#  #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：40人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：42；差異百分比4.8%" & check02$organization_id == "201310", "", check02$flag95)
#   #代理可算全職，可暫不請學校修正
# check02$flag96 <- if_else(check02$flag96 == "教員資料表：高禎雅（代理 輔導室主任）（校內一級主管（主任）原則由專任教職員擔（兼）任，請協助再確認上述教職員是否擔（兼）任校內一級主管（主任），或協助再確認上述教職員之聘任類別）" & check02$organization_id == "201310", "", check02$flag96)
# 
# #私立輔仁高中(201312)
#  #確實沒有圖書館主管 實習處主管
# check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：圖書館主管 實習處主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "201312", "", check02$flag1)
#  #放過學校 職員(工)資料表專任人員人數偏低
# check02$flag18 <- if_else(check02$flag18 == "職員(工)資料表專任人員人數偏低，請再協助確認實際聘任情況，或請確認是否填報完整職員(工)名單資料。" & check02$organization_id == "201312", "", check02$flag18)
#  #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：65人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：62；差異百分比-4.8%" & check02$organization_id == "201312", "", check02$flag95)
#  #約聘僱可算全職，可暫不請學校修正
# check02$flag96 <- if_else(check02$flag96 == "職員(工)資料表：陳旺（約聘僱 校長室主任）（校內一級主管（主任）原則由專任教職員擔（兼）任，請協助再確認上述教職員是否擔（兼）任校內一級主管（主任），或協助再確認上述教職員之聘任類別）" & check02$organization_id == "201312", "", check02$flag96)
# 
# # #私立宏仁女中(201313)
# #   #確實沒有輔導室主管 圖書館主管 主（會）計室主管
# # check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：輔導室主管 圖書館主管 主（會）計室主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "201313", "", check02$flag1)
# #   #本項目不需請學校修正
# # check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：18人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：24；差異百分比25.0%" & check02$organization_id == "201313", "", check02$flag95)
# 
# #私立立仁高中(201314)
#  #確實沒有設置圖書館主管 人事室主管
# check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：圖書館主管 人事室主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "201314", "", check02$flag1)
#  #鐘點教師連續聘任不中斷無誤
# check02$flag80 <- if_else(check02$flag80 != "" & check02$organization_id == "201314", "", check02$flag80)
#  #吳欣頤 在上學年填報後到職且在112/6離職
# check02$flag93 <- if_else(check02$flag93 == "離退教職員(工)資料表：吳欣頤（查貴校上一學年所填資料，上述人員未在貴校教職員(工)資料中，請確認上述人員是否於112年2月1日-112年7月31日有退休或因故離職之情形，或是否屬於貴校教職員(工)，併請確認貴校教職員工名單是否完整正確。）" & check02$organization_id == "201314", "", check02$flag93)
#  #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：10人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：8；差異百分比-25.0%" & check02$organization_id == "201314", "", check02$flag95)
# 
# #私立東吳工家(201408)
#  #確實沒有圖書館主管 僅設組長
# check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：圖書館主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "201408", "", check02$flag1)
#  #職稱及服務單位無誤 實習處即測即評及發證中心確實有兩位組長
#     # 吳文賓（兼任行政職服務單位(二)：技藝學程中心(台南區)） 
#     # 張博良（兼任行政職服務單位(二)：技藝學程中心(嘉義縣)） 
#     # 陳明堂（兼任行政職服務單位(二)：技藝學程中心(嘉義市)；兼任行政職服務單位(一)：實習處即測即評及發證中心 兼任行政職職稱(一)：組長；兼任行政職服務單位(二)：技藝學程中心(嘉義市) 兼任行政職職稱(二)：組長） 
#     # 陳育修（兼任行政職職稱(一)：處長；兼任行政職服務單位(一)：僑生事務處；兼任行政職職稱(二)：僑務副校長）
#     # 張瓊惠（兼任行政職服務單位(一)：技藝學程中心 兼任行政職職稱(一)：組長） 
#     # 林正凰（兼任行政職服務單位(一)：圖書室 兼任行政職職稱(一)：組長） 
#     # 翁韻茹（兼任行政職服務單位(一)：實習處即測即評及發證中心 兼任行政職職稱(一)：組長） 
#     # 陳明堂（兼任行政職服務單位(二)：技藝學程中心(嘉義市)；兼任行政職服務單位(一)：實習處即測即評及發證中心 兼任行政職職稱(一)：組長；兼任行政職服務單位(二)：技藝學程中心(嘉義市) 兼任行政職職稱(二)：組長）
# check02$flag62 <- if_else(check02$flag62 != "" & check02$organization_id == "201408", "", check02$flag62)
# #放過學校 教員資料表專任教學人員人數偏低 職員(工)資料表專任人員人數偏低
# check02$flag18 <- if_else(check02$flag18 == "教員資料表專任教學人員人數偏低，請再協助確認實際聘任情況，或請確認是否填報完整教員名單資料。" & check02$organization_id == "201408", "", check02$flag18)
#   #吳芳瑩（管樂） 王景濡（管樂） 東吳工家有管樂隊
# check02$flag45 <- if_else(check02$flag45 != "" & check02$organization_id == "201408", "", check02$flag45)
#  #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：94人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：88；差異百分比-6.8%" & check02$organization_id == "201408", "", check02$flag95)
#   #徐毓瑩（博士學位畢業系所（一）：大學院人間文化研究科；碩士學位畢業系所（一）：大學院言語文化研究科） 梶原宏之KAJIHARAHIROYUKI（碩士學位畢業學校（一）：國立筑波大学） 正確
# check02$spe6 <- if_else(check02$spe6 != "" & check02$organization_id == "201408", "", check02$spe6)
# 
# #臺北市育達高中(311401)
#  #確實沒有實習處主管
# check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：實習處主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "311401", "", check02$flag1)
#  #放過學校 職員(工)資料表專任人員人數偏低
# check02$flag18 <- if_else(check02$flag18 == "職員(工)資料表專任人員人數偏低，請再協助確認實際聘任情況，或請確認是否填報完整職員(工)名單資料。" & check02$organization_id == "311401", "", check02$flag18)
#  #葉千綺約14歲開始工作無誤
# check02$flag39 <- if_else(check02$flag39 == "請確認該員之「本校到職日期」、「本校任職需扣除之年資」、「本校到職前學校服務總年資」，職員(工)資料表：葉千綺58歲，但學校工作總年資有44年（約14歲開始工作）" & check02$organization_id == "311401", "", check02$flag39)
#   #吳櫻卿（兼任行政職服務單位(一)：中小學部）  學校表示確實有"中小學部教育服務中心"此單位
# check02$flag62 <- if_else(check02$flag62 != "" & check02$organization_id == "311401", "", check02$flag62)
#  #FOSS KRISTIN ELIZABETH 劉一竹 楊美齡 游智雯 盧玫吟 莊美惠 蘇涵瑜 許寶媛 闓安榮 陳美娟 皆非本學期退休或因故離職人員
# check02$flag86 <- if_else(check02$flag86 != "" & check02$organization_id == "311401", "", check02$flag86)
#  #張芸榛 112年2月1日退休 所以沒出現在上學期的資料
# check02$flag93 <- if_else(check02$flag93 == "離退教職員(工)資料表：張芸榛（查貴校上一學年所填資料，上述人員未在貴校教職員(工)資料中，請確認上述人員是否於112年2月1日-112年7月31日有退休或因故離職之情形，或是否屬於貴校教職員(工)，併請確認貴校教職員工名單是否完整正確。）" & check02$organization_id == "311401", "", check02$flag93)
#  #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：81人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：74；差異百分比-9.5%" & check02$organization_id == "311401", "", check02$flag95)
#  #約聘僱可算全職，可暫不請學校修正 楊睿淑（約聘僱 會計室約聘會計主任） 洪毓俊（約聘僱 公關事務中心約聘校務主任兼公關事務主任）
# check02$flag96 <- if_else(check02$flag96 != "" & check02$organization_id == "311401", "", check02$flag96)
#   #中國海事專科學校 正確
# check02$spe6 <- if_else(check02$spe6 == "職員(工)資料表之大學（學士）以上各教育階段學歷資料不完整或不正確：邵　偉（副學士或專科畢業學校（一）：中國海事專科學校） 陳震星（副學士或專科畢業學校（一）：中國海事專科學校）" & check02$organization_id == "311401", "", check02$spe6)
# 
# #市立西松高中(313301)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：111人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：109；差異百分比-1.8%" & check02$organization_id == "313301", "", check02$flag95)
# 
# #市立中崙高中(313302)
#   #吳惠倩 賴宜君 邱肇純 鍾淑秋 陳信奇 皆非本學期退休或因故離職人員
# check02$flag86 <- if_else(check02$flag86 != "" & check02$organization_id == "313302", "", check02$flag86)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：138人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：143；差異百分比3.5%" & check02$organization_id == "313302", "", check02$flag95)
#   #國立彰化師範大學	教育研究所學校行政碩士班 正確
# check02$spe6 <- if_else(check02$spe6 == "教員資料表之大學（學士）以上各教育階段學歷資料不完整或不正確：黃衣婕（碩士學位畢業系所（一）：教育研究所學校行政碩士班）" & check02$organization_id == "313302", "", check02$spe6)
# 
# #臺北市私立協和祐德高級中學(321399)
#  #確實沒有設置圖書館主管 實習處主管
# check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：圖書館主管 實習處主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "321399", "", check02$flag1)
#  #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：23人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：21；差異百分比-9.5%" & check02$organization_id == "321399", "", check02$flag95)
#   #國立政治大學	學校行政研究所 正確
# check02$spe6 <- if_else(check02$spe6 == "教員資料表之大學（學士）以上各教育階段學歷資料不完整或不正確：邢文斐（碩士學位畢業系所（一）：學校行政研究所）" & check02$organization_id == "321399", "", check02$spe6)
# 
# #市立松山高中(323301)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：147人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：152；差異百分比3.3%" & check02$organization_id == "323301", "", check02$flag95)
# #林昇茂 國立臺灣師範大學	教育學系(學校行政班)、陳宣融 美國密蘇里大學	教育、學校和諮商心理學系 正確
# check02$spe6 <- if_else(check02$spe6 != "" & check02$organization_id == "323301", "", check02$spe6)
# 
# #市立永春高中(323302)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：119人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：119；差異百分比0.0%" & check02$organization_id == "323302", "", check02$flag95)
#   #國立政治大學	學校行政碩士在職專班 正確
# check02$spe6 <- if_else(check02$spe6 == "教員資料表之大學（學士）以上各教育階段學歷資料不完整或不正確：浦憶娟（碩士學位畢業系所（一）：學校行政碩士在職專班）" & check02$organization_id == "323302", "", check02$spe6)
# 
# #市立松山家商(323401)
#  #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：161人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：163；差異百分比1.2%" & check02$organization_id == "323401", "", check02$flag95)
# 
# #國立師大附中(330301)
#   #羅鴻城（0250815）出生年月日無誤
# check02$flag7 <- if_else(check02$flag7 == "教員資料表：羅鴻城（0250815）（請確認出生年月日是否正確）" & check02$organization_id == "330301", "", check02$flag7)
#   #教員資料表：羅鴻城87歲，但學校工作總年資有0年（約87歲開始工作）這是正確的
# check02$flag39 <- if_else(check02$flag39 == "請確認該員之「本校到職日期」、「本校任職需扣除之年資」、「本校到職前學校服務總年資」，教員資料表：羅鴻城87歲，但學校工作總年資有0年（約87歲開始工作）" & check02$organization_id == "330301", "", check02$flag39)
#   #周佳瑩 皆非本學期退休或因故離職人員
# check02$flag86 <- if_else(check02$flag86 != "" & check02$organization_id == "330301", "", check02$flag86)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：247人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：258；差異百分比4.3%" & check02$organization_id == "330301", "", check02$flag95)
#   #國立陽明大學	生命科學系暨基因體科學研究所 正確(陽明大學的畢業證書確實是這樣寫 授予學士學位)
#     #德國	國立福萊堡、德國	國立特洛辛根 正確
# check02$spe6 <- if_else(check02$spe6 != "" & check02$organization_id == "330301", "", check02$spe6)
# 
# #私立延平中學(331301)
#  #侯淑敏 吳志雄 皆在上期基準日之前就離職
# check02$flag93 <- if_else(check02$flag93 != "" & check02$organization_id == "331301", "", check02$flag93)
#  #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：133人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：134；差異百分比0.7%" & check02$organization_id == "331301", "", check02$flag95)
#  
# #市立松山工農(323402)
#   #蕭芳玲 蘇俊旗 皆非本學期退休或因故離職人員
# check02$flag86 <- if_else(check02$flag86 != "" & check02$organization_id == "323402", "", check02$flag86)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：188人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：196；差異百分比4.1%" & check02$organization_id == "323402", "", check02$flag95)
# 
# #私立金甌女中(331302)
#  #確實沒有設置圖書館主管 實習處主管 皆只有組長
# check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：圖書館主管 實習處主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "331302", "", check02$flag1)
#  #兼任教師連續聘任不中斷無誤
# check02$flag80 <- if_else(check02$flag80 != "" & check02$organization_id == "331302", "", check02$flag80)
#   #劉怡秀 林慧穎 熊經中 羅運瑛 皆非本學期退休或因故離職人員
# check02$flag86 <- if_else(check02$flag86 != "" & check02$organization_id == "331302", "", check02$flag86)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：61人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：56；差異百分比-8.9%" & check02$organization_id == "331302", "", check02$flag95)
# 
# #私立復興實驗高中(331304)
#  #確實沒有設置輔導室主管
# check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：輔導室主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "331304", "", check02$flag1)
#  #兼任教師連續聘任不中斷無誤
# check02$flag80 <- if_else(check02$flag80 != "" & check02$organization_id == "331304", "", check02$flag80)
#  #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：79人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：90；差異百分比12.2%" & check02$organization_id == "331304", "", check02$flag95)
#    #代理、約聘僱可算全職，可暫不請學校修正 劉雅文（代理(連) 藝文中心主任主任） 朱玉齡（代理(連) 人事室主任） 蔡玲玲（代理(連) 學務處主任）
# check02$flag96 <- if_else(check02$flag96 != "" & check02$organization_id == "331304", "", check02$flag96)
#   #教育學院學校行政在職碩士專班 正確
# check02$spe6 <- if_else(check02$spe6 == "教員資料表之大學（學士）以上各教育階段學歷資料不完整或不正確：蔡玲玲（碩士學位畢業系所（一）：教育學院學校行政在職碩士專班）" & check02$organization_id == "331304", "", check02$spe6)
#  
# #私立東方工商(331402)
#  #確實沒有設置教務處主管 輔導室主管 圖書館主管 人事室主管，圖書館目前是由教師去管 學生人數太少 沒有圖書館主管的編制或職稱
# check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：教務處主管 輔導室主管 圖書館主管 人事室主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "331402", "", check02$flag1)
#  #呂桂芳（0360220）出生年月日無誤
# check02$flag7 <- if_else(check02$flag7 == "職員(工)資料表：呂桂芳（0360220）（請確認出生年月日是否正確）" & check02$organization_id == "331402", "", check02$flag7)
#  #人事室的主管為組長，職稱無誤
# check02$flag62 <- if_else(check02$flag62 != "" & check02$organization_id == "331402", "", check02$flag62)
#  #郭美怡皆非本學期退休或因故離職人員
# check02$flag86 <- if_else(check02$flag86 != "" & check02$organization_id == "331402", "", check02$flag86)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：8人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：7；差異百分比-14.3%" & check02$organization_id == "331402", "", check02$flag95)
#  #李崇懿，美國	加州大學洛杉磯分校	教育行政
# check02$spe6 <- if_else(check02$spe6 == "教員資料表之大學（學士）以上各教育階段學歷資料不完整或不正確：李崇懿（碩士學位畢業系所（一）：教育行政）" & check02$organization_id == "331402", "", check02$spe6)
# 
# #私立喬治工商(331403)
#  #輔導室主管 暫缺，確實沒有設置圖書館主管
# check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：輔導室主管 圖書館主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "331403", "", check02$flag1)
#  #沒有設置科主任
# check02$flag2 <- if_else(check02$flag2 == "請學校確認是否設置科主任或學程主任" & check02$organization_id == "331403", "", check02$flag2)
#  #放過學校 教員資料表專任教學人員人數偏低
# check02$flag18 <- if_else(check02$flag18 == "教員資料表專任教學人員人數偏低，請再協助確認實際聘任情況，或請確認是否填報完整教員名單資料。" & check02$organization_id == "331403", "", check02$flag18)
#  #兼任教師連續聘任不中斷無誤
# check02$flag80 <- if_else(check02$flag80 != "" & check02$organization_id == "331403", "", check02$flag80)
#  #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：19人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：21；差異百分比9.5%" & check02$organization_id == "331403", "", check02$flag95)
#   #代理可算全職，可暫不請學校修正 俞永嘉（代理 教務處主任） 張月珠（代理 實習處主任）（代理 人事室1主任）（代理 總務處主任）
# check02$flag96 <- if_else(check02$flag96 != "" & check02$organization_id == "331403", "", check02$flag96)
#   #賴姵文（副學士或專科畢業學校（一）：光武工商(專科學校)）	畢業證書確實寫 光武工商，也為專科學制，故請學校在校名後面註記專科學制
# check02$spe6 <- if_else(check02$spe6 == "職員(工)資料表之大學（學士）以上各教育階段學歷資料不完整或不正確：賴姵文（副學士或專科畢業學校（一）：光武工商(專科學校)）" & check02$organization_id == "331403", "", check02$spe6)
# 
# #私立開平餐飲(331404)
#  #確實沒有設置總務處主管 人事室主管
# check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：總務處主管 人事室主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "331404", "", check02$flag1)
#  #該員為技術教師 故最高學歷不為大專以上給過
# check02$flag89 <- if_else(check02$flag89 == "教員資料表：周家銜（請再協助確認渠等人員畢業學歷）" & check02$organization_id == "331404", "", check02$flag89)
#  #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：31人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：51；差異百分比39.2%" & check02$organization_id == "331404", "", check02$flag95)
#   #馬嘉延（本校到職前學校服務總年資：0711） 民國102年到職 過去年資為0711 正確
# check02$flag100 <- if_else(check02$flag100 == "馬嘉延（本校到職前學校服務總年資：0711）（校長『本校到職前學校服務總年資』似偏少，請確認校長以『校長身分』就任之日期，此日期請填在『本校到職日期』；校長在就任日期前，在本校及他校擔任教師與主任等全職工作之年資，請填在『本校到職前學校服務總年資』。）" & check02$organization_id == "331404", "", check02$flag100)
#   #吳侑諭（碩士學位畢業系所（一）：COLLEGE OF LAW） 正確
# check02$spe6 <- if_else(check02$spe6 == "職員(工)資料表之大學（學士）以上各教育階段學歷資料不完整或不正確：吳侑諭（碩士學位畢業系所（一）：COLLEGEOFLAW）" & check02$organization_id == "331404", "", check02$spe6)
# 
# #市立和平高中(333301)
#   #職員(工)資料表：潘麗卿55歲，但學校工作總年資有38年（約17歲開始工作）這是正確的
# check02$flag39 <- if_else(check02$flag39 == "請確認該員之「本校到職日期」、「本校任職需扣除之年資」、「本校到職前學校服務總年資」，職員(工)資料表：潘麗卿55歲，但學校工作總年資有38年（約17歲開始工作）" & check02$organization_id == "333301", "", check02$flag39)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：149人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：154；差異百分比3.2%" & check02$organization_id == "333301", "", check02$flag95)
# 
# #市立芳和實驗中學(333304)
#   #學校編制特別 課程發展中心主任 學生事務中心主任 行政管理中心主任 學生輔導中心主任 實驗新創中心主任 外展探索中心主任 學習資源中心主任 學習資源中心圖書設備組長
# check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：教務處主管 總務處主管 圖書館主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "333304", "", check02$flag1)
#   #學校設有東區特教中心，故特教班專職教師人數偏多
# check02$flag18 <- if_else(check02$flag18 == "特教班專職教師人數偏多，請再協助確認實際聘任情況，並依欄位說明修正資料。" & check02$organization_id == "333304", "", check02$flag18)
#   #余怡青 吳菁容 紀芷勛 黃紋嫀 非本學期退休或因故離職人員
# check02$flag86 <- if_else(check02$flag86 != "" & check02$organization_id == "333304", "", check02$flag86)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：93人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：99；差異百分比6.1%" & check02$organization_id == "333304", "", check02$flag95)
# 
# #市立大安高工(333401)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：275人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：282；差異百分比2.5%" & check02$organization_id == "333401", "", check02$flag95)
# 
# # #私立大同高中(341302)
#  #確實沒有設置圖書館主管 實習處主管
# check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：圖書館主管 實習處主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "341302", "", check02$flag1)
#  #沒有設置科主任或學程主任
# check02$flag2 <- if_else(check02$flag2 == "請學校確認是否設置科主任或學程主任" & check02$organization_id == "341302", "", check02$flag2)
#   #胡景山 黃萬福 非本學期退休或因故離職人員
# check02$flag86 <- if_else(check02$flag86 != "" & check02$organization_id == "341302", "", check02$flag86)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：35人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：35；差異百分比0.0%" & check02$organization_id == "341302", "", check02$flag95)
#  
# #私立稻江護家(341402)
#  #圖書館主管為組長
# check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：圖書館主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "341402", "", check02$flag1)
#  #輔導室主任 實習輔導處主任
# check02$flag18 <- if_else(check02$flag18 == "輔導室主管（主任）人數超過一位，請再協助確認實際聘任情況。" & check02$organization_id == "341402", "", check02$flag18)
#  #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：58人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：59；差異百分比1.7%" & check02$organization_id == "341402", "", check02$flag95)
#  #世界新聞專科學校	報業行政 正確
# check02$spe6 <- if_else(check02$spe6 == "教員資料表之大學（學士）以上各教育階段學歷資料不完整或不正確：黃智柔（副學士或專科畢業科系（一）：報業行政）" & check02$organization_id == "341402", "", check02$spe6)
# 
# #市立中山女中(343301)
#   #國立政治大學	學校行政碩士在職專班 正確
# check02$spe6 <- if_else(check02$spe6 == "教員資料表之大學（學士）以上各教育階段學歷資料不完整或不正確：張云棻（碩士學位畢業系所（一）：學校行政碩士在職專班）" & check02$organization_id == "343301", "", check02$spe6)
#  #王怡心 李淑雲 魏豐閔 非本學期退休或因故離職人員
# check02$flag86 <- if_else(check02$flag86 != "" & check02$organization_id == "343301", "", check02$flag86)
#  #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：164人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：168；差異百分比2.4%" & check02$organization_id == "343301", "", check02$flag95)
# 
# #市立大同高中(343302)
#   #鐘點教師	聘任科別：本土語 這次可給通過
# check02$flag39 <- if_else(check02$flag39 == "請確認該員之「本校到職日期」、「本校任職需扣除之年資」、「本校到職前學校服務總年資」，教員資料表：張炳森85歲，但學校工作總年資有0年（約85歲開始工作）" & check02$organization_id == "343302", "", check02$flag39)
#   #倪達俊 江毅中 蕭玉琴 非本學期退休或因故離職人員
# check02$flag86 <- if_else(check02$flag86 != "" & check02$organization_id == "343302", "", check02$flag86)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：178人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：182；差異百分比2.2%" & check02$organization_id == "343302", "", check02$flag95)
# 
# #市立大直高中(343303)
#   #凃韋伯 戴碧蕙 李淑媛 林沛君 湯佩瑜 蔡俊雄 趙宜萍 非本學期退休或因故離職人員
# check02$flag86 <- if_else(check02$flag86 != "" & check02$organization_id == "343303", "", check02$flag86)
#  #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：149人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：159；差異百分比6.3%" & check02$organization_id == "343303", "", check02$flag95)
# 
# #私立強恕中學(351301)
#  #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：18人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：13；差異百分比-38.5%" & check02$organization_id == "351301", "", check02$flag95)
# 
# #臺北市開南高中(351402)
#  #確實沒有設置輔導室主管 圖書館主管
# check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：輔導室主管 圖書館主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "351402", "", check02$flag1)
#   #黃舲（職務名稱：約雇職員；兼任行政職職稱(一)：約雇職員），職稱無誤
# check02$flag62 <- if_else(check02$flag62 != "" & check02$organization_id == "351402", "", check02$flag62)
#  #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：39人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：42；差異百分比7.1%" & check02$organization_id == "351402", "", check02$flag95)
#  #約聘僱可算全職，可暫不請學校修正
# check02$flag96 <- if_else(check02$flag96 == "職員(工)資料表：房佳樺（約聘僱 會計室會計主任）（校內一級主管（主任）原則由專任教職員擔（兼）任，請協助再確認上述教職員是否擔（兼）任校內一級主管（主任），或協助再確認上述教職員之聘任類別）" & check02$organization_id == "351402", "", check02$flag96)
#  #國立中興大學	教師專業發展研究所、國立政治大學	學校行政碩士在職專班
# check02$spe6 <- if_else(check02$spe6 != "" & check02$organization_id == "351402", "", check02$spe6)
# 
# #私立南華高中進修學校(351B09)
#  #確實沒有設置圖書館主管 人事室主管
# check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：圖書館主管 人事室主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "351B09", "", check02$flag1)
#  #進修學校，主聘單位全部都填"高中部進修部"
# check02$flag18 <- if_else(check02$flag18 == "職員(工)資料表主聘單位各類別人數分布異常，請再協助確認實際聘任情況。；教員資料表主聘單位各類別人數分布異常，請再協助確認實際聘任情況。" & check02$organization_id == "351B09", "", check02$flag18)
#  #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：17人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：18；差異百分比5.6%" & check02$organization_id == "351B09", "", check02$flag95)
#   #莊惠安（本校到職前學校服務總年資：0600） 民國92年到職 過去年資為6年 正確
# check02$flag100 <- if_else(check02$flag100 == "莊惠安（本校到職前學校服務總年資：0600）（校長『本校到職前學校服務總年資』似偏少，請確認校長以『校長身分』就任之日期，此日期請填在『本校到職日期』；校長在就任日期前，在本校及他校擔任教師與主任等全職工作之年資，請填在『本校到職前學校服務總年資』。）" & check02$organization_id == "351B09", "", check02$flag100)
# 
# #市立建國中學(353301)
#   #曾奕勛 林秋彣 非本學期退休或因故離職人員
# check02$flag86 <- if_else(check02$flag86 != "" & check02$organization_id == "353301", "", check02$flag86)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：210人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：216；差異百分比2.8%" & check02$organization_id == "353301", "", check02$flag95)
# 
# #市立成功中學(353302)
#  #林玲蓉（學士學位畢業科系（一）：THE DEPARTMENT OF ENGLISH LITERATURE，COLLEGE OF HUMANITIES AND SCIENCES） 陳怡文（學士學位畢業科系（一）：外國語系日語專業）
# check02$spe6 <- if_else(check02$spe6 == "教員資料表之大學（學士）以上各教育階段學歷資料不完整或不正確：林玲蓉（學士學位畢業科系（一）：THEDEPARTMENTOFENGLISHLITERATURE，COLLEGEOFHUMANITIESANDSCIENCES） 陳怡文（學士學位畢業科系（一）：外國語系日語專業）" & check02$organization_id == "353302", "", check02$spe6)
#  #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：162人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：167；差異百分比3.0%" & check02$organization_id == "353302", "", check02$flag95)
# 
# #市立北一女中(353303)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：180人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：184；差異百分比2.2%" & check02$organization_id == "353303", "", check02$flag95)
# 
# #臺北市靜修高中(361301)
#  #確實沒有設置實習處主管 人事室主管
# check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：實習處主管 人事室主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "361301", "", check02$flag1)
#  #沒有設置科主任或學程主任
# check02$flag2 <- if_else(check02$flag2 == "請學校確認是否設置科主任或學程主任" & check02$organization_id == "361301", "", check02$flag2)
#  #約聘僱可算全職，可暫不請學校修正
# check02$flag90 <- if_else(check02$flag90 == "姓名：邱雅萍（約聘僱）（人事資料顯示該教師兼任行政職務）（校內行政職務原則由專任教師兼任，請協助再確認上述教師是否兼任行政職，或協助再確認上述教師之聘任類別）" & check02$organization_id == "361301", "", check02$flag90)
#  #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：78人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：81；差異百分比3.7%" & check02$organization_id == "361301", "", check02$flag95)
#   #國立臺灣師範大學	教育學院教育學系學校行政班 正確
# check02$spe6 <- if_else(check02$spe6 == "教員資料表之大學（學士）以上各教育階段學歷資料不完整或不正確：陳蕾如（碩士學位畢業系所（一）：教育學院教育學系學校行政班）" & check02$organization_id == "361301", "", check02$spe6)
#  
# #私立稻江高商(361401)
#  #確實沒有設置圖書館主管 主（會）計室主管
# check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：圖書館主管 主（會）計室主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "361401", "", check02$flag1)
#  #兼任教師連續聘任不中斷無誤
# check02$flag80 <- if_else(check02$flag80 != "" & check02$organization_id == "361401", "", check02$flag80)
#  #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：43人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：34；差異百分比-26.5%" & check02$organization_id == "361401", "", check02$flag95)
#  #約聘僱可算全職，可暫不請學校修正
# check02$flag96 <- if_else(check02$flag96 == "職員(工)資料表：陳怡秀（約聘僱 實習處實習主任）（校內一級主管（主任）原則由專任教職員擔（兼）任，請協助再確認上述教職員是否擔（兼）任校內一級主管（主任），或協助再確認上述教職員之聘任類別）" & check02$organization_id == "361401", "", check02$flag96)
# 
# #私立志仁中學進修學校(361B09)
#  #圖書館主任編制在總務處下
# check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：圖書館主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "361B09", "", check02$flag1)
#  #放過學校 教員資料表專任教學人員人數偏低
# check02$flag18 <- if_else(check02$flag18 == "教員資料表專任教學人員人數偏低，請再協助確認實際聘任情況，或請確認是否填報完整教員名單資料。" & check02$organization_id == "361B09", "", check02$flag18)
#  #兼任教師連續聘任不中斷無誤
# check02$flag80 <- if_else(check02$flag80 != "" & check02$organization_id == "361B09", "", check02$flag80)
#  #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：16人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：13；差異百分比-23.1%" & check02$organization_id == "361B09", "", check02$flag95)
#   #臺灣師範大學	健康促進與衛生教育學系學校衛生組 正確
# check02$spe6 <- if_else(check02$spe6 == "教員資料表之大學（學士）以上各教育階段學歷資料不完整或不正確：陳玉櫻（碩士學位畢業系所（一）：健康促進與衛生教育學系學校衛生組）" & check02$organization_id == "361B09", "", check02$spe6)
# 
# #市立明倫高中(363301)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：112人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：117；差異百分比4.3%" & check02$organization_id == "363301", "", check02$flag95)
#   #國立政治大學	學校行政碩士在職專班 正確
# check02$spe6 <- if_else(check02$spe6 == "教員資料表之大學（學士）以上各教育階段學歷資料不完整或不正確：蔡旻錡（碩士學位畢業系所（一）：學校行政碩士在職專班）" & check02$organization_id == "363301", "", check02$spe6)
# 
# #市立成淵高中(363302)
#   #人事室主管暫缺 10月才到職
# check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：人事室主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "363302", "", check02$flag1)
#   #沈碧麗16歲開始工作無誤
# check02$flag39 <- if_else(check02$flag39 == "請確認該員之「本校到職日期」、「本校任職需扣除之年資」、「本校到職前學校服務總年資」，職員(工)資料表：沈碧麗62歲，但學校工作總年資有46年（約16歲開始工作）" & check02$organization_id == "363302", "", check02$flag39)
#   #本項目不需請學校修正 學校補了 應該是統計處人數錯
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：165人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：102；差異百分比-61.8%" & check02$organization_id == "363302", "", check02$flag95)
#   #國立政治大學	學校行政碩士 正確
# check02$spe6 <- if_else(check02$spe6 == "教員資料表之大學（學士）以上各教育階段學歷資料不完整或不正確：蔡宗湶（碩士學位畢業系所（一）：學校行政碩士）" & check02$organization_id == "363302", "", check02$spe6)
# 
# #市立華江高中(373301)
#    #張志康 林尚節 陳春梅 皆非本學期退休或因故離職人員
# check02$flag86 <- if_else(check02$flag86 != "" & check02$organization_id == "373301", "", check02$flag86)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：118人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：122；差異百分比3.3%" & check02$organization_id == "373301", "", check02$flag95)
# 
# #市立大理高中(373302)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：98人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：115；差異百分比14.8%" & check02$organization_id == "373302", "", check02$flag95)
# 
# #國立政大附中(380301)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：73人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：87；差異百分比16.1%" & check02$organization_id == "380301", "", check02$flag95)
#   #副學士或專科畢業學校（一）：空大專校(二專) 正確
# check02$spe6 <- if_else(check02$spe6 == "教員資料表之大學（學士）以上各教育階段學歷資料不完整或不正確：黎彩惠（副學士或專科畢業學校（一）：空大專校(二專)）" & check02$organization_id == "380301", "", check02$spe6)
# 
# #私立東山高中(381301)
#  #蔡佩???（兼任行政職服務單位(一)：教務處音樂中心 兼任行政職職稱(一)：組長），職稱無誤
# check02$flag62 <- if_else(check02$flag62 != "" & check02$organization_id == "381301", "", check02$flag62)
#  #柯琳娜 皆非本學期退休或因故離職人員
# check02$flag86 <- if_else(check02$flag86 != "" & check02$organization_id == "381301", "", check02$flag86)
#  #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：162人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：174；差異百分比6.9%" & check02$organization_id == "381301", "", check02$flag95)
# 
# #私立滬江高中(381302)
#  #確實沒有設置輔導室主管 圖書館主管 實習處主管 人事室主管 主（會）計室
# check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：輔導室主管 圖書館主管 實習處主管 人事室主管 主（會）計室主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "381302", "", check02$flag1)
#  #輔導室、會計室、人事室的主管為組長，職稱無誤
# check02$flag62 <- if_else(check02$flag62 != "" & check02$organization_id == "381302", "", check02$flag62)
#  #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：31人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：24；差異百分比-29.2%" & check02$organization_id == "381302", "", check02$flag95)
#   #上越教育大學	學校教育研究科 正確
# check02$spe6 <- if_else(check02$spe6 == "教員資料表之大學（學士）以上各教育階段學歷資料不完整或不正確：陳曄玲（碩士學位畢業系所（一）：學校教育研究科）" & check02$organization_id == "381302", "", check02$spe6)
# 
# #私立大誠高中(381303)
#   #圖書館主管 暫缺
# check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：圖書館主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "381303", "", check02$flag1)
#   #放過學校 教員資料表專任教學人員人數偏低
# check02$flag18 <- if_else(check02$flag18 == "教員資料表專任教學人員人數偏低，請再協助確認實際聘任情況，或請確認是否填報完整教員名單資料。" & check02$organization_id == "381303", "", check02$flag18)
#   #圖書館的主管為組長，職稱無誤
# check02$flag62 <- if_else(check02$flag62 != "" & check02$organization_id == "381303", "", check02$flag62)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：19人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：23；差異百分比17.4%" & check02$organization_id == "381303", "", check02$flag95)
#   #代理可算全職，可暫不請學校修正
# check02$flag96 <- if_else(check02$flag96 == "教員資料表：簡慧超（代理(連) 人事室處主任）（校內一級主管（主任）原則由專任教職員擔（兼）任，請協助再確認上述教職員是否擔（兼）任校內一級主管（主任），或協助再確認上述教職員之聘任類別）" & check02$organization_id == "381303", "", check02$flag96)
# 
# # #私立再興中學(381304)
#  #確實沒有設置圖書館主管，僅有組長
# check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：圖書館主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "381304", "", check02$flag1)
#  #圖書室的主管為組長，職稱無誤
# check02$flag62 <- if_else(check02$flag62 != "" & check02$organization_id == "381304", "", check02$flag62)
#  #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：91人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：89；差異百分比-2.2%" & check02$organization_id == "381304", "", check02$flag95)
#   #國立高雄工商專科 正確
# check02$spe6 <- if_else(check02$spe6 == "教員資料表之大學（學士）以上各教育階段學歷資料不完整或不正確：林茂昌（副學士或專科畢業學校（一）：國立高雄工商專科）" & check02$organization_id == "381304", "", check02$spe6)
# 
# #私立景文高中(381305)
#   #確實沒有設置實習處主管，僅有組長
# check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：實習處主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "381305", "", check02$flag1)
#   #確實沒有設置科主任或學程主任
# check02$flag2 <- if_else(check02$flag2 == "請學校確認是否設置科主任或學程主任" & check02$organization_id == "381305", "", check02$flag2)
#   #兼任教師連續聘任不中斷無誤
# check02$flag80 <- if_else(check02$flag80 != "" & check02$organization_id == "381305", "", check02$flag80)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：77人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：71；差異百分比-8.5%" & check02$organization_id == "381305", "", check02$flag95)
#   #國立政治大學	學校行政所 正確
# check02$spe6 <- if_else(check02$spe6 == "教員資料表之大學（學士）以上各教育階段學歷資料不完整或不正確：蘇麗美（碩士學位畢業系所（一）：學校行政所）" & check02$organization_id == "381305", "", check02$spe6)
# 
# #臺北市靜心高中(381306)
#   #一位主任 一位人事管理員 正確
# check02$flag18 <- if_else(check02$flag18 == "人事室主管（主任）人數超過一位，請再協助確認實際聘任情況。" & check02$organization_id == "381306", "", check02$flag18)
#   #約聘僱可算全職 可暫不請學校修正，代理教師兼任圖書館主任 先給通過
# check02$flag96 <- if_else(check02$flag96 == "教員資料表：莊子賢（代理(連) 圖書館主任）； 職員(工)資料表：王舒葳（約聘僱 英語中心主任）（校內一級主管（主任）原則由專任教職員擔（兼）任，請協助再確認上述教職員是否擔（兼）任校內一級主管（主任），或協助再確認上述教職員之聘任類別）" & check02$organization_id == "381306", "", check02$flag96)
#  #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：65人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：72；差異百分比9.7%" & check02$organization_id == "381306", "", check02$flag95)
#   #國立中興大學	教師專業發展研究所 正確、陸軍官校	軍事情報學校特研班 正確
# check02$spe6 <- if_else(check02$spe6 == "教員資料表之大學（學士）以上各教育階段學歷資料不完整或不正確：陳翊萍（碩士學位畢業系所（一）：教師專業發展研究所）； 職員(工)資料表之大學（學士）以上各教育階段學歷資料不完整或不正確：徐良璧（學士學位畢業科系（一）：軍事情報學校特研班）" & check02$organization_id == "381306", "", check02$spe6)
# 
# #市立景美女中(383301)
#   #扣除年資不為零的人數確實偏高
# check02$flag64 <- if_else(check02$flag64 == "扣除年資不為零的人數似偏高，請再依欄位說明確認。" & check02$organization_id == "383301", "", check02$flag64)
#   #國立政治大學 學校行政研究所 正確
# check02$spe6 <- if_else(check02$spe6 == "教員資料表之大學（學士）以上各教育階段學歷資料不完整或不正確：周寤竹（碩士學位畢業系所（一）：學校行政研究所）" & check02$organization_id == "383301", "", check02$spe6)
#  #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：139人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：143；差異百分比2.8%" & check02$organization_id == "383301", "", check02$flag95)
# 
# #市立萬芳高中(383302)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：142人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：143；差異百分比0.7%" & check02$organization_id == "383302", "", check02$flag95)
# 
# #市立數位實驗高中(383303)
#   #沒有學務處 輔導室 有設學輔處、沒有設置圖書館、人事室暫無主管(人事主任目前是由弘道國中人事主任兼任，短期內(可能兩年內)不會換)
# check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：學務處主管 輔導室主管 圖書館主管 人事室主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "383303", "", check02$flag1)
#   #朱芝屏（兼任行政職職稱(一)：學生事務長） 正確(校內職稱確實為學生事務長 實際上是組長(二級單位主管))
# check02$flag62 <- if_else(check02$flag62 != "" & check02$organization_id == "383303", "", check02$flag62)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：7人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：20；差異百分比65.0%" & check02$organization_id == "383303", "", check02$flag95)
# 
# #市立木柵高工(383401)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：171人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：158；差異百分比-8.2%" & check02$organization_id == "383401", "", check02$flag95)
# 
# #市立南港高中(393301)
#   #補校教務組 補校訓導組 專任行政助理(臨時人員) 正確
# check02$flag62 <- if_else(check02$flag62 != "" & check02$organization_id == "393301", "", check02$flag62)
#   #忻凌琳 謝雅苓 皆非本學期退休或因故離職人員
# check02$flag86 <- if_else(check02$flag86 != "" & check02$organization_id == "393301", "", check02$flag86)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：149人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：159；差異百分比6.3%" & check02$organization_id == "393301", "", check02$flag95)
#   #國立政治大學	學校行政碩士 正確
# check02$spe6 <- if_else(check02$spe6 == "職員(工)資料表之大學（學士）以上各教育階段學歷資料不完整或不正確：謝鎧旭（碩士學位畢業系所（一）：學校行政碩士）" & check02$organization_id == "393301", "", check02$spe6)
# 
# #市立育成高中(393302)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：138人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：143；差異百分比3.5%" & check02$organization_id == "393302", "", check02$flag95)
#   #日本國立熊本商科大學 正確
# check02$spe6 <- if_else(check02$spe6 == "教員資料表之大學（學士）以上各教育階段學歷資料不完整或不正確：林倩穗（學士學位畢業學校（一）：日本國立熊本商科大學）" & check02$organization_id == "393302", "", check02$spe6)
# 
# #市立南港高工(393401)
#   #陳建恒 陳建恆
# check02$flag98 <- if_else(check02$flag98 == "教員資料表：請確認該員基本資料：陳建恆（姓名）" & check02$organization_id == "393401", "", check02$flag98)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：200人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：201；差異百分比0.5%" & check02$organization_id == "393401", "", check02$flag95)
# 
# #私立文德女中(401301)
#  #確實沒有設置學務處主管 輔導室主管 圖書館主管 人事室主管 主（會）計室主管(將停招)
# check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：學務處主管 輔導室主管 圖書館主管 人事室主管 主（會）計室主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "401301", "", check02$flag1)
#  #放過學校 教員資料表專任教學人員人數偏低
# check02$flag18 <- if_else(check02$flag18 == "教員資料表專任教學人員人數偏低，請再協助確認實際聘任情況，或請確認是否填報完整教員名單資料。" & check02$organization_id == "401301", "", check02$flag18)
#  #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：5人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：5；差異百分比0.0%" & check02$organization_id == "401301", "", check02$flag95)
# 
# #私立方濟中學(401302)
#  #確實沒有設置圖書館主管 主（會）計室主管，僅分別設有管理員、會計員
# check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：圖書館主管 主（會）計室主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "401302", "", check02$flag1)
#  #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：16人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：10；差異百分比-60.0%" & check02$organization_id == "401302", "", check02$flag95)
#   #胡嘉強（本校到職前學校服務總年資：0000） 該校校長是董事會找的人員來代理校長，從107年開始一直擔任校長到現在，過去沒有擔任教師或學校服務的經驗，所以到職前年資是0000
# check02$flag100 <- if_else(check02$flag100 == "胡嘉強（本校到職前學校服務總年資：0000）（校長『本校到職前學校服務總年資』似偏少，請確認校長以『校長身分』就任之日期，此日期請填在『本校到職日期』；校長在就任日期前，在本校及他校擔任教師與主任等全職工作之年資，請填在『本校到職前學校服務總年資』。）" & check02$organization_id == "401302", "", check02$flag100)
# 
# #私立達人女中(401303)
#  #確實沒有主（會）計室主管
# check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：主（會）計室主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "401303", "", check02$flag1)
#  #放過學校 職員(工)資料表專任人員人數偏低
# check02$flag18 <- if_else(check02$flag18 == "職員(工)資料表專任人員人數偏低，請再協助確認實際聘任情況，或請確認是否填報完整職員(工)名單資料。" & check02$organization_id == "401303", "", check02$flag18)
#  #會計室的主管為組長，職稱無誤
# check02$flag62 <- if_else(check02$flag62 != "" & check02$organization_id == "401303", "", check02$flag62)
#  #吳季倫 施佳慧 洪碧彩 許紋馨 許瑞敏 楊士弘 劉逸竹 皆非本學期退休或因故離職人員
# check02$flag86 <- if_else(check02$flag86 != "" & check02$organization_id == "401303", "", check02$flag86)
#  #姜孝宗 廖俊幃 王彩蓁 皆在上期基準日之前就離職
# check02$flag93 <- if_else(check02$flag93 != "" & check02$organization_id == "401303", "", check02$flag93)
#  #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：55人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：52；差異百分比-5.8%" & check02$organization_id == "401303", "", check02$flag95)
#  #約聘僱可算全職，可暫不請學校修正
# check02$flag96 <- if_else(check02$flag96 == "職員(工)資料表：吳景蓉（約聘僱 總務處主任）（校內一級主管（主任）原則由專任教職員擔（兼）任，請協助再確認上述教職員是否擔（兼）任校內一級主管（主任），或協助再確認上述教職員之聘任類別）" & check02$organization_id == "401303", "", check02$flag96)
# 
# #市立內湖高中(403301)
#   #侯明 張文駿 黃竣貿 皆非本學期退休或因故離職人員
# check02$flag86 <- if_else(check02$flag86 != "" & check02$organization_id == "403301", "", check02$flag86)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：145人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：145；差異百分比0.0%" & check02$organization_id == "403301", "", check02$flag95)
# 
# #市立麗山高中(403302)
#  #呂雅玲 李佩庭 楊秀香 皆非本學期退休或因故離職人員
# check02$flag86 <- if_else(check02$flag86 != "" & check02$organization_id == "403302", "", check02$flag86)
#  #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：90人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：91；差異百分比1.1%" & check02$organization_id == "403302", "", check02$flag95)
#   #中國海事商業專科學校 正確
# check02$spe6 <- if_else(check02$spe6 == "職員(工)資料表之大學（學士）以上各教育階段學歷資料不完整或不正確：吳筱伶（副學士或專科畢業學校（一）：中國海事商業專科學校）" & check02$organization_id == "403302", "", check02$spe6)
# 
# #市立南湖高中(403303)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：115人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：118；差異百分比2.5%" & check02$organization_id == "403303", "", check02$flag95)
#   #陳建恒 陳建恆
# check02$flag98 <- if_else(check02$flag98 == "教員資料表：請確認該員基本資料：陳建恒（姓名）" & check02$organization_id == "403303", "", check02$flag98)
# 
# #市立內湖高工(403401)
#   #唐瑜琪 張閔涵 彭仁傑 李典匡 洪慈敏 簡水淵 胡南亦 邱子容 邱欣心 陳奕希 皆非本學期退休或因故離職人員
# check02$flag86 <- if_else(check02$flag86 != "" & check02$organization_id == "403401", "", check02$flag86)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：172人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：176；差異百分比2.3%" & check02$organization_id == "403401", "", check02$flag95)
#   #市立內湖高工(403401)溫欣儒 市立士林高商(413401)温欣儒 
# check02$flag98 <- if_else(check02$flag98 == "教員資料表：請確認該員基本資料：溫欣儒（姓名）" & check02$organization_id == "403401", "", check02$flag98)
#   #實踐大學	英語專業溝通與教學科技研究所 正確
# check02$spe6 <- if_else(check02$spe6 == "教員資料表之大學（學士）以上各教育階段學歷資料不完整或不正確：林榆真（碩士學位畢業系所（一）：英語專業溝通與教學科技研究所）" & check02$organization_id == "403401", "", check02$spe6)
# 
# #私立泰北高中(411301)
#  #放過學校 職員(工)資料表專任人員人數偏低
# check02$flag18 <- if_else(check02$flag18 == "職員(工)資料表專任人員人數偏低，請再協助確認實際聘任情況，或請確認是否填報完整職員(工)名單資料。" & check02$organization_id == "411301", "", check02$flag18)
#   #游婕妮 許婕怡 正確
# check02$flag93 <- if_else(check02$flag93 != "" & check02$organization_id == "411301", "", check02$flag93)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：47人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：50；差異百分比6.0%" & check02$organization_id == "411301", "", check02$flag95)
#   #張玲玲（碩士學位畢業系所（一）：學校行政碩士） 鄭安伯（碩士學位畢業學校（一）：美國南加大USC） 正確
# check02$spe6 <- if_else(check02$spe6 != "" & check02$organization_id == "411301", "", check02$spe6)
# 
# #私立衛理女中(411302)
#  #確實沒有設置人事室主管
# check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：人事室主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "411302", "", check02$flag1)
#  #放過學校 職員(工)資料表專任人員人數偏低
# check02$flag18 <- if_else(check02$flag18 == "職員(工)資料表專任人員人數偏低，請再協助確認實際聘任情況，或請確認是否填報完整職員(工)名單資料。" & check02$organization_id == "411302", "", check02$flag18)
#  #服務單位：宗教室 正確、住校處的主管為組長 職稱無誤
# check02$flag62 <- if_else(check02$flag62 != "" & check02$organization_id == "411302", "", check02$flag62)
#  #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：69人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：77；差異百分比10.4%" & check02$organization_id == "411302", "", check02$flag95)
#   #駱威帆（碩士學位畢業學校（一）：台灣神學研究院） 正確
# check02$spe6 <- if_else(check02$spe6 != "" & check02$organization_id == "411302", "", check02$spe6)
# 
# #私立華岡藝校(411401)
#  #確實沒有設置圖書館主管 實習處主管 主（會）計室主管
# check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：圖書館主管 實習處主管 主（會）計室主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "411401", "", check02$flag1)
#  #放過學校 教員資料表專任教學人員人數偏低 一年以上與任教領域相關之業界實務工作經驗人數偏多
# check02$flag18 <- if_else(check02$flag18 == "教員資料表專任教學人員人數偏低，請再協助確認實際聘任情況，或請確認是否填報完整教員名單資料。一年以上與任教領域相關之業界實務工作經驗人數偏多（請再協助確認，『是否具備一年以上與任教領域相關之業界實務工作經驗』填寫『Y』之教員，是否確依欄位說明具備此經驗）" & check02$organization_id == "411401", "", check02$flag18)
#   #兼任行政職職稱(一)：校護 正確
# check02$flag62 <- if_else(check02$flag62 != "" & check02$organization_id == "411401", "", check02$flag62)
#  #林琍羨 黃凱群 在上學年填報後到職
# check02$flag93 <- if_else(check02$flag93 != "" & check02$organization_id == "411401", "", check02$flag93)
#  #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：32人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：35；差異百分比8.6%" & check02$organization_id == "411401", "", check02$flag95)
#  #范昌瑾（碩士學位畢業學校（一）：NEW ENGLAND CONSERVATORY OF MUSIC） 黃翠屏（碩士學位畢業學校（一）：CONSERVATORIO STATALE DIMILANO“GIUSEPPEVERDI”ITALIA）正確
# check02$spe6 <- if_else(check02$spe6 != "" & check02$organization_id == "411401", "", check02$spe6)
# 
# #市立陽明高中(413301)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：151人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：158；差異百分比4.4%" & check02$organization_id == "413301", "", check02$flag95)
#   #國立政治大學	學校行政碩士在職專班 正確
# check02$spe6 <- if_else(check02$spe6 == "教員資料表之大學（學士）以上各教育階段學歷資料不完整或不正確：程維煌（碩士學位畢業系所（一）：學校行政碩士在職專班）" & check02$organization_id == "413301", "", check02$spe6)
# 
# #市立百齡高中(413302)
#   #李至敏 林彩霞 王世珍 莊靜怡 蔡銘仁 鍾琁如 陳嘉嘉 皆非本學期退休或因故離職人員
# check02$flag86 <- if_else(check02$flag86 != "" & check02$organization_id == "413302", "", check02$flag86)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：157人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：158；差異百分比0.6%" & check02$organization_id == "413302", "", check02$flag95)
#   #吳淑楨（碩士學位畢業系所（一）：社會教育學系學校圖書館行政班） 正確 畢業證書確實這樣寫
# check02$spe6 <- if_else(check02$spe6 == "教員資料表之大學（學士）以上各教育階段學歷資料不完整或不正確：吳淑楨（碩士學位畢業系所（一）：社會教育學系學校圖書館行政班）" & check02$organization_id == "413302", "", check02$spe6)
# 
# #市立士林高商(413401)
#   #黃玉婷16歲開始工作無誤
# check02$flag39 <- if_else(check02$flag39 == "請確認該員之「本校到職日期」、「本校任職需扣除之年資」、「本校到職前學校服務總年資」，職員(工)資料表：黃玉婷47歲，但學校工作總年資有31年（約16歲開始工作）" & check02$organization_id == "413401", "", check02$flag39)
#   #陳澤榮 非本學期退休或因故離職人員
# check02$flag86 <- if_else(check02$flag86 != "" & check02$organization_id == "413401", "", check02$flag86)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：195人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：198；差異百分比1.5%" & check02$organization_id == "413401", "", check02$flag95)
#   #市立內湖高工(403401)溫欣儒 市立士林高商(413401)温欣儒 
# check02$flag98 <- if_else(check02$flag98 == "教員資料表：請確認該員基本資料：温欣儒（姓名）" & check02$organization_id == "413401", "", check02$flag98)
#   #私立淡水工商管理專科學校 正確
# check02$spe6 <- if_else(check02$spe6 == "職員(工)資料表之大學（學士）以上各教育階段學歷資料不完整或不正確：吳德明（副學士或專科畢業學校（一）：私立淡水工商管理專科學校） 殷淑玲（副學士或專科畢業學校（一）：私立淡水工商管理專科學校）" & check02$organization_id == "413401", "", check02$spe6)
# 
# #私立薇閣高中(421301)
#  #確實沒有設置圖書館主管 人事室主管 主（會）計室主管
# check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：圖書館主管 人事室主管 主（會）計室主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "421301", "", check02$flag1)
#  #國際部、會計室、人事室的主管為組長，職稱無誤
# check02$flag62 <- if_else(check02$flag62 != "" & check02$organization_id == "421301", "", check02$flag62)
#  #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：145人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：136；差異百分比-6.6%" & check02$organization_id == "421301", "", check02$flag95)
# 
# #臺北市幼華高中(421302)
#  #確實沒有設置圖書館主管 實習處主管 主（會）計室主管
# check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：圖書館主管 實習處主管 主（會）計室主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "421302", "", check02$flag1)
#  #放過學校 職員(工)資料表專任人員人數偏低
# check02$flag18 <- if_else(check02$flag18 == "職員(工)資料表專任人員人數偏低，請再協助確認實際聘任情況，或請確認是否填報完整職員(工)名單資料。" & check02$organization_id == "421302", "", check02$flag18)
#  #兼任教師、鐘點教師連續聘任不中斷無誤
# check02$flag80 <- if_else(check02$flag80 != "" & check02$organization_id == "421302", "", check02$flag80)
#  #吳鳳凰 周明源 夏淑芬 張鈺民 洪雅純 簡文魁 郭靜怡 陳依汝 黃意芬 皆非本學期退休或因故離職人員
# check02$flag86 <- if_else(check02$flag86 != "" & check02$organization_id == "421302", "", check02$flag86)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：48人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：50；差異百分比4.0%" & check02$organization_id == "421302", "", check02$flag95)
#   #臺北市幼華高中的校長是教育局官派的(督學)，直接到其他學校擔任校長6年6個月，沒有擔任教師的年資
# check02$flag100 <- if_else(check02$flag100 == "施博惠（本校到職前學校服務總年資：0606）（校長『本校到職前學校服務總年資』似偏少，請確認校長以『校長身分』就任之日期，此日期請填在『本校到職日期』；校長在就任日期前，在本校及他校擔任教師與主任等全職工作之年資，請填在『本校到職前學校服務總年資』。）" & check02$organization_id == "421302", "", check02$flag100)
# 
# #臺北市私立奎山實驗高級中學(421303)
#   #確實沒有設置圖書館主管 主（會）計室主管，輔導主任有填 隸屬於中學部
# check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：輔導室主管 圖書館主管 主（會）計室主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "421303", "", check02$flag1)
#  #放過學校 職員(工)資料表主聘單位各類別人數分布異常(`奎山``為實驗學校，不容易區分高中部或中學部，故有4位填"其他"，)
#    # 夏荻	人事室	行政秘書
#    # 馮臨燕	人事室	代理主任
#    # 曾台郇	總務處	總務組長
#    # 杜欣祐	圖書館	組長
# check02$flag18 <- if_else(check02$flag18 == "職員(工)資料表主聘單位各類別人數分布異常，請再協助確認實際聘任情況。" & check02$organization_id == "421303", "", check02$flag18)
# #圖書館的主管為組長，職稱無誤
# check02$flag62 <- if_else(check02$flag62 != "" & check02$organization_id == "421303", "", check02$flag62)
#  #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：25人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：22；差異百分比-13.6%" & check02$organization_id == "421303", "", check02$flag95)
#   #廖先怡（碩士學位畢業學校（一）：CAMBRIDGE SCHOOL OF ART (MA)）、夏荻（碩士學位畢業學校（一）：DALLAS BAPTIST UNIV.）
# check02$spe6 <- if_else(check02$spe6 != "" & check02$organization_id == "421303", "", check02$spe6)
# 
# #私立惇敘工商(421404)
#   #仲怡玲 林建光 林長義 梅元吉 游欣璇 王志偉 盧怡安 邱永樵 邱瓊滿 皆非本學期退休或因故離職人員
# check02$flag86 <- if_else(check02$flag86 != "" & check02$organization_id == "421404", "", check02$flag86)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：30人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：22；差異百分比-36.4%" & check02$organization_id == "421404", "", check02$flag95)
#  #代理可算全職，可暫不請學校修正 劉定芬（代理 輔導室主任） 王臨晟（代理 學務處主任） 謝春華（代理 圖書館主任） 陳柱政（代理 教務處主任）
# check02$flag96 <- if_else(check02$flag96 != "" & check02$organization_id == "421404", "", check02$flag96)
# 
# #市立復興高中(423301)
#   #張廣億 皆非本學期退休或因故離職人員
# check02$flag86 <- if_else(check02$flag86 != "" & check02$organization_id == "423301", "", check02$flag86)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：165人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：169；差異百分比2.4%" & check02$organization_id == "423301", "", check02$flag95)
# 
# #市立中正高中(423302)
#   #教員資料表專任教學人員人數偏低 給過 鐘點教師中，有63人的聘任科別為音樂，中正高中有藝才班
# check02$flag18 <- if_else(check02$flag18 == "教員資料表專任教學人員人數偏低，請再協助確認實際聘任情況，或請確認是否填報完整教員名單資料。" & check02$organization_id == "423302", "", check02$flag18)
#   #鐘點教師	聘任科別：本土語文-閩南語 這次可給通過
# check02$flag39 <- if_else(check02$flag39 == "請確認該員之「本校到職日期」、「本校任職需扣除之年資」、「本校到職前學校服務總年資」，教員資料表：張炳森85歲，但學校工作總年資有0年（約85歲開始工作）" & check02$organization_id == "423302", "", check02$flag39)
#   #周明樂 黃業建 皆非本學期退休或因故離職人員
# check02$flag86 <- if_else(check02$flag86 != "" & check02$organization_id == "423302", "", check02$flag86)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：167人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：178；差異百分比6.2%" & check02$organization_id == "423302", "", check02$flag95)
#   #國立政治大學	學校行政研究所 正確
# check02$spe6 <- if_else(check02$spe6 == "教員資料表之大學（學士）以上各教育階段學歷資料不完整或不正確：郭婷婷（碩士學位畢業系所（一）：學校行政研究所）" & check02$organization_id == "423302", "", check02$spe6)
# 
# 
# #天主教明誠高中(521301)
#  #確實沒有設置實習處主管 圖書館有組長
# check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：圖書館主管 實習處主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "521301", "", check02$flag1)
#  #沒有設置科主任
# check02$flag2 <- if_else(check02$flag2 == "請學校確認是否設置科主任或學程主任" & check02$organization_id == "521301", "", check02$flag2)
#  #蕭永逸（兼任行政職服務單位(一)：圖書館 兼任行政職職稱(一)：組長） 正確
# check02$flag62 <- if_else(check02$flag62 != "" & check02$organization_id == "521301", "", check02$flag62)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：58人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：52；差異百分比-11.5%" & check02$organization_id == "521301", "", check02$flag95)
#   #國立政治大學	學校行政、國立高雄師範大學 學校行政領導 正確
# check02$spe6 <- if_else(check02$spe6 == "教員資料表之大學（學士）以上各教育階段學歷資料不完整或不正確：謝怡靜（碩士學位畢業系所（一）：學校行政） 黃冠翔（碩士學位畢業系所（一）：學校行政領導）" & check02$organization_id == "521301", "", check02$spe6)
# 
# #私立大榮高中(521303)
#  #教員王昭月 兼任圖書館主任(圖書館主任隸屬於教務處)
# check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：圖書館主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "521303", "", check02$flag1)
#  #教務處設有英語發展中心主任及高中部主任 總務處設有總務主任及國小總務主任
# check02$flag18 <- if_else(check02$flag18 == "教務處主管（主任）人數超過一位，請再協助確認實際聘任情況。總務處主管（主任）人數超過一位，請再協助確認實際聘任情況。" & check02$organization_id == "521303", "", check02$flag18)
#  #李光庭（兼任行政職服務單位(一)：國小總務處） 正確
# check02$flag62 <- if_else(check02$flag62 != "" & check02$organization_id == "521303", "", check02$flag62)
#  #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：50人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：53；差異百分比5.7%" & check02$organization_id == "521303", "", check02$flag95)
#   #代理可算全職，可暫不請學校修正
# check02$flag96 <- if_else(check02$flag96 == "教員資料表：李佩齡（代理(連) 教務處英語發展中心主任）（校內一級主管（主任）原則由專任教職員擔（兼）任，請協助再確認上述教職員是否擔（兼）任校內一級主管（主任），或協助再確認上述教職員之聘任類別）" & check02$organization_id == "521303", "", check02$flag96)
#   #林宗獻（ 技術教師（兼任）） 過去確實取得資格 退休後回來兼任
# check02$flag99 <- if_else(check02$flag99 != "" & check02$organization_id == "521303", "", check02$flag99)
#   #王昭月 國立台灣師範大學	教育學院社會教育系學校圖書館行政班
#     #魏嫻芳 國立中山大學	社會科學學院教育研究所教師在職進修教學及學校行政碩士學位班教學組
#     #同等學歷 工業類電機工程科 
# check02$spe6 <- if_else(check02$spe6 != "" & check02$organization_id == "521303", "", check02$spe6)
# 
# #私立中華藝校(521401)
#  #確實沒有設置輔導室主管 圖書館主管 實習處主管
# check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：輔導室主管 圖書館主管 實習處主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "521401", "", check02$flag1)
#  #放過學校 教員資料表專任人員人數偏低
# check02$flag18 <- if_else(check02$flag18 == "教員資料表專任教學人員人數偏低，請再協助確認實際聘任情況，或請確認是否填報完整教員名單資料。" & check02$organization_id == "521401", "", check02$flag18)
#  #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：43人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：44；差異百分比2.3%" & check02$organization_id == "521401", "", check02$flag95)
#  #代理可算全職，可暫不請學校修正
# check02$flag96 <- if_else(check02$flag96 == "教員資料表：盧昰余（代理(連) 教務處主任）（校內一級主管（主任）原則由專任教職員擔（兼）任，請協助再確認上述教職員是否擔（兼）任校內一級主管（主任），或協助再確認上述教職員之聘任類別）" & check02$organization_id == "521401", "", check02$flag96)
# 
# #私立立志高中(551301)
#  #確實沒有設置主（會）計室主管
# check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：主（會）計室主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "551301", "", check02$flag1)
#   #柳信榮（0330103） 正確 (對該校來講是顧問 該校以代理教師方式聘他 )
# check02$flag7 <- if_else(check02$flag7 == "教員資料表：柳信榮（0330103）（請確認出生年月日是否正確）" & check02$organization_id == "551301", "", check02$flag7)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：89人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：93；差異百分比4.3%" & check02$organization_id == "551301", "", check02$flag95)
#   #江澈（本校到職前學校服務總年資：0700） 民國80年到職 過去年資為7年 正確
# check02$flag100 <- if_else(check02$flag100 == "江澈（本校到職前學校服務總年資：0700）（校長『本校到職前學校服務總年資』似偏少，請確認校長以『校長身分』就任之日期，此日期請填在『本校到職日期』；校長在就任日期前，在本校及他校擔任教師與主任等全職工作之年資，請填在『本校到職前學校服務總年資』。）" & check02$organization_id == "551301", "", check02$flag100)
#   #淡水工商管理專科學校 正確
# check02$spe6 <- if_else(check02$spe6 == "職員(工)資料表之大學（學士）以上各教育階段學歷資料不完整或不正確：洪秉坤（副學士或專科畢業學校（一）：淡水工商管理專科學校）" & check02$organization_id == "551301", "", check02$spe6)
# 
# #私立樹德家商(551402)
#   #確實沒有設置圖書館主管 (讀者服務組 組長)
# check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：圖書館主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "551402", "", check02$flag1)
#   #兼任教師連續聘任不中斷無誤
# check02$flag80 <- if_else(check02$flag80 != "" & check02$organization_id == "551402", "", check02$flag80)
#   #嚴永珅　 徐麗華 李淑如 李豐昌 解佳蓉 許裕琴 趙自屏 陳映如 皆非本學期退休或因故離職人員
# check02$flag86 <- if_else(check02$flag86 != "" & check02$organization_id == "551402", "", check02$flag86)
#   #倪寶珠 劉䕒珺 李奕廷 林秀敏 王靜怡 陳怡婷 陳樺亭 都是在112/2/1離職 所以上期資料沒有資料
# check02$flag93 <- if_else(check02$flag93 != "" & check02$organization_id == "551402", "", check02$flag93)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：148人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：147；差異百分比-0.7%" & check02$organization_id == "551402", "", check02$flag95)
#   #陳世林（ 技術教師（兼任）） 陳淑美（ 技術教師（兼任）） 過去確實取得資格 退休後回來兼任
# check02$flag99 <- if_else(check02$flag99 != "" & check02$organization_id == "551402", "", check02$flag99)
#   #林靜瑩（副學士或專科畢業學校（一）：自學進修學力鑑定） 正確
# check02$spe6 <- if_else(check02$spe6 == "職員(工)資料表之大學（學士）以上各教育階段學歷資料不完整或不正確：林靜瑩（副學士或專科畢業學校（一）：自學進修學力鑑定）" & check02$organization_id == "551402", "", check02$spe6)
# 
# #私立復華高中(581301)
#  #放過學校
# check02$flag18 <- if_else(check02$flag18 == "教員資料表專任教學人員人數偏低，請再協助確認實際聘任情況，或請確認是否填報完整教員名單資料。" & check02$organization_id == "581301", "", check02$flag18)
#   #余麗娟 吳碧仙 孫淑貞 蔡婉君 皆非本學期退休或因故離職人員
# check02$flag86 <- if_else(check02$flag86 != "" & check02$organization_id == "581301", "", check02$flag86)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：63人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：57；差異百分比-10.5%" & check02$organization_id == "581301", "", check02$flag95)
#   #約聘僱可算全職，可暫不請學校修正 教員資料表：柯東育（代理(連) 國小部主任） 楊順惠（代理(連) 實習處主任） 翁明國（代理(連) 學生事務處主任）； 職員(工)資料表：王詠惠（約聘僱 圖書館主任）
# check02$flag96 <- if_else(check02$flag96 != "" & check02$organization_id == "581301", "", check02$flag96)
# 
# #天主教道明中學(581302)
#  #教務處	音樂教育中心主任
#  #教務處	教務主任
#  #教務處	美術教育中心主任
# check02$flag18 <- if_else(check02$flag18 == "教務處主管（主任）人數超過一位，請再協助確認實際聘任情況。" & check02$organization_id == "581302", "", check02$flag18)
#   #護士師 正確
# check02$flag62 <- if_else(check02$flag62 != "" & check02$organization_id == "581302", "", check02$flag62)
#   #方曉熙 李鎮光 林淑娟 潘郁芬 葉志宏 皆非本學期退休或因故離職人員
# check02$flag86 <- if_else(check02$flag86 != "" & check02$organization_id == "581302", "", check02$flag86)
#   #盧鳳玉 糜蘭華 羅素卿 蔡汝平 邱莉娜 陳滿基 都是在112/2/1離職 所以上期資料沒有資料
# check02$flag93 <- if_else(check02$flag93 != "" & check02$organization_id == "581302", "", check02$flag93)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：163人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：155；差異百分比-5.2%" & check02$organization_id == "581302", "", check02$flag95)
# 
# #私立三信家商(581402)
#  #主（會）計室主管暫缺
# check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：主（會）計室主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "581402", "", check02$flag1)
#  #放過學校 職員(工)資料表專任人員人數偏低、教務處有主任及副主任
# check02$flag18 <- if_else(check02$flag18 == "教務處主管（主任）人數超過一位，請再協助確認實際聘任情況。；職員(工)資料表專任人員人數偏低，請再協助確認實際聘任情況，或請確認是否填報完整職員(工)名單資料。" & check02$organization_id == "581402", "", check02$flag18)
#  #姓名：葉釗珍（約聘僱） 約聘僱可算全職，可暫不請學校修正
# check02$flag90 <- if_else(check02$flag90 != "" & check02$organization_id == "581402", "", check02$flag90)
#  #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：64人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：65；差異百分比1.5%" & check02$organization_id == "581402", "", check02$flag95)
#  #約聘僱可算全職，可暫不請學校修正 劉志文（約聘僱 人事室主任） 林峻銘（約聘僱 總務處主任） 邱士賢（約聘僱 招生中心主任）
# check02$flag96 <- if_else(check02$flag96 != "" & check02$organization_id == "581402", "", check02$flag96)
#  #兵庫教育大學 學校教育學 正確
# check02$spe6 <- if_else(check02$spe6 == "教員資料表之大學（學士）以上各教育階段學歷資料不完整或不正確：陳紀蓉（碩士學位畢業系所（一）：學校教育學）" & check02$organization_id == "581402", "", check02$spe6)
# 
# #國立馬祖高中(720301)
#   #人事室主管暫缺
# check02$flag1 <- if_else(check02$flag1 == "尚待增補之學校主管：人事室主管（請確認是否填報完整名單，倘貴校上開主任尚未到職，請來電告知）" & check02$organization_id == "720301", "", check02$flag1)
#   #沒有設置學程主任
# check02$flag3 <- if_else(check02$flag3 == "請學校確認是否設置學程主任" & check02$organization_id == "720301", "", check02$flag3)
#   #本項目不需請學校修正
# check02$flag95 <- if_else(check02$flag95 == "統計處專任教師人數：34人；本資料庫專任教師、代理教師、校長、教官、主任教官人數：33；差異百分比-3.0%" & check02$organization_id == "720301", "", check02$flag95)
#   #國立臺北商業大學附設空中學院專科部 國立澎湖海事管理專科學校 正確
# check02$spe6 <- if_else(check02$spe6 != "" & check02$organization_id == "720301", "", check02$spe6)


check02$err_flag <- 0

temp <- c("flag1", "flag2", "flag3", "flag6", "flag7", "flag8", "flag9", "flag15", "flag16", "flag18", "flag19", "flag20", "flag24", "flag39", "flag45", "flag47", "flag48", "flag49", "flag50", "flag51", "flag52", "flag57", "flag59", "flag62", "flag64", "flag80", "flag82", "flag83", "flag84", "flag85", "flag86", "flag89", "flag90", "flag91", "flag92", "flag93", "flag94", "flag95", "flag96", "flag97", "flag98", "flag99", "flag100", "spe3", "spe5", "spe6", "flag25", "flag26", "flag29", "flag31", "flag56", "flag58", "flag61", "flag67", "flag68", "flag69", "flag70", "flag74", "flag75", "flag101", "spe4")
for (i in temp){
  check02[[i]] <- if_else(is.na(check02[[i]]), "", check02[[i]])
  check02$err_flag <- if_else(nchar(check02[[i]]) != 0, 1, check02$err_flag)
}

#刪除無錯誤的學校
check02 <- check02 %>%
  subset(err_flag != 0)

if (dim(check02)[1] != 0){
#標誌出無錯誤的處室
check02$err_flag_P <- 0
check02$err_flag_Ps <- 0
temp <- c("flag1", "flag2", "flag3", "flag6", "flag7", "flag8", "flag9", "flag15", "flag16", "flag18", "flag19", "flag20", "flag24", "flag39", "flag45", "flag47", "flag48", "flag49", "flag50", "flag51", "flag52", "flag57", "flag59", "flag62", "flag64", "flag80", "flag82", "flag83", "flag84", "flag85", "flag86", "flag89", "flag90", "flag91", "flag92", "flag93", "flag94", "flag95", "flag96", "flag97", "flag98", "flag99", "flag100", "spe3", "spe5", "spe6")
for (i in temp){
  check02$err_flag_P <- if_else(check02[[i]] == "", 1, check02$err_flag_P)
  check02$err_flag_Ps <- if_else(check02[[i]] != "", 1 + check02$err_flag_Ps, check02$err_flag_Ps)
}

check02$err_flag_Ps <- check02$err_flag_Ps %>% as.character()

check02$err_flag_LC <- 0
check02$err_flag_LCs <- 0
temp <- c("flag25", "flag26", "flag29", "flag31", "flag56", "flag58", "flag61", "flag67", "flag68", "flag69", "flag70", "flag74", "flag75", "flag101", "spe4")
for (i in temp){
  check02$err_flag_LC <- if_else(check02[[i]] == "", 1, check02$err_flag_LC)
  check02$err_flag_LCs <- if_else(check02[[i]] != "", 1 + check02$err_flag_LCs, check02$err_flag_LCs)
}

check02$err_flag_LCs <- check02$err_flag_LCs %>% as.character()

check02$flag_P_txt <- if_else(
  check02$err_flag_P == 0, "貴處室提供的資料，沒有檢查出需要修正之處。謝謝貴處室協助完成填報工作，請等待其他處室重新上傳資料，如果處室間資料比對有誤，系統會再發信通知。謝謝！",
  paste0("經本計畫複檢，仍發現共有",  check02$err_flag_Ps,  "個可能需要修正之處，懇請貴處室協助增補，尚祈見諒！修正後的檔案需重新完成整個填報流程。如有疑問，請與本計畫人員聯繫，謝謝！")
  )

check02$flag_LC_txt <- if_else(
  check02$err_flag_LC == 0, "貴處室提供的資料，沒有檢查出需要修正之處。謝謝貴處室協助完成填報工作，請等待其他處室重新上傳資料，如果處室間資料比對有誤，系統會再發信通知。謝謝！",
  paste0("經本計畫複檢，仍發現共有",  check02$err_flag_LCs,  "個可能需要修正之處，懇請貴處室協助增補，尚祈見諒！修正後的檔案需重新完成整個填報流程。如有疑問，請與本計畫人員聯繫，謝謝！")
)

temp <- c("flag1", "flag2", "flag3", "flag6", "flag7", "flag8", "flag9", "flag15", "flag16", "flag18", "flag19", "flag20", "flag24", "flag39", "flag45", "flag47", "flag48", "flag49", "flag50", "flag51", "flag52", "flag57", "flag59", "flag62", "flag64", "flag80", "flag82", "flag83", "flag84", "flag85", "flag86", "flag89", "flag90", "flag91", "flag92", "flag93", "flag94", "flag95", "flag96", "flag97", "flag98", "flag99", "flag100", "spe3", "spe5", "spe6", "flag25", "flag26", "flag29", "flag31", "flag56", "flag58", "flag61", "flag67", "flag68", "flag69", "flag70", "flag74", "flag75", "flag101", "spe4")
for (i in temp){
  for (j in 1:dim(check02)[1]){
    check02[[i]][j] <- if_else(check02[[i]][j] == "", "通過", check02[[i]][j])
  }
}

check02 <- check02 %>%
  subset(select = -c(err_flag, err_flag_P, err_flag_Ps))
openxlsx :: write.xlsx(check02, file = "C:\\edhr-112t2\\work\\edhr-112t2-check_print-人事.xlsx", rowNames = FALSE, overwrite = TRUE)
}else{
openxlsx :: write.xlsx(check02, file = "C:\\edhr-112t2\\work\\edhr-112t2-check_print-人事.xlsx", rowNames = FALSE, overwrite = TRUE)
}

#####自動化檢誤#####
#若全部學校皆未上傳(print檔案不存在 或 自己管區的學校皆未上傳)，以下皆不執行
if(!file.exists(checkfile_server) | #print檔案不存在
   (check02 %>% select("organization_id") %>% subset(organization_id %in% dis) %>% dim())[1] == 0) # 或 自己管區的學校皆未上傳
{
  print(paste(format(time_now, format = "%Y/%m/%d %H:%M"), " 本次無學校上傳", sep = ""))
}else
{
  #####自動化通知 - 每小時通知<本次上傳學校名單、需補正學校名單、三階檢通過學校名單>#####
  #初次執行需建立pre_list_agree和pre_correct_list兩個xlsx檔，用if else來做
  #若xlsx檔不存在，建立檔案
  if(!file.exists("C:/edhr-112t2/dta/edhr_112t2-202404/pre_list_agree_人事.xlsx"))
  {
    #建立pre_list_agree.xlsx
    pre_list_agree <- list_agree
    openxlsx :: write.xlsx(pre_list_agree, file = "C:/edhr-112t2/dta/edhr_112t2-202404/pre_list_agree_人事.xlsx", rowNames = FALSE, overwrite = TRUE)
    #建立pre_correct_list.xlsx
    correct_list <- readxl :: read_excel("C:\\edhr-112t2\\work\\edhr-112t2-check_print-人事.xlsx") %>% #本次需補正學校
      subset(select = c(organization_id, edu_name2))
    correct_list$edu_name2 <- paste(correct_list$edu_name2, "(", correct_list$organization_id, ")", sep = "")
    correct_list <- correct_list %>%
      mutate(pre_correct = 1)
    #以下是為了解決無法合併的問題
    if(dim(correct_list)[1] == 0)
    {
      correct_list[1, 1 : 2] = "0"
      colnames(correct_list) <- c("organization_id", "edu_name2", "pre_correct")
    }else
    {
      correct_list = correct_list
    }
    openxlsx :: write.xlsx(correct_list, file = "C:/edhr-112t2/dta/edhr_112t2-202404/pre_correct_list_人事.xlsx", rowNames = FALSE, overwrite = TRUE)
  }else
  {
    print("pre_list_agree和pre_correct_list兩個xlsx檔存在，繼續執行")
  }
  
  #若xlsx檔存在，執行
  if(file.exists("C:/edhr-112t2/dta/edhr_112t2-202404/pre_list_agree_人事.xlsx"))
  {
    #讀取上次名單
    pre_list_agree <- readxl :: read_excel("C:/edhr-112t2/dta/edhr_112t2-202404/pre_list_agree_人事.xlsx")
    pre_list_agree$organization_id <- as.character(pre_list_agree$organization_id)
    pre_correct_list <- readxl :: read_excel("C:/edhr-112t2/dta/edhr_112t2-202404/pre_correct_list_人事.xlsx")
    pre_correct_list <- mutate(pre_correct_list, pre_correct = 1)
    #以下是為了解決pre_list_agree無法合併的問題(若出現此問題只會發生在list_agree為空 且 pre_list_agree為空)
    if(dim(pre_list_agree)[1] == 0 & dim(list_agree)[1] == 0)
    {
      pre_list_agree <- list_agree
    }else
    {
      pre_list_agree = pre_list_agree
    }
    
    #本次上傳 - 本次出現但上次沒出現
    organization <- readxl :: read_excel("\\\\192.168.110.245\\Plan_edhr\\教育部高級中等學校教育人力資源資料庫建置第7期計畫(1120201_1130731)\\1121全國學校名單.xls") %>% #[每次填報更改]本次填報的學校名單檔案路徑
      select("學校代碼", "學校名稱") %>%
      rename(organization_id = 學校代碼, edu_name = 學校名稱) %>%
      rename(name = edu_name)
    compare_list <- left_join(list_agree, pre_list_agree, by = c("organization_id")) %>%
      subset(is.na(agree.y))
    compare_list <- merge(x = compare_list, y = organization, by = "organization_id", all.x = TRUE)
    #以下是為了解決compare_list為0
    if(dim(compare_list)[1] == 0)
    {
      compare_list[1, 1 : 4] = 0
    }else
    {
      compare_list = compare_list
    }
    compare_list$name <- paste(compare_list$name, "(", compare_list$organization_id, ")", sep = "")
    
    #本次上傳 - 本次出現且上次出現且出現在上次需修正名單(compare_correct_list的意思是在這次上傳期間未處理上次未通過的學校)
    compare_correct_list <- left_join(list_agree, pre_correct_list, by = c("organization_id")) %>%
      subset(pre_correct == 1)
    #以下是為了解決compare_correct_list為0
    if(dim(compare_correct_list)[1] == 0)
    {
      compare_correct_list[1, c(1, 3)] = "0"
      compare_correct_list[1, c(2, 4)] = 0
    }else
    {
      compare_correct_list = compare_correct_list
    }
    
    #compare_correct_list$edu_name2 <- paste(compare_correct_list$edu_name2, "(", compare_correct_list$organization_id, ")", sep = "")
    #另存'本次已上傳名單"，以便於與下次名單比對
    pre_list_agree <- list_agree
    openxlsx :: write.xlsx(pre_list_agree, file = "C:/edhr-112t2/dta/edhr_112t2-202404/pre_list_agree_人事.xlsx", rowNames = FALSE, overwrite = TRUE)
    
    correct_list <- readxl :: read_excel("C:\\edhr-112t2\\work\\edhr-112t2-check_print-人事.xlsx") %>% #本次需補正學校
      subset(select = c(organization_id, edu_name2))
    correct_list_c <- correct_list %>%
      subset(select = c(organization_id))
    correct_list$edu_name2 <- paste(correct_list$edu_name2, "(", correct_list$organization_id, ")", sep = "")
    
    #處理correct_list為tibble的問題
    if(dim(correct_list)[1] == 0){
      correct_list <- data.frame(
        organization_id = c(""), 
        edu_name2 = c("")
      )
      correct_list <- correct_list[-1, ]
    }else{
      correct_list <- correct_list
    }
    #將correct_list_c 變數的data type改為char
    if(is.character(correct_list_c$organization_id)){
      correct_list_c <- correct_list_c
    }else{
      correct_list_c <- correct_list_c %>% mutate(across(organization_id, as.character))
    }
    correct_list <- left_join(compare_list, correct_list, by = c("organization_id")) %>%
      subset(select = c(organization_id, edu_name2)) %>%
      subset(!is.na(edu_name2))
    correct_list_2 <- left_join(correct_list_c, compare_correct_list, by = c("organization_id")) %>%
      subset(select = c(organization_id, edu_name2, pre_correct)) %>%
      subset(pre_correct == 1)
    correct_list <- bind_rows(correct_list, correct_list_2)
    correct <- apply(as.data.frame(correct_list$edu_name2), 2, paste, collapse = ", ")
    
    #用stata將學校三階檢未通過改為通過之處理
    #出現在上次需修正名單(pre_correct_list) 且未出現在本次需修正名單(correct_list) 且出現在compare_correct_list，則從compare_correct_list刪除
    #也就是我不要pre_correct_list == 1 & is.na(correct_list) & compare_correct_list == 1
    compare_correct_list <- compare_correct_list %>%
      mutate(compare_correct_list = 1)
    pre_correct_list_c <- pre_correct_list %>%
      mutate(pre_correct_list = 1)
    correct_list_c <- correct_list
    #以下是為了解決correct_list_c無法合併的問題
    if(dim(correct_list_c)[1] == 0)
    {
      correct_list_c[1, 1 : 2] = 0
      colnames(correct_list_c) <- c("organization_id", "edu_name2", "pre_correct")
    }else
    {
      correct_list_c = correct_list_c
    }
    correct_list_c <- correct_list_c %>%
      mutate(correct_list = 1)
    compare_correct_list <- merge(x = compare_correct_list, y = pre_correct_list_c, by = "organization_id", all = TRUE)
    compare_correct_list <- merge(x = compare_correct_list, y = correct_list_c, by = "organization_id", all = TRUE)
    compare_correct_list <- compare_correct_list %>%
      subset(compare_correct_list != 1 | pre_correct != 1 | !is.na(correct_list)) #By De Morgan' s Laws, (A交集B交集C)的補集合 = A補集合或B補集合或C補集合
    compare_correct_list <- compare_correct_list %>%
      subset(select = c(organization_id, agree, edu_name2.x, pre_correct.x)) %>%
      rename(edu_name2 = edu_name2.x, pre_correct = pre_correct.x)
    
    #以下是為了解決無法合併的問題
    if(dim(correct_list)[1] == 0)
    {
      correct_list[1, 1 : 2] = 0
      colnames(correct_list) <- c("organization_id", "edu_name2", "pre_correct")
    }else
    {
      correct_list = correct_list
    }
    
    #以下是為了解決correct開頭為","
    str_corr <- str_locate(correct, ",")[ ,1]
    
    if(is.na(str_corr))
    {
      str_corr = " "
    }else
    {
      str_corr = str_corr
    }
    
    if(str_corr == 1)
    {
      correct = substr(correct, start = 2, stop = nchar(correct))  
    }else
    {
      correct = correct
    }
    
    #建立表格內容會用到的學校名單
    now <- apply(as.data.frame(compare_list$name), 2, paste, collapse = ", ") #本次上傳學校
    #以下是為了解決now為0(0)
    if(now == "0(0)")
    {
      now = ""
    }else
    {
      now = now
    }
    now <- paste(now, apply(as.data.frame(compare_correct_list$edu_name2), 2, paste, collapse = ", "), sep = ", ")
    
    #以下是為了解決now開頭為","
    str_now <- str_locate(now, ",")[ ,1]
    
    if(is.na(str_now))
    {
      str_now = " "
    }else
    {
      str_now = str_now
    }
    
    if(str_now == 1)
    {
      now = substr(now, start = 2, stop = nchar(now))  
    }else
    {
      now = now
    }
    #以下是為了解決now為0
    if(now == "0")
    {
      now = ""
    }else
    {
      now = now
    }
    
    #以下是為了解決now為 0
    if(now == " 0")
    {
      now = ""
    }else
    {
      now = now
    }
    
    #以下是為了解決now結尾為", 0"
    str_now <- str_locate(now, ", 0")[ ,1]
    
    if(!is.na(str_now) & now != "")
    {
      now = substr(now, start = 1, stop = nchar(now) - 3)
    }else
    {
      now = now
    }
    
    #以下是為了解決now結尾為", NA"
    if(is.na(str_locate(now, ", NA")[ ,1])){
      now = now
    }else if(str_locate(now, ", NA")[ ,1] == nchar(now) - 3){
      now = substr(now, start = 1, stop = nchar(now) - 4)
    }else{
      now = now
    }
    
    #另存'本次需修正名單"，以便於與下次名單比對
    openxlsx :: write.xlsx(correct_list, file = "C:/edhr-112t2/dta/edhr_112t2-202404/pre_correct_list_人事.xlsx", rowNames = FALSE, overwrite = TRUE)
    
    clear_list <- left_join(compare_list, correct_list, by = c("organization_id")) %>% #本次三階檢通過學校
      subset(select = c(organization_id, edu_name2, name)) %>%
      subset(is.na(edu_name2)) 
    clear <-apply(as.data.frame(clear_list$name), 2, paste, collapse = ", ")
    clear_correct_list <- merge(x = correct_list, y = pre_correct_list, by = c("organization_id"), all = TRUE) %>%
      subset(is.na(edu_name2.x)) %>%
      subset(select = c(organization_id, edu_name2.y))
    clear_correct_list <- merge(x = clear_correct_list , y = pre_correct_list, by = c("organization_id"), all.x = TRUE) %>%  #clear_correct_list: 沒出現在correct_list 且出現在pre_correct_list，可能為(1)本次通過且上次未通過 或(2)本次被退件且上次未通過，需排除(2)，也就是clear_correct_list的名單若也出現在pre_correct_list，需排除
      subset(is.na(edu_name2)) %>%
      subset(select = c(organization_id, edu_name2.y))
    #以下是為了解決clear_correct_list為0
    if(dim(clear_correct_list)[1] == 0)
    {
      clear_correct_list[1, 1 : 2] = 0
    }else
    {
      clear_correct_list = clear_correct_list
    }
    
    clear_correct_list$edu_name2.y <- substr(clear_correct_list$edu_name2.y, start = 1, stop = str_locate(clear_correct_list$edu_name2.y, pattern = "\\(")[1, 1] - 1)
    clear_correct_list$edu_name2.y <- paste(clear_correct_list$edu_name2.y, "(", clear_correct_list$organization_id, ")", sep = "")
    clear_2 <-apply(as.data.frame(clear_correct_list$edu_name2.y), 2, paste, collapse = ", ")
    clear <- paste(clear, clear_2, sep = ",")
    
    #以下是為了解決clear開頭為","
    if(str_locate(clear, ",")[ ,1] == 1)
    {
      clear = substr(clear, start = 2, stop = nchar(clear))  
    }else
    {
      clear = clear
    }
    
    #以下是為了解決"0(0)"
    if(now == "0(0)")
    {
      now = ""
    }else
    {
      now = now
    }
    
    #以下是為了解決now中間出現NA
    now <- gsub(", NA", "", now)
    
    if(correct == "0(0)")
    {
      correct = ""
    }else
    {
      correct = correct
    }
    
    #以下是為了解決"NA(0)"
    if(clear == "NA(0)")
    {
      clear = ""
    }else
    {
      clear = clear
    }
    
    #以下是為了解決clear出現,NA(0)
    clear <- gsub(",NA\\(0\\)", "", clear)
    
    #以下是為了解決clear為"0(0)"
    if(clear == "0(0)")
    {
      clear = ""
    }else
    {
      clear = clear
    }
    
    #以下是為了解決now為" NA"
    now <- gsub(" NA", "", now)
    
    #excel視窗通知
    #先判斷check_print檔案是否使用中(以"是否可更改檔案名稱"來判斷 若可更改 代表未使用中)
    checkprint_filename <- "C:\\edhr-112t2\\work\\edhr-112t2-check_print-人事.xlsx"
    checkprint_filename_2 <- substr(checkprint_filename, start = 1, stop = str_locate(checkprint_filename, ".xlsx")[ ,1] - 1)  
    
    if(file.rename(from = checkprint_filename, to = paste(checkprint_filename_2, "2.xlsx", sep = "")) == TRUE)
    {
      if(nchar(now) == 0)
      {
        file.rename(from = paste(checkprint_filename_2, "2.xlsx", sep = ""), to = checkprint_filename)
        
        paste(format(time_now, format = "%Y/%m/%d %H:%M"), " 本次無學校上傳", sep = "")
      }else
      {
        #存入xlsx，自動開啟
        #建立檔案名稱
        correct_filename_year <- substr(title, start = str_locate(title, "學年度")[ ,1] - 3, stop = str_locate(title, "學年度")[ ,1] - 1)
        if(substr(title, start = str_locate(title, "學期")[ ,1] - 1, stop = str_locate(title, "學期")[ ,1] - 1) == "上")
        {
          correct_filename_sem <- "1"
        }else{
          correct_filename_sem <- "2"
        }
        correct_filename_name <- substr(title, start = str_locate(title, "（")[ ,1] + 1, stop = str_locate(title, "）")[ ,1] - 1)
        correct_filename <- paste(correct_filename_year, correct_filename_sem, correct_filename_name, "_上傳名單", sep = "")
        
        #建立fileopen.bat
        write.table(paste("start C:\\autochecking\\",correct_filename, ".xlsx", sep = ""), file = "C:\\autochecking\\fileopen.bat", append = FALSE, quote = FALSE, col.names = FALSE, row.names = FALSE, fileEncoding = "BIG5")
        
        if(!file.exists(paste("C:\\autochecking\\",correct_filename, ".xlsx", sep = "")))
        {
          #如果檔案不存在
          # 建立 Excel 活頁簿
          wb <- createWorkbook()
          
          # 設定框線樣式
          options("openxlsx.borderColour" = "#4F80BD")
          options("openxlsx.borderStyle" = "thin")
          
          # 設定 Excel 活頁簿預設字型
          modifyBaseFont(wb, fontSize = 20, fontName = "Arial")
          
          # 新增工作表
          addWorksheet(wb, sheetName = "上傳名單", gridLines = FALSE)
          
          # 建立上傳學校名單表格
          body <- data.frame(matrix(0, 1, 4))
          colnames(body) <- c("上傳時間", "本次上傳學校", "本次需補正學校", "本次三階檢通過學校")
          body[1, ] <- c(format(time_now, format = "%Y/%m/%d %H:%M"), now, correct, clear)
          
          # 建立樣式
          headSty <- createStyle(fontSize = 22, fgFill="#DCE6F1", halign="center", border = "TopBottomLeftRight", wrapText = TRUE)
          
          # 將學校名單表格寫入
          txtSty <- createStyle(halign="left", valign = "center", border = "TopBottomLeftRight", wrapText = TRUE)
          writeData(wb, 1, x = body, startCol = "A", startRow=1, borders="rows", headerStyle = headSty)
          addStyle(wb, sheet = 1, style = txtSty, cols = 1:4, rows = 2:(dim(body)[1]+1), gridExpand = TRUE)
          
          # 設定欄寬
          setColWidths(wb, 1, cols=1, widths = 16)
          setColWidths(wb, 1, cols=2:5, widths = 20)
          
          # 儲存 Excel 活頁簿
          saveWorkbook(wb, paste("C:\\autochecking\\",correct_filename, ".xlsx", sep = ""), overwrite = TRUE)
          
          # excel檔開啟30秒後自動關閉
          time_a <- Sys.time()
          a <- as.numeric(format(time_a, format = "%M")) * 60 + as.numeric(format(time_a, format = "%S"))      
          shell.exec("C:\\autochecking\\fileopen.bat")
          
          b <- a
          while (b - a < 30)
          {
            time_b <- Sys.time()
            b <- as.numeric(format(time_b, format = "%M")) * 60 + as.numeric(format(time_b, format = "%S"))
          }
          
          if(!file.exists("C:\\autochecking\\fileclose.bat"))
          {
            write.table(paste("taskkill /FI \"WINDOWTITLE eq ", correct_filename, "*\"", sep = ""), file = "C:\\autochecking\\fileclose.bat", append = FALSE, quote = FALSE, col.names = FALSE, row.names = FALSE)
          }else
          {
            print("建立fileclose.bat檔案")
          }
          
          shell.exec("C:\\autochecking\\fileclose.bat")
        }else{
          #如果檔案存在
          # 建立 Excel 活頁簿
          wb <- createWorkbook()
          
          # 設定框線樣式
          options("openxlsx.borderColour" = "#4F80BD")
          options("openxlsx.borderStyle" = "thin")
          
          # 設定 Excel 活頁簿預設字型
          modifyBaseFont(wb, fontSize = 20, fontName = "Arial")
          
          # 新增工作表
          addWorksheet(wb, sheetName = "上傳名單", gridLines = FALSE)
          
          # 建立上傳學校名單表格
          body <- readxl :: read_excel(paste("C:\\autochecking\\",correct_filename, ".xlsx", sep = ""))
          body <- rbind(c("0", "0", "0", "0"), body)
          colnames(body) <- c("上傳時間", "本次上傳學校", "本次需補正學校", "本次三階檢通過學校")
          body[1, ] <- as.list(c(format(time_now, format = "%Y/%m/%d %H:%M"), now, correct, clear))
          
          # 建立樣式
          headSty <- createStyle(fontSize = 22, fgFill="#DCE6F1", halign="center", border = "TopBottomLeftRight", wrapText = TRUE)
          
          # 將學校名單表格寫入
          txtSty <- createStyle(halign="left", valign = "center", border = "TopBottomLeftRight", wrapText = TRUE)
          writeData(wb, 1, x = body, startCol = "A", startRow=1, borders="rows", headerStyle = headSty)
          addStyle(wb, sheet = 1, style = txtSty, cols = 1:4, rows = 2:(dim(body)[1]+1), gridExpand = TRUE)
          
          # 設定欄寬
          setColWidths(wb, 1, cols=1, widths = 16)
          setColWidths(wb, 1, cols=2:5, widths = 20)
          
          # 儲存 Excel 活頁簿
          saveWorkbook(wb, paste("C:\\autochecking\\",correct_filename, ".xlsx", sep = ""), overwrite = TRUE)
          
          # excel檔開啟30秒後自動關閉
          time_a <- Sys.time()
          a <- as.numeric(format(time_a, format = "%M")) * 60 + as.numeric(format(time_a, format = "%S"))      
          shell.exec("C:\\autochecking\\fileopen.bat")
          
          b <- a
          while (b - a < 30)
          {
            time_b <- Sys.time()
            b <- as.numeric(format(time_b, format = "%M")) * 60 + as.numeric(format(time_b, format = "%S"))
          }
          
          write.table(paste("taskkill /FI \"WINDOWTITLE eq ", correct_filename, "*\"", sep = ""), file = "C:\\autochecking\\fileclose.bat", append = FALSE, quote = FALSE, col.names = FALSE, row.names = FALSE, fileEncoding = "BIG5")
          
          shell.exec("C:\\autochecking\\fileclose.bat")
          
          file.rename(from = "C:\\edhr-112t2\\work\\edhr-112t2-check_print-人事2.xlsx", to = "C:\\edhr-112t2\\work\\edhr-112t2-check_print-人事.xlsx")
        }
      }
    }else
    {
      #建立errortext_fileopen.bat
      if(!file.exists("C:\\autochecking\\errortext_fileopen.bat"))
      {
        write.table("start C:\\autochecking\\errortext_fileopen.xlsx", file = "C:\\autochecking\\errortext_fileopen.bat", append = FALSE, quote = FALSE, col.names = FALSE, row.names = FALSE)
        print("建立errortext_fileopen.bat檔案")
      }else
      {
        print("errortext_fileopen.bat檔案存在")
      }
      
      #如果檔案不存在
      # 建立 Excel 活頁簿
      wb <- createWorkbook()
      
      # 設定框線樣式
      options("openxlsx.borderColour" = "#4F80BD")
      options("openxlsx.borderStyle" = "thin")
      
      # 設定 Excel 活頁簿預設字型
      modifyBaseFont(wb, fontSize = 20, fontName = "Arial")
      
      # 新增工作表
      addWorksheet(wb, sheetName = "上傳名單", gridLines = FALSE)
      
      # 建立上傳學校名單表格
      body <- "本次自動化檢核未執行，請盡速關閉檢核報告word檔，自動化檢核方可繼續執行。閱讀完畢請關閉此檔案。"
      
      # 建立樣式
      headSty <- createStyle(fontSize = 22, fgFill="#DCE6F1", halign="center", border = "TopBottomLeftRight", wrapText = TRUE)
      
      # 將學校名單表格寫入
      txtSty <- createStyle(halign="left", valign = "center", border = "TopBottomLeftRight", wrapText = TRUE)
      writeData(wb, 1, x = body, startCol = "A", startRow=1, borders="rows", headerStyle = headSty)
      addStyle(wb, sheet = 1, style = txtSty, cols = 1:5, rows = 1, gridExpand = TRUE)
      mergeCells(wb, sheet = 1, cols = 1:5, rows = 1:1)
      
      # 設定欄寬
      setColWidths(wb, 1, cols=1, widths = 16)
      setColWidths(wb, 1, cols=2:5, widths = 20)
      
      # 儲存 Excel 活頁簿
      saveWorkbook(wb, "C:\\autochecking\\errortext_fileopen.xlsx", overwrite = TRUE)
      shell.exec("C:\\autochecking\\errortext_fileopen.bat")
    }
  }else
  {
    print("pre_list_agree和pre_correct_list兩個xlsx檔不存在")
  }
}
