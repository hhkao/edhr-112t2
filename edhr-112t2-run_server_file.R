library(httr)
library(readxl)
library(openxlsx)

headers <- c(
  "Host" = "edhr-r.k12ea.gov.tw"
)

res <- VERB("GET", url = "https://edhr.k12ea.gov.tw/edhr-112t2-check_print.xlsx", add_headers(headers))

# 嘗試打開文件
tryCatch(
  {
    writeBin(content(res, "raw"), "\\\\192.168.110.245\\Plan_edhr\\教育部高級中等學校教育人力資源資料庫建置第7期計畫(1120201_1130731)\\檢核語法檔\\R\\自動化資料檢核結果\\edhr-112t2-check_print.xlsx")
  },
  error = function(e) {
    # 發生錯誤時執行通知

    errorMessage <- paste(Sys.time(), "Error: ", e$message)
    print(errorMessage) # 在控制台上輸出錯誤消息

    # 建立errortext_serverfile.bat
    if (!file.exists("C:\\autochecking\\errortext_serverfile.bat")) {
      write.table("start C:\\autochecking\\errortext_serverfile.xlsx", file = "C:\\autochecking\\errortext_serverfile.bat", append = FALSE, quote = FALSE, col.names = FALSE, row.names = FALSE)
      print("建立errortext_serverfile.bat檔案")
    } else {
      print("errortext_serverfile.bat檔案存在")
    }

    # 如果檔案不存在
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
    body <- "檢核結果檔無法寫入，請確認檔案是否開啟。閱讀完畢請關閉此檔案。"

    # 建立樣式
    headSty <- createStyle(fontSize = 22, fgFill = "#DCE6F1", halign = "center", border = "TopBottomLeftRight", wrapText = TRUE)

    # 將學校名單表格寫入
    txtSty <- createStyle(halign = "left", valign = "center", border = "TopBottomLeftRight", wrapText = TRUE)
    writeData(wb, 1, x = body, startCol = "A", startRow = 1, borders = "rows", headerStyle = headSty)
    addStyle(wb, sheet = 1, style = txtSty, cols = 1:5, rows = 1, gridExpand = TRUE)
    mergeCells(wb, sheet = 1, cols = 1:5, rows = 1:1)

    # 設定欄寬
    setColWidths(wb, 1, cols = 1, widths = 16)
    setColWidths(wb, 1, cols = 2:5, widths = 20)

    # 儲存 Excel 活頁簿
    saveWorkbook(wb, "C:\\autochecking\\errortext_serverfile.xlsx", overwrite = TRUE)
    shell.exec("C:\\autochecking\\errortext_serverfile.bat")
  }
)

#organization_id為double的處理
checkfile_server <-
  "\\\\192.168.110.245\\Plan_edhr\\教育部高級中等學校教育人力資源資料庫建置第7期計畫(1120201_1130731)\\檢核語法檔\\R\\自動化資料檢核結果\\\\edhr-112t2-check_print.xlsx" #[每次填報更改]請更改本次server匯出的檢核結果檔之路徑
check02_server <- readxl::read_excel(checkfile_server)
check02_server$organization_id <-
  as.character(check02_server$organization_id)
openxlsx :: write.xlsx(check02_server, file = "\\\\192.168.110.245\\Plan_edhr\\教育部高級中等學校教育人力資源資料庫建置第7期計畫(1120201_1130731)\\檢核語法檔\\R\\自動化資料檢核結果\\\\edhr-112t2-check_print.xlsx", rowNames = FALSE, overwrite = TRUE)
