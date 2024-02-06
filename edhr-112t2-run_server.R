rm(list = ls())

# 載入所需套件
library(DBI)
library(odbc)
library(magrittr)
library(dplyr)
library(readxl)
library(stringr)
library(openxlsx)
library(tidyr)
library(reshape2)
library(scales)

# 匯入學校資料檔 -------------------------------------------------------------------
# input data
# 分頁名稱為系統指定。

#資料讀取#
#連線
source("connection.R")
#edhr <- dbConnect(odbc::odbc(), "CHER04-TIPEDSTG", timeout = 10)

#請輸入本次填報設定檔標題(字串需與標題完全相符，否則會找不到)
title <- "112學年度下學期高級中等學校教育人力資源資料庫（國立學校人事及教務）"

department <- "人事室"

#讀取審核同意之學校名單
list_agree <- dbGetQuery(
  edhr,
  paste(
    "
SELECT DISTINCT b.id AS organization_id , 1 AS agree
FROM [plat5_edhr].[dbo].[teacher_fillers] a
LEFT JOIN
(SELECT a.reporter_id, c.id
FROM [plat5_edhr].[dbo].[teacher_fillers] a LEFT JOIN [plat5_edhr].[dbo].[teacher_reporters] b ON a.reporter_id = b.id
LEFT JOIN [plat5_edhr].[dbo].[organization_details] c ON b.organization_id = c.organization_id
) b ON a.reporter_id = b.reporter_id
WHERE a.agree = 1 AND department_id = (SELECT id FROM [plat5_edhr].[dbo].[teacher_departments]
                                        WHERE report_id = (SELECT id FROM [plat5_edhr].[dbo].[teacher_reports]
                                                            WHERE title = '",
    title,
    "') AND title = '",
    department,
    "')",
    sep = ""
  )
) %>%
  distinct(organization_id, .keep_all = TRUE)

#讀取教員資料表名稱
teacher_tablename <- dbGetQuery(
  edhr,
  paste(
    "
SELECT [name] FROM [plat5_edhr].[dbo].[row_tables]
	where sheet_id = (SELECT [id] FROM [plat5_edhr].[dbo].[row_sheets]
						          where file_id = (SELECT field_component_id FROM [plat5_edhr].[dbo].[teacher_datasets]
											                  WHERE title = '教員資料表' AND department_id = (SELECT id FROM [plat5_edhr].[dbo].[teacher_departments]
																						                                              WHERE title = '",
    department,
    "' AND  report_id = (SELECT id FROM [plat5_edhr].[dbo].[teacher_reports]
		WHERE title = '",
    title,
    "'))))",
    sep = ""
  )
) %>% as.character()

#教員資料表尚未建立的判斷
if (teacher_tablename == "character(0)") {
  stop("The teacher table has not been created yet, so data validation will not be executed.")
} else{
  #讀取教員資料表
  teacher <- dbGetQuery(
    edhr,
    paste(
      "SELECT * FROM [rows].[dbo].[",
      teacher_tablename,
      "] WHERE deleted_at IS NULL",
      sep = ""
    )
  ) %>%
    subset(select = -c(
      id,
      created_at,
      deleted_at,
      updated_by,
      created_by,
      deleted_by
    ))
  
  #欄位名稱更改為設定的欄位代號
  col_names <-
    dbGetQuery(edhr,
               "SELECT id, name, title FROM [plat5_edhr].[dbo].[row_columns]")
  col_names$id <- paste("C", col_names$id, sep = "")
  for (i in 2:dim(teacher)[2])
    #從2開始是因為第一的欄位是update_at
  {
    colnames(teacher)[i] <-
      col_names$name[grep(paste(colnames(teacher)[i], "$", sep = ""), col_names$id)]
  }
  #格式調整
  teacher$gender <-
    formatC(
      teacher$gender,
      dig = 0,
      wid = 1,
      format = "f",
      flag = "0"
    )
  teacher$birthdate <-
    formatC(
      teacher$birthdate,
      dig = 0,
      wid = 7,
      format = "f",
      flag = "0"
    )
  teacher$onbodat <-
    formatC(
      teacher$onbodat,
      dig = 0,
      wid = 7,
      format = "f",
      flag = "0"
    )
  teacher$desedym <-
    formatC(
      teacher$desedym,
      dig = 0,
      wid = 4,
      format = "f",
      flag = "0"
    )
  teacher$beobdym <-
    formatC(
      teacher$beobdym,
      dig = 0,
      wid = 4,
      format = "f",
      flag = "0"
    )
  teacher$organization_id <-
    formatC(
      teacher$organization_id,
      dig = 0,
      wid = 6,
      format = "f",
      flag = "0"
    )
  
  #只留下審核通過之名單
  teacher <-
    merge(x = teacher,
          y = list_agree,
          by = "organization_id",
          all.x = TRUE) %>%
    subset(agree == 1) %>%
    subset(select = -c(updated_at, agree))
  
  #讀取職員(工)資料表名稱
  staff_tablename <- dbGetQuery(
    edhr,
    paste(
      "
SELECT [name] FROM [plat5_edhr].[dbo].[row_tables]
	where sheet_id = (SELECT [id] FROM [plat5_edhr].[dbo].[row_sheets]
						          where file_id = (SELECT field_component_id FROM [plat5_edhr].[dbo].[teacher_datasets]
											                   WHERE title = '職員(工)資料表' AND department_id = (SELECT id FROM [plat5_edhr].[dbo].[teacher_departments]
																							                                                 WHERE title = '",
      department,
      "' AND  report_id = (SELECT id FROM [plat5_edhr].[dbo].[teacher_reports]
			WHERE title = '",
      title,
      "'))))",
      sep = ""
    )
  ) %>% as.character()
  
  #職員(工)資料表尚未建立的判斷
  if (staff_tablename == "character(0)") {
    stop("The staff table has not been created yet, so data validation will not be executed.")
  } else{
    #讀取職員(工)資料表
    staff <- dbGetQuery(
      edhr,
      paste(
        "SELECT * FROM [rows].[dbo].[",
        staff_tablename,
        "] WHERE deleted_at IS NULL",
        sep = ""
      )
    ) %>%
      subset(select = -c(
        id,
        created_at,
        deleted_at,
        updated_by,
        created_by,
        deleted_by
      ))
    #欄位名稱更改為設定的欄位代號
    for (i in 2:dim(staff)[2])
      #從2開始是因為第一的欄位是update_at
    {
      colnames(staff)[i] <-
        col_names$name[grep(paste(colnames(staff)[i], "$", sep = ""), col_names$id)]
    }
    
    #格式調整
    staff$gender <-
      formatC(
        staff$gender,
        dig = 0,
        wid = 1,
        format = "f",
        flag = "0"
      )
    staff$birthdate <-
      formatC(
        staff$birthdate,
        dig = 0,
        wid = 7,
        format = "f",
        flag = "0"
      )
    staff$onbodat <-
      formatC(
        staff$onbodat,
        dig = 0,
        wid = 7,
        format = "f",
        flag = "0"
      )
    staff$desedym <-
      formatC(
        staff$desedym,
        dig = 0,
        wid = 4,
        format = "f",
        flag = "0"
      )
    staff$beobdym <-
      formatC(
        staff$beobdym,
        dig = 0,
        wid = 4,
        format = "f",
        flag = "0"
      )
    staff$organization_id <-
      formatC(
        staff$organization_id,
        dig = 0,
        wid = 6,
        format = "f",
        flag = "0"
      )
    
    #只留下審核通過之名單
    staff <-
      merge(
        x = staff,
        y = list_agree,
        by = "organization_id",
        all.x = TRUE
      ) %>%
      subset(agree == 1) %>%
      subset(select = -c(updated_at, agree))
    
    #讀取離退教職員(工)資料表名稱
    retire_tablename <- dbGetQuery(
      edhr,
      paste(
        "
SELECT [name] FROM [plat5_edhr].[dbo].[row_tables]
	where sheet_id = (SELECT [id] FROM [plat5_edhr].[dbo].[row_sheets]
						          where file_id = (SELECT field_component_id FROM [plat5_edhr].[dbo].[teacher_datasets]
											                   WHERE title = '離退教職員(工)資料表' AND department_id = (SELECT id FROM [plat5_edhr].[dbo].[teacher_departments]
																							                                                 WHERE title = '",
        department,
        "' AND  report_id = (SELECT id FROM [plat5_edhr].[dbo].[teacher_reports]
																												                                                            WHERE title = '",
        title,
        "'))))",
        sep = ""
      )
    ) %>% as.character()
    
    #離退教職員(工)資料表尚未建立的判斷
    if (retire_tablename == "character(0)") {
      stop("The retire table has not been created yet, so data validation will not be executed.")
    } else{
      #讀取離退教職員(工)資料表
      retire <- dbGetQuery(
        edhr,
        paste(
          "SELECT * FROM [rows].[dbo].[",
          retire_tablename,
          "] WHERE deleted_at IS NULL",
          sep = ""
        )
      ) %>%
        subset(select = -c(
          id,
          created_at,
          deleted_at,
          updated_by,
          created_by,
          deleted_by
        ))
      #欄位名稱更改為設定的欄位代號
      for (i in 2:dim(retire)[2])
        #從2開始是因為第一的欄位是update_at
      {
        colnames(retire)[i] <-
          col_names$name[grep(paste(colnames(retire)[i], "$", sep = ""), col_names$id)]
      }
      
      #格式調整
      retire$organization_id <-
        formatC(
          retire$organization_id,
          dig = 0,
          wid = 6,
          format = "f",
          flag = "0"
        )
      
      #只留下審核通過之名單
      retire <-
        merge(
          x = retire,
          y = list_agree,
          by = "organization_id",
          all.x = TRUE
        ) %>%
        subset(agree == 1) %>%
        subset(select = -c(updated_at, agree))
      
      #讀取教學資料表名稱
      department <- "教務處"
      
      load_tablename <- dbGetQuery(
        edhr,
        paste(
          "
SELECT [name] FROM [plat5_edhr].[dbo].[row_tables]
	where sheet_id = (SELECT [id] FROM [plat5_edhr].[dbo].[row_sheets]
						          where file_id = (SELECT field_component_id FROM [plat5_edhr].[dbo].[teacher_datasets]
											                   WHERE title = '教學資料表' AND department_id = (SELECT id FROM [plat5_edhr].[dbo].[teacher_departments]
																							                                                 WHERE title = '",
          department,
          "' AND  report_id = (SELECT id FROM [plat5_edhr].[dbo].[teacher_reports]
			WHERE title = '",
          title,
          "'))))",
          sep = ""
        )
      ) %>% as.character()
      
      #教學資料表尚未建立的判斷
      if (load_tablename == "character(0)") {
        stop("The load table has not been created yet, so data validation will not be executed.")
      } else{
        #讀取教學資料表
        load <- dbGetQuery(
          edhr,
          paste(
            "SELECT * FROM [rows].[dbo].[",
            load_tablename,
            "] WHERE deleted_at IS NULL",
            sep = ""
          )
        ) %>%
          subset(select = -c(
            id,
            created_at,
            deleted_at,
            updated_by,
            created_by,
            deleted_by
          ))
        #欄位名稱更改為設定的欄位代號
        for (i in 2:dim(load)[2])
          #從2開始是因為第一的欄位是update_at
        {
          colnames(load)[i] <-
            col_names$name[grep(paste(colnames(load)[i], "$", sep = ""), col_names$id)]
        }
        
        #格式調整
        load$basic <-
          formatC(
            load$basic,
            dig = 0,
            wid = 2,
            format = "f",
            flag = "0"
          )
        load$cut <-
          formatC(
            load$cut,
            dig = 0,
            wid = 2,
            format = "f",
            flag = "0"
          )
        load$hschftime <-
          load$hschftime %>% as.numeric()
        load$hschcftime <-
          load$hschcftime %>% as.numeric()
        load$hschptime <-
          load$hschptime %>% as.numeric()
        load$jhschftime <-
          load$jhschftime %>% as.numeric()
        load$othertime <-
          formatC(
            load$othertime,
            dig = 0,
            wid = 2,
            format = "f",
            flag = "0"
          )
        load$organization_id <-
          formatC(
            load$organization_id,
            dig = 0,
            wid = 6,
            format = "f",
            flag = "0"
          )
        
        #只留下審核通過之名單
        load <-
          merge(
            x = load,
            y = list_agree,
            by = "organization_id",
            all.x = TRUE
          ) %>%
          subset(agree == 1) %>%
          subset(select = -c(updated_at, agree))
        
        data_teacher <- teacher
        data_staff   <- staff
        data_retire   <- retire
        data_load    <- load
        #data_courseA  <- read_excel(path, sheet = "授課資料表A.有課程代碼（23碼）")
        #data_courseB  <- read_excel(path, sheet = "授課資料表B.無課程代碼（23碼）")
        
        # 匯入上一期人事資料檔 -------------------------------------------------------------------
        # 1121全國學校 教員資料表
        #請輸入本次填報設定檔標題(字串需與標題完全相符，否則會找不到)
        title_pre <- "112學年度上學期高級中等學校教育人力資源資料庫（全國學校人事）"
        
        department_pre <- "人事室"
        
        #讀取審核同意之學校名單
        list_agree_pre <- dbGetQuery(
          edhr,
          paste(
            "
SELECT DISTINCT b.id AS organization_id , 1 AS agree
FROM [plat5_edhr].[dbo].[teacher_fillers] a
LEFT JOIN
(SELECT a.reporter_id, c.id
FROM [plat5_edhr].[dbo].[teacher_fillers] a LEFT JOIN [plat5_edhr].[dbo].[teacher_reporters] b ON a.reporter_id = b.id
LEFT JOIN [plat5_edhr].[dbo].[organization_details] c ON b.organization_id = c.organization_id
) b ON a.reporter_id = b.reporter_id
WHERE a.agree = 1 AND department_id IN (SELECT id FROM [plat5_edhr].[dbo].[teacher_departments]
                                        WHERE report_id = (SELECT id FROM [plat5_edhr].[dbo].[teacher_reports]
                                                            WHERE title = '",
            title_pre,
            "'))",
            sep = ""
          )
        ) %>%
          distinct(organization_id, .keep_all = TRUE)
        
        #讀取教員資料表名稱
        teacher_tablename_pre <- dbGetQuery(
          edhr,
          paste(
            "
SELECT [name] FROM [plat5_edhr].[dbo].[row_tables]
	where sheet_id = (SELECT [id] FROM [plat5_edhr].[dbo].[row_sheets]
						          where file_id = (SELECT field_component_id FROM [plat5_edhr].[dbo].[teacher_datasets]
											                  WHERE title = '教員資料表' AND department_id = (SELECT id FROM [plat5_edhr].[dbo].[teacher_departments]
																						                                              WHERE title = '",
            department_pre,
            "' AND  report_id = (SELECT id FROM [plat5_edhr].[dbo].[teacher_reports]
																												                                                                                      WHERE title = '",
            title_pre,
            "'))))",
            sep = ""
          )
        ) %>% as.character()
        #讀取教員資料表
        teacher_pre <- dbGetQuery(
          edhr,
          paste(
            "SELECT * FROM [rows].[dbo].[",
            teacher_tablename_pre,
            "] WHERE deleted_at IS NULL",
            sep = ""
          )
        ) %>%
          subset(select = -c(
            id,
            created_at,
            deleted_at,
            updated_by,
            created_by,
            deleted_by
          ))
        
        #欄位名稱更改為設定的欄位代號
        col_names_pre <-
          dbGetQuery(edhr,
                     "SELECT id, name, title FROM [plat5_edhr].[dbo].[row_columns]")
        col_names_pre$id <- paste("C", col_names_pre$id, sep = "")
        for (i in 2:dim(teacher_pre)[2])
          #從2開始是因為第一的欄位是update_at
        {
          colnames(teacher_pre)[i] <-
            col_names_pre$name[grep(paste(colnames(teacher_pre)[i], "$", sep = ""), col_names_pre$id)]
        }
        #格式調整
        teacher_pre$gender <-
          formatC(
            teacher_pre$gender,
            dig = 0,
            wid = 1,
            format = "f",
            flag = "0"
          )
        teacher_pre$birthdate <-
          formatC(
            teacher_pre$birthdate,
            dig = 0,
            wid = 7,
            format = "f",
            flag = "0"
          )
        teacher_pre$onbodat <-
          formatC(
            teacher_pre$onbodat,
            dig = 0,
            wid = 7,
            format = "f",
            flag = "0"
          )
        teacher_pre$desedym <-
          formatC(
            teacher_pre$desedym,
            dig = 0,
            wid = 4,
            format = "f",
            flag = "0"
          )
        teacher_pre$beobdym <-
          formatC(
            teacher_pre$beobdym,
            dig = 0,
            wid = 4,
            format = "f",
            flag = "0"
          )
        teacher_pre$organization_id <-
          formatC(
            teacher_pre$organization_id,
            dig = 0,
            wid = 6,
            format = "f",
            flag = "0"
          )
        
        #只留下審核通過之名單
        teacher_pre <-
          merge(
            x = teacher_pre,
            y = list_agree_pre,
            by = "organization_id",
            all.x = TRUE
          ) %>%
          subset(agree == 1) %>%
          subset(select = -c(updated_at, agree))
        
        teacher_pre <- teacher_pre %>%
          mutate(dta_teacher = "教員資料表")
        
        # 1121全國學校 職員(工)資料表
        #讀取職員(工)資料表名稱
        staff_tablename_pre <- dbGetQuery(
          edhr,
          paste(
            "
SELECT [name] FROM [plat5_edhr].[dbo].[row_tables]
	where sheet_id = (SELECT [id] FROM [plat5_edhr].[dbo].[row_sheets]
						          where file_id = (SELECT field_component_id FROM [plat5_edhr].[dbo].[teacher_datasets]
											                   WHERE title = '職員(工)資料表' AND department_id = (SELECT id FROM [plat5_edhr].[dbo].[teacher_departments]
																							                                                 WHERE title = '",
            department_pre,
            "' AND  report_id = (SELECT id FROM [plat5_edhr].[dbo].[teacher_reports]
																												                                                            WHERE title = '",
            title_pre,
            "'))))",
            sep = ""
          )
        ) %>% as.character()
        #讀取職員(工)資料表
        staff_pre <- dbGetQuery(
          edhr,
          paste(
            "SELECT * FROM [rows].[dbo].[",
            staff_tablename_pre,
            "] WHERE deleted_at IS NULL",
            sep = ""
          )
        ) %>%
          subset(select = -c(
            id,
            created_at,
            deleted_at,
            updated_by,
            created_by,
            deleted_by
          ))
        #欄位名稱更改為設定的欄位代號
        for (i in 2:dim(staff_pre)[2])
          #從2開始是因為第一的欄位是update_at
        {
          colnames(staff_pre)[i] <-
            col_names_pre$name[grep(paste(colnames(staff_pre)[i], "$", sep = ""), col_names_pre$id)]
        }
        
        #格式調整
        staff_pre$gender <-
          formatC(
            staff_pre$gender,
            dig = 0,
            wid = 1,
            format = "f",
            flag = "0"
          )
        staff_pre$birthdate <-
          formatC(
            staff_pre$birthdate,
            dig = 0,
            wid = 7,
            format = "f",
            flag = "0"
          )
        staff_pre$onbodat <-
          formatC(
            staff_pre$onbodat,
            dig = 0,
            wid = 7,
            format = "f",
            flag = "0"
          )
        staff_pre$desedym <-
          formatC(
            staff_pre$desedym,
            dig = 0,
            wid = 4,
            format = "f",
            flag = "0"
          )
        staff_pre$beobdym <-
          formatC(
            staff_pre$beobdym,
            dig = 0,
            wid = 4,
            format = "f",
            flag = "0"
          )
        staff_pre$organization_id <-
          formatC(
            staff_pre$organization_id,
            dig = 0,
            wid = 6,
            format = "f",
            flag = "0"
          )
        
        #只留下審核通過之名單
        staff_pre <-
          merge(
            x = staff_pre,
            y = list_agree_pre,
            by = "organization_id",
            all.x = TRUE
          ) %>%
          subset(agree == 1) %>%
          subset(select = -c(updated_at, agree))
        
        staff_pre <- staff_pre %>%
          mutate(dta_teacher = "職員(工)資料表")
        #####合併#####
        drev_person_pre <-
          bind_rows(teacher_pre, staff_pre)
        
        #檢查本次是否有新資料，若否，則不往下執行
        data_teacher_check <- data_teacher %>%
          mutate(count = 1)
        data_teacher_check <-
          aggregate(count ~ organization_id, data_teacher_check, sum)
        data_teacher_check_save <- data_teacher_check #讀上次檔案之後再存
        #讀上次的上傳資料結果
        if (file.exists("./data_teacher_check_pre.xlsx")) {
          data_teacher_check_pre <-
            readxl::read_excel("./data_teacher_check_pre.xlsx")
        } else{
          
        }
        #存本次的上傳資料結果，方便下次比對
        openxlsx::write.xlsx(
          data_teacher_check_save,
          file = "./data_teacher_check_pre.xlsx",
          rowNames = FALSE,
          overwrite = TRUE
        )
        if (exists("data_teacher_check_pre")) {
          data_teacher_check <-
            left_join(data_teacher_check, data_teacher_check_pre, by = "organization_id") %>%
            rename(count = count.x, count_pre = count.y)
        } else{
          
        }
        
        data_staff_check <- data_staff %>%
          mutate(count = 1)
        data_staff_check <-
          aggregate(count ~ organization_id, data_staff_check, sum)
        data_staff_check_save <- data_staff_check #讀上次檔案之後再存
        #讀上次的上傳資料結果
        if (file.exists("./data_staff_check_pre.xlsx")) {
          data_staff_check_pre <-
            readxl::read_excel("./data_staff_check_pre.xlsx")
        } else{
          
        }
        #存本次的上傳資料結果，方便下次比對
        openxlsx::write.xlsx(
          data_staff_check_save,
          file = "./data_staff_check_pre.xlsx",
          rowNames = FALSE,
          overwrite = TRUE
        )
        if (exists("data_staff_check_pre")) {
          data_staff_check <-
            left_join(data_staff_check, data_staff_check_pre, by = "organization_id") %>%
            rename(count = count.x, count_pre = count.y)
        } else{
          
        }
        
        data_retire_check <- data_retire %>%
          mutate(count = 1)
        data_retire_check <-
          aggregate(count ~ organization_id, data_retire_check, sum)
        data_retire_check_save <- data_retire_check #讀上次檔案之後再存
        #讀上次的上傳資料結果
        if (file.exists("./data_retire_check_pre.xlsx")) {
          data_retire_check_pre <-
            readxl::read_excel("./data_retire_check_pre.xlsx")
        } else{
          
        }
        #存本次的上傳資料結果，方便下次比對
        openxlsx::write.xlsx(
          data_retire_check_save,
          file = "./data_retire_check_pre.xlsx",
          rowNames = FALSE,
          overwrite = TRUE
        )
        if (exists("data_retire_check_pre")) {
          data_retire_check <-
            left_join(data_retire_check, data_retire_check_pre, by = "organization_id") %>%
            rename(count = count.x, count_pre = count.y)
        } else{
          
        }
        
        #如果count及count_pre有值且count = count_pre代表沒有新資料，如果count有值且count_pre為NA則有新資料
        
        #count_pre為NA的處理
        #count_pre若不存在，建立0
        if (!"count_pre" %in% colnames(data_teacher_check)) {
          data_teacher_check$count_pre <- 0
        }
        if (!"count_pre" %in% colnames(data_staff_check)) {
          data_staff_check$count_pre <- 0
        }
        if (!"count_pre" %in% colnames(data_retire_check)) {
          data_retire_check$count_pre <- 0
        }
        
        data_teacher_check$count_pre[is.na(data_teacher_check$count_pre)] <-
          0
        data_staff_check$count_pre[is.na(data_staff_check$count_pre)] <-
          0
        data_retire_check$count_pre[is.na(data_retire_check$count_pre)] <-
          0
        
        if (all(data_teacher_check$count == data_teacher_check$count_pre) &
            all(data_staff_check$count == data_staff_check$count_pre) &
            all(data_retire_check$count == data_retire_check$count_pre) &
            length(data_teacher_check$count_pre) > 0) {
          stop("There is no new data for this time, so data validation will not be executed.")
        } else{
          # 合併人事資料表 ----------------------------------------------------------------
          data_teacher <- data_teacher %>%
            mutate(source = 1)
          
          data_staff <- data_staff %>%
            mutate(source = 2)
          
          drev_person <- bind_rows(data_teacher, data_staff)
          
          drev_person$source  <-
            factor(
              drev_person$source,
              levels = c(1, 2),
              labels = c("教員資料表", "職員(工)資料表")
            )
          #這行在更改source的1和2為教員資料表及職員工資料表，levels是排序依據.
          
          #學校科別資料[每學年1月底更新]
          source("edhr-schooltype_112.R")
          
          #更改人事資料表的學校名稱（若學校名單異動，哪幾間學校名稱需簡寫亦要檢視） -------------------------------------------------------------------
          organization <-
            dbGetQuery(
              edhr,
              "SELECT a.id as organization_id, b.name as edu_name2
  FROM
  (SELECT id, max(year) as year
  FROM [plat5_edhr].[dbo].[organization_details]
  group by id) a LEFT JOIN (SELECT id, year, name
							FROM [plat5_edhr].[dbo].[organization_details]) b ON a.id = b.id AND a.year = b.year
  where (substring(a.id, 4, 1) = '1' OR substring(a.id, 4, 1) = '2' OR substring(a.id, 4, 1) = '3' OR substring(a.id, 4, 1) = '4' OR substring(a.id, 4, 1) = 'B' OR substring(a.id, 4, 1) = 'C' OR substring(a.id, 4, 1) = 'F' OR substring(a.id, 4, 1) = 'G') and len(a.id) = 6"
            )
          drev_person <- drev_person %>%
            left_join(organization, by = "organization_id")
          
          # 人事資料表合併學校科別資訊 ----------------------------------------------------------------
          drev_person_1 <- data_schtype_wide %>%
            select(organization_id,
                   typeC,
                   typeD,
                   typeM,
                   typeH,
                   typeV,
                   typeJ,
                   typeE,
                   typeF) %>%
            distinct(organization_id,
                     typeC,
                     typeD,
                     typeM,
                     typeH,
                     typeV,
                     typeJ,
                     typeE,
                     typeF) %>%
            merge(x = drev_person,
                  by = "organization_id",
                  all.x = TRUE) %>%
            distinct()
          
          # 人事資料表資料格式修正 ------------------------------------------------------------------
          source("edhr-dataclean_format-personnel.R")
          
          # 資料表合併
          source("edhr-merge.R")
          
          # 需要每個學期重新調整的項目 -----------------------------------------------------------
          
          ### flag_person
          # flag2、flag3；spe2、spe6。
          # 以上檢查項目依最新的學校名單、群科開設狀況而定。相關資訊可上統計處查詢高級中等學校科別資料。
          # flag6需檢查各表姓名是否為純中文或純英文，或者是否夾雜其他運算字元、特殊符號。
          # flag8需檢查持外來人口統一證號的教職員(工)是否有填其國籍別，又其國籍別是否足以辨認。
          # flag9需檢查最高學歷畢業學校國別（一）(schooln1)所填之國籍別是否足以辨認。
          # flag_person <- drev_person_1 %>%
          #   mutate(err_flag_2 = if_else((organization_id == "011315" | organization_id == "013430" | organization_id == "110409" | organization_id == "193404" | organization_id == "381305" | organization_id == "533402"), 1, 0),
          #          err_flag_3 = if_else(organization_id == "110302", 1, 0),
          #          err_spe_2  = if_else(typeD == 1 & empunit != "雙語部" & source == "教員資料表", 1, 0),
          #          err_spe_6  = if_else(typeJ == 1, 1, 0),
          #          err_flag_6 = if_else(name == "吳淑貞-", 1, 0),
          #          err_flag_8 = if_else(nation == "外籍", 1, 0),
          #          err_flag_9 = 0)
          # 目前僅限於人事資料表範圍內的檢查項目暫無需檢查是否擔任「科主任」、「學程主任」，
          # 因此在備分的檔案(flag_person)先把這兩個職稱拿掉。若往後有此需要，請再另行處理。
          # temp <- seq(from = 18, to = 25 , by = 2)
          # for (x in temp){
          #   flag_person[grep("$科主任", flag_person[x]), ] <- flag_person %>%
          #     slice(grep("$科主任", flag_person[x])) %>%
          #     mutate(err_flag_2, recode(err_flag_2, "1 = 0"))
          #
          #   flag_person[grep("$學程主任", flag_person[x]), ] <- flag_person %>%
          #     slice(grep("$學程主任", flag_person[x])) %>%
          #     mutate(err_flag_3, recode(err_flag_3, "1 = 0"))
          # }
          
          #人事
          source("flag1.R")
          source("flag2.R")
          source("flag3.R")
          source("flag6.R")
          source("flag7.R")
          source("flag8.R")
          source("flag9.R")
          source("flag15.R")
          source("flag16.R")
          source("flag18.R")
          source("flag19.R")
          source("flag20.R")
          source("flag24.R")
          source("flag39.R")
          source("flag45.R")
          source("flag47.R")
          source("flag48.R")
          source("flag49.R")
          source("flag50.R")
          source("flag51.R")
          source("flag52.R")
          source("flag57.R")
          source("flag59.R")
          source("flag62.R")
          source("flag64.R")
          source("flag80.R")
          source("flag82.R")
          source("flag89.R")
          source("flag90.R")
          source("flag94.R")
          source("spe3.R")
          source("spe5.R")
          source("spe6.R")
          source("flag83.R")
          source("flag92.R")
          source("flag84.R")
          source("flag85.R")
          source("flag93.R")
          source("flag86.R")
          source("flag91.R")
          source("flag95.R")
          source("flag96.R")
          source("flag97.R")
          source("flag98.R")
          source("flag99.R")
          source("flag100.R")
          
          #教務
          source("flag25.R")
          source("flag26.R")
          source("flag29.R")
          source("flag31.R")
          source("flag56.R")
          source("flag58.R")
          source("flag61.R")
          source("flag67.R")
          source("flag68.R")
          source("flag69.R")
          source("flag70.R")
          source("flag74.R")
          source("flag75.R")
          source("flag101.R")
          source("spe4.R")
          
          
          # 建立合併列印檔 -------------------------------------------------------------------
          source("edhr-check02.R")
          
        } #本次無新資料的判斷
        
        
      } #教學資料表尚未建立的判斷
      
    } #離退教職員(工)資料表尚未建立的判斷
    
  } #職員(工)資料表尚未建立的判斷
  
} #教員資料表尚未建立的判斷
