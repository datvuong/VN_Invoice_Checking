LoadInvoiceData <- function(invoicePath) {
  suppressMessages({
    require(readr)
    require(dplyr)
    require(tools)
    require(magrittr)
    require(methods)
    require(XLConnect)
    require(futile.logger)
  })
  
  functionName <- "LoadInvoiceData"
  flog.info(paste("Function", functionName, "started"), name = reportName)
  
  output <- tryCatch({
    
    excelFiles <- list.files(invoicePath)
    excelFiles <- excelFiles[grepl("^[^~\\$].*\\.(xls|xlsx|csv)$", excelFiles)]
    invoiceData <- NULL
    colNames <- c("line_id", "3pl_name", "package_pickup_date",
                  "package_pod_date", "invoice_number", "package_number",
                  "tracking_number", "tracking_number_rts", "order_number",
                  "package_volume", "package_height", "package_width",
                  "package_length", "package_weight", "package_chargeable_weight",
                  "carrying_fee", "redelivery_fee", "rejection_fee",
                  "cod_fee", "special_area_fee", "special_handling_fee",
                  "insurance_fee", "vat", "origin_branch",
                  "destination_branch", "delivery_zone_zip_code", "rate_type",
                  "cod_amount", "isRural", "isCrossCheck", 
                  "isVUN", "VUN_sur", "status", "isBulky")
    fullData <- NULL
    for (ifile in excelFiles) {
      if (file_ext(ifile) %in% c("xls", "xlsx")) {
        wb <- loadWorkbook(file.path(invoicePath, ifile))
        invoiceFileData <- readWorksheet(wb, 1, colTypes = c(XLC$DATA_TYPE.STRING, XLC$DATA_TYPE.STRING, XLC$DATA_TYPE.STRING,
                                                             XLC$DATA_TYPE.STRING, XLC$DATA_TYPE.STRING, XLC$DATA_TYPE.STRING,
                                                             XLC$DATA_TYPE.STRING, XLC$DATA_TYPE.STRING, XLC$DATA_TYPE.STRING,
                                                             XLC$DATA_TYPE.NUMERIC, XLC$DATA_TYPE.NUMERIC, XLC$DATA_TYPE.NUMERIC,
                                                             XLC$DATA_TYPE.NUMERIC, XLC$DATA_TYPE.NUMERIC, XLC$DATA_TYPE.NUMERIC,
                                                             XLC$DATA_TYPE.NUMERIC, XLC$DATA_TYPE.NUMERIC, XLC$DATA_TYPE.NUMERIC,
                                                             XLC$DATA_TYPE.NUMERIC, XLC$DATA_TYPE.NUMERIC, XLC$DATA_TYPE.NUMERIC,
                                                             XLC$DATA_TYPE.NUMERIC, XLC$DATA_TYPE.NUMERIC, XLC$DATA_TYPE.STRING,
                                                             XLC$DATA_TYPE.STRING, XLC$DATA_TYPE.STRING, XLC$DATA_TYPE.STRING,
                                                             XLC$DATA_TYPE.NUMERIC, XLC$DATA_TYPE.NUMERIC, XLC$DATA_TYPE.NUMERIC,
                                                             XLC$DATA_TYPE.NUMERIC, XLC$DATA_TYPE.NUMERIC, XLC$DATA_TYPE.STRING, XLC$DATA_TYPE.NUMERIC))
        names(invoiceFileData) <- colNames
      } else {
        invoiceFileData <- read_csv(file = file.path(invoicePath, ifile),
                                    col_names = colNames, skip = 1, 
                                    col_types = list(col_character(), col_character(), col_character(),
                                                  col_character(), col_character(), col_character(),
                                                  col_character(), col_character(), col_character(),
                                                  col_number(), col_number(), col_number(),
                                                  col_number(), col_number(), col_number(),
                                                  col_number(), col_number(), col_number(),
                                                  col_number(), col_number(), col_number(),
                                                  col_number(), col_number(), col_character(),
                                                  col_character(), col_character(), col_character(),
                                                  col_number(), col_number(), col_number(),
                                                  col_number(), col_number(), col_character(), col_number()))
      }
      
      invoiceFileData %<>%
        mutate(rawFile = ifile)
      
      if (is.null(fullData)) {
        fullData <- invoiceFileData
      } else {
        fullData <- rbind_list(fullData, invoiceFileData)
      }
      
      wb <- NULL
      gc()
    }
    
    fullData
    
  }, error = function(err) {
    flog.error(paste(functionName, err, sep = " - "), name = reportName)
  }, finally = {
    flog.info(paste(functionName, "ended"), name = reportName)
  })
  
  output
}


