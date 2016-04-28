source("03_Script/01_Code/00_init.R")

tryCatch({
  reportName <- paste0("VNInvoiceCheck")
  warningLog <- paste0("VNInvoiceCheck", "warning")
  flog.appender(appender.tee(file.path("03_Script/02_Log", paste0("VN_InvoiceChecking_HCMP.csv"))),
                name = reportName)
  
  layout <- layout.format(paste0(timeReport,'|[~l]|[~t]|[~n.~f]|~m'))
  flog.layout(layout, name = reportName)
  
  flog.info("Initial Setup", name = reportName)
  
  source("03_Script/01_Code/01_Loading/Load_Invoice_Data.R")
  
  load("01_Input/RData/packageDataBased.RData")
  invoiceData <- LoadInvoiceData("01_Input/HCMP/01_Invoice/")
  paidInvoiceData <- LoadInvoiceData("01_Input/HCMP/03_Paid_Invoice/")
  
  gc()
  mergedOMSData <- left_join(invoiceData,
                             packageDataBased,
                             by = "package_number")
  rm(packageDataBased)
  gc()
  mergedOMSData %<>%
    mutate(origin_branch = toupper(trimws(origin_branch))) %>%
    mutate(destination_branch = toupper(trimws(destination_branch))) %>%
    mutate(tracking_number = ifelse(is.na(tracking_number.y), tracking_number.x, tracking_number.y)) %>%
    mutate(codAmount = (paidPrice + shippingFee + shippingSurcharge)) %>%
    mutate(existence_flag = ifelse(!is.na(order_nr), "OKAY", "NOT_OKAY")) %>%
    select(-c(tracking_number.x, tracking_number.y))
  
  paidInvoice <- NULL
  paidInvoiceList <- NULL
  
  if (!is.null(paidInvoiceData)) {
    paidInvoice <- paidInvoiceData$tracking_number
    paidInvoiceList <- select(paidInvoiceData, tracking_number, rawFile)
    paidInvoiceList <- paidInvoiceList %>%
      filter(!duplicated(tracking_number))
    row.names(paidInvoiceList) <- paidInvoiceList$tracking_number
  }
  
  # mapping ratecard
  source("03_Script/01_Code/02_Processing/HCMP/HCMP_MapRateCard.R")
  mergedOMSData_rate <- MapRateCard(mergedOMSData, "01_Input/HCMP/04_Ratecards/")

  mergedOMSData_rate %<>%
    mutate(total_carrying_fee_lazvat = total_carrying_fee_laz * 1.1) %>%
    mutate(carrying_fee_flag = ifelse(carrying_fee - total_carrying_fee_lazvat > 1000 , "NOT_OKAY", "OKAY"))
#     mutate(COD_fee = ifelse(payment_method == "CashOnDelivery",
#                                    0.01 * (paidPrice + shippingFee + shippingSurcharge), 0)) %>%
#     mutate(InsuranceFee_Flag=ifelse(insurance_fee - InsuranceFee_Calculate < 1,"Okay","Not-Okay")) %>%
#     mutate(COD_fee_flag=ifelse(cod_fee - COD_calculated < 1,"Okay","Not-Okay"))
#     mutate(Duplication_Flag=ifelse(duplicated(tracking_number),"Duplicated",
#                                    ifelse(tracking_number %in% paidInvoice,
#                                           "Duplicated","Not_Duplicated"))) %>%
#     mutate(DuplicationSource=ifelse(duplicated(tracking_number),"Self_Duplicated",
#                                     ifelse(tracking_number %in% paidInvoice,
#                                            paidInvoiceList[tracking_number,]$InvoiceFile,"")))
  
  flog.info("Done Invoice Calculation", name = reportName)
  
  mergedOMSData_final <- mergedOMSData_rate %>%
    select(-c( #level_4_code, level_4_customer_address_region_type, level_4_fk_customer_address_region,
              level_3_code, level_3_customer_address_region_type, level_3_fk_customer_address_region,
              level_2_code, level_2_customer_address_region_type, level_2_fk_customer_address_region))
  save(mergedOMSData_final, file = "03_Script/03_RData/HCMP/mergedOMSData_final.RData",
       compress = TRUE)
  flog.info("Writting Output File in CSV Format", name = reportName)
  
  invoiceFiles <- unique(mergedOMSData_final$rawFile)
  for (iFile in invoiceFiles) {
    fileName <- gsub(".xls.*$", "_checked.csv", iFile)
    fileData <-  as.data.frame(mergedOMSData_final %>% filter(rawFile == iFile))
    write.csv(fileData, file.path("02_Output/HCMP", fileName),
               row.names = FALSE)
  }
  
  flog.info("Done", name = reportName)
  
},error = function(err){
  flog.error(err, name = reportName)
  flog.error("PLease send 03_Script/Log folder to Regional OPS BI for additional support",
             name = reportName)
})
