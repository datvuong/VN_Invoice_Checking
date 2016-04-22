MapRateCard <- function(mergedOMSData, rateCardFilePath) {
  suppressMessages({
    require(dplyr)
    require(tools)
    require(magrittr)
    require(methods)
    require(futile.logger)
    require(XLConnect)
  })
  
  functionName <- "MapRateCard"
  flog.info(paste("Function", functionName, "started"), name = reportName)
  
  output <- tryCatch({
    
    wb <- loadWorkbook(file.path(rateCardFilePath,"destination_tree.xlsx"))  
    destinationTree <- readWorksheet(object = wb, sheet = 1)
    wb <- loadWorkbook(file.path(rateCardFilePath,"correct_destination.xlsx"))  
    destinationCorrect <- readWorksheet(object = wb, sheet = 1)
    region_matrix <- read.csv(file.path(rateCardFilePath,"region_matrix.csv"), quote = '"', sep=",", row.names = NULL,
                         col.names = c("type", "from", "to", "region", "shippingFeeAdjust" ), 
                         colClasses = c("character", "numeric","numeric", "character", "numeric"))
    rateCard <- read.csv(file.path(rateCardFilePath,"hcmpost_ratecards.csv"), quote = '"', sep=",", row.names = NULL,
                              col.names = c("type", "codRange", "region", 
                                            "w000002", "w002005", "w005010", "w010025", 
                                            "w025050", "w050100", "w100150", "w150200", 
                                            "w200250", "w250300", "w300350", "w350400", 
                                            "w400450", "w450500", "w500550", "w550600", 
                                            "w050100plus" ), 
                              colClasses = c("character", "character", "character",  
                                             "numeric", "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", "numeric", 
                                             "numeric"))
    surcharge <- read.csv(file.path(rateCardFilePath,"hcmpost_surcharge.csv"), quote = '"', sep=",", row.names = NULL,
                         col.names = c("type", "region", "bulky_sur", "fuel_sur", 
                                       "rural_sur_rate", "rural_sur_500999", "super_rural", "VUN",	
                                       "cross_checking", "pickup_sur", "failed_delivery_fee"), 
                         colClasses = c("character", "character", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric"))
    
    destinationTreeRev <- destinationTree %>%
      mutate(destination = toupper(trimws(destination)))
    destinationTreeRev <- filter(destinationTreeRev, !duplicated(destinationTreeRev))
    
    destinationCorrectRev <- destinationCorrect %>%
      mutate(destination_3pl = toupper(trimws(destination_3pl))) %>%
      mutate(destination_rev = toupper(trimws(destination_rev)))
    destinationCorrectRev <- filter(destinationCorrectRev, !duplicated(destinationCorrectRev))
    
    mergedOMSDataRev <- mergedOMSData %>%
      mutate(level_2_name = toupper(level_2_name)) %>%
      mutate(rateType = toupper(trimws(line_id))) %>%
      mutate(codRange = ifelse(rateType == "ECONOMY", 
                               ifelse(codAmount < 150000, "cod000150",
                               ifelse(codAmount < 300000, "cod150300", "cod300999")),
                               "noneco"))
    mergedOMSDataRev <- left_join(mergedOMSDataRev, destinationCorrectRev, by = c("destination_branch" = "destination_3pl"))
    mappedRateCard <- left_join(mergedOMSDataRev, 
                                destinationTreeRev,
                                by = c("origin_branch" = "destination"))
    mappedRateCard %<>% 
      mutate(id_origin = ifelse(rateType == "ECONOMY", id_region_eco, id_region)) %>%
      select(-c(id_region, id_region_eco))
    
    mappedRateCard <- left_join(mappedRateCard, 
                                destinationTreeRev,
                                by = c("destination_rev" = "destination")) # "level_2_name" = "destination"
    mappedRateCard %<>% 
      mutate(id_destination = ifelse(rateType == "ECONOMY", id_region_eco, id_region)) %>%
      select(-c(id_region, id_region_eco))
    mappedRateCard %<>% 
      mutate(rateType = ifelse(id_origin == 6 & id_destination == 6 & rateType == "WAREHOUSE", "WAREHOUSE", 
                               gsub("PICKUP|WAREHOUSE", "PUWH", rateType))) 
    
    mappedRateCard <- left_join(mappedRateCard,
                                region_matrix,
                                by = c("rateType" = "type", "id_origin" = "from", "id_destination" = "to"))
    
    mappedRateCard <- left_join(mappedRateCard,
                                rateCard,
                                by = c("rateType" = "type", "codRange" = "codRange", "region" = "region"))
    
    mappedRateCard %<>%
      mutate(carrying_fee_laz = ifelse(package_chargeable_weight <= 0.02, w000002,
                            ifelse(package_chargeable_weight <= 0.05, w002005,
                            ifelse(package_chargeable_weight <= 0.1, w005010,
                            ifelse(package_chargeable_weight <= 0.25, w010025,
                            ifelse(package_chargeable_weight <= 0.5, w025050,
                            ifelse(package_chargeable_weight <= 1, w050100, 
                            ifelse(package_chargeable_weight <= 1.5, w100150,
                            ifelse(package_chargeable_weight <= 2, w150200,
                            ifelse(package_chargeable_weight <= 2.5, w200250,
                            ifelse(package_chargeable_weight <= 3, w250300,
                            ifelse(package_chargeable_weight <= 3.5, w300350,
                            ifelse(package_chargeable_weight <= 4, w350400,
                            ifelse(package_chargeable_weight <= 4.5, w400450,
                            ifelse(package_chargeable_weight <= 5, w450500,
                            ifelse(package_chargeable_weight <= 5.5, w500550,
                            ifelse(package_chargeable_weight <= 6, w550600, 
                                  w550600 + ceiling((package_chargeable_weight - 6)/ifelse(rateType == "ECONOMY" & region != "HCM", 1, 0.5)) * w050100plus
                                   )))))))))))))))) + shippingFeeAdjust)
    mappedRateCard %<>%
      mutate(RateCardMappedFlag = ifelse(is.na(carrying_fee_laz), "NOT_OKAY","OKAY")) %>%
      select(-c(w000002,	w002005,	w005010,	w010025,	w025050,	w050100,	w100150,	w150200,
                w200250,	w250300,	w300350,	w350400,	w400450,	w450500,	w500550,	w550600,	w050100plus, shippingFeeAdjust))
    
    mappedRateCard <- left_join(mappedRateCard,
                                surcharge,
                                by = c("rateType" = "type", "region" = "region"))
    
    mappedRateCard %<>%
      mutate(fuel_sur_laz = carrying_fee_laz * fuel_sur) %>%
      mutate(rural_sur_laz = ifelse(isRural == 1, ifelse(rateType == "PUWH" | region == "HCM", carrying_fee_laz * rural_sur_rate, 
                                   rural_sur_rate + ceiling((package_chargeable_weight - 5)/1) * rural_sur_500999), 0)) %>%
      mutate(super_rural_laz = ifelse(isRural == 2, carrying_fee_laz * super_rural, 0)) %>%
      mutate(VUN_laz = ifelse(isVUN == 1, ifelse(rateType == "PUWH" | region == "HCM", ceiling((package_chargeable_weight - 5)/1) * VUN,
                              carrying_fee_laz * VUN), 0)) %>%
      mutate(cross_checking_laz = ifelse(isCrossCheck == 1, cross_checking, 0)) %>%
      mutate(pickup_sur_laz = ifelse(line_id == "PICKUP", pickup_sur, 0 )) %>%
      mutate(bulky_sur_laz = ifelse(package_chargeable_weight > 31.5 & isBulky == 1, (carrying_fee_laz + fuel_sur_laz + rural_sur_laz + super_rural_laz + VUN_laz) * bulky_sur, 0))
    
    mappedRateCard %<>% 
      mutate(total_carrying_fee_laz = carrying_fee_laz + fuel_sur_laz + rural_sur_laz + super_rural_laz + bulky_sur_laz) %>%
      select(-c(bulky_sur, fuel_sur, rural_sur_rate, rural_sur_500999, super_rural, VUN, cross_checking, pick_up_at_address, failed_delivery_fee))
    
  }, error = function(err) {
    flog.error(paste(functionName, err, sep = " - "), name = reportName)
  }, finally = {
    flog.info(paste("Function", functionName, "ended"), name = reportName)
  })
  
  output
}