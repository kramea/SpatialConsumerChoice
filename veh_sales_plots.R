setwd("~/Documents/Postdoc/SPCC/SpatialConsumerChoice")

library(ggplot2)
library(plyr)
library(reshape2)

reg_pop <- read.csv("registration_per_zip.csv")
reg_pop <- reg_pop[,c(3,9)]
colnames(reg_pop) <- c("zip_value", "sales")

final_zip <- data.frame(readRDS("final_zip_pp_2017.RDS")) 

## Read the zip, zcta values for each city in the US
zip_zcta <- read.csv("zip_to_zcta_2015.csv")

## Subset the zip, zcta values for California
ca_zip_zcta <- subset(zip_zcta, STATE=="CA")

extract_zip <- function(zip_file){
  region <- read.csv(zip_file)
  region <- region["ZCTA5CE10"]
  colnames(region) <- "ZCTA"
  
  selectedRows <- (ca_zip_zcta$ZCTA %in% region$ZCTA)
  
  region_zip <- ca_zip_zcta[selectedRows, ]["ZIP"]
  
  return(region_zip)
}

sfbay_zip <- extract_zip('sfbayarea_zip.csv')
socal_zip <- extract_zip('socal_zip.csv')
central_zip <- extract_zip('centralca_zip.csv')
colnames(sfbay_zip) <- "zip_value"
colnames(socal_zip) <- "zip_value"
colnames(central_zip) <- "zip_value"

final_sales <- merge(reg_pop, final_zip, by="zip_value")
car_sales <- final_sales[,2]
final_car_sales <- as.matrix(final_sales[,-c(1,2)])
class(final_car_sales) <- "numeric"

car_sales_per_zip <- sweep(final_car_sales/100,MARGIN=1,car_sales,`*`)

car_sales_per_zip <- data.frame(car_sales_per_zip)
car_sales_per_zip[is.na(car_sales_per_zip)] <- 0

tot_car_sales <- colSums(car_sales_per_zip)

ca_veh <- t(data.frame(tot_car_sales))[,-c(3,6,10,12,13,14,18,19,20,23,26,30,32,33,34,38,39,40)]

##Combine cars and trucks
ca_ldv <- ca_veh[(1:11)] + ca_veh[(12:22)]

tech <- c("GSLC", "DSLC", "GHEV", "DHEV", "EP10", "EP20", "EP40", "FCVC", "E100", "E200", "E300")


get_final_sales <- function(region_zip){
  ### SF sales
  
  selectedrows <- (final_zip$zip_value %in% region_zip$zip_value)
  zip_pp <- final_zip[selectedrows, ]
  
  final_sales <- merge(reg_pop, zip_pp, by="zip_value")
  car_sales <- final_sales[,2]
  final_car_sales <- as.matrix(final_sales[,-c(1,2)])
  class(final_car_sales) <- "numeric"
  
  car_sales_per_zip <- sweep(final_car_sales/100,MARGIN=1,car_sales,`*`)
  
  car_sales_per_zip <- data.frame(car_sales_per_zip)
  car_sales_per_zip[is.na(car_sales_per_zip)] <- 0
  
  tot_car_sales <- colSums(car_sales_per_zip)
  
  veh <- t(data.frame(tot_car_sales))[,-c(3,6,10,12,13,14,18,19,20,23,26,30,32,33,34,38,39,40)]
  
  ##Combine cars and trucks
  ldv <- veh[(1:11)] + veh[(12:22)]
  
  return(ldv)
}

sf_sales <- get_final_sales(sfbay_zip)
socal_sales <- get_final_sales(socal_zip)
central_sales <- get_final_sales(central_zip)

selectedrows_rest_ca <- !(final_zip$zip_value %in% c(sfbay_zip$zip_value, 
                                                    socal_zip$zip_value, 
                                                    central_zip$zip_value))
zip_pp_rest_ca <- final_zip[selectedrows_rest_ca, ]

final_sales_rest_ca <- merge(reg_pop, zip_pp_rest_ca, by="zip_value")
car_sales_rest_ca <- final_sales_rest_ca[,2]
final_car_sales_rest_ca <- as.matrix(final_sales_rest_ca[,-c(1,2)])
class(final_car_sales_rest_ca) <- "numeric"

car_sales_per_zip_rest_ca <- sweep(final_car_sales_rest_ca/100,MARGIN=1,car_sales_rest_ca,`*`)

car_sales_per_zip_rest_ca <- data.frame(car_sales_per_zip_rest_ca)
car_sales_per_zip_rest_ca[is.na(car_sales_per_zip_rest_ca)] <- 0

tot_car_sales_rest_ca <- colSums(car_sales_per_zip_rest_ca)

ca_veh_rest_ca <- t(data.frame(tot_car_sales_rest_ca))[,-c(3,6,10,12,13,14,18,19,20,23,26,30,32,33,34,38,39,40)]

##Combine cars and trucks
ca_ldv_rest_ca <- ca_veh_rest_ca[(1:11)] + ca_veh_rest_ca[(12:22)]

tot_sales_state <- cbind(data.frame(ca_ldv), data.frame(sf_sales), 
                         data.frame(socal_sales), data.frame(central_sales),
                         data.frame(ca_ldv_rest_ca))

sum_tot_sales_state <- colSums(tot_sales_state)
ev_sales <- as.matrix(tot_sales_state[9,])
fcv_sales <- as.matrix(tot_sales_state[8,])

par(mai=c(1,1,1,1))
barplot((sum_tot_sales_state/100000), main = "Total Vehicle Sales", ylab = "Millions of Vehicles",
        horiz = FALSE, names.arg = c("CA", "SFBA", "SOCAL", "CENTRAL", "RestOfCA"), las = 1, col = "mediumorchid4",
        xlab = "", cex.axis = 1.3, cex.names = 1.3)




par(mai=c(1,1,1,1))
barplot((ev_sales/1000), main = "Total BEV Sales", ylab = "Thousands of Vehicles",
        horiz = FALSE, names.arg = c("CA", "SFBA", "SOCAL", "CENTRAL", "RestOfCA"), las = 1, col = "orange",
        xlab = "", cex.axis = 1.3, cex.names = 1.3)

par(mai=c(1,1,1,1))
barplot((fcv_sales/1000), main = "Total FCV Sales", ylab = "Millions of Vehicles",
        horiz = FALSE, names.arg = c("CA", "SFBA", "SOCAL", "CENTRAL", "RestOfCA"), las = 1, col = "green",
        xlab = "", cex.axis = 1.3, cex.names = 1.3)
