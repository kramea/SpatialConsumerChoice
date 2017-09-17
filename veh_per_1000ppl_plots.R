setwd("~/Documents/Postdoc/SPCC/SpatialConsumerChoice")

library(ggplot2)
library(plyr)
library(reshape2)

zip_value <- readRDS("zip_value.rds")
income_shares <- readRDS("income_shares.rds")
inc_vmt_shares <- readRDS("inc_vmt_shares.rds")
inc_vmt_amt <- readRDS("inc_vmt_amt.rds")
availability <- readRDS("availability.rds")
homecharge <- readRDS("homecharge_shares.rds")
ca_pop <- read.csv('ca_pop_zip.csv')

final_dat <- data.frame(readRDS("final_tot_pp_2017.RDS"))
final_zip <- data.frame(readRDS("final_zip_pp_2017.RDS"))

regis_per_zip <- read.csv('registration_per_zip.csv')

zip_county <- read.csv('zip_codes_states.csv')


car.reg_pop_per_zip <- regis_per_zip[c("ZIP", "pop", "carsperzip")]
colnames(car.reg_pop_per_zip) <- c("zip_value", "pop", "carsperzip")


final_sales <- merge(car.reg_pop_per_zip, final_zip, by="zip_value")

car_sales <- final_sales[,2]

final_car_sales <- as.matrix(final_sales[,-c(1:3)])
class(final_car_sales) <- "numeric"

car_sales_per_zip <- sweep(final_car_sales/100,MARGIN=1,car_sales,`*`)

car_sales_per_zip <- data.frame(car_sales_per_zip)
car_sales_per_zip <- cbind(final_sales$zip_value, final_sales$pop, car_sales_per_zip)

write.csv(car_sales_per_zip, "car_sales_per_zip.csv")


#### Generate results per city

car_sales_pop_per_zip <- read.csv("car_sales_per_zip.csv")
car_sales_pop_per_zip <- car_sales_pop_per_zip[,-1]

colnames(car_sales_pop_per_zip) <- c("zip_code", "pop", colnames(car_sales_pop_per_zip)[c(-1,-2)])

## Merge city names with pop

car_sales_with_citynames <- merge(car_sales_pop_per_zip, zip_county, by = "zip_code")

## Extract the needed columns
car_sales_city <- car_sales_with_citynames[,c(2:42,45)]

grouped_sales_city <- ddply(car_sales_city, .(city), numcolwise(sum))

grouped_sales_city$evperthouperson <- (grouped_sales_city$EV100.Car + grouped_sales_city$EV200.Car +
                                         grouped_sales_city$EV300.Car + grouped_sales_city$EV100.LTK +
                                         grouped_sales_city$EV200.LTK + grouped_sales_city$EV300.LTK)/ grouped_sales_city$pop 

grouped_sales_city <- grouped_sales_city[order(grouped_sales_city$evperthouperson, decreasing = TRUE),]


metro <- subset(grouped_sales_city, grouped_sales_city$pop > 60000)
metro <- metro[order(metro$evperthouperson, decreasing = TRUE),]

top_cities_ev <- metro[c(1:20), c(1,43)]


grouped_sales_city$fcvperperson <- (grouped_sales_city$FC.HEV.Car + grouped_sales_city$FC.HEV.LTK)/ grouped_sales_city$pop 

grouped_sales_city_h2 <- grouped_sales_city[order(grouped_sales_city$fcvperperson, decreasing = TRUE),]

top_cities_h2 <- grouped_sales_city_h2[c(1:20), c(1,44)]


par(mar=c(5.1, 7.1 ,4.1 ,2.1))
barplot(rev(top_cities_ev$evperthouperson), main = "BEVs per person", horiz = TRUE, 
        names.arg = rev(top_cities_ev$city), las = 1, col="orange")

par(mar=c(5.1, 8.1 ,4.1 ,2.1))
barplot(rev(top_cities_h2$fcvperperson), main = "FCVs per person", horiz = TRUE, 
        names.arg = rev(top_cities_h2$city), las = 1, col="lightgreen")

