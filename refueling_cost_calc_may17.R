setwd("~/Documents/Postdoc/SPCC/SpatialConsumerChoice")

library(ggplot2)
library(plyr)
library(reshape2)
library(abind)

zip_value <- readRDS("zip_value.rds")
consumer_const <- read.csv('ConsumerConst.csv')
row.names(consumer_const) <- consumer_const[,1]
consumer_const[,1] <- NULL

# Whenever reliability of stations is updated, it needs to be saved into a new CSV file
infra_matrix <- read.csv('ca_infra_matrix_VMT_grps.csv') 

zip <- data.frame(zip_value)
colnames(zip) <- "ZipCode"

zip_dist <- dcast(infra_matrix, ZipCode ~ Number, value.var = "Distance..miles.")
zip_dist <- merge(zip, zip_dist, by = "ZipCode", all.x = TRUE)
zip_dist[is.na(zip_dist)] <- 100
zip_dist[,1] <- as.numeric(as.character(zip_dist[,1]))

vmt <- c(2500, 7500, 12500, 17500, 22500, 27500, 32500)

uf_vmt1 <- dcast(infra_matrix, ZipCode ~ Number, value.var = "UF.VMT1")
uf_vmt1 <- merge(zip, uf_vmt1, by = "ZipCode", all.x = TRUE)
uf_vmt1[is.na(uf_vmt1)] <- 0.11

uf_vmt2 <- dcast(infra_matrix, ZipCode ~ Number, value.var = "UF.VMT2")
uf_vmt2 <- merge(zip, uf_vmt2, by = "ZipCode", all.x = TRUE)
uf_vmt2[is.na(uf_vmt2)] <- 0.11

uf_vmt3 <- dcast(infra_matrix, ZipCode ~ Number, value.var = "UF.VMT3")
uf_vmt3 <- merge(zip, uf_vmt3, by = "ZipCode", all.x = TRUE)
uf_vmt3[is.na(uf_vmt3)] <- 0.11

uf_vmt4 <- dcast(infra_matrix, ZipCode ~ Number, value.var = "UF.VMT4")
uf_vmt4 <- merge(zip, uf_vmt4, by = "ZipCode", all.x = TRUE)
uf_vmt4[is.na(uf_vmt4)] <- 0.11


uf_vmt5 <- dcast(infra_matrix, ZipCode ~ Number, value.var = "UF.VMT5")
uf_vmt5 <- merge(zip, uf_vmt5, by = "ZipCode", all.x = TRUE)
uf_vmt5[is.na(uf_vmt5)] <- 0.11


uf_vmt6 <- dcast(infra_matrix, ZipCode ~ Number, value.var = "UF.VMT6")
uf_vmt6 <- merge(zip, uf_vmt6, by = "ZipCode", all.x = TRUE)
uf_vmt6[is.na(uf_vmt6)] <- 0.11


uf_vmt7 <- dcast(infra_matrix, ZipCode ~ Number, value.var = "UF.VMT7")
uf_vmt7 <- merge(zip, uf_vmt7, by = "ZipCode", all.x = TRUE)
uf_vmt7[is.na(uf_vmt7)] <- 0.11


## Combine all these utilizations and distances into a multi-dimensional dataframe

zip_uf_dist <- abind(zip_dist, uf_vmt1, uf_vmt2, uf_vmt3, uf_vmt4, uf_vmt5, uf_vmt6, uf_vmt7, along=3)
#dim(zip_uf_dist)

diesel_full_refill <- (65/26.7537092 * 0.823389532 * (0.3 ^ (-0.4886)) / 60 * 22.85367791) * 3.56

### Refueling cost 

refueling_h2 <- array(0, dim=c(nrow(zip_dist), 7, 5))
refueling_dsl <- array(0, dim=c(nrow(zip_dist), 7, 5))

for(zip in 1:nrow(zip_uf_dist)){
  for(ii in 1:5){
    for(vv in 1:7){
      refueling_dsl[zip, vv, ii] <- diesel_full_refill * (vmt[vv] / 300) * 4.152978
      
      temp_cost <- 0
      for(stn in 1:10){
        if(as.numeric(zip_uf_dist[zip, stn+1, 1]) > 40){
          temp_cost <- as.numeric(zip_uf_dist[zip, stn+1, vv+1])*27375 + temp_cost
        }else{
          temp_cost <- (1.27*as.numeric(zip_uf_dist[zip, stn+1, 1])*as.numeric(zip_uf_dist[zip, stn+1, vv+1])*consumer_const[vv, ii]) + temp_cost
        }
      }
      refueling_h2[zip, vv, ii] <- temp_cost
    }
  }
}

refueling_h2 <- refueling_h2 * 4.152978



saveRDS(refueling_h2, "refueling_cost_h2.rds")
saveRDS(refueling_dsl, "refueling_cost_dsl.rds")

