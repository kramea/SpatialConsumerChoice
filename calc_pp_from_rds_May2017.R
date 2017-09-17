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
risk_shares_zip <- readRDS("risk_shares_zip.rds")

refueling_cost_h2 <- readRDS('refueling_cost_h2.rds')
refueling_cost_dsl <- readRDS('refueling_cost_dsl.rds')

s_rows <- (inc_vmt_amt > 28288)
inc_vmt_amt[s_rows] <- 28288

workcharge <- read.csv("workcharge_shares.csv")
workcharge <- workcharge[,c(-1,-2)]

availability[is.na(availability)] <- 0.0001

## This function calls the nested logit core module to estimate the purchase probability of vehicles
source("nmnl.R")

## Read relevant data
vehprices <- read.csv('vehprices.csv')
eff <- read.csv('dA.csv') ## Non-electric vehicle technology efficiency in ggepm
elec_cd_eff <- read.csv('dBe.csv') ## Electricity CD (charge depletion) efficiency
elec_cs_eff <- read.csv('dB.csv') ## Electricity CS (charge sustaining) efficiency, this is used in PHEVs to measure blended range
constant <- read.csv('constant.csv') ## Calibration constant
storage <- read.csv('storage.csv') ## Fuel storage
veh_range <- read.csv('xd.csv') ## Vehicle Range
fuelprices <- read.csv('fuelprices.csv') ## Fuel prices
modelavail <- read.csv('modelAvailability.csv') ## Make and model diversity cost
risk <- read.csv('risk.csv') ## Risk premium



refueling_cost_calc <- function(vmt, ii, vv, zip){
  
  eff[is.na(eff)] <- 0
  eff_ldc <- eff[c(1:20),3]
  eff_ldt <- eff[c(21:40),3]
  
  storage[is.na(storage)] <- 0
  storage_ldc <- storage[c(1:20),3]
  storage_ldt <- storage[c(21:40),3]
  
  
  #h2_full_refill <- (65/26.7537092 * 0.823389532 * (h2avail ^ (-0.4886)) / 60 * 22.85367791) * 3.56
  #gasoline_full_refill <- (65/26.7537092 * 0.823389532 * (1 ^ (-0.4886)) / 60 * 22.85367791) * 3.56
  #diesel_full_refill <- (65/26.7537092 * 0.823389532 * (0.3 ^ (-0.4886)) / 60 * 22.85367791) * 3.56
  
  
  annual_fuel_ldc = vmt * eff_ldc
  annual_fuel_ldt = vmt * eff_ldt
  
  refueling_cost_ldc <- c(rep(0,20))
  refueling_cost_ldt <- c(rep(0,20))
  
  for(t in 1:20){
    if(t == 1 | t == 4 | t == 7 | t == 8| t == 9){
      refueling_cost_ldc[t] <- 500
      refueling_cost_ldt[t] <- 500
      #refueling_cost_ldc[t] <- (annual_fuel_ldc[t]/storage_ldc[t]/0.75)*gasoline_full_refill * 4.152978
      #refueling_cost_ldt[t] <- (annual_fuel_ldt[t]/storage_ldt[t]/0.75)*gasoline_full_refill * 4.152978
    }
    else if(t == 2 | t == 5){
      refueling_cost_ldc[t] <- refueling_cost_dsl[zip, vv, ii]
      refueling_cost_ldt[t] <- refueling_cost_dsl[zip, vv, ii]
      #refueling_cost_ldc[t] <- (annual_fuel_ldc[t]/storage_ldc[t]/0.75)*diesel_full_refill  * 4.152978
      #refueling_cost_ldt[t] <- (annual_fuel_ldt[t]/storage_ldt[t]/0.75)*diesel_full_refill * 4.152978
    }
    else if(t == 11){
      refueling_cost_ldc[t] <- refueling_cost_h2[zip, vv, ii]
      refueling_cost_ldt[t] <- refueling_cost_h2[zip, vv, ii]
      #refueling_cost_ldc[t] <- (annual_fuel_ldc[t]/storage_ldc[t]/0.75)*h2_full_refill * 2.152978
      #refueling_cost_ldt[t] <- (annual_fuel_ldt[t]/storage_ldt[t]/0.75)*h2_full_refill * 2.152978
    }
  }
  
  refueling_cost_total <- cbind(refueling_cost_ldc, refueling_cost_ldt, annual_fuel_ldc, annual_fuel_ldt)
  return(refueling_cost_total)
}

range_anxiety_calc <- function(vmt, risk, evavail, home, work){
  
  veh_range[is.na(veh_range)] <- 0
  veh_range_ldc <- veh_range[c(1:20),4]
  veh_range_ldt <- veh_range[c(21:40),4]
  
  elec_cd_eff[is.na(elec_cd_eff)] <- 0
  elec_cd_eff_ldc <- elec_cd_eff[c(1:20),4]
  elec_cd_eff_ldt <- elec_cd_eff[c(21:40),4]
  
  elec_cs_eff[is.na(elec_cs_eff)] <- 0
  elec_cs_eff_ldc <- elec_cs_eff[c(1:20),4]
  elec_cs_eff_ldt <- elec_cs_eff[c(21:40),4]
  
  gamma_shape = 1.89756
  gamma_scale = (vmt/(365*gamma_shape))
  
  if(risk=="EA"){
    anx_cost <- 1
  }else if(risk=="EM"){
    anx_cost <- 20 * 2
  }else{
    anx_cost <- 50 * 2
  }
  
  
  Rrh_ldc <- vector(mode='numeric', 20)
  Rrp_ldc <- vector(mode='numeric', 20)
  Rrw_temp_ldc <- vector(mode='numeric', 20)
  Rrw_ldc <- vector(mode='numeric', 20)
  xrcd_ldc <- vector(mode='numeric', 20)
  dCDK_ldc <- vector(mode='numeric', 20)
  dCDKK_ldc <- vector(mode='numeric', 20)
  rental_days_ldc <-vector(mode='numeric', 20)
  rental_cost_ldc <- vector(mode='numeric', 20)
  kwh_ldc <- vector(mode='numeric', 20)
  term1_ldc <- vector(mode='numeric', 20)
  phev_annual_fuel_ldc <- vector(mode='numeric', 20)
  
  for(t in 1:20){
    Rrh_ldc[t] = pmin(veh_range_ldc[t], home * home_speed*home_hours/elec_cd_eff_ldc[t])
    Rrp_ldc[t] = pmin(veh_range_ldc[t], 6*2*evavail/elec_cd_eff_ldc[t])
    if(home == 1){
      Rrw_temp_ldc[t] = veh_range_ldc[t]-(Rrh_ldc[t]-gamma_scale/4)
    }else{
      Rrw_temp_ldc[t] = veh_range_ldc[t]-(0-gamma_scale/4)
    }
    Rrw_ldc[t] <- pmin(Rrw_temp_ldc[t], work*work_speed*work_hours/elec_cd_eff_ldc[t])
    xrcd_ldc[t] <- Rrh_ldc[t] + Rrw_ldc[t] + Rrp_ldc[t]
    dCDK_ldc[t] <- pgamma(xrcd_ldc[t], shape=gamma_shape, scale=gamma_scale) 
    dCDKK_ldc[t] <- pgamma(xrcd_ldc[t], shape=gamma_shape+1, scale=gamma_scale) 
    if(t == 15 | t == 16 | t == 17) {
      rental_days_ldc[t] <- (1-dCDK_ldc[t])*365
      rental_cost_ldc[t] <- rental_days_ldc[t] * anx_cost * 2.15297829332967
    }else{
      rental_cost_ldc[t] <- 0
    }
    kwh_ldc[t] <- ((elec_cd_eff_ldc[t] * (1-dCDK_ldc[t]) * xrcd_ldc[t]) + (elec_cd_eff_ldc[t] * gamma_shape * gamma_scale * dCDKK_ldc[t]))*365
    if(t == 7 | t == 8 | t == 9 ){
      term1_ldc[t] <- (gamma_shape * gamma_scale * (eff_ldc[t] + (elec_cs_eff_ldc[t] - eff_ldc[t])*dCDKK_ldc[t]))
      phev_annual_fuel_ldc[t] <- (((elec_cs_eff_ldc[t] - eff_ldc[t])*xrcd_ldc[t]*(1-dCDK_ldc[t])) + term1_ldc[t])*365
    }
  }
  
  
  
  Rrh_ldt <- vector(mode='numeric', 20)
  Rrp_ldt <- vector(mode='numeric', 20)
  Rrw_temp_ldt <- vector(mode='numeric', 20)
  Rrw_ldt <- vector(mode='numeric', 20)
  xrcd_ldt <- vector(mode='numeric', 20)
  dCDK_ldt <- vector(mode='numeric', 20)
  dCDKK_ldt <- vector(mode='numeric', 20)
  rental_days_ldt <-vector(mode='numeric', 20)
  rental_cost_ldt <- vector(mode='numeric', 20)
  kwh_ldt <- vector(mode='numeric', 20)
  term1_ldt <- vector(mode='numeric', 20)
  phev_annual_fuel_ldt <- vector(mode='numeric', 20)
  
  for(t in 1:20){
    Rrh_ldt[t] = pmin(veh_range_ldt[t], home * home_speed*home_hours/elec_cd_eff_ldt[t])
    Rrp_ldt[t] = pmin(veh_range_ldt[t], 6*2*evavail/elec_cd_eff_ldt[t])
    if(home == 1){
      Rrw_temp_ldt[t] = veh_range_ldt[t]-(Rrh_ldt[t]-gamma_scale/4)
    }else{
      Rrw_temp_ldt[t] = veh_range_ldt[t]-(0-gamma_scale/4)
    }
    Rrw_ldt[t] <- pmin(Rrw_temp_ldt[t], work*work_speed*work_hours/elec_cd_eff_ldt[t])
    xrcd_ldt[t] <- Rrh_ldt[t] + Rrw_ldt[t] + Rrp_ldt[t]
    dCDK_ldt[t] <- pgamma(xrcd_ldt[t], shape=gamma_shape, scale=gamma_scale) 
    dCDKK_ldt[t] <- pgamma(xrcd_ldt[t], shape=gamma_shape+1, scale=gamma_scale) 
    if(t == 15 | t == 16 | t == 17) {
      rental_days_ldt[t] <- (1-dCDK_ldt[t])*365
      rental_cost_ldt[t] <- rental_days_ldt[t] * anx_cost * 2.15297829332967
    }else{
      rental_cost_ldt[t] <- 0
    }
    kwh_ldt[t] <- ((elec_cd_eff_ldt[t] * (1-dCDK_ldt[t]) * xrcd_ldt[t]) + (elec_cd_eff_ldt[t] * gamma_shape * gamma_scale * dCDKK_ldt[t]))*365
    if(t == 7 | t == 8 | t == 9 ){
      term1_ldt[t] <- (gamma_shape * gamma_scale * (eff_ldt[t] + (elec_cs_eff_ldt[t] - eff_ldt[t])*dCDKK_ldt[t]))
      phev_annual_fuel_ldt[t] <- (((elec_cs_eff_ldt[t] - eff_ldt[t])*xrcd_ldt[t]*(1-dCDK_ldt[t])) + term1_ldt[t])*365
    }
  }
  
  rentalcost_kwh_total <- cbind(rental_cost_ldc, rental_cost_ldt, kwh_ldc, kwh_ldt, phev_annual_fuel_ldc, phev_annual_fuel_ldt)
  
  return(rentalcost_kwh_total)
}


risk_grps <- c("EA", "EM", "LM")
#risk_share <- c(0.08, 0.38, 0.54)

year <- 2020
home = 0
work = 0
#home_speed = 1.1
home_speed = 6
home_hours = 8
work_speed = 6
work_hours = 7
fuel_price_data <- fuelprices[,"X2020"]
eff[is.na(eff)] <- 0
eff_ldc <- eff[c(1:20),3]
eff_ldt <- eff[c(21:40),3]

vehprice_current <- vehprices[,"X2020"]
vehprice_current[is.na(vehprice_current)] <- 0

colnames(modelavail) <- c("Tech", 2005:2050)
modelavail_current <- modelavail["2020"]

constant_current <- constant[,4]
full_zip_pp <- vector(mode="numeric", 0)
#final_pp <- vector(mode="numeric", 0)
final_tot_pp <- vector(mode="numeric", 0)

## Revise this code **
for(zip in 1:nrow(income_shares)){
#for(zip in 1253:1253){
#for(zip in 1:10){
  home_charge_share <- c(1 - (homecharge[zip]/100), homecharge[zip]/100)
  work_charge_share <- c((1 - workcharge[zip]), workcharge[zip])
  #h2avail <- availability[zip, 2]
  #evavail <- availability[zip, 1]
  
  final_pp <- vector(mode="numeric", 0)
  #final_tot_pp <- vector(mode="numeric", 0)
  #print(zip_value)
  #zip_income <- subset(inc_trips, home_zipcode == zip_value)
  #zip_income <- subset(zip_income, mode == 5 | mode == 6 | mode == 7)
  
  if(sum(income_shares[zip,]) == 0){
    #final_tot_pp <-  as.data.frame(t(c(rep(NA, 40))))
    #colnames(final_tot_pp) <- as.character(vehprices[,1])
    temp <- as.data.frame(t(c(rep(NA, 40))))
    #colnames(temp) <- as.character(vehprices[,1])
    #final_pp <-  rbind(final_pp, temp)
  }else{
    income_levels <- c(15000, 30000, 62500, 112500, 150000)
    
    for(ii in 1:5){
      if(sum(inc_vmt_shares[zip, ii,])==0){
        #print(ii)
        temp <- as.data.frame(t(c(rep(NA, 40))))
        
        #colnames(temp) <- as.character(vehprices[,1])
        #final_pp <-  rbind(final_pp, temp)
        #colnames(final_pp) <- as.character(vehprices[,1])
        
      }
      else {
        for(vv in 1:7){
          if(income_shares[zip, ii] > 0){
            
            if(inc_vmt_amt[zip, ii, vv] == 0){
              ref_cost <- matrix(NA, 20, 4)
            }else{
              #ref_cost <- refueling_cost_calc(inc_vmt_amt[zip, ii, vv], availability[zip, 2])
              ref_cost <- refueling_cost_calc(inc_vmt_amt[zip, ii, vv], ii, vv, zip)
            }
            annual_fuel_ldc <- ref_cost[,3]
            annual_fuel_ldt <- ref_cost[,4]
            
            annual_fuel_ldc[is.na(annual_fuel_ldc)] <- 999999
            annual_fuel_ldt[is.na(annual_fuel_ldt)] <- 999999
            
            for(risk in 1:length(risk_grps)){
              for(w in 0:1){
                for(h in 0:1){
                  if(inc_vmt_amt[zip, ii, vv] > 0 ){
                    rental_cost <- range_anxiety_calc(inc_vmt_amt[zip, ii, vv], risk_grps[risk], availability[zip, 1], h, w)
                  }else{
                    rental_cost <- matrix(NA, 20, 6)
                  }
                                
                  kwh_ldc <- rental_cost[,3]
                  kwh_ldc[is.na(kwh_ldc)] <- 999999
                
                  kwh_ldt <- rental_cost[,4]
                  kwh_ldt[is.na(kwh_ldt)] <- 999999
                  
                  phev_annual_fuel_ldc <- rental_cost[,5]
                  phev_annual_fuel_ldt <- rental_cost[,6]
                  
                  phev_annual_fuel_ldc[is.na(phev_annual_fuel_ldc)] <- 999999
                  phev_annual_fuel_ldt[is.na(phev_annual_fuel_ldt)] <- 999999
                
                  fuel_cost_ldc <- vector(mode="numeric", 20)
                  fuel_cost_ldt <- vector(mode="numeric", 20)
                  
                  for (i in 1:20){
                    if(i == 2 | i == 5){
                      fuel_cost_ldc[i] <- fuel_price_data[2]*annual_fuel_ldc[i] * 4.152978
                      fuel_cost_ldt[i] <- fuel_price_data[2]*annual_fuel_ldt[i] * 4.152978
                    }else if(i == 3 | i == 6 ){
                      fuel_cost_ldc[i] <- 0.001 ## Placeholder for natural gas
                      fuel_cost_ldt[i] <- 0.001 ## Placeholder for natural gas
                    }else if(i == 10 | i == 11 | i == 12 | i == 13 | i == 14 ){
                      fuel_cost_ldc[i] <- fuel_price_data[4]*annual_fuel_ldc[i] * 4.152978 
                      fuel_cost_ldt[i] <- fuel_price_data[4]*annual_fuel_ldt[i] * 4.152978 
                    }else if(i == 15 | i == 16 | i == 17 ){
                      fuel_cost_ldc[i] <- 0 ## Electricity cost will be given for BEVs
                      fuel_cost_ldt[i] <- 0 ## Electricity cost will be given for BEVs
                    }else if(i == 7 | i == 8 | i == 9){
                      fuel_cost_ldc[i] <- fuel_price_data[1]*phev_annual_fuel_ldc[i] * 4.152978   ## Gasoline vehicles
                      fuel_cost_ldt[i] <- fuel_price_data[1]*phev_annual_fuel_ldt[i] * 4.152978  ## Gasoline vehicles
                    }
                    else {
                      fuel_cost_ldc[i] <- fuel_price_data[1]*annual_fuel_ldc[i] * 4.152978  ## Gasoline vehicles
                      fuel_cost_ldt[i] <- fuel_price_data[1]*annual_fuel_ldt[i] * 4.152978 ## Gasoline vehicles
                    }
                  }
                  
                  electricity_cost_ldc <- c(rep(0,20))
                  electricity_cost_ldt <- c(rep(0,20))
                
                  for (i in 1:20){
                    if(i == 7 | i ==8 | i == 9| i == 12| i == 13 | i == 14 |
                       i == 15 | i == 16 | i == 17){
                      electricity_cost_ldc[i] <- fuel_price_data[3]*kwh_ldc[i]  * 4.152978 ## Electricity cost will be given for BEVs
                      electricity_cost_ldt[i] <- fuel_price_data[3]*kwh_ldt[i] * 4.152978 ## Electricity cost will be given for BEVs
                    }
                  }
                    
                  fuel_cost_current <- c(fuel_cost_ldc, fuel_cost_ldt) 
                  electricity_cost_current <- c(electricity_cost_ldc, electricity_cost_ldt)
                  
                  fuel_cost_current[is.na(fuel_cost_current)] <- 999999
                  electricity_cost_current[is.na(electricity_cost_current)] <- 999999
                  
                  refueling_cost_current <- c(ref_cost[,1], ref_cost[,2]) 
                  refueling_cost_current[is.na(refueling_cost_current)] <- 999999
                  
                  rental_cost_current <- c(rental_cost[,1], rental_cost[,2]) 
                  rental_cost_current[is.na(rental_cost_current)] <- 999999
                
                  #income_pref_current <- (vehprice_current / inc_shares_final$avginc[ii])*10000
                  #income_pref_current <- (vehprice_current / inc_shares_final$avginc[ii])*30000
                  income_pref_current <- (((vehprice_current - vehprice_current[1])) / income_levels[ii]) *100000
                  
                  tot_cost <- vehprice_current + constant_current + rental_cost_current + 
                    refueling_cost_current + fuel_cost_current + electricity_cost_current + modelavail_current + 
                    income_pref_current
                  
                  # tot_cost <- vehprice_current + constant_current + rental_cost_current + 
                  #    refueling_cost_current + fuel_cost_current + electricity_cost_current + 
                  #    income_pref_current
                  tot_cost[is.na(tot_cost)] <- 999999
                  
                  purchase_probabilities <- nmnl(as.data.frame(tot_cost))
                  
                  dem_share <- inc_vmt_shares[zip,ii,vv] *risk_shares_zip[zip, risk + 1] * income_shares[zip, ii] * home_charge_share[h+1] * work_charge_share[w+1]
                  
                  purchase_probabilities <- dem_share * purchase_probabilities
                  final_pp <- rbind(final_pp, purchase_probabilities) 
                  #print(purchase_probabilities)
                }
              }
            }
          }
        }

      }
    }
  }
  final_tot_pp <- rbind(final_tot_pp, colSums(data.frame(final_pp)))
  #final_tot_pp <- as.data.frame(t(final_tot_pp))
}

final_zip_pp <- cbind(zip_value, final_tot_pp)

saveRDS(final_tot_pp, "final_tot_pp_2017.RDS")
saveRDS(final_zip_pp, "final_zip_pp_2017.RDS")

