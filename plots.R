setwd("~/Documents/Postdoc/SPCC/SpatialConsumerChoice")

library(ggplot2)
library(plyr)
library(reshape2)

zip_value <- data.frame(readRDS("zip_value.rds"))

final_dat <- data.frame(readRDS("final_tot_pp_2017.RDS"))
#final_dat <- data.frame(readRDS("final_tot_pp_20000inc.RDS"))
#final_zip <- data.frame(readRDS("final_zip_pp.RDS")) 
## Activate the above line after running the code calc_pp_from_rds again

##Temporary correction on November 14. Remove this after you run the code on calc_pp_from_rds.R
#final_dat <- final_dat[c(-1,-2),]
## The file was mistakenly appended to a test run. Once the code is run again
## it will fix the error

colnames(zip_value) <- "zip"
final_zip <- cbind(zip_value, final_dat)

ca_pop_zip <- read.csv('ca_pop_zip.csv')
ca_pop_only <- ca_pop_zip[c("ZIP", "pop")]
ca_pop_zip <- ca_pop_zip[c("ZIP", "pop_share")]


sf_pop_zip <- read.csv('sf_pop_zip.csv')
sf_pop_zip <- sf_pop_zip[c("ZIP", "pop")]

la_pop_zip <- read.csv('la_pop_zip.csv')
la_pop_zip <- la_pop_zip[c("ZIP", "pop")]

central_pop_zip <- read.csv('central_pop_zip.csv')
central_pop_zip <- central_pop_zip[c("ZIP", "pop")]

rest_of_ca_pop_rows <- !(ca_pop_only$ZIP %in% c(sf_pop_zip$ZIP, 
                                             la_pop_zip$ZIP, 
                                             central_pop_zip$ZIP))
rest_of_ca_pop <- ca_pop_only[rest_of_ca_pop_rows,]
rest_of_ca_pop <- rest_of_ca_pop[c("ZIP", "pop")]

library(ggplot2)

#Gasoline car and truck
car <- rowSums(final_dat[, c(1:20)])
truck <- rowSums(final_dat[,c(21:40)])
dat <- data.frame(dens = c(car, truck)
                  , lines = rep(c("Light-Duty Cars", "Light-Duty Trucks"), each = nrow(final_dat)))
#Plot.
ggplot(dat, aes(x = dens, fill = lines)) + geom_density(alpha = 0.5)


plot_dens_total <- function(df, dftitle = "Purchase Probability Distribution"){
  #Total BEV vs PHEV vs FCV vs HYB
  conv <- df[,1] + df[,21] + df[,2] + df[,22]
  bev <- df[,15] + df[,16] + df[,17] + df[,35] + df[,36] + df[,37]
  phev <- df[,7] + df[,8] + df[,9] + df[,27] + df[,28] + df[,29]
  fcv <- df[,11] + df[,31]
  hyb <- df[,4] + df[,5] + df[,24] + df[,25]
  dat <- data.frame(dens = c(conv, bev, phev, hyb)
                    , lines = rep(c("Conventional", "BEV", "PHEV", "HYB"), each = nrow(df)))
  #Plot.
  ggplot(dat, aes(x = dens, fill = lines)) + geom_density(alpha = 0.5) +  theme_bw() + xlab("Purchase Probability") +
    ylab("Density") + ggtitle(dftitle) +
    theme(axis.title = element_text(size = 18, face="bold")) +
    theme(axis.text = element_text(size = 14, face="bold")) +
    theme(plot.title = element_text(size = 20, face = "bold")) +
    theme(legend.text=element_text(size=12)) + labs(fill="")
}

plot_cars <- function(df, dftitle = "Purchase Probability Distribution"){
  conv <- df[,1] + df[,2] 
  bev <- df[,15] + df[,16] + df[,17] 
  phev <- df[,7] + df[,8] + df[,9] 
  fcv <- df[,11] 
  hyb <- df[,4] + df[,5] 
  dat <- data.frame(dens = c(conv, bev, phev, hyb)
                    , lines = rep(c("Conventional", "BEV", "PHEV", "HYB"), each = nrow(df)))
  #Plot.
  ggplot(dat, aes(x = dens, fill = lines)) + geom_density(alpha = 0.5) +  theme_bw() + xlab("Purchase Probability") +
    ylab("Density") + ggtitle(dftitle) + xlim(c(0,60)) + 
    theme(axis.title = element_text(size = 18, face="bold")) +
    theme(axis.text = element_text(size = 14, face="bold")) +
    theme(plot.title = element_text(size = 20, face = "bold")) +
    theme(legend.text=element_text(size=12)) + labs(fill="")
}

plot_trucks <- function(df, dftitle = "Purchase Probability Distribution"){
  #Total BEV vs PHEV vs FCV vs HYB trucks only
  conv <-  df[,21] + df[,22]
  bev <-  df[,35] + df[,36] + df[,37]
  phev <- df[,27] + df[,28] + df[,29]
  fcv <- df[,31]
  hyb <-  df[,24] + df[,25]
  dat <- data.frame(dens = c(conv, bev, phev, hyb)
                    , lines = rep(c("Conventional", "BEV", "PHEV", "HYB"), each = nrow(df)))
  #Plot.
  ggplot(dat, aes(x = dens, fill = lines)) + geom_density(alpha = 0.5) +  theme_bw() + xlab("Purchase Probability") +
    ylab("Density") + ggtitle(dftitle) + xlim(c(0,60)) +
    theme(axis.title = element_text(size = 18, face="bold")) +
    theme(axis.text = element_text(size = 14, face="bold")) +
    theme(plot.title = element_text(size = 20, face = "bold")) +
    theme(legend.text=element_text(size=12)) + labs(fill="")
  
  
}


##### Regional Plots

sf <- read.csv('sfbay_zip_processed.csv')

selectedRows <- (final_zip$zip %in% sf$ZIP) 

sf_pp <- final_zip[selectedRows, ] ## There is data loss more than expected. WHY?

plot_dens_total(sf_pp[,-1], "Purchase Probability Distribution in SF Bay Area")
plot_cars(sf_pp[,-1], "Purchase Probability Distribution in SF Bay Area")
plot_trucks(sf_pp[,-1], "Purchase Probability Distribution in SF Bay Area")

la <- read.csv('socal_zip_processed.csv')

selectedRows <- (final_zip$zip %in% la$ZIP)

la_pp <- final_zip[selectedRows, ]

plot_dens_total(la_pp[,-1], "Purchase Probability Distribution in So. Cal")
plot_cars(la_pp[,-1], "Purchase Probability Distribution in So. Cal")
plot_trucks(la_pp[,-1], "Purchase Probability Distribution in So. Cal")

central <- read.csv('centralca_zip_processed.csv')

selectedRows <- (final_zip$zip %in% central$ZIP)

central_pp <- final_zip[selectedRows, ]

plot_dens_total(central_pp[,-1], "Purchase Probability Distribution in Central Cal")
plot_cars(central_pp[,-1], "Purchase Probability Distribution in Central Cal")
plot_trucks(central_pp[,-1], "Purchase Probability Distribution in Central Cal")

rest_of_ca_rows <- !(final_zip$zip %in% c(sf$ZIP, la$ZIP, central$ZIP))
rest_of_ca_pp <- final_zip[rest_of_ca_rows,]


#### Weighted average aggregated state

pp_agg <- final_dat * ca_pop_zip[,2]
pp_agg_final <- colSums(pp_agg, na.rm = TRUE)

selectedRows_sf <- (sf_pop_zip$ZIP %in% sf_pp$zip)
sf_pop_zip <- sf_pop_zip[selectedRows_sf, ]
sf_pop_zip$pop_share <- sf_pop_zip$pop / sum(sf_pop_zip[,2])
sf_pp_agg <- sf_pp[,-1] * sf_pop_zip[,3]
sf_pp_agg_final <- colSums(sf_pp_agg)


selectedRows_la <- (la_pop_zip$ZIP %in% la_pp$zip)
la_pop_zip <- la_pop_zip[selectedRows_la, ]
la_pop_zip$pop_share <- la_pop_zip$pop / sum(la_pop_zip[,2])
la_pp_agg <- la_pp[,-1] * la_pop_zip[,3]
la_pp_agg_final <- colSums(la_pp_agg)

selectedRows_central <- (central_pop_zip$ZIP %in% central_pp$zip)
central_pop_zip <- central_pop_zip[selectedRows_central, ]
central_pop_zip$pop_share <- central_pop_zip$pop / sum(central_pop_zip[,2])
central_pp_agg <- central_pp[,-1] * central_pop_zip[,3]
central_pp_agg_final <- colSums(central_pp_agg)

selectedRows_restofca <- (rest_of_ca_pop$ZIP %in% rest_of_ca_pp$zip)
rest_of_ca_pop <- rest_of_ca_pop[selectedRows_restofca, ]
rest_of_ca_pop$pop_share <- rest_of_ca_pop$pop / sum(rest_of_ca_pop[,2])
rest_of_ca_pp_agg <- rest_of_ca_pp[,-1] * rest_of_ca_pop[,3]
rest_of_ca_pp_agg_final <- colSums(rest_of_ca_pp_agg, na.rm=TRUE)


## Remove unused technologies
ca_veh <- t(data.frame(pp_agg_final))[,-c(3,6,10,12,13,14,18,19,20,23,26,30,32,33,34,38,39,40)]
sf_veh <- t(data.frame(sf_pp_agg_final))[,-c(3,6,10,12,13,14,18,19,20,23,26,30,32,33,34,38,39,40)]
la_veh <- t(data.frame(la_pp_agg_final))[,-c(3,6,10,12,13,14,18,19,20,23,26,30,32,33,34,38,39,40)]
central_veh <- t(data.frame(central_pp_agg_final))[,-c(3,6,10,12,13,14,18,19,20,23,26,30,32,33,34,38,39,40)]
restofca_veh <- t(data.frame(rest_of_ca_pp_agg_final))[,-c(3,6,10,12,13,14,18,19,20,23,26,30,32,33,34,38,39,40)]

##Combine cars and trucks
ca_ldv <- ca_veh[(1:11)] + ca_veh[(12:22)]
sf_ldv <- sf_veh[(1:11)] + sf_veh[(12:22)]
la_ldv <- la_veh[(1:11)] + la_veh[(12:22)]
central_ldv <- central_veh[(1:11)] + central_veh[(12:22)]
restofca_ldv <- restofca_veh[(1:11)] + restofca_veh[(12:22)]

tech <- c("GSLC", "DSLC", "GHEV", "DHEV", "EP10", "EP20", "EP40", "FCVC", "E100", "E200", "E300")

ca_final_share <- prop.table(ca_ldv)
sf_final_share <- prop.table(sf_ldv)
la_final_share <- prop.table(la_ldv)
central_final_share <- prop.table(central_ldv)
restofca_final_share <- prop.table(restofca_ldv)

veh_share <- data.frame(cbind(tech, ca_final_share, sf_final_share, la_final_share, central_final_share, restofca_final_share))
colnames(veh_share) <- c("Tech", "California", "SF Bay Area", "SoCal", "Central CA", "Rest of California")


png(file="agg_result_regions.png",width=800,height=550)
melted <- melt(veh_share, id.vars='Tech')
melted[,3] <- as.numeric(melted[,3])
ref <- ggplot(melted, aes(x=variable, y=value*100, fill=Tech))+ geom_bar(stat='identity')
# clabel <- c("2005", " ", " ", " ", "2010", " ", " ", " ", " ", "2015", " ", " ", " ", " ", "2020", " ", " ", " ", " ", "2025", 
#             " ", " ", " ", " ", "2030", " ", " ", " ", " ", "2035", " ", " ", " ", " ", "2040", " ", " ", " ", " ", "2045", " ", " ", " ", " ", "2050", " ")

clabel <- c("California", "SF Bay Area", "SoCal", "Central CA", "Rest of California")

ref + theme_bw() + scale_x_discrete(labels=(clabel)) +xlab("") + scale_fill_manual(values=c("GSLC"="royalblue1","DSLC"="red3", "GHEV" ="lightseagreen", 
                                                                                            "DHEV" ="darkblue","EP10"="mediumorchid2", "EP20" ="mediumpurple2", 
                                                                                            "EP40"="purple2", "FCVC"="olivedrab1", 
                                                                                            "E100" ="darkorange1", "E200"="chocolate4", "E300"="black"))+ ylab("Purchase Probability (%))")+  
  
  theme(axis.title = element_text(size = 20, face="bold"))+  theme(axis.text = element_text(size = 18, face="bold")) + theme(axis.text.x=element_text(angle=0, hjust=1))+theme(legend.position="none") +
  coord_flip()
dev.off()



