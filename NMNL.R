#--------NMNL VEHICLE CHOICE FUNCTION STARTS----#

nmnl <- function(gencostdata){
  
  ## gencostdata input is given as a dataframe, i.e. 1 column and 40 rows
  
  ### Set the working directory
  setwd("~/Documents/Postdoc/SPCC/SpatialConsumerChoice")
  
  ### Read price elastcity slopes
  ### Replace "NA" with '0'
  
  beta <- read.csv('beta.csv', header=FALSE)
  beta[is.na(beta)] <- 0
  
  ### Read the nest heterogeneity structure
  ### Replace "NA" with '0'
  
  nest <- read.csv('nest-heterogeneity.csv', header=FALSE)
  nest[is.na(nest)] <- 0
  
  ## Number of nests in each layer is defined
  nest_layer_one <- 2
  nest_layer_two <- 6
  nest_layer_three <- 18
  nest_layer_four <- 54
  
  ## Nest per Nest for Layers
  
  nest_per_nest_layer_one <- 2
  nest_per_nest_layer_two <- 3
  nest_per_nest_layer_three <- 3
  nest_per_nest_layer_four <- 3
  
  ## Group per Group for Layers
  
  group_per_group_layer_one <- 9
  group_per_group_layer_two <- 3
  group_per_group_layer_three <- 3
  group_per_group_layer_four <- 3
  
  
  #gencostdata <- (read.csv('test_cost_data.csv', header=FALSE)) ## Test data to test the NMNL module
  
  ## Initialize empty matrices for Exponential costs of generalized costs
  eToU <- matrix(0, nrow=4, ncol=54) ## Exponential of generalized costs
  sumEtoU <- matrix(0, nrow=4, ncol=54) ## Sum of exponentials
  prob <- matrix(0, nrow=4, ncol=54) ## Probability values
  dCost <- matrix(0, nrow=4, ncol=54) 
  choiceProb <- matrix(0, nrow=1, ncol=40) ## Final choice probabilities
  
  ## NMNL Model
  ## First exponentials are calculated for layer four
  ## They are summed up to other layer next
  iRealChoice = 1
  for (iChoice in 1:54){
    if(nest[4,iChoice]==0){
      dCost[4, iChoice] = 999999999
      eToU [4, iChoice]= 0
    }
    else{
      if (iChoice == 13){
        iChoice = iChoice
      }
      dCost[4,iChoice] = gencostdata[iRealChoice,1] 
      itemp = as.integer((iChoice-1)/nest_per_nest_layer_four) +1 #index of applied beta
      eToU[4,iChoice] = exp(beta[4,itemp]* dCost[4, iChoice]) 
      sumEtoU[4,itemp] = eToU[4,iChoice] + sumEtoU[4,itemp]
      iRealChoice = iRealChoice + 1
    }
  }
  
  ## Calculate Probabilities
  
  for (iChoice in 1:54){
    itemp = as.integer((iChoice-1)/nest_per_nest_layer_four) + 1 # index of applied beta
    if (nest[4,iChoice]==0){
      prob[4, iChoice] = 0
    }else if(nest[4, iChoice] == 100){
      prob[4, iChoice] = 1
    }else if(sumEtoU[4,itemp] ==0){
      dtemp = 0
      for (itemp2 in 1:group_per_group_layer_four){
        dtemp = dtemp + nest[4, (itemp-1)*group_per_group_layer_four + itemp2]
      }
      prob[4, iChoice] = 1/dtemp/11
    }
    else{
      prob[4, iChoice] = eToU[4, iChoice]/sumEtoU[4, itemp]
    }
  }

  ## Same functions are applied for the other three layers
  
  for (iLayer in 3:1){
    if(iLayer == 3){
      n = 18
      nestnum = nest_per_nest_layer_three
      grp = group_per_group_layer_three
    }else if(iLayer == 2){
      n = 6
      nestnum = nest_per_nest_layer_two
      grp = group_per_group_layer_two
    }else{
      n = 2
      nestnum = nest_per_nest_layer_one
      grp = group_per_group_layer_one
    }
    for (iNest in 1:n){
      if((nest[iLayer, iNest] == 0) | (sumEtoU[iLayer + 1, iNest] ==0 )){
        dCost [iLayer, iNest] = 999999999
        eToU[iLayer, iNest] = 0
      }else{
        dCost [iLayer, iNest] = log(sumEtoU[iLayer+1, iNest]) / beta[iLayer + 1, iNest]
        itemp = as.integer((iNest-1)/nestnum) + 1 # Index of appled beta
        eToU[iLayer, iNest] = exp(beta[iLayer, itemp]*dCost[iLayer,iNest])
        sumEtoU[iLayer, itemp] = sumEtoU[iLayer, itemp] + eToU[iLayer, iNest]
      }
    }
    for (iNest in 1:n){
      itemp = as.integer((iNest-1)/nestnum) + 1 #index of applied beta
      if ((nest[iLayer, iNest])==0){
        prob[iLayer, iNest] = 0
      }else if(nest[iLayer, iNest] == 100){
        prob[iLayer, iNest] = 1
      }else if(sumEtoU[iLayer, itemp]==0){
        dtemp = 0
        for (itemp2 in 1:grp){
          dtemp = dtemp + nest[iLayer, (itemp-1)*grp + itemp2]
        }
        prob[iLayer, iNest] = 1/dtemp/11
      }else{
        prob[iLayer, iNest] = eToU[iLayer, iNest] / sumEtoU[iLayer, itemp]
      }
    }
  }
  
  iChoice = 1
  for (itemp in 1:54){
    if (nest[4,itemp] > 0){
      dtemp = 1
      for (iLayer in 1:4){
        if(iLayer == 3){
          n = 18
          nestnum = nest_per_nest_layer_three
          grp = group_per_group_layer_three
        }else if(iLayer == 2){
          n = 6
          nestnum = nest_per_nest_layer_two
          grp = group_per_group_layer_two
        }else if(iLayer == 1){
          n = 2
          nestnum = nest_per_nest_layer_one
          grp = group_per_group_layer_one
        }else{
          n = 54
        }
        iNest = as.integer((itemp-1)/(54/n))+1 ## index of the container nest at layer iLayer for iChoice
        dtemp = dtemp * prob[iLayer, iNest]
      }
      choiceProb[1,iChoice] = dtemp
      iChoice = iChoice + 1
    }
  }
  
  colnames(choiceProb) <- as.character(vehprices[,1])
  
  purchase_probabilities <- round((choiceProb)*100,5)
  
  return(purchase_probabilities)
  
}

