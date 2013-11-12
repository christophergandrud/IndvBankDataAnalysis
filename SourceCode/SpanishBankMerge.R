###########
# Combine Spanish Commercial Bank Data
# Christopher Gandrud
# 12 November 2013
##########

# Load packages
library(reshape2)
library(stringr)
library(DataCombine)

setwd("~/Dropbox/AMCProject/FailuresAndFederalism/IndvBankDataAnalysis/Data/Spain/CommercialBanks/Domestic/")


Files <- list.files()

for (i in Files){
  Temp <- read.csv(i)
  Temp <- Temp[-1, ]
  Temp$Var <- gsub("[0-9]", "", Temp$Var)
  Temp$Var <- gsub("\\.", "", Temp$Var)
  Temp$Var <- str_trim(Temp$Var)
  TempMolten <- melt(Temp, id.vars = 'Var')
  names(TempMolten) <- c("variable", "bank", "value")
  TempMolten <- MoveFront(TempMolten, "bank")
  TempMolten$value <- gsub(",", "", TempMolten$value)
  TempMolten$value <- as.numeric(TempMolten$value)
  TempCast <- dcast(TempMolten, bank ~ variable, value.var = "value", fun.aggregate=sum)
  
  # Create month variable
  i <- gsub("\\.csv", "", i)
  i <- gsub("2", "-2", i)
  TempCast$date <- i
  TempCast <- MoveFront(TempCast, "date")
}
