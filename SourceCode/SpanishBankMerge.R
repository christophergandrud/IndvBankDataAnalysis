###########
# Combine Spanish Commercial Bank Data
# Christopher Gandrud
# 18 November 2013
##########

# Load packages
library(gdata)
library(reshape2)
library(stringr)
library(DataCombine)
library(plyr)
library(lubridate)

setwd("~/Dropbox/AMCProject/FailuresAndFederalism/IndvBankDataAnalysis/Data/Spain/CommercialBanks/Domestic/")

Files <- list.files()

Combined <- data.frame()

for (i in Files){
  Temp <- read.csv(i, stringsAsFactors = FALSE, fileEncoding = "latin1")
  ColNames <- Temp[1, 2:length(Temp)]
  ColNames <- c("Var", ColNames)
  names(Temp) <- ColNames
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
  i <- gsub("201-2", "2012", i)
  i <- paste0("1-", i)
  TempCast$date <- i
  TempCast <- MoveFront(TempCast, "date")
  
  # Merge Together
  Combined <- rbind.fill(Combined, TempCast)
  
  # Remove temps
  rmExcept("Combined", message = FALSE)
}

# Clean Data
Combined$date <- dmy(Combined$date)
Combined <- Combined[order(Combined$bank, Combined$date), ]


#### Tests ####


