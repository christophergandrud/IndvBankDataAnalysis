###########
# Combine Spanish Commercial Bank Data
# Christopher Gandrud
# 2 December 2013
##########

# Load packages
library(gdata)
library(reshape2)
library(stringr)
library(DataCombine)
library(plyr)
library(lubridate)

# Create translated variable data frame
source('~/Dropbox/AMCProject/FailuresAndFederalism/IndvBankDataAnalysis/SourceCode/SpanishCommercialTranslation.R')

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
  Temp <- FindReplace(data = Temp , Var = 'Var', replaceData = Translated, from = 'from', to = 'to')
  TempMolten <- melt(Temp, id.vars = 'Var')
  names(TempMolten) <- c("variable", "bank", "value")
  TempMolten <- MoveFront(TempMolten, "bank")
  TempMolten$value <- gsub(",", "", TempMolten$value)
  TempMolten$value <- as.numeric(TempMolten$value)
  TempCast <- dcast(TempMolten, bank ~ variable, value.var = "value", fun.aggregate = sum)
  
  # Create month variable
  i <- gsub("\\.csv", "", i)
  i <- gsub("2", "-2", i)
  i <- gsub("201-2", "2012", i)
  i <- paste0("1-", i)
  TempCast$date <- i
  TempCast <- MoveFront(TempCast, "date")
  
  # Merge Together
  Combined <- rbind.fill(Combined, TempCast)
}

# Remove temps
rmExcept("Combined", "Translated", message = FALSE)

# Clean Data
Combined$date <- dmy(Combined$date)
Combined <- Combined[order(Combined$bank, Combined$date), ]


#### Tests ###
CombinedSub <- subset(Combined, bank != "TOTAL BANCOS")
CombinedSub$One <- 1
CombinedSub <- ddply(CombinedSub, .(date), transform, total = sum(One))
CountSub <- CombinedSub[!duplicated(CombinedSub[, 1]), ]

library(ggplot2)

ggplot(CountSub, aes(date, total)) + geom_line() + theme_bw()


Test <- subset(CombinedSub, ACTIVOS.NO.CORRIENTES.EN.VENTA > 3000000)
Test <- subset(Combined, bank == "BANCO GUIPUZCOANO")

CombinedSubSale <- subset(CombinedSub, ACTIVOS.NO.CORRIENTES.EN.VENTA > 50000)

ggplot(CombinedSub, aes(date, ACTIVOS.NO.CORRIENTES.EN.VENTA, colour = bank)) + geom_line() + xlab("") + theme_bw()

CombinedSub$NonCurrentPerTotal <- CombinedSub$ACTIVOS.NO.CORRIENTES.EN.VENTA/CombinedSub$TOTAL.ACTIVO

ggplot(CombinedSub, aes(date, Valores.representativos.de.deuda, colour = bank)) + geom_line() + xlab("") + theme_bw()


CombinedPSub <- subset(Combined, PASIVOS.ASOCIADOS.CON.ACTIVOS.NO.CORRIENTES.EN.VENTA < 100000)
ggplot(CombinedPSub, aes(date, PASIVOS.ASOCIADOS.CON.ACTIVOS.NO.CORRIENTES.EN.VENTA, colour = bank)) + 
  geom_line() + theme_bw()



write.csv(Combined, "~/Dropbox/AMCProject/FailuresAndFederalism/IndvBankDataAnalysis/Data/Spain/SpainCommercialMerged.csv")
