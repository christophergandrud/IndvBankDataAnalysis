#################
# Clean and explore FFIEC data
# Christopher Gandrud
# 8 October 2013
#################

library(xtable)

# Import data 
CRLoad <- function(dir){
  if("README.txt" %in% list.files(path = dir)) unlink("README.txt")
  FNames <- list.files(path = dir)
  CR <- read.delim(FNames[1], stringsAsFactors = FALSE)
  for (i in 2:length(FNames)){
    temp <- read.delim(FNames[i], stringsAsFactors = FALSE)
    CR <- merge(CR, temp)
  }
  CR
}

Merged <- CRLoad('~/Desktop/FFIECTest/FFIEC CDR Call Bulk Subset of Schedules 2013 (1)/')

# Create modified code book
CRCodeBook <- function(x){
  symbol <- names(x)
  # First row has short descriptions #
  descript <- as.character(x[1, ])
  CB <- data.frame(symbol, descript, stringsAsFactors = FALSE)
  CB <- subset(CB, descript != "")
  CB <- subset(CB, descript != "NA")
  CB
}

CodeBook <- CRCodeBook(Merged)

Main <- "
## Call report notes

---

Quarterly call reports for all [very National Bank, State Member Bank and insured Nonmember Bank](http://www2.fdic.gov/Call_TFR_Rpts/inform.asp) since 2001 are available for download in tab delimited format from the [Federal Financial Institutions Examination Council (FFIEC)](https://cdr.ffiec.gov/public/PWS/DownloadBulkData.aspx). Note that thrifts and credit unions are not included in this data set.

### Variable coding scheme

The call reports are all assigned the same series mnemonic: CALL. The variables are assigned a four letter four number. The [letter mnemonics](http://www.federalreserve.gov/apps/mdrm/pdf/Call_2001.pdf denote): 

- RCFD: consolidated balance sheet items

- RCON: domestic data balance sheet items

- RCFN: foreign data balance sheet items ([reporting required](http://www.ffiec.gov/pdf/FFIEC_forms/FFIEC031_FFIEC041_201306_i.pdf) if more than 10% of consolidated total assets, total revenues, or net income come from overseas)

- RIAD: All income statement items 

Banks use forms FFIEC 031, FFIEC 002, FR 2886b to report the items.

The following table includes all of the codes in the FFIEC Call Reports for 2013 and a short description included in the data. 

At this point it looks like we need to look through the forms' supplementary materials to get the long descriptions.  
\n\n
"

CBTable <- print(xtable(CodeBook), type = 'html')

cat(Main, CBTable, file = "~/Dropbox/AMCProject/FailuresAndFederalism/IndvBankDataAnalysis/Data/FFIEC_Explaination.md")
