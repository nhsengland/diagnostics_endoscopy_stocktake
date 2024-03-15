# Load libraries
library(dplyr)
library(readxl)

# Bring in data
endoscopy_stocktake <- read_excel("C:/Users/martin.bloyce/OneDrive - NHS England/Restricted Library/SE/Analysis/Diagnostics/Endoscopy Stocktake/Endoscopy Stocktake Database with pivot table.xlsx", 
                                  sheet = "Backing Data", skip = 2)

# Subset data
sites <- subset(endoscopy_stocktake, endoscopy_stocktake$Question == "What is the units postcode", select = c('Unit Name', 'Free comment'))

colnames(sites) <- c("sitename", "postcode")
# Subset locations

# Sample data of acute hospitals in the south east england
acute_hospitals <- data.frame(
  hospital = sites$sitename,
  postcode = sites$postcode
)