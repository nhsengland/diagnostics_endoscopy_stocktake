library(dplyr)
library(lubridate)
library(readxl)
library(knitr)
library(kableExtra)
library(scales)

# Import data from file
diagnostics_data <- read_excel("C:/Users/martin.bloyce/OneDrive - NHS England/Restricted Library/SE/Analysis/Diagnostics/Diagnostics Dashboard/Diagnostics Dashboard DATA.xlsx", 
                               col_types = c("text", "text", "skip", 
                                             "text", "skip", "skip", "skip", "skip", 
                                             "skip", "numeric", "skip", "skip", 
                                             "skip"))

endoscopy_stocktake <- read_excel("C:/Users/martin.bloyce/OneDrive - NHS England/Restricted Library/SE/Analysis/Diagnostics/Endoscopy Stocktake/Endoscopy Stocktake Database with pivot table.xlsx", 
                                  sheet = "Backing Data", skip = 2)

# Import provider names
Lookups <- read_excel("C:/Users/martin.bloyce/OneDrive - NHS England/Reference/Lookups.xlsx", 
                      sheet = "ProviderMapping")
colnames(Lookups) <- c('orgname', 'orgshortname', 'orgcode', 'systemcode', 'systemname', 'systemshortname', 'orgtype', 'region')
Lookups <- Lookups[ , c('orgcode','orgshortname', 'systemshortname')]

# Clean up data
colnames(diagnostics_data) <- c('month', 'test', 'orgcode', 'activity')  # Rename columns
diagnostics_data <- subset(diagnostics_data,
                           test %in% c('COLONOSCOPY', 'GASTROSCOPY'))  # Subset endoscopy tests
diagnostics_data$month <- as.Date(paste0("01-", diagnostics_data$month), format = "%d-%b-%y")  # Set 'month' to date type
diagnostics_data <- diagnostics_data %>%
  mutate(orgcode = ifelse(orgcode == "RXH", "RYR", orgcode))  # Merge Brighton and UH Sussex

# Bring in provider and ICB names
diagnostics_data <- left_join(diagnostics_data, Lookups, by='orgcode')

# Create 2019 baseline
baseline <- subset(diagnostics_data,
                   month < '2020-01-01')
baseline <- baseline %>%
  group_by(orgshortname) %>%
  summarise(activity_baseline = round(sum(activity)/12, 0))
colnames(baseline) <- c("Loc", "Baseline")  # Rename columns to match and make rbind easier

# Do same for ICB
baseline_ICB <- subset(diagnostics_data,
                       month < '2020-01-01')
baseline_ICB <- baseline_ICB %>%
  group_by(systemshortname) %>%
  summarise(activity_baseline = round(sum(activity)/12, 0))
colnames(baseline_ICB) <- c("Loc", "Baseline")  # Rename columns to match and make rbind easier

# Create last 3 months average
last_three_months <- subset(diagnostics_data,
                            month > max(month) %m-% months(3))
last_three_months <- last_three_months %>%
  group_by(orgshortname) %>%
  summarise(activity = round(sum(activity)/3, 0))
colnames(last_three_months) <- c("Loc", "Activity")

# Do same for ICB
last_three_months_ICB <- subset(diagnostics_data,
                                month > max(month) %m-% months(3))
last_three_months_ICB <- last_three_months_ICB %>%
  group_by(systemshortname) %>%
  summarise(activity = round(sum(activity)/3, 0))
colnames(last_three_months_ICB) <- c("Loc", "Activity")  # Rename columns to match and make rbind easier

# Merge for calculations - doing in this step as preserves the table order
ICB <- merge(baseline_ICB, last_three_months_ICB)
prov <- merge(baseline, last_three_months)

# rbind ICB and provider tables
master <- rbind(ICB, prov)

# Merge and create % calculation
master$percent_of_baseline <- round((master$Activity / master$Baseline) *100)

# Replace 'na' as the total independant sector value
master$Loc[is.na(master$Loc)] <- "Independant Sector"

# Drop not needed objects
rm(list = setdiff(ls(), "master"))