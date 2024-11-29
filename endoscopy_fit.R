# install.packages('remotes')
remotes::install_github('nhs-r-community/NHSRtheme')

# Import libraries
library(dplyr)
library(ggplot2)
library(NHSRtheme)
library(readxl)
library(tidyverse)
library(lubridate)

# Import data
# raw_data_fit <- read_excel("C:/Users/martin.bloyce/OneDrive - NHS England/Restricted Library/SE/Analysis/Diagnostics/Endoscopy Stocktake/FIT reports summary Q1-Q3 2023-2024.xlsx", 
#                            sheet = "2324 FIT returns raw data")

raw_data_fit <- read_excel("C:/Users/NM2.JM9QNXTK/OneDrive - NHS/Analysis/Diagnostics/Endoscopy Stocktake/FIT reports summary Q1-23 to Q2-24.xlsx", 
                           sheet = "23_25_FIT")

#Lookups <- read_excel("C:/Users/martin.bloyce/OneDrive - NHS England/Reference/Lookups.xlsx", 
#                      sheet = "ProviderMapping")
Lookups <- read_excel("C:/Users/NM2.JM9QNXTK/OneDrive - NHS/Analysis/Diagnostics/Endoscopy Stocktake/Lookups.xlsx", 
                                            sheet = "ProviderMapping")
colnames(Lookups) <- c('provider', 'orgshortname', 'orgcode', 'systemcode', 'systemname', 'systemshortname', 'orgtype', 'region')
Lookups <- Lookups[ , c('provider','orgshortname')]

# Rename columns for ease
colnames(raw_data_fit) <- c('cancerAlliance', 'date', 'metric', 'FITBand', 'endDate', 'provider', 'value')

# Set regional cancer alliances filter
se_cancer_alliances <- c('Kent & Medway', 'Surrey and Sussex', 'Thames Valley')

# Filter for south east providers only
raw_data_fit <- raw_data_fit[raw_data_fit$cancerAlliance %in% se_cancer_alliances, ]

# Merge to bring in short provider names (better for chart)
raw_data_fit <- merge(raw_data_fit, Lookups)

metric <- 'The number of colonoscopies performed on the LGI FDS pathway'

# Filter for colonoscopy only
raw_data_fit <- raw_data_fit[raw_data_fit$metric %in% metric, ]

# Summarise data for all three quarters
summary_fit <- raw_data_fit %>%
  group_by(provider, orgshortname, FITBand) %>%
  summarise(value = sum(value))
    
# Pivot table
fit_table <- summary_fit %>%
  pivot_wider(
  names_from = FITBand,
  values_from = value
)

# Reorder columns
fit_table <- fit_table[, c(1,2,4,3,5,6,7,8)]

# Create calculated columns
totalFIT <- rowSums(fit_table[sapply(fit_table, is.numeric)], na.rm=TRUE)  # Sum numeric columns to get total
fit_table <- cbind(fit_table, totalFIT)
colnames(fit_table)[9] <- 'totalFIT'  # Rename total column as for some reason doesn't pull through...
fit_table <- fit_table[,-1]  # Remove 'provider'

fit_table$percentageFit10 <- round(rowSums(fit_table[ , c(3,4)], na.rm=TRUE) / totalFIT, 2) * 100  # FIT10 percentage


# Create trend table
trend_fit <- raw_data_fit %>%
  group_by(endDate, provider, orgshortname, FITBand) %>%
  summarise(value = sum(value)) %>%
  pivot_wider(
    names_from = FITBand,
    values_from = value
  )
trend_fit$endDate <- as.Date(paste0("01-", trend_fit$endDate), format = "%d-%b-%y")  # Set 'month' to date type 
trend_fit <- trend_fit %>% 
  arrange(endDate)
  
totalFIT_trend <- rowSums(trend_fit[sapply(trend_fit, is.numeric)], na.rm=TRUE)  # Sum numeric columns to get total
trend_fit <- cbind(trend_fit, totalFIT_trend)
colnames(trend_fit)[10] <- 'totalFIT'  # Rename total column as for some reason doesn't pull through...
trend_fit <- trend_fit[,-2]  # Remove provider
backup_trend_fit <- trend_fit
# this is where it goes wrong
trend_fit$percentageFit10 <- round(rowSums(trend_fit[ , c(3,5)], na.rm=TRUE) / totalFIT_trend, 2)  # FIT10 percentage

# Create ordered FIT band list for chart
summary_fit$FITBand <- factor(summary_fit$FITBand, 
                              levels = c('No FIT available',
                                         'FIT not appropriate (anal/rectal mass or anal ulceration)',
                                         'FIT available but no numerical value',
                                         '<10ug/mg',
                                         '10-100ug/mg',
                                         '>100ug/mg'
                                         ))

colnames(summary_fit)[2] <- 'Organisation' # rename orgshortname 
colnames(summary_fit)[4] <- 'Value' # rename value 
# Create ggplot chart
a <- summary_fit %>%
  ggplot(., aes(fill = FITBand, y = Value, x = Organisation)) +
  geom_bar(position = "fill", stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_nhs()

a

colnames(trend_fit)[1] <- 'Date' # rename endDate 
colnames(trend_fit)[2] <- 'Organisation' # rename orgshortname 
colnames(trend_fit)[10] <- 'FIT10 %' # rename endDate 
# Create trend lines for providers
b <- trend_fit %>%
  ggplot(., aes(x=`Date`, y=`FIT10 %`, colour=Organisation, group=Organisation)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_nhs()
  
b

rm(list = setdiff(ls(), c("a", "fit_table", "b")))
