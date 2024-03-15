library(dplyr)
library(readxl)

# Import data
endoscopy_stocktake <- read_excel("C:/Users/martin.bloyce/OneDrive - NHS England/Restricted Library/SE/Analysis/Diagnostics/Endoscopy Stocktake/Endoscopy Stocktake Database with pivot table.xlsx", 
                                                            sheet = "Backing Data", skip = 2)

# Rename columns for ease
colnames(endoscopy_stocktake) <- c('ICB', 'Trust', 'Directorate', 'UnitName', 'QuestionKey','Question', 'Value', 'ValueType', 'SetResponseAnswer', 'FreeText', 'blank1', 'blank2', 'blank3')

# Create unit level data
lists_per_week <- subset(endoscopy_stocktake, 
                         endoscopy_stocktake$QuestionKey == '27',
                         select = c('UnitName', 'Value')
                         )
colnames(lists_per_week) <- c("Loc", "Value")  # Rename columns

# Create region level data
lists_per_week_region <- endoscopy_stocktake %>%
  subset(., 
         QuestionKey == '27',
         select = c('ICB','Trust', 'UnitName', 'Value'))

lists_per_week_region <- lists_per_week_region %>%
  mutate(Value = as.numeric(Value))


lists_per_week_region <- lists_per_week_region %>%
  group_by(ICB) %>%
  summarise(Value = sum(Value))

# Create ICB level data
lists_per_week_ICB <- endoscopy_stocktake %>%
                          subset(., 
                                QuestionKey == '27',
                         select = c('ICB', 'Value'))
                         
lists_per_week_ICB <- lists_per_week_ICB %>%
                          mutate(Value = as.numeric(Value))


lists_per_week_ICB <- lists_per_week_ICB %>%
  group_by(ICB) %>%
  summarise(Value = sum(Value))

colnames(lists_per_week_ICB) <- c("Loc", "Value")  # Rename columns

# Bring tables together
lists_per_week <- rbind(lists_per_week_ICB, lists_per_week)

# Create 120% activity calculation ## NEEDS WORK ##
  lists_per_week$Value <- as.numeric(lists_per_week$Value)
  lists_per_week$'120%Activity' <- round(lists_per_week$Value * 1.2)
  
# Create 5% increase models
  #2024-25
lists_per_week <- lists_per_week %>%
                      mutate(
                        ModelA = round(Value * 1.05)
                      )

#2025-26
lists_per_week <- lists_per_week %>%
  mutate(
    ModelB = round(ModelA * 1.05)
  )

#2026-27
lists_per_week <- lists_per_week %>%
  mutate(
    ModelC = round(ModelB * 1.05)
  )

# Gap baseline to 2026-27
lists_per_week <- lists_per_week %>%
  mutate(
    Gap = round(ModelC - Value)
  )