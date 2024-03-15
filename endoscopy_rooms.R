# Create table for number of endoscopy rooms (excluding planned)

library(dplyr)

# Bring in data
endoscopy_stocktake <- read_excel("C:/Users/martin.bloyce/OneDrive - NHS England/Restricted Library/SE/Analysis/Diagnostics/Endoscopy Stocktake/Endoscopy Stocktake Database with pivot table.xlsx", 
                                  sheet = "Backing Data", skip = 2)

# Rename columns for ease
colnames(endoscopy_stocktake) <- c('ICB', 'Trust', 'Directorate', 'UnitName', 'QuestionKey','Question', 'Value', 'ValueType', 'SetResponseAnswer', 'FreeText', 'blank1', 'blank2', 'blank3')

# Create table at region level
constraints_rooms_region <- endoscopy_stocktake %>%
  subset(.,
         Question == 'How many endoscopy rooms are in use at this unit? include any that are only used from time to time') %>%
  summarise(sum(as.numeric(Value)))
  constraints_rooms_region$Loc <- 'SE Region'
  constraints_rooms_region <- constraints_rooms_region[,c("Loc", "sum(as.numeric(Value))")]
  colnames(constraints_rooms_region) <- c('Loc', 'Value')

# Create table at ICB level
constraints_rooms_ICB <- endoscopy_stocktake %>%
                        subset(.,
                            Question == 'How many endoscopy rooms are in use at this unit? include any that are only used from time to time') %>%
                        group_by(ICB) %>%
                        summarise(sum(as.numeric(Value)))
colnames(constraints_rooms_ICB) <- c('Loc', 'Value')

# Create table at unit level ## could rename 'trust' to 'unit? ##
constraints_rooms_trust <- endoscopy_stocktake %>%
  subset(.,
         Question == 'How many endoscopy rooms are in use at this unit? include any that are only used from time to time') %>%
  select(c(UnitName, Value))
colnames(constraints_rooms_trust) <- c('Loc', 'Value')

# Combine tables to create a master
constraints_rooms_master <- rbind(constraints_rooms_region, constraints_rooms_ICB, constraints_rooms_trust)

# Drop not needed objects
rm(constraints_rooms_ICB, constraints_rooms_region, constraints_rooms_trust)
