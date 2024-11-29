# Import libraries
library(dplyr)
library(ggplot2)
library(NHSRtheme)
library(readxl)
library(tidyverse)

# Import data
# endoscopy_stocktake <- read_excel("C:/Users/martin.bloyce/OneDrive - NHS England/Restricted Library/SE/Analysis/Diagnostics/Endoscopy Stocktake/Endoscopy Stocktake Database with pivot table.xlsx", 
#                                                             sheet = "Backing Data", skip = 2)
endoscopy_stocktake <- read_excel("C:/Users/GeorginaCable/OneDrive - NHS/Analysis/Diagnostics/Endoscopy Stocktake/240916 Endoscopy Stocktake Analysis.xlsx", 
                                  sheet = "Backing Data", skip = 2)

# Rename column headers
colnames(endoscopy_stocktake) <- c('ICB', 'Trust', 'Directorate', 'UnitName', 'QuestionKey','Question', 'Value', 'ValueType', 'SetResponseAnswer', 'FreeText', 'blank1', 'blank2', 'blank3')

# Set value to numeric
endoscopy_stocktake$Value <- as.numeric(endoscopy_stocktake$Value)

# Subset to 'lists' questions
lists_questions <- c('27','28','29','30','31','32')
lists <- subset(endoscopy_stocktake, 
                endoscopy_stocktake$QuestionKey %in% lists_questions,
                select = c('UnitName', 'Question', 'Value')
                )
lists <- lists %>% distinct()  # Remove duplicated rows

# Create pivot table for lists
lists <- lists %>%
  pivot_wider(
    names_from = Question,
    values_from = Value
  )

# Rename lists col headers
colnames(lists) <- c('UnitName','TotalLists','NonGILists','BCSPLists','StandardGILists','ERCP_EUSLists','OtherGILists')

colnames(lists)[1] <- 'Unit' # rename Loc 

# Unpivot for chart
lists_chart_data <- subset(lists, select = -2)

lists_chart_data <- lists_chart_data %>% 
  pivot_longer(
    cols = !Unit,
    names_to = "ListType",
    values_to = "Value"
)

lists_chart <- lists_chart_data %>%
  ggplot(., aes(x=Unit, y=Value, fill=`ListType`)) +
  geom_bar(position = "fill", stat = "identity", show.legend = TRUE) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_nhs() +
  coord_flip()

lists_chart

#Remove unnecessary tables
rm(list = setdiff(ls(), c("lists_chart", "lists")))
  