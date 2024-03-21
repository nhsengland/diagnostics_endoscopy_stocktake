# Testing this!
library(dplyr)
library(lubridate)
library(readxl)

# Import population data 
population_data_master <- read_excel("C:/Users/GeorginaCable/NHS England/Performance Analysis Team - SE/Analysis/Inequalities/Catchment Demographics/2022 Trust Catchment Populations Worksheet.xlsx", 
                               sheet = "Trust Analysis")


Org_Code <- c('RHW','RTH','RXQ','RDU',
              'R1F','RHM','RHU','RN5',
              'RN7','RPA','RVV','RWF',
              'RA2','RTK','RTP','RPC',
              'RXC','RYR')

Prov_Short <- c('RBH','OUH','BHT','Frimley',
                         'IOW','UHS','PHU','HHFT',
                         'DGT','MFT','EKH','MTW',
                         'RSCH','ASP','SASH','QVH',
                         'ESH','UHSX')

ICB_Code <- c(rep('QU9',3),'RDU',rep('QRL',4),rep('QKS',4),rep('QXU',3),rep('QNX',3))

ICB_Short <- c(rep('BOB',3),'Frimley',rep('HIOW',4),rep('KM',4),rep('Surrey',3),rep('Sussex',3))

names_lookup <- tibble(Org_Code,Prov_Short,ICB_Code,ICB_Short)

rm(Org_Code,Prov_Short,ICB_Code,ICB_Short)

population_data_master <- left_join(population_data_master,
                       names_lookup,
                       by = c('TrustCode'='Org_Code'))

population_data <- population_data_master %>% 
  filter(CatchmentYear == "2020") %>% 
  filter(AdmissionType == "All Admissions") %>% 
  filter(!is.na(ICB_Code))

pop_data <- population_data %>% 
  select(, -c(CatchmentYear, AdmissionType, TrustType, Sex, Sex_Desc, LCI, UCI, Variance, PatientsAdmitted, Prov_Short)) %>% 
  select(ICB_Code, ICB_Short, TrustCode, TrustName, Prov_Short, Age, Catchment) %>% 
  mutate(Over50 = if_else(Age %in% c("00-04","05-09","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49"), "<50","50+")) %>% 
  select(, -c(Age))

pop_data2 <- pop_data %>% 
  group_by(ICB_Code, ICB_Short, TrustCode, TrustName, Over50) %>% 
  summarise(Pop = sum(Catchment))
  
