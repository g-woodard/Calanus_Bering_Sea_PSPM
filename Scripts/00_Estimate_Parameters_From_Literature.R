
#This Script uses respiration data from Ikeda et al. 2017 to estimate the allometric scalar and exponent


#01. Calculate metabolic allometric scalar and exponent
#------------------------

#Read in Data
library(tidyverse)
library(here)
root <- here()
Ikeda_2007_Respiration_Data_original = read.csv(paste0(root, "/Data/Ikeda_2007_Respiration_Data.csv"))

#Check number of observations at common temperatures
length(which(Ikeda_2007_Respiration_Data_original[,4] == 2))
length(which(Ikeda_2007_Respiration_Data_original[,4] == 3))
length(which(Ikeda_2007_Respiration_Data_original[,4] == 1.5))

#Need observations to be from same reference temperature
#Use and extract observations with a reference temperature of 2 Celsius
#2 Celsius was most common reference temperature
Ikeda_2007_Respiration_Data_2_C = Ikeda_2007_Respiration_Data_original[which(Ikeda_2007_Respiration_Data_original[,4] == 2),] #only use data measured at 2 C

#Convert to micrograms of O2
Ikeda_2007_Respiration_2C = (Ikeda_2007_Respiration_Data_2_C[,5]*24)*10^(-6)*(1/22.4)*15.998*10^6 
#Convert to micrograms
Ikeda_2007_Dry_Mass_2C = (Ikeda_2007_Respiration_Data_2_C[,6]*1000)
#Extract temperature (should all be 2 celsius)
Ikeda_2007_Temp_2C = (Ikeda_2007_Respiration_Data_2_C[,4])


t2 = lm(log(Ikeda_2007_Respiration_2C)~log(Ikeda_2007_Dry_Mass_2C)) #Respiration data at 2 C

summary(t2)
exp(t2[["coefficients"]][["(Intercept)"]])

#02. Calculate Ingestion allometric scalar and exponent
#-------------------------------------

#Read in Data

Saiz_Calbert_2007_max_ingestion_vs_size_15_C = read.csv(paste0(root,"/Data/Saiz_Calbet_2007_Max_ingestion_Vs_Size_15_C.csv"))
#max ingestion rate in µg C per day
#body weight in µg C
#Food Concentration in % body weight

#Filter out carnivorous copepods

Saiz_Calbert_2007_max_ingestion_vs_size_15_C <- Saiz_Calbert_2007_max_ingestion_vs_size_15_C %>%
  filter(Diet == "H")
  

plot(Saiz_Calbert_2007_max_ingestion_vs_size_15_C[,"Max.Ingestion"]~Saiz_Calbert_2007_max_ingestion_vs_size_15_C[,"Body.Size"]) 

plot(log(Saiz_Calbert_2007_max_ingestion_vs_size_15_C[,"Max.Ingestion"])~log(Saiz_Calbert_2007_max_ingestion_vs_size_15_C[,"Body.Size"]))

log_max_ingest_15_C = log(Saiz_Calbert_2007_max_ingestion_vs_size_15_C[,"Max.Ingestion"]) #Leave in micrograms of carbon per liter. Conversion to zooplankton dryweight happens after ingestion in stage structured model

#In the model structure, the allometric relationship uses copepod dry weight, so need to convert Saiz and Calbet weights from Carbon weight to dry weight using Uye conversion of 0.455

log_body_size_15_C = log(Saiz_Calbert_2007_max_ingestion_vs_size_15_C[,"Body.Size"]/0.455) #Convert from Carbon to zooplankton dry weight using a value of 0.455 is conversion factor from Uye 1982 where COPEPOD dry weight is composed of 45.5% Carbon

#Food concentration is reported in percent body weight so multiply concentration by weight first to get food concentration in terms of Carbon
log_food_15_C = log(Saiz_Calbert_2007_max_ingestion_vs_size_15_C[,"Body.Size"]*(Saiz_Calbert_2007_max_ingestion_vs_size_15_C[,"Food.Concentration"]/100) )

#Now the regression is is log(Carbon ingested) ~ log(Copepod DRY WEIGHT)

plot(log_max_ingest_15_C~log_body_size_15_C)
plot(log_max_ingest_15_C~log_food_15_C)

#Size and Resource Dependent
lm1 = lm(log_max_ingest_15_C ~ log_body_size_15_C + log_food_15_C)

#Size dependence only
#lm1 = lm(log_max_ingest_15_C~log_body_size_15_C)

summary(lm1)
exp(lm1[["coefficients"]][["(Intercept)"]])

