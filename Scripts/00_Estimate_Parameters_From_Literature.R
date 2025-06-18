
#This Script uses respiration data from Ikeda et al. 2017 to estimate the allometric scalar and exponent


#01. Calculate metabolic allometric scalar and exponent
#------------------------

#Read in Data

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
Ikeda_2007_Respiration_2C = (Ikeda_2007_Respiration_Data_2_C[,5]*24)*10^(-6)*(22.4)*15.998*10^6 
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

plot(Saiz_Calbert_2007_max_ingestion_vs_size_15_C[,"Max.Ingestion"]~Saiz_Calbert_2007_max_ingestion_vs_size_15_C[,"Body.Size"]) 

plot(log(Saiz_Calbert_2007_max_ingestion_vs_size_15_C[,"Max.Ingestion"])~log(Saiz_Calbert_2007_max_ingestion_vs_size_15_C[,"Body.Size"]))

log_max_ingest_15_C = log(Saiz_Calbert_2007_max_ingestion_vs_size_15_C[,"Max.Ingestion"]) #Leave in micrograms of carbon per liter. Conversion to zooplankton dryweight happens after ingestion in stage. structured model
log_body_size_15_C = log(Saiz_Calbert_2007_max_ingestion_vs_size_15_C[,"Body.Size"]) #Leave in micrograms of carbon per liter. Conversion to zooplankton dryweight happens after ingestion in stage. structured model


plot(log_max_ingest_15_C~log_body_size_15_C)

lm1 = lm(log_max_ingest_15_C~log_body_size_15_C)

summary(lm1)
exp(lm1[["coefficients"]][["(Intercept)"]])
