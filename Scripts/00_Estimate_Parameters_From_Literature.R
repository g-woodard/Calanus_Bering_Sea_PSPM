
#This Script uses respiration data from Ikeda et al. 2007 to estimate the allometric scalar and exponent


#01. Calculate metabolic allometric scalar and exponent
#------------------------

#Read in Data
library(tidyverse)
library(here)
root <- here()

round_half_up <- function(x, n = 0) {
  posneg = sign(x)
  z = abs(x) * 10^n
  z = z + 0.5
  z = trunc(z)
  z = z / 10^n
  z * posneg
}


#Lower precision because there is some rounding in the old dataset. If precision is too high, the check won't work because the values will be slightly different
Ikeda_2007_Respiration_Data_old = read.csv(paste0(root, "/Data/Ikeda_2007_Respiration_Data_old.csv")) %>%
  select(-Ref.data) %>%
  mutate(Depth.water..m. = round_half_up(Depth.water..m., 0),
         Treat.temp....C. = round_half_up(Treat.temp....C., 1),
         Resp.O2.ind...µl...h. = round_half_up(Resp.O2.ind...µl...h., 2),
         BM.dry..mg. = round_half_up(BM.dry..mg., 1),
         BM.N..mg. = round_half_up(BM.N..mg., 1),
         BM.C..mg. = round_half_up(BM.C..mg., 1) )  %>%
  group_by(Depth.water..m., Species) %>%
  arrange(Depth.water..m., Species)


Ikeda_2007_Respiration_Data_new = read.csv(paste0(root, "/Data/Ikeda_2007_Respiration_Data.csv")) %>%
  select(-c("No.", "O2.Sat", "Ref.data"))%>%
  mutate(Depth.water..m. = round_half_up(Depth.water..m., 0),
         Treat.temp....C. = round_half_up(Treat.temp....C., 1),
         Resp.O2.ind...µl...h. = round_half_up(Resp.O2.ind...µl...h., 2),
         BM.dry..mg. = round_half_up(BM.dry..mg., 1),
         BM.N..mg. = round_half_up(BM.N..mg., 1),
         BM.C..mg. = round_half_up(BM.C..mg., 1) )  %>%
  group_by(Depth.water..m., Species) %>%
  arrange(Depth.water..m., Species) 

#Verify all data in old dataset are also present in new dataset (counts should match). If not, identify cause.
Ikeda_2007_Respiration_Data_same <- Ikeda_2007_Respiration_Data_new %>%
  inner_join(Ikeda_2007_Respiration_Data_old) #


#Identify any data in old dataset not present in new dataset, If present, identify cause.
Ikeda_2007_Respiration_Data_unique <- Ikeda_2007_Respiration_Data_old %>%
  anti_join(Ikeda_2007_Respiration_Data_new) #

#The 2 discrepancies found above are just minor differences in rounding. NOT AN ISSUE

#Read in data with full precision

Ikeda_2007_Respiration_Data_full_precision = read.csv(paste0(root, "/Data/Ikeda_2007_Respiration_Data.csv")) %>%
  group_by(Depth.water..m., Species) %>%
  arrange(Depth.water..m., Species) 

#Check number of observations at common temperatures

temperature_treatment_counts <- Ikeda_2007_Respiration_Data_full_precision  %>%
  group_by(Treat.temp....C.) %>%
  summarize(count = n()) 

#Need observations to be from same reference temperature
#Use and extract observations with a reference temperature of 1.5 Celsius
#1.5 Celsius was most common reference temperature
#1.5 Celsius is 274.65 Kelvin

Ikeda_2007_Respiration_Data_1.5_C = Ikeda_2007_Respiration_Data_full_precision[which(Ikeda_2007_Respiration_Data_full_precision[,'Treat.temp....C.'] == 1.5),] #only use data measured at 1.5 C


#IMPORTANT: The 1 mole of O2 = 22.4 L is at STP. This is at 1.5 Celsius so need to adjust using ideal gas law
#Use Ideal gas law to convert at any temperature: Here our temperature is 274.65 Kelvin


R = 0.082057366080960
#n = (PV)/(RT)
#P = pressure, V = volume, R = Ideal gas constant, T = temperature
#n = number of moles of gas in some volume

#Ikeda et al. data was measured at surface under 1 atm, measurements given by Ikeda et al. in microliters which we have converted to LITERS
n = (1*1)/(R*274.65) #So this is number of moles of gas in 1 LITER at 1 atm at 274.65 Kelvin

#Convert to micrograms of O2/INDIVIDUAL/day from MICRO LITERS O2/INDIVIDUAL/hr
#Ikeda_2007_Respiration_1.5C = (unlist(Ikeda_2007_Respiration_Data_1.5_C[,"Resp.O2.ind...µl...h."])*24)*(1/1000000)*(1/22.4)*(15.998*2)*(1000000) 

Ikeda_2007_Respiration_1.5C = (unlist(Ikeda_2007_Respiration_Data_1.5_C[,"Resp.O2.ind...µl...h."])*24)*(1/1000000)*(n/1)*(15.998*2)*(1000000) 

#Ikeda et al. 2000 note that pressure increases with depth at 1 atmosphere or 101.3 kPa per 10 m depth). 
#Ikeda et al. 2006a has details of procedures

#Ikeda T, Sano F, Yamaguchi A, Matsuishi T (2006a) Metabolism
#of mesopelagic and bathypelagic copepods in the western
#North Pacific Ocean. Mar Ecol Prog Ser 322:199–211

#Ikeda et al. 2006a methodology is for in situ temperatures. Zooplankton were sampled at depth strata with a vertical net, and brought back up to surface for experiments (within 17 minutes of net closure)
#Seawater was collected at the depth strata with 20 L Niskin bottles just prior to sampling. However, because experiments were performed at the surface they would now be under standard pressure of 1 atmosphere.
#Ikeda et al. 2006a incubated bottles for 24 hours at 1 atm at 3C for M zone, 2C for UB zone and 1.5C for LB zone
#Used Winkler titration method on subsamples to obtain O2 concentration


#Convert TO micrograms FROM milligrams
Ikeda_2007_Dry_Mass_1.5C = (unlist(Ikeda_2007_Respiration_Data_1.5_C[,"BM.dry..mg."])*1000)


#Extract temperature (should all be 1.5 celsius)

Ikeda_2007_Temp_1.5C = unlist(Ikeda_2007_Respiration_Data_1.5_C[,"Treat.temp....C."])


t2 = lm(log(Ikeda_2007_Respiration_1.5C)~log(Ikeda_2007_Dry_Mass_1.5C)) #Respiration data at 1.5 C

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

log_max_ingest_15_C = log(Saiz_Calbert_2007_max_ingestion_vs_size_15_C[,"Max.Ingestion"]) #Leave in micrograms of carbon per individual per day. Conversion to zooplankton dryweight happens after ingestion in stage structured model
#Encounter rate is liters per day, multiplied by Resource density in ug/L results in ug/day
#This unit matches above max ingestion rate units of ug/individual/day

#In the model structure, the allometric relationship uses copepod dry weight, so need to convert Saiz and Calbet weights from Carbon weight to dry weight using Uye conversion of 0.455

log_body_size_15_C = log(Saiz_Calbert_2007_max_ingestion_vs_size_15_C[,"Body.Size"]/0.455) #Convert from Carbon to zooplankton dry weight using a value of 0.455 is conversion factor from Uye 1982 where COPEPOD dry weight is composed of 45.5% Carbon

#Now the regression is is log(Carbon ingested) ~ log(Copepod DRY WEIGHT)

plot(log_max_ingest_15_C~log_body_size_15_C)


#Size dependence only
lm1 = lm(log_max_ingest_15_C~log_body_size_15_C)

summary(lm1)
exp(lm1[["coefficients"]][["(Intercept)"]])

