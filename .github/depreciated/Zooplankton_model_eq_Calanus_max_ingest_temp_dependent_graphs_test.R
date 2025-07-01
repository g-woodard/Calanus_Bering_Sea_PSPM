library(PSPManalysis)
library(tidyverse)
library(ggplot2)
get.NLL = function(Nt,Ntpred,sigma)
  


{
  
  
  
  
  NLL = c()
  
  for (i in 1:length(years_index)) 
  {
    NLL[i] <- log(1/sqrt(2*3.1415*sigma^2)) - ((Nt[i])-(Ntpred[i]))^2/(2*(sigma[i])^2)
  }
  
  NLL <- sum(NLL)
  return(NLL)
  
} 

#Plot ingestion at an intermediate temperature over a range of resource levels
#Add in temp dependence to turnover?
#Look at critical resource density
#assess competitive dominance of the different life stages
#sensitivity analysis of Mopt
#Plot total production in addition to net production
#Total production as a function of temperature is what we want to compare to productivity metric in bering sea
#Nonlinear attack rate vs temperature
#celsius to fahrenheit is *9/5 + 32  

DefaultParameters <- c(Rho =  1, #.1, #per day.  Range of between approximately .1 and 3 from Marañón et al. 2014.  They found no relationship between phytoplankton turnover rate and temperature  
                       Rmax = 2000, #Rmax is a density micrograms of carbon per liter.  This means all other densities including copepod densities are micrograms per liter. Approximately 2000 from Putland and Iverson 2007
                       
                       A_hat = 0.096, #liters per day filtering rate AKA volume swept clear via frost 1972.  This value is actually nonlinear.  Currently using max value.  Units should be liters per day.
                       # Neocalanus plumchrus is close in size to C. marshallae at 567 µg.  Dagg and Wyman (1983) found a range of clearance rates between .0336 1.3344 L/day
                       
                       #bifurcation with Rho = 3 and A hat = 1 min Rmax was 8.69
                       Temp = 273.15,  #20 C is 293.15 K (reference temp)
                       E_mu = .57,  #.85 eV (Savage et al. 2004) 
                       E_M = .55, #.4 for C. glacialis (Tande 1988)
                       E_I = 0.6, #.46 for C. glacialis (Maps et al. 2012).  Ingestion Activation Energy, see if I can find another one for activity or attack rates
                       
                       E_delta = 0.5, 
                       cI = 0, #Jan assumes a value of 0 in Roach model 
                       cM = 0.0,# Jan tests the Roach model with values of -.02, 0, and .02 
                       Lambda1 = 2, #(Petersen, 1986)
                       Lamda2 = 3.94, #(Petersen, 1986)
                       k= 8.617e-5, #boltzmann constant
                       alpha = .75, #guess
                       t0 = 285.65, #Frost experiment on attack rate conducted at 12.5 C or 285.65 K
                       sigma = .6 , # (Kiørboe, 2008.) Converts ingested energy to biomass
                       Mopt = 150, #???????????
                       
                       #one gram is 1 million micrograms
                       epsilon1 = exp(0.83445), #Approximated from saiz and calbert 2007.  micrograms of carbon per day.  On marine calanoid species. 15 C.
                       epsilon2 = 0.70310 , #Approximated from saiz and calbert 2007.  micrograms of carbon per day.  On marine calanoid species.  15 C.
                       t0_epsilon = 288.15,
                       
                       phi1 = 1.336596, #For mortality rate per day.  Approximated from  Hirst and Kiørboe 2002 calculated at 15 C.  Dry weight
                       phi2 = -0.092, #For mortality rate per day.  Approximated from  Hirst and Kiørboe 2002 calculated at 15 C. Dry Weight.  Might need to change my reference temp?
                       t0_phi = 288.15,
                       
                       rho1 = 0.02310296  , #micro grams per day, from Ikeda_2007.  Dry weight at 2 C
                       rho2 =  0.7581552   , #micro grams per day, from Ikeda_2007.  Dry weight at 2 C
                       t0_rho = 275.15,
                       
                       mh = .75, # .75 ug (micrograms) (Petersen, 1986) -graph pg 68
                       mj = exp(-3.18)*exp(.73*12)   # ug (micrograms) (Petersen, 1986) pg 66
                       
                       
                       #z = 0.002694034 #juvenile to adult ratio 
)
Bering_Calanus_Data = read.csv("Data/Calanus_BSMS.csv")



spring_Calanus = Bering_Calanus_Data[which(Bering_Calanus_Data[,"MONTH"] >=3 & Bering_Calanus_Data[,"MONTH"] < 6),]
summer_Calanus = Bering_Calanus_Data[which(Bering_Calanus_Data[,"MONTH"] >=6 & Bering_Calanus_Data[,"MONTH"] < 10),]
spring_Juvenile = spring_Calanus[which(spring_Calanus[,"STAGE_NAME"] != "ADULT"),"mean_DW_mg_m3"]
spring_Adult = spring_Calanus[which(spring_Calanus[,"STAGE_NAME"] == "ADULT"),"mean_DW_mg_m3"]
summer_Juvenile = summer_Calanus[which(summer_Calanus[,"STAGE_NAME"] != "ADULT"),"mean_DW_mg_m3"]
summer_Adult = summer_Calanus[which(summer_Calanus[,"STAGE_NAME"] == "ADULT"),"mean_DW_mg_m3"]

spring_Juvenile_year = spring_Calanus[which(spring_Calanus[,"STAGE_NAME"] != "ADULT"),"YEAR"]
spring_Adult_year = spring_Calanus[which(spring_Calanus[,"STAGE_NAME"] == "ADULT"),"YEAR"]
summer_Juvenile_year = summer_Calanus[which(summer_Calanus[,"STAGE_NAME"] != "ADULT"),"YEAR"]
summer_Adult_year = summer_Calanus[which(summer_Calanus[,"STAGE_NAME"] == "ADULT"),"YEAR"]


Bering_SST = read.csv("Data/Bering_Sea_SST.csv")[45:71,] #1992 through 2018
Bering_SST_Spring = rowMeans(Bering_SST[,4:6])+273.15
Bering_SST_Summer = rowMeans(Bering_SST[,7:9])+273.15

plot(log(summer_Adult)~summer_Adult_year)
plot(log(summer_Juvenile)~summer_Juvenile_year)

plot(Bering_SST_Summer~Bering_SST[,1])
plot(Bering_SST_Spring~Bering_SST[,1])

year_sequence = as.numeric(seq(1992, 2018, 1))


Spring_Mean_Juvenile_Biomass = c()
Spring_Mean_Adult_Biomass = c()
Summer_Mean_Juvenile_Biomass = c()
Summer_Mean_Adult_Biomass = c()

Temp_DF = data.frame(cbind(Year = c(year_sequence,year_sequence),
                Temperature = c(Bering_SST_Spring, Bering_SST_Summer),
                Season = c(rep("Spring", length(Bering_SST_Spring)), rep("Summer", length(Bering_SST_Summer)))
                ))

Temp_DF[,1] = as.numeric(Temp_DF[,1])
Temp_DF[,2] = as.numeric(Temp_DF[,2])
Temp_DF[,3] = as.factor(Temp_DF[,3])

for(i in 1:length(year_sequence))
{
  Spring_Mean_Juvenile_Biomass[i] = mean(spring_Juvenile[which(spring_Juvenile_year == year_sequence[i])], na.rm = TRUE)
  Spring_Mean_Adult_Biomass[i] = mean(spring_Adult[which(spring_Adult_year == year_sequence[i])], na.rm = TRUE)
  Summer_Mean_Juvenile_Biomass[i] = mean(summer_Juvenile[which(summer_Juvenile_year == year_sequence[i])], na.rm = TRUE)
  Summer_Mean_Adult_Biomass[i] = mean(summer_Adult[which(summer_Adult_year == year_sequence[i])], na.rm = TRUE)
}

Spring = as.numeric(c(Spring_Mean_Juvenile_Biomass,
           Spring_Mean_Adult_Biomass))
Summer = as.numeric(c(Summer_Mean_Juvenile_Biomass,
           Summer_Mean_Adult_Biomass))

Stage = as.character(c(rep("Juvenile", length(year_sequence)),
          rep("Adult", length(year_sequence))))


DF = data.frame(cbind(Year = c(year_sequence,year_sequence),
      Spring = Spring,
      Summer = Summer,
      Stage = Stage))

DF2 = DF %>%
  pivot_longer(c(Spring,Summer))

Temp_reordered = c()
for(i in 1:length(Bering_SST_Spring))
{
  Temp_reordered = c(Temp_reordered,Bering_SST_Spring[i],Bering_SST_Summer[i])
}


DF2 = data.frame(DF2)
DF2[,"Temperature"] = c(Temp_reordered,Temp_reordered)

DF2[,1] = as.numeric(DF2[,1])
DF2[,2] = as.factor(DF2[,2])
DF2[,3] = as.factor(DF2[,3])
DF2[,4] = as.numeric(DF2[,4])
DF2[,5] = as.numeric(DF2[,5])


DF3 = na.omit(DF2)

ggplot(data = DF3, aes(x = Year, y = log(value), linetype = Stage))+
  facet_wrap(~name)+
  geom_line()
# ggplot(data = DF3, aes(x = Temperature, y = log(value), linetype = Stage))+
#   facet_wrap(~name)+
#   geom_line()
ggplot(data = Temp_DF, aes(x = Year, y = Temperature, color = Season))+
  geom_line()

Ikeda_2007_Respiration_Data_original = read.csv("Data/Ikeda_2007_Respiration_Data.csv")


length(which(Ikeda_2007_Respiration_Data_original[,4] == 2))
length(which(Ikeda_2007_Respiration_Data_original[,4] == 3))
length(which(Ikeda_2007_Respiration_Data_original[,4] == 1.5))

Ikeda_2007_Respiration_Data_2_C = Ikeda_2007_Respiration_Data_original[which(Ikeda_2007_Respiration_Data_original[,4] == 2),] #only use data measured at 2 C


Temp_Ikeda_K_all = 1/Ikeda_2007_Respiration_Data_original[,4]+273.15
Ikeda_Respiration_all = (Ikeda_2007_Respiration_Data_original[,5]*24)*10^(-6)*(22.4)*15.998*10^6 #convert to micrograms of O2
Ikeda_Mass_all = Ikeda_2007_Respiration_Data_original[,6]*1000
Ikeda_Respiration_all_mass_specific = Ikeda_Respiration_all/Ikeda_Mass_all

lm_for_determining_EM = lm(log(Ikeda_Respiration_all_mass_specific)~Temp_Ikeda_K_all)
plot(log(Ikeda_Respiration_all_mass_specific)~Temp_Ikeda_K_all)

E_M = -0.09891 *8.617e-5

Ikeda_2007_Respiration_2C = (Ikeda_2007_Respiration_Data_2_C[,5]*24)*10^(-6)*(22.4)*15.998*10^6 #convert to micrograms of O2
Ikeda_2007_Dry_Mass_2C = (Ikeda_2007_Respiration_Data_2_C[,6]*1000)
Ikeda_2007_Temp_2C = (Ikeda_2007_Respiration_Data_2_C[,4])

t = lm(log(Ikeda_Respiration_all)~log(Ikeda_Mass_all)) #All data, not just restricted to 2 C
summary(t)

t2 = lm(log(Ikeda_2007_Respiration_2C)~log(Ikeda_2007_Dry_Mass_2C)) #Respiration data at 2 C
summary(t2)
exp(2.45033)

Ikeda_2007_Respiration_log = log(Ikeda_2007_Respiration)
Ikeda_2007_Dry_Mass_log = log(Ikeda_2007_Dry_Mass)


plot(Ikeda_2007_Respiration_log~Ikeda_2007_Temp)


plot(y = Ikeda_2007_Respiration, x = Ikeda_2007_Dry_Mass, xlab = "µg", ylab = "µg/day")

plot(y = Ikeda_2007_Respiration_log, x = Ikeda_2007_Dry_Mass_log, xlab = "log µg", ylab = "log µg per day")
regression = lm(Ikeda_2007_Respiration_log~Ikeda_2007_Dry_Mass_log)


a = exp(regression[["coefficients"]][1])
b = regression[["coefficients"]][2]

a*200^b
exp(-3.7677+.7581552*log(200))

Saiz_Calbert_2007_DF_original = read.csv("Data/Saiz_Calbert_2007_Calanus_Ingestion.csv")

Saiz_Calbert_2007_DF_10_15_C = Saiz_Calbert_2007_DF_original[which(Saiz_Calbert_2007_DF_original[,2] >= 10 & Saiz_Calbert_2007_DF_original[,2] <=15),]

Saiz_Calbert_2007_DF = Saiz_Calbert_2007_DF_original

mean(Saiz_Calbert_2007_DF[,2])

Dry_Weight = Saiz_Calbert_2007_DF[,3] #Leave in micrograms of carbon per liter. Conversion to zooplankton dryweight happens after ingestion in stage. structured model
Ingestion = Saiz_Calbert_2007_DF[,5]
Ingestion_per_unit_body_weight = Ingestion/Dry_Weight
Food_Concentration = Saiz_Calbert_2007_DF[,4]
Temp = Saiz_Calbert_2007_DF[,2]+273.15
Temp_Saiz_Calbert_2007_DF_10_15_C = Saiz_Calbert_2007_DF_10_15_C[,2]+273.15 
Dry_Weight_Thousands = Dry_Weight/1000



Dry_Weight_DF_10_15_C = Saiz_Calbert_2007_DF_10_15_C[,3] #Leave in micrograms of carbon per liter. Conversion to zooplankton dryweight happens after ingestion in stage. structured model
Ingestion_DF_10_15_C = Saiz_Calbert_2007_DF_10_15_C[,5] #Leave in micrograms of carbon per liter
Food_Concentration_10_15_C = Saiz_Calbert_2007_DF_10_15_C[,4] #Leave in micrograms of carbon per liter

mean(Saiz_Calbert_2007_DF_10_15_C[,2])

plot(log(Ingestion_DF_10_15_C) ~ log(Dry_Weight_DF_10_15_C))
plot((Ingestion_DF_10_15_C) ~ Temp_Saiz_Calbert_2007_DF_10_15_C)

lm_calanoid_copepods_log_Ingestion_Log_Dry_Weight_DF_10_15_C = lm(log(Ingestion_DF_10_15_C)~log(Dry_Weight_DF_10_15_C) + log(Food_Concentration_10_15_C))

lm_calanoid_copepods_log_Ingestion_Log_Dry_Weight_DF_10_15_C[["coefficients"]]




plot(Ingestion ~ Food_Concentration)
plot(Ingestion ~ Temp)
plot(log(Ingestion) ~ (Temp))


#"temperature was not transformed because the logarithm of a biological rate is usually regarded as being a linear function of temperature (Q10 concept)" - Saiz and Calbert_2007 
Ingestion_lm = lm(log(Ingestion)~log(Dry_Weight) + log(Food_Concentration) + (Temp) )

t0 = 285.65
Temp = t0
A_hat = .096
alpha = .75
E_I = .6
k = 8.617e-5
cI = 0
# 
 model = nls(Ingestion_DF_10_15_C  ~ (A_hat*Dry_Weight_DF_10_15_C^alpha*Food_Concentration_10_15_C)/(1+(A_hat*Dry_Weight_DF_10_15_C^alpha*Food_Concentration_10_15_C)/17.74286), start = list(alpha = .6)
             )
 model = nls(Ingestion_DF_10_15_C  ~ (A_hat*Food_Concentration_10_15_C)/(1+(A_hat*Food_Concentration_10_15_C)/Imax), start = list(Imax = .6)
 )
 
 model = nls(Ingestion_DF_10_15_C  ~ (Imax*(A_hat*Food_Concentration_10_15_C)/(A_hat*Food_Concentration_10_15_C+Imax)), start = list(Imax = .6)
 )
 # 
# 
n = exp(E_I*(Temp-t0)/((k)*Temp*t0))*(A_hat*(Dry_Weight_DF_10_15_C)^(alpha+cI*(Temp-t0)))*Food_Concentration_10_15_C
#

#model2 = nls(Ingestion_DF_10_15_C ~ (n/(1+(n/Imax))), start = list(Imax = .1, alpha = .1 ) )

model2 = nls(Ingestion_DF_10_15_C ~ ((A_hat*(Dry_Weight_DF_10_15_C)^(alpha))/(1+((A_hat*(Dry_Weight_DF_10_15_C)^(alpha))/Imax))), start = list(Imax = 10, alpha = .6 ) )

#model2 = nls(Ingestion_DF_10_15_C ~ ((A_hat*(Dry_Weight_DF_10_15_C)^(alpha))/(1+((A_hat*(Dry_Weight_DF_10_15_C)^(alpha))/(omega1*Dry_Weight_DF_10_15_C^omega2)))), start = list(Imax = 10, alpha = .6, omega1 = 20, omega2 = .7) )



model4 = nls(Ingestion_DF_10_15_C ~ (n/(1+(n/(omega1*Dry_Weight_DF_10_15_C^omega2)))), start = list(omega1 = .1, omega2 = .6) )

model4 = nls(Ingestion_DF_10_15_C ~ (Imax*(n/(n+Imax))), start = list(Imax = 1) )

model4 = nls(Ingestion_DF_10_15_C ~ (Imax*(((A_hat*(Dry_Weight_DF_10_15_C)^(alpha+cI*(Temp-t0)))*Food_Concentration_10_15_C)/(((A_hat*(Dry_Weight_DF_10_15_C)^(alpha+cI*(Temp-t0)))*Food_Concentration_10_15_C)+Imax))), start = list(Imax = 1) )

#model4 = nls(Ingestion_DF_10_15_C ~ ((omega1*Dry_Weight_DF_10_15_C^omega2)*(((A_hat*(Dry_Weight_DF_10_15_C)^(alpha+cI*(Temp-t0)))*Food_Concentration_10_15_C)/(((A_hat*(Dry_Weight_DF_10_15_C)^(alpha+cI*(Temp-t0)))*Food_Concentration_10_15_C)+(omega1*Dry_Weight_DF_10_15_C^omega2)))), start = list(Imax = 1, alpha = .01,
#                                                                                                                                                                                                                                                                                                        omega1 = .01, omega2 = .01) )


plot(Ingestion_DF_10_15_C~Dry_Weight_DF_10_15_C)

coefficients = Ingestion_lm[["coefficients"]]


summary(Ingestion_lm)

ANOVA_sum_sq = anova(Ingestion_lm)[,"Sum Sq"]

ANOVA_sum_sq/ sum(ANOVA_sum_sq)

options(buildtools.check = function(action) TRUE )


#Varying Rmax

Negative_to_zero = function(value)
{
  if(value < 0)
  {
    value = 0
  }
  
  else
    value = value
  
  return(value)
}

Saiz_Calbert_2007_max_ingestion_vs_size_15_C = read.csv("Data/Saiz_Calbet_2007_Max_ingestion_Vs_Size_15_C.csv")
#max ingestion rate in µg C per day
#body weight in µg C
#Food Concentration in % body weight

plot(Saiz_Calbert_2007_max_ingestion_vs_size_15_C[,"Max.Ingestion"]~Saiz_Calbert_2007_max_ingestion_vs_size_15_C[,"Body.Size"]) 

plot(log(Saiz_Calbert_2007_max_ingestion_vs_size_15_C[,"Max.Ingestion"])~log(Saiz_Calbert_2007_max_ingestion_vs_size_15_C[,"Body.Size"]))

max_ingest_15_C = (Saiz_Calbert_2007_max_ingestion_vs_size_15_C[,"Max.Ingestion"])
body_size_15_C = (Saiz_Calbert_2007_max_ingestion_vs_size_15_C[,"Body.Size"])
Food_15_C = (Saiz_Calbert_2007_max_ingestion_vs_size_15_C[,"Body.Size"]*(Saiz_Calbert_2007_max_ingestion_vs_size_15_C[,"Food.Concentration"]/100)) 


log_max_ingest_15_C = log(Saiz_Calbert_2007_max_ingestion_vs_size_15_C[,"Max.Ingestion"]) #Leave in micrograms of carbon per liter. Conversion to zooplankton dryweight happens after ingestion in stage. structured model
log_body_size_15_C = log(Saiz_Calbert_2007_max_ingestion_vs_size_15_C[,"Body.Size"]) #Leave in micrograms of carbon per liter. Conversion to zooplankton dryweight happens after ingestion in stage. structured model
log_Food_15_C = log(Saiz_Calbert_2007_max_ingestion_vs_size_15_C[,"Body.Size"]*(Saiz_Calbert_2007_max_ingestion_vs_size_15_C[,"Food.Concentration"]/100)) 

lm0 = lm(log_max_ingest_15_C~log_body_size_15_C+log_Food_15_C) #Unclear if saiz and calbert 2007 controlled for food concentration.  Methods section is very vague
summary(lm0)

mean(Saiz_Calbert_2007_max_ingestion_vs_size_15_C[,"Max.Ingestion"])

#model3 = nls(max_ingest_15_C ~ omega1*body_size_15_C^omega2, start = list(omega1 = .01, omega2 = .5) )
plot(max_ingest_15_C ~ body_size_15_C)

#Control for food concentration
exp(0.0007407)


lm1 = lm(log_max_ingest_15_C~log_body_size_15_C)
summary(lm1)

lm2 = lm(log_max_ingest_15_C~log_Food_15_C)
summary(lm2)


Modified_Parameters_A_hat_.096 = DefaultParameters



output1_1_A_hat_.096 <-PSPMequi(modelname = "Scripts/StageStructuredBiomass_GW_Calanus_max_ingestion_temp_independent_V5.R", biftype = "EQ", startpoint = c(100, 100), 
                     stepsize = 1.25,
                     parbnds = c(1, 1, 10000), parameters = Modified_Parameters_A_hat_.096, minvals = NULL, maxvals = NULL, options = c(c("popZE", "0")),
                     clean = TRUE, force = FALSE, debug = FALSE, silent = FALSE)
