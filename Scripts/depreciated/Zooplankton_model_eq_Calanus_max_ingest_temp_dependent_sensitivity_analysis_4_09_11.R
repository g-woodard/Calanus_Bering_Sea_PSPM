library(PSPManalysis)
library(tidyverse)

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

EQ_Function = function(Delta, Rmax, alpha, Mopt, sigma)
  
{
  
  DefaultParameters <- c(Delta =  Delta, #turnover rate is 1 divided by the per capita growth rate
                         # Turnover is 1, #per day.  Range of between approximately .1 and 3 from Marañón et al. 2014.  They found no relationship between phytoplankton turnover rate and temperature  
                         Rmax = Rmax, #Rmax is a density micrograms of carbon per liter.  This means all other densities including copepod densities are micrograms per liter. Approximately 2000 from Putland and Iverson 2007
                         
                         A_hat = 0.096, #0.096 liters per day filtering rate AKA volume swept clear via frost 1972. Units should be liters per day.
                         # Neocalanus plumchrus is close in size to C. marshallae at 567 µg.  Dagg and Wyman (1983) found a range of clearance rates between .0336 1.3344 L/day
                         
                         
                         Temp = 273.15,  #20 C is 293.15 K 
                         E_mu = .57,  #.57 eV (McCoy et al. 2008) 
                         E_M = .55, #.55 (Maps et al. 2014)
                         E_I = 0.46, #.46 for C. glacialis (Maps et al. 2012).  Ingestion Activation Energy, see if I can find another one for activity or attack rates
                         #Average of .6 Savage et al. 2004, as cited in (Crossier 1926; Raven and Geider 1988; Vetter 1995; Gillooly et al. 2001)
                         E_Delta = 0.5, #average activation energy of phytoplankton growth from Barton and Yvon-Durocher (2019) 
                         #Im = 29.89, #when n = A_hat*R
                         Im = 11.26, #fit from kiorbe et al 2018 formulation of ingestion with alpha = .75 when n = A_hat*Size^alpha*R
                         #mean of just calanus at 15 C is 22.48333.  Mean of all species at 15 C is 17.74286
                         #Im = 17.74286,
                         t0_Im = 285.65, #average of Saiz Calbet data restricted to 10-15 C
                         
                         #286.803, #mean temp of saiz and calbet dataframe when restricted to experiments between 10 and 15 degrees C
                         cI = 0, #Jan assumes a value of 0 in Roach model 
                         cM = 0.0,# Jan tests the Roach model with values of -.02, 0, and .02 
                         Lambda1 = 2, #(Petersen, 1986)
                         Lamda2 = 3.94, #(Petersen, 1986)
                         k = 8.617e-5, #boltzmann constant
                         alpha = alpha, #guess
                         t0 = 285.65, #Frost experiment on attack rate conducted at 12.5 C or 285.65 K
                         sigma = sigma , # .6 (Kiørboe, 2008.) Converts ingested energy to biomass
                         #assimilation efficiencies between .685 and .854 Landry et al. 1984
                         Mopt = Mopt, #???????????
                         
                         gamma1 = exp(-3.211e-06), #from Saiz and Calbet max ingestion data at 15 C
                         gamma2 = 9.683e-03,
                         gamma3 = 9.894e-01,
                         t0_gamma = 288.15,
                         
                         theta1 = exp(-3.6374366),
                         theta2 = 0.2492352,
                         theta3 = 0.8211947,
                         t0_theta = 286.953, #mean of Saiz and Calbet ingestion data when restricted to specimens at between 10 and 15 C
                         
                         epsi1 = exp(0.83445), #Approximated from saiz and calbert 2007.  micrograms of carbon per day.  On marine calanoid species. 15 C.
                         epsi2 = 0.70310 , #Approximated from saiz and calbert 2007.  micrograms of carbon per day.  On marine calanoid species.  15 C.
                         t0_epsi = 288.15,
                         
                         epsilon1 = exp(-8.68361),
                         epsilon2 = 0.35064, #from Saiz and Calbet max ingestion data at 15 C
                         epsilon3 = 0.88576,
                         epsilon4 = 0.01603,
                         t0_epsilon = 288.15,
                         
                         omega1 = 3.6577,
                         omega2 = 0.3307,
                         t0_omega = 286.953, #mean of Saiz and Calbet ingestion data when restricted to specimens at between 10 and 15 C
                         
                         phi1 = 1.336596, #For mortality rate per day.  Approximated from  Hirst and Kiørboe 2002 calculated at 15 C.  Dry weight
                         phi2 = -0.092, #For mortality rate per day.  Approximated from  Hirst and Kiørboe 2002 calculated at 15 C. Dry Weight.  Might need to change my reference temp?
                         t0_phi = 288.15,
                         
                         rho1 = exp(2.45033), #micro grams per day, from Ikeda_2007.  Dry weight at 2 C
                         rho2 = 0.75816, #micro grams per day, from Ikeda_2007.  Dry weight at 2 C
                         t0_rho = 275.15,
                         
                         mh = .75, # .75 ug (micrograms) (Petersen, 1986) -graph pg 68
                         mj = exp(-3.18)*exp(.73*12),   # ug (micrograms) (Petersen, 1986) pg 66
                         
                         t0_Delta = 281.15 #8 degrees celsius from Maranon et al. at an intermediate nutrient level had turnover rate of about 1 day
                         #Turnover_Time = 2.19907e-7 #converted from ms to days from Falkowski et al 1981
                         
                         #z = 0.002694034 #juvenile to adult ratio 
  )
  
Bering_Calanus_Data = read.csv("Data/Calanus_BSMS.csv")



spring_Calanus = Bering_Calanus_Data[which(Bering_Calanus_Data[,"MONTH"] >=3 & Bering_Calanus_Data[,"MONTH"] < 6),]
summer_Calanus = Bering_Calanus_Data[which(Bering_Calanus_Data[,"MONTH"] >=6 & Bering_Calanus_Data[,"MONTH"] < 9),]
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

Ikeda_2007_Respiration_Data = Ikeda_2007_Respiration_Data_original[which(Ikeda_2007_Respiration_Data_original[,4] == 2),]


Ikeda_2007_Respiration = (Ikeda_2007_Respiration_Data[,5]*24)*10^(-6)*(1/(22.4))*15.998*10^6 #convert to micrograms of O2
Ikeda_2007_Dry_Mass = (Ikeda_2007_Respiration_Data[,6]*1000)
Ikeda_2007_Temp = (Ikeda_2007_Respiration_Data[,4])



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

Dry_Weight = Saiz_Calbert_2007_DF[,3]/.455
Ingestion = Saiz_Calbert_2007_DF[,5]
Ingestion_per_unit_body_weight = Ingestion/Dry_Weight
Food_Concentration = Saiz_Calbert_2007_DF[,4]
Temp = Saiz_Calbert_2007_DF[,2]+273.15
Temp_Saiz_Calbert_2007_DF_10_15_C = Saiz_Calbert_2007_DF_10_15_C[,2]+273.15 
Dry_Weight_Thousands = Dry_Weight/1000

plot(Ingestion_per_unit_body_weight~Food_Concentration)

Dry_Weight_DF_10_15_C = Saiz_Calbert_2007_DF_10_15_C[,3]/.455 #convert micrograms of carbon to micrograms of dry weight for copepod
Ingestion_DF_10_15_C = Saiz_Calbert_2007_DF_10_15_C[,5]
Ingestion_per_unit_body_weight_DF10_15_C = Ingestion_DF_10_15_C/Dry_Weight_DF_10_15_C
Food_Concentration_10_15_C = Saiz_Calbert_2007_DF_10_15_C[,4]
Mass_Specific_Ingestion_DF_10_15_C = 
273+mean(Saiz_Calbert_2007_DF_10_15_C[,2])


plot(Ingestion_per_unit_body_weight_DF10_15_C~Food_Concentration_10_15_C) #samples at between 10 and 15 C (mean temp of 286, use for reference temp?)

plot(log(Ingestion_DF_10_15_C) ~ log(Dry_Weight_DF_10_15_C))
plot((Ingestion_DF_10_15_C) ~ Temp_Saiz_Calbert_2007_DF_10_15_C)

lm_calanoid_copepods_log_Ingestion_Log_Dry_Weight_DF_10_15_C = lm(log(Ingestion_DF_10_15_C)~log(Dry_Weight_DF_10_15_C) + log(Food_Concentration_10_15_C))

lm_calanoid_copepods_log_Ingestion_Log_Dry_Weight_DF_10_15_C[["coefficients"]]

Attackrate = .096

alpha = .75

Saiz_Calbert_2007_max_ingestion_vs_size_15_C = read.csv("Data/Saiz_Calbet_2007_Max_ingestion_Vs_Size_15_C.csv")
max_ingest_15_C = (Saiz_Calbert_2007_max_ingestion_vs_size_15_C[,"Max.Ingestion"])
max_ingest_15_C = log(Saiz_Calbert_2007_max_ingestion_vs_size_15_C[,"Body.Size"])


#model <- nls(Ingestion_DF_10_15_C ~ ((0.096*Food_Concentration_10_15_C*Dry_Weight_DF_10_15_C^alpha)/(1+(0.096*Food_Concentration_10_15_C*Dry_Weight_DF_10_15_C^alpha)/(epsilon1*Dry_Weight_DF_10_15_C^epsilon2))), start = list(epsilon1 = 0.1, epsilon2 = 0.1))


plot(Ingestion ~ Food_Concentration)
plot(Ingestion ~ Temp)
plot(log(Ingestion) ~ (Temp))


#"temperature was not transformed because the logarithm of a biological rate is usually regarded as being a linear function of temperature (Q10 concept)" - Saiz and Calbert_2007 
Ingestion_lm = lm(log(Ingestion)~log(Dry_Weight) + log(Food_Concentration) + (Temp) )


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

plot(Saiz_Calbert_2007_max_ingestion_vs_size_15_C[,"Max.Ingestion"]~Saiz_Calbert_2007_max_ingestion_vs_size_15_C[,"Body.Size"])
log_max_ingest_15_C = log(Saiz_Calbert_2007_max_ingestion_vs_size_15_C[,"Max.Ingestion"])
log_body_size_15_C = log(Saiz_Calbert_2007_max_ingestion_vs_size_15_C[,"Body.Size"])
summary(lm(log_max_ingest_15_C~log_body_size_15_C))
exp(1.06825)
mean(Saiz_Calbert_2007_max_ingestion_vs_size_15_C[,"Max.Ingestion"])

Modified_Parameters_A_hat_.096 = DefaultParameters



output1_1_A_hat_.096 <-PSPMequi(modelname = "Scripts/StageStructuredBiomass_GW_Calanus_max_ingestion_temp_dependent_V5_9_15_2023.R", biftype = "EQ", startpoint = c(1, 1), 
                     stepsize = 2,
                     parbnds = c(1, 1, 5000), parameters = Modified_Parameters_A_hat_.096, minvals = NULL, maxvals = NULL, options = c(c("popZE", "0")),
                     clean = TRUE, force = FALSE, debug = FALSE, silent = FALSE)

output1_1_non_trivial_.096 <-PSPMequi(modelname = "Scripts/StageStructuredBiomass_GW_Calanus_max_ingestion_temp_dependent_V5_9_15_2023.R", 
                                      biftype = "EQ", startpoint = c(output1_1_A_hat_.096$bifpoints[1], output1_1_A_hat_.096$bifpoints[2], 0), 
                                      stepsize = 1.5, parbnds = c(1, output1_1_A_hat_.096$bifpoints[1], 5000), parameters = Modified_Parameters_A_hat_.096, 
                                      minvals = NULL, maxvals = NULL, clean = TRUE, force = FALSE, debug = FALSE, silent = FALSE)



t1 = output1_1_non_trivial_.096$curvepoints


start_.096 = median(which(t1[,"Rmax"] < (Modified_Parameters_A_hat_.096[2]+250) & t1[,"Rmax"] > (Modified_Parameters_A_hat_.096[2]-250)))
start_.096 = round(start_.096, 0)



output1_1_non_trivial_varying_temperature_.096 <-PSPMequi(modelname = "Scripts/StageStructuredBiomass_GW_Calanus_max_ingestion_temp_dependent_V5_9_15_2023.R", 
                                                          biftype = "EQ", startpoint = c(273.15, output1_1_non_trivial_.096$curvepoints[start_.096,2], 
                                                          output1_1_non_trivial_.096$curvepoints[start_.096,3]), 
                                                          stepsize = .5,
                                                          parbnds = c(3, 273.15, 305), parameters = Modified_Parameters_A_hat_.096, minvals = NULL, maxvals = NULL, 
                                                          clean = TRUE, force = FALSE, debug = FALSE, silent = FALSE)
  
                                                         
  
 Temp_A_hat_.096 = as.numeric(output1_1_non_trivial_varying_temperature_.096$curvepoints[,1])

 Temp_A_hat = Temp_A_hat_.096

 R_A_hat = output1_1_non_trivial_varying_temperature_.096$curvepoints[,2]
 J_A_hat = output1_1_non_trivial_varying_temperature_.096$curvepoints[,5]
 A_A_hat = output1_1_non_trivial_varying_temperature_.096$curvepoints[,6]
 
 df3 <- data.frame(Temperature = Temp_A_hat,
                   R = R_A_hat,
                   J = J_A_hat,
                   A = A_A_hat
                   ) %>% 
   pivot_longer(c(R, J, A))
 
 

 
 
 Temp_A_hat = Temp_A_hat_.096
 R_A_hat =  output1_1_non_trivial_varying_temperature_.096$curvepoints[,2]
 
  
  
  ggplot(df3, aes(x = Temperature, y= value, color = name, linetype = Â)) + 
    geom_line() + labs(x= "Temperature (K)", y="Density (µg/L)") +
    guides(color=guide_legend(title = "Population"))+
   theme_classic()+
    scale_color_manual(labels=c('Adult (µg/L)', 'Juvenile (µg/L)', 'Resource (µg C/L)'),
                       values = c("Black","Red","Blue"))+
    scale_x_continuous(breaks = round(seq(min(df3$Temperature), max(df3$Temperature), by = 2),0))
  
   
  df3_J = df3[which(df3[,"name"] == "J"),]
  df3_A = df3[which(df3[,"name"] == "A"),]
  DF3_J = DF3[which(DF3[,"Stage"] == "Juvenile" & DF3[,"name"] == "Summer"),]
  DF3_A = DF3[which(DF3[,"Stage"] == "Adult" & DF3[,"name"] == "Summer"),]
  
  rows_to_extract_J = c()
  rows_to_extract_A = c()
  
  
for(i in 1:nrow(DF3_J))
  {
  rows_to_extract_J[i] = min(which(abs(df3_J[,1] - DF3_J[,"Temperature"][i]) == min(abs(df3_J[,1] - DF3_J[,"Temperature"][i]))) )
  
}
  
  for(i in 1:nrow(DF3_A))
  {
  rows_to_extract_A[i] = min(which(abs(df3_A[,1] - DF3_A[,"Temperature"][i]) == min(abs(df3_A[,1] - DF3_A[,"Temperature"][i]))) )
    
  }
  
  
  Year_to_temp_DF_J = data.frame(df3_J[rows_to_extract_J,])
  Year_to_temp_DF_A = data.frame(df3_A[rows_to_extract_A,])
  
  
  Year_to_temp_DF = rbind( Year_to_temp_DF_J,
                           Year_to_temp_DF_A)
  Year_to_temp_DF[,"Year"] = c(DF3_J[,"Year"], DF3_A[,"Year"])
    
  Year_to_temp_DF = Year_to_temp_DF[,c(4,2,3,1)]
  colnames(Year_to_temp_DF) = c("Year", "Stage", "value", "Temperature")
  Year_to_temp_DF[,"Data_Type"] = rep("Predicted",nrow(Year_to_temp_DF))

  DF3_Summer = DF3[which(DF3[,"name"] == "Summer"),]
  DF3_Summer[,2] = as.character(DF3_Summer[,2])
  
  DF3_Summer = DF3_Summer[,c(1,2,4,5)]
  
for(i in 1:nrow(DF3_Summer))
  {
    if(DF3_Summer[i,"Stage"] == "Juvenile")
      
    {
      DF3_Summer[i,"Stage"] = "J"
    }
    
    if(DF3_Summer[i,"Stage"] == "Adult")
      
    {
      DF3_Summer[i,"Stage"] = "A"
    }
    
  }
  
  DF3_Summer[,"Data_Type"] = rep("Observed", nrow(DF3_Summer))
  
  DF_Summer_Observed_Predicted = rbind(DF3_Summer,Year_to_temp_DF)
  DF_Summer_Observed_Predicted[,2] = as.factor(DF_Summer_Observed_Predicted[,2])
  DF_Summer_Observed_Predicted[,5] = as.factor(DF_Summer_Observed_Predicted[,5])
  
  
  ggplot(data =   DF3, aes(x = Year, y = (value), linetype = Stage))+
    facet_wrap(~name)+
    geom_line()+
    geom_line()

  ggplot(data =  DF_Summer_Observed_Predicted, aes(x = Year, y = (value), color = Data_Type, linetype = Stage))+
    #facet_wrap(~name)+
    geom_line()+
    geom_line()
  
return(DF_Summer_Observed_Predicted)
  
}
  
# 
# A_Hat_Vector = seq(.04, .1, .0025)
# #Delta_Vector = seq(.1,3.1,.5)
# 
# sensitivity_array = array(dim = c(length(A_Hat_Vector), 2) )
# Pred_J = array(dim = c(23, length(A_Hat_Vector)))
# Obs_J =  array(dim = c(23, length(A_Hat_Vector)))
# Pred_A =  array(dim = c(21, length(A_Hat_Vector)))
# Obs_A =  array(dim = c(21, length(A_Hat_Vector)))
# 
# 
# 
# output = c()
#   
# for(i in 1:length(A_Hat_Vector))
# {
#     cat("########################################")
#     print(i)
#     output = EQ_Function(A_Hat_Vector[i])
#     
#     # squared_residuals_J = c()
#     # squared_residuals_A = c()
#     
#    
#     # for(k in 1:length(output[which(output[,"Data_Type"] == "Observed" & output[,"Stage"] == "J" ) ,"value"]))
#     # {
#       #squared_residuals_J[k] = ((output[which(output[,"Data_Type"] == "Observed" & output[,"Stage"] == "J" ) ,"value"][k]) - (output[which(output[,"Data_Type"] == "Predicted" & output[,"Stage"] == "J" ) ,"value"][k]) )^2
#       Obs_J[,i] = output[which(output[,"Data_Type"] == "Observed" & output[,"Stage"] == "J" ) ,"value"]
#       Pred_J[,i] = output[which(output[,"Data_Type"] == "Predicted" & output[,"Stage"] == "J" ) ,"value"]
#     #}
#     
#     # for(a in 1:length(output[which(output[,"Data_Type"] == "Observed" & output[,"Stage"] == "A" ) ,"value"]))
#     # {
#       # squared_residuals_A[a] = ((output[which(output[,"Data_Type"] == "Observed" & output[,"Stage"] == "A" ) ,"value"][a]) - (output[which(output[,"Data_Type"] == "Predicted" & output[,"Stage"] == "A" ) ,"value"][a]) )^2   
#       Obs_A[,i] = output[which(output[,"Data_Type"] == "Observed" & output[,"Stage"] == "A" ) ,"value"]
#       Pred_A[,i] = output[which(output[,"Data_Type"] == "Predicted" & output[,"Stage"] == "A" ) ,"value"]
#     #}
#     
#     # J_sum_squares = sum(squared_residuals_J)
#     # A_sum_squares = sum(squared_residuals_A)
#     # 
#     # print(J_sum_squares)
#     # print(A_sum_squares)
#     # 
#     # sensitivity_array[i,1] = J_sum_squares
#     # sensitivity_array[i,2] = A_sum_squares
# 
# }
# 
# # min(sensitivity_array[,1]) # 2,1 .6 and 2000
# # 
# # min(sensitivity_array[,2]) # 2,1
# # 
# # sensitivity_array_total = sensitivity_array[,1] + sensitivity_array[,2]
# # min(sensitivity_array_total, na.rm = TRUE) # 1, 11
# # 
# # Rmax_Vector[2]
# #Delta_Vector[1]
# 
# MAPE_J = colMeans(abs(Pred_J - Obs_J)/Obs_J)
# MAPE_A = colMeans(abs(Pred_A - Obs_A)/Obs_A)
# min(MAPE_J)
# min(MAPE_A)
# 
# which(MAPE_J == min(MAPE_J))
# which(MAPE_A == min(MAPE_A))
# 
# A_Hat_Vector[8] 

Delta_Vector = 1
Rmax_Vector = 2000
#A_Hat_Vector = seq(0.035,.065,.01) 
A_Hat_Vector = 0.096

alpha_Vector = seq(0.70,0.78,0.01)

Mopt_Vector = seq(100,250, 25)

sigma_Vector = seq(.66,.68,.01)


Pred_J = array(dim = c(0, 8, 8))
Obs_J =  array(dim = c(0, 8, 8))
Pred_A =  array(dim = c(0, 8, 8))
Obs_A =  array(dim = c(0, 8, 8))

Error_array_J = array(23, dim = c(length(Delta_Vector), length(Rmax_Vector), length(alpha_vector), length(Mopt_Vector), length(sigma_Vector)))
Error_array_A = array(21, dim = c(length(Delta_Vector), length(Rmax_Vector), length(alpha_vector), length(Mopt_Vector), length(sigma_Vector)))


output = c()

for(i in 1:length(Delta_Vector))
{
  for(j in 1:length(Rmax_Vector))
  {
    for(k in 1:length(alpha_Vector))
        {
      for(z in 1:length(Mopt_Vector))
        
      {
        for(a in 1:length(sigma_Vector))
            {
  cat("########################################")
  print(i)
  print(j)
  print(k)
  print(z)
  
  output = EQ_Function(Delta_Vector[i], Rmax_Vector[j], alpha_Vector[k], Mopt_Vector[z], sigma_Vector[a])
  

  Obs_J = rbind(Obs_J, data.frame(cbind(output[which(output[,"Data_Type"] == "Observed" & output[,"Stage"] == "J" ) ,],
                                        rep(Delta_Vector[i], nrow(output[which(output[,"Data_Type"] == "Observed" & output[,"Stage"] == "J" ) ,])),
                                        rep(Rmax_Vector[j], nrow(output[which(output[,"Data_Type"] == "Observed" & output[,"Stage"] == "J" ) ,])),
                                        rep(alpha_Vector[k], nrow(output[which(output[,"Data_Type"] == "Observed" & output[,"Stage"] == "J" ) ,])),
                                        rep(Mopt_Vector[z], nrow(output[which(output[,"Data_Type"] == "Observed" & output[,"Stage"] == "J" ) ,])),
                                        rep(sigma_Vector[a], nrow(output[which(output[,"Data_Type"] == "Observed" & output[,"Stage"] == "J" ) ,]))
  )))
  
  Obs_A = rbind(Obs_A, data.frame(cbind(output[which(output[,"Data_Type"] == "Observed" & output[,"Stage"] == "A" ) ,],
                                        rep(Delta_Vector[i], nrow(output[which(output[,"Data_Type"] == "Observed" & output[,"Stage"] == "A" ) ,])),
                                        rep(Rmax_Vector[j], nrow(output[which(output[,"Data_Type"] == "Observed" & output[,"Stage"] == "A" ) ,])),
                                        rep(alpha_Vector[k], nrow(output[which(output[,"Data_Type"] == "Observed" & output[,"Stage"] == "A" ) ,])),
                                        rep(Mopt_Vector[z], nrow(output[which(output[,"Data_Type"] == "Observed" & output[,"Stage"] == "A" ) ,])),
                                        rep(sigma_Vector[a], nrow(output[which(output[,"Data_Type"] == "Observed" & output[,"Stage"] == "A" ) ,]))
  )))
           
       
  Pred_J = rbind(Pred_J, data.frame(cbind(output[which(output[,"Data_Type"] == "Predicted" & output[,"Stage"] == "J" ) ,],
                                        rep(Delta_Vector[i], nrow(output[which(output[,"Data_Type"] == "Predicted" & output[,"Stage"] == "J" ) ,])),
                                        rep(Rmax_Vector[j], nrow(output[which(output[,"Data_Type"] == "Predicted" & output[,"Stage"] == "J" ) ,])),
                                        rep(alpha_Vector[k], nrow(output[which(output[,"Data_Type"] == "Predicted" & output[,"Stage"] == "J" ) ,])),
                                        rep(Mopt_Vector[z], nrow(output[which(output[,"Data_Type"] == "Predicted" & output[,"Stage"] == "J" ) ,])),
                                        rep(sigma_Vector[a], nrow(output[which(output[,"Data_Type"] == "Predicted" & output[,"Stage"] == "J" ) ,]))
  )))
  
  Pred_A = rbind(Pred_A, data.frame(cbind(output[which(output[,"Data_Type"] == "Predicted" & output[,"Stage"] == "A" ) ,],
                                        rep(Delta_Vector[i], nrow(output[which(output[,"Data_Type"] == "Predicted" & output[,"Stage"] == "A" ) ,])),
                                        rep(Rmax_Vector[j], nrow(output[which(output[,"Data_Type"] == "Predicted" & output[,"Stage"] == "A" ) ,])),
                                        rep(alpha_Vector[k], nrow(output[which(output[,"Data_Type"] == "Predicted" & output[,"Stage"] == "A" ) ,])),
                                        rep(Mopt_Vector[z], nrow(output[which(output[,"Data_Type"] == "Predicted" & output[,"Stage"] == "A" ) ,])),
                                        rep(sigma_Vector[a], nrow(output[which(output[,"Data_Type"] == "Predicted" & output[,"Stage"] == "A" ) ,]))
  )))
           
  # Obs_A[,i,j,k] = output[which(output[,"Data_Type"] == "Observed" & output[,"Stage"] == "A" ) ,"value"]
  # Pred_A[,i,j,k] = output[which(output[,"Data_Type"] == "Predicted" & output[,"Stage"] == "A" ) ,"value"]
  # Error_array_A[y,i,j,k] = Obs_A[y,i,j,k]-Pred_A[y,i,j,k]
  # #}
  # 
  # }
  # 

        }
    }
  }
}

}

years_A = unique(Obs_A[,"Year"])
years_J = unique(Obs_J[,"Year"])



Diff_A = abs((Obs_A[,"value"]-Pred_A[,"value"])/Obs_A[,"value"])
Diff_J = abs((Obs_J[,"value"]-Pred_J[,"value"])/Obs_J[,"value"])

Parameter_combo_mean_diff_A = c()

Delta_A = c()
Rmax_A = c()
alpha_A = c()
Mopt_A = c()
sigma_A = c()

iterations = length(seq(1,length(Diff_A)/length(years_A),1))

start_A = seq(1, (length(Diff_A)-length(years_A)+1),length(years_A))
end_A = seq(length(years_A), length(Diff_A),length(years_A))

for(i in 1:iterations)
{


  Parameter_combo_mean_diff_A[i] = mean(Diff_A[start_A[i]:end_A[i]])
  Delta_A[i] = Obs_A[(start_A[i]),6]
  Rmax_A[i] = Obs_A[(start_A[i]),7]
  alpha_A[i] = Obs_A[(start_A[i]),8]
  Mopt_A[i] = Obs_A[(start_A[i]),9]
  sigma_A[i] = Obs_A[(start_A[i]),10]
}

Parameter_combo_mean_diff_J = c()

Delta_J = c()
Rmax_J = c()
alpha_J = c()
Mopt_J = c()
sigma_J = c()

iterations = length(seq(1,length(Diff_J)/length(years_J),1))

start_J = seq(1, (length(Diff_J)-length(years_J)+1),length(years_J))
end_J = seq(length(years_J), length(Diff_J),length(years_J))

for(i in 1:length(start_J))
{
  
 
  Parameter_combo_mean_diff_J[i] = mean(Diff_J[start_J[i]:end_J[i]])
  Delta_J[i] = Obs_J[start_J[i],6]
  Rmax_J[i] = Obs_J[start_J[i],7]
  alpha_J[i] = Obs_J[start_J[i],8]
  Mopt_J[i] = Obs_J[start_J[i],9]
  sigma_J[i] = Obs_J[start_J[i],10]
 
  
}

nrow(Obs_J)

rowMeans(cbind(Parameter_combo_mean_diff_A,Parameter_combo_mean_diff_J))
min(rowMeans(cbind(Parameter_combo_mean_diff_A,Parameter_combo_mean_diff_J)))
which(rowMeans(cbind(Parameter_combo_mean_diff_A,Parameter_combo_mean_diff_J)) == min(rowMeans(cbind(Parameter_combo_mean_diff_A,Parameter_combo_mean_diff_J))))
results = data.frame(cbind(Delta_J,Rmax_J,alpha_J,Mopt_J,rowMeans(cbind(Parameter_combo_mean_diff_A,Parameter_combo_mean_diff_J))))
Mopt_J[1]
Rmax_J[1]
alpha_J[1]
# min(sensitivity_array[,1]) # 2,1 .6 and 2000
# 
# min(sensitivity_array[,2]) # 2,1
# 
# sensitivity_array_total = sensitivity_array[,1] + sensitivity_array[,2]
# min(sensitivity_array_total, na.rm = TRUE) # 1, 11
# 
# Rmax_Vector[2]
#Delta_Vector[1]

# Avg_Obs_J = apply(Obs_J, c(2,3), mean)
# Avg_Obs_A = apply(Obs_A, c(2,3), mean)
# Avg_Pred_J = apply(Pred_J, c(2,3), mean)
# Avg_Pred_A = apply(Pred_A, c(2,3), mean)
# 
# 
# MAPE_J = abs((Avg_Pred_J - Avg_Obs_J)/(Avg_Obs_J))
# MAPE_A = abs((Avg_Pred_A - Avg_Obs_A)/(Avg_Obs_A))
# min(MAPE_J)
# min(MAPE_A)
# 
# MAPE_combined = array(dim = c(nrow(MAPE_J), ncol(MAPE_J)))
# 
# for(j in 1:ncol(MAPE_J))
# {
#   for(i in 1:nrow(MAPE_J))
#   {
#     MAPE_combined[i,j] = mean(c(MAPE_J[i,j],MAPE_A[i,j]))
#     
#   }
#   
# }
# which(MAPE_J == min(MAPE_J))
# which(MAPE_A == min(MAPE_A))
# which(MAPE_combined == min(MAPE_combined, na.rm = TRUE))
# A_Hat_Vector[1]
 Rmax_Vector[36]
# Mopt_Vector[6]
# Delta_Vector[2]
# #.092 and 125 work well MAPE of .2365
# #.097 and 200 work well MAPE of .2335
# #0.0965 and 205 work well MAPE of 0.232986
# 
# #0.0968 and 200 work well MAPE of 0.2185869
# 
# #.0968 and 200 work well MAPE of 0.2103425
# 
# #.096 and 193 work well MAPE of 0.1994644
# #MAPE of 0.002398885 at delta of .06 and Rmax of 1550
# #MAPE of 0.004878781 at delta of .055 and Rmax of 1560
# #Rmax = 1597 Delta = 0.295 MAPE. = 0.00832193
# #MAPE of 0.0687 at delta = 0.025 Rmax = 1640 and Emu = .57
# #MAPE of .0441 Rmax = 1650 delta = .21, A_hat = 1.3344