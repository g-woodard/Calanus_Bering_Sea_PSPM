library(PSPManalysis)
library(tidyverse)
                  #Weight length relation for adults is W = 2*L^3.94 (Petersen, 1986)


DefaultParameters <- c(Rho =  .25, #.1, #per day.  Range of between approximately .1 and 3 from Marañón et al. 2014.  They found no relationship between phytoplankton turnover rate and temperature  
                       Rmax = 2000, #Rmax is a density micrograms of carbon per liter.  This means all other densities including copepod densities are micrograms per liter. Approximately 2000 from Putland and Iverson 2007
                       
                       A_hat = 0.096, #liters per day filtering rate AKA volume swept clear via frost 1972.  This value is actually nonlinear.  Currently using max value.  Units should be liters per day.
                       # Neocalanus plumchrus is close in size to C. marshallae at 567 µg.  Dagg and Wyman (1983) found a range of clearance rates between .0336 1.3344 L/day
                       
                       #bifurcation with Rho = 3 and A hat = 1 min Rmax was 8.69
                       Temp = 273.15,  #20 C is 293.15 K (reference temp)
                       E_mu = .85,  #.85 eV (Savage et al. 2004) 
                       E_M = .4, #.4 for C. glacialis (Tande 1988)
                       E_I = 0.46, #.46 for C. glacialis (Maps et al. 2012).  Ingestion Activation Energy, see if I can find another one for activity or attack rates
                       
                       E_delta = 0.43, 
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
                       epsilon1 = 1.678804, #Approximated from saiz and calbert 2007.  micrograms of carbon per day.  On marine calanoid species.  Saiz and calbert found maximum ingestion rate was temp independent.
                       epsilon2 = 0.703, #Approximated from saiz and calbert 2007.  micrograms of carbon per day.  On marine calanoid species.  Saiz and calbert found maximum ingestion rate was temp independent.
                       
                       phi1 = 1.336596, #For mortality rate per day.  Approximated from  Hirst and Kiørboe 2002 calculated at 15 C.  Dry weight
                       phi2 = -0.092, #For mortality rate per day.  Approximated from  Hirst and Kiørboe 2002 calculated at 15 C. Dry Weight.  Might need to change my reference temp?
                       t0_phi = 288.15,
                       
                       rho1 = 0.02310296  , #micro grams per day, from Ikeda_2007.  Dry weight at 2 C
                       rho2 =  0.7581552   , #micro grams per day, from Ikeda_2007.  Dry weight at 2 C
                       t0_rho = 275.15,
                       
                       
                       max_length = 4, # 4 mm (Petersen, 1986) Development, growth, and survivorship of the
                       #copepod Calanus marshallae in the laboratory
                       #Weight length relation for adults is W = 2*L^3.94 (Petersen, 1986)
                       min_length = .25, # .25 mm (Petersen, 1986) - graph.   Development, growth, and survivorship of the
                       #copepod Calanus marshallae in the laboratory
                       
                       E = .75,
                       #  (micrograms) (Petersen, 1986) -graph pg 68
                       N = 0.8,
                       C1 = exp(-3.18)*exp(.73*7),
                       C2 = exp(-3.18)*exp(.73*8),
                       C3 = exp(-3.18)*exp(.73*9),
                       C4 = exp(-3.18)*exp(.73*10), 
                       C5 = exp(-3.18)*exp(.73*11),
                       C6 = exp(-3.18)*exp(.73*12),   # (micrograms) (Petersen, 1986) pg 66
                       
                       z = 0.002829424 #juvenile to adult ratio 
)




Evolutionary_Parameters = as.vector(DefaultParameters)
Evolutionary_Parameters[4] = 275


exp(-3.18)*exp(.73*7) #size at birth from Petersen 1987 pg 68, figure 7

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



Dry_Weight_DF_10_15_C = Saiz_Calbert_2007_DF_10_15_C[,3]/.455 #convert micrograms of carbon to micrograms of dry weight for copepod
Ingestion_DF_10_15_C = Saiz_Calbert_2007_DF_10_15_C[,5]
Food_Concentration_10_15_C = Saiz_Calbert_2007_DF_10_15_C[,4]

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
  
  return(value)
}


Modified_Parameters_A_hat_.04 = DefaultParameters
Modified_Parameters_A_hat_.04[3] = .04

Modified_Parameters_A_hat_1.3344 = DefaultParameters
Modified_Parameters_A_hat_1.3344[3] = 1.3344


output1_1_A_hat_.096 <-PSPMequi(modelname = "Scripts/StageStructuredBiomass_GW_Calanus_max_ingestion_temp_independent_More_Stages.R", biftype = "EQ", startpoint = c(0.05, 0.05), stepsize = 0.1,
                     parbnds = c(1, 0, 4000), parameters = NULL, minvals = NULL, maxvals = NULL, options = c(c("popZE", "0")),
                     clean = TRUE, force = FALSE, debug = FALSE, silent = FALSE)



output1_1_A_hat_.04 <-PSPMequi(modelname = "Scripts/StageStructuredBiomass_GW_Calanus_max_ingestion_temp_independent_More_Stages.R", biftype = "EQ", startpoint = c(0.1, 0.1), stepsize = 0.1,
                     parbnds = c(1, 0, 4000), parameters = Modified_Parameters_A_hat_.04, minvals = NULL, maxvals = NULL, options = c(c("popZE", "0")),
                     clean = TRUE, force = FALSE, debug = FALSE, silent = FALSE)



output1_1_A_hat_1.3344 <-PSPMequi(modelname = "Scripts/StageStructuredBiomass_GW_Calanus_max_ingestion_temp_independent_More_Stages.R", biftype = "EQ", startpoint = c(0.05, 0.05), stepsize = 0.1,
                                 parbnds = c(1, 0, 4000), parameters = Modified_Parameters_A_hat_1.3344, minvals = NULL, maxvals = NULL, options = c(c("popZE", "0")),
                                 clean = TRUE, force = FALSE, debug = FALSE, silent = FALSE)

df = data.frame(R_max = c(output1_1_A_hat_.04$curvepoints[, 1], output1_1_A_hat_.096$curvepoints[, 1], output1_1_A_hat_1.3344$curvepoints[, 1]),
                 R = c(output1_1_A_hat_.04$curvepoints[, 2], output1_1_A_hat_.096$curvepoints[, 2], output1_1_A_hat_1.3344$curvepoints[, 2]),
                 E = c(output1_1_A_hat_.04$curvepoints[, 5], output1_1_A_hat_.096$curvepoints[, 5], output1_1_A_hat_1.3344$curvepoints[, 5]),
                 N = c(output1_1_A_hat_.04$curvepoints[, 6], output1_1_A_hat_.096$curvepoints[, 6], output1_1_A_hat_1.3344$curvepoints[, 6]),
                 C1 = c(output1_1_A_hat_.04$curvepoints[, 7], output1_1_A_hat_.096$curvepoints[, 7], output1_1_A_hat_1.3344$curvepoints[, 7]),
                 C2 = c(output1_1_A_hat_.04$curvepoints[, 8], output1_1_A_hat_.096$curvepoints[, 8], output1_1_A_hat_1.3344$curvepoints[, 8]),
                 C3 = c(output1_1_A_hat_.04$curvepoints[, 9], output1_1_A_hat_.096$curvepoints[, 9], output1_1_A_hat_1.3344$curvepoints[, 9]),
                 C4 = c(output1_1_A_hat_.04$curvepoints[, 10], output1_1_A_hat_.096$curvepoints[, 10], output1_1_A_hat_1.3344$curvepoints[, 10]),
                 C5 = c(output1_1_A_hat_.04$curvepoints[, 11], output1_1_A_hat_.096$curvepoints[, 11], output1_1_A_hat_1.3344$curvepoints[, 11]),
                 C6 = c(output1_1_A_hat_.04$curvepoints[, 12], output1_1_A_hat_.096$curvepoints[, 12], output1_1_A_hat_1.3344$curvepoints[, 12]),
                
                A_Hat = as.factor(c(rep(.04, nrow(output1_1_A_hat_.04$curvepoints)),rep(.096, nrow(output1_1_A_hat_.096$curvepoints)),rep(1.3344, nrow(output1_1_A_hat_1.3344$curvepoints)))) )

output1_1_non_trivial_.096 <-PSPMequi(modelname = "Scripts/StageStructuredBiomass_GW_Calanus_max_ingestion_temp_independent_More_Stages.R", 
                                      biftype = "EQ", startpoint = c(output1_1_A_hat_.096$bifpoints[1], output1_1_A_hat_.096$bifpoints[1], 0), 
                                      stepsize = 1, parbnds = c(1, output1_1_A_hat_.096$bifpoints[1], 3000), parameters = NULL, 
                                      minvals = NULL, maxvals = NULL, clean = TRUE, force = FALSE, debug = FALSE, silent = FALSE)

output1_1_non_trivial_.04 <-PSPMequi(modelname = "Scripts/StageStructuredBiomass_GW_Calanus_max_ingestion_temp_independent_More_Stages.R", 
                                       biftype = "EQ", startpoint = c(output1_1_A_hat_.04$bifpoints[1], output1_1_A_hat_.04$bifpoints[1], 0), 
                                       stepsize = 1, parbnds = c(1, output1_1_A_hat_.04$bifpoints[1], 3000), 
                                       parameters = Modified_Parameters_A_hat_.04, minvals = NULL, maxvals = NULL, 
                                       clean = TRUE, force = FALSE, debug = FALSE, silent = FALSE)

output1_1_non_trivial_1.3344 <-PSPMequi(modelname = "Scripts/StageStructuredBiomass_GW_Calanus_max_ingestion_temp_independent_More_Stages.R", 
                                        biftype = "EQ", startpoint = c(output1_1_A_hat_1.3344$bifpoints[1], output1_1_A_hat_1.3344$bifpoints[1], 0), 
                                        stepsize = 1, parbnds = c(1, output1_1_A_hat_1.3344$bifpoints[1], 3000), 
                                        parameters = Modified_Parameters_A_hat_1.3344, minvals = NULL, maxvals = NULL, 
                                       clean = TRUE, force = FALSE, debug = FALSE, silent = FALSE)

t1 = output1_1_non_trivial_.096$curvepoints
t2 = output1_1_non_trivial_.04$curvepoints
t3 = output1_1_non_trivial_1.3344$curvepoints



df2 <- data.frame(R_max = c(output1_1_non_trivial_.04$curvepoints[, 1], output1_1_non_trivial_.096$curvepoints[, 1], output1_1_non_trivial_1.3344$curvepoints[, 1]),
                R = c(output1_1_non_trivial_.04$curvepoints[, 2], output1_1_non_trivial_.096$curvepoints[, 2], output1_1_non_trivial_1.3344$curvepoints[, 2]),
                E = c(output1_1_non_trivial_.04$curvepoints[, 5], output1_1_non_trivial_.096$curvepoints[, 5], output1_1_non_trivial_1.3344$curvepoints[, 5]),
                N = c(output1_1_non_trivial_.04$curvepoints[, 6], output1_1_non_trivial_.096$curvepoints[, 6], output1_1_non_trivial_1.3344$curvepoints[, 6]),
                C1 = c(output1_1_non_trivial_.04$curvepoints[, 7], output1_1_non_trivial_.096$curvepoints[, 7], output1_1_non_trivial_1.3344$curvepoints[, 7]),
                C2 = c(output1_1_non_trivial_.04$curvepoints[, 8], output1_1_non_trivial_.096$curvepoints[, 8], output1_1_non_trivial_1.3344$curvepoints[, 8]),
                C3 = c(output1_1_non_trivial_.04$curvepoints[, 9], output1_1_non_trivial_.096$curvepoints[, 9], output1_1_non_trivial_1.3344$curvepoints[, 9]),
                C4 = c(output1_1_non_trivial_.04$curvepoints[, 10], output1_1_non_trivial_.096$curvepoints[, 10], output1_1_non_trivial_1.3344$curvepoints[, 10]),
                C5 = c(output1_1_non_trivial_.04$curvepoints[, 11], output1_1_non_trivial_.096$curvepoints[, 11], output1_1_non_trivial_1.3344$curvepoints[, 11]),
                C6 = c(output1_1_non_trivial_.04$curvepoints[, 12], output1_1_non_trivial_.096$curvepoints[, 12], output1_1_non_trivial_1.3344$curvepoints[, 12]),
                
                Â = as.factor(c(rep(.04, nrow(output1_1_non_trivial_.04$curvepoints)),rep(.096, nrow(output1_1_non_trivial_.096$curvepoints)),rep(1.3344, nrow(output1_1_non_trivial_1.3344$curvepoints)))) )%>% 
  pivot_longer(c(R, E, N, C1, C2, C3, C4, C5, C6))

df2.5 <- data.frame(R_max = c(output1_1_non_trivial_.04$curvepoints[, 1], output1_1_non_trivial_.096$curvepoints[, 1], output1_1_non_trivial_1.3344$curvepoints[, 1]),
                  R = c(output1_1_non_trivial_.04$curvepoints[, 2], output1_1_non_trivial_.096$curvepoints[, 2], output1_1_non_trivial_1.3344$curvepoints[, 2]),
                  E = c(output1_1_non_trivial_.04$curvepoints[, 5], output1_1_non_trivial_.096$curvepoints[, 5], output1_1_non_trivial_1.3344$curvepoints[, 5]),
                  N = c(output1_1_non_trivial_.04$curvepoints[, 6], output1_1_non_trivial_.096$curvepoints[, 6], output1_1_non_trivial_1.3344$curvepoints[, 6]),
                  C1_5 = c(rowSums(output1_1_non_trivial_.04$curvepoints[, 7:11]), rowSums(output1_1_non_trivial_.096$curvepoints[, 7:11]), rowSums(output1_1_non_trivial_1.3344$curvepoints[, 7:11])),
                  C6 = c(output1_1_non_trivial_.04$curvepoints[, 12], output1_1_non_trivial_.096$curvepoints[, 12], output1_1_non_trivial_1.3344$curvepoints[, 12]),
                  
                  Â = as.factor(c(rep(.04, nrow(output1_1_non_trivial_.04$curvepoints)),rep(.096, nrow(output1_1_non_trivial_.096$curvepoints)),rep(1.3344, nrow(output1_1_non_trivial_1.3344$curvepoints)))) )%>% 
  pivot_longer(c(R, E, N, C1_5, C6))

ggplot(df2.5, aes(x = R_max, y = value, color = name)) + 
  labs(x = "Rmax", y = "Biomass Density µg/L")+
  facet_wrap(~Â, scales = "free")+
  geom_line()+ 
  guides(color=guide_legend(title = "Stage"))+
  scale_color_manual(labels=c('C1-C5', 'C6', 'Egg', 'N', 'R'), values = c("blue","black","orange","red","green"))+
  theme_classic()

output1_1_non_trivial_varying_temperature_.096 <-PSPMequi(modelname = "Scripts/StageStructuredBiomass_GW_Calanus_max_ingestion_temp_independent_More_Stages.R", 
                                                          biftype = "EQ", startpoint = c(273.15, output1_1_non_trivial_.096$curvepoints[85,2], 
                                                          output1_1_non_trivial_.096$curvepoints[85,3]), 
                                                          stepsize = 1,
                                                          parbnds = c(3, 273.15, 300), parameters = NULL, minvals = NULL, maxvals = NULL, 
                                                          clean = TRUE, force = FALSE, debug = FALSE, silent = FALSE)
  
output1_1_non_trivial_varying_temperature_.04 <-PSPMequi(modelname = "Scripts/StageStructuredBiomass_GW_Calanus_max_ingestion_temp_independent_More_Stages.R", 
                                                          biftype = "EQ", 
                                                          startpoint = c(273.15, output1_1_non_trivial_.04$curvepoints[83,2], output1_1_non_trivial_.04$curvepoints[83,3]), 
                                                          stepsize = 1,
                                                          parbnds = c(3, 273.15, 300), parameters = Modified_Parameters_A_hat_.04, minvals = NULL, maxvals = NULL, 
                                                          clean = TRUE, force = FALSE, debug = FALSE, silent = FALSE)

output1_1_non_trivial_varying_temperature_1.3344 <-PSPMequi(modelname = "Scripts/StageStructuredBiomass_GW_Calanus_max_ingestion_temp_independent_More_Stages.R", 
                                                            biftype = "EQ", 
                                                            startpoint = c(273.15, output1_1_non_trivial_1.3344$curvepoints[92,2], output1_1_non_trivial_1.3344$curvepoints[92,3]), 
                                                          stepsize = 1,
                                                          parbnds = c(3, 273.15, 300), parameters = Modified_Parameters_A_hat_1.3344, minvals = NULL, maxvals = NULL, 
                                                          clean = TRUE, force = FALSE, debug = FALSE, silent = FALSE)

 Net_Production_A_hat_.04_E = output1_1_non_trivial_varying_temperature_.04$curvepoints[,13]
 Net_Production_A_hat_.04_N = output1_1_non_trivial_varying_temperature_.04$curvepoints[,14]
 Net_Production_A_hat_.04_C1_5 = rowSums(output1_1_non_trivial_varying_temperature_.04$curvepoints[,15:19])
 Net_Production_A_hat_.04_C6 = output1_1_non_trivial_varying_temperature_.04$curvepoints[,20]
 
 Net_Production_A_hat_.04 = c(Net_Production_A_hat_.04_E,Net_Production_A_hat_.04_N,Net_Production_A_hat_.04_C1_5,Net_Production_A_hat_.04_C6)
 
 Temp_A_hat_.04 = rep(as.numeric(output1_1_non_trivial_varying_temperature_.04$curvepoints[,1]),4)

 Stage_.04 = as.factor(c(rep("Egg", nrow(output1_1_non_trivial_varying_temperature_.04$curvepoints)),
                          rep("N", nrow(output1_1_non_trivial_varying_temperature_.04$curvepoints)),
                          rep("C1-C5", nrow(output1_1_non_trivial_varying_temperature_.04$curvepoints)),
                          rep("C6", nrow(output1_1_non_trivial_varying_temperature_.04$curvepoints))))
 length(Stage_.04)
 
 Net_Production_A_hat_.096_E = output1_1_non_trivial_varying_temperature_.096$curvepoints[,13]
 Net_Production_A_hat_.096_N = output1_1_non_trivial_varying_temperature_.096$curvepoints[,14]
 Net_Production_A_hat_.096_C1_5 = rowSums(output1_1_non_trivial_varying_temperature_.096$curvepoints[,15:19])
 Net_Production_A_hat_.096_C6 = output1_1_non_trivial_varying_temperature_.096$curvepoints[,20]

 
 Net_Production_A_hat_.096 = c(Net_Production_A_hat_.096_E,Net_Production_A_hat_.096_N,Net_Production_A_hat_.096_C1_5,Net_Production_A_hat_.096_C6)
 
 Temp_A_hat_.096 = rep(as.numeric(output1_1_non_trivial_varying_temperature_.096$curvepoints[,1]),4)

 Stage_.096 = as.factor(c(rep("Egg", nrow(output1_1_non_trivial_varying_temperature_.096$curvepoints)),
                            rep("N", nrow(output1_1_non_trivial_varying_temperature_.096$curvepoints)),
                            rep("C1-C5", nrow(output1_1_non_trivial_varying_temperature_.096$curvepoints)),
                            rep("C6", nrow(output1_1_non_trivial_varying_temperature_.096$curvepoints))))
 
 Net_Production_A_hat_1.3344_E = output1_1_non_trivial_varying_temperature_1.3344$curvepoints[,13]
 Net_Production_A_hat_1.3344_N = output1_1_non_trivial_varying_temperature_1.3344$curvepoints[,14]
 Net_Production_A_hat_1.3344_C1_5 = rowSums(output1_1_non_trivial_varying_temperature_1.3344$curvepoints[,15:19])
 Net_Production_A_hat_1.3344_C6 = output1_1_non_trivial_varying_temperature_1.3344$curvepoints[,20]
 
 
 Net_Production_A_hat_1.3344 = c(Net_Production_A_hat_1.3344_E,Net_Production_A_hat_1.3344_N,Net_Production_A_hat_1.3344_C1_5,Net_Production_A_hat_1.3344_C6)
 
 
 Temp_A_hat_1.3344 = rep(as.numeric(output1_1_non_trivial_varying_temperature_1.3344$curvepoints[,1]),4)
 
 Stage_1.3344 = as.factor(c(rep("Egg", nrow(output1_1_non_trivial_varying_temperature_1.3344$curvepoints)),
                     rep("N", nrow(output1_1_non_trivial_varying_temperature_1.3344$curvepoints)),
                     rep("C1-C5", nrow(output1_1_non_trivial_varying_temperature_1.3344$curvepoints)),
                     rep("C6", nrow(output1_1_non_trivial_varying_temperature_1.3344$curvepoints))))
 
 
 Net_Production_vs_Temp_DF = data.frame(cbind(
   c(Net_Production_A_hat_.04, Net_Production_A_hat_.096, Net_Production_A_hat_1.3344), 
   c(Temp_A_hat_.04, Temp_A_hat_.096, Temp_A_hat_1.3344) ))
 
 colnames(Net_Production_vs_Temp_DF) = c("Net_Production","Temperature")
 
 A_Hat_varying_temp = as.factor(c(rep(.04, length(Net_Production_A_hat_.04)),rep(.096, length(Net_Production_A_hat_.096)),rep(1.3344, length(Net_Production_A_hat_1.3344))))
 length(A_Hat_varying_temp)
 
 Net_Production_vs_Temp_DF[,"Stage"] = as.factor(c(Stage_.04,  Stage_.096,  Stage_1.3344))
 Net_Production_vs_Temp_DF[,"Â"] = as.factor(A_Hat_varying_temp)
 
 ggplot(Net_Production_vs_Temp_DF, aes(x= Temperature, y = Net_Production, color = Stage))+
          geom_line()+ 
   facet_wrap(~Â, scales = "free")+
          
          labs(x= "Temperature (K)", y="Net Production (µ/day)") +
          theme_classic()
 
 Temp_A_hat = c(Temp_A_hat_.04, Temp_A_hat_.096, Temp_A_hat_1.3344)
 
 Biomass_A_hat_.04_Resource = output1_1_non_trivial_varying_temperature_.04$curvepoints[,2]
 Biomass_A_hat_.04_E = output1_1_non_trivial_varying_temperature_.04$curvepoints[,5]
 Biomass_A_hat_.04_N = output1_1_non_trivial_varying_temperature_.04$curvepoints[,6]
 Biomass_A_hat_.04_C1_5 = rowSums(output1_1_non_trivial_varying_temperature_.04$curvepoints[,7:11])
 Biomass_A_hat_.04_C6 = output1_1_non_trivial_varying_temperature_.04$curvepoints[,12]
 
 Biomass_A_hat_.04 = c(Biomass_A_hat_.04_Resource, Biomass_A_hat_.04_E,Biomass_A_hat_.04_N,Biomass_A_hat_.04_C1_5,Biomass_A_hat_.04_C6)
 
 Temp_A_hat_.04 = rep(as.numeric(output1_1_non_trivial_varying_temperature_.04$curvepoints[,1]),5)
 
 Stage_.04 = as.factor(c(rep("Resource", nrow(output1_1_non_trivial_varying_temperature_.04$curvepoints)),
                         rep("Egg", nrow(output1_1_non_trivial_varying_temperature_.04$curvepoints)),
                         rep("N", nrow(output1_1_non_trivial_varying_temperature_.04$curvepoints)),
                         rep("C1-C5", nrow(output1_1_non_trivial_varying_temperature_.04$curvepoints)),
                         rep("C6", nrow(output1_1_non_trivial_varying_temperature_.04$curvepoints))))
 length(Stage_.04)
 

 
 Biomass_A_hat_.096_Resource = output1_1_non_trivial_varying_temperature_.096$curvepoints[,2]
 Biomass_A_hat_.096_E = output1_1_non_trivial_varying_temperature_.096$curvepoints[,5]
 Biomass_A_hat_.096_N = output1_1_non_trivial_varying_temperature_.096$curvepoints[,6]
 Biomass_A_hat_.096_C1_5 = rowSums(output1_1_non_trivial_varying_temperature_.096$curvepoints[,7:11])
 Biomass_A_hat_.096_C6 = output1_1_non_trivial_varying_temperature_.096$curvepoints[,12]
 
 
 Biomass_A_hat_.096 = c(Biomass_A_hat_.096_Resource,Biomass_A_hat_.096_E,Biomass_A_hat_.096_N,Biomass_A_hat_.096_C1_5,Biomass_A_hat_.096_C6)
 
 Temp_A_hat_.096 = rep(as.numeric(output1_1_non_trivial_varying_temperature_.096$curvepoints[,1]),5)
 
 Stage_.096 = as.factor(c(rep("Resource", nrow(output1_1_non_trivial_varying_temperature_.096$curvepoints)),
                          rep("Egg", nrow(output1_1_non_trivial_varying_temperature_.096$curvepoints)),
                          rep("N", nrow(output1_1_non_trivial_varying_temperature_.096$curvepoints)),
                          rep("C1-C5", nrow(output1_1_non_trivial_varying_temperature_.096$curvepoints)),
                          rep("C6", nrow(output1_1_non_trivial_varying_temperature_.096$curvepoints))))
 
 Biomass_A_hat_1.3344_Resource = output1_1_non_trivial_varying_temperature_1.3344$curvepoints[,2]
 Biomass_A_hat_1.3344_E = output1_1_non_trivial_varying_temperature_1.3344$curvepoints[,5]
 Biomass_A_hat_1.3344_N = output1_1_non_trivial_varying_temperature_1.3344$curvepoints[,6]
 Biomass_A_hat_1.3344_C1_5 = rowSums(output1_1_non_trivial_varying_temperature_1.3344$curvepoints[,7:11])
 Biomass_A_hat_1.3344_C6 = output1_1_non_trivial_varying_temperature_1.3344$curvepoints[,12]
 
 
 Biomass_A_hat_1.3344 = c( Biomass_A_hat_1.3344_Resource, Biomass_A_hat_1.3344_E,Biomass_A_hat_1.3344_N,Biomass_A_hat_1.3344_C1_5,Biomass_A_hat_1.3344_C6)
 
 
 Temp_A_hat_1.3344 = rep(as.numeric(output1_1_non_trivial_varying_temperature_1.3344$curvepoints[,1]),5)
 
 Stage_1.3344 = as.factor(c(rep("Resource", nrow(output1_1_non_trivial_varying_temperature_1.3344$curvepoints)),
                            rep("Egg", nrow(output1_1_non_trivial_varying_temperature_1.3344$curvepoints)),
                            rep("N", nrow(output1_1_non_trivial_varying_temperature_1.3344$curvepoints)),
                            rep("C1-C5", nrow(output1_1_non_trivial_varying_temperature_1.3344$curvepoints)),
                            rep("C6", nrow(output1_1_non_trivial_varying_temperature_1.3344$curvepoints))))
 
 
 Biomass_vs_Temp_DF = data.frame(cbind(
   c(Biomass_A_hat_.04, Biomass_A_hat_.096, Biomass_A_hat_1.3344), 
   c(Temp_A_hat_.04, Temp_A_hat_.096, Temp_A_hat_1.3344) ))
 
 colnames(Biomass_vs_Temp_DF) = c("Biomass","Temperature")
 
 A_Hat_varying_temp = as.factor(c(rep(.04, length(Biomass_A_hat_.04)),rep(.096, length(Biomass_A_hat_.096)),rep(1.3344, length(Biomass_A_hat_1.3344))))
 length(A_Hat_varying_temp)
 
 Biomass_vs_Temp_DF[,"Stage"] = as.factor(c(Stage_.04,  Stage_.096,  Stage_1.3344))
 Biomass_vs_Temp_DF[,"Â"] = as.factor(A_Hat_varying_temp)
 
 ggplot(Biomass_vs_Temp_DF, aes(x= Temperature, y = Biomass, color = Stage))+
   geom_line()+ 
   facet_wrap(~Â, scales = "free")+
   guides(color = guide_legend(title = "Population"))+
   labs(x= "Temperature (K)", y="Biomass (µ/day)") +
   scale_color_manual(labels = c("C1-C5", "C6", "Egg", "N", "R"), values = c("blue","black","orange","red","green"))+
   scale_x_continuous(breaks = round(seq(min(Biomass_vs_Temp_DF$Temperature), max(Biomass_vs_Temp_DF$Temperature), by = 2),0))+
   theme_classic()
 
  df4 <- data.frame(rbind(
               Egg = Biomass_vs_Temp_DF[which(Biomass_vs_Temp_DF[,"Stage"] == "Egg"),c(1,3,4)],
               N = Biomass_vs_Temp_DF[which(Biomass_vs_Temp_DF[,"Stage"] == "N"),c(1,3,4)],
               C1_C5 = Biomass_vs_Temp_DF[which(Biomass_vs_Temp_DF[,"Stage"] == "C1-C5"),c(1,3,4)],
               C6 = Biomass_vs_Temp_DF[which(Biomass_vs_Temp_DF[,"Stage"] == "C6"), c(1,3,4)]
               ))
  
  R_Biomass = Biomass_vs_Temp_DF[which(Biomass_vs_Temp_DF[,"Stage"] == "Resource"),"Biomass"]
               
   df4 = cbind(df4,rep( R_Biomass,4))
    colnames(df4) = c("Biomass", "Stage", "Â", "Resource")
      
    ggplot(df4, aes(x = Resource, y= Biomass, color = Stage)) + 
      facet_wrap(~Â, scale = "free")+
      geom_line() + labs(x= "Resource Density (µg/L)", y="Population Density (µg/L)") +
      guides(color=guide_legend(title = "Stage"))+
      theme_classic()+
      scale_color_manual(labels=c('C1-C5', 'C6', 'Egg', 'N'), values = c("blue","black","orange","red","green"))
      
      
    
  Total_Population_A_Hat = c(rowSums(output1_1_non_trivial_varying_temperature_.04$curvepoints[, 5:12]),
                       rowSums(output1_1_non_trivial_varying_temperature_.096$curvepoints[, 5:12]),
                       rowSums(output1_1_non_trivial_varying_temperature_1.3344$curvepoints[, 5:12]))
  
  Net_Production_A_Hat = c(rowSums(output1_1_non_trivial_varying_temperature_.04$curvepoints[, 13:20]),
                           rowSums(output1_1_non_trivial_varying_temperature_.096$curvepoints[, 13:20]),
                           rowSums(output1_1_non_trivial_varying_temperature_1.3344$curvepoints[, 13:20]))
  
  Fecundity_A_Hat = c(output1_1_non_trivial_varying_temperature_.04$curvepoints[, 3], 
                output1_1_non_trivial_varying_temperature_.096$curvepoints[, 3], 
                output1_1_non_trivial_varying_temperature_1.3344$curvepoints[, 3])
  
  Temperature_A_Hat = c(output1_1_non_trivial_varying_temperature_.04$curvepoints[, 1], 
                       output1_1_non_trivial_varying_temperature_.096$curvepoints[, 1], 
                       output1_1_non_trivial_varying_temperature_1.3344$curvepoints[, 1])
  
  df6 <- data.frame(cbind(Temperature = Temperature_A_Hat,
                    
                    Fecundity = Fecundity_A_Hat,
                    
                    Net_Production = Net_Production_A_Hat,
                    
                    Fecundity = Fecundity_A_Hat/(Total_Population_A_Hat),
                    
                    Net_Production = Net_Production_A_Hat/Total_Population_A_Hat),
                    
                    Â = as.factor(c(rep(.04, length(output1_1_non_trivial_varying_temperature_.04$curvepoints[, 1])),
                                    rep(.096, length(output1_1_non_trivial_varying_temperature_.096$curvepoints[, 1])),
                                    rep(1.3344, length(output1_1_non_trivial_varying_temperature_1.3344$curvepoints[, 1]))))
                    
                    )
  
  colnames(df6) = c("Temperature","Fecundity", "Net_Production", "Fecundity", "Net_Production", "Â")
  
  df6 <- df6 %>%
    pivot_longer(c(Fecundity, Net_Production, Fecundity, Net_Production))
  
  df6 = data.frame(df6)
  
  df6[,"Type"] = rep(c("Standard","Standard","Per Unit Biomass", "Per Unit Biomass"), nrow(df6)/4)
  
  ggplot(df6, aes(y=(value), x = Temperature, color = name, linetype = Â)) + 
    facet_wrap(~Type, nrow = 1, scales = "free")+
    geom_line()+
    theme_classic()+
    labs(x = "Temperature (K)", y ="Value") +
    guides(linetype=guide_legend(title = "Â"), color=guide_legend(title = "Measure"))+
    
    scale_color_manual(labels=c("Fecundity", "Net Production (µg/day)"), 
                       values = c("Black","Red"))+
    
    scale_linetype_manual(labels = c("0.04","0.096","1.3344"), values = c(1,2,9) )+
  
    theme(axis.text.x = element_text(size = 11),
          axis.title.x = element_text(size = 13),
          axis.text.y = element_text(size = 11),
          axis.title.y = element_text(size = 13),
          strip.text.x = element_text(size = 15, hjust = 0),
          panel.spacing = unit(2, "lines"),
          strip.background = element_rect(color="white", fill="white", linetype="solid"))+
    scale_x_continuous(breaks = round(seq(min(df6$Temperature), max(df6$Temperature), by = 2),0))
    
   
  # Biomass_A_hat_1.3344_E = output1_1_non_trivial_varying_temperature_1.3344$curvepoints[,5]
  # Biomass_A_hat_1.3344_N = output1_1_non_trivial_varying_temperature_1.3344$curvepoints[,5]
  # Biomass_A_hat_1.3344_C1_5 = rowSums(output1_1_non_trivial_varying_temperature_1.3344$curvepoints[,7:11])
  # Biomass_A_hat_1.3344_C6 = output1_1_non_trivial_varying_temperature_1.3344$curvepoints[,12]
  # 
 
  Adult_Biomass = c(Biomass_A_hat_.04_C6, Biomass_A_hat_.096_C6, Biomass_A_hat_1.3344_C6)
  Juvenile_Biomass = c(rowSums(cbind(Biomass_A_hat_.04_E,Biomass_A_hat_.04_N,Biomass_A_hat_.04_C1_5)),
                       rowSums(cbind(Biomass_A_hat_.096_E,Biomass_A_hat_.096_N,Biomass_A_hat_.096_C1_5)),
                       rowSums(cbind(Biomass_A_hat_1.3344_E,Biomass_A_hat_1.3344_N,Biomass_A_hat_1.3344_C1_5))
                       )
  
  
  
  Adult_Juvenile_Ratio =  Adult_Biomass/Juvenile_Biomass
  
  R_= c(Biomass_A_hat_.04_Resource,
        Biomass_A_hat_.096_Resource,
        Biomass_A_hat_1.3344_Resource)
  A_Hat_ = as.factor(c(rep(".04", length(Biomass_A_hat_.04_C6)),
             rep(".096",length(Biomass_A_hat_.096_C6)),
             rep("1.3344",length(Biomass_A_hat_1.3344_C6)) ) )
                 
  Adult_Juvenile_Ratio_df = data.frame(cbind(Adult_Juvenile_Ratio, R_) )
  Adult_Juvenile_Ratio_df[,"Â"] = A_Hat_
  colnames(Adult_Juvenile_Ratio_df) = c("Ratio", "Resource", "Â")
  
  ggplot(Adult_Juvenile_Ratio_df, aes(y=Ratio, x = Resource, linetype = Â)) + 
    
    geom_line()+
    theme_classic()+
    labs(x = "Resource Density (µg/L)", y ="Adult:Juvenile Biomass Ratio") +
    scale_color_manual(labels=c("0.04", "0.096", "1.3344"))+
    
    scale_linetype_manual(labels = c("0.04","0.096","1.3344"), values = c(1,2,9) )+
    guides(linetype=guide_legend(title = "Â"))+
    
    theme(axis.text.x = element_text(size = 11),
          axis.title.x = element_text(size = 13),
          axis.text.y = element_text(size = 11),
          axis.title.y = element_text(size = 13),
          strip.text.x = element_text(size = 15, hjust = 0),
          panel.spacing = unit(2, "lines"),
          strip.background = element_rect(color="white", fill="white", linetype="solid"))
   
 
  
  Adult_Juvenile_Ratio_Temp_df = data.frame(cbind(Adult_Juvenile_Ratio, Temperature_A_Hat) )
  Adult_Juvenile_Ratio_Temp_df[,"Â"] = A_Hat_ 
  
  ggplot(Adult_Juvenile_Ratio_Temp_df, aes(y=Adult_Juvenile_Ratio, x = Temperature_A_Hat, linetype = Â)) + 
    
    geom_line()+
   
    theme_classic()+
    
    labs(x = "Temperature (K)", y ="Adult:Juvenile Biomass Ratio") +
    
    scale_color_manual(labels=c("0.04", "0.096", "1.3344"))+
    
    scale_linetype_manual(labels = c("0.04","0.096","1.3344"), values = c(1,2,9) )+
    
    guides(linetype=guide_legend(title = "Â"))+
    
    scale_x_continuous(breaks = round(seq(min(Adult_Juvenile_Ratio_Temp_df$Temperature), max(Adult_Juvenile_Ratio_Temp_df$Temperature), by = 2),0))+

    theme(axis.text.x = element_text(size = 11),
          axis.title.x = element_text(size = 13),
          axis.text.y = element_text(size = 11),
          axis.title.y = element_text(size = 13),
          strip.text.x = element_text(size = 15, hjust = 0),
          panel.spacing = unit(2, "lines"),
          strip.background = element_rect(color="white", fill="white", linetype="solid"))
  

  
 

  
  # Evolutionary_dynamics_1_A_hat_.096 <-PSPMevodyn(modelname="Scripts/StageStructuredBiomass_GW_Calanus_max_ingestion_temp_independent_More_Stages.R",
  #                                                  startpoint = c(output1_1_non_trivial_varying_temperature_.096$curvepoints[1,2],
  #                                                                 output1_1_non_trivial_varying_temperature_.096$curvepoints[1,3],
  #                                                                 .75,
  #                                                                 #  (micrograms) (Petersen, 1986) -graph pg 68
  #                                                                 0.8,
  #                                                                 exp(-3.18)*exp(.73*7),
  #                                                                 exp(-3.18)*exp(.73*8),
  #                                                                 exp(-3.18)*exp(.73*9),
  #                                                                 exp(-3.18)*exp(.73*10), 
  #                                                                 exp(-3.18)*exp(.73*11),
  #                                                                 exp(-3.18)*exp(.73*12)),
  #                    curvepars = c(.05,1000), evopars = c(0,27,0,500,
  #                                                         0,28,0,500,
  #                                                         0,29,0,500,
  #                                                         0,30,0,500,
  #                                                         0,31,0,500,
  #                                                         0,32,0,500,
  #                                                         0,33,0,500,
  #                                                         0,34,0,500
  #                                                         ), covars = NULL, parameters = Evolutionary_Parameters, options = NULL,
  #                    clean = TRUE, force = FALSE, debug = FALSE,
  #                    silent = FALSE)
  # 
  # evolved_sizes = Evolutionary_dynamics_1_A_hat_.096$curvepoints[nrow(Evolutionary_dynamics_1_A_hat_.096$curvepoints),]
  # 
  # 