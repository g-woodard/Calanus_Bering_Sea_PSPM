library(PSPManalysis)
library(tidyverse)
# the life history processes.


DefaultParameters <- c(Rho =  1, #.1, #per day.  Range of between approximately .1 and 3 from Marañón et al. 2014.  They found no relationship between phytoplankton turnover rate and temperature  
                       Rmax = 2000, #Rmax is a density micrograms of carbon per liter.  This means all other densities including copepod densities are micrograms per liter. Approximately 2000 from Putland and Iverson 2007
                       
                       A_hat = 0.094, #liters per day filtering rate AKA volume swept clear via frost 1972.  This value is actually nonlinear.  Currently using max value.  Units should be liters per day.
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
                       t0 = 285.65, #Frost expreiment on attack rate conducted at 12.5 C or 285.65 K
                       sigma = .6 , # (Kiørboe, 2008.) Converts ingested energy to biomass
                       Mopt = 180, #???????????
                       
                       #one gram is 1 million micrograms
                       epsilon1 = 2.303662, #Approximated from saiz and calbert 2007.  micrograms of carbon per day.  On marine calanoid species. 15 C.
                       epsilon2 = 0.7031 , #Approximated from saiz and calbert 2007.  micrograms of carbon per day.  On marine calanoid species.  15 C.
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

Saiz_Calbert_2007_max_ingestion_vs_size_15_C = read.csv("Data/Saiz_Calbet_2007_Max_ingestion_Vs_Size_15_C.csv")
#max ingestion rate in µg C per day
#body weight in µg C

plot(Saiz_Calbert_2007_max_ingestion_vs_size_15_C[,"Max.Ingestion"]~Saiz_Calbert_2007_max_ingestion_vs_size_15_C[,"Body.Size"])
log_max_ingest_15_C = log(Saiz_Calbert_2007_max_ingestion_vs_size_15_C[,"Max.Ingestion"])
log_body_size_15_C = log(Saiz_Calbert_2007_max_ingestion_vs_size_15_C[,"Body.Size"])
lm(log_max_ingest_15_C~log_body_size_15_C)
exp(0.8345)


Modified_Parameters_A_hat_.04 = DefaultParameters
Modified_Parameters_A_hat_.04[3] = .04

Modified_Parameters_A_hat_1.3344 = DefaultParameters
Modified_Parameters_A_hat_1.3344[3] = 1.3344

output1_1_A_hat_.096 <-PSPMequi(modelname = "Scripts/StageStructuredBiomass_GW_Calanus_max_ingestion_temp_independent_V5.R", biftype = "EQ", startpoint = c(0.05, 0.05), stepsize = 0.1,
                     parbnds = c(1, 0, 4000), parameters = NULL, minvals = NULL, maxvals = NULL, options = c(c("popZE", "0")),
                     clean = TRUE, force = FALSE, debug = FALSE, silent = FALSE)


output1_1_A_hat_.04 <-PSPMequi(modelname = "Scripts/StageStructuredBiomass_GW_Calanus_max_ingestion_temp_independent_V5.R", biftype = "EQ", startpoint = c(0.05, 0.05), stepsize = 0.1,
                     parbnds = c(1, 0, 4000), parameters = Modified_Parameters_A_hat_.04, minvals = NULL, maxvals = NULL, options = c(c("popZE", "0")),
                     clean = TRUE, force = FALSE, debug = FALSE, silent = FALSE)


output1_1_A_hat_1.3344 <-PSPMequi(modelname = "Scripts/StageStructuredBiomass_GW_Calanus_max_ingestion_temp_independent_V5.R", biftype = "EQ", startpoint = c(0.05, 0.05), stepsize = 0.1,
                                 parbnds = c(1, 0, 4000), parameters = Modified_Parameters_A_hat_1.3344, minvals = NULL, maxvals = NULL, options = c(c("popZE", "0")),
                                 clean = TRUE, force = FALSE, debug = FALSE, silent = FALSE)

df = data.frame(R_max = c(output1_1_A_hat_.04$curvepoints[, 1], output1_1_A_hat_.096$curvepoints[, 1], output1_1_A_hat_1.3344$curvepoints[, 1]),
                 R = c(output1_1_A_hat_.04$curvepoints[, 2], output1_1_A_hat_.096$curvepoints[, 2], output1_1_A_hat_1.3344$curvepoints[, 2]),
                 J = c(output1_1_A_hat_.04$curvepoints[, 5], output1_1_A_hat_.096$curvepoints[, 5], output1_1_A_hat_1.3344$curvepoints[, 5]),
                 A = c(output1_1_A_hat_.04$curvepoints[, 6], output1_1_A_hat_.096$curvepoints[, 6], output1_1_A_hat_1.3344$curvepoints[, 6]),
                A_Hat = as.factor(c(rep(.04, nrow(output1_1_A_hat_.04$curvepoints)),rep(.096, nrow(output1_1_A_hat_.096$curvepoints)),rep(1.3344, nrow(output1_1_A_hat_1.3344$curvepoints)))) )

output1_1_non_trivial_.096 <-PSPMequi(modelname = "Scripts/StageStructuredBiomass_GW_Calanus_max_ingestion_temp_independent_V5.R", 
                                      biftype = "EQ", startpoint = c(output1_1_A_hat_.096$bifpoints[1], output1_1_A_hat_.096$bifpoints[1], 0), 
                                      stepsize = 1, parbnds = c(1, output1_1_A_hat_.096$bifpoints[1], 3000), parameters = NULL, 
                                      minvals = NULL, maxvals = NULL, clean = TRUE, force = FALSE, debug = FALSE, silent = FALSE)

output1_1_non_trivial_.04 <-PSPMequi(modelname = "Scripts/StageStructuredBiomass_GW_Calanus_max_ingestion_temp_independent_V5.R", 
                                       biftype = "EQ", startpoint = c(output1_1_A_hat_.04$bifpoints[1], output1_1_A_hat_.04$bifpoints[1], 0), 
                                       stepsize = 1, parbnds = c(1, output1_1_A_hat_.04$bifpoints[1], 3000), 
                                       parameters = Modified_Parameters_A_hat_.04, minvals = NULL, maxvals = NULL, 
                                       clean = TRUE, force = FALSE, debug = FALSE, silent = FALSE)

output1_1_non_trivial_1.3344 <-PSPMequi(modelname = "Scripts/StageStructuredBiomass_GW_Calanus_max_ingestion_temp_independent_V5.R", 
                                        biftype = "EQ", startpoint = c(output1_1_A_hat_1.3344$bifpoints[1], output1_1_A_hat_1.3344$bifpoints[1], 0), 
                                        stepsize = 1.5, parbnds = c(1, output1_1_A_hat_1.3344$bifpoints[1], 3000), 
                                        parameters = Modified_Parameters_A_hat_1.3344, minvals = NULL, maxvals = NULL, 
                                       clean = TRUE, force = FALSE, debug = FALSE, silent = FALSE)

t1 = output1_1_non_trivial_.096$curvepoints
t2 = output1_1_non_trivial_.04$curvepoints
t3 = output1_1_non_trivial_1.3344$curvepoints



df2 <- data.frame(R_max = c( output1_1_non_trivial_.04$curvepoints[, 1], 
                             output1_1_non_trivial_.096$curvepoints[, 1], 
                             output1_1_non_trivial_1.3344$curvepoints[, 1] ),
                 
                 R = c( output1_1_non_trivial_.04$curvepoints[, 2], 
                        output1_1_non_trivial_.096$curvepoints[, 2], 
                        output1_1_non_trivial_1.3344$curvepoints[, 2]),
                  
                 J = c( output1_1_non_trivial_.04$curvepoints[, 5], 
                        output1_1_non_trivial_.096$curvepoints[, 5], 
                        output1_1_non_trivial_1.3344$curvepoints[, 5]),
                  
                 A = c( output1_1_non_trivial_.04$curvepoints[, 6], 
                        output1_1_non_trivial_.096$curvepoints[, 6], 
                        output1_1_non_trivial_1.3344$curvepoints[, 6]),
                  
                 A_Hat = as.factor(c(rep(.04, nrow(output1_1_non_trivial_.04$curvepoints)),
                                     rep(.096, nrow(output1_1_non_trivial_.096$curvepoints)),
                                     rep(1.3344, nrow(output1_1_non_trivial_1.3344$curvepoints))))) %>% 
  pivot_longer(c(R, J, A))

ggplot(df2, aes(R_max, value, color = name, linetype= A_Hat)) + 
  geom_line()+ 
  theme_light()

output1_1_non_trivial_varying_temperature_.096 <-PSPMequi(modelname = "Scripts/StageStructuredBiomass_GW_Calanus_max_ingestion_temp_independent_V5.R", 
                                                          biftype = "EQ", startpoint = c(273.15, output1_1_non_trivial_.096$curvepoints[86,2], 
                                                          output1_1_non_trivial_.096$curvepoints[86,3]), 
                                                          stepsize = .5,
                                                          parbnds = c(3, 273.15, 300), parameters = NULL, minvals = NULL, maxvals = NULL, 
                                                          clean = TRUE, force = FALSE, debug = FALSE, silent = FALSE)
  
output1_1_non_trivial_varying_temperature_.04 <-PSPMequi(modelname = "Scripts/StageStructuredBiomass_GW_Calanus_max_ingestion_temp_independent_V5.R", 
                                                          biftype = "EQ", 
                                                          startpoint = c(273.15, output1_1_non_trivial_.04$curvepoints[83,2], output1_1_non_trivial_.04$curvepoints[83,3]), 
                                                          stepsize = .5,
                                                          parbnds = c(3, 273.15, 300), parameters = Modified_Parameters_A_hat_.04, minvals = NULL, maxvals = NULL, 
                                                          clean = TRUE, force = FALSE, debug = FALSE, silent = FALSE)

output1_1_non_trivial_varying_temperature_1.3344 <-PSPMequi(modelname = "Scripts/StageStructuredBiomass_GW_Calanus_max_ingestion_temp_independent_V5.R", 
                                                            biftype = "EQ", 
                                                            startpoint = c(273.15, output1_1_non_trivial_1.3344$curvepoints[66,2], output1_1_non_trivial_1.3344$curvepoints[66,3]), 
                                                          stepsize = 1.1,
                                                          parbnds = c(3, 273.15, 300), parameters = Modified_Parameters_A_hat_1.3344, minvals = NULL, maxvals = NULL, 
                                                          clean = TRUE, force = FALSE, debug = FALSE, silent = FALSE)

 Net_Production_A_hat_.04 = output1_1_non_trivial_varying_temperature_.04$curvepoints[,7]+output1_1_non_trivial_varying_temperature_.04$curvepoints[,8]
 Temp_A_hat_.04 = as.numeric(output1_1_non_trivial_varying_temperature_.04$curvepoints[,1])

 Net_Production_A_hat_.096 = output1_1_non_trivial_varying_temperature_.096$curvepoints[,7] + output1_1_non_trivial_varying_temperature_.096$curvepoints[,8]
 Temp_A_hat_.096 = as.numeric(output1_1_non_trivial_varying_temperature_.096$curvepoints[,1])
 
 Net_Production_A_hat_1.3344 = output1_1_non_trivial_varying_temperature_1.3344$curvepoints[,7] + output1_1_non_trivial_varying_temperature_1.3344$curvepoints[,8]
 Temp_A_hat_1.3344 = as.numeric(output1_1_non_trivial_varying_temperature_1.3344$curvepoints[,1])
 
 A_Hat_varying_temp = as.factor(c(rep(.04, length(Net_Production_A_hat_.04)),rep(.096, length(Net_Production_A_hat_.096)),rep(1.3344, length(Net_Production_A_hat_1.3344))))
 
 Net_Production_vs_Temp_DF = data.frame(cbind(c(Net_Production_A_hat_.04, Net_Production_A_hat_.096,Net_Production_A_hat_1.3344), c(Temp_A_hat_.04, Temp_A_hat_.096,Temp_A_hat_1.3344)) )
 
 nrow(Net_Production_vs_Temp_DF)
 length(A_Hat_varying_temp)
 
 Net_Production_vs_Temp_DF[,"A_Hat"] = A_Hat_varying_temp
 
 colnames(Net_Production_vs_Temp_DF) = c("Net_Production", "Temperature", "A_Hat")
 
 ggplot(Net_Production_vs_Temp_DF, aes(x= Temperature, y = Net_Production, linetype = A_Hat))+
          geom_line()+ 
          guides(linetype=guide_legend(title = "Â"))+
          labs(x= "Temperature (K)", y="Net Production (µ/day)") +
   scale_linetype_manual(labels = c("0.04","0.096","1.3344"), values = c(1,2,9) )+
   scale_x_continuous(breaks = round(seq(min(Net_Production_vs_Temp_DF$Temperature), max(Net_Production_vs_Temp_DF$Temperature), by = 2),0))+
   
          theme_classic()
 
 Temp_A_hat = c(Temp_A_hat_.04, Temp_A_hat_.096, Temp_A_hat_1.3344)
 R_A_hat = c(output1_1_non_trivial_varying_temperature_.04$curvepoints[,2], 
             output1_1_non_trivial_varying_temperature_.096$curvepoints[,2], 
             output1_1_non_trivial_varying_temperature_1.3344$curvepoints[,2] )
 
 J_A_hat = c(output1_1_non_trivial_varying_temperature_.04$curvepoints[,5], 
             output1_1_non_trivial_varying_temperature_.096$curvepoints[,5], 
             output1_1_non_trivial_varying_temperature_1.3344$curvepoints[,5] )
 
 A_A_hat = c(output1_1_non_trivial_varying_temperature_.04$curvepoints[,6], 
             output1_1_non_trivial_varying_temperature_.096$curvepoints[,6], 
             output1_1_non_trivial_varying_temperature_1.3344$curvepoints[,6] )

  df3 <- data.frame(Temperature = Temp_A_hat,
                    R = R_A_hat,
                    J = J_A_hat,
                    A = A_A_hat,
                    Â = A_Hat_varying_temp) %>% 
    pivot_longer(c(R, J, A))
  
  ggplot(df3, aes(x = Temperature, y= value, color = name, linetype = Â)) + 
    geom_line() + labs(x= "Temperature (K)", y="Density (µg/L)") +
    guides(linetype=guide_legend(title = "Â"), color=guide_legend(title = "Population"))+
   theme_classic()+
    scale_color_manual(labels=c('Adult (µg/L)', 'Juvenile (µg/L)', 'Resource (µg C/L)'),
                       values = c("Black","Red","Blue"))+
    scale_x_continuous(breaks = round(seq(min(df3$Temperature), max(df3$Temperature), by = 2),0))+
  
    scale_linetype_manual(labels = c("0.04","0.096","1.3344"), values = c(1,2,9) )
  
  df4 <- data.frame(
                    R = R_A_hat,
                    J = J_A_hat,
                    A = A_A_hat,
                    Â = A_Hat_varying_temp) %>% 
    pivot_longer(c(J, A))
    
    ggplot(df4, aes(x = R, y= value, color = name, linetype = Â)) + 
      geom_line() + labs(x= "Resource Density (µg/L)", y="Population Density (µg/L)") +
      guides(linetype=guide_legend(title = "Â"), color=guide_legend(title = "Population"))+
      theme_classic()+
      scale_color_manual(labels=c('Adult (µ/L)', 'Juvenile (µ/L)', 'Resource (µg C/L)'),
                         values = c("Black","Red","Blue"))+
      scale_linetype_manual(labels = c("0.04","0.096","1.3344"), values = c(1,2,9) )
    
  Total_Population_A_Hat = c(output1_1_non_trivial_varying_temperature_.04$curvepoints[, 5]+output1_1_non_trivial_varying_temperature_.04$curvepoints[, 6],
                       output1_1_non_trivial_varying_temperature_.096$curvepoints[, 5]+output1_1_non_trivial_varying_temperature_.096$curvepoints[, 6],
                       output1_1_non_trivial_varying_temperature_1.3344$curvepoints[, 5]+output1_1_non_trivial_varying_temperature_1.3344$curvepoints[, 6])
  
  Net_Production_A_Hat = c(Net_Production_A_hat_.04, Net_Production_A_hat_.096, Net_Production_A_hat_1.3344)/100
  
  Fecundity_A_Hat = c(output1_1_non_trivial_varying_temperature_.04$curvepoints[, 3], 
                output1_1_non_trivial_varying_temperature_.096$curvepoints[, 3], 
                output1_1_non_trivial_varying_temperature_1.3344$curvepoints[, 3])
  
  Temperature_A_Hat = c(Temp_A_hat_.04, Temp_A_hat_.096,Temp_A_hat_1.3344)
  
  df6 <- data.frame(cbind(Temperature = Temperature_A_Hat,
                    
                    Fecundity = Fecundity_A_Hat,
                    
                    Net_Production = Net_Production_A_Hat,
                    
                    Fecundity = Fecundity_A_Hat/(Total_Population_A_Hat),
                    
                    Net_Production = Net_Production_A_Hat/Total_Population_A_Hat),
                    
                    Â = A_Hat_varying_temp)
  
  colnames(df6) = c("Temperature","Fecundity", "Net_Production", "Fecundity", "Net_Production", "Â")
  
  df6 <- df6 %>%
    pivot_longer(c(Fecundity, Net_Production, Fecundity, Net_Production))
  
  df6 = data.frame(df6)
  
  df6[,"Type"] = rep(c("Standard","Standard","Per Capita", "Per Capita"), nrow(df6)/4)
  
  ggplot(df6, aes(y=(value), x = Temperature, color = name, linetype = Â)) + 
    facet_wrap(~Type, nrow = 1, scales = "free")+
    geom_line()+
    theme_classic()+
    labs(x = "Temperature (K)", y ="Value") +
    guides(linetype=guide_legend(title = "Â"), color=guide_legend(title = "Population"))+
    
    scale_color_manual(labels=c("Fecundity", "Net Production (100 µg/day)"), 
                       values = c("Black","Red"))+
    
    scale_linetype_manual(labels = c("0.04","0.096","1.3344"), values = c(1,2,9) )+
  
    theme(axis.text.x = element_text(size = 11),
          axis.title.x = element_text(size = 13),
          axis.text.y = element_text(size = 11),
          axis.title.y = element_text(size = 13),
          strip.text.x = element_text(size = 15, hjust = 0),
          panel.spacing = unit(2, "lines"),
          strip.background = element_rect(color="white", fill="white", linetype="solid"))+
    scale_x_continuous(breaks = round(seq(min(df3$Temperature), max(df3$Temperature), by = 5),0))
    
   
  
 
  Adult_Juvenile_Ratio = A_A_hat/J_A_hat
  Adult_Juvenile_Ratio_df = data.frame(cbind(Adult_Juvenile_Ratio, R_A_hat) )
  Adult_Juvenile_Ratio_df[,"Â"] = as.factor(A_Hat_varying_temp)
  
  ggplot(Adult_Juvenile_Ratio_df, aes(y=Adult_Juvenile_Ratio, x = R_A_hat, linetype = A_Hat_varying_temp)) + 
    
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
  Adult_Juvenile_Ratio_Temp_df[,"Â"] = as.factor(A_Hat_varying_temp)
  
  ggplot(Adult_Juvenile_Ratio_df, aes(y=Adult_Juvenile_Ratio, x = Temperature_A_Hat, linetype = A_Hat_varying_temp)) + 
    
    geom_line()+
   
    theme_classic()+
    
    labs(x = "Temperature (K)", y ="Adult:Juvenile Biomass Ratio") +
    
    scale_color_manual(labels=c("0.04", "0.096", "1.3344"))+
    
    scale_linetype_manual(labels = c("0.04","0.096","1.3344"), values = c(1,2,9) )+
    
    guides(linetype=guide_legend(title = "Â"))+
    
    scale_x_continuous(breaks = round(seq(min(df3$Temperature), max(df3$Temperature), by = 5),0))+

    theme(axis.text.x = element_text(size = 11),
          axis.title.x = element_text(size = 13),
          axis.text.y = element_text(size = 11),
          axis.title.y = element_text(size = 13),
          strip.text.x = element_text(size = 15, hjust = 0),
          panel.spacing = unit(2, "lines"),
          strip.background = element_rect(color="white", fill="white", linetype="solid"))
  
