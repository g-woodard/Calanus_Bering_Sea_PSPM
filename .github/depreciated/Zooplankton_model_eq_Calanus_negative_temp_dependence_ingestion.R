library(PSPManalysis)
library(tidyverse)


DefaultParameters <- c(Rho =  1, #.1, #per day.  Range of between approximately .1 and 3 from Marañón et al. 2014.  They found no relationship between phytoplankton turnover rate and temperature  
                       Rmax = 2000, #Rmax is a density micrograms of carbon per liter.  This means all other densities including copepod densities are micrograms per liter. Approximately 2000 from Putland and Iverson 2007
                       
                       A_hat = 0.094, #liters per day filtering rate AKA volume swept clear via frost 1972.  This value is actually nonlinear.  Currently using max value.  Units should be liters per day.
                       # Neocalanus plumchrus is close in size to C. marshallae at 567 µg.  Dagg and Wyman (1983) found a range of clearance rates between .0336 1.3344 L/day
                       
                       #bifurcation with Rho = 3 and A hat = 1 min Rmax was 8.69
                       Temp = 275.15,  #20 C is 293.15 K (reference temp)
                       E_mu = .85,  #.85 eV (Savage et al. 2004) 
                       E_M = .4, #.4 for C. glacialis (Tande 1988)
                       E_I = 0.46, #.46 for C. glacialis (Maps et al. 2012).  Ingestion Activation Energy, see if I can find another one for activity or attack rates
                       
                       omega =  0.35063579, #size exponent ingestion, 
                       beta = 0.88576314, #Resource exponent ingestion, 
                       theta =  0.01602631, #temperature coefficient ingestion, 
                       gamma =  -8.95972091, #intercept ingestion,
                       
                       E_delta = 0.43, 
                       cI = 0, #Jan assumes a value of 0 in Roach model 
                       cM = 0.02,# Jan tests the Roach model with values of -.02, 0, and .02 
                       Lambda1 = 2, #(Petersen, 1986)
                       Lamda2 = 3.94, #(Petersen, 1986)
                       k= 8.617e-5, #boltzmann constant
                       alpha = .75, #guess
                       t0 = 285.65, #Frost expreiment on attack rate conducted at 12.5 C or 285.65 K
                       sigma = .6 , # (Kiørboe, 2008.) Converts ingested energy to biomass
                       
                       Mopt = 180, #???????????
                       
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
                       
                       mh = .75, # .75 ug (micrograms) (Petersen, 1986) -graph pg 68
                       mj = exp(-3.18)*exp(.73*12)   # ug (micrograms) (Petersen, 1986) pg 66
                       
                     
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



output1_1 <-PSPMequi(modelname = "Scripts/StageStructuredBiomass_GW_Calanus_ingestion_Size_Food_Temp_Simultaneously_V5.R", biftype = "EQ", startpoint = c(0.05, 0.05), stepsize = 0.1,
                     parbnds = c(1, 0, 4000), parameters = NULL, minvals = NULL, maxvals = NULL, options = c(c("popZE", "0")),
                     clean = TRUE, force = FALSE, debug = FALSE, silent = FALSE)

#Modified_Parameters = DefaultParameters

df = data.frame(R_max = output1_1$curvepoints[, 1],
                 R = output1_1$curvepoints[, 2],
                 J = output1_1$curvepoints[, 5], 
                 A = output1_1$curvepoints[, 6]
)


output1_1_non_trivial <-PSPMequi(modelname = "Scripts/StageStructuredBiomass_GW_Calanus_ingestion_Size_Food_Temp_Simultaneously_V5.R", 
                                       biftype = "EQ", startpoint = c(output1_1$bifpoints[1], output1_1$bifpoints[1], 0), 
                                       stepsize = 1, parbnds = c(1, output1_1$bifpoints[1], 3000), 
                                       parameters = NULL, minvals = NULL, maxvals = NULL, 
                                       clean = TRUE, force = FALSE, debug = FALSE, silent = FALSE)

t1 = output1_1_non_trivial$curvepoints



df2 <- data.frame(R_max = output1_1_non_trivial$curvepoints[, 1], 
                           
                 R = output1_1_non_trivial$curvepoints[, 2],
                 J = output1_1_non_trivial$curvepoints[, 5], 
                 A = output1_1_non_trivial$curvepoints[, 6] 
                )%>% 
  pivot_longer(c(R, J, A))

ggplot(df2, aes(R_max, value, color = name)) + 
  geom_line()+ 
  theme_light()

output1_1_non_trivial_varying_temperature <-PSPMequi(modelname = "Scripts/StageStructuredBiomass_GW_Calanus_ingestion_Size_Food_Temp_Simultaneously_V5.R", 
                                                          biftype = "EQ", startpoint = c(273.15, output1_1_non_trivial$curvepoints[85,2], 
                                                          output1_1_non_trivial$curvepoints[85,3]), 
                                                          stepsize = .1,
                                                          parbnds = c(3, 273.15, 300), parameters = NULL, minvals = NULL, maxvals = NULL, 
                                                          clean = TRUE, force = FALSE, debug = FALSE, silent = FALSE)
 
 Net_Production = output1_1_non_trivial_varying_temperature$curvepoints[,7]+output1_1_non_trivial_varying_temperature$curvepoints[,8]
 Temperature = as.numeric(output1_1_non_trivial_varying_temperature$curvepoints[,1]) 
 R = output1_1_non_trivial_varying_temperature$curvepoints[,2]
 J = output1_1_non_trivial_varying_temperature$curvepoints[,5]
 A = output1_1_non_trivial_varying_temperature$curvepoints[,6]
 Fecundity = output1_1_non_trivial_varying_temperature$curvepoints[,3]/10

 Total_Population = c(output1_1_non_trivial_varying_temperature$curvepoints[, 5]+output1_1_non_trivial_varying_temperature$curvepoints[, 6])
                      
  df6 <- data.frame(cbind(Temperature = Temperature,
                                              
  Fecundity = Fecundity,
                                              
  Net_Production = Net_Production,
                                              
  Fecundity = Fecundity/(Total_Population),
                                              
  Net_Production = Net_Production/Total_Population)
                                        
 )
                      
  colnames(df6) = c("Temperature","Fecundity", "Net_Production", "Fecundity", "Net_Production")
                      
  df6 <- df6 %>%
  pivot_longer(c(Fecundity, Net_Production, Fecundity, Net_Production))
                      
  df6 = data.frame(df6)
                      
  df6[,"Type"] = rep(c("Standard","Standard","Per Capita", "Per Capita"), nrow(df6)/4)
                      
  ggplot(df6, aes(y=(value), x = Temperature, color = name)) + 
  facet_wrap(~Type, nrow = 1, scales = "free")+
  geom_line()+
  theme_classic()+
  labs(x = "Temperature (K)", y ="Value") +
  guides(color=guide_legend(title = ""))+
  scale_color_manual(labels=c("Fecundity (10s)", "Net Production (µg/day)"), 
                                           values = c("Black","Red"))+
  
  theme(axis.text.x = element_text(size = 11),
  axis.title.x = element_text(size = 13),
  axis.text.y = element_text(size = 11),
  axis.title.y = element_text(size = 13),
  strip.text.x = element_text(size = 15, hjust = 0),
  panel.spacing = unit(2, "lines"),
  strip.background = element_rect(color="white", fill="white", linetype="solid"))
  
 
  df7 <- data.frame(cbind(
    Temperature = Temperature,
    R = R,
    J = J,
    A = A)) %>%
    pivot_longer(c(R,J,A))
                      
  ggplot(df7, aes(y=(value), x = Temperature, color = name)) + 
    
    geom_line()+
    theme_classic()+
    labs(x = "Temperature (K)", y ="Value") +
    guides(color=guide_legend(title = "Density"))+
    scale_color_manual(labels=c("Adult (µg/L)", "Juvenile (µg/L)", "Resource Density"), 
                       values = c("Black","Red", "Blue"))+
    
    theme(axis.text.x = element_text(size = 11),
          axis.title.x = element_text(size = 13),
          axis.text.y = element_text(size = 11),
          axis.title.y = element_text(size = 13),
          strip.text.x = element_text(size = 15, hjust = 0),
          panel.spacing = unit(2, "lines"),
          strip.background = element_rect(color="white", fill="white", linetype="solid"))+
    scale_x_continuous(breaks = round(seq(min(df3$Temperature), max(df3$Temperature), by = 5),0))
  
  df8 <- data.frame(cbind(
   
    R = R,
    J = J, #juvenile density in 10s
    A = A
   
    )) %>%
    pivot_longer(c(J,A))
  
  ggplot(df8, aes(y=(value), x = R, color = name)) + 
    
    geom_line()+
    theme_classic()+
    labs(x = "Resource Density µg/L", y ="Density µg/L") +
    guides(color=guide_legend(title = ""))+
    scale_color_manual(labels=c("Adult","Juvenile"),
                       values = c("Black","Red", "Blue", "Green" , "Purple"))+

    theme(axis.text.x = element_text(size = 11),
          axis.title.x = element_text(size = 13),
          axis.text.y = element_text(size = 11),
          axis.title.y = element_text(size = 13),
          strip.text.x = element_text(size = 15, hjust = 0),
          panel.spacing = unit(2, "lines"),
          strip.background = element_rect(color="white", fill="white", linetype="solid"))
  
  