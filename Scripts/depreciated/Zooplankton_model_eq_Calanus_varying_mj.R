library(PSPManalysis)
library(tidyverse)
# the life history processes.


DefaultParameters <- c(Rho =  .25, #.1, #per day.  Range of between approximately .1 and 3 from Marañón et al. 2014.  They found no relationship between phytoplankton turnover rate and temperature  
                       Rmax = 2000, #Rmax is a density micrograms of carbon per liter.  This means all other densities including copepod densities are micrograms per liter. Approximately 2000 from Putland and Iverson 2007
                       
                       A_hat = 0.096,
                       #liters per day filtering rate AKA volume swept clear via frost 1972.  This value is actually nonlinear.  Currently using max value.  Units should be liters per day.
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
                       Mopt = 150, #???????????
                       
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

Modified_Parameters_Temp_275 = DefaultParameters
Modified_Parameters_Temp_275[4] = 275

Modified_Parameters_Temp_283 = DefaultParameters
Modified_Parameters_Temp_283[4] = 283

output1_1_Temp_283 <-PSPMequi(modelname = "Scripts/StageStructuredBiomass_GW_Calanus_max_ingestion_temp_independent_V5.R", biftype = "EQ", startpoint = c(0.05, 0.05), stepsize = 0.1,
                     parbnds = c(1, 0, 4000), parameters = Modified_Parameters_Temp_283, minvals = NULL, maxvals = NULL, options = c(c("popZE", "0")),
                     clean = TRUE, force = FALSE, debug = FALSE, silent = FALSE)


output1_1_Temp_275 <-PSPMequi(modelname = "Scripts/StageStructuredBiomass_GW_Calanus_max_ingestion_temp_independent_V5.R", biftype = "EQ", startpoint = c(0.05, 0.05), stepsize = 0.1,
                     parbnds = c(1, 0, 4000), parameters = Modified_Parameters_Temp_275, minvals = NULL, maxvals = NULL, options = c(c("popZE", "0")),
                     clean = TRUE, force = FALSE, debug = FALSE, silent = FALSE)

df = data.frame(R_max = c(output1_1_Temp_275$curvepoints[, 1], output1_1_Temp_283$curvepoints[, 1]),
                 R = c(output1_1_Temp_275$curvepoints[, 2], output1_1_Temp_283$curvepoints[, 2]),
                 J = c(output1_1_Temp_275$curvepoints[, 5], output1_1_Temp_283$curvepoints[, 5]),
                 A = c(output1_1_Temp_275$curvepoints[, 6], output1_1_Temp_283$curvepoints[, 6]),
                Temp = as.factor(c(rep(275, nrow(output1_1_Temp_275$curvepoints)),rep(283, nrow(output1_1_Temp_283$curvepoints))) ) )

output1_1_non_trivial_Temp_283 <-PSPMequi(modelname = "Scripts/StageStructuredBiomass_GW_Calanus_max_ingestion_temp_independent_V5.R", 
                                      biftype = "EQ", startpoint = c(output1_1_Temp_283$bifpoints[1], output1_1_Temp_283$bifpoints[1], 0), 
                                      stepsize = 1, parbnds = c(1, output1_1_Temp_283$bifpoints[1], 3000), parameters = Modified_Parameters_Temp_283, 
                                      minvals = NULL, maxvals = NULL, clean = TRUE, force = FALSE, debug = FALSE, silent = FALSE)

output1_1_non_trivial_Temp_275 <-PSPMequi(modelname = "Scripts/StageStructuredBiomass_GW_Calanus_max_ingestion_temp_independent_V5.R", 
                                       biftype = "EQ", startpoint = c(output1_1_Temp_275$bifpoints[1], output1_1_Temp_275$bifpoints[1], 0), 
                                       stepsize = 1, parbnds = c(1, output1_1_Temp_275$bifpoints[1], 3000), 
                                       parameters = Modified_Parameters_Temp_275, minvals = NULL, maxvals = NULL, 
                                       clean = TRUE, force = FALSE, debug = FALSE, silent = FALSE)


t1 = output1_1_non_trivial_Temp_283$curvepoints
t2 = output1_1_non_trivial_Temp_275$curvepoints




df2 <- data.frame(R_max = c( output1_1_non_trivial_Temp_275$curvepoints[, 1], 
                             output1_1_non_trivial_Temp_283$curvepoints[, 1]),
                 
                 R = c( output1_1_non_trivial_Temp_275$curvepoints[, 2], 
                        output1_1_non_trivial_Temp_283$curvepoints[, 2]),
                  
                 J = c( output1_1_non_trivial_Temp_275$curvepoints[, 5], 
                        output1_1_non_trivial_Temp_283$curvepoints[, 5]),
                  
                 A = c( output1_1_non_trivial_Temp_275$curvepoints[, 6], 
                        output1_1_non_trivial_Temp_283$curvepoints[, 6]),
                  
                 Temperature = as.factor(c(rep(275, nrow(output1_1_non_trivial_Temp_275$curvepoints)),
                                     rep(283, nrow(output1_1_non_trivial_Temp_283$curvepoints))))) %>% 
  pivot_longer(c(R, J, A))

ggplot(df2, aes(R_max, value, color = name, linetype= Temperature)) + 
  geom_line()+ 
  theme_light()

output1_1_non_trivial_varying_mj_Temp_283 <-PSPMequi(modelname = "Scripts/StageStructuredBiomass_GW_Calanus_max_ingestion_temp_independent_V5.R", 
                                                          biftype = "EQ", startpoint = c(exp(-3.18)*exp(.73*12) , output1_1_non_trivial_Temp_283$curvepoints[82,2], 
                                                          output1_1_non_trivial_Temp_283$curvepoints[82,3]), 
                                                          stepsize = -.5,
                                                          parbnds = c(27, 0, exp(-3.18)*exp(.73*12)), parameters = Modified_Parameters_Temp_283, minvals = NULL, maxvals = NULL, 
                                                          clean = TRUE, force = FALSE, debug = FALSE, silent = FALSE)
  
output1_1_non_trivial_varying_mj_Temp_275 <-PSPMequi(modelname = "Scripts/StageStructuredBiomass_GW_Calanus_max_ingestion_temp_independent_V5.R", 
                                                          biftype = "EQ", 
                                                          startpoint = c(exp(-3.18)*exp(.73*12), output1_1_non_trivial_Temp_275$curvepoints[85,2], output1_1_non_trivial_Temp_275$curvepoints[85,3]), 
                                                          stepsize = -1,
                                                          parbnds = c(27, 0, exp(-3.18)*exp(.73*12)), parameters = Modified_Parameters_Temp_275, minvals = NULL, maxvals = NULL, 
                                                          clean = TRUE, force = FALSE, debug = FALSE, silent = FALSE)


 Net_Production_Temp_275 = output1_1_non_trivial_varying_mj_Temp_275$curvepoints[,7]+output1_1_non_trivial_varying_mj_Temp_275$curvepoints[,8]
 mj_275 = as.numeric(output1_1_non_trivial_varying_mj_Temp_275$curvepoints[,1])

 Net_Production_Temp_283 = output1_1_non_trivial_varying_mj_Temp_283$curvepoints[,7] + output1_1_non_trivial_varying_mj_Temp_283$curvepoints[,8]
 mj_283 = as.numeric(output1_1_non_trivial_varying_mj_Temp_283$curvepoints[,1])

 varying_temp = as.factor(c(rep("275 K", length(Net_Production_Temp_275)),rep("283 K", length(Net_Production_Temp_283))))
 
 Net_Production_vs_mj_DF = data.frame(cbind(c(Net_Production_Temp_275, Net_Production_Temp_283), c(mj_275, mj_283)) )
 
 
 
 Net_Production_vs_mj_DF[,"Temperature"] = varying_temp
 
 colnames(Net_Production_vs_mj_DF) = c("Net_Production", "mj", "Temperature")
 
 ggplot(Net_Production_vs_mj_DF, aes(x= mj, y = Net_Production, linetype = Temperature))+
          geom_line()+ 
          guides(linetype=guide_legend(title = "Temperature"))+
          labs(x= "Size at Maturity µg", y="Net Production (µ/day)") +
   scale_linetype_manual(labels = c("275 K","283 K"), values = c(1,2,9) )+
          theme_classic()
 
 Temp_mj = c(mj_275, mj_283)
 R_Temp = c(output1_1_non_trivial_varying_mj_Temp_275$curvepoints[,2], 
             output1_1_non_trivial_varying_mj_Temp_283$curvepoints[,2])
 
 J_Temp = c(output1_1_non_trivial_varying_mj_Temp_275$curvepoints[,5], 
             output1_1_non_trivial_varying_mj_Temp_283$curvepoints[,5])
 
 
 A_Temp = c(output1_1_non_trivial_varying_mj_Temp_275$curvepoints[,6], 
             output1_1_non_trivial_varying_mj_Temp_283$curvepoints[,6])

  df3 <- data.frame(mj = Temp_mj,
                    R = R_Temp,
                    J = J_Temp,
                    A = A_Temp,
                    Total = J_Temp+A_Temp,
                    Temperature = varying_temp) %>% 
    pivot_longer(c(R, J, A, Total))
  
  J_Temp_283 = J_Temp[which( varying_temp == 283)]
  Temp_mj_283 = Temp_mj[which( varying_temp == 283)]
  
  Temp_mj_283[which(J_Temp_283 == max(J_Temp_283))] #max of juvenile density occurs at a size at maturity of 59 µg
  
  ggplot(df3, aes(x = mj, y= value, color = name)) +
    facet_wrap(~Temperature, scale = "free")+
    geom_line() + labs(x= "Size at Maturity µg", y="Density (µg/L)") +
    guides(color=guide_legend(title = "Population"))+
   theme_classic()+
    theme(axis.text.x = element_text(size = 12),
  axis.title.x = element_text(size = 13),
  axis.text.y = element_text(size = 12),
  axis.title.y = element_text(size = 13),
  strip.text.x = element_text(size = 15, hjust = 0),
  panel.spacing = unit(1, "lines"),
  strip.background = element_rect(color="white", fill="white", linetype="solid"))+
    scale_color_manual(labels=c('Adult (µg/L)', 'Juvenile (µg/L)', 'Resource (µg C/L)', "Total Calanus"),
                       values = c("Black","Red","Blue", "Purple"))+
    scale_x_continuous(breaks = round(seq(50, max(df3$mj), by = 50),0))
 
  
  df4 <- data.frame(
                    R = R_Temp,
                    J = J_Temp,
                    A = A_Temp,
                    Temperature = varying_temp) %>% 
    pivot_longer(c(J, A))
    
    ggplot(df4, aes(x = R, y= value, color = name, linetype = Temperature)) + 
      geom_line() + labs(x= "Resource Density (µg/L)", y="Population Density (µg/L)") +
      guides(linetype=guide_legend(title = "Â"), color=guide_legend(title = "Population"))+
      theme_classic()+
      scale_color_manual(labels=c('Adult (µ/L)', 'Juvenile (µ/L)', 'Resource (µg C/L)'),
                         values = c("Black","Red","Blue"))+
      scale_linetype_manual(labels = c("275 K","283 K"), values = c(1,2) )
    
  Total_Population = c(output1_1_non_trivial_varying_mj_Temp_275$curvepoints[, 5]+output1_1_non_trivial_varying_mj_Temp_275$curvepoints[, 6],
                       output1_1_non_trivial_varying_mj_Temp_283$curvepoints[, 5]+output1_1_non_trivial_varying_mj_Temp_283$curvepoints[, 6])
                      
  Net_Production = c(Net_Production_Temp_275, Net_Production_Temp_283)
  
  Fecundity = c(output1_1_non_trivial_varying_mj_Temp_275$curvepoints[, 3], 
                output1_1_non_trivial_varying_mj_Temp_283$curvepoints[, 3])
  
  Temperature_mj = c(mj_275, mj_283)
  
  df6 <- data.frame(cbind(mj = Temperature_mj,
                    
                    Fecundity = Fecundity,
                    
                    Net_Production = Net_Production,
                    
                    Fecundity = Fecundity/(Total_Population),
                    
                    Net_Production = Net_Production/Total_Population),
                    
                    Temperature = varying_temp)
  
  colnames(df6) = c("mj","Fecundity", "Net_Production", "Fecundity", "Net_Production", "Temperature")
  
  df6 <- df6 %>%
    pivot_longer(c(Fecundity, Net_Production, Fecundity, Net_Production))
  
  df6 = data.frame(df6)
  
  df6[,"Type"] = rep(c("Standard","Standard","Per Capita", "Per Capita"), nrow(df6)/4)
  
  ggplot(df6, aes(y=(value), x = mj, color = name, linetype = Temperature)) + 
    facet_wrap(~Type, nrow = 1, scales = "free")+
    geom_line()+
    theme_classic()+
    labs(x = "Size at maturity µg", y ="Value") +
    guides(linetype=guide_legend(title = "Temperature (K)"), color=guide_legend(title = "Population"))+
    
    scale_color_manual(labels=c("Fecundity", "Net Production (µg/day)"), 
                       values = c("Black","Red"))+
    
    scale_linetype_manual(labels = c("275 K","283 K"), values = c(1,2) )+
  
    theme(axis.text.x = element_text(size = 11),
          axis.title.x = element_text(size = 13),
          axis.text.y = element_text(size = 11),
          axis.title.y = element_text(size = 13),
          strip.text.x = element_text(size = 15, hjust = 0),
          panel.spacing = unit(2, "lines"),
          strip.background = element_rect(color="white", fill="white", linetype="solid"))+
    scale_x_continuous(breaks = round(seq(50, max(df3$mj), by = 50),0))
    
   
  
 
  Adult_Juvenile_Ratio = A_Temp/J_Temp
  Adult_Juvenile_Ratio_df = data.frame(cbind(Adult_Juvenile_Ratio, R_Temp) )
  Adult_Juvenile_Ratio_df[,"Temperature"] = as.factor(varying_temp)
  
  ggplot(Adult_Juvenile_Ratio_df, aes(y=Adult_Juvenile_Ratio, x = R_Temp, linetype = Temperature)) + 
    
    geom_line()+
    theme_classic()+
    labs(x = "Resource Density (µg/L)", y ="Adult:Juvenile Biomass Ratio") +
      scale_linetype_manual(labels = c("275 K","283 K"), values = c(1,2) )+
    guides(linetype=guide_legend(title = "Temperature"))+
    
    theme(axis.text.x = element_text(size = 11),
          axis.title.x = element_text(size = 13),
          axis.text.y = element_text(size = 11),
          axis.title.y = element_text(size = 13),
          strip.text.x = element_text(size = 15, hjust = 0),
          panel.spacing = unit(2, "lines"),
          strip.background = element_rect(color="white", fill="white", linetype="solid"))
   
  
  Adult_Juvenile_Ratio_Temp_df = data.frame(cbind(Adult_Juvenile_Ratio, Temperature_mj) )
  Adult_Juvenile_Ratio_Temp_df[,"Temperature"] = as.factor(varying_temp)
  
  ggplot(Adult_Juvenile_Ratio_df, aes(y=Adult_Juvenile_Ratio, x = Temperature_mj, linetype = Temperature)) + 
    
    geom_line()+
   
    theme_classic()+
    xlim(50, exp(-3.18)*exp(.73*12))+
    ylim(0.15,0.3)+
    
    labs(x = "Size at maturity (µg)", y ="Adult:Juvenile Biomass Ratio") +
    
    scale_linetype_manual(labels = c("275 K","283 K"), values = c(1,2) )+
    
    guides(linetype=guide_legend(title = "Temperature"))+
    
  

    theme(axis.text.x = element_text(size = 11),
          axis.title.x = element_text(size = 13),
          axis.text.y = element_text(size = 11),
          axis.title.y = element_text(size = 13),
          strip.text.x = element_text(size = 15, hjust = 0),
          panel.spacing = unit(2, "lines"),
          strip.background = element_rect(color="white", fill="white", linetype="solid"))
  