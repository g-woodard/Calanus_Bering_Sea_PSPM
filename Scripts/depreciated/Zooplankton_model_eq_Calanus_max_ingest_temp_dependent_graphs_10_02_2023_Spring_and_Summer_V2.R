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

DefaultParameters <- c(Delta = 1, #turnover rate is 1 divided by the per capita growth rate
                       # Turnover is 1, #per day.  Range of between approximately .1 and 3 from Marañón et al. 2014.  They found no relationship between phytoplankton turnover rate and temperature  
                       Rmax = 2000, #Rmax is a density micrograms of carbon per liter.  This means all other densities including copepod densities are micrograms per liter. Approximately 2000 from Putland and Iverson 2007
                       
                       A_hat = 0.096, #0.096 liters per day filtering rate AKA volume swept clear via frost 1972. Units should be liters per day.
                       # Neocalanus plumchrus is close in size to C. marshallae at 567 µg.  Dagg and Wyman (1983) found a range of clearance rates between .0336 1.3344 L/day
                       
                       
                       Temp = 273.15,  #20 C is 293.15 K 
                       E_mu = .57,  #.57 eV (McCoy et al. 2008) 
                       E_M = .55, #55, #.55 (Maps et al. 2014)
                       E_I = .46, #.46 for C. glacialis (Maps et al. 2012).  Ingestion Activation Energy, see if I can find another one for activity or attack rates
                       #Average of .6 Savage et al. 2004, as cited in (Crossier 1926; Raven and Geider 1988; Vetter 1995; Gillooly et al. 2001)
                       E_Delta = 0.5, #average activation energy of phytoplankton growth from Barton and Yvon-Durocher (2019) 
                       #Im = 29.89,
                       Im = 11.26, 
                       #mean of just calanus at 15 C is 22.48333.  Mean of all species at 15 C is 17.74286
                       #Im = 17.74286,
                       t0_Im = 285.65, #average of Saiz Calbet data restricted to 10-15 C
                       
                       #286.803, #mean temp of saiz and calbet dataframe when restricted to experiments between 10 and 15 degrees C
                       cI = 0, #Jan assumes a value of 0 in Roach model 
                       cM = 0.0,# Jan tests the Roach model with values of -.02, 0, and .02 
                       Lambda1 = 2, #(Petersen, 1986)
                       Lamda2 = 3.94, #(Petersen, 1986)
                       k = 8.617e-5, #boltzmann constant
                       alpha = 0.75, #guess
                       t0 = 285.65, #Frost experiment on attack rate conducted at 12.5 C or 285.65 K
                       sigma = 0.7 , #0.6 (Kiørboe, 2008.) Converts ingested energy to biomass
                       #0.66 works well
                       Mopt = 150, #???????????
                        
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

library(lubridate)

Calanus_Juvenile = Bering_Calanus_Data[which(Bering_Calanus_Data[,"STAGE_NAME"] != "ADULT"),c(12, 33, 42)]

Juvenile_Months = month(Calanus_Juvenile[,1])

Calanus_Adult = Bering_Calanus_Data[which(Bering_Calanus_Data[,"STAGE_NAME"] == "ADULT"), c(12, 33, 42)]

Adult_Months = month(Calanus_Adult[,1])

Calanus_Juvenile = cbind(Calanus_Juvenile, Juvenile_Months)
Calanus_Juvenile[,"Stage"] = rep("Juvenile", nrow(Calanus_Juvenile))
colnames(Calanus_Juvenile) = c("Date","Year","Weight","Month","Stage")

Calanus_Adult = cbind(Calanus_Adult,Adult_Months)
Calanus_Adult[,"Stage"] = rep("Adult", nrow(Calanus_Adult))
colnames(Calanus_Adult) = c("Date","Year","Weight","Month","Stage")

Calanus_Biomass_DF = rbind(Calanus_Juvenile, Calanus_Adult)

Bering_SST = read.csv("Data/Bering_Sea_SST.csv")[45:71,] #1992 through 2018
Bering_SST_Spring_Summer = (Bering_SST[,4:9]+ 273.15)  %>%
  mutate("Year" = c(1992:2018), .before = March)

year_sequence = as.numeric(seq(1992, 2018, 1))

month_sequence = as.numeric(3:8)

Juvenile_Mean_Weights = data.frame(matrix(nrow =  length(year_sequence), ncol = length(month_sequence)))
colnames(Juvenile_Mean_Weights) = c("March","April","May","June","July","August")


Adult_Mean_Weights = data.frame(matrix(nrow =  length(year_sequence), ncol = length(month_sequence)))
colnames(Adult_Mean_Weights) = c("March","April","May","June","July","August")


for(i in 1:length(year_sequence))
{
  for(j in 1:length(month_sequence))
  {
  Juvenile_Mean_Weights[i,j] = mean(Calanus_Juvenile[which(Calanus_Juvenile$Year == year_sequence[i] & Calanus_Juvenile$Month == month_sequence[j]),"Weight"],na.rm = TRUE)
  Adult_Mean_Weights[i,j] = mean(Calanus_Adult[which(Calanus_Adult$Year == year_sequence[i] & Calanus_Adult$Month == month_sequence[j]),"Weight"],na.rm = TRUE)
  }
}

Juvenile_Mean_Weights <- Juvenile_Mean_Weights %>%
  mutate("Year" = c(1992:2018), .before = March)

Adult_Mean_Weights <- Adult_Mean_Weights %>%
  mutate("Year" = c(1992:2018), .before = March)

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


Modified_Parameters_A_hat_.096 = DefaultParameters



output1_1_A_hat_.096 <-PSPMequi(modelname = "Scripts/StageStructuredBiomass_GW_Calanus_max_ingestion_temp_dependent_V5_9_15_2023.R", biftype = "EQ", startpoint = c(1, 1), 
                     stepsize = 1.5,
                     parbnds = c(1, 1, 5000), parameters = Modified_Parameters_A_hat_.096, minvals = NULL, maxvals = NULL, options = c(c("popZE", "0")),
                     clean = TRUE, force = FALSE, debug = FALSE, silent = FALSE)


df = data.frame(R_max = output1_1_A_hat_.096$curvepoints[, 1],
                 R = output1_1_A_hat_.096$curvepoints[, 2],
                 J =output1_1_A_hat_.096$curvepoints[, 5],
                 A = output1_1_A_hat_.096$curvepoints[, 6],
                A_Hat = as.factor(rep(.096, nrow(output1_1_A_hat_.096$curvepoints))) )

output1_1_non_trivial_.096 <-PSPMequi(modelname = "Scripts/StageStructuredBiomass_GW_Calanus_max_ingestion_temp_dependent_V5_9_15_2023.R", 
                                      biftype = "EQ", startpoint = c(output1_1_A_hat_.096$bifpoints[1], output1_1_A_hat_.096$bifpoints[2], 0), 
                                      stepsize = 1.25, parbnds = c(1, output1_1_A_hat_.096$bifpoints[1], 5000), parameters = Modified_Parameters_A_hat_.096, 
                                      minvals = NULL, maxvals = NULL, clean = TRUE, force = FALSE, debug = FALSE, silent = FALSE)


ingestion_.096 = output1_1_non_trivial_.096$curvepoints[,4]#/output1_1_non_trivial_.096$curvepoints[,5]+output1_1_non_trivial_.096$curvepoints[,6]
Rmax_.096 = output1_1_non_trivial_.096$curvepoints[,1]
max_ingestion_.096 = output1_1_non_trivial_.096$curvepoints[,9]

Ingestion_DF = data.frame(
  Ingestion = ingestion_.096,
  Imax = max_ingestion_.096,
  
  Rmax = Rmax_.096
  
  )

Ingestion_DF2 = Ingestion_DF  %>%
  pivot_longer(c(Ingestion,Imax))

ggplot(Ingestion_DF2, aes(x = Rmax, y = value, color = name))+
  geom_line()+
  #guides(linetype=guide_legend(title = "Â"))+
  labs(x= "Rmax (µg/L)", y="Ingestion rate (µg/day)") +
  theme_light()
  

t1 = output1_1_non_trivial_.096$curvepoints


start_.096 = median(which(t1[,"Rmax"] < (Modified_Parameters_A_hat_.096[2]+500) & t1[,"Rmax"] > (Modified_Parameters_A_hat_.096[2]-500)))

start_.096 = round(start_.096, 0)

df2 <- data.frame(R_max = output1_1_non_trivial_.096$curvepoints[, 1], 
                            
                 
                 R = output1_1_non_trivial_.096$curvepoints[, 2], 
                       
                 J = output1_1_non_trivial_.096$curvepoints[, 5], 
                  
                 A = output1_1_non_trivial_.096$curvepoints[, 6])
                 
df2 <- df2 %>% 
pivot_longer(c(R, J, A))

ggplot(df2, aes(R_max, value, color = name)) + 
  geom_line()+ 
  theme_light()

output1_1_non_trivial_varying_temperature_.096 <-PSPMequi(modelname = "Scripts/StageStructuredBiomass_GW_Calanus_max_ingestion_temp_dependent_V5_9_15_2023.R", 
                                                          biftype = "EQ", startpoint = c(273.15, output1_1_non_trivial_.096$curvepoints[start_.096,2], 
                                                          output1_1_non_trivial_.096$curvepoints[start_.096,3]), 
                                                          stepsize = 0.5,
                                                          parbnds = c(3, 273.15, 305), parameters = Modified_Parameters_A_hat_.096, minvals = NULL, maxvals = NULL, 
                                                          clean = TRUE, force = FALSE, debug = FALSE, silent = FALSE)

Total_Population_A_Hat = output1_1_non_trivial_varying_temperature_.096$curvepoints[, 5]+output1_1_non_trivial_varying_temperature_.096$curvepoints[, 6]

I = output1_1_non_trivial_varying_temperature_.096$curvepoints[,4]
per_capita_I = I/Total_Population_A_Hat
I_max = output1_1_non_trivial_varying_temperature_.096$curvepoints[,9]
per_capita_Imax = I_max/Total_Population_A_Hat
Metabolism = output1_1_non_trivial_varying_temperature_.096$curvepoints[,10]
per_capita_Metabolism = Metabolism/Total_Population_A_Hat
Temp_Imax = output1_1_non_trivial_varying_temperature_.096$curvepoints[,1]
E_Imax = output1_1_non_trivial_varying_temperature_.096$curvepoints[,2]
I_DF = data.frame(cbind(I, I_max, Temp_Imax))
colnames(I_DF) = c("I","Imax","Temperature")

I_DF <- I_DF %>% 
  pivot_longer(c(I, Imax))
colnames(I_DF) = c("Temperature","Measurement", "value")


M_DF = data.frame(cbind(Temp_Imax,Metabolism))
colnames(M_DF) = c("Temperature","Metabolism")

per_capita_M_DF = data.frame(cbind(Temp_Imax,per_capita_Metabolism))
colnames(per_capita_M_DF) = c("Temperature","Metabolism")

ggplot()+
  geom_line(data = per_capita_M_DF, aes(x = Temperature, y = Metabolism) )+
  labs(x= "Temperature (K)", y="µg O2 /day") +
  theme_light()


ggplot()+
  geom_line(data= M_DF, aes(x = Temperature, y = (Metabolism)) )+
  labs(x= "Temperature (K)", y="µg O2/day") +
theme_light()

per_capita_I_DF = data.frame(cbind(Temp_Imax,per_capita_I,per_capita_Imax))

per_capita_I_DF <- per_capita_I_DF %>% 
  pivot_longer(c(per_capita_I,per_capita_Imax))

colnames(per_capita_I_DF) = c("Temperature","Measurement","Value")

ggplot()+
  geom_line(data = per_capita_I_DF, aes(x = Temperature, y = Value, color = Measurement) )+
  labs(x= "Temperature (K)", y="µg /day") +
  theme_light()


ggplot()+
  geom_line(data= I_DF, aes(x = Temperature, y = value, color = Measurement))+
  labs(x= "Temperature (K)", y="Measurement") +
  theme_light()

 Net_Production_A_hat_.096 = output1_1_non_trivial_varying_temperature_.096$curvepoints[,7] + output1_1_non_trivial_varying_temperature_.096$curvepoints[,8]
 Temp_A_hat_.096 = as.numeric(output1_1_non_trivial_varying_temperature_.096$curvepoints[,1])
 Net_Production_A_hat_.096_J = output1_1_non_trivial_varying_temperature_.096$curvepoints[,7]
 #Total_Production_A_hat_.096_J =  Net_Production_A_hat_.096_J*output1_1_non_trivial_varying_temperature_.096$curvepoints[,5]
 Net_Production_A_hat_.096_A = output1_1_non_trivial_varying_temperature_.096$curvepoints[,8]
 #Total_Production_A_hat_.096_A =  Net_Production_A_hat_.096_A*output1_1_non_trivial_varying_temperature_.096$curvepoints[,6]
 

 Net_Production_vs_Temp_DF = data.frame(cbind(Net_Production_A_hat_.096, Temp_A_hat_.096) )

 

 
 colnames(Net_Production_vs_Temp_DF) = c("Net_Production", "Temperature")
 
 ggplot(Net_Production_vs_Temp_DF, aes(x= Temperature, y = Net_Production))+
          geom_line()+ 
          #guides(linetype=guide_legend(title = "Â"))+
          labs(x= "Temperature (K)", y="Net Production (µg/L/day)") +
   scale_x_continuous(breaks = round(seq(min(Net_Production_vs_Temp_DF$Temperature), max(Net_Production_vs_Temp_DF$Temperature), by = 2),0))+
          theme_classic()
 
 
 Temp_A_hat =Temp_A_hat_.096

 R_A_hat = output1_1_non_trivial_varying_temperature_.096$curvepoints[,2]
 J_A_hat = output1_1_non_trivial_varying_temperature_.096$curvepoints[,5]
 A_A_hat = output1_1_non_trivial_varying_temperature_.096$curvepoints[,6]
 print(A_A_hat/J_A_hat)
 df3 <- data.frame(Temperature = Temp_A_hat,
                   R = R_A_hat,
                   J = J_A_hat,
                   A = A_A_hat
                   ) %>% 
   pivot_longer(c(R, J, A))
 
 

 
 
 Temp_A_hat = Temp_A_hat_.096
 R_A_hat =  output1_1_non_trivial_varying_temperature_.096$curvepoints[,2]
 
 J_A_hat_Net_Production = Net_Production_A_hat_.096_J
 
 A_A_hat_Net_Production = Net_Production_A_hat_.096_A
 
  df3.5 <- data.frame(Temperature = Temp_A_hat,
                    J = J_A_hat_Net_Production,
                    A = A_A_hat_Net_Production
                   ) %>% 
    pivot_longer(c(J, A))
  
  Life_Stages_A_J_R = list("A" = "Adult",
                     "J" = "Juvenile",
                     "R" = "Resource")
  
  labeller_A_J_R <- function(variable,value){
    return(Life_Stages_A_J_R[value])
  }
  
  ggplot(df3, aes(x = Temperature, y= value)) + 
    facet_wrap(~name, scale = "free", labeller = labeller_A_J_R)+
    theme_classic()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
    )+
    geom_line() + labs(x= "Temperature (K)", y="Density (µg/L)") +
    guides(color=guide_legend(title = "Population"))+

    scale_color_manual(labels=c('Adult (µg/L)', 'Juvenile (µg/L)', 'Resource (µg C/L)'),
                       values = c("Black","Red","Blue"))+
    scale_x_continuous(breaks = round(seq(min(df3$Temperature), max(df3$Temperature), by = 2),0))
  
  Life_Stages_A_J = list("A" = "Adult",
                     "J" = "Juvenile")
  labeller_A_J <- function(variable,value){
    return(Life_Stages_A_J[value])
  }
  
  
  ggplot(df3.5, aes(x = Temperature, y= (value))) +
    geom_line() + labs(x= "Temperature (K)", y="Net Production (µg/L/day)") +
    facet_wrap(~name, scales = "free", labeller = labeller_A_J)+
    guides(color=guide_legend(title = "Population"))+
    theme_classic()+
    scale_color_manual(labels=c('Adult (µg/L/day)', 'Juvenile (µg/L/day)'),
                       values = c("Black","Red","Blue"))+
    scale_x_continuous(breaks = round(seq(min(df3$Temperature), max(df3$Temperature), by = 2),0))
  
  df4 <- data.frame(
                    R = R_A_hat,
                    J = J_A_hat,
                    A = A_A_hat) %>% 
    pivot_longer(c(J, A))
    
    ggplot(df4, aes(x = R, y= value)) + 
      facet_wrap(~name, scale = "free", labeller = labeller_A_J)+ 
      geom_line() + labs(x= "Resource Density (µg/L)", y="Population Density (µg/L)") +
      guides(color=guide_legend(title = "Population"))+
      theme_classic()+
      scale_color_manual(labels=c('Adult (µ/L)', 'Juvenile (µ/L)', 'Resource (µg C/L)'),
                         values = c("Black","Red","Blue"))
    
  Total_Population_A_Hat = output1_1_non_trivial_varying_temperature_.096$curvepoints[, 5]+output1_1_non_trivial_varying_temperature_.096$curvepoints[, 6]
  
  Net_Production_A_Hat = Net_Production_A_hat_.096/10
  
  Fecundity_A_Hat = output1_1_non_trivial_varying_temperature_.096$curvepoints[, 3]
  
  Temperature_A_Hat = Temp_A_hat_.096
  
  df6 <- data.frame(cbind(Temperature = Temperature_A_Hat,
                    
                    Fecundity = Fecundity_A_Hat,
                    
                    Net_Production = Net_Production_A_Hat,
                    
                    Fecundity = Fecundity_A_Hat/(Total_Population_A_Hat),
                    
                    Net_Production = Net_Production_A_Hat/Total_Population_A_Hat))
  
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
    guides(color=guide_legend(title = "Population"))+
    
    scale_color_manual(labels=c("Fecundity", "Net Production (10 µg/day)"), 
                       values = c("Black","Red"))+
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
  
  ggplot(Adult_Juvenile_Ratio_df, aes(y=Adult_Juvenile_Ratio, x = R_A_hat)) + 
    
    geom_line()+
    theme_classic()+
    labs(x = "Resource Density (µg/L)", y ="Adult:Juvenile Biomass Ratio") +
    scale_color_manual(labels=c("0.04", "0.096", "1.3344"))+
    
    theme(axis.text.x = element_text(size = 11),
          axis.title.x = element_text(size = 13),
          axis.text.y = element_text(size = 11),
          axis.title.y = element_text(size = 13),
          strip.text.x = element_text(size = 15, hjust = 0),
          panel.spacing = unit(2, "lines"),
          strip.background = element_rect(color="white", fill="white", linetype="solid"))
   
  
  Adult_Juvenile_Ratio_Temp_df = data.frame(cbind(Adult_Juvenile_Ratio, Temperature_A_Hat) )
 
  ggplot(Adult_Juvenile_Ratio_df, aes(y=Adult_Juvenile_Ratio, x = Temperature_A_Hat)) + 
    
    geom_line()+
   
    theme_classic()+
    
    labs(x = "Temperature (K)", y ="Adult:Juvenile Biomass Ratio") +
    scale_x_continuous(breaks = round(seq(min(df3$Temperature), max(df3$Temperature), by = 2),0))+

    theme(axis.text.x = element_text(size = 11),
          axis.title.x = element_text(size = 13),
          axis.text.y = element_text(size = 11),
          axis.title.y = element_text(size = 13),
          strip.text.x = element_text(size = 15, hjust = 0),
          panel.spacing = unit(2, "lines"),
          strip.background = element_rect(color="white", fill="white", linetype="solid"))
  
  df3_J = df3[which(df3[,"name"] == "J"),]
  df3_A = df3[which(df3[,"name"] == "A"),]

  
  rows_to_extract_J = c()
  rows_to_extract_A = c()
  Bering_SST_Spring_Summer_Long <- Bering_SST_Spring_Summer %>%
  pivot_longer(c("March",
                 "April",
                 "May",
                 "June",
                 "July",
                 "August"))
  colnames(Bering_SST_Spring_Summer_Long) = c("Year","Month","Temperature")
  
  Juvenile_Mean_Weights <- Juvenile_Mean_Weights  %>%
    pivot_longer(c("March",
                   "April",
                   "May",
                   "June",
                   "July",
                   "August"))
  colnames(Juvenile_Mean_Weights) = c("Year","Month","Weight")
  
  Adult_Mean_Weights <- Adult_Mean_Weights  %>%
    pivot_longer(c("March",
                   "April",
                   "May",
                   "June",
                   "July",
                   "August"))
  colnames(Adult_Mean_Weights) = c("Year","Month","Weight")
  
  
  
  months = c("March",
             "April",
             "May",
             "June",
             "July",
             "August")
  
  rows_to_extract_J = c()
  rows_to_extract_A = c()
  
for(i in 1:nrow(Bering_SST_Spring_Summer_Long))
  {
  model_temp_vector_J = as.vector(df3_J[,1])
  rows_to_extract_J[i] = min(which(abs(model_temp_vector_J - Bering_SST_Spring_Summer_Long[i,"Temperature"]) == min(abs(model_temp_vector_J - Bering_SST_Spring_Summer_Long[i,"Temperature"]))) )
  
  model_temp_vector_A = as.vector(df3_A[,1])
  rows_to_extract_A[i] = min(which(abs(model_temp_vector_A - Bering_SST_Spring_Summer_Long[i,"Temperature"]) == min(abs(model_temp_vector_A - Bering_SST_Spring_Summer_Long[i,"Temperature"]))) )
  
 }

  Model_J_Biomass_Selected = df3_J[rows_to_extract_J,] #Model Predicted Juvenile Biomass
  colnames(Model_J_Biomass_Selected) = c("Temperature",
                                         "Stage",
                                         "Predicted")
  Model_A_Biomass_Selected = df3_A[rows_to_extract_A,] #Model Predicted Adult Biomass
  colnames(Model_A_Biomass_Selected) = c("Temperature",
                                         "Stage",
                                         "Predicted")
  
  Observed_Predicted_DF_J = Model_J_Biomass_Selected
  Observed_Predicted_DF_J[,"Observed"]  = Juvenile_Mean_Weights[,"Weight"]
  Observed_Predicted_DF_J = na.omit(Observed_Predicted_DF_J)
  Observed_Predicted_DF_A = Model_A_Biomass_Selected
  Observed_Predicted_DF_A[,"Observed"]  = Adult_Mean_Weights[,"Weight"]
  Observed_Predicted_DF_A = na.omit(Observed_Predicted_DF_A)
  
  DF_Observed_Predicted = rbind(Observed_Predicted_DF_J, Observed_Predicted_DF_A)
  DF_Observed_Predicted <- DF_Observed_Predicted %>%
  pivot_longer(c("Observed", "Predicted"))
  
      #Gives the observed year/month combination with the closes temperature to the temperatures in the model
  #we need to extract from the model_temp_vector
  
  

  DF_Observed = DF_Observed_Predicted[which(DF_Observed_Predicted[,"name"] =="Observed"),]
 
   ggplot(data = DF_Observed, aes(y = log(value), x = Temperature)) +
    facet_wrap(~Stage, scale = "free", labeller = labeller_A_J)+
    #geom_histogram(aes(y=..density.., color = Data_Type), fill="white")+
    # guides(color=guide_legend(title = "Data Type"))+
    geom_point()+
    theme_classic()+
    labs(x = "Biomass Density (µg/L)", y ="Density") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          legend.title=element_blank())
  
  ggplot(data = DF_Observed_Predicted, aes(x = log(value), color = name, fill = name)) +
    facet_wrap(~Stage, scale = "free", labeller = labeller_A_J)+
    #geom_histogram(aes(y=..density.., color = Data_Type), fill="white")+
   # guides(color=guide_legend(title = "Data Type"))+
    geom_density(alpha=.2)+
    theme_classic()+
    labs(x = "Biomass Density (µg/L)", y ="Density") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          legend.title=element_blank())
    

  
  