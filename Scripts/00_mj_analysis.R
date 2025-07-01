library(PSPManalysis)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(cowplot)
library(here)
library(extrafont)

theme_set(theme(text = element_text(family="Times New Roman")))
set_null_device("png")

root <- here()

mj = exp(-3.18)*exp(.73*12)

# 0.02
# 2000
# 0.096
# 0.75
# 98
# 0.7
# 5055.09

DefaultParameters <- c(Delta = 0.003, #turnover rate is 1 divided by the per capita growth rate
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
                       
                       
                       k = 8.617e-5, #boltzmann constant
                       alpha = 0.75, #(Hjelm and Persson 2001)
                       t0 = 285.65, #Frost experiment on attack rate conducted at 12.5 C or 285.65 K
                       sigma = 0.7, #(de Roos et al. 2007; Peters 1983; Yodzis and Innes 1992)
                       
                       Mopt = 39, #exp(-3.18)*exp(.73*12), #???????????
                       
                       epsi1 = 0.9827528, #Approximated from saiz and calbert 2007 On marine calanoid species. 15 C.
                       epsi2 = 0.006059, #Approximated from saiz and calbert 2007.  micrograms of carbon per day.  On marine calanoid species. 15 C.
                       epsi3 = 0.996373, #Approximated from saiz and calbert 2007.  micrograms of carbon per day.  On marine calanoid species.  15 C.
                       t0_epsi = 288.15,
                       
                       #Kiorbe, Mohlenberg and Nicolajsen (2012) maximum rate equal to 85 % body C · d−1 at 15 °C
                       
                       phi1 = 1.3365966, #For mortality rate per day.  Approximated from  Hirst and Kiørboe 2002 calculated at 15 C.  Dry weight
                       phi2 = -0.092, #For mortality rate per day.  Approximated from  Hirst and Kiørboe 2002 calculated at 15 C. Dry Weight.  
                       t0_phi = 288.15,
                       
                       rho1 = 0.02310296, #micro grams per day, from Ikeda_2007.  Dry weight at 2 C
                       rho2 = 0.75816, #micro grams per day, from Ikeda_2007.  Dry weight at 2 C
                       t0_rho = 275.15,
                       
                       mh = .75, # .75 ug (micrograms) (Petersen, 1986) -graph pg 68
                       mj = exp(-3.18)*exp(.73*12),   # ug (micrograms) (Petersen, 1986) pg 66
                       
                       t0_Delta = 281.15, #8 degrees celsius from Maranon et al. at an intermediate nutrient level had turnover rate of about 1 day
                       #Turnover_Time = 2.19907e-7 #converted from ms to days from Falkowski et al 1981
                       
                       #z = 0.002694034 #juvenile to adult ratio 
                       cI = 0, #Jan assumes a value of 0 in Roach model 
                       cM = 0.0 # Jan tests the Roach model with values of -.02, 0, and .02 
)

library(lubridate)

Bering_Calanus_Data = read.csv(paste0(root,"/Data/Calanus_BSMS.csv"))

all_Adults = Bering_Calanus_Data[which(Bering_Calanus_Data[,"STAGE_NAME"] == "ADULT"), c(12,42)]
all_Juvenile = Bering_Calanus_Data[which(Bering_Calanus_Data[,"STAGE_NAME"] != "ADULT"), c(12,42)]
all_calanus = rbind(all_Adults,all_Juvenile)
all_calanus[,"Stage"] = c(rep("Adult",nrow(all_Adults)),
rep("Juvenile",nrow(all_Juvenile)))
all_calanus = data.frame(all_calanus)

month = format(as.Date(all_calanus$GMT_DATE_TIME_TXT),"%m")

all_calanus_samples_within_month = all_calanus %>%
  mutate(Month = month, .after = GMT_DATE_TIME_TXT) %>%
  group_by(Month) %>%
  summarize(count = n())

p1 <- ggplot(all_calanus_samples_within_month) +
  geom_point(aes(y = count, x = Month)) + 
  labs(y ="Number of Samples") + 
  theme_classic()

p2 <- ggplot(all_calanus) +
  facet_wrap(~Stage) + 
  geom_point(aes(y = (mean_DW_mg_m3), x = month)) + 
  labs(y ="Density (µg/L)", x = "Month") + 
  theme_classic()

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

unique(summer_Juvenile_year)[order(unique(summer_Juvenile_year))]
#missing, 1993, 1995, 2013

unique(summer_Adult_year)[order(unique(summer_Adult_year))]
#missing 1993,1994,1995,2013,2018

Bering_SST = read.csv(paste0(root,"/Data/Bering_SST_12_06.csv"))[c(49:65,67:70),] #1996 through 2012, 2014 through 2017
Bering_SST_Spring = rowMeans(Bering_SST[,4:6])+273.15
Bering_SST_Summer = rowMeans(Bering_SST[,7:9])+273.15

plot(log(summer_Adult)~summer_Adult_year)
plot(log(summer_Juvenile)~summer_Juvenile_year)
min(Bering_SST_Spring)-273.15
plot(Bering_SST_Summer~Bering_SST[,1])
plot(Bering_SST_Spring~Bering_SST[,1])

min(Bering_SST_Summer)-273.15

year_sequence = as.numeric(c(seq(1996, 2012, 1),seq(2014,2017,1)))


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

year_sequence[7]
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
#Fit to Spring rather than summer

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
DF3$Temperature = DF3$Temperature - 273.15

ggplot(data = DF3, aes(x = Year, y = log(value), linetype = Stage))+
  facet_wrap(~name)+
  geom_line()
# ggplot(data = DF3, aes(x = Temperature, y = log(value), linetype = Stage))+
#   facet_wrap(~name)+
#   geom_line()

Temp_DF_2 = Temp_DF
Temp_DF_2$Temperature = Temp_DF_2$Temperature - 273.15
ggplot(data = Temp_DF_2, aes(x = Year, y = Temperature, color = Season))+
  geom_line()+
  theme_classic() +
  labs(y = "Temperature (C°)")


Modified_Parameters_A_hat_.096 = DefaultParameters


modified_parameters_2 = Modified_Parameters_A_hat_.096

mj_sequence = seq(75.0716,265.0716,10)

extinction_temp = c()
change_in_size_ratio = c()

for(a in 1:length(mj_sequence))
  
{
  
modified_parameters_2[25] =  mj_sequence[a]

output1_1_A_hat_.096 <-PSPMequi(modelname = "Scripts/PSPM_Model_Structure.R", biftype = "EQ", startpoint = c(1, 1), 
                     stepsize = 1.25,
                     parbnds = c(1, 1, 20000), parameters = modified_parameters_2, minvals = NULL, maxvals = NULL, options = c(c("popZE", "0")),
                     clean = TRUE, force = FALSE, debug = FALSE, silent = FALSE)


df = data.frame(R_max = output1_1_A_hat_.096$curvepoints[, 1],
                 R = output1_1_A_hat_.096$curvepoints[, 2],
                 J =output1_1_A_hat_.096$curvepoints[, 5],
                 A = output1_1_A_hat_.096$curvepoints[, 6],
                A_Hat = as.factor(rep(.096, nrow(output1_1_A_hat_.096$curvepoints))) )

output1_1_non_trivial_.096 <-PSPMequi(modelname = "Scripts/PSPM_Model_Structure.R", 
                                      biftype = "EQ", startpoint = c(output1_1_A_hat_.096$bifpoints[1], output1_1_A_hat_.096$bifpoints[2], 0), 
                                      stepsize = 1.5, parbnds = c(1, output1_1_A_hat_.096$bifpoints[1], 10000), parameters = modified_parameters_2, 
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

output1_1_non_trivial_varying_temperature_.096 <-PSPMequi(modelname = "Scripts/PSPM_Model_Structure.R", 
                                                          biftype = "EQ", startpoint = c(273.15, output1_1_non_trivial_.096$curvepoints[start_.096,2], 
                                                          output1_1_non_trivial_.096$curvepoints[start_.096,3]), 
                                                          stepsize = .1,
                                                          parbnds = c(3, 273.15, 320), parameters = modified_parameters_2, minvals = NULL, maxvals = NULL, 
                                                          clean = TRUE, force = FALSE, debug = FALSE, silent = FALSE)
 


 R_A_hat = output1_1_non_trivial_varying_temperature_.096$curvepoints[,2]
 J_A_hat = output1_1_non_trivial_varying_temperature_.096$curvepoints[,5]
 A_A_hat = output1_1_non_trivial_varying_temperature_.096$curvepoints[,6]

  
 
  Adult_Juvenile_Ratio = A_A_hat/J_A_hat

  ext_temp = min(which(output1_1_non_trivial_varying_temperature_.096$curvepoints[,3] <= 0), na.rm = TRUE)
  
  extinction_temp[a] = as.numeric(output1_1_non_trivial_varying_temperature_.096$curvepoints[ext_temp,1])
  change_in_size_ratio[a] = max(Adult_Juvenile_Ratio,na.rm = TRUE) - min(Adult_Juvenile_Ratio,na.rm = TRUE)
    
  }

extinction_temp_df = data.frame(cbind(c(extinction_temp-273.15),mj_sequence))
colnames(extinction_temp_df) = c("Extinction_Temp", "mj")

extinction_vs_size <- ggplot(extinction_temp_df)+
  geom_line(aes(x = Extinction_Temp, y = mj)) +
  labs(x = "Extinction Temperature °C", y = "Size at Maturity µg") + 
  theme_classic() +
  scale_y_continuous(limits = c(225,275) ) +
  theme(text = element_text(family = "Times New Roman"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 14))

extinction_vs_size <- plot_grid(extinction_vs_size, label_fontfamily = "Times New Roman", label_colour = "black", labels = c("A"))
extinction_vs_size

