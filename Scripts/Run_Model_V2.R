library(PSPManalysis)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(cowplot)
library(here)
library(extrafont)

set_null_device("png")

root <- here()


DefaultParameters <- c(Delta = .0001, #turnover rate is 1 divided by the per capita growth rate
                       # Turnover is 1, #per day.  Range of between approximately .1 and 3 from Marañón et al. 2014.  They found no relationship between phytoplankton turnover rate and temperature  
                       Rmax = 500, #Rmax is a density micrograms of carbon per liter.  This means all other densities including copepod densities are micrograms per liter. Approximately 2000 from Putland and Iverson 2007
                       
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
                       
                       Mopt = 265, #exp(-3.18)*exp(.73*12), #???????????
                       
                       epsi1 = 1.089234, #Approximated from saiz and calbert 2007 On marine calanoid species. 15 C.
                       epsi2 = 0.76662, #Approximated from saiz and calbert 2007.  micrograms of carbon per day.  On marine calanoid species. 15 C.
                       
                       #The use of epsi3 depends on the Imax formulation used.
                       epsi3 = 0.998725, #Approximated from saiz and calbert 2007.  micrograms of carbon per day.  On marine calanoid species.  15 C.
                       
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

#So zooplankton combined includes years 1993 to 2018
sort(unique(Bering_Calanus_Data$YEAR))

#Spring zooplankton includes 1993 through 1995; 1997 through 2000; 2002 through 2018 (Missing 1996 and 2001)
sort(unique(Bering_Calanus_Data$YEAR[which(Bering_Calanus_Data$MONTH >=3 & Bering_Calanus_Data$MONTH <= 5)]))

#Proceed using summer (MISSING 2013, starts in 1996)
#Summer zooplankton includes 1996 through 2012, and 2014 through 2016 (Missing 2013)
sort(unique(Bering_Calanus_Data$YEAR[which(Bering_Calanus_Data$MONTH >=6 & Bering_Calanus_Data$MONTH <= 8)]))

all_Adults = Bering_Calanus_Data[which(Bering_Calanus_Data[,"STAGE_NAME"] == "ADULT"), c("GMT_DATE_TIME_TXT","YEAR","mean_DW_mg_m3")]
all_Juvenile = Bering_Calanus_Data[which(Bering_Calanus_Data[,"STAGE_NAME"] != "ADULT"), c("GMT_DATE_TIME_TXT","YEAR","mean_DW_mg_m3")]
all_calanus = rbind(all_Adults,all_Juvenile)
all_calanus[,"Stage"] = c(rep("Adult",nrow(all_Adults)),
                          rep("Juvenile",nrow(all_Juvenile)))

all_calanus = data.frame(all_calanus) %>%
  mutate(GMT_DATE_TIME_TXT = as.POSIXct(GMT_DATE_TIME_TXT, "%Y-%m-%d %H:%M") ) %>%
  mutate(Month = month(GMT_DATE_TIME_TXT), .after = GMT_DATE_TIME_TXT) %>%
  mutate(Month = as.factor(Month)) %>%
  mutate(YEAR = year(all_calanus$GMT_DATE_TIME_TXT )) %>%
  mutate(fake_date = as.character(paste0(YEAR,"-01-01")) ) %>%
  mutate(fake_date = as.Date(fake_date)) 

all_calanus_spring_summer <- all_calanus %>%
  mutate(Month = as.numeric(Month)) %>%
  filter(Month >=3 & Month <= 8) %>%
  mutate(Season = if_else(Month <=5, "Spring","Summer")) %>%
  mutate(Month = as.factor(Month),
         Stage = as.factor(Stage))

all_calanus_samples_within_month = all_calanus %>%
  group_by(Month) %>%
  summarize(count = n())

p1 <- ggplot(all_calanus_samples_within_month) +
  geom_point(aes(y = count, x = Month)) + 
  labs(y ="Number of Samples") + 
  theme_classic()+
  theme(text = element_text(family = "Times New Roman", size = 15))

p1

p2 <- ggplot(all_calanus) +
  facet_wrap(~Stage, nrow = 2) + 
  geom_boxplot(aes(y = log(mean_DW_mg_m3), x = Month, group = Month)) + 
  labs(y =expression(paste("Ln(Density) (mg / ", m^{3} ,")")), x = "Month") + 
  #labs(x = "Month", y = NULL) +
  theme_classic() +
  theme(text = element_text(family = "Times New Roman"),
        plot.margin = margin(0.2,0.5,0.2,1, "cm"),
        strip.text.x = element_text(size = 13),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)) +
  scale_y_continuous(position = "right")

p2

ggsave(paste0(root,"/figures/monthly_Calanus_Biomass.png"), plot = p2, dpi = 300, height = 5, width = 5, units = "in")


all_calanus_spring_summer_adults <- all_calanus_spring_summer %>%
  filter(Stage == "Adult")
all_calanus_spring_summer_juveniles <- all_calanus_spring_summer %>%
  filter(Stage == "Juvenile")

p3_adult <- ggplot(all_calanus_spring_summer_adults) + 
  facet_grid(~Season) + 
  geom_boxplot(aes(y = log(mean_DW_mg_m3), x = fake_date, group = YEAR)) + 
  #labs(y = expression(paste("Ln(Density) (mg / ", m^{3} ,")")), x = "Year", title = "Adult Density") + 
  #labs(x = NULL, y = NULL) +
  labs(y = expression(paste("Ln(Density) (mg / ", m^{3} ,")")), x = NULL, title = "Adult Density") + 
  scale_x_date(date_labels = "%Y", ) +
  theme_classic()+
  theme(text = element_text(family = "Times New Roman", size = 15),
        plot.margin = margin(0.2,1,0.2,1, "cm"), plot.title = element_text(hjust = 0.5))

p3_adult

p3_juvenile <- ggplot(all_calanus_spring_summer_juveniles) + 
  facet_grid(~Season) + 
  geom_boxplot(aes(y = log(mean_DW_mg_m3), x = fake_date, group = YEAR)) + 
  #labs(y = expression(paste("Ln(Density) (mg / ", m^{3} ,")")), x = "Year", title = "Juvenile Density") + 
  #labs(x = NULL, y = NULL) +
  labs(y = expression(paste("Ln(Density) (mg / ", m^{3} ,")")), x = NULL, title = "Juvenile Density") + 
  scale_x_date(date_labels = "%Y", ) +
  theme_classic()+
  theme(text = element_text(family = "Times New Roman", size = 12), 
        plot.margin = margin(0.2,1,0.2,1, "cm"), plot.title = element_text(hjust = 0.5))


p3_juvenile

p3_all_stages <- ggplot(all_calanus_spring_summer) + 
  facet_grid(Stage~Season) + 
  geom_boxplot(aes(y = log(mean_DW_mg_m3), x = fake_date, group = YEAR)) + 
  #labs(y = expression(paste("Ln(Density) (mg / ", m^{3} ,")")), x = "Year", title = "Juvenile Density") + 
  #labs(x = NULL, y = NULL) +
  labs(y = expression(paste("Ln(Density) (mg / ", m^{3} ,")")), x = NULL) + 
  scale_x_date(date_labels = "%Y", ) +
  theme_classic()+
  theme(axis.text.x = element_text(size = 12, angle = -75, vjust = 0.25),
        axis.text.y = element_text(size = 12),
        )+
  theme(text = element_text(family = "Times New Roman"),
        plot.margin = margin(0.2,0,0.2,0.5, "cm"), plot.title = element_text(hjust = 0.5),
        strip.text.x = element_text(size = 13),
        strip.text.y = element_text(size = 13),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))

p3_all_stages


#all_calanus_with_temp <- all_calanus %>%
  
  
#p3_with_temp <- 



p2_boxplot <- ggplot(all_calanus) +
  facet_wrap(~Stage) + 
  geom_boxplot(aes(y = log(mean_DW_mg_m3), x = Month)) + 
  #labs(y = expression(paste("Ln(Density) (mg / ", m^{3} ,")")), x = "Month") + 
  labs(x = "Month", y = expression(paste("Ln(Density) (mg /  ", m^{3} ,")"))) +
  theme_classic()+
  theme(text = element_text(family = "Times New Roman", size = 15))

p2_boxplot


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


unique(Bering_Calanus_Data$YEAR)[order(unique(Bering_Calanus_Data$YEAR))]

unique(all_calanus$YEAR)[order(unique(all_calanus$YEAR))]


unique(spring_Juvenile_year)[order(unique(spring_Juvenile_year))]
#Missing 1996, 2001
unique(spring_Adult_year)[order(unique(spring_Adult_year))]
#Missing 1994, 1996, 2001, 2002

unique(summer_Juvenile_year)[order(unique(summer_Juvenile_year))]
#Missing 1994, 1995, 2013, 2017, 2018

unique(summer_Adult_year)[order(unique(summer_Adult_year))]
#Missing 1994, 1995, 2013, 2015, 2017, 2018 

intersect(unique(summer_Juvenile_year)[order(unique(summer_Juvenile_year))], unique(summer_Adult_year)[order(unique(summer_Adult_year))])
#Missing 2013, 2015, 2017

Bering_SST = read.csv(paste0(root,"/Data/Bering_SST_12_06.csv"))[c(49:65,67:69),] #1996 through 2012, 2014 through 2016 #This matches summer zooplankton sample availability


Bering_SST_Spring = rowMeans(Bering_SST[,4:6])+273.15
Bering_SST_Summer = rowMeans(Bering_SST[,7:9])+273.15

plot(log(summer_Adult)~summer_Adult_year)
plot(log(summer_Juvenile)~summer_Juvenile_year)
max(Bering_SST_Spring)-273.15
plot(Bering_SST_Summer~Bering_SST[,1])
plot(Bering_SST_Spring~Bering_SST[,1])

#Spring
max(Bering_SST_Spring)-273.15
#Summer
max(Bering_SST_Summer)-273.15

#Years here should match those selected above (1996:2012, 2014:2016)
year_sequence = as.numeric(c(seq(1996, 2012, 1),seq(2014,2016,1)))


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

min(Summer, na.rm = TRUE)
max(Summer, na.rm = TRUE)

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

Temp_DF_2 = Temp_DF
Temp_DF_2$Temperature = Temp_DF_2$Temperature - 273.15

Temp_DF_2_graph <- Temp_DF_2 %>%
  mutate(Year = as.factor(Year)) %>%
  mutate(fake_date = as.character(paste0(Year,"-01-01")) ) %>%
  mutate(fake_date = as.Date(fake_date)) 
  
Temp_DF_2_graph_summer <- Temp_DF_2_graph %>%
 filter(Season == "Summer") %>%
          na.omit()

m1 <- lm(Temp_DF_2_graph_summer$Temperature~as.numeric(Temp_DF_2_graph_summer$Year) )
summary(m1)

temp_graph <- ggplot(data = Temp_DF_2_graph, aes(x = fake_date, y = Temperature))+
  facet_wrap(~Season)+
  geom_line(size = .75)+
  scale_x_date(date_labels = "%Y", limits = c(as.Date("1993-01-01"), as.Date("2018-01-01"))) +
  theme_classic()+
  labs(y = "Temperature (°C)", x = "Year") +
  theme(text = element_text(family = "Times New Roman"),
        axis.text.x = element_text(size = 12, angle = -75, vjust = 0.25),
        axis.text.y = element_text(size = 12),
        strip.background = element_blank(), 
        strip.text.x = element_blank(),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.margin = margin(0.2,0,0.2,0.5, "cm")
       )

temp_graph

#This code aligns the left margins of the graphs

plots <- list(p3_all_stages, temp_graph)
grobs <- lapply(plots, as_grob)
plot_widths <- lapply(grobs, function(x) {x$widths})
# Aligning the left margins of all plots
aligned_widths <- align_margin(plot_widths, "first")
# Aligning the right margins of all plots as well
aligned_widths <- align_margin(aligned_widths, "last")
# Setting the dimensions of plots to the aligned dimensions
for (i in seq_along(plots)) {
  grobs[[i]]$widths <- aligned_widths[[i]]
}
# Draw aligned plots

temp_combined_with_biomass_graphs <- plot_grid(plotlist = grobs, ncol = 1, labels = c("A","B"), rel_heights = c(7,3),  label_fontfamily = "Times", label_colour = "black")
temp_combined_with_biomass_graphs

monthly_biomass_graph <- plot_grid(p2, ncol = 1, labels = c("C"),  label_fontfamily = "Times", label_colour = "black")
monthly_biomass_graph 

#temp_combined_with_biomass_graphs <- plot_grid(plotlist = grobs, ncol = 1, labels = c("A","B"), rel_heights = c(7,3))

temp_combined_with_yearly_and_monthly_biomass_graphs <- plot_grid(temp_combined_with_biomass_graphs, 
                                                                  monthly_biomass_graph,
                                                                  ncol = 2, labels = NULL, rel_widths = c(6,3.25),  label_fontfamily = "Times", label_colour = "black")

temp_combined_with_yearly_and_monthly_biomass_graphs

ggsave(paste0(root,"/figures/temp_combined_with_biomass_graphs.png"), plot = temp_combined_with_yearly_and_monthly_biomass_graphs, dpi = 500, height = 6, width = 7, units = "in")

Modified_Parameters_A_hat_.096 = DefaultParameters



output1_1_A_hat_.096 <-PSPMequi(modelname = paste0(root,"/Scripts/PSPM_Model_Structure.R"), biftype = "EQ", startpoint = c(1,1), #startpoint is the parameter startpoint (in this case Rmax) and environmental (resource) start point
                                stepsize = .1,
                                #Vector indexes in C start at 0, so parbnds is varying the first parameter (Rmax) when we say to start it from parameter "1"
                                parbnds = c(1, 1, 50000), parameters = Modified_Parameters_A_hat_.096, minvals = NULL, maxvals = NULL, options = c(c("popZE", "0")),
                                clean = TRUE, force = FALSE, debug = FALSE, silent = FALSE)

output1_1_A_hat_.096$curvepoints


df = data.frame(R_max = output1_1_A_hat_.096$curvepoints[, 1],
                R = output1_1_A_hat_.096$curvepoints[, 2],
                J =output1_1_A_hat_.096$curvepoints[, 5],
                A = output1_1_A_hat_.096$curvepoints[, 6],
                A_Hat = as.factor(rep(.096, nrow(output1_1_A_hat_.096$curvepoints))) )

output1_1_non_trivial_.096 <-PSPMequi(modelname = paste0(root,"/Scripts/PSPM_Model_Structure.R"), 
                                      biftype = "EQ", startpoint = c(output1_1_A_hat_.096$bifpoints[1], output1_1_A_hat_.096$bifpoints[2], 0), 
                                      stepsize = 1.25, parbnds = c(1, output1_1_A_hat_.096$bifpoints[1], 10000), parameters = Modified_Parameters_A_hat_.096, 
                                      minvals = NULL, maxvals = NULL, clean = TRUE, force = FALSE, debug = FALSE, silent = FALSE)


output1_1_non_trivial_.096$curvepoints[,12]

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


t1 = output1_1_non_trivial_.096$curvepoints


start_.096 = median(which(t1[,"Rmax"] < (Modified_Parameters_A_hat_.096[2]+500) & t1[,"Rmax"] > (Modified_Parameters_A_hat_.096[2]-500)))

start_.096 = round(start_.096, 0)

df2 <- data.frame(R_max = output1_1_non_trivial_.096$curvepoints[, 1], 
                  
                  
                  R = output1_1_non_trivial_.096$curvepoints[, 2], 
                  
                  J = output1_1_non_trivial_.096$curvepoints[, 5], 
                  
                  A = output1_1_non_trivial_.096$curvepoints[, 6])

df2 <- df2 %>% 
  pivot_longer(c(R, J, A))

output1_1_non_trivial_varying_temperature_.096 <-PSPMequi(modelname = paste0(root,"/Scripts/PSPM_Model_Structure.R"), 
                                                          biftype = "EQ", startpoint = c(273.15, output1_1_non_trivial_.096$curvepoints[start_.096,2], 
                                                                                         output1_1_non_trivial_.096$curvepoints[start_.096,3]), 
                                                          stepsize = .2,
                                                          parbnds = c(3, 273.15, 310), parameters = Modified_Parameters_A_hat_.096, minvals = NULL, maxvals = NULL, 
                                                          clean = TRUE, force = FALSE, debug = FALSE, silent = FALSE)

#This is important. Need to vary at the lowest extinction temperature.
parameters_vary_mj <- Modified_Parameters_A_hat_.096
parameters_vary_mj[4] <- 14.70219 + 273.15

output1_1_non_trivial_varying_mj_.096 <-PSPMequi(modelname = paste0(root,"/Scripts/PSPM_Model_Structure.R"), 
                                                 biftype = "EQ", startpoint = c(265.0716, output1_1_non_trivial_varying_temperature_.096$bifpoints[2], output1_1_non_trivial_varying_temperature_.096$bifpoints[3] 
                                                 ), 
                                                 stepsize = -.1,
                                                 parbnds = c(24, 0, 265.0716), parameters = parameters_vary_mj, minvals = NULL, maxvals = NULL, 
                                                 clean = TRUE, force = FALSE, debug = FALSE, silent = FALSE)

Total_Population_A_Hat = output1_1_non_trivial_varying_temperature_.096$curvepoints[, 5]+output1_1_non_trivial_varying_temperature_.096$curvepoints[, 6]

mortality = output1_1_non_trivial_varying_temperature_.096$curvepoints[,11]
per_capita_mortality = mortality/Total_Population_A_Hat


I = output1_1_non_trivial_varying_temperature_.096$curvepoints[,4]
per_capita_I = I/Total_Population_A_Hat
I_max = output1_1_non_trivial_varying_temperature_.096$curvepoints[,9]
per_capita_Imax = I_max/Total_Population_A_Hat
Metabolism = output1_1_non_trivial_varying_temperature_.096$curvepoints[,10]
per_capita_Metabolism = Metabolism/Total_Population_A_Hat
Temp_Imax = output1_1_non_trivial_varying_temperature_.096$curvepoints[,1]-273.15
E_Imax = output1_1_non_trivial_varying_temperature_.096$curvepoints[,2]


t0_Delta = 281.15
Temp = Temp_Imax+273.15
E_Delta = 0.5
Delta = .01
k = 8.617e-5

Turnover = exp(E_Delta*(Temp-t0_Delta)/((k)*Temp*t0_Delta))*Delta*output1_1_non_trivial_varying_temperature_.096$curvepoints[,2]
per_capita_turnover = exp(E_Delta*(Temp-t0_Delta)/((k)*Temp*t0_Delta))*Delta


Mortality_DF = data.frame(cbind(Temp_Imax, mortality))
colnames(Mortality_DF) = c("Temperature", "Mortality")
Mortality_DF_per_capita = data.frame(cbind(Temp_Imax, per_capita_mortality))
colnames(Mortality_DF_per_capita) = c("Temperature", "Mortality")

Mortality_DF_Both = Mortality_DF %>%
  left_join(Mortality_DF_per_capita, by = "Temperature")

colnames(Mortality_DF_Both) <- c("Temperature", "Population", "Per Capita")

Mortality_DF_Both <- Mortality_DF_Both %>%
  pivot_longer(cols = c("Population","Per Capita"),
               names_to = "Rate") %>%
  mutate(Rate = as.factor(Rate)) %>%
  data.frame

Turnover_DF = data.frame(cbind(Temp_Imax, Turnover))
colnames(Turnover_DF) = c("Temperature", "Turnover")
Turnover_DF_per_capita = data.frame(cbind(Temp_Imax, per_capita_turnover))
colnames(Turnover_DF_per_capita) = c("Temperature", "Turnover")

Turnover_DF_Both = Turnover_DF %>%
  left_join(Turnover_DF_per_capita, by = "Temperature")
colnames(Turnover_DF_Both) <- c("Temperature", "Population", "Per Capita")

Turnover_DF_Both <- Turnover_DF_Both %>%
  pivot_longer(cols = c("Population","Per Capita"),
               names_to = "Rate") %>%
  mutate(Rate = as.factor(Rate)) %>%
  data.frame


M_DF = data.frame(cbind(Temp_Imax,Metabolism))
colnames(M_DF) = c("Temperature","Metabolism")

per_capita_M_DF = data.frame(cbind(Temp_Imax,per_capita_Metabolism))
colnames(per_capita_M_DF) = c("Temperature","Metabolism")

M_DF_Both = M_DF %>%
  left_join(per_capita_M_DF, by = "Temperature")
colnames(M_DF_Both) <- c("Temperature", "Population", "Per Capita")

M_DF_Both <- M_DF_Both %>%
  pivot_longer(cols = c("Population","Per Capita"),
               names_to = "Rate") %>%
  mutate(Rate = as.factor(Rate)) %>%
  data.frame() %>%
  na.omit()


both_metabolism_graph = ggplot(data = M_DF_Both)+
  geom_line(aes(x=Temperature, y=value, linetype = Rate, group = Rate)) +
  labs(x= NULL, y="µg O2 / day", title = "Metabolism") +
  scale_linetype_manual(values = c(1,2)) +
  theme_classic() +
  theme(text = element_text(family = "Times New Roman"),
        axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        title = element_text(size = 11),
        legend.position = "none")

I_only <- data.frame(cbind(Temp_Imax, I)) %>%
  mutate(Rate = "Population")
colnames(I_only) <- c("Temperature", "Value" , "Rate")

I_only_per_capita <- data.frame(cbind(Temp_Imax, per_capita_I)) %>%
  mutate(Rate = "Per Capita")
colnames(I_only_per_capita) <- c("Temperature", "Value" , "Rate")

Imax_only <- data.frame(cbind(Temp_Imax, I_max))%>%
  mutate(Rate = "Population")
colnames(Imax_only) <- c("Temperature", "Value" , "Rate")

Imax_only_per_capita <- data.frame(cbind(Temp_Imax, per_capita_Imax)) %>%
  mutate(Rate = "Per Capita")
colnames(Imax_only_per_capita) <- c("Temperature", "Value" , "Rate")

Ingestion_Only <- rbind(I_only, I_only_per_capita)

Ingestion_max_only <- rbind(Imax_only, Imax_only_per_capita)

Ingestion_graph_both <- ggplot()+
  geom_line(data = Ingestion_Only, aes(x = Temperature, y = Value, linetype = Rate) )+
  labs(x= NULL, y=NULL, title = "Ingestion") +
  scale_linetype_manual(values = c(1,2)) +
  scale_y_continuous(breaks = seq(0.0,1,.25)) +
  theme_classic() +
  theme(text = element_text(family = "Times New Roman"),
        legend.position="none", 
        axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        title = element_text(size = 11))

Ingestion_graph_both

Maximum_Ingestion_graph_both <- ggplot()+
  geom_line(data = Ingestion_max_only, aes(x = Temperature, y = Value, linetype = Rate) )+
  labs(x= NULL, y=NULL, title = "Maximum\nIngestion") +
  scale_linetype_manual(values = c(1,2)) +
  theme_classic() +
  theme(text = element_text(family = "Times New Roman"),
        legend.position="none", 
        axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        title = element_text(size = 11))

Maximum_Ingestion_graph_both 


both_mortality_graph = ggplot(data = Mortality_DF_Both)+
  geom_line(aes(x=Temperature, y=value, linetype = Rate, group = Rate)) +
  labs(x= NULL, y=NULL, title = "Mortality") +
  scale_linetype_manual(values = c(1,2)) +
  theme_classic() +
  theme(text = element_text(family = "Times New Roman"),
        axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        title = element_text(size = 11),
        legend.position = "none")

per_capita_turnover_graph = ggplot()+
  geom_line(data = Turnover_DF_per_capita, aes(x = Temperature, y = Turnover) )+
  labs(x= NULL, y=NULL, title = "Per Capita Resource Turnover") +
  theme_classic() +
  theme(text = element_text(family = "Times New Roman"),
        axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        title = element_text(size = 11))

per_capita_turnover_graph 

total_turnover_graph = ggplot()+
  geom_line(data= Turnover_DF, aes(x = Temperature, y = Turnover), linetype = 2)+
  labs(x= NULL, y=NULL, title = "Population Resource Turnover") +
  theme_classic() +
  theme(text = element_text(family = "Times New Roman"),
        axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        title = element_text(size = 11))

both_turnover_graph = ggplot(data = Turnover_DF_Both)+
  geom_line(aes(x=Temperature, y=value, linetype = Rate, group = Rate)) +
  labs(x= NULL, y="µg / day", title = "Turnover") +
  scale_linetype_manual(values = c(1,2)) +
  theme_classic() +
  theme(text = element_text(family = "Times New Roman"),
        axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        title = element_text(size = 11),
        legend.position = "none")

#parameter_rates_graph <- ggarrange(ggarrange(per_capita_ingestion_graph, per_capita_metabolism_graph, per_capita_mortality_graph, per_capita_turnover_graph, ncol = 4, nrow = 1,align = "h", labels = c("A", "B", "C", "D")),
#          ggarrange(total_ingestion_graph, total_metabolism_graph, total_mortality_graph, total_turnover_graph,ncol = 4, nrow = 1, align = "h", labels = c("E", "F","G","H")), nrow = 2)

turnovergraphs <- ggarrange(per_capita_turnover_graph, total_turnover_graph, nrow = 1, font.label = list(size = 14, color = "black"))

turnovergraphs <- annotate_figure(turnovergraphs,
                                  left = text_grob(expression(paste(day^{-1})), size = 14, rot = 90, family = "Times New Roman"),
                                  #top = text_grob("Resource Turnover", size = 13),
                                  bottom = text_grob("Temperature (°C)", size = 14, family = "Times New Roman")#,left = text_grob("Rate", size = 19, rot = 90)
)

turnovergraphs


parameter_rates_graph_alt <- ggarrange(Ingestion_graph_both, Maximum_Ingestion_graph_both, both_mortality_graph, both_metabolism_graph,  ncol = 4, 
                                   font.label = list(size = 14, color = "black"))

parameter_rates_graph_alt 


parameter_rates_graph_alt <- annotate_figure(parameter_rates_graph_alt,
                                         bottom = text_grob("Temperature (°C)", size = 14, family = "Times New Roman"),
                                         left = text_grob("µg / day", size = 14, rot = 90, family = "Times New Roman")
)

parameter_rates_graph_alt

parameter_rates_graph_alt_combined <- ggarrange(parameter_rates_graph_alt, turnovergraphs, align = "v", nrow = 2, labels = c("A", "B"), font.label = list(family = "Times New Roman"))
parameter_rates_graph_alt_combined

ggsave(paste0(root,"/figures/parameter_rates_graph_alt.png"), plot = parameter_rates_graph_alt_combined, dpi = 500, height = 4, width = 7, units = "in")


Net_Production_A_hat_.096 = output1_1_non_trivial_varying_temperature_.096$curvepoints[,7] + output1_1_non_trivial_varying_temperature_.096$curvepoints[,8]
Temp_A_hat_.096 = as.numeric(output1_1_non_trivial_varying_temperature_.096$curvepoints[,1])-273.15
Net_Production_A_hat_.096_J = output1_1_non_trivial_varying_temperature_.096$curvepoints[,7]
#Total_Production_A_hat_.096_J =  Net_Production_A_hat_.096_J*output1_1_non_trivial_varying_temperature_.096$curvepoints[,5]
Net_Production_A_hat_.096_A = output1_1_non_trivial_varying_temperature_.096$curvepoints[,8]
#Total_Production_A_hat_.096_A =  Net_Production_A_hat_.096_A*output1_1_non_trivial_varying_temperature_.096$curvepoints[,6]


Net_Production_vs_Temp_DF = data.frame(cbind(Net_Production_A_hat_.096, Temp_A_hat_.096) )

max(Net_Production_A_hat_.096)
which(Net_Production_A_hat_.096 == max(Net_Production_A_hat_.096))
Temp_A_hat_.096[which(Net_Production_A_hat_.096 == max(Net_Production_A_hat_.096))]

colnames(Net_Production_vs_Temp_DF) = c("Net_Production", "Temperature")

ggplot(Net_Production_vs_Temp_DF, aes(x= Temperature, y = Net_Production))+
  geom_line()+ 
  #guides(linetype=guide_legend(title = "Â"))+
  labs(x= "Temperature (°C)", y = expression(paste("Net Production (mg / ", m^{3} ," / day)"))) +
  scale_x_continuous(breaks = round(seq(min(Net_Production_vs_Temp_DF$Temperature), max(Net_Production_vs_Temp_DF$Temperature), by = 2),0))+
  theme_classic() +
  theme(text = element_text(family = "Times New Roman"))


Temp_A_hat = Temp_A_hat_.096

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

population_trajectory_graph = ggplot(df3, aes(x = Temperature, y= value)) + 
  facet_wrap(~name, scale = "free", labeller = labeller_A_J_R)+
  theme_classic()+
  theme(text = element_text(family = "Times New Roman", size = 15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 15)
  )+
  geom_line() + labs(x= "Temperature (°C)", y= expression(paste("Density (mg / ", m^{3} ,")"))) +
  guides(color=guide_legend(title = "Population"))+
  
  scale_color_manual(labels=c("Adult", 
                              "Juvenile"),
                     values = c("Black","Red","Blue"))+
  scale_x_continuous(breaks = round(seq(min(df3$Temperature), max(df3$Temperature), by = 2),0))

population_trajectory_graph

ggsave(paste0(root,"/figures/population_trajectory.png"), plot = population_trajectory_graph, dpi = 300, height = 3, width = 7, units = "in")


Life_Stages_A_J = list("A" = "Adult",
                       "J" = "Juvenile")
labeller_A_J <- function(variable,value){
  return(Life_Stages_A_J[value])
}


ggplot(df3.5, aes(x = Temperature, y= (value))) +
  geom_line() + 
  facet_wrap(~name, scales = "free", labeller = labeller_A_J)+
  guides(color=guide_legend(title = "Population"))+
  theme_classic()+
  theme(text = element_text(family = "Times New Roman")) +
  labs(x= "Temperature (°C)", y= expression(paste("Net Production (mg / ", m^{3} ," /day",")")) ) +
  scale_color_manual(labels=c(expression(paste("Adult (mg / ", m^{3} ," /day",")")), 
                              expression(paste("Juvenile (mg / ", m^{3} ," /day",")"))),
                     values = c("Black","Red","Blue"))+
  scale_x_continuous(breaks = round(seq(min(df3$Temperature), max(df3$Temperature), by = 2),0))

df4 <- data.frame(
  R = R_A_hat,
  J = J_A_hat,
  A = A_A_hat) %>% 
  pivot_longer(c(J, A))

ggplot(df4, aes(x = R, y= value)) + 
  facet_wrap(~name, scale = "free", labeller = labeller_A_J)+ 
  geom_line() + labs(x= expression(paste("Resource Density (mg / ", m^{3} ,")")), y= expression(paste("Population Density (mg / ", m^{3} ,")"))) +
  guides(color=guide_legend(title = "Population"))+
  theme_classic()+
  scale_color_manual(labels=c('Adult (µ/L)', 'Juvenile (µ/L)', 'Resource (µg C/L)'),
                     values = c("Black","Red","Blue")) +
  theme(text = element_text(family = "Times New Roman"))

Total_Population_A_Hat = output1_1_non_trivial_varying_temperature_.096$curvepoints[,5] + output1_1_non_trivial_varying_temperature_.096$curvepoints[, 6]

#Divide by 100 so units for net production are 100 mg/m3/day
Net_Production_A_Hat = Net_Production_A_hat_.096/100

birth_rate_A_Hat = output1_1_non_trivial_varying_temperature_.096$curvepoints[, 3]

#Total total birth rate (term used by de Roos) in the impact function is the integral of every individuals fecundity

#Total birth rate here in units of number of individuals per unit time (in this case per day)?


max(birth_rate_A_Hat)

Temperature_A_Hat = Temp_A_hat_.096

Temperature_A_Hat[which(birth_rate_A_Hat == max(birth_rate_A_Hat))]



df6 <- data.frame(cbind(Temperature = Temperature_A_Hat,
                        
                        birth_rate = birth_rate_A_Hat,
                        
                        Net_Production = Net_Production_A_Hat,
                        
                        #per capita birth rate
                        birth_rate = birth_rate_A_Hat/(Total_Population_A_Hat), 
                        
                        #per capita net production
                        Net_Production = Net_Production_A_Hat/Total_Population_A_Hat))

colnames(df6) = c("Temperature","birth_rate", "Net_Production", "birth_rate", "Net_Production")

df6 <- df6 %>%
  pivot_longer(c(birth_rate, Net_Production, birth_rate, Net_Production))

df6 = data.frame(df6)

df6[,"Type"] = rep(c("Population","Population","Per Capita", "Per Capita"), nrow(df6)/4)

fecundity_net_production_graph <- ggplot(df6, aes(y=(value), x = Temperature, color = name)) + 
  facet_wrap(~Type, nrow = 1, scales = "free")+
  geom_line()+
  theme_classic()+
  labs(x = "Temperature (°C)", y ="Value") +
  guides(color=guide_legend(title = "Population"))+
 
  scale_color_manual(labels=c(expression(paste("Birth Rate (Individuals / ", m^{3} ," / day)")), 
                              expression(paste("Net Production (100 mg / ", m^{3} ," / day)"))),
                     values = c("Black","Red"))+
 
   theme(text = element_text(family = "Times New Roman"),
        axis.text.x = element_text(size = 13),
        axis.title.x = element_text(size = 15),
        axis.text.y = element_text(size = 13),
        axis.title.y = element_text(size = 15),
        strip.text.x = element_text(size = 15, hjust = 0),
        panel.spacing = unit(2, "lines"),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.position = "bottom",
        strip.background = element_rect(color="white", fill="white", linetype="solid"))+
  scale_x_continuous(breaks = round(seq(min(df3$Temperature), max(df3$Temperature), by = 5),0))

fecundity_net_production_graph

ggsave(paste0(root,"/figures/fecundity_net_production_graph.png"), plot = fecundity_net_production_graph, dpi = 300, height = 4, width = 7, units = "in")


Adult_Juvenile_Ratio = A_A_hat/J_A_hat
Adult_Juvenile_Ratio_df = data.frame(cbind(Adult_Juvenile_Ratio, R_A_hat) )

ggplot(Adult_Juvenile_Ratio_df, aes(y=Adult_Juvenile_Ratio, x = R_A_hat)) + 
  
  geom_line()+
  theme_classic()+
  labs(x = expression(paste("Resource Density (mg / ", m^{3} ,")")), y ="Adult:Juvenile Biomass Ratio") +
  scale_color_manual(labels=c("0.04", "0.096", "1.3344"))+
  
  theme(text = element_text(family = "Times New Roman"),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        strip.text.x = element_text(size = 16, hjust = 0),
        panel.spacing = unit(2, "lines"),
        strip.background = element_rect(color="white", fill="white", linetype="solid"))


Adult_Juvenile_Ratio_Temp_df = data.frame(cbind(Adult_Juvenile_Ratio, Temperature_A_Hat) )

Adult_juvenile_ratio_graph = ggplot(Adult_Juvenile_Ratio_df, aes(y=Adult_Juvenile_Ratio, x = Temperature_A_Hat)) + 
  
  geom_line()+
  
  theme_classic()+
  
  labs(x = "Temperature (C°)", y ="Adult:Juvenile Biomass Ratio") +
  scale_x_continuous(breaks = round(seq(min(df3$Temperature), max(df3$Temperature), by = 2),0))+
  
  theme(text = element_text(family = "Times New Roman"),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        strip.text.x = element_text(size = 16, hjust = 0),
        panel.spacing = unit(2, "lines"),
        strip.background = element_rect(color="white", fill="white", linetype="solid"))



ggarrange(population_trajectory_graph,  Adult_juvenile_ratio_graph,nrow = 2, ncol = 1, labels = c("A", "B")) 

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
Year_to_temp_DF[,"Data_Type"] = rep("Fitted",nrow(Year_to_temp_DF))

DF3_Summer = DF3[which(DF3[,"name"] == "Summer"),]
DF3_Summer[,2] = as.character(DF3_Summer[,2])

DF3_Summer = DF3_Summer[,c(1,2,4,5)]

#calculate totals
adult_plus_juveniles <- DF3_Summer %>%
  group_by(Year) %>%
  summarize(total = sum(value))


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
DF3_Summer$Temperature = DF3_Summer$Temperature

DF_Summer_Observed_Predicted = rbind(DF3_Summer,Year_to_temp_DF)
DF_Summer_Observed_Predicted[,2] = as.factor(DF_Summer_Observed_Predicted[,2])
DF_Summer_Observed_Predicted[,5] = as.factor(DF_Summer_Observed_Predicted[,5])

DF_Summer_Observed_Predicted$Data_Type <- factor(DF_Summer_Observed_Predicted$Data_Type, levels = c("Observed", "Fitted"), ordered = TRUE)

ggplot(data =   DF3, aes(x = Year, y = (value), linetype = Stage))+
  facet_wrap(~name)+
  labs(y= "Density (µg/L)") +
  geom_line(size = 0.75)+
  theme_classic()+
  theme(text = element_text(family = "Times New Roman", size = 15))



ggplot(data =  DF_Summer_Observed_Predicted, aes(x = Year, y = (value), color = Data_Type))+
  facet_wrap(~Stage, scale = "free", labeller = labeller_A_J)+
  theme_classic()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        text = element_text(family = "Times New Roman", size = 15)
  )+
  geom_line()+
  guides(color=guide_legend(title = "Data Type"))+
  labs(x = "Temperature (C°)", y = expression(paste("Density (mg/", m^{3} ,")"))) 

observed_J <- DF_Summer_Observed_Predicted %>%
  filter(Stage == "J" & Data_Type == "Observed") %>%
  select(Year, value) %>%
  rename(Observed = value)

predicted_J <- DF_Summer_Observed_Predicted %>%
  filter(Stage == "J" & Data_Type == "Fitted") %>%
  select(Year, value) %>%
  rename(Predicted = value)

J_pred_obs <- predicted_J %>%
  left_join(observed_J)

plot(J_pred_obs$Observed ~ J_pred_obs$Predicted)

observed_A <- DF_Summer_Observed_Predicted %>%
  filter(Stage == "A" & Data_Type == "Observed") %>%
  select(Year, value) %>%
  rename(Observed = value)

predicted_A <- DF_Summer_Observed_Predicted %>%
  filter(Stage == "A" & Data_Type == "Fitted") %>%
  select(Year, value) %>%
  rename(Predicted = value)


A_pred_obs <- predicted_A %>%
  left_join(observed_A)



ggplot(data = DF_Summer_Observed_Predicted, aes(x = log(value), color = Data_Type, fill = Data_Type)) +
  facet_wrap(~Stage, scale = "free", labeller = labeller_A_J)+
  #geom_histogram(aes(y=..density.., color = Data_Type), fill="white")+
  # guides(color=guide_legend(title = "Data Type"))+
  geom_density(alpha=.2)+
  theme_classic()+
  labs(x = expression(paste("Ln(Density) (mg / ", m^{3} ,")")), y ="Density") +
  theme(text = element_text(family = "Times New Roman", size = 15), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        legend.title=element_blank(),
        axis.text = element_text(size = 13),
        )

#Boxplot version of above figure

obs_vs_pred_biomass <- ggplot(data = DF_Summer_Observed_Predicted) +
   geom_boxplot(aes(x = Stage, y = log(value), fill = Data_Type)) +
    scale_fill_manual(values = c("#999999", "lightgrey")) +
  labs(y = expression(paste("Ln(Density) (mg / ", m^{3} ,")"))) +
      theme_classic() + 
  scale_x_discrete(labels = c("Adult", "Juvenile")) +
  theme(text = element_text(family = "Times New Roman"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        legend.title=element_blank(),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        strip.text.x = element_text(size = 16, hjust = 0),
        legend.text = element_text(size = 12)
        )


ggsave(paste0(root,"/figures/obs_vs_pred_biomass.png"), plot = obs_vs_pred_biomass, dpi = 300, height = 5, width = 5, units = "in")


size_mat = output1_1_non_trivial_varying_mj_.096$curvepoints[,1]
size_mat = round(size_mat, 2)

juvenile_biomass = as.numeric(output1_1_non_trivial_varying_mj_.096$curvepoints[,5])
adult_biomass = as.numeric(output1_1_non_trivial_varying_mj_.096$curvepoints[,6])

juvenile_biomass = round(juvenile_biomass, 7)
adult_biomass = round(adult_biomass, 7)
Total_biomass = juvenile_biomass+adult_biomass

max(Total_biomass)

size_mat[which(Total_biomass == max(Total_biomass, na.rm = TRUE))]

mj_and_total_biomass <- cbind(size_mat, Total_biomass)

mj_df = data.frame(rbind(cbind(size_mat,juvenile_biomass, rep("Juvenile",length(juvenile_biomass))),cbind(size_mat, adult_biomass, rep("Adult", length(adult_biomass)))))
colnames(mj_df) = c("mj","Biomass","Stage")
mj_df$Stage = as.factor(mj_df$Stage)
mj_df$Biomass = as.numeric(mj_df$Biomass)
mj_df$mj = as.numeric(mj_df$mj)

mj_adult_juvenile_ratio = data.frame(cbind(adult_biomass/juvenile_biomass, size_mat))
colnames(mj_adult_juvenile_ratio) = c("Ratio","mj")

dev.off()

a <- ggplot(data = mj_adult_juvenile_ratio, aes(y = Ratio, x = mj))  +
  geom_line() +
  scale_x_continuous(limits = c(0.76,265.5))+
  scale_y_continuous(limits = c(0,2.5)) + 
  labs(x = "Size at Maturity (µg)", y = "Adult : Juvenile Biomass Ratio") +
  theme_classic() +
  theme(plot.margin = margin(0.5,1,0.5,1, "cm"),
        text = element_text(family = "Times New Roman"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 14))
a

b <- ggplot(data = mj_df, aes(y = Biomass, x = mj))  +
  facet_wrap(~Stage) +
  geom_line() +
  scale_x_continuous(limits = c(40,265.5))+
  labs(x = "Size at Maturity (µg)", y = expression(paste("Density (mg / ", m^{3} ,")"))) +
  theme_classic() +
  theme(plot.margin = margin(0.5,1,0.5,1, "cm"),
        text = element_text(family = "Times New Roman"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 14),
        strip.text.x = element_text(size = 13))
      

b


size_at_maturity_vs_biomass_and_Stage_ratio_graph <- plot_grid(a,b, ncol = 1, rel_heights = c(4,3), labels = c("B", "C"), label_fontfamily = "Times New Roman", label_colour = "black")
size_at_maturity_vs_biomass_and_Stage_ratio_graph

#ggsave(paste0(root,"/figures/size_at_maturity_vs_biomass_and_Stage_ratio_graph.png"), plot = size_at_maturity_vs_biomass_and_Stage_ratio_graph, dpi = 300, height = 7, width = 7, units = "in")

max(mj_adult_juvenile_ratio$Ratio)
min(mj_adult_juvenile_ratio$Ratio)

#Run this script to generate extinction_vs_size graph to combine with other graphs below

source(paste0(root,"/Scripts/00_mj_analysis.R"))

stage_ratio_biomass_extinction_temp <- plot_grid(extinction_vs_size, size_at_maturity_vs_biomass_and_Stage_ratio_graph,  ncol = 2, rel_widths = c(4,5))

stage_ratio_biomass_extinction_temp

ggsave(paste0(root,"/figures/size_at_maturity_vs_biomass_Stage_ratio_and_extinction_graph.png"), plot = stage_ratio_biomass_extinction_temp, dpi = 300, height = 6, width = 7, units = "in")

