library(PSPManalysis)
library(tidyverse)

#demo("deRoosPersson", echo=FALSE)

options(buildtools.check = function(action) TRUE )

output1 <- PSPMequi(modelname = "Scripts/PNAS2002.R", biftype = "EQ", startpoint = c(1.0E-06, 1.0E-06), stepsize = 0.5,
                   parbnds = c(c(1, 0, 4E-4)), parameters = NULL, minvals = NULL, maxvals = NULL, options = c(c("popZE", "0", "envZE", "1", "envZE", "2")), clean
                   = TRUE, force = FALSE, debug = FALSE, silent = FALSE)


output1$curvepoints
output1$bifpoints


output2 <- PSPMequi(modelname = "Scripts/PNAS2002.R", biftype = "EQ", startpoint = output1$bifpoints[c(1,2)], stepsize = 0.1,
                               parbnds = c(c(1, 0, 4E-4)), parameters = NULL, minvals = NULL, maxvals = NULL, clean
                               = TRUE, force = FALSE, debug = FALSE, silent = FALSE)


#For this reason, the PSPMequi function
#is invoked with the option vector c("popZE", "0", "envZE", "1", "envZE", "2"), which
#enforces this zero equilibrium state for the structured population with index 0 (the consumer),
#the environment variable with index 1 (the predator) and the environment variable with index
#2 (the biomass of small consumers that are vulnerable to predation). The latter is obviously in
#a zero equilibrium state, because the variable represents an integral over the 
#population distribution of the consumer. 

#popZE  - Index of structured population in trivial equilibrium (can be used multiple times)

#envZE  - Index of environment variable in trivial equilibrium (can be used multiple times)

output2$curvepoints
output2$bifpoints

output3 <- PSPMequi(modelname = "Scripts/PNAS2002.R", biftype = "EQ", startpoint = output2$bifpoints[c(1,2,3,7,5)], stepsize = -0.1,
                    parbnds = c(c(1, 0, 4E-4)))



output3$curvepoints
output3$bifpoints


output4 <- PSPMequi("Scripts/PNAS2002.R","BP",startpoint = c(output1$bifpoints[1:2],0.01),stepsize = 0.05, parbnds = c(1,0,4E-4,11,0,0.1),
                    options = c("envZE","1","envZE","2","popBP","0"), clean = TRUE)
  

output4$curvepoints
output4$bifpoints

output5 <- PSPMequi("Scripts/PNAS2002.R","BPE", startpoint = c(output2$bifpoints[c(1,2,5)],0.01), stepsize = -0.1, parbnds = c(1,0,4E-4,11,0,0.1), NULL,
                    options = c("envZE","2","envBP","1"), clean = TRUE)
  

output5$curvepoints
output5$bifpoints

output6 <- PSPMequi("Scripts/PNAS2002.R","LP", startpoint = c(output3$bifpoints[1:5],0.01), stepsize = 0.05, parbnds = c(1,0,4E-4,11,0,0.1), NULL, NULL, clean = TRUE)

output6$curvepoints
output6$bifpoints

#output = PSPMdemo("Scripts/Medfly.R",c(2, 11, 0.1, 11, 20), c(47, 0.04, 11, 0.00095, 0.0581), c("isort", "0"), clean=TRUE, force=TRUE)


#startpoint = c(parameter, environmental variables, pop birth rates)
#Testing my model:
# output1 <- PSPMequi(modelname = "Scripts/Woodard_2022.R", biftype = "BPE", startpoint = c(1.0E-06, 1.0E-06), stepsize = 0.05,
#                     parbnds = c(c(1, 0, 4E-4)), parameters = NULL, minvals = NULL, maxvals = NULL, options = c(c("envZE", "0")),
#                     clean = FALSE, force = FALSE, debug = FALSE, silent = FALSE)

output1 <- PSPMequi(modelname = "Scripts/Woodard_2022_test6.R", biftype = "EQ", startpoint = c(0, 0, 0 ), stepsize = 0.5,
                    parbnds = c(c(1, 0, 1)), parameters = NULL, minvals = NULL, maxvals = NULL, 
                    clean = TRUE, force = FALSE, debug = FALSE, silent = FALSE)

#Varying Rmax

Negative_to_zero = function(value)
{
  if(value < 0)
  {
    value = 0
  }
  
  return(value)
}

output1_2 <-PSPMequi(modelname = "Scripts/StageStructuredBiomass_GW_roach.R", biftype = "EQ", startpoint = c(0.05, 0.05), stepsize = 0.1,
                     parbnds = c(1, 0, 1), parameters = NULL, minvals = NULL, maxvals = NULL, options = c(c("popZE", "0")),
                     clean = TRUE, force = FALSE, debug = FALSE, silent = FALSE)

output1_2$curvepoints
output1_2$bifpoints

output1_2$bifpoints[1] #minimum Resource needed to sustain population

df <- data.frame(R_max = output1_2$curvepoints[, 1],
                 R = output1_2$curvepoints[, 2],
                 J = output1_2$curvepoints[, 5],
                 A = output1_2$curvepoints[, 6]) %>% 
  pivot_longer(c(R, J, A))

ggplot(df, aes(R_max, value, color = name, linetype = name)) + 
  geom_line(size = 2) + 
  theme_light() +
  geom_point() +
  geom_vline(xintercept = output1_2$bifpoints[1], linetype = 2) # this is the bifurcation point


output1_3 <-PSPMequi(modelname = "Scripts/StageStructuredBiomass_GW_roach.R", biftype = "EQ", startpoint = c(output1_2$bifpoints[1], output1_2$bifpoints[2], 0.0), stepsize = 1,
                     parbnds = c(1, output1_2$bifpoints[1], 5), parameters = NULL, minvals = NULL, maxvals = NULL, 
                     clean = TRUE, force = FALSE, debug = FALSE, silent = FALSE)

output1_3$curvepoints
output1_3$bifpoints

df2 <- data.frame(R_max = output1_3$curvepoints[, 1],
                  R = output1_3$curvepoints[, 2],
                  J = output1_3$curvepoints[, 5],
                  A = output1_3$curvepoints[, 6]) %>% 
  pivot_longer(c(R, J, A))

ggplot(df2, aes(R_max, value, color = name)) + 
  geom_line() + ylim(c(0,1)) +
  theme_light()

which(output1_3$curvepoints[,6] > output1_3$curvepoints[,2]) 


output1_3$curvepoints[58,2] #Equilibrium Resource Value
output1_3$curvepoints[58,3] #Equilibrium Birth Rate Value

output1_4 <-PSPMequi(modelname = "Scripts/StageStructuredBiomass_GW_roach.R", biftype = "EQ", startpoint = c(292.15, output1_3$curvepoints[59,2], output1_3$curvepoints[59,3]), stepsize = .01,
                     parbnds = c(3, 282.15, 302.15), parameters = NULL, minvals = NULL, maxvals = NULL, 
                     clean = TRUE, force = FALSE, debug = FALSE, silent = FALSE)

 output1_4$curvepoints
output1_4$bifpoints

df6 <- data.frame(Temp = output1_4$curvepoints[, 1],
                  J = output1_4$curvepoints[, 5],
                  A = output1_4$curvepoints[, 6]) %>% 
  pivot_longer(c(J, A))

ggplot(df6, aes(Temp, value, color = name)) + 
  geom_line() + 
  theme_light()
#Model currently doesn't have starvation, but can add
#currently no increased mortality from starvation
#Fecundity only occurs if there is positive net biomass production in Adults
# IMPORTANT:  The resource turnover rate delta is currently not temperature dependent.



#Work with known parameter set
#Be able to articulate biological questions in simple terms

#Secondary production in zooplankton Edmonson editor is Downing
  #estimating zooplankton production rates and reproductive rates

#source("Scripts/Indet_growth.R")

