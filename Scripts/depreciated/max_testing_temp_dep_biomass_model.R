#----- Testing a temperature dependent biomass model
# I'm using this model as a template:
showpspm("StageStructuredBiomass.R")
# and add temperature dependence to it (see attached model).

# The approach is as follows: 
# 1) find the trivial equilibrium (consumers extinct)
# 2) start at the bifurcation that is consumer invasion, and continue along Rmax to plot non-trivial equilibria as a funciton or Rmax
# 3) start from a non-trivial equilibrium and continiue in "another direction", e.g, Q or temperature

# load package
library(PSPManalysis)
library(ggplot2)
library(tidyr)

#----- The non-temperature dependent model
# 1) First step is to find the trivial equilibrium, when consumers are 0. We do this
# Because we can then move from this equilibrium (i.e., "continuation") and by that find the non-trivial equilibra
EQ.triv <-  PSPMequi(modelname = "Scripts/biomass_model.R",
                     biftype = 'EQ', 
                     startpoint = c(0, 0.01), # Consists of the 1) starting value of the parameter to vary 2) the estimated equilibrium values for all the environment variables and 3) the estimated values of the birth rate for all the structured populations in the model. * NOTE the third argument must be omitted *IF* we have opttions popZE and 0, which means we are stating the population is extinct and therefore we should not give a starting value for it
                     stepsize = 1, 
                     parbnds = c("Rmax", 0, 3), # 1) bifurcation parameter, Rmax 2) lower threshold 3) upper threshold
                     parameters = NULL, 
                     options = c("popZE",'0'), # This tells the software that the structured population with index 0 is in the trivial equilibrium and has a birthrate of zero. 0 is the index of the population
                     clean = FALSE, 
                     force = FALSE,
                     debug = FALSE)
# Check output
EQ.triv

# Make a data frame for easy plotting
df <- data.frame(R_max = EQ.triv$curvepoints[, 1],
                 R = EQ.triv$curvepoints[, 2],
                 J = EQ.triv$curvepoints[, 5],
                 A = EQ.triv$curvepoints[, 6]) %>% 
  pivot_longer(c(R, J, A))

# Here we see that in the absence of consumers, R=Rmax
# We can add in our bifurcation point (vertical line), which corresponds to the invasion point 
# or the minimum resource density needed for consumer persistence
ggplot(df, aes(R_max, value, color = name, linetype = name)) + 
  geom_line(size = 2) + 
  theme_light() +
  geom_point() +
  geom_vline(xintercept = EQ.triv$bifpoints[1], linetype = 2) # this is the bifurcation point

# 2) Now we have the minimum R_max to sustain the consumer. It's a bifurcation.
# Continue the equilibrium along Rmax from here to calculate and plot positive consumer biomass densities
# I.e., the non-trivial equilibria along Rmax
BT.Rmax <- PSPMequi('Scripts/biomass_model.R',
                    'EQ', # I will use BP when I want the two-parameter bifurcation, for now I still want equilibrium continuation!
                    startpoint = c(EQ.triv$bifpoints[1], EQ.triv$bifpoints[1], 0.0), # Note the third argument is now a vector of length 3, where each element is
                    # 1) Rmax parameter 2) Environment 3) Birth Rate (I think this is b[]), but here I can still omit the third argument since I know consumers are extinct here!
                    parbnds = c("Rmax", EQ.triv$bifpoints[1], 10), # 1) bifurcation parameter, Rmax 2) lower threshold 3) upper threshold
                    stepsize = 1)


BT.Rmax

# Make dataframe and plot non-trivial equilibrium as a function of Rmax 
df2 <- data.frame(R_max = BT.Rmax$curvepoints[, 1],
                  R = BT.Rmax$curvepoints[, 2],
                  J = BT.Rmax$curvepoints[, 5],
                  A = BT.Rmax$curvepoints[, 6]) %>% 
  pivot_longer(c(R, J, A))

ggplot(df2, aes(R_max, value, color = name)) + 
  geom_line() + 
  theme_light()


# 3) Continue from the non-trivial equilibrium along another parameter, Q
Output <- PSPMequi("Scripts/biomass_model.R", 
                   "EQ", 
                   startpoint = c(0.5, 1.7, 2.44051986E-01), # 1) This now corresponds to the starting value of the bifurcation parameter, and the other correspond to the starting value so that we are close to the equilibrium. I use parameter no. 6, wihich is Q, and start at 0.6 2) the enviroment-value that corresponds to the equilibrium at the parameter-combination given below 3) third argument corresponds to the birth rate at the Rmax=10 equilibrium. We find 2) and 3) in the previous call of the PSPMequi-function!
                   stepsize = 0.1, 
                   parbnds = c("Q", 0, 3), # 1) bifurcation parameter (Q) 2) lower threshold 3) upper threshold 
                   parameters = c(Rho = 0.1, Rmax = 10.0, Sb = 0.5, Sm = 1.0, Mc = 1.0, Hc = 2.0, Q = 0.5, Sigma = 0.5, Tc = 0.1, Mu = 0.02),
                   clean = TRUE,
                   force = TRUE)

# Plot...
df3 <- data.frame(Q = Output$curvepoints[, 1],
                  J = Output$curvepoints[, 5],
                  A = Output$curvepoints[, 6]) %>% 
  pivot_longer(c(J, A))

ggplot(df3, aes(Q, value, color = name)) + 
  geom_line() + 
  theme_light()




#----- Add temperature dependence to ingest only (for simplicity)
# see attached file

# Now do the same for the model with temperature dependent mortality and do it over temperature instead of Q in the last plot

# This is the non-temperature dependent model
EQ.triv2 <-  PSPMequi(modelname = "Scripts/biomass_model_with_temp.R",
                     biftype = 'EQ', 
                     startpoint = c(0, 0.01), 
                     stepsize = 1, 
                     parbnds = c("Rmax", 0, 3),
                     parameters = NULL, 
                     options = c("popZE",'0'),
                     clean = FALSE, 
                     force = FALSE,
                     debug = FALSE)

EQ.triv2

df4 <- data.frame(R_max = EQ.triv2$curvepoints[, 1],
                  R = EQ.triv2$curvepoints[, 2],
                  J = EQ.triv2$curvepoints[, 5],
                  A = EQ.triv2$curvepoints[, 6]) %>% 
  pivot_longer(c(R, J, A))

ggplot(df4, aes(R_max, value, color = name, linetype = name)) + 
  geom_line(size = 2) + 
  theme_light() +
  geom_point() +
  geom_vline(xintercept = EQ.triv2$bifpoints[1], linetype = 2) # this is the bifurcation point


# Now we have the minimum R_max to sustain the consumer. 
# **** For some reason it isn't the same in the temperature and non-temperature dependent models! Even though I set T=T_0 ****
EQ.triv$bifpoints[1]
EQ.triv2$bifpoints[1]

BT.Rmax2 <- PSPMequi('Scripts/biomass_model_with_temp.R',
                     'EQ', 
                     startpoint = c(EQ.triv2$bifpoints[1], EQ.triv2$bifpoints[2], 0.0), 
                     parbnds = c("Rmax", EQ.triv2$bifpoints[1], 10),
                     stepsize = 1)


BT.Rmax2

df5 <- data.frame(R_max = BT.Rmax2$curvepoints[, 1],
                  R = BT.Rmax2$curvepoints[, 2],
                  J = BT.Rmax2$curvepoints[, 5],
                  A = BT.Rmax2$curvepoints[, 6]) %>% 
  pivot_longer(c(R, J, A))

ggplot(df5, aes(R_max, value, color = name)) + 
  geom_line() + 
  theme_light()

BT.Rmax2$curvepoints

# Now we want to continue along a temperature axis:
Output2 <- PSPMequi("Scripts/biomass_model_with_temp.R", 
                   "EQ", 
                   startpoint = c(292.15, 2.009955, 0.09377099), # 1) This now corresponds to the starting value of the bifurcation parameter,
                   # c(<parameter>,<environment variables>,<population birth rates>). The values are the last row in BT.Rmax2, which correspond to 
                   # equilibrium values when Rmax~10.
                   stepsize = 0.1, 
                   parbnds = c(11, 282.15, 302.15), # 1) bifurcation parameter index (temp) 2) lower threshold 3) upper threshold 
                   clean = TRUE,
                   force = TRUE)

head(Output2$curvepoints)

# Plot J and A biomass as a function of temperature given temperature dependent ingestion
df6 <- data.frame(Temp = Output2$curvepoints[, 1],
                  R = Output2$curvepoints[, 2],
                  J = Output2$curvepoints[, 5],
                  A = Output2$curvepoints[, 6]) %>% 
  pivot_longer(c(R, J, A))

ggplot(df6, aes(Temp, value, color = name)) + 
  geom_line() + 
  theme_light()


