library(PSPManalysis)
# 
# 
# PSPMdimensions <- c(PopulationNr = 1, IStateDimension = 1, LifeHistoryStages = 2)
# 
# 
# NumericalOptions <- c(MIN_SURVIVAL = 1.0E-9, # Survival at which individual is considered dead
#                       MAX_AGE = 100000, # Absolute maximum individual age
#                       DYTOL = 1.0E-7, # Variable tolerance
#                       RHSTOL = 1.0E-8) # Function tolerance
# 
# DefaultParameters <- c(Beta0 = 47.0, Beta1 = 0.04, Aj = 11.0, Mu0 = 0.00095, Mu1 = 0.0581)
# 
# StateAtBirth <- function(E, pars)
# {
#   with(as.list(c(pars)),{
#     # We model a single structured population with a single i-state variable (age)
#     c(Age = 0.0)
#   })
# }
# 
# 
# LifeStageEndings <- function(lifestage, istate, birthstate, BirthStateNr, E, pars) {
#   with(as.list(c(E, pars, istate)),{
#     maturation = switch(lifestage, Age - Aj, -1)
#   })
# }
# 
# 
# 
# LifeHistoryRates <- function(lifestage, istate, birthstate, BirthStateNr, E, pars) {
#   with(as.list(c(pars, istate)),{
#     list(
#       # We model a single structured population (nrow=1) with a single i-state variable (age)
#       development = 1.0,
#       fecundity = switch(lifestage, 0, Beta0*exp(-Beta1*(Age - Aj))),
#       mortality = Mu0*exp(Mu1*Age)
#     )
#   })
# }
# 


#showpspm("Medfly.R") 

output = PSPMdemo("Scripts/Medfly.R",c(2, 11, 0.1, 11, 20), c(47, 0.04, 11, 0.00095, 0.0581), c("isort", "0"), clean=TRUE, force=TRUE)


csbread("Medfly-PGR-0000.csb",3)



