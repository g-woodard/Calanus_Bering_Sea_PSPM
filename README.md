# PSPM for Calanus spp in the Bering Sea

This repository fixes calculation errors, and well as makes structural updates to the zooplankton PSPM model described within the second chapter of 
Grant Woodard's 2023 Master's thesis "Climate Change and Density Dependence in the North Pacific: Applications to Salmon Run Forecasting and Plankton Population Dynamics".
That original version of the repository is available here: https://github.com/grant778/Bering-Sea-Calanus-PSPM.git

Notably:

1. There were stoichiometry/conversion errors in two of the scripts.  

1a. In script "Parameter_Estimation.R" during the conversion of the Ikeda et al. metabolism data from uL/hr to Ug O2 per day I had originally multiplied by 22.4 L per 1 mole when I should have divided by this value.
          
1b. Also in script "Parameter_Estimation.R", when estimating the allometric scalar and exponent for maximum ingestion rate from Saiz and Calbet I originally had fit log(max ingestion)~log(Carbon Mass). However, given that the model uses dry weight, I thought it made more sense to convert Carbon mass to dry weight with a conversion factor of 2.19 (Uye 1982) before fitting this regression.

1c. In script "StageStructuredBiomass_GW_Calanus_max_ingestion_temp_dependent_V5_11_15_2023.R" for calculating net production, when converting metabolic costs to copepod dry weight losses, I had originally multiplied the metabolic rate by a copepod energy density of 0.0279 J/ug dw (Davies et al. 2012) when I should have divided by this value.

Errors 1a and 1c more or less cancelled each other out, however, error 1b divided the maximum ingestion rate allometric scalar approximately in half and broke the model. To rectify this I move to discussion 2.

2. In script "Parameter_Estimation.R", maximum ingestion rate has been reformatted from being fit to log(max ingestion) ~ log(DW) to log(max ingestion) ~ log(DW) + log(Resource Density). Maximum ingestion in this formulation depends almost entirely on resource density. The regression has a coefficient of 0.99 for resource density versus a coefficient of 0.006 for mass in dry weight. The calculation of maximum ingestion rate in script PSPM_Model_Structure.R has been updated to reflect this change (e.g. is now mass and resource density dependent, with a scalar and an allometric exponent and a resource density exponent). 


Importantly:
The version of the scripts/model in this repository are the version used in the subsequent peer-review publication:


## Summary

This repository can be used to fit a stage-structured biomass model for the copepod Calanus marshallae/glacialis using the R package `PSPManalysis`. The model is used to predict population impacts of ocean warming by modeling the direct effects of environmental temperature on individual physiology and translating those effects to the population level.

## Data 

The repository contains the following data files:

  `Bering_SST_12_06.xslx`
    Bering SST temperature from NOAA physical sciences laboratory

  `Ikeda_2007_Respiration_Data.csv`
    Copepod respiration data

      Ikeda, T., Sano, F., and Yamaguchi, A., 2007. Respiration in marine pelagic copepods: a global-
      bathymetric model. Marine Ecology Progress Series 339: 215-219
      Intergovernmental Panel on Climate Change (IPCC). 2014. Climate Change 2014: 
      
      Synthesis Report. Contribution of Working Groups I, II and III to the Fifth Assessment 
      Report of the Intergovernmental Panel on Climate Change.

  `Saiz_Calbert_2007_Calanus_Ingestion.csv`
    Calanus Respiration data

      Saiz, E., and A. Calbet. 2007. Scaling of feeding in marine calanoid copepods. Limnology and 
      Oceanography 52: 487-921.

## Analyses 

The repository contains the following files for running the analyses:

`PSPM_Model_Structure.R`

  PSPM model structure is contained here. Required by model implementation scripts for them to run.

`00_Model_Fit.R`

  Code detailing parameter estimation from Ikeda et al. 2007 and Saiz and Calbet 2007 data
  
`Run_Model_V2.R`

  Runs Basic PSPM model using parameters defined at top of code (or if using model defaults in     
    PSPM_Model_Structure.R )

  This model 
  1. identifies a bifurcation point from the trivial equilibrium,
  2. uses the bifurcation point to find the non trival     
    equilibrium,
  3. then varies temperature at the nontrivial equilibrium to identify extinction temperature.
  4. Additionally it varies 
    size at maturity at the default temperature to assess changes in stage ratios.
  5. It also produces relevant graphs

`00_mj_analysis.R`

  Loops through first 3 steps of model 1 at varing sizes at maturity and saves the extinction temperature.
  Graphs temperature at extinction vs size at maturity.

## Software and computational requirements

Analyses performed in R version 4.2.2 (2022-10-31). 

The PSPM model was run using the R package `PSPManalysis` version 0.3.9.

All analyses can be run on a personal laptop computer without the need for parallelization or high performance computing.

## Copyright

MIT License

Copyright (c) 2024 g-woodard

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
