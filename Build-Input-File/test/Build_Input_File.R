##  Script filename = Build Input File.R
## Purpose: Extract input data and build an AGEPRO input file
## Based on 013_BuildAGEPROinputfile.R by Michelle Sculley

## Set libraries
library(r4ss)
library(plyr)
library(dplyr)
library(purrr)
#install.packages(this.path)
library(this.path)
## Clear environment if needed
## rm(list=ls())

## Set model domain variables
endyr <- 2020  ## Set last year of assessment
TimePeriod <- "Year"  ## Set time step to be "Year" or "Quarter" for years as quarters
NYears <- 10  ## Set number of years in projection

## Set AGEPRO recruitment model variables
NRecModel <- 1 ## Set number of recruitment models = 1
RecruitType <- c(14) ## Set recruitment model vector with NRecModel elements
M0 <- 0.54 ## Set age-0 natural mortality rate

## Create a variable for the main directory to use to build paths later on
main.dir <- this.path::here(..=2)

## Set Rscripts folder
#script.dir="C:\\Users\\jon.brodziak\\Desktop\\2024 WCNPO MLS Rebuilding\\Rscripts"
script.dir = file.path(main.dir, "Rscripts")
## Set bootstrap file path
#boot_file <- "C:\\Users\\jon.brodziak\\Desktop\\2024 WCNPO MLS Rebuilding\\Bootstrap-numbers-at-age\\2023_WCNPOMLS.bsn"
boot_file <- file.path(main.dir, "Bootstrap-numbers-at-age", "2023_WCNPOMLS.bsn")
## Set base case model folder
#model.dir <- c("C:\\Users\\jon.brodziak\\Desktop\\2024 WCNPO MLS Rebuilding\\base")
model.dir <- file.path(main.dir, "base")
## Source SS_Agepro.R script
source(file.path(script.dir,"SS_to_Agepro.R"))
              
## Extract SSInput with SS_To_Agepro.R script			  
SSInput <- SS_To_Agepro(model.dir=model.dir, script.dir=script.dir, endyr=endyr, TimeStep=TimePeriod)

## Copy SSInput into SaveInput to save it
SaveInput <- SSInput

## Adjust SSInput to the fleets you want to include: for this I am only choosing the fleets with unique selectivities, then for the fleets with the same selectivities, adding up the catch proportions for the new fleet
UniqueFleets<- SSInput$Fishery_SelAtAge %>%
  filter(Yr == 2020) %>%
  distinct(across(-c(Yr, Fleet)), .keep_all = TRUE) %>%
  select(Fleet) %>% pull()

## Identify set of unique fleets
duplicated_rows = list()
for (i in 1:length(UniqueFleets)){
  target_row <- SSInput$Fishery_SelAtAge[UniqueFleets[i],1]
  match_row <- SSInput$Fishery_SelAtAge[,1] 
  duplicated_rows[[i]] <- which(match_row %in% target_row == TRUE)
}

## Set fishery selectivity at age for unique fleets
SSInput$Fishery_SelAtAge <- SSInput$Fishery_SelAtAge[UniqueFleets,]
##Average your SelAtAge across your interested years
SSInput$Fishery_SelAtAge<-SSInput$Fishery_SelAtAge %>%
  filter(Fleet %in% UniqueFleets) %>%   ##filter out only the unique fleets
  filter(Yr %in% YearAvg)  %>% ##save the year/years you want to average the Fishery selectivity over
  group_by(Fleet) %>%  ## average each age by fleet
  summarize(across(c(2:16),mean))


## Set number of fleets
SSInput$Nfleets <- nrow(SSInput$Fishery_SelAtAge)

## Set CV of process error for fishery selectivity at age
SSInput$Fishery_SelAtAgeCV <- SSInput$Fishery_SelAtAgeCV[UniqueFleets,]

## Set catch at age for each fleet
SSInput$Catage<-SSInput$Catage[UniqueFleets,]

## Set CV of catch at age for each fleet
SSInput$CatageCV<-SSInput$CatageCV[UniqueFleets,]

## Summing the fleets' catch which share selectivities so that they can be used to proportion catch in projections
SSInput$CatchbyFleet<-purrr::map_dbl(duplicated_rows,~sum(as.vector(SSInput$CatchbyFleet)[.x]))
         
### Recruitment input
# You'll need to set a different Recruitment list for each recruitment model choice. They are detailed below:
## SS_To_Agepro has pulled some of the Information from SS for different recruitment scenarios: these include the alpha, beta, and variance parameters for a stock recruitment curve (BH or Richards) and the observed recruitment by year and SSB 


## Model 1 - this isn't set up yet

## Model 2
#Recruitment is a list with two objects: Nobs with is the number of observations, recruits is a dataframe with two columns, recruitment and SSB

#Recruitment <- list()
#Recruitment$Recr_Model <- 2
#Recruitment$Recr_Prob <- rep(1,NYears)
#Recruitment$Nobs <- 20  ## I set this to the last twenty years of the assessment but it can be changed to any timeframe
#firstyr <- 2001 ## change these to match the year range you want
#lastyr <- 2020 
#Recruitment$Recruits<-SSInput$RecruitmentObs %>% 
#  filter(Yr %in% firstyr:lastyr) %>%
#  select(SpawnBio,pred_recr)

## Model 3
# Recruitment is a list with two objects: Nobs is the number of observations, and recruits is a vector of observed recruitment

## Model 4 and 15
# Recruitment is a list with 4 objects: Nobs is a vector with the number of observations in each recruitment level, Level1obs is a dataframe of recruitment and SSB for the level one recruitment observations, Level2Obs is a dataframe of recruitment and SSB for the level two recruitment observations, and SSBBreak is the SSB break limit between the two recruitment levels

## Models 5, 6, 7,10, 11, and 12

#Recruitment is a list with 3 to 5 objects: alpha and beta which are the S/R parameters, Kparm (only for models 7 and 12) is the K parameter, var is the recruitment variance, and for 10, 11 and 12, Phi is the Phi parameter, and LastResid is the last recruitment residual for the auto-correlated error
#Recruitment <- list()
#Recruitment$Recr_Model <- 5
#Recruitment$Recr_Prob <- rep(1,NYears)
#Recruitment$alpha <- SSInput$alpha
#Recruitment$beta <- SSInput$beta
#Recruitment$var <- SSInput$BH_var

## Model 8 and 13
# Recruitment is a list with 2 (model 8) or 4 objects (model 13): the mean and standard deviation (stdev) of the lognormal distribution, and for model 13, Phi and LastResid which is the value of the last recruitment residual

## Model 14

# Recruitment for model 14 is a list with two objects: Nobs is the number of observations and Obs is a vector of observed recruitment
# Note 1: The filter below selects Nobs recruitments from endyr-Nobs+1:endyr
# Note 2: The predicted recruitment values output from this script match the values in the 2023 base case Report.sso file

Recruitment <- list()
Recruitment$Recr_Model <- RecruitType[1]
Recruitment$Recr_Prob <- rep(1,NYears)
Recruitment$Nobs <- 20 ## Assuming the number of observations is the last 20 years of recruitment
Recruitment$Obs <- SSInput$RecruitmentObs %>%
  filter(Yr %in% (endyr-Recruitment$Nobs+1):endyr) %>%
  select(pred_recr)
  
## Save age-0 recruitment (thousands of age-0 fish on July 1st in endyr) for checking input
Recruitment_Age0_July_1st <- Recruitment$Obs

## Calculate age-0.5 recruitment on January 1st for annual projections (thousands of age-0.5 fish on January 1st in endyr+1)
Recruitment$Obs <- exp(-0.5*M0)*Recruitment$Obs

## Model 16, 17, 18, and 19
# Recruitment is an object with 5 objects: Ncoeff is the number of coefficients, var is the variance, Intercept is the y-intercept, coeff is a vector containing the value of each coefficient, and Observations is a dataframe where each row the observations for each coefficient and each column is a year of the projection

## Model 20
# Recruitment is a vector of the fixed recruitment for each year.

## Model 21
# Recruitment is a list containing 3 objects: Nobs is the number of observations, Obs is a vector of observed recruitments, and SSBHingeValue is the value at which the linear decline to zero begins.

## Harvest is the harvest strategy you want to use: first column is the harvest specification, then one column for each fleet for each year. Landings (catch) = 1, F-Mult (F) = 0, Removals = ?

## Here is an example assuming constant catch over the number of years of the model where catch is partitianed based upon the relative catch by fleet in the last year of the assessment, this is calculated in SSInput$CatchbyFleet
TotalCatch <- 2000

FleetRemovals <- t(sapply(SSInput$CatchbyFleet, function(p) rep(p*TotalCatch,NYears)))

Harvest <- list("Type"=c(rep(1,NYears)),"Harvest"=FleetRemovals)

source(file.path(script.dir,"AGEPRO_Input.R"))

## Now write the input file:

AGEPRO_INP(output.dir = "C:\\Users\\jon.brodziak\\Desktop\\2024 WCNPO MLS Rebuilding\\Build-Input-File\\test",
                    boot_file = boot_file,
                    SS_Out = SSInput,
                    ModelName="test_input",
                    ProjStart = 2021,
                    NYears = 10,
                    MinAge = 1,
                    Nsims = 100,
                    NRecr_models = NRecModel,
                    Discards = 0,
                    set.seed = 123,
                    ScaleFactor = c(1000,1000,1000),  #population scaling factor, recruitment scaling factor, SSB scaling factor
                    UserSpecified = c(0,-1,0,0,0,0,0),
                    TimeVary = c(0,0,0,0,0,0,0),
                    FemaleFrac = 0.5,
                    Recruitment = Recruitment,
                    Harvest = Harvest,
                    doRebuild = FALSE,
                    LandingType = 1,
                    ThresholdReport = TRUE,
                    RefPointSSB = 3000,
                    RefPointJan1 = 0,
                    RefPointMidYr = 0,
                    RefPointF = 0.6,
                    SetBounds = TRUE,
                    MaxWeight = 250,
                    MaxNatM = 1,
                    SumReport = 1,
                    AuxFiles = 0,
                    RExport = 0,
                    SetScale = TRUE,
                    PercentileReport = TRUE,
                    Percentile = 50)
