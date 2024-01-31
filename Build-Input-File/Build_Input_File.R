F##  Script filename = Build Input File.R
## Purpose: Extract input data and build an AGEPRO input file
## Based on 013_BuildAGEPROinputfile.R by Michelle Sculley

## Set libraries
library(r4ss)
library(plyr)
library(dplyr)
library(purrr)

## Clear environment if needed
## rm(list=ls())

## Set model domain variables
endyr <- 2020  ## Set last year of assessment
TimePeriod <- "Year"  ## Set time step to be "Year" or "Quarter" for years as quarters
NYears <- 10  ## Set number of years in projection
NSims <- 10 ## Set number of simulations per bootstrap

## Set AGEPRO recruitment model variables
NRecModel <- 1 ## Set number of recruitment models = 1
## Set recruitment model vector with NRecModel elements
RecruitType <- c(14) ## The ordered vector RecruitType is model 14
RecFac <- 1000 ## RecFac is a multiplier to convert units of recruit model output (e.g., 1000 fish) to AGEPRO numbers of fish
SSBFac <- 1000 ## SSBFac is a divisor to convert kg of SSB in AGEPRO to recruit model input units of SSB (e.g., 1000 mt)
## Note: Set SSBFac <- 1 for empirical recruits per spawner models 2 and 4
MaxRecObs <- 100 ## Maximum number of recruitment observations in a recruit model
M0 <- 0.54 ## Set age-0 natural mortality rate to convert age-0.0 fish to age-0.5 fish

## Set Rscripts folder
script.dir="C:\\Users\\jon.brodziak\\Desktop\\2024 WCNPO MLS Rebuilding\\Rscripts"

## Set bootstrap file variables
boot_file <- "C:\\Users\\jon.brodziak\\Desktop\\2024 WCNPO MLS Rebuilding\\Bootstrap-numbers-at-age\\2023_WCNPOMLS.bsn"
NBoot <- 100
BootFac = 1000

## Set base case model folder
model.dir <- c("C:\\Users\\jon.brodziak\\Desktop\\2024 WCNPO MLS Rebuilding\\base")

## Source SS_Agepro.R script
source(file.path(script.dir,"SS_to_Agepro.R"))
              
## Extract SSInput with SS_To_Agepro.R script			  
SSInput <- SS_To_Agepro(model.dir=model.dir, script.dir=script.dir, endyr=2020, TimeStep="Year")

## Copy SSInput into SaveInput to save it
SaveInput <- SSInput

## Adjust SSInput to the fleets you want to include: for this I am only choosing the fleets with unique selectivities, then for the fleets with the same selectivities, adding up the catch proportions for the new fleet
UniqueFleets<- SSInput$Fishery_SelAtAge %>%
  filter(Yr == 2020) %>%
  distinct(across(-c(Yr, Fleet)), .keep_all = TRUE) %>%
  select(Fleet) %>% pull()


##identify years in the model
Years <- seq(min(SSInput$Fishery_SelAtAge$Yr),max(SSInput$Fishery_SelAtAge$Yr))


## Identify set of unique fleets by year
matching_indices<-list()
for(j in 1:length(Years)) {
  matching_indices[[j]]<-list()
  for (i in 1:length(UniqueFleets)){
    # Select the target row (row 1 in this case)
    target_row <- SSInput$Fishery_SelAtAge[which(SSInput$Fishery_SelAtAge$Fleet==UniqueFleets[i]&SSInput$Fishery_SelAtAge$Yr==Years[j]), -2]
    
    # Find rows that match the target row
    matching_indices[[j]][[i]] <- which(apply(SSInput$Fishery_SelAtAge[,-2], 1, function(x) all(x %in% target_row)))
  }}





YearAvg <- seq(endyr-2,endyr)  ## define which years you want to average you selectivity and catch at age over

##Average your SelAtAge across your interested years
SSInput$Fishery_SelAtAge<-SSInput$Fishery_SelAtAge %>%
  filter(Fleet %in% UniqueFleets) %>%   ##filter out only the unique fleets
  filter(Yr %in% YearAvg)  %>% ##save the year/years you want to average the Fishery selectivity over
  group_by(Fleet) %>%  ## average each age by fleet
  summarize(across(c(2:16),mean))

## Number of unique fleets in the assessment
SSInput$Nfleets<-length(UniqueFleets)

## Adjust CV matrix to match the number of unique fleets
SSInput$Fishery_SelAtAgeCV<-SSInput$Fishery_SelAtAgeCV[UniqueFleets,]


## Average your catch at age across your interested years
SSInput$Catage<-SSInput$Catage %>% 
  filter(Fleet %in% UniqueFleets) %>%   ##filter out only the unique fleets
  filter(Yr %in% YearAvg) %>%  ##save the year/years you want to average the catch at age over
  group_by(Fleet) %>%  ## average each age by fleet
  summarize(across(c(2:16),mean))

## Adjust the CV matrix
SSInput$CatageCV<-SSInput$CatageCV[UniqueFleets,]


## For the catch by fleet, produce a list of the rows by year that should be combined for fleets that share selectivity
duplicated_rows<-matching_indices[[1]]
## Summing the fleets' catch and Fs which share selectivities so that they can be used to proportion catch in projections
for (group in duplicated_rows){
  
  col_names<-paste0("sel(B):_",group)
  SSInput$CatchbyFleet[,paste0("sum_",paste(group,collapse="_"))]<-rowSums(SSInput$CatchbyFleet[,col_names,drop=FALSE],na.rm=TRUE)
  col_names2<-paste0("F:_",group)
  SSInput$FbyFleet[,paste0("sum_",paste(group,collapse="_"))]<-rowSums(SSInput$FbyFleet[,col_names2,drop=FALSE],na.rm=TRUE)
}
CatchbyFleet<-SSInput$CatchbyFleet %>%
  select(-starts_with("sel(B):_"))
names(CatchbyFleet)<-c("Yr",UniqueFleets) 


## calculate the proportion of catch by year and fleet from the combined catch by fleet
ProportionCatch<-CatchbyFleet
for( i in 1:length(Years)) {
  
  TempCatch<-CatchbyFleet[i,-1]
  ProportionCatch[i,c(2:ncol(ProportionCatch))]<-TempCatch/sum(TempCatch) 
}

## Repeat exercise to combine fleet fishing mortalities across years and mirrored selectivity
FbyFleet<-SSInput$FbyFleet %>%
  select(-starts_with("F:_"))
names(FbyFleet)<-c("Yr",UniqueFleets) 

##From here you can average catch, average F, or average proportion of catch over the years you are interested in
## sticking with the three year average used above:

CatchbyFleet<- CatchbyFleet %>%
  filter(Yr %in% YearAvg) %>%
  summarize(across(c(2:ncol(CatchbyFleet)),\(x) mean(x,na.rm=TRUE)))

ProportionCatch<- ProportionCatch %>%
  filter(Yr %in% YearAvg) %>%
  summarize(across(c(2:ncol(ProportionCatch)),\(x) mean(x,na.rm=TRUE)))

FbyFleet<- FbyFleet %>%
  filter(Yr %in% YearAvg) %>%
  summarize(across(c(2:ncol(FbyFleet)),\(x) mean(x,na.rm=TRUE)))

         
## Build Recruitment object as an input list
## Each recruitment model from model 1 to 21 has a specific Recruitment object list. 
## The structure of the Recruitment object list for each recruitment model is detailed below:

## Set Recruitment object to be a list
Recruitment <- list()

## Set Recr_Model type (one of 1:21 recruit model choices, except 9 which is deprecated)
Recruitment$Recr_Model <- RecruitType[1]

## Set single recruitment model probability by time period to be 1
Recruitment$Recr_Prob <- rep(1,NYears)

## RecFac, SSBFac, and MaxRecObs in Recruitment object
Recruitment$RecFac <- RecFac
Recruitment$SSBFac <- SSBFac
Recruitment$MaxRecObs <- MaxRecObs

if (RecruitType[1] == 1)
{
## Model 1 - Not implemented yet
}  else if (RecruitType[1] == 2)
{
## Model 2. Empirical Recruits Per Spawning Biomass Distribution
#Recruitment is a list with two objects: Nobs with is the number of observations, recruits is a dataframe with two columns, recruitment and SSB
Recruitment$SSBFac <- 1 ## Set SSBFac=1 for recruits per spawner models 2 and 4
Recruitment$Nobs <- 28  ## Set Nobs to be the number of (R,SSB) observations in Recruitment
firstyr <- 1993 ## Set firstyr to be the first year for the (R,SSB) data
lastyr <- 2020 ## Set lastyr to be the last year for the (R,SSB) data
# Select (R,SSB) data from SSInput object
Recruitment$Recruits <- SSInput$RecruitmentObs %>% 
  filter(Yr %in% firstyr:lastyr) %>%
  select(pred_recr,SpawnBio)
# Save age-0 recruitment (thousands of age-0 fish on July 1st in endyr) for checking input
Recruitment_Age0_July_1st <- Recruitment$Recruits[,1]
# pply survival rate ot calculate age-0.5 recruitment on January 1st for annual projections (thousands of age-0.5 fish on January 1st in endyr+1)
Recruitment$Recruits[,1] <- exp(-0.5*M0)*Recruitment$Recruits[,1]
} else if (RecruitType[1] == 3 || RecruitType[1] == 14)
{
## Models 3 and 14
# Recruitment is a list with two objects: Nobs is the number of observations, and recruits is a vector of observed recruitment
Recruitment$Nobs <- 20  ## Set Nobs to be the number of (R) observations in Recruitment
firstyr <- 2001 ## Set firstyr to be the first year for the (R) data
lastyr <- 2020 ## Set lastyr to be the last year for the (R) data
# Select (R,SSB) data from SSInput object
Recruitment$Recruits <- SSInput$RecruitmentObs %>% 
  filter(Yr %in% firstyr:lastyr) %>%
  select(pred_recr)
## Save age-0 recruitment (thousands of age-0 fish on July 1st in endyr) for checking input
Recruitment_Age0_July_1st <- Recruitment$Recruits
## Calculate age-0.5 recruitment on January 1st for annual projections (thousands of age-0.5 fish on January 1st in endyr+1)
Recruitment$Recruits <- exp(-0.5*M0)*Recruitment$Recruits
} else
{
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

## Model 16, 17, 18, and 19
# Recruitment is an object with 5 objects: Ncoeff is the number of coefficients, var is the variance, Intercept is the y-intercept, coeff is a vector containing the value of each coefficient, and Observations is a dataframe where each row the observations for each coefficient and each column is a year of the projection

## Model 20
# Recruitment is a vector of the fixed recruitment for each year.

## Model 21
# Recruitment is a list containing 3 objects: Nobs is the number of observations, Obs is a vector of observed recruitments, and SSBHingeValue is the value at which the linear decline to zero begins.
}

## Harvest is the harvest strategy you want to use: first column is the harvest specification, then one column for each fleet for each year. Landings (catch) = 1, F-Mult (F) = 0, Removals = ?

## Here is an example assuming constant catch over the number of years of the model where catch is partitianed based upon the relative catch by fleet in the last year of the assessment, this is calculated in SSInput$CatchbyFleet
TotalCatch <- 2000

FleetRemovals <- t(sapply(SSInput$CatchbyFleet, function(p) rep(p*TotalCatch,NYears)))

Harvest <- list("Type"=c(rep(1,NYears)),"Harvest"=FleetRemovals)

source(file.path(script.dir,"AGEPRO_Input.R"))
##source("AGEPRO_Input.R")

## Now write the input file:

AGEPRO_INP(output.dir = "C:\\Users\\jon.brodziak\\Desktop\\2024 WCNPO MLS Rebuilding\\Build-Input-File",
                    boot_file = boot_file,
                    NBoot = NBoot,
                    BootFac = BootFac,
                    SS_Out = SSInput,
                    ModelName="test_model_generic",
                    ProjStart = 2021,
                    NYears = NYears,
                    MinAge = 1,
                    NSims = NSims,
                    NRecr_models = NRecModel,
                    Discards = 0,
                    set.seed = 123,
                    ScaleFactor = c(1000,1000,1000),  #scalebio, scalerec, scalestk
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