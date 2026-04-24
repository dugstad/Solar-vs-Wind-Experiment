library(tidyr)
library(apollo)
library(dplyr)
library(tidyverse)
library(car)

##Install packages if not installed##

### Clear memory
rm(list = ls())
gc()

# ------------------ LOAD & PREPARE RESPONDENT DATA ----------------------
setwd("D:/EnergyWise/Replication package")

# Load respondent data
df <- read_csv("vind_complete.csv") %>%
  mutate(id = row_number())  # Add unique respondent ID

# Pivot from wide to long (one row per choice situation)
df_long <- df %>%
  pivot_longer(cols = starts_with("Q17_"),
               names_to = "choice_set",
               names_prefix = "Q17_",
               values_to = "chosen_alt") %>%
  mutate(
    chosen_alt = case_when(
      chosen_alt == "Utbygging" ~ 1,
      chosen_alt == "Ingen utbygging" ~ 2,
      TRUE ~ NA_real_  # everything else becomes NA
    ),
    choice_set = as.integer(choice_set)
  ) %>%
  filter(!is.na(chosen_alt))  # now safe to filter

# ------------------ LOAD & RESHAPE DCE DESIGN ----------------------

# Load experimental design
design <- read_csv("CE_design_final.csv") %>%
  mutate(alt = as.integer(alt))

# Pivot design to wide format (alt1 and alt2 as columns)
design_wide <- design %>%
  pivot_wider(
    id_cols = choice_set,
    names_from = alt,
    names_sep = "_alt",
    values_from = c(el, plants, wild, dist, view, cost)
  )

# ------------------ MERGE DESIGN WITH CHOICES ----------------------

# Merge respondent data with design
df_apollo_wind <- df_long %>%
  left_join(design_wide, by = "choice_set") %>%
  rename(choice = chosen_alt) %>%
  arrange(id, choice_set)

df_apollo_wind$cost_alt2[is.na(df_apollo_wind$cost_alt2)] <- 0

# ------------------ LOAD & PREPARE RESPONDENT DATA ----------------------

# Load respondent data
df <- read_csv("sol_complete.csv") %>%
  mutate(id = row_number())  # Add unique respondent ID

# Pivot from wide to long (one row per choice situation)
df_long <- df %>%
  pivot_longer(cols = starts_with("Q17_"),
               names_to = "choice_set",
               names_prefix = "Q17_",
               values_to = "chosen_alt") %>%
  mutate(
    chosen_alt = case_when(
      chosen_alt == "Utbygging" ~ 1,
      chosen_alt == "Ingen utbygging" ~ 2,
      TRUE ~ NA_real_  # everything else becomes NA
    ),
    choice_set = as.integer(choice_set)
  ) %>%
  filter(!is.na(chosen_alt))  # now safe to filter

# ------------------ LOAD & RESHAPE DCE DESIGN ----------------------

# Load experimental design
design <- read_csv("CE_design_final.csv") %>%
  mutate(alt = as.integer(alt))

# Pivot design to wide format (alt1 and alt2 as columns)
design_wide <- design %>%
  pivot_wider(
    id_cols = choice_set,
    names_from = alt,
    names_sep = "_alt",
    values_from = c(el, plants, wild, dist, view, cost)
  )

# ------------------ MERGE DESIGN WITH CHOICES ----------------------

# Merge respondent data with design
df_apollo_solar <- df_long %>%
  left_join(design_wide, by = "choice_set") %>%
  rename(choice = chosen_alt) %>%
  arrange(id, choice_set)

df_apollo_solar$cost_alt2[is.na(df_apollo_solar$cost_alt2)] <- 0

# After df_apollo for SOLAR is created
df_apollo_solar <- df_apollo_solar %>%
  mutate(
    solar_dummy = 1,
    wind_dummy = 0
  )

# Get max ID from SOLAR
max_solar_id <- max(df_apollo_solar$id)

# After df_apollo for WIND is created
df_apollo_wind <- df_apollo_wind %>%
  mutate(
    id = id + max_solar_id,  # offset ID
    solar_dummy = 0,
    wind_dummy = 1
  )

# Combine into pooled dataset
df_apollo_pooled <- bind_rows(df_apollo_solar, df_apollo_wind)

write.csv(df_apollo_pooled, "apollo_wind_solar_pooled.csv")

### Clear memory
rm(list = ls())
gc()


### load data
database = read.csv("apollo_wind_solar_pooled.csv",header=TRUE)

#####################
# Model 1 - Pooled1 #
#####################

#These are starting vaulues derived from estiamting a MXL without correlation in preference space.
#Starting values for model in WTP-space is derived from calculating WTA estimates from these
#Starting values.
#Estimated parameters with approximate standard errors from BHHH matrix:
#  Estimate     BHHH se BHH t-ratio (0)
#mu_alt2           1.77876    0.215419          8.2572
#mu_el38          -0.03200    0.103707         -0.3085
#mu_el57          -0.11592    0.100123         -1.1578
#mu_plants3       -0.12621    0.103927         -1.2144
#mu_plants6       -0.30665    0.102188         -3.0009
#mu_wild10        -1.46080    0.130999        -11.1512
#mu_wild20        -0.43639    0.099062         -4.4053
#mu_dist1         -1.35739    0.131865        -10.2938
#mu_dist2         -0.95401    0.114428         -8.3372
#mu_dist3         -0.39858    0.116274         -3.4279
#mu_view2         -0.81449    0.097455         -8.3576
#sigma_alt2        5.25670    0.257028         20.4518
#sigma_el38        0.37042    0.354617          1.0446
#sigma_el57       -0.56651    0.250005         -2.2660
#sigma_plants3    -0.67766    0.239284         -2.8320
#sigma_plants6    -0.40718    0.328705         -1.2387
#sigma_wild10      1.81194    0.178182         10.1691
#sigma_wild20     -0.46667    0.324289         -1.4390
#sigma_dist1      -1.14399    0.198506         -5.7630
#sigma_dist2       0.24047    0.593552          0.4051
#sigma_dist3      -0.31451    0.531906         -0.5913
#sigma_view2       1.16377    0.153401          7.5864
#b_cost            0.02259    0.001800         12.5507
#
#Final LL: -4685.0277


apollo_initialise()

apollo_control <- list(
  modelName       = "POOLED1",
  modelDescr      = "",
  indivID         = "id",
  outputDirectory = "output",
  mixing    = TRUE, 
  nCores    = 8
)

# Parameters to estimate
apollo_beta <- c(
  mu_alt2              =          	78.741,
  mu_el38              =          	-1.417,
  mu_el57              =          	-5.131,
  mu_plants3           =          	-5.587,
  mu_plants6           =          	-13.575,
  mu_wild10            =          	-64.666,
  mu_wild20            =          	-19.318,
  mu_dist1             =          	-60.088,
  mu_dist2             =          	-42.232,
  mu_dist3             =          	-17.644,
  mu_view2             =          	-36.055,
  mu_cost              =          	   -3.8,
  sigma_alt2           =          	232.700,
  sigma_el38           =          	16.398,
  sigma_el57           =          	-25.078,
  sigma_plants3        =          	-29.998,
  sigma_plants6        =          	-18.025,
  sigma_wild10         =          	80.210,
  sigma_wild20         =          	-20.658,
  sigma_dist1          =          	-50.641,
  sigma_dist2          =          	10.645,
  sigma_dist3          =          	-13.923,
  sigma_view2          =          	51.517,
  sigma_cost           =               0,
  sigma_el38_el57            =          0,
  sigma_plants3_plants6      =          0,
  sigma_wild10_wild20        =          0,
  sigma_dist1_dist2          =          0,
  sigma_dist1_dist3          =          0,
  sigma_dist2_dist3          =          0
)

# Parameters to fix (none for now)
apollo_fixed <- c()

# ################################################################# #
#### DEFINE RANDOM COMPONENTS                                    ####
# ################################################################# #

### Set parameters for generating draws
apollo_draws = list(
  interDrawsType = "mlhs",
  interNDraws = 2000,
  interNormDraws = c(
    "xi_alt2", "xi_el38", "xi_el57", "xi_plants3", "xi_plants6",
    "xi_wild10", "xi_wild20", "xi_dist1", "xi_dist2", "xi_dist3", "xi_view2", "xi_cost"
  )
)

apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["b_alt2"]]    = mu_alt2 + sigma_alt2 * xi_alt2
  randcoeff[["b_el38"]]    = mu_el38 + sigma_el38 * xi_el38
  randcoeff[["b_el57"]]    = mu_el57 + sigma_el57 * xi_el57 + sigma_el38_el57 * xi_el38
  randcoeff[["b_plants3"]] = mu_plants3 + sigma_plants3 * xi_plants3
  randcoeff[["b_plants6"]] = mu_plants6 + sigma_plants6 * xi_plants6 + sigma_plants3_plants6 * xi_plants3
  randcoeff[["b_wild10"]]  = mu_wild10 + sigma_wild10 * xi_wild10
  randcoeff[["b_wild20"]]  = mu_wild20 + sigma_wild20 * xi_wild20 + sigma_wild10_wild20 * xi_wild10
  randcoeff[["b_dist1"]]   = mu_dist1 + sigma_dist1 * xi_dist1
  randcoeff[["b_dist2"]]   = mu_dist2 + sigma_dist2 * xi_dist2 + sigma_dist1_dist2 * xi_dist1
  randcoeff[["b_dist3"]]   = mu_dist3 + sigma_dist3 * xi_dist3  + sigma_dist1_dist3 * xi_dist1  + sigma_dist2_dist3 * xi_dist2
  randcoeff[["b_view2"]]   = mu_view2 + sigma_view2 * xi_view2
  randcoeff[["b_cost"]]   = exp(mu_cost + sigma_cost * xi_cost)
  
  return(randcoeff)
}


# Validate
apollo_inputs <- apollo_validateInputs()

# ------------------ PROBABILITY FUNCTION ----------------------

apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate") {
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P <- list()
  
  V <- list()
  V[["alt1"]] = b_cost*(b_el38*(el_alt1==38) + b_el57*(el_alt1==57) + b_plants3 * (plants_alt1==3) + b_plants6 * (plants_alt1==6) + b_wild10*(wild_alt1==0) + b_wild20*(wild_alt1==1) + b_view2 * (view_alt1==2) + b_dist1*(dist_alt1==1) + b_dist2*(dist_alt1==2) + b_dist3*(dist_alt1==3) + cost_alt1/100)
  V[["alt2"]] = b_cost*b_alt2 + b_cost*(cost_alt2)
  
  mnl_settings <- list(
    alternatives = c(alt1 = 1, alt2 = 2),
    avail        = list(alt1 = 1, alt2 = 1),
    choiceVar    = choice,
    utilities    = V
  )
  
  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Average across inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #


model = apollo_estimate(apollo_beta, apollo_fixed,
                        apollo_probabilities, apollo_inputs, estimate_settings=list(maxIterations=1000, estimationRoutine="bfgs"))

apollo_modelOutput(model, modelOutput_settings=list(printPVal="TRUE"))


apollo_saveOutput(model)

### Clear memory
rm(list = ls())
gc()

### load data
database = read.csv("apollo_wind_solar_pooled.csv",header=TRUE)

#####################
# Model 1 - Pooled2 #
#####################

#These are starting vaulues derived from estiamting a MXL without correlation in preference space.
#Starting values for model in WTP-space is derived from calculating WTA estimates from these
#Starting values.
#Estimated parameters with approximate standard errors from BHHH matrix:
#  Estimate     BHHH se BHH t-ratio (0)
#mu_alt2           1.54834    0.195299          7.9280
#mu_el38          -0.01290    0.090256         -0.1429
#mu_el57          -0.07892    0.088549         -0.8912
#mu_plants3       -0.11095    0.091459         -1.2131
#mu_plants6       -0.25926    0.090754         -2.8567
#mu_wild10        -1.27008    0.120198        -10.5666
#mu_wild20        -0.37434    0.087107         -4.2975
#mu_dist1         -1.22059    0.121427        -10.0520
#mu_dist2         -0.87055    0.103336         -8.4244
#mu_dist3         -0.36717    0.102935         -3.5670
#mu_view2         -0.69862    0.088409         -7.9022
#b_cost            0.01967    0.001695         11.6069
#lambda_wind       1.32492    0.106505         12.4399
#sigma_alt2        4.60193    0.262741         17.5151
#sigma_el38        0.35386    0.284856          1.2422
#sigma_el57       -0.54000    0.193005         -2.7979
#sigma_plants3    -0.55463    0.223524         -2.4813
#sigma_plants6    -0.40467    0.264406         -1.5305
#sigma_wild10      1.56600    0.163225          9.5941
#sigma_wild20     -0.36135    0.308932         -1.1697
#sigma_dist1      -1.01704    0.177876         -5.7177
#sigma_dist2       0.16172    0.583867          0.2770
#sigma_dist3      -0.18065    0.611999         -0.2952
#sigma_view2       0.99119    0.137745          7.1958

#Final LL: -4679.3427


apollo_initialise()

apollo_control <- list(
  modelName       = "POOLED2",
  modelDescr      = "",
  indivID         = "id",
  outputDirectory = "output",
  mixing    = TRUE, 
  nCores    = 8
)

# Parameters to estimate
apollo_beta <- c(
  mu_alt2              =      	78.716,
  mu_el38              =      	-0.656,
  mu_el57              =      	-4.012,
  mu_plants3           =      	-5.641,
  mu_plants6           =      	-13.180,
  mu_wild10            =      	-64.569,
  mu_wild20            =      	-19.031,
  mu_dist1             =      	-62.053,
  mu_dist2             =      	-44.258,
  mu_dist3             =      	-18.666,
  mu_view2             =      	-35.517,
  mu_cost              =      	   -3.9,
  lambda_wind          =      	1.325,
  sigma_alt2           =      	233.957,
  sigma_el38           =      	17.990,
  sigma_el57           =      	-27.453,
  sigma_plants3        =      	-28.197,
  sigma_plants6        =      	-20.573,
  sigma_wild10         =      	79.614,
  sigma_wild20         =      	-18.371,
  sigma_dist1          =      	-51.705,
  sigma_dist2          =      	8.222,
  sigma_dist3          =      	-9.184,
  sigma_view2          =      	50.391,
  sigma_cost           =               0,
  sigma_el38_el57            =          0,
  sigma_plants3_plants6      =          0,
  sigma_wild10_wild20        =          0,
  sigma_dist1_dist2          =          0,
  sigma_dist1_dist3          =          0,
  sigma_dist2_dist3          =          0
)

# Parameters to fix (none for now)
apollo_fixed <- c()

# ################################################################# #
#### DEFINE RANDOM COMPONENTS                                    ####
# ################################################################# #

### Set parameters for generating draws
apollo_draws = list(
  interDrawsType = "mlhs",
  interNDraws = 2000,
  interNormDraws = c(
    "xi_alt2", "xi_el38", "xi_el57", "xi_plants3", "xi_plants6",
    "xi_wild10", "xi_wild20", "xi_dist1", "xi_dist2", "xi_dist3", "xi_view2", "xi_cost"
  )
)

apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["b_alt2"]]    = mu_alt2 + sigma_alt2 * xi_alt2
  randcoeff[["b_el38"]]    = mu_el38 + sigma_el38 * xi_el38
  randcoeff[["b_el57"]]    = mu_el57 + sigma_el57 * xi_el57 + sigma_el38_el57 * xi_el38
  randcoeff[["b_plants3"]] = mu_plants3 + sigma_plants3 * xi_plants3
  randcoeff[["b_plants6"]] = mu_plants6 + sigma_plants6 * xi_plants6 + sigma_plants3_plants6 * xi_plants3
  randcoeff[["b_wild10"]]  = mu_wild10 + sigma_wild10 * xi_wild10
  randcoeff[["b_wild20"]]  = mu_wild20 + sigma_wild20 * xi_wild20 + sigma_wild10_wild20 * xi_wild10
  randcoeff[["b_dist1"]]   = mu_dist1 + sigma_dist1 * xi_dist1
  randcoeff[["b_dist2"]]   = mu_dist2 + sigma_dist2 * xi_dist2 + sigma_dist1_dist2 * xi_dist1
  randcoeff[["b_dist3"]]   = mu_dist3 + sigma_dist3 * xi_dist3  + sigma_dist1_dist3 * xi_dist1  + sigma_dist2_dist3 * xi_dist2
  randcoeff[["b_view2"]]   = mu_view2 + sigma_view2 * xi_view2
  randcoeff[["b_cost"]]   = exp(mu_cost + sigma_cost * xi_cost)
  
  return(randcoeff)
}


# Validate
apollo_inputs <- apollo_validateInputs()

# ------------------ PROBABILITY FUNCTION ----------------------

apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate") {
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P <- list()
  
  V <- list()
  V[["alt1"]] = ((solar_dummy + lambda_wind*wind_dummy)/1)*(b_cost*(b_el38*(el_alt1==38) + b_el57*(el_alt1==57) + b_plants3 * (plants_alt1==3) + b_plants6 * (plants_alt1==6) + b_wild10*(wild_alt1==0) + b_wild20*(wild_alt1==1) + b_view2 * (view_alt1==2) + b_dist1*(dist_alt1==1) + b_dist2*(dist_alt1==2) + b_dist3*(dist_alt1==3) + cost_alt1/100))
  V[["alt2"]] = ((solar_dummy + lambda_wind*wind_dummy)/1)*(b_cost*b_alt2 + b_cost*(cost_alt2))
  
  mnl_settings <- list(
    alternatives = c(alt1 = 1, alt2 = 2),
    avail        = list(alt1 = 1, alt2 = 1),
    choiceVar    = choice,
    utilities    = V
  )
  
  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Average across inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #


model = apollo_estimate(apollo_beta, apollo_fixed,
                        apollo_probabilities, apollo_inputs, estimate_settings=list(maxIterations=1000, estimationRoutine="bfgs"))

apollo_modelOutput(model, modelOutput_settings=list(printPVal="TRUE"))


apollo_saveOutput(model)


#CI for relative scale parameter
delmeth1 <- deltaMethod(model$estimate[ model$estimate != 0 ], "lambda_wind", vcov. = model$robvarcov)
delmeth1

### Clear memory
rm(list = ls())
gc()

### load data
database = read.csv("apollo_wind_solar_pooled.csv",header=TRUE)

database = subset(database,database$wind_dummy==1)

##########
# WIND 1 #
##########

#These are starting vaulues derived from estiamting a MXL without correlation in preference space.
#Starting values for model in WTP-space is derived from calculating WTA estimates from these
#Starting values.
#Estimated parameters with approximate standard errors from BHHH matrix:
#  Estimate     BHHH se BHH t-ratio (0)
#mu_alt2           3.17716    0.374697        8.479280
#mu_el38           5.45656    7.060013        0.772883
#mu_el57           6.74076    7.063091        0.954364
#mu_plants3       -7.25486    7.052118       -1.028749
#mu_plants6       -8.04170    7.208904       -1.115523
#mu_wild10       -59.50056    8.681326       -6.853857
#mu_wild20       -18.27027    6.504485       -2.808874
#mu_dist1        -85.19776   12.293607       -6.930249
#mu_dist2        -60.43543    9.749640       -6.198735
#mu_dist3        -25.54237    9.257032       -2.759240
#mu_view2        -24.90517    6.052249       -4.115027
#sigma_alt2        5.87004    0.431485       13.604270
#sigma_el38       29.48217   15.961959        1.847027
#sigma_el57       25.07213   16.464130        1.522834
#sigma_plants3   -13.42901   29.835300       -0.450105
#sigma_plants6    35.67084   15.005479        2.377188
#sigma_wild10    -70.27232   12.053804       -5.829888
#sigma_wild20      0.11197   53.457155        0.002095
#sigma_dist1      63.52203   14.262743        4.453704
#sigma_dist2       0.45310   74.007834        0.006122
#sigma_dist3     -10.36152   42.661879       -0.242875
#sigma_view2      31.95256   12.274241        2.603221
#b_cost            0.02250    0.002742        8.207293
#
#Final LL: -2083.1724

apollo_initialise()

apollo_control <- list(
  modelName       = "WIND1",
  modelDescr      = "",
  indivID         = "id",
  outputDirectory = "output",
  mixing    = TRUE, 
  nCores    = 8
)


# Parameters to estimate
apollo_beta <- c(
  mu_alt2                    =  141.207,
  mu_el38                    =    5.457,
  mu_el57                    =    6.741,
  mu_plants3                 =   -7.255,
  mu_plants6                 =   -8.042,
  mu_wild10                  =  -59.501,
  mu_wild20                  =  -18.270,
  mu_dist1                   =  -85.198,
  mu_dist2                   =  -60.435,
  mu_dist3                   =  -25.542,
  mu_view2                   =  -24.905,
  sigma_alt2                 =    260.891,
  sigma_el38                 =   29.482,
  sigma_el57                 =   25.072,
  sigma_plants3              =  -13.429,
  sigma_plants6              =   35.671,
  sigma_wild10               =  -70.272,
  sigma_wild20               =    0.112,
  sigma_dist1                =   63.522,
  sigma_dist2                =    0.453,
  sigma_dist3                =  -10.362,
  sigma_view2                =   31.953,
  mu_cost                    =         -4,
  sigma_cost                 =          0,
  sigma_el38_el57            =          0,
  sigma_plants3_plants6      =          0,
  sigma_wild10_wild20        =          0,
  sigma_dist1_dist2          =          0,
  sigma_dist1_dist3          =          0,
  sigma_dist2_dist3          =          0
)

# Parameters to fix (none for now)
apollo_fixed <- c()

# ################################################################# #
#### DEFINE RANDOM COMPONENTS                                    ####
# ################################################################# #

### Set parameters for generating draws
apollo_draws = list(
  interDrawsType = "mlhs",
  interNDraws = 2000,
  interNormDraws = c(
    "xi_alt2", "xi_el38", "xi_el57", "xi_plants3", "xi_plants6",
    "xi_wild10", "xi_wild20", "xi_dist1", "xi_dist2", "xi_dist3", "xi_view2", "xi_cost"
  )
)

apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["b_alt2"]]    = mu_alt2 + sigma_alt2 * xi_alt2
  randcoeff[["b_el38"]]    = mu_el38 + sigma_el38 * xi_el38
  randcoeff[["b_el57"]]    = mu_el57 + sigma_el57 * xi_el57 + sigma_el38_el57 * xi_el38
  randcoeff[["b_plants3"]] = mu_plants3 + sigma_plants3 * xi_plants3
  randcoeff[["b_plants6"]] = mu_plants6 + sigma_plants6 * xi_plants6 + sigma_plants3_plants6 * xi_plants3
  randcoeff[["b_wild10"]]  = mu_wild10 + sigma_wild10 * xi_wild10
  randcoeff[["b_wild20"]]  = mu_wild20 + sigma_wild20 * xi_wild20 + sigma_wild10_wild20 * xi_wild10
  randcoeff[["b_dist1"]]   = mu_dist1 + sigma_dist1 * xi_dist1
  randcoeff[["b_dist2"]]   = mu_dist2 + sigma_dist2 * xi_dist2 + sigma_dist1_dist2 * xi_dist1
  randcoeff[["b_dist3"]]   = mu_dist3 + sigma_dist3 * xi_dist3  + sigma_dist1_dist3 * xi_dist1  + sigma_dist2_dist3 * xi_dist2
  randcoeff[["b_view2"]]   = mu_view2 + sigma_view2 * xi_view2
  randcoeff[["b_cost"]]   = exp(mu_cost + sigma_cost * xi_cost)
  return(randcoeff)
}


# Validate
apollo_inputs <- apollo_validateInputs()

# ------------------ PROBABILITY FUNCTION ----------------------

apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate") {
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P <- list()
  
  V <- list()
  V[["alt1"]] = b_cost*(b_el38*(el_alt1==38) + b_el57*(el_alt1==57) + b_plants3 * (plants_alt1==3) + b_plants6 * (plants_alt1==6) + b_wild10*(wild_alt1==0) + b_wild20*(wild_alt1==1) + b_view2 * (view_alt1==2) + b_dist1*(dist_alt1==1) + b_dist2*(dist_alt1==2) + b_dist3*(dist_alt1==3) + cost_alt1/100)
  V[["alt2"]] = b_cost*b_alt2 + b_cost*(cost_alt2)
  
  mnl_settings <- list(
    alternatives = c(alt1 = 1, alt2 = 2),
    avail        = list(alt1 = 1, alt2 = 1),
    choiceVar    = choice,
    utilities    = V
  )
  
  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Average across inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #


model = apollo_estimate(apollo_beta, apollo_fixed,
                        apollo_probabilities, apollo_inputs, estimate_settings=list(maxIterations=1000, estimationRoutine="bfgs"))


apollo_modelOutput(model, modelOutput_settings=list(printPVal="TRUE"))

apollo_saveOutput(model)

mu2 <- model$estimate

sigma2 <- model$robvarcov

mu2 <- mu2[ mu2 != 0 ]


simulate_cov_norobust <- MASS::mvrnorm(10000, mu = mu2, Sigma = sigma2)


write.csv(simulate_cov_norobust, "simulate_cov_WIND.csv")

### Clear memory
rm(list = ls())
gc()

### load data
database = read.csv("apollo_wind_solar_pooled.csv",header=TRUE)

database = subset(database,database$solar_dummy==1)


###########
# SOLAR 1 #
###########

#These are starting vaulues derived from estiamting a MXL without correlation in preference space.
#Starting values for model in WTP-space is derived from calculating WTA estimates from these
#Starting values.
### We end up with the following solution that will be used as starting parameters
#mu_alt2           0.73458    0.267835         2.74267
#mu_el38          -7.07878    6.481801        -1.09210
#mu_el57         -14.65987    6.273231        -2.33689
#mu_plants3       -5.20515    6.134133        -0.84856
#mu_plants6      -20.66561    6.451386        -3.20328
#mu_wild10       -71.07586    9.372536        -7.58342
#mu_wild20       -21.26609    6.329964        -3.35959
#mu_dist1        -39.79839    7.959778        -4.99994
#mu_dist2        -30.50595    7.379942        -4.13363
#mu_dist3        -12.31509    7.194474        -1.71174
#mu_view2        -45.32091    7.068481        -6.41169
#sigma_alt2        4.43951    0.263622        16.84047
#sigma_el38        2.30232   72.768753         0.03164
#sigma_el57        4.60426   53.925986         0.08538
#sigma_plants3    21.39674   18.632921         1.14833
#sigma_plants6     6.92501   39.941347         0.17338
#sigma_wild10     84.22993   12.703187         6.63061
#sigma_wild20    -37.13246   13.591015        -2.73213
#sigma_dist1     -14.31462   28.298780        -0.50584
#sigma_dist2      -2.01674   83.675842        -0.02410
#sigma_dist3       8.37236   47.603839         0.17588
#sigma_view2      63.97959   10.194257         6.27604
#b_cost            0.02097    0.002255         9.29592
#
#Final LL: -2559.1854

apollo_initialise()

apollo_control <- list(
  modelName       = "SOLAR1",
  modelDescr      = "",
  indivID         = "id",
  outputDirectory = "output",
  mixing    = TRUE, 
  nCores    = 8
)

# Parameters to estimate
apollo_beta <- c(
  mu_alt2                    =      35.05,
  mu_el38                    =     -7.079,
  mu_el57                    =    -14.660,
  mu_plants3                 =     -5.205,
  mu_plants6                 =    -20.666,
  mu_wild10                  =    -71.076,
  mu_wild20                  =    -21.266,
  mu_dist1                   =    -39.798,
  mu_dist2                   =    -30.506,
  mu_dist3                   =    -12.315,
  mu_view2                   =    -45.321,
  sigma_alt2                 =     211.73,
  sigma_el38                 =      2.302,
  sigma_el57                 =      4.604,
  sigma_plants3              =     21.397,
  sigma_plants6              =      6.925,
  sigma_wild10               =     84.230,
  sigma_wild20               =    -37.133,
  sigma_dist1                =    -14.315,
  sigma_dist2                =     -2.017,
  sigma_dist3                =      8.372,
  sigma_view2                =     63.980,
  mu_cost                    =         -4,
  sigma_cost                 =          0,
  sigma_el38_el57            =          0,
  sigma_plants3_plants6      =          0,
  sigma_wild10_wild20        =          0,
  sigma_dist1_dist2          =          0,
  sigma_dist1_dist3          =          0,
  sigma_dist2_dist3          =          0
)

# Parameters to fix (none for now)
apollo_fixed <- c()

# ################################################################# #
#### DEFINE RANDOM COMPONENTS                                    ####
# ################################################################# #

### Set parameters for generating draws
apollo_draws = list(
  interDrawsType = "mlhs",
  interNDraws = 2000,
  interNormDraws = c(
    "xi_alt2", "xi_el38", "xi_el57", "xi_plants3", "xi_plants6",
    "xi_wild10", "xi_wild20", "xi_dist1", "xi_dist2", "xi_dist3", "xi_view2", "xi_cost"
  )
)

apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["b_alt2"]]    = mu_alt2 + sigma_alt2 * xi_alt2
  randcoeff[["b_el38"]]    = mu_el38 + sigma_el38 * xi_el38
  randcoeff[["b_el57"]]    = mu_el57 + sigma_el57 * xi_el57 + sigma_el38_el57 * xi_el38
  randcoeff[["b_plants3"]] = mu_plants3 + sigma_plants3 * xi_plants3
  randcoeff[["b_plants6"]] = mu_plants6 + sigma_plants6 * xi_plants6 + sigma_plants3_plants6 * xi_plants3
  randcoeff[["b_wild10"]]  = mu_wild10 + sigma_wild10 * xi_wild10
  randcoeff[["b_wild20"]]  = mu_wild20 + sigma_wild20 * xi_wild20 + sigma_wild10_wild20 * xi_wild10
  randcoeff[["b_dist1"]]   = mu_dist1 + sigma_dist1 * xi_dist1
  randcoeff[["b_dist2"]]   = mu_dist2 + sigma_dist2 * xi_dist2 + sigma_dist1_dist2 * xi_dist1
  randcoeff[["b_dist3"]]   = mu_dist3 + sigma_dist3 * xi_dist3  + sigma_dist1_dist3 * xi_dist1  + sigma_dist2_dist3 * xi_dist2
  randcoeff[["b_view2"]]   = mu_view2 + sigma_view2 * xi_view2
  randcoeff[["b_cost"]]   = exp(mu_cost + sigma_cost * xi_cost)
  return(randcoeff)
}


# Validate
apollo_inputs <- apollo_validateInputs()

# ------------------ PROBABILITY FUNCTION ----------------------

apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate") {
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P <- list()
  
  V <- list()
  V[["alt1"]] = b_cost*(b_el38*(el_alt1==38) + b_el57*(el_alt1==57) + b_plants3 * (plants_alt1==3) + b_plants6 * (plants_alt1==6) + b_wild10*(wild_alt1==0) + b_wild20*(wild_alt1==1) + b_view2 * (view_alt1==2) + b_dist1*(dist_alt1==1) + b_dist2*(dist_alt1==2) + b_dist3*(dist_alt1==3) + cost_alt1/100)
  V[["alt2"]] = b_cost*b_alt2 + b_cost*(cost_alt2)
  
  mnl_settings <- list(
    alternatives = c(alt1 = 1, alt2 = 2),
    avail        = list(alt1 = 1, alt2 = 1),
    choiceVar    = choice,
    utilities    = V
  )
  
  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Average across inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #


model = apollo_estimate(apollo_beta, apollo_fixed,
                        apollo_probabilities, apollo_inputs, estimate_settings=list(maxIterations=1000, estimationRoutine="bfgs"))


apollo_modelOutput(model, modelOutput_settings=list(printPVal="TRUE"))



apollo_saveOutput(model)

mu2 <- model$estimate

sigma2 <- model$robvarcov

mu2 <- mu2[ mu2 != 0 ]


#View(mu)

simulate_cov_norobust <- MASS::mvrnorm(10000, mu = mu2, Sigma = sigma2)


write.csv(simulate_cov_norobust, "simulate_cov_SUN.csv")

### Clear memory
rm(list = ls())
gc()

#############
# Table A.4 #
#############

library(mded)

simulate_cov_WIND = read.csv("simulate_cov_WIND.csv",header=TRUE)
simulate_cov_SUN = read.csv("simulate_cov_SUN.csv",header=TRUE)

### SQ ###
wtp_sq_wind <- simulate_cov_WIND$mu_alt2*100
wtp_sq_SUN <- simulate_cov_SUN$mu_alt2*100

out <- mded(distr1 = wtp_sq_wind, distr2 = wtp_sq_SUN, detail = TRUE)
out

### EL38 ###
wtp_el38_wind <- simulate_cov_WIND$mu_el38*100
wtp_el38_SUN <- simulate_cov_SUN$mu_el38*100

out <- mded(distr1 = wtp_el38_wind, distr2 = wtp_el38_SUN, detail = TRUE)
out

### EL57 ###
wtp_el57_wind <- simulate_cov_WIND$mu_el57*100
wtp_el57_SUN <- simulate_cov_SUN$mu_el57*100

out <- mded(distr1 = wtp_el57_wind, distr2 = wtp_el57_SUN, detail = TRUE)
out

### PLANTS3 ###
wtp_plants3_wind <- simulate_cov_WIND$mu_plants3*100
wtp_plants3_SUN <- simulate_cov_SUN$mu_plants3*100

out <- mded(distr1 = wtp_plants3_wind, distr2 = wtp_plants3_SUN, detail = TRUE)
out


### PLANTS6 ###
wtp_plants6_wind <- simulate_cov_WIND$mu_plants6*100
wtp_plants6_SUN <- simulate_cov_SUN$mu_plants6*100

out <- mded(distr1 = wtp_plants6_wind, distr2 = wtp_plants6_SUN, detail = TRUE)
out


### WILD10 ###
wtp_wild10_wind <- simulate_cov_WIND$mu_wild10*100
wtp_wild10_SUN <- simulate_cov_SUN$mu_wild10*100

out <- mded(distr1 = wtp_wild10_wind, distr2 = wtp_wild10_SUN, detail = TRUE)
out


### WILD20 ###
wtp_wild20_wind <- simulate_cov_WIND$mu_wild20*100
wtp_wild20_SUN <- simulate_cov_SUN$mu_wild20*100

out <- mded(distr1 = wtp_wild20_wind, distr2 = wtp_wild20_SUN, detail = TRUE)
out


### DIST1 ###
wtp_dist1_wind <- simulate_cov_WIND$mu_dist1*100
wtp_dist1_SUN <- simulate_cov_SUN$mu_dist1*100

out <- mded(distr1 = wtp_dist1_wind, distr2 = wtp_dist1_SUN, detail = TRUE)
out

### DIST2 ###
wtp_dist2_wind <- simulate_cov_WIND$mu_dist2*100
wtp_dist2_SUN <- simulate_cov_SUN$mu_dist2*100

out <- mded(distr1 = wtp_dist2_wind, distr2 = wtp_dist2_SUN, detail = TRUE)
out

### DIST3 ###
wtp_dist3_wind <- simulate_cov_WIND$mu_dist3*100
wtp_dist3_SUN <- simulate_cov_SUN$mu_dist3*100

out <- mded(distr1 = wtp_dist3_wind, distr2 = wtp_dist3_SUN, detail = TRUE)
out

### VIEW ###
wtp_view2_wind <- simulate_cov_WIND$mu_view2*100
wtp_view2_SUN <- simulate_cov_SUN$mu_view2*100

out <- mded(distr1 = wtp_view2_wind, distr2 = wtp_view2_SUN, detail = TRUE)
out

### Clear memory
rm(list = ls())
gc()

### load data
database = read.csv("apollo_wind_solar_pooled.csv",header=TRUE)

database = subset(database,database$wind_dummy==1)

##########
# WIND 3 #
##########

#These are starting vaulues derived from estiamting a LC-MXL without correlation in preference space.
#Starting values for model in WTP-space is derived from calculating WTA estimates from these
#Starting values.
#Estimated parameters with approximate standard errors from BHHH matrix:
#  Estimate     BHHH se BHH t-ratio (0)
#mu_alt2_a         0.39552    0.280698        1.409050
#mu_el38_a         0.21304    0.187818        1.134277
#mu_el57_a         0.22107    0.200914        1.100332
#mu_plants3_a     -0.11928    0.192508       -0.619613
#mu_plants6_a     -0.12483    0.194635       -0.641340
#mu_wild10_a      -1.73526    0.326719       -5.311177
#mu_wild20_a      -0.46127    0.180576       -2.554464
#mu_dist1_a       -2.38992    0.375580       -6.363286
#mu_dist2_a       -1.49502    0.252229       -5.927227
#mu_dist3_a       -0.64405    0.226336       -2.845539
#mu_view2_a       -0.66582    0.185490       -3.589530
#mu_alt2_b        18.88614    9.632728        1.960623
#b_cost_a          0.02797    0.004361        6.414343
#b_el38_b          0.00000          NA              NA
#b_el57_b          0.00000          NA              NA
#b_plants3_b       0.00000          NA              NA
#b_plants6_b       0.00000          NA              NA
#b_wild10_b        0.00000          NA              NA
#b_wild20_b        0.00000          NA              NA
#b_dist1_b         0.00000          NA              NA
#b_dist2_b         0.00000          NA              NA
#b_dist3_b         0.00000          NA              NA
#b_view2_b         0.00000          NA              NA
#b_cost_b          0.00000          NA              NA
#sigma_alt2_a      1.35029    0.257485        5.244139
#sigma_alt2_b     25.28896   12.091723        2.091427
#sigma_el38       -0.42076    0.652638       -0.644707
#sigma_el57        0.77044    0.381744        2.018221
#sigma_plants3    -0.29835    0.792247       -0.376585
#sigma_plants6    -0.79976    0.432891       -1.847482
#sigma_wild10     -2.09279    0.414337       -5.050931
#sigma_wild20     -0.06327    2.185932       -0.028945
#sigma_dist1       1.47624    0.477559        3.091223
#sigma_dist2      -0.01950    2.838864       -0.006870
#sigma_dist3       0.02517    2.767565        0.009094
#sigma_view2       0.94504    0.300878        3.140957
#delta_b           0.63661    0.153780        4.139769
#
#Final LL: -2056.7294
#
#
#Summary of class allocation for model component :
#  Mean prob.
#Class_1      0.3460
#Class_2      0.6540


### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName       = "WIND3",
  modelDescr      = "",
  indivID         = "id",
  mixing          = TRUE, 
  nCores          =    8,
  mixing          = TRUE)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

database <- df_apollo

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters
apollo_beta = c(
  mu_alt2_a               =             	14.141,
  mu_el38_a               =             	7.617,
  mu_el57_a               =             	7.904,
  mu_plants3_a            =             	-4.265,
  mu_plants6_a            =             	-4.463,
  mu_wild10_a             =             	-62.040,
  mu_wild20_a             =             	-16.492,
  mu_dist1_a              =             	-85.446,
  mu_dist2_a              =             	-53.451,
  mu_dist3_a              =             	-23.026,
  mu_view2_a              =             	-23.805,
  mu_alt2_b               =             	18.886,
  mu_cost_a               =             	 -3.6,
  b_el38_b                =             	0.000,
  b_el57_b                =             	0.000,
  b_plants3_b             =             	0.000,
  b_plants6_b             =             	0.000,
  b_wild10_b              =             	0.000,
  b_wild20_b              =             	0.000,
  b_dist1_b               =             	0.000,
  b_dist2_b               =             	0.000,
  b_dist3_b               =             	0.000,
  b_view2_b               =             	0.000,
  b_cost_b                =             	0.000,
  sigma_alt2_a            =             	48.276,
  sigma_alt2_b            =             	25.289,
  sigma_el38              =             	-15.043,
  sigma_el57              =             	27.545,
  sigma_plants3           =             	-10.667,
  sigma_plants6           =             	-28.593,
  sigma_wild10            =             	-74.823,
  sigma_wild20            =             	-2.262,
  sigma_dist1             =             	52.779,
  sigma_dist2             =             	-0.697,
  sigma_dist3             =             	0.900,
  sigma_view2             =             	33.788,
  sigma_el38_el57            =          0,
  sigma_plants3_plants6      =          0,
  sigma_wild10_wild20        =          0,
  sigma_dist1_dist2          =          0,
  sigma_dist1_dist3          =          0,
  sigma_dist2_dist3          =          0,
  sigma_cost              =                   0,
  delta_b                 =             	0.637)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("b_el38_b","b_el57_b","b_plants3_b","b_plants6_b","b_wild10_b","b_wild20_b","b_cost_b","b_dist1_b","b_dist2_b","b_dist3_b","b_view2_b")


### Set parameters for generating draws
apollo_draws = list(
  interDrawsType = "mlhs",
  interNDraws = 2000,
  interNormDraws = c(
    "xi_alt2_a","xi_alt2_b", "xi_el38", "xi_el57", "xi_plants3", "xi_plants6",
    "xi_wild10", "xi_wild20", "xi_dist1", "xi_dist2", "xi_dist3", "xi_view2","xi_cost"
  )
)

apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["b_alt2_a"]]    = mu_alt2_a + sigma_alt2_a * xi_alt2_a
  randcoeff[["b_alt2_b"]]    = mu_alt2_b + sigma_alt2_b * xi_alt2_b
  randcoeff[["b_el38_a"]]    = mu_el38_a + sigma_el38 * xi_el38
  randcoeff[["b_el57_a"]]    = mu_el57_a + sigma_el57 * xi_el57 + sigma_el38_el57 * xi_el38
  randcoeff[["b_plants3_a"]] = mu_plants3_a + sigma_plants3 * xi_plants3
  randcoeff[["b_plants6_a"]] = mu_plants6_a + sigma_plants6 * xi_plants6 + sigma_plants3_plants6 * xi_plants3
  randcoeff[["b_wild10_a"]]  = mu_wild10_a + sigma_wild10 * xi_wild10
  randcoeff[["b_wild20_a"]]  = mu_wild20_a + sigma_wild20 * xi_wild20 + sigma_wild10_wild20 * xi_wild10
  randcoeff[["b_dist1_a"]]   = mu_dist1_a + sigma_dist1 * xi_dist1
  randcoeff[["b_dist2_a"]]   = mu_dist2_a + sigma_dist2 * xi_dist2 + sigma_dist1_dist2 * xi_dist1
  randcoeff[["b_dist3_a"]]   = mu_dist3_a + sigma_dist3 * xi_dist3  + sigma_dist1_dist3 * xi_dist1  + sigma_dist2_dist3 * xi_dist2
  randcoeff[["b_view2_a"]]   = mu_view2_a + sigma_view2 * xi_view2
  randcoeff[["b_cost_a"]]   = exp(mu_cost_a + sigma_cost * xi_cost)
  
  return(randcoeff)
}

#' Returns unconditionals for a latent class model model
#'
#' Returns values for random parameters and class allocation probabilities in a latent class model model.
#'
#' @param model Model object. Estimated model object as returned by function \link{apollo_estimate}.
#' @param apollo_probabilities Function. Returns probabilities of the model to be estimated. Must receive three arguments:
#'                          \itemize{
#'                            \item \strong{\code{apollo_beta}}: Named numeric vector. Names and values of model parameters.
#'                            \item \strong{\code{apollo_inputs}}: List containing options of the model. See \link{apollo_validateInputs}.
#'                            \item \strong{\code{functionality}}: Character. Can be either \strong{\code{"components"}}, \strong{\code{"conditionals"}}, \strong{\code{"estimate"}} (default), \strong{\code{"gradient"}}, \strong{\code{"output"}}, \strong{\code{"prediction"}}, \strong{\code{"preprocess"}}, \strong{\code{"raw"}}, \strong{\code{"report"}}, \strong{\code{"shares_LL"}}, \strong{\code{"validate"}} or \strong{\code{"zero_LL"}}.
#'                          }
#' @param apollo_inputs List grouping most common inputs. Created by function \link{apollo_validateInputs}.
#' @return List of object, one per random component and one for the class allocation probabilities.
#' @export
apollo_lcUnconditionals <- function(model, apollo_probabilities, apollo_inputs){
  if(!is.function(apollo_inputs$apollo_lcPars)) stop("SYNTAX ISSUE - This function is for latent class models. For other models use \"apollo_unconditionals\".")
  if(is.null(apollo_inputs$silent)) silent = FALSE else silent = apollo_inputs$silent
  apollo_beta  = model$estimate
  apollo_fixed = model$apollo_fixed
  
  #if(!silent) apollo_print("Updating inputs...")
  #apollo_inputs <- apollo_validateInputs(silent=TRUE, recycle=TRUE)
  ### Warn the user in case elements in apollo_inputs are different from those in the global environment
  apollo_compareInputs(apollo_inputs)
  
  apollo_control   = apollo_inputs[["apollo_control"]]
  database         = apollo_inputs[["database"]]
  draws            = apollo_inputs[["draws"]]
  apollo_randCoeff = apollo_inputs[["apollo_randCoeff"]]
  apollo_lcPars    = apollo_inputs[["apollo_lcPars"]]
  apollo_draws     = apollo_inputs[["apollo_draws"]]
  apollo_checkArguments(apollo_probabilities,apollo_randCoeff,apollo_lcPars)
  
  if(is.null(apollo_control$HB)) apollo_control$HB=FALSE
  if(apollo_control$HB) stop("INCORRECT FUNCTION/SETTING USE - The function \'apollo_lcUnconditionals\' is not applicables for models estimated using HB!") 
  
  # Calculate randCoeff if necessary
  toAttach  <- c(as.list(apollo_beta), apollo_inputs$database)
  if(apollo_control$mixing){
    toAttach  <- c(as.list(apollo_beta), apollo_inputs$database, draws)
    randcoeff = with(toAttach, {
      environment(apollo_randCoeff) <- environment()
      apollo_randCoeff(apollo_beta, apollo_inputs)
    } )
    if(apollo_draws$intraNDraws>0) cat("Your model contains intra-individual draws, so the output will contain draws across individuals and across choices.\n")
    if(apollo_draws$intraNDraws==0) cat("Your model contains only inter-individual draws, so the output will contain draws across individuals only.\n")
    toAttach <- c(toAttach, randcoeff)
  }
  
  # Calculate lcPars
  unconditionals = with(toAttach, {
    environment(apollo_lcPars) <- environment()
    apollo_lcPars(apollo_beta, apollo_inputs)
  } )
  
  # If there are no intraDraws, keep only first row of each individual
  nObs <- nrow(database)
  if (any(!is.na(apollo_draws))) for(i in 1:length(unconditionals)){
    if(is.list(unconditionals[[i]])){
      for(j in 1:length(unconditionals[[i]])){
        x <- unconditionals[[i]][[j]]
        if(is.array(x)) nRows <- dim(x)[1] else nRows <- length(x)
        if(nRows==nObs) unconditionals[[i]][[j]] <- apollo_firstRow(x, apollo_inputs)
      }
    } else {
      x <- unconditionals[[i]]
      if(is.array(x)) nRows <- dim(x)[1] else nRows <- length(x)
      if(nRows==nObs) unconditionals[[i]] <- apollo_firstRow(x, apollo_inputs)
    }
  }
  
  if(!silent) apollo_print("Unconditional distributions computed")
  return(unconditionals)
}


# ################################################################# #
#### DEFINE LATENT CLASS COMPONENTS                              ####
# ################################################################# #

apollo_lcPars=function(apollo_beta, apollo_inputs){
  lcpars = list()
  
  lcpars[["b_alt2"  ]] = list(  b_alt2_a, b_alt2_b)
  lcpars[["b_el38"  ]] = list(  b_el38_a, b_el38_b)
  lcpars[["b_el57"  ]] = list(  b_el57_a, b_el57_b)
  lcpars[["b_plants3"  ]] = list(  b_plants3_a,   b_plants3_b)
  lcpars[["b_plants6"  ]] = list(  b_plants6_a,   b_plants6_b)
  lcpars[["b_wild10"  ]] = list(  b_wild10_a,   b_wild10_b)
  lcpars[["b_wild20"  ]] = list(  b_wild20_a,   b_wild20_b)
  lcpars[["b_dist1"  ]] = list(  b_dist1_a,   b_dist1_b)
  lcpars[["b_dist2"  ]] = list(  b_dist2_a,   b_dist2_b)
  lcpars[["b_dist3"  ]] = list(  b_dist3_a,   b_dist3_b)
  lcpars[["b_view2"  ]] = list(  b_view2_a,   b_view2_b)
  lcpars[["b_cost"  ]] = list(  b_cost_a,   b_cost_b)
  
  
  V=list()
  V[["class_a"]] = 0
  V[["class_b"]] = delta_b
  
  
  ### Settings for class allocation models
  classAlloc_settings = list(
    classes      = c(class_a=1, class_b=2), 
    utilities    = V  
  )
  
  lcpars[["pi_values"]] = apollo_classAlloc(classAlloc_settings)
  
  
  return(lcpars)
}

# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### Define settings for MNL model component that are generic across classes
  mnl_settings = list(
    alternatives  = c(alt1=1, alt2=2),
    avail         = list(alt1=1, alt2=1),
    choiceVar     = choice,
    componentName = "choice"
  )
  
  ### Loop over classes
  for(s in 1:2){
    
    V = list()
    
    if(s==1){
      V[["alt1"]] = b_cost_a*(b_el38_a*(el_alt1==38) + b_el57_a*(el_alt1==57) + b_plants3_a * (plants_alt1==3) + b_plants6_a * (plants_alt1==6) + b_wild10_a*(wild_alt1==0) + b_wild20_a*(wild_alt1==1) + b_view2_a * (view_alt1==2) + b_dist1_a*(dist_alt1==1) + b_dist2_a*(dist_alt1==2) + b_dist3_a*(dist_alt1==3) + cost_alt1/100)
      V[["alt2"]] = b_cost_a*b_alt2_a + b_cost_a*(cost_alt2)
    }
    
    if(s==2){
      V[["alt1"]] = (b_el38_b*(el_alt1==38) + b_el57_b*(el_alt1==57) + b_plants3_b * (plants_alt1==3) + b_plants6_b * (plants_alt1==6) + b_wild10_b*(wild_alt1==0) + b_wild20_b*(wild_alt1==1) + b_view2_b * (view_alt1==2) + b_dist1_b*(dist_alt1==1) + b_dist2_b*(dist_alt1==2) + b_dist3_b*(dist_alt1==3) + b_cost_b*cost_alt1/100)
      V[["alt2"]] = b_alt2_b + b_cost_b*(cost_alt2)
    }
    
    
    mnl_settings$utilities     = V
    mnl_settings$componentName = paste0("Class_",s)
    
    ### Compute within-class choice probabilities using MNL model
    P[[paste0("Class_",s)]] = apollo_mnl(mnl_settings, functionality)
    
    ### Take product across observation for same individual
    P[[paste0("Class_",s)]] = apollo_panelProd(P[[paste0("Class_",s)]], apollo_inputs ,functionality)
    
    if(s == 2){
      P[[paste0("Class_",s)]] = apollo_avgInterDraws(P[[paste0("Class_",s)]], apollo_inputs, functionality)
    }
  }
  
  
  ### Compute latent class model probabilities
  lc_settings  = list(inClassProb = P, classProb=pi_values)
  P[["model"]] = apollo_lc(lc_settings, apollo_inputs, functionality)
  
  ### Average across inter-individual draws in class allocation probabilities
  P[["model"]] = apollo_avgInterDraws(P[["model"]], apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}


# ################################################################# #
#### EM ESTIMATION FOR COVARIANCE MATRIX                         ####
# ################################################################# #

model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(model, modelOutput_settings=list(printPVal="TRUE"))


apollo_saveOutput(model)

mu2 <- model$estimate

sigma2 <- model$robvarcov

mu2 <- mu2[ mu2 != 0 ]


#View(mu)

simulate_cov_norobust <- MASS::mvrnorm(10000, mu = mu2, Sigma = sigma2)


write.csv(simulate_cov_norobust, "simulate_cov_WIND3.csv")


### Clear memory
rm(list = ls())
gc()

### load data
database = read.csv("apollo_wind_solar_pooled.csv",header=TRUE)

database = subset(database,database$solar_dummy==1)

###########
# SOLAR 3 #
###########

#These are starting vaulues derived from estiamting a LC-MXL without correlation in preference space.
#Starting values for model in WTP-space is derived from calculating WTA estimates from these
#Starting values.
#Estimated parameters with approximate standard errors from BHHH matrix:
#  Estimate     BHHH se BHH t-ratio (0)
#mu_alt2_a        0.029848    0.262705        0.113618
#mu_el38_a       -0.163599    0.171052       -0.956429
#mu_el57_a       -0.333603    0.162693       -2.050507
#mu_plants3_a     0.086653    0.177986        0.486851
#mu_plants6_a    -0.351224    0.171681       -2.045793
#mu_wild10_a     -2.085089    0.305419       -6.826985
#mu_wild20_a     -0.560260    0.165043       -3.394636
#mu_dist1_a      -1.079599    0.214178       -5.040675
#mu_dist2_a      -0.760579    0.193185       -3.937057
#mu_dist3_a      -0.183540    0.190932       -0.961286
#mu_view2_a      -1.217931    0.204970       -5.941982
#mu_alt2_b        7.138743    2.904844        2.457531
#b_cost_a         0.029825    0.004183        7.130612
#b_el38_b         0.000000          NA              NA
#b_el57_b         0.000000          NA              NA
#b_plants3_b      0.000000          NA              NA
#b_plants6_b      0.000000          NA              NA
#b_wild10_b       0.000000          NA              NA
#b_wild20_b       0.000000          NA              NA
#b_dist1_b        0.000000          NA              NA
#b_dist2_b        0.000000          NA              NA
#b_dist3_b        0.000000          NA              NA
#b_view2_b        0.000000          NA              NA
#b_cost_b         0.000000          NA              NA
#sigma_alt2_a     1.123347    0.234457        4.791279
#sigma_alt2_b   -17.891489    6.247100       -2.863967
#sigma_el38      -0.066054    1.943448       -0.033988
#sigma_el57      -0.387591    0.571436       -0.678276
#sigma_plants3    0.795634    0.356093        2.234344
#sigma_plants6    0.004603    2.755147        0.001671
#sigma_wild10     2.332667    0.379575        6.145464
#sigma_wild20     0.912504    0.316499        2.883119
#sigma_dist1      0.641410    0.491048        1.306207
#sigma_dist2      0.363413    0.751199        0.483776
#sigma_dist3      0.807995    0.463485        1.743304
#sigma_view2      1.608886    0.261018        6.163885
#delta_b          0.198038    0.142810        1.386720
#
#Final LL: -2520.0428
#
#
#Summary of class allocation for model component :
#  Mean prob.
#Class_1      0.4507
#Class_2      0.5493

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName       = "SOLAR3",
  modelDescr      = "",
  indivID         = "id",
  # mixing          = TRUE, 
  nCores          =    8,
  mixing          = TRUE)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

database <- df_apollo

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters
apollo_beta = c(
  
  mu_alt2_a                  =  	1.001,
  mu_el38_a                  =  	-5.485,
  mu_el57_a                  =  	-11.185,
  mu_plants3_a               =  	2.905,
  mu_plants6_a               =  	-11.776,
  mu_wild10_a                =  	-69.911,
  mu_wild20_a                =  	-18.785,
  mu_dist1_a                 =  	-36.198,
  mu_dist2_a                 =  	-25.501,
  mu_dist3_a                 =  	-6.154,
  mu_view2_a                 =  	-40.836,
  mu_alt2_b                  =  	7.139,
  mu_cost_a                  =  	-3.5,
  b_el38_b                   =  	0.000,
  b_el57_b                   =  	0.000,
  b_plants3_b                =  	0.000,
  b_plants6_b                =  	0.000,
  b_wild10_b                 =  	0.000,
  b_wild20_b                 =  	0.000,
  b_dist1_b                  =  	0.000,
  b_dist2_b                  =  	0.000,
  b_dist3_b                  =  	0.000,
  b_view2_b                  =  	0.000,
  b_cost_b                   =  	0.000,
  sigma_alt2_a               =  	1.123,
  sigma_alt2_b               =  	-17.891,
  sigma_el38                 =  	-2.215,
  sigma_el57                 =  	-12.996,
  sigma_plants3              =  	26.677,
  sigma_plants6              =  	0.154,
  sigma_wild10               =  	78.212,
  sigma_wild20               =  	30.595,
  sigma_dist1                =  	21.506,
  sigma_dist2                =  	12.185,
  sigma_dist3                =  	27.091,
  sigma_view2                =  	53.944,
  sigma_el38_el57            =          0,
  sigma_plants3_plants6      =          0,
  sigma_wild10_wild20        =          0,
  sigma_dist1_dist2          =          0,
  sigma_dist1_dist3          =          0,
  sigma_dist2_dist3          =          0,
  sigma_cost                 =          0,
  delta_b                    =     	0.198)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("b_el38_b","b_el57_b","b_plants3_b","b_plants6_b","b_wild10_b","b_wild20_b","b_cost_b","b_dist1_b","b_dist2_b","b_dist3_b","b_view2_b")


### Set parameters for generating draws
apollo_draws = list(
  interDrawsType = "mlhs",
  interNDraws = 2000,
  interNormDraws = c(
    "xi_alt2_a","xi_alt2_b", "xi_el38", "xi_el57", "xi_plants3", "xi_plants6",
    "xi_wild10", "xi_wild20", "xi_dist1", "xi_dist2", "xi_dist3", "xi_view2","xi_cost"
  )
)

apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["b_alt2_a"]]    = mu_alt2_a + sigma_alt2_a * xi_alt2_a
  randcoeff[["b_alt2_b"]]    = mu_alt2_b + sigma_alt2_b * xi_alt2_b
  randcoeff[["b_el38_a"]]    = mu_el38_a + sigma_el38 * xi_el38
  randcoeff[["b_el57_a"]]    = mu_el57_a + sigma_el57 * xi_el57 + sigma_el38_el57 * xi_el38
  randcoeff[["b_plants3_a"]] = mu_plants3_a + sigma_plants3 * xi_plants3
  randcoeff[["b_plants6_a"]] = mu_plants6_a + sigma_plants6 * xi_plants6 + sigma_plants3_plants6 * xi_plants3
  randcoeff[["b_wild10_a"]]  = mu_wild10_a + sigma_wild10 * xi_wild10
  randcoeff[["b_wild20_a"]]  = mu_wild20_a + sigma_wild20 * xi_wild20 + sigma_wild10_wild20 * xi_wild10
  randcoeff[["b_dist1_a"]]   = mu_dist1_a + sigma_dist1 * xi_dist1
  randcoeff[["b_dist2_a"]]   = mu_dist2_a + sigma_dist2 * xi_dist2 + sigma_dist1_dist2 * xi_dist1
  randcoeff[["b_dist3_a"]]   = mu_dist3_a + sigma_dist3 * xi_dist3  + sigma_dist1_dist3 * xi_dist1  + sigma_dist2_dist3 * xi_dist2
  randcoeff[["b_view2_a"]]   = mu_view2_a + sigma_view2 * xi_view2
  randcoeff[["b_cost_a"]]   = exp(mu_cost_a + sigma_cost * xi_cost)
  
  return(randcoeff)
}

#' Returns unconditionals for a latent class model model
#'
#' Returns values for random parameters and class allocation probabilities in a latent class model model.
#'
#' @param model Model object. Estimated model object as returned by function \link{apollo_estimate}.
#' @param apollo_probabilities Function. Returns probabilities of the model to be estimated. Must receive three arguments:
#'                          \itemize{
#'                            \item \strong{\code{apollo_beta}}: Named numeric vector. Names and values of model parameters.
#'                            \item \strong{\code{apollo_inputs}}: List containing options of the model. See \link{apollo_validateInputs}.
#'                            \item \strong{\code{functionality}}: Character. Can be either \strong{\code{"components"}}, \strong{\code{"conditionals"}}, \strong{\code{"estimate"}} (default), \strong{\code{"gradient"}}, \strong{\code{"output"}}, \strong{\code{"prediction"}}, \strong{\code{"preprocess"}}, \strong{\code{"raw"}}, \strong{\code{"report"}}, \strong{\code{"shares_LL"}}, \strong{\code{"validate"}} or \strong{\code{"zero_LL"}}.
#'                          }
#' @param apollo_inputs List grouping most common inputs. Created by function \link{apollo_validateInputs}.
#' @return List of object, one per random component and one for the class allocation probabilities.
#' @export
apollo_lcUnconditionals <- function(model, apollo_probabilities, apollo_inputs){
  if(!is.function(apollo_inputs$apollo_lcPars)) stop("SYNTAX ISSUE - This function is for latent class models. For other models use \"apollo_unconditionals\".")
  if(is.null(apollo_inputs$silent)) silent = FALSE else silent = apollo_inputs$silent
  apollo_beta  = model$estimate
  apollo_fixed = model$apollo_fixed
  
  #if(!silent) apollo_print("Updating inputs...")
  #apollo_inputs <- apollo_validateInputs(silent=TRUE, recycle=TRUE)
  ### Warn the user in case elements in apollo_inputs are different from those in the global environment
  apollo_compareInputs(apollo_inputs)
  
  apollo_control   = apollo_inputs[["apollo_control"]]
  database         = apollo_inputs[["database"]]
  draws            = apollo_inputs[["draws"]]
  apollo_randCoeff = apollo_inputs[["apollo_randCoeff"]]
  apollo_lcPars    = apollo_inputs[["apollo_lcPars"]]
  apollo_draws     = apollo_inputs[["apollo_draws"]]
  apollo_checkArguments(apollo_probabilities,apollo_randCoeff,apollo_lcPars)
  
  if(is.null(apollo_control$HB)) apollo_control$HB=FALSE
  if(apollo_control$HB) stop("INCORRECT FUNCTION/SETTING USE - The function \'apollo_lcUnconditionals\' is not applicables for models estimated using HB!") 
  
  # Calculate randCoeff if necessary
  toAttach  <- c(as.list(apollo_beta), apollo_inputs$database)
  if(apollo_control$mixing){
    toAttach  <- c(as.list(apollo_beta), apollo_inputs$database, draws)
    randcoeff = with(toAttach, {
      environment(apollo_randCoeff) <- environment()
      apollo_randCoeff(apollo_beta, apollo_inputs)
    } )
    if(apollo_draws$intraNDraws>0) cat("Your model contains intra-individual draws, so the output will contain draws across individuals and across choices.\n")
    if(apollo_draws$intraNDraws==0) cat("Your model contains only inter-individual draws, so the output will contain draws across individuals only.\n")
    toAttach <- c(toAttach, randcoeff)
  }
  
  # Calculate lcPars
  unconditionals = with(toAttach, {
    environment(apollo_lcPars) <- environment()
    apollo_lcPars(apollo_beta, apollo_inputs)
  } )
  
  # If there are no intraDraws, keep only first row of each individual
  nObs <- nrow(database)
  if (any(!is.na(apollo_draws))) for(i in 1:length(unconditionals)){
    if(is.list(unconditionals[[i]])){
      for(j in 1:length(unconditionals[[i]])){
        x <- unconditionals[[i]][[j]]
        if(is.array(x)) nRows <- dim(x)[1] else nRows <- length(x)
        if(nRows==nObs) unconditionals[[i]][[j]] <- apollo_firstRow(x, apollo_inputs)
      }
    } else {
      x <- unconditionals[[i]]
      if(is.array(x)) nRows <- dim(x)[1] else nRows <- length(x)
      if(nRows==nObs) unconditionals[[i]] <- apollo_firstRow(x, apollo_inputs)
    }
  }
  
  if(!silent) apollo_print("Unconditional distributions computed")
  return(unconditionals)
}


# ################################################################# #
#### DEFINE LATENT CLASS COMPONENTS                              ####
# ################################################################# #

apollo_lcPars=function(apollo_beta, apollo_inputs){
  lcpars = list()
  
  lcpars[["b_alt2"  ]] = list(  b_alt2_a, b_alt2_b)
  lcpars[["b_el38"  ]] = list(  b_el38_a, b_el38_b)
  lcpars[["b_el57"  ]] = list(  b_el57_a, b_el57_b)
  lcpars[["b_plants3"  ]] = list(  b_plants3_a,   b_plants3_b)
  lcpars[["b_plants6"  ]] = list(  b_plants6_a,   b_plants6_b)
  lcpars[["b_wild10"  ]] = list(  b_wild10_a,   b_wild10_b)
  lcpars[["b_wild20"  ]] = list(  b_wild20_a,   b_wild20_b)
  lcpars[["b_dist1"  ]] = list(  b_dist1_a,   b_dist1_b)
  lcpars[["b_dist2"  ]] = list(  b_dist2_a,   b_dist2_b)
  lcpars[["b_dist3"  ]] = list(  b_dist3_a,   b_dist3_b)
  lcpars[["b_view2"  ]] = list(  b_view2_a,   b_view2_b)
  lcpars[["b_cost"  ]] = list(  b_cost_a,   b_cost_b)
  
  
  V=list()
  V[["class_a"]] = 0
  V[["class_b"]] = delta_b
  
  
  ### Settings for class allocation models
  classAlloc_settings = list(
    classes      = c(class_a=1, class_b=2), 
    utilities    = V  
  )
  
  lcpars[["pi_values"]] = apollo_classAlloc(classAlloc_settings)
  
  
  return(lcpars)
}

# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### Define settings for MNL model component that are generic across classes
  mnl_settings = list(
    alternatives  = c(alt1=1, alt2=2),
    avail         = list(alt1=1, alt2=1),
    choiceVar     = choice,
    componentName = "choice"
  )
  
  ### Loop over classes
  for(s in 1:2){
    
    V = list()
    
    if(s==1){
      V[["alt1"]] = b_cost_a*(b_el38_a*(el_alt1==38) + b_el57_a*(el_alt1==57) + b_plants3_a * (plants_alt1==3) + b_plants6_a * (plants_alt1==6) + b_wild10_a*(wild_alt1==0) + b_wild20_a*(wild_alt1==1) + b_view2_a * (view_alt1==2) + b_dist1_a*(dist_alt1==1) + b_dist2_a*(dist_alt1==2) + b_dist3_a*(dist_alt1==3) + cost_alt1/100)
      V[["alt2"]] = b_cost_a*b_alt2_a + b_cost_a*(cost_alt2)
    }
    
    if(s==2){
      V[["alt1"]] = (b_el38_b*(el_alt1==38) + b_el57_b*(el_alt1==57) + b_plants3_b * (plants_alt1==3) + b_plants6_b * (plants_alt1==6) + b_wild10_b*(wild_alt1==0) + b_wild20_b*(wild_alt1==1) + b_view2_b * (view_alt1==2) + b_dist1_b*(dist_alt1==1) + b_dist2_b*(dist_alt1==2) + b_dist3_b*(dist_alt1==3) + b_cost_b*cost_alt1/100)
      V[["alt2"]] = b_alt2_b + b_cost_b*(cost_alt2)
    }
    
    
    mnl_settings$utilities     = V
    mnl_settings$componentName = paste0("Class_",s)
    
    ### Compute within-class choice probabilities using MNL model
    P[[paste0("Class_",s)]] = apollo_mnl(mnl_settings, functionality)
    
    ### Take product across observation for same individual
    P[[paste0("Class_",s)]] = apollo_panelProd(P[[paste0("Class_",s)]], apollo_inputs ,functionality)
    
    if(s == 2){
      P[[paste0("Class_",s)]] = apollo_avgInterDraws(P[[paste0("Class_",s)]], apollo_inputs, functionality)
    }
  }
  
  
  ### Compute latent class model probabilities
  lc_settings  = list(inClassProb = P, classProb=pi_values)
  P[["model"]] = apollo_lc(lc_settings, apollo_inputs, functionality)
  
  ### Average across inter-individual draws in class allocation probabilities
  P[["model"]] = apollo_avgInterDraws(P[["model"]], apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}


# ################################################################# #
#### EM ESTIMATION FOR COVARIANCE MATRIX                         ####
# ################################################################# #

model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(model, modelOutput_settings=list(printPVal="TRUE"))


apollo_saveOutput(model)

mu2 <- model$estimate

sigma2 <- model$robvarcov

mu2 <- mu2[ mu2 != 0 ]


#View(mu)

simulate_cov_norobust <- MASS::mvrnorm(10000, mu = mu2, Sigma = sigma2)


write.csv(simulate_cov_norobust, "simulate_cov_SOLAR3.csv")


### Clear memory
rm(list = ls())
gc()

#############
# Table A.4 #
#############

library(mded)

simulate_cov_WIND = read.csv("simulate_cov_WIND4.csv",header=TRUE)
simulate_cov_SUN = read.csv("simulate_cov_SUN4.csv",header=TRUE)

### SQ ###
wtp_sq_wind <- simulate_cov_WIND$mu_alt2_a*100
wtp_sq_SUN <- simulate_cov_SUN$mu_alt2_a*100

out <- mded(distr1 = wtp_sq_wind, distr2 = wtp_sq_SUN, detail = TRUE)
out

### EL38 ###
wtp_el38_wind <- simulate_cov_WIND$mu_el38_a*100
wtp_el38_SUN <- simulate_cov_SUN$mu_el38_a*100

out <- mded(distr1 = wtp_el38_wind, distr2 = wtp_el38_SUN, detail = TRUE)
out

### EL57 ###
wtp_el57_wind <- simulate_cov_WIND$mu_el57_a*100
wtp_el57_SUN <- simulate_cov_SUN$mu_el57_a*100

out <- mded(distr1 = wtp_el57_wind, distr2 = wtp_el57_SUN, detail = TRUE)
out

### PLANTS3 ###
wtp_plants3_wind <- simulate_cov_WIND$mu_plants3_a*100
wtp_plants3_SUN <- simulate_cov_SUN$mu_plants3_a*100

out <- mded(distr1 = wtp_plants3_wind, distr2 = wtp_plants3_SUN, detail = TRUE)
out


### PLANTS6 ###
wtp_plants6_wind <- simulate_cov_WIND$mu_plants6_a*100
wtp_plants6_SUN <- simulate_cov_SUN$mu_plants6_a*100

out <- mded(distr1 = wtp_plants6_wind, distr2 = wtp_plants6_SUN, detail = TRUE)
out


### WILD10 ###
wtp_wild10_wind <- simulate_cov_WIND$mu_wild10_a*100
wtp_wild10_SUN <- simulate_cov_SUN$mu_wild10_a*100

out <- mded(distr1 = wtp_wild10_wind, distr2 = wtp_wild10_SUN, detail = TRUE)
out


### WILD20 ###
wtp_wild20_wind <- simulate_cov_WIND$mu_wild20_a*100
wtp_wild20_SUN <- simulate_cov_SUN$mu_wild20_a*100

out <- mded(distr1 = wtp_wild20_wind, distr2 = wtp_wild20_SUN, detail = TRUE)
out


### DIST1 ###
wtp_dist1_wind <- simulate_cov_WIND$mu_dist1_a*100
wtp_dist1_SUN <- simulate_cov_SUN$mu_dist1_a*100

out <- mded(distr1 = wtp_dist1_wind, distr2 = wtp_dist1_SUN, detail = TRUE)
out

### DIST2 ###
wtp_dist2_wind <- simulate_cov_WIND$mu_dist2_a*100
wtp_dist2_SUN <- simulate_cov_SUN$mu_dist2_a*100

out <- mded(distr1 = wtp_dist2_wind, distr2 = wtp_dist2_SUN, detail = TRUE)
out

### DIST3 ###
wtp_dist3_wind <- simulate_cov_WIND$mu_dist3_a*100
wtp_dist3_SUN <- simulate_cov_SUN$mu_dist3_a*100

out <- mded(distr1 = wtp_dist3_wind, distr2 = wtp_dist3_SUN, detail = TRUE)
out

### VIEW ###
wtp_view2_wind <- simulate_cov_WIND$mu_view2_a*100
wtp_view2_SUN <- simulate_cov_SUN$mu_view2_a*100

out <- mded(distr1 = wtp_view2_wind, distr2 = wtp_view2_SUN, detail = TRUE)
out


### Clear memory
rm(list = ls())
gc()

### load data
database = read.csv("apollo_wind_solar_pooled.csv",header=TRUE)

database = subset(database,database$wind_dummy==1)

#########
# WIND4 #
#########

protest_levels <- c(
  "Å forsyne Europe med fornybar energi fra Norge skal ikke gå på bekostning av norsk natur",
  "Andre årsaker, vennligst spesifiser:",
  "Det vil ikke bidra tilstrekkelig i kampen mot klimaendringer",
  "Jeg synes ikke Norge skal være et grønt batteri for Europa",
  "Jeg tror ikke utbyggingsalternativet vil fungere etter sin hensikt",
  "Jeg visste ikke hvilket alternativ jeg skulle velge",
  "Kraften blir uansett eksportert som gir oss høyere strømpriser",
  "Valgene var for vanskelige",
  "Vet ikke"
)

database <- database %>%
  mutate(
    protest = if_else(Q27 %in% protest_levels, 1L, 0L)
  )

mean(database$protest)


database <- database %>%
  filter(protest == 0)

#These are starting vaulues derived from estiamting a MXL without correlation in preference space.
#Starting values for model in WTP-space is derived from calculating WTA estimates from these
#Starting values.
#Estimated parameters with approximate standard errors from BHHH matrix:
#  Estimate     BHHH se BHH t-ratio (0)
#mu_alt2           0.65509    0.296952         2.20605
#mu_el38           5.84582    6.815232         0.85776
#mu_el57           7.22834    7.508187         0.96273
#mu_plants3       -6.36548    7.169178        -0.88790
#mu_plants6       -7.50397    7.031455        -1.06720
#mu_wild10       -56.04594    8.660558        -6.47140
#mu_wild20       -17.02430    6.335954        -2.68694
#mu_dist1        -85.03270   12.393509        -6.86107
#mu_dist2        -61.32822    9.639028        -6.36249
#mu_dist3        -26.01818    8.939743        -2.91039
#mu_view2        -25.26631    6.126459        -4.12413
#sigma_alt2        4.61070    0.348137        13.24394
#sigma_el38      -15.10873   27.526133        -0.54889
#sigma_el57       37.59442   12.038139         3.12294
#sigma_plants3    24.76819   18.852356         1.31380
#sigma_plants6    35.69607   15.584146         2.29054
#sigma_wild10     74.24947   12.466717         5.95582
#sigma_wild20     13.97708   31.041343         0.45027
#sigma_dist1     -67.03400   14.388168        -4.65897
#sigma_dist2      -3.59389   74.973157        -0.04794
#sigma_dist3      -1.22792   91.183946        -0.01347
#sigma_view2      34.03449   11.407895         2.98342
#b_cost            0.02337    0.002817         8.29619
#
#Final LL: -1881.4462


apollo_initialise()

apollo_control <- list(
  modelName       = "WIND5",
  modelDescr      = "",
  indivID         = "id",
  outputDirectory = "output",
  mixing    = TRUE, 
  nCores    = 8
)

database <- df_apollo_noprotest

# Parameters to estimate
apollo_beta <- c(
  mu_alt2                    =    28.031,
  mu_el38                    =    5.84582,
  mu_el57                    =    7.22834,
  mu_plants3                 =   -6.36548,
  mu_plants6                 =   -7.50397,
  mu_wild10                  =  -56.04594,
  mu_wild20                  =  -17.02430,
  mu_dist1                   =  -85.03270,
  mu_dist2                   =  -61.32822,
  mu_dist3                   =  -26.01818,
  mu_view2                   =  -25.26631,
  sigma_alt2                 =    197.291,
  sigma_el38                 =  -15.10873,
  sigma_el57                 =   37.59442,
  sigma_plants3              =   24.76819,
  sigma_plants6              =   35.69607,
  sigma_wild10               =   74.24947,
  sigma_wild20               =   13.97708,
  sigma_dist1                =  -67.03400,
  sigma_dist2                =   -3.59389,
  sigma_dist3                =   -1.22792,
  sigma_view2                =   34.03449,
  mu_cost                    =         -4,
  sigma_cost                 =          0,
  sigma_el38_el57            =          0,
  sigma_plants3_plants6      =          0,
  sigma_wild10_wild20        =          0,
  sigma_dist1_dist2          =          0,
  sigma_dist1_dist3          =          0,
  sigma_dist2_dist3          =          0
)

# Parameters to fix (none for now)
apollo_fixed <- c()

# ################################################################# #
#### DEFINE RANDOM COMPONENTS                                    ####
# ################################################################# #

### Set parameters for generating draws
apollo_draws = list(
  interDrawsType = "mlhs",
  interNDraws = 2000,
  interNormDraws = c(
    "xi_alt2", "xi_el38", "xi_el57", "xi_plants3", "xi_plants6",
    "xi_wild10", "xi_wild20", "xi_dist1", "xi_dist2", "xi_dist3", "xi_view2", "xi_cost"
  )
)

apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["b_alt2"]]    = mu_alt2 + sigma_alt2 * xi_alt2
  randcoeff[["b_el38"]]    = mu_el38 + sigma_el38 * xi_el38
  randcoeff[["b_el57"]]    = mu_el57 + sigma_el57 * xi_el57 + sigma_el38_el57 * xi_el38
  randcoeff[["b_plants3"]] = mu_plants3 + sigma_plants3 * xi_plants3
  randcoeff[["b_plants6"]] = mu_plants6 + sigma_plants6 * xi_plants6 + sigma_plants3_plants6 * xi_plants3
  randcoeff[["b_wild10"]]  = mu_wild10 + sigma_wild10 * xi_wild10
  randcoeff[["b_wild20"]]  = mu_wild20 + sigma_wild20 * xi_wild20 + sigma_wild10_wild20 * xi_wild10
  randcoeff[["b_dist1"]]   = mu_dist1 + sigma_dist1 * xi_dist1
  randcoeff[["b_dist2"]]   = mu_dist2 + sigma_dist2 * xi_dist2 + sigma_dist1_dist2 * xi_dist1
  randcoeff[["b_dist3"]]   = mu_dist3 + sigma_dist3 * xi_dist3  + sigma_dist1_dist3 * xi_dist1  + sigma_dist2_dist3 * xi_dist2
  randcoeff[["b_view2"]]   = mu_view2 + sigma_view2 * xi_view2
  randcoeff[["b_cost"]]   = exp(mu_cost + sigma_cost * xi_cost)
  return(randcoeff)
}


# Validate
apollo_inputs <- apollo_validateInputs()

# ------------------ PROBABILITY FUNCTION ----------------------

apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate") {
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P <- list()
  
  V <- list()
  V[["alt1"]] = b_cost*(b_el38*(el_alt1==38) + b_el57*(el_alt1==57) + b_plants3 * (plants_alt1==3) + b_plants6 * (plants_alt1==6) + b_wild10*(wild_alt1==0) + b_wild20*(wild_alt1==1) + b_view2 * (view_alt1==2) + b_dist1*(dist_alt1==1) + b_dist2*(dist_alt1==2) + b_dist3*(dist_alt1==3) + cost_alt1/100)
  V[["alt2"]] = b_cost*b_alt2 + b_cost*(cost_alt2)
  
  mnl_settings <- list(
    alternatives = c(alt1 = 1, alt2 = 2),
    avail        = list(alt1 = 1, alt2 = 1),
    choiceVar    = choice,
    utilities    = V
  )
  
  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Average across inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #


model = apollo_estimate(apollo_beta, apollo_fixed,
                        apollo_probabilities, apollo_inputs, estimate_settings=list(maxIterations=1000, estimationRoutine="bfgs"))


apollo_modelOutput(model, modelOutput_settings=list(printPVal="TRUE"))


apollo_saveOutput(model)

mu2 <- model$estimate

sigma2 <- model$robvarcov

mu2 <- mu2[ mu2 != 0 ]


#View(mu)

simulate_cov_norobust <- MASS::mvrnorm(10000, mu = mu2, Sigma = sigma2)


write.csv(simulate_cov_norobust, "simulate_cov_WIND4.csv")


### Clear memory
rm(list = ls())
gc()

### load data
database = read.csv("apollo_wind_solar_pooled.csv",header=TRUE)

database = subset(database,database$SOLAR_dummy==1)



###########
# SOLAR 4 #
###########


protest_levels <- c(
  "Å forsyne Europe med fornybar energi fra Norge skal ikke gå på bekostning av norsk natur",
  "Andre årsaker, vennligst spesifiser:",
  "Det vil ikke bidra tilstrekkelig i kampen mot klimaendringer",
  "Jeg synes ikke Norge skal være et grønt batteri for Europa",
  "Jeg tror ikke utbyggingsalternativet vil fungere etter sin hensikt",
  "Jeg visste ikke hvilket alternativ jeg skulle velge",
  "Kraften blir uansett eksportert som gir oss høyere strømpriser",
  "Valgene var for vanskelige",
  "Vet ikke"
)

database <- database %>%
  mutate(
    protest = if_else(Q27 %in% protest_levels, 1L, 0L)
  )

mean(database$protest)


database <- database %>%
  filter(protest == 0)

#Estimated parameters with approximate standard errors from BHHH matrix:
#  Estimate     BHHH se BHH t-ratio (0)
#mu_alt2          -0.54607    0.246297       -2.217108
#mu_el38          -5.69340    6.382136       -0.892084
#mu_el57         -12.34423    6.073537       -2.032462
#mu_plants3       -4.33712    5.998269       -0.723062
#mu_plants6      -19.60298    6.210947       -3.156199
#mu_wild10       -64.26219    8.836930       -7.272004
#mu_wild20       -19.81080    5.880128       -3.369110
#mu_dist1        -40.49708    7.726667       -5.241209
#mu_dist2        -29.48510    7.108802       -4.147689
#mu_dist3        -12.60533    7.323265       -1.721272
#mu_view2        -41.43865    6.815283       -6.080253
#sigma_alt2        3.33507    0.207002       16.111339
#sigma_el38        6.54170   41.014255        0.159498
#sigma_el57        6.25649   48.159695        0.129911
#sigma_plants3    22.78740   16.564965        1.375638
#sigma_plants6     0.58715   84.989793        0.006908
#sigma_wild10    -83.14977   12.674276       -6.560514
#sigma_wild20    -28.80973   14.819349       -1.944062
#sigma_dist1     -12.70890   30.991640       -0.410075
#sigma_dist2       8.01651   47.311166        0.169442
#sigma_dist3      36.30169   16.939872        2.142973
#sigma_view2      62.79388    9.925049        6.326808
#b_cost            0.02191    0.002287        9.581188

#Final LL: -2339.4778



apollo_initialise()

apollo_control <- list(
  modelName       = "SOLAR4",
  modelDescr      = "",
  indivID         = "id",
  outputDirectory = "output",
  mixing    = TRUE, 
  nCores    = 8
)

database <- df_apollo_noprotest

# Parameters to estimate
apollo_beta <- c(
  mu_alt2                    =   -24.923,
  mu_el38                    =   -5.693,
  mu_el57                    =  -12.344,
  mu_plants3                 =   -4.337,
  mu_plants6                 =  -19.603,
  mu_wild10                  =  -64.262,
  mu_wild20                  =  -19.811,
  mu_dist1                   =  -40.497,
  mu_dist2                   =  -29.485,
  mu_dist3                   =  -12.605,
  mu_view2                   =  -41.439,
  sigma_alt2                 =   152.21,
  sigma_el38                 =    6.542,
  sigma_el57                 =    6.257,
  sigma_plants3              =   22.787,
  sigma_plants6              =    0.587,
  sigma_wild10               =  -83.150,
  sigma_wild20               =  -28.810,
  sigma_dist1                =  -12.709,
  sigma_dist2                =    8.017,
  sigma_dist3                =   36.302,
  sigma_view2                =   62.794,
  mu_cost                    =       -4,
  sigma_cost                 =          0,
  sigma_el38_el57            =          0,
  sigma_plants3_plants6      =          0,
  sigma_wild10_wild20        =          0,
  sigma_dist1_dist2          =          0,
  sigma_dist1_dist3          =          0,
  sigma_dist2_dist3          =          0
)

# Parameters to fix (none for now)
apollo_fixed <- c()

# ################################################################# #
#### DEFINE RANDOM COMPONENTS                                    ####
# ################################################################# #

### Set parameters for generating draws
apollo_draws = list(
  interDrawsType = "mlhs",
  interNDraws = 2000,
  interNormDraws = c(
    "xi_alt2", "xi_el38", "xi_el57", "xi_plants3", "xi_plants6",
    "xi_wild10", "xi_wild20", "xi_dist1", "xi_dist2", "xi_dist3", "xi_view2", "xi_cost"
  )
)

apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["b_alt2"]]    = mu_alt2 + sigma_alt2 * xi_alt2
  randcoeff[["b_el38"]]    = mu_el38 + sigma_el38 * xi_el38
  randcoeff[["b_el57"]]    = mu_el57 + sigma_el57 * xi_el57 + sigma_el38_el57 * xi_el38
  randcoeff[["b_plants3"]] = mu_plants3 + sigma_plants3 * xi_plants3
  randcoeff[["b_plants6"]] = mu_plants6 + sigma_plants6 * xi_plants6 + sigma_plants3_plants6 * xi_plants3
  randcoeff[["b_wild10"]]  = mu_wild10 + sigma_wild10 * xi_wild10
  randcoeff[["b_wild20"]]  = mu_wild20 + sigma_wild20 * xi_wild20 + sigma_wild10_wild20 * xi_wild10
  randcoeff[["b_dist1"]]   = mu_dist1 + sigma_dist1 * xi_dist1
  randcoeff[["b_dist2"]]   = mu_dist2 + sigma_dist2 * xi_dist2 + sigma_dist1_dist2 * xi_dist1
  randcoeff[["b_dist3"]]   = mu_dist3 + sigma_dist3 * xi_dist3  + sigma_dist1_dist3 * xi_dist1  + sigma_dist2_dist3 * xi_dist2
  randcoeff[["b_view2"]]   = mu_view2 + sigma_view2 * xi_view2
  randcoeff[["b_cost"]]   = exp(mu_cost + sigma_cost * xi_cost)
  return(randcoeff)
}


# Validate
apollo_inputs <- apollo_validateInputs()

# ------------------ PROBABILITY FUNCTION ----------------------

apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate") {
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P <- list()
  
  V <- list()
  V[["alt1"]] = b_cost*(b_el38*(el_alt1==38) + b_el57*(el_alt1==57) + b_plants3 * (plants_alt1==3) + b_plants6 * (plants_alt1==6) + b_wild10*(wild_alt1==0) + b_wild20*(wild_alt1==1) + b_view2 * (view_alt1==2) + b_dist1*(dist_alt1==1) + b_dist2*(dist_alt1==2) + b_dist3*(dist_alt1==3) + cost_alt1/100)
  V[["alt2"]] = b_cost*b_alt2 + b_cost*(cost_alt2)
  
  mnl_settings <- list(
    alternatives = c(alt1 = 1, alt2 = 2),
    avail        = list(alt1 = 1, alt2 = 1),
    choiceVar    = choice,
    utilities    = V
  )
  
  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Average across inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #


model = apollo_estimate(apollo_beta, apollo_fixed,
                        apollo_probabilities, apollo_inputs, estimate_settings=list(maxIterations=1000, estimationRoutine="bfgs"))


apollo_modelOutput(model, modelOutput_settings=list(printPVal="TRUE"))


apollo_saveOutput(model)


mu2 <- model$estimate

sigma2 <- model$robvarcov

mu2 <- mu2[ mu2 != 0 ]


#View(mu)

simulate_cov_norobust <- MASS::mvrnorm(10000, mu = mu2, Sigma = sigma2)


write.csv(simulate_cov_norobust, "simulate_cov_SOLAR4.csv")

############
# FIGURE 8 #
############

model_sun <- readRDS("SOLAR1.rds")
model_sun <- readRDS("WIND1.rds")


### Policy 1 - Visibility ###


#WIND:
delmeth1 <- deltaMethod(model_wind$estimate[ model_wind$estimate != 0 ], "((((mu_alt2*100-mu_el38*100 - mu_dist1*100-mu_view2*100)*4100)/600000000))/13.88", vcov. = model_wind$robvarcov)
delmeth1

#SOL
delmeth1 <- deltaMethod(model_sun$estimate[ model_sun$estimate != 0 ], "((((mu_alt2*100-mu_el38*100 - mu_dist1*100-mu_view2*100)*4100)/600000000))/13.88", vcov. = model_sun$robvarcov)
delmeth1

### Policy 1 - No visibility ###

#WIND:
delmeth1 <- deltaMethod(model_wind$estimate[ model_wind$estimate != 0 ], "((((mu_alt2*100-mu_el38*100 - mu_dist1*100)*4100)/600000000))/13.88", vcov. = model_wind$robvarcov)
delmeth1

#SOL
delmeth1 <- deltaMethod(model_sun$estimate[ model_sun$estimate != 0 ], "((((mu_alt2*100-mu_el38*100 - mu_dist1*100)*4100)/600000000))/13.88", vcov. = model_sun$robvarcov)
delmeth1

### Policy 2 - Visibility ###

#WIND:
delmeth1 <- deltaMethod(model_wind$estimate[ model_wind$estimate != 0 ], "((((mu_alt2*100-mu_el38*100 - mu_plants6*100 - mu_dist1*100-mu_view2*100)*4100)/600000000))/13.88", vcov. = model_wind$robvarcov)
delmeth1

#SOL
delmeth1 <- deltaMethod(model_sun$estimate[ model_sun$estimate != 0 ], "((((mu_alt2*100-mu_el38*100 - mu_plants6*100 - mu_dist1*100-mu_view2*100)*4100)/600000000))/13.88", vcov. = model_sun$robvarcov)
delmeth1

### Policy 2 - No visibility ###

#WIND:
delmeth1 <- deltaMethod(model_wind$estimate[ model_wind$estimate != 0 ], "((((mu_alt2*100-mu_el38*100 - mu_plants6*100 - mu_dist1*100)*4100)/600000000))/13.88", vcov. = model_wind$robvarcov)
delmeth1

#SOL
delmeth1 <- deltaMethod(model_sun$estimate[ model_sun$estimate != 0 ], "((((mu_alt2*100-mu_el38*100 - mu_plants6*100 - mu_dist1*100)*4100)/600000000))/13.88", vcov. = model_sun$robvarcov)
delmeth1


### Policy 3 - Visibility ###

#WIND:
delmeth1 <- deltaMethod(model_wind$estimate[ model_wind$estimate != 0 ], "((((mu_alt2*100-mu_el38*100 - mu_wild10*100 - mu_view2*100)*4100)/600000000))/13.88", vcov. = model_wind$robvarcov)
delmeth1

#SOL
delmeth1 <- deltaMethod(model_sun$estimate[ model_sun$estimate != 0 ], "((((mu_alt2*100-mu_el38*100 - mu_wild10*100 - mu_view2*100)*4100)/600000000))/13.88", vcov. = model_sun$robvarcov)
delmeth1


### Policy 3 - No visibility ###

#WIND:
delmeth1 <- deltaMethod(model_wind$estimate[ model_wind$estimate != 0 ], "((((mu_alt2*100-mu_el38*100 - mu_wild10*100)*4100)/600000000))/13.88", vcov. = model_wind$robvarcov)
delmeth1

#SOL
delmeth1 <- deltaMethod(model_sun$estimate[ model_sun$estimate != 0 ], "((((mu_alt2*100-mu_el38*100 - mu_wild10*100)*4100)/600000000))/13.88", vcov. = model_sun$robvarcov)
delmeth1

### Policy 4 - Visibility ###

#WIND:
delmeth1 <- deltaMethod(model_wind$estimate[ model_wind$estimate != 0 ], "((((mu_alt2*100-mu_el38*100 - mu_plants6*100 - mu_wild10*100 - mu_view2*100)*4100)/600000000))/13.88", vcov. = model_wind$robvarcov)
delmeth1

#SOL
delmeth1 <- deltaMethod(model_sun$estimate[ model_sun$estimate != 0 ], "((((mu_alt2*100-mu_el38*100 - mu_plants6*100 - mu_wild10*100 - mu_view2*100)*4100)/600000000))/13.88", vcov. = model_sun$robvarcov)
delmeth1

### Policy 4 - No visibility ###

#WIND:
delmeth1 <- deltaMethod(model_wind$estimate[ model_wind$estimate != 0 ], "((((mu_alt2*100-mu_el38*100 - mu_plants6*100 - mu_wild10*100)*4100)/600000000))/13.88", vcov. = model_wind$robvarcov)
delmeth1

#SOL
delmeth1 <- deltaMethod(model_sun$estimate[ model_sun$estimate != 0 ], "((((mu_alt2*100-mu_el38*100 - mu_plants6*100 - mu_wild10*100)*4100)/600000000))/13.88", vcov. = model_sun$robvarcov)
delmeth1

### Policy 5 - Visibility ###

#WIND:
delmeth1 <- deltaMethod(model_wind$estimate[ model_wind$estimate != 0 ], "((((mu_alt2*100-mu_el38*100 - mu_plants3*100 - mu_wild20*100 - mu_dist3*100 - mu_view2*100)*4100)/600000000))/13.88", vcov. = model_wind$robvarcov)
delmeth1

#SOL
delmeth1 <- deltaMethod(model_sun$estimate[ model_sun$estimate != 0 ], "((((mu_alt2*100-mu_el38*100 - mu_plants3*100 - mu_wild20*100 - mu_dist3*100 - mu_view2*100)*4100)/600000000))/13.88", vcov. = model_sun$robvarcov)
delmeth1

### Policy 5 - No visibility ###

#WIND:
delmeth1 <- deltaMethod(model_wind$estimate[ model_wind$estimate != 0 ], "((((mu_alt2*100-mu_el38*100 - mu_plants3*100 - mu_wild20*100 - mu_dist3*100)*4100)/600000000))/13.88", vcov. = model_wind$robvarcov)
delmeth1

#SOL
delmeth1 <- deltaMethod(model_sun$estimate[ model_sun$estimate != 0 ], "((((mu_alt2*100-mu_el38*100 - mu_plants3*100 - mu_wild20*100 - mu_dist3*100)*4100)/600000000))/13.88", vcov. = model_sun$robvarcov)
delmeth1

### Policy 6 - Visibility ###

#WIND:
delmeth1 <- deltaMethod(model_wind$estimate[ model_wind$estimate != 0 ], "((((mu_alt2*100-mu_el38*100 - mu_view2*100)*4100)/600000000))/13.88", vcov. = model_wind$robvarcov)
delmeth1

#SOL
delmeth1 <- deltaMethod(model_sun$estimate[ model_sun$estimate != 0 ], "((((mu_alt2*100-mu_el38*100 - mu_view2*100)*4100)/600000000))/13.88", vcov. = model_sun$robvarcov)
delmeth1

### Policy 6 - No visibility ###

#WIND:
delmeth1 <- deltaMethod(model_wind$estimate[ model_wind$estimate != 0 ], "((((mu_alt2*100-mu_el38*100)*4100)/600000000))/13.88", vcov. = model_wind$robvarcov)
delmeth1

#SOL
delmeth1 <- deltaMethod(model_sun$estimate[ model_sun$estimate != 0 ], "((((mu_alt2*100-mu_el38*100)*4100)/600000000))/13.88", vcov. = model_sun$robvarcov)
delmeth1


############
# FIGURE 9 #
############


### Policy 5 - 19 thousand households electricity consumption - Visibility ###

#WIND:
delmeth1 <- deltaMethod(model_wind$estimate[ model_wind$estimate != 0 ], "((((mu_alt2*100 - mu_plants3*100 - mu_wild20*100 - mu_dist3*100 - mu_view2*100)*4100)/300000000))/13.88", vcov. = model_wind$robvarcov)
delmeth1

#SOL
delmeth1 <- deltaMethod(model_sun$estimate[ model_sun$estimate != 0 ], "((((mu_alt2*100 - mu_plants3*100 - mu_wild20*100 - mu_dist3*100 - mu_view2*100)*4100)/300000000))/13.88", vcov. = model_sun$robvarcov)
delmeth1

### Policy 5 - Policy 5 - 19 thousand households electricity consumption - No visibility ###

#WIND:
delmeth1 <- deltaMethod(model_wind$estimate[ model_wind$estimate != 0 ], "((((mu_alt2*100 - mu_plants3*100 - mu_wild20*100 - mu_dist3*100)*4100)/300000000))/13.88", vcov. = model_wind$robvarcov)
delmeth1

#SOL
delmeth1 <- deltaMethod(model_sun$estimate[ model_sun$estimate != 0 ], "((((mu_alt2*100 - mu_plants3*100 - mu_wild20*100 - mu_dist3*100)*4100)/300000000))/13.88", vcov. = model_sun$robvarcov)
delmeth1

### Policy 5 - 38 thousand households electricity consumption - Visibility ###

#WIND:
delmeth1 <- deltaMethod(model_wind$estimate[ model_wind$estimate != 0 ], "((((mu_alt2*100-mu_el38*100 - mu_plants3*100 - mu_wild20*100 - mu_dist3*100 - mu_view2*100)*4100)/600000000))/13.88", vcov. = model_wind$robvarcov)
delmeth1

#SOL
delmeth1 <- deltaMethod(model_sun$estimate[ model_sun$estimate != 0 ], "((((mu_alt2*100-mu_el38*100 - mu_plants3*100 - mu_wild20*100 - mu_dist3*100 - mu_view2*100)*4100)/600000000))/13.88", vcov. = model_sun$robvarcov)
delmeth1

### Policy 5 - 38 thousand households electricity consumption - No visibility ###

#WIND:
delmeth1 <- deltaMethod(model_wind$estimate[ model_wind$estimate != 0 ], "((((mu_alt2*100-mu_el38*100 - mu_plants3*100 - mu_wild20*100 - mu_dist3*100)*4100)/600000000))/13.88", vcov. = model_wind$robvarcov)
delmeth1

#SOL
delmeth1 <- deltaMethod(model_sun$estimate[ model_sun$estimate != 0 ], "((((mu_alt2*100-mu_el38*100 - mu_plants3*100 - mu_wild20*100 - mu_dist3*100)*4100)/600000000))/13.88", vcov. = model_sun$robvarcov)
delmeth1

### Policy 5 - 57 thousand households electricity consumption - Visibility ###

#WIND:
delmeth1 <- deltaMethod(model_wind$estimate[ model_wind$estimate != 0 ], "((((mu_alt2*100-mu_el57*100 - mu_plants3*100 - mu_wild20*100 - mu_dist3*100 - mu_view2*100)*4100)/900000000))/13.88", vcov. = model_wind$robvarcov)
delmeth1

#SOL
delmeth1 <- deltaMethod(model_sun$estimate[ model_sun$estimate != 0 ], "((((mu_alt2*100-mu_el57*100 - mu_plants3*100 - mu_wild20*100 - mu_dist3*100 - mu_view2*100)*4100)/900000000))/13.88", vcov. = model_sun$robvarcov)
delmeth1

### Policy 5 - 57 thousand households electricity consumption - No visibility ###

#WIND:
delmeth1 <- deltaMethod(model_wind$estimate[ model_wind$estimate != 0 ], "((((mu_alt2*100-mu_el57*100 - mu_plants3*100 - mu_wild20*100 - mu_dist3*100)*4100)/900000000))/13.88", vcov. = model_wind$robvarcov)
delmeth1

#SOL
delmeth1 <- deltaMethod(model_sun$estimate[ model_sun$estimate != 0 ], "((((mu_alt2*100-mu_el57*100 - mu_plants3*100 - mu_wild20*100 - mu_dist3*100)*4100)/900000000))/13.88", vcov. = model_sun$robvarcov)
delmeth1

##############
# FIGURE A.5 #
##############

model_sun <- readRDS("SOLAR4.rds")
model_sun <- readRDS("WIND4.rds")


### Policy 1 - Visibility ###


#WIND:
delmeth1 <- deltaMethod(model_wind$estimate[ model_wind$estimate != 0 ], "((((mu_alt2*100-mu_el38*100 - mu_dist1*100-mu_view2*100)*4100)/600000000))/13.88", vcov. = model_wind$robvarcov)
delmeth1

#SOL
delmeth1 <- deltaMethod(model_sun$estimate[ model_sun$estimate != 0 ], "((((mu_alt2*100-mu_el38*100 - mu_dist1*100-mu_view2*100)*4100)/600000000))/13.88", vcov. = model_sun$robvarcov)
delmeth1

### Policy 1 - No visibility ###

#WIND:
delmeth1 <- deltaMethod(model_wind$estimate[ model_wind$estimate != 0 ], "((((mu_alt2*100-mu_el38*100 - mu_dist1*100)*4100)/600000000))/13.88", vcov. = model_wind$robvarcov)
delmeth1

#SOL
delmeth1 <- deltaMethod(model_sun$estimate[ model_sun$estimate != 0 ], "((((mu_alt2*100-mu_el38*100 - mu_dist1*100)*4100)/600000000))/13.88", vcov. = model_sun$robvarcov)
delmeth1

### Policy 2 - Visibility ###

#WIND:
delmeth1 <- deltaMethod(model_wind$estimate[ model_wind$estimate != 0 ], "((((mu_alt2*100-mu_el38*100 - mu_plants6*100 - mu_dist1*100-mu_view2*100)*4100)/600000000))/13.88", vcov. = model_wind$robvarcov)
delmeth1

#SOL
delmeth1 <- deltaMethod(model_sun$estimate[ model_sun$estimate != 0 ], "((((mu_alt2*100-mu_el38*100 - mu_plants6*100 - mu_dist1*100-mu_view2*100)*4100)/600000000))/13.88", vcov. = model_sun$robvarcov)
delmeth1

### Policy 2 - No visibility ###

#WIND:
delmeth1 <- deltaMethod(model_wind$estimate[ model_wind$estimate != 0 ], "((((mu_alt2*100-mu_el38*100 - mu_plants6*100 - mu_dist1*100)*4100)/600000000))/13.88", vcov. = model_wind$robvarcov)
delmeth1

#SOL
delmeth1 <- deltaMethod(model_sun$estimate[ model_sun$estimate != 0 ], "((((mu_alt2*100-mu_el38*100 - mu_plants6*100 - mu_dist1*100)*4100)/600000000))/13.88", vcov. = model_sun$robvarcov)
delmeth1


### Policy 3 - Visibility ###

#WIND:
delmeth1 <- deltaMethod(model_wind$estimate[ model_wind$estimate != 0 ], "((((mu_alt2*100-mu_el38*100 - mu_wild10*100 - mu_view2*100)*4100)/600000000))/13.88", vcov. = model_wind$robvarcov)
delmeth1

#SOL
delmeth1 <- deltaMethod(model_sun$estimate[ model_sun$estimate != 0 ], "((((mu_alt2*100-mu_el38*100 - mu_wild10*100 - mu_view2*100)*4100)/600000000))/13.88", vcov. = model_sun$robvarcov)
delmeth1


### Policy 3 - No visibility ###

#WIND:
delmeth1 <- deltaMethod(model_wind$estimate[ model_wind$estimate != 0 ], "((((mu_alt2*100-mu_el38*100 - mu_wild10*100)*4100)/600000000))/13.88", vcov. = model_wind$robvarcov)
delmeth1

#SOL
delmeth1 <- deltaMethod(model_sun$estimate[ model_sun$estimate != 0 ], "((((mu_alt2*100-mu_el38*100 - mu_wild10*100)*4100)/600000000))/13.88", vcov. = model_sun$robvarcov)
delmeth1

### Policy 4 - Visibility ###

#WIND:
delmeth1 <- deltaMethod(model_wind$estimate[ model_wind$estimate != 0 ], "((((mu_alt2*100-mu_el38*100 - mu_plants6*100 - mu_wild10*100 - mu_view2*100)*4100)/600000000))/13.88", vcov. = model_wind$robvarcov)
delmeth1

#SOL
delmeth1 <- deltaMethod(model_sun$estimate[ model_sun$estimate != 0 ], "((((mu_alt2*100-mu_el38*100 - mu_plants6*100 - mu_wild10*100 - mu_view2*100)*4100)/600000000))/13.88", vcov. = model_sun$robvarcov)
delmeth1

### Policy 4 - No visibility ###

#WIND:
delmeth1 <- deltaMethod(model_wind$estimate[ model_wind$estimate != 0 ], "((((mu_alt2*100-mu_el38*100 - mu_plants6*100 - mu_wild10*100)*4100)/600000000))/13.88", vcov. = model_wind$robvarcov)
delmeth1

#SOL
delmeth1 <- deltaMethod(model_sun$estimate[ model_sun$estimate != 0 ], "((((mu_alt2*100-mu_el38*100 - mu_plants6*100 - mu_wild10*100)*4100)/600000000))/13.88", vcov. = model_sun$robvarcov)
delmeth1

### Policy 5 - Visibility ###

#WIND:
delmeth1 <- deltaMethod(model_wind$estimate[ model_wind$estimate != 0 ], "((((mu_alt2*100-mu_el38*100 - mu_plants3*100 - mu_wild20*100 - mu_dist3*100 - mu_view2*100)*4100)/600000000))/13.88", vcov. = model_wind$robvarcov)
delmeth1

#SOL
delmeth1 <- deltaMethod(model_sun$estimate[ model_sun$estimate != 0 ], "((((mu_alt2*100-mu_el38*100 - mu_plants3*100 - mu_wild20*100 - mu_dist3*100 - mu_view2*100)*4100)/600000000))/13.88", vcov. = model_sun$robvarcov)
delmeth1

### Policy 5 - No visibility ###

#WIND:
delmeth1 <- deltaMethod(model_wind$estimate[ model_wind$estimate != 0 ], "((((mu_alt2*100-mu_el38*100 - mu_plants3*100 - mu_wild20*100 - mu_dist3*100)*4100)/600000000))/13.88", vcov. = model_wind$robvarcov)
delmeth1

#SOL
delmeth1 <- deltaMethod(model_sun$estimate[ model_sun$estimate != 0 ], "((((mu_alt2*100-mu_el38*100 - mu_plants3*100 - mu_wild20*100 - mu_dist3*100)*4100)/600000000))/13.88", vcov. = model_sun$robvarcov)
delmeth1

### Policy 6 - Visibility ###

#WIND:
delmeth1 <- deltaMethod(model_wind$estimate[ model_wind$estimate != 0 ], "((((mu_alt2*100-mu_el38*100 - mu_view2*100)*4100)/600000000))/13.88", vcov. = model_wind$robvarcov)
delmeth1

#SOL
delmeth1 <- deltaMethod(model_sun$estimate[ model_sun$estimate != 0 ], "((((mu_alt2*100-mu_el38*100 - mu_view2*100)*4100)/600000000))/13.88", vcov. = model_sun$robvarcov)
delmeth1

### Policy 6 - No visibility ###

#WIND:
delmeth1 <- deltaMethod(model_wind$estimate[ model_wind$estimate != 0 ], "((((mu_alt2*100-mu_el38*100)*4100)/600000000))/13.88", vcov. = model_wind$robvarcov)
delmeth1

#SOL
delmeth1 <- deltaMethod(model_sun$estimate[ model_sun$estimate != 0 ], "((((mu_alt2*100-mu_el38*100)*4100)/600000000))", vcov. = model_sun$robvarcov)
delmeth1



