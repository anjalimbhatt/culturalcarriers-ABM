#!/bin/env Rscript

# Cultural Transmission & Variation in Organizational Populations
# Simulations over parameter grid and fixed initial conditions
# 
# To run via command line:
# bsub < ./src/runbaselinemodel.sh
# bsub -q short -n 16 -R "rusage[mem=5G]" -M 5G -hl -o ./log/baselinemodel_%J.out Rscript ./src/culturalcarriers_fixedinitial_baselinemodel_202404.R
# 
# Originally written March 2017
# Recoded Apr 2018
# Recoded for fixed initial conditions Jun 2018
# Recoded for bug fixes Oct 2018
# Recoded Apr 2019: turnover > hiring > socialization
# Recoded Apr 2024: for new HBS cluster
# 
# author: Anjali Bhatt

### Define workspace details
# setwd("/export/projects1/abhatt_culturalcarriers/cultural-carriers-ABM/")
filename <- paste("data/", Sys.Date(), "_results_baselinemodel.csv", sep="") # output file
n_cores <- as.integer(Sys.getenv('LSB_DJOB_NUMPROC')) # detect number of CPUs for parallelization

### Load libraries
library(data.table)
library(matrixStats)
library(parallel)

### Set global parameters for simulations
n_reps <- 5 # number of replications per set of parameters, per initial condition
f <- 30 # number of firms
n <- 30 # number of employees per firm [10, 100, 1000]
t <- 120 # number of time periods (months)

### Read in initializations
init_conds <- read.csv("data/input/initial_conditions_201810.csv", header=T)
init_conds <- data.table(init_conds)
init_conds <- init_conds[var_win == 0.1] # subset for relevant initial variance within firms

### Make data frame of varying parameter settings
params <- CJ(
  cond = 1:max(init_conds$sim_no),
  rep_no = 1:n_reps,
  
  # socialization params (no noise)
  #b0 = 0.0, # asymptotic socialization susceptibility
  b1 = seq(0, 1, 0.1), # initial socialization susceptibility
  b2 = 0.30, # speed of socialization susceptibility decline by tenure
  b3 = 0.10, # speed of socialization susceptibility decline by employments
  
  # turnover params
  r0 = c(0.01, 0.05), # turnover base rate (3.5% monthly according to JOLTS)
  r1 = seq(0.1, 1.0, 0.1), # turnover alienation rate
  r2 = c(0.05), # max increase in turnover probability
  
  # hiring params (no noise)
  s0 = c(0.03, 1.0), # base rate of random entry
  s1 = seq(0.1, 1.0, 0.1) # hiring selectivity threshold
)

# deduplicate for iterations with functions off
# params[r2 == 0, c('r1') := 1]
# params <- unique(params)

### Define function for cultural evolution in population
culture_fn <- function(par) {
  
  ### Create local copy of initial conditions
  sims <- init_conds[sim_no==par$cond, list(firm, culture, tenure, employments)]
  
  ### Initialize stats
  stats <- data.table(var_win=rep(0, t+1), var_btwn=0, hires=0, rehires=0)
  stats[1, var_win := mean(sims[, sd(culture, na.rm=T), by=firm]$V1)]
  stats[1, var_btwn := sd(sims[, median(culture, na.rm=T), by=firm]$V1)]
  
  ### Loop over months
  for (i in 1:t) {
    
    # Calculate firm culture at beginning of time period
    med_cult <- setorder(sims[, median(culture, na.rm=T), by=firm], firm)$V1
    
    ### TURNOVER
    
    # Temporary copy of df for population change (turnover/hiring)
    sims2 <- data.table(sims)
    
    # If no alienation, then assume only base turnover rate
    # Otherwise, retention is modeled as a gaussian shape
    sims2[, firm := firm * (runif(n*f) > ((par$r0 + par$r2) - par$r2 * par$r1 *
                                            sqrt(2*pi) * (dnorm(culture, med_cult[firm], par$r1))))]
    
    #  Restart tenure clock for departed employees
    sims2[, tenure := (tenure+1) * (firm!=0)]

    ### HIRING
   
    # Track how many hires made each period
    stats$hires[i+1] <- sum(sims2$firm==0)
    
    if (stats$hires[i+1] > 0) {
      # Set random order of hiring and prepopulate random entrant info
      queue <- data.table(firm = sims[sims2$firm==0,firm],
                          culture = NA,
                          tenure = 0,
                          employments=ceiling(rlnorm(stats$hires[i+1],0,0.5)),
                          draw = (runif(stats$hires[i+1]) <= par$s0))

      queue[, firm := firm[sample.int(length(firm))]]
      
      # Iterate over ordered hiring spots
      for (j in 1:stats$hires[i+1]) {
        focal_firm <- queue$firm[j]
        
        # unemployed pool is those within cultural threshold
        unemployed <- which(sims2$firm==0 & sims$firm!=focal_firm &
                              abs(sims2$culture - med_cult[focal_firm]) < 2*par$s1)
        
        # First check for new entry
        if (queue$draw[j] | length(unemployed)==0) {
          queue[j, culture := rnorm(1, med_cult[focal_firm], par$s1)]
          
          # Else draw random unemployed
        } else {
          chosen <- unemployed[sample.int(length(unemployed), 1)]
          sims2[chosen, `:=`(firm = focal_firm,
                             employments = employments + 1)]
          stats[i+1, rehires := rehires + 1]
        }
      }
      
      # Reset copy of df based on changes to population
      # Append all random entrants and remove all non-hires
      sims <- rbind(sims2[firm!=0],
                    queue[!is.na(culture), list(firm, culture, tenure, employments)])
      
    }
    
    ### SOCIALIZATION

    # no noise, no asymptotic socialization
    sims[, culture := culture + (med_cult[firm] - culture) *
            par$b1 * exp(- (par$b2 * (tenure-1)) - (par$b3 * (employments-1)))]
    
    # Calculate statistics
    stats[i+1, var_win := mean(sims[, sd(culture, na.rm=T), by=firm]$V1)]
    stats[i+1, var_btwn := sd(sims[, median(culture, na.rm=T), by=firm]$V1)]
  }
  
  ### Return summary statistics for each simulation run
  summary <- data.table(varbtwn_start = stats$var_btwn[1],
                        varwin_start = stats$var_win[1],
                        varbtwn_end = stats$var_btwn[t+1],
                        varwin_end = stats$var_win[t+1],
                        turnover = mean(stats$hires)/(n*f),
                        carriers = mean(stats$rehires/stats$hires, na.rm=T),
                        tenure_end = median(sims$tenure),
                        emps_end = median(sims$employments))
  return(summary)
}

### Apply culture evolution function for each set of parameters
mc_stats <- mclapply(1:nrow(params), function(i) {
  result <- culture_fn(params[i,])
  result <- cbind(params[i,], result)
  
  # write out to csv file
  write.table(result, file=filename, sep = ",", row.names=F, col.names=F, append=T)
  cat(i, '/', nrow(params), '\n')
  
  # return(result)
}, mc.cores=n_cores)

# global_stats <- Reduce(rbind, mc_stats)

### Write simulation results to csv file
# write.csv(global_stats, file=filename)
