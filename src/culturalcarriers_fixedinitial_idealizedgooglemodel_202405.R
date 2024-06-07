#!/bin/env Rscript

# Cultural Transmission & Variation in Organizational Populations
# Simulations over parameter grid and fixed initial conditions
# 
# To run via command line:
# bsub -q short -n 16 -R "rusage[mem=5G]" -M 15G -hl -o ./log/idealizedgooglemodel_%J.out -B -N Rscript ./src/culturalcarriers_fixedinitial_idealizedgooglemodel_202405.R
# 
# Coded May 2024: Idealized Behemoth Model (aka Google)
# 
# author: Anjali Bhatt


### Define workspace details
# setwd("/export/projects1/abhatt_culturalcarriers/cultural-carriers-ABM/")
filename <- paste("data/large/", Sys.Date(), "_results_idealizedgooglemodel.csv", sep="") # output file
n_cores <- as.integer(Sys.getenv('LSB_DJOB_NUMPROC')) # detect number of CPUs for parallelization

### Load libraries
library(data.table)
library(matrixStats)
library(parallel)

### Set global parameters for simulations
n_reps <- 5 # number of replications per set of parameters, per initial condition
f <- 30 # number of other firms
n <- 30 # number of employees per other firm
r_google <- 1/5 # fraction of labor market employed by behemoth
c_google <- as.numeric(2) # initial culture of behemoth (assumed constant in this model)
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
  b1 = seq(0,1,0.1), # initial socialization susceptibility
  b2 = 0.30, # speed of socialization susceptibility decline by tenure
  b3 = 0.10, # speed of socialization susceptibility decline by employments
  
  # turnover params
  r0 = c(0.05), # turnover base rate (3.5% monthly according to JOLTS)
  r1 = seq(0.1,1.0,0.1), # turnover alienation rate
  r2 = c(0.05), # max increase in turnover probability
  
  # hiring params (no noise)
  s0 = r_google, # rate of behemoth entry
  s1 = seq(0.1,1.0,0.1) # hiring selectivity threshold
  
  # no behemoth params (just simulate)
)

### FOR TEST USE ONLY
# params <- params[1:100]

### Define function for cultural evolution in population
culture_fn <- function(par) {
  
  ### Create local copy of initial conditions
  sims <- init_conds[sim_no==par$cond, list(firm, culture, tenure, employments)]
  
  ### Initialize stats
  stats <- data.table(var_win=rep(0, t+1), # within-firm variance per period
                      c_mean = 0, # mean of median culture per period
                      hires=0) # total hires per period
  stats[1, var_win := mean(sims[, sd(culture, na.rm=T), by=firm]$V1)]
  stats[1, c_mean := mean(sims[, median(culture, na.rm=T), by=firm]$V1)]
  
  ### Loop over months
  for (i in 1:t) {
    
    # Calculate firm culture at beginning of time period
    med_cult <- setorder(sims[, median(culture, na.rm=T), by=firm], firm)$V1
    
    ### TURNOVER
    
    # Temporary copy of df for population change (turnover/hiring)
    sims2 <- data.table(sims)
    
    # If no alienation, then assume only base turnover rate
    # Otherwise, retention is modeled as a gaussian shape
    
    # turnover for firms
    sims2[, firm := firm * (runif(n*f) > ((par$r0 + par$r2) - par$r2 * par$r1 *
                                            sqrt(2*pi) * (dnorm(culture, med_cult[firm], par$r1))))]
    
    #  Restart tenure clock for departed employees
    sims2[, tenure := (tenure+1) * (firm!=0)]

    ### HIRING
   
    # Track how many hires made each period
    departures <- sum(sims2$firm==0)
    stats$hires[i+1] <- sum(sims2$firm==0)
    
    # Set random order of hiring and prepopulate random entrant info
    queue <- data.table(firm = sims[sims2$firm==0,firm],
                        culture = 0, # initialize as numeric so easier to replace (don't change!)
                        tenure = 0,
                        employments=ceiling(rlnorm(departures,0,0.5)),
                        draw = (runif(departures) <= par$s0))
    
    # Set culture of random entrant based on draw (from behemoth vs. random entry)
    n_googlers <- sum(queue$draw==T)
    queue[draw==T, culture := c_google]
    queue[draw==F, culture := rnorm(departures-n_googlers, med_cult[firm], par$s1)]
    
    # Reset copy of df based on changes to population
    # Append all random entrants and remove all non-hires
    sims <- rbind(sims2[firm!=0],
                  queue[!is.na(culture), list(firm, culture, tenure, employments)])
    
    ### SOCIALIZATION (no noise, no asymptotic socialization)

    # socialization for firms
    sims[, culture := culture + (med_cult[firm] - culture) *
            par$b1 * exp(- (par$b2 * (tenure-1)) - (par$b3 * (employments-1)))]

    # Calculate statistics
    stats[i+1, var_win := mean(sims[, sd(culture, na.rm=T), by=firm]$V1)]
    stats[i+1, c_mean := mean(sims[, median(culture, na.rm=T), by=firm]$V1)]
  }
  
  ### Return summary statistics for each simulation run
  summary <- data.table(c_change = stats$c_mean[t+1] - stats$c_mean[1],
                        varwin_ratio = stats$var_win[t+1] / stats$var_win[1],
                        turnover = mean(stats[2:t+1, hires/(n*f)]),
                        tenure_end = median(sims[, tenure]),
                        emps_end = median(sims[, employments]))
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
