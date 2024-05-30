#!/bin/env R

"
Cultural Transmission & Variation in Organizational Populations
Simulations over parameter grid and fixed initial conditions

Use source() to run code on server

Coded Apr 2024: Behemoth Model (aka Google)

@author: Anjali Bhatt
"

setwd("/export/projects1/abhatt_culturalcarriers/cultural-carriers-ABM/")
library(data.table)
library(matrixStats)
library(parallel)

### Set global parameters for simulations
n_reps <- 5 # number of replications per set of parameters, per initial condition
f <- 24 # number of other firms
n <- 30 # number of employees per other firm
n_google <- 6*n # number of employees at behemoth
f_google <- f+1 # index of behemoth firm
c_google <- 2 # initial culture of behemoth
t <- 120 # number of time periods (months)

### Read in initializations
init_conds <- read.csv("data/initial_conditions_google.csv", header=T)
init_conds <- data.table(init_conds)

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
  s0 = c(0.03), # base rate of random entry
  s1 = seq(0.1,1.5,0.1), # hiring selectivity threshold
  
  # behemoth params
  s0_google = s0, # base rate of random entry for behemoth
  r1_google = c(0.3, 1.0), # turnover alienation for behemoth
  b1_google = c(0.1, 0.7), # socialization susc for behemoth
  s1_google = c(0.3, 1.0) # selectivity for behemoth
  
)

# deduplicate for iterations with functions off
# params[r2 == 0, c('r1') := 1]
# params <- unique(params)

### Define function for cultural evolution in population
culture_fn <- function(par) {
  
  ### Create local copy of initial conditions
  sims <- init_conds[sim_no==par$cond, list(firm, c, tenure, employments)]
  
  ### Initialize stats
  stats <- data.table(var_win_other=rep(0, t+1), # within-firm variance for others per period
                      var_win_google=0, # within-firm variance for behemoth per period
                      c_google = 0, # median culture for behemoth per period
                      c_other = 0, # median culture for others per period
                      hires_other=0, # total hires for other firms per period
                      hires_google=0, # total hires for behemoth per period
                      google_to_other=0, # rehires from behemoth to others per period
                      other_to_other=0, # rehires from other to other per period
                      other_to_google=0) # rehires from other to behemoth per period
  stats[1, var_win_other := mean(sims[firm!=f_google, sd(culture, na.rm=T), by=firm]$V1)]
  stats[1, var_win_google := sims[firm==f_google, sd(culture, na.rm=T)]$V1]
  stats[1, c_other := mean(sims[firm!=f_google, median(culture, na.rm=T), by=firm]$V1)]
  stats[1, c_google := sims[firm==f_google, median(culture, na.rm=T)]$V1]
  
  ### Loop over months
  for (i in 1:t) {
    
    # Calculate firm culture at beginning of time period
    med_cult <- setorder(sims[, median(culture, na.rm=T), by=firm], firm)$V1
    
    ### TURNOVER
    
    # Temporary copy of df for population change (turnover/hiring)
    sims2 <- data.table(sims)
    
    # If no alienation, then assume only base turnover rate
    # Otherwise, retention is modeled as a gaussian shape
    
    # turnover for other firms
    sims2[firm!=f_google, firm := firm * (runif(n*f) > ((par$r0 + par$r2) - par$r2 * par$r1 *
                                            sqrt(2*pi) * (dnorm(culture, med_cult[firm], par$r1))))]
    
    # turnover for behemoth
    sims2[firm==f_google, firm := firm * (runif(n_google) > ((par$r0 + par$r2) - par$r2 * par$r1_google *
                                            sqrt(2*pi) * (dnorm(culture, med_cult[firm], par$r1_google))))]
    
    #  Restart tenure clock for departed employees
    sims2[, tenure := (tenure+1) * (firm!=0)]

    ### HIRING
   
    # Track how many hires made each period
    stats$hires[i+1] <- sum(sims2$firm==0)
    
    if (stats$hires[i+1] > 0) {
      # Set random order of hiring and prepopulate random entrant info
      queue <- data.table(firm = rep(0, stats$hires[i+1]),
                          culture = 0,
                          tenure = 0,
                          employments=ceiling(rlnorm(stats$hires[i+1],0,0.5)),
                          draw = (runif(stats$hires[i+1]) <= par$s0))
      j <- 1
      for (k in 1:f) {
        hires <- n - sum(sims2$firm==k)
        if (hires>0) {
          queue[j:(j + hires - 1), firm := k]
          j <- j + hires
        }
      }
      queue[, firm := firm[sample.int(length(firm))]]
      
      # Iterate over ordered hiring spots
      for (h in 1:stats$hires[i+1]) {
        focal_firm <- queue$firm[h]
        
        # unemployed pool is those within cultural threshold
        unemployed <- which(sims2$firm==0 & sims$firm!=focal_firm &
                              abs(sims2$culture - med_cult[focal_firm]) < 2*par$s1)
        
        # First check for new entry
        if (queue$draw[h] | length(unemployed)==0) {
          queue[h, culture := rnorm(1, med_cult[focal_firm], par$s1)]
          
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
                    queue[culture!=0, list(firm, culture, tenure, employments)])
      
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
  result <- cbind(result, params[i,])
  cat(i, '/', nrow(params), '\n')
  return(result)
}, mc.cores=16)
global_stats <- Reduce(rbind, mc_stats)

### Write simulation results to csv file
filename = paste("data/", Sys.Date(), "_results_baselinemodel.csv", sep="")
write.csv(global_stats, file=filename)
