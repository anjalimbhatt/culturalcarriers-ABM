#!/bin/env Rscript

# Cultural Transmission & Variation in Organizational Populations
# Simulations over parameter grid and fixed initial conditions
# 
# To run via command line:
# bsub < ./src/rungooglemodel.sh ### MUCH SLOWER?
# bsub -q short -n 16 -R "rusage[mem=5G]" -M 15G -hl -o ./log/googlemodel_%J.out -B -N Rscript ./src/culturalcarriers_fixedinitial_googlemodel_202404.R
# 
# Coded Apr 2024: Behemoth Model (aka Google)
# 
# author: Anjali Bhatt


### Define workspace details
# setwd("/export/projects1/abhatt_culturalcarriers/cultural-carriers-ABM/")
setwd("~/Documents/GitHub/culturalcarriers-ABM")
filename <- paste("data/large/", Sys.Date(), "_results_googlemodel.csv", sep="") # output file
# n_cores <- as.integer(Sys.getenv('LSB_DJOB_NUMPROC')) # detect number of CPUs for parallelization
n_cores <- as.integer(detectCores())

### Load libraries
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
init_conds <- read.csv("data/input/initial_conditions_201810.csv", header=T)
init_conds <- data.table(init_conds)
init_conds <- init_conds[var_win == 0.1] # subset for relevant initial variance within firms
n_conds <- length(unique(init_conds$cond)) # number of unique conditions after subsetting
init_conds[firm > f, `:=` (firm = f_google, culture = rnorm(n_google * n_conds, c_google, var_win))]
write.csv(init_conds, file = "data/input/initial_conditions_google_202406.csv")

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
  s1 = seq(0.1,1.0,0.1), # hiring selectivity threshold
  
  # behemoth params
  # s0_google = s0, # assume base rate of random entry for behemoth is same as others
  r1_google = c(0.2, 0.8), # turnover alienation for behemoth
  b1_google = c(0.2, 0.8), # socialization susc for behemoth
  s1_google = c(0.2, 0.8) # selectivity for behemoth
  
)

### FOR TEST USE ONLY
# params <- params[1:100]

### Define function for cultural evolution in population
culture_fn <- function(par) {
  
  ### Create local copy of initial conditions
  sims <- init_conds[sim_no==par$cond, list(firm, culture, tenure, employments)]
  
  ### Initialize stats
  stats <- data.table(var_win_other=rep(0, t+1), # within-firm variance for others per period
                      var_win_google=0, # within-firm variance for behemoth per period
                      c_google = 0, # median culture for behemoth per period
                      c_other = 0, # mean of median culture for others per period
                      hires_other=0, # total hires for other firms per period
                      hires_google=0, # total hires for behemoth per period
                      google_to_other=0, # rehires from behemoth to others per period
                      other_to_other=0, # rehires from other to other per period
                      other_to_google=0) # rehires from other to behemoth per period
  stats[1, var_win_other := mean(sims[firm!=f_google, sd(culture, na.rm=T), by=firm]$V1)]
  stats[1, var_win_google := sims[firm==f_google, sd(culture, na.rm=T)]]
  stats[1, c_google := sims[firm==f_google, median(culture, na.rm=T)]]
  stats[1, c_other := mean(sims[firm!=f_google, median(culture, na.rm=T), by=firm]$V1)]
  
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
    departures <- sum(sims2$firm==0)
    stats$hires_other[i+1] <- sum(sims$firm!=f_google & sims2$firm==0)
    stats$hires_google[i+1] <- sum(sims$firm==f_google & sims2$firm==0)
    
    if (departures > 0) {
      # Set random order of hiring and prepopulate random entrant info
      queue <- data.table(firm = sims[sims2$firm==0,firm],
                          culture = 0, # initialize as numeric so easier to replace (don't change!)
                          tenure = 0,
                          employments=ceiling(rlnorm(departures,0,0.5)),
                          draw = (runif(departures) <= par$s0))
      
      queue[, firm := firm[sample.int(length(firm))]]
      
      # Iterate over ordered hiring spots
      for (j in 1:departures) {
        focal_firm <- queue$firm[j]
        
        # determine selectivity of firm 
        if(focal_firm == f_google) {
          s1 <- par$s1_google
        } else
          s1 <- par$s1
        
        # unemployed pool is those within cultural threshold
        unemployed <- which(sims2$firm==0 & sims$firm!=focal_firm &
                              abs(sims2$culture - med_cult[focal_firm]) < 2*s1)
        
        # First check for new entry
        if (queue$draw[j] | length(unemployed)==0) {
          queue[j, culture := rnorm(1, med_cult[focal_firm], s1)]
          
        # Else draw random unemployed
        } else {
          chosen <- unemployed[sample.int(length(unemployed), 1)]
          sims2[chosen, `:=`(firm = focal_firm,
                             employments = employments + 1)]
          
          # update count if rehire is joining behemoth
          if (focal_firm == f_google) {
            stats[i+1, other_to_google := other_to_google + 1]
            
          # update count if rehire was previously at behemoth
          } else if (sims[chosen, firm] == f_google) {
            stats[i+1, google_to_other := google_to_other + 1]
            
          # update count if rehire is moving between other firms
          } else {
            stats[i+1, other_to_other := other_to_other + 1]
          }
        }
      }
      
      # Reset copy of df based on changes to population
      # Append all random entrants and remove all non-hires
      sims <- rbind(sims2[firm!=0],
                    queue[!is.na(culture), list(firm, culture, tenure, employments)])
      
    }
    
    ### SOCIALIZATION (no noise, no asymptotic socialization)

    # socialization for other firms
    sims[firm!=f_google, culture := culture + (med_cult[firm] - culture) *
            par$b1 * exp(- (par$b2 * (tenure-1)) - (par$b3 * (employments-1)))]
    
    # socialization for behemoth
    sims[firm==f_google, culture := culture + (med_cult[firm] - culture) *
           par$b1_google * exp(- (par$b2 * (tenure-1)) - (par$b3 * (employments-1)))]
    
    # Calculate statistics
    stats[i+1, var_win_other := mean(sims[firm!=f_google, sd(culture, na.rm=T), by=firm]$V1)]
    stats[i+1, var_win_google := sims[firm==f_google, sd(culture, na.rm=T)]]
    stats[i+1, c_google := sims[firm==f_google, median(culture, na.rm=T)]]
    stats[i+1, c_other := mean(sims[firm!=f_google, median(culture, na.rm=T), by=firm]$V1)]
  }
  
  ### Return summary statistics for each simulation run
  summary <- data.table(change_google = stats$c_google[t+1] - stats$c_google[1],
                        change_other = stats$c_other[t+1] - stats$c_other[1],
                        varwin_ratio_google = stats$var_win_google[t+1] / stats$var_win_google[1],
                        varwin_ratio_other = stats$var_win_other[t+1] / stats$var_win_other[1],
                        turnover_overall = mean(stats[2:t+1, (hires_other + hires_google)/(n*f + n_google)]),
                        turnover_google = mean(stats[2:t+1, hires_google/n_google]),
                        turnover_other = mean(stats[2:t+1, hires_other/(n*f)]),
                        carriers_overall = mean(stats[2:t+1, (other_to_google + google_to_other + other_to_other)/(hires_google + hires_other)]),
                        carriers_google = mean(stats[2:t+1, google_to_other/hires_other]),
                        random_entry_google = mean(stats[2:t+1, 1 - other_to_google/hires_google]),
                        random_entry_other = mean(stats[2:t+1, 1 - (google_to_other + other_to_other)/hires_other]),
                        tenure_end_google = median(sims[firm==f_google, tenure]),
                        emps_end_google = median(sims[firm==f_google, employments]),
                        tenure_end_other = median(sims[firm!=f_google, tenure]),
                        emps_end_other = median(sims[firm!=f_google, employments]))
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
