"
Cultural Transmission & Variation in Organizational Populations
Simulations over parameter grid and fixed initial conditions

Use source() to run code on server

Originally written March 2017
Recoded Apr 2018
Recoded for fixed initial conditions Jun 2018
Recoded for bug fixes Oct 2018
Adapted for org heterogeneity April 2019

Hiring by satisficing instead of optimizing
Left order as turnover > socialization > hiring (may switch later)

@author: Anjali Bhatt
"

#setwd("/afs/.ir/users/a/m/ambhatt/Git/orgculture-ABM/")
setwd("~/Documents/GitHub/orgculture-ABM")
library(tidyverse)
library(data.table)
library(matrixStats)
library(parallel)

### Set global parameters for simulations
n_reps <- 3 # number of replications per set of parameters, per initial condition
f <- 30 # number of firms
n <- 30 # number of employees per firm [10, 100, 1000]
t <- 120 # number of time periods (months)

### Read in initializations (subset)
init_conds <- read.csv("data/initial_conditions_hetero_201904.csv", header=T)
init_conds <- init_conds %>% data.table()

### Make data frame of varying parameter settings
params <- CJ(
  cond = unique(init_conds$cond),
  rep_no = 1:n_reps,
  
  # socialization params (no noise)
  #b0 = 0.0, # asymptotic socialization susceptibility
  b1 = 0.3, # initial socialization susceptibility
  b2 = 0.30, # speed of socialization susceptibility decline by tenure
  b3 = 0.10, # speed of socialization susceptibility decline by employments
  
  # turnover params
  r0 = 0.03, # turnover base rate (3.5% monthly according to JOLTS)
  r1 = c(0.1,1), # turnover alienation rate
  r2 = 0.05, # max increase in turnover probability
  
  # hiring params (no noise)
  s0 = 0.03, # base rate of random entry
  s1 = 1, # hiring selectivity threshold
  
  # org heterogeneity
  #n_tight = c(1,2), # number of tight organizations
  b1_tight = 0.6,
  r1_tight = 0.1,
  s1_tight = 0.1
)

# deduplicate for iterations with functions off
# params[r2 == 0, c('r1') := 1]
# params <- unique(params)

### Define function for cultural evolution in population
culture_fn <- function(par) {
  
  ### Create local copy of initial conditions
  sims <- init_conds[cond==par$cond, list(firm, culture, tenure, employments)]
  n_emps <- nrow(sims)
  
  ### Initialize stats
  stats <- data.table(var_win=rep(0, t+1), var_btwn=0, hires=0, rehires=0, mean_cult=0)
  stats[1, var_win := mean(sims[, sd(culture, na.rm=T), by=firm]$V1)]
  stats[1, var_btwn := sd(sims[, median(culture, na.rm=T), by=firm]$V1)]
  stats[1, mean_cult := mean(sims[, median(culture, na.rm=T), by=firm]$V1)]
  
  ### Create parameters by firm
  firms <- data.table(firm=1:(f+1),
                      culture=sims[, median(culture, na.rm=T), by=firm]$V1,
                      b1=par$b1,
                      r1=par$r1,
                      s1=par$s1,
                      size=sims[, .N, by=firm]$N)
  
  ### Change cultural management for selected firms (linked to init conds)
  firms[(f+1), c('b1', 'r1', 's1') := .(par$b1_tight, par$r1_tight, par$s1_tight)]
  # if(par$n_tight==2) {
  #   firms[2, c('b1', 'r1', 's1') := .(par$b1_tight, par$r1_tight, par$s1_tight)]
  # }
  
  ### Loop over months
  for (i in 1:t) {
    
    sims2 <- data.table(sims)
    med_cult <- setorder(sims[, median(culture, na.rm=T), by=firm], firm)$V1
    
    ### Turnover first
    # If no alienation, then assume only base turnover rate
    # Otherwise, retention is modeled as a gaussian shape
    sims2[, firm := firm * (runif(n_emps) > ((par$r0 + par$r2) - par$r2 * firms$r1[firm] *
                                            sqrt(2*pi) * (dnorm(culture, med_cult[firm], firms$r1[firm]))))]

    #  Restart tenure clock for departed employees
    sims2[, tenure := (tenure+1) * (firm!=0)]
    
    ### Then socialization
    # no noise, no asymptotic socialization
    sims2[firm!=0, culture := culture + (med_cult[firm] - culture) *
            firms$b1[firm] * exp(- (par$b2 * (tenure-1)) - (par$b3 * (employments-1)))]
    
    ### Then hiring
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
      for (k in 1:(f+1)) {
        hires <- firms$size[k] - sum(sims2$firm==k)
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
                              abs(sims2$culture - med_cult[focal_firm]) < 2*firms$s1[focal_firm])
        
        # First check for new entry
        if (queue$draw[h] | length(unemployed)==0) {
          queue[h, culture := rnorm(1, med_cult[focal_firm], firms$s1[focal_firm])]
          
          # Else draw random unemployed
        } else {
          chosen <- unemployed[sample.int(length(unemployed), 1)]
          sims2[chosen, `:=`(firm = focal_firm,
                             employments = employments + 1)]
          stats[i+1, rehires := rehires + 1]
        }
      }
      
      # Append all random entrants and remove all non-hires
      sims <- rbind(sims2[firm!=0],
                    queue[culture!=0, list(firm, culture, tenure, employments)])
      
    }
    
    # Calculate statistics
    stats[i+1, var_win := mean(sims[, sd(culture, na.rm=T), by=firm]$V1)]
    stats[i+1, var_btwn := sd(sims[, median(culture, na.rm=T), by=firm]$V1)]
    stats[i+1, mean_cult := mean(sims[, median(culture, na.rm=T), by=firm]$V1)]
  }
  
  # # Plot (once per parameter set and initial condition)
  # plot <- ggplot(stats, aes(x=var_win, y=var_btwn)) +
  #   geom_point(size=3, shape=21, alpha=0.8, color="black", aes(fill=as.numeric(row.names(stats)))) +
  #   theme_bw() + scale_fill_gradient(low = "yellow", high = "red") + labs(fill="month")
  # 
  # name <- paste("plots/", Sys.Date(), "_soc", par$b1, "_turnover", par$r0,
  #               "_select", par$s0, "_random", par$s1, "_cond", par$cond,
  #               ".png", sep="")
  # ggsave(filename=name, plot=plot, units="in", width=6, height=6, pointsize=16)
  
  ### Return summary statistics for each simulation run
  summary <- data.table(varbtwn_start = stats$var_btwn[1],
                        varwin_start = stats$var_win[1],
                        meancult_start = stats$mean_cult[1],
                        varbtwn_end = stats$var_btwn[t+1],
                        varwin_end = stats$var_win[t+1],
                        meancult_end = stats$mean_cult[t+1],
                        turnover = mean(stats$hires)/(n_emps),
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
}, mc.cores=2)
global_stats <- Reduce(rbind, mc_stats)

### Write simulation results to csv file
filename = paste("data/", Sys.Date(), "_results_orghetero_alternatehiring.csv", sep="")
write.csv(global_stats, file=filename)
