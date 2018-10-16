"
Cultural Transmission & Variation in Organizational Populations
Simulations over parameter grid and fixed initial conditions

Use source() to run code on server

Originally written March 2017
Recoded Apr 2018
Recoded for fixed initial conditions Jun 2018
Recoded for bug fixes Oct 2018

Hiring by satisficing vs. optimizing

@author: Anjali Bhatt
"

setwd("/afs/.ir/users/a/m/ambhatt/Git/orgculture-ABM/")
library(data.table)
library(matrixStats)
library(parallel)
library(ggplot2)

### Set global parameters for simulations
n_reps <- 5 # number of replications per set of parameters, per initial condition
f <- 30 # number of firms
n <- 30 # number of employees per firm [10, 100, 1000]
t <- 120 # number of time periods (months)

### Read in initializations
init_conds <- read.csv("data/initial_conditions_201810.csv", header=T)
init_conds <- data.table(init_conds)

### Make data frame of varying parameter settings
params <- CJ(
  optimize = c(0,1),
  cond = seq(1,30,3),
  rep_no = 1:n_reps,
  
  # socialization params (no noise)
  #b0 = 0.0, # asymptotic socialization susceptibility
  b1 = c(0.3), # initial socialization susceptibility
  b2 = 0.30, # speed of socialization susceptibility decline by tenure
  b3 = 0.10, # speed of socialization susceptibility decline by employments
  
  # turnover params
  r0 = c(0.03), # turnover base rate (3.5% monthly according to JOLTS)
  r1 = c(0.1), # turnover alienation rate
  r2 = c(0.05), # max increase in turnover probability
  
  # hiring params (no noise)
  s0 = c(0.03), # base rate of random entry
  s1 = seq(0.1,1.5,0.1) # hiring selectivity threshold
)

# deduplicate for iterations with functions off
# params[r2 == 0, c('r1') := 1]
# params <- unique(params)

### Define function for cultural evolution in population
culture_fn <- function(par) {
  
  ### Create local copy of initial conditions
  sims <- init_conds[cond==par$cond, list(firm, culture, tenure, employments)]
  
  ### Initialize stats
  stats <- data.table(var_win=rep(0, t+1), var_btwn=0, hires=0, rehires=0)
  stats[1, var_win := mean(sims[, sd(culture, na.rm=T), by=firm]$V1)]
  stats[1, var_btwn := sd(sims[, median(culture, na.rm=T), by=firm]$V1)]
  
  ### Loop over months
  for (i in 1:t) {
    
    sims2 <- data.table(sims)
    med_cult <- setorder(sims[, median(culture, na.rm=T), by=firm], firm)$V1
    
    ### Turnover first
    # If no alienation, then assume only base turnover rate
    # Otherwise, retention is modeled as a gaussian shape
    sims2[, firm := firm * (runif(n*f) > ((par$r0 + par$r2) - par$r2 * par$r1 *
                                            sqrt(2*pi) * (dnorm(culture, med_cult[firm], par$r1))))]
    
    #  Restart tenure clock for departed employees
    sims2[, tenure := (tenure+1) * (firm!=0)]
    
    ### Then socialization
    # no noise, no asymptotic socialization
    sims2[firm!=0, culture := culture + (med_cult[firm] - culture) *
            par$b1 * exp(- (par$b2 * (tenure-1)) - (par$b3 * (employments-1)))]
    
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
      for (k in 1:f) {
        hires <- n - sum(sims2$firm==k)
        if (hires>0) {
          queue[j:(j + hires - 1), firm := k]
          j <- j + hires
        }
      }
      queue[, firm := firm[sample.int(length(firm))]]
      
      if (par$optimize==1) {
        # Iterate over ordered hiring spots
        for (h in 1:stats$hires[i+1]) {
          focal_firm <- queue$firm[h]
          unemployed <- which(sims2$firm==0 & sims$firm!=focal_firm)
          
          # First check for random entry
          if (queue$draw[h] | length(unemployed)==0) {
            queue[h, culture := rnorm(1, med_cult[focal_firm], par$s1)]
            
            # If no selectivity, then draw random unemployed
          } else if (par$select==0) {
            chosen <- unemployed[sample.int(length(unemployed), 1)]
            sims2[chosen, `:=`(firm = focal_firm,
                               employments = employments + 1)]
            stats[i+1, rehires := rehires + 1]
            
            # Hire the now-unemployed person with the best cultural fit within the threshold
            # Otherwise hire outside the existing pool
          } else if ((min(abs(sims2$culture[unemployed] - med_cult[focal_firm]))) >= 2*par$s1) {
            queue[h, culture := rnorm(1, med_cult[focal_firm], par$s1)]
            
          } else {
            chosen <- unemployed[which.min(abs(sims2$culture[unemployed] - med_cult[focal_firm]))]
            sims2[chosen, `:=`(firm = focal_firm,
                               employments = employments + 1)]
            stats[i+1, rehires := rehires + 1]
          }
        }
      }
      
      if (par$optimize==0) {
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
      }
      
      # Append all random entrants and remove all non-hires
      sims <- rbind(sims2[firm!=0],
                    queue[culture!=0, list(firm, culture, tenure, employments)])
      
    }
    
    # Calculate statistics
    stats[i+1, var_win := mean(sims[, sd(culture, na.rm=T), by=firm]$V1)]
    stats[i+1, var_btwn := sd(sims[, median(culture, na.rm=T), by=firm]$V1)]
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
}, mc.cores=30)
global_stats <- Reduce(rbind, mc_stats)

### Write simulation results to csv file
filename = paste("data/", Sys.Date(), "_results_detailedhiring.csv", sep="")
write.csv(global_stats, file=filename)
