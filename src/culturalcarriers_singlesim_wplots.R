"
Cultural Transmission & Variation in Organizational Populations
Simulations over parameter grid and fixed initial conditions
Use source() to run code on server
Originally written March 2017
Recoded Apr 2018
Recoded for fixed initial conditions Jun 2018
***Only for simulating WITHOUT inter-org mobility***
*** SINGLE SIMULATION PLOTTING ***
@author: Anjali Bhatt
"

setwd("~/Documents/GitHub/orgculture-ABM/")
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
init_conds <- read.csv("data/initial_conditions.csv", header=T)
init_conds <- data.table(init_conds)

### Make data frame of varying parameter settings
params <- CJ(
  cond = 1,
  
  # socialization params (no noise)
  #b0 = 0.0, # asymptotic socialization susceptibility
  # b1 = 0.0, # initial socialization susceptibility
  # b2 = 0.30, # speed of socialization susceptibility decline by tenure
  # b3 = 0.10, # speed of socialization susceptibility decline by employments
  
  # turnover params
  alienate = 0, # alienation on
  r0 = 0.05, # turnover base rate (3.5% monthly according to JOLTS)
  # r1 = 10, # turnover alienation rate
  # r2 = 0.06, # max turnover probability
  
  # hiring params (no noise)
  select = c(1), # selectivity on
  s0 = 0.1, # hiring selectivity threshold
  s1 = c(1) # base rate of random entry
)

# deduplicate for iterations with functions off
# params[alienate == 0, c('r1','r2') := NA]
# params[select == 0, c('s0') := 10]
# params <- unique(params)

### Define function for cultural evolution in population
culture_fn <- function(par) {
  
  ### Create local copy of initial conditions
  sims <- init_conds[cond==par$cond, list(firm, culture, tenure, employments)]
  
  
  ### Initialize plots
  plot_list = list()
  p <- ggplot(sims, aes(group=firm, x=firm, y = culture)) +
    geom_boxplot() + scale_x_discrete() + xlab("Firm") + ylab("Culture")
  plot_list[[1]] <- p
  
  ### Initialize stats
  stats <- data.table(var_win=rep(0, t+1), var_btwn=0, hires=0, rehires=0)
  stats[1, var_win := mean(sims[, sd(culture, na.rm=T), by=firm]$V1)]
  stats[1, var_btwn := sd(sims[, median(culture, na.rm=T), by=firm]$V1)]
  
  ### Loop over months
  for (i in 1:t) {
    
    sims2 <- data.table(sims)
    sims2[, tenure := tenure + 1]
    med_cult <- sims[, median(culture, na.rm=T), by=firm]$V1
    
    
    # SHORTENED VERSION (all three functions together)
    draws <- runif(n*f)
    sims2[, `:=`(culture = (culture*(draws > par$r0) + rnorm(n*f, med_cult[firm], par$s0)*(draws <= par$r0)),
                 tenure = (tenure*(draws > par$r0) + 0),
                 employments = (employments*(draws > par$r0) + ceiling(rlnorm(n*f,0,0.5))*(draws <= par$r0)))]
    
    ### Turnover first
    # If no alienation, then assume only base turnover rate
    sims2[, firm := 0 + firm * (runif(n*f) > par$r0)]
    
    
    ### Then socialization
    
    ### Then hiring
    # Track how many hires made each period
    stats$hires[i+1] <- sum(sims2$firm==0)

    if (stats$hires[i+1] > 0) {
      # Set random order of hiring and prepopulate random entry draw
      queue <- data.table(firm = rep(0, stats$hires[i+1]))
      j <- 1
      for (k in 1:f) {
        hires <- n - sum(sims2$firm==k)
        if (hires>0) {
          queue[j:(j + hires - 1), firm := k]
          j <- j + hires
        }
      }

      # Iterate over ordered hiring spots
      for (h in 1:stats$hires[i+1]) {
        focal_firm <- queue$firm[h]

        # First check for random entry
        sims2 <- rbind(sims2, list(focal_firm, rnorm(1, med_cult[focal_firm], par$s0),
                                   0, ceiling(rlnorm(1,0,0.5))))
      }
    }

    # Remove all non-hires
    sims <- sims2[firm!=0]
    
    # Calculate statistics
    stats[i+1, var_win := mean(sims[, sd(culture, na.rm=T), by=firm]$V1)]
    stats[i+1, var_btwn := sd(sims[, median(culture, na.rm=T), by=firm]$V1)]
    
    # Save plots
    p <- ggplot(sims, aes(group=firm, x=firm, y = culture)) +
                      geom_boxplot() + scale_x_discrete() + xlab("Firm") + ylab("Culture")
    plot_list[[i+1]] <- p
  }
  

  for (p in 1:i+1) {
    filename <- paste("plots/", Sys.Date(), "_nointerorg_",p,".png", sep="")
    png(filename=filename, units="in", width=3, height=3, pointsize=16, res=256)
    print(plot_list[[p]])
    dev.off()
  }
  
  # Plot (once per parameter set and initial condition)
  plot <- ggplot(stats, aes(x=var_win, y=var_btwn)) +
    geom_point(size=3, shape=21, alpha=0.8, color="black", aes(fill=as.numeric(row.names(stats)))) +
    theme_bw() + scale_fill_gradient(low = "yellow", high = "red") + labs(fill="month")

  name <- paste("plots/", Sys.Date(), "_soc", par$b1, "_turnover", par$r0,
                "_select", par$s0, "_random", par$s1, "_cond", par$cond,
                ".png", sep="")
  ggsave(filename=name, plot=plot, units="in", width=6, height=6, pointsize=16)
  
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

set.seed(39285)
culture_fn(params)

