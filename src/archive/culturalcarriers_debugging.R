## Debugging no-interorg code
## Looping through step-by-step


setwd("~/Documents/GitHub/orgculture-ABM/")
library(data.table)
library(matrixStats)
library(parallel)
library(ggplot2)

### Set global parameters for simulations
n_reps <- 1 # number of replications per set of parameters, per initial condition
f <- 30 # number of firms
n <- 30 # number of employees per firm [10, 100, 1000]
t <- 120 # number of time periods (months)

### Read in initializations
init_conds <- read.csv("data/initial_conditions_201810_plots.csv", header=T)
init_conds <- data.table(init_conds)

### Make data frame of varying parameter settings
par <- CJ(
  cond = 4,
  rep_no = 1,
  
  # socialization params (no noise)
  #b0 = 0.0, # asymptotic socialization susceptibility
  b1 = c(0.3), # initial socialization susceptibility
  b2 = 0.30, # speed of socialization susceptibility decline by tenure
  b3 = 0.10, # speed of socialization susceptibility decline by employments
  
  # turnover params
  r0 = 0.03, # turnover base rate (3.5% monthly according to JOLTS)
  r1 = c(0.1), # turnover alienation rate
  r2 = c(0.03), # max increase in turnover probability
  
  # hiring params (no noise)
  select = 1, # selectivity on
  s0 = c(1), # base rate of random entry
  s1 = c(0.1) # hiring selectivity threshold
)

### Define function for cultural evolution in population
culture_fn <- function(sims) {
  
    sims2 <- data.table(sims)
    med_cult <- setorder(sims[, median(culture, na.rm=T), by=firm], firm)$V1

    # # SHORTENED VERSION (all three functions together) for NO MOBILITY ONLY (par$s0 not considered)
    # # NOT TOTALLY SURE SOCIALIZATION WORKS PROPERLY HERE...
    # sims2[, tenure := (tenure + 1) * (runif(n*f) > ((par$r0 + par$r2) - par$r2 * par$r1 *
    #                                  sqrt(2*pi) * (dnorm(culture, med_cult[firm], par$r1))))]
    # 
    # sims2[, culture := (tenure>0) * (culture + (med_cult[firm] - culture) * par$b1 *
    #                                 exp(- (par$b2 * (tenure-1)) - (par$b3 * (employments-1)))) +
    #                   (tenure==0) * rnorm(n*f, med_cult[firm], par$s1)]
    # 
    # sims2[, employments := (tenure>0) * employments +
    #                       (tenure==0) * ceiling(rlnorm(n*f,0,0.5))]
    
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
      # queue[, firm := firm[sample.int(length(firm))]]
      
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
      
      # Append all random entrants and remove all non-hires
      sims <- rbind(sims2[firm!=0],
                    queue[culture!=0, list(firm, culture, tenure, employments)])
      
    }
    
    # sims <- data.table(sims2)
    
    
    
    # Calculate statistics
    stats[i+1, var_win := mean(sims[, sd(culture, na.rm=T), by=firm]$V1)]
    stats[i+1, var_btwn := sd(sims[, median(culture, na.rm=T), by=firm]$V1)]
    
    # Save plot
    p <- ggplot(sims, aes(group=firm, x=firm, y = culture)) +
      geom_point(size=0.5) + xlab("Firm") + ylab("Culture")
    filename <- paste("plots/", Sys.Date(), "_nointerorg_",i,".png", sep="")
    ggsave(filename=filename, plot=p, units="in", width=3, height=3, pointsize=16)
    
    return(sims)
}


### Create local copy of initial conditions
actualsims <- init_conds[cond==par$cond, list(firm, culture, tenure, employments)]
i <- 1

### Initialize stats
stats <- data.table(var_win=rep(0, t+1), var_btwn=0, hires=0, rehires=0)
stats[1, var_win := mean(actualsims[, sd(culture, na.rm=T), by=firm]$V1)]
stats[1, var_btwn := sd(actualsims[, median(culture, na.rm=T), by=firm]$V1)]

# REPEAT TO LOOP
actualsims <- culture_fn(actualsims)
i <- i + 1
