## Debugging no-interorg code
## Looping through step-by-step


setwd("~/Documents/GitHub/orgculture-ABM/")
library(data.table)
library(matrixStats)
library(parallel)
library(ggplot2)

### Set global parameters for simulations
f <- 30 # number of firms
n <- 30 # number of employees per firm [10, 100, 1000]
t <- 120 # number of time periods (months)

### Read in initializations
init_conds <- read.csv("data/initial_conditions.csv", header=T)
init_conds <- data.table(init_conds)

### Make data frame of varying parameter settings
par <- CJ(
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
culture_fn <- function() {
    
    sims2 <- data.table(sims)
    sims2[, tenure := tenure + 1]
    med_cult <- sims[, median(culture, na.rm=T), by=firm]$V1 ### PROBLEM HERE NEED TO REORDER
    
    
    # # SHORTENED VERSION (all three functions together)
    # draws <- runif(n*f)
    # sims2[, `:=`(culture = (culture*(draws > par$r0) + rnorm(n*f, med_cult[firm], par$s0)*(draws <= par$r0)),
    #              tenure = (tenure*(draws > par$r0) + 0),
    #              employments = (employments*(draws > par$r0) + ceiling(rlnorm(n*f,0,0.5))*(draws <= par$r0)))]
    
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
        h <- 1
        focal_firm <- queue$firm[h]
        
        # First check for random entry ##### USE RBIND
        sims2 <- rbind(sims2, list(focal_firm, rnorm(1, med_cult[focal_firm], par$s0),
                                   0, ceiling(rlnorm(1,0,0.5))))
        
        h <- h+1
      }
    }
    
    # Remove all non-hires
    sims <- sims2[firm!=0]
    
    # Calculate statistics
    stats[i+1, var_win := mean(sims[, sd(culture, na.rm=T), by=firm]$V1)]
    stats[i+1, var_btwn := sd(sims[, median(culture, na.rm=T), by=firm]$V1)]
    
    # Save plot
    p <- ggplot(sims, aes(group=firm, x=firm, y = culture)) +
      geom_boxplot() + scale_x_discrete() + xlab("Firm") + ylab("Culture")
    filename <- paste("plots/", Sys.Date(), "_nointerorg_",i,".png", sep="")
    ggsave(filename=filename, plot=p, units="in", width=3, height=3, pointsize=16)
    
    return(sims)
}


### Create local copy of initial conditions
sims <- init_conds[cond==par$cond, list(firm, culture, tenure, employments)]
i <- 1

### Initialize first plot
p <- ggplot(sims, aes(group=firm, x=firm, y = culture)) +
  geom_boxplot() + scale_x_discrete() + xlab("Firm") + ylab("Culture")
filename <- paste("plots/", Sys.Date(), "_nointerorg_",i,".png", sep="")
ggsave(filename=filename, plot=p, units="in", width=3, height=3, pointsize=16)

### Initialize stats
stats <- data.table(var_win=rep(0, t+1), var_btwn=0, hires=0, rehires=0)
stats[1, var_win := mean(sims[, sd(culture, na.rm=T), by=firm]$V1)]
stats[1, var_btwn := sd(sims[, median(culture, na.rm=T), by=firm]$V1)]

### RUN LOOP
sims <- culture_fn()
i <- i + 1

