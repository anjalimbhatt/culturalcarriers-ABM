setwd("~/Documents/GitHub/orgculture-ABM")
library(tidyverse)
library(data.table)
library(matrixStats)
library(parallel)

### Set global parameters for simulations
f <- 30 # number of firms
n <- 30 # number of employees per firm [10, 100, 1000]
t <- 1200 # number of time periods (months)
#var_win <- 0.3
n_paragon <- 300 # number of employees in paragon firm

### Initialize population
sims <- data.table(firm=rep(1:f, each=n),
                   culture=0.0,
                   tenure=rlnorm(n*f, 2.5, 1),
                   employments=ceiling(rlnorm(n*f,0,0.5)))
paragon <- data.table(firm=rep(f+1, each=n_paragon),
                      culture=2,
                      tenure=rlnorm(n_paragon, 2.5, 1),
                      employments=ceiling(rlnorm(n_paragon,0,0.5)))
sims <- rbind(sims, paragon)
n_emps <- nrow(sims)

par <- data.table(
  r0 = 0.03, # turnover base rate (3.5% monthly according to JOLTS)
  s0 = 0.03, # base rate of random entry
  b1 = 0.3, # initial socialization susceptibility
  b2 = 0.30, # speed of socialization susceptibility decline by tenure
  b3 = 0.10 # speed of socialization susceptibility decline by employments
)


### Initialize stats
stats <- data.table(var_win=rep(0, t+1), var_btwn=0, hires=0, rehires=0, mean_cult=0)
stats[1, var_win := mean(sims[firm!=31, sd(culture, na.rm=T), by=firm]$V1)]
stats[1, var_btwn := sd(sims[firm!=31, mean(culture, na.rm=T), by=firm]$V1)]
stats[1, mean_cult := mean(sims[firm!=31, mean(culture, na.rm=T), by=firm]$V1)]

### Create parameters by firm
firms <- data.table(firm=1:(f+1),
                    culture=sims[, mean(culture, na.rm=T), by=firm]$V1,
                    size=sims[, .N, by=firm]$N)

### Loop over months
for (i in 1:t) {
  
  sims2 <- data.table(sims)
  
  ### Turnover first
  # If no alienation, then assume only base turnover rate
  sims2[, firm := firm * (runif(n_emps) > par$r0)]
  
  #  Restart tenure clock for departed employees
  sims2[, tenure := (tenure+1) * (firm!=0)]
  
  ### Then hiring (recalculate firm culture)
  med_cult <- setorder(sims2[firm!=0, mean(culture, na.rm=T), by=firm], firm)$V1
  
  # Track how many hires made each period
  stats$hires[i+1] <- sum(sims2$firm==0)
  
  if (stats$hires[i+1] > 0) {
    # Set random order of hiring and prepopulate random entrant info
    queue <- data.table(firm = rep(0, stats$hires[i+1]),
                        culture = NA_real_,
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
      unemployed <- which(sims2$firm==0 & sims$firm!=focal_firm)
      
      # First check for paragon
      if (queue$firm[h]==31) {
        queue[h, culture := 2]
        
        # Then check for new entry
      } else if (queue$draw[h] | length(unemployed)==0) {
        queue[h, culture := med_cult[focal_firm]]
        
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
                  queue[!is.na(culture), list(firm, culture, tenure, employments)])
    
  }
  
  # ### Then socialization (recalculate firm culture)
  med_cult <- setorder(sims[, mean(culture, na.rm=T), by=firm], firm)$V1
  # # no noise, no asymptotic socialization
  sims[firm!=31, culture := culture + (med_cult[firm] - culture) *
         par$b1]
         # par$b1 * exp(- (par$b2 * (tenure-1)) - (par$b3 * (employments-1)))]
  
  # Calculate statistics
  stats[i+1, var_win := mean(sims[firm!=31, sd(culture, na.rm=T), by=firm]$V1)]
  stats[i+1, var_btwn := sd(sims[firm!=31, mean(culture, na.rm=T), by=firm]$V1)]
  stats[i+1, mean_cult := mean(sims[firm!=31, mean(culture, na.rm=T), by=firm]$V1)]
}

################## TRY WITH MEDIAN INSTEAD OF MEAN ##########################

# ### Initialize population
# sims <- data.table(firm=rep(1:f, each=n),
#                    culture=0.0,
#                    tenure=rlnorm(n*f, 2.5, 1),
#                    employments=ceiling(rlnorm(n*f,0,0.5)))
# paragon <- data.table(firm=rep(f+1, each=n_paragon),
#                       culture=2,
#                       tenure=rlnorm(n_paragon, 2.5, 1),
#                       employments=ceiling(rlnorm(n_paragon,0,0.5)))
# sims <- rbind(sims, paragon)
# n_emps <- nrow(sims)
# 
# par <- data.table(
#   r0 = 0.03, # turnover base rate (3.5% monthly according to JOLTS)
#   s0 = 0.03, # base rate of random entry
#   b1 = 0.3, # initial socialization susceptibility
#   b2 = 0.30, # speed of socialization susceptibility decline by tenure
#   b3 = 0.10 # speed of socialization susceptibility decline by employments
# )
# 
# ### Initialize stats
# stats <- data.table(var_win=rep(0, t+1), var_btwn=0, hires=0, rehires=0, mean_cult=0)
# stats[1, var_win := mean(sims[firm!=31, sd(culture, na.rm=T), by=firm]$V1)]
# stats[1, var_btwn := sd(sims[firm!=31, median(culture, na.rm=T), by=firm]$V1)]
# stats[1, mean_cult := mean(sims[firm!=31, median(culture, na.rm=T), by=firm]$V1)]
# 
# ### Create parameters by firm
# firms <- data.table(firm=1:(f+1),
#                     culture=sims[, median(culture, na.rm=T), by=firm]$V1,
#                     size=sims[, .N, by=firm]$N)
# 
# ### Loop over months
# for (i in 1:t) {
#   
#   sims2 <- data.table(sims)
#   
#   ### Turnover first
#   # If no alienation, then assume only base turnover rate
#   sims2[, firm := firm * (runif(n_emps) > par$r0)]
#   
#   #  Restart tenure clock for departed employees
#   sims2[, tenure := (tenure+1) * (firm!=0)]
#   
#   ### Then hiring (recalculate firm culture)
#   med_cult <- setorder(sims2[firm!=0, median(culture, na.rm=T), by=firm], firm)$V1
#   
#   # Track how many hires made each period
#   stats$hires[i+1] <- sum(sims2$firm==0)
#   
#   if (stats$hires[i+1] > 0) {
#     # Set random order of hiring and prepopulate random entrant info
#     queue <- data.table(firm = rep(0, stats$hires[i+1]),
#                         culture = NA_real_,
#                         tenure = 0,
#                         employments=ceiling(rlnorm(stats$hires[i+1],0,0.5)),
#                         draw = (runif(stats$hires[i+1]) <= par$s0))
#     j <- 1
#     for (k in 1:(f+1)) {
#       hires <- firms$size[k] - sum(sims2$firm==k)
#       if (hires>0) {
#         queue[j:(j + hires - 1), firm := k]
#         j <- j + hires
#       }
#     }
#     queue[, firm := firm[sample.int(length(firm))]]
#     
#     # Iterate over ordered hiring spots
#     for (h in 1:stats$hires[i+1]) {
#       focal_firm <- queue$firm[h]
#       
#       # unemployed pool is those within cultural threshold
#       unemployed <- which(sims2$firm==0 & sims$firm!=focal_firm)
#       
#       # First check for paragon
#       if (queue$firm[h]==31) {
#         queue[h, culture := 2]
#         
#         # Then check for new entry
#       } else if (queue$draw[h] | length(unemployed)==0) {
#         queue[h, culture := med_cult[focal_firm]]
#         
#         # Else draw random unemployed
#       } else {
#         chosen <- unemployed[sample.int(length(unemployed), 1)]
#         sims2[chosen, `:=`(firm = focal_firm,
#                            employments = employments + 1)]
#         stats[i+1, rehires := rehires + 1]
#       }
#     }
#     
#     # Append all random entrants and remove all non-hires
#     sims <- rbind(sims2[firm!=0],
#                   queue[!is.na(culture), list(firm, culture, tenure, employments)])
#     
#   }
#   
#   # ### Then socialization (recalculate firm culture)
#   med_cult <- setorder(sims[, median(culture, na.rm=T), by=firm], firm)$V1
#   # # no noise, no asymptotic socialization
#   # sims[firm!=31, culture := culture + (med_cult[firm] - culture) *
#   #        par$b1]
#   # par$b1 * exp(- (par$b2 * (tenure-1)) - (par$b3 * (employments-1)))]
#   
#   # Calculate statistics
#   stats[i+1, var_win := mean(sims[firm!=31, sd(culture, na.rm=T), by=firm]$V1)]
#   stats[i+1, var_btwn := sd(sims[firm!=31, median(culture, na.rm=T), by=firm]$V1)]
#   stats[i+1, mean_cult := mean(sims[firm!=31, median(culture, na.rm=T), by=firm]$V1)]
# }

# Plot (once per parameter set and initial condition)
test <- function(x) {1*(1-0.992725^x)} # theoretical simplified model (with soc)
#test_S <- function(x) {1/(1+0.992725^(x-94.93))} # logistic curve
test2 <- function(x) {1*(1-(0.992725)^(x/3.6))} # simple empirical with decaying soc
test3 <- function(x) {1*(1-exp(-x/600))} # empirical full sims result
test4 <- function(x) {1*(1-exp(-x/900))} # empirical w/ all except dist around paragon (delta instead)
stats %>%
  ggplot(aes(x=as.numeric(row.names(stats)), y=mean_cult/2)) +
  geom_point(size=1, alpha=0.3) +
  stat_function(fun=test, aes(color='1) exponential model')) +
  stat_function(fun=test2, aes(color='2) simple empirical with decaying soc')) +
  stat_function(fun=test3, aes(color='3) empirical full sims result')) +
  stat_function(fun=test4, aes(color='4) empirical w/ all except delta paragon')) +
  xlab('Time (months)') +
  ylab('Mean Firm Culture (w/o Paragon)')

stats %>%
  ggplot(aes(x=as.numeric(row.names(stats)), y=var_win)) +
  geom_point(size=1, alpha=0.3) +
  geom_point(aes(x=as.numeric(row.names(stats)), y=var_win),
             size=1, alpha=0.3, color='red', data=stats_soc)

sims2 %>% ggplot(aes(x=culture)) + geom_freqpoly()

stats_soc <- stats
