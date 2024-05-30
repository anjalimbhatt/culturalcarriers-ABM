setwd("~/Documents/GitHub/orgculture-ABM")
library(tidyverse)
library(data.table)
library(matrixStats)
library(parallel)

### Set global parameters for simulations
f <- 30 # number of firms
n <- 30 # number of employees per firm [10, 100, 1000]
t <- 2400 # number of time periods (months)

init_conds <- read.csv("data/initial_conditions_hetero_201906.csv", header=T)
init_conds <- init_conds %>% data.table()

params <- CJ(
  cond = unique(init_conds$cond),
  
  # socialization params (no noise)
  #b0 = 0.0, # asymptotic socialization susceptibility
  b1 = 0.3, # initial socialization susceptibility
  b2 = 0.30, # speed of socialization susceptibility decline by tenure
  b3 = 0.10, # speed of socialization susceptibility decline by employments
  
  # turnover params
  r0 = 0.03, # turnover base rate (3.5% monthly according to JOLTS)
  r1 = 1, # turnover alienation rate
  r2 = 0.05, # max increase in turnover probability
  
  # hiring params (no noise)
  s0 = 0.03, # base rate of random entry
  s1 = 1, # hiring selectivity threshold
  
  # org heterogeneity
  # n_tight = c(1,2), # number of tight organizations
  # b1_tight = 0.6,
  r1_tight = 0.1
  # s1_tight = 0.1
)

par <- params[2,]

sims <- init_conds[cond==par$cond, list(firm, culture, tenure, employments)]
n_emps <- nrow(sims)

### Initialize stats
stats <- data.table(var_win=rep(0, t+1), var_btwn=0, hires=0, rehires=0, mean_cult=0)
stats[1, var_win := mean(sims[, sd(culture, na.rm=T), by=firm]$V1)]
stats[1, var_btwn := sd(sims[, median(culture, na.rm=T), by=firm]$V1)]
stats[1, mean_cult := mean(sims[firm!=31, median(culture, na.rm=T), by=firm]$V1)]

### Create parameters by firm
firms <- data.table(firm=1:(f+1),
                    culture=sims[, median(culture, na.rm=T), by=firm]$V1,
                    b1=par$b1,
                    r1=par$r1,
                    s1=par$s1,
                    size=sims[, .N, by=firm]$N)

### Change cultural management for selected firms (linked to init conds)
firms[(f+1), r1 := par$r1_tight]
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
  # sims2[, firm := firm * (runif(n_emps) > par$r0)]
  sims2[, firm := firm * (runif(n_emps) > ((par$r0 + par$r2) - par$r2 * firms$r1[firm] *
                                          sqrt(2*pi) * (dnorm(culture, med_cult[firm], firms$r1[firm]))))]
  
  #  Restart tenure clock for departed employees
  sims2[, tenure := (tenure+1) * (firm!=0)]
  
  ### Then hiring (recalculate firm culture)
  med_cult <- setorder(sims2[firm!=0, median(culture, na.rm=T), by=firm], firm)$V1
  
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
      unemployed <- which(sims2$firm==0 & sims$firm!=focal_firm &
                            abs(sims2$culture - med_cult[focal_firm]) < 2*firms$s1[focal_firm])
      
      # First check for paragon
      if (queue$firm[h]==31) {
        queue[h, culture := 2]
        
      # Then check for new entry
      } else if (queue$draw[h] | length(unemployed)==0) {
        queue[h, culture := rnorm(1, med_cult[focal_firm], firms$s1[focal_firm])]
        # queue[h, culture := med_cult[focal_firm]]
        
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
                  #queue[!is.na(culture), list(firm, culture)])
                  queue[!is.na(culture), list(firm, culture, tenure, employments)])
    
  }
  
  ### Then socialization (recalculate firm culture)
  med_cult <- setorder(sims[, median(culture, na.rm=T), by=firm], firm)$V1
  # no noise, no asymptotic socialization
  sims[firm!=31, culture := culture + (med_cult[firm] - culture) *
         firms$b1[firm] * exp(- (par$b2 * (tenure-1)) - (par$b3 * (employments-1)))]
  
  # Calculate statistics
  stats[i+1, var_win := mean(sims[, sd(culture, na.rm=T), by=firm]$V1)]
  stats[i+1, var_btwn := sd(sims[, median(culture, na.rm=T), by=firm]$V1)]
  stats[i+1, mean_cult := mean(sims[firm!=31, median(culture, na.rm=T), by=firm]$V1)]
}

# Plot (once per parameter set and initial condition)
test <- function(x) {.9*(1-exp(-x/94.93)) + .1} # theoretical simplified model
test2 <- function(x) {.9*(1-exp(-x/600)) + .1} # empirical full sims result
test3 <- function(x) {.9*(1-exp(-x/1200)) + .1} # empirical w/ socialization but no dist, alien
test4 <- function(x) {.9*(1-exp(-x/900)) + .1} # empirical w/ all except dist around paragon (delta instead)
test5 <- function(x) {.9*(tanh(x/600)) + .1} # empirical w/ all except dist around paragon (delta instead)
stats %>%
  ggplot(aes(x=as.numeric(row.names(stats)), y=mean_cult/2)) +
  geom_point(size=0.5, alpha=0.1) +
  stat_function(fun=test, aes(color='1) simple model')) +
  stat_function(fun=test2, aes(color='4) full empirical')) +
  stat_function(fun=test3, aes(color='2) simple model + socialization')) +
  stat_function(fun=test4, aes(color='3) empirical w/ delta distribution')) +
  stat_function(fun=test5, aes(color='5) tanh')) +
  xlab('Time (months)') +
  ylab('Mean Firm Culture (w/o Paragon)') +
  xlim(c(0,300))

test_S <- function(x) {1/(1+exp((94.93-x)/139.9564))} # logistic curve
