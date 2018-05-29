# -*- coding: utf-8 -*-
"""
Cultural Transmission & Variation in Organizational Populations
Full set of simulations over parameter grid

Originally written in R in March 2017
Recoded in Python in Apr 2018

@author: Anjali Bhatt
"""

from time import time
import pandas as pd
import multiprocessing as mp

### Set global parameters for simulations

# functions
def culture_fn(par):
    
    return df.values.tolist() # some list
    
def collect_results(result):
    results.extend(result)
    

# script to run
if __name__ == "__main__":
    start_time = time()
    
    # set global parameters (number of simulations, firms, employees per firm)
    globals = pd.DataFrame({'n_sims': 100, 'f': 30, 'n':30, 'key':1})
    
    # set socialization parameters
    params = pd.DataFrame({'n_sims': 100, 'b2': 0.3, 'b3': 0.1, 'eps':0.1, 'key':1})
    b = # [0,1] socialization on/off
    b0 = 0.02 # asymptotic socialization susceptibility
    #b1 = 0.60 # initial socialization susceptibility, varying [0.3, 2]
    b2 = 0.30 # speed of susceptibility decline by tenure [0.1, 0.5]
    b3 = 0.10 # speed of susceptibility decline by employments
    eps = 0.1 # error term for socialization (standard deviation)
    
    # set hiring parameters
    params2 =
    s [0,1] selectivity on/off
    #s = 1 # hiring selectivity threshold, varying [0.1, 10]
    eps2 = 0.1 # error term for hiring (standard deviation)
    
    # set turnover parameters
    params3 = pd.merge(pd.DataFrame({'r0':[0.006, 0.01, 0.03, 0.06, 0.1], 'r2':}))
    #r0 = 0.01 # base rate of turnover, varying [0, 0.1]
    #r1 = 0.3 # alienation rate (for turnover), varying [0.1, 10]
    r2 = 0.30 # max probability of turnover
    
    params = pd.merge(globals, params1, params2, params3).drop('key', axis=1)
    
    pool = mp.Pool(processes=mp.cpu_count())
    
    for par in params: 
        pool.apply_async(culture_fn, args=(par, ), callback=collect_results)
    
    pool.close()
    pool.join()
    
    # converts list of lists to data frame
    df = pd.DataFrame(results)
    print("--- %s seconds ---" % (time() - start_time))

### Make data frame of varying parameter settings
params = df
params.columns = []
# populate params


### Set global parameters for simulations
n.sims <- 100 # number of simulations per set of parameters
f <- 30 # number of firms
n <- 30 # number of employees per firm [10, 100, 1000]
t <- 500 # max number of time periods (months)

b0 <- 0.02 # asymptotic socialization susceptibility [0, 0.3]

b2 <- 0.30 # speed of susceptibility decline by tenure [0.1, 0.5]
b3 <- 0.10 # speed of susceptibility decline by employments
eps <- 0.1 # error term for socialization (standard deviation)
eps2 <- 0.1 # error term for hiring (standard deviation)

r2 <- 0.30 # max probability of turnover
# varying r0 <- 0.01 # base rate of turnover [0, 0.1]
# varying r1 <- 0.3 # alienation rate (for turnover) [0.1, 10]
# varying s <- 1 # hiring selectivity (threshold for upper and lower cultural distance) [0.1, 10]

socialization on/off
hiring selectivity on/off

### Write simulation results to csv file

df = pd.read_csv(response)
results = []



# Use source() in terminal to run code
setwd("/afs/.ir.stanford.edu/users/a/m/ambhatt/Culture")
library(data.table)
library(matrixStats)
library(parallel)

### Modeling org culture development at nascent firms

### Set global parameters for simulations
n.sims <- 100 # number of simulations per set of parameters
f <- 30 # number of firms
n <- 30 # number of employees per firm [10, 100, 1000]
t <- 500 # max number of time periods (months)

b0 <- 0.02 # asymptotic socialization susceptibility [0, 0.3]
# varying b1 <- 0.60 # initial socialization susceptibility [0.3, 2]
b2 <- 0.30 # speed of susceptibility decline by tenure [0.1, 0.5]
b3 <- 0.10 # speed of susceptibility decline by employments
eps <- 0.1 # error term for socialization (standard deviation)
eps2 <- 0.1 # error term for hiring (standard deviation)

r2 <- 0.30 # max probability of turnover
# varying r0 <- 0.01 # base rate of turnover [0, 0.1]
# varying r1 <- 0.3 # alienation rate (for turnover) [0.1, 10]
# varying s <- 1 # hiring selectivity (threshold for upper and lower cultural distance) [0.1, 10]

### Define function for cultural evolution in population
culture.fn <- function(par) {
  
  ### Initialize firm culture
  init.firm.cult <- rnorm(f)
  
  ### Initialize employees' culture and tenure
  sims <- data.table(firm=rep(1:f, each=n), culture=0.0, tenure=rlnorm(n*f, 2.5, 1), employments=0.0)
  sims$culture <- rnorm(n*f, init.firm.cult[sims$firm], par$s)
  
  ### Initialize stats
  stats <- (as.data.table(array(0, dim=c(t+1,3))))
  colnames(stats) <- c("var.firm.cult", "attrition", "rehires")
  stats$var.firm.cult[1] <- var(sims[, mean(culture, na.rm=T), by=firm]$V1)
  sims$firm = factor(sims$firm, labels = 1:f)
  anova <- anova(lm(culture ~ firm, data=sims))
  init.stats <- cbind(var.btwn.init=stats$var.firm.cult[1],
                      var.win.init=anova$`Mean Sq`[[2]],
                      var.emp.cult.init=var(sims$culture),
                      mean.emp.cult.init=mean(sims$culture),
                      f.init=anova$`F value`[[1]],
                      f.sig.init=anova$`Pr(>F)`[[1]])
  
  ### Run simulations
  for (i in 1:t) {
    
    sims2 <- data.table(sims)
    sims2$tenure <- sims$tenure + 1
    mean.cult <- sims[, mean(culture, na.rm=T), by=firm]$V1
    
    # Attrition first (restart tenure clock)
    # Retention is modeled as a power function
    # sims2$firm <- 0 + (sims2$firm*(runif(nrow(sims2)) > par$r0 + par$r1*((sims$culture - mean.cult[sims$firm])/(2*par$s))^4 ))
    # Retention is modeled as a gaussian shape
    sims2$firm <- 0 + (as.numeric(sims2$firm)*(runif(nrow(sims2)) > (r2 - (r2-par$r0)*(par$r1)*sqrt(2*pi)*(dnorm(sims$culture, mean.cult[sims$firm], par$r1)))))
    sims2$tenure <- 0 + sims2$tenure*(sims2$firm!=0)
    
    # Then socialization
    sims2$culture <- sims$culture + (sims2$firm!=0) *
      (mean.cult[sims$firm] - sims$culture + rnorm(nrow(sims2), 0, eps)) * (b0 + exp(-par$b1-(b2*sims$tenure)-(b3*sims$employments)))
    
    # Then hiring
    hires <- matrix(0, f)
    for (k in 1:f) {
      hires[k] <- n - sum(sims2$firm==k)
    }
    stats$attrition[i+1] <- sum(hires)

    # Hire in random order
    while (sum(abs(hires))>0) {
      if (length(which(hires>0))==1) {
        focal.firm <- which(hires>0)
      } else {
        focal.firm <- sample(which(hires>0),1)
      }

      # Hire the now-unemployed person with the best cultural fit within the threshold
      # Otherwise hire outside the existing pool
      unemployed <- which(sims2$firm==0 & sims$firm!=focal.firm)
      if (length(unemployed)==0 | min(abs(sims2$culture[unemployed]-mean.cult[focal.firm])) >= 2*par$s*(1+rnorm(1,0,eps2))) {
        sims2 <- rbind(sims2, list(focal.firm, rnorm(1, mean.cult[focal.firm], par$s), 0, 0))
      } else {
        chosen <- unemployed[which.min(abs(sims2$culture[unemployed]-mean.cult[focal.firm]))]
        sims2$firm[chosen] <- focal.firm
        sims2$employments[chosen] <- sims2$employments[chosen] + 1
        stats$rehires[i+1] <- stats$rehires[i+1] + 1
      }

      hires[focal.firm] <- hires[focal.firm] - 1
    }
    
    # Remove all non-hires
    sims <- subset(sims2, !(firm==0))
    
    # Calculate statistics
    stats$var.firm.cult[i+1] <- var(sims[, mean(culture, na.rm=T), by=firm]$V1)
    
    # Break if no change in statistics
    if (i>50) {
      if(abs(stats$var.firm.cult[i+1]-stats$var.firm.cult[i-25]) < 0.01 &
         abs(stats$var.firm.cult[i+1]-stats$var.firm.cult[i-50]) < 0.01) {
        break
      }
    }
    
  }
  
  ### Return initial and final statistics
  sims$firm = factor(sims$firm, labels = 1:30)
  anova <- anova(lm(culture ~ firm, data=sims))
  final.stats <- cbind(var.btwn=stats$var.firm.cult[i+1],
                      var.win=anova$`Mean Sq`[[2]],
                      var.emp.cult=var(sims$culture),
                      mean.emp.cult=mean(sims$culture),
                      f=anova$`F value`[[1]],
                      f.sig=anova$`Pr(>F)`[[1]],
                      t.eq=i)
  summary <- cbind(init.stats, final.stats,
                   init.turnover=mean(stats$attrition[2:25]), init.frac.rehires=sum(stats$rehires[2:25])/sum(stats$attrition[2:25]),
                   eq.turnover=mean(stats$attrition[i-25:i+1]), eq.frac.rehires=sum(stats$rehires[i-25:i+1])/sum(stats$attrition[i-25:i+1]))
  return(summary)
  
}

### Make data frame of varying parameter settings
params <- CJ(r0=c(0.006, 0.01, 0.03, 0.06, 0.1),
             r1=c(0.1, 0.3, 0.6, 1, 3, 6, 10),
             s=c(0.1, 0.3, 0.6, 1, 3, 6, 10),
             b1=c(0.3, 0.6, 1.0, 1.3, 1.6, 2),
             sim.no=1:n.sims)

### Apply culture evolution function for each set of parameters
mc.stats <- mclapply(1:nrow(params), function(i) {
  result <- culture.fn(params[i,])
  result <- cbind(result, params[i,])
  cat(i, '/', nrow(params), '\n')
  return(result)
}, mc.cores=20)
global.stats <- Reduce(rbind, mc.stats)

### Write simulation results to csv file
write.csv(global.stats, file="2017.03.16 Sim Results_full params_nonrandominit_server.csv")



