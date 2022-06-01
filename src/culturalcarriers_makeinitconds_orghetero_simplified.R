"
Cultural Transmission & Variation in Organizational Populations
Build set of initial conditions

Use source() to run code on server

@date: April 2019
@author: Anjali Bhatt
"

setwd("~/Documents/GitHub/orgculture-ABM")
library(data.table)
library(matrixStats)
library(parallel)

"
initial conditions:
only low var_win
two possibilities: 1 or 2 tight orgs (constrained at -2 and 2)

10 initializations per initial condition
"

### Set global parameters for simulations
n_sims <- 3 # number of initializations per set of initial conditions
f <- 30 # number of firms
n <- 30 # number of employees per firm
params <- CJ(
  sim_no = 1:n_sims,
  var_win = 0.3,
  n_paragon = c(180, 300, 450) # number of employees in paragon firm
)
params$cond <- 1:nrow(params)

### Function to build each initialization
build_init <- function(par) {

  # Initialize firm culture
  init_firm_cult <- rnorm(f,0,1)
  # init_firm_cult <- rep(0,f)
  
  # Initialize employees' culture, tenure, and prior employments
  sims <- data.table(firm=rep(1:f, each=n), culture=0.0,
                     tenure=rlnorm(n*f, 2.5, 1), employments=ceiling(rlnorm(n*f,0,0.5)))
  sims[, culture := rnorm(n*f, init_firm_cult[firm], par$var_win)]
  
  paragon <- data.table(firm=rep(f+1, each=par$n_paragon),
                        culture=2,
                        tenure=rlnorm(par$n_paragon, 2.5, 1),
                        employments=ceiling(rlnorm(par$n_paragon,0,0.5)))
  
  sims <- rbind(sims, paragon)
  
  return(sims)
}

### Build initializations based on each set of initial condition parameters
all_cond <- mclapply(1:nrow(params), function(i) {
  result <- build_init(params[i,])
  result <- cbind(result, params[i,])
  cat(i, '/', nrow(params), '\n')
  return(result)
}, mc.cores=1)

initial_conditions <- Reduce(rbind, all_cond)

### Write simulation results to csv file
filename = paste("data/", "initial_conditions_hetero_201906.csv", sep="")
write.csv(initial_conditions, file=filename)
