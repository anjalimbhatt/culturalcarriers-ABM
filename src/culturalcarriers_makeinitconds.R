"
Cultural Transmission & Variation in Organizational Populations
Build set of initial conditions

Use source() to run code on server

@date: Jun 2018
@author: Anjali Bhatt
"

setwd("/afs/.ir/users/a/m/ambhatt/Git/orgculture-ABM/")
library(data.table)
library(matrixStats)
library(parallel)

"
initial conditions: 3x3=9 possibilities
var_win = high, medium, low
var_btwn = high, medium, low

10 initializations per initial condition
"

### Set global parameters for simulations
n_sims <- 10 # number of initializations per set of initial conditions
f <- 30 # number of firms
n <- 30 # number of employees per firm
params <- CJ(
  sim_no = 1:n_sims,
  var_win = c(0.1,1,10),
  var_btwn = c(0.1,1,10)
)
params$cond <- 1:nrow(params)

### Function to build each initialization
build_init <- function(par) {

  # Initialize firm culture
  init_firm_cult <- rnorm(f, par$var_btwn)
  
  # Initialize employees' culture, tenure, and prior employments
  sims <- data.table(firm=rep(1:f, each=n), culture=0.0,
                     tenure=rlnorm(n*f, 2.5, 1), employments=ceiling(rlnorm(n*f,0,0.5)))
  sims[, culture := rnorm(n*f, init_firm_cult[firm], par$var_win)]
  
  return(sims)
}

### Build initializations based on each set of initial condition parameters
all_cond <- mclapply(1:nrow(params), function(i) {
  result <- built_init(params[i,])
  result <- cbind(result, params[i,])
  return(result)
}, mc.cores=2)
initial_conditions <- Reduce(rbind, all_cond)

### Write simulation results to csv file
filename = paste("data/", "initial_conditions.csv")
write.csv(initial_conditions, file=filename)
