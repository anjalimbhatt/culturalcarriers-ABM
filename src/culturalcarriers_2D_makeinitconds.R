"
Cultural Transmission & Variation in Organizational Populations
Build set of initial conditions

Use source() to run code on server

@date: Jun 2018
@author: Anjali Bhatt
"

setwd("/Users/ambhatt/Library/CloudStorage/OneDrive-Personal/02 Research/Cultural Carriers/")
library(data.table)
library(matrixStats)
library(parallel)

"
initial conditions: only 1 parameter --> var_win = low relative to var_btwn

used to be 3 possibilities
var_win = high, medium, low (relative to var_btwn)

10 initializations per initial condition
"

### Set global parameters for simulations
n_sims <- 30 # number of initializations per set of initial conditions
f <- 30 # number of firms
n <- 30 # number of employees per firm
var_win <- 0.1
params <- CJ(
  sim_no = 1:n_sims#,
  # var_win = c(0.1,1,10)
)
# params$cond <- 1:nrow(params)

### Function to build each initialization
build_init <- function(par) {

  # Initialize firm culture
  init_firm_cult <- data.table(c1=runif(f,-1,1),
                               c2=runif(f,-1,1)) #rnorm(f,0,1)
  
  # Initialize employees' culture, tenure, and prior employments
  sims <- data.table(firm=rep(1:f, each=n), c1=0.0, c2=0.0,
                     tenure=rlnorm(n*f, 2.5, 1), employments=ceiling(rlnorm(n*f,0,0.5)))
  sims[, c1 := rnorm(n*f, init_firm_cult$c1[firm], var_win)]
  sims[, c2 := rnorm(n*f, init_firm_cult$c2[firm], var_win)]
  
  return(sims)
}

### Build initializations based on each set of initial condition parameters
all_cond <- mclapply(1:nrow(params), function(i) {
  result <- build_init(params[i,])
  result <- cbind(result, params[i,])
  cat(i, '/', nrow(params), '\n')
  return(result)
}, mc.cores=2)

initial_conditions <- Reduce(rbind, all_cond)

### Write simulation results to csv file
filename = paste("data/", "initial_conditions_202306.csv", sep="")
write.csv(initial_conditions, file=filename)

# initial_conditions %>%
#   filter(sim_no==2) %>%
#   ggplot(aes(x=c1, y=c2, color=factor(firm))) + geom_point()
