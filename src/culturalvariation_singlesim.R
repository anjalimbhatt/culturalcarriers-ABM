par <- params[323,]

### Initialize firm culture
init_firm_cult <- rnorm(f)

### Initialize employees' culture and tenure
sims <- data.table(firm=rep(1:f, each=n), culture=0.0,
                   tenure=rlnorm(n*f, 2.5, 1), employments=0.0)
sims[, culture := rnorm(n*f, init_firm_cult[firm], par$s0)]

### Initialize stats
stats <- data.table(var_win=rep(0, t+1), var_btwn=0, hires=0, rehires=0)
stats[1, var_win := mean(sims[, var(culture, na.rm=T), by=firm]$V1)]
stats[1, var_btwn := var(sims[, mean(culture, na.rm=T), by=firm]$V1)]

### Loop over months
for (i in 1:t) {
  
  sims2 <- data.table(sims)
  sims2[, tenure := tenure + 1]
  med_cult <- sims[, median(culture, na.rm=T), by=firm]$V1
  
  ### Turnover first
  # If no alienation, then assume only base turnover rate
  if (par$alienate==0) {
    sims2[, firm := 0 + firm * (runif(n*f) > par$r0)]
  } else {
    # Otherwise, retention is modeled as a gaussian shape
    sims2[, firm := 0 + firm * (runif(n*f) > (par$r2 - (par$r2-par$r0) * (par$r1) *
                                                sqrt(2*pi) * (dnorm(culture, med_cult[firm], par$r1))))]
  }
  #  Restart tenure clock for departed employees
  sims2[, tenure := 0 + tenure*(firm!=0)]
  
  ### Then socialization (skip if none)
  if (par$socialize==1) {
    # Even unemployed individuals experience noisy socialization
    sims2[, culture := culture + rnorm(n*f, 0, par$eps_soc) +
            (firm!=0) * (med_cult[firm] - culture) *
            (par$b0 + par$b1 * exp(-(par$b2 * (tenure-1))-(par$b3 * (employments-1))))]
  }
  
  ### Then hiring
  # Track how many hires made each period
  stats$hires[i+1] <- sum(sims2$firm==0)
  
  if (stats$hires[i+1] > 0) {
    # Set random order of hiring and prepopulate random entry draw
    queue <- data.table(firm = rep(0, stats$hires[i+1]),
                        draw = runif(stats$hires[i+1]))
    j <- 1
    for (k in 1:f) {
      hires <- n - sum(sims2$firm==k)
      if (hires>0) {
        queue[j:(j + hires - 1), firm := k]
        j <- j + hires
      }
    }
    queue[, firm := sample(firm)]
    
    # Iterate over ordered hiring spots
    for (h in 1:stats$hires[i+1]) {
      focal_firm <- queue$firm[h]
      unemployed <- which(sims2$firm==0 & sims$firm!=focal_firm)
      
      # First check for random entry
      if (queue$draw[h] <= par$s1 | length(unemployed)==0) {
        sims2 <- rbind(sims2, list(focal_firm, rnorm(1, med_cult[focal_firm], par$s0), 0, 0))
      } else if (par$select==0) {
        # If no selectivity, then draw random unemployed
        chosen <- sample(unemployed, 1)
        sims2[chosen, `:=`(firm = focal_firm,
                           employments = employments + 1)]
        stats[i+1, rehires := rehires + 1]
      } else if ((min(abs(sims2$culture[unemployed] - med_cult[focal_firm])) + rnorm(1,0,par$eps_hire)) >= 2*par$s0) {
        # Hire the now-unemployed person with the best cultural fit within the threshold
        # Otherwise hire outside the existing pool
        sims2 <- rbind(sims2, list(focal_firm, rnorm(1, med_cult[focal_firm], par$s0), 0, 0))
      } else {
        chosen <- unemployed[which.min(abs(sims2$culture[unemployed] - med_cult[focal_firm]))]
        sims2[chosen, `:=`(firm = focal_firm,
                           employments = employments + 1)]
        stats[i+1, rehires := rehires + 1]
      }
    }
  }
  
  # Remove all non-hires
  sims <- sims2[firm!=0]
  
  # Calculate statistics
  stats[i+1, var_win := mean(sims[, var(culture, na.rm=T), by=firm]$V1)]
  stats[i+1, var_btwn := var(sims[, mean(culture, na.rm=T), by=firm]$V1)]
}

### Return summary statistics for each simulation run
summary <- data.table(varbtwn_0 = stats$var_btwn[1],
                      varwin_0 = stats$var_win[1],
                      varbtwn_100 = mean(stats$var_btwn[91:101]),
                      varwin_100 = mean(stats$var_win[91:101]),
                      varbtwn_200 = mean(stats$var_btwn[191:201]),
                      varwin_200 = mean(stats$var_win[191:201]),
                      varbtwn_300 = mean(stats$var_btwn[291:301]),
                      varwin_300 = mean(stats$var_win[291:301]),
                      turnover = mean(stats$hires)/(n*f),
                      carriers = mean(stats$rehires/stats$hires))