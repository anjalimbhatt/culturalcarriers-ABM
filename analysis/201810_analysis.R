"
Cultural Carriers
Analysis of full simulations over parameter grid with fixed initial conditions

Written Oct 2018
@author: Anjali Bhatt
"

library(tidyverse)

# load data
setwd("~/Documents/GitHub/orgculture-ABM/")
data <- read.csv("data/2018-10-13_results_fullparams.csv", header=T)
cond <- read.csv("data/initial_conditions_201810.csv", header=T)
cond <- unique(cond[,c("var_win","cond")])
data <- data %>% left_join(cond, by="cond")

# create relevant variables
data$mobility <- data$turnover*data$carriers
data$random_entry <- data$turnover*(1-data$carriers)
data$fstat_start <- 30 * data$varbtwn_start^2 / data$varwin_start^2
data$fstat_end <- 30 * data$varbtwn_end^2 / data$varwin_end^2
data <- data.table(data)

# plots
ggplot(data[var_win==0.1
              #& b1==0.0
              & r0==0.06
              #& r1==
              #& r2==0.0
              #& s0==0
              & s1==1
              #& select==0
              ,],
       aes(x=factor(b1),
           y=fstat_end/fstat_start,
           col=factor(s0==1))) + theme_bw() + geom_violin() +
      facet_grid(. ~ select, labeller=label_both)

# when is var_btwn minimized? THESE CONDITIONS - to 0.5-1% its original value
ggplot(data[var_win==0.1
            & b1==0.3 # probably could optimize on this. somewhere between [0,0.6]
            & r0==0.06
            & r1==0.1
            & r2==0.05
            & s0==0.0
            & s1==1
            & select==0
            ,],
       aes(x=varbtwn_start,
           y=varbtwn_end/varbtwn_start,
           col=factor(cond))) + theme_bw() + geom_point(alpha=0.5)

# when is f_stat minimized?
ggplot(data[var_win==0.1
            & b1==0.3
            & r0==0.06
            & r1==1 # slightly different than above
            & r2==0.0
            & s0==0.0
            & s1==1
            & select==0
            ,],
       aes(x=factor(s0),
           y=fstat_end/fstat_start,
           col=factor(s0))) + theme_bw() + geom_point(alpha=0.5)

# under these above conditions what happens to var_win? it goes to 0.5-1 its original value
# this is not the minimimization of var_win. the following gets var_win to ~0.2 its original value.
ggplot(data[var_win==0.1
            & b1==0.6
            & r0==0.06
            #& r1==1
            #& r2==0.0 unclear
            & s0!=1
            & s1==0.1
            & select==1
            ,],
       aes(x=factor(var_win),
           y=varwin_end/varwin_start,
           col=factor(s0))) + theme_bw() + geom_violin()

# if you allow var_win to start higher, then we get var_win to ~0.1 its original value.
# here, though, var_btwn goes to ~0.9 its original value...
ggplot(data[var_win==1
            & b1==0.6
            & r0==0.06
            & r1==1
            & r2==0.0
            & s0==0.06
            & s1==1
            & select==1
            ,],
       aes(x=factor(s0),
           y=varwin_end/varwin_start,
           col=factor(s0))) + theme_bw() + geom_violin()


### so the story is that when firms are "merit/experience-based" and
# not culturally selective or alienating, and there's nonzero turnover,
# then these firms can reduce the population variation (if they socialize even a bit)
# socialization is necessary to remain cultural distinctive, but not to ensure strength.
ggplot(data[var_win==0.1
            #& b1==0.3
            & r0==0.06
            & r1==1 # slightly different than above
            & r2==0.0
            #& s0==0.0
            & s1==1
            & select==0
            ,],
       aes(x=factor(b1),
           y=varwin_end,
           col=factor(s0))) + theme_bw() + geom_violin()

# how do you maintain population variation? stay selective! (and less turnover, socialization)
# in other words, maintain firm culture rather than allowing your firm to move with the industry.
ggplot(data[var_win==0.1
            #& b1==0.3
            #& r0==0.06
            #& r1==1 # slightly different than above
            #& r2==0.0
            #& s0==0.0
            & s1==0.1
            & select==1
            ,],
       aes(x=factor(b1),
           y=varbtwn_end/varbtwn_start,
           col=factor(s0))) + theme_bw() + geom_violin() +
  facet_grid(r1 ~ r0, labeller=label_both)


# regressions
interorg <- data[s0!=1 & select==1 & r2==0.05 & var_win==0.1,]
nointerorg <- data[s0==1 & select==1 & r2==0.05 & var_win==0.1,] 

library(plm)
library(stargazer)
felm_varwin_mob <- plm(log10(varwin_end) ~ b1 + log10(r1) + log10(s1) + r0 + s0,
                   data=interorg,
                   index=c("cond"), model="within")
felm_varwin_nomob <- plm(log10(varwin_end) ~ b1 + log10(r1) + log10(s1) + r0,
                         data=nointerorg,
                         index=c("cond"), model="within")
felm_varbtwn_mob <- plm(log10(varbtwn_end) ~ b1 + log10(r1) + log10(s1) + r0 + s0,
                       data=interorg,
                       index=c("cond"), model="within")
felm_varbtwn_nomob <- plm(log10(varbtwn_end) ~ b1 + log10(r1) + log10(s1) + r0,
                         data=nointerorg,
                         index=c("cond"), model="within")
felm_fstat_mob <- plm(log10(fstat_end) ~ b1 + log10(r1) + log10(s1) + r0 + s0,
                        data=interorg,
                        index=c("cond"), model="within")
felm_fstat_nomob <- plm(log10(fstat_end) ~ b1 + log10(r1) + log10(s1) + r0,
                          data=nointerorg,
                          index=c("cond"), model="within")

stargazer(felm_varwin_mob, felm_varwin_nomob, felm_varbtwn_mob, felm_varbtwn_nomob, felm_fstat_mob, felm_fstat_nomob,
          title="OLS Regression Results",
          align=TRUE, dep.var.labels=c("sigma_text{within} (logged)","sigma_text{between} (logged)", "tilde{F}_text{stat}"),
          omit.stat=c("LL","ser","aic","bic"), no.space=T)







