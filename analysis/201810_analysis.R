"
Cultural Carriers
Analysis of full simulations over parameter grid with fixed initial conditions

Written Oct 2018
@author: Anjali Bhatt
"

library(tidyverse)
library(data.table)
library(plm)
library(stargazer)
library(gridExtra)

# load data (satisficing hiring)
setwd("~/Documents/GitHub/orgculture-ABM/")
data1 <- as.data.table(read.csv("data/2018-10-15_results_fullparams_alternatehiring.csv", header=T))
data1$optimize <- 0
cond <- read.csv("data/initial_conditions_201810.csv", header=T)
cond <- unique(cond[,c("var_win","cond")])

# add data from optimizing hiring
data2 <- as.data.table(read.csv("data/2018-10-13_results_fullparams.csv", header=T))
data2 <- data2[select==1,]
data2 <- data2[, select := NULL]
data2$optimize <- 1

# bind data
data <- data1 %>% rbind(data2) %>% left_join(cond, by="cond")
data <- data1 %>% left_join(cond, by="cond")

# create relevant variables
data$mobility <- data$turnover*data$carriers
data$random_entry <- data$turnover*(1-data$carriers)
data$fstat_start <- 30 * data$varbtwn_start^2 / data$varwin_start^2
data$fstat_end <- 30 * data$varbtwn_end^2 / data$varwin_end^2
data <- as.data.table(data)

### Correlations
data[,`:=`(varwin_ratio = varwin_end/varwin_start,
           varbtwn_ratio = varbtwn_end/varbtwn_start)]
correlation.matrix <- cor(data[which(data$optimize==0),c("var_win","b1","r0","r1","s0","s1","varwin_ratio","varbtwn_ratio", "fstat_end", "mobility","random_entry", "tenure_end", "emps_end")])
correlation.matrix <- round(correlation.matrix,2)
stargazer(correlation.matrix, title="Correlation Matrix")

### BETWEEN FIRM VARIATION

# var btwn regression
interorg <- data[s0!=1 & r2==0.05 & var_win==0.1 & optimize==0 & s1!=10,]
interorg[is.na(mobility), mobility:=0]
nointerorg <- data[s0==1 & r2==0.05 & var_win==0.1 & optimize==0 & s1!=10,]
felm_varbtwn <- plm(log10(varbtwn_end/varbtwn_start) ~
                      b1 + I(b1^2) + r0 + log10(r1) + log10(s1) + s0 + s0:log10(s1),
                    data=interorg, index=c("cond"), model="within")
felm_mob <- plm(mobility ~ b1 + I(b1^2) + r0 + log10(r1) + log10(s1) + s0 + s0:log10(s1),
                data=interorg, index=c("cond"), model="within")


stargazer(felm_varbtwn, felm_mob,
          title="Fixed-Effect Regression: Between-Firm Variation", align=TRUE,
          dep.var.labels=c("$hat{sigma}_text{between}$ (logged ratio of final to initial)", "Mobility Rate (per period)"),
          covariate.labels=c("$b_1$ (socialization)","$b_1^2$","$r_0$ (turnover)","$log_{10}(r_1)$ (alienation bw)","$log_{10}(s_1)$ (selection bw)","$s_0$ (random entry rate)","$s_0 * log_{10}(s_1)$","Mobility","Random entry"),
          omit.stat=c("LL","ser","aic","bic"))

# stargazer(felm_varbtwn_mob, felm_varbtwn_nomob,
#           title="Fixed-Effect Regression: Between-Firm Variation", align=TRUE,
#           dep.var.labels=c("$log_{10}(frac{sigma_text{between, final}}{sigma_text{between, initial}}$"),
#           column.labels=c("With Inter-firm Mobility", "Isolated Firm as Counterfactual"),
#           covariate.labels=c("$b_1$","$b_1^2$","$r_0$","$log_{10}(r_1)$","$log_{10}(s_1)$","$s_0$","$s_0 * log_{10}(s_1)$"),
#           omit.stat=c("LL","ser","aic","bic"))



# hiring selectivity
hiring <- as.data.table(read.csv("data/2018-10-15_results_detailedhiring.csv", header=T))
data <- hiring %>% left_join(cond, by="cond")
data <- as.data.table(data)
data[optimize==1, hiring := "Optimizing"]
data[optimize==0, hiring := "Satisficing"]
plot <- ggplot(data, aes(x=s1, y=varbtwn_end/varbtwn_start, col=factor(hiring, levels=c("Satisficing", "Optimizing")))) +
  scale_color_manual(values=c("black","darkgrey"), guide=F) +
  geom_boxplot(aes(group=s1)) +
  facet_grid(. ~ factor(hiring, levels=c("Satisficing", "Optimizing"))) +
  theme(panel.spacing = unit(1, "lines")) +
  xlab(expression(paste("Hiring Selection Bandwidth ", s[1]))) +
  ylab(expression(paste("Ratio of final to initial ", widehat(sigma)["between"])))
ggsave(filename="figures/varbtwn_hiring.png", plot=plot, units="in", width=6, height=4, pointsize=16)

# socialization susceptibility
socdata <- as.data.table(read.csv("data/2018-10-15_results_socbtwn.csv", header=T))
data <- socdata %>% left_join(cond, by="cond")
data <- as.data.table(data)
plot <- ggplot(data, aes(x=b1, y=varbtwn_end/varbtwn_start)) +
  geom_boxplot(aes(group=b1)) +
  xlab(expression(paste("Initial Socialization Susceptibility ", b[1]))) +
  ylab(expression(paste("Ratio of final to initial ", widehat(sigma)["between"])))
ggsave(filename="figures/varbtwn_socialization.png", plot=plot, units="in", width=6, height=5, pointsize=16)

# base turnover
turnoverdata <- as.data.table(read.csv("data/2018-10-16_results_turnoverbtwn.csv", header=T))
data <- turnoverdata %>% left_join(cond, by="cond")
data <- as.data.table(data)
plot <- ggplot(data, aes(x=r0, y=varbtwn_end/varbtwn_start)) +
  geom_boxplot(aes(group=r0)) +
  xlab(expression(paste("Base Rate of Turnover ", r[0]))) +
  ylab(expression(paste("Ratio of final to initial ", widehat(sigma)["between"])))
ggsave(filename="figures/varbtwn_turnover.png", plot=plot, units="in", width=6, height=5, pointsize=16)


# all plots together
alienationdata <- as.data.table(read.csv("data/2018-10-25_results_alienationbtwn.csv", header=T))
alienationdata2 <- as.data.table(read.csv("data/2018-10-25_results_alienationbtwn_nomob.csv", header=T))
alienation <- rbind(alienationdata, alienationdata2)
hiring <- hiring %>% left_join(cond, by="cond") %>% as.data.table()
socdata <- socdata %>% left_join(cond, by="cond") %>% as.data.table()
alienation <- alienation %>% left_join(cond, by="cond") %>% as.data.table()

varbtwn_alienation <- ggplot(alienation[s0!=1,], aes(x=r1, y=varbtwn_end/varbtwn_start)) +
  geom_boxplot(aes(group=r1), outlier.alpha = 0.5) +
  scale_y_continuous(limits=c(0,1)) +
  xlab(expression(paste("Turnover Alienation Bandwidth ", r[1]))) +
  ylab(expression(paste("Ratio of final to initial ", widehat(sigma)["between"])))

varbtwn_hiring <- ggplot(hiring[optimize==0,], aes(x=s1, y=varbtwn_end/varbtwn_start)) +
  geom_boxplot(aes(group=s1), outlier.alpha = 0.5) +
  scale_y_continuous(limits=c(0,1), labels=NULL) +
  xlab(expression(paste("Hiring Selection Bandwidth ", s[1]))) +
  ylab(NULL)

varbtwn_soc <- ggplot(socdata, aes(x=b1, y=varbtwn_end/varbtwn_start)) +
  geom_boxplot(aes(group=b1), outlier.alpha = 0.5) +
  scale_y_continuous(limits=c(0,1), labels=NULL) +
  xlab(expression(paste("Initial Socialization Rate ", b[1]))) +
  ylab(NULL)

ggsave(filename="figures/varbtwn_alien.png", plot=varbtwn_alienation, units="in", width=4, height=5, pointsize=16)
ggsave(filename="figures/varbtwn_soc.png", plot=varbtwn_soc, units="in", width=4, height=5, pointsize=16)
ggsave(filename="figures/varbtwn_hiring.png", plot=varbtwn_hiring, units="in", width=4, height=5, pointsize=16)


### WITHIN FIRM VARIATION
hiring2 <- as.data.table(read.csv("data/2018-10-16_results_detailedhiring2.csv", header=T))
hiring <- hiring[optimize==0,] %>% rbind(hiring2[s0==1,]) %>% left_join(cond, by="cond")
hiring <- as.data.table(hiring)
hiring[s0==1, mobility := "Isolated Firms"]
hiring[s0==0.03, mobility := "Inter-firm Mobility"]
varwin_hiring <- ggplot(hiring[s1<=1.5,], aes(x=s1, y=varwin_end/varwin_start, col=factor(mobility, levels=c("Inter-firm Mobility", "Isolated Firms")))) +
  scale_color_manual(values=c("black","darkgrey"), guide=F) +
  geom_boxplot(aes(group=s1)) +
  facet_grid(cols=factor(mobility)) +
  theme(panel.spacing = unit(1, "lines")) +
  xlab(expression(paste("Hiring Selection Bandwidth ", s[1]))) +
  ylab(expression(paste("Ratio of final to initial ", widehat(sigma)["within"]))) +
  scale_y_continuous(breaks=seq(0,15,3))
ggsave(filename="figures/varwin_hiring.png", plot=varwin_hiring, units="in", width=6, height=3, pointsize=16)

socdata <- as.data.table(read.csv("data/2018-10-15_results_socbtwn.csv", header=T))
socdata2 <- as.data.table(read.csv("data/2018-10-16_results_socwin.csv", header=T))
socdata <- socdata %>% rbind(socdata2) %>% left_join(cond, by="cond")
socdata <- as.data.table(socdata)
socdata[s0==1, mobility := "Isolated Firms"]
socdata[s0==0.03, mobility := "Inter-firm Mobility"]
varwin_soc <- ggplot(socdata, aes(x=b1, y=varwin_end/varwin_start, col=factor(mobility, levels=c("Inter-firm Mobility", "Isolated Firms")))) +
  scale_color_manual(values=c("black","darkgrey"), guide=F) +
  geom_boxplot(aes(group=b1)) +
  facet_grid(factor(mobility, levels=c("Inter-firm Mobility", "Isolated Firms")) ~ .) +
  theme(panel.spacing = unit(1, "lines")) +
  xlab(expression(paste("Initial Socialization Rate ", b[1]))) +
  ylab(expression(paste("Ratio of final to initial ", widehat(sigma)["within"]))) +
  scale_y_continuous(limits=c(0,15), breaks=seq(0,15,3))
ggsave(filename="figures/varwin_soc.png", plot=varwin_soc, units="in", width=6, height=3, pointsize=16)

# all together
alienation[s0==1, mobility := "Isolated Firms"]
alienation[s0==0.03, mobility := "Inter-firm Mobility"]
varwin_alienation <- ggplot(alienation, aes(x=r1, y=varwin_end/varwin_start, col=factor(mobility, levels=c("Inter-firm Mobility", "Isolated Firms")))) +
  scale_color_manual(values=c("black","darkgrey"), guide=F) +
  geom_boxplot(aes(group=r1), outlier.alpha = 0.5) +
  facet_grid(factor(mobility, levels=c("Inter-firm Mobility", "Isolated Firms")) ~ .) +
  theme(panel.spacing = unit(1, "lines")) +
  scale_y_continuous(limits=c(0,15), breaks=seq(0,15,3)) +
  xlab(expression(paste("Turnover Alienation Bandwidth ", r[1]))) +
  ylab(expression(paste("Ratio of final to initial ", widehat(sigma)["within"])))
ggsave(filename="figures/varwin_alien.png", plot=varwin_alienation, units="in", width=6, height=3, pointsize=16)

#FSTAT
# 
# felm_fstat <- plm(log10(fstat_end/fstat_start) ~
#                       b1 + r0 + log10(r1) + log10(s1),
#                     data=interorg, index=c("cond"), model="within")
# felm_fstat2 <- plm(log10(fstat_end/fstat_start) ~
#                     b1 + r0 + log10(r1) + log10(s1),
#                   data=nointerorg, index=c("cond"), model="within")
# 
# data$fstat_start <- 30 * data$varbtwn_start^2 / data$varwin_start^2
# data$fstat_end <- 30 * data$varbtwn_end^2 / data$varwin_end^2
# plot <- ggplot(data[s1>0.3,], aes(x=s1, y=fstat_end/fstat_start, col=factor(mobility, levels=c("Inter-firm Mobility", "Isolated Firms")))) +
#   scale_color_manual(values=c("black","darkgrey"), guide=F) +
#   geom_boxplot(aes(group=s1)) +
#   facet_grid(. ~ factor(mobility, levels=c("Inter-firm Mobility", "Isolated Firms"))) +
#   theme(panel.spacing = unit(1, "lines")) +
#   xlab(expression(paste("Hiring Selection Bandwidth ", s[1]))) +
#   ylab(expression(paste("Ratio of final to initial ", tilde(F)["stat"])))
# 
# data_fstat <- data[s1<=1.5,] %>% group_by(mobility,s1) %>%
#   summarise(f_stat = mean(fstat_end/fstat_start))
# 
# plot <- ggplot(data_fstat, aes(x=s1, y=f_stat, col=factor(mobility, levels=c("Inter-firm Mobility", "Isolated Firms")))) +
#   scale_color_manual(values=c("black","darkgrey"), guide=F) +
#   geom_point() +
#   xlab(expression(paste("Hiring Selection Bandwidth ", s[1]))) +
#   ylab(expression(paste("Ratio of final to initial ", tilde(F)["stat"])))
# 
# socdata <- as.data.table(read.csv("data/2018-10-15_results_socbtwn.csv", header=T))
# socdata2 <- as.data.table(read.csv("data/2018-10-16_results_socwin.csv", header=T))
# data <- socdata %>% rbind(socdata2) %>% left_join(cond, by="cond")
# data <- as.data.table(data)
# data[s0==1, mobility := "Isolated Firms"]
# data[s0==0.03, mobility := "Inter-firm Mobility"]
# data$fstat_start <- 30 * data$varbtwn_start^2 / data$varwin_start^2
# data$fstat_end <- 30 * data$varbtwn_end^2 / data$varwin_end^2
# plot <- ggplot(data, aes(x=b1, y=fstat_end/fstat_start, col=factor(mobility, levels=c("Inter-firm Mobility", "Isolated Firms")))) +
#   scale_color_manual(values=c("black","darkgrey"), guide=F) +
#   geom_boxplot(aes(group=b1)) +
#   facet_grid(. ~ factor(mobility, levels=c("Inter-firm Mobility", "Isolated Firms"))) +
#   theme(panel.spacing = unit(1, "lines")) +
#   xlab(expression(paste("Initial Socialization Susceptibility ", b[1]))) +
#   ylab(expression(paste("Ratio of final to initial ", tilde(F)["stat"])))
# 
# turnoverdata <- as.data.table(read.csv("data/2018-10-16_results_turnoverbtwn.csv", header=T))
# data <- turnoverdata %>% left_join(cond, by="cond")
# data <- as.data.table(data)
# plot <- ggplot(data, aes(x=r0, y=varwin_end/varwin_start)) +
#   geom_boxplot(aes(group=r0)) +
#   xlab(expression(paste("Base Rate of Turnover ", r[0]))) +
#   ylab(expression(paste("Ratio of final to initial ", widehat(sigma)["within"])))
# ggsave(filename="figures/varwin_turnover.png", plot=plot, units="in", width=6, height=5, pointsize=16)

















# ggplot(data[var_win!=0
#               #& b1==0.0
#               & r0==0.06
#               #& r1==
#               & r2==0.05
#               #& s0!=0
#               #& s1==1
#               ,],
#        aes(x=factor(log10(s1)),
#            y=log10(varbtwn_end/varbtwn_start),
#            col=factor(s0))) + theme_bw() + geom_violin() +
#       facet_grid(b1 ~ log10(var_win), labeller=label_both)
# 
# 
# 
# 
# # when is var_btwn minimized? THESE CONDITIONS - to 0.5-1% its original value
# ggplot(data[var_win==0.1
#             & b1==0.3 # probably could optimize on this. somewhere between [0,0.6]
#             & r0==0.06
#             & r1==0.1
#             & r2==0.05
#             & s0==0.0
#             & s1==1
#             & select==0
#             ,],
#        aes(x=varbtwn_start,
#            y=varbtwn_end/varbtwn_start,
#            col=factor(cond))) + theme_bw() + geom_point(alpha=0.5)
# 
# # when is f_stat minimized?
# ggplot(data[var_win==0.1
#             & b1==0.3
#             & r0==0.06
#             & r1==1 # slightly different than above
#             & r2==0.0
#             & s0==0.0
#             & s1==1
#             & select==0
#             ,],
#        aes(x=factor(s0),
#            y=fstat_end/fstat_start,
#            col=factor(s0))) + theme_bw() + geom_point(alpha=0.5)
# 
# # under these above conditions what happens to var_win? it goes to 0.5-1 its original value
# # this is not the minimimization of var_win. the following gets var_win to ~0.2 its original value.
# ggplot(data[var_win==0.1
#             & b1==0.6
#             & r0==0.06
#             #& r1==1
#             #& r2==0.0 unclear
#             & s0!=1
#             & s1==0.1
#             & select==1
#             ,],
#        aes(x=factor(var_win),
#            y=varwin_end/varwin_start,
#            col=factor(s0))) + theme_bw() + geom_violin()
# 
# # if you allow var_win to start higher, then we get var_win to ~0.1 its original value.
# # here, though, var_btwn goes to ~0.9 its original value...
# ggplot(data[var_win==1
#             & b1==0.6
#             & r0==0.06
#             & r1==1
#             & r2==0.0
#             & s0==0.06
#             & s1==1
#             & select==1
#             ,],
#        aes(x=factor(s0),
#            y=varwin_end/varwin_start,
#            col=factor(s0))) + theme_bw() + geom_violin()
# 
# 
# ### so the story is that when firms are "merit/experience-based" and
# # not culturally selective or alienating, and there's nonzero turnover,
# # then these firms can reduce the population variation (if they socialize even a bit)
# # socialization is necessary to remain cultural distinctive, but not to ensure strength.
# ggplot(data[var_win==0.1
#             #& b1==0.3
#             & r0==0.06
#             & r1==1 # slightly different than above
#             & r2==0.0
#             #& s0==0.0
#             & s1==1
#             & select==0
#             ,],
#        aes(x=factor(b1),
#            y=varwin_end,
#            col=factor(s0))) + theme_bw() + geom_violin()
# 
# # how do you maintain population variation? stay selective! (and less turnover, socialization)
# # in other words, maintain firm culture rather than allowing your firm to move with the industry.
# ggplot(data[var_win==0.1
#             #& b1==0.3
#             #& r0==0.06
#             #& r1==1 # slightly different than above
#             #& r2==0.0
#             #& s0==0.0
#             & s1==0.1
#             & select==1
#             ,],
#        aes(x=factor(b1),
#            y=varbtwn_end/varbtwn_start,
#            col=factor(s0))) + theme_bw() + geom_violin() +
#   facet_grid(r1 ~ r0, labeller=label_both)
# 
# 
# # regressions
# 
# 
# felm_varwin_mob <- plm(log10(varwin_end) ~ b1 + log10(r1) + log10(s1) + r0 + s0,
#                    data=interorg,
#                    index=c("cond"), model="within")
# felm_varwin_nomob <- plm(log10(varwin_end) ~ b1 + log10(r1) + log10(s1) + r0,
#                          data=nointerorg,
#                          index=c("cond"), model="within")
# felm_fstat_mob <- plm(log10(fstat_end) ~ b1 + log10(r1) + log10(s1) + r0 + s0,
#                         data=interorg,
#                         index=c("cond"), model="within")
# felm_fstat_nomob <- plm(log10(fstat_end) ~ b1 + log10(r1) + log10(s1) + r0,
#                           data=nointerorg,
#                           index=c("cond"), model="within")
# 
# stargazer(felm_varwin_mob, felm_varwin_nomob, felm_varbtwn_mob, felm_varbtwn_nomob, felm_fstat_mob, felm_fstat_nomob,
#           title="OLS Regression Results",
#           align=TRUE, dep.var.labels=c("sigma_text{within} (logged)","sigma_text{between} (logged)", "tilde{F}_text{stat}"),
#           omit.stat=c("LL","ser","aic","bic"), no.space=T)
# 






