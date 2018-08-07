library(tidyverse)

data <- read.csv("data/2018-06-23_results_fullparams.csv", header=T)
data2 <- read.csv("data/2018-06-25_results_fullparams_nointerorg.csv", header=T)
cond <- read.csv("data/initial_conditions.csv", header=T)
cond <- unique(cond[,c("var_win","cond")])
data <- data %>% rbind(data2) %>% left_join(cond, by="cond")

data$varwin_ratio <- data$varwin_end/data$varwin_start
data$varbtwn_ratio <- data$varbtwn_end/data$varbtwn_start
data$mobility <- data$turnover*data$carriers
data$random_entry <- data$turnover*(1-data$carriers)
data$differentiated <- data$varbtwn_end/data$varwin_end
data$diff_start <- data$varbtwn_start/data$varwin_start

## lowest varbtwn_ratio for high socialization, high turnover, high alienation, high selectivity.
## but why high selectivity? not due to mobility/random entry (it's lower in both).
## it's high random entry in the beginning (up to 1/3 of new entrants), then down to baseline random entry.
## this is also least differentiated.

ggplot(data[which(data$var_win==1 & data$b1==0.6 & data$r1==0.1 & data$r0==0.05),],
       aes(x=factor(log10(s0)+(1-select)), y=varbtwn_ratio, col=factor(s1))) +
  geom_boxplot(alpha=0.5, size=0.5)

## lowest varwin_ratio? same, and also low random entry. this is to be expected (except for high turnover).

ggplot(data[which(data$var_win==1 & data$b1==0.6 & data$r1==0.1 & data$r0==0.05),],
       aes(x=factor(log10(s0)+(1-select)), y=varwin_ratio, col=factor(s1))) +
  geom_boxplot(alpha=0.5, size=0.5)

## so when do we get least differentiation? same, except now high random entry.

ggplot(data[which(data$var_win==1 & data$b1==0.6 & data$r1==0.1 & data$r0==0.05),],
       aes(x=factor(log10(s0)+(1-select)), y=differentiated, col=factor(s1))) +
  geom_boxplot(alpha=0.5, size=0.5)

## so when do we get MOST differentiation? no socialization, low turnover, but high selectivity & alienation.

ggplot(data[which(data$var_win==1 & data$b1==0.0 & data$r1==0.1 & data$r0==0.01 & data$s0==0.1),],
       aes(x=factor(s1), y=differentiated, col=factor(r1))) +
  geom_boxplot(alpha=0.5, size=0.5)

## so when do we get MOST varwin_ratio? no socialization, no selectivity, but high turnover, random entry, and alienation. this is not the opposite of above.

ggplot(data[which(data$var_win==1 & data$b1==0.0 & data$select==0 & data$s1==0.05 & data$r0==0.05 & data$r1==0.1),],
       aes(x=factor(r1), y=varwin_ratio, col=factor(r0))) +
  geom_boxplot(alpha=0.5, size=0.5)

## so when do we get MOST varbtwn_ratio? no socialization, low turnover, no selectivity, no alienation. this is the opposite of above.

ggplot(data[which(data$var_win==1 & data$b1==0.0 & data$r0==0.01 & data$select==0 & data$alienate==0),],
       aes(x=factor(s1), y=varbtwn_ratio, col=factor(s1))) +
  geom_boxplot(alpha=0.5, size=0.5)

ggplot(data[which(data$var_win==1 & data$s0==0.1 & data$r1==0.1 & data$b1==0.6),],
       aes(x=factor(s1), y=varwin_ratio, col=factor(b1))) +
  geom_boxplot(alpha=0.5, size=0.5) + facet_wrap(~r0)

### THIS IS IT: compare to case of no interorg mobility. mobility always decreases variation, both win and btwn.
### But base turnover matters for case of mobility, not for case without.

plot <- ggplot(data[which(data$var_win==0.1 & data$s0==0.1 & data$r1==0.1 & data$b1==0.6),],
       aes(x=factor(r0), y=varwin_ratio, col=factor(s1==1))) +
  geom_violin() + theme_bw() +
  scale_color_discrete(name=element_blank(), labels=c("Interfirm Mobility", "No Mobility")) +
  xlab("Base Turnover Rate") + ylab(expression(sigma["within"])) + theme(legend.position="top")

ggsave(filename="tables/mobility_matters.png", plot=plot, units="in", width=6, height=6, pointsize=16)








##############

ggplot(data, aes(x=s0, y=carriers, col=factor(b1))) + geom_point(alpha=0.2, size=2)
# carriers increases with s0, but turnover decreases with s0
ggplot(data[which(data$var_win==1),], aes(x=mobility, y=varwin_ratio, col=factor(b1))) + geom_point(alpha=0.2, size=2)
# controlling for s0 & r0, increasing b1 decreases mobility & varwin_ratio - STRONG
ggplot(data, aes(x=b1, y=random_entry, col=factor(s0))) + geom_point(alpha=0.2, size=2)
ggplot(data, aes(x=random_entry, y=varbtwn_ratio, col=factor(s0))) + geom_point(alpha=0.2, size=2) # don't understand
ggplot(data, aes(x=turnover, y=random_entry, col=factor(s0))) + geom_point(alpha=0.2, size=2) # it's cuz there's more turnover in general
ggplot(data, aes(x=b1, y=carriers, col=factor(s0))) + geom_point(alpha=0.2, size=2)

ggplot(data[which(data$s0==data$var_win),], aes(x=log10(s0), y=varbtwn_end, col=factor(b1))) + geom_point(alpha=0.2, size=2)
ggplot(data[which(data$s0==1 & data$b1==0.2),], aes(x=mobility, y=varbtwn_ratio, col=factor(var_win))) + geom_point(alpha=0.2, size=2)

'So you should need high s0, low r1, and non-zero b1 in order to get population-driven convergence'
ggplot(data, aes(x=log10(s0), y=varbtwn_ratio, color=factor(b1))) + geom_col(position="dodge")

ggplot(data, aes(x=log10(s0), y=differentiated, col=factor(b1))) + geom_col(position="dodge")
ggplot(data, aes(x=factor(b1), y=differentiated, fill=factor(r0))) + geom_boxplot(position="dodge", alpha=0.5)

summary <- summary(data)
  
######## REGRESSIONS

# Descriptives
library(pastecs)
library(stargazer)
stargazer(data[,c("varwin_ratio","varbtwn_ratio","mobility","random_entry")])
stat.desc(data)

correlation.matrix <- cor(data[which(data$alienate==1),c("var_win","r0","r1","s0","s1","b1","varwin_ratio","varbtwn_ratio","mobility","random_entry")])
correlation.matrix <- round(correlation.matrix,2)
stargazer(correlation.matrix, title="Correlation Matrix")

# Run regressions
full.stats <- data[which(data$alienate==1 & data$s1!=1),
                   c('varwin_ratio','varbtwn_ratio','var_win','s0','r1','r0','b1','s1','mobility','random_entry')]
lm.var.win.1 <- lm(varwin_ratio ~ log10(var_win)  + log10(s0) + log10(r1) + r0 + b1 + s1, data=full.stats)
lm.var.win.2 <- lm(varwin_ratio ~ (log10(var_win)  + log10(s0) + log10(r1) + r0 + b1 + s1)^2, data=full.stats)
lm.var.win.3 <- lm(varwin_ratio ~ mobility + random_entry, data=full.stats)
lm.var.btwn.1 <- lm(varbtwn_ratio ~ log10(var_win)  + log10(s0) + log10(r1) + r0 + b1 + s1, data=full.stats)
lm.var.btwn.2 <- lm(varbtwn_ratio ~ (log10(var_win)  + log10(s0) + log10(r1) + r0 + b1 + s1)^2, data=full.stats)
lm.var.btwn.3 <- lm(varbtwn_ratio ~ mobility + random_entry, data=full.stats)

library(plm)
felm_varwin <- plm(varwin_ratio ~ log10(s0) + log10(r1) + r0 + b1 + s1,
                  data=data[which(data$alienate==1 & data$s1!=1),],
                  index=c("cond"), model="within")
stargazer(lm.var.win.1, lm.var.btwn.1,
          title="OLS Regression Results",
          covariate.labels=c("Init. sigma_{within} (logged)","Selection Bandwidth (logged)","Alienation Bandwidth (logged)","Base Turnover Rate","Base Socialization Rate","Base Random Entry Rate"),
          align=TRUE, dep.var.labels=c("sigma_{within}","sigma_{between}"),
          omit.stat=c("LL","ser","aic","bic"), no.space=T)

lm.mobility <- lm(mobility ~ r0 + log(r1) + log(s) + log(b1))
lm.random <- lm(random ~ r0 + log(r1) + log(s) + log(b1))
lm.teq <- lm(t.eq ~ r0 + log(r1) + log(s) + log(b1))

stargazer(lm.mobility, lm.random, lm.teq,
          title="OLS Regression Results, cont'd (Mediators on Parameters)",
          covariate.labels=c("r_0","log r_1","log s","log b_1"),
          align=TRUE, dep.var.labels=c("Mobility","Random Entry","t_text{eq}"),
          omit.stat=c("LL","ser","aic","bic"), no.space=T)

##### Plots







