library(tidyverse)

data <- read.csv("data/2018-06-19_results_partialparams.csv", header=T)
cond <- read.csv("data/initial_conditions_plots.csv", header=T)
cond <- unique(cond[,c("var_win","cond")])
data <- data %>% left_join(cond, by="cond")

data$varwin_ratio <- data$varwin_end/data$varwin_start
data$varbtwn_ratio <- data$varbtwn_end/data$varbtwn_start
data$mobility <- data$turnover*data$carriers
data$random_entry <- data$turnover*(1-data$carriers)

ggplot(data, aes(x=log10(s0), y=varwin_ratio, col=factor(b1))) + geom_point(alpha=0.2, size=2)
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
ggplot(data, aes(x=log10(s0), y=varbtwn_ratio, group=factor(b1))) + geom_col(position="dodge")








