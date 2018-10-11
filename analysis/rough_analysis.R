library(tidyverse)

setwd("~/Documents/GitHub/orgculture-ABM/")
data <- read.csv("data/2018-06-23_results_fullparams.csv", header=T)
data2 <- read.csv("data/2018-06-25_results_fullparams_nointerorg.csv", header=T)
cond <- read.csv("data/initial_conditions.csv", header=T)
cond <- unique(cond[,c("var_win","cond")])
data <- data %>% rbind(data2) %>% left_join(cond, by="cond")

#data$varwin_ratio <- data$varwin_end/data$varwin_start
#data$varbtwn_ratio <- data$varbtwn_end/data$varbtwn_start
data$mobility <- data$turnover*data$carriers
data$random_entry <- data$turnover*(1-data$carriers)
data$varbtwn_end <- data$varbtwn_end^2
data$varbtwn_start <- data$varbtwn_start^2
data$varwin_end <- data$varwin_end^2
data$varwin_start <- data$varwin_start^2
data$fstat_start <- 30 * data$varbtwn_start / data$varwin_start
data$fstat_end <- 30 * data$varbtwn_end / data$varwin_end



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

correlation.matrix <- cor(data[which(data$alienate==1 & data$select==1),c("var_win","r0","r1","s0","s1","b1","varwin_end","varbtwn_end", "fstat_end", "mobility","random_entry", "tenure_end", "emps_end")])
correlation.matrix <- round(correlation.matrix,2)
stargazer(correlation.matrix, title="Correlation Matrix")






library(xtable)
library(Hmisc)

corstarsl <- function(x){ 
  require(Hmisc) 
  x <- as.matrix(x) 
  R <- rcorr(x)$r 
  p <- rcorr(x)$P 
  
  ## define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", " ")))
  
  ## trunctuate the matrix that holds the correlations to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1] 
  
  ## build a new matrix that includes the correlations with their apropriate stars 
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x)) 
  diag(Rnew) <- paste(diag(R), " ", sep="") 
  rownames(Rnew) <- colnames(x) 
  colnames(Rnew) <- paste(colnames(x), "", sep="") 
  
  ## remove upper triangle
  Rnew <- as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew) 
  
  ## remove last column and return the matrix (which is now a data frame)
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  return(Rnew) 
}

xtable(corstarsl(data[which(data$alienate==1 & data$select==1 & data$s1!=1),c("var_win","r0","r1","b1","s1","s0","varwin_end","varbtwn_end", "fstat_end", "mobility","random_entry", "tenure_end", "emps_end")])) #Latex code






# Run regressions
full.stats <- data[which(data$alienate==1 & data$select==1 & data$s1!=1),
                   c("var_win","r0","r1","b1","s1","s0","varwin_end","varbtwn_end", "fstat_end", "mobility","random_entry", "tenure_end", "emps_end", "var_win", "cond")]

nointerorg.stats <- data[which(data$alienate==1 & data$select==1 & data$s1==1),
                        c("var_win","r0","r1","b1","s1","s0","varwin_end","varbtwn_end", "fstat_end", "mobility","random_entry", "tenure_end", "emps_end", "var_win", "cond")]



library(plm)
felm_varwin <- plm(log10(sqrt(varwin_end)) ~ log10(s0) + log10(r1) + b1 + r0 + s1,
                  data=full.stats,
                  index=c("cond"), model="within")
felm_varbtwn <- plm(log10(sqrt(varbtwn_end)) ~ log10(s0) + log10(r1) + b1 + r0 + s1,
                   data=full.stats,
                   index=c("cond"), model="within")
felm_fstat <- plm(log10(fstat_end) ~ log10(s0) + log10(r1) + b1 + r0 + s1,
                    data=full.stats,
                    index=c("cond"), model="within")

felm_mobility <- plm(mobility ~ log10(s0) + log10(r1) + b1 + r0 + s1,
                    data=full.stats,
                    index=c("cond"), model="within")

felm_randomentry <- plm(random_entry ~ log10(s0) + log10(r1) + b1 + r0 + s1,
                     data=full.stats,
                     index=c("cond"), model="within")

felm_tenure <- plm(tenure_end ~ log10(s0) + log10(r1) + b1 + r0 + s1,
                     data=full.stats,
                     index=c("cond"), model="within")

felm_emps <- plm(emps_end ~ log10(s0) + log10(r1) + b1 + r0 + s1,
                     data=full.stats,
                     index=c("cond"), model="within")

felm_varwin_nointerorg <- plm(log10(sqrt(varwin_end)) ~ log10(s0) + log10(r1) + b1 + r0,
                              data=nointerorg.stats,
                              index=c("cond"), model="within")

felm_varbtwn_nointerorg <- plm(log10(sqrt(varbtwn_end)) ~ log10(s0) + log10(r1) + b1 + r0,
                              data=nointerorg.stats,
                              index=c("cond"), model="within")

felm_fstat_nointerorg <- plm(log10(sqrt(fstat_end)) ~ log10(s0) + log10(r1) + b1 + r0,
                               data=nointerorg.stats,
                               index=c("cond"), model="within")


stargazer(felm_varwin, felm_varbtwn, felm_fstat,
          title="OLS Regression Results",
          covariate.labels=c("Selection Bandwidth (logged)","Alienation Bandwidth (logged)","Base Socialization Rate","Base Turnover Rate","Base Random Entry Rate"),
          align=TRUE, dep.var.labels=c("sigma_text{within} (logged)","sigma_text{between} (logged)", "tilde{F}_text{stat}"),
          omit.stat=c("LL","ser","aic","bic"), no.space=T)

stargazer(felm_mobility, felm_randomentry,
          title="OLS Regression Results, cont'd (Mediators on Parameters)",
          covariate.labels=c("Selection Bandwidth (logged)","Alienation Bandwidth (logged)","Base Socialization Rate","Base Turnover Rate","Base Random Entry Rate"),
          align=TRUE, dep.var.labels=c("Mobility","Random Entry"),
          omit.stat=c("LL","ser","aic","bic"), no.space=T)





ggplot(data[which(data$var_win==0.1 & data$s0==1 & data$r1==1 & data$b1==0.6 & data$alienate==1 & data$select==1),],
               aes(x=factor(r0), y=fstat_end, col=factor(s1==1))) +
  geom_violin() + theme_bw() +
  scale_color_discrete(name=element_blank(), labels=c("Interfirm Mobility", "No Mobility")) +
  xlab("Base Turnover Rate") + ylab(expression(F["stat"])) + theme(legend.position="top")

plot

ggsave(filename="tables/mobility_matters.png", plot=plot, units="in", width=6, height=6, pointsize=16)


ggplot(data[which(data$var_win==0.1 & data$r1==0.1 & data$r0==0.05 & data$s1!=1 & data$alienate==1),],
       aes(x=factor(b1), y=varbtwn_end, col=factor(s0))) +
  geom_violin() + theme_bw()

ggplot(data[which(data$var_win==0.1 & data$s1!=1 & data$b1==0.6 & data$r0==0.05),],
       aes(x=factor(r1), y=varbtwn_end, col=factor(s0))) +
  geom_violin() + theme_bw()

ggplot(data[which(data$var_win==0.1 & data$b1==0.0 & data$select==1 & data$alienate==1 & data$s1!=1 &
                    data$s0==0.1 & data$r0==0.05),],
       aes(x=factor(r1), y=varbtwn_end, col=factor(s1))) +
  theme_bw() + geom_violin()

ggplot(data[which(data$var_win==0.1 & data$select==1 & data$s0==0.1 & data$alienate==0 & data$b1==0 & data$s1==0.01),],
       aes(x=random_entry, y=varbtwn_end, col=factor(r0))) +
  theme_bw() + geom_violin()

ggplot(data[which(data$var_win==0.1 & data$s0==1 & data$alienate==0 & data$b1==0),],
       aes(x=factor(r0), y=varwin_end, col=factor(s1))) +
  theme_bw() + geom_violin()


ggplot(data[which(data$var_win==0.1 & data$s0==0.1 & data$r1==0.1 & data$b1==0 & data$s1!=1 & data$r0==0.01),],
       aes(x=factor(s1), y=fstat_end, col=factor(s1))) +
  theme_bw() + geom_violin()


ggplot(data[which(data$var_win==1 & data$b1==0 & data$s1!=1),],
       aes(x=factor(s0), y=fstat_end, col=factor(r1))) +
  theme_bw() + geom_violin()




