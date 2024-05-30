# INFO --------------------------------------------------------------------
# Date: May 2024
# Author: Anjali Bhatt
# Purpose: Code to analyze output of cultural carriers ABM simulations
# Notes: These are based on the baseline & behemoth models run in Apr/May 2024

# SETUP -------------------------------------------------------------------

# libraries
library(tidyverse)
library(data.table)
library(gridExtra)

# working directory
setwd("/Users/ambhatt/Documents/GitHub/culturalcarriers-ABM/")

# read data for analysis
df_baseline <- read.csv("data/baseline_model.csv", header=T) %>% as.data.table()
df_behemoth <- read.csv("data/behemoth_model.csv", header=T) %>% as.data.table()
df_idealized <- read.csv("data/idealized_behemoth.csv", header=T) %>%
  select(initcond, run, s0, r0,
         r1_other = r1, b1_other = b1, s1_other = other_s1, s1_google = google_s1, other_culture_change = culture_change) %>%
  as.data.table()

df_baseline[s0==1, mobility := "Isolated Firms"]
df_baseline[s0==0.03, mobility := "Inter-firm Mobility"]
df_baseline[r0==0.01, industry := "Low Turnover"]
df_baseline[r0==0.05, industry := "High Turnover"]

df_behemoth <- df_behemoth %>%
  rbind(df_idealized, fill=T)
df_behemoth[s0==0.8, mobility := "Isolated Firms"]
df_behemoth[s0==0.03, mobility := "Inter-firm Mobility"]

# ANALYSIS ----------------------------------------------------------------

# Strong Culture (within-firm variation) ----------------------------------
varwin_s1 <- df_baseline %>%
  filter(s0!=0.10 & r1==0.1 & b1==0.3 & industry=="High Turnover") %>%
  ggplot(aes(x=s1, y=within_final/within_initial, col=factor(mobility, levels=c("Inter-firm Mobility", "Isolated Firms")))) +
  scale_color_manual(values=c("black","darkgrey"), guide=F) +
  geom_boxplot(aes(group=s1)) +
  facet_grid(rows=vars(mobility)) +
  theme(panel.spacing = unit(1, "lines")) +
  xlab(expression(paste("Hiring Selection Bandwidth ", s[1]))) +
  ylab(expression(paste("Ratio of final to initial ", widehat(sigma)["within"]))) +
  scale_y_continuous(limits=c(0,10), breaks=seq(0,10,2), labels=NULL) +
  ylab(NULL)
varwin_s1

varwin_b1 <- df_baseline %>%
  filter(s0!=0.10 & r1==0.1 & s1==1.0 & industry=="High Turnover") %>%
  ggplot(aes(x=b1, y=within_final/within_initial, col=factor(mobility, levels=c("Inter-firm Mobility", "Isolated Firms")))) +
  scale_color_manual(values=c("black","darkgrey"), guide=F) +
  geom_boxplot(aes(group=b1)) +
  facet_grid(rows=vars(mobility)) +
  theme(panel.spacing = unit(1, "lines")) +
  xlab(expression(paste("Initial Socialization Rate ", b[1]))) +
  ylab(expression(paste("Ratio of final to initial ", widehat(sigma)["within"]))) +
  scale_y_continuous(limits=c(0,10), breaks=seq(0,10,2), labels=NULL) +
  ylab(NULL)
varwin_b1

varwin_r1 <- df_baseline %>%
  filter(s0!=0.10 & b1==0.3 & s1==1.0 & industry=="High Turnover") %>%
  ggplot(aes(x=r1, y=within_final/within_initial, col=factor(mobility, levels=c("Inter-firm Mobility", "Isolated Firms")))) +
  scale_color_manual(values=c("black","darkgrey"), guide=F) +
  geom_boxplot(aes(group=r1)) +
  facet_grid(rows=vars(mobility)) +
  theme(panel.spacing = unit(1, "lines")) +
  xlab(expression(paste("Turnover Alienation Bandwidth ", r[1]))) +
  ylab(expression(paste("Ratio of final to initial ", widehat(sigma)["within"]))) +
  scale_y_continuous(limits=c(0,10), breaks=seq(0,10,2))
varwin_r1

varwin_plot <- grid.arrange(varwin_r1, varwin_s1, varwin_b1, ncol=3)
ggsave(filename="figures/202405/varwin_plot.png", plot=varwin_plot, units="in", width=9, height=6.5)

# Baseline Model Mechanism ------------------------------------------------

# variation between for all 3 parameters
# heat maps for s1-b1, variation within & differentiation (f-stat)

# Cultural Change (Behemoth Model) ----------------------------------------

g_change_r1 <- df_behemoth %>%
  filter(r1_google %in% c(0.3,NA) & s1_google==0.3 & b1_google %in% c(0.7,NA) &
    b1_other==0.3 & s1_other==0.5) %>%
  ggplot(aes(x=r1_other, y=other_culture_change, col=factor(mobility, levels=c("Inter-firm Mobility", "Isolated Firms")))) +
  scale_color_manual(values=c("black","darkgrey"), guide=F) +
  geom_boxplot(aes(group=r1_other)) +
  facet_grid(rows=vars(mobility)) +
  theme(panel.spacing = unit(1, "lines")) +
  xlab(expression(paste("Turnover Alienation Bandwidth of Small Firms ", r[1]))) +
  ylab(expression(paste("Cultural Change of Small Firms ", Delta,"c"))) +
  ylim(0,2)
g_change_r1

g_change_s1 <- df_behemoth %>%
  filter(r1_google %in% c(0.3,NA) & s1_google==0.3 & b1_google %in% c(0.7,NA) &
           r1_other==0.3 & b1_other==0.3) %>%
  ggplot(aes(x=s1_other, y=other_culture_change, col=factor(mobility, levels=c("Inter-firm Mobility", "Isolated Firms")))) +
  scale_color_manual(values=c("black","darkgrey"), guide=F) +
  geom_boxplot(aes(group=s1_other)) +
  facet_grid(rows=vars(mobility)) +
  theme(panel.spacing = unit(1, "lines")) +
  xlab(expression(paste("Hiring Selectivity Bandwidth of Small Firms ", s[1]))) +
  ylab(expression(paste("Cultural Change of Small Firms ", Delta,"c"))) +
  ylim(0,2)
g_change_s1

g_change_b1 <- df_behemoth %>%
  filter(r1_google %in% c(0.3,NA) & s1_google==0.3 & b1_google %in% c(0.7,NA) &
           r1_other==0.3 & s1_other==0.5) %>%
  ggplot(aes(x=b1_other, y=other_culture_change, col=factor(mobility, levels=c("Inter-firm Mobility", "Isolated Firms")))) +
  scale_color_manual(values=c("black","darkgrey"), guide=F) +
  geom_boxplot(aes(group=b1_other)) +
  facet_grid(rows=vars(mobility)) +
  theme(panel.spacing = unit(1, "lines")) +
  xlab(expression(paste("Initial Socialization Rate of Small Firms ", b[1]))) +
  ylab(expression(paste("Cultural Change of Small Firms ", Delta,"c"))) +
  ylim(0,2)
g_change_b1

b1s1_heatmap <- df_behemoth %>%
  filter(r1_google==0.3 & s1_google==0.3 & b1_google==0.7 &
           r1_other==0.9) %>%
  ggplot(aes(x=b1_other, y=s1_other)) +
  geom_tile(aes(fill=other_culture_change)) +
  theme_minimal() +
  scale_fill_viridis_c(option='H', #trans="log10",
                       name = "Small Firms' Cultural Change") +
  xlab("Small Firms' Socialization Rate") +
  ylab("Small Firms' Selectivity Bandwidth")
b1s1_heatmap

r1s1_heatmap <- df_behemoth %>%
  filter(r1_google==0.3 & s1_google==0.3 & b1_google==0.7 &
           b1_other==0.1) %>%
  ggplot(aes(x=r1_other, y=s1_other)) +
  geom_tile(aes(fill=other_culture_change)) +
  theme_minimal() +
  scale_fill_viridis_c(option='H', #trans="log10",
                       name = "Small Firms' Cultural Change") +
  xlab("Small Firms' Alienation Bandwidth") +
  ylab("Small Firms' Selectivity Bandwidth")
r1s1_heatmap

r1s1_smooth <- df_behemoth %>%
  filter(r1_google==0.3 & s1_google==0.3 & b1_google==0.7 &
           b1_other==0.1) %>%
  ggplot(aes(x=s1_other, y=other_culture_change, color=factor(r1_other))) +
  geom_smooth() +
  scale_color_viridis_d() +
  xlab("Small Firms' Selectivity Bandwidth") +
  ylab("Small Firms' Cultural Change")
r1s1_smooth







# ARCHIVE -----------------------------------------------------------------

socdata <- as.data.table(read.csv("data/2018-10-15_results_socbtwn.csv", header=T))
socdata2 <- as.data.table(read.csv("data/2018-10-16_results_socwin.csv", header=T))
socdata <- socdata %>% rbind(socdata2) %>% left_join(cond, by="cond")
socdata <- as.data.table(socdata)
socdata[s0==1, mobility := "Isolated Firms"]
socdata[s0==0.03, mobility := "Inter-firm Mobility"]
varwin_soc <- ggplot(socdata, aes(x=b1, y=varwin_end/varwin_start, col=factor(mobility, levels=c("Inter-firm Mobility", "Isolated Firms")))) +
  scale_color_manual(values=c("black","darkgrey"), guide=F) +
  geom_boxplot(aes(group=b1)) +
  facet_grid(rows=vars(mobility)) +
  theme(panel.spacing = unit(1, "lines")) +
  xlab(expression(paste("Initial Socialization Rate ", b[1]))) +
  ylab(expression(paste("Ratio of final to initial ", widehat(sigma)["within"]))) +
  scale_y_continuous(limits=c(0,15), breaks=seq(0,15,3), labels=NULL) +
  ylab(NULL)
ggsave(filename="figures/varwin_soc.png", plot=varwin_soc, units="in", width=3, height=6, pointsize=16)

# all together
alienation[s0==1, mobility := "Isolated Firms"]
alienation[s0==0.03, mobility := "Inter-firm Mobility"]
varwin_alienation <- ggplot(alienation, aes(x=r1, y=varwin_end/varwin_start, col=factor(mobility, levels=c("Inter-firm Mobility", "Isolated Firms")))) +
  scale_color_manual(values=c("black","darkgrey"), guide=F) +
  geom_boxplot(aes(group=r1), outlier.alpha = 0.5) +
  facet_grid(rows=vars(mobility)) +
  theme(panel.spacing = unit(1, "lines")) +
  scale_y_continuous(limits=c(0,15), breaks=seq(0,15,3)) +
  xlab(expression(paste("Turnover Alienation Bandwidth ", r[1]))) +
  ylab(expression(paste("Ratio of final to initial ", widehat(sigma)["within"])))
ggsave(filename="figures/varwin_alien.png", plot=varwin_alienation, units="in", width=3, height=6, pointsize=16)

b1s1_heatmap <- df_stronggoogle %>%
  filter(google_r1==0.1) %>%
  ggplot(aes(x=google_b1, y=google_s1)) +
  geom_tile(aes(fill=other_culture_change)) +
  theme_minimal() +
  scale_fill_viridis_c(option='H', #trans="log10",
                       name = "Other Firms' Cultural Change") +
  xlab("Paragon Socialization Rate") +
  ylab("Paragon Selectivity")
b1s1_heatmap

r1s1_heatmap <- df_stronggoogle %>%
  filter(google_b1==0.9) %>%
  ggplot(aes(x=google_r1, y=google_s1)) +
  geom_tile(aes(fill=other_culture_change)) +
  theme_minimal() +
  scale_fill_viridis_c(option='H', #trans="log10",
                       name = "Other Firms' Cultural Change") +
  xlab("Paragon Alienation") +
  ylab("Paragon Selectivity")
r1s1_heatmap

df_stronggoogle %>%
  ggplot(aes(x=google_s1, y=other_culture_change, color=factor(google_r1))) +
  facet_wrap(vars(google_b1)) +
  geom_smooth()

google_plot <- df_stronggoogle %>%
  filter(google_r1==0.1) %>%
  ggplot(aes(x=google_s1, y=other_culture_change, color=factor(google_b1))) +
  geom_smooth() + geom_jitter(alpha=0.3) +
  scale_color_viridis_d() +
  labs(color=expression(paste("Behemoth Socialization Rate ", b[1])),
       x=expression(paste("Behemoth Hiring Selection Bandwidth ",s[1])),
       y=expression(paste("Cultural Change of Small Firms ",Delta,c))) +
  theme_minimal()
google_plot
ggsave('google_plot.png', google_plot, width=7, height=4, units='in')

google_plot2 <- df_stronggoogle %>%
  filter(google_r1==0.1 & google_b1 %in% c(0.1,1)) %>%
  ggplot(aes(x=factor(google_s1), y=other_culture_change)) +
  geom_boxplot() +
  scale_color_viridis_d() +
  facet_wrap(vars(google_b1)) +
  labs(x=expression(paste("Behemoth Hiring Selection Bandwidth ",s[1])),
       y=expression(paste("Cultural Change of Small Firms ",Delta,c)))
google_plot2
