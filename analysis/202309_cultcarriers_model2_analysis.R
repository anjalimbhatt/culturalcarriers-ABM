# September 2023
# Anjali Bhatt
#
# Code to analyze data produced by model 2: 1 behemoth paragon (e.g., Google)

library(tidyverse)
library(data.table)
library(plm)
library(stargazer)
library(gridExtra)
library(scales)

# read in data
setwd("~/My Drive/02 Research/Honglin/cultural carrier/")
df <- as.data.table(read.csv("data/regression/overall.csv", header=T))
df <- as.data.table(read.csv("data/two_firms_culture.csv", header=T))
df <- as.data.table(read.csv("polarization/average.csv", header=T))
df2 <- as.data.table(read.csv("polarization/alldata.csv", header=T))

df2 %>% sort_by

# heatmaps for cultural drift
hm1 <- df %>%
  filter(google_r1==0.25 & google_s0==0.03) %>%
  group_by(google_s1, google_b1) %>%
  summarize(dv = mean(other_culture_change)) %>%
  ggplot(aes(google_s1, google_b1)) + geom_raster(aes(fill = dv)) +
  scale_fill_gradient2(limits=c(0.35,0.85),
                       low = muted("blue"), mid = "white", high = muted("red"), midpoint = 0.6,
                       name = "Cultural Drift by Other Firms") +
  xlab(expression(paste("Behemoth's Hiring Selection Bandwidth ", s[1]))) +
  ylab(expression(paste("Behemoth's Initial Socialization Rate ", b[1])))

hm1

hm2 <- df %>%
  filter(google_r1==0.25 & google_s0==0.03) %>%
  group_by(google_s1, google_b1) %>%
  summarize(dv = mean(google_culture_change)) %>%
  ggplot(aes(google_s1, google_b1)) + geom_raster(aes(fill = dv)) +
  scale_fill_gradient2(limits=c(-1.5,0),
    low = muted("blue"), mid = "white", high = muted("red"), midpoint = -0.75,
    name = "Cultural Drift by Behemoth") +
  xlab(expression(paste("Behemoth's Hiring Selection Bandwidth ", s[1]))) +
  ylab(expression(paste("Behemoth's Initial Socialization Rate ", b[1])))

hm2

hm3 <- df %>%
  filter(google_s1==0.5 & google_s0==0.03) %>%
  group_by(google_r1, google_b1) %>%
  summarize(dv = mean(other_culture_change)) %>%
  ggplot(aes(google_r1, google_b1)) + geom_raster(aes(fill = dv)) +
  scale_fill_gradient2(limits=c(0.35,1),
                       low = muted("blue"), mid = "white", high = muted("red"), midpoint = 0.7,
                       name = "Cultural Drift by Other Firms") +
  xlab(expression(paste("Behemoth's Retention Bandwidth ", r[1]))) +
  ylab(expression(paste("Behemoth's Initial Socialization Rate ", b[1])))

hm3

hm4 <- df %>%
  filter(google_s1==0.5 & google_s0==0.03) %>%
  group_by(google_r1, google_b1) %>%
  summarize(dv = mean(google_culture_change)) %>%
  ggplot(aes(google_r1, google_b1)) + geom_raster(aes(fill = dv)) +
  scale_fill_gradient2(limits=c(-1.5,0),
                       low = muted("blue"), mid = "white", high = muted("red"), midpoint = -0.75,
                       name = "Cultural Drift by Behemoth") +
  xlab(expression(paste("Behemoth's Retention Bandwidth ", r[1]))) +
  ylab(expression(paste("Behemoth's Initial Socialization Rate ", b[1])))

hm4

hm5 <- df %>%
  filter(google_b1==0.2 & google_s0==0.03) %>%
  group_by(google_r1, google_s1) %>%
  summarize(dv = mean(other_culture_change)) %>%
  ggplot(aes(google_r1, google_s1)) + geom_raster(aes(fill = dv)) +
  scale_fill_gradient2(limits=c(0.35,1),
                       low = muted("blue"), mid = "white", high = muted("red"), midpoint = 0.7,
                       name = "Cultural Drift by Other Firms") +
  xlab(expression(paste("Behemoth's Retention Bandwidth ", r[1]))) +
  ylab(expression(paste("Behemoth's Hiring Selection Bandwidth ", s[1])))

hm5

hm6 <- df %>%
  filter(google_b1==0.2 & google_s0==0.03) %>%
  group_by(google_r1, google_s1) %>%
  summarize(dv = mean(google_culture_change)) %>%
  ggplot(aes(google_r1, google_s1)) + geom_raster(aes(fill = dv)) +
  scale_fill_gradient2(limits=c(-1.5,0),
                       low = muted("blue"), mid = "white", high = muted("red"), midpoint = -0.75,
                       name = "Cultural Drift by Behemoth") +
  xlab(expression(paste("Behemoth's Retention Bandwidth ", r[1]))) +
  ylab(expression(paste("Behemoth's Hiring Selection Bandwidth ", s[1])))

hm6






df %>%
  # filter(google_s0==1) %>%
  filter(google_s1==1 & google_r1==0.25 & google_s0==0.03) %>%
  ggplot(aes(x=google_b1, y=other_culture_change, group=google_b1)) + geom_boxplot()

df %>% ggplot(aes(x=s1, y=google_s0.1, group=s1)) + geom_boxplot() # this is consistent - when s0=1 for google, google's culture doesn't change as much
df %>% ggplot(aes(x=s1, y=other_s0.1)) + geom_jitter() + geom_smooth(method='lm') # this is also consistent
df %>% ggplot(aes(x=tenure_google, y=other_s0.1)) + geom_jitter() + geom_smooth(method='lm') # 

reg1 <- lm(other_firm ~ s1 + 1, data=df)
reg2 <- lm(other_firm ~ s1 + google + 1, data=df)
reg3 <- lm(google ~ s1 + 1, data=df)

stargazer(
  reg1, reg2, reg3,
  no.space=TRUE,
  star.char = c("+", "*", "**", "***"),
  star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
  notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"), 
  notes.append = F)

reg4 <- lm(other_s0.1 ~ s1 + 1, data=df)
reg5 <- lm(other_s0.1 ~ s1 + tenure_google + 1, data=df)
reg6 <- lm(tenure_google ~ s1 + 1, data=df)
reg7 <- lm(other_s0.1 ~ google_s0.1 + 1, data=df)

stargazer(
  reg4, reg5, reg6, reg7,
  no.space=TRUE,
  star.char = c("+", "*", "**", "***"),
  star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
  notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"), 
  notes.append = F)


