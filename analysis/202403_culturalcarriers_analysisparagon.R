library(tidyverse)

df_stronggoogle <- read.csv("strong_google_all.csv", header=T)
df_amazongoogle <- read.csv("alldata.csv", header=T)

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

df_amazongoogle %>%
  ggplot(aes(x=google_s1, y=SD_ratio, color=factor(google_r1))) +
  facet_wrap(vars(google_b1)) +
  geom_smooth()

df <- df_amazongoogle %>%
  pivot_longer(
    cols = matches("other[:digit:]_c"),
    names_to = "firm",
    names_prefix = "other",
    values_to = "culture")
