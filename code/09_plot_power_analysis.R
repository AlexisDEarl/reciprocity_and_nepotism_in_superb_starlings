# A cryptic role for reciprocal helping in a cooperatively breeding bird
# Alexis Earl, ade2102@columbia.edu
# Gerry Carter, gc1511@princeton.edu

# This script plots the power analysis.

# load packages
library(tidyverse)
library(performance)
library(patchwork)
library(brms)

# get data--------------
d2 <- read.csv("results/power_analysis_results.csv")

# remove models that failed to converge (CIs more than doubled from past season)
d3 <-
  d2 %>%
  mutate(conf= Q97.5-Q2.5) %>%
  group_by(term) %>%
  arrange(term, N.seasons) %>%
  mutate(bad= ((2.5*lag(conf) < conf))) %>%
  filter(!bad) %>%
  ungroup()

# how many estimates were removed?
d2 %>%
  group_by(term) %>%
  summarize(n=n())
d3 %>%
  group_by(term) %>%
  summarize(n=n())
# we removed 3 kinship and 4 reciprocal help estimates failed to converge and had CIs over 2.5 times larger than the previous season

# plot
(pplot2 <-
    d3 %>%
    ggplot(aes(x=N.seasons, y=Estimate,group= term, color=term))+
    geom_hline(yintercept = 0)+
    geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5, fill= term), alpha=0.7)+
    geom_line(aes(y=Q2.5), size=0.8)+
    geom_line(aes(y=Q97.5), size=0.8)+
    geom_line(size=1)+
    coord_cartesian(ylim= c(-2,2))+
    ylab("regression coefficient estimate")+
    xlab("number of sampled breeding seasons")+
    theme_classic()+
    theme(legend.position = c(0.75,0.3),
          legend.title = element_blank(),
          legend.background = element_blank())+
    scale_fill_grey(start = 0.2, end = 0.7)+
    scale_color_grey(start = 0.1, end = 0.5))+
  scale_linetype_manual(values=c("dashed", "solid"))

# save plot
ggsave(
  "results/Figure 6.pdf",
  plot = pplot2,
  scale = 1,
  width = 4,
  height = 4,
  units = c("in", "cm", "mm", "px"),
  dpi = 700)

# how many seasons to detect nepotism and reciprocal help?
d2 %>%
  mutate(detect= Q2.5>=0) %>%
  filter(detect) %>%
  group_by(term) %>%
  summarize(season= first(N.seasons))

