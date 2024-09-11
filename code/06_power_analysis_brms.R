# Test of reciprocal helping bias and nepotism in helping decisions of superb starlings
# Alexis Earl, ade2102@columbia.edu
# Gerry Carter, gcarter1640@gmail.com

# power analysis

# clear workspace
rm(list=ls())

# load packages
library(tidyverse)
library(glmmTMB)
library(broom.mixed)
library(performance)
library(patchwork)
library(brms)

# pick how fast to run models
nchains = 4
chain_length = 5000
warmup_length = 1000

# get helping observations
d <-
  read.csv("daily_helping.csv") %>%
  as_tibble() %>%
  # label helper-nest dyads
  mutate(helper_nest= paste(helper,nest)) %>%
  # use microsat kinship for immigrants
  mutate(kinship.max= ifelse(helper.dispersal== "I", microsat.kinship.max, kinship.max)) %>%
  mutate(reciprocal.help= reciprocal.help.max>0) %>%
  filter(help>=0)

# get reciprocal helping and nepotism across seasons

seasons <- sort(unique(d$breeding_season))

# create empty list
ci.list <- list()

for (i in 1:length(seasons)) {

  # get all data up to season i
  season <- seasons[i]
  da <-
    d %>%
    filter(breeding_season <= season)

  # re-estimate reciprocal help
  # get reciprocal helping relationships
  t <-
    da %>%
    group_by(mom.dyad, dad.dyad) %>%
    summarize(help= sum(help, na.rm=T), .groups= 'drop') %>%
    pivot_longer(mom.dyad:dad.dyad, names_to = 'type', values_to = 'dyad') %>%
    group_by(dyad) %>%
    summarize(help= mean(help, na.rm=T)) %>%
    separate(dyad, into = c("helper", "receiver"), sep= "-->", remove = F) %>%
    mutate(reciprocal.dyad = paste(receiver, helper, sep= "-->"))

  da$reciprocal.help.mom <- t$help[match(da$mom.dyad, t$reciprocal.dyad)]
  da$reciprocal.help.dad <- t$help[match(da$dad.dyad, t$reciprocal.dyad)]
  da$reciprocal.help.max <- ifelse(da$reciprocal.help.dad > da$reciprocal.help.mom, da$reciprocal.help.dad, da$reciprocal.help.mom)
  da$reciprocal.help <- da$reciprocal.help.max > 0

  # get number of seasons
  n.seasons <- n_distinct(da$breeding_season)
  n.obs <- nrow(da)

  # fit models
  t1 <-
    brm(help ~
          scale(kinship.max) +
          offset(log(sample.duration)) +
          (1|helper) +
          (1|nest)+
          (1|helper_nest),
        data = da,
        family = "negbinomial",
        cores = nchains,
        chains = nchains,
        iter = chain_length,
        warmup = warmup_length)

  t2 <-
    brm(help ~
          reciprocal.help +
          offset(log(sample.duration)) +
          (1|helper) +
          (1|nest)+
          (1|helper_nest),
        data = da,
        family = "negbinomial",
        cores = nchains,
        chains = nchains,
        iter = chain_length,
        warmup = warmup_length)

# get CIs
  ci1 <-
    fixef(t1) %>%
    as_tibble() %>%
    mutate(N.seasons= n.seasons) %>%
    mutate(N.obs = n.obs)

  ci2 <-
    fixef(t2) %>%
    as_tibble() %>%
    mutate(N.seasons= n.seasons) %>%
    mutate(N.obs = n.obs)


  # combine them and add to list
  ci.list[[i]] <-  rbind(ci1,ci2)

  # show progress
  print(paste(i,"of",length(seasons)))
}

# convert list to dataframe
d2 <-
  bind_rows(ci.list) %>%
  mutate(term= rep(c("intercept", "kinship", "intercept", "reciprocal help"), 40)) %>%
  filter(term != "intercept")

# save data
write.csv(d2, file= "results/power_analysis_results_brms.csv")

# read data--------------
d2 <- read.csv("results/power_analysis_results_brms.csv")

# remove models that failed to converge (CIs is more than doubled from past season)
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
    ylab("coefficient estimate")+
    xlab("number of sampled breeding seasons")+
    theme_bw()+
    theme(legend.position = c(0.75,0.3),
          legend.title = element_blank(),
          legend.background = element_blank())+
    scale_fill_grey(start = 0.2, end = 0.7)+
    scale_color_grey(start = 0.1, end = 0.5))+
    scale_linetype_manual(values=c("dashed", "solid"))

# save plot
ggsave(
  "results/power analysis_brms.pdf",
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



