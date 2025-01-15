# A cryptic role for reciprocal helping in a cooperatively breeding bird
# Alexis Earl, ade2102@columbia.edu
# Gerry Carter, gc1511@princeton.edu

# This script runs many models of helping to simulate ability to detect evidence of effects of kinship and reciprocal help as a function of sampling time (number of seasons and years), i.e. a power analysis.

# clear workspace
rm(list=ls())

# load packages
library(tidyverse)
library(performance)
library(patchwork)
library(brms)

# pick how fast to run models
nchains = 4
chain_length = 5000
warmup_length = 1000

# get helping observations
d <-
  read.csv("data/daily_helping.csv") %>%
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
write.csv(d2, file= "results/power_analysis_results.csv")

