# Test of reciprocal helping bias and nepotism in helping decisions of superb starlings
# Alexis Earl, ade2102@columbia.edu
# Gerry Carter, gcarter1640@gmail.com

# clear workspace
rm(list=ls())

# load packages
library(tidyverse)

setwd("/Users/alexisearl/Library/CloudStorage/GoogleDrive-ade2102@columbia.edu/My Drive/PhD/Chapter 1/manuscript/Reciprocal_helping_and_kin_bias_in_superb_starlings/output")

# get helping observations
d <- read.csv("daily_helping.csv")

# function to get permutation test
hist_perm <- function(exp=exp, obs=obs, perms=perms, label=''){
  exp.range <- signif(quantile(exp, probs= c(0.025, 0.975)),3)
  ggplot()+
    geom_histogram(aes(x=exp), color="black",fill="light blue")+
    geom_vline(aes(xintercept=obs), color="red", size=1)+
    xlab("expected values from null model")+
    ggtitle(label, subtitle = paste('obs = ',signif(obs,3), ', exp 95% CI= ', exp.range[1], ' to ', exp.range[2], ", Prob exp >= obs: p", ifelse(mean(exp>=obs)==0,paste("<",1/perms), paste("=",signif(mean(exp>=obs),digits=2))),", permutations=",perms, sep=""))
}

# get help given and received among nonkin
d2 <-
  d %>%
  filter(help>=0) %>%
  mutate(kinship.max= ifelse(helper.dispersal== "I", microsat.kinship.max, kinship.max)) %>%
  filter(kinship.max <= 0) %>%
  group_by(mom.dyad, dad.dyad) %>%
  summarize(help= sum(help, na.rm=T), .groups= 'drop') %>%
  pivot_longer(mom.dyad:dad.dyad, names_to = 'type', values_to = 'dyad') %>%
  group_by(dyad) %>%
  summarize(help= mean(help, na.rm=T)) %>%
  separate(dyad, into = c("helper", "receiver"), sep= "-->", remove = F) %>%
  mutate(reciprocal.dyad = paste(receiver, helper, sep= "-->")) %>%
  mutate(udyad = ifelse(helper<receiver,
                        paste(helper, receiver, sep= "_"),
                        paste(receiver, helper, sep= "_")))
d2$reciprocal.help <- d2$help[match(d2$dyad, d2$reciprocal.dyad)]

# test social differentiation---------------
cv <- sd(d2$help) / mean(d2$help)
cv

# count reciprocal helping relationships under null model
perms <- 5000
exp.cv <-rep(NA, perms)

for (i in 1:perms) {

  # randomize helping rates
  t <-
    d %>%
    filter(help>=0) %>%
    mutate(kinship.max= ifelse(helper.dispersal== "I", microsat.kinship.max, kinship.max)) %>%
    filter(kinship.max <= 0) %>%
    group_by(date, nest) %>%
    mutate(help = sample(help, n())) %>%
    ungroup()

  t2 <-
    t %>%
    filter(help>=0) %>%
    group_by(mom.dyad, dad.dyad) %>%
    summarize(help= sum(help, na.rm=T), .groups= 'drop') %>%
    pivot_longer(mom.dyad:dad.dyad, names_to = 'type', values_to = 'dyad') %>%
    group_by(dyad) %>%
    summarize(help= mean(help, na.rm=T)) %>%
    separate(dyad, into = c("helper", "receiver"), sep= "-->", remove = F) %>%
    mutate(reciprocal.dyad = paste(receiver, helper, sep= "-->")) %>%
    mutate(udyad = ifelse(helper<receiver,
                          paste(helper, receiver, sep= "_"),
                          paste(receiver, helper, sep= "_")))
  d2$reciprocal.help <- d2$help[match(d2$dyad, d2$reciprocal.dyad)]

  # count reciprocal helping relationships
  exp.cv[i] <- sd(t2$help) / mean(t2$help)

  print(paste( i, "of", perms))
}

hist_perm(exp.cv, obs=cv, perms=perms)
mean(exp.cv>=cv)
