# A cryptic role for reciprocal helping in a cooperatively breeding bird
# Alexis Earl, ade2102@columbia.edu
# Gerry Carter, gc1511@princeton.edu

# This script fits Bayesian negative binomial models for predicting helping rates and saves the results.

# This script takes about 1.5 hours to run on a 2021 Macbook Pro

# clear workspace
rm(list=ls())

# OPTIONAL: install Bayesian packages
#install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
options(mc.cores = parallel::detectCores())

# load packages
library(tidyverse)
library(performance)
library(patchwork)
library(rstan)
library(brms)
library(tidybayes)

# set timer to measure run time
start <- Sys.time()

# set chains and chain length
nchains = 4
chain_length = 5000
warmup_length = 1000

# get helping observations
d <-
  read.csv("daily_helping.csv") %>%
  as_tibble() %>%
  # label helper-nest dyads
  mutate(helper_nest= paste(helper,nest)) %>%
  # use microsatellite estimates of kinship for immigrants
  mutate(kinship.max= ifelse(helper.dispersal== "I", microsat.kinship.max, kinship.max)) %>%
  # get observed helping and possible helping
  filter(help>=0) %>%
  # label reciprocal help
  mutate(reciprocal.help= reciprocal.help.max>0)


### KINSHIP MODEL OF HELPING ###############################################

### model kin-biased helping by type----

# what is mean kinship?
mean(d$kinship.max, na.rm=T)

# what is 1 std dev in kinship?
sd(d$kinship.max, na.rm=T)
# 0.1943897

# create function to fit model
fit_model <- function(data= data){
  brm(help ~
        scale(kinship.max) +
        offset(log(sample.duration)) +
        (1|helper) +
        (1|nest)+
        (1|helper_nest),
      data = data,
      family = "negbinomial",
      cores = nchains,
      chains = nchains,
      iter = chain_length,
      warmup = warmup_length)
}

# get observations that have kinship
d2 <-
  d %>%
  filter(!is.na(kinship.max))

# fit model for all birds
fit.all <- fit_model(data= d2)

# get coefficient estimates
ci.all <-
  fixef(fit.all) %>%
  as_tibble(rownames= "name") %>%
  mutate(type= "all")

# get observations of female immigrants
fi <-
  d2 %>%
  filter(helper.sex== "F",
         helper.dispersal == "I")

# fit model for female immigrants
fit.fi <- fit_model(data= fi)

# get coeffs
ci.fi <-
  fixef(fit.fi) %>%
  as_tibble(rownames= "name") %>%
  mutate(type= "immigrant female")

# get observations of male immigrants
mi <-
  d2 %>%
  filter(helper.sex== "M",
         helper.dispersal == "I")

fit.mi <- fit_model(data= mi)

ci.mi <-
  fixef(fit.mi) %>%
  as_tibble(rownames= "name") %>%
  mutate(type= "immigrant male")

# male residents
mr <-
  d2 %>%
  filter(helper.sex== "M",
         helper.dispersal == "N")

fit.mr <- fit_model(data= mr)

ci.mr <-
  fixef(fit.mr) %>%
  as_tibble(rownames= "name") %>%
  mutate(type= "resident male")

# female residents
fr <-
  d2 %>%
  filter(helper.sex== "F",
         helper.dispersal == "N")

fit.fr <- fit_model(data= fr)

ci.fr <-
  fixef(fit.fr) %>%
  as_tibble(rownames= "name") %>%
  mutate(type= "resident female")

# get number of observations
n.all <- NA
n.mi <- NA
n.fi <- NA
n.mr <- NA
n.fr <- NA
n.all <- nrow(d)
n.mi <- nrow(mi)
n.fi <- nrow(fi)
n.mr <- nrow(mr)
n.fr <- nrow(fr)

# get number of individuals
n.all2 <- NA
n.mi2 <- NA
n.fi2 <- NA
n.mr2 <- NA
n.fr2 <- NA
n.all2 <- d2 %>% pull(helper) %>% n_distinct()
n.mi2 <- mi %>% pull(helper) %>% n_distinct()
n.fi2 <- fi %>% pull(helper) %>% n_distinct()
n.mr2 <- mr %>% pull(helper) %>% n_distinct()
n.fr2 <- fr %>% pull(helper) %>% n_distinct()

#### compile results----------
results1 <-
  rbind(ci.all, ci.mi, ci.fi, ci.mr, ci.fr) %>%
  mutate(n.obs = case_when(
    type == "all" ~ n.all,
    type == "immigrant female" ~ n.fi,
    type == "immigrant male" ~ n.mi,
    type == "resident female" ~ n.fr,
    type == "resident male" ~ n.mr)) %>%
  mutate(n.birds = case_when(
    type == "all" ~ n.all2,
    type == "immigrant female" ~ n.fi2,
    type == "immigrant male" ~ n.mi2,
    type == "resident female" ~ n.fr2,
    type == "resident male" ~ n.mr2)) %>%
  mutate(test= "nepotism")

#### get summary-----------------

s.all <-
  summary(fit.all)$fixed %>%
  as_tibble(rownames= "name") %>%
  mutate(sample = "all") %>%
  mutate(model = 'nepotism')

s.fi <-
  summary(fit.fi)$fixed %>%
  as_tibble(rownames= "name") %>%
  mutate(sample = "immigrant females") %>%
  mutate(model = 'nepotism')

s.mi <-
  summary(fit.mi)$fixed %>%
  as_tibble(rownames= "name") %>%
  mutate(sample = "immigrant males") %>%
  mutate(model = 'nepotism')

s.mr <-
  summary(fit.mr)$fixed %>%
  as_tibble(rownames= "name") %>%
  mutate(sample = "resident males") %>%
  mutate(model = 'nepotism')

s.fr <-
  summary(fit.fr)$fixed %>%
  as_tibble(rownames= "name") %>%
  mutate(sample = "resident females") %>%
  mutate(model = 'nepotism')

# save summary of models
sum1 <- rbind(s.all, s.fi, s.mi, s.mr, s.fr)

# save models
nepotism.models <- list(fit.all, fit.mi, fit.fi, fit.mr, fit.fr)

#### get samples from posterior distribution-------
pk.all <-
  fit.all %>%
  spread_draws(b_scalekinship.max) %>%
  mutate(model = "Kinship both both") %>%
  pivot_longer(b_scalekinship.max, names_to = 'term', values_to= 'coeff')
pk.fi <-
  fit.fi %>%
  spread_draws(b_scalekinship.max) %>%
  mutate(model = "Kinship immigrant female") %>%
  pivot_longer(b_scalekinship.max, names_to = 'term', values_to= 'coeff')
pk.mi <-
  fit.mi %>%
  spread_draws(b_scalekinship.max) %>%
  mutate(model = "Kinship immigrant male") %>%
  pivot_longer(b_scalekinship.max, names_to = 'term', values_to= 'coeff')
pk.mr <-
  fit.mr %>%
  spread_draws(b_scalekinship.max) %>%
  mutate(model = "Kinship resident male") %>%
  pivot_longer(b_scalekinship.max, names_to = 'term', values_to= 'coeff')
pk.fr <-
  fit.fr %>%
  spread_draws(b_scalekinship.max) %>%
  mutate(model = "Kinship resident female") %>%
  pivot_longer(b_scalekinship.max, names_to = 'term', values_to= 'coeff')

# compile posterior distributions
(all_post.kinship <- rbind(pk.all,pk.fi, pk.mi, pk.fr, pk.mr))

# erase model fits
rm(s.all, s.fi, s.mi, s.mr, s.fr)
rm(ci.all, ci.mi, ci.fi, ci.mr, ci.fr, fit.all, fit.mi, fit.fi, fit.mr, fit.fr)

### RECIPROCAL HELP MODEL ###############################################
# create function to fit model
fit_model <- function(data= data){
  brm(help ~
        reciprocal.help +
        offset(log(sample.duration)) +
        (1|helper) +
        (1|nest)+
        (1|helper_nest),
      data = data,
      family = "negbinomial",
      cores = nchains,
      chains = nchains,
      iter = chain_length,
      warmup = warmup_length)
}

# get observations where reciprocal help was possible to observe
d2 <-
  d %>%
  filter(!is.na(reciprocal.help))

# fit model for all birds
fit.all <- fit_model(data= d2)

ci.all <-
  fixef(fit.all) %>%
  as_tibble(rownames= "name") %>%
  mutate(type= "all")

# female immigrants
fi <-
  d2 %>%
  filter(helper.sex== "F",
         helper.dispersal == "I")

fit.fi <- fit_model(data= fi)

ci.fi <-
  fixef(fit.fi) %>%
  as_tibble(rownames= "name") %>%
  mutate(type= "immigrant female")

# male immigrants
mi <-
  d2 %>%
  filter(helper.sex== "M",
         helper.dispersal == "I")

fit.mi <- fit_model(data= mi)

ci.mi <-
  fixef(fit.mi) %>%
  as_tibble(rownames= "name") %>%
  mutate(type= "immigrant male")

# male residents
mr <-
  d2 %>%
  filter(helper.sex== "M",
         helper.dispersal == "N")

fit.mr <- fit_model(data= mr)

ci.mr <-
  fixef(fit.mr) %>%
  as_tibble(rownames= "name") %>%
  mutate(type= "resident male")

# get number of observations
n.all <- NA
n.mi <- NA
n.fi <- NA
n.mr <- NA
n.fr <- NA
n.all <- nrow(d)
n.mi <- nrow(mi)
n.fi <- nrow(fi)
n.mr <- nrow(mr)
n.fr <- nrow(fr)

# get number of individuals
n.all2 <- NA
n.mi2 <- NA
n.fi2 <- NA
n.mr2 <- NA
n.fr2 <- NA
n.all2 <- d2 %>% pull(helper) %>% n_distinct()
n.mi2 <- mi %>% pull(helper) %>% n_distinct()
n.fi2 <- fi %>% pull(helper) %>% n_distinct()
n.mr2 <- mr %>% pull(helper) %>% n_distinct()
n.fr2 <- fr %>% pull(helper) %>% n_distinct()

#### compile results----------
results2 <-
  rbind(ci.all, ci.mi, ci.fi, ci.mr) %>%
  mutate(n.obs = case_when(
    type == "all" ~ n.all,
    type == "immigrant female" ~ n.fi,
    type == "immigrant male" ~ n.mi,
    type == "resident female" ~ n.fr,
    type == "resident male" ~ n.mr)) %>%
  mutate(n.birds = case_when(
    type == "all" ~ n.all2,
    type == "immigrant female" ~ n.fi2,
    type == "immigrant male" ~ n.mi2,
    type == "resident female" ~ n.fr2,
    type == "resident male" ~ n.mr2)) %>%
  mutate(test= "reciprocity")

#### get summary-----------------

# function to get summary
s.all <-
  summary(fit.all)$fixed %>%
  as_tibble(rownames= "name") %>%
  mutate(sample = "all") %>%
  mutate(model = 'reciprocity')

s.fi <-
  summary(fit.fi)$fixed %>%
  as_tibble(rownames= "name") %>%
  mutate(sample = "immigrant females") %>%
  mutate(model = 'reciprocity')

s.mi <-
  summary(fit.mi)$fixed %>%
  as_tibble(rownames= "name") %>%
  mutate(sample = "immigrant males") %>%
  mutate(model = 'reciprocity')

s.mr <-
  summary(fit.mr)$fixed %>%
  as_tibble(rownames= "name") %>%
  mutate(sample = "resident males") %>%
  mutate(model = 'reciprocity')

# get model summaries
sum2 <- rbind(s.all, s.fi, s.mi, s.mr)

# save models
reciprocity.models <- list(fit.all, fit.mi, fit.fi, fit.mr)

#### get samples from posterior distribution-------
p.all <-
  fit.all %>%
  spread_draws(b_reciprocal.helpTRUE) %>%
  mutate(model = "Reciprocal_help both both") %>%
  pivot_longer(b_reciprocal.helpTRUE, names_to = 'term', values_to= 'coeff')
p.fi <-
  fit.fi %>%
  spread_draws(b_reciprocal.helpTRUE) %>%
  mutate(model = "Reciprocal_help immigrant female") %>%
  pivot_longer(b_reciprocal.helpTRUE, names_to = 'term', values_to= 'coeff')
p.mi <-
  fit.mi %>%
  spread_draws(b_reciprocal.helpTRUE) %>%
  mutate(model = "Reciprocal_help immigrant male") %>%
  pivot_longer(b_reciprocal.helpTRUE, names_to = 'term', values_to= 'coeff')
p.mr <-
  fit.mr %>%
  spread_draws(b_reciprocal.helpTRUE) %>%
  mutate(model = "Reciprocal_help resident male") %>%
  pivot_longer(b_reciprocal.helpTRUE, names_to = 'term', values_to= 'coeff')

# compile posterior distributions
(all_post.reciprocity <- rbind(p.all,p.fi, p.mi, p.mr))

# erase model fits
rm(s.all, s.fi, s.mi, s.mr)
rm(ci.all, ci.mi, ci.fi, ci.mr, ci.fr, fit.all, fit.mi, fit.fi, fit.mr)

### RECIPROCAL HELP & KINSHIP MODEL ###############################################

# create function to fit model
fit_model <- function(data= data){
  brm(help ~
      reciprocal.help+
      scale(kinship.max) +
      offset(log(sample.duration)) +
      (1|helper) +
      (1|nest)+
      (1|helper_nest),
    data = data,
    family = "negbinomial",
    cores = nchains,
    chains = nchains,
    iter = chain_length,
    warmup = warmup_length)
}

# get observations with both kinship and possibility of reciprocal help
d2 <-
  d %>%
  filter(!is.na(reciprocal.help)) %>%
  filter(!is.na(kinship.max))

# fit for all individuals
fit.all <- fit_model(data= d2)

ci.all <-
  fixef(fit.all) %>%
  as_tibble(rownames= "name") %>%
  mutate(type= "all")


# female immigrants
fi <-
  d2 %>%
  filter(helper.sex== "F",
         helper.dispersal == "I")

fit.fi <- fit_model(data= fi)

ci.fi <-
  fixef(fit.fi) %>%
  as_tibble(rownames= "name") %>%
  mutate(type= "immigrant female")

# male immigrants
mi <-
  d2 %>%
  filter(helper.sex== "M",
         helper.dispersal == "I")

fit.mi <- fit_model(data= mi)

ci.mi <-
  fixef(fit.mi) %>%
  as_tibble(rownames= "name") %>%
  mutate(type= "immigrant male")

# male residents
mr <-
  d2 %>%
  filter(helper.sex== "M",
         helper.dispersal == "N")

fit.mr <- fit_model(data= mr)

ci.mr <-
  fixef(fit.mr) %>%
  as_tibble(rownames= "name") %>%
  mutate(type= "resident male")


# get number of observations
n.all <- NA
n.mi <- NA
n.fi <- NA
n.mr <- NA
n.fr <- NA
n.all <- nrow(d)
n.mi <- nrow(mi)
n.fi <- nrow(fi)
n.mr <- nrow(mr)

# get number of individuals
n.all2 <- NA
n.mi2 <- NA
n.fi2 <- NA
n.mr2 <- NA
n.fr2 <- NA
n.all2 <- d2 %>% pull(helper) %>% n_distinct()
n.mi2 <- mi %>% pull(helper) %>% n_distinct()
n.fi2 <- fi %>% pull(helper) %>% n_distinct()
n.mr2 <- mr %>% pull(helper) %>% n_distinct()

##### compile results-------------
results3 <-
  rbind(ci.all, ci.mi, ci.fi, ci.mr) %>%
  mutate(n.obs = case_when(
    type == "all" ~ n.all,
    type == "immigrant female" ~ n.fi,
    type == "immigrant male" ~ n.mi,
    type == "resident female" ~ n.fr,
    type == "resident male" ~ n.mr)) %>%
  mutate(n.birds = case_when(
    type == "all" ~ n.all2,
    type == "immigrant female" ~ n.fi2,
    type == "immigrant male" ~ n.mi2,
    type == "resident female" ~ n.fr2,
    type == "resident male" ~ n.mr2)) %>%
  mutate(test= "reciprocity and nepotism")

#### get summary-----------
s.all <-
  summary(fit.all)$fixed %>%
  as_tibble(rownames= "name") %>%
  mutate(sample = "all") %>%
  mutate(model = 'reciprocity and nepotism')

s.fi <-
  summary(fit.fi)$fixed %>%
  as_tibble(rownames= "name") %>%
  mutate(sample = "immigrant females") %>%
  mutate(model = 'reciprocity and nepotism')

s.mi <-
  summary(fit.mi)$fixed %>%
  as_tibble(rownames= "name") %>%
  mutate(sample = "immigrant males") %>%
  mutate(model = 'reciprocity and nepotism')

s.mr <-
  summary(fit.mr)$fixed %>%
  as_tibble(rownames= "name") %>%
  mutate(sample = "resident males") %>%
  mutate(model = 'reciprocity and nepotism')

sum3 <-
  rbind(s.all, s.fi, s.mi, s.mr)

# save models
reciprocity_nepotism.models <- list(fit.all, fit.mi, fit.fi, fit.mr)

#### get samples from posterior distribution-------
p.all <-
  fit.all %>%
  spread_draws(b_reciprocal.helpTRUE, b_scalekinship.max) %>%
  mutate(model = "Reciprocal_Kinship both both") %>%
  pivot_longer(b_reciprocal.helpTRUE:b_scalekinship.max , names_to = 'term', values_to= 'coeff')
p.fi <-
  fit.fi %>%
  spread_draws(b_reciprocal.helpTRUE, b_scalekinship.max) %>%
  mutate(model = "Reciprocal_Kinship immigrant female") %>%
  pivot_longer(b_reciprocal.helpTRUE:b_scalekinship.max , names_to = 'term', values_to= 'coeff')
p.mi <-
  fit.mi %>%
  spread_draws(b_reciprocal.helpTRUE, b_scalekinship.max) %>%
  mutate(model = "Reciprocal_Kinship immigrant male") %>%
  pivot_longer(b_reciprocal.helpTRUE:b_scalekinship.max , names_to = 'term', values_to= 'coeff')
p.mr <-
  fit.mr %>%
  spread_draws(b_reciprocal.helpTRUE, b_scalekinship.max) %>%
  mutate(model = "Reciprocal_Kinship resident male") %>%
  pivot_longer(b_reciprocal.helpTRUE:b_scalekinship.max , names_to = 'term', values_to= 'coeff')

# compile posterior distributions
(all_post.reciprocity_kinship <- rbind(p.all,p.fi, p.mi, p.mr))

# erase model fits
rm(s.all, s.fi, s.mi, s.mr)
rm(ci.all, ci.mi, ci.fi, ci.mr, ci.fr, fit.all, fit.mi, fit.fi, fit.mr)

### INTERACTION BETWEEN RECIPROCAL AND KINSHIP MODEL ###############################################

# create function to fit model
fit_model <- function(data= data){
  brm(help ~
        reciprocal.help*scale(kinship.max) +
        offset(log(sample.duration)) +
        (1|helper) +
        (1|nest)+
        (1|helper_nest),
      data = data,
      family = "negbinomial",
      cores = nchains,
      chains = nchains,
      iter = chain_length,
      warmup = warmup_length)
}

# get observations with both kinship and possibility of reciprocal help
d2 <-
  d %>%
  filter(!is.na(reciprocal.help)) %>%
  filter(!is.na(kinship.max))

# fit for all individuals
fit.all <- fit_model(data= d2)

ci.all <-
  fixef(fit.all) %>%
  as_tibble(rownames= "name") %>%
  mutate(type= "all")

# female immigrants
fi <-
  d2 %>%
  filter(helper.sex== "F",
         helper.dispersal == "I")

fit.fi <- fit_model(data= fi)

ci.fi <-
  fixef(fit.fi) %>%
  as_tibble(rownames= "name") %>%
  mutate(type= "immigrant female")

# male immigrants
mi <-
  d2 %>%
  filter(helper.sex== "M",
         helper.dispersal == "I")

fit.mi <- fit_model(data= mi)

ci.mi <-
  fixef(fit.mi) %>%
  as_tibble(rownames= "name") %>%
  mutate(type= "immigrant male")

# male residents
mr <-
  d2 %>%
  filter(helper.sex== "M",
         helper.dispersal == "N")

fit.mr <- fit_model(data= mr)

ci.mr <-
  fixef(fit.mr) %>%
  as_tibble(rownames= "name") %>%
  mutate(type= "resident male")

fixef(fit.mr)
summary(fit.mr)$fixed
str(summary(fit.mr))

# get number of observations
n.all <- NA
n.mi <- NA
n.fi <- NA
n.mr <- NA
n.fr <- NA
n.all <- nrow(d)
n.mi <- nrow(mi)
n.fi <- nrow(fi)
n.mr <- nrow(mr)

# get number of individuals
n.all2 <- NA
n.mi2 <- NA
n.fi2 <- NA
n.mr2 <- NA
n.fr2 <- NA
n.all2 <- d2 %>% pull(helper) %>% n_distinct()
n.mi2 <- mi %>% pull(helper) %>% n_distinct()
n.fi2 <- fi %>% pull(helper) %>% n_distinct()
n.mr2 <- mr %>% pull(helper) %>% n_distinct()

#### compile results ---------------
results4 <-
  rbind(ci.all, ci.mi, ci.fi, ci.mr) %>%
  mutate(n.obs = case_when(
    type == "all" ~ n.all,
    type == "immigrant female" ~ n.fi,
    type == "immigrant male" ~ n.mi,
    type == "resident female" ~ n.fr,
    type == "resident male" ~ n.mr)) %>%
  mutate(n.birds = case_when(
    type == "all" ~ n.all2,
    type == "immigrant female" ~ n.fi2,
    type == "immigrant male" ~ n.mi2,
    type == "resident female" ~ n.fr2,
    type == "resident male" ~ n.mr2)) %>%
  mutate(test= "reciprocity x nepotism interaction")

#### get summary-----------
s.all <-
  summary(fit.all)$fixed %>%
  as_tibble(rownames= "name") %>%
  mutate(sample = "all") %>%
  mutate(model = 'reciprocity x nepotism interaction')

s.fi <-
  summary(fit.fi)$fixed %>%
  as_tibble(rownames= "name") %>%
  mutate(sample = "immigrant females") %>%
  mutate(model = 'reciprocity x nepotism interaction')

s.mi <-
  summary(fit.mi)$fixed %>%
  as_tibble(rownames= "name") %>%
  mutate(sample = "immigrant males") %>%
  mutate(model = 'reciprocity x nepotism interaction')

s.mr <-
  summary(fit.mr)$fixed %>%
  as_tibble(rownames= "name") %>%
  mutate(sample = "resident males") %>%
  mutate(model = 'reciprocity x nepotism interaction')

sum4 <-
  rbind(s.all, s.fi, s.mi, s.mr)

# save models
interaction.models <- list(fit.all, fit.mi, fit.fi, fit.mr)

#### get samples from posterior distribution-------
p.all <-
  fit.all %>%
  spread_draws(`b_reciprocal.helpTRUE:scalekinship.max`) %>%
  mutate(model = "Interaction both both") %>%
  pivot_longer(`b_reciprocal.helpTRUE:scalekinship.max`, names_to = 'term', values_to= 'coeff')
p.fi <-
  fit.fi %>%
  spread_draws(`b_reciprocal.helpTRUE:scalekinship.max`) %>%
  mutate(model = "Interaction immigrant female") %>%
  pivot_longer(`b_reciprocal.helpTRUE:scalekinship.max`, names_to = 'term', values_to= 'coeff')
p.mi <-
  fit.mi %>%
  spread_draws(`b_reciprocal.helpTRUE:scalekinship.max`) %>%
  mutate(model = "Interaction immigrant male") %>%
  pivot_longer(`b_reciprocal.helpTRUE:scalekinship.max`, names_to = 'term', values_to= 'coeff')
p.mr <-
  fit.mr %>%
  spread_draws(`b_reciprocal.helpTRUE:scalekinship.max`) %>%
  mutate(model = "Interaction resident male") %>%
  pivot_longer(`b_reciprocal.helpTRUE:scalekinship.max`, names_to = 'term', values_to= 'coeff')

# compile posterior distributions
(all_post.interact <- rbind(p.all, p.fi, p.mi, p.mr))

# erase model fits
rm(s.all, s.fi, s.mi, s.mr)
rm(ci.all, ci.mi, ci.fi, ci.mr, ci.fr, fit.all, fit.mi, fit.fi, fit.mr)

# SAVE RESULTS FOR MAIN ANALYSIS --------

# get sample sizes
ss <-
  rbind(results1, results2, results3, results4) %>%
  mutate(model = test) %>%
  mutate(sample = case_when(
    type == "immigrant male" ~ "immigrant males",
    type == "immigrant female" ~ "immigrant females",
    type == "resident male" ~ "resident males",
    type == "resident female" ~ "resident females",
    TRUE ~ type)) %>%
  select(model, sample, name, N_observations= n.obs, N_birds= n.birds)

# save results
results <-
  rbind(sum1, sum2, sum3, sum4) %>%
  mutate(model = ifelse(model == 'reciprocity X nepotism interaction',
                        'reciprocity x nepotism interaction',
                        model)) %>%
  full_join(ss) %>%
  filter(name!= "Intercept") %>%
  mutate(name= case_when(
    name == "reciprocal.helpTRUE" ~ "reciprocal help",
    name == "scalekinship.max"  ~ "kinship",
    name == "reciprocal.helpTRUE:scalekinship.max" ~ "reciprocal help x kinship interaction"
  )) %>%
  rename(Model = model, Sample = sample, Coefficient= name) %>%
  relocate(Model, Sample)
write.csv(results, file= "model_terms.csv")

# save posteriors for plotting
save(all_post.kinship, all_post.reciprocity, all_post.reciprocity_kinship, all_post.interact,
     file= "data_to_plot_model_estimates.Rdata")

# OVERALL HELP RECEIVED (GENERALIZED RECIPROCITY) ####

# Fit models comparing direct and "generalized" reciprocity

# get mean receiving rate for each individual
t <-
  read.csv("dyads.csv") %>%
  group_by(receiver) %>%
  summarize(mean.received= mean(help.rate, na.rm=T),
            total.received= sum(help.rate, na.rm=T)) %>%
  rename(bird= receiver)

# get helping observations
d3 <-
  read.csv("daily_helping.csv") %>%
  as_tibble() %>%
  # label helper-nest dyads
  mutate(helper_nest= paste(helper,nest)) %>%
  filter(help>=0) %>%
  mutate(reciprocal.help= reciprocal.help.max>0) %>%
  mutate(mean.received = t$mean.received[match(.$helper, t$bird)]) %>%
  mutate(total.received = t$total.received[match(.$helper, t$bird)])


# fit mean overall help model -----------
fit1 <-
  brm(help ~
        reciprocal.help+
        scale(mean.received) +
        offset(log(sample.duration)) +
        (1|helper) +
        (1|nest)+
        (1|helper_nest),
      data = d3,
      family = "negbinomial",
      cores = nchains,
      chains = nchains,
      iter = chain_length,
      warmup = warmup_length)

t1 <-
  summary(fit1)$fixed %>%
  as_tibble(rownames= "term") %>%
  mutate(type = 'mean.received')


# fit total overall help model ------------
fit2 <-
  brm(help ~
        reciprocal.help+
        scale(total.received) +
        offset(log(sample.duration)) +
        (1|helper) +
        (1|nest)+
        (1|helper_nest),
      data = d3,
      family = "negbinomial",
      cores = nchains,
      chains = nchains,
      iter = chain_length,
      warmup = warmup_length)

t2 <-
  summary(fit2)$fixed %>%
  as_tibble(rownames= "term") %>%
  mutate(type = 'total.received')

# compile results--------
(gr.results <-
   rbind(t1,t2))

colnames(gr.results) <- c("term", "estimate", 'error', 'low95', 'high95', 'Rhat', 'bulk_ESS', "tail_ESS", 'type')

#### get samples from posterior distribution-------
p1 <-
  fit1 %>%
  spread_draws(b_reciprocal.helpTRUE, b_scalemean.received) %>%
  pivot_longer(b_reciprocal.helpTRUE:b_scalemean.received, names_to = 'term', values_to= 'coeff') %>%
  mutate(model= "mean.received")

p2 <-
  fit2 %>%
  spread_draws(b_reciprocal.helpTRUE, b_scaletotal.received) %>%
  pivot_longer(b_reciprocal.helpTRUE:b_scaletotal.received, names_to = 'term', values_to= 'coeff') %>%
  mutate(model= "total.received")

# compile posterior distributions
post.gr <- rbind(p1,p2)

# save posteriors for plotting
save(post.gr, file= "data_to_plot_generalized_reciprocity.Rdata")

# save results----------------
write.csv(gr.results, file= "gr.model_terms.csv")
gr.results <- read.csv("gr.model_terms.csv")

# get runtime
end <- Sys.time()
runtime <- end - start
runtime

# save workspace
if(TRUE){
  timestamp <- substr(gsub(x=gsub(":","",Sys.time()),
                           pattern=" ", replace="_"), start=1, stop=15)
  timestamp
  save.image(file= paste("model_workspace_", timestamp, ".Rdata", sep=""))
}


