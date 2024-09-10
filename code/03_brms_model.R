# Test of reciprocal helping bias and nepotism in helping decisions of superb starlings
# Alexis Earl, ade2102@columbia.edu
# Gerry Carter, gcarter1640@gmail.com

# fit Bayesian negative binomial models for predicting helping rates

# clear workspace
rm(list=ls())

# load packages
library(tidyverse)
library(glmmTMB)
library(broom.mixed)
library(performance)
library(patchwork)
library(rstan)
library(brms)

# load Bayesian packages
#install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
options(mc.cores = parallel::detectCores())

# set timer
start <- Sys.time()

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
  filter(help>=0) %>%
  mutate(reciprocal.help= reciprocal.help.max>0)

# function to get relative amount of variance explained by random intercepts
get_vars <- function(fit){
  tidy(fit,conf.int=F,exponentiate=F) %>%
    filter(effect== "ran_pars") %>%
    mutate(variance= round(estimate^2,digits=3)) %>%
    dplyr::select(effect, group, variance) %>%
    group_by(effect) %>%
    mutate(percentage= round(100*(variance/sum(variance)),digits=3)) %>%
    arrange(desc(percentage)) %>%
    ungroup()
    }
# we will fit all random effects, and always include helper_nest, but we remove helper, nest, and group when they explained <0.001% of the variance

# set colors and shapes
colors <- c("black", "red", "darkblue")
shapes <- c("circle", "square", "triangle")

### NEPOTISM ####

### model nepotism by type----

# what is mean max kinship
mean(d$kinship.max, na.rm=T)

# what is 1 std dev in kinship
sd(d$kinship.max, na.rm=T)
# 0.2


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

d2 <-
  d %>%
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

# male resident
mr <-
  d2 %>%
  filter(helper.sex== "M",
         helper.dispersal == "N")

fit.mr <- fit_model(data= mr)

ci.mr <-
  fixef(fit.mr) %>%
  as_tibble(rownames= "name") %>%
  mutate(type= "resident male")

# female resident
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

#### plot-----
(plot1 <-
    results1 %>%
    filter(name != "Intercept") %>%
    separate(type, into=c("dis", "sex"), convert=T, remove=F) %>%
    mutate(sex= ifelse(is.na(sex), "all", sex)) %>%
    ggplot(aes(x=Estimate, y=type, color= sex, shape= dis))+
    geom_point(size=3)+
    geom_errorbarh(aes(xmin=Q2.5, xmax= Q97.5, height=0.2),
                   position = position_dodge(width = 0.5),
                   linewidth=1)+
    geom_vline(xintercept = 0, linetype= "dashed")+
    ylab("")+
    xlab("coefficient for kinship")+
    coord_cartesian(xlim= c(-2,2))+
    scale_color_manual(values= colors)+
    scale_shape_manual(values= shapes)+
    theme_classic()+
    theme(legend.position= 'none',
          axis.text=element_text(size=12),
          strip.text = element_text(size=12, hjust=0),
          strip.background = element_blank()))
plot1

ggsave(
  "results/nepotism by type.pdf",
  plot = plot1,
  scale = 1,
  width = 6,
  height = 2.5,
  units = c("in", "cm", "mm", "px"),
  dpi = 600)

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

sum1 <-
  rbind(s.all, s.fi, s.mi, s.mr, s.fr)

# save models
nepotism.models <- list(fit.all, fit.mi, fit.fi, fit.mr, fit.fr)

# erase model fits
rm(s.all, s.fi, s.mi, s.mr, s.fr)
rm(ci.all, ci.mi, ci.fi, ci.mr, ci.fr, fit.all, fit.mi, fit.fi, fit.mr, fit.fr)

### RECIPROCITY ##############################################################################
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

d2 <-
  d %>%
  filter(!is.na(reciprocal.help))

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

# male resident
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

# get number of birds
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

#### plot --------------
(plot2 <-
    results2 %>%
    filter(name != "Intercept") %>%
    separate(type, into=c("dis", "sex"), convert=T, remove=F) %>%
    mutate(sex= ifelse(is.na(sex), "all", sex)) %>%
    ggplot(aes(x=Estimate, y=type, color= sex, shape= dis))+
    geom_point(size=3)+
    geom_errorbarh(aes(xmin=Q2.5, xmax= Q97.5, height=0.2),
                   linewidth=1)+
    geom_vline(xintercept = 0, linetype= "dashed")+
    ylab("")+
    xlab("coefficient for reciprocal help")+
    coord_cartesian(xlim= c(-3,3))+
    scale_color_manual(values= colors)+
    scale_shape_manual(values= shapes)+
    theme_classic()+
    theme(legend.position= 'none',
          axis.text=element_text(size=12),
          strip.text = element_text(size=12, hjust=0),
          strip.background = element_blank()))
plot2


ggsave(
  "results/reciprocity by type.pdf",
  plot = plot2,
  scale = 1,
  width = 6,
  height = 2.5,
  units = c("in", "cm", "mm", "px"),
  dpi = 600)

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

sum2 <-
  rbind(s.all, s.fi, s.mi, s.mr)

# save models
reciprocity.models <- list(fit.all, fit.mi, fit.fi, fit.mr)

# erase model fits
rm(s.all, s.fi, s.mi, s.mr)
rm(ci.all, ci.mi, ci.fi, ci.mr, ci.fr, fit.all, fit.mi, fit.fi, fit.mr)

### RECIPROCITY & NEPOTISM ##############################################################################

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

# male resident
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

#### plot-----------------

(plot3 <-
   results3 %>%
   filter(name == "reciprocal.helpTRUE") %>%
   separate(type, into=c("dis", "sex"), convert=T, remove=F) %>%
   mutate(sex= ifelse(is.na(sex), "all", sex)) %>%
   ggplot(aes(x=Estimate, y=type, color= sex, shape= dis))+
   geom_point(size=3)+
   geom_errorbarh(aes(xmin=Q2.5, xmax= Q97.5, height=0.2),
                  linewidth=1)+
   geom_vline(xintercept = 0, linetype= "dashed")+
   ylab("")+
   xlab("coefficient for reciprocal help controlling for kinship")+
   coord_cartesian(xlim= c(-3,3))+
   scale_color_manual(values= colors)+
   scale_shape_manual(values= shapes)+
   theme_classic()+
   theme(legend.position= 'none',
         axis.text=element_text(size=12),
         strip.text = element_text(size=12, hjust=0),
         strip.background = element_blank()))
plot3

# include kinship coefficients?
include_kinship <- T

# to include kinship
if(include_kinship){
  colors2 <- c("darkgrey", colors)
  (plot3 <-
     results3 %>%
     filter(name != "Intercept") %>%
     separate(type, into=c("dis", "sex"), convert=T, remove=F) %>%
     mutate(sex= ifelse(is.na(sex), "all", sex)) %>%
     mutate(predictor= ifelse(name=="reciprocal.helpTRUE", "rec", "kin")) %>%
     mutate(temp= ifelse(predictor== "rec", sex, "aa")) %>%
     ggplot(aes(x=Estimate, y=type, color= temp, shape= dis, group= predictor))+
     geom_point(size=3, position = position_dodge(width = 0.5))+
     geom_errorbarh(aes(xmin=Q2.5, xmax= Q97.5, height=0.35),
                    position = position_dodge(width = 0.5),
                    linewidth=1)+
     geom_vline(xintercept = 0, linetype= "dashed")+
     ylab("")+
     xlab("coefficient for reciprocal help controlling for kinship")+
     coord_cartesian(xlim=c(-3,3))+
     scale_color_manual(values= colors2)+
     scale_shape_manual(values= shapes)+
     theme_classic()+
     theme(legend.position= 'none',
           axis.text=element_text(size=12),
           strip.text = element_text(size=12, hjust=0),
           strip.background = element_blank()))
}


(plot23 <- plot2/plot3 + plot_annotation(tag_levels = 'A') + plot_layout(heights = c(1, 1)))

ggsave(
  "results/reciprocity by type controlling for kinship (SI).pdf",
  plot = plot23,
  scale = 1,
  width = 6,
  height = 5,
  units = c("in", "cm", "mm", "px"),
  dpi = 600)


ggsave(
  "results/reciprocity by type controlling for kinship (SI).pdf",
  plot = plot3,
  scale = 1,
  width = 6,
  height = 2.5,
  units = c("in", "cm", "mm", "px"),
  dpi = 600)


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

# erase model fits
rm(s.all, s.fi, s.mi, s.mr)
rm(ci.all, ci.mi, ci.fi, ci.mr, ci.fr, fit.all, fit.mi, fit.fi, fit.mr)

### INTERACTION ##############################################################################

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

# male resident
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

# get number of birds
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


#### plot--------------------
(plot4 <-
    results4 %>%
    filter(name != "Intercept") %>%
    separate(type, into=c("dis", "sex"), convert=T, remove=F) %>%
    mutate(sex= ifelse(is.na(sex), "all", sex)) %>%
    mutate(predictor= ifelse(name=="reciprocal.helpTRUE:scalekinship.max",
                             "kinship x reciprocal help interaction",
                             "other")) %>%
    filter(predictor== "kinship x reciprocal help interaction") %>%
    ggplot(aes(x=Estimate, y=type, color= sex, shape= dis))+
    geom_point(size=3)+
   geom_errorbarh(aes(xmin=Q2.5, xmax= Q97.5, height=0.2),
                  linewidth=1)+
    geom_vline(xintercept = 0, linetype= "dashed")+
    ylab("")+
    xlab("coefficient for kinship x reciprocal help interaction")+
    coord_cartesian(xlim=c(-3,3))+
    scale_color_manual(values= colors)+
    scale_shape_manual(values= shapes)+
    theme_classic()+
    theme(legend.position= 'none',
          axis.text=element_text(size=12),
          strip.text = element_text(size=12, hjust=0),
          strip.background = element_blank()))
plot4

ggsave(
  "results/interaction by type.pdf",
  plot = plot4,
  scale = 1,
  width = 5,
  height = 3,
  units = c("in", "cm", "mm", "px"),
  dpi = 600)

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

# erase model fits
rm(s.all, s.fi, s.mi, s.mr)
rm(ci.all, ci.mi, ci.fi, ci.mr, ci.fr, fit.all, fit.mi, fit.fi, fit.mr)

# SAVE RESULTS--------

# get sample sizes
ss <-
  rbind(results1, results2, results3,results4) %>%
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

write.csv(results, file= "results/model_terms.csv")


# get runtime
end <- Sys.time()
runtime <- end - start
runtime

# save workspace
timestamp <- substr(gsub(x=gsub(":","",Sys.time()),
                         pattern=" ", replace="_"), start=1, stop=15)
timestamp
save.image(file= paste("results/model_workspace_", timestamp, ".Rdata", sep=""))


#load('results/model_workspace_2024-03-20_1512.Rdata')
#
