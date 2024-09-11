# Test of reciprocal helping bias and nepotism in helping decisions of superb starlings
# Alexis Earl, ade2102@columbia.edu
# Gerry Carter, gcarter1640@gmail.com

# test of generalized reciprocity

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

# pick how fast to run models
nchains = 4
chain_length = 5000
warmup_length = 1000

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


# fit model 1-----------
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


# fit model 2------------
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

# plot-------------
(gr.plot1 <-
 gr.results %>%
   filter(type=="total.received") %>%
   filter(term != "Intercept") %>%
    mutate(type= case_when(
      type == "mean.received" ~ "direct reciprocity vs generalized reciprocity (per group member)",
      type == "total.received" ~ "direct reciprocity vs generalized reciprocity (across group members)")) %>%
    mutate(term= case_when(
      term == "scalemean.received" ~ "mean help received \nper group member",
      term == "scaletotal.received" ~ "overall help received \nacross group members",
      term == "reciprocal.helpTRUE" ~ "reciprocal help")) %>%
    ggplot(aes(x=estimate, y=term))+
   facet_wrap(~type, scales= "free_y", nrow=2)+
    geom_point(size=2)+
    geom_errorbarh(aes(xmin= low95, xmax=high95, height=0.2), size=1)+
    geom_vline(xintercept = 0, linetype= "dashed")+
    ylab("")+
    xlab("coefficient")+
    coord_cartesian(xlim=c(-2,2))+
    theme_classic()+
    theme(legend.position= 'none',
          axis.text=element_text(size=11),
          strip.text = element_text(size=11),
          strip.background = element_blank()))

(gr.plot2 <-
    gr.results %>%
    filter(type=="mean.received") %>%
    filter(term != "Intercept") %>%
    mutate(type= case_when(
      type == "mean.received" ~ "direct reciprocity vs generalized reciprocity (per group member)",
      type == "total.received" ~ "direct reciprocity vs generalized reciprocity (across group members)")) %>%
    mutate(term= case_when(
      term == "scalemean.received" ~ "mean help received \nper group member",
      term == "scaletotal.received" ~ "overall help received \nacross group members",
      term == "reciprocal.helpTRUE" ~ "reciprocal help")) %>%
    ggplot(aes(x=estimate, y=term))+
    facet_wrap(~type, scales= "free_y", nrow=2)+
    geom_point(size=2)+
    geom_errorbarh(aes(xmin= low95, xmax=high95, height=0.2), size=1)+
    geom_vline(xintercept = 0, linetype= "dashed")+
    ylab("")+
    xlab("coefficient")+
    coord_cartesian(xlim=c(-2,2))+
    theme_classic()+
    theme(legend.position= 'none',
          axis.text=element_text(size=11),
          strip.text = element_text(size=11),
          strip.background = element_blank()))

(gr.plot <- gr.plot1/gr.plot2 + plot_annotation(tag_levels=c("A")))

ggsave(
  "results/generalized reciprocity.pdf",
  plot = gr.plot,
  scale = 1,
  width = 7,
  height = 4,
  units = c("in", "cm", "mm", "px"),
  dpi = 600)

# save results----------------
write.csv(gr.results, file= "results/gr.model_terms.csv")


gr.results <- read.csv("results/gr.model_terms.csv")
