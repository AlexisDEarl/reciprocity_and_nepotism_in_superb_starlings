# Test of reciprocal helping bias and nepotism in helping decisions of superb starlings
# Alexis Earl, ade2102@columbia.edu
# Gerry Carter, gcarter1640@gmail.com

# double permutation test

# set working directory
setwd("/Users/alexisearl/Library/CloudStorage/GoogleDrive-ade2102@columbia.edu/My Drive/PhD/Chapter 1/manuscript/Reciprocal_helping_and_kin_bias_in_superb_starlings/output")

# clear workspace
rm(list=ls())

# get clock time (takes about 15 min on 2021 Macbook Pro with M1 chip)
start <- Sys.time()

# load packages
library(tidyverse)
library(igraph)
library(vegan)
library(asnipe)
library(boot)
library(patchwork)
library(ggrepel)


# function to create network with missing values
a_b_edgelist_to_matrix <- function(el=el, symbol="_", directed= T, make.NA.zero=T){
  a <- str_split(as.data.frame(el)[,1],symbol, simplify = TRUE)[,1]
  r <- str_split(as.data.frame(el)[,1],symbol, simplify = TRUE)[,2]
  y <- as.data.frame(el)[,2]
  e <- data.frame(a,r,y, stringsAsFactors = F)
  require(igraph)
  if (make.NA.zero){
    g <- graph_from_data_frame(e, directed=directed)
    m <- get.adjacency(g, attr='y', sparse=FALSE)
    m
  }else{
    e$y <- e$y+1 # temporarily add one to distinguish between 0 and NA
    g <- graph_from_data_frame(e, directed=directed)
    m <- get.adjacency(g, attr='y', sparse=FALSE)
    m[m==0] <- NA # relabel missing values as NA
    m <- m-1 # subtract one to adjust values back
    m
  }
}

# function to get 95% frequentist confidence interval of mean of vector x using classical bootstrapping
# argument 'bca = T' gives you bias-corrected and accelerated bootstrapping
boot_ci <- function(x, perms=5000, bca=F) {
  library(boot)
  get_mean <- function(x, d) {
    return(mean(x[d]))
  }
  x <- as.vector(na.omit(x))
  mean <- mean(x)
  if(bca){
    boot <- boot.ci(boot(data=x,
                         statistic=get_mean,
                         R=perms,
                         parallel = "multicore",
                         ncpus = 4),
                    type="bca")
    low <- boot$bca[1,4]
    high <- boot$bca[1,5]
  }else{
    boot <- boot.ci(boot(data=x,
                         statistic=get_mean,
                         R=perms,
                         parallel = "multicore",
                         ncpus = 4),
                    type="perc")
    low <- boot$perc[1,4]
    high <- boot$perc[1,5]
  }
  c(low=low,mean=mean,high=high, N=round(length(x)))
}

# get mean and 95% CIs via bootstrapping of values y within grouping variable x
# argument 'bca = T' gives you bias-corrected and accelerated bootstrapping
# get mean and 95% CIs via bootstrapping of values y within grouping variable x
# argument 'bca = T' gives you bias-corrected and accelerated bootstrapping
boot_ci2 <- function(d=d, y=d$y, x=d$x, perms=5000, bca=F){
  df <- data.frame(effect=unique(x))
  df$low <- NA
  df$mean <- NA
  df$high <- NA
  df$n.obs <- NA
  for (i in 1:nrow(df)) {
    ys <- y[which(x==df$effect[i])]
    if (length(ys)>1 & var(ys)>0 ){
      b <- boot_ci(y[which(x==df$effect[i])], perms=perms, bca=bca)
      df$low[i] <- b[1]
      df$mean[i] <- b[2]
      df$high[i] <- b[3]
      df$n.obs[i] <- b[4]
    }else{
      df$low[i] <- min(ys)
      df$mean[i] <- mean(ys)
      df$high[i] <- max(ys)
      df$n.obs[i] <- length(ys)
    }
  }
  df
}



# set number of perms to use
perms <- 5000

# get helping observations
d <- read.csv("daily_helping.csv") %>% as_tibble()

# get helpers and receivers
helpers <- d %>% pull(helper) %>% unique()
moms <-  d %>% pull(mother) %>% unique()
dads <-  d %>% pull(father) %>% unique()

# get birds that could be both helpers and receivers
birds_that_could_reciprocate <-
  data.frame(bird= c(helpers, moms, dads)) %>%
  filter(bird %in% helpers) %>%
  filter(bird %in% c(moms, dads)) %>%
  pull(bird)

# get data from birds that could reciprocate?
d2 <-
  d %>%
  filter(helper %in% birds_that_could_reciprocate) %>%
  filter(mother %in% birds_that_could_reciprocate) %>%
  filter(father %in% birds_that_could_reciprocate)

# get groups
groups <- unique(d$group)

# get results
out <- data.frame(groups,
                  n_helpers= NA,
                  n_obs = NA,
                  mean.help= NA,
                  mean.kinship=NA,
                  reciprocity=NA,
                  reciprocity.p=NA,
                  reciprocity2=NA,
                  reciprocity2.p=NA,
                  reciprocity3=NA,
                  reciprocity3.p=NA,
                  kinship=NA,
                  kinship.p=NA,
                  kinship2=NA,
                  kinship2.p=NA,
                  kinship3=NA,
                  kinship3.p=NA)

# get symmetry for each group

for (i in 1:length(groups)) {

  # get data from one group
  focal_group <- groups[i]

  # get data from that group
  t <-
    d2 %>%
    filter(group == focal_group)

  # get number of helpers in that group
  out$n_helpers[i] <-
    t %>%
    filter(help>0) %>%
    pull(helper) %>%
    n_distinct()

  # get number of obs of help in that group
  out$n_obs[i] <-
    t %>%
    filter(help>0) %>%
    nrow()

  # get helping matrix
    help.m <-
      t %>%
      select(mom.dyad, dad.dyad, help) %>%
      pivot_longer(mom.dyad:dad.dyad, values_to = 'dyad') %>%
      group_by(dyad) %>%
      summarize(rate = sum(help, na.rm=T), .groups= 'drop') %>%
      separate(dyad, into= c("helper", "receiver"), sep= "-->") %>%
      graph.data.frame(directed=T) %>%
      get.adjacency(attr='rate', sparse=FALSE)

  # get expected helping matrix for that group
    rand.nets <- array(data= NA, dim= c(dim(help.m), perms))
    for (ii in 1:perms) {
      rand.nets[,,ii] <-
        t %>%
        # shuffle helping minutes between possible helpers within nest
        group_by(date, nest)  %>%
        mutate(help = sample(help, n())) %>%
        ungroup() %>%

        # get helping matrix
        select(mom.dyad, dad.dyad, help) %>%
        pivot_longer(mom.dyad:dad.dyad, values_to = 'dyad') %>%
        group_by(dyad) %>%
        summarize(rate = sum(help, na.rm=T), .groups= 'drop') %>%
        separate(dyad, into= c("helper", "receiver"), sep= "-->") %>%
        graph.data.frame(directed=T) %>%
        get.adjacency(attr='rate', sparse=FALSE)

      print(paste(ii, "of", perms, "---", i,"of", length(groups)))
    }
    expected.help.m <- apply(rand.nets, 1:2, median)
    colnames(expected.help.m) <- colnames(help.m)

 # get adjusted helping rates (difference between observed and expected (log counts))
    help2.m <- help.m - expected.help.m
    #help2.m <- log(help.m+1) - log(expected.help.m+1)

  # get kinship matrix
  kinship.m <-
    t %>%
    mutate(kinship.mom= ifelse(helper.dispersal== "I", microsat.kinship.mom, kinship.mom)) %>%
    mutate(kinship.dad= ifelse(helper.dispersal== "I", microsat.kinship.dad, kinship.dad)) %>%
    select(mom.dyad, dad.dyad, kinship.mom, kinship.dad) %>%
    pivot_longer(mom.dyad:dad.dyad, values_to = 'dyad') %>%
    mutate(kinship = ifelse(name== "mom.dyad", kinship.mom, kinship.dad)) %>%
    group_by(dyad) %>%
    summarize(kinship= mean(kinship, na.rm=T)) %>%
    separate(dyad, into= c("helper", "receiver"), sep= "-->") %>%
    graph.data.frame(directed=T) %>%
    get.adjacency(attr='kinship', sparse=FALSE)

  # test symmetry of helping
  t <- mantel(help.m, t(help.m), na.rm=T)
  out$reciprocity[i] <- t$statistic
  out$reciprocity.p[i] <- t$signif
  rm(t)

  # test symmetry of adjusted helping
  t <- mantel(help2.m, t(help2.m), na.rm=T)
  out$reciprocity2[i] <- t$statistic
  out$reciprocity2.p[i] <- t$signif
  rm(t)

  # test kinship as predictor of helping rates
  t <- mantel(help.m, kinship.m, na.rm=T)
  out$kinship[i] <- t$statistic
  out$kinship.p[i] <- t$signif
  rm(t)

  # test kinship as predictor of adjusted helping rates
  t <- mantel(help2.m, kinship.m, na.rm=T)
  out$kinship2[i] <- t$statistic
  out$kinship2.p[i] <- t$signif
  rm(t)

  # test kinship and reciprocal help in MRQAP
  t <- mrqap.dsp(scale(help2.m) ~ scale(t(help2.m)) + scale(kinship.m),
                 test.statistic= "beta",
                 randomisations = perms)
  out$reciprocity3[i] <- t$coefficients[2]
  out$reciprocity3.p[i] <- t$P.greater[2]
  out$kinship3[i] <- t$coefficients[3]
  out$kinship3.p[i] <- t$P.greater[3]
  rm(t)

  # get average helping rate
  diag(help.m) <- NA
  out$mean.help[i] <- mean(help.m, na.rm=T)

  # get average kinship
  diag(kinship.m) <- NA
  out$mean.kinship[i] <- mean(kinship.m, na.rm=T)

}


# get results table
results <-
  out %>%
  as_tibble() %>%
  pivot_longer(reciprocity:kinship3.p) %>%
  separate(name, into=c('name', 'estimate'), sep="[.]") %>%
  mutate(estimate= ifelse(is.na(estimate), "statistic", estimate)) %>%
  pivot_wider(names_from = estimate, values_from = value) %>%
  mutate(test = case_when(
    name == "reciprocity" ~ "3. reciprocity in observed helping (Mantel test)",
    name == "reciprocity2" ~ "4. reciprocity in adjusted helping (Mantel test)",
    name == "reciprocity3" ~ "6. reciprocity in adjusted helping after controlling for kinship (MRQAP)",
    name == "kinship" ~ "1. nepotism in observed helping (Mantel test)",
    name == "kinship2" ~ "2. nepotism in adjusted helping (Mantel test)",
    name == "kinship3" ~ "5. nepotism in adjusted helping after controlling for reciprocal help (MRQAP)")) %>%
  select(test, groups, n_helpers, n_obs, mean.help, mean.kinship, test, statistic, p) %>%
  mutate(significance= case_when(
    p < 0.001 ~ '***',
    p < 0.01 ~ '**',
    p < 0.05 ~ '*',
    p >= 0.05 ~ "")) %>%
  arrange(test)
results


# save results--------------
(timestamp <- substr(gsub(x=gsub(":","",Sys.time()),
                         pattern=" ", replace="_"), start=1, stop=15))
write.csv(results, file= paste("results/double.perm.test.results", timestamp, ".csv", sep=""))

# results <- read.csv("results/double.perm.test.results2024-03-14_0940.csv")

# save workspace (optional)
if(FALSE){
  save.image(file= paste("double_perm_workspace_", timestamp, ".Rdata", sep=""))
}

# plot MRQAP results
(plot1a <-
    results %>%
    filter(test== "6. reciprocity in adjusted helping after controlling for kinship (MRQAP)") %>%
    #mutate(groups = fct_reorder(groups, desc(statistic))) %>%
    ggplot(aes(y=statistic, x=groups))+
    geom_col(size=3)+
    geom_text(aes(label=significance), size=10) +
    geom_hline(yintercept = 0)+
    ylab("beta coefficient for reciprocal helping")+
    xlab("group")+
    ggtitle("reciprocal helping controlling for kinship")+
    coord_cartesian(ylim= c(-0.4, 0.4))+
    theme_classic()+
    theme(legend.position= 'none',
          axis.text=element_text(size=12),
          strip.text = element_text(size=12, hjust=0),
          strip.background = element_blank()))

set.seed(123)
(plot1b <-
  results %>%
  filter(test== "6. reciprocity in adjusted helping after controlling for kinship (MRQAP)") %>%
  mutate(t= 'overall') %>%
  boot_ci2(x=.$t, y=.$statistic, bca=T) %>%
  ggplot(aes(x=effect, y=mean))+
  geom_point(size=3)+
  geom_hline(yintercept = 0)+
  coord_cartesian(ylim= c(-0.4, 0.4))+
  geom_errorbar(aes(ymin=low, ymax=high, width=.1), size=1)+
  ylab("")+
  xlab("")+
    theme_classic()+
    theme(legend.position= 'none',
          axis.text=element_text(size=12),
          strip.text = element_text(size=12, hjust=0),
          strip.background = element_blank()))

(plot1 <- plot1a+plot1b +  plot_layout(widths = c(7, 1)))

(plot2a <-
    results %>%
    filter(test== "4. reciprocity in adjusted helping (Mantel test)") %>%
    #mutate(groups = fct_reorder(groups, desc(statistic))) %>%
    ggplot(aes(y=statistic, x=groups))+
    geom_col(size=3)+
    geom_text(aes(label=significance), size=10) +
    geom_hline(yintercept = 0)+
    ylab("correlation between help given and received")+
    xlab("group")+
    ggtitle("reciprocal helping")+
    coord_cartesian(ylim= c(-0.3, 0.3))+
    theme_classic()+
    theme(legend.position= 'none',
          axis.text=element_text(size=12),
          strip.text = element_text(size=12, hjust=0),
          strip.background = element_blank()))

set.seed(123)
(plot2b <-
    results %>%
    filter(test== "4. reciprocity in adjusted helping (Mantel test)") %>%
    mutate(t= 'overall') %>%
    boot_ci2(x=.$t, y=.$statistic, bca=T) %>%
    ggplot(aes(x=effect, y=mean))+
    geom_point(size=3)+
    geom_hline(yintercept = 0)+
    coord_cartesian(ylim= c(-0.25, 0.25))+
    geom_errorbar(aes(ymin=low, ymax=high, width=.1), size=1)+
    ylab("")+
    xlab("")+
    theme_classic()+
    theme(legend.position= 'none',
          axis.text=element_text(size=12),
          strip.text = element_text(size=12, hjust=0),
          strip.background = element_blank()))

(plot2 <- plot2a+plot2b +  plot_layout(widths = c(7, 1)))

(plot <- plot2/plot1 +  plot_layout(heights = c(1, 2)) + plot_annotation(tag_levels= "A"))

ggsave(
  "results/permutation_tests.pdf",
  plot = plot,
  scale = 1,
  width = 8,
  height = 11,
  units = c("in", "cm", "mm", "px"),
  dpi = 600)

# alternative plot----------------

points1 <-
  results %>%
  # label p-values to show
  mutate(p2= p<0.05) %>%
  mutate(p3= ifelse(p2, paste0("p=",p), ""))  %>%
  mutate(p3= ifelse(p3== "p=0", "p<0.0002", as.character(p3)))  %>%
  filter(test== "4. reciprocity in adjusted helping (Mantel test)") %>%
  mutate(effect= "")

(means1 <-
  points1 %>%
  # label p-values to show
  boot_ci2(x=.$effect, y=.$statistic, bca=T) %>%
  ggplot(aes(x=effect, y=mean))+
  geom_hline(yintercept = 0)+
  geom_jitter(data= points1, aes(y= statistic, color= p2), size=2, height=0, width=0.01)+
  geom_text_repel(data= points1, aes(y= statistic, label= p3), size=4, nudge_x=-0.2)+
  geom_point(position = position_nudge(x = 0.1), size=3, shape= "square")+
  geom_errorbar(aes(ymin=low, ymax=high, width=.1), position = position_nudge(x = 0.1), size=1)+
    coord_cartesian(ylim= c(-0.25, 0.25))+
    scale_color_manual(values= c("darkgrey", "black"))+
     ylab("Pearson's correlation coefficient")+
     xlab("given and received\nadjusted help scores ")+
     theme_classic()+
     theme(legend.position= 'none',
           axis.text.x= element_blank(),
           axis.text=element_text(size=12),
           strip.text = element_text(size=12, hjust=0),
           strip.background = element_blank()))

points2 <-
  results %>%
  # label p-values to show
  mutate(p= round(p,digits=3)) %>%
  mutate(p2= p<0.05) %>%
  mutate(p3= ifelse(p2, paste0("p=",p), ""))  %>%
  mutate(p3= ifelse(p3== "p=0", "p<0.0002", as.character(p3)))  %>%
  filter(test== "6. reciprocity in adjusted helping after controlling for kinship (MRQAP)") %>%
  mutate(effect= "")

(means2 <-
    points2 %>%
    # label p-values to show
    boot_ci2(x=.$effect, y=.$statistic, bca=T) %>%
    ggplot(aes(x=effect, y=mean))+
    geom_hline(yintercept = 0)+
    geom_jitter(data= points2, aes(y= statistic, color= p2), size=2, height=0, width=0.01)+
    geom_text_repel(data= points2, aes(y= statistic, label= p3), size=4, nudge_x=-0.2)+
    geom_point(position = position_nudge(x = 0.1), size=3, shape= "square")+
    geom_errorbar(aes(ymin=low, ymax=high, width=.1), position = position_nudge(x = 0.1), size=1)+
    coord_cartesian(ylim= c(-0.4, 0.4))+
    scale_color_manual(values= c("darkgrey", "black"))+
    ylab("Beta coefficient")+
    xlab("effect of adjusted help received\n when controlling for kinship")+
    theme_classic()+
    theme(legend.position= 'none',
          axis.text.x= element_blank(),
          axis.text=element_text(size=12),
          strip.text = element_text(size=12, hjust=0),
          strip.background = element_blank()))

(aplot <- means1+means2 + plot_layout(guides= 'collect') + plot_annotation(tag_levels= "A"))

ggsave(
  "results/permutation_tests2.pdf",
  plot = aplot,
  width = 6,
  height = 5,
  units = c("in", "cm", "mm", "px"),
  dpi = 600)


# get runtime of script
end <- Sys.time()
runtime <- end-start
runtime
