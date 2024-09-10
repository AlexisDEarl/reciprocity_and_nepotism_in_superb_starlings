# Reciprocal helping and nepotism in superb starlings
# Alexis Earl, ade2102@columbia.edu
# Gerry Carter, gcarter1640@gmail.com

# Do helpers help kin nests more than they help nonkin nests when they have the option to help kin and nonkin simultaneously?

# set working directory
setwd("/Users/alexisearl/Library/CloudStorage/GoogleDrive-ade2102@columbia.edu/My Drive/PhD/Chapter 1/manuscript/Reciprocal_helping_and_kin_bias_in_superb_starlings/output")

# clear workspace
rm(list=ls())

# load packages
library(tidyverse)
library(boot)
library(patchwork)

# get helping rates
raw <- read.csv(file="daily_helping.csv")

# set colors and shapes
colors <- c("red", "darkblue")
shapes <- c("square", "triangle")

# get mean and 95% CI of values x via bootstrapping
boot_ci <- function(x, perms=5000, bca=F) {
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


# get mean and 95% CI via bootstrapping of values y within grouping variable x
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

# get helping dyadic helping rates for immigrants and residents
dr <-
  raw %>%
  mutate(kinship.max = ifelse(helper.dispersal=="I", microsat.kinship.max, kinship.max)) %>%
  filter(!is.na(kinship.max)) %>%
  filter(helper.dispersal!= "NI") %>%
  filter(helper.dispersal!= "EX") %>%
  mutate(help.rate= help/sample.duration)

# run sensitivity analysis to see effect of kinship threshold

# get kinship thresholds
threshold.list <- c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5)

# make list to store dfs
df.list <- list(NA)

# repeat analysis at every threshold
for (i in 1:length(threshold.list)){
  # select kinship threshold
  threshold <- threshold.list[i]

  # select data
  t <-
    dr %>%
    # choose kinship measure and threshold
    mutate(kin= ifelse(kinship.max >= threshold, "kin", "nonkin"))

  # get difference in helping rate for helpers of each type
  diff <-
    t %>%
    dplyr::mutate(helper.type= paste(helper.dispersal, helper.sex)) %>%
    group_by(date, helper.type, helper, kin) %>%
    dplyr::summarize(help= mean(help.rate, na.rm=T), .groups= 'drop') %>%
    group_by(date, helper) %>%
    dplyr::mutate(n=n()) %>%
    filter(n>1) %>%
    ungroup() %>%
    pivot_wider(names_from = kin, values_from= help) %>%
    filter(kin+nonkin>0) %>%
    dplyr::mutate(diff= kin-nonkin) %>%
    dplyr::mutate(effect= helper.type) %>%
    dplyr::mutate(type= case_when(
      effect == "N M" ~ 'resident male',
      effect == "I M" ~ 'immigrant male',
      effect == "N F" ~ 'resident female',
      effect == "I F" ~ 'immigrant female')) %>%
    group_by(type, helper) %>%
    dplyr::summarize(diff= mean(diff), .groups= 'drop') %>%
    ungroup()

  # means and CIs
  df.list[[i]] <-
    diff %>%
    separate (type, into= c("dispersal","sex"), remove=F) %>%
    boot_ci2(y=.$diff, x=.$type, bca=T) %>%
    rename(type=effect, n.helpers= n.obs) %>%
    separate (type, into= c("dispersal","sex"), remove=F) %>%
    mutate(threshold= threshold)

  print(paste(i, "of", length(threshold.list)))

}

# compile
kin_vs_nonkin <- bind_rows(df.list)

# function to plot mean difference and CIs by kinship threshold
custom_plot <- function(type1, color, shape){
  kin_vs_nonkin %>%
    filter(type== type1) %>%
    filter(n.helpers>5) %>%
    mutate(type= fct_rev(type)) %>%
    ggplot(aes(x=threshold, y=mean, group= sex, color= sex, shape= dispersal))+
    geom_hline(yintercept = 0, color= 'grey', linetype= 'dashed')+
    geom_errorbar(aes(ymin=low, ymax=high), width=.01)+
    geom_point(size=2)+
    geom_text(aes(label= n.helpers), y=-0.28, size=3, color= 'darkgrey')+
    geom_text(aes(label= type, x=0.05, y=0.3), hjust=0, color= 'black')+
    ylab("difference in helping rate (kin - nonkin)")+
    xlab("kinship threshold")+
    coord_cartesian(ylim=c(-0.3, 0.3), xlim=(c(0, 0.5)))+
    scale_color_manual(values= color)+
    scale_shape_manual(values= shape)+
    theme_classic()+
    theme(legend.position= 'none',
          axis.text=element_text(size=12),
          strip.text = element_text(size=12, hjust=0, color="black"),
          strip.background = element_blank())
}

plot.if <- custom_plot(type1= "immigrant female", color="red", shape= "square")
plot.rf <- custom_plot(type1= "resident female",color="red", shape= "triangle")
plot.im <- custom_plot(type1="immigrant male",color="darkblue", shape= "square")
plot.rm <- custom_plot(type1= "resident male",color="darkblue", shape= "triangle")

(plot1 <- (plot.rm + plot.rf)/ (plot.im + plot.if) + plot_annotation(tag_levels = "A"))

# save plot to PDF
ggsave(
  "results/kin_v_nonkin.png",
  plot = plot1,
  width = 7,
  height = 7,
  units = "in",
  dpi = 600)




## now get probability of helping nonkin in presence of kin------------

# make list to store dfs
df.list2 <- list(NA)

# repeat analysis at every threshold
for (i in 1:length(threshold.list)){
  # select kinship threshold
  threshold <- threshold.list[i]

  # select data
  t <-
    dr %>%
    # choose kinship measure and threshold
    mutate(kin= ifelse(kinship.max >= threshold, "kin", "nonkin"))

  # get difference in helping probability for helpers of each type
  probs <-
    t %>%
    dplyr::mutate(helper.type= paste(helper.dispersal, helper.sex)) %>%
    group_by(date, helper.type, helper, kin) %>%
    dplyr::summarize(help= mean(help.rate, na.rm=T), .groups= 'drop') %>%
    group_by(date, helper) %>%
    dplyr::mutate(n=n()) %>%
    filter(n>1) %>%
    ungroup() %>%
    pivot_wider(names_from = kin, values_from= help) %>%
    filter(kin+nonkin>0) %>%
    # get proportion of cases where nonkin were helped more than kin
    dplyr::mutate(prob= as.numeric(nonkin>0)) %>%
    dplyr::mutate(effect= helper.type) %>%
    dplyr::mutate(type= case_when(
      effect == "N M" ~ 'resident male',
      effect == "I M" ~ 'immigrant male',
      effect == "N F" ~ 'resident female',
      effect == "I F" ~ 'immigrant female')) %>%
    group_by(type, helper) %>%
    dplyr::summarize(prob= mean(prob), .groups= 'drop') %>%
    ungroup()

  # means and CIs
  df.list2[[i]] <-
    probs %>%
    separate (type, into= c("dispersal","sex"), remove=F) %>%
    group_by(type) %>%
    summarize(prob= mean(prob, na.rm=T), n.helpers= n()) %>%
    separate (type, into= c("dispersal","sex"), remove=F) %>%
    mutate(threshold= threshold)

  print(paste(i, "of", length(threshold.list)))

}

# compile
nonkin.help2 <- bind_rows(df.list2)

# plot  by kinship threshold
custom_plot2 <- function(type1, color){
  nonkin.help2 %>%
    filter(type== type1) %>%
    ggplot(aes(x=threshold, y=prob))+
    geom_hline(yintercept = 0, color= 'grey')+
    geom_col(size=1, aes(fill= sex))+
    geom_text(aes(label= n.helpers), y=0.1, size=3, color= 'white')+
    geom_text(aes(label= type, x=0.05, y=1), hjust=0, color= 'black')+
    ylab("proportion of cases with nonkin helping")+
    xlab("kinship threshold")+
    coord_cartesian(ylim=c(0, 1))+
    scale_fill_manual(values= color)+
    theme_classic()+
    theme(legend.position= 'none',
          axis.text=element_text(size=12),
          axis.title = element_text(size=12),
          strip.text = element_text(size=12, hjust=0, color='black'),
          strip.background = element_blank())
}

plot.if2 <- custom_plot2(type1= "immigrant female", color="red")
plot.rf2 <- custom_plot2(type1= "resident female",color="red")
plot.im2 <- custom_plot2(type1="immigrant male",color="darkblue")
plot.rm2 <- custom_plot2(type1= "resident male",color="darkblue")


(plot2 <- (plot.rm2 + plot.rf2)/ (plot.im2 + plot.if2) + plot_annotation(tag_levels = "A"))

# save plot to PDF
ggsave(
  "results/kin_v_nonkin_prob.png",
  plot = plot2,
  width = 7,
  height = 10,
  units = "in",
  dpi = 600)

# get range of probabilities
nonkin.help2 %>% arrange(prob)
nonkin.help2 %>% arrange(desc(prob))

# across all definitions of kinship, mean probabilities of helping nonkin ranges from
# 16% in immigrant males to
# 67% in immigrant females

nonkin.help2 %>% arrange(prob) %>% pull(prob) %>% mean() # 42%

################################

# get overall mean helping rate

## now get probability of helping nonkin in presence of kin-------------

# make list to store dfs
df.list3 <- list(NA)

# repeat analysis at every threshold
for (i in 1:length(threshold.list)){
  # select kinship threshold
  threshold <- threshold.list[i]

  # select data
  t <-
    dr %>%
    # choose kinship measure and threshold
    mutate(kin= ifelse(kinship.max >= threshold, "kin", "nonkin"))

  # get difference in helping probability for helpers of each type
  probs <-
    t %>%
    dplyr::mutate(helper.type= paste(helper.dispersal, helper.sex)) %>%
    group_by(date, helper.type, helper, kin) %>%
    dplyr::summarize(help= mean(help.rate, na.rm=T), .groups= 'drop') %>%
    group_by(date, helper) %>%
    dplyr::mutate(n=n()) %>%
    filter(n>1) %>%
    ungroup() %>%
    pivot_wider(names_from = kin, values_from= help) %>%
    filter(kin+nonkin>0) %>%
    # get proportion of cases where kin were helped more than nonkin
    dplyr::mutate(prob= as.numeric(nonkin>0)) %>%
    dplyr::mutate(type= "all") %>%
    group_by(type, helper) %>%
    dplyr::summarize(prob= mean(prob), .groups= 'drop') %>%
    ungroup()

  # means and CIs
  df.list3[[i]] <-
    probs %>%
    group_by(type) %>%
    summarize(prob= mean(prob, na.rm=T), n.helpers= n()) %>%
    mutate(threshold= threshold)

  print(paste(i, "of", length(threshold.list)))

}

# compile
nonkin.help3 <- bind_rows(df.list3)

# get range of probabilities
nonkin.help3 %>% arrange(threshold) %>% pull(prob) %>% mean() #39%
nonkin.help3 %>% arrange(desc(prob))

