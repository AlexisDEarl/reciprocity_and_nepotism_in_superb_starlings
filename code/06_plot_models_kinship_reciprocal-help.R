# Test of reciprocal helping bias and nepotism (kinship bias) in helping decisions of superb starlings
# Alexis Earl, ade2102@columbia.edu
# Gerry Carter, gc1511@princeton.edu

# This script plots the results of Bayesian negative binomial models for predicting helping rates

# clear workspace
rm(list=ls())

# load data from models
load("results/data_to_plot_model_estimates_published.Rdata")
load("results/data_to_plot_generalized_reciprocity.Rdata")

# set colors and shapes
colors <- c("violet", "pink", "deepskyblue")
colors2 <- c("darkviolet", "red", "darkblue")
shapes <- c("circle", "square", "triangle")

#### plot nepotism model-----
(plot1 <-
   all_post.kinship %>%
   separate(model, into=c("predictor", "dispersal", "sex")) %>%
   mutate(label= paste(dispersal, sex)) %>%
   mutate(label= ifelse(label== "both both", "all", label)) %>%
   # to put "all" at top of plot
   mutate(label = fct_relevel(label, "all", after = Inf)) %>%
   ggplot(aes(y = label, x = coeff, fill= sex, color= sex)) +
   geom_vline(xintercept = 0, linetype= "dashed")+
   stat_halfeye(aes(shape= dispersal),
                size= 3,
                linewidth= 1,
                alpha=0.8)+
   ylab("")+
   xlab("regression coefficient for kinship")+
   coord_cartesian(xlim= c(-1.5,1.5))+
   scale_fill_manual(values= colors)+
   scale_color_manual(values= colors2)+
   scale_shape_manual(values= shapes)+
   theme_classic()+
   theme(legend.position= 'none',
         axis.text.x =element_text(size=12),
         axis.text.y = element_text(size=12),
         strip.text = element_text(size=12, hjust=0),
         strip.background = element_blank()))

# save plot
ggsave(
  "results/Figure 1.pdf",
  plot = plot1,
  scale = 1,
  width = 4,
  height = 3,
  units = c("in"),
  dpi = 600)

#### plot reciprocity model --------------
(plot2 <-
   all_post.reciprocity %>%
   separate(model, into=c("predictor", "dispersal", "sex"), sep= " ") %>%
   mutate(label= paste(dispersal, sex)) %>%
   mutate(label= ifelse(label== "both both", "all", label)) %>%
   # to put "all" at top of plot
   mutate(label = fct_relevel(label, "all", after = Inf)) %>%
   ggplot(aes(y = label, x = coeff, fill= sex, color= sex)) +
   geom_vline(xintercept = 0, linetype= "dashed")+
   stat_halfeye(aes(shape= dispersal),
                size= 3,
                linewidth= 1,
                alpha=0.8)+
   ylab("")+
   xlab("regression coefficient for reciprocal help")+
   coord_cartesian(xlim= c(-3,3))+
   scale_fill_manual(values= colors)+
   scale_color_manual(values= colors2)+
   scale_shape_manual(values= shapes)+
   theme_classic()+
   theme(legend.position= 'none',
         axis.text.x = element_text(size=12),
         axis.text.y = element_text(size=12),
         strip.text = element_text(size=12, hjust=0),
         strip.background = element_blank()))

#### plot reciprocity and nepotism model-----------------
(plot3 <-
   all_post.reciprocity_kinship %>%
   filter(term == "b_reciprocal.helpTRUE") %>%
   separate(model, into=c("predictor", "dispersal", "sex"), sep= " ") %>%
   mutate(label= paste(dispersal, sex)) %>%
   mutate(label= ifelse(label== "both both", "all", label)) %>%
   # to put "all" at top of plot
   mutate(label = fct_relevel(label, "all", after = Inf)) %>%
   ggplot(aes(y = label, x = coeff, fill= sex, color= sex)) +
   geom_vline(xintercept = 0, linetype= "dashed")+
   stat_halfeye(aes(shape= dispersal),
                size= 3,
                linewidth= 1,
                alpha=0.8)+
   ylab("")+
   xlab("regression coefficient for reciprocal help\nadjusting for kinship")+
   coord_cartesian(xlim= c(-3,3))+
   scale_fill_manual(values= colors)+
   scale_color_manual(values= colors2)+
   scale_shape_manual(values= shapes)+
   theme_classic()+
   theme(legend.position= 'none',
         axis.text.x =element_text(size=12),
         axis.text.y = element_text(size=12),
         strip.text = element_text(size=12, hjust=0),
         strip.background = element_blank()))

# save plot
(plot2and3 <- plot2/plot3 + plot_annotation(tag_levels = 'A') + plot_layout(heights = c(1, 1)))
ggsave(
  "results/Figure 4.pdf",
  plot = plot2and3,
  scale = 1,
  width = 5,
  height = 6,
  units = c("in", "cm", "mm", "px"),
  dpi = 600)

# plot reciprocity and nepotism model with coefficients for kinship

colors3 <- c("lightgrey", "violet", "pink", "deepskyblue")
colors4 <- c("darkgrey","darkviolet", "red", "darkblue")

(plot3b <-
    all_post.reciprocity_kinship %>%
    separate(model, into=c("predictor", "dispersal", "sex"), sep= " ") %>%
    mutate(label= paste(dispersal, sex)) %>%
    mutate(label= ifelse(label== "both both", "all", label)) %>%
    # to put "all" at top of plot
    mutate(label = fct_relevel(label, "all", after = Inf)) %>%
    mutate(temp= ifelse(term== "b_reciprocal.helpTRUE", sex, "aa")) %>%
    ggplot(aes(y = label, x = coeff, fill= temp, color= temp)) +
    geom_vline(xintercept = 0, linetype= "dashed")+
    stat_halfeye(aes(shape= dispersal),
                 size= 3,
                 linewidth= 1,
                 position = position_dodge(width = 0.5),
                 alpha=0.8)+
    ylab("")+
    xlab("regression coefficients for reciprocal help (colors)\nand kinship (light grey)")+
    coord_cartesian(xlim= c(-3,3))+
    scale_fill_manual(values= colors3)+
    scale_color_manual(values= colors4)+
    scale_shape_manual(values= shapes)+
    theme_classic()+
    theme(legend.position= 'none',
          axis.text.x =element_text(size=12),
          axis.text.y = element_text(size=12),
          strip.text = element_text(size=12, hjust=0),
          strip.background = element_blank()))
ggsave(
  "results/Figure S6.pdf",
  plot = plot3b,
  scale = 1,
  width = 5,
  height = 4,
  units = c("in", "cm", "mm", "px"),
  dpi = 600)

#### plot interaction between reciprocity and nepotism--------------------
(plot4 <-
   all_post.interact %>%
   separate(model, into=c("predictor", "dispersal", "sex"), sep= " ") %>%
   mutate(label= paste(dispersal, sex)) %>%
   mutate(label= ifelse(label== "both both", "all", label)) %>%
   # to put "all" at top of plot
   mutate(label = fct_relevel(label, "all", after = Inf)) %>%
   ggplot(aes(y = label, x = coeff, fill= sex, color= sex)) +
   geom_vline(xintercept = 0, linetype= "dashed")+
   stat_halfeye(aes(shape= dispersal),
                size= 3,
                linewidth= 1,
                alpha=0.8)+
   ylab("")+
   xlab("regression coefficient for interaction\nbetween reciprocal help and kinship")+
   coord_cartesian(xlim= c(-3,3))+
   scale_fill_manual(values= colors)+
   scale_color_manual(values= colors2)+
   scale_shape_manual(values= shapes)+
   theme_classic()+
   theme(legend.position= 'none',
         axis.text.x =element_text(size=12),
         axis.text.y = element_text(size=12),
         strip.text = element_text(size=12, hjust=0),
         strip.background = element_blank()))

ggsave(
  "results/Figure S4.pdf",
  plot = plot4,
  scale = 1,
  width = 5,
  height = 4,
  units = c("in", "cm", "mm", "px"),
  dpi = 600)

# plot direct vs generalized reciprocity models -------------
(plot5a <-
   post.gr %>%
   filter(model == "total.received") %>%
   mutate(title =  "reciprocal help adjusting for total help received") %>%
   mutate(label= case_when(
     term == "b_scalemean.received" ~ "mean help received \nper group member",
     term == "b_scaletotal.received" ~ "overall help received \nacross group members",
     term == "b_reciprocal.helpTRUE" ~ "reciprocal help")) %>%
   ggplot(aes(y = label, x = coeff)) +
   facet_wrap(~title, scales= "free_y", nrow=2)+
   geom_vline(xintercept = 0, linetype= "dashed")+
   stat_halfeye(
                size= 3,
                linewidth= 1,
                color = "darkviolet",
                fill = "violet",
                alpha=0.8)+
   ylab("")+
   xlab("regression coefficient")+
   coord_cartesian(xlim= c(-3,3))+
   theme_classic()+
   theme(legend.position= 'none',
         axis.text.x =element_text(size=12),
         axis.text.y = element_text(size=12),
         strip.text = element_text(size=12, hjust=0),
         strip.background = element_blank()))


(plot5b <-
    post.gr %>%
    filter(model == "mean.received") %>%
    mutate(title =  "reciprocal help adjusting for mean help received") %>%
    mutate(label= case_when(
      term == "b_scalemean.received" ~ "mean help received \nper group member",
      term == "b_scaletotal.received" ~ "overall help received \nacross group members",
      term == "b_reciprocal.helpTRUE" ~ "reciprocal help")) %>%
    ggplot(aes(y = label, x = coeff)) +
    geom_vline(xintercept = 0, linetype= "dashed")+
    facet_wrap(~title, scales= "free_y", nrow=2)+
    stat_halfeye(
      size= 3,
      linewidth= 1,
      color = "darkviolet",
      fill = "violet",
      alpha=0.8)+
    ylab("")+
    xlab("regression coefficient")+
    coord_cartesian(xlim= c(-3,3))+
    theme_classic()+
    theme(legend.position= 'none',
          axis.text.x =element_text(size=12),
          axis.text.y = element_text(size=12),
          strip.text = element_text(size=12, hjust=0),
          strip.background = element_blank()))

(plot5 <- plot5a/plot5b + plot_annotation(tag_levels=c("A")))

# save as PDF
ggsave(
  "results/Figure 5.pdf",
  plot = plot5,
  scale = 1,
  width = 7.5,
  height = 4,
  units = c("in", "cm", "mm", "px"),
  dpi = 600)



