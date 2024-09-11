# Test of reciprocal helping bias and kin bias in helping decisions of superb starlings
# Alexis Earl, ade2102@columbia.edu
# Gerry Carter, gcarter1640@gmail.com

# look at histogram for relatedness of helping events

# clear workspace
rm(list=ls())

# check directory
getwd()

# load packages
library(tidyverse)

# data
dr <-read.csv("daily_helping.csv")
dyads<-read.csv("dyads.csv")

# all individuals
dr$parents<-paste(dr$father,dr$mother,sep=" ")
dr$helper.parents<-paste(dr$helper,dr$parents,sep="_")
length(unique(dr$helper.parents))
dr$helper.type<-paste(dr$helper.dispersal,dr$helper.sex,sep="_")

kinship.means <- dr %>%
  summarize(kinship.mean = mean(microsat.kinship.max,na.rm=TRUE),n=n())

all_plot<-ggplot(dr)+
  aes(x=microsat.kinship.max)+
  geom_histogram(binwidth = 0.08,col=I("grey"), aes(fill = microsat.kinship.max > .2)) +
  scale_fill_manual(values = c("black", "black"))+
  #xlab("\n Maximum helper-breeder relatedness")+
  #ylab("Number of helpers \n ")+
  xlab("")+
  ylab("")+
  theme(panel.background = element_blank(), axis.line = element_line(colour = 'black'),
        axis.text.x = element_text( color="black", size=16, angle=0),
        axis.title = element_text(size=18,color="black"),
        axis.text.y = element_text(color="black", size=17, angle=0))+theme(legend.position = "none")+
  scale_x_continuous(breaks=seq(-0.25,1.0,0.25))+
  scale_y_continuous(limits=c(0,4000))+
  geom_vline(aes(xintercept = mean(microsat.kinship.max,na.rm=TRUE)),color = "black",size=1.5,linetype="dashed") # or median(dr$microsat.kinship.max,na.rm=TRUE)
all_plot

helper_parents<-c(dr$helper,dr$mother,dr$father)
length(unique(helper_parents)) # individuals

length(unique(dr$microsat.kinship.max))
length(unique(dr$helper)) # total (unique) number of helpers included
length(unique(dr$father)) # total (unique) number of fathers included
length(unique(dr$mother)) # total (unique) number of mothers included
#length(unique(dr$parents)) # total (unique) parents (mother-father combos) included

range(dr$microsat.kinship.max,na.rm=TRUE) # range of r-values between breeders and helpers
mean(dr$microsat.kinship.max,na.rm=TRUE) # mean of r-values between breeders and helpers
sd(dr$microsat.kinship.max,na.rm=TRUE) # sd of r-values between breeders and helpers

### by helper type
dr$type<-paste(dr$helper.dispersal,dr$helper.sex,sep="_")

helper_types <- c(
  "I_F" = "Immigrant female helpers",
  "I_M" = "Immigrant male helpers",
  "N_F" = "Natal female helpers",
  "N_M" = "Natal male helpers"
)

sex<-c(
  "F" = "Female",
  "M" = "Male"
)

dispersal<-c(
  "I" = "Immigrant",
  "N" = "Natal"
)

dr$type<-factor(dr$type, levels=c("N_M","I_M","N_F","I_F"))
dr$helper.sex<-factor(dr$helper.sex, levels=c("M","F"))
dr$helper.dispersal<-factor(dr$helper.dispersal, levels=c("N","I"))

dr1<-dr %>% filter(type=="I_F"|type=="I_M"|type=="N_F"|type=="N_M")

kinship.means <- dr1 %>%
  group_by(helper.sex,helper.dispersal) %>%
  summarize(kinship.mean = mean(microsat.kinship.max,na.rm=TRUE),n=n())

ABCD <- c(
  "N_M" = "\nA\n ",
  "I_M" = "\nB\n",
  "N_F" = "\nC\n",
  "I_F" = "\nD\n"
)

dr1_N_F<-dr1%>%filter(type=="N_F")

kinship.means_N_F <- dr1_N_F %>%
  summarize(kinship.mean = mean(microsat.kinship.max,na.rm=TRUE),n=n())

N_F_plot<-ggplot(dr1_N_F)+
  aes(x=microsat.kinship.max)+
  geom_histogram(binwidth = 0.08,col=I("black"),fill=c("red")) +
  xlab(" ")+
  ylab(" ")+
  #theme_bw()+
  theme(panel.background = element_blank(), axis.line.x = element_line(colour = 'black', linetype='solid'),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line.y = element_line(colour = 'black', linetype='solid'),
        axis.text.x = element_text(color="black", size=16, angle=0),
        axis.title = element_text(size=18,color="black"),
        axis.text.y = element_text(color="black", size=17, angle=0))+
  theme(legend.position = "none")+
  scale_x_continuous(breaks=seq(-0.25,1.0,0.25))+
  scale_y_continuous(limits=c(0,1300))+
  geom_vline(data=kinship.means_N_F, aes(xintercept = kinship.mean),color = "black",size=1.5,linetype = "dashed") # or median(dr$microsat.kinship.max,na.rm=TRUE)
  #annotate(geom = 'text', label = 'C', x = -Inf, y = Inf, hjust = -0.5, vjust = 1.2,size=6)
N_F_plot


dr1_I_F<-dr1%>%filter(type=="I_F")

kinship.means_I_F <- dr1_I_F %>%
  summarize(kinship.mean = mean(microsat.kinship.max,na.rm=TRUE),n=n())

I_F_plot<-ggplot(dr1_I_F)+
  aes(x=microsat.kinship.max)+
  geom_histogram(binwidth = 0.08,col=I("black"),fill=c("red")) +
  xlab(" ")+
  ylab(" ")+
  # theme_bw()+
  theme(panel.background = element_blank(), axis.line.x = element_line(colour = 'black', linetype='solid'),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line.y = element_line(colour = 'black', linetype='solid'),
        axis.text.x = element_text(color="black", size=16, angle=0),
        axis.title = element_text(size=18,color="black"),
        axis.text.y = element_text(color="black", size=17, angle=0))+
  theme(legend.position = "none")+
  scale_x_continuous(breaks=seq(-0.25,1.0,0.25))+
  scale_y_continuous(limits=c(0,1300))+
  geom_vline(data=kinship.means_I_F, aes(xintercept = kinship.mean),color = "black",size=1.5,linetype = "dashed") # or median(dr$microsat.kinship.max,na.rm=TRUE)
  #annotate(geom = 'text', label = 'D', x = -Inf, y = Inf, hjust = -0.5, vjust = 1.2,size=6)
I_F_plot

dr1_I_M<-dr1%>%filter(type=="I_M")

kinship.means_I_M <- dr1_I_M %>%
  summarize(kinship.mean = mean(microsat.kinship.max,na.rm=TRUE),n=n())

I_M_plot<-ggplot(dr1_I_M)+
  aes(x=microsat.kinship.max)+
  geom_histogram(binwidth = 0.08,col=I("black"),fill=c("blue")) +
  xlab(" ")+
  ylab(" ")+
  # theme_bw()+
  theme(panel.background = element_blank(), axis.line.x = element_line(colour = 'black', linetype='solid'),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line.y = element_line(colour = 'black', linetype='solid'),
        axis.text.x = element_text(color="black", size=16, angle=0),
        axis.title = element_text(size=18,color="black"),
        axis.text.y = element_text(color="black", size=17, angle=0))+
  theme(legend.position = "none")+
  scale_x_continuous(breaks=seq(-0.25,1.0,0.25))+
  scale_y_continuous(limits=c(0,1300))+
  geom_vline(data=kinship.means_I_M, aes(xintercept = kinship.mean),color = "black",size=1.5,linetype = "dashed") # or median(dr$microsat.kinship.max,na.rm=TRUE)
  #annotate(geom = 'text', label = 'B', x = -Inf, y = Inf, hjust = -0.5, vjust = 1.2,size=6)
I_M_plot


dr1_N_M<-dr1%>%filter(type=="N_M")

kinship.means_N_M <- dr1_N_M %>%
  summarize(kinship.mean = mean(microsat.kinship.max,na.rm=TRUE),n=n())

N_M_plot<-ggplot(dr1_N_M)+
  aes(x=microsat.kinship.max)+
  geom_histogram(binwidth = 0.08,col=I("black"),fill=c("blue")) +
  xlab(" ")+
  ylab(" ")+
  # theme_bw()+
  theme(panel.background = element_blank(), axis.line.x = element_line(colour = 'black', linetype='solid'),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line.y = element_line(colour = 'black', linetype='solid'),
        axis.text.x = element_text(color="black", size=16, angle=0),
        axis.title = element_text(size=18,color="black"),
        axis.text.y = element_text(color="black", size=17, angle=0))+
  theme(legend.position = "none")+
  scale_x_continuous(breaks=seq(-0.25,1.0,0.25))+
  scale_y_continuous(limits=c(0,1300))+
  geom_vline(data=kinship.means_N_M, aes(xintercept = kinship.mean),color = "black",size=1.5,linetype = "dashed") # or median(dr$microsat.kinship.max,na.rm=TRUE)
  #annotate(geom = 'text', label = 'A', x = -Inf, y = Inf, hjust = -0.5, vjust = 1.2,size=6)
N_M_plot


#n = unique number of dyads (helper - parents) for each type
length(unique(dr$helper.parents))      # 5889 total helper-parent dyads
length(unique(dr1_N_M$helper.parents)) # 1572 resident male helper dyads
length(unique(dr1_I_M$helper.parents)) # 819 immigrant male helper dyads
length(unique(dr1_I_F$helper.parents)) # 1435 immigrant female helper dyads
length(unique(dr1_N_F$helper.parents)) # 1022 resident female helper dyads


# align all x-axes by expanding xlim
library(cowplot)
library(gridExtra)
library(grid)

N_M_plot1<-N_M_plot+coord_cartesian(xlim = c(-0.25,1.0))
I_M_plot1<-I_M_plot+coord_cartesian(xlim = c(-0.25,1.0))
N_F_plot1<-N_F_plot+coord_cartesian(xlim = c(-0.25,1.0))
I_F_plot1<-I_F_plot+coord_cartesian(xlim = c(-0.25,1.0))

# combine plots
helper_type_plot_list<-list(N_M_plot1,I_M_plot1,N_F_plot1,I_F_plot1)
# helper_type_plots<-plot_grid(plotlist=helper_type_plot_list, nrow = 2,ncol=2,labels=ABCD)
helper_type_plots<-plot_grid(plotlist=helper_type_plot_list, nrow = 2,ncol=2,labels=c("B","C","D", "E"), label_fontfamily = "serif", label_fontface = "bold",label_size =24)

# create common x and y labels
y.grob <- textGrob("\n number of helpers \n",
                   gp=gpar(col="black", fontsize=26), rot=90)

x.grob <- textGrob("maximum breeder-helper relatedness \n ",
                   gp=gpar(col="black", fontsize=26))

# add to plot
grid.arrange(arrangeGrob(helper_type_plots, left = y.grob, bottom = x.grob))
# save (export as image) with height 900 width 900 (maintain aspect ratio)

# combine plot for all individuals with plots by helper type
# expand xlim so same as othr plots
all_plot1<-all_plot+coord_cartesian(xlim = c(-0.25,1.0))
# add label
all_plot2<-plot_grid(plotlist=list(all_plot1), nrow = 1,ncol=1,labels=c("A"),label_size =24, label_fontfamily = "serif", label_x = 0,
                     hjust = 0.5)
# combine
everything_plot<-grid.arrange(arrangeGrob(all_plot2,helper_type_plots,nrow=1,ncol=2, left = y.grob, bottom = x.grob))
# save (export as image) with height 1000 width 2000 (do *not* maintain aspect ratio)

# save plot
ggsave(
  "results/breeder_helper_kinship_distribution.png",
  plot = everything_plot,
  width = 20,
  height = 10,
  units = "in",
  dpi = 600)
