# A cryptic role for reciprocal helping in a cooperatively breeding bird
# Alexis Earl, ade2102@columbia.edu
# Gerry Carter, gc1511@princeton.edu

# This script summarizes social-role switching, examines the possibility of "redirected helping", and plots the role switching events.

# clear workspace
rm(list=ls())

# load packages
library(tidyverse)
library(patchwork)
library(see)
library(data.table)
library(plotrix)

# get data
d <- read.csv("daily_helping.csv")

# get breeders for each nest
Mom<-d %>%
  mutate(id=mother,
         role="B",
         breeding.season=breeding_season)
Mom<-distinct(Mom[!is.na(Mom$id), ])
Mom$dispersal<-"I"
Mom$sex<-"F"
Mom<-Mom %>%
  dplyr::select(id,
                role,
                sex,
                group,
                breeding.season,
                date,
                nest,
                mother,
                father)
Mom<-distinct(Mom)

Dad<-d %>%
  mutate(id=father,
         role="B",
         breeding.season=breeding_season)
Dad<-distinct(Dad[!is.na(Dad$id), ])
Dad$sex<-"M"
Dad<-Dad %>%
  dplyr::select(id,
                role,
                sex,
                group,
                breeding.season,
                date,
                nest,
                mother,
                father)
Dad<-distinct(Dad)

Breeders<-distinct(rbind(Mom,Dad))
rm(Mom)
rm(Dad)

# get all unique helpers at each nest attempt
Helpers<-distinct(d %>%
                    filter(help>0) %>%
                    dplyr::select(helper,
                                  helper.sex,
                                  group,
                                  breeding_season,
                                  date,
                                  nest,
                                  mother,
                                  father) %>%
                    rename(id=helper,
                           sex=helper.sex,
                           breeding.season=breeding_season) %>%
                    mutate(role="H"))

B_and_H<-distinct(rbind(Breeders,Helpers))

dr_roles<-B_and_H %>%
  mutate(breeding.season=as.factor(breeding.season))
levels(dr_roles$breeding.season)

dr_roles$date<-as.Date(dr_roles$date,format = "%Y-%m-%d")

# do individuals switch roles within a breeding season?
roles_per_season<-dr_roles %>%
  group_by(id,breeding.season) %>%
  mutate(roles_per_season=length(unique(role))) %>%
  ungroup()

# get number of seasons present for each individual
presence<-roles_per_season %>%
  group_by(id)%>%
  arrange(date)%>%
  mutate(breeding.season.num=as.numeric(breeding.season),
         final.season.num=max(as.numeric(breeding.season)),
         first.season.num=min(as.numeric(breeding.season))) %>%
  ungroup()

# add NB (non-breeder/non-helper) for when individuals were around but not helping or breeding
dr_roles_fill<-expand.grid(unique(dr_roles$id),unique(dr_roles$breeding.season)) %>%
  rename(id=Var1,breeding.season=Var2)
dr_roles_fill$role<-NA

dr_roles_df<-merge(presence,dr_roles_fill,by=c("id","breeding.season"),na_matches="never",all=TRUE)
dr_roles_df$role.y<-NULL
dr_roles_df<-dr_roles_df %>%
  rename(role=role.x)

df<-dr_roles_df %>%
  mutate(breeding.season.num=as.numeric(breeding.season)) %>%
  group_by(id) %>%
  mutate(first.season.num=max(first.season.num,na.rm=T),
         final.season.num=max(final.season.num,na.rm=T)) %>%
  filter(breeding.season.num>=first.season.num,breeding.season.num<=final.season.num) %>%
  mutate(seasons.since.first.obs=breeding.season.num-first.season.num)%>%
  mutate(role=ifelse(is.na(role),"NB",role))%>%
  ungroup()
df<-df %>%
  mutate(roles_per_season=ifelse(is.na(roles_per_season),1,roles_per_season))
df<-df %>%
  group_by(id) %>%
  fill(sex, .direction= "updown") %>%
  fill(group, .direction= "updown")

# add date for NB roles based on dates observed as each breeding season
seasons<-d %>%
  dplyr::select(breeding_season,date) %>%
  rename(breeding.season=breeding_season) %>%
  mutate(date=as.Date(date,format = "%Y-%m-%d")) %>%
  group_by(breeding.season) %>%
  summarise(date.first=min(date),
            date.last=max(date))

df<-left_join(df, seasons %>% dplyr::select(breeding.season,date.first), by="breeding.season")
df<-as.data.frame(df)

df$date1<-dplyr::coalesce(df$date,df$date.first)
df$date<-df$date1
df$date1<-NULL
df$date.first<-NULL

df1<-df %>%
  group_by(id, breeding.season, role, nest) %>%
  summarise(
    sex=first(sex),
    group=first(group),
    date=first(date),
    mother=first(mother),
    father=first(father),
    roles_per_season=last(roles_per_season),
    breeding.season.num=first(breeding.season.num),
    final.season.num=first(final.season.num),
    first.season.num=first(first.season.num),
    seasons.since.first.obs=first(seasons.since.first.obs)) %>%
  ungroup()
df1$breeding.season<-as.factor(df1$breeding.season)

df1<-df1 %>%
  group_by(id) %>%
  arrange(date) %>%
  mutate(role_previous=lag(role)) %>%
  mutate(role_next=lead(role)) %>%
  mutate(switch=paste(role,role_next,sep="->")) %>%
  ungroup()

# season.entries should always be equal to nests_per_season
df2 <- df1 %>%
  group_by(id,breeding.season) %>%
  mutate(season.entries=n(),
         season.rows=seq_along(breeding.season),
         nests_per_season=length(unique(nest))) %>%
  mutate(season.portions=season.rows/season.entries) %>%
  ungroup()

df3<-df2 %>%
  group_by(id) %>%
  arrange(id,breeding.season,date) %>%
  mutate(id.rows=seq_along(id),
         seasons=as.numeric(factor(breeding.season))) %>%
  mutate(time.period=ifelse(breeding.season==lag(breeding.season),lag(seasons)+lag(season.portions), lag(seasons)+1)) %>%
  mutate(time.period=ifelse(is.na(time.period),seasons,time.period))%>%
  ungroup()

rm(df2)
rm(df1)
rm(df)

# how much breeding/helping was done up until each season?
df<-df3 %>%
  group_by(id,breeding.season) %>%
  mutate(breed=ifelse(role=="B",1,0),
         help=ifelse(role=="H",1,0),
         nbnh=ifelse(role=="NB",1,0)) %>%
  ungroup() %>%
  group_by(id) %>%
  arrange(time.period) %>%
  mutate(nbnhed=cumsum(nbnh),
         helped=cumsum(help),
         bred=cumsum(breed),
         first_role=first(role)) %>%
  mutate(B.ever=as.numeric(sum(breed)>0),
         H.ever=as.numeric(sum(help)>0),
         NB.ever=as.numeric(sum(nbnh)>0)) %>%
  ungroup()

rm(df3)
rm(B_and_H)

# add dispersal status back in
# NA dispersal status means first captured or born too recently to know their dispersal status yet
ids_helpers<-distinct(d %>%
                        select(helper,helper.dispersal) %>%
                        rename("id"="helper",
                               "dispersal"="helper.dispersal"))
ids_moms<-data.frame(
  id = setdiff(d$mother,d$helper),
  dispersal = c("I","I","I")
)

ids<-rbind(ids_helpers, ids_moms)
rm(ids_helpers)
rm(ids_moms)

df1<-left_join(df,ids,by="id")
rm(df)
df<-df1 %>%
  mutate(type=ifelse(!is.na(sex),paste(dispersal,sex,sep="_"),NA))

head(as.data.frame(df),2)

# remove individuals that were only observed at one nest ever (helping or breeding), i.e., only have one time.period in the data so no opportunity to switch or maintain their social role
diffs_df <-
  aggregate(cbind(minDiff=time.period)~id, FUN=function(x) min(diff(x)),data=df)
one.obs <- merge(df,diffs_df,by='id',all.x=T)
df_sample <- one.obs %>% filter(is.finite(minDiff))
df_sample$minDiff<-NULL

length(unique(df_sample$id)) # n=393 individuals (* 161 individuals were excluded because they were only observed at one nest ever )

# calculate how many role switches for each individual across lifespan

# remove individuals that we don't have full lifespan for (i.e., still alive) - individuals presumed dead if missing 5 or more breeding seasons so anyone still in the data after 2019LR is removed for this part - also, min cut-off = if individuals aren't observed more than 1 breeding season (* min cut-off was handled by diffs_df above)
full_lifetimes_all<- df_sample %>%
  group_by(id) %>%
  arrange(id,time.period) %>%
  filter(final.season.num<36) %>% # this line removes those still alive at end of study
  mutate(seasons_total=length(unique(breeding.season))) %>%
  mutate(role_switch = rleid(role) - 1) %>%
  ungroup()
length(unique(full_lifetimes_all$id))

role.switch.count<-full_lifetimes_all %>%
  group_by(id) %>%
  summarise(switch_count=max(role_switch),
            n=n(),
            seasons=length(unique(breeding.season)),
            sex=first(sex),
            dispersal=first(dispersal),
            type=first(type))

role.switch.count.tbl<-as.data.frame(role.switch.count %>%
                                       as_tibble() %>%
                                       count(switch_count))

# how many individuals switched roles more than once in their lives?
sum(role.switch.count.tbl$n[role.switch.count.tbl$switch_count>1])

# percentage of individuals that switched roles more than once in their lives
(sum(role.switch.count.tbl$n[role.switch.count.tbl$switch_count>1])/sum(role.switch.count.tbl$n))*100

# mean role switches per individual per lifetime ± s.e
summary(role.switch.count$switch_count)
mean(role.switch.count$switch_count)
std.error(role.switch.count$switch_count)
range(role.switch.count$switch_count)

# Summarise role switching
switches_all<-full_lifetimes_all %>%
  filter(!switch =="B->NA") %>%
  filter(!switch=="H->NA") %>%
  filter(!switch=="NB->NA")

full_lifetimes_all<-full_lifetimes_all %>%
  group_by(id) %>%
  mutate(switch_binom=ifelse(role==lead(role),0,1)) %>%
  ungroup()

current_B<-full_lifetimes_all %>%
  group_by(id) %>%
  filter(role=="B") %>%
  mutate(switch_to_H_binom=ifelse(role_next=="H",1,0)) %>%
  mutate(switch_to_NB_binom=ifelse(role_next=="NB",1,0)) %>%
  mutate(switch_to_B_binom=ifelse(role_next=="B",1,0)) %>%
  ungroup()

current_B_sum<-current_B %>%
  group_by(id) %>%
  summarise(sex=first(sex),
            dispersal=first(dispersal),
            type=first(type),
            switch_to_H=sum(switch_to_H_binom, na.rm=T),
            switch_to_NB=sum(switch_to_NB_binom, na.rm=T),
            switch_to_B=sum(switch_to_B_binom, na.rm=T)) %>%
  mutate(switch_to_H_binom=as.numeric(switch_to_H>0),
         switch_to_NB_binom=as.numeric(switch_to_NB>0),
         switch_to_B_binom=as.numeric(switch_to_B>0))

current_H<-full_lifetimes_all %>%
  group_by(id) %>%
  filter(role=="H") %>%
  mutate(switch_to_H_binom=ifelse(role_next=="H",1,0)) %>%
  mutate(switch_to_NB_binom=ifelse(role_next=="NB",1,0)) %>%
  mutate(switch_to_B_binom=ifelse(role_next=="B",1,0)) %>%
  ungroup()

current_H_sum<-current_H %>%
  group_by(id) %>%
  summarise(sex=first(sex),
            dispersal=first(dispersal),
            type=first(type),
            switch_to_H=sum(switch_to_H_binom, na.rm=T),
            switch_to_NB=sum(switch_to_NB_binom, na.rm=T),
            switch_to_B=sum(switch_to_B_binom, na.rm=T)) %>%
  mutate(switch_to_H_binom=as.numeric(switch_to_H>0),
         switch_to_NB_binom=as.numeric(switch_to_NB>0),
         switch_to_B_binom=as.numeric(switch_to_B>0))

current_NB<-full_lifetimes_all %>%
  group_by(id) %>%
  filter(role=="NB") %>%
  mutate(switch_to_H_binom=ifelse(role_next=="H",1,0)) %>%
  mutate(switch_to_NB_binom=ifelse(role_next=="NB",1,0)) %>%
  mutate(switch_to_B_binom=ifelse(role_next=="B",1,0)) %>%
  ungroup()

current_NB_sum<-current_NB %>%
  group_by(id) %>%
  summarise(sex=first(sex),
            dispersal=first(dispersal),
            type=first(type),
            switch_to_H=sum(switch_to_H_binom, na.rm=T),
            switch_to_NB=sum(switch_to_NB_binom, na.rm=T),
            switch_to_B=sum(switch_to_B_binom, na.rm=T)) %>%
  mutate(switch_to_H_binom=as.numeric(switch_to_H>0),
         switch_to_NB_binom=as.numeric(switch_to_NB>0),
         switch_to_B_binom=as.numeric(switch_to_B>0))

# resident males ####
current_H_sum %>%
  filter(type=="N_M") %>%
  summarise(sum(switch_to_B_binom))/nrow(current_H_sum %>% filter(type=="N_M"))
# 21% resident males H->B at least once

current_B_sum %>%
  filter(type=="N_M") %>%
  summarise(sum(switch_to_H_binom))/nrow(current_B_sum %>% filter(type=="N_M"))
# 45% resident males B->H at least once

# immigrant males ####
current_H_sum %>%
  filter(type=="I_M") %>%
  summarise(sum(switch_to_B_binom))/nrow(current_H_sum %>% filter(type=="I_M"))
# 35% immigrant males H->B at least once

current_B_sum %>%
  filter(type=="I_M") %>%
  summarise(sum(switch_to_H_binom))/nrow(current_B_sum %>% filter(type=="I_M"))
# 46% immigrant males B->H at least once

# immigrant females ####
current_H_sum %>%
  filter(type=="I_F") %>%
  summarise(sum(switch_to_B_binom))/nrow(current_H_sum %>% filter(type=="I_F"))
# 37% immigrant females H->B at least once

current_B_sum %>%
  filter(type=="I_F") %>%
  summarise(sum(switch_to_H_binom))/nrow(current_B_sum %>% filter(type=="I_F"))
# 47% immigrant females B->H at least once

# how often does switch from B->H happen after nest failure ("redirected helping")?
nest_outcomes <- as.data.frame(distinct(
  read.csv("nest_outcomes.csv") %>%
    mutate(nest_success_binary=ifelse(nest_success=="yes",1,0)) %>% select (nest, nest_success_binary) %>% rename(nest_success=nest_success_binary))) %>%
  group_by(nest) %>%
  filter(!(is.na(nest_success) & n() > 1)) %>%
  ungroup()

current_B1<-left_join(current_B,nest_outcomes,by="nest")
current_B<-current_B1
rm(current_B1)

current_B %>% filter(nest_success==0) %>% summarise(sum(switch_to_H_binom,na.rm=T)) # B->H = 24%

current_B %>% filter(nest_success==0) %>% summarise(sum(switch_to_NB_binom,na.rm=T)) # B->NBNH = 36%

current_B %>% filter(nest_success==0) %>% summarise(sum(switch_to_B_binom,na.rm=T)) # B->B = 40%

# how often does switch from B->H happen after nest success?
current_B %>% filter(nest_success==1) %>% summarise(sum(switch_to_H_binom,na.rm=T)) # B->H = 28%

current_B %>% filter(nest_success==1) %>% summarise(sum(switch_to_NB_binom,na.rm=T)) # B->NBNH = 30%

current_B %>% filter(nest_success==1) %>% summarise(sum(switch_to_B_binom,na.rm=T)) # B->B = 42%

# create role-switching data frame for plotting
role_switchingHtoB<-current_H_sum %>%
  filter(!dispersal=="EX") %>%
  select(type,switch_to_B_binom) %>%
  group_by(type)%>%
  mutate(possible=n()) %>%
  summarise(observed=sum(switch_to_B_binom),
            possible=first(possible))
role_switchingHtoB$from<-"helper"
role_switchingHtoB$to<-"breeder"

role_switchingBtoH<-current_B_sum %>%
  filter(!dispersal=="EX") %>%
  select(type,switch_to_H_binom) %>%
  group_by(type)%>%
  mutate(possible=n()) %>%
  summarise(observed=sum(switch_to_H_binom),
            possible=first(possible))
role_switchingBtoH$from<-"breeder"
role_switchingBtoH$to<-"helper"
role_switchingBtoH_NF<-data.frame(type="N_F",observed=0,possible=0,from="breeder",to="helper")
role_switchingBtoH<-rbind(role_switchingBtoH,role_switchingBtoH_NF)

role_switching<-rbind(role_switchingBtoH,role_switchingHtoB)

role_switching<-role_switching %>%
  mutate(type=ifelse(type=="I_F","immigrant female",type))%>%
  mutate(type=ifelse(type=="I_M","immigrant male",type))%>%
  mutate(type=ifelse(type=="N_M","resident male",type))%>%
  mutate(type=ifelse(type=="N_F","resident female",type))

# plot role switch events ####
d<-role_switching
d$ci.low <- NA
d$ci.high <- NA
d$proportion <- NA

# add 95% CIs
for (i in 1:nrow(d)) {
  obs <- d$observed[i]
  pos <- d$possible[i]
  if(pos>0){
    d$proportion[i] <- obs/pos
    ci <- as.numeric(binom.test(x=obs, n= pos)$conf.int)
    d$ci.low[i] <- ci[1]
    d$ci.high[i] <- ci[2]
  }
}
d

# plot showing switches only
(plot <-
    d %>%
    separate(type, into= c('dispersal', 'sex'), remove = F) %>%
    mutate(name =paste(from, "to", to)) %>%
    mutate(type = factor(type, levels= c("resident male", "resident female", "immigrant male", "immigrant female"))) %>%
    mutate(proportion = round(proportion, 2)) %>%
    mutate(label= paste0(observed, "/",possible)) %>%
    ggplot(aes(y=name, x=proportion, color=sex, fill=sex ))+
    facet_wrap(~type, ncol=1)+
    geom_col(alpha=0.3)+
    geom_errorbarh(aes(xmin= ci.low, xmax= ci.high), height=0.25)+
    geom_point(shape= "square",size=2)+
    geom_text(aes(x= proportion/2, label= label), color= 'black', hjust=1)+
    coord_cartesian(xlim=c(0,1.02),expand=F)+
    ylab("")+
    xlab("proportion of individuals")+
    scale_color_manual(values= c("red", "blue"))+
    scale_fill_manual(values= c("red", "blue"))+
    theme_classic()+
    theme(
      legend.position = 'none',
      strip.background = element_blank(),
      axis.text.y=element_text(size=10),
      axis.text.x=element_text(size=10),
      axis.title.y=element_text(size=10),
      axis.title.x=element_text(size=12),
      strip.text = element_text(size=12, hjust =0)))

# save as PDF
ggsave(
  filename= 'results/Figure 3.pdf',
  plot = plot,
  scale = 1,
  width = 8,
  height = 4,
  units = c("in", "cm", "mm", "px"),
  dpi = 300)
