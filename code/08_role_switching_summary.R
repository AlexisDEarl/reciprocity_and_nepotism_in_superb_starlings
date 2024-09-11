# How common is social role switching?
# Does role switching only happen after nest failure? ("redirected helping")
# Alexis Earl, ade2102@columbia.edu
# Gerry Carter, gcarter1640@gmail.com

# clear workspace
rm(list=ls())

# load packages
library(readxl)
library(tidyverse)
library(igraph)
library(boot)
library(patchwork)
library(plotrix)
library(tibble)
library(data.table)
library(ggplot2)

# load data
d <- read.csv("daily_helping.csv")
dyads<-read.csv("dyads.csv")

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
                # dispersal,
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
                                  #helper.dispersal,
                                  group,
                                  breeding_season,
                                  date,
                                  nest,
                                  mother,
                                  father) %>%
                    rename(id=helper,
                           sex=helper.sex,
                           #dispersal=helper.dispersal,
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
  #fill(dispersal, .direction= "updown") %>%
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
    #dispersal=first(dispersal),
            sex=first(sex),
            group=first(group),
            date=first(date),
            mother=first(mother),
            father=first(father),
            #fledge=last(fledge),
            #nest_success=last(nest_success),
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
#rm(B_and_H1)

# add dispersal status
ids<-read.csv("data/individuals.csv") %>%
  select(Individual,dispersal.status) %>%
  rename("id"="Individual",
         "dispersal"="dispersal.status")
df1<-left_join(df,ids,by="id")

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
sd(role.switch.count$switch_count)
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

sum(current_B_sum$switch_to_H_binom)/length(unique(current_B_sum$id))

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

sum(current_H_sum$switch_to_B_binom)/length(unique(current_H_sum$id))

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

sum(current_NB_sum$switch_to_B_binom)/length(unique(current_NB_sum$id))

# males (all) ####
current_H_sum %>%
  filter(sex=="M") %>%
  summarise(sum(switch_to_H_binom))/nrow(current_H_sum %>% filter(sex=="M"))
# 105/161 (65%) resident males H->H at least once

current_H_sum %>%
  filter(sex=="M") %>%
  summarise(sum(switch_to_B_binom))/nrow(current_H_sum %>% filter(sex=="M"))
# 49/161 (30%) males H->B at least once

current_H_sum %>%
  filter(sex=="M") %>%
  summarise(sum(switch_to_NB_binom))/nrow(current_H_sum %>% filter(sex=="M"))
# 103/161 (64%) males H->NB at least once

current_B_sum %>%
  filter(sex=="M") %>%
  summarise(sum(switch_to_H_binom))/nrow(current_B_sum %>% filter(sex=="M"))
# 47/95 (50%) males B->H at least once

current_B_sum %>%
  filter(sex=="M") %>%
  summarise(sum(switch_to_B_binom))/nrow(current_B_sum %>% filter(sex=="M"))
# 39/95 (41%) males B->B at least once

current_B_sum %>%
  filter(sex=="M") %>%
  summarise(sum(switch_to_NB_binom))/nrow(current_B_sum %>% filter(sex=="M"))
# 54/95 (57%) males B->NB at least once

current_NB_sum %>%
  filter(sex=="M") %>%
  summarise(sum(switch_to_B_binom))/nrow(current_NB_sum %>% filter(sex=="M"))
# 61/122 (50%) males NB->B at least once

current_NB_sum %>%
  filter(sex=="M") %>%
  summarise(sum(switch_to_H_binom))/nrow(current_NB_sum %>% filter(sex=="M"))
# 95/122 (78%) males NB->H at least once

current_NB_sum %>%
  filter(sex=="M") %>%
  summarise(sum(switch_to_NB_binom))/nrow(current_NB_sum %>% filter(sex=="M"))
# 73/122 (60%) males NB->NB at least once

# resident males ####
current_H_sum %>%
  filter(type=="N_M") %>%
  summarise(sum(switch_to_H_binom))/nrow(current_H_sum %>% filter(type=="N_M"))
# 50/66 (76%) resident males H->H at least once

current_H_sum %>%
  filter(type=="N_M") %>%
  summarise(sum(switch_to_B_binom))/nrow(current_H_sum %>% filter(type=="N_M"))
# 14/66 (21%) resident males H->B at least once

current_H_sum %>%
  filter(type=="N_M") %>%
  summarise(sum(switch_to_NB_binom))/nrow(current_H_sum %>% filter(type=="N_M"))
# 40/66 (61%) resident males H->NB at least once

current_B_sum %>%
  filter(type=="N_M") %>%
  summarise(sum(switch_to_H_binom))/nrow(current_B_sum %>% filter(type=="N_M"))
# 10/23 (44%) resident males B->H at least once

current_B_sum %>%
  filter(type=="N_M") %>%
  summarise(sum(switch_to_B_binom))/nrow(current_B_sum %>% filter(type=="N_M"))
# 9/23 (39%) resident males B->B at least once

current_B_sum %>%
  filter(type=="N_M") %>%
  summarise(sum(switch_to_NB_binom))/nrow(current_B_sum %>% filter(type=="N_M"))
# 14/23 (61%) resident males B->NB at least once

current_NB_sum %>%
  filter(type=="N_M") %>%
  summarise(sum(switch_to_B_binom))/nrow(current_NB_sum %>% filter(type=="N_M"))
# 18/43 (42%) resident males NB->B at least once

current_NB_sum %>%
  filter(type=="N_M") %>%
  summarise(sum(switch_to_H_binom))/nrow(current_NB_sum %>% filter(type=="N_M"))
# 37/43 (86%) resident males NB->H at least once

current_NB_sum %>%
  filter(type=="N_M") %>%
  summarise(sum(switch_to_NB_binom))/nrow(current_NB_sum %>% filter(type=="N_M"))
# 28/43 (65%) resident males NB->NB at least once

# immigrant males ####
current_H_sum %>%
  filter(type=="I_M") %>%
  summarise(sum(switch_to_H_binom))/nrow(current_H_sum %>% filter(type=="I_M"))
# 28/49 (57%) immigrant males H->H at least once

current_H_sum %>%
  filter(type=="I_M") %>%
  summarise(sum(switch_to_B_binom))/nrow(current_H_sum %>% filter(type=="I_M"))
# 17/49 (35%) immigrant males H->B at least once

current_H_sum %>%
  filter(type=="I_M") %>%
  summarise(sum(switch_to_NB_binom))/nrow(current_H_sum %>% filter(type=="I_M"))
# 29/49 (59%) immigrant males H->NB at least once

current_B_sum %>%
  filter(type=="I_M") %>%
  summarise(sum(switch_to_H_binom))/nrow(current_B_sum %>% filter(type=="I_M"))
# 16/34 (47%) immigrant males B->H at least once

current_B_sum %>%
  filter(type=="I_M") %>%
  summarise(sum(switch_to_B_binom))/nrow(current_B_sum %>% filter(type=="I_M"))
# 12/34 (35%) immigrant males B->B at least once

current_B_sum %>%
  filter(type=="I_M") %>%
  summarise(sum(switch_to_NB_binom))/nrow(current_B_sum %>% filter(type=="I_M"))
# 18/34 (53%) immigrant males B->NB at least once

current_NB_sum %>%
  filter(type=="I_M") %>%
  summarise(sum(switch_to_B_binom))/nrow(current_NB_sum %>% filter(type=="I_M"))
# 15/35 (43%) immigrant males NB->B at least once

current_NB_sum %>%
  filter(type=="I_M") %>%
  summarise(sum(switch_to_H_binom))/nrow(current_NB_sum %>% filter(type=="I_M"))
# 29/35 (83%) immigrant males NB->H at least once

current_NB_sum %>%
  filter(type=="I_M") %>%
  summarise(sum(switch_to_NB_binom))/nrow(current_NB_sum %>% filter(type=="I_M"))
# 18/35 (51%) immigrant males NB->NB at least once

# resident females ####
current_H_sum %>%
  filter(type=="N_F") %>%
  summarise(sum(switch_to_H_binom))/nrow(current_H_sum %>% filter(type=="N_F"))
# 43/45 (96%) resident females H->H at least once

current_H_sum %>%
  filter(type=="N_F") %>%
  summarise(sum(switch_to_NB_binom))/nrow(current_H_sum %>% filter(type=="N_F"))
# 15/45 (33%) resident females H->NB at least once

current_NB_sum %>%
  filter(type=="N_F") %>%
  summarise(sum(switch_to_H_binom))/nrow(current_NB_sum %>% filter(type=="N_F"))
# 15/15 (100%) resident females NB->H at least once

current_NB_sum %>%
  filter(type=="N_F") %>%
  summarise(sum(switch_to_NB_binom))/nrow(current_NB_sum %>% filter(type=="N_F"))
# 6/15 (40%) resident females NB->NB at least once

# immigrant females ####
current_H_sum %>%
  filter(type=="I_F") %>%
  summarise(sum(switch_to_H_binom))/nrow(current_H_sum %>% filter(type=="I_F"))
# 37/90 (41%) immigrant females H->H at least once

current_H_sum %>%
  filter(type=="I_F") %>%
  summarise(sum(switch_to_B_binom))/nrow(current_H_sum %>% filter(type=="I_F"))
# 33/90 (37%) immigrant females H->B at least once

current_H_sum %>%
  filter(type=="I_F") %>%
  summarise(sum(switch_to_NB_binom))/nrow(current_H_sum %>% filter(type=="I_F"))
# 50/90 (56%) immigrant females H->NB at least once

current_B_sum %>%
  filter(type=="I_F") %>%
  summarise(sum(switch_to_H_binom))/nrow(current_B_sum %>% filter(type=="I_F"))
# 38/82 (46%) immigrant females B->H at least once

current_B_sum %>%
  filter(type=="I_F") %>%
  summarise(sum(switch_to_B_binom))/nrow(current_B_sum %>% filter(type=="I_F"))
# 40/82 (49%) immigrant females B->B at least once

current_B_sum %>%
  filter(type=="I_F") %>%
  summarise(sum(switch_to_NB_binom))/nrow(current_B_sum %>% filter(type=="I_F"))
# 58/82 (71%) immigrant females B->NB at least once

current_NB_sum %>%
  filter(type=="I_F") %>%
  summarise(sum(switch_to_B_binom))/nrow(current_NB_sum %>% filter(type=="I_F"))
# 54/85 (64%) immigrant females NB->B at least once

current_NB_sum %>%
  filter(type=="I_F") %>%
  summarise(sum(switch_to_H_binom))/nrow(current_NB_sum %>% filter(type=="I_F"))
# 50/85 (59%) immigrant females NB->H at least once

current_NB_sum %>%
  filter(type=="I_F") %>%
  summarise(sum(switch_to_NB_binom))/nrow(current_NB_sum %>% filter(type=="I_F"))
# 55/85 (65%) immigrant females NB->NB at least once

# how often does switch from B->H happen after nest failure ("redirected helping")?
current_B %>% filter(nest_success==1) %>% summarise(sum(switch_to_H_binom,na.rm=T))
current_B %>% filter(nest_success==0) %>% summarise(sum(switch_to_H_binom,na.rm=T))
current_B %>% filter(nest_success==0) %>% summarise(sum(switch_to_NB_binom,na.rm=T))
current_B %>% filter(nest_success==0) %>% summarise(sum(switch_to_B_binom,na.rm=T))
