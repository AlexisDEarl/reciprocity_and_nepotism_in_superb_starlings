# Reciprocal helping and nepotism in superb starlings
# Alexis Earl, ade2102@columbia.edu
# Gerry Carter, gcarter1640@gmail.com

# clean the data

# set working directory
setwd("/Users/alexisearl/Library/CloudStorage/GoogleDrive-ade2102@columbia.edu/My Drive/PhD/Chapter 1/manuscript/Reciprocal_helping_and_kin_bias_in_superb_starlings")

# clear workspace
rm(list=ls())

# remove MASS package to use dplyr "select" function
if("MASS" %in% (.packages())){detach("package:MASS", unload=TRUE)}

# load packages
library(tidyverse)
library(readxl)

# get data on individual traits
# raw.ids <- read.csv("data/individuals.csv")
raw.ids <- read_excel("data/individuals.xlsx")

# get observations of helping events
# raw.events <- read.csv("data/observations.csv")
raw.events <- read_excel("data/observations.xlsx")

# get kinship
kinship <-
  #read.csv("data/kinship.csv") %>%
  read_excel("data/kinship.xlsx") %>%
  # label dyad in alphanumeric order
  mutate(dyad= ifelse(individual1<individual2,
                      paste(individual1, individual2, sep="_"),
                      paste(individual2, individual1, sep="_")))

# get birds seen (outside helping data) by breeding season
birds.seen <-
  #read.csv('data/superbstarlings_by_group_by_season.csv') %>%
  read_excel('data/superbstarlings_by_group_by_season.xlsx') %>%
  mutate(group= case_when(
    group == "CF2/CF1?" ~ "CF2",
    group == "MRC1/SRB2" ~ "MRC1",
    group == "WDB1" ~ "WDB",
    group == "MRC2" ~ "MRC1",
    TRUE ~ group))

birds.seen %>%
  group_by(group) %>%
  summarize(n=n())

# tidy individual data
ids <-
  raw.ids %>%
  dplyr::rename(id=Individual, group=Social.unit, sex=Sex) %>%
  # list every individual only once and use most recent record
  # in raw data there were 4 individuals repeated with identical information
  group_by(id) %>%
  dplyr::summarize(group= last(group),
            sex= last(sex),
            dispersal.status= last(dispersal.status)) %>%
   mutate(dispersal.status= ifelse(dispersal.status== "I ", "I", dispersal.status))

# get total number of individuals banded
nrow(ids)
#1175

# clean observed helping events----

# fix typos/errors
unique(raw.events$date)
raw.events$date <- as.Date(raw.events$date,format = "%m/%d/%y")
raw.events$mother[which(raw.events$mother=="UNMK")] <- NA
raw.events$mother[which(raw.events$mother=="?")] <- NA
raw.events$mother[which(raw.events$mother=="BB-11997 (METAL)")] <- "BB-11997"
raw.events$father[which(raw.events$father=="UNMK")] <- NA
raw.events$father[which(raw.events$father=="?")] <- NA
raw.events$father[which(raw.events$father=="METAL")] <- NA
raw.events$mother[which(raw.events$mother=="B-0169")] <- "BB-0169"
raw.events$visitor[which(raw.events$visitor=="B-0169")] <- "BB-0169"
raw.events$mother[which(raw.events$mother=="B-4237")] <- "BB-4237"
raw.events$visitor[which(raw.events$visitor=="B-4237")] <- "BB-4237"

# fix events where mother and father were accidentally swapped
raw.events$mother[which(raw.events$father=='B-41268' & raw.events$mother=='BB-4238')] <- 'B-41268'
raw.events$father[which(raw.events$father=='B-41268' & raw.events$mother=='BB-4238')] <- 'BB-4238'
raw.events$father[which(raw.events$father=='B-41268')] <- NA

# remove all other cases where visitor IDs are not in the individuals dataset
unique(raw.events$visitor[which(!raw.events$visitor %in% ids$id)])
raw.events$visitor[which(!raw.events$visitor %in% ids$id)] <- NA

# set visitor roles to be either father, mother, or other
unique(raw.events$visitor_role)
raw.events$visitor_role[which(!raw.events$visitor_role %in% c("FATHER", "MOTHER", "OTHER"))] <- NA

# fix fledging status
unique(raw.events$fledge)
raw.events$fledge <- as.integer(raw.events$fledge)

# fix nest success
unique(raw.events$nest_success)
raw.events$nest_success<- ifelse(
  raw.events$nest_success=="yes",TRUE,
                          ifelse(
  raw.events$nest_success=="no", FALSE,
  NA))

# check cases with missing sampling start times
unique(raw.events$sampling_start_time)
raw.events %>% filter(is.na(sampling_start_time)) %>% as_tibble() # 716 cases without observation start times
raw.events %>%  filter(is.na(sampling_time_total_in_min)) %>% as_tibble() # 9 events without observation duration

# what are most common start times
raw.events %>%
  group_by(sampling_start_time) %>%
  summarize(n=n()) %>%
  arrange(desc(n))

# check visitor attendance (helping)
raw.events %>%
  group_by(visitor_attendance_min) %>%
  summarize(n=n()) %>%
  arrange(visitor_attendance_min) %>%
  print(n=100)

# round up half-minutes
raw.events$visitor_attendance_min[which(raw.events$visitor_attendance_min == 0.5)] <- 1

# NA is zero minutes
raw.events$visitor_attendance_min[which(is.na(raw.events$visitor_attendance_min))] <- 0

# check helping at nest (feeding time)
raw.events %>%
  group_by(duration_helping_in_nest) %>%
  summarize(n=n()) %>%
  arrange(duration_helping_in_nest) %>%
  print(n=100)

# fix typo in group
raw.events$group[which(raw.events$group=="SB2")] <- "SRB2"

# additional cleaning of helping data--------
events <-
  raw.events %>%
  as_tibble() %>%
  # get feeding time minutes and seconds
  separate(duration_helping_in_nest, into= c('tmin', 'tsec'), sep=":", remove = T) %>%
  # remove 2 typos ("23" minutes)
  mutate(tmin= ifelse(tmin==23, NA, tmin)) %>%
  # convert feeding time to minutes
  mutate(feeding = as.numeric(tmin) + (as.numeric(tsec)/60)) %>%
  # define helping
  mutate(helping= case_when(
    # if a bird was observed feeding but nest attendance was zero minutes, then assume attendance is at least one minute
    # change 87 of 14588 cases
    feeding > 0 & visitor_attendance_min== 0 ~ 1,
    # if a bird was observed with food but nest attendance was zero minutes, then assume attendance is at least one minute
    # change 155 of 14588 cases
    food == "Y" & visitor_attendance_min== 0 ~ 1,
    # otherwise use nest attendance
    TRUE ~ visitor_attendance_min)) %>%
  # delete 9 events without observation duration
  filter(!is.na(sampling_time_total_in_min)) %>%
  # fix sampling duration
  mutate(sample.duration= as.numeric(sampling_time_total_in_min)) %>%
  # convert variables into numbers
  mutate(offspring_age= as.numeric(offspring_age),
       number_of_offspring= as.numeric(number_of_offspring),
       fledge= as.numeric(fledge)) %>%
  separate(sampling_start_time, into=c("sample.h", "sample.min"), sep=":") %>%
  # convert sample time to 24 h
  mutate(sample.h= ifelse(AMPM=="PM" & as.integer(sample.h)<12,
                          as.integer(sample.h)+12,
                          as.integer(sample.h)),
         sample.min= as.integer(sample.min)) %>%

  # label parental care
  mutate(parental.care= visitor==father | visitor==mother) %>%
  # combine father and mother
  mutate(parents = paste(father, mother)) %>%
  # delete variables
  select(-sampling_stop_time, -sampling_time_total_hrs_mins, -sampling_time_total_in_min, -AMPM, -tmin, -tsec, -visitor_attendance_min, -food, -food.type, -nest_code, -offspring_age, fledge, -number_of_birds, -activity) %>%
  filter(!is.na(visitor)) %>%
  # change variable order
  relocate(breeding_season, date, sample.h, sample.min, group, nest= nest_code_with_attempt, visitor, helping, sample.duration, parental.care, parents)

# fix cases where same date has different season
events$breeding_season[which(events$date== "2016-03-02" & events$breeding_season== "2015SR")] <- "2016LR"
events$breeding_season[which(events$date== "2016-03-01" & events$breeding_season== "2015SR")] <- "2016LR"
events$breeding_season[which(events$date== "2016-03-03" & events$breeding_season== "2015SR")] <- "2016LR"

birds.seen$n.days <-  1

# get all birds seen in every breeding season and group-----
birds.seen2 <-
  events %>%
  select(date, breeding_season, group, father, mother, visitor) %>%
  pivot_longer(father: visitor, names_to = 'type', values_to = 'id') %>%
  group_by(breeding_season, group, id, date) %>%
  summarize(n=n(), .groups= 'drop') %>%
  group_by(breeding_season, group, id) %>%
  summarize(n.days=n(), .groups= 'drop') %>%
  # 1789 cases
  # add birds seen outside helping observations
  full_join(birds.seen) %>%
  group_by(breeding_season, group, id) %>%
  summarize(n.days=sum(n.days, na.rm= T), .groups= 'drop')
  # 8467 cases

# get first and last observation days for every bird's breeding seasons
t <-
  birds.seen2 %>%
  group_by(id) %>%
  summarize(first= min(breeding_season),
            last= max(breeding_season))


# get birds by season and group
birds.season.group <-
  # get every possible combo of bird and season
  expand_grid(id= unique(birds.seen2$id),
              breeding_season= unique(birds.seen2$breeding_season),
              n.days=0) %>%
  # add birds seen outside helping observations
  full_join(birds.seen2) %>%
  filter(!is.na(id)) %>%
  filter(!is.na(breeding_season)) %>%
  # add first and last seasons for every bird
  mutate(first.season = t$first[match(.$id, t$id)]) %>%
  mutate(last.season = t$last[match(.$id, t$id)]) %>%
  # remove group assignments before and after those seasons
  filter(breeding_season >= first.season) %>%
  filter(breeding_season <= last.season) %>%
  # summarize
  group_by(id, breeding_season, group) %>%
  summarize(n.days= sum(n.days, na.rm=T)) %>%
  # fill in any group locations for birds not seen in a breeding season
  group_by(id) %>%
  fill(group, .direction= "down") %>%
  group_by(id, breeding_season, group) %>%
  summarize(n.days= sum(n.days, na.rm=T), .groups= 'drop') %>%
  group_by(id, breeding_season) %>%
  mutate(total.days= sum(n.days, na.rm=T))  %>%
  ungroup()  %>%
  # get days present for each bird in each group
  pivot_wider(names_from = group, values_from = n.days) %>%
  # label birds as missing for that season
  mutate(missing = as.numeric(total.days==0)) %>%
  group_by(id) %>%
  # label missing last season
  mutate(lag.missing = lag(missing)) %>%
  # count cumulative missing seasons (only if bird was missing the last season)
  mutate(c.missing= cumsum(missing)*lag.missing)  %>%
  # birds not seen for 5 or more seasons are probably dead
  mutate(dead= c.missing >=5)  %>%
  # remove probably dead birds
  filter(!dead)  %>%
  ungroup() %>%
  select(-missing, -lag.missing, -c.missing, -dead, -total.days)

# get birds present per breeding_season and group
birds.present <-
  birds.season.group %>%
  pivot_longer(MRC1:CF1, names_to = 'group', values_to = 'n.days') %>%
  filter(!is.na(n.days))


# get possible cases of birds helping in different groups in same season?
possible.group.movements <-
  birds.present %>%
  group_by(id, breeding_season, group) %>%
  summarize(n=n()) %>%
  group_by(id, breeding_season) %>%
  mutate(n=n()) %>%
  filter(n>1) %>%
  arrange(id, breeding_season, group) %>%
  mutate(label= paste0('group', row_number())) %>%
  ungroup() %>%
  pivot_wider(values_from = group, names_from = label) %>%
  mutate(obs= ifelse(is.na(group3), paste(group1, group2, sep= "_"), paste(group1, group2, group3, sep= "_"))) %>%
  group_by(obs) %>%
  summarize(n=n()) %>%
  arrange(desc(n)) %>%
  # Dustin manually vetted which ones were possible
  filter(n>=10) %>%
  pull(obs)

# fix cases where birds were in the wrong group
birds.present2 <-
  birds.present %>%
  arrange(id, breeding_season, group) %>%
  group_by(id) %>%
  mutate(lag.group= lag(group)) %>%
  ungroup() %>%
  mutate(possible = case_when(
    is.na(lag.group) ~ TRUE,
    group == lag.group ~ TRUE,
    paste(lag.group, group, sep="_") %in% possible.group.movements ~ TRUE,
    paste(group, lag.group, sep="_") %in% possible.group.movements ~ TRUE,
    TRUE ~ FALSE))

# look at birds with impossible obs
birds.present2 %>%
  group_by(id) %>%
  mutate(always.possible= mean(possible)==1) %>%
  filter(!always.possible) %>%
  print(n=200)

# get birds present without impossible obs (possible visitors) per breeding season
possible.helpers <-
  birds.present2 %>%
  filter(possible) %>%
  select(helper= id, breeding_season, group, obs= n.days)

# check durations of sampling periods
sp <-
  events %>%
  group_by(sample.duration) %>%
  summarize(n=n()) %>%
  mutate(proportion= n/sum(n)*100) %>%
  arrange(desc(proportion)) %>%
  print(n=46)

# 90% of sampling durations were at least 2 hour
sp %>%
  mutate(at.least.hour = sample.duration >= 120) %>%
  group_by(at.least.hour) %>%
  summarize(n= sum(n)) %>%
  mutate(proportion= n/sum(n)*100)

# 99.5% of sampling durations were at least 1 hour
sp %>%
  mutate(at.least.hour = sample.duration >= 60) %>%
  group_by(at.least.hour) %>%
  summarize(n= sum(n)) %>%
  mutate(proportion= n/sum(n)*100)

# check sampling duration
min(events$sample.duration) # 3
max(events$sample.duration) # 240
mean(events$sample.duration) # 125
median(events$sample.duration)  # 120

# get sampling times for each date and nest
date_nest_sampling <-
  events %>%
  group_by(group, breeding_season, date, sample.h, sample.min, nest, sample.duration) %>%
  summarize(n=n()) %>%
  group_by(group, breeding_season, date, sample.h, sample.min, nest) %>%
  summarize(sample.duration = max(sample.duration, na.rm=T)) %>%
  group_by(group, breeding_season, date, nest) %>%
  summarize(sample.duration = sum(sample.duration, na.rm=T)) %>%
  ungroup() %>%
  filter(!is.na(date)) %>%
  filter(!is.na(group)) %>%
  mutate(date_nest= paste(date, nest)) %>%
  mutate(date_season_group= paste(date, breeding_season, group))

# get total sampling effort per nest
nest_sampling <-
  date_nest_sampling %>%
  group_by(nest) %>%
  summarize(sample.duration= sum(sample.duration))

# get all possible helpers and all possible nests for each date and group
date_groups <- sort(unique(date_nest_sampling$date_season_group))
pos <- list()

# for each date and group, get all possible helpers and all possible nests
for (i in 1:length(date_groups)) {

  date_group <- date_groups[i]
  date1 <- substr(date_group,1,10)
  season1 <- substr(date_group,12,17)
  group1 <- substr(date_group,19,100)

  # get possible helpers
  pos.helpers <-
    possible.helpers %>%
    filter(breeding_season== season1) %>%
    filter(group == group1) %>%
    pull(helper) %>%
    unique()

  # get possible nests
  pos.nests <-
    date_nest_sampling %>%
    filter(date== date1) %>%
    filter(breeding_season== season1) %>%
    filter(group == group1) %>%
    pull(nest) %>%
    unique()

  # get possible helping for that day, and group
  pos[[i]] <- expand_grid(breeding_season= season1, date= date1, group= group1, helper= pos.helpers, nest= pos.nests)
}

# turn list into dataframe
possible.help <-
  bind_rows(pos) %>%
  mutate(helping= 0) %>%
  mutate(obs.type="possible")

# add back fathers and mothers
nests <-
  events %>%
  group_by(nest, father, mother) %>%
  summarize(n=1) %>%
  group_by(nest) %>%
  summarize(father= first(father), mother= first(mother), n=n()) %>%
  ungroup()
possible.help$father <- nests$father[match(possible.help$nest, nests$nest)]
possible.help$mother <- nests$mother[match(possible.help$nest, nests$nest)]

# remove parents as helpers
possible.help <-
  possible.help %>%
  filter(helper != father) %>%
  filter(helper != mother)

# summarize helping for each day
observed.help <-
  events %>%
  mutate(helper= visitor) %>%
  # get only alloparental care events
  filter(!parental.care) %>%
  # label obs as the unique combo of group, nest, parent, date
  group_by(breeding_season, group, nest, date, helper, mother, father) %>%
  # get visit duration (minutes) and counts of visits
  summarize(helping= sum(helping, na.rm=T)) %>%
  ungroup() %>%
  mutate(date= as.character(date)) %>%
  mutate(obs.type="actual")

# combine actual and possible visits and summarize for each helper-nest dyad
d <-
  full_join(observed.help, possible.help) %>%
  # get sampling duration for each date and nest
  mutate(date_nest= paste(date, nest)) %>%
  mutate(sample.duration= date_nest_sampling$sample.duration[match(.$date_nest, date_nest_sampling$date_nest)]) %>%
  # get sum of helping from helper to nest per day and max sample duration for that day
  group_by(breeding_season, date, group, helper, father, mother, nest) %>%
  summarize(help = sum(helping, na.rm=T),
            sample.duration= first(sample.duration),
            .groups= 'drop') %>%
  # 33522
  # add mom and dad dyads in alphanumeric order
  mutate(mom.dyad= ifelse(helper<mother,
                          paste(helper, mother, sep="_"),
                          paste(mother, helper, sep="_"))) %>%
  mutate(dad.dyad= ifelse(helper<father,
                          paste(helper, father, sep="_"),
                          paste(father, helper, sep="_"))) %>%
  # add kinship
  mutate(kinship.mom = kinship$kinship[match(.$mom.dyad, kinship$dyad)]) %>%
  mutate(kinship.dad = kinship$kinship[match(.$dad.dyad, kinship$dyad)]) %>%
  mutate(kinship.max= case_when(
    kinship.mom>=kinship.dad ~ kinship.mom,
    kinship.mom<=kinship.dad ~ kinship.dad,
    is.na(kinship.mom) ~ kinship.dad,
    is.na(kinship.dad) ~ kinship.mom)) %>%
  # add microsatellite based kinship
  mutate(microsat.kinship.mom = kinship$relatedness[match(.$mom.dyad, kinship$dyad)]) %>%
  mutate(microsat.kinship.dad = kinship$relatedness[match(.$dad.dyad, kinship$dyad)]) %>%
  mutate(microsat.kinship.max= case_when(
    microsat.kinship.mom>=microsat.kinship.dad ~ microsat.kinship.mom,
    microsat.kinship.mom<=microsat.kinship.dad ~ microsat.kinship.dad,
    is.na(microsat.kinship.mom) ~ microsat.kinship.dad,
    is.na(microsat.kinship.dad) ~ microsat.kinship.mom)) %>%
  # add helper sex
  mutate(helper.sex = ids$sex[match(.$helper, ids$id)]) %>%
  # add helper dispersal status
  mutate(helper.dispersal = ids$dispersal.status[match(.$helper, ids$id)]) %>%
  # helping cannot exceed sampling
  mutate(help = ifelse(help>sample.duration, sample.duration, help)) %>%
  # relabel mother and father dyads as directed to get directed rates
  mutate(dad.dyad= paste(helper, father, sep="-->")) %>%
  mutate(mom.dyad= paste(helper, mother, sep="-->")) %>%
  mutate(helping.rate = help/sample.duration)

# assign missing  dispersal status for female breeders-----

# what is probability that natal females become breeders
# get female breeders
moms <- unique(d$mother)
# get natal helpers
natal.f.helpers <-
  d %>%
  filter(help>0) %>%
  filter(helper.sex=="F" & helper.dispersal=="N") %>%
  pull(helper) %>%
  unique() %>%
  sort()

natal.f.helpers
length(natal.f.helpers)
# 91 natal female helpers

# how many were also breeders?
(t <- length(natal.f.helpers[which(natal.f.helpers %in% moms)]))
# 0 became breeders

# how many breeding opportunities?
(t2 <-
  observed.help %>%
  filter(helper %in% natal.f.helpers) %>%
  group_by(helper, breeding_season) %>%
  summarize(n=n()) %>%
  summarize(seasons=n()) %>%
  summarize(seasons= sum(seasons)) %>%
  pull(seasons))
# 152

# what is probability that natal helper is also a breeder? 0 of 152 cases
binom.test(t,t2)
# 0 of 152, 95% CI = 0% to 2.4%

# what is probability that immigrant female helpers are also breeders?
# get immigrant helpers
i.f.helpers <- d %>% filter(help>0) %>% filter(helper.sex=="F" & helper.dispersal=="I") %>% pull(helper) %>% unique()
i.f.helpers
length(i.f.helpers)
# 112 immigrant female helpers

# how many were also breeders?
length(i.f.helpers[which(i.f.helpers %in% moms)])
# 68 became breeders

binom.test(68,112)
# 61%, [51% to 70%]

# how many breeding seasons opportunities across those natal helpers
observed.help %>%
  filter(helper %in% i.f.helpers | mother %in% i.f.helpers ) %>%
  mutate(bird = ifelse(helper %in% i.f.helpers, helper, mother)) %>%
  group_by(bird, breeding_season) %>%
  summarize(n=n()) %>%
  summarize(seasons=n()) %>%
  summarize(seasons= sum(seasons))
# 332

# how many breeding seasons with breeding
observed.help %>%
  filter(mother %in% i.f.helpers ) %>%
  group_by(mother, breeding_season) %>%
  summarize(n=n()) %>%
  summarize(seasons=n()) %>%
  summarize(seasons= sum(seasons))
# 181

# existing females that became breeders are almost certainly immigrant females

# which existing (EX) females are also breeders?
ex.f.helpers <- d %>% filter(help>0) %>% filter(helper.sex=="F" & helper.dispersal=="EX") %>% pull(helper) %>% unique()
ex.f.helpers
length(ex.f.helpers)
# 48 female helpers with unknown dispersal status
ex.f.breeders <- ex.f.helpers[which(ex.f.helpers %in% moms)]
length(ex.f.breeders)
# 21 of those females became breeders

# relabel those 21 females as "immigrant females
d2 <-
  d %>%
  mutate(helper.dispersal= ifelse(helper %in% ex.f.breeders, "I", helper.dispersal))

# get mean dyadic rates across all days-------------
mom.dyads <-
  d2 %>%
  group_by(helper, mother, helper.sex, helper.dispersal) %>%
  summarize(help.rate = mean(helping.rate, na.rm=T),
            kinship.mom = mean(kinship.mom, na.rm=T),
            microsat.kinship.mom = mean(microsat.kinship.mom, na.rm=T)) %>%
  mutate(kinship= ifelse(helper.dispersal== "I", microsat.kinship.mom, kinship.mom)) %>%
  mutate(receiver.sex= "F") %>%
  mutate(helper.type = case_when(
    helper.sex == "F" & helper.dispersal == "N" ~ "Resident Female",
    helper.sex == "F" & helper.dispersal == "I" ~ "Immigrant Female",
    helper.sex == "M" & helper.dispersal == "N" ~ "Resident Male",
    helper.sex == "M" & helper.dispersal == "I" ~ "Immigrant Male",
    helper.sex == "M" & helper.dispersal == "EX" ~ "Other Male",
    helper.sex == "F" & helper.dispersal == "EX" ~ "Other Female",
    helper.sex == "M" & is.na(helper.dispersal) ~ "Other Male",
    helper.sex == "F" & is.na(helper.dispersal) ~ "Other Female",
    is.na(helper.sex) ~ "Unknown sex")) %>%
  select(helper, receiver= mother, helper.sex, helper.type, receiver.sex, kinship, help.rate) %>%
  ungroup()

dad.dyads <-
  d2 %>%
  group_by(helper, father, helper.sex, helper.dispersal) %>%
  summarize(help.rate = mean(helping.rate, na.rm=T),
            kinship.dad = mean(kinship.dad, na.rm=T),
            microsat.kinship.dad = mean(microsat.kinship.dad, na.rm=T)) %>%
  mutate(kinship= ifelse(helper.dispersal== "I", microsat.kinship.dad, kinship.dad)) %>%
  mutate(receiver.sex= "M") %>%
  mutate(helper.type = case_when(
    helper.sex == "F" & helper.dispersal == "N" ~ "Resident Female",
    helper.sex == "F" & helper.dispersal == "I" ~ "Immigrant Female",
    helper.sex == "M" & helper.dispersal == "N" ~ "Resident Male",
    helper.sex == "M" & helper.dispersal == "I" ~ "Immigrant Male",
    helper.sex == "M" & helper.dispersal == "EX" ~ "Other Male",
    helper.sex == "F" & helper.dispersal == "EX" ~ "Other Female",
    helper.sex == "M" & is.na(helper.dispersal) ~ "Other Male",
    helper.sex == "F" & is.na(helper.dispersal) ~ "Other Female",
    is.na(helper.sex) ~ "Unknown sex")) %>%
  select(helper, receiver= father, helper.sex, helper.type, receiver.sex, kinship, help.rate) %>%
  ungroup()

# combine
dyads <-
  rbind(mom.dyads, dad.dyads) %>%
  mutate(dyad= paste(helper, receiver, sep= "-->")) %>%
  mutate(reciprocal.dyad = paste(receiver, helper, sep= "-->")) %>%
  # label undirected dyads
  mutate(udyad = ifelse(helper<receiver,
                        paste(helper, receiver, sep= "_"),
                        paste(receiver, helper, sep= "_")))

# add reciprocal helping rate
dyads$reciprocal.help.rate <- dyads$help.rate[match(dyads$dyad, dyads$reciprocal.dyad)]

# add reciprocal helping rate to daily helping rates
d2$reciprocal.help.mom <- dyads$help.rate[match(d2$mom.dyad, dyads$reciprocal.dyad)]
d2$reciprocal.help.dad <- dyads$help.rate[match(d2$dad.dyad, dyads$reciprocal.dyad)]

# get max reciprocal help from either father or mother
d2 <-
  d2 %>%
  mutate(reciprocal.help.max = case_when(
    is.na(reciprocal.help.dad) & is.na(reciprocal.help.mom) ~ NA,
    !is.na(reciprocal.help.dad) & is.na(reciprocal.help.mom) ~ reciprocal.help.dad,
    is.na(reciprocal.help.dad) & !is.na(reciprocal.help.mom) ~ reciprocal.help.mom,
    reciprocal.help.dad > reciprocal.help.mom ~ reciprocal.help.dad,
    reciprocal.help.dad < reciprocal.help.mom ~ reciprocal.help.mom,
    reciprocal.help.dad == reciprocal.help.mom ~ reciprocal.help.mom)) %>%
  # label helper-nest pairs with reciprocal help
  mutate(reciprocal.help = reciprocal.help.max > 0)

# check reciprocal helping rate
dyads %>%
  filter(!is.na(reciprocal.help.rate)) %>%
  select(udyad, helper, receiver, help.rate, reciprocal.help.rate) %>%
  arrange(udyad, helper, receiver) %>%
  print(n=100)

# check there is one dyad per row
nrow(dyads)
#9569

dyads %>%
  group_by(dyad) %>%
  summarize(n=n()) %>%
  nrow()
#9569

# How many undirected dyads have reciprocal helping? 142
dyads %>%
  filter(help.rate>0) %>%
  filter(reciprocal.help.rate >0) %>%
  group_by(udyad) %>%
  summarize(n=n()) %>%
  nrow()

# How many undirected dyads could have had reciprocal helping? 528
dyads %>%
  filter(help.rate>0) %>%
  filter(reciprocal.help.rate >=0) %>%
  group_by(udyad) %>%
  summarize(n=n(), help.rate= mean(help.rate)) %>%
  nrow()

# How many undirected dyads could have had reciprocal helping? 1070
dyads %>%
  filter(help.rate>=0) %>%
  filter(reciprocal.help.rate >=0) %>%
  group_by(udyad) %>%
  summarize(n=n()) %>%
  nrow()

# what proportion of reciprocal helping dyads are same sex?
dyads %>%
  mutate(same.sex= helper.sex == receiver.sex) %>%
  filter(help.rate>0) %>%
  filter(reciprocal.help.rate>0) %>%
  mutate(type= case_when(
    helper.sex== "F" & same.sex ~ "both female",
    helper.sex== "F" & !same.sex ~ "mixed sex",
    helper.sex== "M" & same.sex ~ "both male",
    helper.sex== "M" & !same.sex ~ "mixed sex")) %>%
  group_by(type) %>%
  summarize(n=n()/2)

# 19 female-female dyads
# 50 male-male dyads

# 69 same-sex dyads
# 73 mixed-sex dyads

# counts of helping dyads by type of parent---------
dyads %>%
  filter(help.rate>0) %>%
  # add receiver dispersal
  mutate(receiver.dispersal = ids$dispersal.status[match(.$receiver, ids$id)]) %>%
  mutate(receiver.dispersal= ifelse(receiver %in% ex.f.breeders, "I", receiver.dispersal)) %>%
  group_by(receiver.sex, receiver.dispersal, receiver) %>%
  summarize(n=n()) %>%
  group_by(receiver.sex, receiver.dispersal) %>%
  summarize(n.birds=n())

# counts of reciprocal helping dyads by type of parent---------
dyads %>%
  filter(help.rate>0) %>%
  filter(reciprocal.help.rate>0) %>%
  # add receiver dispersal
  mutate(receiver.dispersal = ids$dispersal.status[match(.$receiver, ids$id)]) %>%
  mutate(receiver.dispersal= ifelse(receiver %in% ex.f.breeders, "I", receiver.dispersal)) %>%
  group_by(receiver.sex, receiver.dispersal, receiver) %>%
  summarize(n=n()) %>%
  group_by(receiver.sex, receiver.dispersal) %>%
  summarize(n.birds=n())

# counts of reciprocal helping dyads by type of helper---------
dyads %>%
  filter(help.rate>0) %>%
  filter(reciprocal.help.rate>0) %>%
  group_by(helper.type) %>%
  summarize(n=n())

# counts of reciprocal helping dyads by type of helper and receiver---------
dyads %>%
  filter(help.rate>0) %>%
  filter(reciprocal.help.rate>0) %>%
  # add receiver dispersal
  mutate(receiver.dispersal = ids$dispersal.status[match(.$receiver, ids$id)]) %>%
  mutate(receiver.dispersal= ifelse(receiver %in% ex.f.breeders, "I", receiver.dispersal)) %>%
  mutate(receiver.type = case_when(
    receiver.sex == "F" & receiver.dispersal == "N" ~ "Resident Female",
    receiver.sex == "F" & receiver.dispersal == "I" ~ "Immigrant Female",
    receiver.sex == "M" & receiver.dispersal == "N" ~ "Resident Male",
    receiver.sex == "M" & receiver.dispersal == "I" ~ "Immigrant Male",
    receiver.sex == "M" & receiver.dispersal == "EX" ~ "Other Male",
    receiver.sex == "F" & receiver.dispersal == "EX" ~ "Other Female",
    receiver.sex == "M" & is.na(receiver.dispersal) ~ "Other Male",
    receiver.sex == "F" & is.na(receiver.dispersal) ~ "Other Female",
    is.na(receiver.sex) ~ "Unknown sex")) %>%

  # optional
  filter(receiver.type != "Other Male") %>%
  filter(receiver.type != "Other Female") %>%
  filter(helper.type != "Other Male") %>%
  filter(helper.type != "Other Female") %>%

  group_by(receiver.type, helper.type) %>%
  summarize(n=n())  %>%
  arrange(desc(n))


# counts of reciprocal helping dyads by type of helper and receiver, and kinship---------
dyads %>%
  filter(help.rate>0) %>%
  filter(reciprocal.help.rate>0) %>%
  mutate(kin= ifelse(kinship >= 0.125, "kin", "nonkin")) %>%
  mutate(kin= ifelse(is.na(kinship), "nonkin", kin)) %>%
  # add receiver dispersal
  mutate(receiver.dispersal = ids$dispersal.status[match(.$receiver, ids$id)]) %>%
  mutate(receiver.dispersal= ifelse(receiver %in% ex.f.breeders, "I", receiver.dispersal)) %>%
  mutate(receiver.type = case_when(
    receiver.sex == "F" & receiver.dispersal == "N" ~ "Resident Female",
    receiver.sex == "F" & receiver.dispersal == "I" ~ "Immigrant Female",
    receiver.sex == "M" & receiver.dispersal == "N" ~ "Resident Male",
    receiver.sex == "M" & receiver.dispersal == "I" ~ "Immigrant Male",
    receiver.sex == "M" & receiver.dispersal == "EX" ~ "Other Male",
    receiver.sex == "F" & receiver.dispersal == "EX" ~ "Other Female",
    receiver.sex == "M" & is.na(receiver.dispersal) ~ "Other Male",
    receiver.sex == "F" & is.na(receiver.dispersal) ~ "Other Female",
    is.na(receiver.sex) ~ "Unknown sex")) %>%

  # optional
  # filter(receiver.type != "Other Male") %>%
  # filter(receiver.type != "Other Female") %>%
  # filter(helper.type != "Other Male") %>%
  # filter(helper.type != "Other Female") %>%

  group_by(helper.type, receiver.type, kin) %>%
  summarize(n=n())  %>%
  ungroup() %>%
  print(n=50)

# inspect counts that should be identical
dyads %>%
  filter(help.rate>0) %>%
  filter(reciprocal.help.rate>0) %>%
  mutate(kin= ifelse(kinship >= 0.125, "kin", "nonkin")) %>%
  mutate(kin= ifelse(is.na(kinship), "nonkin", kin)) %>%
  # add receiver dispersal
  mutate(receiver.dispersal = ids$dispersal.status[match(.$receiver, ids$id)]) %>%
  mutate(receiver.dispersal= ifelse(receiver %in% ex.f.breeders, "I", receiver.dispersal)) %>%
  mutate(receiver.type = case_when(
    receiver.sex == "F" & receiver.dispersal == "N" ~ "Resident Female",
    receiver.sex == "F" & receiver.dispersal == "I" ~ "Immigrant Female",
    receiver.sex == "M" & receiver.dispersal == "N" ~ "Resident Male",
    receiver.sex == "M" & receiver.dispersal == "I" ~ "Immigrant Male",
    receiver.sex == "M" & receiver.dispersal == "EX" ~ "Other Male",
    receiver.sex == "F" & receiver.dispersal == "EX" ~ "Other Female",
    receiver.sex == "M" & is.na(receiver.dispersal) ~ "Other Male",
    receiver.sex == "F" & is.na(receiver.dispersal) ~ "Other Female",
    is.na(receiver.sex) ~ "Unknown sex")) %>%

  # optional
  filter(receiver.type == "Immigrant Male" | receiver.type == "Immigrant Female") %>%
  filter(helper.type == "Immigrant Male" | helper.type == "Immigrant Female") %>%
  filter(helper.type != receiver.type) %>%

  group_by(udyad, dyad, helper.type, receiver.type, reciprocal.help.rate) %>%
  summarize(help.rate= mean(help.rate, na.rm=T))  %>%
  ungroup() %>%
  arrange(udyad) %>%
  print(n=50)

# number of helping events, etc
raw.events %>% nrow()
events %>% nrow()
events$breeding_season %>% n_distinct()

# number of helpers
events$visitor %>% n_distinct()

# number of breeders
sum(events$mother %>% n_distinct(),
events$father %>% n_distinct())

# number of nests
events$nest %>% n_distinct()

events$date %>% unique() %>% sort() %>% length()

# most common foods
raw.events %>%
  group_by(food.type) %>%
  summarize(n=n()) %>%
  arrange(desc(n))

# save data
write.csv(dyads, file = "output/dyads.csv", row.names=F)
write.csv(d2, file="output/daily_helping.csv", row.names=F)
