packages.used=as.list(
  c(
    "tidyverse",
    "haven",
    "devtools",
    "RColorBrewer",
    "data.table",
    "ggplot2")
)

check.pkg = function(x){
  if(!require(x, character.only=T)) install.packages(x, 
                                                     character.only=T,
                                                     dependence=T)
}
lapply(packages.used, check.pkg)
library(tidyverse)
library(haven)
library(devtools)
library(RColorBrewer)
library(DT)
library(ggplot2)

anes_dat <- 
  read_sav("C:/Users/Jingbin Cao/Documents/GitHub/Spring2021-Project1-JingbinCao/data/anes_timeseries_cdf.sav")
dim(anes_dat)



barplot(table(anes_dat$VCF0004),
        las=2,
        main="Number of Respondents Over the Years")

Election_years=as.character(seq(1952, 2016, 4))
anes_use=anes_dat%>%
  mutate(
    year=as_factor(VCF0004),
    econ_past=as_factor(VCF0870),
    econ_next=as_factor(VCF0872),
    econ_gov_market=as_factor(VCF9132),
    econ_party=as_factor(VCF9205),
    econ_stock=as_factor(VCF9224),
    econ_home=as_factor(VCF0146),
    job_working=as_factor(VCF0150),
    job_unemp_past=as_factor(VCF9226),
    job_unemp_next=as_factor(VCF9229),
    job_foreign=as_factor(VCF9231),
    secu_tort=as_factor(VCF9233),
    secu_fed_spend=as_factor(VCF9049),
    immig_job=as_factor(VCF9223),
    immig_muslim=as_factor(VCF9267),
    immig_illeg=as_factor(VCF0233),
    immig_incre=as_factor(VCF0879),
    insu_status=as_factor(VCF9281),
    insu_curr=as_factor(VCF9218),
    insu_type=as_factor(VCF0806),
    insu_demo=as_factor(VCF0508),
    insu_rep=as_factor(VCF0509),
    weal_wastetax=as_factor(VCF0606),
    weal_gap=as_factor(VCF9228),
    weal_rich=as_factor(VCF9268),
    intera_army=as_factor(VCF0844),
    lead_demo=as_factor(VCF9209),
    lead_rep=as_factor(VCF9213),
    court_perf=as_factor(VCF0655),
    envir_regul=as_factor(VCF0842),
    envir_party=as_factor(VCF9008),
    envir_fedspend=as_factor(VCF9047),
    trust_first=as_factor(VCF0632),
    trust_second=as_factor(VCF0633),
    trust_third=as_factor(VCF0634),
    satisf=as_factor(VCF9245),
    like_demo=as_factor(VCF9207),
    like_rep=as_factor(VCF9208),
    demo_rep=as_factor(VCF0413),
    prefer=as_factor(VCF9022),
    vote=as_factor(VCF0706),
    race=as_factor(VCF0105a),
    gender=as_factor(VCF0104),
    income=as_factor(VCF0114)
  )%>%
  filter(year %in% Election_years)

anes_use = anes_use%>%select(year, 
                             econ_past, econ_next, econ_gov_market, econ_party,econ_stock,econ_home,
                             job_working,job_unemp_past,job_unemp_next,job_foreign,
                             secu_tort,secu_fed_spend,
                             immig_job,immig_muslim,immig_illeg,immig_incre,
                             insu_status,insu_curr,insu_type,insu_demo,insu_rep,
                             weal_wastetax,weal_gap,
                             intera_army,
                             lead_demo,lead_rep,
                             court_perf,
                             trust_first, trust_second, trust_third,
                             envir_regul,envir_party,envir_fedspend,
                             like_demo, like_rep,
                             prefer,
                             demo_rep,
                             satisf,
                             vote, race, gender,income)
save(anes_use, file="../output/data_use.RData")

load(file="../output/data_use.RData")
dat_2016 <- anes_dat %>% filter(VCF0004 == 2016)

dat_2016 =dat_2016%>%
  mutate(
    year=as_factor(VCF0004),
    econ_past=as_factor(VCF0870),
    econ_next=as_factor(VCF0872),
    econ_gov_market=as_factor(VCF9132),
    econ_party=as_factor(VCF9205),
    econ_stock=as_factor(VCF9224),
    econ_home=as_factor(VCF0146),
    job_working=as_factor(VCF0150),
    job_unemp_past=as_factor(VCF9226),
    job_unemp_next=as_factor(VCF9229),
    job_foreign=as_factor(VCF9231),
    secu_tort=as_factor(VCF9233),
    secu_fed_spend=as_factor(VCF9049),
    immig_job=as_factor(VCF9223),
    immig_muslim=as_factor(VCF9267),
    immig_illeg=as_factor(VCF0233),
    immig_incre=as_factor(VCF0879),
    insu_status=as_factor(VCF9281),
    insu_curr=as_factor(VCF9218),
    insu_type=as_factor(VCF0806),
    insu_demo=as_factor(VCF0508),
    insu_rep=as_factor(VCF0509),
    weal_wastetax=as_factor(VCF0606),
    weal_gap=as_factor(VCF9228),
    weal_rich=as_factor(VCF9268),
    intera_army=as_factor(VCF0844),
    lead_demo=as_factor(VCF9209),
    lead_rep=as_factor(VCF9213),
    court_perf=as_factor(VCF0655),
    envir_regul=as_factor(VCF0842),
    envir_party=as_factor(VCF9008),
    envir_fedspend=as_factor(VCF9047),
    trust_first=as_factor(VCF0632),
    trust_second=as_factor(VCF0633),
    trust_third=as_factor(VCF0634),
    like_demo=as_factor(VCF9207),
    like_rep=as_factor(VCF9208),
    demo_rep=as_factor(VCF0413),
    prefer=as_factor(VCF9022),
    satisf=as_factor(VCF9245),
    vote=as_factor(VCF0706),
    race=as_factor(VCF0105a),
    gender=as_factor(VCF0104),
    income=as_factor(VCF0114)
  )%>%
  filter(year %in% Election_years)
dat_2016 = dat_2016 %>%select(year, 
                              econ_past, econ_next, econ_gov_market, econ_party,econ_stock,econ_home,
                              job_working,job_unemp_past,job_unemp_next,job_foreign,
                              secu_tort,secu_fed_spend,
                              immig_job,immig_muslim,immig_illeg,immig_incre,
                              insu_status,insu_curr,insu_type,insu_demo,insu_rep,
                              weal_wastetax,weal_gap,
                              intera_army,
                              lead_demo,lead_rep,
                              court_perf,
                              trust_first, trust_second, trust_third,
                              envir_regul,envir_party,envir_fedspend,
                              like_demo, like_rep,
                              prefer,
                              demo_rep,
                              satisf,
                              vote, race, gender,income)
save(dat_2016, file="../output/dat_2016.RData")

load(file="../output/dat_2016.RData")
load(file="../output/data_use.RData")

# 1.1Past
econ_past_hist = anes_use %>%
  filter(!is.na(econ_past))  %>%
  filter(vote == "1. Democrat" | vote == "2. Republican") %>%
  group_by(year, vote) %>%
  count(econ_past) %>%
  group_by(year, vote) %>%
  mutate(
    prop = n/sum(n)
  )


ggplot(econ_past_hist,
       aes(x=year, y=prop, fill=econ_past))+
  geom_bar(stat="identity",colour="black")+
  facet_wrap(~vote,ncol=1) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0))+
  scale_fill_brewer(palette="Blues")+
  labs(title="How much economy better or worse last year?")

econ_past_2016 = dat_2016 %>%
  filter(!is.na(econ_past) ) %>%
  filter(vote == "1. Democrat" | vote == "2. Republican") %>%
  group_by(year, vote) %>%
  count(econ_past) %>%
  group_by(year, vote) %>%
  mutate(
    prop = n/sum(n)
  )

ggplot(econ_past_2016,
       aes(x=prop, y=year, fill=econ_past))+
  geom_bar(stat="identity",colour="black")+
  facet_wrap(~vote,ncol=1) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0))+
  scale_fill_brewer(palette="Blues")+
  labs(title="How much economy better or worse last year?")

# 1.2Future

econ_next_hist = anes_use %>%
  filter(!is.na(econ_next))  %>%
  filter(vote == "1. Democrat" | vote == "2. Republican") %>%
  group_by(year, vote) %>%
  count(econ_next) %>%
  group_by(year, vote) %>%
  mutate(
    prop = n/sum(n)
  )

ggplot(econ_next_hist,
       aes(x=year, y=prop, fill=econ_next))+
  geom_bar(stat="identity",colour="black")+
  facet_wrap(~vote,ncol=1) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0))+
  scale_fill_brewer(palette="Blues")+
  labs(title="How much economy better or worse next year?")


econ_next_2016 = dat_2016 %>%
  filter(!is.na(econ_next) ) %>%
  filter(vote == "1. Democrat" | vote == "2. Republican") %>%
  group_by(year, vote) %>%
  count(econ_next) %>%
  group_by(year, vote) %>%
  mutate(
    prop = n/sum(n)
  )

ggplot(econ_next_2016,
       aes(x=prop, y=year, fill=econ_next))+
  geom_bar(stat="identity",colour="black")+
  facet_wrap(~vote,ncol=1) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0))+
  scale_fill_brewer(palette="Blues")+
  labs(title="How much economy better or worse next year?")

# 2.Goverment control or Freemarket?

econ_gov_mar_2016 = dat_2016 %>%
  filter(!is.na(econ_gov_market) ) %>%
  filter(vote == "1. Democrat" | vote == "2. Republican") %>%
  group_by(year, vote) %>%
  count(econ_gov_market) %>%
  group_by(year, vote) %>%
  mutate(
    prop = n/sum(n)
  )

ggplot(econ_gov_mar_2016,
       aes(x=prop, y=year, fill=econ_gov_market))+
  geom_bar(stat="identity",colour="black")+
  facet_wrap(~vote,ncol=1) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0))+
  scale_fill_brewer(palette="Blues")+
  labs(title="Government can handel economy or freemarket handel?")

# 3.Which party can handel economy

econ_party_2016 = dat_2016 %>%
  filter(!is.na(econ_party) ) %>%
  filter(vote == "1. Democrat" | vote == "2. Republican") %>%
  group_by(year, vote) %>%
  count(econ_party) %>%
  group_by(year, vote) %>%
  mutate(
    prop = n/sum(n)
  )

ggplot(econ_party_2016,
       aes(x=prop, y=year, fill=econ_party))+
  geom_bar(stat="identity",colour="black")+
  facet_wrap(~vote,ncol=1) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0))+
  labs(title="Which party would do a better job handeling national economy?")

# 4.Invest in stock market?
econ_stock_2016 = dat_2016 %>%
  filter(!is.na(econ_stock) ) %>%
  filter(vote == "1. Democrat" | vote == "2. Republican") %>%
  group_by(year, vote) %>%
  count(econ_stock) %>%
  group_by(year, vote) %>%
  mutate(
    prop = n/sum(n)
  )

ggplot(econ_stock_2016,
       aes(x=prop, y=year, fill=econ_stock))+
  geom_bar(stat="identity",colour="black")+
  facet_wrap(~vote,ncol=1) +
  theme_bw()+
  scale_fill_brewer(palette="Blues")+
  theme(axis.text.x = element_text(angle = 0))+
  labs(title="Do you invest in stock market?")

econ_stock_hist = anes_use %>%
  filter(!is.na(econ_stock))  %>%
  filter(vote == "1. Democrat" | vote == "2. Republican") %>%
  group_by(year, vote) %>%
  count(econ_stock) %>%
  group_by(year, vote) %>%
  mutate(
    prop = n/sum(n)
  )


ggplot(econ_stock_hist,
       aes(x=year, y=prop, fill=econ_stock))+
  geom_bar(stat="identity",colour="black")+
  facet_wrap(~vote,ncol=1) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0))+
  scale_fill_brewer(palette="Blues")+
  labs(title="Do you invest in stock market?")

# 5.Own home?
econ_home_2016 = dat_2016 %>%
  filter(!is.na(econ_home) ) %>%
  filter(vote == "1. Democrat" | vote == "2. Republican") %>%
  group_by(year, vote) %>%
  count(econ_home) %>%
  group_by(year, vote) %>%
  mutate(
    prop = n/sum(n)
  )


ggplot(econ_home_2016,
       aes(x=prop, y=year, fill=econ_home))+
  geom_bar(stat="identity",colour="black")+
  facet_wrap(~vote,ncol=1) +
  theme_bw()+
  scale_fill_brewer(palette="Blues")+
  theme(axis.text.x = element_text(angle = 0))+
  labs(title="Do you own home?")


econ_home_hist = anes_use %>%
  filter(!is.na(econ_home))  %>%
  filter(vote == "1. Democrat" | vote == "2. Republican") %>%
  group_by(year, vote) %>%
  count(econ_home) %>%
  group_by(year, vote) %>%
  mutate(
    prop = n/sum(n)
  )


ggplot(econ_home_hist,
       aes(x=year, y=prop, fill=econ_home))+
  geom_bar(stat="identity",colour="black")+
  facet_wrap(~vote,ncol=1) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0))+
  scale_fill_brewer(palette="Blues")+
  labs(title="Do you own home?")

#Job
# 1.Current working status
job_working_2016 = dat_2016 %>%
  filter(!is.na(job_working) ) %>%
  filter(vote == "1. Democrat" | vote == "2. Republican") %>%
  group_by(year, vote) %>%
  count(job_working) %>%
  group_by(year, vote) %>%
  mutate(
    prop = n/sum(n)
  )

ggplot(job_working_2016,
       aes(x=prop, y=year, fill=job_working))+
  geom_bar(stat="identity",colour="black")+
  facet_wrap(~vote,ncol=1) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0))+
  labs(title="Current working status of people who voted.")


# 2. Level of unemployment last year
job_unemp_past_2016 = dat_2016 %>%
  filter(!is.na(job_unemp_past) ) %>%
  filter(vote == "1. Democrat" | vote == "2. Republican") %>%
  group_by(year, vote) %>%
  count(job_unemp_past) %>%
  group_by(year, vote) %>%
  mutate(
    prop = n/sum(n)
  )

ggplot(job_unemp_past_2016,
       aes(x=prop, y=year, fill=job_unemp_past))+
  geom_bar(stat="identity",colour="black")+
  facet_wrap(~vote,ncol=1) +
  theme_bw()+
  scale_fill_brewer(palette="Blues")+
  theme(axis.text.x = element_text(angle = 0))+
  labs(title="Rate of umployment last year?")

# 3. Level of unemployment next year
job_unemp_next_2016 = dat_2016 %>%
  filter(!is.na(job_unemp_next) ) %>%
  filter(vote == "1. Democrat" | vote == "2. Republican") %>%
  group_by(year, vote) %>%
  count(job_unemp_next) %>%
  group_by(year, vote) %>%
  mutate(
    prop = n/sum(n)
  )

ggplot(job_unemp_next_2016,
       aes(x=prop, y=year, fill=job_unemp_next))+
  geom_bar(stat="identity",colour="black")+
  facet_wrap(~vote,ncol=1) +
  theme_bw()+
  scale_fill_brewer(palette="Blues")+
  theme(axis.text.x = element_text(angle = 0))+
  labs(title="Rate of umployment next year?")

# 4. New limits on foreign imports
job_foreign_2016 = dat_2016 %>%
  filter(!is.na(job_foreign) ) %>%
  filter(vote == "1. Democrat" | vote == "2. Republican") %>%
  group_by(year, vote) %>%
  count(job_foreign) %>%
  group_by(year, vote) %>%
  mutate(
    prop = n/sum(n)
  )

ggplot(job_foreign_2016,
       aes(x=prop, y=year, fill=job_foreign))+
  geom_bar(stat="identity",colour="black")+
  facet_wrap(~vote,ncol=1) +
  theme_bw()+
  scale_fill_brewer(palette="Blues")+
  theme(axis.text.x = element_text(angle = 0))+
  labs(title="Should U.S. government have new limits on foreign imports \n in order to protect job market?")

#Immigration
#1. Immigration took jobs
immig_job_2016 = dat_2016 %>%
  filter(!is.na(immig_job) ) %>%
  filter(vote == "1. Democrat" | vote == "2. Republican") %>%
  group_by(year, vote) %>%
  count(immig_job) %>%
  group_by(year, vote) %>%
  mutate(
    prop = n/sum(n)
  )


ggplot(immig_job_2016,
       aes(x=prop, y=year, fill=immig_job))+
  geom_bar(stat="identity",colour="black")+
  facet_wrap(~vote,ncol=1) +
  theme_bw()+
  scale_fill_brewer(palette="Blues")+
  theme(axis.text.x = element_text(angle = 0))+
  labs(title="How likely new immigrants will take jobs from the people already here?")


# 2. Muslim Thermometer
immig_muslim_2016_demo = dat_2016 %>%
  filter(!is.na(immig_muslim) ) %>%
  filter(vote == "1. Democrat") %>%
  count(immig_muslim) %>%
  mutate(
    prop = n/sum(n)
  )

ggplot(data=immig_muslim_2016_demo)+
  geom_point(mapping = aes(x=immig_muslim, y=prop))+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title="Muslim Thermometer from the people who voted for democratic")

immig_muslim_2016_rep = dat_2016 %>%
  filter(!is.na(immig_muslim) ) %>%
  filter(vote == "2. Republican") %>%
  count(immig_muslim) %>%
  mutate(
    prop = n/sum(n)
  )

ggplot(data=immig_muslim_2016_rep)+
  geom_point(mapping = aes(x=immig_muslim, y=prop))+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title="Muslim Thermometer from the people who voted for republican")

#3.Illegal Immigration Theometer
immig_illeg_2016_demo = dat_2016 %>%
  filter(!is.na(immig_illeg) ) %>%
  filter(vote == "1. Democrat") %>%
  count(immig_illeg) %>%
  mutate(
    prop = n/sum(n)
  )

ggplot(data=immig_illeg_2016_demo)+
  geom_point(mapping = aes(x=immig_illeg, y=prop))+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title="Illegal Immigration Thermometer from the people who voted for democratic")

immig_illeg_2016_rep = dat_2016 %>%
  filter(!is.na(immig_illeg) ) %>%
  filter(vote == "2. Republican") %>%
  count(immig_illeg) %>%
  mutate(
    prop = n/sum(n)
  )
ggplot(data=immig_illeg_2016_rep)+
  geom_point(mapping = aes(x=immig_illeg, y=prop))+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title="Illegal Immigration Thermometer from the people who voted for republican")

#4. Increasing Immigration
immig_incre_2016 = dat_2016 %>%
  filter(!is.na(immig_incre) ) %>%
  filter(vote == "1. Democrat" | vote == "2. Republican") %>%
  group_by(year, vote) %>%
  count(immig_incre) %>%
  group_by(year, vote) %>%
  mutate(
    prop = n/sum(n)
  )

ggplot(immig_incre_2016,
       aes(x=prop, y=year, fill=immig_incre))+
  geom_bar(stat="identity",colour="black")+
  facet_wrap(~vote,ncol=1) +
  theme_bw()+
  scale_fill_brewer(palette="Blues")+
  theme(axis.text.x = element_text(angle = 0))+
  labs(title="Increase or decrease number of immigrant to the U.S.?")

#Tax
# 1.Waste tax
weal_wastetax_2016 = dat_2016 %>%
  filter(!is.na(weal_wastetax) ) %>%
  filter(vote == "1. Democrat" | vote == "2. Republican") %>%
  group_by(year, vote) %>%
  count(weal_wastetax) %>%
  group_by(year, vote) %>%
  mutate(
    prop = n/sum(n)
  )

ggplot(weal_wastetax_2016,
       aes(x=prop, y=year, fill=weal_wastetax))+
  geom_bar(stat="identity",colour="black")+
  facet_wrap(~vote,ncol=1) +
  theme_bw()+
  scale_fill_brewer(palette="Blues")+
  theme(axis.text.x = element_text(angle = 0))+
  labs(title="How much does the Federal Government waste tax money?")

# 2.Wealth gap
weal_gap_2016 = dat_2016 %>%
  filter(!is.na(weal_gap) ) %>%
  filter(vote == "1. Democrat" | vote == "2. Republican") %>%
  group_by(year, vote) %>%
  count(weal_gap) %>%
  group_by(year, vote) %>%
  mutate(
    prop = n/sum(n)
  )

ggplot(weal_gap_2016,
       aes(x=prop, y=year, fill=weal_gap))+
  geom_bar(stat="identity",colour="black")+
  facet_wrap(~vote,ncol=1) +
  theme_bw()+
  scale_fill_brewer(palette="Blues")+
  theme(axis.text.x = element_text(angle = 0))+
  labs(title="Is there smaller or larger income gaps \n between poor and wealthy people in the U.S. than 20 years ago?")

#Candidates
# 1. Demo Leadership

lead_demo_hist = anes_use %>%
  filter(!is.na(lead_demo) ) %>%
  filter(vote == "1. Democrat" | vote == "2. Republican") %>%
  group_by(year, vote) %>%
  count(lead_demo) %>%
  group_by(year, vote) %>%
  mutate(
    prop = n/sum(n)
  )

ggplot(lead_demo_hist,
       aes(x=prop, y=year, fill=lead_demo))+
  geom_bar(stat="identity",colour="black")+
  facet_wrap(~vote,ncol=1) +
  theme_bw()+
  scale_fill_brewer(palette="Blues")+
  theme(axis.text.x = element_text(angle = 0))+
  labs(title="Leadership of Democratic Candidate?")

# 2. Rep Leadership

lead_rep_hist = anes_use %>%
  filter(!is.na(lead_rep) ) %>%
  filter(vote == "1. Democrat" | vote == "2. Republican") %>%
  group_by(year, vote) %>%
  count(lead_rep) %>%
  group_by(year, vote) %>%
  mutate(
    prop = n/sum(n)
  )

ggplot(lead_rep_hist,
       aes(x=prop, y=year, fill=lead_rep))+
  geom_bar(stat="identity",colour="black")+
  facet_wrap(~vote,ncol=1) +
  theme_bw()+
  scale_fill_brewer(palette="Blues")+
  theme(axis.text.x = element_text(angle = 0))+
  labs(title="Leadership of Republican Candidate?")

# 3. Like Demo 
like_demo_hist = anes_use %>%
  filter(!is.na(like_demo) ) %>%
  filter(vote == "1. Democrat" | vote == "2. Republican") %>%
  group_by(year, vote) %>%
  count(like_demo) %>%
  group_by(year, vote) %>%
  mutate(
    prop = n/sum(n)
  )

ggplot(like_demo_hist,
       aes(x=prop, y=year, fill=like_demo))+
  geom_bar(stat="identity",colour="black")+
  facet_wrap(~vote,ncol=1) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0))+
  labs(title="Do you like Democratic Candidate?")

# 4. Like Rep

like_rep_hist = anes_use %>%
  filter(!is.na(like_rep) ) %>%
  filter(vote == "1. Democrat" | vote == "2. Republican") %>%
  group_by(year, vote) %>%
  count(like_rep) %>%
  group_by(year, vote) %>%
  mutate(
    prop = n/sum(n)
  )

ggplot(like_rep_hist,
       aes(x=prop, y=year, fill=like_rep))+
  geom_bar(stat="identity",colour="black")+
  facet_wrap(~vote,ncol=1) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 0))+
  labs(title="Do you like Republican Candidate?")

# 5. Would you prefer the candidate you voted?

prefer_hist = anes_use %>%
  filter(!is.na(prefer) ) %>%
  filter(vote == "1. Democrat" | vote == "2. Republican") %>%
  group_by(year, vote) %>%
  count(prefer) %>%
  group_by(year, vote) %>%
  mutate(
    prop = n/sum(n)
  )

ggplot(prefer_hist,
       aes(x=prop, y=year, fill=prefer))+
  geom_bar(stat="identity",colour="black")+
  facet_wrap(~vote,ncol=1) +
  theme_bw()+
  scale_fill_brewer(palette="Blues")+
  theme(axis.text.x = element_text(angle = 0))+
  labs(title="Would you prefer the candidate you voted?")

#Environment
# 1. Environment regulation
envir_regul_2016 = dat_2016 %>%
  filter(!is.na(envir_fedspend) ) %>%
  filter(vote == "1. Democrat" | vote == "2. Republican") %>%
  group_by(year, vote) %>%
  count(envir_fedspend) %>%
  group_by(year, vote) %>%
  mutate(
    prop = n/sum(n)
  )

ggplot(envir_regul_2016,
       aes(x=prop, y=year, fill=envir_fedspend))+
  geom_bar(stat="identity",colour="black")+
  facet_wrap(~vote,ncol=1) +
  theme_bw()+
  scale_fill_brewer(palette="Blues")+
  theme(axis.text.x = element_text(angle = 0))+
  labs(title="Should federal spending on improving and protecting the environment?")

#Income
income_2016 = dat_2016 %>%
  filter(!is.na(income) ) %>%
  filter(vote == "1. Democrat" | vote == "2. Republican") %>%
  group_by(year, vote) %>%
  count(income) %>%
  group_by(year, vote) %>%
  mutate(
    prop = n/sum(n)
  )

ggplot(income_2016,
       aes(x=prop, y=year, fill=income))+
  geom_bar(stat="identity",colour="black")+
  facet_wrap(~vote,ncol=1) +
  theme_bw()+
  scale_fill_brewer(palette="Blues")+
  theme(axis.text.x = element_text(angle = 0))+
  labs(title="Income Group")

#Gender
income_2016 = dat_2016 %>%
  filter(!is.na(income) ) %>%
  filter(vote == "1. Democrat" | vote == "2. Republican") %>%
  group_by(year, vote) %>%
  count(income) %>%
  group_by(year, vote) %>%
  mutate(
    prop = n/sum(n)
  )

ggplot(income_2016,
       aes(x=prop, y=year, fill=income))+
  geom_bar(stat="identity",colour="black")+
  facet_wrap(~vote,ncol=1) +
  theme_bw()+
  scale_fill_brewer(palette="Blues")+
  theme(axis.text.x = element_text(angle = 0))+
  labs(title="Income Group")