setwd(".")

library(plyr)
library(dplyr)
library(tidyverse)

random<- read.csv("allfreqs_typicalwint_randpoints_2007_20240224.csv", stringsAsFactors = F)

atyp<- read.csv("allfreqs_atypicalwint_byroost_2007_20231229.csv", header =T)
typ<- read.csv("allfreqs_typicalwint_byroost_2007_20231229.csv", header =T)

fa<- read.csv("unconventional_wintdiet_2023128.csv")
ft<- read.csv("conventional_wintdiet_2023128.csv")

a<- fa[,c(1,2,6)]
a$value = paste(a$Genus, a$Species, sep = " ")
t<- ft[,c(1,2,6)]

t$value = paste(t$Genus, t$Species, sep = " ")

#merge the atypical
df<- merge(atyp, a, by = "value", all.x = T)

#merge the typical
nf<- merge(typ, t, by = "value", all.x = T)

#merge the typical
rf<- merge(random, t, by = "value", all.x = T)



##### intercept here to pick out just the winter species 
winter<- read.csv("master_dietlist_simple_20230911.csv", header = T)
winter<- subset(winter, Productive.season1 == "not_listed" | Productive.season1 == "winter" | Productive.season2 == "winter" )

df_wint<- merge(df, winter, by.x = "value", by.y ="Species")

nf_wint<- merge(nf, winter, by.x = "value", by.y ="Species")

rf_wint<- merge(rf, winter, by.x = "value", by.y ="Species")


typ_freqs6<- nf_wint %>%
  group_by(camp) %>%
  arrange(desc(n)) %>%
  slice(1:6) 

atyp_freqs6<- df_wint %>%
  group_by(camp) %>%
  arrange(desc(n)) %>%
  slice(1:6) 

rand_freqs6<- rf_wint %>%
  group_by(camp) %>%
  arrange(desc(n)) %>%
  slice(1:6)

asums<- atyp_freqs6 %>%
  group_by(camp) %>%
  dplyr::summarize(range_atypreliability= range(annual_reliability, na.rm = T))
rng = c("low", "high" )
asums$rng_type<- rep(rng,438)


tsums<- typ_freqs6 %>%
  group_by(camp) %>%
  dplyr::summarize(range_typreliability= range(annual_reliability, na.rm = T))
tsums$rng_type<- rep(rng,455)

tsums<- typ_freqs6 
asums<- atyp_freqs6 


rsums<- rand_freqs6 %>% 
  group_by(camp) %>%
  mutate(num_typwintsp = n())






#extract the years but do it really inneficiently 
new<- as.data.frame(str_split_fixed(tsums$camp, "_xy_20kbuff_REM2007_typclwintuppr8diss_bckgrnd_clip.shp",2))

new[2]<- NULL


tsums<- cbind(tsums, new)

tsums<- tsums[!is.na(tsums$Genus),]
tsums2<- tsums[,c(10,6, 3,1)]
tsums3<-tsums2 %>% 
  group_by(V1) %>%
  mutate(num_typwintsp = n())

tsums3$V1 <- str_remove(tsums3$V1 , "camp_")

 
#extract the years but do it really inneficiently 
new2<- as.data.frame(str_split_fixed(asums$camp, "_xy_20kbuff_REM2007_atypclwintfix2uppr8m_clip.shp",2))

new2[2]<- NULL

asums<- cbind(asums, new2)
asums2<- asums[,c(10,6)]
asums2$type<- "atypical"
tsums2$type<- "typical"



together<- merge(tsums2, asums2, by = c( "V1", "annual_reliability", "type"), all = T)

together$V1 <- str_remove(together$V1, "camp_")

colnames(together)[1]= "camp"


## seperate out winter roost sampled from not sampled

out<- read.csv("roosts_neversmpld_winter_20240218.csv", stringsAsFactors = F)

reliab_out<- tsums3 %>%
  filter(V1 %in% out$camp)

reliab_inwint<- tsums3 %>%
  filter(! V1 %in% out$camp)

roosts_inwint<-as.data.frame(unique(reliab_inwint$V1))
roosts_out<-as.data.frame(unique(reliab_out$V1))
rand_out<-as.data.frame(unique(rsums$camp))
colnames(rand_out)[1]<- "V1"

############### take a subsample of these data to match # of roosts not sampled in winter
sampl_inwint<- roosts_inwint %>%
  sample_frac(0.32) ##125 roosts
colnames(sampl_inwint)[1]<- "V1"

sampl_rand<- rand_out %>%
  sample_frac(1) ##123 points
colnames(sampl_rand)[1]<- "V1"


reliab_inwint_s<- reliab_inwint %>%
  filter(V1 %in% sampl_inwint$V1)

reliab_rand_s<- rsums %>%
  filter(camp %in% sampl_rand$V1)


mini.in<- reliab_inwint_s[c(1:3)]
mini.in$type<- "Roost Surveyed Winter"
mini.out<- reliab_out[c(1:3)]
mini.out$type<- "Roost Not Surveyed Winter"
mini.rand<- reliab_rand_s[c(2,6,3)]
colnames(mini.rand)[1]<- "V1"
mini.rand$type<- "Random Point"

twosample<- rbind(mini.in, mini.rand)

megasample<- rbind(mini.in, mini.out, mini.rand)
megasample$type2<- ifelse(megasample$type == "Random Point", "Random Point", "Roosts Surveyed" )

#################### new plot July 17  2024 making box plots 

r_inwint<- reliab_inwint[c(2,3)]
r_inwint$type<- "Roost Surveyed Winter"
r_out<- reliab_out[c(2,3)]
r_out$type<- "Roosts Not Surveyed Winter"
r_rand<- rsums[c(6,3)]
r_rand$type<- "Random Point"

r_all<- rbind(r_inwint, r_out, r_rand)
r_all$type2<- ifelse(r_all$type == "Random Point", "Random Point", "Roosts Surveyed" )

################# SAMPLING 
table(megasample$type)
table(twosample$type)



twosample %>%
  filter(annual_reliability != "NA") %>%
ggplot() +
  geom_boxplot(aes(x= as.factor(annual_reliability), y = n, color = type), position = position_dodge2(preserve = "single")) +
  scale_color_manual(values = c( "purple3", "mediumseagreen" ))+
  theme_bw(base_size = 8)+
  labs(x = "Annual Reliability Typical Winter Diet Species", y = "Species Count in Foraging Buffer", fill = "Type") +
  expand_limits(x=c(0,1))+
  #facet_wrap(~factor(type,levels = c("Roosts sampled in winter","Roosts not sampled in winter", "Random points")), ncol =1)+
  theme( legend.position = "bottom" )

r_all %>%
  filter(annual_reliability != "NA") %>%
  ggplot() +
  geom_boxplot(aes(x=as.factor(annual_reliability), y = n, color = type), position = "dodge") +
  expand_limits(x=c(0,1))+
  theme_bw(base_size = 10)+
  labs(x = "Annual Reliability Typical Winter Diet Species", y = "Species Count in Foraging Buffer", color = "Type") +
  scale_color_manual(values = c( "purple3","mediumseagreen", "orange3" ))+
  # scale_fill_manual(values = c( "purple3","mediumseagreen", "orange3" ))+
  
  theme( legend.position = "bottom")

