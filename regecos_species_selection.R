setwd(".")

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

##############################################################################################################
######## Identify the regional ecosystems with the major and possible diet list for BFF in QLD ######### 
##############################################################################################################
#read in already cleaned data from QLD_flora_list_filter script
redd<- read.csv("regecos_expandedspdf_V12_10sp_valid_20230129.csv", header = TRUE, stringsAsFactors = F)


####### Major diet species from studies in QLD ###########
all_dietsp<- read.csv("master_dietlist_simple_20230911.csv", header = TRUE, stringsAsFactors = F)

##!make a character vector of the full taxon names 
sp_full<-all_dietsp$Species

######## making abbreviated version of full taxon name ######
#make a dataframe that is the full taxon name 
sp<-as.data.frame(all_dietsp$Species, stringsAsFactors = F)
colnames(sp)[1]<-"Species"

#separate the genus and species column 
sp<- sp %>% tidyr::separate(Species, into = c("Genus","Species"), sep = "[\\s]")
#sp$Species3<- paste(sp$Species ,sp$species2)
#pull out first letter of genus and add a period 
sp$abbrev<- substr(sp$Genus, 1,1)
sp$abbrev<- paste(sp$abbrev,".", sep ="")
sp$abbrev_sp<-paste(sp$abbrev, sp$Species)

###!make character vector of abbreviated sp names 
sp_abbrev<- sp$abbrev_sp

#join full taxon name and abbreviated vectors into one complete list of summer diet species 
alldiet_list<-c(sp_full, sp_abbrev)

##################### find all regional ecosystems where any summer species is found #############  
#filter for summer species in top 5 spots 
alldiet_sp1 <- filter(redd, grepl(paste(alldiet_list, collapse="|"), redd$X1))
alldiet_sp2 <- filter(redd, grepl(paste(alldiet_list, collapse="|"), redd$X2))
alldiet_sp3 <- filter(redd, grepl(paste(alldiet_list, collapse="|"), redd$X3))
alldiet_sp4 <- filter(redd, grepl(paste(alldiet_list, collapse="|"), redd$X4))
alldiet_sp5 <- filter(redd, grepl(paste(alldiet_list, collapse="|"), redd$X5))


alldiet_majspecies_top5<- rbind(alldiet_sp1, alldiet_sp2, alldiet_sp3, alldiet_sp4, alldiet_sp5 )

#remove the duplicated reg ecos from multiple matching 
alldiet_majspecies_top5_unq<- alldiet_majspecies_top5[!duplicated(alldiet_majspecies_top5), ]
#n = 1682

####################################################
winter<- subset(all_dietsp, all_dietsp$Productive.season1 == "winter" | all_dietsp$Productive.season1 == "not_listed"|
                  all_dietsp$Productive.season2 == "winter" )
winter<- winter %>% tidyr::separate(Species, into = c("Genus","Species"), sep = "[\\s]")

##!make a character vector of the full taxon names 
wint_full<- winter$Species


######## making abbreviated version of full taxon name ######
#make a dataframe that is the full taxon name 
sp<-as.data.frame(all_dietsp$Species, stringsAsFactors = F)
colnames(sp)[1]<-"Species"

#separate the genus and species column 
sp<- winter %>% tidyr::separate(Species, into = c("Genus","Species"), sep = "[\\s]")
#sp$Species3<- paste(sp$Species ,sp$species2)
#pull out first letter of genus and add a period 
sp$abbrev<- substr(sp$Genus, 1,1)
sp$abbrev<- paste(sp$abbrev,".", sep ="")
sp$abbrev_sp<-paste(sp$abbrev, sp$Species)

###!make character vector of abbreviated sp names 
sp_abbrev<- sp$abbrev_sp

#join full taxon name and abbreviated vectors into one complete list of summer diet species 
wintdiet_list<-c(wint_full, sp_abbrev)

#subset out the typical winter diet species by genera
typical<- subset(winter, winter$Genus == "Banksia" |
                       winter$Genus == "Eucalyptus" |
                       winter$Genus == "Corymbia" |
                       winter$Genus == "Angophora" |
                       winter$Genus == "Ficus" |
                        winter$Genus == "Syncarpia"|
                       winter$Genus == "Melaleuca" )

## paste the names. back together
typical$Species2<- paste(typical$Genus, typical$Species, sep = " " )
#make a dataframe of the species names 
typicalwint_sp<- typical$Species2
#colnames(typicalwint_sp)[1]<-"Species"

#turn the genus into the first letter abbreviation
typical$abbrev<- substr(typical$Genus, 1,1)
typical$abbrev<- paste(typical$abbrev,".", sep ="")
typical$abbrev_sp<-paste(typical$abbrev,typical$Species)

#make character vector of abbreviation species 
conventional_wint_sp_abbrev<- typical$abbrev_sp

#join full taxon name and abbreviated vectors into one complete list of summer diet species 
conventional_wintdiet_list<-c(typicalwint_sp, conventional_wint_sp_abbrev)

#filter for summer species in top 5 spots 
typicalwintdiet_sp1 <- filter(redd, grepl(paste(conventional_wintdiet_list, collapse="|"), redd$X1))
typicalwintdiet_sp2 <- filter(redd, grepl(paste(conventional_wintdiet_list, collapse="|"), redd$X2))
typicalwintdiet_sp3 <- filter(redd, grepl(paste(conventional_wintdiet_list, collapse="|"), redd$X3))
typicalwintdiet_sp4 <- filter(redd, grepl(paste(conventional_wintdiet_list, collapse="|"), redd$X4))
typicalwintdiet_sp5 <- filter(redd, grepl(paste(conventional_wintdiet_list, collapse="|"), redd$X5))

typicalwintdiet_top5<- rbind(typicalwintdiet_sp1, typicalwintdiet_sp2, typicalwintdiet_sp3, typicalwintdiet_sp4, typicalwintdiet_sp5 )

#remove the duplicated reg ecos from multiple matching 
typicalwintdiet_top5_unq<- typicalwintdiet_top5[!duplicated(typicalwintdiet_top5), ]

################################################################
atypical<- subset(winter, winter$Genus != "Banksia" &
                        winter$Genus != "Eucalyptus" &
                        winter$Genus != "Corymbia" &
                        winter$Genus != "Angophora" &
                        winter$Genus != "Ficus" &
                        winter$Genus!= "Syncarpia" &
                        winter$Genus != "Melaleuca" )



## paste the names. back together
atypical$Species2<- paste(atypical$Genus, atypical$Species, sep = " " )
#make a dataframe of the species names 
atypicalwint_sp<-atypical$Species2
#colnames(typicalwint_sp)[1]<-"Species"

#turn the genus into the first letter abbreviation
atypical$abbrev<- substr(atypical$Genus, 1,1)
atypical$abbrev<- paste(atypical$abbrev,".", sep ="")
atypical$abbrev_sp<-paste(atypical$abbrev,atypical$Species)

#make character vector of abbreviation species 
atypical_wint_sp_abbrev<- atypical$abbrev_sp
atypical_wint_sp_abbrev<-atypical_wint_sp_abbrev[5:82]
#join full taxon name and abbreviated vectors into one complete list of summer diet species 
atypical_wintdiet_list<-c(atypicalwint_sp, atypical_wint_sp_abbrev)


atypicalwintdiet_sp1 <- filter(redd, grepl(paste(atypical_wintdiet_list, collapse="|"), redd$X1))
atypicalwintdiet_sp2 <- filter(redd, grepl(paste(atypical_wintdiet_list, collapse="|"), redd$X2))
atypicalwintdiet_sp3 <- filter(redd, grepl(paste(atypical_wintdiet_list, collapse="|"), redd$X3))
atypicalwintdiet_sp4 <- filter(redd, grepl(paste(atypical_wintdiet_list, collapse="|"), redd$X4))
atypicalwintdiet_sp5 <- filter(redd, grepl(paste(atypical_wintdiet_list, collapse="|"), redd$X5))


atypicalwintdiet_top5<- rbind(atypicalwintdiet_sp1, atypicalwintdiet_sp2, atypicalwintdiet_sp3, atypicalwintdiet_sp4, atypicalwintdiet_sp5)


#remove the duplicated reg ecos from multiple matching 
atypicalwintdiet_top5_unq<- atypicalwintdiet_top5[!duplicated(atypicalwintdiet_top5), ]

#######remove the marshy ecosystem thats not actually forested but contains a diet species 
atypicalwintdiet_top5_unq2<- subset( atypicalwintdiet_top5_unq, re_id != "11.1.2a")

##############################
############################################################
###########################################################################################

fruits<- subset(all_dietsp, Productive.season1 == "not_listed") 
fruits<- fruits%>% tidyr::separate(Species, into = c("Genus","Species"), sep = "[\\s]")

## paste the names. back together
fruits$Species2<-paste(fruits$Genus, fruits$Species , sep = " ")
#make a dataframe of the species names 
fruits_sp<-fruits$Species2
#colnames(typicalwint_sp)[1]<-"Species"

#turn the genus into the first letter abbreviation
fruits$abbrev<- substr(fruits$Genus, 1,1)
fruits$abbrev<- paste(fruits$abbrev,".", sep ="")
fruits$abbrev_sp<-paste(fruits$abbrev,fruits$Species)

#make character vector of abbreviation species 
fruits_abbrev<- fruits$abbrev_sp

#join full taxon name and abbreviated vectors into one complete list of summer diet species 
fruits_list<-c(fruits_sp, fruits_abbrev)


fruits_sp1 <- filter(redd, grepl(paste(fruits_list, collapse="|"), redd$X1))
fruits_sp2 <- filter(redd, grepl(paste(fruits_list, collapse="|"), redd$X2))
fruits_sp3 <- filter(redd, grepl(paste(fruits_list, collapse="|"), redd$X3))
fruits_sp4 <- filter(redd, grepl(paste(fruits_list, collapse="|"), redd$X4))
fruits_sp5 <- filter(redd, grepl(paste(fruits_list, collapse="|"), redd$X5))


fruits_top5<- rbind(fruits_sp1, fruits_sp2, fruits_sp3, fruits_sp4, fruits_sp5)

#remove the duplicated reg ecos from multiple matching 
fruits_top5_unq<- fruits_top5[!duplicated(fruits_top5), ]


