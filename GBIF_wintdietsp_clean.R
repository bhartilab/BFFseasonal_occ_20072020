setwd(".")

# libraries
library(plyr) #manipulating dfs
library(dplyr) #manipulating dfs
library(tidyverse)
library(stringr)


## importing raw data
files = list.files(pattern=".csv") # file list of all .csv files

#loop over reading tables 
sp <- lapply(files, read.csv,sep ="\t", header =T)

alld<- do.call("rbind", sp)

#clean out heavy useless columns 
alldiet<- alld[,c(2,4:14,22:33,36:45,48:50)]

#remove observations with no longitude
locs <- alldiet[!is.na(alldiet$decimalLongitude),]

#split lat/long to count number of decimal places of spatial resolution 
locs$longdd<-as.numeric(locs$decimalLongitude)
locs<- locs %>%
  separate(longdd, c("long", "dd"), sep = "\\.")

#count the numbers in the dec.deg. column I split 
locs$longddcnt<- str_count(locs$dd, "[0-9]")

#remove observations with less than 2 decimal degress of resolution in longitude column 
locs_1<- subset(locs, longddcnt > 2)
locs_1<- locs_1[!is.na(locs_1$dd),]
range(locs_1$longddcnt) #3-7


#count the number of significant digits in the reported longitude values  
locs_1$latdd<-as.numeric(locs_1$decimalLatitude)
locs_1<- locs_1 %>%
  separate(latdd, c("lat", "latdd"), sep = "\\.")
locs_1$latddcnt<- str_count(locs_1$latdd, "[0-9]")

#take out the NAs that got put in 
locs_2<- locs_1[!is.na(locs_1$latdd),]
range(locs_2$latddcnt) #1-7
locs_2<- subset(locs_2, latddcnt > 2)


##clean out preserved specimens
obs<- subset(locs_2, locs_2$basisOfRecord == "HUMAN_OBSERVATION" | 
               locs_2$basisOfRecord == "OCCURRENCE" |
               locs_2$basisOfRecord == "OBSERVATION" |
               locs_2$basisOfRecord == "LIVING_SPECIMEN" )

########################## trim observations to be within 100km of roosts ##################
library(rgdal)
library(raster)
library(rgeos)


buffs<-readOGR(dsn=".", layer="all_qld_ff_roosts_100kmbuff_diss") 

#make spatial points dataframe
xy <- obs[,c(14,13)]

obs_spdf <- SpatialPointsDataFrame(coords = xy, data = obs,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

obs_spdf_albs <- spTransform(obs_spdf,
                              crs(buffs))
crs(obs_spdf_albs)

#clip the spatial GBIF observations with the 100km dissolved buffer of roosts 
GBIF_in_100kmbuffs<-crop(obs_spdf_albs, buffs)


#write it back out to a a normal dataframe 
GBIF_in_100kmbuffs2<- as.data.frame(GBIF_in_100kmbuffs)


#aggregate by species and decimal degree loc 
GBIF_100kbuff_sums<- GBIF_in_100kmbuffs2 %>%
  group_by( datasetKey, genus, species,scientificName, decimalLongitude, decimalLatitude, eventDate, year, collectionCode    ) %>%
  dplyr::summarise(num_obs= n())

#remove obs with no species recorded
GBIF_100kbuff_sums_cl<- subset(GBIF_100kbuff_sums, species != "")
GBIF_100kbuff_sums_cl$sp_fact<- as.factor(GBIF_100kbuff_sums_cl$species)



#read in diet list from else where in the computer
dir = '/Users/ktb5143/Documents/Research/BFF_roostoccupancy_20220510/data_postdef/RE_selection/'
diet_list = read.csv(file.path(dir,"master_dietlist_simple_20230911.csv"), header = TRUE)
diet_list$sp_fact<-as.factor(diet_list$Species)


#merege to see how many are missing or gapping info
GBIF100k_dietlist<- merge(GBIF_100kbuff_sums_cl, diet_list, by = "sp_fact", all.x = T)

####see whats missing, theres extra species in the GBIF data (like limes) that should be cleaned out 
missing<-GBIF100k_dietlist[is.na(GBIF100k_dietlist$Productive.season1),]
unique(missing$species)

# [1] "Acmena smithii f."          "Acmena smithii race"        "Citrus grandis"            
# [4] "Citrus hystrix"             "Citrus taitensis"           "Citrus trifoliata"         
# [7] "Cudrania cochinchinensis"   "Elaeocarpus angustifolius"  "Eucalyptus sideroxylon"    
# [10] "Eucalyptus tetrodonta"      "Heptapleurum actinophyllum" "Nothocissus hypoglauca"    
# [13] "Passiflora foetida"         "Prunus dulcis"              "Psidium cattleianum"       
# [16] "Psidium guineense" 

##read in manually fixed table associating these GBIF names with the 
dir2 = '/Users/ktb5143/Documents/Research/BFF_roostoccupancy_20220510/data_postdef/'
fix_list = read.csv(file.path(dir2,"missing_Sp_fixtable.csv"), header = TRUE)
fix_list$sp_fact<-as.factor(fix_list$Other_name)
fix_list2<- fix_list[!is.na(fix_list$Productive.season1),]
unique(fix_list2$Species)

#only 9 species left that should be filleed in, then we can filter out incidental species after by who has no season listed

#manually filling in those that need season data 
GBIF100k_dietlist$Productive.season1[GBIF100k_dietlist$species == 'Acmena smithii f.'] <- 'not_listed'
GBIF100k_dietlist$Productive.season1[GBIF100k_dietlist$species == 'Acmena smithii race'] <- 'not_listed'
GBIF100k_dietlist$Productive.season1[GBIF100k_dietlist$species == 'Cudrania cochinchinensis'] <- 'not_listed'
GBIF100k_dietlist$Productive.season1[GBIF100k_dietlist$species == 'Eucalyptus pilularis'] <- 'winter'
GBIF100k_dietlist$Productive.season1[GBIF100k_dietlist$species == 'Eucalyptus sideroxylon'] <- 'winter'
GBIF100k_dietlist$Productive.season1[GBIF100k_dietlist$species == 'Heptapleurum actinophyllum'] <- 'not_listed'
GBIF100k_dietlist$Productive.season1[GBIF100k_dietlist$species == 'Nothocissus hypoglauca'] <- 'not_listed'
GBIF100k_dietlist$Productive.season1[GBIF100k_dietlist$species == 'Psidium cattleianum'] <- 'not_listed'
GBIF100k_dietlist$Productive.season1[GBIF100k_dietlist$species == 'Rhaphiolepis loquata'] <- 'not_listed'


GBIF100k_dietlist$Productive.season2[GBIF100k_dietlist$species == 'Eucalyptus pilularis'] <- 'autumn'
GBIF100k_dietlist$Productive.season2[GBIF100k_dietlist$species == 'Eucalyptus sideroxylon'] <- 'spring'

#now check the missing data so I can remove them
missing<-GBIF100k_dietlist[is.na(GBIF100k_dietlist$Productive.season1),]
unique(missing$species)

### clean it up 
GBIF100k_dietlist_cl<-GBIF100k_dietlist[!is.na(GBIF100k_dietlist$Productive.season1),]
GBIF100k_dietlist_cl<- GBIF100k_dietlist_cl[,c(2:10,12:14)]


####
GBIF100k_dietlist_cl<- GBIF100k_dietlist_cl %>%
  group_by(species) %>%
  dplyr::mutate(num_species_obs= n())

#pick out winter
winter<- subset(GBIF100k_dietlist_cl, GBIF100k_dietlist_cl$Productive.season1 == "winter"|
                  GBIF100k_dietlist_cl$Productive.season2 == "winter")

#identify conventional vs unconventional winter species
unique(winter$genus)
#use genus column to make that 
winter$conventional<- ifelse(winter$genus == "Eucalyptus" |
                              winter$genus == "Corymbia" |
                              winter$genus == "Angophora" |
                              winter$genus == "Ficus" |
                              winter$genus == "Banksia" |
                              winter$genus == "Callistemon" |
                              winter$genus == "Melaleuca" |
                              winter$genus == "Syncarpia", "conventional", "unconventional" )

####################
#take out missing years 
GBIF100k_dietlist_final<-subset(GBIF100k_dietlist_cl, year != "")
winter_final<-subset(winter, year != "")

write.csv(GBIF100k_dietlist_final, "GBIF100k_alldietlist_final.csv", row.names = F)
write.csv(winter_final, "GBIF100k_winterdietlist_final.csv", row.names = F)
