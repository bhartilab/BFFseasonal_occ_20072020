setwd(".")
library(data.table)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(stringi)
library(scales)


##################### Regional Ecosystems dominant with upper diet species  ####################
#read in the tables of reg ecos maps where winter vegetation is present in new version 12 maps released March 2021
#maps received through email contact Tim Ryan --> Dan Richter at QLD herbarium
box_dir =  (".")
setwd(box_dir)
ff_upper_diet = list.files(pattern="*.csv$") # file list of all .csv files
winter_upper = lapply(ff_upper_diet, read.csv, skipNul = TRUE, stringsAsFactors = FALSE ) #reading each csv in as a dataframe in a list


########## chunk to add in file name s
# read file name
filenames <- ff_upper_diet %>%
  basename() %>%
  as.list()

# combine file content list and file name list
lists <- mapply(c, winter_upper, filenames, SIMPLIFY = FALSE)

# unlist all lists and change column name
result <- rbindlist(lists, fill = T)
# change column name
#result[1]<- NULL
colnames(result)[24] <- "File.Path"

#extract the years but do it really inneficiently 
new<- as.data.frame(str_split_fixed(result$File.Path, "_xy_20kbuff_REM2000_winterGBIFupper8m_clip.csv",2))
new2<- as.data.frame(str_split_fixed(new$V1, "camp_",2))
new2[1]<- NULL
colnames(new2)<- "camp"


veg<- cbind(result, new2)
veg<-veg[,-c(1,24)]


upper_RE1<- subset(veg, veg$RE1 == veg$re_id)
upper_RE2<- subset(veg, veg$RE2 == veg$re_id)
upper_RE3<- subset(veg, veg$RE3 == veg$re_id)
upper_RE4<- subset(veg, veg$RE4 == veg$re_id)
upper_RE5<- subset(veg, veg$RE5 == veg$re_id)

upper_RE1$scaled_Ha = upper_RE1$Shape_Area*((upper_RE1$PC1*0.01)*0.0001)  
upper_RE2$scaled_Ha = upper_RE2$Shape_Area*((upper_RE2$PC2*0.01)*0.0001)  
upper_RE3$scaled_Ha = upper_RE3$Shape_Area*((upper_RE3$PC3*0.01)*0.0001)  
upper_RE4$scaled_Ha = upper_RE4$Shape_Area*((upper_RE4$PC4*0.01)*0.0001)  
upper_RE5$scaled_Ha = upper_RE5$Shape_Area*((upper_RE5$PC5*0.01)*0.0001)  


upper_allpatches<- rbind(upper_RE1,upper_RE2, upper_RE3,upper_RE4,upper_RE5)

upper_allpatches_sums2<- aggregate(upper_allpatches$scaled_Ha, by=list(camp_name = upper_allpatches$camp, xcord=upper_allpatches$xcord, ycord = upper_allpatches$ycord ), FUN=sum)

upper_allpatches_sums3<- upper_allpatches_sums2 %>%
  group_by(camp_name) %>%
  dplyr::summarise(scaled_wint_sum_ha = sum(x),
                   num_patches_RE2000 = n())

upper_allpatches_sums3$prop_20kbuff_RE2000 = upper_allpatches_sums3$scaled_wint_sum_ha/ 125663.7

#PLOT A HISTOGRAM
ggplot(upper_allpatches_sums3, aes( x = prop_20kbuff_RE2000)) +
  geom_histogram(bins = 100) +
  labs(y= "Number of roosts", x ='Scaled proportion of winter diet food in 20km buffer') +
  #scale_fill_manual(values = c("darkblue", "cyan4"), labels = c("Possible", "upper")) +
  theme_classic(base_size = 16) +
  scale_y_continuous(labels = comma) +
  #facet_wrap(~diet2) +
  theme(legend.position  = "bottom")

upper_allpatches_sums3[2]<- NULL

write.csv(upper_allpatches_sums3, "camps_prop_winter_RE2000_GBIFupperwint8m_veg.csv", row.names = F)
