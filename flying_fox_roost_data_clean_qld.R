##########
# Cleaning raw data of flying fox roost counts
# Written for Queensland (QLD)  FFMP
# christina.faust@gmail.com
# first written Dec 12 2019
# last updated by C.F. Jul 28 2021
# Updated by Kelsee Baranowski July 5 2022 

library(data.table)
library(readxl)  # for reading multiple sheets
library(purrr) # for joining
library(scales)
library(stringr)
library(dplyr)
library(plyr)
library(sp)
library(tidyr)
library(ggplot2)
library(RColorBrewer)

#########
# Reading in Queensland data and cleaning
# QLD data available online 
# published quarterly as .csv tables
# all QLD are in .csv (one was a .xlsx format but a single sheet
# so I converted to .csv for ease of import)
# otherwise all cleaning is done below 
# https://www.data.qld.gov.au/dataset/flying-fox-monitoring-program

## importing raw data
qld_dir =  "/Users/ktb5143/Documents/Research/Flying_Fox_Monitoring_Program/ff_qld" #directory with qld roosts
setwd(qld_dir) 
ff_files_qld = list.files(pattern="*.csv$") # file list of all .csv files
all_csv_qld = lapply(ff_files_qld,read.csv) #reading each csv in as a dataframe in a list

## cleaning dataframes
# cleaning column names: the csv files have different names 
# and capitalizations, so this is getting them all in the same
# format

#str(all_csv_qld)

qld_old = c("Name.of.local.government",              
            "nameofcamp",                            
            "latitude",                              
            "longitude",                             
            "date.of.survey",                        
            "flying.fox.absent..true.false.",        
            "distribution.of.species.range",         
            "total.number.of.black.flying.fox",      
            "total.number.of.Grey.headed.flying.fox",
            "total.number.of.Little.red.flying.fox", 
            "total.number.of.spectacled.flying.fox", 
            "Total.number.of.all.species",           
            "DES.camp.identification",               
            "CSIRO.camp.identification",            
            "Name.of.camp",                          
            "EHP.camp.identification",               
            "Latitude",                             
            "Longitude",                             
            "Date.of.survey",                     
            "Flying.fox.absent..true.false.",
            "Flying.fox.absent..true...false.",       
            "Total.number.of.black.flying.fox",      
            "Total.number.of.Grey.headed.flying.fox",
            "Total.number.of.Little.red.flying.fox" ,
            "Total.number.of.Spectacled.flying.fox", 
            "Total.number.of.grey.headed.flying.fox",
            "Total.number.of.little.red.flying.fox" ,
            "Total.number.of.spectacled.flying.fox" ,
            "Name.of.Local.Government",
            "Roost.Name",  
            "Name.of.site",                         
            "Date",  
            "DES.roost.identification.number",
            "DES.site.identification",
            "X",
            "Time", 
            "X_id",   
            "Local.Government.Areas",
            "Name.of.Camp",
            "absent",    
            "Black.flying.fox",  
            "Grey.headed.flying.fox",
            "Little.Red.flying.fox",
            "Spectacled.flying.fox",
            "Flying.fox.Total",  
            "DES.camp.ID",    
            "CSIRO.camp.ID",
            "lga",
            "lat",
            "long",
            "bff",
            "ghff",
            "lrff",
            "sff",
            "fftotal",
            "c_id",
            "csiro_id")

qld_new = c("loc.gov",              
            "camp.name",                            
            "lat",                              
            "long",                             
            "date",                        
            "ff.absence",        
            "species.dist",         
            "bff.count",      
            "ghff.count",
            "lrff.count", 
            "sff.count", 
            "species.rich",           
            "des.camp.id",               
            "csiro.camp.id",            
            "camp.name",                          
            "ehp.camp.id",               
            "lat",                             
            "long",                             
            "date",                     
            "ff.absence", 
            "ff.absence",       
            "bff.count",      
            "ghff.count",
            "lrff.count" ,
            "sff.count", 
            "ghff.count",
            "lrff.count" ,
            "sff.count", 
            "loc.gov", 
            "camp.name", 
            "camp.name",  
            "date",
            "des.camp.id",
            "des.camp.id",  
            "X",
            "date",
            "X",
            "loc.gov",              
            "camp.name",
            "ff.absence",        
            "bff.count",      
            "ghff.count",
            "lrff.count", 
            "sff.count", 
            "species.rich",           
            "des.camp.id",               
            "csiro.camp.id",
            "loc.gov",   
            "lat",
            "long",
            "bff.count",      
            "ghff.count",
            "lrff.count", 
            "sff.count", 
            "species.rich",           
            "des.camp.id",               
            "csiro.camp.id")

col_simplify = function(x) { #function to rename columns
  setnames(x, old = qld_old, new = qld_new, skip_absent=TRUE)
}
all_csv_same = lapply(all_csv_qld, col_simplify) # changing col names to be consistent
all_ff_qld = rbindlist(all_csv_same, fill = TRUE) #converting to a single dataframe
colnames(all_ff_qld) # double checking column names
#all_ff_qld = all_ff_qld[,-'X'] #removing X column (all NAs)

#remove doubled date column and the ehs camp ID which I don't even know where that comes from
all_ff_qld[,c(13:16)]<- NULL

# dates are in different formats
unique(all_ff_qld$date)


all_ff_qld[all_ff_qld$date == '',] <- NA# two NA rows
all_ff_qld = all_ff_qld[!is.na(all_ff_qld$date),]

sum(is.na(all_ff_qld$date))


#for future, adding in extra date formats here is easier than CFs method 
library(lubridate)
dates_fix<- parse_date_time(x = all_ff_qld$date,
                orders = c("d-b-y", "d/M/Y H:S", "d/m/y H:S", "Y-M-d", "Y-m-d", "d/m/Y", "d/m/y", "m/d/y", "M/d/y"))

all_ff_qld$date = dates_fix


# some dates wrong century
all_ff_qld$date[order(all_ff_qld$date)]
ind_date = which(all_ff_qld$date < as.Date("2000-01-01"))
all_ff_qld$date[ind_date]= format(all_ff_qld$date[ind_date], "20%y-%m-%d")
unique(all_ff_qld$date)[order(unique(all_ff_qld$date))]

all_ff_qld = as.data.frame(all_ff_qld) #converting from data.table to data.frame for ease of notation

#fix missing spatial info
spmissing = all_ff_qld[is.na(all_ff_qld[,'long']),] #missing spatial info
head(spmissing)#only 1 row 
all_ff_qld[all_ff_qld$camp.name %in% spmissing$camp.name,] # lat/long in other rows
all_ff_qld[is.na(all_ff_qld[,'long']),'long'] <- 152.1266
all_ff_qld[is.na(all_ff_qld[,'lat']),'lat'] <- -27.554828

#The 2020 years have an extra ' before the - symbol
all_ff_qld$lat <- str_remove(all_ff_qld$lat, "^'+")

##### 
# check for duplicates by camp name and date
dupe = all_ff_qld[,c('camp.name','date')] # select columns to check duplicates
dupe_df = all_ff_qld[duplicated(dupe) | duplicated(dupe, fromLast=TRUE),]
row.ids = as.integer(row.names(dupe_df))
all_ff_qld = all_ff_qld[-row.ids,]

sum(is.na(all_ff_qld$bff.count))

codedir = '~/Desktop/Research/Flying_Fox_Monitoring_Program/outputs_clean_code/'

count_col = c('ghff.count', 'bff.count', 'lrff.count','sff.count')
ids = c("camp.name",'date')
names(dupe_df)
dupe_info = dupe_df[,c('camp.name','lat','long') ]
dupe_info = distinct(dupe_info)
dupe_mean = aggregate(dupe_df[count_col], by = dupe_df[ids],  mean)
ff_species = c('lrff.count', 'bff.count', 'ghff.count','sff.count')
dupe_df$ff.count = rowSums(dupe_df[,ff_species], na.rm = T)
dupe_zeros = aggregate(dupe_df['ff.count'], by = dupe_df[ids],  min)
dupe_max = aggregate(dupe_df[count_col], by = dupe_df[ids],  max)
dupe_combo = dupe_mean
for (i in 1: nrow(dupe_combo)){
	#i = 1
	dupe_combo[i,ff_species]= ifelse(dupe_zeros[i,'ff.count']==0,
									dupe_max[i, ff_species],
									dupe_mean[i, ff_species])					
	
}
# Peggy said if there are two counts, one of which is 0 - just take the one with FF in it, all others I averaged
dupe_add = merge(dupe_mean, dupe_info, all.x = T, by = 'camp.name')
ff_qld = rbind.fill(all_ff_qld, dupe_add)

##########
# New variables
ff_qld$state = "QLD"

# check for missing data in counts
sum(is.na(ff_qld[,ff_species]))

colnames(ff_qld)
ff_qld$ff.count = rowSums(ff_qld[,ff_species], na.rm = T)
ff_qld$ff.presence = ifelse(ff_qld$ff.count>0, 1, 0)
ff_qld$ghff.presence = ifelse(ff_qld$ghff.count>0, 1, 0)
ff_qld$bff.presence = ifelse(ff_qld$bff.count>0, 1, 0)
ff_qld$lrff.presence = ifelse(ff_qld$lrff.count>0, 1, 0)
ff_qld$sff.presence = ifelse(ff_qld$sff.count>0, 1, 0)
ff_qld$species.rich = rowSums(ff_qld[,c("ghff.presence",'bff.presence',
                                      'lrff.presence', 'sff.presence')], na.rm = TRUE)
range(ff_qld$date) #"2003-04-01" to  "2022-03-11"
sum(is.na(ff_qld$date))

########
# in consistencies in dataset
# different names 
ff_qld[ff_qld$camp.name=='Aucklalnd Inlet','camp.name'] <- 'Auckland Inlet'
ff_qld[ff_qld$camp.name=='Mount Isa Sunset memorial cemetery','camp.name'] <- 'Mount Isa Sunset Memorial Cemetery'
ff_qld[ff_qld$camp.name=='Gayndah, Brambah Ck','camp.name'] <- 'Gayndah, Barambah Ck'
ff_qld[ff_qld$camp.name=='The Gap, Riaweena St - West','camp.name'] <- 'The Gap, Riaweena St'

#aug 2016 - locations (particularly longitude are different)
ff_qld[ff_qld$camp.name=='Burleigh, Marymount College'& ff_qld$lat== -28.081933,'lat'] <- -28.0946
ff_qld[ff_qld$camp.name=='Burleigh, Marymount College'& ff_qld$long== 153.439014,'long'] <- 153.4292
ff_qld[ff_qld$camp.name=='Finch Hatton Gorge'& ff_qld$long== 148.492,'long'] <- 148.6347
ff_qld[ff_qld$camp.name=='Helidon State School'& ff_qld$long== 152.1266,'long'] <- 152.12664
ff_qld[ff_qld$camp.name=='Sandstone Point, Bestman Rd'& ff_qld$long== 153.411992,'long'] <- 153.1279
x = unique(ff_qld[ff_qld$camp.name=='Albany Creek, Kingfisher Street', 'lat'])
ff_qld[ff_qld$camp.name=='Albany Creek, Kingfisher Street' & ff_qld$lat == x[2],'lat'] <- x[1]

###2020 
ff_qld[ff_qld$camp.name =='Nerangba Settlement Dr' & ff_qld$long == 3152.93417,'long'] <- 152.93417


#Kelsee's fix to 750K observation of bff, which is crazy bc thats more than the estimated total pop 
ff_qld[!is.na(ff_qld$bff.count) & ff_qld$bff.count==750000,]$bff.count<- 75000

#Christinas code that doesn't work for me 
# ff_qld[ff_qld$bff.count == 750000,'bff.count']<-75000 #likely an extra 0
# sum(ff_qld$bff.count == 750000)
#ff_qld <- ff_qld[ff_qld$bff.count == 750000,] 

#make sure all weird apostraphes are removed from lat category 
ff_qld$lat <- str_remove(ff_qld$lat, "^'+")

missing<- ff_qld[is.na(ff_qld$bff.count),]  #all NAs are 0 for bff count 

ff_qld$bff.count[is.na(ff_qld$bff.count)] <- 0 # put in 0s for NAs


#specify date format and add year column
ff_qld$date <- as.Date(ff_qld$date, format = '%Y-%m-%d')
ff_qld$year <- as.factor(format(ff_qld$date,'%Y'))

table(ff_qld$year)

#add in column for season 
getseason <- function(dates) { # function for AUS seasons
  SS = as.Date("2013-12-01", format = "%Y-%m-%d") # Winter Solstice for US
  FE = as.Date("2013-3-01",  format = "%Y-%m-%d") # Spring Equinox for US
  WS = as.Date("2013-6-01",  format = "%Y-%m-%d") # Summer Solstice for US
  SE = as.Date("2013-9-01",  format = "%Y-%m-%d") # Fall Equinox for US
  
  # convert dates from any year to 2013 dates
  dates = ff_qld$date
  d = as.Date(strftime(dates, format="2013-%m-%d"))
  ifelse (d >= SS | d < FE, "summer",
          ifelse (d >= SE & d < SS, "spring",
                  ifelse (d >= WS & d < SE, "winter", "autumn")))
}
ff_qld$season = getseason(ff_qld$date)




