# Script to download the Mendota phytoplankton data, based on the R example script on
# https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-ntl.88.31

# Further down I added my own code to select the relevant parts of the Mendota
# data

Sys.setenv(TZ = "UTC")

# Package ID: knb-lter-ntl.88.31 Cataloging System:https://pasta.edirepository.org.
# Data set title: North Temperate Lakes LTER: Phytoplankton - Madison Lakes Area 1995 - current.
# Data set creator:  John Magnuson - University of Wisconsin 
# Data set creator:  Stephen Carpenter - University of Wisconsin 
# Data set creator:  Emily Stanley - University of Wisconsin 
# Contact:    -  NTL LTER  - ntl.infomgr@gmail.com
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/88/31/f2de15b2fff6ae962a04c150c0a1c510" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "lakeid",     
                 "year4",     
                 "sampledate",     
                 "sta",     
                 "depth_range",     
                 "division",     
                 "taxa_name",     
                 "gald",     
                 "cells_per_nu",     
                 "nu_per_ml",     
                 "cells_per_ml",     
                 "biovolume_conc",     
                 "biomass_conc",     
                 "relative_total_biovolume",     
                 "genus"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$lakeid)!="factor") dt1$lakeid<- as.factor(dt1$lakeid)
if (class(dt1$year4)=="factor") dt1$year4 <-as.numeric(levels(dt1$year4))[as.integer(dt1$year4) ]               
if (class(dt1$year4)=="character") dt1$year4 <-as.numeric(dt1$year4)                                   
# attempting to convert dt1$sampledate dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1sampledate<-as.Date(dt1$sampledate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1sampledate) == length(tmp1sampledate[!is.na(tmp1sampledate)])){dt1$sampledate <- tmp1sampledate } else {print("Date conversion failed for dt1$sampledate. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1sampledate) 
if (class(dt1$sta)!="factor") dt1$sta<- as.factor(dt1$sta)
if (class(dt1$depth_range)!="factor") dt1$depth_range<- as.factor(dt1$depth_range)
if (class(dt1$division)!="factor") dt1$division<- as.factor(dt1$division)
if (class(dt1$taxa_name)!="factor") dt1$taxa_name<- as.factor(dt1$taxa_name)
if (class(dt1$gald)=="factor") dt1$gald <-as.numeric(levels(dt1$gald))[as.integer(dt1$gald) ]               
if (class(dt1$gald)=="character") dt1$gald <-as.numeric(dt1$gald)
if (class(dt1$cells_per_nu)=="factor") dt1$cells_per_nu <-as.numeric(levels(dt1$cells_per_nu))[as.integer(dt1$cells_per_nu) ]               
if (class(dt1$cells_per_nu)=="character") dt1$cells_per_nu <-as.numeric(dt1$cells_per_nu)
if (class(dt1$nu_per_ml)=="factor") dt1$nu_per_ml <-as.numeric(levels(dt1$nu_per_ml))[as.integer(dt1$nu_per_ml) ]               
if (class(dt1$nu_per_ml)=="character") dt1$nu_per_ml <-as.numeric(dt1$nu_per_ml)
if (class(dt1$cells_per_ml)=="factor") dt1$cells_per_ml <-as.numeric(levels(dt1$cells_per_ml))[as.integer(dt1$cells_per_ml) ]               
if (class(dt1$cells_per_ml)=="character") dt1$cells_per_ml <-as.numeric(dt1$cells_per_ml)
if (class(dt1$biovolume_conc)=="factor") dt1$biovolume_conc <-as.numeric(levels(dt1$biovolume_conc))[as.integer(dt1$biovolume_conc) ]               
if (class(dt1$biovolume_conc)=="character") dt1$biovolume_conc <-as.numeric(dt1$biovolume_conc)
if (class(dt1$biomass_conc)=="factor") dt1$biomass_conc <-as.numeric(levels(dt1$biomass_conc))[as.integer(dt1$biomass_conc) ]               
if (class(dt1$biomass_conc)=="character") dt1$biomass_conc <-as.numeric(dt1$biomass_conc)
if (class(dt1$relative_total_biovolume)=="factor") dt1$relative_total_biovolume <-as.numeric(levels(dt1$relative_total_biovolume))[as.integer(dt1$relative_total_biovolume) ]               
if (class(dt1$relative_total_biovolume)=="character") dt1$relative_total_biovolume <-as.numeric(dt1$relative_total_biovolume)
if (class(dt1$genus)!="factor") dt1$genus<- as.factor(dt1$genus)

##### End of LTER code, start of my code -----
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(data.table)

folder_out = "../trainingData"
if(!dir.exists(folder_out)){
  dir.create(folder_out, recursive = T)
}

### Settings
remove_suspect_outOfRange_data = T # Quality control

setDT(dt1)

df_mendota = dt1[lakeid == "ME",
                 .(datetime = sampledate,
                   depth_range,
                   division,
                   taxa_name,
                   biomass_conc, # mg/l
                   genus)] 

fwrite(df_mendota, file.path(folder_out, "MendotaData_phyto_raw.csv"))
# df_mendota = fread(file.path(folder_out, "MendotaData_phyto_raw.csv"))

df_mendota[, datetime := as.POSIXct(datetime)]

##### Clean data and quality checks -----
### Check all measurement if they have a problematic flag, set to NA if needed
# No flags, so a quick manual check based on plots

# library(ggplot2)
# col_to_plot = "biomass_conc"
# ggplot(df_mendota) +
#   geom_point(aes(datetime, .data[[col_to_plot]]))

# The only measurement that's clearly dubious is a measurement of more than 80 mg/l
# Coelastrum on 2011-07-25, and no Coelastrum was found the week before or after

if(remove_suspect_outOfRange_data){
  df_mendota[datetime == as.POSIXct("2011-07-25") & biomass_conc > 80, biomass_conc := NA]
}

# Convert biomass column to numeric
df_mendota[, biomass_conc := as.numeric(biomass_conc)]

### Remove columns with all NA
cols_to_keep = colSums(is.na(df_mendota)) < df_mendota[, .N]

df_mendota = df_mendota[, ..cols_to_keep]

##### Write data -----
fwrite(df_mendota, file.path(folder_out, "MendotaData_phyto_checked.csv"))

##### Optional: plotting -----
# library(ggplot2)
# col_to_plot = "drp"
# ggplot(df_mendota) +
#   geom_point(aes(datetime, .data[[col_to_plot]]))
