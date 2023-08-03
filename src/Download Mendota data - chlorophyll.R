# Script to download the Mendota chlorophyll data, based on the R example script on
# https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-ntl.38.28

# Further down I added my own code to select the relevant parts of the Mendota
# data

# IMPORTANT: First run script "Download Mendota data - phytoplankton.R"!!!
#            This script calculates total and grooup-specific phytoplankton

Sys.setenv(TZ = "UTC")

# Package ID: knb-lter-ntl.38.28 Cataloging System:https://pasta.edirepository.org.
# Data set title: North Temperate Lakes LTER: Chlorophyll - Madison Lakes Area 1995 - current.
# Data set creator:  John Magnuson - University of Wisconsin 
# Data set creator:  Stephen Carpenter - University of Wisconsin 
# Data set creator:  Emily Stanley - University of Wisconsin 
# Metadata Provider:  NTL Information Manager - University of Wisconsin 
# Contact:    -  NTL LTER  - ntl.infomgr@gmail.com
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/38/28/66796c3bc77617e7cc95c4b09d4995c5" 
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
                 "depth_range_m",     
                 "rep",     
                 "tri_chl_spec",     
                 "mono_chl_spec",     
                 "phaeo_spec",     
                 "uncorrect_chl_fluor",     
                 "correct_chl_fluor",     
                 "phaeo_fluor",     
                 "flag_spec",     
                 "flag_fluor"    ), check.names=TRUE)

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
if (class(dt1$depth_range_m)!="factor") dt1$depth_range_m<- as.factor(dt1$depth_range_m)
if (class(dt1$rep)!="factor") dt1$rep<- as.factor(dt1$rep)
if (class(dt1$tri_chl_spec)=="factor") dt1$tri_chl_spec <-as.numeric(levels(dt1$tri_chl_spec))[as.integer(dt1$tri_chl_spec) ]               
if (class(dt1$tri_chl_spec)=="character") dt1$tri_chl_spec <-as.numeric(dt1$tri_chl_spec)
if (class(dt1$mono_chl_spec)=="factor") dt1$mono_chl_spec <-as.numeric(levels(dt1$mono_chl_spec))[as.integer(dt1$mono_chl_spec) ]               
if (class(dt1$mono_chl_spec)=="character") dt1$mono_chl_spec <-as.numeric(dt1$mono_chl_spec)
if (class(dt1$phaeo_spec)=="factor") dt1$phaeo_spec <-as.numeric(levels(dt1$phaeo_spec))[as.integer(dt1$phaeo_spec) ]               
if (class(dt1$phaeo_spec)=="character") dt1$phaeo_spec <-as.numeric(dt1$phaeo_spec)
if (class(dt1$uncorrect_chl_fluor)=="factor") dt1$uncorrect_chl_fluor <-as.numeric(levels(dt1$uncorrect_chl_fluor))[as.integer(dt1$uncorrect_chl_fluor) ]               
if (class(dt1$uncorrect_chl_fluor)=="character") dt1$uncorrect_chl_fluor <-as.numeric(dt1$uncorrect_chl_fluor)
if (class(dt1$correct_chl_fluor)=="factor") dt1$correct_chl_fluor <-as.numeric(levels(dt1$correct_chl_fluor))[as.integer(dt1$correct_chl_fluor) ]               
if (class(dt1$correct_chl_fluor)=="character") dt1$correct_chl_fluor <-as.numeric(dt1$correct_chl_fluor)
if (class(dt1$phaeo_fluor)=="factor") dt1$phaeo_fluor <-as.numeric(levels(dt1$phaeo_fluor))[as.integer(dt1$phaeo_fluor) ]               
if (class(dt1$phaeo_fluor)=="character") dt1$phaeo_fluor <-as.numeric(dt1$phaeo_fluor)
if (class(dt1$flag_spec)!="factor") dt1$flag_spec<- as.factor(dt1$flag_spec)
if (class(dt1$flag_fluor)!="factor") dt1$flag_fluor<- as.factor(dt1$flag_fluor)

##### End of LTER code, start of my code -----
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(data.table)
library(ggplot2)

folder_out = "../trainingData"
if(!dir.exists(folder_out)){
  dir.create(folder_out, recursive = T)
}

### Settings
remove_suspect_outOfRange_data = T # Quality control
round_decimals = 2L # Rounding of total chl concentration in ug/l 
make_plots = F

setDT(dt1)

df_mendota = dt1[lakeid == "ME",
                 .(datetime = sampledate,
                   depth_range_m,
                   rep,
                   tri_chl_spec, # ug/l
                   mono_chl_spec, # ug/l
                   correct_chl_fluor, # ug/l
                   flag_spec,
                   flag_fluor)] 

fwrite(df_mendota, file.path(folder_out, "MendotaData_chl_raw.csv"))
# df_mendota = fread(file.path(folder_out, "MendotaData_chl_raw.csv"))

df_mendota[, datetime := as.POSIXct(datetime)]

### Check all measurement if they have a problematic flag, set to NA if needed
# Most flags either point to specifics how data were processed, rather than to
# clear errors. However, in some cases, flags indicate unreliable data, which
# can be removed here.

if(remove_suspect_outOfRange_data){
  df_mendota[!is.na(flag_spec) & flag_spec == "K", `:=`(tri_chl_spec = NA,
                                                        mono_chl_spec = NA)]
  df_mendota[!is.na(flag_fluor) & flag_fluor == "E", correct_chl_fluor := NA]
}

flagnames = names(df_mendota)[grepl("^flag", names(df_mendota))]
df_mendota = df_mendota[, -c(..flagnames)]

# Plots don't show odd values for correct_chl_fluor, but for the spectometry,
# there are some values that need to be corrected

df_mendota[tri_chl_spec == 0 & mono_chl_spec == 0 & correct_chl_fluor > 0,
           `:=`(tri_chl_spec = NA,
                mono_chl_spec = NA)]
df_mendota[mono_chl_spec < 0, mono_chl_spec := 0]

### Handle depth ranges
df_mendota[depth_range_m == "0-2", depth_range_m := "1"]
df_mendota[depth_range_m == "0-8", depth_range_m := "4"]
df_mendota[, depth_range_m := as.numeric(depth_range_m)]

### There are replicates, so average per date & depth
# Convert all columns to numeric
num_cols = names(df_mendota)[-c(1, 2, 3)]
df_mendota[, (num_cols) := lapply(.SD, as.numeric), .SDcols = num_cols]

# Average (ignore NAs) over datetime and depth
df_mendota = df_mendota[, lapply(.SD, function(x) mean(x, na.rm = T)),
                        by = .(datetime, depth_range_m),
                        .SDcols = num_cols]

### Calculate single Chl value per sample
# All three methods seem to follow the same trend and are correlated somewhat
# close to a 1:1 line. Robert asked the data curators and they said that
# fluorescence is more or less the standard and should be trusted. Because
# spectometry data covers a larger period and there seem to be mostly good
# correlations, the spectometry data will be lineary bias-corrected and used
# in stead of fluorescence data if fluorescence is not available.
# R2 with tri_chl_spec is higher, so preferentially use this to correct the data

lm_tri = lm(correct_chl_fluor ~ tri_chl_spec, data = df_mendota)
lm_mono = lm(correct_chl_fluor ~ mono_chl_spec, data = df_mendota)

if(make_plots){
  ### Look at similarities of different methods
  # Surface values
  ggplot(df_mendota[depth_range_m %in% c("0-2", "0-8", "0", "2", "4", "6")]) +
    geom_point(aes(datetime, tri_chl_spec, colour = "Tri_chl_spec")) +
    geom_point(aes(datetime, mono_chl_spec, colour = "Mono_chl_spec")) +
    geom_point(aes(datetime, correct_chl_fluor, colour = "Chl_fluor")) +
    scale_colour_manual(name = "Legend",
                        values = c("Tri_chl_spec" = "blue",
                                   "Mono_chl_spec" = "red",
                                   "Chl_fluor" = "green"))
  
  pairs(df_mendota[, .(tri_chl_spec, mono_chl_spec, correct_chl_fluor)])
  
  # summary(lm(tri_chl_spec ~ mono_chl_spec, data = df_mendota))
  # summary(lm(correct_chl_fluor ~ tri_chl_spec, data = df_mendota))
  # summary(lm(correct_chl_fluor ~ mono_chl_spec, data = df_mendota))
}

df_mendota[, tot_chl := correct_chl_fluor]
df_mendota[is.na(tot_chl), tot_chl := predict(lm_tri, list(tri_chl_spec = tri_chl_spec))]
df_mendota[is.na(tot_chl), tot_chl := predict(lm_mono, list(mono_chl_spec = mono_chl_spec))]

# # Old method, before bias-correction:
# df_mendota[, tot_chl := rowMeans(df_mendota[, .(mono_chl_spec, tri_chl_spec, correct_chl_fluor)],
#                                  na.rm = T)]

### Rounding and sorting
df_mendota[, tot_chl := round(tot_chl, digits = round_decimals)]

df_mendota = df_mendota[, .(datetime,
                            depth = depth_range_m,
                            tot_chl)]
df_mendota = df_mendota[complete.cases(df_mendota)]
setorder(df_mendota, datetime, depth)

### Write file

fwrite(df_mendota, file.path(folder_out, "MendotaData_chl_total_checked.csv"))

### Optional: plotting
# ggplot(df_mendota) +
#   geom_point(aes(datetime, tot_chl))

##### Calculating group-specific chlorophyll -----
# All phytoplankton samples are from the surface layer (0-2 or 0.8). Assume
# there is no variation in composition over depth

df_phyto = fread(file.path(folder_out, "MendotaData_phyto_checked.csv"))

df_phyto_wide = dcast(df_phyto, datetime + depth_range ~ division, value.var = "biomass_conc",
                      fun.aggregate = mean)
df_phyto_wide[is.na(df_phyto_wide)] = 0

# Use "division" column from the Mendota data file and simplify into 3 groups
df_phyto_wide = df_phyto_wide[, .(datetime,
                                  depth_range,
                                  diatoms = Bacillariophyta,
                                  cyanobacteria = Cyanophyta,
                                  others = Chlorophyta + Chrysophyta + Cryptophyta +
                                    Euglenophyta + Haptophyta +
                                    Miscellaneous + Pyrrhophyta + Xanthophyta)]

# Calculate percentages
df_phyto_wide[, `:=`(diatoms_frac = diatoms / rowSums(.SD),
                     cyano_frac = cyanobacteria / rowSums(.SD),
                     others_frac = others / rowSums(.SD)),
              .SDcols = c("diatoms", "cyanobacteria", "others")]

if(make_plots){
  library(cowplot)
  
  p1 = ggplot(df_mendota) +
    geom_line(aes(x = datetime, y = tot_chl)) +
    theme_light()
  
  df_plot = melt(df_phyto_wide[, .(datetime, diatoms_frac, cyano_frac, others_frac)],
                 id.vars = "datetime")
  
  p2 = ggplot(df_plot) +
    geom_area(aes(datetime, value, fill = variable), position = "fill")
  
  plot_grid(p1, p2, nrow = 2, ncol = 1, align = "v", axis = "lr",
            rel_heights = c(0.3,0.7))
}

# Ensure that datetimes are unique and merge
df_mendota_wide = dcast(df_mendota, datetime ~ depth, value.var = "tot_chl")
setnames(df_mendota_wide,
         old = names(df_mendota_wide)[-1],
         new = paste0("tot_chl_", names(df_mendota_wide)[-1]))

df_all = merge(df_mendota_wide, df_phyto_wide,
               by = "datetime", all = T)

# Calculate group-specific chlorophyll for each depth
chl_cols = names(df_mendota_wide)[-1]

for(i in c("diatoms", "cyano", "others")){
  df = df_all[, .(datetime)]
  frac_name = paste0(i, "_frac")
  
  for(j in chl_cols){
    df[, (j) := df_all[[j]] * df_all[[frac_name]]]
  }
  
  # Wide to long
  df = melt(df, id.vars = "datetime", measure = patterns("tot_chl_"))
  df[, variable := as.numeric(gsub("tot_chl_", "", variable))]
  setorder(df, datetime, variable)
  
  # Rounding and only keeping dates with data
  df[, value := round(value, digits = round_decimals)]
  df = df[complete.cases(df)]
  
  # Set correct column names and write
  setnames(df, c("datetime", "depth", paste0("chl_", i)))
  fwrite(df, paste0(folder_out, "/MendotaData_chl_", i, "_checked.csv"))
}
