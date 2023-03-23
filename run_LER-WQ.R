setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load LakeEnsemblR
library(LakeEnsemblR)
library(LakeEnsemblR.WQ)
library(ggplot2)

# Set config file
config_file <- "LakeEnsemblR.yaml"
model <- c("GLM", "GOTM", "Simstrat")
# 1. Example - creates directories with all model setup
LakeEnsemblR::export_config(config_file = config_file, model = model,
                            folder = ".")
# 2. Run ensemble lake models
run_ensemble(config_file = config_file,
             model = model,
             return_list = FALSE, parallel = FALSE)


# path of the output netcdf file
ncdf <- "output/ensemble_output.nc"
# plot heatmap
p1 <- plot_heatmap(ncdf)

p1 <- p1 +
  theme_classic(base_size = 24) + 
  scale_colour_gradientn(limits = c(0, 35),
                         colours = rev(RColorBrewer::brewer.pal(11, "Spectral")))+
  xlab('') + ylab('Depth (m)')+  labs(col='Temp. (degC)');p1
# Save as a png file
ggsave("output/ensemble_heatmap.png", p1,  dpi = 300,width = 700,height = 250, units = "mm")

# water quality
visualise_dictionary()

create_input_tables(folder = ".", config_file = 'LakeEnsemblR_WQ.yaml', folder_out = 'WQinput', 
                    input = 'all',
                    models_coupled = c("GLM-AED2", "Simstrat-AED2", "GOTM-Selmaprotbas",
                                       'GOTM-WET'))
#("GLM-AED2", "GOTM-Selmaprotbas", "GOTM-WET", "Simstrat-AED2", "MyLake", "PCLake")

LakeEnsemblR.WQ::export_config(config_file = 'LakeEnsemblR_WQ.yaml', 
                               folder = ".", 
                               verbose = FALSE,
                               convert_from_lakeensemblr = TRUE,
                               ler_config_file = "LakeEnsemblR.yaml")


library(configr)


GLM3r::run_glm('GLM-AED2/')
SimstratR::run_simstrat('Simstrat-AED2/')
WETr::run_wet('GOTM-WET/')
SelmaprotbasR::run_gotm_sp('GOTM-Selmaprotbas/')

glmtools::sim_vars('GLM-AED2/output/output.nc')

glmtools::plot_var(nc_file = 'GLM-AED2/output/output.nc', var_name = 'OXY_oxy', reference = 'surface')
glmtools::plot_var(nc_file = 'GLM-AED2/output/output.nc', var_name = 'SIL_rsi', reference = 'surface')
glmtools::plot_var(nc_file = 'GLM-AED2/output/output.nc', var_name = 'NIT_nit', reference = 'surface')
glmtools::plot_var(nc_file = 'GLM-AED2/output/output.nc', var_name = 'NIT_amm', reference = 'surface')
glmtools::plot_var(nc_file = 'GLM-AED2/output/output.nc', var_name = 'PHS_frp', reference = 'surface')
glmtools::plot_var(nc_file = 'GLM-AED2/output/output.nc', var_name = 'CAR_dic', reference = 'surface')
glmtools::plot_var(nc_file = 'GLM-AED2/output/output.nc', var_name = 'OGM_poc', reference = 'surface')
glmtools::plot_var(nc_file = 'GLM-AED2/output/output.nc', var_name = 'OGM_don', reference = 'surface')

glmtools::plot_var(nc_file = 'GLM-AED2/output/output.nc', var_name = 'CAR_ch4', reference = 'surface')

glmtools::plot_var(nc_file = 'GLM-AED2/output/output.nc', var_name = 'PHY_diatoms', reference = 'surface')
glmtools::plot_var(nc_file = 'GLM-AED2/output/output.nc', var_name = 'PHY_cyanobacteria', reference = 'surface')
glmtools::plot_var(nc_file = 'GLM-AED2/output/output.nc', var_name = 'PHY_some_random_group', reference = 'surface')
glmtools::plot_var(nc_file = 'GLM-AED2/output/output.nc', var_name = 'temp', reference = 'surface')
glmtools::plot_var(nc_file = 'GLM-AED2/output/output.nc', var_name = 'PHY_TCHLA', reference = 'surface')

glmtools::plot_var(nc_file = 'GLM-AED2/output/output.nc', var_name = 'extc_coef', reference = 'surface')
glmtools::plot_var(nc_file = 'GLM-AED2/output/output.nc', var_name = 'NCS_ss1', reference = 'surface')

chla = glmtools::get_var(file  = 'GLM-AED2/output/output.nc', var_name  = 'PHY_TCHLA', reference = 'surface', z_out = 2)


SelmaprotbasR::run_gotm_sp('GOTM-Selmaprotbas/')
library(ncdf4)
nc <- nc_open('GOTM-Selmaprotbas/output/output.nc')
names(nc$var)

oxy <- ncvar_get(nc, 'selmaprotbas_DO_mg')
ox.df <- data.frame(cbind('time' = nc$dim$time$vals,
                       t(oxy)))
colnames(ox.df) <- c('time', unique(as.numeric(nc$dim$z$vals[1:50])))

chla <- ncvar_get(nc, 'total_chlorophyll_calculator_result')
chla.df <- data.frame(cbind('time' = nc$dim$time$vals,
                          t(chla)))
colnames(chla.df) <- c('time', unique(as.numeric(nc$dim$z$vals[1:50])))

npp <- ncvar_get(nc, 'cyanobacteria_NPP') + ncvar_get(nc, 'diatoms_NPP') +
  + ncvar_get(nc, 'some_random_group_NPP')
npp.df <- data.frame(cbind('time' = nc$dim$time$vals,
                            t(npp)))
colnames(npp.df) <- c('time', unique(as.numeric(nc$dim$z$vals[1:50])))

nc_close(nc)

timelength <- seq(from = as.POSIXct('2009-01-01 00:00:00'), to = as.POSIXct('2015-12-30 00:00:00'), by = "days")


# OXYGEN SELMA
df <- reshape2::melt(ox.df, 'time')
df$Dep = as.numeric(as.character(sub(".*_", "", df$variable)))   
# df$Dep = df$Dep - max(df$Dep)
df$time = as.POSIXct(timelength)

g1<- ggplot(df, aes((time), (1) * Dep)) +
  geom_raster(aes(fill = 1/1000* as.numeric(value)), interpolate = TRUE) +
  scale_fill_gradientn(limits = c(0,20),
                       colours = rev(RColorBrewer::brewer.pal(11, 'Spectral')))+
  theme_minimal()  +xlab('') +
  ylab('Depth') + ggtitle('GOTM-Selmaprotbas') +
  labs(fill = 'DO [mg/L]'); g3

# CHLA SELMA
df <- reshape2::melt(chla.df, 'time')
df$Dep = as.numeric(as.character(sub(".*_", "", df$variable)))   
# df$Dep = df$Dep - max(df$Dep)
df$time = as.POSIXct(timelength)

g2 <- ggplot(df, aes((time), (1) * Dep)) +
  geom_raster(aes(fill = 1* as.numeric(value)), interpolate = TRUE) +
  scale_fill_gradientn(limits = c(0,150),
                       colours = rev(RColorBrewer::brewer.pal(11, 'Spectral')))+
  theme_minimal()  +xlab('') +
  ylab('Depth') + ggtitle('GOTM-Selmaprotbas') +
  labs(fill = 'Chla [ug/L]'); g1

# NPP SELMA
df <- reshape2::melt(npp.df, 'time')
df$Dep = as.numeric(as.character(sub(".*_", "", df$variable)))   
# df$Dep = df$Dep - max(df$Dep)
df$time =as.POSIXct(timelength)

g3 <- ggplot(df, aes((time), (1) * Dep)) +
  geom_raster(aes(fill = 1* as.numeric(value)), interpolate = TRUE) +
  scale_fill_gradientn(limits = c(-30,100),
                       colours = rev(RColorBrewer::brewer.pal(11, 'Spectral')))+
  theme_minimal()  +xlab('') +
  ylab('Depth') + ggtitle('GOTM-Selmaprotbas') +
  labs(fill = 'NPP [mmol/m3/d]'); g1



GLM3r::run_glm('GLM-AED2/')
# DO GLM
data <- glmtools::get_var('GLM-AED2/output/output.nc', var_name = 'OXY_oxy', reference = 'surface')
time = data$DateTime
data = data[-ncol(data)]
df <- reshape2::melt(data, 'DateTime')
df$Dep = as.numeric(as.character(sub(".*_", "", df$variable)))   

g4 <- ggplot(df, aes((DateTime), (-1) * Dep)) +
  geom_raster(aes(fill = 32/1000* as.numeric(value)), interpolate = TRUE) +
  scale_fill_gradientn(limits = c(0,20),
                       colours = rev(RColorBrewer::brewer.pal(11, 'Spectral')))+
  theme_minimal()  +xlab('') +
  ylab('Depth') + ggtitle('GLM-AED2') +
  labs(fill = 'DO [mg/L]'); g1

# CHLA GLM
data <- glmtools::get_var('GLM-AED2/output/output.nc', var_name = 'PHY_TCHLA', reference = 'surface')
time = data$DateTime
data = data[-ncol(data)]
df <- reshape2::melt(data, 'DateTime')
df$Dep = as.numeric(as.character(sub(".*_", "", df$variable)))   

g5 <- ggplot(df, aes((DateTime), (-1) * Dep)) +
  geom_raster(aes(fill = 1* as.numeric(value)), interpolate = TRUE) +
  scale_fill_gradientn(limits = c(0,150),
                       colours = rev(RColorBrewer::brewer.pal(11, 'Spectral')))+
  theme_minimal()  +xlab('') +
  ylab('Depth') +ggtitle('GLM-AED2') +
  labs(fill = 'Chla [ug/L]'); g1

# NPP GLM
data <- glmtools::get_var('GLM-AED2/output/output.nc', var_name = 'PHY_NCP', reference = 'surface')
time = data$DateTime
data = data[-ncol(data)]
df <- reshape2::melt(data, 'DateTime')
df$Dep = as.numeric(as.character(sub(".*_", "", df$variable)))   

g6 <- ggplot(df, aes((DateTime), (-1) * Dep)) +
  geom_raster(aes(fill = 1* as.numeric(value)), interpolate = TRUE) +
  scale_fill_gradientn(limits = c(-30,100),
                       colours = rev(RColorBrewer::brewer.pal(11, 'Spectral')))+
  theme_minimal()  +xlab('') +
  ylab('Depth') +ggtitle('GLM-AED2') +
  labs(fill = 'NPP [mmol/m3/d]'); g1





library(tidyverse)
SimstratR::run_simstrat('Simstrat-AED2/')

data <- read_csv('Simstrat-AED2/output/T_out.dat')

# DO Simstrat
data <- read_csv('../../../../../Dropbox/LER-WQ/LakeMendota/Simstrat-AED2/output/OXY_oxy_out.dat')
time =  data$Datetime
df <- reshape2::melt(data, "Datetime")
df$time = timelength
df$value[df$value < 0] = 0

g7 <- ggplot(df, aes((time), (1) * as.numeric(as.character(variable)))) +
  geom_raster(aes(fill = 32/1000* as.numeric(value)), interpolate = TRUE) +
  scale_fill_gradientn(limits = c(0,20),
  colours = rev(RColorBrewer::brewer.pal(11, 'Spectral')))+
  theme_minimal()  +xlab('') +
  ylab('Depth') + ggtitle('Simstrat-AED2') +
  labs(fill = 'DO [mg/L]'); g7

# CHLA Simstrat
data <- read_csv('../../../../../Dropbox/LER-WQ/LakeMendota/Simstrat-AED2/output/PHY_TCHLA_out.dat')
time =  data$Datetime
df <- reshape2::melt(data, "Datetime")
df$time = timelength


g8 <- ggplot(df, aes((time), (1) * as.numeric(as.character(variable)))) +
  geom_raster(aes(fill = 1* as.numeric(value)), interpolate = TRUE) +
  scale_fill_gradientn(limits = c(0,150),
                       colours = rev(RColorBrewer::brewer.pal(11, 'Spectral')))+
  theme_minimal()  +xlab('') +
  ylab('Depth') + ggtitle('Simstrat-AED2') +
  labs(fill = 'Chla [ug/L]'); g8

# NPP Simstrat
data <- read_csv('../../../../../Dropbox/LER-WQ/LakeMendota/Simstrat-AED2/output/PHY_NCP_out.dat')
time =  data$Datetime
df <- reshape2::melt(data, "Datetime")
df$time = timelength


g9 <- ggplot(df, aes((time), (1) * as.numeric(as.character(variable)))) +
  geom_raster(aes(fill = 1* as.numeric(value)), interpolate = TRUE) +
  scale_fill_gradientn(limits = c(-30,100),
                       colours = rev(RColorBrewer::brewer.pal(11, 'Spectral')))+
  theme_minimal()  +xlab('') +
  ylab('Depth') + ggtitle('Simstrat-AED2') +
  labs(fill = 'NPP [mmol/m3/d]'); g9


library(patchwork)
g<-(g1 + g4 + g7) / (g2+ g5 + g8) / (g3+ g6 + g9) +  plot_layout(guides = "collect");g
ggsave(filename = 'output/wq_ensemble.png', plot = g, units = "in", width = 11, height = 5, dpi = 300)

g_do <- (g1 + g4 + g7)+  plot_layout(guides = "collect")
ggsave(filename = 'output/do_ensemble.png', plot = g_do, units = "in", width = 11, height = 3, dpi = 300)

g_chla <-  (g2+ g5 + g8)+  plot_layout(guides = "collect")
ggsave(filename = 'output/chla_ensemble.png', plot = g_chla, units = "in", width = 11, height = 3, dpi = 300)

g_npp <- (g3+ g6 + g9)+  plot_layout(guides = "collect")
ggsave(filename = 'output/npp_ensemble.png', plot = g_npp, units = "in", width = 11, height = 3, dpi = 300)

#DO

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/29/30/03e232a1b362900e0f059859abe8eb97" 
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
                 "daynum",     
                 "sampledate",     
                 "depth",     
                 "rep",     
                 "sta",     
                 "event",     
                 "wtemp",     
                 "o2",     
                 "o2sat",     
                 "deck",     
                 "light",     
                 "frlight",     
                 "flagdepth",     
                 "flagwtemp",     
                 "flago2",     
                 "flago2sat",     
                 "flagdeck",     
                 "flaglight",     
                 "flagfrlight"    ), check.names=TRUE)

unlink(infile1)
str(dt1)                            

# surface at 1 m, bottom at 20 m
ox.df$surface = ox.df$'-0.75' + (1 - 0.75) * (ox.df$'-1.25' - ox.df$'-0.75')/(1.25-0.75)
ox.df$bottom = ox.df$'-19.75' + (20 - 19.75) * (ox.df$'-20.25' - ox.df$'-19.75')/(20.25-19.75)
ox.df$DateTime =  as.POSIXct(timelength)
selma.do <- ox.df %>%
  mutate(surface = surface/1000, bottom = bottom/1000) %>%
  select(DateTime, surface, bottom)

ox.glm <- glmtools::get_var('GLM-AED2/output/output.nc', var_name = 'OXY_oxy', reference = 'surface')
ox.glm$surface = ox.glm$OXY_oxy_0 + (1 - 0) * (ox.glm$OXY_oxy_1.31861334945136 - ox.glm$OXY_oxy_0)/(1.31861334945136-0.)
ox.glm$bottom = ox.glm$OXY_oxy_19.7792002417704 + (20 - 19.7792002417704) * (ox.glm$OXY_oxy_21.0978135912218 - ox.glm$OXY_oxy_19.7792002417704)/
  (21.0978135912218-19.7792002417704)
glm.do <- ox.glm %>%
  mutate(surface = surface*32/1000, bottom = bottom*32/1000) %>%
  select(DateTime, surface, bottom)

ox.sim <- data <- read_csv('../../../../../Dropbox/LER-WQ/LakeMendota/Simstrat-AED2/output/OXY_oxy_out.dat')
ox.sim$surface = ox.sim$`-1.000`
ox.sim$bottom = ox.sim$`-20.000`
ox.sim$DateTime =  as.POSIXct(timelength)
sim.do <- ox.sim %>%
  mutate(surface = surface*32/1000, bottom = bottom*32/1000) %>% 
  select(DateTime, surface, bottom)

surface.obs <- dt1 %>%
  filter(lakeid == 'ME', depth == 1) %>%
  select(sampledate, o2)
bottom.obs <- dt1 %>%
  filter(lakeid == 'ME', depth == 20) %>%
  select(sampledate, o2)
obs.ox <- merge(surface.obs, bottom.obs, by = 'sampledate')

h1 <- ggplot() +
  geom_line(data = selma.do, aes(as.Date(DateTime), surface, col = 'GOTM-Selmaprotbas')) +
  geom_line(data = glm.do, aes(as.Date(DateTime), surface, col = 'GLM-AED2')) +
  geom_line(data = sim.do, aes(as.Date(DateTime), surface, col = 'Simstrat-AED2')) +
  geom_point(data = obs.ox, aes(as.Date(sampledate), o2.x, col = 'Observed'), size =2 , alpha = 0.5) +
  xlim(as.Date('2009-01-01'), as.Date('2015-12-31')) +  theme_minimal() +
  xlab('') + ylab('Dissolved oxygen (mg/L)') +
  ggtitle('Surface (1 m)') +
  theme(legend.title=element_blank()); h1

h2 <- ggplot() +
  geom_line(data = selma.do, aes(as.Date(DateTime), bottom, col = 'GOTM-Selmaprotbas')) +
  geom_line(data = glm.do, aes(as.Date(DateTime), bottom, col = 'GLM-AED2')) +
  geom_line(data = sim.do, aes(as.Date(DateTime), bottom, col = 'Simstrat-AED2')) +
  geom_point(data = obs.ox, aes(as.Date(sampledate), o2.y, col = 'Observed'), size =2 , alpha = 0.5) +
  xlim(as.Date('2009-01-01'), as.Date('2015-12-31')) +  theme_minimal() +
  xlab('') + ylab('Dissolved oxygen (mg/L)') +
  ggtitle('Bottom (20 m)') +
  theme(legend.title=element_blank()); h2

ox <- h1 / h2 +  plot_layout(guides = "collect");ox
ggsave(filename = 'output/do_timeseries.png', plot = ox, units = "in", width = 11, height = 5, dpi = 300)



# CHLA

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/38/27/66796c3bc77617e7cc95c4b09d4995c5" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt2 <-read.csv(infile1,header=F 
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



# surface at 1 m, bottom at 20 m
chla.df$surface = chla.df$'-0.75' + (1 - 0.75) * (chla.df$'-1.25' - chla.df$'-0.75')/(1.25-0.75)
chla.df$bottom = chla.df$'-19.75' + (20 - 19.75) * (chla.df$'-20.25' - chla.df$'-19.75')/(20.25-19.75)
chla.df$DateTime =  as.POSIXct(timelength)
selma.chla <- chla.df %>%
  mutate(surface = surface, bottom = bottom) %>%
  select(DateTime, surface, bottom)

cha.glm <- glmtools::get_var('GLM-AED2/output/output.nc', var_name = 'PHY_TCHLA', reference = 'surface')
cha.glm$surface = cha.glm$PHY_TCHLA_0 + (1 - 0) * (cha.glm$PHY_TCHLA_1.31861334945136 - cha.glm$PHY_TCHLA_0)/(1.31861334945136-0.)
cha.glm$bottom = cha.glm$PHY_TCHLA_19.7792002417704 + (20 - 19.7792002417704) * (cha.glm$PHY_TCHLA_21.0978135912218 - cha.glm$PHY_TCHLA_19.7792002417704)/
  (21.0978135912218-19.7792002417704)
glm.chla <- cha.glm %>%
  mutate(surface = surface*1, bottom = bottom*1) %>%
  select(DateTime, surface, bottom)

chla.sim <- data <- read_csv('../../../../../Dropbox/LER-WQ/LakeMendota/Simstrat-AED2/output/PHY_TCHLA_out.dat')
chla.sim$surface = chla.sim$`-1.000`
chla.sim$bottom = chla.sim$`-20.000`
chla.sim$DateTime =  as.POSIXct(timelength)
sim.chla <- chla.sim %>%
  mutate(surface = surface*1, bottom = bottom*1) %>% 
  select(DateTime, surface, bottom)

obs.chla <- dt2 %>%
  filter(lakeid == 'ME', depth_range_m == '0-2') %>%
  select(sampledate, tri_chl_spec)

h3 <- ggplot() +
  geom_line(data = selma.chla, aes(as.Date(DateTime), surface, col = 'GOTM-Selmaprotbas')) +
  geom_line(data = glm.chla, aes(as.Date(DateTime), surface, col = 'GLM-AED2')) +
  geom_line(data = sim.chla, aes(as.Date(DateTime), surface, col = 'Simstrat-AED2')) +
  geom_point(data = obs.chla, aes(as.Date(sampledate), tri_chl_spec, col = 'Observed'), size =2 , alpha = 0.5) +
  xlim(as.Date('2009-01-01'), as.Date('2015-12-31')) +  theme_minimal() +
  xlab('') + ylab('Chlorophyll-a (ug/L)') +
  ggtitle('Surface (1 m)') +
  theme(legend.title=element_blank()); h3

ggsave(filename = 'output/chla_timeseries.png', plot = h3, units = "in", width = 11, height = 5, dpi = 300)



# anoxic factor

hyps = read_csv('LakeEnsemblR_bathymetry_standard.csv')

ox.df$DateTime =  as.POSIXct(timelength)
ox.gotm <- ox.df[,-c(1)]
m.ox.gotm <- reshape2::melt(ox.gotm, id ='DateTime')
m.ox.gotm$Depth = as.numeric(as.character(m.ox.gotm$variable)) * (-1)

ox.glm <- glmtools::get_var('GLM-AED2/output/output.nc', var_name = 'OXY_oxy', reference = 'surface')
m.ox.glm <- reshape2::melt(ox.glm, id = 'DateTime')
m.ox.glm$Depth <- as.numeric(as.character(sub(".*_", "", m.ox.glm$variable)))  

ox.sim <- data <- read_csv('../../../../../Dropbox/LER-WQ/LakeMendota/Simstrat-AED2/output/OXY_oxy_out.dat')
ox.sim$DateTime =  as.POSIXct(timelength)
ox.sim <- ox.sim[,-c(1)]
m.ox.sim <- reshape2::melt(ox.sim, id = 'DateTime')
m.ox.sim$Depth = as.numeric(as.character(m.ox.sim$variable)) * (-1)

gotm=m.ox.gotm %>%
  group_by(DateTime) %>%
  mutate(value = ifelse(value < 0, 0, value)) %>%
  filter(value <= 1000) %>%
  summarise(anox.dep = min(Depth))

glm=m.ox.glm %>%
  group_by(DateTime) %>%
  mutate(value = ifelse(value < 0, 0, value),
         value = value *32 /1000) %>%
  filter(value <= 1) %>%
  summarise(anox.dep = min(Depth))

sim=m.ox.sim %>%
  group_by(DateTime) %>%
  mutate(value = ifelse(value < 0, 0, value),
         value = value *32 /1000) %>%
  filter(value <= 1) %>%
  summarise(anox.dep = min(Depth))

gotm.af <- gotm %>%
  mutate(year = lubridate::year(DateTime),
         area = approx(hyps$Depth_meter, hyps$Area_meterSquared, anox.dep)$y) %>%
  group_by(year) %>%
  summarise(af = sum(area)/max(hyps$Area_meterSquared))

glm.af <- glm %>%
  mutate(year = lubridate::year(DateTime),
         area = approx(hyps$Depth_meter, hyps$Area_meterSquared, anox.dep)$y) %>%
  group_by(year) %>%
  summarise(af = sum(area, na.rm = T)/max(hyps$Area_meterSquared, na.rm = T))

sim.af <- sim %>%
  mutate(year = lubridate::year(DateTime),
         area = approx(hyps$Depth_meter, hyps$Area_meterSquared, anox.dep)$y) %>%
  group_by(year) %>%
  summarise(af = sum(area, na.rm = T)/max(hyps$Area_meterSquared, na.rm = T))

h4 <- ggplot() +
  geom_line(data = gotm.af, aes(year, af, col = 'GOTM-Selmaprotbas')) +
  geom_line(data = glm.af, aes(year, af, col = 'GLM-AED2')) +
  geom_line(data = sim.af, aes(year, af, col = 'Simstrat-AED2')) +
  geom_point(data = gotm.af, aes(year, af, col = 'GOTM-Selmaprotbas'),size =2) +
  geom_point(data = glm.af, aes(year, af, col = 'GLM-AED2'),size =2) +
  geom_point(data = sim.af, aes(year, af, col = 'Simstrat-AED2'),size =2) +
  theme_minimal() +
  xlab('') + ylab('Anoxic factor (days per season)') +
  ggtitle('Anoxia (DO <= 1 mg/L)') +
  theme(legend.title=element_blank()); h4
ggsave(filename = 'output/hypaf_timeseries.png', plot = h4, units = "in", width = 8, height = 4, dpi = 300)

  
obs.dat <- dt1 %>%
  select(sampledate, depth, o2)

obs <- obs.dat %>%
  group_by(sampledate, depth) %>%
  arrange(depth) %>%
  summarise(do_int = approx(depth, o2, seq(0,24,1))$y)
  