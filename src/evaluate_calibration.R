# remove everything from workspace
rm(list = ls())


library(glmtools)
library(tidyverse)
library(rLakeAnalyzer)
library(GLM3r)
library(lubridate)
library(LakeEnsemblR)
library(LakeEnsemblR.WQ)
library(ggplot2)
library(reshape2)

# declare output files
sim_folder= 'GLM-AED2/'
out_file <- file.path(sim_folder, "output","output.nc")

# read observed data
df_obs <- read.csv('trainingData/MendotaData_observedGLM_checked.csv')

# read best simulation results
df = read.csv("GLM-AED2/calib_results_nse.csv")

df = df %>% mutate(flag = temp + OXY_oxy + NIT_nit + NIT_amm + PHS_frp + SIL_rsi + OGM_doc +
                     PHY_TCHLA) %>%
  dplyr::filter(flag > -1e8)

ggplot2::ggplot(reshape2::melt(df, id.vars = c('time', 'NRMSE'))) +
  geom_point(aes(as.POSIXct(time), value, col = NRMSE)) + 
  geom_line(aes(as.POSIXct(time), value, col = NRMSE)) + 
  theme(legend.position = 'bottom') + xlab('time') + ylab('NSE')+
  scale_colour_continuous(type = rev("viridis")) +
  facet_wrap(~variable, scales = 'free')

best_run = df[which.min(df$NRMSE),]

df.head=df %>%
  arrange(NRMSE)
head(df.head, n = 10)

# get best parameters
parms = read.csv("GLM-AED2/calib_par.csv")

best_parms = parms %>% filter(NRMSE == best_run$NRMSE)

# get calibration setup
config_file <- "LakeEnsemblR.yaml"
model <- c("GLM", "GOTM", "Simstrat")

path = "GLM-AED2/"
nml_file <- file.path("GLM-AED2/", 'glm3.nml')
eg_nml <- read_nml(nml_file = file.path(nml_file))

var = c('temp', 'OXY_oxy' ,'NIT_nit','NIT_amm', "PHS_frp", 'SIL_rsi', "OGM_doc", 'PHY_TCHLA')         # variable to which we apply the calibration procedure
# which parameter do you want to calibrate? a sensitivity analysis helps
calib_setup <- data.frame('pars' = as.character(c('wind_factor','lw_factor','coef_mix_hyp',"inflow_factor",'Kw','outflow_factor',
                                                  'Fsed_oxy', 'Ksed_oxy', 'Kdom_minerl', 'Rdom_minerl',
                                                  'Fsed_nit', 'Ksed_nit', 'Knitrif', 'Fsed_amm',
                                                  'Fsed_frp',
                                                  'Fsed_rsi',
                                                  'pd%R_growth', 'pd%R_growth', 'pd%R_growth',
                                                  "Fsed_dic", 'Rnitrif', 'Kdenit',  'Ksed_frp',
                                                  'Kpom_hydrol',
                                                  "pd%R_resp", "pd%R_resp", "pd%R_resp",
                                                  "pd%R_nuptake","pd%R_nuptake", "pd%R_nuptake",
                                                  "pd%R_puptake",   "pd%R_puptake",   "pd%R_puptake"
)),
'lb' = NA,
'ub' = NA,
'x0' = c(1,1,0.4,1,0.8,1, -150, 50, 60, 0.02, -10, 10, 80, 30, 0.5, 18, 4, 1, 3,
         250, 0.5, 3,  200, 60,
         0.085,0.085,0.085,
         0.068, 0.068, 0.068,
         0.009, 0.0039, 0.0039),
'type' = c('glm', 'glm', 'glm','glm', 'glm','glm','aed','aed','aed','aed','aed','aed','aed','aed','aed','aed',
           "phyto", 'phyto', 'phyto',
           'aed','aed','aed','aed','aed','phyto', 'phyto', 'phyto',
           'phyto', 'phyto', 'phyto','phyto', 'phyto', 'phyto'),
'file' = c('glm3.nml', 'glm3.nml', 'glm3.nml','glm3.nml','glm3.nml','glm3.nml', 'aed2.nml','aed2.nml','aed2.nml',
           'aed2.nml','aed2.nml','aed2.nml','aed2.nml','aed2.nml','aed2.nml','aed2.nml',
           "aed2_phyto_pars.nml", 'aed2_phyto_pars.nml', 'aed2_phyto_pars.nml',
           'aed2.nml','aed2.nml','aed2.nml','aed2.nml','aed2.nml',
           'aed2_phyto_pars.nml', 'aed2_phyto_pars.nml', 'aed2_phyto_pars.nml',
           'aed2_phyto_pars.nml', 'aed2_phyto_pars.nml', 'aed2_phyto_pars.nml',
           'aed2_phyto_pars.nml', 'aed2_phyto_pars.nml', 'aed2_phyto_pars.nml'))
calib_setup$lb[ calib_setup$x0 >= 0] = calib_setup$x0[ calib_setup$x0 >= 0] * 0.3
calib_setup$ub[ calib_setup$x0 >= 0] = calib_setup$x0[ calib_setup$x0 >= 0] * 1.7
calib_setup$lb[ calib_setup$x0 < 0] = calib_setup$x0[ calib_setup$x0 < 0] * 1.7
calib_setup$ub[ calib_setup$x0 < 0] = calib_setup$x0[ calib_setup$x0 < 0] * 0.3
calib_setup$lb[5] = 0.01
calib_setup$ub[5] = 2.0
print(calib_setup)

aed_file = 'aed2'
glm_file = 'glm3'
phyto_file = 'aed2_phyto_pars'

p = as.numeric(best_parms[-c(1, length(best_parms))])

# run model with best parameter set
for (nml_file in unique(calib_setup$file)){
  eg_nml <- read_nml(paste0(path,nml_file))
  
  idx = which(calib_setup$file == nml_file)
  use_p = p[idx]
  nml_setup = calib_setup %>% filter(file == nml_file)
  
  for(i in 1:length(nml_setup$pars[!duplicated(nml_setup$pars)])){
    if (any(nml_setup$pars[!duplicated(nml_setup$pars)][i] == nml_setup$pars[duplicated(nml_setup$pars)])){
      eg_nml <- set_nml(eg_nml, nml_setup$pars[!duplicated(nml_setup$pars)][i], 
                        use_p[which(nml_setup$pars[!duplicated(nml_setup$pars)][i] == nml_setup$pars)])
    } else {
      eg_nml <- set_nml(eg_nml,nml_setup$pars[!duplicated(nml_setup$pars)][i],use_p[!duplicated(nml_setup$pars)][i])
    }
  }
  
  write_nml(eg_nml, file = paste0(path, nml_file))
  
}

# run GLM
GLM3r::run_glm('GLM-AED2/')


# visualize FITS, important stuff
x_start = as.POSIXct('1995-01-01 00:00:00')
x_end = as.POSIXct('2004-12-31 23:00:00')

# TEMPERATURE
var_name = 'temp'
surface_temp <- get_var(file = out_file, 
                        var_name = var_name,
                        reference = 'surface',
                        z_out = 2)
surface_temp$var = surface_temp[,2]
y_label = 'Temp. (deg C)'

h1 <- ggplot(surface_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = df_obs %>% filter(depth == 2), aes(as.POSIXct(datetime), temp, col = 'obs')) +
  ggtitle(paste0('Surface (2m) ',var_name)) +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

middle_temp <- get_var(file = out_file, 
                       var_name = var_name,
                       reference = 'surface',
                       z_out = 12)
middle_temp$var = middle_temp[,2]

h2 <- ggplot(middle_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = df_obs %>% filter(depth == 12), aes(as.POSIXct(datetime), temp, col = 'obs')) +
  ggtitle(paste0('Middle (12m) ',var_name)) +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

# visualize change of bottom water temp. over time
bottom_temp <- get_var(file = out_file, 
                       var_name = var_name,
                       reference = 'surface',
                       z_out = 20)
bottom_temp$var = bottom_temp[,2]

h3 <- ggplot(bottom_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = df_obs %>% filter(depth == 20), aes(as.POSIXct(datetime), temp, col = 'obs')) +
  ggtitle(paste0('Bottom (20m) ',var_name)) + #, water temperature') +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

h <- h1 / h2 / h3; h
ggsave(filename = paste0('GLM-AED2/A_150124/figures/',var_name,'.png'), plot = h, dpi = 300, width = 9, height = 15, units = 'in')

# OXYGEN
var_name = 'OXY_oxy'
surface_temp <- get_var(file = out_file, 
                        var_name = var_name,
                        reference = 'surface',
                        z_out = 2)
surface_temp$var = surface_temp[,2]
y_label = 'DO (mmol/m3)'
plot_df_obs = df_obs %>% select(datetime, depth, var_name)
plot_df_obs$var = plot_df_obs[,3]

h1 <- ggplot(surface_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 2), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Surface (2m) ',var_name)) +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

middle_temp <- get_var(file = out_file, 
                       var_name = var_name,
                       reference = 'surface',
                       z_out = 12)
middle_temp$var = middle_temp[,2]

h2 <- ggplot(middle_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 12), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Middle (12m) ',var_name)) +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

# visualize change of bottom water temp. over time
bottom_temp <- get_var(file = out_file, 
                       var_name = var_name,
                       reference = 'surface',
                       z_out = 20)
bottom_temp$var = bottom_temp[,2]

h3 <- ggplot(bottom_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 20), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Bottom (20m) ',var_name)) + #, water temperature') +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

h <- h1 / h2 / h3; h
ggsave(filename = paste0('GLM-AED2/A_150124/figures/',var_name,'.png'), plot = h, dpi = 300, width = 9, height = 15, units = 'in')


# NITORGEN
var_name = 'NIT_nit'
surface_temp <- get_var(file = out_file, 
                        var_name = var_name,
                        reference = 'surface',
                        z_out = 4)
surface_temp$var = surface_temp[,2]
y_label = 'NO3 (mmol/m3)'
plot_df_obs = df_obs %>% select(datetime, depth, var_name)
plot_df_obs$var = plot_df_obs[,3]

h1 <- ggplot(surface_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 4), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Surface (4m) ',var_name)) +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

middle_temp <- get_var(file = out_file, 
                       var_name = var_name,
                       reference = 'surface',
                       z_out = 12)
middle_temp$var = middle_temp[,2]

h2 <- ggplot(middle_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 12), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Middle (12m) ',var_name)) +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

# visualize change of bottom water temp. over time
bottom_temp <- get_var(file = out_file, 
                       var_name = var_name,
                       reference = 'surface',
                       z_out = 20)
bottom_temp$var = bottom_temp[,2]

h3 <- ggplot(bottom_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 20), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Bottom (20m) ',var_name)) + #, water temperature') +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

h <- h1 / h2 / h3; h
ggsave(filename = paste0('GLM-AED2/A_150124/figures/',var_name,'.png'), plot = h, dpi = 300, width = 9, height = 15, units = 'in')



# PHOSPHORUS
var_name = 'PHS_frp'
surface_temp <- get_var(file = out_file, 
                        var_name = var_name,
                        reference = 'surface',
                        z_out = 4)
surface_temp$var = surface_temp[,2]
y_label = 'SRP (mmol/m3)'
plot_df_obs = df_obs %>% select(datetime, depth, var_name)
plot_df_obs$var = plot_df_obs[,3]

h1 <- ggplot(surface_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 4), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Surface (4m) ',var_name)) +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

middle_temp <- get_var(file = out_file, 
                       var_name = var_name,
                       reference = 'surface',
                       z_out = 12)
middle_temp$var = middle_temp[,2]

h2 <- ggplot(middle_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 12), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Middle (12m) ',var_name)) +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

# visualize change of bottom water temp. over time
bottom_temp <- get_var(file = out_file, 
                       var_name = var_name,
                       reference = 'surface',
                       z_out = 20)
bottom_temp$var = bottom_temp[,2]

h3 <- ggplot(bottom_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 20), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Bottom (20m) ',var_name)) + #, water temperature') +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

h <- h1 / h2 / h3; h
ggsave(filename = paste0('GLM-AED2/A_150124/figures/',var_name,'.png'), plot = h, dpi = 300, width = 9, height = 15, units = 'in')



# pH
var_name = 'CAR_pH'
surface_temp <- get_var(file = out_file, 
                        var_name = var_name,
                        reference = 'surface',
                        z_out = 4)
surface_temp$var = surface_temp[,2]
y_label = 'pH (log10 mmol/m3)'
plot_df_obs = df_obs %>% select(datetime, depth, var_name)
plot_df_obs$var = plot_df_obs[,3]

h1 <- ggplot(surface_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 4), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Surface (4m) ',var_name)) +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

middle_temp <- get_var(file = out_file, 
                       var_name = var_name,
                       reference = 'surface',
                       z_out = 12)
middle_temp$var = middle_temp[,2]

h2 <- ggplot(middle_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 12), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Middle (12m) ',var_name)) +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

# visualize change of bottom water temp. over time
bottom_temp <- get_var(file = out_file, 
                       var_name = var_name,
                       reference = 'surface',
                       z_out = 20)
bottom_temp$var = bottom_temp[,2]

h3 <- ggplot(bottom_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 20), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Bottom (20m) ',var_name)) + #, water temperature') +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

h <- h1 / h2 / h3; h
ggsave(filename = paste0('GLM-AED2/A_150124/figures/',var_name,'.png'), plot = h, dpi = 300, width = 9, height = 15, units = 'in')

# NIT_amm
var_name = 'NIT_amm'
surface_temp <- get_var(file = out_file, 
                        var_name = var_name,
                        reference = 'surface',
                        z_out = 4)
surface_temp$var = surface_temp[,2]
y_label = 'Amm (mmol/m3)'
plot_df_obs = df_obs %>% select(datetime, depth, var_name)
plot_df_obs$var = plot_df_obs[,3]

h1 <- ggplot(surface_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 4), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Surface (4m) ',var_name)) +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

middle_temp <- get_var(file = out_file, 
                       var_name = var_name,
                       reference = 'surface',
                       z_out = 12)
middle_temp$var = middle_temp[,2]

h2 <- ggplot(middle_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 12), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Middle (12m) ',var_name)) +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

# visualize change of bottom water temp. over time
bottom_temp <- get_var(file = out_file, 
                       var_name = var_name,
                       reference = 'surface',
                       z_out = 20)
bottom_temp$var = bottom_temp[,2]

h3 <- ggplot(bottom_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 20), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Bottom (20m) ',var_name)) + #, water temperature') +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

h <- h1 / h2 / h3; h
ggsave(filename = paste0('GLM-AED2/A_150124/figures/',var_name,'.png'), plot = h, dpi = 300, width = 9, height = 15, units = 'in')

# Silica
var_name = 'SIL_rsi'
surface_temp <- get_var(file = out_file, 
                        var_name = var_name,
                        reference = 'surface',
                        z_out = 4)
surface_temp$var = surface_temp[,2]
y_label = 'pH (log10 mmol/m3)'
plot_df_obs = df_obs %>% select(datetime, depth, var_name)
plot_df_obs$var = plot_df_obs[,3]

h1 <- ggplot(surface_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 4), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Surface (4m) ',var_name)) +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

middle_temp <- get_var(file = out_file, 
                       var_name = var_name,
                       reference = 'surface',
                       z_out = 12)
middle_temp$var = middle_temp[,2]

h2 <- ggplot(middle_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 12), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Middle (12m) ',var_name)) +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

# visualize change of bottom water temp. over time
bottom_temp <- get_var(file = out_file, 
                       var_name = var_name,
                       reference = 'surface',
                       z_out = 20)
bottom_temp$var = bottom_temp[,2]

h3 <- ggplot(bottom_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 20), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Bottom (20m) ',var_name)) + #, water temperature') +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

h <- h1 / h2 / h3; h
ggsave(filename = paste0('GLM-AED2/A_150124/figures/',var_name,'.png'), plot = h, dpi = 300, width = 9, height = 15, units = 'in')


# PHYTO
var_name = 'PHY_TCHLA'
surface_temp <- get_var(file = out_file, 
                        var_name = var_name,
                        reference = 'surface',
                        z_out = 5)
surface_temp$var = surface_temp[,2]
y_label = 'pH (log10 mmol/m3)'
plot_df_obs = df_obs %>% select(datetime, depth, var_name)
plot_df_obs$var = plot_df_obs[,3]

h1 <- ggplot(surface_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 5), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Surface (5m) ',var_name)) +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

middle_temp <- get_var(file = out_file, 
                       var_name = var_name,
                       reference = 'surface',
                       z_out = 13)
middle_temp$var = middle_temp[,2]

h2 <- ggplot(middle_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 13), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Middle (13m) ',var_name)) +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

# visualize change of bottom water temp. over time
bottom_temp <- get_var(file = out_file, 
                       var_name = var_name,
                       reference = 'surface',
                       z_out = 20)
bottom_temp$var = bottom_temp[,2]

h3 <- ggplot(bottom_temp, aes(DateTime, var, col = 'sim')) +
  geom_line() +
  geom_point(data = plot_df_obs %>% filter(depth == 20), aes(as.POSIXct(datetime), var, col = 'obs')) +
  ggtitle(paste0('Bottom (20m) ',var_name)) + #, water temperature') +
  xlim(x_start, x_end) +
  xlab(label = '') + ylab(label = y_label) +
  theme_minimal()

h <- h1 / h2 / h3; h
ggsave(filename = paste0('GLM-AED2/A_150124/figures/',var_name,'.png'), plot = h, dpi = 300, width = 9, height = 15, units = 'in')
