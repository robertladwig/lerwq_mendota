# remove everything from workspace
rm(list = ls())

# set wd to current dir of script
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(glmtools)
library(tidyverse)
library(rLakeAnalyzer)
# devtools::install_github("aemon-j/GLM3r",ref="v3.1.1")
library(GLM3r)
library(lubridate)
library(LakeEnsemblR)
library(LakeEnsemblR.WQ)
library(ggplot2)

setwd('../GLM-AED2/')

# read in observed data for calibration
obs_wtemp <- read.csv("../trainingData/MendotaData_wtempdo_checked.csv")
df_wq <- read.csv('../trainingData/MendotaData_checked.csv')
df_chla <- read.csv('../trainingData/MendotaData_chl_total_checked.csv')
df_diatoms <- read.csv('../trainingData/MendotaData_chl_diatoms_checked.csv')
df_cyano <- read.csv('../trainingData/MendotaData_chl_cyano_checked.csv')
df_others <- read.csv('../trainingData/MendotaData_chl_others_checked.csv')

# get the new meteorology file into LER format
df_meteo <- read.csv('../boundaryconditions/meteorology.csv')
write.csv(x = df_meteo,file = '../LakeEnsemblR_meteo_standard.csv', quote = F, row.names = F)

# run first LER run to create GLM files
config_file <- "LakeEnsemblR.yaml"
model <- c("GLM", "GOTM", "Simstrat")

setwd('..')
LakeEnsemblR::export_config(config_file = config_file, model = model,
                            folder = ".")
# 
# run_ensemble(config_file = config_file,
#              model = model,
#              return_list = FALSE, parallel = FALSE)
# 
# ncdf <- "output/ensemble_output.nc"
# p1 <- plot_heatmap(ncdf)
# p1 <- p1 +
#   theme_classic(base_size = 24) + 
#   scale_colour_gradientn(limits = c(0, 35),
#                          colours = rev(RColorBrewer::brewer.pal(11, "Spectral")))+
#   xlab('') + ylab('Depth (m)')+  labs(col='Temp. (degC)');p1
# 
# # run LER.WQ to create water quality files
# visualise_dictionary()
# 
# create_input_tables(folder = ".", config_file = 'LakeEnsemblR_WQ.yaml', folder_out = 'WQinput', 
#                     input = 'all',
#                     models_coupled = c("GLM-AED2", "Simstrat-AED2", "GOTM-Selmaprotbas",
#                                        'GOTM-WET'))
# 
# LakeEnsemblR.WQ::export_config(config_file = 'LakeEnsemblR_WQ.yaml', 
#                                folder = ".", 
#                                verbose = FALSE,
#                                convert_from_lakeensemblr = TRUE,
#                                ler_config_file = "LakeEnsemblR.yaml")
# 
# # run GLM
# GLM3r::run_glm('GLM-AED2/')
# 
# glmtools::sim_vars('GLM-AED2/output/output.nc')
# 
# glmtools::plot_var(nc_file = 'GLM-AED2/output/output.nc', var_name = 'OXY_oxy', reference = 'surface')
# glmtools::plot_var(nc_file = 'GLM-AED2/output/output.nc', var_name = 'SIL_rsi', reference = 'surface')
# glmtools::plot_var(nc_file = 'GLM-AED2/output/output.nc', var_name = 'NIT_nit', reference = 'surface')
# glmtools::plot_var(nc_file = 'GLM-AED2/output/output.nc', var_name = 'NIT_amm', reference = 'surface')
# glmtools::plot_var(nc_file = 'GLM-AED2/output/output.nc', var_name = 'PHS_frp', reference = 'surface')
# glmtools::plot_var(nc_file = 'GLM-AED2/output/output.nc', var_name = 'CAR_dic', reference = 'surface')
# glmtools::plot_var(nc_file = 'GLM-AED2/output/output.nc', var_name = 'OGM_poc', reference = 'surface')
# glmtools::plot_var(nc_file = 'GLM-AED2/output/output.nc', var_name = 'OGM_don', reference = 'surface')
# glmtools::plot_var(nc_file = 'GLM-AED2/output/output.nc', var_name = 'CAR_ch4', reference = 'surface')
# glmtools::plot_var(nc_file = 'GLM-AED2/output/output.nc', var_name = 'PHY_diatoms', reference = 'surface')
# glmtools::plot_var(nc_file = 'GLM-AED2/output/output.nc', var_name = 'PHY_cyanobacteria', reference = 'surface')
# glmtools::plot_var(nc_file = 'GLM-AED2/output/output.nc', var_name = 'PHY_some_random_group', reference = 'surface')
# glmtools::plot_var(nc_file = 'GLM-AED2/output/output.nc', var_name = 'temp', reference = 'surface')
# glmtools::plot_var(nc_file = 'GLM-AED2/output/output.nc', var_name = 'PHY_TCHLA', reference = 'surface')
# glmtools::plot_var(nc_file = 'GLM-AED2/output/output.nc', var_name = 'extc_coef', reference = 'surface')
# glmtools::plot_var(nc_file = 'GLM-AED2/output/output.nc', var_name = 'NCS_ss1', reference = 'surface')

# modify files to account for warm-up and flows
nml_file <- file.path("GLM-AED2/", 'glm3.nml')

file.copy(from = 'GLM/meteo_file.csv', to = 'GLM-AED2/meteo_file.csv', overwrite = T)

df_meteo <- read.csv("GLM-AED2/meteo_file.csv")
warm_up = df_meteo %>%
  mutate(year = year(Date)) %>%
  filter(year < 2000) %>%
  select(-year)

start <- as.POSIXct("1990-01-01 00:00:00")
end <- as.POSIXct("1994-12-31 23:00:00")
warmup_period <- seq(from=start, by=3600, to=end)

warm_up$Date = warmup_period

write.csv(x = rbind(warm_up, df_meteo), file = "GLM-AED2/meteo_file.csv", quote = F, row.names = F)

eg_nml <- read_nml(nml_file = file.path(nml_file))
eg_nml <- set_nml(eg_nml, 'start', "1990-01-01 00:00:00")

write_nml(eg_nml, file = nml_file)

file.copy(from = "boundaryconditions/inflow.csv", to = 'GLM-AED2/inflow.csv')
file.copy(from = "boundaryconditions/outflow.csv", to = 'GLM-AED2/outflow.csv')

inflow = read.csv("GLM-AED2/inflow.csv")
warm_up = inflow %>%
  mutate(year = year(Time)) %>%
  filter(year < 2000) %>%
  select(-year)

start <- as.POSIXct("1990-01-01")
end <- as.POSIXct("1994-12-31")
warmup_period <- as.Date(seq(from=start, by=24 * 3600, to=end))

warm_up$Time = warmup_period

write.csv(x = rbind(warm_up, inflow), file = "GLM-AED2/inflow.csv", quote = F, row.names = F)

outflow = read.csv("GLM-AED2/outflow.csv")
warm_up = outflow %>%
  mutate(year = year(time)) %>%
  filter(year < 2000) %>%
  select(-year)

start <- as.POSIXct("1990-01-01")
end <- as.POSIXct("1994-12-31")
warmup_period <- as.Date(seq(from=start, by=24 * 3600, to=end))

warm_up$time = warmup_period

ggplot(rbind(warm_up, outflow)) +
  geom_line(aes(time, FLOW))

write.csv(x = rbind(warm_up, outflow), file = "GLM-AED2/outflow.csv", quote = F, row.names = F)

eg_nml <- read_nml(nml_file = file.path(nml_file))
eg_nml <- set_nml(eg_nml, 'num_inflows', 1)
eg_nml <- set_nml(eg_nml, 'inflow_fl', "inflow.csv")
eg_nml <- set_nml(eg_nml, 'inflow_varnum', length(inflow))
eg_nml <- set_nml(eg_nml, 'inflow_vars', colnames(inflow))

eg_nml <- set_nml(eg_nml, 'num_outlet',1)
eg_nml <- set_nml(eg_nml, 'outl_elvs',257)
eg_nml <- set_nml(eg_nml, 'outflow_fl', "outflow.csv")

write_nml(eg_nml, file = nml_file)

# run it again, GLM
met <- read.csv("GLM-AED2/meteo_file.csv")
infl <- read.csv("GLM-AED2/inflow.csv")
outfl <- read.csv("GLM-AED2/outflow.csv")

m.met = reshape2::melt(met, by = 'Date')

ggplot(m.met) +
  geom_line(aes(as.Date(Date), value)) + facet_wrap(~ variable, scales = 'free')

ggplot() +
  geom_line(data = infl, aes(as.Date(Time), FLOW, col = 'in')) +
  geom_line(data = outfl, aes(as.Date(time), FLOW, col = 'out'))

out_file <- file.path("GLM-AED2/", "output","output.nc")
GLM3r::run_glm('GLM-AED2/')

water_height <- get_surface_height(file = out_file)
ggplot(water_height, aes(DateTime, surface_height)) +
  geom_line() +
  ggtitle('Surface water level') +
  xlab(label = '') + ylab(label = 'Water level (m)') +
  theme_minimal()

glmtools::plot_var(nc_file = 'GLM-AED2/output/output.nc', var_name = 'temp', reference = 'surface')
glmtools::plot_var(nc_file = 'GLM-AED2/output/output.nc', var_name = 'PHY_TCHLA', reference = 'surface')
glmtools::plot_var(nc_file = 'GLM-AED2/output/output.nc', var_name = 'OXY_oxy', reference = 'surface')

# work on calibration algorithm
nml_file <- file.path("GLM-AED2/", 'glm3.nml')
eg_nml <- read_nml(nml_file = file.path(nml_file))
out_file <- file.path("GLM-AED2/", "output","output.nc")

var = c('temp', 'OXY_oxy' ,'NIT_nit','NIT_amm', "PHS_frp", 'SIL_rsi', "OGM_doc", 'PHY_TCHLA')         # variable to which we apply the calibration procedure
path = "GLM-AED2/"      # simulation path/folder
nml_file = nml_file  # path of the nml configuration file that you want to calibrate on
glm_file = nml_file # # path of the gml configuration file
# which parameter do you want to calibrate? a sensitivity analysis helps
calib_setup <- data.frame('pars' = as.character(c('wind_factor','lw_factor','coef_mix_hyp',
                                                  'Fsed_oxy', 'Ksed_oxy', 'Kdom_minerl', 'Rdom_minerl',
                                                  'Fsed_nit', 'Ksed_nit', 'Knitrif', 'Fsed_amm',
                                                  'Fsed_frp',
                                                  'Fsed_rsi',
                                                  'R_growth', 'R_growth', 'R_growth'
                                                  )),
                          'lb' = NA,
                          'ub' = NA,
                          'x0' = c(1,1,0.5, -150, 50, 60, 0.02, -10, 10, 78.1, 30, 0.5, 18, 3, 1, 3),
                          'type' = c('glm', 'glm', 'glm', 'aed','aed','aed','aed','aed','aed','aed','aed','aed','aed',
                                     "phyto", 'phyto', 'phyto'))
calib_setup$lb = calib_setup$x0 * 0.7
calib_setup$ub = calib_setup$x0 * 1.3
print(calib_setup)

glmcmd = NULL        # command to be used, default applies the GLM3r function
# glmcmd = '/Users/robertladwig/Documents/AquaticEcoDynamics_gfort/GLM/glm'        # custom path to executable
# Optional variables
first.attempt = TRUE # if TRUE, deletes all local csv-files that stores the 
#outcome of previous calibration runs
period = list('calibration' = list('start' = get_nml_value(eg_nml, 'start'),
                                   'end' = '2004-12-31 23:00:00'),
              validation = list('start' = '2004-12-31 23:00:00',
                                   'end' = get_nml_value(eg_nml, 'stop'))) # define a period for the calibration, 
# this supports a split-sample calibration (e.g. calibration and validation period)
# the ratio value is the ratio of calibration period to validation period
print(period)
scaling = TRUE       # scaling of the variables in a space of [0,10]; TRUE for CMA-ES
verbose = TRUE
method = 'CMA-ES'    # optimization method, choose either `CMA-ES` or `Nelder-Mead`
metric = 'NRMSE'      # objective function to be minimized, here the root-mean square error
target.fit = 0.1     # refers to a target fit of 2.0 degrees Celsius (stops when RMSE is below that)
target.iter = 20    # refers to a maximum run of 20 calibration iterations (stops after that many runs)
plotting = TRUE      # if TRUE, script will automatically save the contour plots
output = out_file    # path of the output file
field_file = field_data # path of the field data
conversion.factor = 1 # conversion factor for the output, e.g. 1 for water temp.

