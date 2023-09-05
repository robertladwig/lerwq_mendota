library(ggplot2)
library(tidyverse)
library(glmtools)

df = read.csv("GLM-AED2/calib_results_nse.csv")

df = df %>% mutate(flag = temp + OXY_oxy + NIT_nit + NIT_amm + PHS_frp + SIL_rsi + OGM_doc +
                     PHY_TCHLA) %>%
  dplyr::filter(flag > -1e8)
# fits_rm = df[-c(which.max(df$NRMSE)), ]
# while(TRUE){
#   df = read.csv("GLM-AED2/calib_results_nse.csv")
#   
#   df = df %>% mutate(flag = temp + OXY_oxy + NIT_nit + NIT_amm + PHS_frp + SIL_rsi + OGM_doc +
#                        PHY_TCHLA) %>%
#     dplyr::filter(flag > -1e8)
#   ggplot2::ggplot(reshape2::melt(df, id.vars = c('time', 'NRMSE'))) +
#     geom_point(aes(as.POSIXct(time), value, col = NRMSE)) + 
#     geom_line(aes(as.POSIXct(time), value, col = NRMSE)) + 
#     theme(legend.position = 'bottom') + xlab('time') + ylab('NSE')+
#     scale_colour_continuous(type = rev("viridis")) +
#     facet_wrap(~variable, scales = 'free')
# }
ggplot2::ggplot(reshape2::melt(df, id.vars = c('time', 'NRMSE'))) +
  geom_point(aes(as.POSIXct(time), value, col = NRMSE)) + 
  geom_line(aes(as.POSIXct(time), value, col = NRMSE)) + 
  theme(legend.position = 'bottom') + xlab('time') + ylab('NSE')+
  scale_colour_continuous(type = rev("viridis")) +
  facet_wrap(~variable, scales = 'free')

df[which.min(df$NRMSE),]

df.head=df %>%
  arrange(NRMSE)
head(df.head, n = 10)


df = read.csv("GLM-AED2/calib_results_nrmse.csv")

df = df %>% mutate(flag = temp + OXY_oxy + NIT_nit + NIT_amm + PHS_frp + SIL_rsi + OGM_doc +
                     PHY_TCHLA) %>%
  dplyr::filter(flag < 300)

df.obs = read.csv('trainingData/MendotaData_observedGLM_checked.csv')

eg_nml = read_nml(paste0('GLM-AED2/glm3.nml'))
variable = 'OGM_doc'
observed <- df.obs %>%
  filter(datetime >= get_nml_value(eg_nml, 'start') & datetime <= get_nml_value(eg_nml, 'stop') ) %>%
  select(datetime, depth, all_of(variable)) %>%
  mutate(datetime = as.POSIXct(paste0(as.Date(datetime),' 12:00:00')))
model_output = get_var(file = paste0('GLM-AED2/output/output.nc'), var_name = variable, reference = 'surface', z_out = seq(0, 25, 0.1), t_out = unique(observed$datetime))
colnames(model_output)[2:ncol(model_output)] = as.numeric(gsub("[^0-9.]", "", colnames(model_output)[2:ncol(model_output)]))
model_output$DateTime =  unique(observed$datetime)

mod <- reshape2::melt(model_output, id.vars = 1) 
mod$variable = as.numeric(as.character(mod$variable))

mod = mod %>%
  rename(datetime = DateTime, depth = variable, modeled = value) 




ggplot2::ggplot(reshape2::melt(df, id.vars = c('time', 'NRMSE'))) +
  geom_point(aes(as.POSIXct(time), value)) +   
  theme(legend.position = 'bottom') + xlab('time') + ylab('NRMSE')+
  facet_wrap(~variable, scales = 'free')


df = read.csv("GLM-AED2/calib_results_loglike.csv")

df = df %>% mutate(flag = temp + OXY_oxy + NIT_nit + NIT_amm + PHS_frp + SIL_rsi + OGM_doc +
                     PHY_TCHLA) %>%
  dplyr::filter(flag > -2e6)

ggplot2::ggplot(reshape2::melt(df, id.vars = c('time', 'NRMSE'))) +
  geom_point(aes(as.POSIXct(time), value)) +   
  theme(legend.position = 'bottom') + xlab('time') + ylab('NRMSE')+
  facet_wrap(~variable, scales = 'free')

glmtools::plot_var(nc_file = 'GLM-AED2/output/output.nc', var_name = 'PHY_TCHLA', reference = 'surface')
