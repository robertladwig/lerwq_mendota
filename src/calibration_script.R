calibrate_glm <- function(var = 'temp',
                             path,
                             field_file,
                             aed_file = 'aed2',
                             glm_file = 'glm3',
                             phyto_file = 'aed2_phyto_pars',
                             calib_setup,
                             glmcmd = NULL,
                             period,
                             scaling = TRUE,
                             verbose = TRUE,
                             metric = 'NRMSE',
                             target.fit = 1.5,
                             target.iter = 100,
                             plotting = TRUE,
                             output,
                          parallelMode = F){
  
  # Development message 
  message('Starting new calibration routine.')
 readline(prompt = "This will overwrite any nml-files in the current directory, press any button to continue.");
  
    if (file.exists(paste0(path,'/calib_results_nse.csv'))){
      file.remove(paste0(path,'/calib_results_nse.csv'))
    }
 if (file.exists(paste0(path,'/calib_results_nrmse.csv'))){
   file.remove(paste0(path,'/calib_results_nrmse.csv'))
 }
 if (file.exists(paste0(path,'/calib_results_loglike.csv'))){
   file.remove(paste0(path,'/calib_results_loglike.csv'))
 }
    if (file.exists(paste0(path,'/calib_par.csv'))){
      file.remove(paste0(path,'/calib_par.csv'))
    }

  
  if(file.exists(paste0(path, glm_file,'-template.nml'))){
    file.copy(paste0(path, glm_file,'-template.nml'), paste0(path, glm_file, '.nml'), overwrite = T)
  } 
 
 if(file.exists(paste0(path, aed_file,'-template.nml'))){
   file.copy(paste0(path, aed_file,'-template.nml'), paste0(path, aed_file, '.nml'), overwrite = T)
 } 
 
 if(file.exists(paste0(path, phyto_file,'-template.nml'))){
   file.copy(paste0(path, phyto_file,'-template.nml'), paste0(path, phyto_file, '.nml'), overwrite = T)
 } 
 


  variable <- var
  
  if (scaling){
    parameters <- (calib_setup$x0 - calib_setup$lb) *10 /(calib_setup$ub-calib_setup$lb) 
  }
  
  eg_nml <- read_nml(nml_file = paste0(path,glm_file,'.nml'))
  eg_nml <- set_nml(eg_nml, 'start', period$calibration$start)
  eg_nml <- set_nml(eg_nml, 'stop', period$calibration$end)
  write_nml(eg_nml, file = paste0(path,glm_file,'.nml'))

  # glmOPT <- pureCMAES(par = parameters, fun = run_glm_optim, lower = rep(0,length(parameters)),
  #                       upper = rep(10,length(parameters)),
  #                       sigma = 0.5,
  #                       stopfitness = target.fit,
  #                       stopeval = target.iter,
  #                       glmcmd = glmcmd, var = var,
  #                       scaling = scaling, metric = metric, verbose = verbose,
  #                       calib_setup = calib_setup, path = path, field_file = field_file,
  #                     phyto_file = phyto_file,
  #                     glm_file = glm_file,
  #                     aed_file = aed_file)
  if (parallelMode == F){
    glmOPT <- DEoptim(fn = run_glm_optim, lower = calib_setup$lb,
                      upper = calib_setup$ub,
                      control = list(itermax = target.iter),
                      glmcmd = glmcmd, var = var,
                      scaling = scaling, metric = metric, verbose = verbose,
                      calib_setup = calib_setup, path = path, field_file = field_file,
                      phyto_file = phyto_file,
                      glm_file = glm_file,
                      aed_file = aed_file)
  } else {
    glmOPT <- DEoptim(fn = run_glm_optim_parallel, lower = calib_setup$lb,
                      upper = calib_setup$ub,
                      control = list(itermax = target.iter, parallelType=1,
                                     packages = c('glmtools', 'dplyr'),
                                     parVar = c('glmcmd', 'var', 'scaling', 'metric', 'verbose',
                                                'calib_setup', 'path', 'field_file', 'phyto_file', 'glm_file', 'aed_file')),
                      glmcmd = glmcmd, var = var,
                      scaling = scaling, metric = metric, verbose = verbose,
                      calib_setup = calib_setup, path = path, field_file = field_file,
                      phyto_file = phyto_file,
                      glm_file = glm_file,
                      aed_file = aed_file)
  }


  # glmOPT <- cma_es(par = parameters, fn = run_glm_optim, lower = rep(0,length(parameters)), 
  #                     upper = rep(10,length(parameters)), 
  #                     #sigma = 0.5, 
  #                     control = list(
  #                     stopfitness = target.fit, 
  #                     maxit = target.iter,
  #                     vectorize = T),
  #                     glmcmd = glmcmd, var = var,
  #                     scaling = scaling, metric = metric, verbose = verbose,
  #                     calib_setup = calib_setup, path = path, field_file = field_file,
  #                     phyto_file = phyto_file,
  #                     glm_file = glm_file,
  #                     aed_file = aed_file)

  # 
  # 
  # 
  # 
  # 
  # 
  # glmFUN_2d(p = glmOPT$xmin, nml_file = nml_file, glmcmd = glmcmd, var = var, scaling, metric, verbose, ub = ub, lb = lb, path = path, pars = pars, obs = obs,
  #           conversion.factor = conversion.factor, additional.var = additional.var)
  # 
  # # glmFUN_2d(glmOPT$xmin, glmcmd, scaling, metric, verbose)
  # calib <- read.csv(paste0(path,'/calib_results_',metric,'_',var,'.csv'))
  # eval(parse(text = paste0('best_par <- calib[which.min(calib$',metric,'),]')))
  # write.csv(best_par, paste0(path,'/calib_par_',var,'.csv'), row.names = F, quote = F)
  # best_par <- read.csv(paste0(path,'/calib_par_',var,'.csv'))
  # 
  # #Input best parameter set
  # nml <- read_nml(nml_file = nml_file)
  # check_duplicates <- c()
  # for (i in 2:(ncol(best_par)-2)){
  #   string1 <- colnames(best_par)[i]
  #   for (j in (i+1):(ncol(best_par)-1)){
  #     string2 <- colnames(best_par)[j]
  #     if (substr(string1,1,floor(nchar(string1)*9/10)) == substr(string2,1,floor(nchar(string1)*9/10))){
  #       check_duplicates <- append(check_duplicates, i)
  #       check_duplicates <- append(check_duplicates, j)
  #     }
  #   }
  # }
  # checked <- 2:(ncol(best_par)-1)
  # for (i in 1:length(check_duplicates)){
  #   checked <- checked[!checked == check_duplicates[i]]
  # }
  # 
  # for(i in checked){
  #   nml <- set_nml(nml,colnames(best_par)[i],best_par[1,i])
  # }
  # 
  # if (!is.null(check_duplicates)){
  #   check_duplicates <- matrix(check_duplicates,ncol=2, byrow = TRUE)
  #   find_dupl_groups <- list()
  #   it <- 1
  #   for (ii in 1:nrow(check_duplicates)){
  #     if (ii == 1){
  #       find_dupl_groups[[it]] <- (check_duplicates[ii,])
  #     } else {
  #       if (ii > 1){
  #         if (any(check_duplicates[ii,] %in% find_dupl_groups[[it]])){
  #           place <- !(check_duplicates[ii,] %in% find_dupl_groups[[it]])
  #           find_dupl_groups[[it]]<-append(find_dupl_groups[[it]], check_duplicates[ii, which(place == TRUE)])
  #         } else {
  #           it <- it+1
  #           find_dupl_groups[[it]] <- (check_duplicates[ii,])
  #         } 
  #       }
  #     } }
  #   
  #   
  #   for (i in 1:length(find_dupl_groups)){
  #     nml <- set_nml(nml,
  #                    gsub('[.]','%',colnames(best_par))[find_dupl_groups[[i]][1]],
  #                    as.numeric(best_par[find_dupl_groups[[i]]]))
  #     #print(gsub('[.]','%',colnames(best_par))[find_dupl_groups[[i]][1]])
  #     #print(as.numeric(best_par[unlist(find_dupl_groups[[i]])]))
  #   }
  # } else {
  #   for(i in 2:(ncol(best_par)-1)){
  #     nml <- set_nml(nml,colnames(best_par)[i],best_par[1,i])
  #   }
  # }
  # 
  # write_nml(nml, file = nml_file)
  # 
  # #Run GLM
  # run_glmcmd(glmcmd, path, verbose)
  # 
  # g1 <- diag.plots(
  #   mod2obs(paste0(path,'/output/output.nc'), obs, reference = 'surface', var,
  #           additional.var = additional.var), 
  #   obs, ggplot=F)
  # 
  # ggsave(filename = paste0(path,'/diagnostics_',method,'_',var,'.png'), plot = g1, 
  #        dpi = 300,width = 384,height = 216, units = 'mm')
  # 
  # 
  # # loads all iterations
  # results <- read.csv(paste0(path,'/calib_results_RMSE_',var,'.csv'))
  # results$DateTime <- as.POSIXct(results$DateTime)
  # 
  # g1 <- ggplot(results, 
  #              aes(nrow(results):1, .data$RMSE)) +
  #   geom_point() +
  #   geom_smooth(se = FALSE, method = "gam", formula = y ~ s(x), color = 'lightblue4') +
  #   theme_bw() + xlab('Iterations') +
  #   theme(text = element_text(size = 10), axis.text.x = element_text(angle = 90, hjust = 1))
  # 
  # if (plotting == TRUE) {
  #   ggsave(filename = paste0(path,'/optim_',method,'_',var,'.png'), g1, dpi = 300,width = 384,height = 216, units = 'mm')
  # }
  # 
  # print(g1)
  # 
  # # Compare simulated with observed data
  # if (var == 'temp'){
  #   temp_rmse1 <- compare_to_field(output, field_file = field_file, 
  #                                  metric = 'water.temperature', as_value = FALSE, precision= 'hours')
  # } else {
  #   mod <- mod2obs(mod_nc = paste0(path,'/output/output.nc'), obs = obs, reference = 'surface',var = var,
  #                  additional.var = additional.var)
  #   mod[,3] = mod[,3] * conversion.factor
  #   temp_rmse1 = get_rmse(mod,obs)
  # }
  # 
  # plot.heat = plot_var_compare(nc_file = output, field_file = field_file, var_name = var,
  #                              resample = TRUE, conversion = conversion.factor) + 
  #   labs(title = 'Calibration Period')
  # print(plot.heat)
  # if (plotting == TRUE){
  #   ggsave(plot = plot.heat, paste0(path,'/calib_',method,'_',var,'_',metric,round(temp_rmse1,2),'.png'))
  # }
  # 
  # # Check the model fit during the validation period
  # init.temps <- read_nml(glm_file)$init_profiles$the_temps
  # get_calib_init_validation(file = glm_file, output = output)
  # nml <- read_nml(glm_file)
  # nml <- set_nml(nml, arg_list = period$validation)
  # write_nml(nml,glm_file)
  # 
  # run_glmcmd(glmcmd, path, verbose)
  # if (var == 'temp'){
  #   temp_rmse2 <- compare_to_field(output, field_file = field_file, 
  #                                  metric = 'water.temperature', as_value = FALSE, precision= 'hours')
  # } else {
  #   mod <- mod2obs(mod_nc = paste0(path,'/output/output.nc'), obs = obs, reference = 'surface',var = var,
  #                  additional.var = additional.var)
  #   mod[,3] = mod[,3] * conversion.factor
  #   temp_rmse2 = get_rmse(mod,obs)
  # }
  # 
  # plot.heat = plot_var_compare(nc_file = output, field_file = field_file, var_name = var,
  #                              resample = TRUE, conversion = conversion.factor) + 
  #   labs(title = 'Validation Period')
  # print(plot.heat)
  # if (plotting == TRUE){
  #   ggsave(plot = plot.heat, paste0(path,'/valid_',method,'_',var,'_',metric,round(temp_rmse2,2),'.png'))
  # }
  # 
  # 
  # # check the model fit during the whole time period
  # nml <- read_nml(glm_file)
  # total.list <- period$total
  # total.list[['the_temps']] <- init.temps
  # nml <- set_nml(nml, arg_list =total.list)
  # write_nml(nml,glm_file)
  # 
  # run_glmcmd(glmcmd, path, verbose)
  # if (var == 'temp'){
  #   temp_rmse3 <- compare_to_field(output, field_file = field_file, 
  #                                  metric = 'water.temperature', as_value = FALSE, precision= 'hours')
  # } else {
  #   mod <- mod2obs(mod_nc = paste0(path,'/output/output.nc'), obs = obs, reference = 'surface',var = var,
  #                  additional.var = additional.var)
  #   mod[,3] = mod[,3] * conversion.factor
  #   temp_rmse3 = get_rmse(mod,obs)
  # }
  # 
  # plot.heat = plot_var_compare(nc_file = output, field_file = field_file, var_name = var,
  #                              resample = TRUE, conversion = conversion.factor) + 
  #   labs(title = 'Total Time Period')
  # print(plot.heat)
  # if (plotting == TRUE){
  #   ggsave(plot = plot.heat, paste0(path,'/total_',method,'_',var,'_',metric,round(temp_rmse3,2),'.png'))
  # }
  # 
  # 
  # # print a matrix of our constrained variable space, the initial value and the calibrated value
  # calibrated_results <- cbind(calib_setup, 'calibrated' = as.numeric(round(results[1,2:(ncol(results)-1)],4)))
  # 
  # print(paste('calibration:',round(temp_rmse1,2),' RMSE'))
  # print(paste('validation:',round(temp_rmse2,2),' RMSE'))
  # print(paste('total time period:',round(temp_rmse3,2),' RMSE'))
  # print(calibrated_results)
  # return(calibrated_results)
  
  return(glmOPT)
}

run_glm_optim <- function(p, glmcmd, var, scaling, metric, verbose, calib_setup, path, field_file, phyto_file = phyto_file,
                          glm_file = glm_file,
                          aed_file = aed_file){

  if (scaling == TRUE){
    p <- wrapper_scales(p, calib_setup$lb, calib_setup$ub)
  }

  
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
  

  error <- try(run_glmcmd(glmcmd, path, verbose), silent = T)
  
  if (error != 0){
    fit = 999999
  } else{
  
  eg_nml = read_nml(paste0(path, glm_file,'.nml'))
  
  all_nrmse = c()
  all_nse = c()
  all_likelihood = c()
  
  for (variable in var){
    observed <- field_file %>%
      filter(datetime >= get_nml_value(eg_nml, 'start') & datetime <= get_nml_value(eg_nml, 'stop') ) %>%
      select(datetime, depth, all_of(variable)) %>%
      mutate(datetime = as.POSIXct(paste0(as.Date(datetime),' 12:00:00')))
    
    error = try(get_var(file = paste0(path, 'output/output.nc'), var_name = variable, reference = 'surface', z_out = seq(0, 25, 0.1)), silent = T)
    
    if (is.data.frame(error)){
      model_output = get_var(file = paste0(path, 'output/output.nc'), var_name = variable, reference = 'surface', z_out = seq(0, 25, 0.1), t_out = unique(observed$datetime))
      
      colnames(model_output)[2:ncol(model_output)] = as.numeric(gsub("[^0-9.]", "", colnames(model_output)[2:ncol(model_output)]))
      
      model_output$DateTime =  unique(observed$datetime)
      
      mod <- reshape2::melt(model_output, id.vars = 1) 
      mod$variable = as.numeric(as.character(mod$variable))
      
      mod = mod %>%
        rename(datetime = DateTime, depth = variable, modeled = value) 
      
      
    } else {
      mod = observed
      colnames(mod)[3] = 'modeled'
      mod$modeled = -99999
    }
    

    
    colnames(observed)[3] = 'observed'
    
    df = merge(observed, mod, by = c('datetime', 'depth'), all = T)
    
    df = df %>% 
      mutate(residual = (observed - modeled)^2)
    
    nrmse = sqrt(sum(df$residual, na.rm = T)/length(na.omit(df$observed))) / (max(df$observed, na.rm = T) - min(df$observed, na.rm = T))
    
    nse = 1 - sum(df$residual, na.rm = T)/sum((df$observed - mean(df$observed, na.rm = T))^2, na.rm = T)
    
    lnlikelihood = sum(dnorm(df$observed, mean = df$modeled, log = TRUE), na.rm = T)
    
    all_nrmse <- append(all_nrmse, nrmse)
    all_nse = append(all_nse, nse)
    all_likelihood = append(all_likelihood, lnlikelihood)
    
    
  }
  
  fit = (sum(all_nrmse))
  }
  
  dat = (matrix(all_nrmse, nrow= 1))
  colnames(dat) = var
  dat.df = as.data.frame(cbind(data.frame('time' = format(Sys.time())), dat,data.frame('NRMSE' = fit)))

  if(!file.exists(paste0(path,'/calib_results_nrmse.csv'))){
    df = dat.df
    write.csv(dat.df,paste0(path,'/calib_results_nrmse.csv'), row.names = F, quote = F)
  }else{
    df = read.csv(paste0(path,'/calib_results_nrmse.csv'))
    df = rbind.data.frame(dat.df, df)
    write.csv(df,paste0(path,'/calib_results_nrmse.csv'), row.names = F, quote = F)
  }
  g1 = ggplot(reshape2::melt(df, id.vars = c('time', 'NRMSE'))) +
    geom_point(aes(NRMSE, value, col  =as.numeric(as.POSIXct(time)))) + ylab('NRMSE') +  xlab('NRMSE') +
    theme(legend.position="bottom") +
    facet_wrap(~variable, scales = 'free')
  ggsave(g1, filename= paste0(path,'/nrmse.png'), dpi = 300, width =20, height = 20, units = 'cm')
  
  dat = (matrix(all_nse, nrow= 1))
  colnames(dat) = var
  dat.df = as.data.frame(cbind(data.frame('time' = format(Sys.time())), dat,data.frame('NRMSE' = fit)))
  
  if(!file.exists(paste0(path,'/calib_results_nse.csv'))){
    df = dat.df
    write.csv(dat.df,paste0(path,'/calib_results_nse.csv'), row.names = F, quote = F)
  }else{
    df = read.csv(paste0(path,'/calib_results_nse.csv'))
    df = rbind.data.frame(dat.df, df)
    write.csv(df,paste0(path,'/calib_results_nse.csv'), row.names = F, quote = F)
  }
  
  g1 = ggplot(reshape2::melt(df, id.vars = c('time', 'NRMSE'))) +
    geom_point(aes(NRMSE, value, col  =as.numeric(as.POSIXct(time)))) + ylab('NSE') +  xlab('NRMSE') +
    theme(legend.position="bottom") +
    facet_wrap(~variable, scales = 'free')
  ggsave(g1, filename= paste0(path,'/nse.png'), dpi = 300, width =20, height = 20, units = 'cm')
  
  dat = (matrix(all_likelihood, nrow= 1))
  colnames(dat) = var
  dat.df = as.data.frame(cbind(data.frame('time' = format(Sys.time())), dat,data.frame('NRMSE' = fit)))
  
  if(!file.exists(paste0(path,'/calib_results_loglike.csv'))){
    df = dat.df
    write.csv(dat.df,paste0(path,'/calib_results_loglike.csv'), row.names = F, quote = F)
  }else{
    df = read.csv(paste0(path,'/calib_results_loglike.csv'))
    df = rbind.data.frame(dat.df, df)
    write.csv(df,paste0(path,'/calib_results_loglike.csv'), row.names = F, quote = F)
  }
  
  

  # g1 = ggplot(reshape2::melt(df, id.vars = c('time', 'NRMSE'))) +
  #   geom_point(aes(NRMSE, value, col  =variable)) + ylab('NSE') +  xlab('NRMSE') +theme(legend.position="bottom")
  g1 = ggplot(reshape2::melt(df, id.vars = c('time', 'NRMSE'))) +
    geom_point(aes(NRMSE, value, col  =as.numeric(as.POSIXct(time)))) + ylab('logLik') +  xlab('NRMSE') +
    theme(legend.position="bottom") +
    facet_wrap(~variable, scales = 'free')
  ggsave(g1, filename= paste0(path,'/logLik.png'), dpi = 300, width =20, height = 20, units = 'cm')
  
  dat_parm = matrix(p, nrow = 1)
  colnames(dat_parm) = calib_setup$pars
  
  dat.df_parm = as.data.frame(cbind(data.frame('time' = format(Sys.time())), dat_parm, data.frame('NRMSE' = fit)))
  
  if(!file.exists(paste0(path,'/calib_par.csv'))){
    write.csv(dat.df_parm,paste0(path,'/calib_par.csv'), row.names = F, quote = F)
  }else{
    df = read.csv(paste0(path,'/calib_par.csv'))
    colnames(dat.df_parm) = colnames(df)
    df = rbind.data.frame(dat.df_parm, df)
    write.csv(df,paste0(path,'/calib_par.csv'), row.names = F, quote = F)
  }

  
  print(paste("NRMSE:", round(fit,3)))
  return(fit)
}

wrapper_scales <- function(x, lb, ub){
  y <-  lb+(ub-lb)/(10)*(x)
  return(y)
}


run_glmcmd <- function(glmcmd, path, verbose){
  if (is.null(glmcmd)){
    run_glm(path, verbose = verbose)
  } else{
    system(glmcmd,ignore.stdout=TRUE)
  }
}


run_glm_optim_parallel <- function(p, glmcmd, var, scaling, metric, verbose, calib_setup, path, field_file, phyto_file = phyto_file,
                          glm_file = glm_file,
                          aed_file = aed_file){
  
  wrapper_scales <- function(x, lb, ub){
    y <-  lb+(ub-lb)/(10)*(x)
    return(y)
  }
  
  
  run_glmcmd <- function(glmcmd, path, verbose){
    if (is.null(glmcmd)){
      run_glm(path, verbose = verbose)
    } else{
      system(glmcmd,ignore.stdout=TRUE)
    }
  }
  
  
  if (scaling == TRUE){
    p <- wrapper_scales(p, calib_setup$lb, calib_setup$ub)
  }
  
  temp_folder = paste0(tempfile(),'/')
  
  dir.create(temp_folder)
  
  file.copy(from = paste0(path,glm_file,'.nml'), to = paste0(temp_folder,glm_file,'.nml'))
  file.copy(from = paste0(path,aed_file,'.nml'), to = paste0(temp_folder,aed_file,'.nml'))
  file.copy(from = paste0(path,phyto_file,'.nml'), to = paste0(temp_folder,phyto_file,'.nml'))
  file.copy(from = paste0(path,'inflow.csv'), to = paste0(temp_folder,'inflow.csv'))
  file.copy(from = paste0(path,'outflow.csv'), to = paste0(temp_folder,'outflow.csv'))
  file.copy(from = paste0(path,'meteo_file.csv'), to = paste0(temp_folder,'meteo_file.csv'))
  
  for (nml_file in unique(calib_setup$file)){
    eg_nml <- read_nml(paste0(temp_folder,nml_file))
    
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
    
    write_nml(eg_nml, file = paste0(temp_folder, nml_file))
    
  }
  
  
  error <- try(run_glmcmd(glmcmd, temp_folder, verbose), silent = T)
  
  if (error != 0){
    fit = 999999
  } else{
    
    eg_nml = read_nml(paste0(temp_folder, glm_file,'.nml'))
    
    all_nrmse = c()
    all_nse = c()
    all_likelihood = c()
    
    for (variable in var){
      observed <- field_file %>%
        filter(datetime >= get_nml_value(eg_nml, 'start') & datetime <= get_nml_value(eg_nml, 'stop') ) %>%
        select(datetime, depth, all_of(variable)) %>%
        mutate(datetime = as.POSIXct(paste0(as.Date(datetime),' 12:00:00')))
      
      error = try(get_var(file = paste0(temp_folder, 'output/output.nc'), var_name = variable, reference = 'surface', z_out = seq(0, 25, 0.1)), silent = T)
      
      if (is.data.frame(error)){
        model_output = get_var(file = paste0(temp_folder, 'output/output.nc'), var_name = variable, reference = 'surface', z_out = seq(0, 25, 0.1), t_out = unique(observed$datetime))
        
        colnames(model_output)[2:ncol(model_output)] = as.numeric(gsub("[^0-9.]", "", colnames(model_output)[2:ncol(model_output)]))
        
        model_output$DateTime =  unique(observed$datetime)
        
        mod <- reshape2::melt(model_output, id.vars = 1) 
        mod$variable = as.numeric(as.character(mod$variable))
        
        mod = mod %>%
          rename(datetime = DateTime, depth = variable, modeled = value) 
        
        
      } else {
        mod = observed
        colnames(mod)[3] = 'modeled'
        mod$modeled = -99999
      }
      
      
      
      colnames(observed)[3] = 'observed'
      
      df = merge(observed, mod, by = c('datetime', 'depth'), all = T)
      
      df = df %>% 
        mutate(residual = (observed - modeled)^2)
      
      nrmse = sqrt(sum(df$residual, na.rm = T)/length(na.omit(df$observed))) / (max(df$observed, na.rm = T) - min(df$observed, na.rm = T))
      
      nse = 1 - sum(df$residual, na.rm = T)/sum((df$observed - mean(df$observed, na.rm = T))^2, na.rm = T)
      
      lnlikelihood = sum(dnorm(df$observed, mean = df$modeled, log = TRUE), na.rm = T)
      
      all_nrmse <- append(all_nrmse, nrmse)
      all_nse = append(all_nse, nse)
      all_likelihood = append(all_likelihood, lnlikelihood)
      
      
    }
    
    fit = (sum(all_nrmse))
  }
  
  unlink(temp_folder, recursive = T)
  # 
  # dat = (matrix(all_nrmse, nrow= 1))
  # colnames(dat) = var
  # dat.df = as.data.frame(cbind(data.frame('time' = format(Sys.time())), dat,data.frame('NRMSE' = fit)))
  # 
  # if(!file.exists(paste0(path,'/calib_results_nrmse.csv'))){
  #   df = dat.df
  #   write.csv(dat.df,paste0(path,'/calib_results_nrmse.csv'), row.names = F, quote = F)
  # }else{
  #   df = read.csv(paste0(path,'/calib_results_nrmse.csv'))
  #   df = rbind.data.frame(dat.df, df)
  #   write.csv(df,paste0(path,'/calib_results_nrmse.csv'), row.names = F, quote = F)
  # }
  # g1 = ggplot(reshape2::melt(df, id.vars = c('time', 'NRMSE'))) +
  #   geom_point(aes(NRMSE, value, col  =as.numeric(as.POSIXct(time)))) + ylab('NRMSE') +  xlab('NRMSE') +
  #   theme(legend.position="bottom") +
  #   facet_wrap(~variable, scales = 'free')
  # ggsave(g1, filename= paste0(path,'/nrmse.png'), dpi = 300, width =20, height = 20, units = 'cm')
  # 
  # dat = (matrix(all_nse, nrow= 1))
  # colnames(dat) = var
  # dat.df = as.data.frame(cbind(data.frame('time' = format(Sys.time())), dat,data.frame('NRMSE' = fit)))
  # 
  # if(!file.exists(paste0(path,'/calib_results_nse.csv'))){
  #   df = dat.df
  #   write.csv(dat.df,paste0(path,'/calib_results_nse.csv'), row.names = F, quote = F)
  # }else{
  #   df = read.csv(paste0(path,'/calib_results_nse.csv'))
  #   df = rbind.data.frame(dat.df, df)
  #   write.csv(df,paste0(path,'/calib_results_nse.csv'), row.names = F, quote = F)
  # }
  # 
  # g1 = ggplot(reshape2::melt(df, id.vars = c('time', 'NRMSE'))) +
  #   geom_point(aes(NRMSE, value, col  =as.numeric(as.POSIXct(time)))) + ylab('NSE') +  xlab('NRMSE') +
  #   theme(legend.position="bottom") +
  #   facet_wrap(~variable, scales = 'free')
  # ggsave(g1, filename= paste0(path,'/nse.png'), dpi = 300, width =20, height = 20, units = 'cm')
  # 
  # dat = (matrix(all_likelihood, nrow= 1))
  # colnames(dat) = var
  # dat.df = as.data.frame(cbind(data.frame('time' = format(Sys.time())), dat,data.frame('NRMSE' = fit)))
  # 
  # if(!file.exists(paste0(path,'/calib_results_loglike.csv'))){
  #   df = dat.df
  #   write.csv(dat.df,paste0(path,'/calib_results_loglike.csv'), row.names = F, quote = F)
  # }else{
  #   df = read.csv(paste0(path,'/calib_results_loglike.csv'))
  #   df = rbind.data.frame(dat.df, df)
  #   write.csv(df,paste0(path,'/calib_results_loglike.csv'), row.names = F, quote = F)
  # }
  # 
  # 
  # 
  # # g1 = ggplot(reshape2::melt(df, id.vars = c('time', 'NRMSE'))) +
  # #   geom_point(aes(NRMSE, value, col  =variable)) + ylab('NSE') +  xlab('NRMSE') +theme(legend.position="bottom")
  # g1 = ggplot(reshape2::melt(df, id.vars = c('time', 'NRMSE'))) +
  #   geom_point(aes(NRMSE, value, col  =as.numeric(as.POSIXct(time)))) + ylab('logLik') +  xlab('NRMSE') +
  #   theme(legend.position="bottom") +
  #   facet_wrap(~variable, scales = 'free')
  # ggsave(g1, filename= paste0(path,'/logLik.png'), dpi = 300, width =20, height = 20, units = 'cm')
  # 
  # dat_parm = matrix(p, nrow = 1)
  # colnames(dat_parm) = calib_setup$pars
  # 
  # dat.df_parm = as.data.frame(cbind(data.frame('time' = format(Sys.time())), dat_parm, data.frame('NRMSE' = fit)))
  # 
  # if(!file.exists(paste0(path,'/calib_par.csv'))){
  #   write.csv(dat.df_parm,paste0(path,'/calib_par.csv'), row.names = F, quote = F)
  # }else{
  #   df = read.csv(paste0(path,'/calib_par.csv'))
  #   colnames(dat.df_parm) = colnames(df)
  #   df = rbind.data.frame(dat.df_parm, df)
  #   write.csv(df,paste0(path,'/calib_par.csv'), row.names = F, quote = F)
  # }
  
  
  print(paste("NRMSE:", round(fit,3)))
  return(fit)
}
