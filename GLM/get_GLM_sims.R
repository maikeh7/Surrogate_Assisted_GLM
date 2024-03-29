

# function to make MONTH/DAY/DOY dataframe for merging w/ GLM sims
make_ymd = function(){
  month_table = data.frame(MONTH = 1:12, freq = c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31))
  MONTH = rep(1:12, month_table$freq)
  DAY = unlist(Map(function(start_num, stop_num) seq(start_num,stop_num ),1,month_table$freq ))
  DOY = 1:366
  ymd = data.frame(MONTH = MONTH, DAY = DAY, DOY = DOY)
}

# nsave should always be set to 24--> save one output of GLM at 00UTC. If it is NOT set to 24, 
# program will terminate with an error message
get_GLM_sims = function(curr_results_dir, nsave = 24, coeff_list, initial_temps, start_date = NULL){
  Kw = coeff_list$Kw
  coeff_mix_hyp = coeff_list$coeff_mix_hyp
  sw_factor = coeff_list$sw_factor
  lw_factor = coeff_list$lw_factor
  sed_temp_mean = coeff_list$sed_temp_mean
 # initial_temps = inputs_param_list$initial_temps
  
  ymd = make_ymd()
  df = data.frame()
  # only use the 30 ensemble members
  for (i in 1:31){
    GLM_sim_dir = paste0("GLM_sim_", i)
    output_file = paste0(file.path(curr_results_dir, GLM_sim_dir), "/output/output.nc", sep = "")
    nml_file = read_nml(nml_file = paste0(file.path(curr_results_dir, GLM_sim_dir), "/glm3.nml"))
    surface_temp = glmtools::get_var(file = output_file,
                           var_name = "temp",
                           reference = "surface",
                           z_out = nml_file$init_profiles$the_depths)
    surface_melt = reshape2::melt(surface_temp, id.vars = "DateTime")
    colnames(surface_melt)[2:3] = c("depth_int", "Temp_C")
    surface_melt$ensemble_number = i
    x = surface_melt$depth
    levels(x) = as.character(0:9) 
    x_char = as.character(x)
    x_int = as.numeric(x_char)
    surface_melt$depth_int = x_int
    date_time = surface_melt$DateTime
    Date_char = gsub("\\s+\\d{1,2}:\\d{1,2}:\\d{1,2}", "", date_time)
    Date_char = strsplit(Date_char, "-")
    dates_df = as.data.frame(do.call("rbind", Date_char))
    names(dates_df) = c("YEAR", "MONTH", "DAY")
    dates_df$YEAR = as.numeric(dates_df$YEAR)
    dates_df$MONTH = as.numeric(dates_df$MONTH)
    dates_df$DAY = as.numeric(dates_df$DAY)
    surface_melt = cbind(surface_melt, dates_df)
    surface_melt = dplyr::right_join(ymd, surface_melt, by = c("MONTH", "DAY"))
  
    df = rbind(df, surface_melt)
  }
  
  # nsave == 24 means GLM will ONLY SAVE one output for every 24 hours, so 00UTC
  # this summarise statement for averaging will not change the data--it's just for formatting
  # you should not be running GLM with nsave == 1 
  if (nsave == 24){
    sims = df %>% group_by(ensemble_number, YEAR, MONTH, DAY, depth_int) %>%
      summarise(Temp_C_00UTC = mean(Temp_C))
    sims = dplyr::right_join(ymd, sims, by = c("MONTH", "DAY"))
    sims_wide = sims #%>%
    #tidyr::pivot_wider(names_from = ensemble_number, values_from = mean_Temp_C) 
    #colnames(sim_aves_wide)[6:36] = paste0("ensemble_member_",1:31)
    
    sims_wide$Kw = Kw
    sims_wide$coeff_mix_hyp = coeff_mix_hyp
    sims_wide$sw_factor = sw_factor
    sims_wide$lw_factor = lw_factor
    sims_wide$sed_temp_mean_1 = sed_temp_mean[1]
    sims_wide$sed_temp_mean_2 = sed_temp_mean[2]
    init_sim_wide = data.frame(matrix(rep(initial_temps, each = nrow(sims_wide)), 
                                     ncol = length(initial_temps)))
    colnames(init_sim_wide) = paste0("init",0:9)
    sims_wide = cbind(sims_wide, init_sim_wide)
    sims_wide$start_date = start_date
    # 
   
    df_wide = df #%>% tidyr::pivot_wider(names_from = ensemble_number, values_from = Temp_C) 
    #colnames(df_wide)[7:37] = paste0("ensemble_member_",1:31)
    df_wide$Kw = Kw
    df_wide$coeff_mix_hyp = coeff_mix_hyp
    df_wide$sw_factor = sw_factor
    df_wide$lw_factor = lw_factor
    df_wide$sed_temp_mean_1 = sed_temp_mean[1]
    df_wide$sed_temp_mean_2 = sed_temp_mean[2]
    
    init_df_wide = data.frame(matrix(rep(initial_temps, each = nrow(df_wide)), 
                                    ncol = length(initial_temps)))
    colnames(init_df_wide) = paste0("init",0:9)
    df_wide = cbind(df_wide, init_df_wide)
    df_wide$start_date = start_date
    print(head(sims_wide))
    return(list(df_wide = df_wide, sims_wide = sims_wide))
  }else{
   stop("You should not be running GLM with anything other than nsave=24 for forecasting. Please check your nml file!")   
 }
}

# JUNK JUNK JUNK
#this was in the else statement, but I don't think I'd ever use it...
  #  df_wide = df
   # df_wide$Kw = Kw
   # df_wide$coeff_mix_hyp = coeff_mix_hyp
   # df_wide$sw_factor = sw_factor
   # df_wide$lw_factor = lw_factor
   # df_wide$sed_temp_mean_1 = sed_temp_mean[1]
   # df_wide$sed_temp_mean_2 = sed_temp_mean[2]
   # init_df_wide = data.frame(matrix(rep(initial_temps, each = nrow(df_wide)), 
    #                                 ncol = length(initial_temps)))
    #colnames(init_df_wide) = paste0("init",0:9)
    #df_wide = cbind(df_wide, init_df_wide)
    #df_wide$start_date = start_date
    #return(list(df = df, df_wide = df_wide))
  #}
