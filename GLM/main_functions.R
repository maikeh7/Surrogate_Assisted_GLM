library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(GLM3r)
library(glmtools)
library(arrow)
library(reshape2)

source("aux_functions.R")
source("get_GLM_sims.R")
##############################################################################################
# main() will loop over 31 ensemble members and for each ensemble will run GLM:
# extract met file, extract inflow/outflow files
# create directory for that set of simulations, put input files in a diretory called 'inputs'
# also includes a function called 'update_nml' that will update the nml file for
# each NOAA ensemble member
##############################################################################################

main=function(start_date, stop_date=NULL,
     Kw = 0.87, 
     coeff_mix_hyp = 0.6458725, 
     sw_factor = 1,
     lw_factor = 1, 
     sed_temp_mean = c(11.39072901, 14.85279613), 
     results_dir_number = 1,
     nml_file_name = "glm3.nml",
     lake_temps = data.table::fread("https://s3.flare-forecast.org/targets/fcre_v2/fcre/fcre-targets-insitu.csv"),
     obs_inflow = data.table::fread("https://s3.flare-forecast.org/targets/fcre_v2/fcre/fcre-targets-inflow.csv")){
  
  if (!dir.exists("FINAL_RESULTS")){
    dir.create("FINAL_RESULTS")
  }
  final_results_dir = "FINAL_RESULTS"
  
  # get inputs
  print("working on day")
  print(start_date)
  inputs_param_list = get_inputs(start_date, lake_temps, obs_inflow)
  print(inputs_param_list)
  print("made it to main")
  # will be null if there are too many missing observations for start_date_hindcast from which to 
  # derive initial values. Day will be skipped.
  if (is.null(inputs_param_list)){
    print("day is null")
    return(NA)
  }
  # all ensemble members
  ensemble_members = unique(inputs_param_list$noaa_met_GLM$ensemble)
  
  # coeffs we might be interested in
  Kw = inputs_param_list$coeff_list$Kw
  coeff_mix_hyp = inputs_param_list$coeff_list$coeff_mix_hyp
  sw_factor = inputs_param_list$coeff_list$sw_factor
  lw_factor = inputs_param_list$coeff_list$lw_factor
  sed_temp_mean = inputs_param_list$coeff_list$sed_temp_mean
  initial_temps = inputs_param_list$initial_temps
  
  # inflow/outflow/noaa met file (includes ALL ensemble members)
  inflows = inputs_param_list$inflows
  outflows = inputs_param_list$outflows
  noaa_met_GLM = inputs_param_list$noaa_met_GLM
  
  # create main directory for set of coeffs/input files
  main_dir = paste0("Results", results_dir_number)
  dir.create(main_dir)
  
  # create log file that will show coeff vals
  param_log = file.path(final_results_dir, "param_log.txt")
  
  write(paste("\n start_date = ", start_date,
                   "Kw =", Kw, 
                   "| coeff_mix_hyp = ", coeff_mix_hyp, 
                   "| sw_factor = " , sw_factor,
                   "| lw_factor = ", lw_factor, 
                   "| sed_temp_mean = ", sed_temp_mean[1], " and ", sed_temp_mean[2], "\n"),
             param_log, append = TRUE)
  

  # loop over ensemble members
  # we only want the 30 ensemble members, so I only go up to 30
  for (i in 1:31){
    # extract current NOAA met data
    curr_met_file = dplyr::filter(noaa_met_GLM, ensemble == i)
    curr_met_file$ensemble = NULL
    
    # extract current NOAA inflow/outflow data
    curr_inflow_file = dplyr::filter(inflows, ensemble == i)
    curr_inflow_file$ensemble = NULL
    
    curr_outflow_file = dplyr::filter(outflows, ensemble == i)
    curr_outflow_file$ensemble = NULL
    
    # read in nml file
    # if file is not there or incorrect, glmtools will give an intuitive error
    nml_file = read_nml(nml_file_name)
    
    # create file names for current met/inflow/outflow files
    met_file_name = paste("forecast_ensemble_", i, ".csv", sep = "")
    
    inflow_file_name = "inflow.csv"
    outflow_file_name = "outflow.csv"
    
    # create sim directory, input/output dirs
    sim_dir_i = file.path(main_dir, paste0("GLM_sim_", i))
    dir.create(sim_dir_i)
    
    output_dir = file.path(sim_dir_i, "output")
    dir.create(output_dir)
    
    input_dir = file.path(sim_dir_i, "inputs")
    dir.create(input_dir)
    
    # write met file to input directory
    write.csv(curr_met_file, file.path(input_dir, met_file_name), quote= FALSE, row.names = FALSE)
    
    # write inflow/outflow files to input dir
    write.csv(curr_inflow_file, file.path(input_dir, inflow_file_name), row.names = FALSE, quote = FALSE)
    write.csv(curr_outflow_file, file.path(input_dir, outflow_file_name), row.names = FALSE, quote = FALSE)
    
    # edit nml file
    # start/stop
    mystart = paste0(curr_met_file$time[1], ":00")
    mystop = paste0(curr_met_file$time[nrow(curr_met_file)], ":00")
    nml_file$time$start = mystart
    nml_file$time$stop = mystop
    # This saves one output per day at 00UTC!! This is what we want!! 
    # NOTE: it should not be anything other than 24
    nml_file$output$nsave = 24 
    
    # update names of met/inflow/outflow...altho can leave out in/outflow b/c they never change....
    #nml_input_dir = file.path(paste0("GLM_sim_", i), "inputs")
    nml_file$meteorology$meteo_fl = file.path("inputs", met_file_name)
    nml_file$inflow$inflow_fl = file.path("inputs", inflow_file_name)
    nml_file$outflow$outflow_fl = file.path("inputs", outflow_file_name)
    
    # Coefficients of interest/inital temps
    nml_file$light$Kw = Kw
    nml_file$mixing$coef_mix_hyp = coeff_mix_hyp
    nml_file$meteorology$sw_factor = sw_factor
    nml_file$meteorology$lw_factor = lw_factor
    nml_file$sediment$sed_temp_mean = sed_temp_mean
    
    # change inflow vars to be FLOW, TEMP SALT
    #nml_file$inflow$inflow_vars = c("FLOW", "TEMP", "SALT")
    # change inflow var no to be 3
    #nml_file$inflow$inflow_varnum = 3
    
    # set initial temps
    nml_file$init_profiles$the_temps = initial_temps
    # set depths to be 0:10
    nml_file$init_profiles$the_depths = c(0.10, 1:9)
    # want only 10 salt values (set to 0)
    nml_file$init_profiles$the_sals = rep(0, 10)
    nml_file$inflow$num_inflows = 1
    
    # write changes to nml file
    write_nml(nml_file, file.path(sim_dir_i, "glm3.nml"))
    
    # finally, run GLM on the folder for current ensemble member
    print(paste("running sim", i))
    GLM3r::run_glm(sim_folder = sim_dir_i, verbose = FALSE)
    
    print(paste("GLM simulation", i, "is complete."))

  }
  
  #process output from GLM, collect data and params in dataframe, delete extra folders
  # Note that nsave must be equal to 24 (saving one GLM output every 24 hours, at 00UTC.
  # if nsave != 24, get_GLM_sims() will stop on error
  print(paste("main_dir is ", main_dir))
  my_results = get_GLM_sims(curr_results_dir = main_dir, nsave = 24, 
                            coeff_list = inputs_param_list$coeff_list, 
                            initial_temps = inputs_param_list$initial_temps, 
                            start_date = start_date)

  # write to a place where I will find files
  write.csv(my_results$sims_wide, file.path(final_results_dir,
            paste0("results_", start_date, ".csv")))

  # delete the result files
  unlink(grep("*Results", list.files(), value = TRUE), recursive = TRUE)
}




lake_temps <- data.table::fread("https://s3.flare-forecast.org/targets/fcre_v2/fcre/fcre-targets-insitu.csv")
obs_inflow <- data.table::fread("https://s3.flare-forecast.org/targets/fcre_v2/fcre/fcre-targets-inflow.csv")


# make training dates
df = make_ymd()
df2 = make_ymd()
df3 = make_ymd()
df4 = make_ymd()
df$YEAR = 2020
df2$YEAR = 2021
df3$YEAR = 2022
df4$YEAR = 2023
dates20 = paste(df$YEAR, df$MONTH, df$DAY, sep = "-")
mydates = as.character(as.Date(dates20, tz = "UTC"))
dates_char20 = mydates[!is.na(mydates)]

dates21 = paste(df2$YEAR, df2$MONTH, df2$DAY, sep = "-")
mydates = as.character(as.Date(dates21, tz = "UTC"))
dates_char21 = mydates[!is.na(mydates)]

dates22 = paste(df3$YEAR, df3$MONTH, df3$DAY, sep = "-")
mydates = as.character(as.Date(dates22, tz = "UTC"))
dates_char22 = mydates[!is.na(mydates)]

dates23 = paste(df4$YEAR, df4$MONTH, df4$DAY, sep = "-")
mydates = as.character(as.Date(dates23, tz = "UTC"))
dates_char23 = mydates[!is.na(mydates)]


alldates = c(dates_char20, dates_char21, dates_char22, dates_char23)

# the date after last date of GLM sims
myidx = which(alldates== "2023-03-30")
# get current date
cur_date = strsplit(as.character(Sys.time()), " +")[[1]][1]
end_idx = which(alldates == cur_date)
alldates = alldates[myidx:end_idx]
#alldates = "2021-01-30"

for(i in 1:length(alldates)){
  main(start_date = alldates[i])
}
