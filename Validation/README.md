## In-sample and out-of-sample forecasts (model validation)
This folder contains code necessary for running all models involving GLM for the forecasting exercise described in Section 5.2 of the manuscript. We highly recommend 
running code on a Linux machine. Each subfolder within this directory is for one specific type of model (e.g. 'GPBC'). First, the initial training data must be constructed 
(except for the case of 'Validation-TrainOnly', in which we perform in-sample prediction). To construct initial training data for each of the model types, use 
make_train_data = TRUE in main.R. To perform validation (e.g. make out-of-sample forecasts), use make_train_data=FALSE in main.R. Before making out of sample forecasts, ensure that training 
data has been constructed. For convenience, we suggest using the 'runModel.sh' file to run main.R as a background process.  
