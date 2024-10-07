#------------------------------------------------------------------------------#
#                      Function to fit case study models                       #
#------------------------------------------------------------------------------#

# covariates <- Prednames
# proxies <- proxycovs
# spatial_ctrl <- sctrl
# traindf <- DataSaison
# n_tree = NTREE

fitvalpred_rf <- function(covariates, 
                          proxies=NULL,
                          #random_ctrl, 
                          spatial_ctrl,
                          traindf,
                          n_tree
                          #,rstack
                          ){
  
  # # 1. Tune (find the best mtry)
  print("tuning model")
  tune_grid <- round(seq(2, length(c(covariates, proxies)), length.out=3))
  tune_grid <- data.frame(mtry = tune_grid[!duplicated(tune_grid)])
  tune_ctrl <- caret::trainControl(method="oob")
  tune_mod <- caret::train(x = as.data.frame(traindf)[,c(covariates, proxies)], 
                           y = as.data.frame(traindf)[,"ActLog10"],
                           method="rf", 
                           importance=TRUE,
                           trControl=tune_ctrl, 
                           ntree=n_tree, 
                           tuneGrid=tune_grid)
  print("model tuned")
  # AOA <- suppressMessages(aoa(rstack, tune_mod))
  # AOA <- as.numeric(global(AOA$AOA, na.rm=TRUE))
  # names(AOA) <- "AOA"
  
  # 2. # build model and calculate RMSE and R² using the kNNDM cross-validation method
  spatial_grid <- data.frame(mtry = tune_mod$bestTune$mtry)
  #spatial_grid <- data.frame(mtry = round(length(covariates)*2/3))
  print("building model")
  A=Sys.time()
  spatial_mod <- caret::train(x = as.data.frame(traindf)[c(covariates, proxies)], # train model
                              y = as.data.frame(traindf)[,"ActLog10"], 
                              method="rf", 
                              importance=FALSE,
                              trControl=spatial_ctrl, 
                              ntree = n_tree, 
                              tuneGrid=spatial_grid)
  B=Sys.time()
  print(B-A)
  print("model built, calculating RMSE and R²")
  spatial_stats <- global_validation(spatial_mod)[c("RMSE", "Rsquared")] 
  names(spatial_stats) <- paste0("kNNDM_", names(spatial_stats))
  
  # 3. Surface predictions
  #preds <- predict(rstack, spatial_mod, na.rm=TRUE)
  
  # 4. Variable importance
  impfeat <- importance(spatial_mod$finalModel, type = 2)
  impfeat <- sum(impfeat[row.names(impfeat) %in% covariates, 1])/sum(impfeat[,1])*100
  names(impfeat) <- "impfeat"
  
  # Tidy and return results
  tabres <- as.data.frame(t(c(#random_stats, 
                              spatial_stats, 
                              #AOA, 
                              impfeat)))
  #names(preds) <- c("prediction")
  list(tab = tabres, 
       #preds = preds, 
       tunemod = tune_mod,
       spatmod = spatial_mod)
}
