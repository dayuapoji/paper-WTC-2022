
prep_df <- function(rf_chainage) {
  
  output <- rf_chainage %>% 
    # remove unused columns
    select(!contains('GeologicSegment')) %>%
    select(!contains('Chainage')) %>%
    select(!contains('Ring')) %>%
    # set label as factor
    mutate(., SoilLabel = factor(as.character(.$SoilLabel)))
  
  return(output)
}

# ------------------------------------------------------------------------------

get_static_predictions <- function(rf_df, train_frac) {
  
  # create df labeled only
  rf_df_labeled <- rf_df %>%
    .[(.$SoilLabel != 'Unlabeled'), ]
  
  # set train index
  train_index <- sample(nrow(rf_df_labeled), 
                        train_frac * nrow(rf_df_labeled), 
                        replace = FALSE)
  
  # train data
  rf_train_chainage <- rf_df_labeled[train_index, ]
  rf_train <- prep_df(rf_train_chainage)
  
  # upsampling for imbalance data
  rf_train_up <- upSample(x = rf_train[1:(ncol(rf_train)-1)],
                          y = factor(rf_train$SoilLabel),
                          yname = "SoilLabel") 
  
  # test data
  rf_test_chainage <- rf_df %>% 
    filter(Chainage %!in% rf_train_chainage$Chainage)
  rf_test <- prep_df(rf_test_chainage)
  
  # train
  rf_model <- ranger(SoilLabel~., data = rf_train_up,
                     num.trees = 500,
                     mtry = 1,
                     min.node.size = 10,
                     splitrule = "extratrees")
  
  # test
  rf_predictions <- predict(rf_model, data = rf_test, seed = 1)
  
  # get prediction results
  predictions <- rf_predictions$predictions
  
  # create results df
  rf_results <- data.frame(
    Ring = rf_test_chainage$Ring,
    Chainage = rf_test_chainage$Chainage,
    SoilLabel = rf_test$SoilLabel, 
    Prediction = factor(predictions),
    Error = ifelse((as.character(rf_test$SoilLabel) == 'Unlabeled' |
                      as.character(rf_test$SoilLabel) == predictions), 0, 1))
  
  return(rf_results)
}

# ------------------------------------------------------------------------------

# get_static_probability <- function(rf_df, train_frac) {
#   
#   # create df labeled only
#   rf_df_labeled <- rf_df %>%
#     .[(.$SoilLabel != 'Unlabeled'), ]
#   
#   # set train index
#   train_index <- sample(nrow(rf_df_labeled), 
#                         train_frac * nrow(rf_df_labeled), 
#                         replace = FALSE)
#   
#   # train data
#   rf_train_chainage <- rf_df_labeled[train_index, ]
#   rf_train <- prep_df(rf_train_chainage)
#   
#   # test data
#   rf_test_chainage <- rf_df %>% 
#     filter(Chainage %!in% rf_train_chainage$Chainage)
#   rf_test <- prep_df(rf_test_chainage)
#   
#   # upsampling for imbalance data
#   rf_train_up <- upSample(x = rf_train[1:(ncol(rf_train)-1)],
#                           y = factor(rf_train$SoilLabel),
#                           yname = "SoilLabel") 
#   
#   # train
#   rf_model <- ranger(SoilLabel~., data = rf_train_up,
#                      probability = TRUE,
#                      num.trees = 500,
#                      min.node.size = 5,
#                      splitrule = "extratrees",
#                      seed = 1)
#   
#   # test
#   rf_predictions <- predict(rf_model, data = rf_test, 
#                             seed = 1)$predictions
#   
#   # get prediction results
#   predictions <- NULL
#   for (i in 1:nrow(rf_predictions)) {
#     col_index <- which(rf_predictions[i, ] == max(rf_predictions[i, ]))
#     max_prob <- colnames(rf_predictions)[col_index]
#     predictions[i] <- max_prob
#   }
#   
#   # create results df
#   rf_results <- data.frame(
#     Ring = rf_test_chainage$Ring,
#     Chainage = rf_test_chainage$Chainage,
#     SoilLabel = rf_test$SoilLabel, 
#     Prob = rf_predictions,
#     Prediction = factor(predictions),
#     Error = ifelse((as.character(rf_test$SoilLabel) == 'Unlabeled' |
#                       as.character(rf_test$SoilLabel) == predictions), 0, 1))
#   
#   return(rf_results)
# }

# ------------------------------------------------------------------------------

get_confusion_matrix <- function(rf_results) {
  
  cm_df <- rf_results %>% .[(.$SoilLabel != 'Unlabeled'), ]
  cm_df$SoilLabel <- droplevels(cm_df$SoilLabel)
  cm <- confusionMatrix(cm_df$Prediction, cm_df$SoilLabel)
  
  return(cm)
}

# ------------------------------------------------------------------------------

get_accuracy <- function(cm) {
  
  accuracy <- cm$overall %>% t(.) %>% data.frame(.)
  
  return(accuracy)
}
