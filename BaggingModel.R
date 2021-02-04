set.seed(123456) # Set seed for reproducability
# Create cross-validation index
cv_ind <- sample(1:5, nrow(SubTrainH), replace = TRUE )

# Create accuracy store
cv_acc <- rep(NA, 5)
for(i in 1:5){ # For 1 to 5
  cv_train <- SubTrainH[cv_ind != i ,c(2:7)] # Create training data
  cv_test <- SubTrainH[cv_ind == i,  c(2:7)] # Create test data
  
  bag_mod_3 <- randomForest(sub_effect_2 ~., # Set tree formula
                            data = SubTrainH, # Set dataset
                            mtry = 6, # set number of variables to use
                            ntree = 200, # Set number of trees to generate
                            nodesize = 500) # Set node size
  bag_preds_3 <- predict(bag_mod_3, SubTestH) # Create test data predictions
  
  
  t <- table(bag_preds_3,SubTestH$sub_effect_2) # Create table
  cf_mat <- confusionMatrix(t,  positive = "1") # Create confusion matrix
  cv_acc[i] <- cf_mat$overall[1] # Extract accuracy
}

# Print cross validated accuracy scores
cv_acc
