# Load required packages

pacman::p_load(pacman, rio, tidyverse, dplyr, ggplot2, rlang)

# Load the processed data

# Read in the cleaned dataframe
df <- read.csv('./data/lagged_data_nomissing.csv', row.names=1)
head(df)

# We won't be using the cycle column
df <- select(df,-cycle)
head(df)

# Inspect the dataframe
str(df$target)
df$target <- factor(df$target)

prop.table(table(df$target))

# Do a stratified train test split because of the class imbalance
set.seed(4)
library(caret)
split_idx <- createDataPartition(df$target, p=0.6, list=FALSE)
train_df <- df[split_idx, ]
testval_df <- df[-split_idx, ]

# make sure the proportions match
prop.table(table(train_df$target))
prop.table(table(testval_df$target))

# Split testval into test and validation set
split_idx <- createDataPartition(testval_df$target, p=0.5, list=FALSE)
test_df <- testval_df[split_idx, ]
val_df <- testval_df[-split_idx, ]

prop.table(table(test_df$target))
prop.table(table(val_df$target))

# Save these files
write.csv(train_df, './data/train_df.csv')
write.csv(test_df, './data/test_df.csv')
write.csv(val_df, './data/val_df.csv')


# build a random forest model using caret and the randomForest library
library(randomForest)

# Testing
rf_classifier = randomForest(target ~ ., data=train_df, ntree=100)
rf_classifier

preds <- predict(rf_classifier, val_df)
table(preds, val_df$target)
cm <- confusionMatrix(preds, val_df$target)
print(cm$overall[['Accuracy']])
print(cm$byClass[['Specificity']])

# Hyperparameter tuning
for (nt in c(50,100,200,500)){
  for (ns in c(1,3,5,10)){
    rf_classifier = randomForest(target ~ ., data=train_df, ntree=nt, nodesize=ns)
    preds <- predict(rf_classifier, val_df)
    cm <- confusionMatrix(preds, val_df$target)
    #print(cm$overall[['Accuracy']])
    #print(cm$byClass[['Specificity']])
    cat(sprintf("For tree size = %d, node size = %d the accuracy = %f and the specificity = %f \n", 
                nt, ns, cm$overall[['Accuracy']], print(cm$byClass[['Specificity']])))
  }
}

# Best model is tree size 200, node size 3 with Accuracy = 98.77% and specificity = 77.14%

rf_classifier <- randomForest(target ~ ., data=train_df, ntree=200, nodesize=3)

preds <- predict(rf_classifier, test_df)
table(preds, test_df$target)
cm <- confusionMatrix(preds, test_df$target)
print(cm$overall[['Accuracy']])
print(cm$byClass[['Specificity']])

# 98.74% accuracy and 79.28% specificity in the test set

# save the model
saveRDS(rf_classifier, "./results/final_model.rds")

# load the model
#super_model <- readRDS("./final_model.rds")

# end program cleanup
# Clear data
rm(list = ls())
detach("package:datasets", unload = T)
p_unload(all)  

graphics.off()

cat("\014") 
