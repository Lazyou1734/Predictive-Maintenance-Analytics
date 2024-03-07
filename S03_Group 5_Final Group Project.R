#setwd("C:/Users/Andy/Desktop/BC2406/Team Project")
setwd("/Users/rachelkoh/Desktop/BC2406/PROJ")


# Libraries Used: -------------------------------------------------------------------
{
#for Importation and Exploratory Data Analysis
library(data.table)
library(ggplot2)
library(tidyr)
library(corrplot)
library(e1071)
library(dplyr)

library(caTools) #for train test split
library(car) #for vif
library(caret) #for confusion matrix

library(rpart) #for cart model
library(rpart.plot)

library(class) # for knn model
  
library(smotefamily) #for oversampling
}

# Data Exploration --------------------------------------------------------------------

machine.dt <- fread("augmented_machine_3.csv")

summary(machine.dt)

#Finding NA values
{
na_counts <- colSums(is.na(machine.dt))
print(na_counts)
}

# Outlier detection
{
print("Number of Outliers:")
numeric_columns <- c("Air.temperature..K.", "Process.temperature..K.", "Rotational.speed..rpm.", "Torque..Nm.", "Tool.wear..min.")
  
for(column in numeric_columns) {
  Q1 <- quantile(machine.dt[[column]], 0.25, na.rm = TRUE)
  Q3 <- quantile(machine.dt[[column]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  number <- sum(machine.dt[[column]] < lower_bound | machine.dt[[column]] > upper_bound, na.rm = TRUE)
  cat(column, ":", number, "\n")
}
}

# Check for any inconsistent rows where Machine.failure = 1 but there are no failures
{
inconsistent_rows <- which(machine.dt$Machine.failure == 1 & 
                               rowSums(machine.dt[, .(TWF, HDF, PWF, OSF, RNF)]) == 0)
print(inconsistent_rows)  
}

# Data Visualizations  ----------------------------------------------------------------
# cleaning so plots can run
{
  raw_dataset <- fread("augmented_machine_3.csv")
  cleaned_dataset <- raw_dataset
  
  # Handling NA values for the Type column
  mode_value <- names(sort(table(raw_dataset$Type), decreasing=TRUE)[1])
  cleaned_dataset$Type[is.na(cleaned_dataset$Type)] <- mode_value
  
  # Handling NA values for numeric columns:
  numeric_columns <- c("Air.temperature..K.", "Process.temperature..K.", "Rotational.speed..rpm.", "Torque..Nm.", "Tool.wear..min.")
  
  for (col in numeric_columns) {
    cleaned_dataset[[col]][is.na(cleaned_dataset[[col]])] <- median(raw_dataset[[col]], na.rm=TRUE)
  }
  
  # Check for NA values
  any(is.na(cleaned_dataset))
  
  # Cleaning inconsistent rows
  inconsistent_rows <- which(cleaned_dataset$Machine.failure == 1 & 
                               rowSums(cleaned_dataset[, .(TWF, HDF, PWF, OSF, RNF)]) == 0)

  # Set Machine.failure to 0
  cleaned_dataset[inconsistent_rows, Machine.failure := 0]
  
  # Check that there are no more inconsistent rows
  sum(cleaned_dataset$Machine.failure == 1 & rowSums(cleaned_dataset[, .(TWF, HDF, PWF, OSF, RNF)]) == 0)
  
  machine.dt <- cleaned_dataset
}

# Figure 2
# Imbalanced data
ggplot(machine.dt, aes(x=Machine.failure)) +
  geom_bar(fill = "steelblue") +
  geom_text(stat = 'count', aes(label =..count..), vjust=-0.5) +
  labs(title="Imbalance of Machine Failures", x="0 = Not Failed, 1 = Failed", y="Count") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.4),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)  
  )

# Figure 3
#Correlation Plot
# Convert 'Type' column to numeric based on the given levels
{
  # Convert 'Type' column to numeric based on the given levels
  machine.dt[, Type := as.numeric(factor(Type, levels = c("L", "M", "H"), ordered = TRUE))]
  
  # Drop the 'UDI' and 'Product.ID' columns
  machine.dt[, c("UDI", "Product.ID") := NULL]
  
  corrplot(cor(machine.dt), method="color", addCoef.col = "black")
  
  # Assuming machine.dt is your correlation matrix:
  corr_matrix <- cor(machine.dt)
  
  # Generate the correlation plot
  corrplot(corr_matrix, method = "color", 
           addCoef.col = "black",   
           tl.col = "black",        
           tl.srt = 45,             
           tl.cex = 0.6)            
  
  cor(machine.dt)
  }

# Figure 4
# To identify outliers & calculate skewness,
{
  par(mfrow = c(2, 3))
  for (col in numeric_columns) {
    density_data <- density(machine.dt[[col]])
    plot(density_data, main = col)
    
    skew_value <- skewness(machine.dt[[col]])
    skew_label <- paste("Skewness:", round(skew_value, 2))
    mtext(text = skew_label, side = 3, line = 0, cex = 0.7)  }
  par(mfrow = c(1, 1))
}

# Figure 6
# BarChart of machine failure types

machine_long <- machine.dt %>%
  pivot_longer(cols = c(TWF, HDF, PWF, OSF, RNF), 
               names_to = "Failure_Type", 
               values_to = "Count")

ggplot(machine_long, aes(x = reorder(Failure_Type, -Count), y = Count, fill = Failure_Type)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Number of Different Failure Types", x = "Failure Type", y = "Count") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    panel.grid.major.x = element_blank(),  
    panel.grid.minor.x = element_blank()
  ) +
  scale_fill_manual(values = c(
    "TWF" = "#1f77b4",  
    "HDF" = "#1f77b4",  
    "PWF" = "#1f77b4",  
    "OSF" = "#1f77b4",  
    "RNF" = "#1f77b4"
  ))


# Figure 7
# Layered KDE Plots
{
  # Reshape from wide to long format for ggplot2
  dt_long <- melt(machine.dt, id.vars = c('HDF', 'OSF', 'PWF', 'TWF', 'RNF'), 
                  measure.vars = c('Air.temperature..K.', 'Process.temperature..K.', 
                                   'Rotational.speed..rpm.', 'Torque..Nm.', 'Tool.wear..min.'))
  
  # Create a new column that identifies the failure type
  dt_long[, Failure := fifelse(HDF == 1, 'HDF', 
                               fifelse(OSF == 1, 'OSF', 
                                       fifelse(PWF == 1, 'PWF', 
                                               fifelse(TWF == 1, 'TWF', 
                                                       fifelse(RNF == 1, 'RNF', NA_character_)))))]
  
  # Remove rows where Failure is NA (i.e., no failure of the types specified)
  dt_long <- dt_long[!is.na(Failure)]
  
  # Create the KDE plot using ggplot2
  ggplot(dt_long, aes(x = value, fill = Failure)) + 
    geom_density(alpha = 0.5) + 
    facet_wrap(~variable, scales = 'free', nrow = 1) + 
    theme_minimal() +
    labs(x = "KDEPlots of failure types", y = "Density") +
    theme(legend.position = "bottom")
  }

# Figure 8
# Facetted scatterplot for machine failures
{base_var <- "Machine.failure"
  
  # Filtering out NA values in the base_var column
  machine_filtered <- machine.dt[!is.na(get(base_var)), ]
  
  machine_long <- melt(machine_filtered, id.vars = base_var, 
                       measure.vars = numeric_columns)
  
  ggplot(machine_long, aes(x=factor(get(base_var)), y=value)) + 
    geom_point(alpha=0.7, position = position_jitter(width = 0.2), size = 0.2, color = "blue") +  
    facet_wrap(~variable, scales = "free_y") + 
    labs(x = base_var, y = "Value") +
    theme_minimal()}

# Figure 9
# Boxplots of the 5 numeric variables when the machine has failed
{
  long_data <- machine.dt %>%
    pivot_longer(cols = numeric_columns, names_to = "variable", values_to = "value")
  
  ggplot(long_data, aes(x = factor(Machine.failure), y = value, fill = factor(Machine.failure))) + 
    geom_boxplot() +
    facet_wrap(~ variable, scales = "free", nrow = 1) +  
    labs(x = "Machine Failure", y = "Value", fill = "Failure Type") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
      strip.background = element_blank(),
      strip.text.x = element_text(size = 10)  
    ) + 
    scale_fill_manual(values = c("1" = "#1f77b4", "0" = "#4169e1"))
}



# Data Cleaning ----------------------------------------------------------------
{
  raw_dataset <- fread("augmented_machine_3.csv")
  
  # Check the number of NA values in each column
  na_counts <- colSums(is.na(raw_dataset))
  print(na_counts)
  
  #Duplicate the dataset
  cleaned_dataset <- raw_dataset
  
  # Handling NA values for the Type column
  mode_value <- names(sort(table(raw_dataset$Type), decreasing=TRUE)[1])
  cleaned_dataset$Type[is.na(cleaned_dataset$Type)] <- mode_value
  
  # Handling NA values for numeric columns:
  numeric_columns <- c("Air.temperature..K.", "Process.temperature..K.", "Rotational.speed..rpm.", "Torque..Nm.", "Tool.wear..min.")
  
  for (col in numeric_columns) {
    cleaned_dataset[[col]][is.na(cleaned_dataset[[col]])] <- median(raw_dataset[[col]], na.rm=TRUE)
  }
  
  # Check for NA values
  any(is.na(cleaned_dataset))
  
  # Check for any inconsistent rows where Machine.failure = 1 but there are no failures
  inconsistent_rows <- which(cleaned_dataset$Machine.failure == 1 & 
                               rowSums(cleaned_dataset[, .(TWF, HDF, PWF, OSF, RNF)]) == 0)
  print(inconsistent_rows)
  
  # Set Machine.failure to 0
  cleaned_dataset[inconsistent_rows, Machine.failure := 0]
  
  # Check for any inconsistent rows where RNF = 1 but there Machine.failure = 0
  inconsistent_rows <- which(cleaned_dataset$Machine.failure == 0 & cleaned_dataset$RNF == 1)
  print(inconsistent_rows)
  
  # Remove rows with RNF
  cleaned_dataset <- cleaned_dataset[-inconsistent_rows, ]
  
  # Check that there are no more inconsistent rows
  sum(cleaned_dataset$Machine.failure == 1 & rowSums(cleaned_dataset[, .(TWF, HDF, PWF, OSF, RNF)]) == 0)
  
  # Outlier detection
  print("Number of Outliers:")
  for(column in numeric_columns) {
    Q1 <- quantile(cleaned_dataset[[column]], 0.25, na.rm = TRUE)
    Q3 <- quantile(cleaned_dataset[[column]], 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    
    number <- sum(cleaned_dataset[[column]] < lower_bound | cleaned_dataset[[column]] > upper_bound, na.rm = TRUE)
    cat(column, ":", number, "\n")
  }
  
  machine.dt <- cleaned_dataset
}

# Dataset Manipulation ---------------------------------------------------------
{
# Check the class of each column
sapply(machine.dt, class)

# Relevel Type to 'L, M, H'
machine.dt$Type <- factor(machine.dt$Type, levels = c("L", "M", "H"))

# Drop column 'Product.ID' and 'UDI'
machine.dt <- machine.dt[, !"Product.ID", with = FALSE]
machine.dt <- machine.dt[, !"UDI", with = FALSE]

# Converting to categorical
columns <- c("Machine.failure", "TWF", "HDF", "PWF", "OSF", "RNF")

for (col in columns) {
  machine.dt[[col]] <- as.factor(machine.dt[[col]])
}

# Define failure types
failure_types <- c("TWF", "HDF", "PWF", "OSF")

# Convert TWF, HDF, PWF, OSF columns to a single column
machine.dt[, Failure_Type := ifelse(Machine.failure == 0, NA_character_, failure_types[max.col(.SD, ties.method = "first")]), .SDcols = failure_types]

# Set baseline group for Failure_Type
machine.dt$Failure_Type <- as.factor(machine.dt$Failure_Type)
machine.dt$Failure_Type <- relevel(machine.dt$Failure_Type, ref = "TWF")
levels(machine.dt$Failure_Type)

# Relevel Machine Failure
machine.dt$Machine.failure <- factor(machine.dt$Machine.failure, levels = c("1","0"))

summary(machine.dt)

# Train-Test Split
set.seed(101)
train <- sample.split(Y = machine.dt$Machine.failure, SplitRatio = 0.7)
trainset <- subset(machine.dt, train == T)
testset <- subset(machine.dt, train == F)

# Checking the distribution of Y to ensure that it is similar in trainset vs testset.
summary(trainset)
summary(testset)
}

# Logistic Regression to predict Machine Failure (Before Oversampling) -------------------------------
{
# Develop model on the trainset
model <- glm(Machine.failure ~  Type + Air.temperature..K. + Process.temperature..K.
             + Rotational.speed..rpm. + Torque..Nm. + Tool.wear..min.
             , family = binomial, data = trainset)
print(summary(model))

vif(model)

# Predict machine failures on testset using the model
prob <- predict(model, newdata=testset, type='response')
y.hat <- ifelse(prob > 0.5, 0, 1)

# Calculate Odds Ratios and Confidence Intervals
OR <- exp(coef(model))
OR.CI <- exp(confint(model))

print(OR)
print(OR.CI)

# Create a confusion matrix with predictions on rows and testset on columns
confusion_matrix <- confusionMatrix(as.factor(y.hat), as.factor(testset$Machine.failure),mode = "everything")
print(confusion_matrix)
}

# CART Analysis (Before Oversampling) ------------------------------------------------------------------

# Training the CART Model
{
cart_model <- rpart(Machine.failure ~ Type + Air.temperature..K. + Process.temperature..K. + 
                      Rotational.speed..rpm. + Torque..Nm. + Tool.wear..min., 
                    data = trainset, method = "class",
                    control = rpart.control(minsplit = 2, cp = 0))

rpart.plot(cart_model, nn= T, main = "Maximal Tree for machine.csv")
print(cart_model)

predictions <- predict(cart_model, newdata = testset, type = "class")

class(predictions)

# Create a confusion matrix
confusion_matrix <- confusionMatrix(predictions, testset$Machine.failure, mode = "everything")

# Print the confusion matrix
print(confusion_matrix)
}

# Model Pruning
{
plotcp(cart_model, main = "Subtrees in machine.csv")
printcp(cart_model)

# Extracting the Optimal Tree
CVerror.cap <- cart_model$cptable[which.min(cart_model$cptable[,"xerror"]), "xerror"] + cart_model$cptable[which.min(cart_model$cptable[,"xerror"]), "xstd"]

# Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree cart_model.
i <- 1; j<- 4
while (cart_model$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}

print(i)
## i = 7

# Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
cp.opt = ifelse(i > 1, sqrt(cart_model$cptable[i,1] * cart_model$cptable[i-1,1]), 1)

#prune tree using found cp value 
cart_model2 <- prune(cart_model, cp = cp.opt) 
printcp(cart_model2,digits = 3)

print(cart_model2)
rpart.plot(cart_model2, nn= T, main = "Optimal Tree for machine.csv")

cart_model2$variable.importance
## Torque(Nm) has the highest importance. Process Temperature (K) is secondary. Type has the least importance. 

predictions_final <- predict(cart_model2, newdata = testset, type = "class")
summary(testset$Machine.failure)
summary(predictions_final)

confusion_matrix_final <- confusionMatrix(predictions_final, testset$Machine.failure, mode = "everything")
print(confusion_matrix_final)
}

# K-Nearest Neighbours to predict Machine Failure (Before Oversampling) ------------------------------
{

# Converting Type into numeric
trainset$Type <- as.numeric((trainset$Type))
testset$Type <- as.numeric((testset$Type))

# Scaling the data
train_scaled <- scale(trainset[, 1:6])
test_scaled <- scale(testset[, 1:6], 
                     center = attr(train_scaled, "scaled:center"),
                     scale = attr(train_scaled, "scaled:scale"))


# Choosing the value of k
k_values <- c(1:100)
best_k <- NULL
best_accuracy <- 0
class_labels <- as.factor(trainset$Machine.failure)

for (k in k_values) {
  knn_pred <- knn(train = train_scaled, test = test_scaled, cl = class_labels, k = k)
  accuracy <- mean(knn_pred == testset$Machine.failure)
  
  if (accuracy > best_accuracy) {
    best_accuracy <- accuracy
    best_k <- k
  }
}

print(paste("Best k:", best_k))
print(paste("Best accuracy:", best_accuracy))

# Predicting machine failure on test set
knn_pred <- knn(train = train_scaled, test = test_scaled, cl = trainset$Machine.failure, k = best_k)

# Create a confusion matrix with predictions on rows and testset on columns
confusion_matrix <- confusionMatrix(knn_pred, testset$Machine.failure, mode = "everything")
print(confusion_matrix)
}

# Oversampling -----------------------------------------------------------------
{
machine.dt_balanced <- cleaned_dataset

# Relevel Type to 'L, M, H'
machine.dt_balanced$Type <- factor(machine.dt_balanced$Type, levels = c("L", "M", "H"))

# Drop column Product.ID
machine.dt_balanced <- machine.dt_balanced[, !"Product.ID", with = FALSE]

# Check for imbalanced data
count_result <- table(machine.dt_balanced$Machine.failure)
count_df <- as.data.frame(count_result)
colnames(count_df) <- c("Machine.failure", "Count")
print(count_df)

#  Machine.failure Count
#1               0  9661
#2               1   330

# Performing oversampling using smotelibrary
smotetest <- machine.dt_balanced

# Converting columns to numeric
smotetest[, (names(smotetest)) := lapply(.SD, as.numeric), .SDcols = names(smotetest)]
print(sapply(smotetest,class))

# Perform SMOTE
dup_size = sum(smotetest$Machine.failure == 0)/sum(smotetest$Machine.failure == 1)
smotetest_oversampled = SMOTE(smotetest[,-8],target=smotetest$Machine.failure, K = 3, dup_size = dup_size-1)

View(smotetest_oversampled)

machine.dt_balanced <- smotetest_oversampled$data

# Rename the "class" column to "Machine.failure" and recode the values
machine.dt_balanced$Machine.failure <- ifelse(machine.dt_balanced$class == 1, 1, 0)

machine.dt_balanced$Machine.failure <- factor(machine.dt_balanced$Machine.failure, levels = c("1","0"))

# Remove the original "class" column
machine.dt_balanced$class <- NULL

# Rounding the values after doing oversampling
machine.dt_balanced$Type <- round(machine.dt_balanced$Type)
machine.dt_balanced$TWF <- round(machine.dt_balanced$TWF)
machine.dt_balanced$HDF <- round(machine.dt_balanced$HDF)
machine.dt_balanced$PWF <- round(machine.dt_balanced$PWF)
machine.dt_balanced$OSF <- round(machine.dt_balanced$OSF)
machine.dt_balanced$RNF <- round(machine.dt_balanced$RNF)

level = c("1","0")

machine.dt_balanced$TWF <- factor(machine.dt_balanced$TWF,levels = level)
machine.dt_balanced$HDF <- factor(machine.dt_balanced$HDF,levels = level)
machine.dt_balanced$PWF <- factor(machine.dt_balanced$PWF,levels = level)
machine.dt_balanced$OSF <- factor(machine.dt_balanced$OSF,levels = level)
machine.dt_balanced$RNF <- factor(machine.dt_balanced$RNF,levels = level)


# Relevel Type to '1, 2, 3'
machine.dt_balanced$Type <- factor(machine.dt_balanced$Type, levels = c("1", "2", "3"))

# Converting to categorical
columns <- c("Machine.failure", "TWF", "HDF", "PWF", "OSF", "RNF")

for (col in columns) {
  machine.dt_balanced[[col]] <- as.factor(machine.dt_balanced[[col]])
}

# Define failure types
failure_types <- c("TWF", "HDF", "PWF", "OSF")

# Convert TWF, HDF, PWF, OSF columns to a single column
machine.dt_balanced[, Failure_Type := ifelse(Machine.failure == 0, NA_character_, failure_types[max.col(.SD, ties.method = "first")]), .SDcols = failure_types]

# Set baseline group for Failure_Type
machine.dt_balanced$Failure_Type <- as.factor(machine.dt_balanced$Failure_Type)
machine.dt_balanced$Failure_Type <- relevel(machine.dt_balanced$Failure_Type, ref = "TWF")
levels(machine.dt_balanced$Failure_Type)

summary(machine.dt_balanced)

# Train-Test Split

set.seed(123)
train2 <- sample.split(Y = machine.dt_balanced$Machine.failure, SplitRatio = 0.7)
trainset2 <- subset(machine.dt_balanced, train2 == T)
testset2 <- subset(machine.dt_balanced, train2 == F)

# Checking the distribution of Y to ensure that it is similar in trainset2 vs testset2.
summary(trainset2)
summary(testset2)
}

# Logistic Regression to predict Machine Failure (After Oversampling) -------------------------------
{
# Develop model on trainset2
model <- glm(Machine.failure ~  Type + Air.temperature..K. + Process.temperature..K.
             + Rotational.speed..rpm. + Torque..Nm. + Tool.wear..min.
             , family = binomial, data = trainset2)
summary(model)

# Predict machine failures on testset2 using the model
prob <- predict(model, newdata=testset2, type='response')
y.hat <- ifelse(prob > 0.5, 0, 1)

# Calculate Odds Ratios and Confidence Intervals
OR <- exp(coef(model))
OR.CI <- exp(confint(model))

print(OR)
print(OR.CI)

# Create a confusion matrix with predictions on rows and testset2 on columns
confusion_matrix <- confusionMatrix(as.factor(y.hat), as.factor(testset2$Machine.failure), mode = "everything")
print(confusion_matrix)

vif(model)
}

# Logistic Regression to predict type of Machine Failure (After Oversampling) -----------------------
{
# Define failure types
failure_types <- c("TWF", "HDF", "PWF", "OSF")

# Loop through each failure type
for (failure in failure_types){
  # Develop model on the trainset2
  formula <- paste(failure, "~ Type + Air.temperature..K. + Process.temperature..K. + Rotational.speed..rpm. + Torque..Nm. + Tool.wear..min.")
  model <- glm(as.formula(formula), family = binomial, data = trainset2)
  print(paste("Summary of", failure, "model :"))
  print(summary(model))
  
  # Predict machine failures on testset2 using the model
  prob <- predict(model, newdata=testset2, type='response')
  y.hat <- ifelse(prob > 0.5, 0, 1)
  
  # Calculate Odds Ratios and Confidence Intervals
  OR <- exp(coef(model))
  OR.CI <- exp(confint(model))
  
  print(paste("Odds ratio for", failure, ":"))
  print(OR)
  print(paste("Confidence Interval for", failure, ":"))
  print(OR.CI)
  
  # Create a confusion matrix with predictions on rows and testset2 on columns
  confusion_matrix <- confusionMatrix(as.factor(y.hat), as.factor(testset2[[failure]]), mode = "everything")
  print(paste("Confusion matrix for", failure, ":"))
  print(confusion_matrix)
  
  print(paste("VIF for", failure, ":"))
  print(vif(model))
  cat("\n")
}
}

# CART Analysis (After Oversampling) ------------------------------------------------------------------

# Training the CART Model
{
cart_model <- rpart(Machine.failure ~ Type + Air.temperature..K. + Process.temperature..K. + 
                      Rotational.speed..rpm. + Torque..Nm. + Tool.wear..min., 
                    data = trainset2, method = "class",
                    control = rpart.control(minsplit = 2, cp = 0))

rpart.plot(cart_model, nn= T, main = "Maximal Tree for machine.csv")
print(cart_model)

predictions <- predict(cart_model, newdata = testset2, type = "class")

class(predictions)

# Create a confusion matrix
confusion_matrix <- confusionMatrix(predictions, testset2$Machine.failure, mode = "everything")

# Print the confusion matrix
print(confusion_matrix)
}

# Model Pruning
{
plotcp(cart_model, main = "Subtrees in machine.csv")
printcp(cart_model)

# Extracting the Optimal Tree
CVerror.cap <- cart_model$cptabl[which.min(cart_model$cptable[,"xerror"]), "xerror"] + cart_model$cptable[which.min(cart_model$cptable[,"xerror"]), "xstd"]

# Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree cart_model.
i <- 1; j<- 4
while (cart_model$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}

# Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
cp.opt = ifelse(i > 1, sqrt(cart_model$cptable[i,1] * cart_model$cptable[i-1,1]), 1)

#prune tree using found cp value 
cart_model2 <- prune(cart_model, cp = cp.opt) 
printcp(cart_model2,digits = 3)

print(cart_model2)
rpart.plot(cart_model2, nn= T, main = "Optimal Tree for machine.csv")

variable_importance<- cart_model2$variable.importance
## Torque(Nm) has the highest importance. Process Temperature (K) is secondary. Type has the least importance. 

sorted_importance <- sort(variable_importance, decreasing = TRUE)
total_importance <- sum(sorted_importance)
percentages <- sorted_importance / total_importance * 100  # Calculate percentages
{
sorted_importance <- sort(variable_importance, decreasing = TRUE)
total_importance <- sum(sorted_importance)
percentages <- sorted_importance / total_importance * 100  
upper_ylim <- max(sorted_importance) * 1.2  

bar_midpoints <- barplot(sorted_importance, main = "Variable Importance in CART Model",
                         ylab = "Importance",
                         ylim = c(0, upper_ylim),
                         col = "grey", las = 1, 
                         xaxt = 'n') 

text(x = bar_midpoints, y = par("usr")[3] - par("usr")[3] * 0.1, 
     labels = names(sorted_importance), srt = 45, adj = 1,
     xpd = TRUE, cex = 0.8)

# Adding the percentage labels at the top of each bar
text(x = bar_midpoints, y = sorted_importance + upper_ylim * 0.05, 
     labels = sprintf("%.1f%%", percentages), 
     pos = 3, 
     cex = 0.9, col = "black") 
}

predictions_final <- predict(cart_model2, newdata = testset2, type = "class")
summary(testset2$Machine.failure)
summary(predictions_final)

confusion_matrix_final <- confusionMatrix(predictions_final, testset2$Machine.failure, mode = "everything")
print(confusion_matrix_final)
}

# CART Analysis to predict type of Machine Failure (After Oversampling)-----------------------------------

# Loop through each failure type
for (failure in failure_types) {
  # Develop a CART model on the trainset2
  formula <- paste(failure, "~ Type + Air.temperature..K. + Process.temperature..K. + Rotational.speed..rpm. + Torque..Nm. + Tool.wear..min.")
  cart_model <- rpart(as.formula(formula), data = trainset2, method = "class",
                      control = rpart.control(minsplit = 2, cp = 0))  
  
  # Extracting the Optimal Tree
  CVerror.cap <- cart_model$cptable[which.min(cart_model$cptable[,"xerror"]), "xerror"] + cart_model$cptable[which.min(cart_model$cptable[,"xerror"]), "xstd"]
  
  # Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree cart_model.
  i <- 1; j<- 4
  while (cart_model$cptable[i,j] > CVerror.cap) {
    i <- i + 1
  }
  
  # Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
  cp.opt = ifelse(i > 1, sqrt(cart_model$cptable[i,1] * cart_model$cptable[i-1,1]), 1)
  
  # Prune tree using found cp value 
  cart_model2 <- prune(cart_model, cp = cp.opt)
  
  rpart.plot(cart_model2, nn = TRUE, main = paste("Optimal Tree for", failure))
  
  variable_importance<- cart_model2$variable.importance
  sorted_importance <- sort(variable_importance, decreasing = TRUE)
  total_importance <- sum(sorted_importance)
  percentages <- sorted_importance / total_importance * 100  # Calculate percentages
  {
    sorted_importance <- sort(variable_importance, decreasing = TRUE)
    total_importance <- sum(sorted_importance)
    percentages <- sorted_importance / total_importance * 100  
    upper_ylim <- max(sorted_importance) * 1.2  
    
    bar_midpoints <- barplot(sorted_importance, main = paste("Variable Importance in CART Model for", failure),
                             ylab = "Importance",
                             ylim = c(0, upper_ylim),
                             col = "grey", las = 1, 
                             xaxt = 'n') 
    
    text(x = bar_midpoints, y = par("usr")[3] - par("usr")[3] * 0.1, 
         labels = names(sorted_importance), srt = 45, adj = 1,
         xpd = TRUE, cex = 0.8)
    
    # Adding the percentage labels at the top of each bar
    text(x = bar_midpoints, y = sorted_importance + upper_ylim * 0.05, 
         labels = sprintf("%.1f%%", percentages), 
         pos = 3, 
         cex = 0.9, col = "black") 
  }
  
  # Predict machine failures on the testset2 using the model
  predictions_type <- predict(cart_model2, newdata = testset2, type = "class")
  levels(predictions_type) <- levels(testset2[[failure]])
  
  # Create a confusion matrix with predictions on rows and testset2 on columns
  confusion_matrix <- confusionMatrix(predictions_type, testset2[[failure]], mode = "everything")
  
  # Print confusion matrix results
  cat("Confusion matrix for", failure, ":\n")
  print(confusion_matrix)
  cat("\n")
}


# K-Nearest Neighbours to predict Machine Failure (After Oversampling) ------------------------------
{

# Converting Type into numeric
trainset2$Type <- as.numeric((trainset2$Type))
testset2$Type <- as.numeric((testset2$Type))

# Scaling the data
train_scaled <- scale(trainset2[, 1:6])
test_scaled <- scale(testset2[, 1:6], 
                     center = attr(train_scaled, "scaled:center"),
                     scale = attr(train_scaled, "scaled:scale"))


# Choosing the value of k
k_values <- c(1:100)
best_k <- NULL
best_accuracy <- 0
class_labels <- as.factor(trainset2$Machine.failure)

for (k in k_values) {
  knn_pred <- knn(train = train_scaled, test = test_scaled, cl = class_labels, k = k)
  accuracy <- mean(knn_pred == testset2$Machine.failure)
  
  if (accuracy > best_accuracy) {
    best_accuracy <- accuracy
    best_k <- k
  }
}

print(paste("Best k:", best_k))
print(paste("Best accuracy:", best_accuracy))

# Predicting machine failure on test set
knn_pred <- knn(train = train_scaled, test = test_scaled, cl = trainset2$Machine.failure, k = best_k)

# Create a confusion matrix with predictions on rows and testset2 on columns
confusion_matrix <- confusionMatrix(knn_pred, testset2$Machine.failure, mode = "everything")
print(confusion_matrix)
}

# K-Nearest Neighbours to predict type of Machine Failure (After Oversampling) ----------------------
{
# Converting Type into numeric
trainset2$Type <- as.numeric((trainset2$Type))
testset2$Type <- as.numeric((testset2$Type))
  
# Scaling the failure data
train_scaled <- scale(trainset2[, 1:6])
test_scaled <- scale(testset2[, 1:6], 
                     center = attr(train_scaled, "scaled:center"),
                     scale = attr(train_scaled, "scaled:scale"))

for (failure in failure_types){
  # Choosing the value of k
  k_values <- c(1:100)
  best_k <- NULL
  best_accuracy <- 0
  class_labels <- as.factor(trainset2[[failure]])
  
  for (k in k_values) {
    knn_pred <- knn(train = train_scaled, test = test_scaled, cl = class_labels, k = k)
    accuracy <- mean(knn_pred == testset2[[failure]])
    
    if (accuracy > best_accuracy) {
      best_accuracy <- accuracy
      best_k <- k
    }
  }
  
  print(paste("For", failure, "failure:"))
  print(paste("Best k:", best_k))
  print(paste("Best accuracy:", best_accuracy))
  
  # Predicting machine failure on test set
  knn_pred <- knn(train = train_scaled, test = test_scaled, cl = trainset2[[failure]], k = best_k)
  
  # Create a confusion matrix with predictions on rows and testset2 on columns
  confusion_matrix <- confusionMatrix(knn_pred, testset2[[failure]], mode = "everything")
  print(confusion_matrix)
  cat("\n")
}
}

# Thank You ! -------------------------------------------------------------------------------------------
