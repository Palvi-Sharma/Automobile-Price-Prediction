# PRICE AND AUTOMOBILE

automobile_data = read.csv("C:/Users/Palvi/Desktop/Fall Semester/MGSC 661 - Serpa/FINAL PROJECT/Dataset 5 — Automobile data.csv")
attach(automobile_data)
install.packages("ggplot2")
library(ggplot2)
library(dplyr)
library(cluster)
library(factoextra)
library(reshape2)

# Initial EDA
plot(automobile_data$price, automobile_data$horsepower, xlab = "Car Price",
     ylab = "Horsepower", main = "Price vs Horsepower")
plot(automobile_data$engine.size, automobile_data$city.mpg, xlab = "Engine Size (cc)",
     ylab = "City MPG",main = "Engine Size vs City MPG")

automobile_data$price = as.numeric(automobile_data$price)
ggplot(automobile_data, aes(x = factor(1), y = price)) +
  geom_boxplot() +
  labs(title = "Boxplot of Price", x = "", y = "Price") +
  theme_minimal()


# Missing Values
automobile_data[automobile_data == '?'] = NA
automobile_data = na.omit(automobile_data)
automobile_data$horsepower = as.numeric(automobile_data$horsepower)


################################ Correlation ###############################################################

correlation_matrix = cor(automobile_data[, sapply(automobile_data, is.numeric)], use = "complete.obs")
correlation_long = melt(correlation_matrix)

# Plot 
ggplot(data = correlation_long, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  geom_text(aes(label = round(value, 2)), size = 3) +
  theme_minimal() +                                  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Correlation Matrix", x = "", y = "") 


# Dropping columns to resolve multicollinearity
automobile_data = automobile_data[, !names(automobile_data) %in% c("highway.mpg")]
automobile_data = automobile_data[, !names(automobile_data) %in% c("curb.weight")]
automobile_data = automobile_data[, !names(automobile_data) %in% c("engine.size")]

numeric_columns = sapply(automobile_data, is.numeric)
numeric_data = automobile_data[, numeric_columns]



############################### Clustering the data #####################################################################################

# Elbow Method for optimal k
fviz_nbclust(numeric_data, kmeans, method = "wss") +
  labs(title = "Elbow Method for Optimal K", x = "Number of Clusters", y = "Total Within-Cluster Sum of Squares")


# K-means clustering (with k = 3, based on elbow plot)
set.seed(123)
kmeans_model = kmeans(numeric_data, centers = 3, nstart = 25)
automobile_data$Cluster = as.factor(kmeans_model$cluster)

# Visualize clusters using PCA
pca_model = prcomp(numeric_data)
pca_data = as.data.frame(pca_model$x[, 1:2])  
pca_data$Cluster = automobile_data$Cluster

# Plot the clusters
ggplot(pca_data, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(size = 2) +
  labs(title = "Clusters Visualized in 2D (PCA)", x = "Principal Component 1", y = "Principal Component 2") +
  theme_minimal() +
  scale_color_brewer(palette = "Set2")

# Silhouette Score 
silhouette_scores = silhouette(as.numeric(automobile_data$Cluster), dist(numeric_data))
silhouette_scores

#visualize silhouette score
fviz_silhouette(silhouette_scores) +
  labs(title = "Silhouette Plot for Clustering",
       x = "Silhouette Width",
       y = "Clusters") +
  theme_minimal()


####################### Cluster Analysis and Visualizations #############################################

# Summary
cluster_summary = aggregate(numeric_data, by = list(Cluster = automobile_data$Cluster), FUN = mean)
print(cluster_summary)

# Cluster sizes
cluster_sizes = table(automobile_data$Cluster)
print(cluster_sizes)

# Visualize cluster sizes
ggplot(as.data.frame(cluster_sizes), aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  labs(title = "Cluster Size Distribution", x = "Cluster", y = "Number of Observations") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

# Price
ggplot(automobile_data, aes(x = Cluster, y = price, fill = Cluster)) +
  geom_boxplot() +
  labs(title = "Price Distribution by Cluster", x = "Cluster", y = "Price") +
  theme_minimal()

# Fuel type 
ggplot(automobile_data, aes(x = Cluster, fill = fuel.type)) +
  geom_bar(position = "fill") +
  labs(title = "Fuel Type Distribution by Cluster", x = "Cluster", y = "Proportion") +
  theme_minimal()


# City MPG
ggplot(automobile_data, aes(x = Cluster, y = city.mpg, fill = Cluster)) +
  geom_boxplot() +
  labs(title = "City MPG by Cluster", x = "Cluster", y = "City MPG") +
  theme_minimal()

# Drive wheels 
ggplot(automobile_data, aes(x = Cluster, fill = drive.wheels)) +
  geom_bar() +
  labs(title = "Drive Wheels by Cluster", x = "Cluster", y = "Proportion") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")

# length 
ggplot(automobile_data, aes(x = Cluster, y = length, fill = Cluster)) +
  geom_boxplot() +
  labs(title = "Car length by Cluster", x = "Cluster", y = "length") +
  theme_minimal()

# width 
ggplot(automobile_data, aes(x = Cluster, y = width, fill = Cluster)) +
  geom_boxplot() +
  labs(title = "Car width by Cluster", x = "Cluster", y = "width") +
  theme_minimal()

# wheel base 
ggplot(automobile_data, aes(x = Cluster, y = wheel.base, fill = Cluster)) +
  geom_boxplot() +
  labs(title = "Wheel Base by Cluster", x = "Cluster", y = "Wheel base") +
  theme_minimal()

# horse power 
ggplot(automobile_data, aes(x = Cluster, y = horsepower, fill = Cluster)) +
  geom_boxplot() +
  labs(title = "Horse power by Cluster", x = "Cluster", y = "Horse power") +
  theme_minimal()

# Feature importance for clustering
library(randomForest)
clustering = randomForest(Cluster ~ ., data = automobile_data, importance = TRUE)

# Visualize feature importance
importance = as.data.frame(clustering$importance)
importance$Feature = rownames(importance)

ggplot(importance, aes(x = reorder(Feature, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Feature Importance for Clustering", x = "Feature", y = "Importance (Mean Decrease Gini)") +
  theme_minimal()



############################# Random Forest ###########################################################################

# Feature Engineering
automobile_data$Cluster = as.factor(automobile_data$Cluster)

set.seed(123)
train_indices = sample(1:nrow(automobile_data), 0.7 * nrow(automobile_data))
train_data = automobile_data[train_indices, ]
test_data = automobile_data[-train_indices, ]

#Random Forest Feature Importance
final_rf = randomForest(price ~ ., data = train_data, ntree = 500, mtry = 4)
varImpPlot(final_rf)

# Extract importance from the final_rf model
importance_rf = as.data.frame(importance(final_rf))
importance_rf$Feature = rownames(importance_rf)

# Plot 
ggplot(importance_rf, aes(x = reorder(Feature, IncNodePurity), y = IncNodePurity)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Random Forest Variable Importance",
    x = "Feature",
    y = "Increase in Node Purity"
  ) +
  theme_minimal()

# Cross-validation setup
library(caret)
cv_control = trainControl(
  method = "cv",
  number = 10,
  verboseIter = TRUE
)

# Train Random Forest Model with Cross-Validation
rf_cv_model = train(
  price ~ city.mpg + width + length + wheel.base + horsepower + Cluster,
  data = train_data,
  method = "rf",
  trControl = cv_control,
  tuneLength = 5,
)

summary(rf_cv_model)

# Evaluate Random Forest Model
test_predictions = predict(rf_cv_model, newdata = test_data)
mse = mean((test_predictions - test_data$price)^2)
rmse = sqrt(mse)
r_squared = 1 - sum((test_predictions - test_data$price)^2) / 
  sum((test_data$price - mean(test_data$price))^2)

cat("Test RMSE:", rmse, "\n")
cat("Test R-squared:", r_squared, "\n")


importance = varImp(rf_cv_model, scale = TRUE)
print(importance)

library(stargazer)

importance_df = data.frame(
  Feature = c("Cluster2", "width", "length", "horsepower", "Cluster3", "city.mpg", "wheel.base"),
  Overall = c(100.0000, 7.2472, 4.8707, 4.5473, 3.0442, 0.4042, 0.0000)
)


stargazer(
  importance_df,
  type = "html",  summary = FALSE,
  title = "Variable Importance from Random Forest Model",
  rownames = FALSE
)

# Plot Actual vs Predicted Prices
ggplot(data.frame(Actual = test_data$price, Predicted = test_predictions), aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Random Forest: Actual vs Predicted Prices", x = "Actual Price", y = "Predicted Price") +
  theme_minimal()


############################### THE END #############################################################

