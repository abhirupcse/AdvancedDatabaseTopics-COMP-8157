#Install Packages
install.packages("stats")
install.packages("ggplot2")
install.packages("dbscan")

#Related library files
library(ggplot2)
library(dbscan)

# 1 import the file
dataset <- read.csv("/Users/abhirup/Desktop/RLanguage/Vehicle.csv")
#head
head(dataset)
#display the summary
summary(dataset)

# 2 structure of the dataset provided
str(dataset)
# Show dimensions of the dataset
dim(dataset)

# 3 Print the first 3 entry from the csv file
head(dataset, n = 3)
# Print the last 6 entry from the csv file
tail(dataset,n=6)


# 4 Using the 'aggregate' function to calculate the average for Kms_Driven for each Car_Name
average_kms <- aggregate(dataset$Kms_Driven, by = list(dataset$Car_Name), FUN = mean)
# Rename the columns in the result
colnames(average_kms) <- c("Car_Name", "Average_Kms_Driven")
# Print the result
print(average_kms)


# 5 Using the 'aggregate' function to calculate the average for Selling_Price for each Car_Name
average_sp <- aggregate(dataset$Selling_Price, by = list(Year = dataset$Year), FUN = mean)
# Rename the columns in the result
colnames(average_sp) <- c("Year", "Average_Selling_Price of the cars")
# Print the result
print(average_sp)


# 6 Create a new dataframe with unique combinations of Car_Name, Fuel_Type, Seller_Type, and Transmission
unique_elements <- unique(subset(dataset, select = c("Car_Name", "Fuel_Type", "Seller_Type", "Transmission")))
# Print the unique combinations
print(unique_elements)


# 7 Create a new column with the required column name as Car_Name, Fuel_Type, Seller_Type, and Transmission
dataset$Combine <- paste(dataset$Car_Name, dataset$Fuel_Type, dataset$Seller_Type, dataset$Transmission)
# Count the frequency of each column
combine_count <- table(dataset$Combine)
# Sort combinations and their frequencies in ascending order
sort_combine_asc <- sort(combine_count)
print(sort_combine_asc)
# Sort combinations and their frequencies in descending order
sort_combine_des <- sort(combine_count, decreasing = TRUE)
print(sort_combine_des)


#8 check if any empty value in the dataset
any_missing <- anyNA(dataset)
print(any_missing)

#9 Check the missing values and column in the dataset.
missing_values <- colSums(is.na(dataset))
print(missing_values)
missed_columns <- names(missing_values)[missing_values]
print(missed_columns)


#10 Replace the empty data with most occurring one from dataset  
for (column in names(dataset)) {
  mode_value <- names(which.max(table(dataset[[column]])))
  dataset[[column]][is.na(dataset[[column]])] <- mode_value
}
missing_values <- sapply(dataset, function(x) sum(is.na(x)))
print(missing_values)


#11 Finding duplicate rows and remove them if exist
dup_rows <- dataset[duplicated(dataset), ]
# Remove duplicate rows
dataset_without_dup_rows <- dataset[!duplicated(dataset), ]
# Duplicate check
if (nrow(dup_rows) > 0) {
  cat("Duplicate rows has been identified and removed successfully!!.\n")
} else {
  cat("No duplicate rows found in the dataset!!.\n")
}
# Update the dataset with unique rows
dataset <- dataset_without_dup_rows
print(dataset)

#11 Finding duplicate rows and remove them if exist

dup_rows <- dataset[duplicated(dataset),]
if (nrow(dup_rows) > 0)
{
  cat("Duplicate rows has been identified and removed successfully!!.\n")
  
  # Remove duplicate rows
  dataset_without_dup_rows <- dataset[!duplicated(dataset), ]
  
  # Update the dataset with unique rows
  data <- dataset_without_dup_rows
  print(data)
} else
{
  cat("No duplicate rows found in the dataset!!.\n")
}


#12 Replacing the data 
#problem: a Fuel_Type: “Petrol”: 0, “Diesel”: 1, “CNG”: 2
dataset$Fuel_Type <- replace(dataset$Fuel_Type, dataset$Fuel_Type == "Petrol", 0)
dataset$Fuel_Type <- replace(dataset$Fuel_Type, dataset$Fuel_Type == "Diesel", 1)
dataset$Fuel_Type <- replace(dataset$Fuel_Type, dataset$Fuel_Type == "CNG", 2)
print(dataset)
#problem: b Seller_Type: “Dealer”: 0, “Individual”: 1
dataset$Seller_Type <- replace(dataset$Seller_Type, dataset$Seller_Type == "Dealer", 0)
dataset$Seller_Type <- replace(dataset$Seller_Type, dataset$Seller_Type == "Individual", 1)
print(dataset)
#problem: c #Transmission: “Manual”: 0, “Automatic”: 1
dataset$Transmission <- replace(dataset$Transmission, dataset$Transmission == "Manual", 0)
dataset$Transmission <- replace(dataset$Transmission, dataset$Transmission == "Automatic", 1)
print(dataset)
#Displaying the updated column
cat("Fuel_Type values after Replacement:\n")
table(dataset$Fuel_Type)
cat("\nSeller_Type values after Replacement:\n")
table(dataset$Seller_Type)
cat("\nTransmission values after Replacement:\n")
table(dataset$Transmission)


#13 Add and new field  and use value of year for that field
dataset$Year <- as.numeric(dataset$Year)
# Calculate current year by function Sys.Date() hence age = current-year
current_year <- as.integer(format(Sys.Date(), "%Y"))
# Add new Age field
dataset$Age <- current_year - dataset$Year
# Show the output
print(dataset)


# 14 Create a new updated dataset for columns “Car_name”, “Selling_Price”, “Present_Price”, and “Kms_Drive”.
new_dataset <- subset(dataset, select = c("Car_Name", "Selling_Price", "Present_Price", "Kms_Driven"))
print(new_dataset)


#15 Shuffle the rows of main dataset
shuffle_dataset <- dataset[sample(nrow(dataset)), ]
print(shuffle_dataset)


#16 Create a Scatter Plot
#a. Red for Transmission type ‘0’ and Blue for ‘1’.
transmission_colour <- c("0" = "red", "1" = "blue")
# Scatter plot
plot(dataset$Present_Price, dataset$Selling_Price,
     col = transmission_colour[as.character(dataset$Transmission)],
     xlab = "Present_Price", ylab = "Selling_Price",
     main = "Selling_Price vs Present_Price",
     pch = 16)
legend("topright", legend = c("Manual", "Automatic"),
       col = c("red", "blue"), pch = 16, title = "Transmission")
#b Adding open triangles to the plot
plot(dataset$Present_Price, dataset$Selling_Price,
     col = transmission_colour[as.character(dataset$Transmission)],
     xlab = "Present_Price", ylab = "Selling_Price",
     main = "Selling_Price vs Present_Price",
     pch = ifelse(dataset$Transmission == 0, 2, 3))


# 17 Box plot for Selling_Price Vs Transmission and Fuel_Type
dataset$Selling_Price <- as.numeric(dataset$Selling_Price)
dataset$Transmission <- as.numeric(dataset$Transmission)
dataset$Fuel_Type <- as.numeric(dataset$Fuel_Type)

boxplot(Selling_Price ~ Transmission + Fuel_Type, data = dataset,
        main = "Selling_Price vs. Transmission & Fuel_Type",
        xlab = "Transmission & Fuel_Type", ylab = "Selling_Price",
        col = c("blue", "red", "green"),
        names = c("Manual-Petrol", "Automatic-Petrol", "Manual-Diesel", "Automatic-Diesel", "Manual-CNG", "Automatic-CNG"),
        outline = FALSE)

legend("topright", legend = c("Petrol", "Diesel", "CNG"),
       fill = c("blue", "red", "green"), title = "Fuel_Type")


#18 Scatter plot for Selling_Price Vs Kms_Driven
dataset$Selling_Price <- as.numeric(dataset$Selling_Price)
dataset$Kms_Driven <- as.numeric(dataset$Kms_Driven)

plot(dataset$Kms_Driven, dataset$Selling_Price, 
     xlab = "Kms_Driven", ylab = "Selling_Price",
     main = "Selling_Price vs Kms_Driven")
#using k-means clustering for 4
k <- 4  
set.seed(123)  
cluster <- kmeans(dataset[, c("Kms_Driven", "Selling_Price")], centers = k)
#Colourcoding
colors <- c("red", "orange", "blue", "green")
points(dataset$Kms_Driven, dataset$Selling_Price, 
       col = colors[cluster$cluster], pch = 16)

points(cluster$centers[, "Kms_Driven"], cluster$centers[, "Selling_Price"],
       col = "black", pch = 4, cex = 2)

legend("topright", legend = paste("Cluster", 1:k), col = colors, pch = 16)


#19 Scatter plotting for the Selling_Price Vs Present_Price using hierarchical clustering
# Building a new dataframe with Selling_Price and Present_Price data
data_frame <- dataset[, c("Selling_Price", "Present_Price")]
# Doing hierarchical clustering here
hierarchicalClust <- hclust(dist(data_frame))
#Cluster the points into 3 clusters
clusterPoint <- cutree(hierarchicalClust, k = 3)
# Now putting the cluster info to dataframe
data_frame$Clusters <- factor(clusterPoint)
# Plot the scatter
ggplot(data_frame, aes(x = Present_Price, y = Selling_Price, color = Clusters)) +
  geom_point(size = 3) +
  labs(x = "Present_Price", y = "Selling_Price", title = "Selling_Price vs Present_Price") +
  scale_color_manual(values = c("red", "blue", "green"))  # Customizing cluster colours


#20 Adding a new column as ‘Age’, and calculate it ‘Year’
#a. ‘Age’, ‘Year’, ‘Transmission’, ‘Seller_Type’, ‘Fuel_Type’ and ‘Owner’
#b. Add labels, titles, and colours to the plot.

# The age was already caculated in the previous question i.e. Q.13 hence using the same concept
# Adding the 'Age' field based on 'Year'
dataset$Age <- as.numeric(format(Sys.Date(), "%Y")) - dataset$Year
new_dataset <- subset(dataset, select = c("Age", "Year", "Transmission", "Seller_Type", "Fuel_Type", "Owner"))
print(new_dataset)

barFields <- c("Age", "Year", "Transmission", "Seller_Type", "Fuel_Type", "Owner")
# Create Barplot for fields
par(mfrow = c(3, 2)) # Barplot 3x2 grid
for (field in barFields) {
  counts <- table(dataset[[field]])
  barplot(counts, main = field, xlab = field, ylab = "Count", col = rainbow(length(counts)))
}


#21 Correlation plot for entier dataset variables
# Removing 'Car_Name' and 'Combine' field from the dataset as they are string
dataset <- dataset[, !(names(dataset) %in% c("Car_Name"))]
dataset <- dataset[, !(names(dataset) %in% c("Combine"))]
# Converting few variables to numeric data type
dataset$Present_Price <- as.numeric(dataset$Present_Price)
dataset$Kms_Driven <- as.numeric(dataset$Kms_Driven)
dataset$Fuel_Type <- as.numeric(dataset$Fuel_Type)
dataset$Seller_Type <- as.numeric(dataset$Seller_Type)
dataset$Transmission <- as.numeric(dataset$Transmission)
dataset$Owner <- as.numeric(dataset$Owner)
dataset$Age <- as.numeric(dataset$Age)
dataset$Year <- as.numeric(dataset$Year)
# Compute correlation matrix
cor_dataset <- cor(dataset)
ggplot(data = reshape2::melt(cor_dataset), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Correlation Plot", x = "", y = "") +
  theme_minimal()


#22 Scatter plot for Selling_Price Vs Kms_Driven using DBSCAN clustering and cluster 3 points.  
install.packages("dbscan")
library(dbscan)
# Creating a new dataframe with the required columns
newdata_frame <- data.frame(Selling_Price = dataset$Selling_Price, Kms_Driven = dataset$Kms_Driven)
# Perform DBSCAN clustering
  dbscan_data <- dbscan(newdata_frame, eps = 1, MinPts = 3)
# Create a scatter plot
ggplot(newdata_frame, aes(x = Selling_Price, y = Kms_Driven, color = factor(dbscan_data$cluster))) +
  geom_point() +
  labs(x = "Selling Price", y = "Kms Driven", title = "Selling Price vs Kms Driven with DBSCAN Clustering") +
  scale_color_discrete(name = "Clusters") +
  theme_minimal()


