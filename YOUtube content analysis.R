
# Read the CSV file into a variable named data_physics and data_maths
data_physics <- read.csv("C:/Users/admin/Downloads/p.csv")
data_maths <- read.csv("C:/Users/admin/Downloads/m.csv")

#1 VIEW COUNT ANALYSIS

#  Calculate the average, median, maximum, and minimum view counts
average_view_count <- mean(data_physics$viewCount)
median_view_count <- median(data_physics$viewCount)
max_view_count <- max(data_physics$viewCount)
min_view_count <- min(data_physics$viewCount)

# Print the calculated statistics
cat("Average View Count (physics):", average_view_count, "\n")
cat("Median View Count (physics):", median_view_count, "\n")
cat("Maximum View Count (physics):", max_view_count, "\n")
cat("Minimum View Count (physics):", min_view_count, "\n")

# Calculate the average, median, maximum, and minimum view counts
average_view_count_m <- mean(data_maths$viewCount)
median_view_count_m <- median(data_maths$viewCount)
max_view_count_m <- max(data_maths$viewCount)
min_view_count_m <- min(data_maths$viewCount)

# Print the calculated statistics
cat("Average View Count (maths):", average_view_count_m, "\n")
cat("Median View Count (maths):", median_view_count_m, "\n")
cat("Maximum View Count (maths):", max_view_count_m, "\n")
cat("Minimum View Count (maths):", min_view_count_m, "\n")

#quantile
quantiles <- quantile(data_physics$viewCount, probs = c(0.25, 0.75))
iqr <- quantiles[2] - quantiles[1]
lower_fence <- quantiles[1] - 1.5 * iqr
upper_fence <- quantiles[2] + 1.5 * iqr

# Identify videos with view counts beyond the fences

outliers_with <- data_physics[data_physics$viewCount < lower_fence | data_physics$viewCount > upper_fence, ]
outliers_without <- data_physics[data_physics$viewCount < lower_fence , ]
print("Videos with exceptionally high or low view counts:\n")
print("with lowerfence")
print(outliers_with)
print("without lower fence")
print(outliers_without)



#2.ENGAGEMENT ANALYSIS
# Compute correlation matrix
correlation_matrix <- cor(data_maths[c("viewCount", "likeCount", "commentCount")])

# Visualize correlation matrix
install.packages("corrplot")

library(corrplot)
corrplot(correlation_matrix, method = "color", type = "lower", tl.cex = 0.7, tl.col = "black", col = colorRampPalette(c("blue", "white", "red"))(20))

correlation_matrix

# Calculate engagement rates for mathematics videos
data_maths$likes_per_view_math <- data_maths$likeCount / data_maths$viewCount
data_maths$comments_per_view_math <- data_maths$commentCount / data_maths$viewCount


# Calculate engagement rates for physics videos
data_physics$likes_per_view_physics <- data_physics$likeCoun / data_physics$viewCount
data_physics$comments_per_view_physics <- data_physics$commentCount / data_physics$viewCount

# Create scatter plot for likes per view
# Adjust plot margins
par(mar = c(6, 6, 2, 2))  # Set bottom, left, top, and right margins

plot(data_physics$viewCount, 
     data_physics$likes_per_view_physics, 
     main = "Likes per View - PHYSICS", 
     xlab = "View Count", 
     ylab = "Likes per View", 
     col = "blue",
     xaxt = "n")  # Suppress automatic x-axis labels

# Define custom axis labels format
axis_labels <- format(data_physics$viewCount, scientific = FALSE, big.mark = ",")
axis(side = 1, at = data_physics$viewCount, labels = axis_labels, las = 2)  # las = 2 for vertical labels

#TIMESERIES ANALYSIS

# Plot time series of view counts over time




plot(data_maths$publishDate, data_maths$viewCount, type = "l", 
     main = "View Counts Over Time--maths", xlab = "Publish Date",ylab=" ",yaxt = "n")

axis_labels <- format(data_maths$viewCount, scientific = FALSE, big.mark = ",")
axis(side = 2, at = data_maths$viewCount, labels = axis_labels, las = 2)

#-----------

data_physics$publishDate <- as.POSIXct(data_physics$publishDate, format = "%d-%m-%Y %H:%M")


# Plot time series of view counts over time
plot(data_physics$publishDate, data_physics$viewCount, type = "l", 
     main = "View Counts Over Time (Physics)", xlab = "Publish Date", ylab = " ",yaxt = "n")

# Manually format the y-axis labels to display integers
axis_labels <- format(data_maths$viewCount, scientific = FALSE, big.mark = ",")
axis(side = 2, at = data_maths$viewCount, labels = axis_labels, las = 2)



# Convert publish date to Date format
data_physics$publishDate <- as.Date(data_physics$publishDate, format = "%d-%m-%Y %H:%M")



# Extract month from the publishDate
data_physics$month <- format(data_physics$publishDate, "%B")  # Full month name

# Aggregate view counts by month
engagement_by_month <- aggregate(viewCount ~ month, data = data_physics, FUN = sum)

# Plot the aggregated data
barplot(engagement_by_month$viewCount, names.arg = engagement_by_month$month,
        main = "Total View Counts by Month (physics)",
        xlab = "Month", ylab = "  ",yaxt="n")
# Manually format the y-axis labels to display integers
axis_labels <- format(data_physics$viewCount, scientific = FALSE, big.mark = ",")
axis(side = 2, at = data_physics$viewCount, labels = axis_labels, las = 2)


