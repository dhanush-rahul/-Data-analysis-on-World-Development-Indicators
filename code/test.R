# Set the working directory
setwd("C:/Graduate-Studies/AdvanceDataAnalytics/dataset")
library(dplyr)
library(proxy)
library(gplots)
library(RColorBrewer)
library(ggplot2)
library(gganimate)
library(stringr)
library(pheatmap)
library(animation)
library(transformr)
library(fpc)
library(cluster) 
library(amap)
library(tidyr)
library(reshape2)
library(magrittr)
library(gifski)

clean_dataset <- function(df){

	# Checking the number of rows in the DataFrame
	#cat("Number of rows", nrow(df), "\n")
	
	# Replacing all empty '..' with NA
	df[df == ".."] <- NA

	# Cleaning the Missing Values
	cat("Missing Values BEFORE Cleaning\n")
	#print(colSums(is.na(df)))

	for (column in names(df)[5:ncol(df)]) {
  		df[[column]] <- as.numeric(as.character(df[[column]]))
	}

	for (column in names(df)[5:ncol(df)]) {
  		df[[column]][is.na(df[[column]])] <- mean(df[[column]], na.rm = TRUE)
	}

	#cat("Missing Values AFTER Cleaning\n")
	#print(colSums(is.na(df)))

	# Cleaning the Duplicate Values
	subset_cols <- c("Country.Name", "Time")
	duplicate_rows <- df[duplicated(df[, subset_cols]) | duplicated(df[, subset_cols], fromLast = TRUE), ]
	
	if (nrow(duplicate_rows) == 0) {
  		cat("No Duplicates found\n")
	} else {
  		cat("Duplicate Rows:\n")
  		# print(duplicate_rows)

  		# Delete duplicate rows
  		df <- df[!duplicated(df[subset_cols]) & complete.cases(df[subset_cols]), ]
  		cat("Duplicates deleted. Updated DataFrame:\n")
  		# print(df)
	}

	cat("DATA HAS BEEN CLEANED!!!\n")

	# Return cleaned DataFrame
	return(df)
}

normalizeDataset <- function(df) {
  # Identify and drop non-numeric columns
  numeric_columns <- names(df)[sapply(df, is.numeric)]
  numeric_columns <- setdiff(numeric_columns, 'Time')
	
  non_numeric_columns <- setdiff(names(df), numeric_columns)

  # If there are non-numeric columns, drop them before normalization
  if (length(non_numeric_columns) > 0) {
    df_numeric <- df[, numeric_columns]
  } else {
    df_numeric <- df
  }

  # Create scalers
  standard <- scale(df_numeric)
  minmax <- apply(df_numeric, 2, function(x) (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
  robust_scaler <- scale(df_numeric, center = TRUE, scale = apply(df_numeric, 2, function(x) quantile(x, 0.75) - quantile(x, 0.25)))
  maxabs_scaler <- scale(df_numeric, center = FALSE, scale = apply(df_numeric, 2, max))
  maxabs <- apply(df_numeric, 2, function(x) x / max(abs(x), na.rm = TRUE))
  robust <- apply(df_numeric, 2, function(x) (x - median(x, na.rm = TRUE)) / IQR(x, na.rm = TRUE))

  # Create plots
  print("Bhenchodo")
  par(mfrow = c(2, 1))
  matplot(df$Time, df_numeric, type = "l", col = 1:ncol(df_numeric), lty = 1,
        main = 'Original', xlab = 'Time', ylab = 'Values')
  par(mfrow = c(1, 1))
  par(mfrow = c(2, 1))
  
  matplot(df$Time, standard, type = "l", col = 1:ncol(standard), lty = 1,
        main = 'Standard Scaling', xlab = 'Time', ylab = 'Values')
  par(mfrow = c(1, 1))
  par(mfrow = c(2, 1))
  
  matplot(df$Time, minmax, type = "l", col = 1:ncol(minmax), lty = 1,
        main = 'Min-Max Scaling', xlab = 'Time', ylab = 'Values')
  par(mfrow = c(1, 1))
  par(mfrow = c(2, 1))
  
  matplot(df$Time, maxabs, type = "l", col = 1:ncol(maxabs), lty = 1,
        main = 'Max Abs Scaler', xlab = 'Time', ylab = 'Values')
  par(mfrow = c(1, 1))
  par(mfrow = c(2, 1))
  
  matplot(df$Time, robust, type = "l", col = 1:ncol(robust), lty = 1,
        main = 'Robust Scaler', xlab = 'Time', ylab = 'Values')

  legend("topright", legend = colnames(df), col = 1:ncol(df), cex = 0.8)
  print("Bhenchodo")
  # Reset par to default settings
  par(mfrow = c(1, 1))

  # Create the normalized DataFrame
  normalized_df <- as.data.frame(standard)
  standard_normalized <- as.data.frame(standard)
  
  if (length(numeric_columns) > 0) {
    normalized_df <- cbind(df[, non_numeric_columns, drop = FALSE], normalized_df)
  }

  return(normalized_df)
}

calculate_similarity <- function(df){
	new_df <- df %>% select(-Time, -Time.Code, -Country.Code)
	avg_values <- new_df %>%
  			group_by(`Country.Name`) %>%
  			summarize_all(mean)
	avg_values <- avg_values %>% select(-Country.Name)
	avg_values_transposed <- t(avg_values)
	#print(dim(avg_values_transposed))

	similarity_matrix <- proxy::simil(x = avg_values_transposed, y = avg_values_transposed, method = "cosine")
	similarity_df <- as.data.frame(similarity_matrix)
	#print(dim(similarity_matrix))

	par(mfrow=c(2,2))
	#print("D")
	similarity_matrix_trimmed <- round(similarity_matrix, 2)
	

	print(rownames(similarity_matrix_trimmed))
	# Create a heatmap
windows()
 	heatmap_obj <- heatmap.2(similarity_matrix, dendogram = "none", scale = "none",
				col = colorRampPalette(brewer.pal(11, "RdYlGn"))(100),
				margins = c(15, 15),trace = "none",main = "Cosine Similarity Between Attributes",
				key.title = NA,cexRow = 1, cexCol = 1,keysize = 1,key = TRUE,
				density.info = "none",notecol="black",notecex=1,symm = TRUE, 
				cellnote=similarity_matrix_trimmed, Rowv = FALSE, Colv=FALSE)
	jpeg("heatmap.jpg", width = 800, height = 600)
	dev.off()
	par(mfrow = c(1, 1))
	mask <- lower.tri(similarity_matrix_trimmed, diag = TRUE)

	# Set the lower triangle values to NA
	similarity_matrix_trimmed[!mask] <- NA
	windows()
	heatmap_obj2 <- heatmap.2(similarity_matrix_trimmed, margins = c(15, 15),col = colorRampPalette(brewer.pal(11, 'RdYlGn'))(100),dendrogram = 'none', Rowv = FALSE, Colv = FALSE,main = 'Cosine Similarity Between Countries',key.title = NA, cexRow = 1, cexCol = 1, keysize = 1,key = TRUE,trace = 'none', notecol="black", cellnote = similarity_matrix_trimmed)
	jpeg("heatmap2.jpg", width = 800, height = 600)
	dev.off()
	
	return(avg_values)
}
normalize <- function(data) {
  return ((data - min(data)) / (max(data) - min(data)))
}
feature_creation <- function(df){
	weights_social <- c(
  		'Agricultural.land....of.land.area...AG.LND.AGRI.ZS.' = 0.25,
  		'Access.to.electricity....of.population...EG.ELC.ACCS.ZS.' = 0.25,
  		'Life.expectancy.at.birth..total..years...SP.DYN.LE00.IN.' = 0.25,
  		'Employment.to.population.ratio..15...total......modeled.ILO.estimate...SL.EMP.TOTL.SP.ZS.' = 0.25)
	# Assuming normalized_dataset is a data frame
	df$SDI <- 0

	# Iterate through the keys and add the weighted values to 'SDI' column
	for (key in names(weights_social)) {
		weight <- weights_social[key]
  		norm <- normalize(df[[key]])
  		# Adjust the following line based on your calculation
  		df$SDI <- df$SDI + ((norm * weight) * 100)
	}

	weights_financial <- c(
  		'GDP.per.capita..current.US....NY.GDP.PCAP.CD.' = 0.25,
  		'Current.account.balance..BoP..current.US....BN.CAB.XOKA.CD.' = 0.25,
  		'Total.reserves..includes.gold..current.US....FI.RES.TOTL.CD.' = 0.25,
  		'Net.migration..SM.POP.NETM.' = 0.25)
	# Assuming normalized_dataset is a data frame
	df$FDI <- 0

	# Iterate through the keys and add the weighted values to 'FDI' column
	for (key in names(weights_financial)) {
		weight <- weights_financial[key]
  		norm <- normalize(df[[key]])

  		# Adjust the following line based on your calculation
  		df$FDI <- df$FDI + ((norm * weight) * 100)
	}
	weights_environmental <- c(
  		'Forest.area....of.land.area...AG.LND.FRST.ZS.' = 0.33,
  		'CO2.emissions..metric.tons.per.capita...EN.ATM.CO2E.PC.' = 0.33,
  		'Urban.population....of.total.population...SP.URB.TOTL.IN.ZS.' = 0.34)
	# Assuming normalized_dataset is a data frame
	df$EFI <- 0

	# Iterate through the keys and add the weighted values to 'EFI' column
	for (key in names(weights_environmental )) {
		weight <- weights_environmental [key]
  		norm <- normalize(df[[key]])

  		# Adjust the following line based on your calculation
  		df$EFI <- df$EFI + ((norm * weight) * 100)
	}
	return(df)
}

plot_function <- function(i) {
  par(mfrow = c(4, 1), mar = c(5, 4, 2, 1))
  matplot(x[1:i, ], y[1:i, ], col = BROWN, type = "l", lwd = 5, xaxt = "n", ylab = "SDI", main = "Animated Plot")
  points(x[i, ], y[i, ], col = RED, pch = 16, cex = 1.5)
  text(x[i, ], y[i, ], labels = x[i, ], pos = 3, offset = 0, cex = 0.8)

  # Add labels and remove x-axis ticks
  mtext("Countries", side = 1, line = 3, cex = 1.2)
  axis(1, at = numeric(0))
}

graphs_for_new_features <- function(df){
print(colnames(df))
	# Group by 'Country.Name' and calculate the mean for 'SDI', 'FDI', 'EFI'
	avg_indices_by_country <- df %>%
  					group_by(Country.Name) %>%
  					summarize(SDI = mean(SDI),
            				FDI = mean(FDI),
            				EFI = mean(EFI))

	# Add 'Country' column
	avg_indices_by_country$Country <- avg_indices_by_country$Country.Name

	# Remove the original 'Country.Name' column
	avg_indices_by_country <- avg_indices_by_country %>% select(-Country.Name)

	# Print the result
	print(avg_indices_by_country)
	BROWN <- "#AD8C97"
	BROWN_DARKER <- "#7d3a46"
	GREEN <- "#2FC1D3"
	BLUE <- "#076FA1"
	GREY <- "#C7C9CB"
	GREY_DARKER <- "#5C5B5D"
	RED <- "#E3120B"
	
  x = avg_indices_by_country$Country
  y = avg_indices_by_country$SDI

windows()
# Assuming you have already loaded your data and calculated avg_indices_by_country

# Set up the plotting area
par(mfrow = c(1, 1), mar = c(5, 4, 2, 1))
# Extract data
x <- avg_indices_by_country$Country
y <- avg_indices_by_country$SDI

# Create a new plot with custom size and no points, and remove x-axis
plot(1, type = "n", xlab = "", ylab = "", xlim = c(1, length(y)), ylim = range(y), main = "SDI by Country", axes = FALSE)
rect(0.5, par("usr")[3], length(y) + 0.5, par("usr")[4], col = "white")

# Plot the line
lines(seq_along(y), y, col = "#AD8C97", lwd = 5)

# Scatter points
points(seq_along(y), y, col = "#E3120B", pch = 16, cex = 1.5)

# Add y-axis
axis(2)

# Add annotations
for (i in seq_along(y)) {
  text(i, y[i], labels = x[i], pos = 3, offset = 0, cex = 0.8)
}

windows()
y <- avg_indices_by_country$FDI
# Create a new plot with custom size and no points, and remove x-axis
plot(1, type = "n", xlab = "", ylab = "", xlim = c(1, length(y)), ylim = range(y), main = "FDI by Country", axes = FALSE)
rect(0.5, par("usr")[3], length(y) + 0.5, par("usr")[4], col = "white")

# Plot the line
lines(seq_along(y), y, col = GREEN, lwd = 5)

# Scatter points
points(seq_along(y), y, col = RED, pch = 16, cex = 1.5)

# Add y-axis
axis(2)

# Add annotations
for (i in seq_along(y)) {
  text(i, y[i], labels = x[i], pos = 3, offset = 0, cex = 0.8)
}

windows()
y <- avg_indices_by_country$EFI
# Create a new plot with custom size and no points, and remove x-axis
plot(1, type = "n", xlab = "", ylab = "", xlim = c(1, length(y)), ylim = range(y), main = "EFI by Country", axes = FALSE)
rect(0.5, par("usr")[3], length(y) + 0.5, par("usr")[4], col = "white")

# Plot the line
lines(seq_along(y), y, col = GREY, lwd = 5)

# Scatter points
points(seq_along(y), y, col = RED, pch = 16, cex = 1.5)

# Add y-axis
axis(2)

# Add annotations
for (i in seq_along(y)) {
  text(i, y[i], labels = x[i], pos = 3, offset = 0, cex = 0.8)
}
windows()
y_sdi <- avg_indices_by_country$SDI
y_fdi <- avg_indices_by_country$FDI
y_efi <- avg_indices_by_country$EFI

# Determine the y-axis range
y_range <- range(c(0, y_sdi, y_fdi, y_efi))

# Create a new plot with specified y-axis range
plot(1, type = "n", xlab = "", ylab = "", xlim = c(1, length(x)), ylim = y_range,
     main = "Social, Financial, and Economic Indicators by Country", axes = FALSE)

# Plot the lines
lines(seq_along(x), y_sdi, type = "o", col = "blue", pch = 16, lwd = 2, cex = 1.5)
lines(seq_along(x), y_fdi, type = "o", col = "green", pch = 16, lwd = 2, cex = 1.5)
lines(seq_along(x), y_efi, type = "o", col = "orange", pch = 16, lwd = 2, cex = 1.5)

# Add labels and legend
axis(2, at = seq(0, y_range[2], by = 20))
axis(1, at = seq_along(x), labels = x, las = 2)
legend("topright", legend = c("SDI", "FDI", "EFI"), col = c("blue", "green", "orange"), pch = 16, lwd = 2, cex = 1)

# Rotate x-axis labels
par(las = 2)

}

k_means_countries <- function(normalized_dataset, clusters){

	# Group by 'Country Name' and calculate the mean for numeric columns
	grouped <- normalized_dataset %>% group_by(`Country.Name`) %>% summarise_all(mean)
	#print(grouped)
	normal_data <- grouped
	grouped <- grouped %>% select(-Country.Name, -Country.Code, -Time.Code)

	numeric_columns <- names(grouped)[sapply(grouped, is.numeric)]
	
	# Check if there are any non-numeric columns
numeric_columns <- sapply(grouped, is.numeric)
non_numeric_columns <- names(grouped)[!numeric_columns]

# If there are non-numeric columns, drop them before normalization
if (length(non_numeric_columns) > 0) {
  df_numeric <- grouped[, numeric_columns]
} else {
  df_numeric <- grouped
}

# Print the result
#print(df_numeric)
kmeans_result <- kmeans(df_numeric[, -1], centers = clusters, nstart = 20)
df_numeric$Cluster <- kmeans_result$cluster
print(df_numeric$Cluster)

# Calculate the centroids
centroids <- aggregate(. ~ Cluster, data = df_numeric, FUN = mean)

# Merge centroids back to the original data
merged_data <- merge(df_numeric, centroids, by = "Cluster")

# Calculate the squared Euclidean distance from each point to its cluster centroid
squared_distances <- rowSums((merged_data[, -which(names(merged_data) == "Cluster")] - 
                              merged_data[, -which(names(merged_data) %in% c("Cluster", ".y"))])^2)

# Calculate the total sum of squared errors (SSE)
sse <- sum(squared_distances)

print(paste("SSE for", clusters, "clusters:", sse))



pca_result <- prcomp(df_numeric[, -which(names(df_numeric) %in% c("Cluster"))], scale = F)
data_pca <- pca_result$x[, 1:2]
df_pca <- data.frame(PC1 = data_pca[, 1], PC2 = data_pca[, 2], Cluster = df_numeric$Cluster)
df_pca$Country <- normal_data$Country.Name
print(df_pca)
windows()
ggplot(df_pca, aes(x = PC1, y = PC2, color = as.factor(Cluster))) +
  geom_point() +
  labs(title = "Scatter Plot of PCA Components by Cluster",
       x = "Principal Component 1",
       y = "Principal Component 2") +
  theme_minimal() +
  theme(legend.position = "top") +
  geom_text(aes(label = Country), vjust = -0.5, hjust = -0.5)  # Add labels for each point
}

attribute_vs_country_over_time <- function(df, attribute){
	# Extract relevant columns for plotting
  df_plot <- df[c('Country.Name', 'Time', attribute)]

  # Aggregate the values (mean) for any duplicate entries
  df_plot <- aggregate(. ~ Country.Name + Time, data = df_plot, mean)

  # Pivot the DataFrame for better plotting
  df_plot_pivot <- reshape2::dcast(df_plot, Time ~ Country.Name, value.var = attribute)

  ############ANIMATE THIS PLOT ################
  # Plotting
  # Generate a colormap with a number of colors equal to the number of countries
  colors <- colorRampPalette(brewer.pal(10, 'Set3'))(length(df_plot_pivot))

  # Set the device for plotting (e.g., PNG)
  png("animated_plot.png", width = 1400, height = 1300, res = 200)

  # Plot lines
  matplot(df_plot_pivot$Time, df_plot_pivot[, -1], pch = 19, col = colors, type = 'l', lty = 1, lwd = 2, xlab = 'Year', ylab = attribute)

  # Add country names beside the lines
  for (country_name in colnames(df_plot_pivot)[-1]) {
    last_value <- tail(df_plot_pivot[, country_name], 1, na.rm = TRUE)
    text(tail(df_plot_pivot$Time, 1), last_value, labels = country_name, pos = 4, offset = 0.5, col = colors[which(colnames(df_plot_pivot)[-1] == country_name)], cex = 0.7, adj = 0)
  }

  # Add legend
  legend('topright', legend = colnames(df_plot_pivot)[-1], fill = colors, bty = 'n', cex = 0.8)

  # Save the plot
  dev.off()

}
showAttributsVsCountryOverTimeAnimated <- function(df, attribute) {
  # Extract relevant columns for plotting
  df_plot <- df[c('Country.Name', 'Time', attribute)]
  print(df_plot)
  animated_plot <- ggplot(df, aes(x = Time, y = attribute, group = `Country.Name`, color = `Country.Name`)) +
  	geom_line() +
  	transition_states(Country.Name, transition_length = 2, state_length = 1) +
  	enter_fade() +
  	exit_fade() +
  	labs(title = "Stacked Time Series Animation")
  p <- ggplot(df, aes(x=Time, y = attribute, group = `Country.Name`, color = `Country.Name`)) + 
    geom_line() + 
    geom_segment(aes(xstart=1998, xend = 2022, yend = attribute), linetype = 2, colour = 'grey') +
    geom_point(size = 2) + 
    geom_text(aes(x = 2022, label = attribute), hjust = 0) + 
    transition_reveal(Time) + 
    coord_cartesian(clip = 'off') + 
    labs(title = 'National GDP', x='Year', y = 'GDP per Capits') + 
    theme_minimal() + 
    theme(plot.margin = margin(5.5, 40, 5.5, 5.5)) +
    ease_aes('linear', interval = 0.01)
  animate(p, nframes = 725, fps=10)
  anim_save("animation.gif", p)
  
animate(animated_plot, nframes = 725, fps=10)
anim_save("stacked_time_series_animation.gif", animated_plot)

}

yo <- function(df, attribute){
  
  df_plot <- df[c('Country.Name', 'Time', attribute)]
  df_plot$Time <- as.factor(df_plot$Time)
  # Create animated plot
  windows()
  p <- ggplot(df, aes(x = Time, y = Agricultural.land....of.land.area...AG.LND.AGRI.ZS., group = Country.Name, color = Country.Name)) +
    geom_line() +
    geom_point() +
    transition_states(Country.Name, transition_length = 2, state_length = 1) +
    ease_aes('linear', interval = 0.1) +
    labs(title = 'Agricultural Land Area Over Time by Country', x = 'Year', y = 'Agricultural Land Area (%)') +
    theme_minimal()
  
  # Animate the plot
  animate(p, nframes = 300, fps = 10)

  anim_save("p.gif", p)

}

# Read the dataset
dataset <- read.csv("C:/Graduate-Studies/AdvanceDataAnalytics/dataset/dataset5.csv")

cleaned_dataset <- clean_dataset(dataset)
normalized_dataset <- normalizeDataset(cleaned_dataset)
#grouped_country_similarity <- calculate_similarity(normalized_dataset)
#added_features_dataset <- feature_creation(normalized_dataset)
#grouped_country_new_features <- graphs_for_new_features(added_features_dataset)
#k_means_countries(normalized_dataset, 2)
#k_means_countries(normalized_dataset, 3)
#k_means_countries(normalized_dataset, 4)
#k_means_countries(normalized_dataset, 5)

#attribute_vs_country_over_time(cleaned_dataset, "Agricultural.land....of.land.area...AG.LND.AGRI.ZS.")
#showAttributsVsCountryOverTimeAnimated(cleaned_dataset, "Agricultural.land....of.land.area...AG.LND.AGRI.ZS.")
yo(cleaned_dataset, "Agricultural.land....of.land.area...AG.LND.AGRI.ZS.")

#attribute_vs_country_over_time(cleaned_dataset, "GDP.per.capita..current.US....NY.GDP.PCAP.CD.")
#attribute_vs_country_over_time(cleaned_dataset, "Total.reserves..includes.gold..current.US....FI.RES.TOTL.CD.")

