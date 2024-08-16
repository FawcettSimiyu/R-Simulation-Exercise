# a) Generate a matrix of numbers
generate_matrix <- function(nrows = 10, ncols = 11, min_val = 0, max_val = 255, seed = 123) {
  set.seed(seed)
  matrix(sample(min_val:max_val, nrows * ncols, replace = TRUE), nrow = nrows, ncol = ncols)
}

matrix_data <- generate_matrix(10, 11, 0, 255)
print("Original Matrix:")
print(matrix_data)

# b) Generate a simulated image
generate_image <- function(matrix_data, color_scheme = "gray", interpolate = FALSE) {
  colors <- switch(color_scheme,
                   "gray" = gray.colors(256),
                   "heat" = heat.colors(256),
                   terrain.colors(256))  # default
  image(matrix_data, col = colors, axes = FALSE, main = "Simulated Image", useRaster = interpolate)
}

generate_image(matrix_data, "gray")
generate_image(matrix_data, "heat", interpolate = TRUE)

# c) Classify the simulated image
classify_image <- function(matrix_data, breaks, labels = NULL) {
  if (is.null(labels)) {
    labels <- seq_along(breaks) - 1
  }
  cut(matrix_data, breaks = breaks, labels = labels, include.lowest = TRUE)
}

breaks <- c(0, 50, 100, 150, 200, 255)
labels <- c("Very Low", "Low", "Medium", "High", "Very High")
classified_matrix <- classify_image(matrix_data, breaks, labels)
print("Classified Matrix:")
print(classified_matrix)

classified_matrix_numeric <- as.numeric(classified_matrix)
generate_image(matrix(classified_matrix_numeric, nrow = nrow(matrix_data), ncol = ncol(matrix_data)), "heat")

# d) Reclassify the simulated image
reclassify_image <- function(matrix_data, method = "quartile") {
  if (method == "quartile") {
    breaks <- quantile(matrix_data, probs = seq(0, 1, 0.25), na.rm = TRUE)
    labels <- c("Low", "Medium-Low", "Medium-High", "High")
  } else {
    breaks <- seq(min(matrix_data), max(matrix_data), length.out = 4)
    labels <- c("Low", "Medium", "High")
  }
  cut(matrix_data, breaks = breaks, labels = labels, include.lowest = TRUE)
}

reclassified_matrix <- reclassify_image(matrix_data, method = "quartile")
print("Reclassified Matrix (Quartiles):")
print(reclassified_matrix)

reclassified_matrix_numeric <- matrix(as.numeric(reclassified_matrix), nrow = nrow(matrix_data), ncol = ncol(matrix_data))
generate_image(reclassified_matrix_numeric, "heat")

#Extras
plot_distribution <- function(matrix_data, title = "Matrix Values") {
  par(mfrow = c(1, 2))
  hist(matrix_data, main = paste(title, "- Histogram"), xlab = "Value", col = "lightblue", breaks = 20)
  barplot(table(matrix_data), main = paste(title, "- Bar Plot"), xlab = "Class", ylab = "Freq", col = "lightgreen")
  par(mfrow = c(1, 1))
}

# Visualize the distributions
plot_distribution(matrix_data, "Original Matrix")
plot_distribution(as.numeric(classified_matrix), "Classified Matrix")
plot_distribution(as.numeric(reclassified_matrix), "Reclassified Matrix")
