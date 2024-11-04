library(mlbench)
data(PimaIndiansDiabetes)
str(PimaIndiansDiabetes)
print(PimaIndiansDiabetes)
summary(PimaIndiansDiabetes)
# Boxplot
boxplot(PimaIndiansDiabetes, col = "lightblue", main = "Boxplots of All Variables")
#Plotting the histogram
# Loop through each variable and create histograms
# Load the necessary package and data
library(mlbench)
#for plotting the mass function 
library(MASS)
data(PimaIndiansDiabetes)

# Plotting the chart of the curve
for (variable in names(PimaIndiansDiabetes)) {
  if (is.numeric(PimaIndiansDiabetes[[variable]])) {
    # Plot the histogram
    hist(PimaIndiansDiabetes[[variable]], 
         main = paste("Histogram of", variable, "with Normal Curve"), 
         xlab = variable, 
         col = "lightblue", 
         border = "black", 
         probability = TRUE)  # Scale the histogram to display density
    
    # Calculate the mean and standard deviation for the normal distribution
    mean_value <- mean(PimaIndiansDiabetes[[variable]], na.rm = TRUE)
    sd_value <- sd(PimaIndiansDiabetes[[variable]], na.rm = TRUE)
    
    # Overlay the normal distribution curve
    curve(dnorm(x, mean = mean_value, sd = sd_value), 
          col = "red", 
          lwd = 2, 
          add = TRUE)
  }
}

#Plotting through the functions to fit the plots

# Loop through each numeric variable and fit distributions
for (variable in names(PimaIndiansDiabetes)) {
  if (is.numeric(PimaIndiansDiabetes[[variable]])) {
    # Extract the data for the current variable, excluding NA values
    data <- na.omit(PimaIndiansDiabetes[[variable]])
    
    # Fit Normal Distribution
    normal_fit <- fitdistr(data, "normal")
    normal_aic <- AIC(normal_fit)
    
    # Fit Lognormal Distribution (only if data is all positive)
    if (all(data > 0)) {
      lognormal_fit <- fitdistr(data, "lognormal")
      lognormal_aic <- AIC(lognormal_fit)
    } else {
      lognormal_aic <- NA  # Not applicable if data has non-positive values
    }
    
    # Fit Exponential Distribution (only if data is non-negative)
    if (all(data >= 0)) {
      exponential_fit <- fitdistr(data, "exponential")
      exponential_aic <- AIC(exponential_fit)
    } else {
      exponential_aic <- NA  # Not applicable if data has negative values
    }
    
    # Print the AIC values for each distribution
    cat("AIC values for", variable, ":\n")
    cat("  Normal: ", normal_aic, "\n")
    cat("  Lognormal: ", lognormal_aic, "\n")
    cat("  Exponential: ", exponential_aic, "\n\n")
  }
}
mle_result <- mle(neg_log_likelihood, start = list(mean = mean(PimaIndiansdata), sd = sd(data)))
