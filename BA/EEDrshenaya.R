# load libraries
library(ggplot2)
library(dplyr)
library(readxl)
library(car)
library(nortest)
library(ggpubr)
library(patchwork)


# Set my working directory 
setwd("C:/Users/shena/OneDrive/Desktop/sem4/BA/")

#Data Preparation

# Load the datasheet
data <- read.csv("energy_efficiency_data.csv")

# View the entire dataset
View(data)

# Check for missing values
sum(is.na(data)) # Should return 0 if no missing values

# Convert Orientation to a factor variable
data$Orientation <- as.factor(data$Orientation)


# Function to calculate mode
calculate_mode <- function(x) {
  uniq_vals <- unique(x)
  uniq_vals[which.max(tabulate(match(x, uniq_vals)))]
}

# Calculate descriptive statistics for Cooling Load, Surface Area, Roof Area, and Wall Area
summary_stats <- data %>%
  summarise(
    Cooling_Load_Mean = mean(Cooling_Load),
    Cooling_Load_Median = median(Cooling_Load),
    Cooling_Load_Mode = calculate_mode(Cooling_Load),
    Cooling_Load_SD = sd(Cooling_Load),
    Cooling_Load_Min = min(Cooling_Load),
    Cooling_Load_Max = max(Cooling_Load),
    
    Surface_Area_Mean = mean(Surface_Area),
    Surface_Area_Median = median(Surface_Area),
    Surface_Area_Mode = calculate_mode(Surface_Area),
    Surface_Area_SD = sd(Surface_Area),
    Surface_Area_Min = min(Surface_Area),
    Surface_Area_Max = max(Surface_Area),
    
    Roof_Area_Mean = mean(Roof_Area),
    Roof_Area_Median = median(Roof_Area),
    Roof_Area_Mode = calculate_mode(Roof_Area),
    Roof_Area_SD = sd(Roof_Area),
    Roof_Area_Min = min(Roof_Area),
    Roof_Area_Max = max(Roof_Area),
    
    Wall_Area_Mean = mean(Wall_Area),
    Wall_Area_Median = median(Wall_Area),
    Wall_Area_Mode = calculate_mode(Wall_Area),
    Wall_Area_SD = sd(Wall_Area),
    Wall_Area_Min = min(Wall_Area),
    Wall_Area_Max = max(Wall_Area)
  )

# Print the summary statistics
print(summary_stats)

# task 3
# Calculate central tendency analysis for Cooling Load  , Surface Area, Wall Area and Roof Area

summary(data$Cooling_Load)
summary(data$Surface_Area)
summary(data$Wall_Area)
summary(data$Roof_Area)

# Summary statistics for the 'Cooling_Load' column
summary(data$Cooling_Load)

# Histogram of the 'Cooling_Load' column with a normal curve overlay
hist(data$Cooling_Load, 
     main = "Cooling Load vs. Building Energy Requirements",
     col = "navyblue", 
     xlab = "Cooling Load (BTU)", 
     ylab = "Density", 
     probability = TRUE)

curve(dnorm(x, mean = mean(data$Cooling_Load, na.rm = TRUE), 
            sd = sd(data$Cooling_Load, na.rm = TRUE)), 
      add = TRUE, col = "green", lwd = 2)

# Summary statistics for the 'Surface_Area' column
summary(data$Surface_Area)

# Histogram of the 'Surface_Area' column with a normal curve overlay
hist(data$Surface_Area, 
     main = "Surface Area vs. Building Size Distribution",
     col = "navyblue", 
     xlab = "Surface Area (sq ft)", 
     ylab = "Density", 
     probability = TRUE)

curve(dnorm(x, mean = mean(data$Surface_Area, na.rm = TRUE), 
            sd = sd(data$Surface_Area, na.rm = TRUE)), 
      add = TRUE, col = "green", lwd = 2)

# Summary statistics for the 'Wall_Area' column
summary(data$Wall_Area)

# Histogram of the 'Wall_Area' column with a normal curve overlay
hist(data$Wall_Area, 
     main = "Wall Area vs. Thermal Performance",
     col = "navyblue", 
     xlab = "Wall Area (sq ft)", 
     ylab = "Density",  
     probability = TRUE)

curve(dnorm(x, mean = mean(data$Wall_Area, na.rm = TRUE), 
            sd = sd(data$Wall_Area, na.rm = TRUE)), 
      add = TRUE, col = "green", lwd = 2)


# Summary statistics for the 'Roof_Area' column
summary(data$Roof_Area)

# Histogram of the 'Roof_Area' column with a normal curve overlay
hist(data$Roof_Area, 
     main = "Roof Area vs. Energy Efficiency Potential",
     col = "navyblue", 
     xlab = "Roof Area (sq ft)", 
     ylab = "Density",  
     probability = TRUE)

curve(dnorm(x, mean = mean(data$Roof_Area, na.rm = TRUE), 
            sd = sd(data$Roof_Area, na.rm = TRUE)), 
      add = TRUE, col = "green", lwd = 2)

#task 4

# Formulation of Hypotheses

# Null Hypothesis        (H0): Building orientation does not significantly affect cooling load.
# Alternative Hypothesis (HA): Building orientation significantly affects cooling load.

# load libraries
library(ggplot2)
library(dplyr)
library(readxl)
library(car)
library(nortest)

# Set my working directory 
setwd("C:/Users/shena/OneDrive/Desktop/sem4/BA/")

#Data Preparation

# Load the datasheet
data <- read.csv("energy_efficiency_data.csv")


# Proper way to check for missing values in the entire data set
sum(is.na(data))

# Convert Orientation to factor (if categorical)
data$Orientation <- as.factor(data$Orientation)

# Check Normality of Cooling Load Data
# Shapiro-Wilk test
shapiro.test(data$Cooling_Load)

# Histogram to visualize distribution 
ggplot(data, aes(x = Cooling_Load)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "navyblue", color = "black", alpha = 0.7) +
  stat_function(fun = dnorm, args = list(mean = mean(data$Cooling_Load, na.rm = TRUE), 
                                         sd = sd(data$Cooling_Load, na.rm = TRUE)), 
                color = "green", lwd = 1.5) +
  labs(title = "Cooling Load Distribution", x = "Cooling Load", y = "Density")

# Q-Q plot(this plot is used for assessing the normality of the Cooling_Load data.)
ggplot(data, aes(sample = Cooling_Load, color = Orientation)) +
  stat_qq() +
  stat_qq_line(aes(group = Orientation), show.legend = FALSE) +
  labs(title = "Q-Q Plot of Cooling Load by Orientation",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  scale_color_manual(values = c("red", "blue", "green", "orange")) +
  theme_minimal()

# Check Homogeneity of Variance
leveneTest(Cooling_Load ~ Orientation, data = data)

# Perform One-Way ANOVA (if normality holds)
anova_result <- aov(Cooling_Load ~ Orientation, data = data)
summary(anova_result)



# Post-Hoc Analysis (if ANOVA is significant)
TukeyHSD(anova_result)

# If normality is violated, use Kruskal-Wallis Test
kruskal.test(Cooling_Load ~ Orientation, data = data)

# Graphical Analysis
# Boxplot
ggplot(data, aes(x = Orientation, y = Cooling_Load, fill = Orientation)) +
  geom_boxplot() +
  labs(title = " Boxplot of Cooling Load Across Different Orientations", x = "Building Orientation", y = "Cooling Load") +
  theme_minimal()

# Density Plot
ggplot(data, aes(x = Cooling_Load, fill = Orientation)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Cooling Load", x = "Cooling Load", y = "Density") +
  theme_minimal()

# Histogram for Cooling Load Distribution by Orientation(W/E/N/S)
ggplot(data, aes(x = Cooling_Load, fill = Orientation)) +
  geom_histogram(aes(y = ..density..), bins = 30, color = "black", alpha = 0.7) +
  facet_wrap(~Orientation) +  # Creates separate histograms for each orientation
  stat_function(fun = dnorm, 
                args = list(mean = mean(data$Cooling_Load, na.rm = TRUE), 
                            sd = sd(data$Cooling_Load, na.rm = TRUE)), 
                color = "red", lwd = 1.5) +
  labs(title = "Cooling Load Distribution by Orientation", 
       x = "Cooling Load", 
       y = "Density") +
  theme_minimal()




#task 5

# Cooling load vs Surface Area


# load libraries
library(ggplot2)
library(dplyr)
library(readxl)
library(car)
library(nortest)
library(ggpubr) 

# Set my working directory 
setwd("C:/Users/shena/OneDrive/Desktop/sem4/BA/")

#Data Preparation

# Load the datasheet
data <- read.csv("energy_efficiency_data.csv")

# Cooling load vs Surface Area

# Calculate descriptive statistics for Cooling Load, Surface Area
summary_stats <- data %>%
  summarise(
    Cooling_Load_Mean = mean(Cooling_Load),
    Cooling_Load_Median = median(Cooling_Load),
    Cooling_Load_SD = sd(Cooling_Load),
    Cooling_Load_Min = min(Cooling_Load),
    Cooling_Load_Max = max(Cooling_Load),
    
    Surface_Area_Mean = mean(Surface_Area),
    Surface_Area_Median = median(Surface_Area),
    Surface_Area_SD = sd(Surface_Area),
    Surface_Area_Min = min(Surface_Area),
    Surface_Area_Max = max(Surface_Area),
    
  )

# Print the summary statistics
print(summary_stats)


# Calculate central tendency analysis for Cooling Load  , Surface Area, 

summary(data$Cooling_Load)
summary(data$Surface_Area)

# Shapiro-Wilk normality test for Cooling Load
shapiro.test(data$Cooling_Load)

# Shapiro-Wilk normality test for Surface Area
shapiro.test(data$Surface_Area)

cor(data$Cooling_Load, data$Surface_Area, method = "spearman")



par(mfrow = c(1, 2))  

# Histogram of the 'Cooling_Load' column with a normal curve overlay
hist(data$Cooling_Load, 
     main = "Cooling Load ",
     col = "navyblue", 
     xlab = "Cooling Load (BTU)", 
     ylab = "Density", 
     probability = TRUE)

curve(dnorm(x, mean = mean(data$Cooling_Load, na.rm = TRUE), 
            sd = sd(data$Cooling_Load, na.rm = TRUE)), 
      add = TRUE, col = "green", lwd = 2)

# Histogram of the 'Surface_Area' column with a normal curve overlay
hist(data$Surface_Area, 
     main = "Surface Area ",
     col = "navyblue", 
     xlab = "Surface Area (sq ft)", 
     ylab = "Density", 
     probability = TRUE)

curve(dnorm(x, mean = mean(data$Surface_Area, na.rm = TRUE), 
            sd = sd(data$Surface_Area, na.rm = TRUE)), 
      add = TRUE, col = "green", lwd = 2)

# Reset plotting layout to default
par(mfrow = c(1, 1))



# Perform Spearman correlation test
cor.test(data$Cooling_Load, data$Surface_Area, method = "spearman", exact = FALSE)

# Linear Regression Model
model <- lm(Cooling_Load ~ Surface_Area, data = data)
summary(model)

#scatter plot
ggplot(data, aes(x = Surface_Area, y = Cooling_Load)) +
  geom_point(color = "navyblue") +
  geom_smooth(method = "lm", color = "yellow", se = FALSE) +
  labs(title = "Scatter Plot of Cooling Load vs Surface Area",
       x = "Surface Area (sq ft)",
       y = "Cooling Load (BTU)") +
  theme_minimal()


#task 5
# Cooling load vs Wall Area

# load libraries
library(ggplot2)
library(dplyr)
library(readxl)
library(car)
library(nortest)
library(ggpubr) 

# Set my working directory 
setwd("C:/Users/shena/OneDrive/Desktop/sem4/BA/")

#Data Preparation

# Load the datasheet
data <- read.csv("energy_efficiency_data.csv")


# Calculate descriptive statistics for Cooling Load, wall Area
summary_stats <- data %>%
  summarise(
    Cooling_Load_Mean = mean(Cooling_Load),
    Cooling_Load_Median = median(Cooling_Load),
    Cooling_Load_SD = sd(Cooling_Load),
    Cooling_Load_Min = min(Cooling_Load),
    Cooling_Load_Max = max(Cooling_Load),
    
    Wall_Area_Mean = mean(Wall_Area),
    Wall_Area_Median = median(Wall_Area),
    Wall_Area_SD = sd(Wall_Area),
    Wall_Area_Min = min(Wall_Area),
    Wall_Area_Max = max(Wall_Area)
  )

# Print the summary statistics
print(summary_stats)

# Calculate central tendency analysis for Cooling Load  , wall Area, 

summary(data$Cooling_Load)
summary(data$Wall_Area)

# Shapiro-Wilk normality test for Cooling Load
shapiro.test(data$Cooling_Load)

# Shapiro-Wilk normality test for Wall Area
shapiro.test(data$Wall_Area)

cor(data$Cooling_Load, data$Wall_Area, method = "spearman")

par(mfrow = c(1, 2)) 

# Histogram of the 'Cooling_Load' column with a normal curve overlay
hist(data$Cooling_Load, 
     main = "Cooling Load ",
     col = "navyblue", 
     xlab = "Cooling Load (BTU)", 
     ylab = "Density", 
     probability = TRUE)

curve(dnorm(x, mean = mean(data$Cooling_Load, na.rm = TRUE), 
            sd = sd(data$Cooling_Load, na.rm = TRUE)), 
      add = TRUE, col = "green", lwd = 2)

# Histogram of the 'Wall_Area' column with a normal curve overlay
hist(data$Wall_Area, 
     main = "Wall Area ",
     col = "navyblue", 
     xlab = "Wall Area (sq ft)", 
     ylab = "Density",  
     probability = TRUE)

curve(dnorm(x, mean = mean(data$Wall_Area, na.rm = TRUE), 
            sd = sd(data$Wall_Area, na.rm = TRUE)), 
      add = TRUE, col = "green", lwd = 2)

# Perform Spearman correlation test
cor.test(data$Cooling_Load, data$Wall_Area, method = "spearman", exact = FALSE)

# Linear Regression Model
model <- lm(Cooling_Load ~ Wall_Area, data = data)
summary(model)


#scatter plot
ggplot(data, aes(x = Wall_Area, y = Cooling_Load)) +
  geom_point(color = "navyblue") +
  geom_smooth(method = "lm", color = "yellow", se = FALSE) +
  labs(title = "Cooling Load vs Wall Area",
       x = "Wall Area (sq ft)",
       y = "Cooling Load (BTU)") +
  theme_minimal()


#task 5
# Cooling load vs Roof Area

# load libraries
library(ggplot2)
library(dplyr)
library(readxl)
library(car)
library(nortest)
library(ggpubr) 

# Set my working directory 
setwd("C:/Users/shena/OneDrive/Desktop/sem4/BA/")

#Data Preparation

# Load the datasheet
data <- read.csv("energy_efficiency_data.csv")

# Calculate descriptive statistics for Cooling Load, Roof Area
summary_stats <- data %>%
  summarise(
    Cooling_Load_Mean = mean(Cooling_Load),
    Cooling_Load_Median = median(Cooling_Load),
    Cooling_Load_SD = sd(Cooling_Load),
    Cooling_Load_Min = min(Cooling_Load),
    Cooling_Load_Max = max(Cooling_Load),
    
    Roof_Area_Mean = mean(Roof_Area),
    Roof_Area_Median = median(Roof_Area),
    Roof_Area_SD = sd(Roof_Area),
    Roof_Area_Min = min(Roof_Area),
    Roof_Area_Max = max(Roof_Area),
    
  )

# Print the summary statistics
print(summary_stats)

# Calculate central tendency analysis for Cooling Load  , Roof Area, 

summary(data$Cooling_Load)
summary(data$Roof_Area)


# Shapiro-Wilk normality test for Cooling Load
shapiro.test(data$Cooling_Load)

# Shapiro-Wilk normality test for Roof Area
shapiro.test(data$Roof_Area)

cor(data$Cooling_Load, data$Roof_Area, method = "spearman")

par(mfrow = c(1, 2)) 

# Histogram of the 'Cooling_Load' column with a normal curve overlay
hist(data$Cooling_Load, 
     main = "Cooling Load ",
     col = "navyblue", 
     xlab = "Cooling Load (BTU)", 
     ylab = "Density", 
     probability = TRUE)

curve(dnorm(x, mean = mean(data$Cooling_Load, na.rm = TRUE), 
            sd = sd(data$Cooling_Load, na.rm = TRUE)), 
      add = TRUE, col = "green", lwd = 2)

# Histogram of the 'Roof_Area' column with a normal curve overlay
hist(data$Roof_Area, 
     main = "Roof Area ",
     col = "navyblue", 
     xlab = "Roof Area (sq ft)", 
     ylab = "Density",  
     probability = TRUE)

curve(dnorm(x, mean = mean(data$Roof_Area, na.rm = TRUE), 
            sd = sd(data$Roof_Area, na.rm = TRUE)), 
      add = TRUE, col = "green", lwd = 2)

# Perform Spearman correlation test
cor.test(data$Cooling_Load, data$Roof_Area, method = "spearman", exact = FALSE)

# Linear Regression Model
model <- lm(Cooling_Load ~ Roof_Area, data = data)
summary(model)


#scatter plot
ggplot(data, aes(x = Roof_Area, y = Cooling_Load)) +
  geom_point(color = "navyblue") +
  geom_smooth(method = "lm", color = "yellow", se = FALSE) +
  labs(title = "Cooling Load vs Roof Area",
       x = "Roof Area (sq ft)",
       y = "Cooling Load (BTU)") +
  theme_minimal()

#TASK 5TH/ 6TH
# Combined Regression Analysis for Cooling Load vs. Surface Area, Wall Area, and Roof Area

# Fit the multiple linear regression model
combined_model <- lm(Cooling_Load ~ Surface_Area + Wall_Area + Roof_Area, data = data)

# Display the summary of the model
summary(combined_model)



