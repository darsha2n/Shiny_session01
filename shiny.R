# Install and load ggplot2 if you haven't already
# install.packages("ggplot2") 
library(ggplot2)

# The iris dataset is built-in, so you don't need to load it from a file.
# You can view its structure and first few rows:
head(iris)
summary(iris)

# --- Scatter Plot (most common for Iris data) ---
# Visualize Petal.Length vs. Petal.Width, colored by Species
ggplot(data = iris, aes(x = Petal.Length, y = Petal.Width, color = Species)) +
  geom_point() +
  labs(title = "Iris Petal Length vs. Petal Width",
       x = "Petal Length (cm)",
       y = "Petal Width (cm)",
       color = "Species") +
  theme_minimal() # A clean, minimalist theme

# --- Scatter Plot with Smooth Trend Lines ---
# Add linear regression lines for each species
ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + # Add linear model trend line, no confidence interval
  labs(title = "Iris Sepal Length vs. Sepal Width with Trend Lines",
       x = "Sepal Length (cm)",
       y = "Sepal Width (cm)",
       color = "Species") +
  theme_bw() # A black and white theme

# --- Box Plot ---
# Visualize the distribution of Sepal.Length for each Species
ggplot(data = iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_boxplot() +
  labs(title = "Box Plot of Sepal Length by Species",
       x = "Species",
       y = "Sepal Length (cm)") +
  theme_classic() + # A classic theme
  theme(legend.position = "none") # Remove the legend as species is on the x-axis

# --- Violin Plot (similar to box plot, shows density) ---
ggplot(data = iris, aes(x = Species, y = Petal.Width, fill = Species)) +
  geom_violin() +
  geom_boxplot(width = 0.1, outlier.shape = NA) + # Add small boxplots on top, hide outliers
  labs(title = "Violin Plot of Petal Width by Species",
       x = "Species",
       y = "Petal Width (cm)") +
  theme_light() + # A light theme
  theme(legend.position = "none")

# --- Pair Plot (matrix of all pairwise scatter plots) ---
# Requires the 'GGally' package (install.packages("GGally"))
# This is great for an overall view of correlations between variables.
# install.packages("GGally")
library(GGally)

ggpairs(data = iris, aes(color = Species)) +
  labs(title = "Pair Plot of Iris Dataset")
  
