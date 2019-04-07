# Explore cars with summary()
library(FactoMineR)
library(ggcorrplot)

# Load data
data("mtcars")
dim(mtcars)
head(mtcars, 6)
#my_data <- mtcars[,c(1,2,3,4,5,6,7,8,9,10,11)]
#my_data <- mtcars[,1:11]
# print the first 6 rows
#head(my_data, 6)
#mtcars_pca <- prcomp(mtcars)

cor(mtcars, y=mtcars, use="all.obs")
cov(mtcars, y=mtcars, use="all.obs")
mtcars_pca <- PCA(mtcars)

# Run a PCA for the 10 non-binary numeric variables of cars.
# mtcars_pca <- PCA(cars[,9:19], ncp = 4, graph = FALSE)
# Run a PCA with active and supplementary variables
# pca_output_all <- PCA(cars, quanti.sup = 1:8, quali.sup = 20:21, graph = FALSE)
mtcars_pca$eig

# pct of variance of first 3 components
mtcars_pca$eig[,2][1:3]

# cumlative variance of first 3 components
mtcars_pca$eig[,3][1:3]

mtcars_pca$var$cos2
mtcars_pca$var$contrib

# Get the most correlated variables
dimdesc(mtcars_pca)
dimdesc(pca_output_all, axes = 1:2)

# Get the summary of the first 100 cars
summary(mtcars_pca,nbelements=100)

# Get the variance of the first 3 new dimensions
mtcars_pca$eig[,2][1:3]

# Get the correlation matrix with cor()
# correl <- cor(mtcars[9:19], use = "complete.obs")
correl <- cor(mtcars, use = "complete.obs")

# Use ggcorrplot() to explore the correlation matrix
ggcorrplot(correl)

# Conduct hierarchical clustering on the correlation matrix
ggcorrplot_clustered <- ggcorrplot(correl, hc.order = TRUE, type = "lower")
ggcorrplot_clustered