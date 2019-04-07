# Principal Component Analysis and Factor Analysis in R

library(RODBC)
library(FactoMineR)
library(ggcorrplot)

#mydata<- read.csv("Downloads/pca_gsp.csv")
#attach(mydata)
# setup connection to database
conn <- odbcConnect("TDAPRD-sde", uid="sde", pwd="SDE", believeNRows = FALSE, rows_at_time = 1)

topics <- "select category_id, category_name as topic from impower.imp_dig_categories 
           where  source = 'interest'
           and    category_name like '%Food%'  
           and    rownum < 12
           and    category_id in (select fk_category_id from impower.imp_dig_interest_atz where geocode like '%')"
topics_df <- sqlQuery(conn, topics)
dim(topics_df)
head(topics_df,20)


# pivot query of 11 variables
query <- "select geocode, i1_score, i2_score, i3_score, i4_score, i5_score, i6_score, i7_score, i8_score, i9_score, i10_score, i11_score from (
with cat as
(select category_id from impower.imp_dig_categories 
where  source = 'interest' 
and    rownum < 12
and    category_name like '%Food%'  
)
select geocode, dense_rank() over(order by fk_category_id) fk_category_id, hhc, dma_score
from (
select z.geocode, z.fk_category_id, t.solo_s as hhc, z.dma_score
from impower.imp_dig_interest_atz z, cat c, cl_atztab14 t
where z.fk_category_id = c.category_id
and z.geocode = t.geocode
and z.geocode like '4815%'))
pivot (sum(dma_score) as score FOR (fk_category_id) IN (1 as i1,2 as i2,3 as i3,4 as i4,5 as i5, 6 as i6,7 as i7,8 as i8,9 as i9,10 as i10, 11 as i11))
where i1_score is not null
and i2_score is not null
and i3_score is not null
and i4_score is not null
and i5_score is not null
and i6_score is not null
and i7_score is not null
and i8_score is not null
and i9_score is not null
and i10_score is not null
and i11_score is not null
order by geocode"
df <- sqlQuery(conn, query)
dim(df)
head(df,10)

attach(df)
# Define variables
X <- cbind(I1_SCORE, I2_SCORE, I3_SCORE, I4_SCORE, I5_SCORE, I6_SCORE, I7_SCORE, I8_SCORE, I9_SCORE, I10_SCORE, I11_SCORE)
# Descriptive statistics
summary(X)
cor(X)
cov(X)
# Principal component analysis
pca1 <- princomp(X, scores=TRUE, cor=TRUE)
summary(pca1)
# Loadings of principal components
loadings(pca1)
#pca1$loadings
# Scree plot of eigenvalues
plot(pca1)
screeplot(pca1, type="line", main="Scree Plot")
# Biplot of score variables
biplot(pca1)
# Scores of the components
pca1$scores[1:10,]
# Rotation
#varimax(pca1$rotation)
#promax(pca1$rotation)
# Factor analysis - different results from other softwares and no rotation
fa1 <- factanal(X, factor=3)
fa1
fa2 <- factanal(X, factor=3, rotation="varimax")
fa2
fa3 <- factanal(X, factors=3, rotation="varimax", scores="regression")
fa3


cor(X, y=X, use="all.obs")
cov(X, y=X, use="all.obs")
df_pca <- PCA(X)

# Run a PCA for the 10 non-binary numeric variables of cars.
# mtcars_pca <- PCA(cars[,9:19], ncp = 4, graph = FALSE)
# Run a PCA with active and supplementary variables
# pca_output_all <- PCA(cars, quanti.sup = 1:8, quali.sup = 20:21, graph = FALSE)
df_pca$eig

# pct of variance of first 3 components
df_pca$eig[,2][1:3]

# cumlative variance of first 3 components
df_pca$eig[,3][1:3]

df_pca$var$cos2
df_pca$var$contrib

# Get the most correlated variables
dimdesc(df_pca)
dimdesc(df_pca, axes = 1:2)

# Get the summary of the first 100 cars
summary(df_pca,nbelements=100)

# Get the variance of the first 3 new dimensions
df_pca$eig[,2][1:3]

# Get the correlation matrix with cor()
# correl <- cor(mtcars[9:19], use = "complete.obs")
correl <- cor(X, use = "complete.obs")

# Use ggcorrplot() to explore the correlation matrix
ggcorrplot(correl)

# Conduct hierarchical clustering on the correlation matrix
ggcorrplot_clustered <- ggcorrplot(correl, hc.order = TRUE, type = "lower")
ggcorrplot_clustered

