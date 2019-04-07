# Get necessary variable data for pca analysis
library(RODBC)
library(plyr)
library(reshape2)

library(FactoMineR)
library(ggcorrplot)
# source("writeTDE.r")

# setup connection to database
conn <- odbcConnect("TDAPRD-sde", uid="sde", pwd="SDE", believeNRows = FALSE, rows_at_time = 1)

# pivot query of 11 variables
query <- "select geocode, i1_score, i2_score, i3_score, i4_score, i5_score, i6_score, i7_score, i8_score, i9_score, i10_score, i11_score from (
with cat as
(select category_id from impower.imp_dig_categories where source = 'interest' and rownum < 12)
select z.geocode, z.fk_category_id, t.solo_s as hhc, z.dma_score
from impower.imp_dig_interest_zip z, cat c, cl_ziptab14 t
where z.fk_category_id = c.category_id
and z.geocode = t.geocode
and z.geocode like '4606%')
pivot (sum(dma_score) as score FOR (fk_category_id) IN (1 as i1,2 as i2,3 as i3,4 as i4,5 as i5,
                                                        6 as i6,7 as i7,8 as i8,9 as i9,10 as i10,
                                                        11 as i11))
order by geocode"
df <- sqlQuery(conn, query)
dim(df)
head(df,10)

cor(df, y=df, use="all.obs")
cov(df, y=df, use="all.obs")
df_pca <- PCA(df)

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
correl <- cor(df, use = "complete.obs")

# Use ggcorrplot() to explore the correlation matrix
ggcorrplot(correl)

# Conduct hierarchical clustering on the correlation matrix
ggcorrplot_clustered <- ggcorrplot(correl, hc.order = TRUE, type = "lower")
ggcorrplot_clustered
# writeTDE(df,  "Wrap Zone")
