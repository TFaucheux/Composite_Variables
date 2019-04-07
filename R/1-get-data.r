# Get necessary variable data for pca analysis
library(RODBC)
library(plyr)
library(reshape2)

library(FactoMineR)
library(ggcorrplot)
# source("writeTDE.r")

conn <- odbcConnect("TDAPRD-sde", uid="sde", pwd="SDE", believeNRows = FALSE, rows_at_time = 1)


# Query for wrap data by geocode (cl_wrap_*)
query <- "select ws.geocode, ws.tablename, ws.fieldname, ws.value, wn.id, wn.name, dt.fielddescr, dt.natl_avg 
from claritas.cl_wrap_summary ws, claritas.cl_wrap_name wn, claritas.cl_desctab dt
where ws.fieldname in 
('CL0C00','CL2HSZ','CL2I08','CL2I09','CL2I0A','CL2I0B','CL2I0C','CL2I0D','CL2I0E','CL2I0F','CL2I0G','CL2I0H','CL2I0I',
'CL2I0J','CL2I0K','CL2PRA','CL2PRB','CL2PRH','CL2PRW','CL2PRX',
'CL2A0004','CL2A0509','CL2A1014','CL2A1517','CL2A1820','CL2A2124','CL2A2529',
'CL2A3034','CL2A3539','CL2A4049','CL2A5054','CL2A5559','CL2A6069','CL2A7074','CL2A7579','CL2A8084','CL2A8500',
'CL0PE1','CL0PE2','CL0PE3','CL0PE5','CL0PE6','CL0PE7','CL0PE9','CL0UTW','CL2HWV','CL2A00','CL2I00',
'R007','HF00','SA00','SEF0','SEF1','SEG0','SER5','SER4','YDR7','SOLO_S','SOLO_W','SMC_S','SMC_W','PERCAPTS','PERCAPTW',
'PERSFDUS','PERSFDUW','ANNECNTS','ANNECNTW','HHLD_S','HHLD_W')
AND geocode in (select wrap from claritas.cl_wrap_name where smc_w > 0)
and ws.geocode = wn.wrap
and ws.fieldname = dt.fieldname"

df <- sqlQuery(conn, query)
df

# Getting additional variables
# Median value owner occ hus, median hh income, median age, avg hh size, hh w/children
query2 <- "select tab05.geocode, tab05.CL2I00 as MedianIncome, tab04.Cl2HWV as MedianValueOwner, tab05.CL2HA0 as MedianAge,
tab01.Cl2HSZ as AvgHHSize, tab01.CL0C00 as HHwChildren
from claritas.cl_wrap_tab01 tab01, claritas.cl_wrap_tab04 tab04, claritas.cl_wrap_tab05 tab05
where tab01.geocode = tab04.geocode and tab01.geocode = tab05.geocode
and tab05.geocode in (select wrap from claritas.cl_wrap_name where smc_w > 0)"
df2 <- sqlQuery(conn, query2)
df2

query3 <- "select geocode, i1_score, i2_score, i3_score, i4_score, i5_score, i6_score, i7_score, i8_score, i9_score, i10_score, i11_score from (
with cat as
(select category_id from impower.imp_dig_categories where source = 'interest' and rownum < 12)
select z.geocode, z.fk_category_id, t.solo_s as hhc, z.dma_score
from impower.imp_dig_interest_zip z, cat c, cl_ziptab14 t
where z.fk_category_id = c.category_id
and z.geocode = t.geocode
and z.geocode like '4815%')
pivot (sum(dma_score) as score FOR (fk_category_id) IN (1 as i1,2 as i2,3 as i3,4 as i4,5 as i5,
                                                        6 as i6,7 as i7,8 as i8,9 as i9,10 as i10,
                                                        11 as i11,12 as i12))
order by geocode"
df3 <- sqlQuery(conn, query3)
df3


dim(df3)
head(df3, 6)

cor(mtcars, y=df3, use="all.obs")
cov(mtcars, y=df3, use="all.obs")
df3_pca <- PCA(df3)

# Run a PCA for the 10 non-binary numeric variables of cars.
# mtcars_pca <- PCA(cars[,9:19], ncp = 4, graph = FALSE)
# Run a PCA with active and supplementary variables
# pca_output_all <- PCA(cars, quanti.sup = 1:8, quali.sup = 20:21, graph = FALSE)
df3_pca$eig

# pct of variance of first 3 components
df3_pca$eig[,2][1:3]

# cumlative variance of first 3 components
df3_pca$eig[,3][1:3]

df3_pca$var$cos2
df3_pca$var$contrib

# Get the most correlated variables
dimdesc(df3_pca)
dimdesc(df3, axes = 1:2)

# Get the summary of the first 100 cars
summary(df3_pca,nbelements=100)

# Get the variance of the first 3 new dimensions
df3_pca$eig[,2][1:3]

# Get the correlation matrix with cor()
# correl <- cor(mtcars[9:19], use = "complete.obs")
correl <- cor(df3, use = "complete.obs")

# Use ggcorrplot() to explore the correlation matrix
ggcorrplot(correl)

# Conduct hierarchical clustering on the correlation matrix
ggcorrplot_clustered <- ggcorrplot(correl, hc.order = TRUE, type = "lower")
ggcorrplot_clustered
# writeTDE(df,  "Wrap Zone")
