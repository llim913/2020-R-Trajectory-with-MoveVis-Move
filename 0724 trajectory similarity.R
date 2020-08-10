#install.packages("trajectorySim", repos="http://R-Forge.R-project.org")
#install.packages("SimilarityMeasures")
#install.packages("vegclust")
#install.packages("RColorBrewer")
#install.packages("smacof")
#install.packages("simmer")
#install.packages("MASS")
#install.packages("ggpubr")
#install.packages("factoextra")
methods(class="trajectory")
library(vegclust)
library(RColorBrewer)
library(smacof)
library(SimilarityMeasures)
library(trajectorySim)
library(simmer)
library(leaflet)
library(MASS)
library(ggpubr)
library(factoextra)
library (dplyr)
help(SimilarityMeasures)
help(trajectorySim)
help(timeout)

file <- choose.files()
file
df <- read.csv(file,header =  T)
as <- subset(df, select = -c(X,Length,Beam,TRGT_SENSOR_KIND,AREA_ID,MSG_ID,LOC_ACCRCY)) 
head(as)
tail(as)

#안됨
t0 <- trajectory() %>%
  seize("res0", 1) %>%
  branch(function() 1, c(TRUE, FALSE),
         trajectory() %>%
           clone(2,
                 trajectory() %>%
                   seize("res1", 1) %>%
                   timeout(1) %>%
                   release("res1", 1),
                 trajectory() %>%
                   trap("signal",
                        handler=trajectory() %>%
                          timeout(1)) %>%
                   timeout(1)),
         trajectory() %>%
           set_attribute("dummy", 1) %>%
           seize("res2", function() 1) %>%
           timeout(function() rnorm(1, 20)) %>%
           release("res2", function() 1) %>%
           release("res0", 1) %>%
           rollback(11)) %>%
  synchronize() %>%
  rollback(2) %>%
  release("res0", 1)


# re

as$trj <- paste(as$LAT_VAL ,as$LON_VAL, sep = ','  )
as
#par(mar=c(4,4,1,1))
#trajectoryPCoA(as, as$stype, surveys, traj.colors = c("black","red", "blue"), lwd = 2)
#legend("topleft", col=c("black","red", "blue"), 
#       legend=c("Trajectory 1", "Trajectory 2", "Trajectory 3"), bty="n", lty=1, lwd = 2)
trajectoryLengths(as, as$SHIP_ID, surveys)
s1 = step1measures(as, example.data$time, ID = TRUE)
s2 = step2factors(as)

install.packages("traj")
library(traj)
s1 = step1measures(as$trj, as$RECV_DT , ID = T)
s2 = step2factors(as)

###########################################################################
file <- choose.files()
file
df <- read.csv(file,header =  T)
as <- subset(df, select = -c(X,Length,Beam,TRGT_SENSOR_KIND,AREA_ID,MSG_ID,LOC_ACCRCY)) 
head(as)
tail(as)
# Setup data and time
zz <- subset(df, select = -c(X,Length,ShipName,stype,Beam,TRGT_SENSOR_KIND,AREA_ID,MSG_ID,LOC_ACCRCY)) 
head(zz)

data = zz$SOG_VAL
time = zz$RECV_DT
data
time
# Run step1measures, step2factors and step3clusters
s1 = step1measures(data, time, SHIP_ID=TRUE)
s2 = step2factors(s1)
s3 = step3clusters(s2)

# Print and plot "traj object"
s3
plot(s3)

###########################################################

a1 <- filter (zz, SHIP_ID == 440004750 )# 케이스 추출(filter)
a2 <- select(a1,LON_VAL , LAT_VAL, SOG_VAL) # 변수 추출(select)
library (tidyr)
a3 <- spread (a2, SHIP_ID) #케이스를 변수로(spread)

a1 <- zz[duplicated(zz$SHIP_ID),]
tail(a1)

############################ https://www.datanovia.com/en/blog/k-means-clustering-visualization-in-r-step-by-step-guide/
data("iris")
dd <- iris
head(zz, 3)
# Compute k-means with k = 3
set.seed(123)
res.km <- kmeans(scale(zz[, 1]), 3, nstart = 25)
# K-means clusters showing the group of each individuals
tail(res.km$cluster)
fviz_cluster(res.km, data = zz[, 1],
             palette = c("#2E9FDF", "#00AFBB"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)
# Dimension reduction using PCA
res.pca <- prcomp(dd[, 1],  scale = TRUE)
# Coordinates of individuals
ind.coord <- as.data.frame(get_pca_ind(res.pca)$coord)
# Add clusters obtained using the K-means algorithm
ind.coord$cluster <- factor(res.km$cluster)
# Add Species groups from the original data sett
ind.coord$Species <- df$Species
# Data inspection
head(ind.coord)

# Percentage of variance explained by dimensions
eigenvalue <- round(get_eigenvalue(res.pca), 1)
variance.percent <- eigenvalue$variance.percent
head(eigenvalue)

ggscatter(
  ind.coord, x = "Dim.1", y = "Dim.2", 
  color = "cluster", palette = "npg", ellipse = TRUE, ellipse.type = "convex",
  shape = "cluster", size = 1.5,  legend = "right", ggtheme = theme_bw(),
  xlab = paste0("Dim 1 (", variance.percent[1], "% )" ),
  ylab = paste0("Dim 2 (", variance.percent[2], "% )" )
) +
  stat_mean(aes(color = cluster), size = 4)

ind.coord

zz

##############################
# Compute k-means with k = 3
set.seed(123)
res.km <- kmeans(scale(zz[, -5]), 3, nstart = 25)
# K-means clusters showing the group of each individuals
res.km$cluste

zz[, -5]

data("iris")
df <- iris
head(df, 3)
df[, -5]












