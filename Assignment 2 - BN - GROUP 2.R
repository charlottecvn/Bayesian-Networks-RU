### Assignment 2, Bayesian Network, TWINS
### Linda Schmeitz, Enrico Schmitz, Charlotte Cambier van Nooten

# Data can be reached from https://github.com/AMLab-Amsterdam/CEVAE/tree/master/datasets/TWINS

# Variables & paths to declare ----
Path_dir = "/Users/charlottecvn/Programming/RStudio/Bayesian Networks/Assignment 2 (BN)/"
N_samples = 15000
output_dir = "/Users/charlottecvn/Programming/RStudio/Bayesian Networks/Assignment 2 (BN)/"
alpha <- 0.05

# Libraries ----
library(plyr)
library(dagitty)
library(tidyverse)
library(bnlearn)
library(pcalg)
library(energy)

BiocManager::install("graph")
BiocManager::install("RBGL")
BiocManager::install("Rgraphviz")

# Paths ----
try(setwd(Path_dir))

# Read datasets ----
X_ds <- read.csv(file = 'TWINS (data)/twin_pairs_X_3years_samesex.csv', stringsAsFactors = FALSE)
Y_ds <- read.csv(file = 'TWINS (data)/twin_pairs_Y_3years_samesex.csv', stringsAsFactors = FALSE)
T_ds <- read.csv(file = 'TWINS (data)/twin_pairs_T_3years_samesex.csv', stringsAsFactors = FALSE)

XY <- merge(X_ds,Y_ds,by = "X") # Merge files to 1 dataframe
Dataset <- merge(XY,T_ds,by = "X") 

remove("X_ds","Y_ds","T_ds","XY") # Delete variables

drops = c("X", "Unnamed..0", "infant_id_0", "infant_id_1") # remove variables that are IDs
Dataset <- Dataset[ , !(names(Dataset) %in% drops)]

# Only get babies born in week 37-39
Dataset <- Dataset[Dataset$gestat10 == 6,]
Dataset <- Dataset[-Dataset$gestat10]

# Get N samples
Dataset <- Dataset[complete.cases(Dataset), ] #nan
Dataset <- head(Dataset,N_samples)

# Keep variables based on knowledge
Keep = c("alcohol", "anemia", "cardiac",
         "csex", "diabetes", "dtotord_min",
         "eclamp", "feduc6", "frace", "hemo", "herpes", "hydra",
         "incervix", "lung", "mager8", "meduc6", "mpre5", "mrace", "phyper",
         "pre4000", "preterm", "renal", "tobacco",
         "dbirwt_0", "dbirwt_1", "mort_0", "mort_1")
Dataset <- Dataset[ , (names(Dataset) %in% Keep)]
Dataset <- Dataset[ , order(names(Dataset))]

# Preprocessing data ------
Dataset_int <- as.data.frame(lapply(Dataset, as.integer))
head(Dataset_int)

drops_01 = c("dbirwt_0", "dbirwt_1", "feduc6", 
             "dtotord_min", "frace", "mpre5", "mrace",
             "mager8", "meduc6")
Dataset_drop01 <- Dataset_int[ , !(colnames(Dataset_int) %in% drops_01)]
colnames_drop01 <- colnames(Dataset_drop01)

levels(Dataset_int$anemia) <- c('True', 'False')

for (val in colnames_drop01){
  Dataset_int[,val] <- factor(Dataset_int[ , val], ordered = TRUE, levels = c(0,1))
}

cols_other = c("frace", "mrace", "mpre5", "meduc6", "mager8", "feduc6")
Dataset_colsother <- Dataset_int[ , (colnames(Dataset_int) %in% cols_other)]
colnames_other <- colnames(Dataset_colsother)

for (val in colnames_other){
  num = unique(Dataset_int[ , val])
  range_levels = range(Dataset_int[, val])
  Dataset_int[,val] <- factor(Dataset_int[ , val], ordered = TRUE, levels = seq(1,max(range_levels)))
  Dataset_int[,val] <- mapvalues(Dataset_int[,val], from = seq(1,max(range_levels)), to = seq(0,max(range_levels)-1))
}

Dataset_int$mrace <- ifelse(Dataset_int$mrace == 1, 1, Dataset_int$mrace )
Dataset_int$mrace <- ifelse(Dataset_int$mrace >= 2, 2, Dataset_int$mrace )
Dataset_int$frace <- ifelse(Dataset_int$frace == 1, 1, Dataset_int$mrace )
Dataset_int$frace <- ifelse(Dataset_int$frace >= 2, 2, Dataset_int$mrace )
Dataset_int$mrace <- factor(Dataset_int$mrace, ordered = FALSE, levels = seq(1,2))
Dataset_int$mrace <- mapvalues(Dataset_int$mrace, from = c(1,2), to = c(0,1))
Dataset_int$frace <- factor(Dataset_int$frace, ordered = FALSE, levels = seq(1,2))
Dataset_int$frace <- mapvalues(Dataset_int$frace, from = c(1,2), to = c(0,1))

Dataset_int$mager8 <- ifelse(Dataset_int$mager8 <=2, 1, Dataset_int$mager8)
Dataset_int$mager8 <- ifelse((Dataset_int$mager8 >=3) & (Dataset_int$mager8 <=5) , 2, Dataset_int$mager8)
Dataset_int$mager8 <- ifelse(Dataset_int$mager8 >=6, 3, Dataset_int$mager8)
Dataset_int$mager8 <- factor(Dataset_int$mager8, ordered = TRUE, levels = seq(1,3))
Dataset_int$mager8 <- mapvalues(Dataset_int$mager8, from = c(1,2,3), to = c(0,1,2))

Dataset_int$feduc6 <- ifelse((Dataset_int$feduc6 >=2) & (Dataset_int$feduc6 <=3), 2, Dataset_int$feduc6)
Dataset_int$feduc6 <- ifelse(Dataset_int$feduc6 >=4, 3, Dataset_int$feduc6)
Dataset_int$meduc6 <- ifelse((Dataset_int$meduc6 >=2) & (Dataset_int$meduc6 <=3), 2, Dataset_int$meduc6)
Dataset_int$meduc6 <- ifelse(Dataset_int$meduc6 >=4, 3, Dataset_int$meduc6)
Dataset_int$feduc6 <- factor(Dataset_int$feduc6, ordered = TRUE, levels = seq(1,3))
Dataset_int$feduc6 <- mapvalues(Dataset_int$feduc6, from = c(1,2,3), to = c(0,1,2))
Dataset_int$meduc6 <- factor(Dataset_int$meduc6, ordered = TRUE, levels = seq(1,3))
Dataset_int$meduc6 <- mapvalues(Dataset_int$meduc6, from = c(1,2,3), to = c(0,1,2))

Dataset_int$dbirwt_0 <- ifelse(Dataset_int$dbirwt_0 <=2000, 1, Dataset_int$dbirwt_0)
Dataset_int$dbirwt_0 <- ifelse((Dataset_int$dbirwt_0 >2000) & (Dataset_int$dbirwt_0 <=2750), 2, Dataset_int$dbirwt_0)
Dataset_int$dbirwt_0 <- ifelse((Dataset_int$dbirwt_0 >2750) & (Dataset_int$dbirwt_0 <=3500), 3, Dataset_int$dbirwt_0)
Dataset_int$dbirwt_0 <- ifelse(Dataset_int$dbirwt_0 >3500, 4, Dataset_int$dbirwt_0)
Dataset_int$dbirwt_0 <- factor(Dataset_int$dbirwt_0, ordered = TRUE, levels = seq(1,4))
Dataset_int$dbirwt_0 <- mapvalues(Dataset_int$dbirwt_0, from = c(1,2,3,4), to = c(0,1,2,3))

Dataset_int$dbirwt_1 <- ifelse(Dataset_int$dbirwt_1 <=2000, 1, Dataset_int$dbirwt_1)
Dataset_int$dbirwt_1 <- ifelse((Dataset_int$dbirwt_1 >2000) & (Dataset_int$dbirwt_1 <=2750), 2, Dataset_int$dbirwt_1)
Dataset_int$dbirwt_1 <- ifelse((Dataset_int$dbirwt_1 >2750) & (Dataset_int$dbirwt_1 <=3500), 3, Dataset_int$dbirwt_1)
Dataset_int$dbirwt_1 <- ifelse(Dataset_int$dbirwt_1 >3500, 4, Dataset_int$dbirwt_1)
Dataset_int$dbirwt_1 <- factor(Dataset_int$dbirwt_1, ordered = TRUE, levels = seq(1,4))
Dataset_int$dbirwt_1 <- mapvalues(Dataset_int$dbirwt_1, from = c(1,2,3,4), to = c(0,1,2,3))

Dataset_int$dtotord_min <- ifelse(Dataset_int$dtotord_min >=4, 4, Dataset_int$dtotord_min)
Dataset_int$dtotord_min <- factor(Dataset_int$dtotord_min, ordered = TRUE, levels = seq(1,4))
Dataset_int$dtotord_min <- mapvalues(Dataset_int$dtotord_min, from = c(1,2,3,4), to = c(0,1,2,3))

# Check for multivariate normal data ----
Dataset_num <- data.frame(sapply(Dataset_int, function(x) as.numeric(as.character(x))))
for (i in colnames(Dataset_num)){
  print(i)
  print(mvnorm.etest(Dataset_num$i, R=10))
} # All variables are multivariate normal (p-value < 0.05), Energy test of multivariate normality


# Naive Structure Learning, pcalg ----

n <- nrow(Dataset_int)
p <- ncol(Dataset_int)
V <- colnames(Dataset_int)

nlev <- sapply(Dataset_int, nlevels)

indepTest <- disCItest #gaussCItest, disCItest, binCItest, dsepTest
suffStat <- list(dm=Dataset_num, nlev=nlev, adaptDF=FALSE) #list(C=cor(Dataset_num), n = n)

# pc
pc.fit <- pc(suffStat, indepTest, labels = V, p = p, alpha = alpha, verbose = TRUE)
pc.fit_alpha10 <- pc(suffStat, indepTest, labels = V, p = p, alpha = 0.10, verbose = FALSE)
pc.fit_alpha01 <- pc(suffStat, indepTest, labels = V, p = p, alpha = 0.01, verbose = FALSE)

plot.new()
title = "Fit PC (pc, pcalg), alpha = 0.05"
jpeg(file = paste(title,".jpg"), res = 100, height =1500, width = 1500 )
plot(pc.fit)
graphics.off()

plot.new()
title = "Fit PC (pc, pcalg), alpha = 0.01"
jpeg(file = paste(title,".jpg"), res = 100, height =1500, width = 1500 )
plot(pc.fit_alpha01)
graphics.off()

plot.new()
title = "Fit PC (pc, pcalg), alpha = 0.10"
jpeg(file = paste(title,".jpg"), res = 100, height =1500, width = 1500 )
plot(pc.fit_alpha10)
graphics.off()

# rfci
rfci.fit <- rfci(suffStat, indepTest, labels = V, p = p, alpha = alpha, verbose = TRUE)
rfci.fit_alpha01 <- rfci(suffStat, indepTest, labels = V, p = p, alpha = 0.01, verbose = FALSE)
rfci.fit_alpha10 <- rfci(suffStat, indepTest, labels = V, p = p, alpha = 0.10, verbose = FALSE)

plot.new()
title = "Fit RFCI (rfci, pcalg), alpha = 0.05"
jpeg(file = paste(title,".jpg"), res = 100, height =1500, width = 1500 )
plot(rfci.fit)
graphics.off()

plot.new()
title = "Fit RFCI (rfci, pcalg), alpha = 0.01"
jpeg(file = paste(title,".jpg"), res = 100, height =1500, width = 1500 )
plot(rfci.fit_alpha01)
graphics.off()

plot.new()
title = "Fit RFCI (rfci, pcalg), alpha = 0.10"
jpeg(file = paste(title,".jpg"), res = 100, height =1500, width = 1500 )
plot(rfci.fit_alpha10)
graphics.off()

# GES (greedy, BIC)
score <- new("GaussL0penObsScore", Dataset_num)
ges.fit_forward <- ges(score,adaptive = c("vstructures"),verbose = TRUE)

plot.new()
title = "Estimated CPDAG, forward (ges, pcalg)"
jpeg(file = paste(title,".jpg"), res = 100, height =1500, width = 1500 )
plot(ges.fit_forward$essgraph)
graphics.off()

# Distance measures ----

#Structural Hamming Distance (SHD)
shd.val_pc1 <- shd(pc.fit_alpha01, pc.fit)
shd.val_pc2 <- shd(pc.fit, pc.fit_alpha10)
shd.val_pc3 <- shd(pc.fit_alpha01, pc.fit_alpha10)
print(shd.val_pc1)
print(shd.val_pc2)
print(shd.val_pc3)


