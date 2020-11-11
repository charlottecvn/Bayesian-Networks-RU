### Assignment 1, Bayesian Network, TWINS
### Linda Schmeitz, Enrico Schmitz, Charlotte Cambier van Nooten

# Variables & paths to declare ----
Path_dir = "/Users/charlottecvn/Downloads/BN/Assignment 1/TWINS"
N_samples = 15000
output_dir = "/Users/charlottecvn/Downloads/BN/Assignment 1/"
alpha <- 0.05
rmsea_cutoff <- 0.08

# Libraries ----
library(plyr)
library(dagitty)
library(bnlearn)
library(lavaan)
library(tidyverse)
library(psych)
library( bayesianNetworks)

# Paths ----
try(setwd(Path_dir))
pdf_file = file.path(output_dir, "Plots.pdf")

# Read datasets ----
X_ds <- read.csv(file = 'twin_pairs_X_3years_samesex.csv', stringsAsFactors = FALSE)
Y_ds <- read.csv(file = 'twin_pairs_Y_3years_samesex.csv', stringsAsFactors = FALSE)
T_ds <- read.csv(file = 'twin_pairs_T_3years_samesex.csv', stringsAsFactors = FALSE)

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

# Weight distribution ----
title = "Birthweight distribution"
jpeg(file = file.path(output_dir,paste(title,".jpg")), res = 100)
par(mfrow=c(2,1))
br=100
xlab = "Birthweight (gram)"
XL <- c(0,round_any(max(Dataset$dbirwt_1), 1000, f = ceiling))

h0 <- hist(Dataset$dbirwt_0, breaks = br, col = heat.colors(br, rev=TRUE), main = paste(title,"lightest twin", collapse = " "), xlab = xlab, xlim = XL, las=1)
h1 <- hist(Dataset$dbirwt_1, breaks = br, col = heat.colors(br, rev=TRUE), main = paste(title,"heaviest twin", collapse = " "), xlab = xlab, xlim = XL, las=1)
dev.off()

# Create initial DAG ----
DAG <- dagitty('
dag {
bb="0,0,1,1"
alcohol [pos="0.196,0.429"]
anemia [pos="0.335,0.349"]
cardiac [pos="0.334,0.538"]
csex [pos="0.060,0.594"]
dbirwt_0 [pos="0.669,0.379"]
dbirwt_1 [pos="0.671,0.519"]
diabetes [pos="0.233,0.955"]
dtotord_min [pos="0.560,0.959"]
eclamp [pos="0.340,0.439"]
feduc6 [pos="0.054,0.412"]
fraceB [pos="0.059,0.830"]
fraceO [pos="0.059,0.917"]
fraceW [pos="0.059,0.747"]
hemo [pos="0.328,0.713"]
herpes [pos="0.332,0.626"]
hydra [pos="0.294,0.880"]
incervix [pos="0.289,0.178"]
lung [pos="0.334,0.267"]
mager8 [pos="0.059,0.500"]
meduc6 [pos="0.051,0.333"]
mort_0 [pos="0.876,0.377"]
mort_1 [pos="0.876,0.512"]
mpre5 [pos="0.569,0.803"]
mraceB [pos="0.056,0.121"]
mraceO [pos="0.059,0.206"]
mraceW [pos="0.062,0.039"]
phyper [pos="0.316,0.802"]
pre4000 [pos="0.531,0.210"]
preterm [pos="0.524,0.094"]
renal [pos="0.237,0.116"]
tobacco [pos="0.195,0.531"]
alcohol -> cardiac
alcohol -> dbirwt_0
alcohol -> dbirwt_1
alcohol -> herpes
alcohol -> renal
alcohol <-> tobacco
anemia -> dbirwt_0
anemia -> dbirwt_1
anemia -> dtotord_min
cardiac -> dbirwt_0
cardiac -> dbirwt_1
cardiac -> dtotord_min
cardiac -> mpre5
csex -> dbirwt_0
csex -> dbirwt_1
csex -> pre4000
dbirwt_0 -> mort_0
dbirwt_1 -> mort_1
diabetes -> dbirwt_0
diabetes -> dbirwt_1
diabetes -> dtotord_min
diabetes -> hydra
diabetes -> mort_0
diabetes -> mort_1
diabetes -> pre4000
eclamp -> dbirwt_0
eclamp -> dbirwt_1
eclamp -> dtotord_min
eclamp -> mort_0
eclamp -> mort_1
feduc6 -> alcohol
feduc6 -> tobacco
fraceO -> alcohol
fraceO -> anemia
fraceO -> cardiac
fraceO -> diabetes
fraceO -> eclamp
fraceO -> hemo
fraceO -> herpes
fraceO -> phyper
fraceO -> tobacco
fraceB -> alcohol
fraceB -> anemia
fraceB -> cardiac
fraceB -> diabetes
fraceB -> eclamp
fraceB -> hemo
fraceB -> herpes
fraceB -> phyper
fraceB -> tobacco
fraceW -> alcohol
fraceW -> anemia
fraceW -> cardiac
fraceW -> diabetes
fraceW -> eclamp
fraceW -> hemo
fraceW -> herpes
fraceW -> phyper
fraceW -> tobacco
hemo -> dbirwt_0
hemo -> dbirwt_1
hemo -> dtotord_min
herpes -> dtotord_min
incervix -> dtotord_min
incervix -> preterm
lung -> dbirwt_0
lung -> dbirwt_1
lung -> dtotord_min
mager8 -> alcohol
mager8 -> anemia
mager8 -> cardiac
mager8 -> diabetes
mager8 -> eclamp
mager8 -> hemo
mager8 -> herpes
mager8 -> hydra
mager8 -> incervix
mager8 -> lung
mager8 -> mpre5
mager8 -> phyper
mager8 -> renal
mager8 -> tobacco
meduc6 -> alcohol
meduc6 -> mpre5
meduc6 -> tobacco
mort_0 -> dtotord_min
mort_1 -> dtotord_min
mpre5 -> dbirwt_0
mpre5 -> dbirwt_1
mraceW -> alcohol
mraceW -> anemia
mraceW -> cardiac
mraceW -> diabetes
mraceW -> eclamp
mraceW -> hemo
mraceW -> herpes
mraceW -> phyper
mraceW -> tobacco
mraceB -> alcohol
mraceB -> anemia
mraceB -> cardiac
mraceB -> diabetes
mraceB -> eclamp
mraceB -> hemo
mraceB -> herpes
mraceB -> phyper
mraceB -> tobacco
mraceO -> alcohol
mraceO -> anemia
mraceO -> cardiac
mraceO -> diabetes
mraceO -> eclamp
mraceO -> hemo
mraceO -> herpes
mraceO -> phyper
mraceO -> tobacco
phyper -> dbirwt_0
phyper -> dbirwt_1
phyper -> dtotord_min
phyper -> mort_0
phyper -> mort_1
phyper -> preterm
pre4000 -> dbirwt_0
pre4000 -> dbirwt_1
preterm -> dbirwt_0
preterm -> dbirwt_1
preterm -> mort_0
preterm -> mort_1
renal -> dtotord_min
tobacco -> dbirwt_0
tobacco -> dbirwt_1
tobacco -> eclamp
tobacco -> herpes
tobacco -> lung
}
')
title = "Dagitty"
jpeg(file = file.path(output_dir,paste(title,".jpg")), res = 100, height =960, width = 960 )
plot(DAG)
graphics.off()

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
}

Dataset_int$mraceW <- ifelse(Dataset_int$mrace == 1, 1, 0)
Dataset_int$mraceB <- ifelse(Dataset_int$mrace == 2, 1, 0)
Dataset_int$mraceO <- ifelse(Dataset_int$mrace >= 3, 1, 0)
Dataset_int$fraceW <- ifelse(Dataset_int$frace == 1, 1, 0)
Dataset_int$fraceB <- ifelse(Dataset_int$frace == 2, 1, 0)
Dataset_int$fraceO <- ifelse(Dataset_int$frace >= 3, 1, 0)
Dataset_int$mrace <- NULL
Dataset_int$frace <- NULL

Dataset_int$mager8 <- ifelse(Dataset_int$mager8 <=2, 1, Dataset_int$mager8)
Dataset_int$mager8 <- ifelse((Dataset_int$mager8 >=3) & (Dataset_int$mager8 <=5) , 2, Dataset_int$mager8)
Dataset_int$mager8 <- ifelse(Dataset_int$mager8 >=6, 3, Dataset_int$mager8)

Dataset_int$feduc6 <- ifelse((Dataset_int$feduc6 >=2) & (Dataset_int$feduc6 <=3), 2, Dataset_int$feduc6)
Dataset_int$feduc6 <- ifelse(Dataset_int$feduc6 >=4, 3, Dataset_int$feduc6)
Dataset_int$meduc6 <- ifelse((Dataset_int$meduc6 >=2) & (Dataset_int$meduc6 <=3), 2, Dataset_int$meduc6)
Dataset_int$meduc6 <- ifelse(Dataset_int$meduc6 >=4, 3, Dataset_int$meduc6)

Dataset_int$dbirwt_0 <- ifelse(Dataset_int$dbirwt_0 <=2000, 1, Dataset_int$dbirwt_0)
Dataset_int$dbirwt_0 <- ifelse((Dataset_int$dbirwt_0 >2000) & (Dataset_int$dbirwt_0 <=2750), 2, Dataset_int$dbirwt_0)
Dataset_int$dbirwt_0 <- ifelse((Dataset_int$dbirwt_0 >2750) & (Dataset_int$dbirwt_0 <=3500), 3, Dataset_int$dbirwt_0)
Dataset_int$dbirwt_0 <- ifelse(Dataset_int$dbirwt_0 >3500, 4, Dataset_int$dbirwt_0)

Dataset_int$dbirwt_1 <- ifelse(Dataset_int$dbirwt_1 <=2000, 1, Dataset_int$dbirwt_1)
Dataset_int$dbirwt_1 <- ifelse((Dataset_int$dbirwt_1 >2000) & (Dataset_int$dbirwt_1 <=2750), 2, Dataset_int$dbirwt_1)
Dataset_int$dbirwt_1 <- ifelse((Dataset_int$dbirwt_1 >2750) & (Dataset_int$dbirwt_1 <=3500), 3, Dataset_int$dbirwt_1)
Dataset_int$dbirwt_1 <- ifelse(Dataset_int$dbirwt_1 >3500, 4, Dataset_int$dbirwt_1)

Dataset_int$dtotord_min <- ifelse(Dataset_int$dtotord_min >=4, 4, Dataset_int$dtotord_min)

# Distribution variables ----
Nsamples = nrow(Dataset_int)
max_uni = 0
for (col in names(Dataset_int)){
  uni = length(table(Dataset_int[col]))
  if (max_uni < uni){
    max_uni = uni
  }
}
print(max_uni) 
Variable_dataframe <- matrix(0, max_uni, ncol(Dataset_int))
colnames(Variable_dataframe) <- colnames(Dataset_int)
rownames(Variable_dataframe) <- seq(0, nrow(Variable_dataframe)-1)
n = 1
for (col in names(Dataset_int)){
  print(col)
  tab <- table(Dataset_int[col])
  m = 1
  for (value in tab){
    Variable_dataframe[m,n] <- (value/Nsamples)*100
    m <- m+1
  }
  n <- n+1
}

title = "Barplot_variables"
jpeg(file = file.path(output_dir,paste(title,".jpg")), res = 100, height =400, width = 1000 )
par(mar = c(4, 4, 4, 5),xpd = TRUE)
barplot(Variable_dataframe, las=2,col = rainbow(max_uni), main = "Variable distribution", ylab = "Percentage (%)",cex.names=0.8)
legend(40,100, c("0","1","2","3"), col=rainbow(max_uni), pch=22, pt.bg = rainbow(max_uni))
graphics.off()

# Localtets (Chisq) ------
localtests_dag <- localTests(DAG, Dataset_int, type='cis.chisq', max.conditioning.variables = 2)
plotLocalTestResults(localtests_dag)

p_values <- localtests_dag[order(localtests_dag$p.value,decreasing=TRUE),]
high_p <- p_values[which(p_values$p.value < alpha),] 
p_rmsea_cutoff <- high_p[which(high_p$rmsea < rmsea_cutoff),]

full_names <- sort(colnames(Dataset_int))
short_names <- c("alch", "anem", "crdc", "csex", "db_0", "db_1", "dbts" ,"dtt_", 
                 "eclm" ,"fdc6" ,"frcB","frcO","frcW" ,"hemo", "hrps", "hydr", "incr", "lung", 
                 "mgr8" ,"mdc6","mr_0", "mr_1", "mpr5",  "mrcB","mrcO","mrcW" ,"phyp", "p400" ,
                 "prtr" ,"renl" ,"tbcc")

replace_var_name <- function(var_name, full_names, short_names){
  var_name <- full_names[which(var_name==short_names)]
  return(var_name)
}

extra_variables_dag <- function(p_rmsea_cutoff, full_names, short_names){
  names <- rownames(p_rmsea_cutoff)
  list_all <- list()
  for (name in names){
    num_char <- nchar(name)
    var1 <- substring(name, 0,4)
    var2 <- substring(name, 11,14)
    
    var1 <- replace_var_name(var1, full_names,short_names)
    var2 <- replace_var_name(var2, full_names,short_names)

    list_all <- append(list_all, paste(var1, var2, sep = " -> "))
    
    if (num_char > 14){
      var3 <- substring(name, 18,21)
      var3 <- replace_var_name(var3, full_names,short_names)
      list_all <- append(list_all, paste(var3, var1, sep = " -> "))
      if (num_char > 21){
        var4 <- substring(name, 24,28)
        var4 <- replace_var_name(var4, full_names,short_names)
        list_all <- append(list_all, paste(var4, var1, sep = " -> "))
      }
    }
  }
  return (list_all)
}

test_extra <- extra_variables_dag(p_rmsea_cutoff, full_names, short_names)

string_dag = c(DAG)
string_dag <- str_remove_all(string_dag, "[}]")
extra_var <- string_dag

for (var in (unique(test_extra))){
  paste0 <- paste(extra_var, var, sep="")
  extra_var_acyclic  <- paste(paste0, ";\n", sep="")
  if (isAcyclic(dagitty(paste(extra_var_acyclic, "}")))){
    extra_var <- extra_var_acyclic
  }
}

DAG_improved <- dagitty(paste(extra_var, "}"))

title = "Dagitty Improved"
jpeg(file = file.path(output_dir,paste(title,".jpg")), res = 100, height =960, width = 960 )
plot(DAG_improved)
graphics.off()

localTests(DAG_improved, Dataset_int, type='cis.chisq', max.conditioning.variables = 1)

# Create merged dataset ------
Dataset_merge <- Dataset_int

diseases <- c("anemia", "cardiac", "diabetes", "eclamp", "hemo", "herpes", "hydra", "incervix", "lung", "phyper", "renal")
Dataset_merge$diseases <- matrix(0, nrow(Dataset_merge))
for (disease in diseases){
  Dataset_merge$diseases <- ifelse((Dataset_merge[disease] ==1), 1, Dataset_merge$diseases)
  Dataset_merge[disease] <- NULL
}

# Distribution merged variables ------
Nsamples = nrow(Dataset_merge)
max_uni = 0
for (col in names(Dataset_merge)){
  uni = length(table(Dataset_merge[col]))
  if (max_uni < uni){
    max_uni = uni
  }
}
print(max_uni) 
Variable_dataframe <- matrix(0, max_uni, ncol(Dataset_merge))
colnames(Variable_dataframe) <- colnames(Dataset_merge)
rownames(Variable_dataframe) <- seq(0, nrow(Variable_dataframe)-1)
n = 1
for (col in names(Dataset_merge)){
  print(col)
  tab <- table(Dataset_merge[col])
  m = 1
  for (value in tab){
    Variable_dataframe[m,n] <- (value/Nsamples)*100
    m <- m+1
  }
  n <- n+1
}

title = "Barplot_variables_merged"
jpeg(file = file.path(output_dir,paste(title,".jpg")), res = 100, height =400, width = 1000 )
par(mar = c(4, 4, 4, 5),xpd = TRUE)
barplot(Variable_dataframe, las=2,col = rainbow(max_uni), main = "Merged variable distribution", ylab = "Percentage (%)",cex.names=0.8)
legend(26,100, c("0","1","2","3"), col=rainbow(max_uni), pch=22, pt.bg = rainbow(max_uni))
graphics.off()

# Create merged DAG -----
DAG_merged <- dagitty('
dag {
bb="0,0,1,1"
alcohol [pos="0.342,0.390"]
csex [pos="0.154,0.563"]
dbirwt_0 [pos="0.669,0.379"]
dbirwt_1 [pos="0.671,0.519"]
diseases [pos="0.500,0.500"]
dtotord_min [pos="0.560,0.959"]
feduc6 [pos="0.041,0.722"]
fraceB [pos="0.158,0.932"]
fraceO [pos="0.232,1.019"]
fraceW [pos="0.104,0.852"]
mager8 [pos="0.156,0.454"]
meduc6 [pos="0.045,0.297"]
mort_0 [pos="0.876,0.377"]
mort_1 [pos="0.876,0.512"]
mpre5 [pos="0.569,0.803"]
mraceB [pos="0.159,0.074"]
mraceO [pos="0.089,0.164"]
mraceW [pos="0.229,-0.006"]
pre4000 [pos="0.531,0.210"]
preterm [pos="0.524,0.094"]
tobacco [pos="0.342,0.619"]
alcohol -> diseases
alcohol -> dbirwt_0
alcohol -> dbirwt_1
alcohol <-> tobacco
diseases -> dbirwt_0
diseases -> dbirwt_1
diseases -> dtotord_min
diseases -> mpre5
csex -> dbirwt_0
csex -> dbirwt_1
csex -> pre4000
dbirwt_0 -> mort_0
dbirwt_1 -> mort_1
diseases -> mort_0
diseases -> mort_1
diseases -> pre4000
feduc6 -> alcohol
feduc6 -> tobacco
fraceO -> alcohol
fraceO -> diseases
fraceO -> tobacco
fraceB -> alcohol
fraceB -> diseases
fraceB -> tobacco
fraceW -> alcohol
fraceW -> diseases
fraceW -> tobacco
diseases -> preterm
mager8 -> alcohol
mager8 -> diseases
mager8 -> mpre5
mager8 -> tobacco
meduc6 -> alcohol
meduc6 -> mpre5
meduc6 -> tobacco
mort_0 -> dtotord_min
mort_1 -> dtotord_min
mpre5 -> dbirwt_0
mpre5 -> dbirwt_1
mraceW -> alcohol
mraceW -> diseases
mraceW -> tobacco
mraceB -> alcohol
mraceB -> diseases
mraceB -> tobacco
mraceO -> alcohol
mraceO -> diseases
mraceO -> tobacco
pre4000 -> dbirwt_0
pre4000 -> dbirwt_1
preterm -> dbirwt_0
preterm -> dbirwt_1
preterm -> mort_0
preterm -> mort_1
diseases -> dtotord_min
tobacco -> dbirwt_0
tobacco -> dbirwt_1
tobacco -> diseases
}
')
title = "Dagitty_merged"
jpeg(file = file.path(output_dir,paste(title,".jpg")), res = 100, height =960, width = 960 )
plot(DAG_merged)
graphics.off()

# Localtests (Chisq, DAG_merged) ----
localtests_dag_merge <- localTests(DAG_merged, Dataset_merge, type='cis.chisq', max.conditioning.variables = 2)
plotLocalTestResults(localtests_dag_merge)

p_values <- localtests_dag_merge[order(localtests_dag_merge$p.value,decreasing=TRUE),]
high_p <- p_values[which(p_values$p.value < alpha),] 
p_rmsea_cutoff <- high_p[which(high_p$rmsea < rmsea_cutoff),]
p_rmsea_cutoff <- p_rmsea_cutoff[c(0:2, 4:nrow(p_rmsea_cutoff)),]

full_names <- sort(colnames(Dataset_merge)) 
short_names <- c("alch", "csex", "db_0", "db_1","dsss", "dtt_","fdc6" ,"frcB","frcO","frcW", 
                 "mgr8" ,"mdc6","mr_0", "mr_1", "mpr5",  "mrcB","mrcO","mrcW", "p400" ,
                 "prtr" ,"tbcc")

test_extra <- extra_variables_dag(p_rmsea_cutoff, full_names, short_names)

string_dag_merged = c(DAG_merged)
string_dag_merged <- str_remove_all(string_dag_merged, "[}]")
extra_var <- string_dag_merged

for (var in (unique(test_extra))){
  paste0 <- paste(extra_var, var, sep="")
  extra_var_acyclic  <- paste(paste0, ";\n", sep="")
  if (isAcyclic(dagitty(paste(extra_var_acyclic, "}")))){
    extra_var <- extra_var_acyclic
  }
}

DAG_improved_merged <- dagitty(paste(extra_var, "}"))

title = "Dagitty Improved Merged"
jpeg(file = file.path(output_dir,paste(title,".jpg")), res = 100, height =960, width = 960 )
plot(DAG_improved_merged)
graphics.off()

localtests_dag_merge_impr <- localTests(DAG_improved_merged, Dataset_merge, type='cis.chisq', max.conditioning.variables = 2)
localtests_dag_merge_impr

# Pruning and testing (DAG_merged) -----
pruning_dag <- function (g){
  net1 <-model2network(toString(g,"bnlearn"))
  for( x in names(g) ){
    px <- parents( net1,x )
    for( y in px ){
      tst <- ci.test( x, y,setdiff(px,y), data=Dataset_merge) 
      if (tst$p.value > alpha){
        g <- str_remove(g, paste(y,'->',x))
      }
    }
  }
  return(dagitty(g))
}

pruned_DAG_merged <- pruning_dag(DAG_improved_merged)

title = "Dagitty_pruned_merged"
jpeg(file = file.path(output_dir,paste(title,".jpg")), res = 100, height =2000, width = 4000 )
plot(pruned_DAG_merged, show.coefficients=TRUE)
graphics.off()

# Coefficients (DAG_merged) ------
coeff_dag <- function(g) {
  coeff <- list()
  net1 <-model2network(toString(g,"bnlearn"))
  i = 0
  coeff_g <- g
  for ( x in names(g) ) {
    px <- parents( net1,x )
    for ( y in px ) {
      tst <- ci.test( x, y,setdiff(px,y), data=Dataset_merge) 
      coeff_g <- str_replace(coeff_g, paste(y,'->',x), paste(paste(y,'->',x, "[beta=\""), round(tst$effect, digits=2), "\"]"))
      coeff[i] <- tst$effect
      i = i+1
    }
  }
  return(coeff_g)
}

cg <- coeff_dag(pruned_DAG_merged)

title = "Dagitty_coefficients_merged"
jpeg(file = file.path(output_dir,paste(title,".jpg")), res = 100, height =1000, width = 1000 )
plot(dagitty(cg), show.coefficients=TRUE)
graphics.off()
