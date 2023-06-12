## ------------------------------------------------------------------------------------------------
## ------------------------------------------------------------------------------------------------
## Script name: ExploratoryAnalysis.R
##
## Purpose of script: Evaluate and explore our datasets through an Exploratory Data Analysis (EDA)
## To run this script use the following command: Rscript ExploratoryAnalysis.R 
##
## Author: Edgar de Souza Vismara
##         Rafael Gomes Mantovani
##
## Date Created: 2023-01-06

## ------------------------------------------------------------------------------------------
## ------------------------------------------------------------------------------------------

cat(" @ Loading all required files:\n")
R.files = list.files(path = "R", full.names = TRUE)
for(file in R.files) {
  print(file)
  source(file)
}

## ------------------------------------------------------------------------------------------
## 1. Reading data
## ------------------------------------------------------------------------------------------

cat(" - Loading data\n")
features = read_csv("~/capsule/data/dataset/features_prepro.csv")
labels   = read_csv("~/capsule/data/dataset/rotulos.csv")
labels   = labels %>% mutate(rotulo = as.factor(recode(rotulo, c1 = 1, c2 = 1, c3 = 2, c4 = 3)))


## ------------------------------------------------------------------------------------------
## Creating Output dirs
## ------------------------------------------------------------------------------------------

if(!dir.exists("~/capsule/plots/")) {
  dir.create("~/capsule/plots/", showWarnings=FALSE, recursive=TRUE)
}

## ------------------------------------------------------------------------------------------
# Plot 1: Class frequency
## ------------------------------------------------------------------------------------------

cat(" - Plot: classes' frequency\n")

values = c("Initial","Intermediate","Advanced")
df = data.frame(
  class = factor(values, levels=values),
  freq = as.numeric(table(labels$rotulo))
)

g1 = ggplot(data = df, mapping = aes(x = class, y = freq, fill = class, colour = class))
g1 = g1 + geom_bar(position = 'dodge', stat='identity')
g1 = g1 + geom_text(aes(label = freq), colour = "black", position=position_dodge(width=0.9), vjust=-0.25) 
g1 = g1 + labs(x = "Class", y = "Number of Examples") 
g1 = g1 + theme_bw() + theme(legend.position="none") 
# g1 #(for debug)
ggsave(g1, file = "~/capsule/plots/fig_class_distribution.pdf", width = 4.25, height = 4.14)

## ------------------------------------------------------------------------------------------
## 2. Preprocessing data
## ------------------------------------------------------------------------------------------

cat(" - Preprocessing data \n")

### 2.1 Removing NAs: 
## ----------------------------
tmp = features[ , colSums(is.na(features)) < nrow(features)]
features = tmp[complete.cases(tmp), ] 
dim(features)
# [1] 541  95

### 2.2 Removing constant features(variance = 0):
## ------------------------------------------------------------------------------------------

# Creating a function for this ...
rm0var = function(df, threshold = 0){
  df = df[, sapply(df, function(x) {var(x)}) > threshold]
  return(df)
}

## ------------------------------------------------------------------------------------------
cat("@Removing constant features ...\n")
cat("- Before:  ", ncol(features)-1, "features \n")
X = rm0var(features[,-1], threshold = 0)
X = cbind(features[,1],X)
cat("- After: ", ncol(X)-1, "features \n")

## ------------------------------------------------------------------------------------------
## Creating datasets (4 different versions)
## ------------------------------------------------------------------------------------------

cat("@Creating datasets \n")

# dataset 1: only color channels' features
X_cor = X %>%  select(Name_file, starts_with("cor")| starts_with("std_")
    |starts_with("mean")|starts_with("entropy") ,-contains("hist"))

# dataset 2: only histogram features
X_esta = X %>%  select(Name_file, starts_with("skew_hist")| starts_with("kurt_hist") | starts_with("std_hist"))

#dataset 3: all the remaining features
X_rest = X %>%  select(Name_file, -names(X_cor) & -names(X_esta))

#dataset 4: All the features
X_all  = X

## ------------------------------------------------------------------------------------------
# Plot 2: Number of features per dataset
## ------------------------------------------------------------------------------------------

cat(" - Plot: Datasets x Number of features\n")

values2 = c("Color Channels", "Histograms", "Other features", "Complete")
data  = factor(values2,levels = values2)
n.value = c(ncol(X_cor)-1, ncol(X_esta)-1, ncol(X_rest)-1, ncol(X_all)-1) 
df2 = data.frame(data = data, value = n.value)
# head(df2)

g2 = ggplot(data = df2, mapping = aes(x = data, y = value, fill = data, colour = data))
g2 = g2 + geom_bar(position = 'dodge', stat='identity')
g2 = g2 + geom_text(aes(label = value), colour = "black", position=position_dodge(width=0.9), vjust=-0.25)
g2 = g2 + labs(x = "Dataset", y = "Number of Features") + theme_bw() + theme(legend.position="none")
# g2 #(for debug)
ggsave(g2, file = "~/capsule/plots/fig_number_of_features_dataset.pdf", width = 4.25, height = 4.14)


cat("@Removing high correlated features\n")
### 2.3 Removing high correlated features
# For each dataset, there are 4 different cut points: > 0.8, > 0.85, > 0.9, > 0.95 

# Color Channel datasets:
## ------------------------------------------------------------------------------------------
tmp_cor = cor(X_cor[,-1])

hc80_cor = findCorrelation(tmp_cor, cutoff=0.8,  names = T, exact = TRUE) 
hc85_cor = findCorrelation(tmp_cor, cutoff=0.85, names = T, exact = TRUE) 
hc90_cor = findCorrelation(tmp_cor, cutoff=0.9,  names = T, exact = TRUE) 
hc95_cor = findCorrelation(tmp_cor, cutoff=0.95, names = T, exact = TRUE) 

n_out_cor = lengths(list(hc80_cor,hc85_cor,hc90_cor,hc95_cor))
graf_cor = data.frame('n_out' = n_out_cor, 'cutoff' = factor(c(0.8,0.85,0.9,0.95)))
# head(graf_cor)

# Histograms datasets:
## ------------------------------------------------------------------------------------------
tmp_esta = cor(X_esta[,-1])

hc80_esta = findCorrelation(tmp_esta, cutoff=0.8,  names = T, exact = TRUE) 
hc85_esta = findCorrelation(tmp_esta, cutoff=0.85, names = T, exact = TRUE) 
hc90_esta = findCorrelation(tmp_esta, cutoff=0.9,  names = T, exact = TRUE) 
hc95_esta = findCorrelation(tmp_esta, cutoff=0.95, names = T, exact = TRUE) 

n_out_esta = lengths(list(hc80_esta,hc85_esta,hc90_esta,hc95_esta))
graf_esta = data.frame('n_out' = n_out_esta, 'cutoff' = factor(c(0.8,0.85,0.9,0.95)))
# head(graf_esta)

# Other features' datasets:
## ------------------------------------------------------------------------------------------
tmp_rest = cor(X_rest[,-1])

hc80_rest = findCorrelation(tmp_rest, cutoff=0.8, names = T, exact = TRUE) 
hc85_rest= findCorrelation(tmp_rest, cutoff=0.85, names = T, exact = TRUE) 
hc90_rest= findCorrelation(tmp_rest, cutoff=0.9, names = T, exact = TRUE) 
hc95_rest= findCorrelation(tmp_rest, cutoff=0.95, names = T, exact = TRUE) 

n_out_rest = lengths(list(hc80_rest,hc85_rest,hc90_rest,hc95_rest))
graf_rest = data.frame('n_out' = n_out_rest, 'cutoff' = factor(c(0.8,0.85,0.9,0.95)))
# head(graf_rest)

# Complete datasets:
## ------------------------------------------------------------------------------------------
tmp = cor(X_all[,-1])

hc80 = findCorrelation(tmp, cutoff=0.8,  names = T, exact = TRUE) 
hc85 = findCorrelation(tmp, cutoff=0.85, names = T, exact = TRUE) 
hc90 = findCorrelation(tmp, cutoff=0.9,  names = T, exact = TRUE) 
hc95 = findCorrelation(tmp, cutoff=0.95, names = T, exact = TRUE) 
n_out = lengths(list(hc80,hc85,hc90,hc95))
graf_all = data.frame('n_out' = n_out, 'cutoff' = factor(c(0.8,0.85,0.9,0.95)))
# head(graf_all)

# We can plot all this aggregated information in a single image
graf_cor$set  = "Color Channels"
graf_esta$set = "Histograms"
graf_rest$set = "Other Features" 
graf_all$set  = "Complete"

# merging all the 4 data frames 
full.df = rbind(graf_cor, graf_esta, graf_rest, graf_all)
# full.df

## ------------------------------------------------------------------------------------------
# Plot 3: Number of features per dataset
## ------------------------------------------------------------------------------------------

cat(" - Plot: cut off criteria (correlation) \n")
# Plotar a informação conjunta, com 4 curvas
# library(ggrepel)

g3 = ggplot(data=full.df, aes(x=cutoff, y=n_out, group = set, colour=set, linetype=set, shape=set, label=n_out))
g3 = g3 + geom_line() + geom_point() + theme_bw()
g3 = g3 + ggrepel::geom_label_repel( nudge_x = 0.1, vjust=+0.25, show.legend = F)
g3 = g3 + labs(x = "Cutt-off criteria (Correlation)", y = "Number of features removed", 
       colour='Dataset', linetype = 'Dataset', shape = 'Dataset') 
g3 = g3 + theme(legend.title = element_text(size=10), #change legend title font size
        legend.text = element_text(size=10))  
# g3 #(for debug)
ggsave(g3, file = "~/capsule/plots/fig_cor_datasets.pdf", width = 6.75, height = 4.15)


## ------------------------------------------------------------------------------------------
## ------------------------------------------------------------------------------------------
# Obs: selected the cut off criteria = 0.85.
# Then, applying this cutt to all the datasets 

X_cor = X_cor[, ! names(X_cor) %in% c(hc85_cor)]
# head(X_cor)

X_esta = X_esta[, ! names(X_esta) %in% c(hc85_esta)]
# head(X_esta)

X_rest = X_rest[, ! names(X_rest) %in% c(hc85_rest)]
# head(X_rest)

X_all = X_all[, ! names(X_all) %in% c(hc85)]
# head(X_all)

## ------------------------------------------------------------------------------------------
# Creating the Aggregared dataset, with all the features that are not correlated
X_all2 = cbind(X_cor, X_esta[,-1], X_rest[,-1]) 

## ------------------------------------------------------------------------------------------
# Plot 4: Correlation plot of the two biggest datasets (with most features)
## ------------------------------------------------------------------------------------------

cat(" - Plot: correlation between features\n")

pdf(file = "~/capsule/plots/fig_correlation_complete_dataset.pdf") 
corrplot(cor(X_all[,-1]), method = "circle", type= "lower", diag = FALSE, order = 'hclust', tl.col = 'black', 
         cl.ratio = 0.1, tl.srt = 45)
dev.off()

pdf(file = "~/capsule/plots/fig_correlation_aggregated_dataset.pdf") 
corrplot(cor(X_all2[,-1]), method = "circle", type= "lower", diag = FALSE, order = 'hclust', tl.col = 'black', 
         cl.ratio = 0.1, tl.srt = 45)
dev.off()

## ------------------------------------------------------------------------------------------
## ------------------------------------------------------------------------------------------
cat ("Exploratory Data Analysis: Done!!!\n")
## ------------------------------------------------------------------------------------------
## ------------------------------------------------------------------------------------------
