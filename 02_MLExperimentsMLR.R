## ------------------------------------------------------------------------------------------------
## ------------------------------------------------------------------------------------------------
## Script name: MLExperimentsMLR
##
## Purpose of script: run ML experiments based on the mlr package
##
## Author: Edgar de Souza Vismara
##         Rafael Gomes Mantovani
##
## Date Created: 2023-01-06
## ------------------------------------------------------------------------------------------------
## ------------------------------------------------------------------------------------------------

cat(" @ Loading all required files:\n")
R.files = list.files(path = "R", full.names = TRUE)
for(file in R.files) {
  print(file)
  source(file)
}

# TODO: if results are available, does not run again, skip to analysis

## ------------------------------------------------------------------------------------------
## 1. Reading data
## ------------------------------------------------------------------------------------------

cat(" @ Loading Dataset\n")
features = read_csv("data/dataset/features_prepro.csv")
labels   = read_csv("data/dataset/rotulos.csv")
labels   = labels %>% mutate(rotulo = as.factor(recode(rotulo, c1 = 1, c2 = 1, c3 = 2, c4 = 3)))

## ------------------------------------------------------------------------------------------
## Creating output dirs 
## ------------------------------------------------------------------------------------------

cat(" @ Creating output folders\n")
if(!dir.exists("plots/")) {
  dir.create("plots/", showWarnings=FALSE, recursive=TRUE)
}

if(!dir.exists("results/")) {
  dir.create("results/", showWarnings=FALSE, recursive=TRUE)
}

## ------------------------------------------------------------------------------------------
## 2. Preprocessing data
## ------------------------------------------------------------------------------------------
### 2.1 Removing NAs: 
## ----------------------------
cat(" @ Data Preprocessing\n")
tmp = features[ , colSums(is.na(features)) < nrow(features)]
features = tmp[complete.cases(tmp), ] 
dim(features)

### 2.2 Removing constant features(variance = 0):
## ------------------------------------------------------------------------------------------

## ------------------------------------------------------------------------------------------
cat("@ Removing constant features ...\n")
cat("- Before:  ", ncol(features)-1, "features \n")
X = rm0var(features[,-1], threshold = 0)
X = cbind(features[,1],X)
cat("- After: ", ncol(X)-1, "features \n")

## ------------------------------------------------------------------------------------------
## Creating datasets (4 different versions)
## ------------------------------------------------------------------------------------------

cat("@ Creating differnt datasets\n")
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
## ------------------------------------------------------------------------------------------
### 2.3 Removing high correlated features
# For each dataset, there are 4 different cut points: > 0.8, > 0.85, > 0.9, > 0.95 
# Obs: opted to use the cut off criteria = 0.85.
# Then, applying this cutt to all the datasets 

cat("@ Removing High correlated features (cut off = 0.85)\n")

# Color Channel datasets:
## ------------------------------------------------------------------------------------------
tmp_cor = cor(X_cor[,-1])
hc85_cor = findCorrelation(tmp_cor, cutoff=0.85, names = T, exact = TRUE) 
X_cor = X_cor[, ! names(X_cor) %in% c(hc85_cor)]

# Histograms datasets:
## ------------------------------------------------------------------------------------------
tmp_esta = cor(X_esta[,-1])
hc85_esta = findCorrelation(tmp_esta, cutoff=0.85, names = T, exact = TRUE) 
X_esta = X_esta[, ! names(X_esta) %in% c(hc85_esta)]

# Other features' datasets:
## ------------------------------------------------------------------------------------------
tmp_rest = cor(X_rest[,-1])
hc85_rest= findCorrelation(tmp_rest, cutoff=0.85, names = T, exact = TRUE) 
X_rest = X_rest[, ! names(X_rest) %in% c(hc85_rest)]

# Complete datasets:
## ------------------------------------------------------------------------------------------
tmp = cor(X_all[,-1])
hc85 = findCorrelation(tmp, cutoff=0.85, names = T, exact = TRUE) 
X_all = X_all[, ! names(X_all) %in% c(hc85)]

# Creating the Aggregared dataset, with all the features that are not correlated
## ------------------------------------------------------------------------------------------

X_all2 = cbind(X_cor, X_esta[,-1], X_rest[,-1]) 

## ------------------------------------------------------------------------------------------
## ------------------------------------------------------------------------------------------
### 2.4 Creating Datasets for ML training (adding target values)
## ------------------------------------------------------------------------------------------

df_all  = labels %>% inner_join(X_all,  by = "Name_file") %>% select(-one_of('Name_file'))
df_all2 = labels %>% inner_join(X_all2, by = "Name_file") %>% select(-one_of('Name_file'))
df_cor  = labels %>% inner_join(X_cor,  by = "Name_file") %>% select(-one_of('Name_file'))
df_esta = labels %>% inner_join(X_esta, by = "Name_file") %>% select(-one_of('Name_file'))
df_rest = labels %>% inner_join(X_rest, by = "Name_file") %>% select(-one_of('Name_file'))


## ------------------------------------------------------------------------------------------
## Saving datasets 
## ------------------------------------------------------------------------------------------


tmp_all  = labels %>% inner_join(X_all,  by = "Name_file")
tmp_all2 = labels %>% inner_join(X_all2,  by = "Name_file")
tmp_cor  = labels %>% inner_join(X_cor,  by = "Name_file")
tmp_esta = labels %>% inner_join(X_esta,  by = "Name_file")
tmp_rest = labels %>% inner_join(X_rest,  by = "Name_file")

write.csv(as.data.frame(tmp_all),  file = "data/dataset/task_complete.csv")
write.csv(as.data.frame(tmp_all2), file = "data/dataset/task_aggregated.csv")
write.csv(as.data.frame(tmp_cor),  file = "data/dataset/task_color_channels.csv")
write.csv(as.data.frame(tmp_esta), file = "data/dataset/task_histograms.csv")
write.csv(as.data.frame(tmp_rest), file = "data/dataset/task_other_features.csv")

## ------------------------------------------------------------------------------------------
## ------------------------------------------------------------------------------------------

## Rescaling dataframes
df_all_resc  = df_all  %>% mutate_each_(
  list(~rescale(., to = c(-1, 1)) %>% as.vector), vars = names(df_all)[-1])
df_all2_resc = df_all2 %>% mutate_each_(
  list(~rescale(., to = c(-1, 1)) %>% as.vector), vars = names(df_all2)[-1])
df_cor_resc  = df_cor  %>% mutate_each_(
  list(~rescale(., to = c(-1, 1)) %>% as.vector), vars = names(df_cor)[-1])
df_esta_resc = df_esta %>% mutate_each_(
  list(~rescale(., to = c(-1, 1)) %>% as.vector), vars = names(df_esta)[-1])
df_rest_resc = df_rest %>% mutate_each_(
  list(~rescale(., to = c(-1, 1)) %>% as.vector), vars = names(df_rest)[-1])


## ------------------------------------------------------------------------------------------
## 3. ML training using mlr3
## ------------------------------------------------------------------------------------------

cat("@ Creating learning tasks\n")
# a) creating learning tasks (classification)
task_all  = mlr::makeClassifTask(id = "Complete", data = as.data.frame(df_all_resc), target = "rotulo")
task_cor  = mlr::makeClassifTask(id = "Color Channels", data = as.data.frame(df_cor_resc), target = "rotulo")
task_esta = mlr::makeClassifTask(id = "Histograms", data = as.data.frame(df_esta_resc), target = "rotulo")
task_rest = mlr::makeClassifTask(id = "Other Features", data = as.data.frame(df_rest_resc), target = "rotulo")
task_all2 = mlr::makeClassifTask(id = "Aggregated", data = as.data.frame(df_all2_resc), target = "rotulo")

## ------------------------------------------------------------------------------------------

#  list of all tasks
tasks = list(task_cor, task_all, task_esta, task_rest, task_all2)
print(tasks)

## ------------------------------------------------------------------------------------------
# b) creating the learning algorithms (using an inner function)

cat("@ Creating ML learners\n")
learners     = createLearnersMLR()
ovb.learners = createOverBaggingLearners(learners = learners)
learners = c(learners, ovb.learners)
print(learners)

## ------------------------------------------------------------------------------------------
# c) choosing evaluation measures
# BAC  : Balanced Accuracy per Class
# F1   : FScore
# GMean: Geometrical Mean

measures = list(mlr::bac, f1_multiclass_measure, gmean_multiclass_measure)
print(measures)

## ------------------------------------------------------------------------------------------
# d) Define a resampling 

# for debug purposes
# cv = mlr::makeResampleDesc(method = "RepCV", folds = 10, rep = 3, stratify = TRUE)
cv = mlr::makeResampleDesc(method = "RepCV", folds = 10, rep = 10, stratify = TRUE)
print(cv)

## ------------------------------------------------------------------------------------------
# e) execute experiments (tasks, algorithms) -> benchmark

cat("@ Running experiment\n")
res = mlr::benchmark(learners = learners, tasks = tasks, resamplings = cv, 
    measures = measures, show.info = TRUE, keep.pred = TRUE, models = FALSE)
print(res)

# complete results
perf.complete   = mlr::getBMRPerformances(bmr = res, as.df = TRUE)
perf.aggregated = mlr::getBMRAggrPerformances(bmr = res, as.df = TRUE)

cat("@ Saving results\n")
# use load() to read it again
save(res, file = "results/mlr_results_complete.RData")
write.csv(perf.complete, file = "results/mlr_performances_complete.csv")
write.csv(perf.aggregated, file = "results/mlr_performances_aggregated.csv")

cat("Done !!! \n")
##-------------------------------------------------------------------------------------------
##-------------------------------------------------------------------------------------------
