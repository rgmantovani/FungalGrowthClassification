## ------------------------------------------------------------------------------------------------
## ------------------------------------------------------------------------------------------------

# ALGOS.MLR3 = c("rpart", "kknn", "multinom", "ranger", "svm", "nnet", "naive_bayes")

# If they are not installed, please, uncomment the following lines:
# install.packages(c("mlr3", "ranger", "e1071", "mlr3learners", "mlr3viz", "caret", "kknn","scales", 
   # "corrplot","factoextra","agricolae", "mlr", "metrica"))

# library(mlr3)
# library(mlr3learners)
# library(mlr3viz)

source("R/config.R")
source("R/createLearners.R")


## ------------------------------------------------------------------------------------------
## 1. Reading data
## ------------------------------------------------------------------------------------------

features = read_csv("data/dataset/features_prepro.csv")
labels   = read_csv("data/dataset/rotulos.csv")
labels   = labels %>% mutate(rotulo = as.factor(recode(rotulo, c1 = 1, c2 = 1, c3 = 2, c4 = 3)))

## ------------------------------------------------------------------------------------------
## Creating output dirs 
## ------------------------------------------------------------------------------------------

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
tmp = features[ , colSums(is.na(features)) < nrow(features)]
features = tmp[complete.cases(tmp), ] 
dim(features)

### 2.2 Removing constant features(variance = 0):
## ------------------------------------------------------------------------------------------

# Creating a function for this ...
rm0var = function(df, threshold = 0){
  df = df[, sapply(df, function(x) {var(x)}) > threshold]
  return(df)
}

## ------------------------------------------------------------------------------------------
cat("@ Removing constant features ...\n")
cat("- Before:  ", ncol(features)-1, "features \n")
X = rm0var(features[,-1], threshold = 0)
X = cbind(features[,1],X)
cat("- After: ", ncol(X)-1, "features \n")

## ------------------------------------------------------------------------------------------
## Creating datasets (4 different versions)
## ------------------------------------------------------------------------------------------

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


## Rescaling dataframes

df_all_resc  = df_all  %>% mutate_each_(list(~rescale(., to = c(-1, 1)) %>% as.vector),
                                      vars = names(df_all)[-1])
df_all2_resc = df_all2 %>% mutate_each_(list(~rescale(., to = c(-1, 1)) %>% as.vector),
                                        vars = names(df_all2)[-1])
df_cor_resc  = df_cor  %>% mutate_each_(list(~rescale(., to = c(-1, 1)) %>% as.vector),
                                      vars = names(df_cor)[-1])
df_esta_resc = df_esta %>% mutate_each_(list(~rescale(., to = c(-1, 1)) %>% as.vector),
                                        vars = names(df_esta)[-1])
df_rest_resc = df_rest %>% mutate_each_(list(~rescale(., to = c(-1, 1)) %>% as.vector),
                                        vars = names(df_rest)[-1])

## ------------------------------------------------------------------------------------------
## 3. ML training using mlr3
## ------------------------------------------------------------------------------------------

# a) creating learning tasks (classification)

task_all  = mlr3::TaskClassif$new(id = "Complete",  backend = df_all_resc,  target = "rotulo")
task_all$col_roles$stratum = task_all$target_names

task_cor  = mlr3::TaskClassif$new(id = "Color Channels",  backend = df_cor_resc,  target = "rotulo")
task_cor$col_roles$stratum = task_cor$target_names

task_esta = mlr3::TaskClassif$new(id = "Histograms", backend = df_esta_resc, target = "rotulo")
task_esta$col_roles$stratum = task_esta$target_names

task_rest = mlr3::TaskClassif$new(id = "Other Features", backend = df_rest_resc, target = "rotulo")
task_rest$col_roles$stratum = task_rest$target_names

task_all2 = mlr3::TaskClassif$new(id = "Aggregated", backend = df_all2_resc, target = "rotulo")
task_all2$col_roles$stratum = task_all2$target_names


## ------------------------------------------------------------------------------------------
#  list of all tasks
tasks = list(task_cor, task_all, task_esta, task_rest, task_all2)
print(tasks)

## ------------------------------------------------------------------------------------------
# b) creating the learning algorithms

# learners = createLearnersMLR3()

clr_bas_1 = mlr3::lrn("classif.featureless", method="mode", id = "Majority")  
clr_bas_2 = mlr3::lrn("classif.featureless", method="sample", id = "Random")  
clf_dt    = mlr3::lrn("classif.rpart", id = "Decision Tree") 
clf_knn   = mlr3::lrn("classif.kknn", id = "k-NN")
clf_mn    = mlr3::lrn("classif.multinom", id = "Multinomial")   
clf_rf    = mlr3::lrn("classif.ranger", id = "Random Forest")         
clf_svm   = mlr3::lrn("classif.svm", id = "SVM")            
clf_mlp   = mlr3::lrn("classif.nnet", id = "MLP")          
clf_nb    = mlr3::lrn("classif.naive_bayes", id = "Naive Bayes")    

# list of all learners
learners = list(clr_bas_1, clr_bas_2, clf_dt, clf_knn, clf_mn, clf_nb, clf_rf, clf_svm, clf_mlp)
print(learners)

#TODO: add imabalancement techniques fused with learners
# (smote, over, under), there will be 4 times the number of learners

# TODO: Add CNN using R (check mlr3)
# https://github.com/mlr-org/mlr3keras

## ------------------------------------------------------------------------------------------
# c) choosing evaluation measures
# BACC = Balanced Accuracy per Class
  
bacc.measure = mlr3::msr("classif.bacc")

# FBETA  = FScore
# fbeta(truth, response, positive, beta = 1, na_value = NaN, ...)
# It measures the effectiveness of retrieval with respect to a user who attaches beta times as much importance to recall as precision.
 # For beta = 1, this measure is called "F1" score.
# meas2 = mlr3::msr("classif.fbeta")
# measures = list(meas1, meas2)
# print(measures)

## ------------------------------------------------------------------------------------------
# d) Define a resampling 

cv = mlr3::rsmp("repeated_cv", folds = 10, repeats = 30) # change for 30
print(cv)

## ------------------------------------------------------------------------------------------
# e) execute experiments (tasks, algorithms) -> benchmark

design = mlr3::benchmark_grid(tasks = tasks, learners = learners, resamplings = cv) #definindo  
print(design)

# training algorithms
res = mlr3::benchmark(design = design) 


## ------------------------------------------------------------------------------------------
# f) Extracting results of all iterations

results.bacc = res$score(bacc.measure)
# use load() to read it again
# save(results.bacc, file = "results/bac_results_complete.RData")
readr::write_csv(results.bacc, file = "results/bac_results_complete.csv")

# TODO: problem with FBETA - it only works with binary datasets
# results.f1 = res$score(meas2)
# # head(results.f1)
# save(f1, file = "results/f1_results_complete.RData")
# readr::write_csv(results.f1, file = "results/f1_results_complete.csv")

## ------------------------------------------------------------------------------------------
#Agregando resultados por tarefa x algoritmo
agg.bacc = res$aggregate(bacc.measure)

#ordenando os resultados pela performance
agg.bacc = agg.bacc[order(agg.bacc$classif.bacc, decreasing = TRUE),]
# save(results.bacc, file = "results/bac_results_complete.RData")
readr::write_csv(agg.bacc, file = "results/bac_aggregated_results_sorted.csv")


# ##-------------------------------------------------------------------------------------------
# # Visulaização de resultados
# # Customizando a saida (plots proprios)
# results = res$score(measure)
results = results.bacc
colnames(results) = c("unhash", "nr", "task", "task_id", "learner", "learner_id", 
                      "resampling", "resampling_id", "iteration", "prediction", "baac")
head(results)

# #customizando a visualização

task_id.labs = c("df_all_resc", "df_all2_resc", "df_cor_resc", "df_esta_resc", "df_rest_resc")
names(task_id.labs) = c("Complete", "Aggregated", "Color Channels", "Histograms", "Other Features")

# # Graficando os resultados
gf = ggplot(data = results, mapping = aes(x = learner_id, y = baac, group = learner_id))
gf = gf + geom_boxplot() + facet_grid(.~task_id, labeller = labeller(task_id.labs))
gf = gf + theme(axis.text.x = element_text(angle = 45, hjust = 1))
gf = gf + labs(x="Algorithms", y="Balanced per Class Accuracy") 
gf = gf + geom_hline(yintercept=0.8, linetype="dashed", color = "red")
# gf
ggsave(gf, file = "plots/fig_boxplot_learning_algorithms.pdf", width = 9.41, height = 5.24)

# ##------------------------------------------------------------------------------------------
# ## Análise estatística 
# ##------------------------------------------------------------------------------------------

# ## Extraindo os melhores
# limite = bests[16,]$classif.bacc
# top = resagg[resagg$classif.bacc>=limite,]$nr

# ## criando o dataset para o teste
# analise = results %>% select(nr, task_id, learner_id, iteration, baac)  %>%
#   filter(nr %in% top)
# ### Teste de Kruskal wallis
# library(agricolae)
# (comparison<-with(analise, kruskal(baac, nr,p.adj="bonferroni", group=TRUE, main="teste de hipótese")))

# ##------------------------------------------------------------------------------------------
# ##------------------------------------------------------------------------------------------