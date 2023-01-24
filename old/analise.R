#
#title: An R Markdown document converted from "analise.ipynb"
#
## ------------------------------------------------------------------------------------------
#instalando os pacotes
#@Rafael: da p instalar tudo de uma vez
# install.packages(c("mlr3", "ranger", "e1071", "mlr3learners", "mlr3viz", "caret", "kknn"))

# 
## ------------------------------------------------------------------------------------------
#carregando os pacotes
library(mlr3)
library(mlr3learners)
library(mlr3viz)
library(tidyverse)
library(future)
library(caret)
library(corrplot)
library(agricolae)
library(scales)
library(factoextra)
library(ggrepel)
#' 
## ------------------------------------------------------------------------------------------
# Pipeline
# 1 - ler os dados
#    - da p puxar do google drive via URL

# análise exploratória pode vir aqui (plots, stats dos dados)

# 2 - criar tarefas
#    - especificar coluna preditiva

# 3 - criar algoritmos
# 4 - executar algoritmos nas tarefas

#' 
#' ## 1. Leitura dos dados
#' 
## ------------------------------------------------------------------------------------------

set.seed(123)

# Matriz de caracteríticas

features = read_csv("features_prepro.csv")

#features = features  %>% 
  #separate(Name_file, c("dia", "oleo","dose"), "_")

# Vetor de rótulos

labels = read_csv("rotulos.csv")

labels = labels  %>% 
  mutate(rotulo = as.factor(recode(rotulo, c1 = 1, c2 = 1, c3 = 2, c4 = 3)))

#' 
#' ### 1.1 Visualizando as features
#' 
## ------------------------------------------------------------------------------------------
head(features)

#' 
#' ### 1.1 Frequência de imagens de cada rótulo
#' 
## ------------------------------------------------------------------------------------------
#dados desbalanceados
table(labels$rotulo) 

#' 
## ------------------------------------------------------------------------------------------
# @Rafael: podemos visualizar a quantidade de classes por um gráfico de barras
# A ideia é criar então um data frame com duas colunas:
#  primeira coluna é a classe (1, 2, 3, 4)
#  segunda coluna é a frequencia de cada uma, calculada via table()
df = data.frame(class = factor(c("Inicial","Intermediário","Avançado"),levels=c("Inicial","Intermediário","Avançado")), freq = as.numeric(table(labels$rotulo)))
# head(df)

df$class= c("Initial","Intermediate","Advanced")
df$class = factor(df$class,levels=c("Initial","Intermediate","Advanced"))

# @Rafael: aqui o gráfico de barras, usando ggplot2
g = ggplot(data = df, mapping = aes(x = class, y = freq, fill = class, colour = class))
g = g + geom_bar(position = 'dodge', stat='identity') + 
  geom_text(aes(label = freq), position=position_dodge(width=0.9), vjust=-0.25) + 
  labs(x = "Class", y = "Frequency of examples") + theme_bw() + theme(legend.position="none") 

png("res_rótulos.png", units="px", width=1408, height=1056, res=300, pointsize = 12)
g
dev.off()

#' 
#' ---
#' 
#' ## 2. Pré-processamento dos dados.
#' 
#' ### 2.1 Removendo os Nas: 
#' 
## ------------------------------------------------------------------------------------------
# @Edgar: removendo todas as colunas que só possuem valores NAs 
tmp <- features[ , colSums(is.na(features)) < nrow(features)]
# @Edgar: removendo todas as linhas com NAs 
features <- tmp[complete.cases(tmp), ] 
dim(features)

#' 
#' ### 2.2 Removendo features constantes (variância = 0):
#' 
## ------------------------------------------------------------------------------------------
# @Rafael: adicionei um segundo parâmetro, assim temos mais liberdade para 
# avaliar até mesmo features quase totalmente invariaveis
rm0var = function(df, threshold = 0){
  df = df[, sapply(df, function(x) {var(x)}) > threshold]
  return(df)
}

#' 
## ------------------------------------------------------------------------------------------
cat("Removendo features constantes ...\n")
cat("- Antes:  ", ncol(features)-1, "features \n")

X = rm0var(features[,-1], threshold = 0)
X = cbind(features[,1],X)
#dim(X)[2]

cat("- Depois: ", ncol(X)-1, "features \n")

# @Rafael: para se pensar depois ... porque acontece isso, 
#   se os atributos variam muito pouco mesmo
# Com threshold = 0     -> restam 94 features
# Com threshold = 0.001 -> restam 59 features
# Com threshold = 0.005 -> restam 48 features
# Com threshold = 0.01  -> restam 45 features
# Com threshold = 0.05  -> restam 37 features

#' 
#' 
#' ### 2.2 Criando os 4 datasets:
#' 
## ------------------------------------------------------------------------------------------
#dataset 1 (só features de cores)
X_cor = X %>%  select(Name_file, starts_with("cor")| starts_with("std_")|starts_with("mean")|starts_with("entropy"),-contains("hist"))
#dataset 2 (só features estatísticas)
X_esta = X %>%  select(Name_file, starts_with("skew_hist")| starts_with("kurt_hist") | starts_with("std_hist"))
#dataset 3 (features restantes)
X_rest = X %>%  select(Name_file, -names(X_cor) & -names(X_esta))
#dataset 4 (todas as features)
X_all = X

#' 
## ------------------------------------------------------------------------------------------
# @Rafael: visualizando o dataset com features de cores
head(X_cor)

#' 
## ------------------------------------------------------------------------------------------
# @Rafael: visualizando o dataset com features histogramas
head(X_esta)

#' 
## ------------------------------------------------------------------------------------------
# @Rafael: visualizando o dataset com features de filtros, etc
head(X_rest)

#' 
## ------------------------------------------------------------------------------------------
# @Rafael: visualizando o dataset completo
head(X_all)

#' 
## ------------------------------------------------------------------------------------------
# @Rafael: fazendo um plot para ver numero de features por dataset

data  = factor(c("Canais de cor", "Histogramas", "Demais características", "Completo","Agregado"),levels = c("Canais de cor", "Histogramas", "Demais características", "Completo","Agregado") )
value = c(ncol(X_cor)-1, ncol(X_esta)-1, ncol(X_rest)-1, ncol(X_all)-1, 94-49) 
df = data.frame(data = data, value = value)
head(df)

# # @Rafael: aqui o gráfico de barras, usando ggplot2
g = ggplot(data = df, mapping = aes(x = data, y = value, fill = data, colour = data))
g = g + geom_bar(position = 'dodge', stat='identity') + 
  geom_text(aes(label = value), position=position_dodge(width=0.9), vjust=-0.25) + 
  labs(x = "Conjuntos de dados", y = "Número de características") + theme_bw() + theme(legend.position="none")

png("res_data_sets_rest.png", units="px", width=1830, height=1373, res=300, pointsize = 12)
g
dev.off()


#' 
#' ### 2.3 Remoção das features muito correlacionadas 
#' 
#' Para cada dataset quatro critérios de corte (> 0.8, 0.85, 0.9, 0.95): 
#' 
#' * Características Cores:
#' 
## ------------------------------------------------------------------------------------------
tmp_cor = cor(X_cor[,-1])

hc80_cor = findCorrelation(tmp_cor, cutoff=0.8,  names = T, exact = TRUE) 
hc85_cor = findCorrelation(tmp_cor, cutoff=0.85, names = T, exact = TRUE) 
hc90_cor = findCorrelation(tmp_cor, cutoff=0.9,  names = T, exact = TRUE) 
hc95_cor = findCorrelation(tmp_cor, cutoff=0.95, names = T, exact = TRUE) 

n_out_cor = lengths(list(hc80_cor,hc85_cor,hc90_cor,hc95_cor))
graf_cor = data.frame('n_out' = n_out_cor, 'cutoff' = factor(c(0.8,0.85,0.9,0.95)))
head(graf_cor)

#' 
## ------------------------------------------------------------------------------------------
g1 = ggplot(data=graf_cor, aes(x=cutoff, y=n_out_cor, group=1)) 
g1 = g1 + geom_line()+ geom_point() + theme_bw()
g1 = g1 + geom_label(label = n_out_cor, nudge_x = 0.15) + ylim(c(0,36))
g1 

#' 
## ------------------------------------------------------------------------------------------
#Optei por usar como critério 0.85 de correlação como critério de corte.
X_cor = X_cor[, ! names(X_cor) %in% c(hc85_cor)]
head(X_cor)

#' 
#' * Características Estatísticas:
#' 
## ------------------------------------------------------------------------------------------
tmp_esta = cor(X_esta[,-1])

hc80_esta = findCorrelation(tmp_esta, cutoff=0.8,  names = T, exact = TRUE) 
hc85_esta = findCorrelation(tmp_esta, cutoff=0.85, names = T, exact = TRUE) 
hc90_esta = findCorrelation(tmp_esta, cutoff=0.9,  names = T, exact = TRUE) 
hc95_esta = findCorrelation(tmp_esta, cutoff=0.95, names = T, exact = TRUE) 

n_out_esta = lengths(list(hc80_esta,hc85_esta,hc90_esta,hc95_esta))
graf_esta = data.frame('n_out' = n_out_esta, 'cutoff' = factor(c(0.8,0.85,0.9,0.95)))
head(graf_esta)

#' 
## ------------------------------------------------------------------------------------------
g2 = ggplot(data=graf_esta, aes(x=cutoff, y=n_out_esta, group=1)) 
g2 = g2 + geom_line()+ geom_point() + theme_bw()
g2 = g2 + geom_label(label = n_out_esta, nudge_x = 0.15) + ylim(c(0,21))
g2 

#' 
## ------------------------------------------------------------------------------------------
#Optei por usar como critério 0.85 de correlação como critério de corte.
X_esta = X_esta[, ! names(X_esta) %in% c(hc85_esta)]
head(X_esta)

#' 
#' * Características restantes:
#' 
## ------------------------------------------------------------------------------------------
tmp_rest = cor(X_rest[,-1])

hc80_rest = findCorrelation(tmp_rest, cutoff=0.8, names = T, exact = TRUE) 
hc85_rest= findCorrelation(tmp_rest, cutoff=0.85, names = T, exact = TRUE) 
hc90_rest= findCorrelation(tmp_rest, cutoff=0.9, names = T, exact = TRUE) 
hc95_rest= findCorrelation(tmp_rest, cutoff=0.95, names = T, exact = TRUE) 

n_out_rest = lengths(list(hc80_rest,hc85_rest,hc90_rest,hc95_rest))
graf_rest = data.frame('n_out' = n_out_rest, 'cutoff' = factor(c(0.8,0.85,0.9,0.95)))
head(graf_rest)

#' 
## ------------------------------------------------------------------------------------------
g3 = ggplot(data=graf_rest, aes(x=cutoff, y=n_out_rest, group=1))
g3 = g3 + geom_line() + geom_point() + theme_bw()
g3 = g3 + geom_label(label = n_out_rest, nudge_x = 0.15) + ylim(c(0,37))
g3 

#' 
## ------------------------------------------------------------------------------------------
#Optei por usar como critério 0.85 de correlação como critério de corte. 
X_rest = X_rest[, ! names(X_rest) %in% c(hc85_rest)]
head(X_rest)
 

#' 
#' * Todas as características:
#' 
## ------------------------------------------------------------------------------------------
tmp = cor(X_all[,-1])

hc80 = findCorrelation(tmp, cutoff=0.8,  names = T, exact = TRUE) 
hc85 = findCorrelation(tmp, cutoff=0.85, names = T, exact = TRUE) 
hc90 = findCorrelation(tmp, cutoff=0.9,  names = T, exact = TRUE) 
hc95 = findCorrelation(tmp, cutoff=0.95, names = T, exact = TRUE) 
n_out = lengths(list(hc80,hc85,hc90,hc95))
graf_all = data.frame('n_out' = n_out, 'cutoff' = factor(c(0.8,0.85,0.9,0.95)))
head(graf_all)

#' 
## ------------------------------------------------------------------------------------------
g4 = ggplot(data=graf_all, aes(x=cutoff, y=n_out, group=1))
g4 = g4 + geom_line() + geom_point() 
g4 = g4 + geom_label(label = n_out, nudge_x = 0.15) + ylim(c(0,94)) + theme_bw()
g4

#' 
## ------------------------------------------------------------------------------------------
# Optei por manter o critério 0.85 de correlação como critério de corte.
X_all = X_all[, ! names(X_all) %in% c(hc85)]
head(X_all)
#Ao final restaram 39 features. SEIS à menos que no processo anterior 45)

#' 
## ------------------------------------------------------------------------------------------
#Checando se existem muitas diferenças

geral = names(X_all[,-1]);length(geral)
agregado = c(names(X_cor[,-1]),names(X_esta[,-1]),names(X_rest[,-1]));length(agregado)

!(geral%in%agregado)

#' 
## ------------------------------------------------------------------------------------------
# Checando as diferenças
geral[!(geral%in%agregado)] # quais do geral não estão presentes no agregado
agregado[!(agregado%in%geral)] # quais do agregado não estão presentes no geral

#' 
## ------------------------------------------------------------------------------------------
# @Rafael: tentando fazer um plot agregado com as informações de Gg1, g2, g3, g4
graf_cor$set  = "Color channels"
graf_esta$set = "Histograms"
graf_rest$set = "Other features" 
graf_all$set  = "Complete"

# merge nos 3 datasets 
full.df = rbind( graf_cor, graf_esta, graf_rest, graf_all)
full.df

#' 
## ------------------------------------------------------------------------------------------
# @Rafael: Plotar a informação conjunta, com 4 curvas
library(ggrepel)
g5 = ggplot(data=full.df, aes(x=cutoff, y=n_out, group = set, colour=set, linetype=set, shape=set, label=n_out))
g5 = g5 + geom_line() + geom_point()  
g5 = g5 + geom_label_repel( nudge_x = 0.1, vjust=+0.25, show.legend = F) + 
  labs(x = "Cut-off criteria (correlation)", 
       y = "Number of features removed", 
       colour='Datasets', 
       linetype = 'Datasets',
       shape = 'Datasets') + 
  theme(legend.title = element_text(size=10), #change legend title font size
        legend.text = element_text(size=10)) 

png("cor_data_sets.png", units="in", width=7.5, height=4.5, res=300, pointsize = 10)
g5
dev.off()


#' 
## ------------------------------------------------------------------------------------------
#@ Rafael: matriz de correlação do dataset completo (All)
# -1 em azul, +1 em vermelho, 0 - branco
# omite a triangular superior ou inferior
# omite a diaginal
# o que sobrar analisar

#' 
## ------------------------------------------------------------------------------------------
# dataset total, unindo as features sem correlação
X_all2 = cbind(X_cor, X_esta[,-1], X_rest[,-1]) 


#' 
## ------------------------------------------------------------------------------------------

corrplot(cor(X_all2[,-1]), method = "number", type= "lower", diag = FALSE, order = 'hclust', tl.col = 'black', 
         cl.ratio = 0.1, tl.srt = 45)
corrplot(cor(X_all2[,-1]), method = "circle", type= "lower", diag = FALSE, order = 'hclust')

png("cor_all.png", units="in", width=8, height=6.956522, res=300, pointsize = 10)
corrplot(cor(X_all[,-1]), method = "circle", type= "lower", diag = FALSE, order = 'hclust', tl.col = 'black', 
         cl.ratio = 0.1, tl.srt = 45)
dev.off()

png("cor_all2.png", units="in", width=8, height=6.956522, res=300, pointsize = 10)
corrplot(cor(X_all2[,-1]), method = "circle", type= "lower", diag = FALSE, order = 'hclust', tl.col = 'black', 
         cl.ratio = 0.1, tl.srt = 45)
dev.off()


#' 
#' ### 2.4 Criando os datasets para treinamento
#' 
## ------------------------------------------------------------------------------------------
df_all <- labels %>% inner_join(X_all,  by = "Name_file") %>% select(-one_of('Name_file'))
df_all2 <- labels %>% inner_join(X_all2,  by = "Name_file") %>% select(-one_of('Name_file'))
df_cor <- labels %>% inner_join(X_cor,  by = "Name_file") %>% select(-one_of('Name_file'))
df_esta <- labels %>% inner_join(X_esta,  by = "Name_file") %>% select(-one_of('Name_file'))
df_rest <- labels %>% inner_join(X_rest,  by = "Name_file") %>% select(-one_of('Name_file'))

## Normalizando os dados
library(scales)
df_all_resc = df_all %>% mutate_each_(list(~rescale(., to = c(-1, 1)) %>% as.vector),
                                  vars = names(df_all)[-1])
df_all2_resc = df_all2 %>% mutate_each_(list(~rescale(., to = c(-1, 1)) %>% as.vector),
                                      vars = names(df_all2)[-1])
df_cor_resc = df_cor %>% mutate_each_(list(~rescale(., to = c(-1, 1)) %>% as.vector),
                                      vars = names(df_cor)[-1])
df_esta_resc = df_esta %>% mutate_each_(list(~rescale(., to = c(-1, 1)) %>% as.vector),
                                      vars = names(df_esta)[-1])
df_rest_resc = df_rest %>% mutate_each_(list(~rescale(., to = c(-1, 1)) %>% as.vector),
                                        vars = names(df_rest)[-1])
## ------------------------------------------------------------------------------------------
#' 
#' ---
#' ## 3. Treinamento
#' 
## ------------------------------------------------------------------------------------------
# 2) criar as tarefas (classificação)
# especificar qual é a coluna target
task_all  = TaskClassif$new(id = "Completo",  backend = df_all_resc,  target = "rotulo")
task_all$col_roles$stratum = task_all$target_names
task_cor  = TaskClassif$new(id = "Canais de cor",  backend = df_cor_resc,  target = "rotulo")
task_cor$col_roles$stratum = task_cor$target_names
task_esta = TaskClassif$new(id = "Histogramas", backend = df_esta_resc, target = "rotulo")
task_esta$col_roles$stratum = task_esta$target_names
task_rest = TaskClassif$new(id = "Demais características", backend = df_rest_resc, target = "rotulo")
task_rest$col_roles$stratum = task_rest$target_names
task_all2 = TaskClassif$new(id = "Agregado", backend = df_all2_resc, target = "rotulo")
task_all2$col_roles$stratum = task_all2$target_names

#' 
## ------------------------------------------------------------------------------------------
# tasks <- list(task_all)
tasks <- list(task_cor, task_all, task_esta, task_rest, task_all2)
tasks

#' 
## ------------------------------------------------------------------------------------------
# 3) instanciar os algoritmos

# @Rafael: adicionando diferentes algoritmos com diferentes vieses
clr_bas_1 = lrn("classif.featureless", method="mode", id = "Majoritária")  
clr_bas_2 = lrn("classif.featureless", method="sample", id = "Aleatória")  
clf_dt  = lrn("classif.rpart", id = "Árvore de decisão") 
clf_knn = lrn("classif.kknn", id = "K-nn")
clf_mn  = lrn("classif.multinom", id = "Multinomial")   
clf_rf  = lrn("classif.ranger", id = "Random Forest")         
clf_svm = lrn("classif.svm", id = "SVM")            
clf_mlp = lrn("classif.nnet", id = "MLP")          
clf_nb  = lrn("classif.naive_bayes", id = "Naive Bayes")    

#@Edgar: usando todos algoritmos
learners = list(clr_bas_1, clr_bas_2, clf_dt, clf_knn, clf_mn, clf_nb, clf_rf, clf_svm, clf_mlp)
print(learners)

#' 
## ------------------------------------------------------------------------------------------
# 4) Setup experimental
#  - escolher medidas de desempenho (classificação)
# @Rafael: medida que leve em consideração desbalanceamento dos dados
# BACC = Balanced Accuracy per Class 
measure = msr("classif.bacc")

#' 
## ------------------------------------------------------------------------------------------
#  - definir um resampling (holdout, CV, RepCV)
# @Rafael: garantir a amostragem estratificada, pq o dataset é desbalanceado

cv = rsmp("repeated_cv", folds = 10, repeats = 10)
# cv = rsmp("repeated_cv", folds = 10, repeats = 10)
print(cv)

#' 
## ------------------------------------------------------------------------------------------
# - executar run (tasks, algorithms) -> benchmark
design = benchmark_grid(tasks = tasks, learners = learners, resamplings = cv)
print(design)

#' 
## ------------------------------------------------------------------------------------------
# future::plan("multicore")
set.seed(123)
res = benchmark(design = design)

#' 
## ------------------------------------------------------------------------------------------
print(res)

#' 
## ------------------------------------------------------------------------------------------
#'pegar as performances finais
results = res$score(measure)
head(results)

#' 
## ------------------------------------------------------------------------------------------
#@Rafael: resultados agregados por tarefa x algoritmo
resagg = res$aggregate(measure)
resagg

#' 
## ------------------------------------------------------------------------------------------
# @Rafael: ordenar os resultados pela performance
resagg[order(resagg$classif.bacc, decreasing = TRUE),]

#' 
## ------------------------------------------------------------------------------------------
#plotar e visualizar resultados do benchmark
#autoplot(object = res) + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

#' 
## ------------------------------------------------------------------------------------------
# https://mlr3viz.mlr-org.com/reference/autoplot.BenchmarkResult.html

# Só faz boxplot e roc curve
# autoplot(object, type = "boxplot", measure = NULL, ...)
# plotar e visualizar resultados do benchmark
# autoplot(object = res) + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

#' 
## ------------------------------------------------------------------------------------------
# @Rafael: Customizando a saida (plots proprios)
# results = res$score(measure)
colnames(results) = c("unhash", "nr", "task", "task_id", "learner", "learner_id", 
 "resampling", "resampling_id", "iteration", "prediction", "baac")
head(results)

#' 
## ------------------------------------------------------------------------------------------
# @Rafael: customizando a visualização
# task_id.labs <- c("df_all_resc", "df_all2_resc", "df_cor_resc", "df_esta_resc", "df_rest_resc")
# names(task_id.labs) <- c("Complete", "Agregated", "Color channels", "Histogram", "Other features")
# 
# task_id.labs <- c(
#   `Completo` = "Complete",
#   `Agregado` = "Agregated",
#   `Canais de Cor` = "Color channels",
#   `Histograma` = "Histogram",
#   `Demais características` = "Other features"
#   
# )

results = results %>% mutate(task_id = recode(task_id, `Completo` = "Complete",
                                              `Agregado` = "Agregated",
                                              `Canais de cor` = "Color channels",
                                              `Histogramas` = "Histograms",
                                              `Demais características` = "Other features"),
                             learner_id = recode(learner_id, `Majoritária` = "Majority",
                                                 `Aleatória` = "Random",
                                                 `Árvore de decisão` = "Decision tree",
                                                 `K-nn` = "k-NN"
                                                 ))



gf = ggplot(data = results, mapping = aes(x = learner_id, y = baac, group = learner_id))
gf = gf + geom_boxplot() + facet_grid(.~task_id, labeller = labeller(task_id.labs))
# ampliando fonte do eixo x e y
gf = gf + theme(axis.text.x = element_text(angle = 45, hjust = 1))
gf = gf + theme(axis.text = element_text(size=14), axis.title=element_text(size=14,face="bold"))
gf = gf + labs(x="Algorithms", y="Balanced accuracy") + theme(strip.text.x = element_text(size = 14)) +
geom_hline(yintercept=0.8, linetype='dashed', color='red')
# ampliando a janela para visualizar
#options(repr.plot.width=18, repr.plot.height=6)
#gf 

png("res_treinamento.png", units="in", width=12.8, height=7.2, res=300, pointsize = 20)
gf
dev.off()

#' 
## ------------------------------------------------------------------------------------------
# head(resagg)
ids = which(resagg$classif.bacc > 0.8)
bests = resagg[ids, ]
bests = bests[order(bests$classif.bac, decreasing = TRUE),]
bests

#' 
## ------------------------------------------------------------------------------------------
# @Rafael: 
# Identificar o melhor modelo: eu ordenei os resultados em ordem decrescente de acordo com 
# BACC. O melhor resultado foi o SVM  no dataset df_cor.
# Se você perceber, a primeira coluna mostra o id do resampling dessa combinação de 
# task + learner. Guardar essa info então: nr = 8!

# Só para desencargo de consciencia, verificar se é o que precisamos mesmo
# na posicao 4, 5 e 8

print(resagg$learner_id[[5]])
print(resagg$task_id[[5]])
print(resagg$learner_id[[4]])
print(resagg$task_id[[4]])
print(resagg$learner_id[[8]])
print(resagg$task_id[[8]])

#' 
## ------------------------------------------------------------------------------------------
# @Rafael: usamos esse id para pegar as predições:
knn_rr  = resagg$resample_result[[4]] # pega a posicao 4 da lista das predicoes
mat_knn = data.table::as.data.table(knn_rr$prediction()) 
head(mat_knn)
#computando as predições majoritárias
mat_knn_res=mat_knn[,list(kNN=as.numeric(names(which.max(table(response))))),by=row_ids]
mat_knn_truth=mat_knn[,list(Reference=as.numeric(names(which.max(table(truth))))),by=row_ids]
mat_knn_majority = merge(mat_knn_res, mat_knn_truth)


multi_rr  = resagg$resample_result[[5]] # pega a posicao 5 da lista das predicoes
mat_multi = data.table::as.data.table(multi_rr$prediction()) 
head(mat_multi)
#computando as predições majoritárias
mat_multi_res=mat_multi[,list(Multinomial=as.numeric(names(which.max(table(response))))),by=row_ids]
mat_multi_truth=mat_multi[,list(Reference=as.numeric(names(which.max(table(truth))))),by=row_ids]
mat_multi_majority = merge(mat_multi_res, mat_multi_truth)


svm_rr  = resagg$resample_result[[8]] # pega a posicao 8 da lista das predicoes
mat_svm = data.table::as.data.table(svm_rr$prediction()) 
head(mat_svm)
#computando as predições majoritárias
mat_svm_res=mat_svm[,list(SVM=as.numeric(names(which.max(table(response))))),by=row_ids]
mat_svm_truth=mat_svm[,list(Reference=as.numeric(names(which.max(table(truth))))),by=row_ids]
mat_svm_majority = merge(mat_svm_res, mat_svm_truth)

tmp = mat_multi_majority[mat_knn_majority[mat_svm_majority]]
tmp2 = tmp[,-c(5,7)]

modefunc <- function(x){
  tabresult <- tabulate(x)
  themode <- which(tabresult == max(tabresult))
  if(sum(tabresult == max(tabresult))>1) themode <- max(tabresult)
  return(themode)
}


tmp2$Voting = apply(tmp2[,c(2,4,5)], 1, modefunc)
row = tmp2[order(tmp2$Reference), row_ids]

predicoes =tmp2 %>%gather(key=Algoritmo, value = Classe, -row_ids) %>%
  mutate(Classe=as.factor(Classe),row_ids=factor(row_ids,levels=row),Algoritmo=factor(Algoritmo,levels=c("Reference","Voting", "Multinomial","kNN","SVM")))


g_pred = ggplot(predicoes, aes(row_ids, Algoritmo, fill= Classe)) + 
  geom_tile() +
  theme(axis.text.y=element_blank()) + coord_flip() +
  labs(x="Images",y="Algorithms", fill = 'Class') +
  scale_fill_manual(values = c("black", "red", "gray")) +
  theme(axis.text = element_text(size=14), axis.title=element_text(size=12))

png("heat_predi.png", units="in", width=7, height=9.3, res=300, pointsize = 20)
g_pred
dev.off()

# Acurácia voting

conf_voting=table(tmp2[,c(6,3)])
conf_voting_stats = confusionMatrix(conf_voting)
conf_voting_stats
mean(c(0.9319,0.8990,0.8990))
#0.9099667
## Seleção de imagens problemáticas

tmp3 =  labels %>% inner_join(X_all,  by = "Name_file")
tmp3 = tmp3[,c(1,2)]

## classe 1 sendo classificada por todos como sendo 2 

identi = function(x, y, obs,predito)
{
  idx = x[x$Verdade==obs&x$Multinomial==predito&x$kNN==predito&x$SVM==predito,row_ids]
return(y[idx,])
}

identi(tmp2,tmp3,1,2)

## classe 1 sendo classificada por todos como sendo 3 

identi(tmp2,tmp3,1,3)

## classe 2 sendo classificada por todos como sendo 1 

identi(tmp2,tmp3,2,1)

## classe 2 sendo classificada por todos como sendo 3 

identi(tmp2,tmp3,2,3)

## classe 3 sendo classificada por todos como sendo 1 

identi(tmp2,tmp3,3,1)

## classe 3 sendo classificada por todos como sendo 2 

identi(tmp2,tmp3,3,2)

#' 
## ------------------------------------------------------------------------------------------
# @Rafael: extraindo a matriz de confusao das predições
#  fazer gráficos

#knn
conf_knn=table(mat_knn_majority[,c(2,3)])
#conf_knn_stats = confusionMatrix(conf_knn)
plt_knn = data.frame(conf_knn)

#plt_knn$response <- factor(plt_knn$response, levels=rev(levels(plt_knn$response)))

g_knn = ggplot(plt_knn, aes(kNN,Reference)) +
  geom_tile(colour = "grey50", fill="#C9FADB") + geom_text(aes(label=Freq),size=6) +
  labs(x = "Prediction",y = "Reference") +
  scale_y_discrete(labels=c("Initial","Intermediate","Advanced")) +
  scale_x_discrete(labels=c("Initial","Intermediate","Advanced")) + 
  theme(legend.position="none", axis.text = element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

png("conf_knn.png", units="in", width=6, height=4, res=300, pointsize = 20)
g_knn
dev.off()

#Multinomial
conf_multi=table(mat_multi_majority[,c(2,3)])
plt_multi = data.frame(conf_multi)

#plt_multi$response <- factor(plt_multi$response, levels=rev(levels(plt_multi$response)))

g_multi = ggplot(plt_multi, aes(Multinomial,Reference, fill= Freq)) +
  geom_tile(colour = "grey50", fill="#C9FADB") + geom_text(aes(label=Freq),size=6) +
  labs(x = "Prediction",y = "Reference") +
  scale_y_discrete(labels=c("Initial","Intermediate","Advanced")) +
  scale_x_discrete(labels=c("Initial","Intermediate","Advanced")) + 
  theme(legend.position="none", axis.text = element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

png("conf_multi.png", units="in", width=6, height=4, res=300, pointsize = 20)
g_multi
dev.off()

#SVM
conf_svm=table(mat_svm_majority[,c(2,3)])
plt_svm = data.frame(conf_svm)


#plt_multi$response <- factor(plt_multi$response, levels=rev(levels(plt_multi$response)))

g_svm = ggplot(plt_svm, aes(SVM,Reference, fill= Freq)) +
  geom_tile(colour = "grey50", fill="#C9FADB") + geom_text(aes(label=Freq),size=6) +
  labs(x = "Prediction",y = "Reference") +
  scale_y_discrete(labels=c("Initial","Intermediate","Advanced")) +
  scale_x_discrete(labels=c("Initial","Intermediate","Advanced")) +  
  theme(legend.position="none", axis.text = element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

png("conf_svm.png", units="in", width=6, height=4, res=300, pointsize = 20)
g_svm
dev.off()

#table(mat_knn_majority[,c(2,3)])
#' 
## ------------------------------------------------------------------------------------------
# @Rafael: 2/3 dos erros sao predicoes erradas entre as classes 1 e 2
# 211 exemplos eram 2 e foram preditos como 1
# 212 exemplos eram 1 e foram preditos como 2

# 423 de 693, sobram 270 outros exemplos nas demais combinaçoes da matriz de confusão

# Perguntas:
# como sao os descritores para essas duas classes? mudam mto? Deve ter um overlap nos valores
# essas classes sao realmente mto distintas?
# faria sentido considerar o problema como 3 classes: (1+2), 3, 4?

#' 
## ------------------------------------------------------------------------------------------
# @Rafael: exemplos com predições erradas
#wrong_ids = which(mat[,2] != mat[,3])
#wrong_exs = mat[wrong_ids, ]

#quantidade de exemplos errados x totais
#cat("Qtde de exemplos errados: ", nrow(wrong_exs), "\n")
#cat("Qtde de exemplos totais: ", nrow(mat), "\n")
#print(1-(nrow(wrong_exs)/nrow(mat))) # acuracia simples

#' 
## ------------------------------------------------------------------------------------------
# Todos os exemplos (imagens) que foram classificados erradamente
#wrong_exs

#' 
#' 

#'PCA

pca_todos <- prcomp(df_all2[,-1], scale=T)
pca_cor <- prcomp(df_cor[,-1], scale=T)
library(factoextra)
fviz_pca_ind(pca_todos, label='rotulo', habillage=as.factor(df_all2$rotulo)) +
  labs(color=NULL) + 
  ggtitle("") + guides(shape='none') +
theme(text = element_text(size = 15),
      panel.background = element_blank(), 
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black"))

fviz_pca_ind(pca_cor, label='rotulo', habillage=as.factor(df_cor$rotulo)) +
  labs(color=NULL) + 
  ggtitle("") + guides(shape='none') +
  theme(text = element_text(size = 15),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))


## Análise estatística 

limite = bests[16,]$classif.bacc
top = resagg[resagg$classif.bacc>=limite,]$nr

analise = results %>% select(nr, task_id, learner_id, iteration, baac)  %>%
  filter(nr %in% top)
options(OutDec = '.')
### Teste de Kruskal wallis
library(agricolae)
(comparison<-with(analise, kruskal(baac, nr,p.adj="bonferroni", group=TRUE, main="teste de hipótese")))

# Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS = "true")
# if (!require("remotes")) install.packages("remotes")
# remotes::install_github("GegznaV/biostat")
# library(biostat)
# teste=pairwise.wilcox.test(analise$baac, analise$nr, p.adj = "bonf")
# teste2=make_cld(teste)
# teste3=merge(res,bests,by.x="group",by.y="nr")
# teste3[order(-teste3$classif.bacc),] mesmo resultado do kruskal wallis

library(xtable)
tabela = as.data.frame(bests[1:16,c('learner_id','task_id','classif.bacc')])
tabela = tabela %>% 
  mutate(learner_id = recode(learner_id, SVM = "SVM", 
                             `Random Forest` = "Random Forest", 
                             Multinomial = "Multinomial", 
                             `K-nn` = "k-NN", 
                             MLP = "MLP"),
         task_id = recode(task_id, Agregado = "Agregated",
                          Completo = "Complete",
                          `Canais de cor` = "Color channels"),
         letras = comparison$groups$groups) %>% 
  rename('Classificador' = learner_id, 
         'Conjunto de dados' = task_id, 
         'Acurácia balanceada' = classif.bacc)
options(OutDec = ",")
xtable::xtable(tabela,digits=5)

library(plyr)
dfl <- ddply(tabela, .(learner_id), summarize, y=length(learner_id))
dfl$learner_id <- factor(dfl$learner_id,
                            levels = dfl$learner_id[order(dfl$y, decreasing = TRUE)])
g_class= ggplot(data=dfl, aes(x=learner_id,y=y)) +
  geom_bar(stat="identity",fill="blue")+
  geom_text(aes(label=y), vjust=1.6, color="white", size=6)+
   labs(y='Frequency', x = 'Classifier') 
g_class = g_class + theme(axis.text.x = element_text(angle = 45, hjust = 1))
g_class = g_class + theme(axis.text = element_text(size=14), axis.title=element_text(size=14,face="bold"))

dfl2 <- ddply(tabela, .(`task_id`), summarize, y=length(`task_id`))
dfl2$`task_id` <- factor(dfl2$`task_id`,
                            levels = dfl2$`task_id`[order(dfl2$y, decreasing = TRUE)])
g_ds= ggplot(data=dfl2, aes(x=`task_id`,y=y)) +
    geom_bar(stat="identity",fill="red")+
    geom_text(aes(label=y), vjust=1.6, color="white", size=6)+
    labs(y='Frequency', x = 'Dataset')  
g_ds = g_ds + theme(axis.text.x = element_text(angle = 45, hjust = 1))
g_ds = g_ds + theme(axis.text = element_text(size=14), axis.title=element_text(size=14,face="bold"))

png("freq_class.png", units="in", width=6, height=4.5, res=300, pointsize = 20)
g_class
dev.off() 

png("freq_ds.png", units="in", width=6, height=4.5, res=300, pointsize = 20)
g_ds
dev.off() 
  
## Random Forest

task = as_task_classif(df_all2_resc, id = "Agregado", target = "rotulo")
clf_rf  = lrn("classif.ranger", id = "Random Forest", importance = "permutation")
clf_rf$train(task)
clf_rf$importance()

importance = as.data.table(clf_rf$importance(), keep.rownames = TRUE)
colnames(importance) = c("Feature", "Importance")
g_importance = ggplot(importance, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_col(width = 0.8,fill="lightblue",col="darkblue")  + labs(y="Importance", x = "Feature") + theme_bw() 

g_importance10 = ggplot(importance[1:10,], aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_col(width = 0.8,fill="lightblue",col="darkblue") + coord_flip() + labs(y="Importance", x = "Feature") + theme_bw() 
    
png("rf_importance.png", units="in", width=9, height=6, res=300, pointsize = 20)
g_importance
dev.off()




library(nnet)
library(caret)
rf=train(rotulo~.,
                  data=df_all2_resc, 
                  method="rf")


importance <- varImp(rf)
plot(importance)

## Multinomial
library(nnet)
library(caret)
multinomial=train(rotulo~.,
                data=df_all2_r esc, 
                method="multinom")


importance2 <- varImp(multinomial)

plot(importance2)

library(data.table)
importance2 = as.data.table(importance2, keep.rownames = TRUE)
colnames(importance) = c("Caracteristica", "Importancia")
ggplot(importance) 



# Árvore de decisão


library(rpart)
tr<-rpart(rotulo~.,
           data=df_all2_resc)
library (rpart.plot)
rpart.plot(tr,fallen.leaves=F, tweak=1,type=1,cex=0.35)

options(OutDec = ",")     
png("arvore2.png", units="in", width=12.8, height=7.2, res=300, pointsize = 20)
rpart.plot(tr,fallen.leaves=F, tweak=1.2,type=2, shadow.col="gray",extra=104, branch=0)
dev.off()


