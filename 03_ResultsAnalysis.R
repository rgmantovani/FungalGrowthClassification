##-------------------------------------------------------------------------------------------
##-------------------------------------------------------------------------------------------

cat(" @ Loading all required files:\n")
library(ggplot2)
library(reshape2)
library(dplyr)

## ------------------------------------------------------------------------------------------------
## ------------------------------------------------------------------------------------------------

R.files = list.files(path = "R", full.names = TRUE)
for(file in R.files) {
  print(file)
  source(file)
}

##-------------------------------------------------------------------------------------------
##-------------------------------------------------------------------------------------------

# use load() to read it again
cat(" @ Loading results \n")
load(file = "results/mlr_results_complete.RData", verbose = TRUE)

##-------------------------------------------------------------------------------------------
##-------------------------------------------------------------------------------------------

results = mlr::getBMRPerformances(res, as.df = TRUE)
colnames(results) = c("task", "algo", "iter", "bac", "fscore", "gmean")

ids =  which(grepl(x = results$algo, pattern = "overbagged"))
results$setup = "Original"
results$setup[ids] = "Balanced"

results$algo.name = results$algo
results$algo.name = gsub(x = results$algo.name, pattern = ".overbagged", replacement = "")

results$gmean = NULL

# ---------------
# Boxplot: bac x fscore results
# ---------------

cat(" @ Plot: Performances' Boxplot\n")

# Using algorithm without overbagging
ovb.ids = which(grepl(x = results$algo, pattern = "overbagged"))
df = results[-ovb.ids,]

# melt no data frame
df.melt = melt(df, id.vars = c(1,2,3,6,7))

# facet_grid with task~measure 
gf = ggplot(data = df.melt, mapping = aes(x = algo, y = value, group = algo))
gf = gf + geom_boxplot() + facet_grid(variable~task)
gf = gf + theme(axis.text.x = element_text(angle = 90, hjust = 1))
gf = gf + geom_hline(yintercept=0.8, linetype="dashed", color = "red")
# gf
ggsave(gf, file = "plots/fig_algorithms_performance.pdf", width = 8.8, height = 5.16)

# ---------------
# Lineplot: Original vs Overbagged algos
# ---------------

cat(" @ Plot: Lineplot - Original vs Overbagged Learners\n")

df.agg = mlr::getBMRAggrPerformances(bmr = res, as.df = TRUE)
colnames(df.agg) = c("task", "algo", "BAC", "FScore", "GMean")
 df.agg$GMean = NULL

ids =  which(grepl(x = df.agg$algo, pattern = "overbagged"))
df.agg$Setup = "Original"
df.agg$Setup[ids] = "Balanced"

df.agg$algo.name = df.agg$algo
df.agg$algo.name = gsub(x = df.agg$algo.name, pattern = ".overbagged", replacement = "")

# renaming factor levels (algorithms)
df.agg$algo.name = renameAlgoFactors(algos = df.agg$algo.name)

# getting standard deviation
bac.sd = getSdsFromRepetitions(results = results, measure = "bac") 
fsc.sd = getSdsFromRepetitions(results = results, measure = "fscore") 

# new order of factor levels
df.agg$algo.name = factor(df.agg$algo.name, levels = c("MLP", "SVM", "Multinomial", 
	"RF", "KNN", "NB", "DT", "Random", "Majority"))

df.agg.melted = melt(df.agg, id.vars = c(1, 2, 5, 6))

# Adding standard deviation to plot geom_ribbon curves
aux1 = dplyr::filter(df.agg.melted, variable == "BAC")
aux1 = merge(aux1, bac.sd, by = c("task", "algo"))
aux2 = dplyr::filter(df.agg.melted, variable == "FScore")
aux2 = merge(aux2, fsc.sd, by = c("task", "algo"))
df.agg.melted = rbind(aux1, aux2)


g2 = ggplot(df.agg.melted, aes(x = algo.name, y = value, group = Setup, 
	colour = Setup, linetype = Setup, shape = Setup, fill = Setup,  
	ymin = value-sd, ymax = value+sd))
g2 = g2 + geom_line() + geom_point() + theme_bw()
g2 = g2 + geom_ribbon(alpha = .25, colour = NA)

g2 = g2 + facet_grid(variable~task, scales = "free")
g2 = g2 + labs(x = "Algorithm", y = "Value")
g2 = g2 + theme(axis.text.x = element_text(angle = 90, hjust = 1))
g2 = g2 + scale_fill_manual(values = c("red", "black"))
g2 = g2 + scale_colour_manual(values = c("red", "black"))
# g2 
ggsave(g2, file = "plots/fig_original_vs_balanced.pdf", width = 9.66, height = 4.07)


# TODO: check wilcoxon pair test (balanced vs original)

# ---------------
# Heatmap: Rankings
# ---------------

# TODO: use this mlr::convertBMRToRankMatrix(res, measure = "bac.test")

cat(" @ Computing Rankings\n")

bac.rankings = getRankings(df.agg = df.agg, measure = "BAC")
bac.rankings$measure = "BAC"

fsc.rankings = getRankings(df.agg = df.agg, measure = "FScore")
fsc.rankings$measure = "FScore"

#complete ranking information
cmp.rankings = rbind(bac.rankings, fsc.rankings)
cmp.rankings$algo = renameAlgoFactors(algos = cmp.rankings$algo)
df.rk = melt(cmp.rankings, id.vars = c(6,7,8))

#ordering algos
tmp = merge(bac.rankings[,c(6,7)], fsc.rankings[,c(6,7)], by = "algo")
tmp$algo = renameAlgoFactors(algos = tmp$algo)
ids = order(colMeans(t(tmp[,-1])))

# tmp[ids,]
#                    algo avg.rk.x avg.rk.y
# 17                  SVM      2.4      1.4
# 12         MLP.Balanced      1.4      2.8
# 7           Multinomial      4.6      4.4
# 13                   RF      5.4      4.0
# 18         SVM.Balanced      5.8      6.0
# 8  Multinomial.Balanced      5.6      6.6
# 11                  MLP      6.0      6.8
# 14          RF.Balanced      7.2      7.2
# 5                   KNN      8.2      7.6
# 6          KNN.Balanced      9.6      9.8
# 16          DT.Balanced     11.0     11.4
# 15                   DT     12.4     11.4
# 9                    NB     12.2     12.6
# 10          NB.Balanced     13.2     13.0
# 3                Random     16.4     15.0
# 4       Random.Balanced     15.8     16.0
# 2     Majority.Balanced     16.9     17.3
# 1              Majority     16.9     17.7

df.rk$algo = factor(df.rk$algo, levels = tmp[ids,]$algo)

cat(" @ Plot: Heatmap - Algorithms' Ranking\n")

g3 = ggplot(df.rk, aes(x = algo, y = variable, fill = value))
g3 = g3 + geom_tile()+ facet_grid(measure~.)
g3 = g3 + theme(axis.text.x = element_text(angle = 90, hjust = 1))
g3 = g3 + labs(x = "Algorithm", y = "", fill = "Ranking")
g3 = g3 + scale_fill_gradient2(high = "red", mid = "white", low = "blue", midpoint = 9)
ggsave(g3, file = "plots/fig_average_ranking.pdf", width = 7.99, height = 4.39)

# ---------------
# Best models - matriz de confusão
# ---------------

# TOP-4 (SVM, MLP.balanced, Multinomial, RF) 

# 15     Aggregated       classif.nnet.overbagged 0.8708687 0.8694717 0.905806286
# 5      Aggregated                   classif.svm 0.8583093 0.8660905 0.892570721
# 12     Aggregated   classif.multinom.overbagged 0.8582311 0.8582335 0.896718862
# 40       Complete                classif.ranger 0.8533169 0.8600085 0.891231460

cat(" @ Obtaining Top3 Algorithms' Predictions \n")

preds = mlr::getBMRPredictions(bmr = res, 
	learner.ids = c("classif.svm", "classif.nnet.overbagged", "classif.multinom.overbagged", "classif.ranger"),
	task.ids = c("Aggregated", "Complete"))


# ---------------
# Plot: confusion matrices
# ---------------

cat(" @ Plot: Confusion Matrices\n")

svm.matrix = mlr::calculateConfusionMatrix(pred = preds$Aggregated$classif.svm)
mlp.matrix = mlr::calculateConfusionMatrix(pred = preds$Aggregated$classif.nnet.overbagged)
ml.matrix  = mlr::calculateConfusionMatrix(pred = preds$Aggregated$classif.multinom.overbagged)
rf.matrix  = mlr::calculateConfusionMatrix(pred = preds$Complete$classif.ranger)

lrns = c("svm", "mlp", "multinomial", "rf")
cm.matrices = list(svm.matrix, mlp.matrix, ml.matrix, rf.matrix)

for(i in 1:4) {
	
	cm = round((cm.matrices[[i]]$result)/10)
	cm = cm[1:3, 1:3]
	plt = melt(cm)
	plt$goodbad = "Bad"
	plt$goodbad[c(1, 5, 9)] = "Good"
	plt$predicted = factor(plt$predicted, levels = c(3, 2, 1))

	g4 = ggplot(data = plt, mapping = aes(x = true, y = predicted, fill = goodbad, alpha = value)) 
	g4 = g4 + geom_tile() + geom_text(aes(label = value), vjust = .5, fontface  = "bold", alpha = 1)
	g4 = g4 + scale_fill_manual(values = c(Good = "green", Bad = "red"))
	g4 = g4 + theme_bw() + guides(fill = "none", text = "none", alpha = "none")
	g4 = g4 + labs(x = "True", y = "Predicted")
	# g4 
	ggsave(g4, file = paste0("plots/fig_", lrns[i], "_confusion_matrix.pdf"), width = 3.19, height = 2.62)

}

# ---------------
# Plot: Voting composed by the top-3 algorithms
# ---------------

cat(" @ Computing Voting \n")

df1 = preds$Aggregated$classif.svm
df1 = aggregatePredictions(df = df1)

df2 = preds$Aggregated$classif.nnet.overbagged
df2 = aggregatePredictions(df = df2)

df3 = preds$Aggregated$classif.multinom.overbagged
df3 = aggregatePredictions(df = df3)


# Compute voting predictions
voting.df    = cbind(df1$response, df2$response, df3$response)
voting.preds = voting(voting.df = voting.df)

df4 = df3
df4$response = voting.preds

# generating an unique data frame for plot issues
m1 = merge(df1, df2, by = c("id", "truth", "set", "iter"))
m2 = merge(m1, df3, by = c("id", "truth", "set", "iter"))
m3 = merge(m2, df4, by = c("id", "truth", "set", "iter"))

colnames(m3)[5:8] = c("SVM", "MLP", "Multinomial", "Voting")
m3 = m3[order(m3$truth),]

# Melting Data frame before plotting
dff = melt(m3, id.vars = c(1,3,4))
dff$variable = dplyr::recode_factor(dff$variable, truth = "Reference")

dff$id = factor(as.character(dff$id))
dff$id = factor(dff$id, levels = unique(dff$id))

dff$variable = factor(dff$variable, levels = c("Reference", "Voting", "Multinomial", 
	"MLP", "SVM"))


cat(" @ Plot: Voting Predictions \n")

g5 = ggplot(dff, aes(y = as.factor(id), x = variable, fill = value)) # colour = value))
g5 = g5 + geom_tile() + theme_bw()
g5 = g5 + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())
g5 = g5 + labs(x = "Algorithms", y = "Images")
g5 = g5 + scale_colour_manual(values = c("black", "red", "grey50"))
g5 = g5 + scale_fill_manual(values = c("black", "red", "grey50"))
g5 = g5 + labs(fill = "Class")
# g5 
ggsave(g5, file = "plots/fig_voting_predictions.pdf", width = 7.45, height = 5.81)

# --------------------------
# Voting preformance
# --------------------------

print(" @ Voting Performance: ")

obj.voting = caret::confusionMatrix(
	as.factor(m3$Voting), #pred
  	m3$truth,    #actual
  	mode   = "everything", positive = "1")

  f1 = obj.voting$byClass[,7]
  f1[is.na(f1)] = 0
  f1 = mean(f1)

cat(" F-Measure:", f1, "\n") # [1] 0.9026198

bc = mlr::measureBAC(truth = m3$truth, response = as.factor(m3$Voting))
cat(" Balanced accuracy: ", bc, "\n") # [1] 0.9014316

# --------------------------
# Checking images missclassified
# --------------------------

sel.ids = which(m3$truth != m3$Voting)
sel.df = m3[sel.ids,]

# sel.df
#      id truth  set iter SVM MLP Multinomial Voting
# 194 273     1 test   10   2   2           1      2
# 195 274     1 test    2   2   2           2      2
# 196 275     1 test    2   1   2           2      2
# 199 278     1 test   10   2   2           2      2
# 238 312     1 test    1   2   2           2      2
# 277 348     1 test    4   1   2           2      2
# 307 375     1 test    9   2   2           2      2
# 313 380     1 test    4   2   2           2      2
# 319 386     1 test    5   2   2           2      2
# 322 389     1 test    8   2   1           2      2
# 327 393     1 test   10   1   2           2      2
# 439 494     1 test    2   2   2           1      2
# 511  76     1 test    7   1   3           3      3
# 132 217     2 test    9   2   1           1      1
# 133 218     2 test   10   1   1           1      1
# 138 222     2 test    6   1   1           3      1
# 140 224     2 test    4   2   3           3      3
# 214 291     2 test    8   2   1           1      1
# 245 319     2 test    2   1   1           1      1
# 310 378     2 test    9   3   3           3      3
# 325 391     2 test    7   1   1           1      1
# 331 397     2 test    5   1   1           1      1
# 350 413     2 test    2   2   3           3      3
# 353 416     2 test   10   1   3           3      3
# 358 420     2 test    3   2   3           3      3
# 403 461     2 test    8   2   3           3      3
# 437 492     2 test   10   3   3           3      3
# 450 503     2 test    1   1   1           1      1
# 452 505     2 test    6   1   1           1      1
# 247 320     3 test    2   2   2           2      2
# 248 321     3 test    5   1   1           1      1
# 250 323     3 test    4   3   2           2      2
# 271 342     3 test    9   1   1           3      1
# 272 343     3 test    9   2   3           2      2
# 341 405     3 test    2   2   2           2      2
# 344 408     3 test    6   2   2           2      2
# 347 410     3 test    8   2   2           2      2
# 388 448     3 test    5   1   3           2      1
# 391 450     3 test    1   1   3           1      1
# 406 464     3 test    1   1   3           2      1

# table(m3$truth, m3$Voting)
   
#       1   2   3
#   1 312  12   1
#   2   9  90   7
#   3   5   6  94


##------------------------------------------------------------------------------------------
#  Interpretability plots
##------------------------------------------------------------------------------------------

df_all2_resc = read.csv(file = "data/dataset/task_aggregated.csv", row.names = 1)

# --------------------------
## Decision Tree
# --------------------------

cat(" @ Plot: Decision Tree \n")

library(rpart)
library (rpart.plot)

tree = rpart(rotulo~., data=df_all2_resc[,-1], method = "class")
# rpart.plot(tree,fallen.leaves=F, tweak=1,type=1,cex=0.35)
options(OutDec = ",")     
png("plots/fig_decision_tree.png", units="in", width=12.8, height=7.2, res=300, pointsize = 20)
rpart.plot(tree,fallen.leaves=F, tweak=1.2,type=2, shadow.col="gray",extra=104, branch=0)
dev.off()

# --------------------------
## Random Forest
# --------------------------

cat(" @ Plot: Ranfom Forest (importance) \n")

mlrTask = mlr::makeClassifTask(df_all2_resc[,-1], id = "test", target = "rotulo")
lrn     = mlr::makeLearner("classif.ranger", importance = "permutation")
model   = mlr::train(task = mlrTask, learner = lrn)

trueModel = model$learner.model

importance = as.data.frame(trueModel$variable.importance)
rf.df = cbind(rownames(importance), importance)
rownames(rf.df) = NULL
colnames(rf.df) = c("Feature", "Importance")

g_importance = ggplot(rf.df, aes(x = reorder(Feature, Importance), y = Importance))
g_importance = g_importance  + geom_col(width = 0.8, fill="lightblue", col="darkblue")
g_importance = g_importance  + labs(y="Importance", x = "Feature") + coord_flip() + theme_bw() 
# g_importance # for debug
ggsave(g_importance, file = "plots/fig_randomForest.pdf", units = "in", width = 9, 
	height = 6, dpi = 300, pointsize = 20)

##------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------

cat("------------------------\n")
cat (" @Done :)\n")
cat("------------------------\n")
  
##------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------