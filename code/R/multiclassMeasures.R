# -----------------------------------------------
# -----------------------------------------------

# https://machinelearningmastery.com/tour-of-evaluation-metrics-for-imbalanced-classification/

# -----------------------------------------------
# Multiclass F-measure using caret package
# -----------------------------------------------

f1_fun = function(task, model, pred, feat, extra.args) {
  obj = caret::confusionMatrix(
    pred$data$response, #pred
    pred$data$truth,    #actual
    mode   = "everything",
    positive = "1")

  f1 = obj$byClass[,7]
  
  #Assuming that F1 is zero when it's not possible compute it
  # Exemple:
  # caret::confusionMatrix(sample(x = iris$Species[1], replace = TRUE, size = length(iris$Species)), iris$Species)$byClass[,7]
  #   Class: setosa Class: versicolor  Class: virginica 
      # 0.5                NA                NA 
  f1[is.na(f1)] = 0
  f1 = mean(f1)
  return(f1)
}


f1_multiclass_measure = mlr::makeMeasure(
    id = "multiclass.fscore", 
    minimize = FALSE,
    properties = c("classif.multi", "classif", "req.pred", "req.truth"),
    fun = f1_fun,
    best = 1, worst = 0
)

# -----------------------------------------------
# Multiclass G-mean using metrica package
# https://cran.r-project.org/web/packages/metrica/vignettes/available_metrics_classification.html
# -----------------------------------------------

 # metrica::gmean(obs = iris$Species, pred = sample(x = iris$Species[1], replace = TRUE, size = length(iris$Species)), atom = TRUE)
gmean_fun = function(task, model, pred, feat, extra.args) {
  obj = metrica::gmean(
    obs = pred$data$truth, 
    pred = pred$data$response,
    atom = TRUE)

  obj = unlist(obj)
  obj[is.na(obj)] = 0
  gvalue = mean(obj)
  return(gvalue)
}

gmean_multiclass_measure = mlr::makeMeasure(
    id = "multiclass.gmean", 
    minimize = FALSE,
    properties = c("classif.multi", "classif", "req.pred", "req.truth"),
    fun = gmean_fun,
    best = 1, worst = 0
)
# -----------------------------------------------
# -----------------------------------------------