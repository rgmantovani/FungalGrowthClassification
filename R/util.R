## ------------------------------------------------------------------------------------------------
## ------------------------------------------------------------------------------------------------

rm0var = function(df, threshold = 0){
  df = df[, sapply(df, function(x) {var(x)}) > threshold]
  return(df)
}

## ------------------------------------------------------------------------------------------------
## ------------------------------------------------------------------------------------------------

renameAlgoFactors = function(algos) {
  
  algos = dplyr::recode_factor(algos, 
      classif.kknn = "KNN",
      classif.multinom = "Multinomial",
      classif.naiveBayes = "NB",
      classif.nnet = "MLP",
      classif.ranger = "RF",
      classif.rpart = "DT",
      classif.svm = "SVM",
      classif.kknn.overbagged = "KNN.Balanced",
      classif.multinom.overbagged  = "Multinomial.Balanced",
      classif.naiveBayes.overbagged  = "NB.Balanced",
      classif.nnet.overbagged  = "MLP.Balanced",
      classif.ranger.overbagged  = "RF.Balanced",
      classif.rpart.overbagged  = "DT.Balanced",
      classif.svm.overbagged  = "SVM.Balanced",
      Majority.overbagged = "Majority.Balanced",
      Random.overbagged = "Random.Balanced"
  )
  return(algos)
}

## ------------------------------------------------------------------------------------------------
## ------------------------------------------------------------------------------------------------

getSdsFromRepetitions = function(results, measure = "bac") {

  all.tasks =  unique(results$task)
  all.algos =  unique(results$algo)
  
  #all combinations
  comb = expand.grid(all.tasks, all.algos)
  aux = lapply(1:nrow(comb), function(i) {
    elem = comb[i,]
    subset = dplyr::filter(results, task == elem$Var1 & algo == elem$Var2)
    ret = sd(as.numeric(subset[,measure]))
    return(ret)
  })

  res = cbind(comb, unlist(aux))
  colnames(res) = c("task", "algo", "sd")
  return(res)
}

## ------------------------------------------------------------------------------------------------
## ------------------------------------------------------------------------------------------------

# https://gist.github.com/abelsonlive/4112423
cbind.fill = function(...){
    nm = list(...) 
    nm = lapply(nm, as.matrix)
    n = max(sapply(nm, nrow)) 
    do.call(cbind, lapply(nm, function (x) 
    rbind(x, matrix(, n-nrow(x), ncol(x))))) 
}

## ------------------------------------------------------------------------------------------------
## ------------------------------------------------------------------------------------------------

# https://www.statology.org/mode-in-r/
findMode = function(x) {
  u   = unique(x)
  tab = tabulate(match(x, u))
  return (u[tab == max(tab)])
}

## ------------------------------------------------------------------------------------------------
## ------------------------------------------------------------------------------------------------