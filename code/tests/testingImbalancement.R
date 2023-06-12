##-------------------------------------------------------------------------------------------
##-------------------------------------------------------------------------------------------

ids  = c(1:40, 51:70, 101:120)
data = iris[ids, ] 

# table(data$Species)
# setosa versicolor  virginica 
# 40         20         20 

task = mlr::makeClassifTask(data = data, id = "test", target = "Species")
lrn  = mlr::makeLearner("classif.rpart")

# overbagging for data imbalancement
new.lrn = mlr::makeOverBaggingWrapper(learner = lrn, obw.rate = 2)

cv  = mlr::makeResampleDesc("CV", iter = 3, stratify = TRUE)
res = mlr::resample(task = task, learner = new.lrn, measure = list(bac), 
      resampling = cv)
print(res)

##-------------------------------------------------------------------------------------------
##-------------------------------------------------------------------------------------------
