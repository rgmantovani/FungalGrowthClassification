## ------------------------------------------------------------------------------------------
## ------------------------------------------------------------------------------------------

createLearnersMLR = function() {
	
	aux = lapply(ALGOS.MLR, function(algo) {
		algo.name = paste0("classif.", algo)
		cl = mlr::makeLearner(cl = algo.name, id = algo.name, predict.type = "response")
		return(cl) 
	})

	# https://www.rdocumentation.org/packages/mlr/versions/2.15.0/topics/classif.featureless
	baseline1 = mlr::makeLearner(cl = "classif.featureless", id = "Random",
		method = "sample-prior", predict.type = "response")
	baseline2 = mlr::makeLearner(cl = "classif.featureless", id = "Majority", 
		method = "majority", predict.type = "response")

	aux[[8]] = baseline1
	aux[[9]] = baseline2

	learners.list = aux
	return(learners.list)
}

## ------------------------------------------------------------------------------------------
## ------------------------------------------------------------------------------------------

createOverBaggingLearners = function(learners) {

	aux = lapply(learners, function(lrn) {
		new.lrn = mlr::makeOverBaggingWrapper(learner = lrn, obw.rate = 3)
		return(new.lrn)	
	})
	return(aux)
}

## ------------------------------------------------------------------------------------------
## ------------------------------------------------------------------------------------------