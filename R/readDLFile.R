# -----------------------------------------------
# -----------------------------------------------

readDLFile = function(dl.file) {

	df = read.csv(dl.file)
	df = as.data.frame(t(df))
	rownames(df) = NULL
	colnames(df) = paste0("it", 1:100)
	return(df)
	
}

# -----------------------------------------------
# -----------------------------------------------

evaluateDLModels = function(df.targets, df.dnn) {

	aux.measures = lapply(1:ncol(df.dnn), function(i) {
		# print(i)
		real = na.omit(as.factor(df.targets[,i]))
		pred = na.omit(factor(df.dnn[,i], levels = levels(real)))
	
		# bac values
		bac  = mlr::measureBAC(truth = real, response = pred)

		# fscore values
		obj = caret::confusionMatrix(pred, real, mode = "everything", positive = "1")
		f1  = obj$byClass[,7]
		f1[is.na(f1)] = 0
		f1  = mean(f1)

		# returning metrics
		ret = c(bac, f1)
		return(ret)
	})

	values = data.frame(do.call("rbind", aux.measures))
	colnames(values) = c("BAC", "FScore")
	return(values)

}

# -----------------------------------------------
# -----------------------------------------------
