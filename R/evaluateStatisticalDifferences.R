## ------------------------------------------------------------------------------------------------
## ------------------------------------------------------------------------------------------------

# alpha = 0.05 means 95% of significance
evaluateStatisticalDifferences = function(res, alpha = 0.05) {

	# ---------------------
	#creating Setups
	# ---------------------
	
	inner.df = mlr::getBMRPerformances(res, as.df = TRUE)
	inner.df$algo.name = inner.df$learner.id
	inner.df$setup = "Original"

	ovb.ids = which(grepl(x = inner.df$learner.id, pattern = "overbagged"))
	inner.df$setup[ovb.ids] = "Balanced"
	inner.df$learner.id = gsub(x = inner.df$learner.id, pattern = ".overbagged", replacement = "")
	inner.df$learner.id = renameAlgoFactors(inner.df$learner.id)

	# ---------------------
	# evaluating Setups
	# ---------------------

	tasks  = unique(inner.df$task.id)
	algos  = unique(inner.df$learner.id)
	pairs  = expand.grid(tasks, algos)

	aux.stats = lapply(1:nrow(pairs), function(i) {

		elem = pairs[i,]
		# print(elem) # debug
		original.values = dplyr::filter(inner.df, learner.id == elem[,2] 
			& task.id == elem[,1] & setup == "Original")
		
		balanced.values = dplyr::filter(inner.df, learner.id == elem[,2] 
			& task.id == elem[,1] & setup == "Balanced")

		# -----------------
		# BAC Significance
		# -----------------

		bac.wilcoxon = suppressWarnings(wilcox.test(x = original.values$bac, 
			y = balanced.values$bac, paired = TRUE))
		bac.significance = bac.wilcoxon$p.value < alpha

		# highest BAC (which setup?)
		highest.bac = NULL
		if(mean(original.values$bac) >= mean(balanced.values$bac)) {
			highest.bac = "original"
		} else {
			highest.bac = "balanced"
		}
	
		# -----------------
		# FScore Significance
		# -----------------

		fsc.wilcoxon = suppressWarnings(wilcox.test(x = original.values$multiclass.fscore, 
			y = balanced.values$multiclass.fscore, paired = TRUE))
		fsc.significance = fsc.wilcoxon$p.value < alpha

		# highest fscore (which setup?)
		highest.fsc = NULL
		if(mean(original.values$multiclass.fscore) >= mean(balanced.values$multiclass.fscore)) {
			highest.fsc = "original"
		} else {
			highest.fsc = "balanced"
		}

		# -----------------
		# Binding values
		# -----------------

		ret =  cbind(elem, bac.significance, highest.bac, fsc.significance, highest.fsc)
		return(ret)
	})

	df.stats = do.call("rbind", aux.stats)
	df.stats[is.na(df.stats)] = FALSE
	colnames(df.stats) = c("task", "algo.name", "BAC", "Highest.BAC", "FScore", "Highest.FScore")

	return(df.stats)


}

## ------------------------------------------------------------------------------------------------
## ------------------------------------------------------------------------------------------------
