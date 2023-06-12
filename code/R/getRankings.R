## ------------------------------------------------------------------------------------------
## ------------------------------------------------------------------------------------------

# bac.rankings = getRanking(df.agg = df.agg, measure = "BAC")
# fsc.rankings = getRanking(df.agg = df.agg, measure = "FScore")

getRankings = function(df.agg, measure = "BAC") {

	all.tasks = unique(df.agg$task)
	subset.algos <<- NULL
	aux.rk = lapply(all.tasks, function(sel.task) {
		# print(sel.task)
		subset = dplyr::filter(df.agg, task == sel.task)
		subset.algos <<- subset$algo
		rk = data.table::frank(x = (1- subset[measure]))
		return(rk)
	})

	df.rk = as.data.frame(do.call("cbind", aux.rk))
	colnames(df.rk) = all.tasks
	
	# calculating the average ranking over all tasks
	df.rk$avg.rk = as.numeric(colMeans(as.data.frame(do.call("rbind", aux.rk))))
	df.rk$algo = subset.algos

	# ordering ranking (lowest to hightest) - lower: better
	df.rk = df.rk[order(df.rk$avg.rk),]
	return(df.rk)
}

## ------------------------------------------------------------------------------------------
## ------------------------------------------------------------------------------------------
