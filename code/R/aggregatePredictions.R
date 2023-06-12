## ------------------------------------------------------------------------------------------
## ------------------------------------------------------------------------------------------

aggregatePredictions = function(df) {

	tmp = as.data.frame(df)
	ids = unique(tmp$id)

	aux.ids = lapply(ids, function(value) {
		sel = dplyr::filter(tmp, id == value)	
		md = findMode(sel$response)[1]
		ret = sel[1, ]
		ret$response = md
		return(ret)
	})

	agg.preds = do.call("rbind", aux.ids)
	return(agg.preds)
}

## ------------------------------------------------------------------------------------------
## ------------------------------------------------------------------------------------------

# Compute the Voting's prediction (ensemble)
voting = function(voting.df) {
	preds = apply(voting.df, 1, findMode)
	aux = lapply(preds, function(elem) return(elem[1]))
	ret = unlist(aux)
	return(ret)
}

## ------------------------------------------------------------------------------------------
## ------------------------------------------------------------------------------------------
