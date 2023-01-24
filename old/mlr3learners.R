## ------------------------------------------------------------------------------------------
## ------------------------------------------------------------------------------------------

createLearnersMLR3 = function() {
	
	aux = lapply(ALGOS, function(algo) {
		algo.name = paste0("classif.", algo)
		cl = mlr3::lrn(algo.name, id = algo.name)
		return(cl) 
	})

	clr_bas_1 = mlr3::lrn("classif.featureless", method="mode",   id = "Majority")  
	clr_bas_2 = mlr3::lrn("classif.featureless", method="sample", id = "Random") 

	aux = append(aux, clr_bas_1)
	aux = append(aux, clr_bas_2)
	
	learners.list = aux

	# https://mlr-org.com/gallery/basic/2020-03-30-imbalanced-data/

	# undersample majority class (relative to majority class)
	# po_under = po("classbalancing",
  	# 	id = "undersample", adjust = "major",
  	# 	reference = "major", shuffle = FALSE, ratio = 1 / 6)

	# # oversample majority class (relative to majority class)
	# po_over = po("classbalancing",
  	# 	id = "oversample", adjust = "minor",
  	# 	reference = "minor", shuffle = FALSE, ratio = 6)

	# # SMOTE enriches the minority class with synthetic data
	# gr_smote =
	#   po("colapply", id = "int_to_num",
	#     applicator = as.numeric, affect_columns = selector_type("integer")) %>>%
	#   po("smote", dup_size = 6) %>>%
	#   po("colapply", id = "num_to_int",
	#     applicator = function(x) as.integer(round(x, 0L)), affect_columns = selector_type("numeric"))

	# 	 # create random forest learner
	# learner = lrn("classif.ranger", num.trees = 50)

	# # combine learner with pipeline graph
	# learner_under = as_learner(po_under %>>% learner)
	# learner_under$id = "undersample.ranger"
	# learner_over = as_learner(po_over %>>% learner)
	# learner_over$id = "oversample.ranger"
	# learner_smote = as_learner(gr_smote %>>% learner)
	# learner_smote$id = "smote.ranger"

	return(learners.list)
}
