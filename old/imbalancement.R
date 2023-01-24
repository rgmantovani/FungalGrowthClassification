##-------------------------------------------------------------------------------------------
##-------------------------------------------------------------------------------------------

# TODO: in the future, put SMOTE in the wrapped learner

##-------------------------------------------------------------------------------------------
##-------------------------------------------------------------------------------------------

# Solution (for now): 
# say you have 3 classes, what you do is:
# Forget about class 3, apply SMOTE on classes 1 and 2
# Next, forget about class 2, apply SMOTE on classes 2 and 3
# You can do this for any number of classes, you just run SMOTE (c-1) times.

# SMOTE, undersampling and oversampling methods
# class distribution = 352, 106, 105
# output = 325 318 315 

##-------------------------------------------------------------------------------------------
##-------------------------------------------------------------------------------------------

makeMulticlassSMOTEDTask = function(task, sw.rate = 3, sw.nn = 5, sw.standardize = TRUE, 
	sw.alt.logic = FALSE) {

	# smote with classes: 1 and 2
	sub.ids1     = which(mlr::getTaskTargets(task) %in% c(1, 2))
	subtask1     = mlr::subsetTask(task = task, subset = sub.ids1)
	
	tmp = mlr::getTaskData(task = subtask1)
	tmp$rotulo = factor(tmp$rotulo, levels = c(1, 2))
	new.task1   = mlr::makeClassifTask(id = "subtask1", data = tmp, target = "rotulo") 
	smote.task.1 = mlr::smote(task = new.task1, rate = sw.rate, nn = sw.nn, 
		standardize = sw.standardize, alt.logic = sw.alt.logic)

	# smote with classes: 1 and 3
	sub.ids2     = which(mlr::getTaskTargets(task) %in% c(1, 3))
	subtask2     = mlr::subsetTask(task = task, subset = sub.ids2)

	tmp2 = mlr::getTaskData(task = subtask2)
	tmp2$rotulo = factor(tmp2$rotulo, levels = c(1, 3))
	new.task2   = mlr::makeClassifTask(id = "subtask2", data = tmp2, target = "rotulo") 
	smote.task.2 = mlr::smote(task = new.task2, rate = sw.rate, nn = sw.nn, 
		standardize = sw.standardize, alt.logic = sw.alt.logic)

	#unifying tasks with smoted examples
	new.data     = mlr::getTaskData(task = smote.task.1)
	new.examples = mlr::getTaskData(task = smote.task.2)
	sel.examples = new.examples[which(new.examples$rotulo == 3),]
	new.data     = rbind(new.data, sel.examples) 

	new.task = makeClassifTask(id = paste0(mlr::getTaskId(task), " SMOTED"), data = new.data,
		target = "rotulo")
	# Smoted distribution (output)
	return(new.task)
}

##-------------------------------------------------------------------------------------------
##-------------------------------------------------------------------------------------------