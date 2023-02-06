# ------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------

source("../R/config.R")
source("../R/multiclassMeasures.R")
source("../R/readDLFile.R")

# ------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------

all.files = list.files(path = "../results/deepLearningResults", full.names = TRUE)

# dl.files
# [1] "../results/deepLearningResults/DNN.csv"        
# [2] "../results/deepLearningResults/DNN_with_DA.csv"
# [3] "../results/deepLearningResults/Targets.csv"    
# [4] "../results/deepLearningResults/VGG.csv"        
# [5] "../results/deepLearningResults/VGG_with_DA.csv"

# which file is the target file
target.id   = grep(x = all.files, pattern = "Targets")
target.file = all.files[target.id] 


# target data
df.targets = readDLFile(dl.file = target.file)
dl.files = all.files[-3] 

# DL results
aux.dl = lapply(df.files,function(file) {
	# print(file) # debug
	algo.name = gsub(x = file, pattern = paste0("../results/deepLearningResults/|.csv"), replacement = "")
	dl.results = readDLFile(dl.file = file)
	dl.measures = evaluateDLModels(df.targets = df.targets, df.dnn = dl.results)	
	dl.measures$algo = algo.name
	return(dl.measures)
}) 


# boxplot
df.test = do.call("rbind", aux.dl)
df.melt = reshape2::melt(df.test, id.vars = c(3))

g = ggplot(df.melt, aes(x = algo, y = value)) + geom_boxplot() + facet_grid(~variable)
g 

# ------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------