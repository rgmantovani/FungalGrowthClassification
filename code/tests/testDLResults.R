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
aux.dl = lapply(dl.files,function(file) {
	# print(file) # debug
	algo.name = gsub(x = file, pattern = paste0("../results/deepLearningResults/|.csv"), replacement = "")
	dl.results = readDLFile(dl.file = file)
	dl.measures = evaluateDLModels(df.targets = df.targets, df.dnn = dl.results)	
	dl.measures$algo = algo.name
	return(dl.measures)
}) 

# mean and sd
aux.tmp = lapply(aux.dl, function(sub) {
	mean.bac = mean(sub$BAC)
	sd.bac   = sd(sub$BAC)
	mean.fsc = mean(sub$FScore)
	sd.fsc   = sd(sub$FScore)
	ret = c(mean.bac, sd.bac, mean.fsc, sd.fsc)
	return(ret)
})

# data.frame(do.call("rbind", aux.tmp))
#   mean.bac    sd.bac   mean.fscore  sd.fscore algo 
# 1 0.3535833 0.05594014 0.2440030 0.09422872  "DNN"
# 2 0.3230574 0.05808085 0.2082388 0.08281150  "DNN_with_DA"
# 3 0.3518775 0.10090484 0.2937412 0.09769129  "VGG"
# 4 0.3187229 0.07592340 0.2015858 0.08171121  "VGG_with_DA"

# boxplot
df.test = do.call("rbind", aux.dl)
df.melt = reshape2::melt(df.test, id.vars = c(3))

g = ggplot(df.melt, aes(x = algo, y = value)) + geom_boxplot() + facet_grid(~variable)
g 

# ------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------