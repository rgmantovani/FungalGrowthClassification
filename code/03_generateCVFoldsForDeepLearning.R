## ------------------------------------------------------------------------------------------------
## ------------------------------------------------------------------------------------------------

cat(" @ Loading all required files:\n")
R.files = list.files(path = "R", full.names = TRUE)
for(file in R.files) {
  print(file)
  source(file)
}

#library(mlr)
set.seed(123)

df = read.csv(file = "~/capsule/data/dataset/task_complete.csv")
df$X = NULL
df$Name_file = as.factor(df$Name_file)

task = mlr::makeClassifTask(data = df, target = "rotulo")

cv = mlr::makeResampleDesc(method = "RepCV", folds = 10, rep = 10, stratify = TRUE)
print(cv)
rin = mlr::makeResampleInstance(desc = cv, task = task)
print(rin)

# --------------------
# Training sets
# --------------------

df.train = do.call("cbind.fill", rin$train.inds)
df.train = as.data.frame(df.train)
colnames(df.train) = paste("Train", 1:100)
write.csv2(df.train, file = "~/capsule/data/Training_folds.csv", row.names = FALSE)

# --------------------
# Testing sets
# --------------------

df.test = do.call("cbind.fill", rin$test.inds)
df.test = as.data.frame(df.test)
colnames(df.test) = paste("Test", 1:100)
write.csv2(df.test, file = "~/capsule/data/Testing_folds.csv", row.names = FALSE)

## ------------------------------------------------------------------------------------------------
## ------------------------------------------------------------------------------------------------
