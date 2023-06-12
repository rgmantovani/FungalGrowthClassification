# ---------------------------
# Available classification algorithms
# ---------------------------

ALGOS.MLR  = c("rpart", "kknn", "multinom", "ranger", "svm", "nnet", "naiveBayes")
   
# ---------------------------
# Required packages
# ---------------------------

# If they are not installed, please, uncomment the following lines:
# install.packages(c("ranger", "e1071", "caret", "kknn","scales", 
   # "corrplot","factoextra","agricolae", "mlr", "metrica", "rpart",
   # "rpart.plot", "tidyverse", "reshape2"))

# ---------------------------
# Loading packages
# ---------------------------


library(mlr,        quietly = TRUE, warn.conflicts = FALSE)
library(tidyverse,  quietly = TRUE, warn.conflicts = FALSE)
library(caret,      quietly = TRUE, warn.conflicts = FALSE)
library(corrplot,   quietly = TRUE, warn.conflicts = FALSE)
library(scales,     quietly = TRUE, warn.conflicts = FALSE)
library(factoextra, quietly = TRUE, warn.conflicts = FALSE)
library(agricolae,  quietly = TRUE, warn.conflicts = FALSE)
library(metrica,    quietly = TRUE, warn.conflicts = FALSE)
library(reshape2,   quietly = TRUE, warn.conflicts = FALSE)

# ---------------------------
# Seed for reproducibility
# ---------------------------

set.seed(123)

# ---------------------------
# MLR settings
# ---------------------------

mlr::configureMlr(on.learner.error = "warn")
mlr::configureMlr(show.info = TRUE)


# ---------------------------
# ---------------------------
