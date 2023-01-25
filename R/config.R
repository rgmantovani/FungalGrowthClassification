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
   # "rpart.plot", "dplyr", "reshape2", "ggplot2"))

# ---------------------------
# Loading packages
# ---------------------------


library(mlr)
library(tidyverse)
library(caret)
library(corrplot)
library(scales)
library(factoextra)
library(agricolae)
library(metrica)

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
