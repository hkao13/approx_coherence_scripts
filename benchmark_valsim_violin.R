library(ggplot2)
library(ggthemes)
library(MASS)
library(data.table)
library(bit64)

file_name = "value_similarity.csv"

# READ THE FILE AND REMOVE COLUMN PC 
data = fread(file_name,sep=",")
data$pc = NULL
data$count = NULL

# AGGREGATE THE REALTIVE DIFFERENCE VALUES AND SUM THE COUNTS
agg_data = unique(data, incomparables=FALSE, fromLast=FALSE, by="realtive_difference")

ggplot(agg_data, aes(norm, realtive_difference)) + 
  geom_violin()