library(ggplot2)
library(ggthemes)
library(MASS)
library(data.table)
library(bit64)


format_data = function(data, name)
{
  data$pc = NULL
  # AGGREGATE THE REALTIVE DIFFERENCE VALUES AND SUM THE COUNTS
  agg_data = data[, .(sum = sum(count)), by = realtive_difference]
  # NORMALIZE THE SUMS OF COUNTS
  agg_data$norm = agg_data$sum / sum(agg_data$sum)
  agg_data
  # TARGET SECTION OF INTEREST
  agg_data = agg_data[agg_data$realtive_difference <= 1000]
  agg_data = agg_data[agg_data$realtive_difference >= -1000]
  agg_data[, realtive_difference:=as.numeric(realtive_difference)]
  agg_data$benchmark = name
  agg_data 
}

file_name = "~/cluster_results/results/histogram_base/m5out/value_similarity.csv"
# READ THE FILE
data = fread(file_name,sep=",")
agg_data = format_data(data, "histogram")

file_name = "~/cluster_results/results/linear_regression_base/m5out/value_similarity.csv"
# READ THE FILE
data = fread(file_name,sep=",")
agg_data = rbind(agg_data, format_data(data, "lin_reg"))

file_name = "~/cluster_results/results/kmeans_base/m5out/value_similarity.csv"
# READ THE FILE
data = fread(file_name,sep=",")
agg_data = rbind(agg_data, format_data(data, "kmeans"))

file_name = "~/cluster_results/results/fmm_base/m5out/value_similarity.csv"
# READ THE FILE
data = fread(file_name,sep=",")
agg_data = rbind(agg_data, format_data(data, "fmm"))

file_name = "~/cluster_results/results/water-nsquared_base/m5out/value_similarity.csv"
# READ THE FILE
data = fread(file_name,sep=",")
agg_data = rbind(agg_data, format_data(data, "water-nsq"))



ggplot(agg_data, aes(x=realtive_difference, weight=norm, color=benchmark, fill=benchmark)) + 
  geom_density(alpha=0) +
  theme_bw() +
  ylab("Density") +
  xlab("Store and Overwritten L1 Value Relative Difference (%)")

p = ggplot(agg_data, aes(x=benchmark, y=realtive_difference, weight=norm)) + 
  geom_violin() +
  geom_boxplot(outlier.shape = NA, width = 0.75/length(unique(agg_data$benchmark))) +
  theme_bw() +
  ylab("Store Value Relative Difference (%)") +
  xlab("Benchmarks") +
  theme(
    axis.title.x = element_text(size=11),
    axis.title.y = element_text(size=11),
    axis.text.x  = element_text(size=11),
    axis.text.y  = element_text(size=11)
  )

pdf("value_similarity.pdf", width=8, height=3)
print(p)

dev.off()