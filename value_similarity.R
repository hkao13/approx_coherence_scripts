library(ggplot2)
library(ggthemes)
library(MASS)

# statsFunc <- function(data)
# {
# 	dframe = data.frame( size = length(data), mean = mean(data), median = median(data), var = var(data), sd = sd(data), min = min(data), max = max(data) )
# 	return(dframe)
# }
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

statsFunc <- function(data)
{

	size = sum(data$count)
	m = sum(data$realtive_difference * data$count) / sum(data$count)
	sigma = sqrt(sum((data$realtive_difference - m)**2 * data$count) / (sum(data$count)-1))
	max = max(data$realtive_difference)
	min = min(data$realtive_difference)

	med = 0
	middle = 1
	if (size %% 2 == 0)
	{
		middle = size / 2
	}
	else 
	{
		middle = (size + 1) / 2
	}

	count = 0
	for (row in 1:nrow(data))
	{
		count = count + data[row, "count"]
		if (count <= middle)
		{
			med = data[row, "realtive_difference"]
		}
		else
		{
			break
		}
	}

	dframe = data.frame( size = size, 
		mean = m,
		sd = sigma,
		max = max,
		min = min,
		median = med )
	return(dframe)
}

print("Opening data...")
# file_name <- "~/cluster_results/results/linear_regression/m5out/traffic.csv"
# file_name <- "~/cluster_results/approx_coherence_results/histo/base/m5out/traffic_misses.csv"
# file_name <- "~/cluster_results/results/histogram/m5out/traffic.csv"
# file_name <- "~/cluster_results/results/kmeans/m5out/traffic.csv"
# file_name <- "~/cluster_results/results/matrix_multiply/m5out/traffic.csv"
file_name <- "~/cluster_results/npb_test/lu/base/m5out/traffic_misses.csv"
# file_name <- "traffic.csv"
traffic_data = read.csv(file_name)

# file_name <- "~/cluster_results/results/linear_regression/m5out/value_similarity.csv"
# file_name <- "~/cluster_results/approx_coherence_results/histo/base/m5out/value_similarity_misses.csv"
# file_name <- "~/cluster_results/results/histogram/m5out/value_similarity.csv"
# file_name <- "~/cluster_results/results/kmeans/m5out/value_similarity.csv"
# file_name <- "~/cluster_results/results/matrix_multiply/m5out/value_similarity.csv"
file_name <- "~/cluster_results/npb_test/lu/base/m5out/value_similarity_misses.csv"
# file_name <- "value_similarity.csv"
val_sim_data = read.csv(file_name)

traffic_data <- traffic_data[order(traffic_data$traffic_count, decreasing = TRUE),]

top_addrs = traffic_data[1:5,]$pc
print(top_addrs)

val_sim = data.frame()
for (addr in top_addrs)
{
	print(addr)
	dt = val_sim_data[which(val_sim_data$pc == addr), ]
	val_sim = rbind(val_sim, dt)
}

val_sim_data = val_sim



# traffic reduction
# total_misses = sum(val_sim_data$count)
# print(total_misses)
# 
# num_1 = sum(val_sim_data[which(val_sim_data$realtive_difference < 1 & val_sim_data$realtive_difference > -1),]$count)
# num_5 = sum(val_sim_data[which(val_sim_data$realtive_difference < 5 & val_sim_data$realtive_difference > -5),]$count)
# num_10 = sum(val_sim_data[which(val_sim_data$realtive_difference < 10 & val_sim_data$realtive_difference > -10),]$count)
# num_20 = sum(val_sim_data[which(val_sim_data$realtive_difference < 20 & val_sim_data$realtive_difference > -20),]$count)
# num_30 = sum(val_sim_data[which(val_sim_data$realtive_difference < 30 & val_sim_data$realtive_difference > -30),]$count)
# num_40 = sum(val_sim_data[which(val_sim_data$realtive_difference < 40 & val_sim_data$realtive_difference > -40),]$count)
# num_50 = sum(val_sim_data[which(val_sim_data$realtive_difference < 50 & val_sim_data$realtive_difference > -50),]$count)
# 
# reduction_1 = (num_1)/total_misses * 100
# reduction_5 = (num_5)/total_misses * 100
# reduction_10 = (num_10)/total_misses * 100
# reduction_20 = (num_20)/total_misses * 100
# reduction_30 = (num_30)/total_misses * 100
# reduction_40 = (num_40)/total_misses * 100
# reduction_50 = (num_50)/total_misses * 100
# 
# print(paste("1%", reduction_1))
# print(paste("5%", reduction_5))
# print(paste("10%", reduction_10))
# print(paste("20%", reduction_20))
# print(paste("30%", reduction_30))
# print(paste("40%", reduction_40))
# print(paste("50%", reduction_50))



# print("Traffic data bar chart...")
# p1 <- ggplot(data=traffic_data, aes(x=pc, y=traffic_count)) + 
# 	geom_bar(stat = "identity", width = 0.1, color = "black") +
# 	theme_bw() +
# 	ggtitle("Traffic Generating Stores") +
# 	ylab("Coherence Traffic Count") +
# 	xlab("Store Address") +
# 	theme(
# 		axis.title.x = element_text(size=14),
# 		axis.title.y = element_text(size=14),
# 		axis.text.x  = element_text(angle=90, vjust=0.6, size=10),
# 		axis.text.y  = element_text(size=14)
# 		)
# print(p1)



print("Processing Value Similarity data...")
# print(val_sim_data)
# val_sim = val_sim_data[,-1]
# val_sim = val_sim_data
# print(unique(val_sim_data[,1]))
# val_sim = val_sim[order(val_sim$realtive_difference, decreasing = TRUE),]
# val_sim_data[which(val_sim_data$pc == ), ]
# val_sim = val_sim[-1,]
 # print(val_sim)

# unique_addrs = unique(val_sim_data[,1])
# for (addr in unique_addrs)
# {

# 	print(paste0("Processing Data for Addr: ", addr))

# 	val_sim = val_sim_data[which(val_sim_data$pc == addr), ]
# 	val_sim = val_sim[,-1]

# 	# plot = plot + 
# 	# geom_histogram(val_sim, aes(x=realtive_difference, weight=count), color = "black", fill="white", bins = 100) +
# 	# theme_bw()
# }


stats = statsFunc(val_sim_data)
print(stats)
mean = stats$mean
sd = stats$sd
med = stats$median



print("Normalizing Value Similarity data...")


unique_addrs = unique(val_sim_data[,1])
print(length(unique_addrs))
for (addr in unique_addrs)
{

	# print(paste0("Processing Data for Addr: ", addr))

	data_for_addr = val_sim_data[which(val_sim_data$pc == addr), ]
	count_sum = sum(data_for_addr$count)

	val_sim_data[which(val_sim_data$pc == addr), ]$count = val_sim_data[which(val_sim_data$pc == addr), ]$count/count_sum

}


# val_sim_data$count = val_sim_data$count / sum(val_sim_data$count)



p1 = ggplot(val_sim_data, aes(x=realtive_difference, weight=count, fill=pc)) +
geom_histogram(bins = 20, color="black") +
#geom_bar(stat = "identity", width = 0.1, color = "black") +
theme_bw() +
theme(legend.position="none") +
    ylab("Fraction of Stores to L1-d") +
	xlab("Relative Difference (%)") +
	theme(
		axis.title.x = element_text(size=14),
		axis.title.y = element_text(size=14),
		axis.text.x  = element_text(angle=30, vjust=0.6, size=14),
		axis.text.y  = element_text(size=14)
		) 

p1
pdf("full_hist.pdf", width=6, height=6)
print(p1)
dev.off()


ggplot(val_sim_data, aes(x=realtive_difference, weight=count, fill=pc)) +
geom_density(alpha=0.2) +
theme_bw() +
theme(legend.position="none") +
    ylab("Density of Stores to L1-d") +
	xlab("Relative Difference (%)") +
	theme(
		axis.title.x = element_text(size=14),
		axis.title.y = element_text(size=14),
		axis.text.x  = element_text(angle=30, vjust=0.6, size=14),
		axis.text.y  = element_text(size=14)
		)


val_sim_data = val_sim_data[!rowSums(val_sim_data[2] > (med + 500)),];
val_sim_data = val_sim_data[!rowSums(val_sim_data[2] < (med - 500)),];
# print(val_sim)

p2 = ggplot(val_sim_data, aes(x=realtive_difference, weight=count, fill=pc)) +
geom_histogram(bins = 20, color="black") +
theme_bw() +
theme(legend.position="none") +
    ylab("Fraction of Stores to L1-d") +
	xlab("Relative Difference (%)") +
	theme(
		axis.title.x = element_text(size=14),
		axis.title.y = element_text(size=14),
		axis.text.x  = element_text(angle=30, vjust=0.6, size=14),
		axis.text.y  = element_text(size=14)
		) 
p2
pdf("narrow_hist.pdf", width=6, height=6)
print(p2)
dev.off()

p3 = ggplot(val_sim_data, aes(x=realtive_difference, weight=count, fill=pc)) +
geom_density(alpha=0.5) +
theme_bw() +
theme(legend.position="none") +
theme(legend.position="none") +
    ylab("Density of Stores to L1-d") +
	xlab("Relative Difference (%)") +
	theme(
		axis.title.x = element_text(size=14),
		axis.title.y = element_text(size=14),
		axis.text.x  = element_text(angle=30, vjust=0.6, size=14),
		axis.text.y  = element_text(size=14)
		) 
p3
pdf("narrow_dist.pdf", width=6, height=6)
print(p3)
dev.off()


# rel_diff = c()

# for (pc_i in top_traffic_pc$pc)
# {
# 	# print(pc_i)
# 	found <- which(val_sim_data$pc == pc_i)
	
# 	bin <- val_sim_data[found, "realtive_difference"]
# 	freq <- val_sim_data[found, "count"]

# 	dfreq <- rep(bin, freq)
# 	rel_diff = c(rel_diff, dfreq);

# }


# dist_stats = statsFunc(rel_diff)
# print(dist_stats)
# freq_df = as.data.frame(table(rel_diff))
# freq_df = data.frame(realtive_difference=freq_df$rel_diff, count=freq_df$Freq)
# freq_df$realtive_difference <- as.numeric(as.character(freq_df$realtive_difference))
# # print(freq_df)

# st_dev = dist_stats$sd
# median = dist_stats$median
# min = dist_stats$min;
# max = dist_stats$max;
# width = as.integer( (max - min) * 0.01 )

# rel_diff_data_frame = data.frame(realtive_difference=rel_diff)

# hist_plot1 = ggplot(rel_diff_data_frame, aes(x=realtive_difference)) +
# geom_histogram(color = "black") + 
# #geom_bar(stat = "identity", width = 0.1, color = "black") +
# theme_bw()
# hist_plot1


# dense_plot1 = ggplot(rel_diff_data_frame, aes(x=realtive_difference)) +
# geom_density(aes(y=..scaled..)) + 
# #geom_bar(stat = "identity", width = 0.1, color = "black") +
# theme_bw() +
# ylab("Probability Density") +
# xlab("Store-Value Realtive Difference (%)") +
# theme(
# 		axis.title.x = element_text(size=14),
# 		axis.title.y = element_text(size=14),
# 		axis.text.x  = element_text(angle=45, vjust=0.6, size=14),
# 		axis.text.y  = element_text(size=14)
# 		)

# dense_plot1
# pdf("kmeans_valsim_dense_total.pdf", width=6, height=6)
# print(dense_plot1)
# dev.off()



# # traffic reduction
# total_misses = length(rel_diff)
#  print(total_misses)
# num_5 = sum(rel_diff < 5 & rel_diff > -5)
# num_10 = sum(rel_diff < 10 & rel_diff > -10)
# num_25 = sum(rel_diff < 25 & rel_diff > -25)
# num_50 = sum(rel_diff < 50 & rel_diff > -50)
# num_100 = sum(rel_diff < 100 & rel_diff > -100)

# reduction_5 = (num_5)/total_misses * 100
# reduction_10 = (num_10)/total_misses * 100
# reduction_25 = (num_25)/total_misses * 100
# reduction_50 = (num_50)/total_misses * 100
# reduction_100 = (num_100)/total_misses * 100

# print(paste("5%", reduction_5))
# print(paste("10%", reduction_10))
# print(paste("25%", reduction_25))
# print(paste("50%", reduction_50))
# print(paste("100%", reduction_100))