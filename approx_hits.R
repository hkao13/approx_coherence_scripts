library(ggplot2)
library(reshape2)

sumData = function(data) {
  dframe = data.frame(
    total_misses=sum(data$total_misses),
    demand_misses=sum(data$demand_misses),
    demand_accesses=sum(data$demand_accesses),
    read_hits=sum(data$read_hits),
    read_misses=sum(data$read_misses),
    read_misses_coherence=sum(data$read_misses_coherence)
  )
  #print(dframe)
  return(dframe)
}


processFile = function(filepath) {
  con = file(filepath, "r")
  
  demand_hits = c()
  demand_misses = c()
  demand_accesses = c()
  read_hits = c()
  read_misses = c()
  read_misses_coherence = c()
  
  write_hits = c()
  write_hits_approx = c()
  write_misses = c()
  write_misses_coherence = c()
  
  while ( TRUE ) {
    line = readLines(con, n = 1)
    if ( length(line) == 0 ) {
      break
    }
    
    if (grepl(".*l1_cntrl[1-9]+\\.L1Dcache.demand_hits.*", line)) {
      # print(line)
      matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
      miss = as.numeric(unlist(matches))[4]
      demand_hits = c(demand_hits, miss)
      # print(miss)
    }
    
    if (grepl(".*l1_cntrl[1-9]+\\.L1Dcache.demand_misses.*", line)) {
      # print(line)
      matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
      miss = as.numeric(unlist(matches))[4]
      demand_misses = c(demand_misses, miss)
      # print(miss)
    }
    
    if (grepl(".*l1_cntrl[1-9]+\\.L1Dcache.demand_accesses.*", line)) {
      #print(line)
      matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
      miss = as.numeric(unlist(matches))[4]
      demand_accesses = c(demand_accesses, miss)
      #print(miss)
    }
    
    if (grepl(".*l1_cntrl[1-9]+\\.L1Dcache.read_hits.*", line)) {
      #print(line)
      matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
      miss = as.numeric(unlist(matches))[4]
      read_hits = c(read_hits, miss)
      #print(miss)
    }
    
    if (grepl(".*l1_cntrl[1-9]+\\.L1Dcache.read_misses .*", line)) {
      #print(line)
      matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
      miss = as.numeric(unlist(matches))[4]
      read_misses = c(read_misses, miss)
      #print(miss)
    }
    
    if (grepl(".*l1_cntrl[1-9]+\\.L1Dcache.read_misses_coherence.*", line)) {
      #print(line)
      matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
      miss = as.numeric(unlist(matches))[4]
      read_misses_coherence = c(read_misses_coherence, miss)
      #print(miss)
    }
    
    if (grepl(".*l1_cntrl[1-9]+\\.L1Dcache.write_hits.*", line)) {
      #print(line)
      matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
      miss = as.numeric(unlist(matches))[4]
      write_hits = c(write_hits, miss)
      #print(miss)
    }
    
    if (grepl(".*l1_cntrl[1-9]+\\.L1Dcache.write_hits_approx.*", line)) {
      #print(line)
      matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
      miss = as.numeric(unlist(matches))[4]
      write_hits_approx = c(write_hits_approx, miss)
      #print(miss)
    }
    
    if (grepl(".*l1_cntrl[1-9]+\\.L1Dcache.write_misses .*", line)) {
      #print(line)
      matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
      miss = as.numeric(unlist(matches))[4]
      write_misses = c(write_misses, miss)
      #print(miss)
    }
    
    if (grepl(".*l1_cntrl[1-9]+\\.L1Dcache.write_misses_coherence.*", line)) {
      # print(line)
      matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
      miss = as.numeric(unlist(matches))[4]
      write_misses_coherence = c(write_misses_coherence, miss)
      # print(miss)
    }
  }
  
  dframe = data.frame(
    demand_hits=sum(demand_hits),
    demand_misses=sum(demand_misses),
    demand_accesses=sum(demand_accesses),
    read_hits=sum(read_hits),
    read_misses=sum(read_misses),
    read_misses_coherence=sum(read_misses_coherence),
    
    write_hits=sum(write_hits),
    write_hits_approx=sum(write_hits_approx),
    write_misses=sum(write_misses),
    write_misses_coherence=sum(write_misses_coherence)
  )
  
  close(con)
  #print(dframe)
  #summed = sumData(dframe)
  return(dframe)
}

normalizeTo = function(data, reference) {
  dframe = data.frame(
    demand_hits=data$demand_hits/reference,
    demand_misses=data$demand_misses/reference,
    demand_accesses=data$demand_accesses/reference,
    read_hits=data$read_hits/reference,
    read_misses=data$read_misses/reference,
    read_misses_coherence=data$read_misses_coherence/reference,
    
    write_hits=data$write_hits/reference,
    write_hits_approx=data$write_hits_approx/reference,
    write_misses=data$write_misses/reference,
    write_misses_coherence=data$write_misses_coherence/reference
  )
  return(dframe)
}

benchmark_names = c("lin_reg", "kmeans", "histogram", "fmm", "water-nsq")
benchmark_names = c(benchmark_names, "lin_reg_50", "kmeans_50", "histogram_50", "fmm_50", "water-nsq_50")
benchmark_names = c(benchmark_names, "lin_reg_200", "kmeans_200", "histogram_200", "fmm_200", "water-nsq_200")
# benchmark_names = c("lin_reg")

# Summed data from stats file
all_benchmarks = data.frame(
  demand_hits=integer(),
  demand_misses=integer(),
  demand_accesses=integer(),
  read_hits=integer(),
  read_misses=integer(),
  read_misses_coherence=integer(),
  
  write_hits=integer(),
  write_hits_approx=integer(),
  write_misses=integer(),
  write_misses_coherence=integer()
)


# cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbPalette <- c("#999999", "#333333", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")



file = "~/cluster_results/results/linear_regression_base/m5out/stats.txt"
file_data = processFile(file)
all_benchmarks[nrow(all_benchmarks) + 1,] = file_data

file = "~/cluster_results/results/kmeans_base/m5out/stats.txt"
file_data = processFile(file)
all_benchmarks[nrow(all_benchmarks) + 1,] = file_data

file = "~/cluster_results/results/histogram_base/m5out/stats.txt"
file_data = processFile(file)
all_benchmarks[nrow(all_benchmarks) + 1,] = file_data

file = "~/cluster_results/results/fmm_base/m5out/stats.txt"
file_data = processFile(file)
all_benchmarks[nrow(all_benchmarks) + 1,] = file_data

file = "~/cluster_results/results/water-nsquared_base/m5out/stats.txt"
file_data = processFile(file)
all_benchmarks[nrow(all_benchmarks) + 1,] = file_data



file = "~/cluster_results/results/linear_regression_50/m5out/stats.txt"
file_data = processFile(file)
all_benchmarks[nrow(all_benchmarks) + 1,] = file_data

file = "~/cluster_results/results/kmeans_50/m5out/stats.txt"
file_data = processFile(file)
all_benchmarks[nrow(all_benchmarks) + 1,] = file_data

file = "~/cluster_results/results/histogram_50/m5out/stats.txt"
file_data = processFile(file)
all_benchmarks[nrow(all_benchmarks) + 1,] = file_data

file = "~/cluster_results/results/fmm_50/m5out/stats.txt"
file_data = processFile(file)
all_benchmarks[nrow(all_benchmarks) + 1,] = file_data

file = "~/cluster_results/results/water-nsquared_50/m5out/stats.txt"
file_data = processFile(file)
all_benchmarks[nrow(all_benchmarks) + 1,] = file_data

print(all_benchmarks)


file = "~/cluster_results/results_more/linear_regression_200/m5out/stats.txt"
file_data = processFile(file)
all_benchmarks[nrow(all_benchmarks) + 1,] = file_data

file = "~/cluster_results/results_more/kmeans_200/m5out/stats.txt"
file_data = processFile(file)
all_benchmarks[nrow(all_benchmarks) + 1,] = file_data

file = "~/cluster_results/results_more/histogram_200/m5out/stats.txt"
file_data = processFile(file)
all_benchmarks[nrow(all_benchmarks) + 1,] = file_data

file = "~/cluster_results/results_more/fmm_200/m5out/stats.txt"
file_data = processFile(file)
all_benchmarks[nrow(all_benchmarks) + 1,] = file_data

file = "~/cluster_results/results_more/water-nsquared_200/m5out/stats.txt"
file_data = processFile(file)
all_benchmarks[nrow(all_benchmarks) + 1,] = file_data

print(all_benchmarks)


# Plot read and write misses normalized to all demand misses
all_benchmarks_frac = normalizeTo(all_benchmarks, all_benchmarks$demand_accesses)
data_subset = subset(all_benchmarks_frac, select=c("demand_hits"))
# Reshape to get into stacked bar form
data_subset$benchmarks <- benchmark_names 

data_reshape = melt(data_subset, id.vars = "benchmarks")

plot9 <- ggplot() + 
  geom_bar(aes(y=value, x=benchmarks, fill=variable), data=data_reshape,
           stat="identity", color="black", width=0.5) +
  theme_bw() +
  ylab("Normalized L1-d Coherence Cache Misses") +
  xlab("Benchmarks") +
  theme(
    legend.title = element_blank(),
    legend.position = "top",
    legend.text=element_text(size=11),
    axis.title.x = element_text(size=11),
    axis.title.y = element_text(size=11),
    axis.text.x  = element_text(angle=90, vjust=0.6, size=11),
    axis.text.y  = element_text(size=11)
  ) +
  scale_fill_manual(
    values=cbPalette,
    labels=c(" store hits " ))
  # ylim(c(0,1.00))

plot9
# pdf("coherence_misses_L1.pdf", width=8, height=3)
# print(plot9)
# dev.off()


# # Plot read and write misses normalized to all demand misses
# all_benchmarks_frac = normalizeTo(all_benchmarks, all_benchmarks$demand_accesses)
# data_subset = subset(all_benchmarks_frac, select=c("read_hits"))
# # Reshape to get into stacked bar form
# data_subset$benchmarks <- benchmark_names 
# 
# data_reshape = melt(data_subset, id.vars = "benchmarks")
# 
# plot1 <- ggplot() + 
#   geom_bar(aes(y=value, x=benchmarks, fill=variable), data=data_reshape,
#            stat="identity", color="black", width=0.5) +
#   theme_bw() +
#   ylab("Normalized L1-d Coherence Cache Misses") +
#   xlab("Benchmarks") +
#   theme(
#     legend.title = element_blank(),
#     legend.position = "top",
#     legend.text=element_text(size=11),
#     axis.title.x = element_text(size=11),
#     axis.title.y = element_text(size=11),
#     axis.text.x  = element_text(angle=90, vjust=0.6, size=11),
#     axis.text.y  = element_text(size=11)
#   ) +
#   scale_fill_manual(
#     values=cbPalette,
#     labels=c(" read hits ")) +
#   ylim(c(0,1.00))
# 
# plot1