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

# benchmark_names = c("lin_reg", "kmeans", "histogram", "pca", "mat_mul")
# benchmark_names = c("lin_reg", "kmeans", "histogram", "pca", "mat_mul", "histo", "mri-grid", "mri-q", "sgemm", "spmv", "stencil", "tpacf", "cg", "ft", "lu", "mg")
# benchmark_names = c("lin_reg")

benchmark_names = c("histogram", "lin_reg", "pca", "adi", "conv-3d", "fdtd-apml", "jacobi-2d", "seidel-2d")
# benchmark_names = c("bc", "bfs", "bfs_atomic", "con_comp_atomic", "con_comp_lock", "dfs", "pagerank", "pagerank_lock", "sorted_neighbors", "sssp", "sssp_outer", "sssp_outer_atomic", "triangle_atomic", "triangle_lock", "tsp")
# benchmark_names = c("adi", "conv-2d", "conv-3d", "fdtd-2d", "fdtd-apml", "jacobi-1d", "jacobi-2d", "seidel-2d", "2mm", "3mm", "atax", "bicg", "cholesky", "doitgen", "durbin", "dynprog", "gemm", "gemver", "gemsummv", "gramschmidt", "lu", "ludcmp", "mvt", "symm", "syr2k", "syrk", "trisolv", "trmm")

# Summed data from stats file
all_benchmarks = data.frame(
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

file = "~/cluster_results/store_reset_study_MESI/histogram/base/m5out/stats.txt"
file_data = processFile(file)
all_benchmarks[nrow(all_benchmarks) + 1,] = file_data


file = "~/cluster_results/store_reset_study_MESI/linear_regression/base/m5out/stats.txt"
file_data = processFile(file)
all_benchmarks[nrow(all_benchmarks) + 1,] = file_data

# file = "~/cluster_results/oracle_study/kmeans/base/m5out/stats.txt"
# file_data = processFile(file)
# all_benchmarks[nrow(all_benchmarks) + 1,] = file_data

file = "~/cluster_results/store_reset_study_MESI/pca/base/m5out/stats.txt"
file_data = processFile(file)
all_benchmarks[nrow(all_benchmarks) + 1,] = file_data

# file = "~/cluster_results/oracle_study/matrix_multiply/base/m5out/stats.txt"
# file_data = processFile(file)
# all_benchmarks[nrow(all_benchmarks) + 1,] = file_data

file = "~/cluster_results/store_reset_study_MESI/adi/base/m5out/stats.txt"
file_data = processFile(file)
all_benchmarks[nrow(all_benchmarks) + 1,] = file_data

# file = "~/cluster_results/store_reset_study_MESI/conv-2d/base/m5out/stats.txt"
# file_data = processFile(file)
# all_benchmarks[nrow(all_benchmarks) + 1,] = file_data

file = "~/cluster_results/store_reset_study_MESI/conv-3d/base/m5out/stats.txt"
file_data = processFile(file)
all_benchmarks[nrow(all_benchmarks) + 1,] = file_data

# file = "~/cluster_results/store_reset_study_MESI/fdtd-2d/base/m5out/stats.txt"
# file_data = processFile(file)
# all_benchmarks[nrow(all_benchmarks) + 1,] = file_data

file = "~/cluster_results/store_reset_study_MESI/fdtd-apml/base/m5out/stats.txt"
file_data = processFile(file)
all_benchmarks[nrow(all_benchmarks) + 1,] = file_data

# file = "~/cluster_results/store_reset_study_MESI/jacobi-1d/base/m5out/stats.txt"
# file_data = processFile(file)
# all_benchmarks[nrow(all_benchmarks) + 1,] = file_data

file = "~/cluster_results/store_reset_study_MESI/jacobi-2d/base/m5out/stats.txt"
file_data = processFile(file)
all_benchmarks[nrow(all_benchmarks) + 1,] = file_data

file = "~/cluster_results/store_reset_study_MESI/seidel-2d/base/m5out/stats.txt"
file_data = processFile(file)
all_benchmarks[nrow(all_benchmarks) + 1,] = file_data
print(all_benchmarks)







# # -----------------------------------------------------------------------
# 
# file = "~/cluster_results/CRONO_test/bc/CA/m5out/stats.txt"
# # file = "~/gem5-nej/m5out/stats.txt"
# file_data = processFile(file)
# all_benchmarks[nrow(all_benchmarks) + 1,] = file_data
# 
# file = "~/cluster_results/CRONO_test/bfs/facebook/m5out/stats.txt"
# file_data = processFile(file)
# all_benchmarks[nrow(all_benchmarks) + 1,] = file_data
# 
# file = "~/cluster_results/CRONO_test/bfs_atomic/facebook/m5out/stats.txt"
# file_data = processFile(file)
# all_benchmarks[nrow(all_benchmarks) + 1,] = file_data
# 
# file = "~/cluster_results/CRONO_test/connected_components_atomic/facebook/m5out/stats.txt"
# file_data = processFile(file)
# all_benchmarks[nrow(all_benchmarks) + 1,] = file_data
# 
# file = "~/cluster_results/CRONO_test/connected_components_lock/facebook/m5out/stats.txt"
# file_data = processFile(file)
# all_benchmarks[nrow(all_benchmarks) + 1,] = file_data
# 
# file = "~/cluster_results/CRONO_test/dfs/facebook/m5out/stats.txt"
# file_data = processFile(file)
# all_benchmarks[nrow(all_benchmarks) + 1,] = file_data
# 
# file = "~/cluster_results/CRONO_test/pagerank/facebook/m5out/stats.txt"
# file_data = processFile(file)
# all_benchmarks[nrow(all_benchmarks) + 1,] = file_data
# 
# file = "~/cluster_results/CRONO_test/pagerank_lock/facebook/m5out/stats.txt"
# file_data = processFile(file)
# all_benchmarks[nrow(all_benchmarks) + 1,] = file_data
# 
# file = "~/cluster_results/CRONO_test/sorted_neighbors_tri_lock/facebook/m5out/stats.txt"
# file_data = processFile(file)
# all_benchmarks[nrow(all_benchmarks) + 1,] = file_data
# 
# file = "~/cluster_results/CRONO_test/sssp/facebook/m5out/stats.txt"
# file_data = processFile(file)
# all_benchmarks[nrow(all_benchmarks) + 1,] = file_data
# 
# file = "~/cluster_results/CRONO_test/sssp_outer/facebook/m5out/stats.txt"
# file_data = processFile(file)
# all_benchmarks[nrow(all_benchmarks) + 1,] = file_data
# 
# file = "~/cluster_results/CRONO_test/sssp_outer_atomic/facebook/m5out/stats.txt"
# file_data = processFile(file)
# all_benchmarks[nrow(all_benchmarks) + 1,] = file_data
# 
# file = "~/cluster_results/CRONO_test/triangle_counting_atomic/facebook/m5out/stats.txt"
# file_data = processFile(file)
# all_benchmarks[nrow(all_benchmarks) + 1,] = file_data
# 
# file = "~/cluster_results/CRONO_test/triangle_counting_lock/facebook/m5out/stats.txt"
# file_data = processFile(file)
# all_benchmarks[nrow(all_benchmarks) + 1,] = file_data
# 
# file = "~/cluster_results/CRONO_test/tsp/16/m5out/stats.txt"
# file_data = processFile(file)
# all_benchmarks[nrow(all_benchmarks) + 1,] = file_data
# 
# 
# # -----------------------------------------------------------------------





# # -----------------------------------------------------------------------
# 
# file = "~/cluster_results/polybench_small/adi/m5out/stats.txt"
# # file = "~/gem5-nej/m5out/stats.txt"
# file_data = processFile(file)
# all_benchmarks[nrow(all_benchmarks) + 1,] = file_data
# 
# file = "~/cluster_results/polybench_small/conv-2d/m5out/stats.txt"
# # file = "~/gem5-nej/m5out/stats.txt"
# file_data = processFile(file)
# all_benchmarks[nrow(all_benchmarks) + 1,] = file_data
# 
# file = "~/cluster_results/polybench_small/conv-3d/m5out/stats.txt"
# # file = "~/gem5-nej/m5out/stats.txt"
# file_data = processFile(file)
# all_benchmarks[nrow(all_benchmarks) + 1,] = file_data
# 
# file = "~/cluster_results/polybench_small/fdtd-2d/m5out/stats.txt"
# # file = "~/gem5-nej/m5out/stats.txt"
# file_data = processFile(file)
# all_benchmarks[nrow(all_benchmarks) + 1,] = file_data
# 
# file = "~/cluster_results/polybench_small/fdtd-apml/m5out/stats.txt"
# # file = "~/gem5-nej/m5out/stats.txt"
# file_data = processFile(file)
# all_benchmarks[nrow(all_benchmarks) + 1,] = file_data
# 
# file = "~/cluster_results/polybench_small/jacobi-1d/m5out/stats.txt"
# # file = "~/gem5-nej/m5out/stats.txt"
# file_data = processFile(file)
# all_benchmarks[nrow(all_benchmarks) + 1,] = file_data
# 
# file = "~/cluster_results/polybench_small/jacobi-2d/m5out/stats.txt"
# # file = "~/gem5-nej/m5out/stats.txt"
# file_data = processFile(file)
# all_benchmarks[nrow(all_benchmarks) + 1,] = file_data
# 
# file = "~/cluster_results/polybench_small/seidel-2d/m5out/stats.txt"
# # file = "~/gem5-nej/m5out/stats.txt"
# file_data = processFile(file)
# all_benchmarks[nrow(all_benchmarks) + 1,] = file_data
# 
# 
# 
# 
# file = "~/cluster_results/polybench_small/2mm/m5out/stats.txt"
# # file = "~/gem5-nej/m5out/stats.txt"
# file_data = processFile(file)
# all_benchmarks[nrow(all_benchmarks) + 1,] = file_data
# 
# 
# file = "~/cluster_results/polybench_small/3mm/m5out/stats.txt"
# # file = "~/gem5-nej/m5out/stats.txt"
# file_data = processFile(file)
# all_benchmarks[nrow(all_benchmarks) + 1,] = file_data
# 
# file = "~/cluster_results/polybench_small/atax/m5out/stats.txt"
# # file = "~/gem5-nej/m5out/stats.txt"
# file_data = processFile(file)
# all_benchmarks[nrow(all_benchmarks) + 1,] = file_data
# 
# file = "~/cluster_results/polybench_small/bicg/m5out/stats.txt"
# # file = "~/gem5-nej/m5out/stats.txt"
# file_data = processFile(file)
# all_benchmarks[nrow(all_benchmarks) + 1,] = file_data
# 
# file = "~/cluster_results/polybench_small/cholesky/m5out/stats.txt"
# # file = "~/gem5-nej/m5out/stats.txt"
# file_data = processFile(file)
# all_benchmarks[nrow(all_benchmarks) + 1,] = file_data
# 
# file = "~/cluster_results/polybench_small/doitgen/m5out/stats.txt"
# # file = "~/gem5-nej/m5out/stats.txt"
# file_data = processFile(file)
# all_benchmarks[nrow(all_benchmarks) + 1,] = file_data
# 
# file = "~/cluster_results/polybench_small/durbin/m5out/stats.txt"
# # file = "~/gem5-nej/m5out/stats.txt"
# file_data = processFile(file)
# all_benchmarks[nrow(all_benchmarks) + 1,] = file_data
# 
# file = "~/cluster_results/polybench_small/dynprog/m5out/stats.txt"
# # file = "~/gem5-nej/m5out/stats.txt"
# file_data = processFile(file)
# all_benchmarks[nrow(all_benchmarks) + 1,] = file_data
# 
# file = "~/cluster_results/polybench_small/gemm/m5out/stats.txt"
# # file = "~/gem5-nej/m5out/stats.txt"
# file_data = processFile(file)
# all_benchmarks[nrow(all_benchmarks) + 1,] = file_data
# 
# file = "~/cluster_results/polybench_small/gemver/m5out/stats.txt"
# # file = "~/gem5-nej/m5out/stats.txt"
# file_data = processFile(file)
# all_benchmarks[nrow(all_benchmarks) + 1,] = file_data
# 
# file = "~/cluster_results/polybench_small/gesummv/m5out/stats.txt"
# # file = "~/gem5-nej/m5out/stats.txt"
# file_data = processFile(file)
# all_benchmarks[nrow(all_benchmarks) + 1,] = file_data
# 
# file = "~/cluster_results/polybench_small/gramschmidt/m5out/stats.txt"
# # file = "~/gem5-nej/m5out/stats.txt"
# file_data = processFile(file)
# all_benchmarks[nrow(all_benchmarks) + 1,] = file_data
# 
# file = "~/cluster_results/polybench_small/lu/m5out/stats.txt"
# # file = "~/gem5-nej/m5out/stats.txt"
# file_data = processFile(file)
# all_benchmarks[nrow(all_benchmarks) + 1,] = file_data
# 
# file = "~/cluster_results/polybench_small/ludcmp/m5out/stats.txt"
# # file = "~/gem5-nej/m5out/stats.txt"
# file_data = processFile(file)
# all_benchmarks[nrow(all_benchmarks) + 1,] = file_data
# 
# file = "~/cluster_results/polybench_small/mvt/m5out/stats.txt"
# # file = "~/gem5-nej/m5out/stats.txt"
# file_data = processFile(file)
# all_benchmarks[nrow(all_benchmarks) + 1,] = file_data
# 
# file = "~/cluster_results/polybench_small/symm/m5out/stats.txt"
# # file = "~/gem5-nej/m5out/stats.txt"
# file_data = processFile(file)
# all_benchmarks[nrow(all_benchmarks) + 1,] = file_data
# 
# file = "~/cluster_results/polybench_small/syr2k/m5out/stats.txt"
# # file = "~/gem5-nej/m5out/stats.txt"
# file_data = processFile(file)
# all_benchmarks[nrow(all_benchmarks) + 1,] = file_data
# 
# file = "~/cluster_results/polybench_small/syrk/m5out/stats.txt"
# # file = "~/gem5-nej/m5out/stats.txt"
# file_data = processFile(file)
# all_benchmarks[nrow(all_benchmarks) + 1,] = file_data
# 
# file = "~/cluster_results/polybench_small/trisolv/m5out/stats.txt"
# # file = "~/gem5-nej/m5out/stats.txt"
# file_data = processFile(file)
# all_benchmarks[nrow(all_benchmarks) + 1,] = file_data
# 
# file = "~/cluster_results/polybench_small/trmm/m5out/stats.txt"
# # file = "~/gem5-nej/m5out/stats.txt"
# file_data = processFile(file)
# all_benchmarks[nrow(all_benchmarks) + 1,] = file_data
# 
# # -----------------------------------------------------------------------






# Plot miss and hits mornalized to all demand accesses
all_benchmarks_frac = normalizeTo(all_benchmarks, all_benchmarks$demand_accesses)
data_subset = subset(all_benchmarks_frac, select=c("demand_misses"))
# Reshape to get into stacked bar form
data_subset$benchmarks <- benchmark_names 

data_reshape = melt(data_subset, id.vars = "benchmarks")

plot1 <- ggplot() + 
  geom_bar(aes(y=value, x=benchmarks, fill=variable), data=data_reshape,
           stat="identity", color="black", width=0.5) +
  theme_bw() +
  ylab("Fraction of L1-d Cache Misses") +
  xlab("Benchmarks") +
  theme(
    legend.title = element_blank(),
    legend.position = "top",
    legend.text=element_text(size=14),
    axis.title.x = element_text(size=14),
    axis.title.y = element_text(size=14),
    axis.text.x  = element_text(angle=30, vjust=0.6, size=14),
    axis.text.y  = element_text(size=14)
  ) +
  scale_fill_manual(
    values=cbPalette, labels=c(" cache misses ")) +
  ylim(c(0,1.00))

plot1
# pdf("cache_misses_L1.pdf", width=6, height=6)
# print(plot1)
# dev.off()



# Plot read and write misses normalized to all demand misses
all_benchmarks_frac = normalizeTo(all_benchmarks, all_benchmarks$demand_misses)
data_subset = subset(all_benchmarks_frac, select=c("write_misses", "read_misses"))
# Reshape to get into stacked bar form
data_subset$benchmarks <- benchmark_names 

data_reshape = melt(data_subset, id.vars = "benchmarks")

plot2 <- ggplot() + 
  geom_bar(aes(y=value, x=benchmarks, fill=variable), data=data_reshape,
           stat="identity", color="black", width=0.5) +
  theme_bw() +
  ylab("Normalized L1-d Cache Misses") +
  xlab("Benchmarks") +
  theme(
    legend.title = element_blank(),
    legend.position = "top",
    legend.text=element_text(size=14),
    axis.title.x = element_text(size=14),
    axis.title.y = element_text(size=14),
    axis.text.x  = element_text(angle=30, vjust=0.6, size=14),
    axis.text.y  = element_text(size=14)
  ) +
  scale_fill_manual(
    values=cbPalette,
    labels=c(" store miss ", " load miss ")) +
  ylim(c(0,1.00))

plot2
# pdf("all_misses_L1.pdf", width=6, height=6)
# print(plot2)
# dev.off()







# Plot read and write misses normalized to all demand misses
all_benchmarks_frac = normalizeTo(all_benchmarks, all_benchmarks$demand_misses)
data_subset = subset(all_benchmarks_frac, select=c("read_misses_coherence", "write_misses_coherence"))
# Reshape to get into stacked bar form
data_subset$benchmarks <- benchmark_names 

data_reshape = melt(data_subset, id.vars = "benchmarks")


# Calculate the Average
benchmarks = c(rep("avg",2))


variable = c("read_misses_coherence", "write_misses_coherence")
value = c(
  mean(data_reshape$value[data_reshape$variable == "read_misses_coherence"]),
  mean(data_reshape$value[data_reshape$variable == "write_misses_coherence"])
)
avg_data = data.frame(benchmarks, variable, value)
data_reshape = rbind(data_reshape, avg_data)

data_reshape$benchmarks = factor(data_reshape$benchmarks, levels = data_reshape$benchmarks)

plot9 <- ggplot() + 
  geom_bar(aes(y=value, x=benchmarks, fill=variable), data=data_reshape,
           stat="identity", color="black", width=0.5) +
  theme_bw() +
  ylab("Fraction L1-d Coherence Misses") +
  xlab("Benchmarks") +
  theme(
    legend.title = element_blank(),
    legend.position = "top",
    legend.text=element_text(size=22),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=22),
    axis.text.x  = element_text(angle=90, vjust=0.6, size=22),
    axis.text.y  = element_text(size=22),
    panel.grid.major.y = element_line(colour = "black"),
    panel.grid.major.x = element_blank()
  ) +
  scale_fill_manual(
    values=cbPalette,
    labels=c(" load miss ", " store miss ")) +
  ylim(c(0,1.00)) 
  # scale_y_continuous(limits = c(0, 1.00))

plot9
pdf("coherence_misses_L1.pdf", width=9, height=6)
print(plot9)
dev.off()