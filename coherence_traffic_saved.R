library(ggplot2)
library(reshape2)
# library(dplyr)

options(scipen=999)

processFile = function(filepath) {
  con = file(filepath, "r")
  
  state_vec = c()
  transition_vec = c()
  count_vec = c()
  
  while ( TRUE ) {
    line = readLines(con, n = 1)
    if ( length(line) == 0 ) {
      break
    }
    
    if (grepl(".*End Simulation Statistics.*", line))
    {
      print(line)
      break
    }
    
    if (grepl(".*system.ruby.L1Cache_Controller.[[:graph:]]+[.]{1}[[:graph:]]+::total.*", line)) {
      # print(line)
      matches <- strsplit(line, "\\.|::total[\\s]*")
      # matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
      state = unlist(matches)[4]
      transition = unlist(matches)[5]
      count = as.numeric(unlist(matches))[6]
      
      state_vec = c(state_vec, state)
      transition_vec = c(transition_vec, transition)
      count_vec = c(count_vec, count)
    }
    
    dframe = data.frame(
      state = state_vec,
      request = transition_vec,
      count = count_vec
    )
    
    
  }
  
  close(con)
  #print(dframe)
  #summed = sumData(dframe)
  return(dframe)
}




# ---------------- STATS FILES ------------------
# file = "~/gem5-nej/m5out/base.txt"
# file = "~/cluster_results/sweep_cores/4/base/m5out/stats.txt"
# file = "~/cluster_results/approx_coherence_results/linear_regression/base/m5out/stats.txt"
# file = "~/cluster_results/store_reset_study_MESI/adi/base/m5out/stats.txt"
# file = "~/cluster_results/polybench_small_store_count_8/seidel-2d/m5out/stats.txt"
file = "~/gem5-nej/m5out/base.txt"
data = processFile(file)

# file = "~/cluster_results/sweep_cores/8/base/m5out/stats.txt"
# file = "~/cluster_results/approx_coherence_results/linear_regression/100/m5out/stats.txt"
# file = "~/cluster_results/store_reset_study_MESI/adi/8/m5out/stats.txt"
# file = "~/cluster_results/standard_mod.txt"
file = "~/gem5-nej/m5out/stats.txt"
data_approx = processFile(file)

states = c()
counts = c()
requests = c()

# Remove Ifetch request since we don't care about that.
data = data[data$request != "Ifetch", ]
data = data[data$request != "Store_Approx", ]

data = data[data$request != "Store", ]
data = data[data$request != "Load", ]

data = data[data$request != "Use_Timeout", ]

data_approx = data_approx[data_approx$request != "Ifetch", ]
data_approx = data_approx[data_approx$request != "Store_Approx", ]

data_approx = data_approx[data_approx$request != "Store", ]
data_approx = data_approx[data_approx$request != "Load", ]
data_approx = data_approx[data_approx$request != "Load_Approx", ]

data_approx = data_approx[data_approx$request != "Use_Timeout", ]

# data = data[data$state %in% c("I", "S", "O"), ]

data$percent = 100*data$count / sum(data$count)

for (s in unique(data$state))
{
  print(s)
  states = c(states, s)
  total = sum(data$count[data$state == s])
  counts = c(counts, total)
}



# pdf(file = "L1_state.pdf")
# pie(counts, labels = states, main="L1 Coherence States", cex=1.8)
# dev.off()

req_counts = c()

for (r in unique(data$request))
{
  # print(r)
  requests = c(requests, r)
  total = sum(data$count[data$request == r])
  req_counts = c(req_counts, total)
}

requests = c(requests, "Average")
req_counts = c(req_counts, sum(req_counts))

base_df = cbind(requests, req_counts)
base_df = data.frame(base_df)
base_df

requests_approx = c()
req_counts_approx = c()

for (r in unique(data_approx$request))
{
  # print(r)
  requests_approx = c(requests_approx, r)
  total = sum(data_approx$count[data_approx$request == r])
  req_counts_approx = c(req_counts_approx, total)
}

requests_approx = c(requests_approx, "Average")
req_counts_approx = c(req_counts_approx, sum(req_counts_approx))

approx_df = cbind(requests_approx, req_counts_approx)
approx_df = data.frame(approx_df)
approx_df




colnames(approx_df) = colnames(base_df)
merged_df = merge(base_df, approx_df, by = "requests", all = TRUE)

print(merged_df)

merged_df$ratio = as.numeric(levels(merged_df$req_counts.y)[merged_df$req_counts.y])/ as.numeric(levels(merged_df$req_counts.x)[merged_df$req_counts.x])

merged_df$ratio[is.na(merged_df$ratio)] = 0
merged_df = merged_df[merged_df$requests != "Store", ]
merged_df = merged_df[merged_df$requests != "Load", ]

print(merged_df)

pp <- ggplot(merged_df, aes(requests, ratio)) +
  geom_col() +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = "top",
    legend.text=element_text(size=11),
    axis.title.x = element_text(size=11),
    axis.title.y = element_text(size=11),
    axis.text.x  = element_text(angle=60, vjust=0.6, size=11),
    axis.text.y  = element_text(size=11)
  )


# pdf("coherence_traffic_saved.pdf", width=8, height=3)
print(pp)
# dev.off()
# pdf(file = "L1_requests.pdf")
# pie(req_counts, labels = requests, main="L1 Coherence Requests", cex=1.8)
# dev.off()

# data = data[order(-data$count),]

data$percent <- format(round(data$percent,2), nsmall = 2)

# print(data)

write.csv(data, file = "L1_coherence.csv", row.names=FALSE)

