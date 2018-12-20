library(ggplot2)
library(reshape2)
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
file = "~/cluster_results/approx_coherence_results/pca/base/m5out/stats.txt"
data = processFile(file)

file = "~/cluster_results/approx_coherence_results/pca/10/m5out/stats.txt"
data_approx = processFile(file)

states = c()
counts = c()
requests = c()

# Remove Ifetch request since we don't care about that.
data = data[data$request != "Ifetch", ]
data_approx = data_approx[data_approx$request != "Ifetch", ]
data_approx = data_approx[data_approx$request != "Store_Approx", ]

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
pie(counts, labels = states, main="L1 Coherence States", cex=1.8)
# dev.off()

req_counts = c()

for (r in unique(data$request))
{
  # print(r)
  requests = c(requests, r)
  total = sum(data$count[data$request == r])
  req_counts = c(req_counts, total)
}





requests_approx = c()
req_counts_approx = c()

for (r in unique(data_approx$request))
{
  # print(r)
  requests_approx = c(requests_approx, r)
  total = sum(data_approx$count[data_approx$request == r])
  req_counts_approx = c(req_counts_approx, total)
}

requests
req_counts

requests_approx
req_counts_approx



requests
req_counts_approx / req_counts





# pdf(file = "L1_requests.pdf")
pie(req_counts, labels = requests, main="L1 Coherence Requests", cex=1.8)
# dev.off()

# data = data[order(-data$count),]

data$percent <- format(round(data$percent,2), nsmall = 2)

print(data)

write.csv(data, file = "L1_coherence.csv", row.names=FALSE)

