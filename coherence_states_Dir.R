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
    
    if (grepl(".*system.ruby.Directory_Controller.[[:graph:]]+[.]{1}[[:graph:]]+.*", line)) {
      print(line)
      matches <- strsplit(line, "\\.|[[:blank:]]+")
      # matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
      
      # print(matches)
      
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
# file = "~/gem5-nej/m5out/stats.txt"
file = "~/cluster_results/approx_coherence_results/mri-gridding/base/m5out/stats.txt"
data = processFile(file)


unique(data$state)

states = c()
counts = c()
requests = c()

data$percent = 100*data$count / sum(data$count)

for (s in unique(data$state))
{
  print(s)
  states = c(states, s)
  total = sum(data$count[data$state == s])
  counts = c(counts, total)
}

req_counts = c()

# pdf(file = "L2_state.pdf")
pie(counts, labels = states, main="L2 Coherence States", cex=1.8)
# dev.off()

for (r in unique(data$request))
{
  print(r)
  requests = c(requests, r)
  total = sum(data$count[data$request == r])
  req_counts = c(req_counts, total)
}

requests
req_counts

# pdf(file = "L2_requests.pdf")
pie(req_counts, labels = requests, main="L2 Coherence Requests", cex=1.8)
# dev.off()

data = data[order(-data$count),]

data$percent <- format(round(data$percent,2), nsmall = 2)

print(data)

print(sum(data$count))

write.csv(data, file = "L2_coherence.csv", row.names=FALSE)

