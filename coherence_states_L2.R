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
    
    if (grepl(".*system.ruby.L2Cache_Controller.[[:graph:]]+[.]{1}[[:graph:]]+::total.*", line)) {
      print(line)
      matches <- strsplit(line, "\\.|::total[\\s]*")
      # matches <- regmatches(line, gregexpr("[[:digit:]]+", line))

      # print(matches)

      state = unlist(matches)[4]
      transition = unlist(matches)[5]
      count = as.numeric(unlist(matches))[6]

      state_vec = c(state_vec, state)
      transition_vec = c(transition_vec, transition)
      count_vec = c(count_vec, count)
    }
    
    
    # if (grepl(".*system.ruby.L2Cache_Controller.[[:graph:]]+[.]{1}[[:graph:]].*", line)) {
    #   print(line)
    #   matches <- strsplit(line, "\\.|[[:space:]]+")
    #   # matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
    #   
    #   print(matches)
    #   
    #   state = unlist(matches)[4]
    #   transition = unlist(matches)[5]
    #   count = as.numeric(unlist(matches))[6]
    #   
    #   
    #   print(transition)
    #   
    #   state_vec = c(state_vec, state)
    #   transition_vec = c(transition_vec, transition)
    #   count_vec = c(count_vec, count)
    # }
    
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

# benchmark = "jacobi-2d"
# file = paste("~/cluster_results/store_reset_study/", benchmark, "/base/m5out/stats.txt", sep="")
# file = paste("~/cluster_results/hammer_test/", benchmark, "/base/m5out/stats.txt", sep="")

# file = "~/cluster_results/CRONO_test/bc/CA/m5out/stats.txt"
# file = "~/cluster_results/results_parboil/tpacf/m5out/stats.txt"
# file = "~/cluster_results/approx_coherence_results/mri-q/base/m5out/stats.txt"
# file = "~/cluster_results/hammer_test/histogram/base/m5out/stats.txt"
# file = "~/cluster_results/polybench_small_store_count_8/conv-2d/m5out/stats.txt"
# file = "~/cluster_results/stats.txt"
# file = "~/cluster_results/store_reset_study/histogram/base/m5out/stats.txt"
# file = "~/gem5-nej/m5out/stats.txt"
file = "~/cluster_results/phoenix_MESI/8core/linear_regression/base/m5out/stats.txt"
data = processFile(file)


states = c()
counts = c()
requests = c()


# -----------------------------------
gets_df = data[data$request == "L1_GETS", ]
sum_gets = sum(gets_df$count)
gets_df$percent = 100*gets_df$count/ sum_gets
print(sum_gets)
# -----------------------------------

# -----------------------------------
getx_df = data[data$request == "L1_GETX", ]
sum_getx = sum(getx_df$count)
getx_df$percent = 100*getx_df$count/ sum_getx
print(sum_getx)
# -----------------------------------

# -----------------------------------
replacement_df = data[data$request == "L1_Replacement", ]
sum_replacement = sum(replacement_df$count)
replacement_df$percent = 100*replacement_df$count/ sum_replacement
print(sum_replacement)
# -----------------------------------



# Remove Ifetch request since we don't care about that.
data = data[data$request != "Ifetch", ]

data = data[data$request != "Store", ]
data = data[data$request != "Store_Approx", ]
data = data[data$request != "Load", ]
data = data[data$request != "Use_Timeout", ]

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
# pie1 = pie(counts, labels = states, main="L1 Coherence States", cex=1.8)
# dev.off()

req_counts = c()

for (r in unique(data$request))
{
  # print(r)
  requests = c(requests, r)
  total = sum(data$count[data$request == r])
  req_counts = c(req_counts, total)
}


# pdf(file = "L1_requests.pdf")

request_df = data.frame(requests = requests, counts = req_counts)

bp1 <- ggplot(request_df, aes(x=requests, y=counts))+
  geom_bar(stat = "identity", color="black", width=0.5) +
  ggtitle("Requests to L1") +
  ylab("Count") +
  theme_bw() +
  theme(
    legend.position = "top",
    legend.text=element_text(size=22),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=12),
    axis.text.x  = element_text(angle=90, hjust=1,vjust=0.2, size=12),
    axis.text.y  = element_text(size=12),
    panel.grid.major.y = element_line(colour = "black"),
    panel.grid.major.x = element_blank()
  ) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE))
# bp1



# pie2 <- pie(req_counts, labels = requests, main="L1 Coherence Requests", cex=1.2)
# dev.off()

# data = data[order(-data$count),]

data$percent <- format(round(data$percent,2), nsmall = 2)

print(data)
print(gets_df)
print(getx_df)


bp4 <- ggplot(gets_df, aes(x=state, y=percent, fill=request))+
  geom_bar(stat = "identity", color="black", width=0.5) +
  ylab("Percent(%)") +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = "top",
    legend.text=element_text(size=22),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=14),
    axis.text.x  = element_text(angle=90, hjust=1,vjust=0.2, size=14),
    axis.text.y  = element_text(size=14),
    panel.grid.major.y = element_line(colour = "black"),
    panel.grid.major.x = element_blank()
  ) +
  scale_fill_brewer(palette = "Blues", labels=c(" L1_GETS "))
# bp4

bp5 <- ggplot(getx_df, aes(x=state, y=percent, fill=request))+
  geom_bar(stat = "identity", color="black", width=0.5) +
  ylab("Percent(%)") +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = "top",
    legend.text=element_text(size=22),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=14),
    axis.text.x  = element_text(angle=90, hjust=1,vjust=0.2, size=14),
    axis.text.y  = element_text(size=14),
    panel.grid.major.y = element_line(colour = "black"),
    panel.grid.major.x = element_blank()
  ) +
  scale_fill_brewer(palette = "Blues", labels=c(" L1_GETX "))


plot_list <- list()
plot_list[[1]] <- bp1
plot_list[[2]] = bp4
plot_list[[3]] = bp5
# 
ml1 <- marrangeGrob(plot_list, nrow = 1, ncol = 3)

# pdf(paste(benchmark, "_coherenceL2.pdf", sep=""), width=9, height=4.5)
print(ml1)
# dev.off()
# write.csv(data, file = "L1_coherence.csv", row.names=FALSE)

