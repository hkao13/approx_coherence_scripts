library(ggplot2)
library(reshape2)

processFile = function(filepath) {
  con = file(filepath, "r")
  
  
  kmeans_vec = c()
  
  count = 0
  
  while ( TRUE ) {
    line = readLines(con, n = 1)
    if ( length(line) == 0 ) {
      break
    }
    

    
    # print(line)
    
    if (grepl("[:digit:][:cntrl:]", line)) {
      # print(line)
      matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
      print(matches)
      
    }
    
  }
  
  close(con)
  return(kmeans_vec)
}


# ---------------- STATS FILES ------------------

# file = "~/cluster_results/results/pca/m5out/system.pc.com_1.device"
# baseline_out = processFile(file)

file = "~/cluster_results/results_approx_0/pca/m5out/system.pc.com_1.device"
approx_out = processFile(file)

