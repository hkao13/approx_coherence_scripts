library(ggplot2)
library(reshape2)

processFile = function(filepath) {
  con = file(filepath, "r")
  
  
  kmeans_vec = c()
  
  while ( TRUE ) {
    line = readLines(con, n = 1)
    if ( length(line) == 0 ) {
      break
    }
    
    
    if (grepl("^\\s+[[:digit:]]", line)) {
      # cat("line: ", line, "\n")
      matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
      mean1 = as.numeric(unlist(matches))[1]
      mean2 = as.numeric(unlist(matches))[2]
      mean3 = as.numeric(unlist(matches))[3]
      kmeans_vec = c(kmeans_vec, mean1, mean2, mean3)
      
    }
    
  }
  
  close(con)
  return(kmeans_vec)
}


# ---------------- STATS FILES ------------------

file = "~/cluster_results/results/kmeans/m5out/system.pc.com_1.device"
baseline_out = processFile(file)

file = "~/cluster_results/results_approx_0/kmeans/m5out/system.pc.com_1.device"
approx_out = processFile(file)

# Both kmeans output should have 100*3 = 300 output values
stopifnot(length(baseline_out) == 300)
stopifnot(length(approx_out) == 300)

percent_err = 100*abs((approx_out - baseline_out)/baseline_out)
percent_err[is.na(percent_err)] = 0
# covert dataframe to vector
err_vector = c(t(percent_err))

mean_err = mean(err_vector)

max_abs_err = max(abs(err_vector))

cat("Mean Absolute Percent Error: ", mean_err)
cat("Max Absolute Percent Error: ", max_abs_err)
