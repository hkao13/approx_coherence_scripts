library(ggplot2)
library(reshape2)


processFile = function(filepath) {
  con = file(filepath, "r")
  
  ticks = 0
  
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
    
    if (grepl("^sim_ticks.*", line)) {
      print(line)
      matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
      ticks = as.numeric(unlist(matches))[1]
    }
    
  }
  
  close(con)
  return(ticks)
}


# ---------------- STATS FILES ------------------

benchmark = "mri-q"

baseline_file = paste(benchmark, "base/m5out/stats.txt", sep="/")
approx_file = paste(benchmark, "2/m5out/stats.txt", sep="/")




path = "~/cluster_results/oracle_study"
file = paste(path, baseline_file, sep="/")
file = "~/gem5-nej/m5out/base.txt"
ticks_baseline = processFile(file)

path = "~/cluster_results/store_reset_study"
file = paste(path, approx_file, sep="/")
file = "~/gem5-nej/m5out/stats.txt"
ticks_approx = processFile(file)


# --------- COMPUTE SPEEDUP -----------

speedup = (ticks_baseline/ticks_approx - 1)*100

cat("Speedup (%): ", speedup)

