library(ggplot2)
library(reshape2)


processFile = function(filepath) {
  con = file(filepath, "r")
  
  ticks = 0

  flits_injected = 0
  
  num_routers = 0
  
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
    
    
    if (grepl(".*system.ruby.network.routers[0-9]+.buffer_reads.*", line)) {
      print(line)
      num_routers = num_routers + 1
      print(num_routers)
    }
    
    # if (grepl(".*system.ruby.network.pwrStateResidencyTicks::UNDEFINED.*", line)) {
    #   print(line)
    #   matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
    #   ticks = as.numeric(unlist(matches))[1]
    # }

    if (grepl("^sim_ticks.*", line)) {
      print(line)
      matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
      ticks = as.numeric(unlist(matches))[1]
    }
    
    
    if (grepl(".*system.ruby.network.flits_injected::total.*", line)) {
      # print(line)
      matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
      flits_injected = as.numeric(unlist(matches))[1]
      # print(flits)
    }
    
  }
  
  close(con)
  
  injection_rate = flits_injected/ticks
  return(injection_rate)
}


# ---------------- STATS FILES ------------------

benchmark = "spmv"
path = "~/cluster_results/oracle_study_128L2"
baseline_file = paste(benchmark, "base/m5out/stats.txt", sep="/")
approx_file = paste(benchmark, "100/m5out/stats.txt", sep="/")

# path = "~/cluster_results/"
file = paste(path, baseline_file, sep="/")
baseline_injection_rate = processFile(file)

# path = "~/cluster_results/"
file = paste(path, approx_file, sep="/")
approx_injection_rate = processFile(file)


# --------- COMPUTE ENERGY -----------

percent_reduction = (1 - (approx_injection_rate/baseline_injection_rate))*100

cat("Baseline injection rate: ", baseline_injection_rate)
cat("Approx injection rate: ", approx_injection_rate)
cat("Percent Reduction (%): ", percent_reduction)

