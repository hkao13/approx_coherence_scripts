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
  
  ticks = c()
  
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
  
  
  l2_demand_hits = c()
  l2_demand_misses = c()
  l2_demand_accesses = c()
  
  mem_reads = c()
  mem_writes = c()
  
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
    
    if (grepl(".*mem_ctrls[0-9]+\\.num_reads::total.*", line)) {
      # print(line)
      matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
      miss = as.numeric(unlist(matches))[2]
      mem_reads = c(mem_reads, miss)
       # print(miss)
    }
    
    if (grepl(".*mem_ctrls[0-9]+\\.num_writes::total.*", line)) {
      # print(line)
      matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
      miss = as.numeric(unlist(matches))[2]
      mem_writes = c(mem_writes, miss)
      # print(miss)
    }
    
    if (grepl("^sim_ticks.*", line)) {
      # print(line)
      matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
      sim_ticks = as.numeric(unlist(matches))[1]
      ticks = c(ticks, sim_ticks)
    }
    
    if (grepl(".*l1_cntrl[0-9]+\\.L1Dcache.demand_hits.*", line)) {
      # print(line)
      matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
      miss = as.numeric(unlist(matches))[4]
      demand_hits = c(demand_hits, miss)
      # print(miss)
    }
    
    if (grepl(".*l1_cntrl[0-9]+\\.L1Dcache.demand_misses.*", line)) {
      # print(line)
      matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
      miss = as.numeric(unlist(matches))[4]
      demand_misses = c(demand_misses, miss)
      # print(miss)
    }
    
    if (grepl(".*l1_cntrl[0-9]+\\.L1Dcache.demand_accesses.*", line)) {
      #print(line)
      matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
      miss = as.numeric(unlist(matches))[4]
      demand_accesses = c(demand_accesses, miss)
      #print(miss)
    }
    
    if (grepl(".*l1_cntrl[0-9]+\\.L1Dcache.read_hits.*", line)) {
      #print(line)
      matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
      miss = as.numeric(unlist(matches))[4]
      read_hits = c(read_hits, miss)
      #print(miss)
    }
    
    if (grepl(".*l1_cntrl[0-9]+\\.L1Dcache.read_misses .*", line)) {
      #print(line)
      matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
      miss = as.numeric(unlist(matches))[4]
      read_misses = c(read_misses, miss)
      #print(miss)
    }
    
    if (grepl(".*l1_cntrl[0-9]+\\.L1Dcache.read_misses_coherence.*", line)) {
      #print(line)
      matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
      miss = as.numeric(unlist(matches))[4]
      read_misses_coherence = c(read_misses_coherence, miss)
      #print(miss)
    }
    
    if (grepl(".*l1_cntrl[0-9]+\\.L1Dcache.write_hits.*", line)) {
      #print(line)
      matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
      miss = as.numeric(unlist(matches))[4]
      write_hits = c(write_hits, miss)
      #print(miss)
    }
    
    if (grepl(".*l1_cntrl[0-9]+\\.L1Dcache.write_hits_approx.*", line)) {
      #print(line)
      matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
      miss = as.numeric(unlist(matches))[4]
      write_hits_approx = c(write_hits_approx, miss)
      #print(miss)
    }
    
    if (grepl(".*l1_cntrl[0-9]+\\.L1Dcache.write_misses .*", line)) {
      #print(line)
      matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
      miss = as.numeric(unlist(matches))[4]
      write_misses = c(write_misses, miss)
      #print(miss)
    }
    
    if (grepl(".*l1_cntrl[0-9]+\\.L1Dcache.write_misses_coherence.*", line)) {
      # print(line)
      matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
      miss = as.numeric(unlist(matches))[4]
      write_misses_coherence = c(write_misses_coherence, miss)
      # print(miss)
    }
    
    
    
    
    
    
    if (grepl(".*l2_cntrl[0-9]+\\.L2cache.demand_hits.*", line)) {
       # print(line)
      matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
      miss = as.numeric(unlist(matches))[4]
      l2_demand_hits = c(l2_demand_hits, miss)
      # print(miss)
    }
    
    if (grepl(".*l2_cntrl[0-9]+\\.L2cache.demand_misses.*", line)) {
      # print(line)
      matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
      miss = as.numeric(unlist(matches))[4]
      l2_demand_misses = c(l2_demand_misses, miss)
      # print(miss)
    }
    
    if (grepl(".*l2_cntrl[0-9]+\\.L2cache.demand_accesses.*", line)) {
      #print(line)
      matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
      miss = as.numeric(unlist(matches))[4]
      l2_demand_accesses = c(l2_demand_accesses, miss)
      #print(miss)
    }
    
    
    
    
    
    
    
    
  }
  
  dframe = data.frame(
    ticks=sum(ticks),
    demand_hits=sum(demand_hits),
    demand_misses=sum(demand_misses),
    demand_accesses=sum(demand_accesses),
    read_hits=sum(read_hits),
    read_misses=sum(read_misses),
    read_misses_coherence=sum(read_misses_coherence),
    
    write_hits=sum(write_hits),
    write_hits_approx=sum(write_hits_approx),
    write_misses=sum(write_misses),
    write_misses_coherence=sum(write_misses_coherence),
    
    l2_demand_hits=sum(l2_demand_hits),
    l2_demand_misses=sum(l2_demand_misses),
    l2_demand_accesses=sum(l2_demand_accesses),
    
    mem_reads = sum(mem_reads),
    mem_writes = sum(mem_writes)
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


# ----------- CACTI POWER VALUES --------------------

# These values are in nJ
l1_read_energy = 0.0581365
l1_write_energy = 0.0822679

l2_read_energy =  0.430608
l2_write_energy = 0.463798


# ---------------- STATS FILES ------------------
file = "~/gem5-nej/m5out/base.txt"
# file = "~/cluster_results/results/linear_regression/m5out/stats.txt"
baseline_data = processFile(file)

file = "~/gem5-nej/m5out/stats.txt"
# file = "~/cluster_results/results_approx_0/linear_regression/m5out/stats.txt"
approx_data = processFile(file)

print(baseline_data)
print(approx_data)
print(approx_data / baseline_data)


# ---------- COMPUTE SPEEDUP ---------
# speedup = exec time old / exec time new
speedup = (baseline_data$ticks/approx_data$ticks - 1)*100

cat("Baseline cycles: ", baseline_data$ticks)
cat("Enhanced cycles: ", approx_data$ticks)
cat("Speedup (%): ", speedup)

# --------- COMPUTE ENERGY -----------

l1_energy_baseline = (baseline_data$read_hits + baseline_data$read_misses) * (l1_read_energy) + 
  (baseline_data$write_hits + baseline_data$write_hits_approx + baseline_data$write_misses) * (l1_write_energy)
# l2 energy is wrong right now. Update once the benchmarks finish with the read/write stats
l2_energy_baseline = (baseline_data$demand_hits + baseline_data$demand_misses) * (l1_read_energy) 

baseline_energy_total = l1_energy_baseline + l2_energy_baseline
baseline_energy_total

l1_energy_approx = (approx_data$read_hits + approx_data$read_misses) * (l1_read_energy) + 
  (approx_data$write_hits + approx_data$write_hits_approx + approx_data$write_misses) * (l1_write_energy)
# l2 energy is wrong right now. Update once the benchmarks finish with the read/write stats
l2_energy_approx = (approx_data$demand_hits + approx_data$demand_misses) * (l1_read_energy)

approx_energy_total = l1_energy_approx + l2_energy_approx
approx_energy_total

energy_reduction = (1 - approx_energy_total/baseline_energy_total)*100

cat("Energy Reduction (%): ", energy_reduction)




