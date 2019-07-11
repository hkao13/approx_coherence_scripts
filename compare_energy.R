library(ggplot2)
library(reshape2)

processFile = function(filepath) {
  con = file(filepath, "r")
  
  sim_seconds = 0
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
  
  l2_read_hits = c()
  l2_read_misses = c()
  l2_write_hits = c()
  l2_write_misses = c()
  
  mem_reads = c()
  mem_read_bytes = c()
  mem_read_bw = c()
  mem_writes = c()
  mem_write_bytes = c()
  mem_write_bw = c()
  
  l1_data = c()
  l1_exclusive_data = c()
  
  writeback_clean_data = c()
  writeback_dirty_data = c()
  
  while ( TRUE ) {
    line = readLines(con, n = 1)
    if ( length(line) == 0 ) {
      break
    }
    
    if (grepl(".*End Simulation Statistics.*", line))
    {
      # print(line)
      break
    }
    
    if (grepl(".*sim_seconds.*", line))
    {
      # print(line)
      matches <- regmatches(line, gregexpr("([0-9]*\\.[0-9]+|[0-9]+)", line))
      sim_seconds = as.numeric(unlist(matches))[1]
    }
    
    if (grepl(".*mem_ctrls[0-9]+\\.num_reads::total.*", line)) {
      # print(line)
      matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
      miss = as.numeric(unlist(matches))[2]
      mem_reads = c(mem_reads, miss)
      # print(miss)
    }
    
    if (grepl(".*mem_ctrls[0-9]+\\.bytes_read::total.*", line)) {
      # print(line)
      matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
      bytes = as.numeric(unlist(matches))[2]
      mem_read_bytes = c(mem_read_bytes, bytes)
      # print(miss)
    }
    
    if (grepl(".*mem_ctrls[0-9]+\\.bw_read::total.*", line)) {
      # print(line)
      matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
      bw = as.numeric(unlist(matches))[2]
      mem_read_bw = c(mem_read_bw, bw)
      # print(miss)
    }
    
    if (grepl(".*mem_ctrls[0-9]+\\.num_writes::total.*", line)) {
      # print(line)
      matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
      miss = as.numeric(unlist(matches))[2]
      mem_writes = c(mem_writes, miss)
      # print(miss)
    }
    
    if (grepl(".*mem_ctrls[0-9]+\\.bytes_written::total.*", line)) {
      # print(line)
      matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
      bytes = as.numeric(unlist(matches))[2]
      mem_write_bytes = c(mem_write_bytes, bytes)
      # print(miss)
    }
    
    if (grepl(".*mem_ctrls[0-9]+\\.bw_write::total.*", line)) {
      # print(line)
      matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
      bw = as.numeric(unlist(matches))[2]
      mem_write_bw = c(mem_write_bw, bw)
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
    
    if (grepl(".*l1_cntrl[0-9]+\\.L1Dcache.read_hits\\s.*", line)) {
      #print(line)
      matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
      miss = as.numeric(unlist(matches))[4]
      read_hits = c(read_hits, miss)
      #print(miss)
    }
    
    if (grepl(".*l1_cntrl[0-9]+\\.L1Dcache.read_misses\\s.*", line)) {
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
    
    if (grepl(".*l1_cntrl[0-9]+\\.L1Dcache.write_hits\\s.*", line)) {
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
    
    if (grepl(".*l1_cntrl[0-9]+\\.L1Dcache.write_misses\\s.*", line)) {
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
    
    if (grepl(".*l2_cntrl[0-9]+\\.L2cache.read_hits\\s.*", line)) {
      # print(line)
      matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
      miss = as.numeric(unlist(matches))[4]
      l2_read_hits = c(l2_read_hits, miss)
      # print(miss)
    }
    
    if (grepl(".*l2_cntrl[0-9]+\\.L2cache.read_misses\\s.*", line)) {
      # print(line)
      matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
      miss = as.numeric(unlist(matches))[4]
      l2_read_misses = c(l2_read_misses, miss)
      # print(miss)
    }
    
    if (grepl(".*l2_cntrl[0-9]+\\.L2cache.write_hits\\s.*", line)) {
      # print(line)
      matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
      miss = as.numeric(unlist(matches))[4]
      l2_write_hits = c(l2_write_hits, miss)
      # print(miss)
    }
    
    if (grepl(".*l2_cntrl[0-9]+\\.L2cache.write_misses\\s.*", line)) {
      # print(line)
      matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
      miss = as.numeric(unlist(matches))[4]
      l2_write_misses = c(l2_write_misses, miss)
      # print(miss)
    }
    
    
    if (grepl(".*system.ruby.L1Cache_Controller.Data::total\\s.*", line)) {
      # print(line)
      matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
      val = as.numeric(unlist(matches))[2]
      l1_data = c(l1_data, val)
    }
    
    if (grepl(".*system.ruby.L1Cache_Controller.Exclusive_Data::total\\s.*", line)) {
      # print(line)
      matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
      val = as.numeric(unlist(matches))[2]
      l1_exclusive_data = c(l1_exclusive_data, val)
    } 
    
    if (grepl(".*system.ruby.L2Cache_Controller.L1_WBCLEANDATA\\s.*", line)) {
      # print(line)
      matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
      val = as.numeric(unlist(matches))[3]
      writeback_clean_data = c(writeback_clean_data, val)
    } 
    
    if (grepl(".*system.ruby.L2Cache_Controller.L1_WBDIRTYDATA\\s.*", line)) {
      # print(line)
      matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
      val = as.numeric(unlist(matches))[3]
      writeback_dirty_data = c(writeback_dirty_data, val)
    }
    
  }
  
  dframe = data.frame(
    sim_seconds = sim_seconds,
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
    
    l2_read_hits=sum(l2_read_hits),
    l2_read_misses=sum(l2_read_misses),
    l2_write_hits=sum(l2_write_hits),
    l2_write_misses=sum(l2_write_misses),
    
    mem_reads = sum(mem_reads),
    mem_read_bytes = sum(mem_read_bytes),
    mem_read_bw = sum(mem_read_bw),
    mem_writes = sum(mem_writes),
    mem_write_bytes = sum(mem_write_bytes),
    mem_write_bw = sum(mem_write_bw),
    
    l1_data = sum(l1_data) + sum(l1_exclusive_data),
    writeback_data = sum(writeback_clean_data) + sum(writeback_dirty_data)
  )
  
  close(con)
  #print(dframe)
  #summed = sumData(dframe)
  return(dframe)
}

l1_cache_cpu_access_energy = function(data, read_energy, write_energy) {
    energy = (data$read_hits + data$read_misses) * (read_energy) + 
      (data$write_hits + data$write_hits_approx + data$write_misses) * (write_energy)
    
    return(energy)
}

l2_cache_cpu_access_energy = function(data, read_energy, write_energy) {
  energy = (data$l2_read_hits + data$l2_read_misses) * (read_energy) + 
    (data$l2_write_hits + data$l2_write_misses) * (write_energy)
  
  return(energy)
}

l1_coherence_data_energy = function(data, read_energy, write_energy) {
  energy = (data$l1_data) * (read_energy + write_energy)
}

writeback_data_energy = function(data, read_energy, write_energy) {
  energy = (data$writeback_data) * (read_energy + write_energy)
}

mem_access_energy = function(data, read_energy, write_energy) {
  # energy = (data$mem_reads) * read_energy * (data$mem_read_bytes) / (data$mem_read_bw) +
  #   (data$mem_writes) * write_energy * (data$mem_write_bytes) / (data$mem_write_bw)
  
  energy = (data$mem_reads) * read_energy +
    (data$mem_writes) * write_energy
}

# ----------- CACTI POWER VALUES --------------------

# These values are in nJ
l1_read_energy = 0.0338659
l1_write_energy = 0.0333791

l2_read_energy =  0.200211
l2_write_energy = 0.216801

# # DRAM power in (mW) from Mircron Power Sheet
# mem_read_power = 33.8 * 10^6
# mem_write_power = 37.0 * 10^6

# DRAM energy in (nJ) from Cacti
mem_read_power = 1.87983
mem_write_power = 1.87986


# ---------------- STATS FILES ------------------

# benchmark = "~/gem5-nej/"
benchmark = "~/cluster_results/oracle_study_128L2/sgemm"
# baseline_file = paste(benchmark, "base/m5out/stats.txt", sep="/")
baseline_file = "~/gem5-nej/m5out/base.txt"

benchmark = "~/cluster_results/oracle_study_128L2/sgemm"
# approx_file = paste(benchmark, "100/m5out/stats.txt", sep="/")
approx_file = "~/gem5-nej/m5out/stats.txt"
baseline_data = processFile(baseline_file)
approx_data = processFile(approx_file)


# benchmark = "linear_regression"
# path = "~/cluster_results/store_reset_study"
# baseline_file = paste(benchmark, "2/m5out/stats.txt", sep="/")
# approx_file = paste(benchmark, "8/m5out/stats.txt", sep="/")
# 
# file = paste(path, baseline_file, sep="/")
# baseline_data = processFile(file)
# 
# file = paste(path, approx_file, sep="/")
# approx_data = processFile(file)

print(baseline_data)
print(approx_data)

# scaling_factor = (baseline_data$demand_accesses) / approx_data$demand_accesses
# 
# approx_data = approx_data * (scaling_factor)

print(baseline_data)
print(approx_data)

print(approx_data / baseline_data)


# --------- COMPUTE ENERGY -----------

l1_cache_access_energy_base = l1_cache_cpu_access_energy(baseline_data, l1_read_energy, l1_write_energy)
l2_cache_access_energy_base = l2_cache_cpu_access_energy(baseline_data, l2_read_energy, l2_write_energy)
coherence_data_energy_base = l1_coherence_data_energy(baseline_data, l1_read_energy, l1_write_energy)
writeback_data_energy_base = writeback_data_energy(baseline_data, l1_read_energy, l2_write_energy)
dram_access_energy_base = mem_access_energy(baseline_data, mem_read_power, mem_write_power)


l1_cache_access_energy_approx = l1_cache_cpu_access_energy(approx_data, l1_read_energy, l1_write_energy)
l2_cache_access_energy_approx = l2_cache_cpu_access_energy(approx_data, l2_read_energy, l2_write_energy)
coherence_data_energy_approx = l1_coherence_data_energy(approx_data, l1_read_energy, l1_write_energy)
writeback_data_energy_approx = writeback_data_energy(approx_data, l1_read_energy, l2_write_energy)
dram_access_energy_approx = mem_access_energy(approx_data, mem_read_power, mem_write_power)

baseline_energy_total = l1_cache_access_energy_base + l2_cache_access_energy_base + coherence_data_energy_base + writeback_data_energy_base
baseline_energy_total
dram_access_energy_base

approx_energy_total = l1_cache_access_energy_approx + l2_cache_access_energy_approx + coherence_data_energy_approx + writeback_data_energy_approx
approx_energy_total

energy_reduction = (1 - approx_energy_total/baseline_energy_total)*100

mem_energy_reduction = (1 - dram_access_energy_approx/dram_access_energy_base)*100

baseline_total = baseline_energy_total + dram_access_energy_base
approx_total = approx_energy_total + dram_access_energy_approx
total_reduction = 1 - approx_total/baseline_total

cat("Cache Energy Reduction (%): ", energy_reduction)
cat("DRAM Energy Reduction (%): ", mem_energy_reduction)
cat("Total Energy Reduction (%): ", 100*total_reduction, "\n")
cat("\n")



# specie=c(rep("sorgho" , 3) , rep("poacee" , 3) , rep("banana" , 3) , rep("triticum" , 3) )
# condition=rep(c("normal" , "stress" , "Nitrogen") , 4)
# value=abs(rnorm(12 , 0 , 15))
# data=data.frame(specie,condition,value)
# print(data)




