library(ggplot2)
library(reshape2)
library(tidyr)
# library(dplyr)

options(scipen=999)

processFile = function(filepath) {
  con = file(filepath, "r")
  
  state_vec = c()
  transition_vec = c()
  count_vec = c()
  
  getx_vec = c()
  gets_vec = c()
  data_vec = c()
  other_vec = c()
  
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
    
    if (grepl(".*system.ruby.L1Cache_Controller.[[:graph:]]+[.]{1}[[:graph:]]+::total.*", line)) {
      # print(line)
      matches <- strsplit(line, "\\.|::total[\\s]*")
      # matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
      state = unlist(matches)[4]
      transition = unlist(matches)[5]
      count = as.numeric(unlist(matches))[6]
      
      if (grepl('GETX', transition))
      {
        # print(transition)
        getx_vec = c(getx_vec, count)
      }

      else if (grepl('GETS', transition))
      {
        # print(transition)
        gets_vec = c(gets_vec, count)
      }
      
      else if (grepl('^Data', transition))
      {
        # print(transition)
        data_vec = c(data_vec, count)
      }
      
      else if (grepl('^Exclusive_Data', transition))
      {
        # print(transition)
        data_vec = c(data_vec, count)
      }
      
      else if (grepl('Ifetch', transition))
      {
        # Do Nothing
        
      }
      
      else if (grepl('Load', transition))
      {
        # Do Nothing
        
      }
      
      else if (grepl('Store', transition))
      {
        # Do Nothing
      }
      
      else if (grepl('Replacement', transition))
      {
        # Do Nothing
      }
      
      else if (grepl('Timeout', transition))
      {
        # Do Nothing
      }
      
      # else 
      # {
      #   # print(transition)
      #   other_vec = c(other_vec, count)
      # }
      
    }
    
    
    if (grepl(".*system.ruby.L2Cache_Controller.[[:graph:]]+[.]{1}[[:graph:]]+::total.*", line)) {
      # print(line)
      matches <- strsplit(line, "\\.|::total[\\s]*")
      # matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
      state = unlist(matches)[4]
      transition = unlist(matches)[5]
      count = as.numeric(unlist(matches))[6]
      
      if (grepl('GETX', transition))
      {
        # print(transition)
        getx_vec = c(getx_vec, count)
      }

      else if (grepl('GETS', transition))
      {
        # print(transition)
        gets_vec = c(gets_vec, count)
      }
      
      else if (grepl('^L1_WBCLEANDATA', transition))
      {
        
        data_vec = c(data_vec, count)
      }
      
      else if (grepl('^L1_WBDIRTYDATA', transition))
      {
        
        data_vec = c(data_vec, count)
      }
      
      
      else if (grepl('Replacement', transition))
      {
        # Do Nothing
      }
      
      else 
      {
        # print(transition)
        other_vec = c(other_vec, count)
      }
      
    }
    
    dframe = data.frame(
      getx = sum(getx_vec),
      gets = sum(gets_vec),
      data = sum(data_vec),
      other = sum(other_vec)
    )
    
    
  }
  
  close(con)
  return(dframe)
}

process_traffic = function(benchmark_name, baseline_data, approx_data, source_name)
{
  total = sum(baseline_data)
  
  normalized = approx_data/total
  
  
  cat("Total Reduction (%): ", 100*(1 - sum(approx_data)/total), "\n")

  
  if (benchmark_name == "linear_regression")
  {
    benchmark_name = "linear"
  }
  
  if (benchmark_name == "histogram")
  {
    benchmark_name = "hist"
  }
  
  if (benchmark_name == "conv-3d")
  {
    benchmark_name = "conv"
  }
  
  if (benchmark_name == "fdtd-apml")
  {
    benchmark_name = "fdtd"
  }
  
  if (benchmark_name == "jacobi-2d")
  {
    benchmark_name = "jacobi"
  }
  
  if (benchmark_name == "seidel-2d")
  {
    benchmark_name = "seidel"
  }
  
  benchmark = c(benchmark_name)
  source = rep(source_name)
  getx = c(normalized$getx)
  gets = c(normalized$gets)
  data = c(normalized$data)
  other = c(normalized$other)
  
  df = data.frame(benchmark, source, getx, gets, data, other)
  # 
  # return(data)
  
  
}


path = "~/cluster_results/store_reset_study_hammer"

# benchmark_names = c("linear_regression", "histogram", "pca", "histo", "mri-gridding", "mri-q", "sgemm", "spmv", "tpacf", "adi", "conv-2d", "conv-3d", "fdtd-2d", "fdtd-apml", "jacobi-1d", "jacobi-2d", "seidel-2d")
# benchmark_names = c("linear_regression", "histogram", "pca", "histo", "mri-gridding", "mri-q", "sgemm", "spmv", "tpacf")
benchmark_names = c("adi")
# benchmark_names = c("linear_regression", "histogram")
# benchmark_names = c("linear_regression", "histogram", "pca", "adi", "fdtd-2d", "fdtd-apml", "jacobi-2d", "seidel-2d")
# benchmark_names = c("histogram", "linear_regression",  "pca", "adi", "conv-3d", "fdtd-apml", "jacobi-2d", "seidel-2d")

all_data=data.frame(benchmark = c(),
                    source = c(),
                    getx = c(),
                    gets = c(),
                    data = c(),
                    other = c()
)

for (benchmark in benchmark_names)
{
  
  cat("Processing Benchmark: ", benchmark, "\n")
  
  baseline_file = paste(benchmark, "base/m5out/stats.txt", sep="/")
  file = paste(path, baseline_file, sep="/")
  baseline_data = processFile(file)
  
  data = process_traffic(benchmark, baseline_data, baseline_data, "0")
  all_data = rbind(all_data, data)
  
  # -----------------------------------------------

  approx_file = paste(benchmark, "2/m5out/stats.txt", sep="/")
  file = paste(path, approx_file, sep="/")
  approx_data = processFile(file)
  # print(approx_data)

  data = process_traffic(benchmark, baseline_data, approx_data, "2")
  all_data = rbind(all_data, data)

  # -----------------------------------------------

  approx_file = paste(benchmark, "4/m5out/stats.txt", sep="/")
  file = paste(path, approx_file, sep="/")
  approx_data = processFile(file)

  data = process_traffic(benchmark, baseline_data, approx_data, "4")
  all_data = rbind(all_data, data)

  # -----------------------------------------------

  approx_file = paste(benchmark, "8/m5out/stats.txt", sep="/")
  file = paste(path, approx_file, sep="/")
  approx_data = processFile(file)

  data = process_traffic(benchmark, baseline_data, approx_data, "8")
  all_data = rbind(all_data, data)
}


# Calculate the Average
benchmark = c(rep("avg", 4))
source = rep(c("0", "2", "4", "8"), 1)
getx = c(
  mean(all_data$getx[all_data$source == "0"]),
  mean(all_data$getx[all_data$source == "2"]),
  mean(all_data$getx[all_data$source == "4"]),
  mean(all_data$getx[all_data$source == "8"])
)
gets = c(
  mean(all_data$gets[all_data$source == "0"]),
  mean(all_data$gets[all_data$source == "2"]),
  mean(all_data$gets[all_data$source == "4"]),
  mean(all_data$gets[all_data$source == "8"])
)
data = c(
  mean(all_data$data[all_data$source == "0"]),
  mean(all_data$data[all_data$source == "2"]),
  mean(all_data$data[all_data$source == "4"]),
  mean(all_data$data[all_data$source == "8"])
)
other = c(
  mean(all_data$other[all_data$source == "0"]),
  mean(all_data$other[all_data$source == "2"]),
  mean(all_data$other[all_data$source == "4"]),
  mean(all_data$other[all_data$source == "8"])
)
data = data.frame(benchmark, source, getx, gets, data, other)
all_data = rbind(all_data, data)

base_avg = all_data[all_data$benchmark=="avg" & all_data$source=="0" , c(3,4,5,6) ]
avg_2 = all_data[all_data$benchmark=="avg" & all_data$source=="2" , c(3,4,5,6) ]
avg_4 = all_data[all_data$benchmark=="avg" & all_data$source=="4" , c(3,4,5,6) ]
avg_8 = all_data[all_data$benchmark=="avg" & all_data$source=="8" , c(3,4,5,6) ]

process_traffic("none", base_avg, avg_2, "none")
process_traffic("none", base_avg, avg_4, "none")
process_traffic("none", base_avg, avg_8, "none")


all_data2 <- all_data %>% gather(type, value, "other", "data", "gets", "getx")
all_data2$type = factor(all_data2$type, levels = all_data2$type)

plot = ggplot(all_data2, aes(x=source, y = value, fill=type)) +   
  geom_bar(position="stack", stat="identity", color="black", width=1) +
  facet_grid(~benchmark, space = "free_x") +
  theme_bw() +
  ylab("Normalized\nCoherence Traffic") +
  xlab("N") + 
  theme(
    legend.title = element_blank(),
    legend.position = "top",
    legend.text=element_text(size=22),
    axis.title.x = element_text(size=22),
    axis.title.y = element_text(size=22),
    axis.text.x  = element_text(angle=0, size=18),
    axis.text.y  = element_text(size=22),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    
    strip.text.x = element_text(size=18, face="bold", angle=0, hjust=0.5),
    strip.background = element_blank(),
    panel.border = element_rect(colour = "black")
  ) +
  coord_cartesian(expand = FALSE, clip = "off") +
  scale_fill_brewer(palette = "Blues", labels = c(" Other ", " Data ", " GetS ", " GetX "))

# print(plot)
# pdf("coherence_saving.pdf", width=9, height=4.5)
print(plot)
# dev.off()

