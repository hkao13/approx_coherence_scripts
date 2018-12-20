library(ggplot2)
library(reshape2)

processFile = function(filepath) {
  con = file(filepath, "r")
  
  
  hist_vec = c()
  
  counter = 0
  
  while ( TRUE ) {
    line = readLines(con, n = 1)
    if ( length(line) == 0 ) {
      break
    }
    
    # if (grepl("^.*serial", line)) {
    #   # cat("line: ", line, "\n")
    #   matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
    #   index = as.numeric(unlist(matches))[1]
    #   val = as.numeric(unlist(matches))[2]
    #   
    #   
    # }
    
    if (grepl("^[[:digit:]]+[\\s]?", line)) {
      # cat("line: ", line, "\n")
      matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
      index = as.numeric(unlist(matches))[1]
      val = as.numeric(unlist(matches))[2]
      
      matches <- regmatches(line, gregexpr("[[:lower:]]+", line))
      serial = as.character(unlist(matches))[1]
      
      
      # cat("index: ", index, counter, val, "\n")
      if (index == counter)
      {
        counter = counter + 1
        
        if (!is.na(serial))
        {
          val = NaN
        }
        # cat("line: ", line, val, "\n")
        hist_vec = c(hist_vec, val)
      }
      else
      {
        print(line)
      }
      
      if (counter == 256)
      {
        counter = 0
      }
    }
    
  }
  
  close(con)
  return(hist_vec)
}


# ---------------- STATS FILES ------------------

file = "~/cluster_results/results/histogram/m5out/system.pc.com_1.device"
baseline_out = processFile(file)

file = "~/cluster_results/results/histogram/m5out/system.pc.com_1.device"
approx_out = processFile(file)

# Both histogram output should have 256*3 = 768 output values
stopifnot(length(baseline_out) == 768)
stopifnot(length(approx_out) == 768)

percent_err = 100*abs((approx_out - baseline_out)/baseline_out)
percent_err[is.na(percent_err)] = 0
# covert dataframe to vector
err_vector = c(t(percent_err))

mean_err = mean(err_vector)

max_abs_err = max(abs(err_vector))

cat("Mean Absolute Percent Error: ", mean_err)
cat("Max Absolute Percent Error: ", max_abs_err)
