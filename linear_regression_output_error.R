library(ggplot2)
library(reshape2)

processFile = function(filepath) {
  con = file(filepath, "r")
  
  a = c()
  b = c()
  
  xbar = c()
  ybar = c()
  
  r2 = c()
  
  sx = c()
  sy = c()
  
  sxx = c()
  syy = c()
  sxy = c()
  
  while ( TRUE ) {
    line = readLines(con, n = 1)
    if ( length(line) == 0 ) {
      break
    }
    
    if (grepl("^[\t]+a", line)) {
      print(line)
      matches <- regmatches(line, gregexpr("[[:digit:]]+[[:punct:]][[:digit:]]+", line))
      a_val = as.numeric(unlist(matches))[1]
      a = c(a, a_val)
    }
    
    if (grepl("^[\t]+b", line)) {
      print(line)
      matches <- regmatches(line, gregexpr("[[:digit:]]+[[:punct:]][[:digit:]]+", line))
      b_val = as.numeric(unlist(matches))[1]
      b = c(b, b_val)
    }
    
    if (grepl("^[\t]+xbar", line)) {
      print(line)
      matches <- regmatches(line, gregexpr("[[:digit:]]+[[:punct:]][[:digit:]]+", line))
      xbar_val = as.numeric(unlist(matches))[1]
      xbar = c(xbar, xbar_val)
    }
    
    if (grepl("^[\t]+ybar", line)) {
      print(line)
      matches <- regmatches(line, gregexpr("[[:digit:]]+[[:punct:]][[:digit:]]+", line))
      ybar_val = as.numeric(unlist(matches))[1]
      ybar = c(ybar, ybar_val)
    }
    
    if (grepl("^[\t]+r2", line)) {
      print(line)
      matches <- regmatches(line, gregexpr("[[:digit:]]+[[:punct:]][[:digit:]]+", line))
      r2_val = as.numeric(unlist(matches))[1]
      r2 = c(r2, r2_val)
    }
    
    if (grepl("^[\t]+(SX){1}\\s", line)) {
      print(line)
      matches <- regmatches(line, gregexpr("[[:digit:]]+[[:punct:]]?[[:digit:]]+", line))
      sx_val = as.numeric(unlist(matches))[1]
      sx = c(sx, sx_val)
    }
    
    if (grepl("^[\t]+(SY){1}\\s", line)) {
      print(line)
      matches <- regmatches(line, gregexpr("[[:digit:]]+[[:punct:]]?[[:digit:]]+", line))
      sy_val = as.numeric(unlist(matches))[1]
      sy = c(sy, sy_val)
    }
    
    if (grepl("^[\t]+(SXX){1}\\s", line)) {
      print(line)
      matches <- regmatches(line, gregexpr("[[:digit:]]+[[:punct:]]?[[:digit:]]+", line))
      sxx_val = as.numeric(unlist(matches))[1]
      sxx = c(sxx, sxx_val)
    }
    
    if (grepl("^[\t]+(SYY){1}\\s", line)) {
      print(line)
      matches <- regmatches(line, gregexpr("[[:digit:]]+[[:punct:]]?[[:digit:]]+", line))
      syy_val = as.numeric(unlist(matches))[1]
      syy = c(syy, syy_val)
    }
    
    if (grepl("^[\t]+(SXY){1}\\s", line)) {
      print(line)
      matches <- regmatches(line, gregexpr("[[:digit:]]+[[:punct:]]?[[:digit:]]+", line))
      sxy_val = as.numeric(unlist(matches))[1]
      sxy = c(sxy, sxy_val)
    }
    
  }
  
  dframe = data.frame(
    a=a,
    b=b,
    xbar=xbar,
    ybar=ybar,
    r2=r2,
    sx=sx,
    sy=sy,
    sxx=sxx,
    syy=syy,
    sxy=sxy
  )
  
  close(con)
  return(dframe)
}


# ---------------- STATS FILES ------------------

file = "~/cluster_results/store_reset_study/linear_regression/base/m5out/system.pc.com_1.device"
# file = "~/cluster_results/top_bytes_study/linear_regression/base/m5out/system.pc.com_1.device"
baseline_out = processFile(file)

file = "~/cluster_results/store_reset_study/linear_regression/8/m5out/system.pc.com_1.device"
# file = "~/cluster_results/top_bytes_study/linear_regression/3/m5out/system.pc.com_1.device"
approx_out = processFile(file)

print(baseline_out)
print(approx_out)

# ------------- CALCULATE ERROR ------------------------

# Percent error is (experimental - theoretical) / theoretical

percent_err = 100*abs((approx_out - baseline_out)/baseline_out)
print(percent_err)

# covert dataframe to vector
err_vector = c(t(percent_err))

mean_err = mean(err_vector)
max_abs_err = max(abs(err_vector))

deviation = (approx_out - baseline_out)
deviation_vector = c(t(deviation))
RMSD = sqrt(mean((deviation_vector)^2))
y_bar = max(baseline_out) - min(baseline_out)

NRMSD = RMSD/y_bar
NRMSE = NRMSD * 100

cat("Mean Absolute Percent Error: ", mean_err)
cat("Max Absolute Percent Error: ", max_abs_err)
cat("NRMSE: ", NRMSE)