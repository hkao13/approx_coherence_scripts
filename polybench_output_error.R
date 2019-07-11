library(ggplot2)
library(reshape2)

# ---------------- STATS FILES ------------------

file = "~/cluster_results/adi_test/store_reset_study/adi/base/m5out/output.txt"
baseline_out = read.csv(file=file, sep=",", header = FALSE)
baseline_out[] <- lapply(baseline_out, function(x) as.numeric(as.character(x)))
baseline_out[] <- do.call(data.frame,lapply(baseline_out, function(x) replace(x, is.infinite(x),NA)))
baseline_out[is.na(baseline_out)] = 0


file = "~/cluster_results/adi_test/store_reset_study/adi/8/m5out/output.txt"
approx_out = read.csv(file=file, sep=",", header = FALSE)
approx_out[] <- lapply(approx_out, function(x) as.numeric(as.character(x)))
approx_out[] <- do.call(data.frame,lapply(approx_out, function(x) replace(x, is.infinite(x),NA)))
approx_out[is.na(approx_out)] = 0

# Ensure both dataframes have the same dimensions
stopifnot(all.equal(dim(baseline_out), dim(approx_out)))

percent_err = 100*abs((approx_out - baseline_out)/baseline_out)
percent_err[is.na(percent_err)] = 0
# covert dataframe to vector
err_vector = c(t(percent_err))

mean_err = mean(err_vector)
max_abs_err = max(abs(err_vector))

deviation = (approx_out - baseline_out)
deviation_vector = c(t(deviation))
deviation_vector[is.na(deviation_vector)] = 0
RMSD = sqrt(mean((deviation_vector)^2))
RMSD[is.na(RMSD)] = 0
y_bar = max(baseline_out) - min(baseline_out)

NRMSD = RMSD/y_bar
NRMSE = NRMSD * 100

cat("Mean Absolute Percent Error: ", mean_err)
cat("Max Absolute Percent Error: ", max_abs_err)
cat("NRMSE: ", NRMSE)
