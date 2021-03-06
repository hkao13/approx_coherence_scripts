library(ggplot2)
library(reshape2)

# ---------------- STATS FILES ------------------

file = "~/cluster_results/store_reset_study/conv-3d/base/m5out/output.txt"
baseline_out = read.csv(file=file, sep=",")
is.na(baseline_out)

file = "~/cluster_results/store_reset_study/conv-3d/2/m5out/output.txt"
approx_out = read.csv(file=file, header=TRUE, sep=",")
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
y_bar = max(approx_out) - min(approx_out)

NRMSD = RMSD/y_bar
NRMSE = NRMSD * 100

cat("Mean Absolute Percent Error: ", mean_err)
cat("Max Absolute Percent Error: ", max_abs_err)
cat("NRMSE: ", NRMSE)
