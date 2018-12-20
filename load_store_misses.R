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

	demand_hits = c()
	demand_misses = c()
	demand_accesses = c()
	read_hits = c()
	read_misses = c()
	read_misses_coherence = c()

	write_hits = c()
	write_misses = c()
	write_misses_coherence = c()

	while ( TRUE ) {
		line = readLines(con, n = 1)
		if ( length(line) == 0 ) {
		 	break
		}

		if (grepl(".*Dcache.demand_hits.*", line)) {
			#print(line)
			matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
			miss = as.numeric(unlist(matches))[3]
			demand_hits = c(demand_hits, miss)
		}

		if (grepl(".*Dcache.demand_misses.*", line)) {
			#print(line)
			matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
			miss = as.numeric(unlist(matches))[3]
			demand_misses = c(demand_misses, miss)
			# print(miss)
		}

		if (grepl(".*Dcache.demand_accesses.*", line)) {
			#print(line)
			matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
			miss = as.numeric(unlist(matches))[3]
			demand_accesses = c(demand_accesses, miss)
			#print(miss)
		}

		if (grepl(".*Dcache.read_hits.*", line)) {
			#print(line)
			matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
			miss = as.numeric(unlist(matches))[3]
			read_hits = c(read_hits, miss)
			#print(miss)
		}

		if (grepl(".*Dcache.read_misses .*", line)) {
			#print(line)
			matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
			miss = as.numeric(unlist(matches))[3]
			read_misses = c(read_misses, miss)
			#print(miss)
		}

		if (grepl(".*Dcache.read_misses_coherence.*", line)) {
			#print(line)
			matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
			miss = as.numeric(unlist(matches))[3]
			read_misses_coherence = c(read_misses_coherence, miss)
			#print(miss)
		}

		if (grepl(".*Dcache.write_hits.*", line)) {
			#print(line)
			matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
			miss = as.numeric(unlist(matches))[3]
			write_hits = c(write_hits, miss)
			#print(miss)
		}

		if (grepl(".*Dcache.write_misses .*", line)) {
			#print(line)
			matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
			miss = as.numeric(unlist(matches))[3]
			write_misses = c(write_misses, miss)
			#print(miss)
		}

		if (grepl(".*Dcache.write_misses_coherence.*", line)) {
			#print(line)
			matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
			miss = as.numeric(unlist(matches))[3]
			write_misses_coherence = c(write_misses_coherence, miss)
			#print(miss)
		}
	}

	dframe = data.frame(
		demand_misses=sum(demand_misses),
		demand_accesses=sum(demand_accesses),
		read_hits=sum(read_hits),
		read_misses=sum(read_misses),
		read_misses_coherence=sum(read_misses_coherence),

		write_hits=sum(write_hits),
		write_misses=sum(write_misses),
		write_misses_coherence=sum(write_misses_coherence)
	)

	close(con)
	#print(dframe)
	#summed = sumData(dframe)
	return(dframe)
}


processFileL2 = function(filepath) {
	con = file(filepath, "r")

	demand_hits = c()
	demand_misses = c()
	demand_accesses = c()
	read_hits = c()
	read_misses = c()
	read_misses_coherence = c()

	write_hits = c()
	write_misses = c()
	write_misses_coherence = c()

	while ( TRUE ) {
		line = readLines(con, n = 1)
		if ( length(line) == 0 ) {
		 	break
		}

		if (grepl(".*\\.cache.demand_hits.*", line)) {
			#print(line)
			matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
			miss = as.numeric(unlist(matches))[3]
			demand_hits = c(demand_hits, miss)
		}

		if (grepl(".*\\.cache.demand_misses.*", line)) {
			# print(line)
			matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
			miss = as.numeric(unlist(matches))[3]
			demand_misses = c(demand_misses, miss)
			# print(miss)
		}

		if (grepl(".*\\.cache.demand_accesses.*", line)) {
			#print(line)
			matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
			miss = as.numeric(unlist(matches))[3]
			demand_accesses = c(demand_accesses, miss)
			#print(miss)
		}

		if (grepl(".*\\.cache.read_hits.*", line)) {
			#print(line)
			matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
			miss = as.numeric(unlist(matches))[3]
			read_hits = c(read_hits, miss)
			#print(miss)
		}

		if (grepl(".*\\.cache.read_misses .*", line)) {
			#print(line)
			matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
			miss = as.numeric(unlist(matches))[3]
			read_misses = c(read_misses, miss)
			#print(miss)
		}

		if (grepl(".*\\.cache.read_misses_coherence.*", line)) {
			#print(line)
			matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
			miss = as.numeric(unlist(matches))[3]
			read_misses_coherence = c(read_misses_coherence, miss)
			#print(miss)
		}

		if (grepl(".*\\.cache.write_hits.*", line)) {
			#print(line)
			matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
			miss = as.numeric(unlist(matches))[3]
			write_hits = c(write_hits, miss)
			#print(miss)
		}

		if (grepl(".*\\.cache.write_misses .*", line)) {
			#print(line)
			matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
			miss = as.numeric(unlist(matches))[3]
			write_misses = c(write_misses, miss)
			#print(miss)
		}

		if (grepl(".*\\.cache.write_misses_coherence.*", line)) {
			#print(line)
			matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
			miss = as.numeric(unlist(matches))[3]
			write_misses_coherence = c(write_misses_coherence, miss)
			#print(miss)
		}
	}

	dframe = data.frame(
		demand_misses=sum(demand_misses),
		demand_accesses=sum(demand_accesses),
		read_hits=sum(read_hits),
		read_misses=sum(read_misses),
		read_misses_coherence=sum(read_misses_coherence),

		write_hits=sum(write_hits),
		write_misses=sum(write_misses),
		write_misses_coherence=sum(write_misses_coherence)
	)

	close(con)
	#print(dframe)
	#summed = sumData(dframe)
	return(dframe)
}

normalizeTo = function(data, reference) {
	dframe = data.frame(
		demand_misses=data$demand_misses/reference,
		demand_accesses=data$demand_accesses/reference,
		read_hits=data$read_hits/reference,
		read_misses=data$read_misses/reference,
		read_misses_coherence=data$read_misses_coherence/reference,

		write_hits=data$write_hits/reference,
		write_misses=data$write_misses/reference,
		write_misses_coherence=data$write_misses_coherence/reference
	)
	return(dframe)
}

benchmark_names = c("lin_reg", "kmeans", "histogram", "pca", "mat_mul")

# Summed data from stats file
all_benchmarks = data.frame(
	demand_misses=integer(),
	demand_accesses=integer(),
	read_hits=integer(),
	read_misses=integer(),
	read_misses_coherence=integer(),

	write_hits=integer(),
	write_misses=integer(),
	write_misses_coherence=integer()
)

all_benchmarksL2 = data.frame(
	demand_misses=integer(),
	demand_accesses=integer(),
	read_hits=integer(),
	read_misses=integer(),
	read_misses_coherence=integer(),

	write_hits=integer(),
	write_misses=integer(),
	write_misses_coherence=integer()
)

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")



file = "~/cluster_results/results/linear_regression/m5out/stats.txt"
file_data = processFile(file)
file_dataL2 = processFileL2(file)
all_benchmarks[nrow(all_benchmarks) + 1,] = file_data
all_benchmarksL2[nrow(all_benchmarksL2) + 1,] = file_dataL2


file = "~/cluster_results/results/kmeans/m5out/stats.txt"
file_data = processFile(file)
file_dataL2 = processFileL2(file)
all_benchmarks[nrow(all_benchmarks) + 1,] = file_data
all_benchmarksL2[nrow(all_benchmarksL2) + 1,] = file_dataL2

file = "~/cluster_results/results/histogram/m5out/stats.txt"
file_data = processFile(file)
file_dataL2 = processFileL2(file)
all_benchmarks[nrow(all_benchmarks) + 1,] = file_data
all_benchmarksL2[nrow(all_benchmarksL2) + 1,] = file_dataL2


file = "~/cluster_results/results/pca/m5out/stats.txt"
file_data = processFile(file)
file_dataL2 = processFileL2(file)
all_benchmarks[nrow(all_benchmarks) + 1,] = file_data
all_benchmarksL2[nrow(all_benchmarksL2) + 1,] = file_dataL2


file = "~/cluster_results/results/matrix_multiply/m5out/stats.txt"
file_data = processFile(file)
file_dataL2 = processFileL2(file)
all_benchmarks[nrow(all_benchmarks) + 1,] = file_data
all_benchmarksL2[nrow(all_benchmarksL2) + 1,] = file_dataL2

print(all_benchmarks)
print(all_benchmarksL2)


# Plot miss and hits mornalized to all demand accesses
all_benchmarks_frac = normalizeTo(all_benchmarks, all_benchmarks$demand_accesses)
data_subset = subset(all_benchmarks_frac, select=c("demand_misses"))
# Reshape to get into stacked bar form
data_subset$benchmarks <- benchmark_names 

data_reshape = melt(data_subset, id.vars = "benchmarks")

plot1 <- ggplot() + 
	geom_bar(aes(y=value, x=benchmarks, fill=variable), data=data_reshape,
    stat="identity", color="black", width=0.5) +
    theme_bw() +
    ylab("Fraction of L1-d Cache Misses") +
	xlab("Benchmarks") +
	theme(
		legend.title = element_blank(),
		legend.position = "top",
		legend.text=element_text(size=14),
		axis.title.x = element_text(size=14),
		axis.title.y = element_text(size=14),
		axis.text.x  = element_text(angle=30, vjust=0.6, size=14),
		axis.text.y  = element_text(size=14)
		) +
	scale_fill_manual(
		values=cbPalette, labels=c(" cache misses ")) +
	ylim(c(0,1.00))

plot1
pdf("cache_misses_L1.pdf", width=6, height=6)
print(plot1)
dev.off()



# Plot read and write misses normalized to all demand misses
all_benchmarks_frac = normalizeTo(all_benchmarks, all_benchmarks$demand_misses)
data_subset = subset(all_benchmarks_frac, select=c("write_misses", "read_misses"))
# Reshape to get into stacked bar form
data_subset$benchmarks <- benchmark_names 

data_reshape = melt(data_subset, id.vars = "benchmarks")

plot2 <- ggplot() + 
	geom_bar(aes(y=value, x=benchmarks, fill=variable), data=data_reshape,
    stat="identity", color="black", width=0.5) +
    theme_bw() +
    ylab("Normalized L1-d Cache Misses") +
	xlab("Benchmarks") +
	theme(
		legend.title = element_blank(),
		legend.position = "top",
		legend.text=element_text(size=14),
		axis.title.x = element_text(size=14),
		axis.title.y = element_text(size=14),
		axis.text.x  = element_text(angle=30, vjust=0.6, size=14),
		axis.text.y  = element_text(size=14)
		) +
	scale_fill_manual(
		values=cbPalette,
    	labels=c(" store miss ", " load miss ")) +
	ylim(c(0,1.00))

plot2
pdf("all_misses_L1.pdf", width=6, height=6)
print(plot2)
dev.off()


# Plot coherence write misses normalized to just write misses
all_benchmarks_frac = normalizeTo(all_benchmarks, all_benchmarks$write_misses)
data_subset = subset(all_benchmarks_frac, select=c("write_misses_coherence"))
# Reshape to get into stacked bar form
data_subset$benchmarks <- benchmark_names 

data_reshape = melt(data_subset, id.vars = "benchmarks")

plot3 <- ggplot() + 
	geom_bar(aes(y=value, x=benchmarks, fill=variable), data=data_reshape,
    stat="identity", color="black", width=0.5) +
    theme_bw() +
    ylab("Fraction of L1-d Coherence Store Misses") +
	xlab("Benchmarks") +
	theme(
		legend.title = element_blank(),
		legend.position = "top",
		legend.text=element_text(size=14),
		axis.title.x = element_text(size=14),
		axis.title.y = element_text(size=14),
		axis.text.x  = element_text(angle=30, vjust=0.6, size=14),
		axis.text.y  = element_text(size=14)
		) +
	scale_fill_manual(
		values=cbPalette,
    	labels=c(" coherence store misses ")) +
	ylim(c(0,1.00))

plot3


# Plot coherence read misses normalized to just read misses
all_benchmarks_frac = normalizeTo(all_benchmarks, all_benchmarks$read_misses)
data_subset = subset(all_benchmarks_frac, select=c("read_misses_coherence"))
# Reshape to get into stacked bar form
data_subset$benchmarks <- benchmark_names 

data_reshape = melt(data_subset, id.vars = "benchmarks")

plot4 <- ggplot() + 
	geom_bar(aes(y=value, x=benchmarks, fill=variable), data=data_reshape,
    stat="identity", color="black", width=0.5) +
    theme_bw() +
    ylab("Fraction of L1-d Coherence Load Misses") +
	xlab("Benchmarks") +
	theme(
		legend.title = element_blank(),
		legend.position = "top",
		legend.text=element_text(size=14),
		axis.title.x = element_text(size=14),
		axis.title.y = element_text(size=14),
		axis.text.x  = element_text(angle=30, vjust=0.6, size=14),
		axis.text.y  = element_text(size=14)
		) +
	scale_fill_manual(
		values=cbPalette,
    	labels=c(" coherence load misses ")) +
	ylim(c(0,1.00))

plot4



# Plot read and write misses normalized to all demand misses
all_benchmarks_frac = normalizeTo(all_benchmarks, all_benchmarks$demand_misses)
data_subset = subset(all_benchmarks_frac, select=c("write_misses_coherence", "read_misses_coherence"))
# Reshape to get into stacked bar form
data_subset$benchmarks <- benchmark_names 

data_reshape = melt(data_subset, id.vars = "benchmarks")

plot9 <- ggplot() + 
	geom_bar(aes(y=value, x=benchmarks, fill=variable), data=data_reshape,
    stat="identity", color="black", width=0.5) +
    theme_bw() +
    ylab("Normalized L1-d Coherence Cache Misses") +
	xlab("Benchmarks") +
	theme(
		legend.title = element_blank(),
		legend.position = "top",
		legend.text=element_text(size=14),
		axis.title.x = element_text(size=14),
		axis.title.y = element_text(size=14),
		axis.text.x  = element_text(angle=30, vjust=0.6, size=14),
		axis.text.y  = element_text(size=14)
		) +
	scale_fill_manual(
		values=cbPalette,
    	labels=c(" store miss ", " load miss ")) +
	ylim(c(0,1.00))

plot9
pdf("coherence_misses_L1.pdf", width=6, height=6)
print(plot9)
dev.off()



#  STUFF FOR THE L2



# Plot miss and hits mornalized to all demand accesses
all_benchmarks_frac = normalizeTo(all_benchmarksL2, all_benchmarksL2$demand_accesses)
data_subset = subset(all_benchmarks_frac, select=c("demand_misses"))
# Reshape to get into stacked bar form
data_subset$benchmarks <- benchmark_names 

data_reshape = melt(data_subset, id.vars = "benchmarks")

plot5 <- ggplot() + 
	geom_bar(aes(y=value, x=benchmarks, fill=variable), data=data_reshape,
    stat="identity", color="black", width=0.5) +
    theme_bw() +
    ylab("Fraction of L2 Cache Misses") +
	xlab("Benchmarks") +
	theme(
		legend.title = element_blank(),
		legend.position = "top",
		legend.text=element_text(size=14),
		axis.title.x = element_text(size=14),
		axis.title.y = element_text(size=14),
		axis.text.x  = element_text(angle=30, vjust=0.6, size=14),
		axis.text.y  = element_text(size=14)
		) +
	scale_fill_manual(
		values=cbPalette, labels=c(" cache misses ")) +
	ylim(c(0,1.00))

plot5
pdf("cache_misses_L2.pdf", width=6, height=6)
print(plot5)
dev.off()

# Plot read and write misses normalized to all demand misses
all_benchmarks_frac = normalizeTo(all_benchmarksL2, all_benchmarksL2$demand_misses)
data_subset = subset(all_benchmarks_frac, select=c("write_misses", "read_misses"))
# Reshape to get into stacked bar form
data_subset$benchmarks <- benchmark_names 

data_reshape = melt(data_subset, id.vars = "benchmarks")

plot6 <- ggplot() + 
	geom_bar(aes(y=value, x=benchmarks, fill=variable), data=data_reshape,
    stat="identity", color="black", width=0.5) +
    theme_bw() +
    ylab("Normalized L2 Cache Misses") +
	xlab("Benchmarks") +
	theme(
		legend.title = element_blank(),
		legend.position = "top",
		legend.text=element_text(size=14),
		axis.title.x = element_text(size=14),
		axis.title.y = element_text(size=14),
		axis.text.x  = element_text(angle=30, vjust=0.6, size=14),
		axis.text.y  = element_text(size=14)
		) +
	scale_fill_manual(
		values=cbPalette,
    	labels=c(" store miss ", " load miss ")) +
	ylim(c(0,1.00))

plot6
pdf("all_misses_L2.pdf", width=6, height=6)
print(plot6)
dev.off()

# Plot coherence write misses normalized to just write misses
all_benchmarks_frac = normalizeTo(all_benchmarksL2, all_benchmarksL2$write_misses)
data_subset = subset(all_benchmarks_frac, select=c("write_misses_coherence"))
# Reshape to get into stacked bar form
data_subset$benchmarks <- benchmark_names 

data_reshape = melt(data_subset, id.vars = "benchmarks")

plot7 <- ggplot() + 
	geom_bar(aes(y=value, x=benchmarks, fill=variable), data=data_reshape,
    stat="identity", color="black", width=0.5) +
    theme_bw() +
    ylab("Fraction of L2 Coherence Store Misses") +
	xlab("Benchmarks") +
	theme(
		legend.title = element_blank(),
		legend.position = "top",
		legend.text=element_text(size=14),
		axis.title.x = element_text(size=14),
		axis.title.y = element_text(size=14),
		axis.text.x  = element_text(angle=30, vjust=0.6, size=14),
		axis.text.y  = element_text(size=14)
		) +
	scale_fill_manual(
		values=cbPalette,
    	labels=c(" coherence store misses ")) +
	ylim(c(0,1.00))

plot7

# Plot coherence read misses normalized to just read misses
all_benchmarks_frac = normalizeTo(all_benchmarksL2, all_benchmarksL2$read_misses)
data_subset = subset(all_benchmarks_frac, select=c("read_misses_coherence"))
# Reshape to get into stacked bar form
data_subset$benchmarks <- benchmark_names 

data_reshape = melt(data_subset, id.vars = "benchmarks")

plot8 <- ggplot() + 
	geom_bar(aes(y=value, x=benchmarks, fill=variable), data=data_reshape,
    stat="identity", color="black", width=0.5) +
    theme_bw() +
    ylab("Fraction of L2 Coherence Load Misses") +
	xlab("Benchmarks") +
	theme(
		legend.title = element_blank(),
		legend.position = "top",
		legend.text=element_text(size=14),
		axis.title.x = element_text(size=14),
		axis.title.y = element_text(size=14),
		axis.text.x  = element_text(angle=30, vjust=0.6, size=14),
		axis.text.y  = element_text(size=14)
		) +
	scale_fill_manual(
		values=cbPalette,
    	labels=c(" coherence load misses ")) +
	ylim(c(0,1.00))

plot8


# Plot read and write misses normalized to all demand misses
all_benchmarks_frac = normalizeTo(all_benchmarksL2, all_benchmarksL2$demand_misses)
data_subset = subset(all_benchmarks_frac, select=c("write_misses_coherence", "read_misses_coherence"))
# Reshape to get into stacked bar form
data_subset$benchmarks <- benchmark_names 

data_reshape = melt(data_subset, id.vars = "benchmarks")

plot10 <- ggplot() + 
	geom_bar(aes(y=value, x=benchmarks, fill=variable), data=data_reshape,
    stat="identity", color="black", width=0.5) +
    theme_bw() +
    ylab("Normalized L2 Coherence Cache Misses") +
	xlab("Benchmarks") +
	theme(
		legend.title = element_blank(),
		legend.position = "top",
		legend.text=element_text(size=14),
		axis.title.x = element_text(size=14),
		axis.title.y = element_text(size=14),
		axis.text.x  = element_text(angle=30, vjust=0.6, size=14),
		axis.text.y  = element_text(size=14)
		) +
	scale_fill_manual(
		values=cbPalette,
    	labels=c(" store miss ", " load miss ")) +
	ylim(c(0,1.00))

plot10
pdf("coherence_misses_L2.pdf", width=6, height=6)
print(plot10)
dev.off()