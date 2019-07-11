library(ggplot2)
library(reshape2)

data = read.csv("output_error.csv")

data$benchmark <- factor(data$benchmark, levels = data$benchmark)

plot = ggplot(data, aes(x=benchmark)) +
  geom_bar(aes(fill=source, y=Accuracy), position="dodge", stat="identity", color="black", width=0.8) +
  theme_bw() +
  ylab("Accuracy (%)") +
  xlab("Benchmarks")

plot = plot +
  theme(
    legend.title = element_blank(),
    legend.position = "top",
    legend.text=element_text(size=22),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=22),
    axis.text.x  = element_text(angle=90, hjust=1,vjust=0.2, size=22),
    axis.text.y  = element_text(size=22),
    panel.grid.major.y = element_line(colour = "black"),
    panel.grid.major.x = element_blank()
  ) +
  # scale_fill_manual(
  #   values=cbPalette
  #   # labels=c(" Base ", " SR2 ", " SR4 ")
  # )
  scale_fill_brewer(palette = "Blues", labels=c(" N=2 ", " N=4 ", " N=8 "))

plot
pdf("output_error.pdf", width=9, height=4.5)
print(plot)
dev.off()