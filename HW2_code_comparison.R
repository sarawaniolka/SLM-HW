require(ggplot2)
require(cowplot)
require(dplyr)


# reproduce the plots, colors are not important

# Reading the data

data <- "problem,language,time,size
n-body,C,2.13,1633
mandelbrot,C,1.3,1135
spectral norm,C,0.41,1197
fannkuch-redux,C,7.58,910
fasta,C,0.78,1463
k-nucleotide,C,3.96,1506
binary-trees,C,1.58,809
reverse-complement,C,0.41,1965
pidigits,C,0.56,1090
regex-redux,C,0.8,1397
n-body,Java,6.77,1489
mandelbrot,Java,4.1,796
spectral norm,Java,1.55,756
fannkuch-redux,Java,10.48,1282
fasta,Java,1.2,2543
k-nucleotide,Java,4.83,1812
binary-trees,Java,2.51,835
reverse-complement,Java,1.57,2183
pidigits,Java,0.79,764
regex-redux,Java,5.34,929
n-body,Python,541.34,1196
mandelbrot,Python,177.35,688
spectral norm,Python,112.97,407
fannkuch-redux,Python,341.45,950
fasta,Python,36.9,1947
k-nucleotide,Python,46.31,1967
binary-trees,Python,44.7,660
reverse-complement,Python,6.62,814
pidigits,Python,1.16,567
regex-redux,Python,1.34,1403
n-body,Julia,4.21,1111
mandelbrot,Julia,1.42,619
spectral norm,Julia,1.11,429
fannkuch-redux,Julia,7.83,1067
fasta,Julia,1.13,1082
k-nucleotide,Julia,4.94,951
binary-trees,Julia,7.28,634
reverse-complement,Julia,1.44,522
pidigits,Julia,0.97,506
regex-redux,Julia,1.74,759"

d <- read.table(textConnection(object=data), header=TRUE, sep=",", stringsAsFactors = FALSE)
d$language <- as.factor(d$language)
labels <- unique(d$problem)
d$problem <-factor(d$problem, levels=labels)
d$size = d$size/1000

g1 <- ggplot(d %>% filter(language != "C"), aes(x=problem, y=time, group=language)) + geom_point(aes(shape=language, color=language, size =5)) +
  scale_y_continuous(
    breaks = 10^c(0,1,2),
    labels = c("10^0","10^1","10^2"),
    trans = "pseudo_log") +
  scale_x_discrete(limits = labels) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle=20),
    legend.position = c(0.85,0.85),
    panel.border = element_rect(colour = NA, fill=NA, size=0),
    legend.title = element_blank(),
    legend.key.size = unit(1,'cm'),
    legend.text = element_text(size=10),
    legend.box.background = element_rect(color = "black",fill = "white")
  ) +
  labs(
    x = element_blank(),
    y = "execution time (relative to C)"
    )+
  scale_shape_manual(values = c(15,16,18)) +
  scale_size(guide="none") +
  geom_hline(aes(yintercept = 1.0, linetype="C"), color="orange") +
  scale_linetype_manual(name = "C", values = c(2, 2), 
                        guide = guide_legend(override.aes = list(color = "orange")))


g2<- ggplot(d %>% filter(language != "C"), aes(x=problem, y=size, group=language)) + geom_point(aes(shape=language, color=language, size =5)) +
  ylim(0.3,1.85) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle=20),
    legend.position = c(0.85,0.85),
    panel.border = element_rect(colour = NA, fill=NA, size=0),
    legend.title = element_blank(),
    legend.key.size = unit(1,'cm'),
    legend.text = element_text(size=10),
    legend.box.background = element_rect(color = "black",fill = "white")
  ) +
  labs(
    x = element_blank(),
    y = "code size (relative to C)"
  )+
  scale_shape_manual(values = c(15,16,18)) +
  scale_size(guide="none") +
  geom_hline(aes(yintercept = 1.0, linetype="C"), color="orange") +
  scale_linetype_manual(name = "C", values = c(2, 2), 
                        guide = guide_legend(override.aes = list(color = "orange")))


plot_grid(g1,g2, align="h", ncol = 2, rel_widths = c(1/2, 1/2))
