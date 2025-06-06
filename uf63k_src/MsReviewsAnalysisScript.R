#Analysis Code for Blog Post on Signed Reviews, http://getsyeducated.blogspot.com/
#13 Feb 2020

library(rio)
library(psych)
library(ggplot2)
library(cowplot)

#importing raw data file

revdat <- import("MsReviewsDataDeID.xlsx")
revdat

#Mean Differences####
#Comparing signed and unsigned reviews on four aspects
#For each, first t-test then violin plot
#Paneled plot created at end of series

#Testing word count

t.test(revdat$wc ~ revdat$signed)

wc_sign <- ggplot(revdat, aes(signed, wc, fill=signed)) +
  geom_violin(
    trim = FALSE,
    draw_quantiles = c(0.25, 0.5, 0.75), 
    alpha = 0.5) + 
  geom_jitter(
    width = 0.20, 
    height = 0, 
    alpha = 0.5, 
    size = 1) +
    xlab("Signed Reviews") +
    ylab("Word Count")

#Testing positive emotion words

t.test(revdat$posemo ~ revdat$signed)

pos_sign <- ggplot(revdat, aes(signed, posemo, fill=signed)) +
  geom_violin(
    trim = FALSE,
    draw_quantiles = c(0.25, 0.5, 0.75), 
    alpha = 0.5) + 
  geom_jitter(
    width = 0.20, 
    height = 0, 
    alpha = 0.5, 
    size = 1) +
    xlab("Signed Reviews") +
    ylab("Positive Emotion Words")

#Testing negative emotion words

t.test(revdat$negemo ~ revdat$signed)

neg_sign <- ggplot(revdat, aes(signed, negemo, fill=signed)) +
  geom_violin(
    trim = FALSE,
    draw_quantiles = c(0.25, 0.5, 0.75), 
    alpha = 0.5) + 
  geom_jitter(
    width = 0.20, 
    height = 0, 
    alpha = 0.5, 
    size = 1) +
    xlab("Signed Reviews") +
    ylab("Negative Emotion Words")

#Testing cognitive mechanism words

t.test(revdat$cogmech ~ revdat$signed)

cog_sign <- ggplot(revdat, aes(signed, cogmech, fill=signed)) +
  geom_violin(
    trim = FALSE,
    draw_quantiles = c(0.25, 0.5, 0.75), 
    alpha = 0.5) + 
  geom_jitter(
    width = 0.20, 
    height = 0, 
    alpha = 0.5, 
    size = 1) +
    xlab("Signed Reviews") +
    ylab("Cognitive Mechanism Words")

#creating four-paneled figure

plot_grid(wc_sign, pos_sign, neg_sign, cog_sign)
ggsave("Reviews_violin_panelplot.png")


#Correlations####
#Examining correlations between time and aspects of reviews
#For each, first compute r and then scatter plot
#Plots also mark whether or not review was signed
#Paneled plot created at end of series

cor.test(revdat$order, revdat$wc)

wc_time <- ggplot(revdat, aes(order, wc, color=signed)) +
  geom_point(size=1)+
  xlab("Time") +
  ylab("Word Count")

cor.test(revdat$order, revdat$posemo)

pos_time <- ggplot(revdat, aes(order, posemo, color=signed)) +
  geom_point(size=1) +
  xlab("Time") +
  ylab("Positive Emotion Words")

cor.test(revdat$order, revdat$negemo)

neg_time <- ggplot(revdat, aes(order, negemo, color=signed)) +
  geom_point(size=1) +
  xlab("Time") +
  ylab("Negative Emotion Words")

cor.test(revdat$order, revdat$cogmech)

cog_time <- ggplot(revdat, aes(order, cogmech, color=signed)) +
  geom_point(size=1) +
  xlab("Time") +
  ylab("Cognitive Mechanism Words")

#creating four-paneled figure

plot_grid(wc_time, pos_time, neg_time, cog_time)
ggsave("Reviews_scatter_panelplot.png")





