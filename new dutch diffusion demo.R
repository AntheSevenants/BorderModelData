options(scipen=5)

# ---- Libraries ----
library(tidyverse)
library(cowplot)
library(tikzDevice)

# ---- Functions ----
logit <- function(p) {
  log(p/(1 - p))
}

ilogit <- function(lgt) {
  exp(lgt)/(1 + exp(lgt))
}

s_curve <- function(x) {
  1 / ( 1 + exp(1)^(-0.025* (x - 250)) )
}

# ---- Generating the data ---
# Theoretical data
d_example_hypothetical <- tibble(values = (1:499)/500, logits = logit(values))

# Run data
steps <- 1:500
probabilities <- s_curve(steps)
logits <- logit(probabilities)


d_example_runs <- tibble(steps = steps,
                         values = probabilities,
                         logits = logits)

# ---- Plots ----
diffusion_plot <- d_example_runs %>% ggplot(aes(x = steps, y=values)) +
  theme(legend.text=element_text(size=7),
        legend.title=element_text(size=8),
        plot.margin = unit(c(0.05,0.5,0,0),"cm")) +
  geom_line(color="#FDB071", size=2) +
  geom_point(data=(d_example_runs %>% filter(steps == 300)), size=2) +
  geom_text(data=(d_example_runs %>% filter(steps == 300)), size=4, aes(label=round(values, 4)),
            nudge_x=-70, nudge_y=0.04) +
  labs(y="Hypothetical sound value", x="Step")

example_value <- 0.7773

density_plot <- ggplot(data.frame(x = c(example_value - 0.20, example_value + 0.20)), aes(x)) + 
  theme(legend.text=element_text(size=7),
        legend.title=element_text(size=8),
        plot.margin = unit(c(0.05,0.5,0,0),"cm")) +
  stat_function(fun = dnorm, n = 1000, args = list(mean = example_value, sd = 0.05), color="#FDB071", size=2) +
  geom_vline(xintercept=example_value, color="black", size=1) +
  geom_text(aes(x=example_value, label=paste0(example_value,"\n"), y=4), colour="black", angle=90, show.legend = FALSE) +
  labs(x="Sound value", y="Probability density")

tikz(file = "diffusion_plot.tex", width=6, height=3, standAlone = TRUE)
plot_grid(diffusion_plot, density_plot, labels = "AUTO", label_x = 0, label_y = 0, hjust = -0.5, vjust = -0.5)
endofffile <- dev.off()
system("lualatex diffusion_plot.tex; rm *.aux; rm *.log")

