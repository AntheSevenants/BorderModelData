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
  1 / ( 1 + exp(1)^(-0.1* (x - 50)) )
}

# ---- Generating the data ---
# Theoretical data
d_example_hypothetical <- tibble(values = (1:99)/100, logits = logit(values))

# Run data
steps <- 1:100
probabilities <- s_curve(steps)
logits <- logit(probabilities)


d_example_runs <- tibble(steps = steps,
                         values = probabilities,
                         logits = logits)

# ---- Plots ----

tikz(file = "logit_relation.tex", width=6, height=3, standAlone=TRUE)
d_example_hypothetical %>% ggplot(aes(x = logits, y=values)) +
  theme_light() +
  theme(legend.text=element_text(size=7),
        legend.title=element_text(size=8),
        plot.margin = unit(c(0.05,0.2,0.05,0.05),"cm")) +
  geom_point(color="#FDB071", size=1, stroke = 0.2) +
  labs(y="Hypothetical sound values", x="Hypothethical sound values, converted to logit")
endofffile <- dev.off()
system("lualatex logit_relation.tex; rm *.aux; rm *.log")

prob_plot <- d_example_runs %>% ggplot(aes(x = steps, y=values)) +
  theme_light() +
  theme(legend.text=element_text(size=7),
        legend.title=element_text(size=8),
        plot.margin = unit(c(0.05,0.5,0,0),"cm")) +
  geom_point(color="#FDB071", size=1, stroke = 0.2) +
  labs(y="Hypothetical sound value", x="Step")

lgt_plot <- d_example_runs %>% ggplot(aes(x = steps, y=logits)) +
  theme_light() +
  theme(legend.text=element_text(size=7),
        legend.title=element_text(size=8),
        plot.margin = unit(c(0.05,0.2,0,0),"cm")) +
  geom_point(color="#FDB071", size=1, stroke = 0.2) +
  labs(y="Hypothetical sound value,\nconverted to logit", x="Step")

tikz(file = "logit_conversion.tex", width=6, height=3, standAlone=TRUE)
plot_grid(prob_plot, lgt_plot, labels = "AUTO")
endofffile <- dev.off()
system("lualatex logit_conversion.tex; rm *.aux; rm *.log")

