# --- Libraries ----
library(tidyverse)
library(skimr)
library(tikzDevice)

# ---- Loading the artefact dataset ----
d <- read_delim("nl_artefact_test_stage1.csv", ";")

# ---- Check starting values in NL ----

d_start = d %>% filter(step == 0)

tikz(file = "artefact_test_boxplot.tex", width=6, height=3, standAlone=TRUE)
ggplot(d_start, aes(y = avg_sound_nl)) +
  theme(legend.text=element_text(size=7),
        legend.title=element_text(size=8),
        plot.margin = unit(c(0.05,0.5,0,0),"cm")) +
  geom_boxplot() +
  labs(y="Average sound value") +
  theme(axis.title.y=element_text(size=12))
endofffile <- dev.off()
system("lualatex artefact_test_boxplot.tex; rm *.aux; rm *.log")

# Check the median, Q1 and Q3
d %>% filter(step == 0) %>% pull(avg_sound_nl) %>% skim()
# ---- Checking for a substitute town ----

d_towns <- read_delim("nl_towns.csv", ";")
d_towns <- d_towns %>% mutate(sound_mean_zero = as.logical(sound_mean_zero)) %>%
  filter(sound_mean_zero)

tikz(file = "artefact_distance.tex", width=6, height=3, standAlone=TRUE)
ggplot(d_towns, aes(x = name, y = distance)) +
  theme(legend.text=element_text(size=7),
        legend.title=element_text(size=8),
        plot.margin = unit(c(0.05,0.5,0,0),"cm")) +
  geom_col(fill="#FDB071") +
  coord_flip() +
  labs(y="Distance from the Randstad area", x="Influence sphere") +
  theme(axis.title.y=element_text(size=12),
        axis.title.x=element_text(size=12))
endofffile <- dev.off()
system("lualatex artefact_distance.tex; rm *.aux; rm *.log")

# todo: maybe change colours
