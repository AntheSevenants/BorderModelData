options(scipen=999)

# --- Libraries ----
library(tidyverse)
library(effects)
library(ggnewscale)
library(tikzDevice)

# ---- Loading the target dataset ----
d <- read_delim("ethnocentrism_stage1.csv", ";")
d <- read_delim("ethnocentrism_stage2.csv", ";")
d <- read_delim("ethnocentrism_stage3.csv", ";")
d_scaled <- read_delim("scaled_ethnocentrism_stage1.csv", ";")

# ---- Defining functions ----

# Logit: from probabilities to logit scale
logit <- function(p) {
  log(p/(1 - p))
}

# Inverse logit: from logit scale to probabilities scale
ilogit <- function(lgt) {
  exp(lgt)/(1 + exp(lgt))
}

# ---- Transforming the data ----

# I need to one-index every step, because logarithms don't like zeroes...
d <- d %>% mutate(step = step + 1)

# Generate the logits for avg_sound_be
d <- d %>% mutate(avg_sound_be_logit = logit(avg_sound_be)) 

# Multiply the ethnocentrism_be_thousand 100, so regression is easier to understand
# There's a bug where the values are saved as ethnocentrism_be OR abroad_travel_chance_be by accident...
d <- d %>% mutate(ethnocentrism_be_thousand = ethnocentrism_be * 100) # stage 1
d <- d %>% mutate(ethnocentrism_be_thousand = abroad_travel_chance_be * 100) # stage 2
d <- d %>% mutate(ethnocentrism_be_thousand = ethnocentrism_be * 100) # stage 3
d <- d %>% mutate(scaled = FALSE)
d_scaled <- d_scaled %>% mutate(scaled = TRUE, ethnocentrism_be_thousand=0)

# Performance trick... for now
#d <- d %>% filter(step < 500)
#d_scaled <- d_scaled %>% filter(step < 500)

# ---- Explorative plots ----

# First, let's just plot the data as-is
tikz(file = "ethnocentrism_stage2_real.tex", width=6, height=3, standAlone = TRUE)
d %>%
  filter(step == 1 | step %% 50 == 0) %>%
  ggplot(aes(x = step, y = avg_sound_be, color = ethnocentrism_be_thousand, group=ethnocentrism_be_thousand)) +
  theme(legend.text=element_text(size=7),
        legend.title=element_text(size=8),
        plot.margin = unit(c(0,0,0,0),"cm")) +
  geom_line(size = 0.1) +
  scale_color_gradient(low="#a692fd", high="#FDB071") +
  labs(y="Predicted sound values", x="Step", color=expression(paste("Ethnocentrism\nin Belgium ×", 10^2)))
endofffile <- dev.off()
system("lualatex ethnocentrism_stage2_real.tex; rm *.aux; rm *.log")

## SCALED ETHNO GRAPH
tikz(file = "scaled_ethnocentrism_real.tex", width=6, height=3, standAlone = TRUE)
ggplot(mapping = aes(x = step, y=avg_sound_be)) +
  labs(y="Predicted sound values", x="Step", fill="Scaled ethnocentrism") +
  theme(legend.text=element_text(size=7),
        legend.title=element_text(size=8),
        plot.margin = unit(c(0,0,0,0),"cm")) +
  geom_line(data=(d %>% filter(step == 1 | step %% 50 == 0)),
             aes(x = step, y = avg_sound_be, color = ethnocentrism_be_thousand, group=ethnocentrism_be_thousand), size = 0.1) +
  scale_color_gradient(low="#a692fd", high="#FDB071", name=expression(paste("Ethnocentrism\nin Belgium ×", 10^2))) +
  new_scale_color() +
  geom_line(data=(d_scaled %>% filter(step == 1 | step %% 50 == 0)), size = 0.1, alpha=0.5, aes(color="Yes")) +
  scale_color_manual(values = c("#92FDA6"), name="Scaled ethnocentrism?",
                     guide = guide_legend(override.aes = list(size = 5,
                                                              alpha = 1)))
endofffile <- dev.off()
system("lualatex scaled_ethnocentrism_real.tex; rm *.aux; rm *.log")

# Very clear -> these are not linear data! Maybe logit can help us make the data linear
# for the regression analysis?
ggplot(d, aes(x = step, y = avg_sound_be_logit, color = ethnocentrism_be_thousand)) +
  geom_point(size = 0.1) +
  scale_color_gradient(low="blue", high="red")

# Not quite. What if we model is logarithmically?
ggplot(d, aes(x = log(step), y = avg_sound_be_logit, color = ethnocentrism_be_thousand)) +
  geom_point(size = 0.1) +
  scale_color_gradient(low="blue", high="red")

# Somewhat better...

# ---- Regression ----

# Run the regression
fit <- lm(avg_sound_be_logit ~ log(step) * ethnocentrism_be_thousand, data = d)

# Output
summary(fit)

# Add the predicted data to the dataset
d$predicted <- predict(fit)

# What do the predictions look like on the logit scale?
d %>%
  ggplot(aes(x = step, y = avg_sound_be_logit, color = ethnocentrism_be_thousand)) +
  geom_point(alpha = 1, size=0.1) +
  scale_color_gradient(low="blue", high="red") +
  geom_line(aes(y = predicted, group=ethnocentrism_be_thousand), size = 0.1, alpha=0.2)

# What do the predictions look like on the probability scale?
tikz(file = "ethnocentrism_stage1_real.tex", width=6, height=3, standAlone = TRUE)
d %>%
  filter(step == 1 | step %% 50 == 0) %>%
  ggplot(aes(x = step, y = ilogit(avg_sound_be_logit), color = ethnocentrism_be_thousand, group = ethnocentrism_be_thousand)) +
  theme(legend.text=element_text(size=7),
        legend.title=element_text(size=8),
        plot.margin = unit(c(0,0,0,0),"cm")) +
  geom_line(alpha = 1, size=0.1) +
  scale_color_gradient(low="#a692fd", high="#FDB071") +
  geom_line(aes(y = ilogit(predicted), group=ethnocentrism_be_thousand), size = 0.1, alpha=0.2) +
  labs(y="Predicted sound values", x="Step", color=expression(paste("Ethnocentrism\nin Belgium ×", 10^2)))
endofffile <- dev.off()
system("lualatex ethnocentrism_stage1_real; rm *.aux; rm *.log")

# Poor man's "all effects" plot
tikz(file = "ethnocentrism_stage1_predict.tex", width=6, height=3, standAlone = TRUE)
d %>% 
  filter(step == 1 | step %% 50 == 0) %>%
  filter(ethnocentrism_be_thousand %% 10 == 0 & ethnocentrism_be_thousand != 100) %>%
  mutate(ethnocentrism_be_thousand = as.factor(ethnocentrism_be_thousand)) %>%
  ggplot(aes(x = step, y=ilogit(predicted), color = ethnocentrism_be_thousand)) +
  theme(legend.text=element_text(size=7),
        legend.title=element_text(size=8),
        plot.margin = unit(c(0,0,0,0),"cm")) +
  geom_line(size = 0.1) +
  geom_line(aes(group = ethnocentrism_be_thousand)) +
  labs(y="Predicted sound values", x="Step", color=expression(paste("Ethnocentrism\nin Belgium ×", 10^2)))
#facet_wrap(~ ethnocentrism_be_thousand) # This looks bad
endofffile <- dev.off()
system("lualatex ethnocentrism_stage1_predict.tex; rm *.aux; rm *.log")

# Todo: maybe make colours prettier