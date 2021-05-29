options(scipen=999)

# --- Libraries ----
library(tidyverse)
library(effects)
library(tikzDevice)

# ---- Loading the target dataset ----
d <- read_delim("target_stage1.csv", ";")
d <- read_delim("target_stage2.csv", ";")

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

# Multiply the media domestic_travel_chance_nl by 1000 (or 10), so regression is easier to understand
d <- d %>% mutate(domestic_travel_chance_nl_thousand = domestic_travel_chance_nl * 1000) # stage 1
d <- d %>% mutate(domestic_travel_chance_nl_thousand = domestic_travel_chance_nl * 10) # stage 2

# Performance trick... for now
#d <- d %>% filter(step < 1001)

# ---- Explorative plots ----

# First, let's just plot the data as-is

# Stage 1
d %>%
  ggplot(aes(x = step, y = avg_sound_be, color = domestic_travel_chance_nl_thousand)) +
  geom_point(size = 0.1) +
  scale_color_gradient(low="#a692fd", high="#FDB071") +
  labs(y="Predicted sound values", x="Step", color=expression(paste("Domestic travel <probability for the Netherlands ×", 10^3)))

# Stage 2
tikz(file = "target_stage2_real.tex", width=6, height=3, standAlone=TRUE)
d %>%
  filter(step == 1 | step %% 50 == 0) %>%
  ggplot(aes(x = step, y = avg_sound_be, color = domestic_travel_chance_nl_thousand, group = domestic_travel_chance_nl_thousand)) +
  theme(legend.text=element_text(size=7),
        legend.title=element_text(size=8),
        plot.margin = unit(c(0.05,0.2,0.05,0.05),"cm")) +
  geom_line(size = 0.1) +
  scale_color_gradient(low="#a692fd", high="#FDB071") +
  labs(y="Predicted sound values", x="Step", color=expression(paste("Domestic travel\nprobability for\nthe Netherlands ×", 10)))
endofffile <- dev.off()
system("lualatex target_stage2_real.tex; rm *.aux; rm *.log")

# Very clear -> these are not linear data! Maybe logit can help us make the data linear
# for the regression analysis?
ggplot(d, aes(x = step, y = avg_sound_be_logit, color = domestic_travel_chance_nl_thousand)) +
  geom_point(size = 0.1) +
  scale_color_gradient(low="blue", high="red")

# Not quite. What if we model is logarithmically?
ggplot(d, aes(x = log(step), y = avg_sound_be_logit, color = domestic_travel_chance_nl_thousand)) +
  geom_point(size = 0.1) +
  scale_color_gradient(low="blue", high="red")

# Somewhat better...

# ---- Regression ----

# Run the regression
fit <- lm(avg_sound_be_logit ~ log(step) * domestic_travel_chance_nl_thousand, data = d)

# Output
summary(fit)

# Add the predicted data to the dataset
d$predicted <- predict(fit)

# What do the predictions look like on the logit scale?
d %>%
ggplot(aes(x = step, y = avg_sound_be_logit, color = domestic_travel_chance_nl_thousand)) +
  geom_point(alpha = 1, size=0.1) +
  scale_color_gradient(low="#a692fd", high="#FDB071") +
  geom_line(aes(y = predicted, group=domestic_travel_chance_nl_thousand), size = 0.1, alpha=0.2) +
  labs(y="Predicted sound values", x="Step", color=expression(paste("Domestic travel probability for the Netherlands ×", 10^3)))

# What do the predictions look like on the probability scale?
tikz(file = "target_stage1_real.tex", width=6, height=3, standAlone=TRUE)
d %>% filter(step == 1 | step %% 50 == 0) %>%
  ggplot(aes(x = step, y = ilogit(avg_sound_be_logit), color = domestic_travel_chance_nl_thousand, group=domestic_travel_chance_nl_thousand)) +
  theme(legend.text=element_text(size=7),
        legend.title=element_text(size=8),
        plot.margin = unit(c(0.05,0.2,0.05,0.05),"cm")) +
  geom_line(alpha = 1, size=0.1) +
  scale_color_gradient(low="#a692fd", high="#FDB071") +
  geom_line(aes(y = ilogit(predicted), group=domestic_travel_chance_nl_thousand), size = 0.1, alpha=0.2) +
  labs(y="Predicted sound values", x="Step", color=expression(paste("Domestic travel\nprobability for\nthe Netherlands ×", 10^3)))
endofffile <- dev.off()
system("lualatex target_stage1_real.tex; rm *.aux; rm *.log")

# Poor man's "all effects" plot
tikz(file = "target_stage1_predict.tex", width=6, height=3, standAlone=TRUE)
d %>% filter(step == 1 | step %% 50 == 0) %>%
  filter(domestic_travel_chance_nl_thousand %% 10 == 0) %>%
  mutate(domestic_travel_chance_nl_thousand = as.factor(domestic_travel_chance_nl_thousand)) %>%
  ggplot(aes(x = step, y=ilogit(predicted), color = domestic_travel_chance_nl_thousand, group=domestic_travel_chance_nl_thousand)) +
  theme(legend.text=element_text(size=7),
        legend.title=element_text(size=8),
        plot.margin = unit(c(0.05,0.2,0.05,0.05),"cm")) +
  guides(color = guide_legend(ncol = 2)) +
  geom_line(size = 0.1) +
  geom_line(aes(group = domestic_travel_chance_nl_thousand)) +
  labs(y="Predicted sound values", x="Step", color=expression(paste("Domestic travel\nprobability for\nthe Netherlands ×", 10^3)))
#facet_wrap(~ domestic_travel_chance_nl_thousand) # This looks bad
endofffile <- dev.off()
system("lualatex target_stage1_predict.tex; rm *.aux; rm *.log")


# Todo: maybe make colours prettier