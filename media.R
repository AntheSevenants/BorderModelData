options(scipen=999)

# --- Libraries ----
library(tidyverse)
library(effects)
library(tikzDevice)

# ---- Loading the artefact dataset ----
d <- read_delim("media_stage1.csv", ";")
d <- read_delim("media_stage2.csv", ";")

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

# Multiply the media receptiveness by 100, so regression is easier to understand
d <- d %>% mutate(media_receptiveness_thousand = media_receptiveness * 100)
# Yes, it's the bug again. I don't know what causes it, but I'll have to live with it for now
d <- d %>% mutate(media_receptiveness_thousand = abroad_travel_chance_nl * 100) # stage 1 + 2

# Performance trick... for now
#d <- d %>% filter(step < 1001)

# ---- Explorative plots ----

# First, let's just plot the data as-is
tikz(file = "media_stage2_real.tex", width=6, height=3, standAlone = TRUE)
d %>%
  filter(step == 1 | step %% 50 == 0) %>%
  ggplot(aes(x = step, y = avg_sound_be, color = media_receptiveness_thousand, group = media_receptiveness_thousand)) +
  theme(legend.text=element_text(size=7),
        legend.title=element_text(size=8),
        plot.margin = unit(c(0.05,0.5,0,0),"cm")) +
  geom_line(size = 0.1) +
  scale_color_gradient(low="#a692fd", high="#FDB071") +
  labs(y="Predicted sound values", x="Step", color=expression(paste("Media\nreceptiveness ×", 10^2)))
endofffile <- dev.off()
system("lualatex media_stage2_real.tex; rm *.aux; rm *.log")

# Very clear -> these are not linear data! Maybe logit can help us make the data linear
# for the regression analysis?
ggplot(d, aes(x = step, y = avg_sound_be_logit, color = media_receptiveness_thousand)) +
  geom_point(size = 0.1) +
  scale_color_gradient(low="blue", high="red")

# Better.

# ---- Regression ----

# Run the regression
fit <- lm(avg_sound_be_logit ~ log(step) * media_receptiveness_thousand, data = d)

# Output
summary(fit)

# Add the predicted data to the dataset
d$predicted <- predict(fit)

# What do the predictions look like on the logit scale?
d %>%
ggplot(aes(x = log(step), y = avg_sound_be_logit, color = media_receptiveness_thousand)) +
  geom_point(alpha = 1, size=0.1) +
  scale_color_gradient(low="blue", high="red") +
  geom_line(aes(y = predicted, group=media_receptiveness_thousand), size = 0.1, alpha=0.2)

# What do the predictions look like on the probability scale?
tikz(file = "media_stage1_real.tex", width=6, height=3, standAlone = TRUE)
d %>%
  filter(step == 1 | step %% 50 == 0) %>%
  ggplot(aes(x = step, y = ilogit(avg_sound_be_logit), color = media_receptiveness_thousand, group = media_receptiveness_thousand)) +
  theme(legend.text=element_text(size=7),
        legend.title=element_text(size=8),
        plot.margin = unit(c(0.05,0.5,0,0),"cm")) +
  geom_line(alpha = 1, size=0.1) +
  scale_color_gradient(low="#a692fd", high="#FDB071") +
  geom_line(aes(y = ilogit(predicted), group=media_receptiveness_thousand), size = 0.1, alpha=0.2) +
  labs(y="Predicted sound values", x="Step", color=expression(paste("Media\nreceptiveness ×", 10^2)))
endofffile <- dev.off()
system("lualatex media_stage1_real.tex; rm *.aux; rm *.log")


# Poor man's "all effects" plot
tikz(file = "media_stage1_predict.tex", width=6, height=3, standAlone = TRUE)
d %>% 
  filter(step == 1 | step %% 50 == 0) %>%
  filter(media_receptiveness_thousand %% 10 == 0 | media_receptiveness_thousand == 1) %>%
  mutate(media_receptiveness_thousand = as.factor(media_receptiveness_thousand)) %>%
  ggplot(aes(x = step, y=ilogit(predicted), color = media_receptiveness_thousand)) +
  theme(legend.text=element_text(size=7),
        legend.title=element_text(size=8),
        plot.margin = unit(c(0.05,0.5,0,0),"cm")) +
  geom_line(size = 0.1) +
  geom_line(aes(group = media_receptiveness_thousand)) +
  labs(y="Predicted sound values", x="Step", color=expression(paste("Media\nreceptiveness ×", 10^2)))
#facet_wrap(~ media_receptiveness_thousand) # This looks bad
endofffile <- dev.off()
system("lualatex media_stage1_predict.tex; rm *.aux; rm *.log")

# Todo: maybe make colours prettier