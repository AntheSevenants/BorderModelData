options(scipen=999)

# --- Libraries ----
library(tidyverse)
library(effects)
library(tikzDevice)

# ---- Loading the contact dataset ----
d <- read_delim("contact_stage1.csv", ";")
d <- read_delim("contact_stage2.csv", ";")

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

# Multiply the abroad_travel_chance_be by 10000, so regression is easier to understand
d <- d %>% mutate(abroad_travel_chance_be_thousand = abroad_travel_chance_be * 10000)
# Create a log column
d <- d %>% mutate(abroad_travel_chance_be_log = log(abroad_travel_chance_be, 10))

# Performance trick... for now
#d <- d %>% filter(step < 1001)

# ---- Explorative plots ----

# First, let's just plot the data as-is

# Stage 1 data
d %>%
  ggplot(aes(x = step, y = avg_sound_be, color = abroad_travel_chance_be_thousand)) +
  geom_point(size = 0.1) +
  scale_color_gradient(low="#a692fd", high="#FDB071") +
  labs(y="Predicted sound values", x="Step", color=expression(paste("Abroad travel probability ×", 10^4)))

# Stage 2 data
tikz(file = "contact_stage2_real.tex", width=6, height=3, standAlone=TRUE)
d %>%
  filter(step == 1 | step %% 50 == 0) %>%
  mutate(abroad_travel_chance_be_log = as.factor(abroad_travel_chance_be_log)) %>%
  ggplot(aes(x = step, y = avg_sound_be, color = abroad_travel_chance_be_log, group= abroad_travel_chance_be_log)) +
  theme(legend.text=element_text(size=7),
        legend.title=element_text(size=8),
        plot.margin = unit(c(0.05,0.2,0.05,0.05),"cm")) +
  geom_line(size = 0.1) +
  labs(y="Predicted sound values", x="Step", color=expression(atop("Abroad travel  ", "probability (" ~ log[10] ~ ")")))
endofffile <- dev.off()
system("lualatex contact_stage2_real.tex; rm *.aux; rm *.log")

# Very clear -> these are not linear data! Maybe logit can help us make the data linear
# for the regression analysis?
ggplot(d, aes(x = step, y = avg_sound_be_logit, color = abroad_travel_chance_be_thousand)) +
  geom_point(size = 0.1) +
  scale_color_gradient(low="blue", high="red")

# Not quite. What if we model is logarithmically?
ggplot(d, aes(x = log(step), y = avg_sound_be_logit, color = abroad_travel_chance_be_thousand)) +
  geom_point(size = 0.1) +
  scale_color_gradient(low="blue", high="red")

# Somewhat better...

# ---- Regression ----

# Run the regression
fit <- lm(avg_sound_be_logit ~ log(step) * abroad_travel_chance_be_thousand, data = d)

# Output
summary(fit)

# Add the predicted data to the dataset
d$predicted <- predict(fit)

# What do the predictions look like on the logit scale?
d %>%
  ggplot(aes(x = log(step), y = avg_sound_be_logit, color = abroad_travel_chance_be_thousand)) +
  geom_point(alpha = 1, size=0.1) +
  scale_color_gradient(low="#a692fd", high="#FDB071") +
  geom_line(aes(y = predicted, group=abroad_travel_chance_be_thousand), size = 0.1, alpha=0.2) +
  labs(y="Predicted sound values", x="Step", color=expression(paste("Abroad travel probability ×", 10^4)))

# What do the predictions look like on the probability scale?
tikz(file = "contact_stage1_real.tex", width=6, height=3, standAlone=TRUE)
d %>%
  filter(step == 1 | step %% 50 == 0) %>%
  ggplot(aes(x = step, y = ilogit(avg_sound_be_logit), color = abroad_travel_chance_be_thousand, group = abroad_travel_chance_be_thousand)) +
  theme(legend.text=element_text(size=7),
        legend.title=element_text(size=8),
        plot.margin = unit(c(0.05,0.2,0.05,0.05),"cm")) +
  geom_line(alpha = 1, size=0.1) +
  scale_color_gradient(low="#a692fd", high="#FDB071") +
  geom_line(aes(y = ilogit(predicted), group=abroad_travel_chance_be_thousand), size = 0.1, alpha=0.2) +
labs(y="Predicted sound values", x="Step", color=expression(paste("Abroad travel\nprobability ×", 10^4)))
endofffile <- dev.off()
system("lualatex contact_stage1_real.tex; rm *.aux; rm *.log")

# Poor man's "all effects" plot
tikz(file = "contact_stage1_predict.tex", width=6, height=3, standAlone=TRUE)
d %>% 
  filter(step == 1 | step %% 50 == 0) %>%
  filter((abroad_travel_chance_be_thousand %% 10 == 0 & abroad_travel_chance_be_thousand != 0) | abroad_travel_chance_be_thousand == 1) %>%
  mutate(abroad_travel_chance_be_thousand = as.factor(abroad_travel_chance_be_thousand)) %>%
  ggplot(aes(x = step, y=ilogit(predicted), color = abroad_travel_chance_be_thousand)) +
  theme(legend.text=element_text(size=7),
        legend.title=element_text(size=8),
        plot.margin = unit(c(0.05,0.2,0.05,0.05),"cm")) +
  #geom_line(size = 0.1) +
  geom_line(aes(group = abroad_travel_chance_be_thousand)) +
  labs(y="Predicted sound values", x="Step", color=expression(paste("Abroad travel\nprobability ×", 10^4)))
#facet_wrap(~ abroad_travel_chance_be_thousand) # This looks bad
endofffile <- dev.off()
system("lualatex contact_stage1_predict.tex; rm *.aux; rm *.log")
# Todo: maybe make colours prettier