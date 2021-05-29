options(scipen=999)

# --- Libraries ----
library(tidyverse)
library(effects)
library(tikzDevice)

# ---- Loading the artefact dataset ----
d <- read_delim("nl_artefact_test_stage1.csv", ";")

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

# Generate the logits for sphere_Breda
d <- d %>% mutate(sphere_Breda_logit = logit(sphere_Breda)) 

# Multiply the travel probability for NL by 1000, so regression is easier to understand
d <- d %>% mutate(domestic_travel_chance_nl_thousand = domestic_travel_chance_nl * 1000)

# ---- Explorative plots ----

# First, let's just plot the data as-is
ggplot(d, aes(x = step, y = sphere_Breda, color = domestic_travel_chance_nl_thousand)) +
  geom_point(size = 0.1) +
  scale_color_gradient(low="blue", high="red")

# Very clear -> these are not linear data! Maybe logit can help us make the data linear
# for the regression analysis?
ggplot(d, aes(x = step, y = sphere_Breda_logit, color = domestic_travel_chance_nl_thousand)) +
  geom_point(size = 0.1) +
  scale_color_gradient(low="blue", high="red")

# Better.

# ---- Regression ----

# Run the regression
fit <- lm(sphere_Breda_logit ~ log(step) * domestic_travel_chance_nl_thousand, data = d)

# Output
summary(fit)

# Add the predicted data to the dataset
d$predicted <- predict(fit)

# What do the predictions look like on the logit scale?
ggplot(d, aes(x = step, y = sphere_Breda_logit, color = domestic_travel_chance_nl_thousand)) +
  geom_point(alpha = 1, size=0.1) +
  scale_color_gradient(low="blue", high="red") +
  geom_line(aes(y = predicted, group=domestic_travel_chance_nl_thousand), size = 0.1, alpha=0.2)

# What do the predictions look like on the probability scale?
ggplot(d, aes(x = step, y = ilogit(sphere_Breda_logit), color = domestic_travel_chance_nl)) +
  geom_point(alpha = 1, size=0.1) +
  scale_color_gradient2(low="blue", high="red") +
  geom_line(aes(y = ilogit(predicted), group=domestic_travel_chance_nl), size = 0.1, alpha=0.2)

# Poor man's "all effects" plot
tikz(file = "nl_artefact_test.tex", width=6, height=3, standAlone=TRUE)
d %>% 
  filter(step == 1 | step %% 25 == 0) %>%
  filter((domestic_travel_chance_nl_thousand %% 10 == 0 & domestic_travel_chance_nl_thousand != 0) | domestic_travel_chance_nl_thousand == 1) %>%
  mutate(domestic_travel_chance_nl_thousand = as.factor(domestic_travel_chance_nl_thousand)) %>%
  ggplot(aes(x = step, y=ilogit(predicted), color = domestic_travel_chance_nl_thousand)) +
  theme_gray() +
  theme(legend.text=element_text(size=7),
        legend.title=element_text(size=8),
        plot.margin = unit(c(0.05,0.2,0.05,0.05),"cm")) +
  geom_line(size = 0.1) +
  geom_line(aes(group = domestic_travel_chance_nl_thousand)) +
  labs(y="Predicted sound values", x="Step", color=expression(paste("Domestic travel\nprobability ×", 10^3)))
  #facet_wrap(~ domestic_travel_chance_nl_thousand) # This looks bad
endofffile <- dev.off()
system("lualatex nl_artefact_test.tex; rm *.aux; rm *.log")

# Todo: maybe make colours prettier