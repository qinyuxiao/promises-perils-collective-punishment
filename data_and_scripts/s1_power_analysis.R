library(MPTinR)
library(tidyverse)

mpt_model <- "
# Baseline tree
d1+(1-d1)*p
(1-d1)*(1-p)
# CP tree
d2+(1-d2)*p
(1-d2)*(1-p)
"

d_cps <- seq(from = 0.2, to = 0.1, by = -0.005) # Estimated d in CP condition
power <- c()

set.seed(2024) # For reproducibility
for (d in d_cps) {
  sig_results <- c()
  
  for (i in 1:5000) { # 5000 random datasets
    # Generate random data
    dat <- gen.data(
      parameter.values = c(d1 = 0.25, d2 = d, p = 0.2),
      samples = 1,
      model.filename = textConnection(mpt_model),
      n.per.item.type = c(500, 500)
    )
    dat <- as.vector(dat)
    
    # Fit restricted model (force d1 = d2)
    r_model <- fit.model(
      data = dat, textConnection(mpt_model), list("p = 0.2", "d1 = d2"),
      output = "full", show.messages = FALSE
    )
    
    # Get p-value from model fit
    p_value = r_model$goodness.of.fit[1, "p.value"]
    sig_results[i] <- p_value < 0.05
  }
  
  # Compute power at this d
  power[match(d, d_cps)] <- mean(sig_results)
}

ggplot(as.data.frame(cbind(d_cps, power)), aes(x = d_cps, y = power)) +
  geom_point() +
  geom_smooth(se = FALSE, method = "loess") +
  labs(
    x = "Proportion of dishonest respondents (d) in the CP condition",
    y = "Statistical power"
  ) +
  scale_x_continuous(breaks = seq(0.1, 0.2, by = 0.01)) +
  scale_y_continuous(breaks = seq(0.2, 1, by = 0.1)) +
  geom_vline(xintercept = 0.14, linetype = "dashed", color = "gray") +
  geom_hline(yintercept = 0.80, linetype = "dashed", color = "red") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


# Sensitivity analysis (fix d_baseline and power)

d_cps <- seq(from = 0.35, to = 0.25, by = -0.005) # Estimated d in CP condition
power <- c()

set.seed(2025) # For reproducibility
for (d in d_cps) {
  sig_results <- c()
  
  for (i in 1:5000) { # 5000 random datasets
    # Generate random data
    dat <- gen.data(
      parameter.values = c(d1 = 0.3966, d2 = d, p = 0.2),
      samples = 1,
      model.filename = textConnection(mpt_model),
      n.per.item.type = c(522, 512)
    )
    dat <- as.vector(dat)
    
    # Fit restricted model (force d1 = d2)
    r_model <- fit.model(
      data = dat, textConnection(mpt_model), list("p = 0.2", "d1 = d2"),
      output = "full", show.messages = FALSE
    )
    
    # Get p-value from model fit
    p_value = r_model$goodness.of.fit[1, "p.value"]
    sig_results[i] <- p_value < 0.05
  }
  
  # Compute power at this d
  power[match(d, d_cps)] <- mean(sig_results)
}

ggplot(as.data.frame(cbind(d_cps, power)), aes(x = d_cps, y = power)) +
  geom_point() +
  geom_smooth(se = FALSE, method = "loess") +
  labs(
    x = "Proportion of dishonest respondents (d) in the CP condition",
    y = "Statistical power"
  ) +
  scale_x_continuous(breaks = seq(0.25, 0.35, by = 0.01)) +
  scale_y_continuous(breaks = seq(0.2, 1, by = 0.1)) +
  geom_vline(xintercept = 0.14, linetype = "dashed", color = "gray") +
  geom_hline(yintercept = 0.80, linetype = "dashed", color = "red") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
