# =========================================================
# Forest plot example
# Author: Natalia Llonga
#
# This script uses simulated data to generate a simple,
# reproducible forest plot. No real data are included.
# =========================================================

# Load required library
library(ggplot2)

# ---------------------------------------------------------
# 1. Create simulated example data
# ---------------------------------------------------------

forest_data <- data.frame(
  variable = c(
    "Variable 1",
    "Variable 2",
    "Variable 3",
    "Variable 4"
  ),
  estimate = c(0.28, 0.19, -0.11, 0.07),
  standard_error = c(0.06, 0.05, 0.04, 0.03)
)

# ---------------------------------------------------------
# 2. Calculate 95% confidence intervals
# ---------------------------------------------------------

forest_data$ci_lower <- forest_data$estimate - 1.96 * forest_data$standard_error
forest_data$ci_upper <- forest_data$estimate + 1.96 * forest_data$standard_error

# ---------------------------------------------------------
# 3. Generate forest plot
# ---------------------------------------------------------

forest_plot <- ggplot(
  forest_data,
  aes(
    x = estimate,
    y = variable,
    xmin = ci_lower,
    xmax = ci_upper
  )
) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed"
  ) +
  geom_errorbarh(
    height = 0.2
  ) +
  geom_point(
    size = 3
  ) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Example forest plot",
    subtitle = "Simulated effect estimates",
    x = "Effect estimate",
    y = NULL
  )

# Display plot
forest_plot

# ---------------------------------------------------------
# 4. Save figure
# ---------------------------------------------------------

ggsave(
  filename = "figures/forest_plot_example.png",
  plot = forest_plot,
  width = 8,
  height = 5,
  dpi = 300
)
