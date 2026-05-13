# =========================================================
# QQ plot example
# Author: Natalia Llonga
#
# This script uses simulated GWAS summary statistics to
# generate a QQ plot with genomic inflation factor (lambda).
# No real data are included.
# =========================================================

# Load required libraries
library(ggplot2)

# ---------------------------------------------------------
# 1. Simulate example GWAS p-values
# ---------------------------------------------------------

set.seed(123)

n_snps <- 10000
lambda_target <- 1.02

chi_sq <- rchisq(n_snps, df = 1) * lambda_target

p_values <- pchisq(
  chi_sq,
  df = 1,
  lower.tail = FALSE
)

# Add simulated association signals
signal_snps <- sample(1:n_snps, 20)

p_values[signal_snps] <- runif(
  length(signal_snps),
  min = 1e-10,
  max = 5e-8
)

qq_data <- data.frame(
  p_value = p_values
)

# ---------------------------------------------------------
# 2. Calculate genomic inflation factor
# ---------------------------------------------------------

observed_chi_sq <- qchisq(
  qq_data$p_value,
  df = 1,
  lower.tail = FALSE
)

lambda_gc <- median(observed_chi_sq, na.rm = TRUE) /
  qchisq(0.5, df = 1)

# ---------------------------------------------------------
# 3. Prepare observed and expected p-values
# ---------------------------------------------------------

qq_data <- qq_data[order(qq_data$p_value), , drop = FALSE]

qq_data$observed <- -log10(qq_data$p_value)

qq_data$expected <- -log10(
  ppoints(nrow(qq_data))
)

# ---------------------------------------------------------
# 4. Generate QQ plot
# ---------------------------------------------------------

qq_plot <- ggplot(
  qq_data,
  aes(
    x = expected,
    y = observed
  )
) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    linewidth = 0.7,
    color = "grey45"
  ) +
  geom_point(
    alpha = 0.75,
    size = 1.4,
    color = "#C97C9A"
  ) +
  annotate(
    "text",
    x = 0.15,
    y = max(qq_data$observed) * 0.94,
    label = paste0(
      "\u03BBGC = ",
      round(lambda_gc, 3)
    ),
    hjust = 0,
    size = 4.5,
    color = "grey20"
  ) +
  labs(
    title = "QQ plot",
    subtitle = "Simulated GWAS summary statistics",
    x = expression(Expected ~ -log[10](italic(P))),
    y = expression(Observed ~ -log[10](italic(P)))
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "grey35"),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    axis.line = element_line(linewidth = 0.5, color = "grey30"),
    axis.ticks = element_line(linewidth = 0.4, color = "grey30")
  )

# Display plot
qq_plot

# ---------------------------------------------------------
# 5. Save figure
# ---------------------------------------------------------

ggsave(
  filename = "figures/qq_plot_example.png",
  plot = qq_plot,
  width = 6,
  height = 6,
  dpi = 300
)
