# =========================================================
# Manhattan plot example
# Author: Natalia Llonga
#
# This script uses simulated GWAS summary statistics to
# generate a Manhattan plot.
# No real data are included.
# =========================================================

# Load required libraries
library(ggplot2)
library(dplyr)

# ---------------------------------------------------------
# 1. Simulate example GWAS summary statistics
# ---------------------------------------------------------

set.seed(123)

n_snps <- 10000

gwas_data <- data.frame(
  SNP = paste0("rs", 1:n_snps),
  CHR = sample(1:22, n_snps, replace = TRUE),
  BP = sample(1:1e8, n_snps, replace = TRUE),
  P = runif(n_snps)
)

# Add simulated association signals
signal_snps <- sample(1:n_snps, 20)
gwas_data$P[signal_snps] <- runif(20, min = 1e-10, max = 5e-8)

# ---------------------------------------------------------
# 2. Prepare data for plotting
# ---------------------------------------------------------

gwas_data <- gwas_data %>%
  arrange(CHR, BP)

chr_lengths <- gwas_data %>%
  group_by(CHR) %>%
  summarise(chr_length = max(BP), .groups = "drop") %>%
  mutate(chr_start = lag(cumsum(chr_length), default = 0))

gwas_data <- gwas_data %>%
  left_join(chr_lengths, by = "CHR") %>%
  mutate(
    cumulative_position = BP + chr_start,
    log_p = -log10(P)
  )

axis_data <- gwas_data %>%
  group_by(CHR) %>%
  summarise(
    center = mean(cumulative_position),
    .groups = "drop"
  )

# ---------------------------------------------------------
# 3. Define significance thresholds
# ---------------------------------------------------------

genome_wide_threshold <- -log10(5e-8)
suggestive_threshold <- -log10(1e-5)

# ---------------------------------------------------------
# 4. Generate Manhattan plot
# ---------------------------------------------------------

manhattan_plot <- ggplot(
  gwas_data,
  aes(
    x = cumulative_position,
    y = log_p,
    color = as.factor(CHR)
  )
) +
  geom_point(
    alpha = 0.8,
    size = 1.2
  ) +
  geom_hline(
    yintercept = genome_wide_threshold,
    linetype = "dashed",
    linewidth = 0.7,
    color = "#C97C9A"
  ) +
  geom_hline(
    yintercept = suggestive_threshold,
    linetype = "dotted",
    linewidth = 0.7,
    color = "#7389AE"
  ) +
  scale_color_manual(
    values = rep(c("#7389AE", "#C97C9A"), 11)
  ) +
  scale_x_continuous(
    labels = axis_data$CHR,
    breaks = axis_data$center
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, max(gwas_data$log_p) + 1)
  ) +
  labs(
    title = "Example Manhattan plot",
    subtitle = "Simulated GWAS summary statistics",
    x = "Chromosome",
    y = expression(-log[10](italic(P)))
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(size = 10)
  )

# Display plot
manhattan_plot

# ---------------------------------------------------------
# 5. Save figure
# ---------------------------------------------------------

ggsave(
  filename = "figures/manhattan_plot_example.png",
  plot = manhattan_plot,
  width = 11,
  height = 6,
  dpi = 300
)
