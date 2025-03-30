# Family-Wise Error Rate (FWER)

This repository contains the code for visualizing the concepts of multiple hypothesis testing and the correction methods for controlling the type I error rate. The code generates plots for the family-wise error rate (FWER) and Bonferroni correction. If you want to learn more about the concepts of multiple hypothesis testing and the correction methods, please check the [Comparing samples—part II from Nature Methods](https://www.nature.com/articles/nmeth.2900).

Libraries we need to create the FWER distribution and Bonferroni _p-values_.
```r
library(tidyverse)
library(ggtext)
library(patchwork)
```

Plotting a family-wise error rate (FWER) using the formula `FWER = 1 - (1 – 𝛼)^k`. Where k is the number of tests and 𝛼 is the significance level. The FWER gives you the probability of making at least one type I error when performing multiple hypothesis tests.

```r
number_of_comparison <- seq(1, 100, 1)
alpha_levels <- c(0.05, 0.01, 0.001)

FWER <- expand.grid(number_of_comparison, alpha_levels) %>%
    rename(k = Var1, alpha = Var2) %>%
    dplyr::mutate(FWER = 1 - (1 - alpha)^k)

# we can check how many significant hits we have
FWER_significant <- FWER %>%
    filter(FWER <= 0.05) %>%
    group_by(alpha) %>%
    summarize(`significant hits` = n())

FWER_plot <- FWER %>%
    ggplot() +
    geom_point(aes(x = k,
                  y = FWER,
                  color = factor(alpha))) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 5)) +
  scale_color_brewer(palette = "Dark2") +
  guides(colour = guide_legend(override.aes = list(size = 6))) +
    labs(
        color = "Significance level (α)",
        x = "Number of comparisons",
        y = "FWER (%)",
        title = "Family-Wise Error Rate: FWER = 1 - (1 - α)<sup>k</sup>"
    ) +
  geom_hline(yintercept = 0.05,
             linetype = "dashed",
             color = "black",
             linewidth = 1) +
  theme_bw() +
  theme(
    text = element_text(size = 20),
    plot.title = element_markdown(hjust = 0.5, face = "bold"),
    legend.position = "bottom",
    legend.title.position = "top",
    legend.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black", face = "bold", size = 15),
    axis.ticks = element_line(color = "black"),
    line = element_blank()
  )
```

Plotting the Bonferroni correction using the formula `𝛼 = 𝛼 / k`. The Bonferroni correction is the simplest method to correct for multiple comparisons. It works well when the tests are independent and few in number. However, it is conservative when the tests are correlated or when the number of tests is large.

```r
Bonferroni_correction <- expand.grid(number_of_comparison, alpha_levels) %>%
    rename(k = Var1, alpha = Var2) %>%
    dplyr::mutate(Bonferroni = alpha / k)

Bonferroni_plot <- Bonferroni_correction %>%
    ggplot() +
    geom_point(aes(x = k,
                  y = Bonferroni,
                  color = factor(alpha))) +
  scale_y_continuous(limits = c(0, 0.05), breaks = seq(0, 0.05, 0.005)) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 5)) +
  scale_color_brewer(palette = "Dark2") +
  guides(colour = guide_legend(override.aes = list(size = 6))) +
    labs(
        color = "Significance level (α)",
        x = "Number of comparisons",
        y = "Bonferroni adjusted α",
        title = "Bonferroni Correction: α = α / k"
    ) +
  theme_bw() +
  theme(
    text = element_text(size = 20),
    plot.title = element_markdown(hjust = 0.5, face = "bold"),
    legend.position = "bottom",
    legend.title.position = "top",
    legend.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black", face = "bold", size = 15),
    axis.ticks = element_line(color = "black"),
    line = element_blank()
  )
```

Save the combined plot to your directory.

```{r}
type_I_error <- (FWER_plot + Bonferroni_plot) + plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave(
  filename = "type_I_error.png",
  plot = type_I_error,
  path = "plots",
  width = 18,
  height = 7,
  dpi = 300
)
```

<p align = "center">
<img src = "https://github.com/41ison/FWER/blob/main/FWER.png" width = "1000">
</p>
