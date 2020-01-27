library(gridExtra)
library(grid)
library(ggplot2)

# join Unimodal RMSE plots

cp <- cowplot::plot_grid(um_rmse, um_rmse_skewed, nrow = 2)

ggsave(filename = "figures_outputs/combined_um_rmse.png", dpi = 300, width = 10,
       height = 12)

# Unimodal bias

umbias <- cowplot::plot_grid(um_bias, um_bias_skewed, nrow = 2)

ggsave(filename = "figures_outputs/combined_um_bias.png", dpi = 300, width = 10,
       height = 12)

#bimodal rmse
bmrmse <- cowplot::plot_grid(bm_rmse, bm_rmse_skewed, nrow = 2)

ggsave(filename = "figures_outputs/combined_bm_rmse.png", dpi = 300,  width = 10,
       height = 12)

#bimodal bias

bmbias <- cowplot::plot_grid(bm_bias, bm_bias_skewed, nrow = 2)

ggsave(filename = "figures_outputs/combined_bm_bias.png", dpi = 300,  width = 10,
       height = 12)
