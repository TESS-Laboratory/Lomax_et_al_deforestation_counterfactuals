#### Results visualisation script
#### Generates figures from results data and saves them to disk

source("scripts/load.R")

## 1. Set parameters --------

# Data selection
COUNTRIES <- c(
  "Bolivia",
  "Brazil",
  "Colombia",
  "Cote d'Ivoire",
  "Democratic Republic of the Congo",
  "Madagascar",
  "Indonesia",
  "Malaysia",
  "Myanmar"
)

START_YEAR <- 2016  # Simulated start year of protection project
RQ4_START_YEAR <- 1998  # Simulated start year for RQ4 simulations
POLY_SIZE <- c(10000, 60000, 600000) # size of polygons in hectares
VIZ_SEED <- 111
CUMULATIVE <- FALSE

## 2. Load data --------

# Country polygons for maps
country_polys <- map(COUNTRIES, gadm, level = 0, path = "data/raw/vector/gadm") %>%
  map(st_as_sf) %>%
  bind_rows()

# Polygons by country

# SC results by country
cumulative_flag <- ifelse(CUMULATIVE == TRUE, "_cumulative", "")

sc_results_df <- map(COUNTRIES, function(country) {
  country_df <- map(POLY_SIZE, function(size) {
    filename <- paste0("sc_results_", country, "_", START_YEAR, "_", size, cumulative_flag, ".csv")
    if(file.exists(paste0("results/sc_results/", filename))) {
      df <- read_csv(paste0("results/sc_results/", filename)) %>%
        mutate(poly_size = size)
      df
    } else {
      NULL
    }
  }) %>% 
    purrr::discard(\(x) is.null(x)) %>%
    purrr::discard(\(x) nrow(x) == 0) %>%
    bind_rows()
  
  country_df <- mutate(country_df, country = country)
  
  country_df
}) %>% bind_rows() %>%
  mutate(country = ifelse(country == "Democratic Republic of the Congo", "DRC", country)) %>%
  mutate(sc_loss = 100 * sc_loss, loss = 100 * loss)


# Variable importance by country
importance_df <- map(COUNTRIES, function(country) {
  filepath <- paste0("results/var_importance/", country, "_importance.csv")
  if (file.exists(filepath)) {
    filepath %>%
      read_csv() %>%
      mutate(country = country)
  } else {
    NULL
  }
}) %>%
  purrr::discard(\(x) is.null(x)) %>%
  bind_rows() %>%
  mutate(country = ifelse(country == "Democratic Republic of the Congo", "DRC", country))

# Deforestation rates by country (for population-level estimates)
defor <- map(COUNTRIES, function(country) {
  country_defor <- map(POLY_SIZE, function(size) {
    filename <- paste0(country, "_", size, "_", START_YEAR, "_data.rds")
    df <- read_rds(paste0("data/processed/rds/", filename)) %>% mutate(poly_size = size)
  }) %>% bind_rows()
  
  country_defor <- mutate(country_defor, country = country)
  
  country_defor
}) %>% bind_rows() %>%
  select(ID, country, poly_size, cum_loss, stratum, starts_with("loss")) %>%
  mutate(country = ifelse(country == "Democratic Republic of the Congo", "DRC", country))

## 3. Visualise SC trajectories --------

## Visualisation theme
sc_plot_theme <- theme(strip.background = element_rect(fill = "white", colour = "white"),
                          strip.text = element_text(face = "bold", size = 12),
                          legend.position = "right",
                          # legend.key.size = unit(1, "cm"),
                          legend.text = element_text(size = 12),
                          legend.title = element_text(size = 14),
                          axis.title = element_text(size = 14),
                          axis.text = element_text(size = 12),
                          # panel.grid.major = element_blank(),
                          # panel.grid.minor = element_blank()
)


set.seed(VIZ_SEED)
sc_sample <- sc_results_df %>%
  filter(match == 8 & poly_size == 60000) %>%
  group_by(country, ID) %>%
  summarise(ID_all = cur_group_id(),
            mean_loss = mean(loss[year >= START_YEAR]),
            stratum = first(stratum)) %>%
  group_by(country, stratum) %>%
  arrange(desc(mean_loss), .by_group = TRUE) %>%
  slice_sample(n = 1) %>%
  # mutate(country_id = LETTERS[1:n()]) %>%
  left_join(sc_results_df) %>%
  group_by(country) %>%
  mutate(max_loss = max(c(loss, sc_loss))) %>%
  ungroup()

ts_plots <- sc_sample %>%
  filter(match == 8 & poly_size == 60000 & year > (START_YEAR - match)) %>%
  mutate(stratum = paste0("Forest loss bin ", stratum)) %>%
  ggplot(aes(x = as.numeric(year))) +
  geom_line(aes(y = loss, colour = "Observed"), lwd = 1.2) +
  geom_line(aes(y = sc_loss, colour = as.factor(sim))) +
  geom_vline(xintercept = START_YEAR, colour = "grey20") +
  facet_grid(cols = vars(stratum), rows = vars(reorder(country, max_loss, decreasing = TRUE)), scales = "free_y") +
  theme_bw() +
  labs(x = "Year", y = "Annual forest loss\n(% of project area)", colour = "Simulation") +
  scale_colour_manual(
    # labels = c(paste0("S", 1:5), "Observed"),
    values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00', "grey10")) +
  sc_plot_theme

ts_plots

ggsave(
  paste0("results/figures/sc_plots/", START_YEAR, "_60000_SIM_ts_plots.jpg"),
  ts_plots,
  width = 24, height = 28, dpi = 300, units = "cm"
)

## 4. RQ1 - Boxplots of stratum-level performance by simulation --------

# Filter to RQ1 simulations and calculate simulation-level MAE and bias
stratum_error_test <- sc_results_df %>%
  filter(match == 8 & poly_size == 60000 & year > START_YEAR) %>%
  group_by(country, sim, ID, stratum) %>%
  summarise(mae = mean(abs(sc_loss - loss)),
            bias = mean(sc_loss - loss),
            mean_loss = mean(loss)) %>%
  group_by(country, sim, stratum) %>%
  mutate(mean_loss = mean(mean_loss))

# Summary error and bias metrics across (i) all polygons and (ii) by stratum

overall_error_by_sim <- stratum_error_test %>%
  group_by(sim) %>%
  summarise(mae_mean = mean(mae), bias_mean = mean(bias))

overall_error_by_sim_stratum <- stratum_error_test %>%
  group_by(sim, stratum) %>%
  summarise(mae_mean = mean(mae), bias_mean = mean(bias))

# Lookup table for positioning bin size labels (e.g., n = 25)
strata_n_lookup <- stratum_error_test %>%
  group_by(country, stratum) %>%
  summarise(n = length(unique(ID)),
            bias_label_y = max(bias) + 0.1 * (max(bias) - min(bias)),
            error_label_y = max(mae) + 0.1 * (max(mae) - min(mae))) %>%
  group_by(country) %>%
  mutate(bias_label_y = max(bias_label_y),
         error_label_y = max(error_label_y)) %>%
  ungroup() %>%
  mutate(max_bias_label_y = max(bias_label_y),
         max_error_label_y = max(error_label_y))

# Visualisation theme
panel_plot_theme <- theme(strip.background = element_rect(fill = "white", colour = "white"),
                          strip.text = element_text(face = "bold", size = 14),
                          legend.position = "inside",
                          legend.position.inside = c(0.9, 0.25),
                          legend.key.size = unit(1, "cm"),
                          legend.text = element_text(size = 14),
                          legend.title = element_text(size = 16),
                          axis.title = element_text(size = 16),
                          axis.text = element_text(size = 14),
                          # panel.grid.major = element_blank(),
                          # panel.grid.minor = element_blank()
)

# Error boxplot by simulation, bin and country 
strata_error_boxplot_sims <- stratum_error_test %>%
  {ggplot(., aes(x = as.factor(stratum))) +
      geom_boxplot(aes(y = mae, fill = as.factor(sim)), outliers = FALSE, coef = 1000) +
      # geom_hline(yintercept = 0, colour = "#F8766D") +
      geom_text(aes(label = paste0("n = ", n), x = as.factor(stratum), y = max_error_label_y),
                size = 4,
                data = strata_n_lookup) +
      scale_fill_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00'),
                        labels = paste0("S", unique(.$sim))) +
      facet_wrap(~country, scales = "fixed", ncol = 5, nrow = 2) +
      theme_bw() +
      labs(x = "Forest loss bin", y = "Mean absolute error\n(% of project area per year)",
           fill = "Covariate\nsimulation") +
      panel_plot_theme}

# strata_error_boxplot_legend <- ggpubr::get_legend(strata_error_boxplot_sims)
# 
# strata_error_boxplot_main <- strata_error_boxplot_sims +
#   theme(legend.position = "none")

# Bias boxplot by simulation, bin and country 
strata_bias_boxplot_sims <- stratum_error_test %>%
  {ggplot(., aes(x = as.factor(stratum))) +
      geom_hline(yintercept = 0, colour = "#F8766D") +
      geom_boxplot(aes(y = bias, fill = as.factor(sim)), outliers = FALSE, coef = 1000) +
      geom_text(aes(label = paste0("n = ", n), x = as.factor(stratum), y = max_bias_label_y),
                size = 4,
                data = strata_n_lookup) +
      scale_fill_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00'),
                        labels = paste0("S", unique(.$sim))) +
      facet_wrap(~country, scales = "fixed", ncol = 5, nrow = 2) +
      theme_bw() +
      labs(x = "Forest loss bin", y = "Mean bias\n(% of project area per year)",
           fill = "Matching\ncovariates") +
      panel_plot_theme}

ggsave("results/figures/boxplots/strata_error_boxplots_by_sim.png",
       strata_error_boxplot_sims,
       width = 32, height = 16, units = "cm", dpi = 300)

ggsave("results/figures/boxplots/strata_bias_boxplots_by_sim.png",
       strata_bias_boxplot_sims,
       width = 32, height = 16, units = "cm", dpi = 300)

## 5. RQ1 - Continuous plots of MAE and bias vs. observed forest loss --------

# Create summary df for easier plotting
sc_df_summary <- sc_results_df %>%
  mutate(period = ifelse(year > START_YEAR, "test", "matching")) %>%
  group_by(country, sim, poly_size, match, period, ID, stratum) %>%
  summarise(
    mean_loss = mean(loss),
    mean_sc_loss = mean(sc_loss),
    mae = mean(abs(sc_loss - loss)),
    bias = mean(sc_loss - loss)
  ) %>%
  pivot_wider(names_from = "period", values_from = c("mean_loss", "mean_sc_loss", "mae", "bias")) %>%
  ungroup()

sc_df_central <- filter(sc_df_summary, sim == 4 & match == 8 & poly_size == 60000)

theme_scatter <- theme(
  strip.background = element_rect(fill = "white", colour = "white"),
  strip.text = element_text(face = "bold", size = 16),
  axis.text = element_text(size = 14),
  axis.title = element_text(size = 16)
)

# Test period bias against test period observed forest loss
continuous_viz_bias_loss_country <- ggplot(sc_df_central, aes(x = mean_loss_test, y = bias_test)) +
  geom_hline(yintercept = 0, colour = "#F8766D") +
  geom_point(size = 0.5, show.legend = FALSE, alpha = 0.5) +
  facet_wrap(~country, scales = "fixed") +
  theme_bw() +
  labs(x = "Annual forest loss in post-intervention period\n(% of project area)",
       y = "Bias in post-intervention period\n(% of project area)")

continuous_viz_bias_loss_global <- sc_df_central %>%
  mutate(panel_label = "All countries") %>%
  ggplot(aes(x = mean_loss_test, y = bias_test)) +
  geom_hline(yintercept = 0, colour = "#F8766D") +
  geom_point(size = 1.2, alpha = 0.5) +
  facet_wrap(~panel_label) +
  # geom_smooth(method = "loess", colour = "steelblue") +
  # coord_cartesian(ylim = c(-2, 2)) +
  theme_bw() +
  labs(x = "Annual forest loss in post-intervention period\n(% of project area)",
       y = "Bias in post-intervention period\n(% of project area)")

continuous_viz_bias_loss_all <- continuous_viz_bias_loss_global +
  continuous_viz_bias_loss_country +
  plot_layout(axis_titles = "collect") &
  theme_scatter

continuous_viz_bias_loss_all

# MAE vs. test period forest loss
continuous_viz_mae_loss_country <- ggplot(sc_df_central, aes(x = mean_loss_test, y = mae_test)) +
  # geom_abline(intercept = 0, slope = 1, colour = "#F8766D") +
  geom_point(size = 0.5, alpha = 0.5, show.legend = FALSE) +
  facet_wrap(~country, scales = "fixed") +
  xlim(0, 7) + ylim(0, 7) +
  # scale_colour_viridis_c(option = "A") +
  theme_bw() +
  labs(x = "Annual forest loss in post-intervention period\n(% of project area)",
       y = "MAE in post-intervention period\n(% of project area)")

continuous_viz_mae_loss_global <- sc_df_central %>%
  mutate(panel_label = "All countries") %>%
  ggplot(aes(x = mean_loss_test, y = mae_test)) +
  # geom_abline(intercept = 0, slope = 1, colour = "#F8766D") +
  geom_point(size = 1.2, alpha = 0.5) +
  facet_wrap(~panel_label) +
  xlim(0, 7) + ylim(0, 7) +
  theme_bw() +
  labs(x = "Annual forest loss in post-intervention period\n(% of project area)",
       y = "MAE in post-intervention period\n(% of project area)")

continuous_viz_mae_loss_all <- continuous_viz_mae_loss_global + 
  continuous_viz_mae_loss_country +
  plot_layout(axis_titles = "collect") &
  theme_scatter

continuous_viz_mae_loss_all

ggsave("results/figures/scatter_plots/continuous_viz_bias_loss_all.png",
       continuous_viz_bias_loss_all,
       width = 30, height = 16, units = "cm", dpi = 300)

ggsave("results/figures/scatter_plots/continuous_viz_mae_loss_all.png",
       continuous_viz_mae_loss_all,
       width = 30, height = 16, units = "cm", dpi = 300)

# continuous_viz_global_mae_bias <- continuous_viz_mae_loss_global +
#   continuous_viz_bias_loss_global +
#   plot_layout(axis_titles = "keep")
# 
# ggsave("results/figures/scatter_plots/continuous_viz_global_mae_bias.png",
#        continuous_viz_global_mae_bias,
#        width = 30, height = 16, units = "cm", dpi = 300)

## 6. RQ1 - Continuous plots of test bias against matching period loss and bias ----

# Test period bias against matching period forest loss

# continuous_viz_bias_matching_loss_country <- ggplot(sc_df_central, aes(x = mean_loss_matching, y = bias_test)) +
#   geom_hline(yintercept = 0, colour = "#F8766D") +
#   geom_point(size = 0.5, show.legend = FALSE) +
#   facet_wrap(~country, scales = "free") +
#   theme_bw() +
#   labs(x = "Annual forest loss in pre-intervention\nperiod (% of polygon area)",
#        y = "Bias in post-intervention period\n(% of polygon area)") +
#   theme(strip.background = element_rect(fill = "white", colour = "white"),
#         strip.text = element_text(face = "bold", size = 14))

sc_df_central_cor <- sc_df_central %>%
  summarise(match_loss_mae_cor = cor(mean_loss_matching, mae_test),
            match_mae_mae_cor = cor(mae_matching, mae_test),
            match_loss_bias_cor = cor(mean_loss_matching, bias_test),
            match_bias_bias_cor = cor(bias_matching, bias_test)) %>%
  mutate(across(everything(), ~ paste0("r = ", round(.x, 2))))

continuous_viz_bias_matching_loss_global <- ggplot(sc_df_central, aes(x = mean_loss_matching, y = bias_test)) +
  geom_hline(yintercept = 0, colour = "#F8766D") +
  geom_point(size = 1.2, alpha = 0.5) +
  geom_text(data = sc_df_central_cor, aes(label = match_loss_bias_cor),
            x = 5, y = 2, size = 8) +
  theme_bw() +
  labs(x = "Annual forest loss in pre-intervention\nperiod (% of polygon area)",
       y = "Bias in post-intervention period\n(% of polygon area)")

# continuous_viz_bias_matching_loss_all <- continuous_viz_bias_matching_loss_country + 
#   continuous_viz_bias_matching_loss_global +
#   plot_layout(axis_titles = "collect") &
#   theme(
#     axis.text = element_text(size = 12),
#     axis.title = element_text(size = 14)
#   )
# 
# continuous_viz_bias_matching_loss_all

# continuous_viz_mae_matching_loss_global <- ggplot(sc_df_central, aes(x = mean_loss_matching, y = mae_test)) +
#   geom_point(size = 0.5) +
#   # geom_hline(yintercept = 0, colour = "#F8766D") +
#   theme_bw() +
#   labs(x = "Annual forest loss in matching\nperiod (% of polygon area)",
#        y = "Mean absolute error in test period\n(% of polygon area)")

# Test period bias against matching period bias

# continuous_viz_bias_matching_bias_country <- ggplot(sc_df_central, aes(x = bias_matching, y = bias_test)) +
#   geom_hline(yintercept = 0, colour = "#F8766D") +
#   geom_vline(xintercept = 0, colour = "#F8766D") +
#   geom_point(size = 0.5, show.legend = FALSE) +
#   facet_wrap(~country, scales = "free") +
#   theme_bw() +
#   labs(x = "Bias in pre-intervention period\n(% of polygon area)",
#        y = "Bias in post-intervention period\n(% of polygon area)")

continuous_viz_bias_matching_bias_global <- ggplot(sc_df_central, aes(x = bias_matching, y = bias_test)) +
  geom_hline(yintercept = 0, colour = "#F8766D") +
  geom_vline(xintercept = 0, colour = "#F8766D") +
  geom_point(size = 1.2, alpha = 0.5) +
  geom_text(data = sc_df_central_cor, aes(label = match_bias_bias_cor),
            x = 0.4, y = 2, size = 8) +
  # scale_x_reverse() +
  theme_bw() +
  labs(x = "Bias in pre-intervention period\n(% of polygon area)",
       y = "Bias in post-intervention period\n(% of polygon area)")

# continuous_viz_bias_matching_bias_all <- continuous_viz_bias_matching_bias_country + 
#   continuous_viz_bias_matching_bias_global +
#   plot_layout(axis_titles = "collect") &
#   theme_scatter
# 
# continuous_viz_bias_matching_bias_all

continuous_viz_bias_global_predictors <- continuous_viz_bias_matching_loss_global +
  continuous_viz_bias_matching_bias_global +
  plot_layout(axis_titles = "collect_y") &
  theme_scatter

continuous_viz_bias_global_predictors

ggsave("results/figures/scatter_plots/continous_test_bias_vs_predictors.png",
       continuous_viz_bias_global_predictors,
       width = 30, height = 16, units = "cm", dpi = 300)

## 5b. RQ1 - Continuous predicted vs. actual mean forest loss
## (alternative visualisation based on Rau et al. (2025))

pred_vs_actual_country <- ggplot(sc_df_central, aes(x = mean_loss_test, y = mean_sc_loss_test)) +
  geom_abline(slope = 1, intercept = 0, colour = "#F8766D") +
  geom_point(size = 0.75, alpha = 0.5, show.legend = FALSE) +
  facet_wrap(~country, scales = "fixed") +
  xlim(-0.02, 7) + ylim(-0.02, 7) +
  theme_bw() +
  labs(x = "Observed mean loss in project period (% yr-1)",
       y = "Predicted mean loss in project period (% yr-1)")

pred_vs_actual_global <- sc_df_central %>%
  mutate(dummy = "All countries") %>%
  ggplot(aes(x = mean_loss_test, y = mean_sc_loss_test)) +
  geom_abline(slope = 1, intercept = 0, colour = "#F8766D") +
  geom_point(size = 1.2, alpha = 0.5, show.legend = FALSE) +
  facet_wrap(~dummy, scales = "fixed") +
  xlim(-0.02, 7) + ylim(-0.02, 7) +
  theme_bw() +
  labs(x = "Observed mean loss in project period (% yr-1)",
       y = "Predicted mean loss in project period (% yr-1)")

pred_vs_actual_combined <- pred_vs_actual_country + pred_vs_actual_global +
  plot_layout(ncol = 2, axes = "collect") &
  theme_scatter

ggsave("results/figures/scatter_plots/continuous_pred_vs_observed_loss.png",
       pred_vs_actual_combined,
       width = 30, height = 16, units = "cm", dpi = 300)

## 7. RQ1 - Population and bin 4 mean bias and error by simulation --------

# Tables of overall MAE and bias by simulation and forest loss stratum

overall_error_bias_sim <- sc_df_summary %>%
  filter(poly_size == 60000 & match == 8) %>%
  group_by(sim,) %>%
  summarise(mean_mae = mean(mae_test),
            mean_bias = mean(bias_test)) %>%
  mutate(stratum = "All", sim = paste0("S", sim))

stratum_error_bias_sim <- sc_df_summary %>%
  filter(poly_size == 60000 & match == 8) %>%
  group_by(sim, stratum, country) %>%
  summarise(mean_mae = mean(mae_test),
            mean_bias = mean(bias_test)) %>%
  mutate(stratum = as.character(stratum), sim = paste0("S", sim))

error_bias_sim <- bind_rows(overall_error_bias_sim, stratum_error_bias_sim) %>%
  pivot_wider(names_from = "sim", values_from = c(mean_mae, mean_bias))

# Create population-level and stratum-level results

# Assign forest loss strata for all polygons
defor_long <- defor %>%
  st_drop_geometry() %>%
  pivot_longer(starts_with("loss")) %>%
  separate_wider_delim(name, delim = ".", names = c("var", "year")) %>%
  mutate(year = as.numeric(year)) %>%
  pivot_wider(names_from = var, values_from = value)

defor_long_loss <- defor_long %>%
  group_by(country, poly_size, ID) %>%
  summarise(test_loss = mean(loss * (year > START_YEAR)),
            match_loss = mean(loss * (year <= START_YEAR))) %>%
  group_by(country, poly_size) %>%
  mutate(stratum = cut_interval(test_loss, n = 4, labels = FALSE))

# Calculate relative frequency of strata for each country and polygon size
stratum_freq <- defor_long_loss %>%
  group_by(country, poly_size) %>%
  mutate(n = n(),
         test_loss_all = mean(test_loss),
         match_loss_all = mean(match_loss)) %>%
  group_by(country, poly_size, stratum) %>%
  summarise(n_stratum = n(),
            frac_stratum = n_stratum / mean(n),
            test_loss_all = mean(test_loss_all) * 100,
            match_loss_all = mean(match_loss_all) * 100,
            stratum_loss = mean(test_loss) * 100) 

# Back-calculate population-level errors for each country and polygon size (for text)

stratum_error <- sc_results_df %>%
  filter(year > START_YEAR) %>%
  group_by(country, poly_size, match, stratum, sim, ID) %>%
  summarise(mae = abs(mean(sc_loss) - mean(loss)) * 100,
            mean_bias = mean(sc_loss - loss) * 100,
            frac_mae = mae / mean(loss * 100),
            frac_bias = mean_bias / mean(loss * 100)) %>%
  group_by(country, poly_size, match, stratum, sim) %>%
  summarise(stratum_mae = mean(mae),
            stratum_bias = mean(mean_bias),
            mean_frac_mae = mean(frac_mae),
            mean_frac_bias = mean(frac_bias)) %>%
  left_join(stratum_freq)

pop_error <- stratum_error %>%
  group_by(country, poly_size, match, sim) %>%
  summarise(pop_mae = sum(frac_stratum * stratum_mae),
            pop_bias = sum(frac_stratum * stratum_bias),
            n = sum(n_stratum),
            test_loss_all = mean(test_loss_all),
            pop_mae_frac = pop_mae / test_loss_all,
            pop_bias_frac = pop_bias / test_loss_all,
            s4_mae = stratum_mae[stratum == 4],
            s4_bias = stratum_bias[stratum == 4],
            s4_n = n_stratum[stratum == 4],
            s4_loss = stratum_loss[stratum == 4],
            s4_mae_frac = s4_mae / s4_loss,
            s4_bias_frac = s4_bias / s4_loss)

## 8. RQ1 - Variable importance by country ----

importance_country <- importance_df %>%
  group_by(variable, country) %>%
  summarise(mean_weight = mean(min.loss.w, na.rm = TRUE),
            median_weight = median(min.loss.w, na.rm = TRUE),
            sd_weight = sd(min.loss.w, na.rm = TRUE),
            iqr_weight = IQR(min.loss.w, na.rm = TRUE)) %>%
  ungroup()

importance_ordered_global <- importance_country %>%
  group_by(variable) %>%
  summarise(mean_country_weight = mean(mean_weight, na.rm = TRUE),
            mean_median_weight = mean(median_weight, na.rm = TRUE)) %>%
  arrange(mean_country_weight)

variable_labels <- c(
  cropland = "Cropland fraction",
  protected_frac = "Protected fraction",
  dist_to_edge = "Distance to edge",
  ag_grp_frac = "Agriculture as % of GRP",
  grp_pc_usd_2015 = "GRP per capita (2015 US$)",
  biomass = "Forest biomass per hectare",
  jurisdiction_loss = "Jurisdiction forest loss rate",
  buffer_loss = "Buffer forest loss rate",
  dist_to_road = "Distance to road",
  pop_density = "Population density",
  ag_suitability = "Agricultural suitability index",
  fc_start = "Forest cover in start year",
  temperature_2m = "Mean annual temperature",
  time_to_port = "Travel time to nearest port",
  precipitation = "Mean annual precipitation",
  elevation = "Mean elevation",
  time_to_city = "Travel time to nearest city",
  slope = "Mean slope",
  dist_to_river = "Distance to river",
  eco_frac_shared = "Fraction of ecoregion shared",
  dist_to_treated = "Distance to treated unit"
)

importance_theme <- theme(strip.background = element_rect(fill = "white", colour = "white"),
                          strip.text = element_text(face = "bold", size = 14),
                          legend.text = element_text(size = 12),
                          legend.title = element_text(size = 14),
                          axis.title = element_text(size = 14),
                          axis.text = element_text(size = 12)
)

# Importance - all strata

importance_viz_country <- importance_country %>%
  mutate(variable = ordered(variable, levels = importance_ordered_global$variable)) %>%
  ggplot(aes(y = variable)) +
  # geom_boxplot(aes(x = min.loss.w), coef = 10) +
  # scale_x_log10() +
  geom_point(aes(x = median_weight), colour = "darkred") +
  # geom_errorbar(aes(xmin = lq_weight, xmax = uq_weight), alpha = 0.6) +
  facet_wrap(~country) +
  theme_bw() +
  scale_y_discrete(labels = variable_labels) +
  labs(x = "Mean weight", y = "Variable") +
  importance_theme

importance_viz_global <- importance_ordered_global %>%
  mutate(variable = ordered(variable, levels = importance_ordered_global$variable)) %>%
  group_by(country, variable) %>%
  summarise(mean_weight = mean(min.loss.w)) %>%
  group_by(variable) %>%
  summarise(mean_weight_global = mean(mean_weight)) %>%
  mutate(dummy_name = "All countries") %>%
  ggplot(aes(x = mean_weight_global, y = variable)) +
  geom_point(colour = "darkred") +
  facet_wrap(~dummy_name) +
  # geom_boxplot(coef = 10) +
  # scale_x_log10() +
  theme_bw() +
  labs(x = "Mean weight", y = "Variable", fill = "Forest loss\nstratum") +
  scale_y_discrete(labels = variable_labels) +
  importance_theme

importance_viz_all <- importance_viz_global + importance_viz_country +
  plot_layout(ncol = 2, axis_titles = "collect")

ggsave(plot = importance_viz_all, filename = "results/figures/importance_plots/importance_all.png",
       width = 32, height = 28, units = "cm", dpi = 300)

# Importance - S3-4 only

importance_ordered_s34 <- importance_df %>%
  filter(stratum %in% c(3,4)) %>%
  group_by(variable, country) %>%
  summarise(mean_weight = mean(min.loss.w, na.rm = TRUE)) %>%
  group_by(variable) %>%
  summarise(mean_country_weight = mean(mean_weight, na.rm = TRUE)) %>%
  arrange(mean_country_weight)


importance_viz_country_s34 <- importance_df %>%
  filter(stratum %in% c(3,4)) %>%
  mutate(variable = ordered(variable, levels = importance_ordered_s34$variable)) %>%
  group_by(country, variable) %>%
  summarise(
    mean_weight = mean(min.loss.w),
    median_weight = median(min.loss.w),
    uq_weight = quantile(min.loss.w, 0.75),
    lq_weight = quantile(min.loss.w, 0.25)
  ) %>%
  ggplot(aes(y = variable)) +
  # geom_boxplot(aes(x = min.loss.w), coef = 10) +
  # scale_x_log10() +
  geom_point(aes(x = mean_weight), colour = "darkred") +
  # geom_errorbar(aes(xmin = lq_weight, xmax = uq_weight), alpha = 0.6) +
  facet_wrap(~country) +
  theme_bw() +
  scale_y_discrete(labels = variable_labels) +
  labs(x = "Mean weight", y = "Variable") +
  importance_theme

importance_viz_global_s34 <- importance_df %>%
  mutate(variable = ordered(variable, levels = importance_ordered_s34$variable)) %>%
  filter(stratum %in% c(3,4)) %>%
  group_by(country, variable) %>%
  summarise(mean_weight = mean(min.loss.w)) %>%
  group_by(variable) %>%
  summarise(mean_weight_global = mean(mean_weight)) %>%
  mutate(dummy_name = "All countries") %>%
  ggplot(aes(x = mean_weight_global, y = variable)) +
  geom_point(colour = "darkred") +
  facet_wrap(~dummy_name) +
  # geom_boxplot(coef = 10) +
  # scale_x_log10() +
  theme_bw() +
  labs(x = "Mean weight", y = "Variable", fill = "Forest loss\nstratum") +
  scale_y_discrete(labels = variable_labels) +
  importance_theme

importance_viz_all_s34 <- importance_viz_global_s34 + importance_viz_country_s34 +
  plot_layout(ncol = 2, axis_titles = "collect")

ggsave(plot = importance_viz_all_s34, filename = "results/figures/importance_plots/importance_all_s34.png",
       width = 32, height = 28, units = "cm", dpi = 300)

# importance_viz_global_s234 <- importance_df %>%
#   filter(stratum >= 2) %>%
#   group_by(country, variable) %>%
#   summarise(mean_weight = mean(min.loss.w)) %>%
#   group_by(variable) %>%
#   summarise(mean_weight_global = mean(mean_weight)) %>%
#   ggplot(aes(x = mean_weight_global, y = variable, fill = variable)) +
#   geom_col() +
#   # facet_wrap(~country) +
#   theme_classic()
# 
# importance_viz_country_boxplot_s34 <- importance_df %>%
#   filter(stratum %in% c(3, 4)) %>%
#   ggplot(aes(x = min.loss.w, y = variable, fill = variable)) +
#   geom_boxplot() +
#   facet_wrap(~country) +
#   theme_classic()



## 9. RQ2 - Boxplots of mean absolute error and bias by polygon size --------

# pop_error_poly_viz <- pop_error %>%
#   filter(match == 8 & sim == 5) %>%
#   pivot_longer(cols = c(pop_mae, mean_loss_all)) %>%
#   ggplot(aes(x = as.factor(poly_size), y = value, colour = name)) +
#   geom_point(shape = 18, size = 2) +
#   # geom_point(
#   #   aes(y = mean_loss_all, colour = "Mean forest\nloss rate\n(% of polygon\narea)"),
#   #   shape = 18, size = 2) +
#   theme_few() +
#   facet_wrap(~country) +
#   scale_colour_manual(labels = c("Mean forest loss rate", "MAE"), values = c("black", "blue")) +
#   ylim(0, 0.5) +
#   labs(x = "polygon size", y = "% of polygon area", colour = "",
#        title = "Population-level mean prediction error by polygon size") +
#   theme(strip.background = element_rect(fill = "white", colour = "white"),
#         strip.text = element_text(face = "bold"))
# 
# stratum_4_error_poly_viz <- stratum_error %>%
#   filter(match == 8 & stratum == 4 & sim == 5) %>%
#   pivot_longer(cols = c(stratum_mae, stratum_loss)) %>%
#   ggplot(aes(x = as.factor(poly_size), y = value, colour = name)) +
#   geom_point(shape = 18, size = 2) +
#   theme_few() +
#   facet_wrap(~country) +
#   scale_colour_manual(labels = c("Mean forest loss rate", "MAE"), values = c("black", "red")) +
#   ylim(0, 12) +
#   labs(x = "polygon size", y = "% of polygon area", colour = "",
#        title = "Upper stratum mean prediction error by match period") +
#   theme(strip.background = element_rect(fill = "white", colour = "white"),
#         strip.text = element_text(face = "bold"))
# 
# frac_error_poly_viz <- pop_error %>%
#   filter(match == 8 & sim == 5) %>%
#   pivot_longer(cols = ends_with("mae_frac")) %>%
#   ggplot(aes(x = as.factor(poly_size), y = value, colour = name)) +
#   geom_point(shape = 18, size = 2) + 
#   scale_colour_manual(labels = c("Population", "Highest loss bin"), values = c("blue", "red")) +
#   geom_hline(yintercept = 1, colour = "#F8766D") +
#   theme_few() +
#   facet_wrap(~country) +
#   ylim(0, 1.5) +
#   labs(x = "polygon size", y = "Mean absolute prediction error\n(fraction of mean loss rate)", colour = "",
#        title = "Fractional prediction error by polygon size") +
#   theme(strip.background = element_rect(fill = "white", colour = "white"),
#         strip.text = element_text(face = "bold"))

# Boxplots of performance range by polygon size

stratum_error_test_poly_size <- sc_results_df %>%
  filter(match == 8 & sim == 4 & year > START_YEAR) %>%
  group_by(country, poly_size, ID, stratum) %>%
  summarise(mae = mean(abs(sc_loss - loss)),
            bias = mean(sc_loss - loss),
            mean_loss = mean(loss)) %>%
  group_by(country, poly_size, stratum) %>%
  mutate(mean_loss = mean(mean_loss))

strata_error_boxplot_poly_size <- stratum_error_test_poly_size %>%
  mutate(poly_size = ordered(paste0(poly_size / 1000, "k"),
                             levels = c("10k", "60k", "600k"))) %>%
  ggplot(aes(x = as.factor(stratum), y = mae, fill = as.factor(poly_size))) +
  geom_boxplot(outlier.size = 0.2) +
  # geom_point(aes(y = mean_loss), colour = "red", size = 3, shape = 18, show.legend = FALSE) +
  facet_wrap(~country, ncol = 5, nrow = 2) +
  scale_fill_brewer(palette = "Set2") +
  theme_bw() +
  labs(x = "Forest loss bin", y = "Mean absolute prediction error\n(% of project area per year)",
       fill = "Project size (ha)") +
  panel_plot_theme

strata_bias_boxplot_poly_size <- stratum_error_test_poly_size %>%
  mutate(poly_size = ordered(paste0(poly_size / 1000, "k"),
                             levels = c("10k", "60k", "600k"))) %>%
  ggplot(aes(x = as.factor(stratum), y = bias, fill = as.factor(poly_size))) +
  geom_hline(yintercept = 0, colour = "#F8766D") +
  geom_boxplot(outlier.size = 0.2, coef = 1000) +
  # geom_point(aes(y = mean_loss), colour = "red", size = 3, shape = 18, show.legend = FALSE) +
  facet_wrap(~country, scales = "fixed", ncol = 5, nrow = 2) +
  scale_fill_brewer(palette = "Set2") +
  theme_bw() +
  labs(x = "Forest loss bin", y = "Mean bias\n(% of project area per year)",
       fill = "Project size (ha)") +
  panel_plot_theme

ggsave("results/figures/boxplots/mae_boxplot_by_poly_size.png",
       strata_error_boxplot_poly_size,
       width = 32, height = 16, units = "cm", dpi = 300)

ggsave("results/figures/boxplots/bias_boxplot_by_poly_size.png",
       strata_bias_boxplot_poly_size,
       width = 32, height = 16, units = "cm", dpi = 300)

## 10. RQ3 - Mean absolute error and bias by match period --------

MATCH_SIM <- 4

## WRAP INTO FUNCTION FOR REPRODUCIBILITY (with arguments for simulation, poly_size etc.)

stratum_error_by_match <- sc_results_df %>%
  filter(poly_size == 60000 & year > START_YEAR) %>%
  group_by(country, ID, stratum, match, sim) %>%
  summarise(mean_loss = mean(loss),
            mae = mean(abs(sc_loss - loss)),
            bias = mean(sc_loss - loss))

stratum_error_bias_summary <- stratum_error_by_match %>%
  filter(sim == MATCH_SIM) %>%
  group_by(country, stratum, match, sim) %>%
  summarise(stratum_mae = mean(mae),
            stratum_mae_upper = max(mae),
            stratum_mae_lower = min(mae),
            stratum_bias = mean(bias),
            stratum_bias_upper = max(bias),
            stratum_bias_lower = min(bias)) %>%
  ungroup()

stratum_error_bias_overall <- stratum_error_by_match %>%
  group_by(stratum, sim, match) %>%
  summarise(mean_bias = mean(bias),
            mean_error = mean(mae))

stratum_error_match_viz <- ggplot(
  stratum_error_bias_summary,
  aes(x = match, y = stratum_mae, ymin = stratum_mae_lower, ymax = stratum_mae_upper, fill = as.factor(stratum), colour = after_scale(fill))
  ) +
  geom_hline(yintercept = 0, colour = "black") +
  geom_point() +
  geom_ribbon(alpha = 0.1, colour = NA) +
  geom_line(alpha = 0.5) +
  theme_bw() +
  facet_wrap(~country, scales = "free", ncol = 5, nrow = 2) +
  # ylim(0, 6) +
  scale_x_continuous(breaks = seq(0, 24, 4)) +
  scale_colour_brewer(palette = "Set1") +
  labs(x = "Match period (years)", y = "Mean absolute prediction error\n(% of polygon area)", colour = "Forest\nloss bin") +
  panel_plot_theme

stratum_bias_match_viz <- ggplot(
  stratum_error_bias_summary,
  aes(x = match, y = stratum_bias, ymin = stratum_bias_lower, ymax = stratum_bias_upper, fill = as.factor(stratum), colour = after_scale(fill))
  ) +
  geom_hline(yintercept = 0, colour = "black") +
  geom_point() +
  geom_ribbon(alpha = 0.1, colour = NA) +
  geom_line(alpha = 0.5) +
  theme_bw() +
  facet_wrap(~country, scales = "free", ncol = 5, nrow = 2) +
  # ylim(-6, 6) +
  scale_x_continuous(breaks = seq(0, 24, 4)) +
  scale_colour_brewer(palette = "Set1") +
  labs(x = "Match period (years)", y = "Mean bias\n(% of polygon area)", colour = "Forest\nloss bin", fill = "Forest\nloss bin") +
  panel_plot_theme

ggsave(paste0("results/figures/line_plots/mae_by_country_bins_match_s", MATCH_SIM, ".png"),
       stratum_error_match_viz,
       width = 32, height = 16, units = "cm", dpi = 300)

ggsave(paste0("results/figures/line_plots/bias_by_country_bins_match_s", MATCH_SIM, ".png"),
       stratum_bias_match_viz,
       width = 32, height = 16, units = "cm", dpi = 300)

# # Boxplot (alternative)
# 
# # S4
# 
# strata_error_boxplot_match <- stratum_error_by_match %>%
#   filter(sim == 4) %>%
#   ggplot(aes(x = as.factor(stratum), y = mae, fill = as.factor(match))) +
#   geom_boxplot(coef = 1000, outliers = FALSE) +
#   # geom_point(aes(y = mean_loss), colour = "red", size = 3, shape = 18, show.legend = FALSE) +
#   facet_wrap(~country, scales = "free", ncol = 5, nrow = 2) +
#   scale_fill_brewer(palette = "Blues") +
#   theme_bw() +
#   labs(x = "Forest loss bin", y = "Mean absolute prediction error\n(% of project area per year)",
#        fill = "Match period (years)") +
#   theme(strip.background = element_rect(fill = "white", colour = "white"),
#         strip.text = element_text(face = "bold"))
# 
# strata_bias_boxplot_match <- stratum_error_by_match %>%
#   filter(sim == 4) %>%
#   ggplot(aes(x = as.factor(stratum), y = bias, fill = as.factor(match))) +
#   geom_boxplot(coef = 1000, outliers = FALSE) +
#   geom_hline(yintercept = 0, colour = "#F8766D") +
#   facet_wrap(~country, scales = "free", ncol = 5, nrow = 2) +
#   scale_fill_brewer(palette = "Blues") +
#   theme_bw() +
#   labs(x = "Forest loss bin", y = "Mean bias\n(% of project area per year)",
#        fill = "Match period (years)") +
#   panel_plot_theme
# 
# # Save to disk
# 
# ggsave("results/figures/boxplots/mae_boxplot_by_match_s4.png",
#        strata_error_boxplot_match,
#        width = 24, height = 16, units = "cm", dpi = 300)
# 
# ggsave("results/figures/boxplots/bias_boxplot_by_match_s4.png",
#        strata_bias_boxplot_match,
#        width = 24, height = 16, units = "cm", dpi = 300)

## 11. RQ4 - Performance of SC method over a long test period --------

sc_df_rq4 <- map(COUNTRIES, function(country) {
  filepath <- paste0("results/sc_results/sc_results_", country, "_1998_60000.csv")
  
  country_df <- filepath %>%
    read_csv() %>%
    mutate(country = country)
  
  country_df
}) %>% bind_rows() %>%
  mutate(country = ifelse(country == "Democratic Republic of the Congo", "DRC", country))

annual_performance_rq4 <- sc_df_rq4 %>%
  mutate(loss = 100 * loss,
         sc_loss = 100 * sc_loss) %>%
  # filter(year > RQ4_START_YEAR) %>%
  group_by(country, stratum, year) %>%
  summarise(mean_loss = mean(loss),
            mean_error = mean(abs(sc_loss - loss)),
            mean_bias = mean(sc_loss - loss),
            max_error = max(abs(sc_loss - loss)),
            min_error = min(abs(sc_loss - loss)),
            max_bias = max(sc_loss - loss),
            min_bias = min(sc_loss - loss)) %>%
  ungroup()

year_label_df <- data.frame(
  country = factor("Bolivia", levels = unique(sc_df_rq4$country)),
  x = 1998, y = 2,
  label = "Project start"
)

annual_performance_rq4_smoothed <- annual_performance_rq4 %>%
  group_by(country, stratum) %>%
  mutate(bias_smoothed = (ksmooth(year, mean_bias, kernel = "normal", bandwidth = 7, x.points = year))$y,
         mae_smoothed = (ksmooth(year, mean_error, kernel = "normal", bandwidth = 7, x.points = year))$y)

annual_bias_rq4_viz <- ggplot(annual_performance_rq4_smoothed, aes(x = year, colour = as.factor(stratum))) +
  geom_hline(yintercept = 0, colour = "grey20") +
  geom_vline(xintercept = 1998, colour = "grey20", lwd = 1) +
  # geom_ribbon(aes(ymin = min_bias, ymax = max_bias), colour = NA, alpha = 0.2) +
  # geom_rect(xmin = 1991, ymin = -0.04, xmax = 1998, ymax = 0.01, fill = "grey90", colour = "grey90", alpha = 0.5) +
  geom_line(aes(y = mean_bias), alpha = 0.3, lwd = 0.6) +
  geom_line(aes(y = bias_smoothed), alpha = 0.8, lwd = 1) +
  # geom_smooth(se = FALSE) +
  # geom_point() +
  geom_label(data = year_label_df, label = "Project start", aes(x = x, y = y), fill = "white", colour = "grey20", size = 3) +
  facet_wrap(~country, ncol = 5, nrow = 2) +
  theme_bw() +
  scale_x_continuous(breaks = seq(1990, 2024, 8)) +
  # coord_cartesian(ylim = c(-10, 10)) +
  scale_colour_brewer(palette = "Set1") +
  labs(x = "Year", y = "Mean bias\n(% of polygon area)", colour = "Forest loss bin") +
  panel_plot_theme +
  theme

ggsave("results/figures/line_plots/bias_rq4.png",
       annual_bias_rq4_viz,
       width = 32, height = 16, units = "cm", dpi = 300)

## 12. Comparison of bias for filtered vs unfiltered donor pool ----

# Load SC results for filtered donor pools
sc_results_df_filtered <- map(COUNTRIES, function(country) {
  filename <- paste0("sc_results_", country, "_2016_60000_filtered.csv")
  if(file.exists(paste0("results/sc_results/", filename))) {
    read_csv(paste0("results/sc_results/", filename)) %>%
      mutate(country = country)
    } else {
    NULL
    }
  }) %>% 
  purrr::discard(\(x) is.null(x)) %>%
  purrr::discard(\(x) nrow(x) == 0) %>%
  bind_rows() %>%
  mutate(country = ifelse(country == "Democratic Republic of the Congo", "DRC", country)) %>%
  mutate(sc_loss = 100 * sc_loss, loss = 100 * loss)

# Join to compare filtered vs. unfiltered values and calculate error/bias
filtered_vs_unfiltered <- sc_results_df %>%
  filter(sim == 4 & match == 8 & poly_size == 60000) %>%
  left_join(
    sc_results_df_filtered,
    by = c("sim", "ID", "stratum", "year", "country"),
    suffix = c("_unfiltered", "_filtered"))

filtered_vs_unfiltered_error <- filtered_vs_unfiltered %>%
  filter(year > START_YEAR) %>%
  group_by(country, stratum, sim, ID) %>%
  summarise(
    mean_loss_unfiltered = mean(loss_unfiltered),
    mean_sc_loss_unfiltered = mean(sc_loss_unfiltered),
    mean_loss_filtered = mean(loss_filtered),
    mean_sc_loss_filtered = mean(sc_loss_filtered),
    mae_unfiltered = mean(abs(sc_loss_unfiltered - loss_unfiltered)),
    bias_unfiltered = mean(sc_loss_unfiltered - loss_unfiltered),
    mae_filtered = mean(abs(sc_loss_filtered - loss_filtered)),
    bias_filtered = mean(sc_loss_filtered - loss_filtered)
  )

# Plot bias vs. observed loss rate for filtered donor SCs

# continuous_viz_bias_loss_country_filtered <- ggplot(filtered_vs_unfiltered_error, aes(x = mean_loss_filtered, y = bias_filtered)) +
#   geom_hline(yintercept = 0, colour = "#F8766D") +
#   geom_point(size = 0.5, show.legend = FALSE, alpha = 0.5) +
#   facet_wrap(~country, scales = "fixed") +
#   theme_bw() +
#   labs(x = "Annual forest loss in post-intervention period\n(% of project area)",
#        y = "Bias in post-intervention period\n(% of project area)")

continuous_viz_bias_loss_global_filtered <- filtered_vs_unfiltered_error %>%
  mutate(panel_label = "Filtered donor pool") %>%
  ggplot(aes(x = mean_loss_filtered, y = bias_filtered)) +
  geom_hline(yintercept = 0, colour = "#F8766D") +
  geom_point(size = 1.2, alpha = 0.5) +
  facet_wrap(~panel_label) +
  ylim(-6, 4) +
  theme_bw() +
  labs(x = "Annual forest loss in post-intervention period\n(% of project area)",
       y = "Bias in post-intervention period\n(% of project area)")

continuous_viz_bias_loss_global_unfiltered <- filtered_vs_unfiltered_error %>%
  mutate(panel_label = "Unfiltered donor pool") %>%
  ggplot(aes(x = mean_loss_unfiltered, y = bias_unfiltered)) +
  geom_hline(yintercept = 0, colour = "#F8766D") +
  geom_point(size = 1.2, alpha = 0.5) +
  facet_wrap(~panel_label) +
  ylim(-6, 4) +
  theme_bw() +
  labs(x = "Annual forest loss in post-intervention period\n(% of project area)",
       y = "Bias in post-intervention period\n(% of project area)")

continuous_viz_filtered_vs_unfiltered_bias <- continuous_viz_bias_loss_global_filtered +
  continuous_viz_bias_loss_global_unfiltered +
  plot_layout(axis_titles = "collect") &
  theme_scatter

continuous_viz_filtered_vs_unfiltered_bias

ggsave("results/figures/scatter_plots/filtered_vs_unfiltered_bias.png",
       continuous_viz_filtered_vs_unfiltered_bias,
       units = "cm", width = 30, height = 16, dpi = 300)

# Plot MAE vs. observed loss rate for filtered donor SCs
continuous_viz_mae_loss_country_filtered <- ggplot(filtered_vs_unfiltered_error, aes(x = mean_loss_filtered, y = mae_filtered)) +
  geom_abline(slope = 1, intercept = 0, colour = "#F8766D") +
  geom_point(size = 0.5, show.legend = FALSE, alpha = 0.5) +
  facet_wrap(~country, scales = "fixed") +
  theme_bw() +
  labs(x = "Annual forest loss in post-intervention period\n(% of project area)",
       y = "MAE in post-intervention period\n(% of project area)")

continuous_viz_mae_loss_global_filtered <- filtered_vs_unfiltered_error %>%
  mutate(panel_label = "All countries") %>%
  ggplot(aes(x = mean_loss_filtered, y = mae_filtered)) +
  geom_abline(slope = 1, intercept = 0, colour = "#F8766D") +
  geom_point(size = 1.2, alpha = 0.5) +
  facet_wrap(~panel_label) +
  # geom_smooth(method = "loess", colour = "steelblue") +
  # coord_cartesian(ylim = c(-2, 2)) +
  theme_bw() +
  labs(x = "Annual forest loss in post-intervention period\n(% of project area)",
       y = "MAE in post-intervention period\n(% of project area)")

continuous_viz_mae_loss_all_filtered <- continuous_viz_mae_loss_global_filtered +
  continuous_viz_mae_loss_country_filtered +
  plot_layout(axis_titles = "collect") &
  theme_scatter

continuous_viz_mae_loss_all_filtered

ggsave("results/figures/scatter_plots/continuous_viz_bias_loss_all_filtered.png",
       continuous_viz_bias_loss_all_filtered,
       width = 30, height = 16, units = "cm", dpi = 300)

ggsave("results/figures/scatter_plots/continuous_viz_mae_loss_all_filtered.png",
       continuous_viz_mae_loss_all_filtered,
       width = 30, height = 16, units = "cm", dpi = 300)

# Plot filtered vs. unfiltered error and bias

filtered_vs_unfiltered_mae <- ggplot(filtered_vs_unfiltered_error, aes(x = mae_unfiltered, y = mae_filtered)) +
  # geom_vline(xintercept = 0, colour = "grey60") +
  # geom_hline(yintercept = 0, colour = "grey60") +
  geom_abline(slope = 1, intercept = 0, colour = "#F8766D") +
  geom_point(size = 1.2, alpha = 0.5) +
  facet_wrap(~stratum, scales = "fixed", labeller = as_labeller(function(x) paste0("Forest loss bin ", x))) +
  xlim(0, 5) + ylim(0, 5) +
  theme_bw() +
  theme_scatter +
  labs(x = "Post-intervention MAE - unfiltered donor pool\n(% of project area)",
       y = "Post-intervention MAE - filtered donor pool\n(% of project area)")


filtered_vs_unfiltered_bias <- ggplot(filtered_vs_unfiltered_error, aes(x = bias_unfiltered, y = bias_filtered)) +
  geom_vline(xintercept = 0, colour = "grey60") +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_abline(slope = 1, intercept = 0, colour = "#F8766D") +
  geom_point(size = 1.2, alpha = 0.5) +
  facet_wrap(~stratum, scales = "fixed", labeller = as_labeller(function(x) paste0("Forest loss bin ", x))) +
  xlim(-5, 5) + ylim(-5, 5) +
  theme_bw() +
  theme_scatter +
  labs(x = "Post-intervention bias - unfiltered donor pool\n(% of project area)",
       y = "Post-intervention bias - filtered donor pool\n(% of project area)")

ggsave("results/figures/scatter_plots/filtered_vs_unfiltered_mae.png",
       filtered_vs_unfiltered_mae,
       width = 20, height = 16, units = "cm", dpi = 300)

ggsave("results/figures/scatter_plots/filtered_vs_unfiltered_bias.png",
       filtered_vs_unfiltered_bias,
       width = 20, height = 16, units = "cm", dpi = 300)

## 13. Sampling scheme map figures ----

# Load data lookup table (needed for data loading functions)

data_lookup <- read_csv("data/raw/csv/data_lookup.csv")

# Load country boundary, protected areas and FC raster
EXAMPLE_COUNTRY <- "Bolivia"
CRS <- "ESRI:54034"

country_poly <- gadm(EXAMPLE_COUNTRY, level = 0, path = "data/raw/vector/gadm") %>%
  st_as_sf() %>%
  st_transform(CRS)

country_fc <- get_tiled_raster("data/raw/raster/tmf_binary_agg", match = EXAMPLE_COUNTRY, names = paste0("fc.", 1990:2023))
country_fc_start <- country_fc %>%
  subset("fc.1990") %>%
  terra::project(CRS) %>%
  terra::aggregate(fact = 10) %>%
  terra::mask(country_poly)

country_fc_masked <- mask(country_fc_start, country_fc_start, maskvalues = c(0, NA))

protected_areas <- get_vector("data/raw/vector/protected_areas", match = EXAMPLE_COUNTRY, ext = ".geojson") %>%
  st_transform(CRS) %>%
  mutate(type = "PA designated post-1991") %>%
  filter(STATUS_YR >= 1991 & MARINE == 0) %>%
  st_intersection(country_poly)
redd_projects <- st_read("data/processed/vector/redd_polys_renoster.gpkg") %>%
  filter(Country == EXAMPLE_COUNTRY) %>%
  st_transform(CRS) %>%
  mutate(type = "Existing REDD+ Project") %>%
  rename(geometry = geom)

protected_polys <- bind_rows(redd_projects, protected_areas)

# Load polygons

country_donors <- paste0("data/processed/rds/", EXAMPLE_COUNTRY, "_60000_2016_data.rds") %>%
  read_rds() %>%
  select(ID, fc_start, stratum, x) %>%
  mutate(type = "Donor polygons")

# Load results

country_data <- read_rds(paste0("data/processed/rds/", EXAMPLE_COUNTRY, "_60000_2016_data.rds"))

country_results <- sc_results_df %>%
  filter(country == EXAMPLE_COUNTRY & sim == 4 & match == 8 & poly_size == 60000)

country_synth <- paste0("data/processed/rds/", EXAMPLE_COUNTRY, "_60000_2016_sc_results.rds") %>%
  read_rds() %>%
  bind_rows() %>%
  filter(match == 8 & sim == 4) %>%
  distinct(sim, match, ID, stratum, .keep_all = TRUE)  # Remove duplicate synths (issue with previous lookup table)

country_synth_weights <- country_synth %>%
  rename(treated_id = ID) %>%
  mutate(synth_weights = map(synth, extract_synth_weights)) %>%
  select(-synth) %>%
  unnest(synth_weights) %>%
  rename(donor_id = ID)

# # Testing
# country_synth_candidates <- country_synth_weights %>%
#   group_by(treated_id) %>%
#   summarise(candidate = (sum(weight > 0.05) > 6) & (sum(weight < -0.1) < 0.2 * n()),
#             stratum = first(stratum)) %>%
#   filter(candidate == TRUE)
# 
# ggplot(country_synth_weights %>% filter(treated_id %in% country_synth_candidates$treated_id)) +
#   geom_col(aes(x = weight, y = as.factor(donor_id))) +
#   facet_wrap(~treated_id) +
#   theme_classic()

fig_id <- 961 #904

# Panel A: Potential donor polygons

panel_a <- tm_shape(country_fc_start, bbox = st_bbox(country_poly)) +
  tm_raster(col.scale = tm_scale_continuous(limits = c(0, 2), values = "greens"),
            col.alpha = 0.3,
            col.legend = tm_legend(show = FALSE)) +
  tm_add_legend(
    type = "polygons",
    labels = "TMF extent in 1991",
    fill = "#74C476",
    title = "Tropical Moist Forest"
  ) +
  tm_shape(country_donors) +
  tm_polygons(
    col = "grey20",
    lwd = 1,
    fill = "stratum",
    fill.scale = tm_scale_discrete(values = "yl_or_rd", value.na = "transparent", label.na = "Donor units"),
    fill.legend = tm_legend(title = "Sample units\nby forest loss bin", frame = FALSE)) +
  tm_shape(protected_polys) +
  tm_fill(
    fill = "type",
    fill.scale = tm_scale_categorical(values = c("#bebada", "#a0d1f3")),
    fill.alpha = 0.3,
    fill.legend = tm_legend(title = "Excluded areas")) +
  tm_shape(country_poly) +
  tm_borders(lwd = 2) +
  tm_layout(frame = FALSE,
            asp = 1)

# Panel B: Donor weights

WEIGHT_THRESHOLD <- 0.01

poly_weights <- country_donors %>%
  select(ID, x) %>%
  inner_join(
    country_synth_weights %>% filter(treated_id == fig_id),
    by = c("ID" = "donor_id")
  ) %>%
  rename(donor_id = ID) %>%
  select(treated_id, donor_id, weight, x)

poly_weights_threshold <- filter(poly_weights, weight >= WEIGHT_THRESHOLD)

treated_unit <- country_donors %>%
  filter(ID == fig_id) %>%
  mutate(type = "Treated unit")

panel_b <- tm_shape(country_poly) +
  tm_borders(lwd = 2) +
  tm_shape(poly_weights) +
  tm_borders(col = "grey80", lwd = 1) +
  tm_shape(poly_weights_threshold) +
  tm_polygons(
    col = "black",
    lwd = 1.5,
    fill = "weight",
    fill.scale = tm_scale_continuous(
      values = "yl_gn_bu",
      limits = c(0, 0.3),
      outliers.trunc = c(TRUE, TRUE),
      ticks = seq(0, 0.3, 0.1),
      labels = seq(0, 0.3, 0.1)
    ),
    fill.legend = tm_legend(reverse = TRUE, frame = FALSE, title = paste0("Donor weights\n(>", WEIGHT_THRESHOLD, " shown)"))
  ) +
  tm_shape(treated_unit) +
  tm_polygons(
    col = "black",
    lwd = 2,
    fill = "type",
    fill.scale = tm_scale_categorical(values = "green", labels = "Treated unit"),
    fill.legend = tm_legend(title = "")
  ) +
  tm_layout(frame = FALSE,
            asp = 1)

# Panel C: Time series

weighted_poly_ts <- country_data %>%
  filter(ID %in% poly_weights_threshold$donor_id) %>%
  wide_to_long() %>%
  select(ID, year, loss) %>%
  left_join(st_drop_geometry(poly_weights_threshold),
            by = c("ID" = "donor_id")) %>%
  filter(year >= 2009) %>%
  mutate(loss = 100 * loss)  # Convert from fraction to percentage

example_donor_ts <- weighted_poly_ts %>%
  arrange(desc(weight)) %>%
  filter(ID == first(ID)) %>%
  mutate(var = "Donor unit")

treated_sc_ts <- country_results %>%
  filter(ID == fig_id) %>%
  rename(`Treated unit` = loss, `Synthetic control` = sc_loss) %>%
  pivot_longer(c("Treated unit", "Synthetic control"), names_to = "var", values_to = "loss") %>%
  bind_rows(example_donor_ts)

panel_c <- ggplot(weighted_poly_ts, aes(x = year, y = loss)) +
  
  # Vertical line at start year
  geom_vline(xintercept = 2016, lwd = 0.5, colour = "grey20") +
  geom_label(label = "Project Start", x = 2016, y = 8.5, size = 6) +
  
  # Donor polygons coloured by weight
  geom_line(aes(group = as.factor(ID), colour = weight), lwd = 0.75, linetype = "dashed", alpha = 0.5) +
  scale_colour_distiller(type = "seq", palette = "YlGnBu", direction = 1, values = c(-0.2, 1), limits = c(0, 0.3), breaks = seq(0, 0.3, 0.1)) +
  labs(colour = "Donor weight") +
  
  # Treated unit and synthetic control prediction coloured black and green
  ggnewscale::new_scale_colour() +
  geom_line(data = treated_sc_ts, aes(colour = var, linewidth = var, linetype = var)) +
  scale_colour_manual(values = c("#0c2c84", "green3", "black")) +
  scale_linewidth_manual(values = c(0.75, 1.5, 1.5)) +
  scale_linetype_manual(values = c(2, 1, 1)) +
  labs(x = "Year", y = "Annual forest loss\n(% of project area)", colour = "Unit", linewidth = "Unit", linetype = "Unit") +
  theme_classic() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.9, 0.7),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 24),
        legend.title = element_text(size = 24, margin = margin(b = 15)),
        legend.text = element_text(size = 20))

tmap_save(panel_a, "results/figures/panel_fig/panel_a.png",
          width = 20, height = 16, units = "cm", dpi = 400)

tmap_save(panel_b, "results/figures/panel_fig/panel_b.png",
          width = 20, height = 16, units = "cm", dpi = 400)

ggsave(plot = panel_c, filename = "results/figures/panel_fig/panel_c.png",
       width = 48, height = 20, units = "cm", dpi = 400)
