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

sc_df <- map(COUNTRIES, function(country) {
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

set.seed(VIZ_SEED)
sc_sample <- sc_df %>%
  filter(match == 8 & poly_size == 60000) %>%
  group_by(country, ID) %>%
  summarise(ID_all = cur_group_id(),
            mean_loss = mean(loss[year >= START_YEAR]),
            stratum = first(stratum)) %>%
  group_by(country, stratum) %>%
  arrange(desc(mean_loss), .by_group = TRUE) %>%
  slice_sample(n = 1) %>%
  # mutate(country_id = LETTERS[1:n()]) %>%
  left_join(sc_df) %>%
  group_by(country) %>%
  mutate(max_loss = max(c(loss, sc_loss))) %>%
  ungroup()

ts_plots <- sc_sample %>%
  filter(match == 8 & poly_size == 60000 & year > (START_YEAR - match)) %>%
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
  theme(strip.background = element_rect(fill = "white", colour = "white"),
        strip.text = element_text(face = "bold"))

ts_plots

ggsave(
  paste0("results/figures/sc_plots/", START_YEAR, "_60000_SIM_ts_plots.jpg"),
  ts_plots,
  width = 24, height = 20, dpi = 300, units = "cm"
)

## 4. RQ1 - Boxplots of stratum-level performance by simulation --------

# Filter to RQ1 simulations and calculate simulation-level MAE and bias
stratum_error_test <- sc_df %>%
  filter(match == 8 & poly_size == 60000 & year > START_YEAR) %>%
  group_by(country, sim, ID, stratum) %>%
  summarise(mae = mean(abs(sc_loss - loss)),
            bias = mean(sc_loss - loss),
            mean_loss = mean(loss)) %>%
  group_by(country, sim, stratum) %>%
  mutate(mean_loss = mean(mean_loss))

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

boxplot_theme <- theme(strip.background = element_rect(fill = "white", colour = "white"),
                       strip.text = element_text(face = "bold", size = 10),
                       legend.position = "inside",
                       legend.position.inside = c(0.9, 0.25),
                       legend.key.size = unit(1, "cm"),
                       legend.text = element_text(size = 10),
                       legend.title = element_text(size = 12),
                       axis.title = element_text(size = 12),
                       axis.text = element_text(size = 10),
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
      boxplot_theme}

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
      boxplot_theme}

ggsave("results/figures/boxplots/strata_error_boxplots_by_sim.png",
       strata_error_boxplot_sims,
       width = 32, height = 16, units = "cm", dpi = 300)

ggsave("results/figures/boxplots/strata_bias_boxplots_by_sim.png",
       strata_bias_boxplot_sims,
       width = 32, height = 16, units = "cm", dpi = 300)

## 5. RQ1 - Continuous plots of MAE and bias vs. observed forest loss --------

# Create summary df for easier plotting
sc_df_summary <- sc_df %>%
  mutate(period = ifelse(year > START_YEAR, "test", "matching")) %>%
  group_by(country, sim, poly_size, match, period, ID) %>%
  summarise(
    mean_loss = mean(loss),
    mean_sc_loss = mean(sc_loss),
    mae = mean(abs(sc_loss - loss)),
    bias = mean(sc_loss - loss)
  ) %>%
  pivot_wider(names_from = "period", values_from = c("mean_loss", "mean_sc_loss", "mae", "bias"))

# Test period bias against test period observed forest loss
continuous_viz_bias_loss_country <- sc_df_summary %>%
  filter(sim == 4 & match == 8 & poly_size == 60000) %>%
  ggplot(aes(x = mean_loss_test, y = bias_test)) +
  geom_hline(yintercept = 0, colour = "#F8766D") +
  geom_point(size = 0.5, show.legend = FALSE, alpha = 0.5) +
  facet_wrap(~country, scales = "fixed") +
  theme_bw() +
  labs(x = "Annual forest loss in post-intervention period\n(% of project area)",
       y = "Bias in post-intervention period\n(% of project area)")

continuous_viz_bias_loss_global <- sc_df_summary %>%
  filter(sim == 4 & match == 8 & poly_size == 60000) %>%
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

continuous_viz_bias_loss_all <- continuous_viz_bias_loss_country +
  continuous_viz_bias_loss_global +
  plot_layout(axis_titles = "collect") &
  theme(
    strip.background = element_rect(fill = "white", colour = "white"),
    strip.text = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

continuous_viz_bias_loss_all

# MAE vs. test period forest loss
continuous_viz_mae_loss_country <- sc_df_summary %>%
  filter(sim == 4 & match == 8 & poly_size == 60000) %>%
  ggplot(aes(x = mean_loss_test, y = mae_test)) +
  # geom_abline(intercept = 0, slope = 1, colour = "#F8766D") +
  geom_point(size = 0.5, alpha = 0.5, show.legend = FALSE) +
  facet_wrap(~country, scales = "fixed") +
  xlim(0, 7) + ylim(0, 7) +
  # scale_colour_viridis_c(option = "A") +
  theme_bw() +
  labs(x = "Annual forest loss in post-intervention period\n(% of project area)",
       y = "MAE in post-intervention period\n(% of project area)")

continuous_viz_mae_loss_global <- sc_df_summary %>%
  filter(sim == 4 & match == 8 & poly_size == 60000) %>%
  mutate(panel_label = "All countries") %>%
  ggplot(aes(x = mean_loss_test, y = mae_test)) +
  # geom_abline(intercept = 0, slope = 1, colour = "#F8766D") +
  geom_point(size = 1.2, alpha = 0.5) +
  facet_wrap(~panel_label) +
  xlim(0, 7) + ylim(0, 7) +
  theme_bw() +
  labs(x = "Annual forest loss in post-intervention period\n(% of project area)",
       y = "MAE in post-intervention period\n(% of project area)")

continuous_viz_mae_loss_all <- continuous_viz_mae_loss_country + 
  continuous_viz_mae_loss_global +
  plot_layout(axis_titles = "collect") &
  theme(
    strip.background = element_rect(fill = "white", colour = "white"),
    strip.text = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

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

# Test period bias against matching period forest loss

# continuous_viz_bias_matching_loss_country <- sc_df_summary %>%
#   filter(sim == 5 & match == 8 & poly_size == 60000) %>%
#   ggplot(aes(x = mean_loss_matching, y = bias_test)) +
#   geom_hline(yintercept = 0, colour = "#F8766D") +
#   geom_point(size = 0.5, show.legend = FALSE) +
#   facet_wrap(~country, scales = "free") +
#   theme_bw() +
#   labs(x = "Annual forest loss in pre-intervention\nperiod (% of polygon area)",
#        y = "Bias in post-intervention period\n(% of polygon area)") +
#   theme(strip.background = element_rect(fill = "white", colour = "white"),
#         strip.text = element_text(face = "bold", size = 14))

continuous_viz_bias_matching_loss_global <- sc_df_summary %>%
  filter(sim == 4 & match == 8 & poly_size == 60000) %>%
  ggplot(aes(x = mean_loss_matching, y = bias_test)) +
  geom_hline(yintercept = 0, colour = "#F8766D") +
  geom_point(size = 1.2, alpha = 0.5) +
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

# continuous_viz_mae_matching_loss_global <- sc_df_summary %>%
#   filter(sim == 5 & match == 8 & poly_size == 60000) %>%
#   ggplot(aes(x = mean_loss_matching, y = mae_test)) +
#   geom_point(size = 0.5) +
#   # geom_hline(yintercept = 0, colour = "#F8766D") +
#   theme_bw() +
#   labs(x = "Annual forest loss in matching\nperiod (% of polygon area)",
#        y = "Mean absolute error in test period\n(% of polygon area)")

# Test period bias against matching period bias

continuous_viz_bias_matching_bias_country <- sc_df_summary %>%
  filter(sim == 4 & match == 8 & poly_size == 60000) %>%
  ggplot(aes(x = bias_matching, y = bias_test)) +
  geom_hline(yintercept = 0, colour = "#F8766D") +
  geom_vline(xintercept = 0, colour = "#F8766D") +
  geom_point(size = 0.5, show.legend = FALSE) +
  facet_wrap(~country, scales = "free") +
  theme_bw() +
  labs(x = "Bias in pre-intervention period\n(% of polygon area)",
       y = "Bias in post-intervention period\n(% of polygon area)") +
  theme(strip.background = element_rect(fill = "white", colour = "white"),
        strip.text = element_text(face = "bold", size = 14))

continuous_viz_bias_matching_bias_global <- sc_df_summary %>%
  filter(sim == 4 & match == 8 & poly_size == 60000) %>%
  ggplot(aes(x = bias_matching, y = bias_test)) +
  geom_hline(yintercept = 0, colour = "#F8766D") +
  geom_vline(xintercept = 0, colour = "#F8766D") +
  geom_point(size = 1.2, alpha = 0.5) +
  scale_x_reverse() +
  theme_bw() +
  labs(x = "Bias in pre-intervention period\n(% of polygon area)",
       y = "Bias in post-intervention period\n(% of polygon area)")

continuous_viz_bias_matching_bias_all <- continuous_viz_bias_matching_bias_country + 
  continuous_viz_bias_matching_bias_global +
  plot_layout(axis_titles = "collect") &
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

continuous_viz_bias_matching_bias_all

continuous_viz_bias_global_predictors <- continuous_viz_bias_matching_loss_global +
  continuous_viz_bias_matching_bias_global +
  plot_layout(axis_titles = "collect_y") &
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )


continuous_viz_bias_global_predictors

ggsave("results/figures/scatter_plots/continous_test_bias_vs_predictors.png",
       continuous_viz_bias_global_predictors,
       width = 30, height = 16, units = "cm", dpi = 300)

## 5b. RQ1 - Continuous predicted vs. actual mean forest loss

pred_vs_actual_country <- sc_df_summary %>%
  filter(sim == 4 & match == 8 & poly_size == 60000) %>%
  ggplot(aes(x = mean_loss_test, y = mean_sc_loss_test)) +
  geom_abline(slope = 1, intercept = 0, colour = "#F8766D") +
  geom_point(size = 0.75, alpha = 0.5, show.legend = FALSE) +
  facet_wrap(~country, scales = "fixed") +
  xlim(0, 6) + ylim(0, 6) +
  theme_bw() +
  labs(x = "Observed mean loss in project period (% yr-1)",
       y = "Predicted mean loss in project period (% yr-1)") +
  theme(strip.background = element_rect(fill = "white", colour = "white"),
        strip.text = element_text(face = "bold", size = 14))

pred_vs_actual_global <- sc_df_summary %>%
  filter(sim == 4 & match == 8 & poly_size == 60000) %>%
  mutate(dummy = "All countries") %>%
  ggplot(aes(x = mean_loss_test, y = mean_sc_loss_test)) +
  geom_abline(slope = 1, intercept = 0, colour = "#F8766D") +
  geom_point(size = 1.2, alpha = 0.5, show.legend = FALSE) +
  facet_wrap(~dummy, scales = "fixed") +
  xlim(0, 6) + ylim(0, 6) +
  theme_bw() +
  labs(x = "Observed mean loss in project period (% yr-1)",
       y = "Predicted mean loss in project period (% yr-1)") +
  theme(strip.background = element_rect(fill = "white", colour = "white"),
        strip.text = element_text(face = "bold", size = 14))

pred_vs_actual_country + pred_vs_actual_global +
  plot_layout(ncol = 2, axes = "collect")
  

## 6. RQ1 - Population and bin 4 mean bias and error by simulation --------

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

# Back-calculate population-level errors for each country and polygon size

stratum_error <- sc_df %>%
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

# Plot mean stratum and population error values

pop_sim_error_viz <- pop_error %>%
  filter(poly_size == 60000 & match == 8) %>%
  ggplot(aes(x = as.factor(sim), y = pop_mae)) +
  geom_point(shape = 18) +
  geom_hline(aes(yintercept = test_loss_all, colour = "Mean forest\nloss rate\n(% of polygon\narea)")) +
  theme_few() +
  facet_wrap(~country) +
  ylim(0, 0.5) +
  labs(x = "Simulation", y = "Mean absolute prediction error\n(% of polygon area)", colour = "",
       title = "Population-level mean prediction error by simulation")

stratum_4_sim_error_viz <- stratum_error %>%
  filter(poly_size == 60000 & stratum == 4 & match == 8) %>%
  ggplot(aes(x = as.factor(sim), y = stratum_mae)) +
  geom_point(shape = 18) +
  geom_hline(aes(yintercept = stratum_loss, colour = "Mean forest\nloss rate\n(% of polygon\narea)")) +
  theme_few() +
  facet_wrap(~country) +
  ylim(0, 8) +
  labs(x = "Simulation", y = "Mean absolute prediction error\n(% of polygon area)", colour = "",
       title = "Upper stratum mean prediction error by simulation")

pop_sim_bias_viz <- pop_error_viz <- pop_error %>%
  filter(poly_size == 60000 & match == 8) %>%
  ggplot(aes(x = as.factor(sim), y = pop_bias)) +
  geom_point(shape = 18) +
  geom_hline(aes(yintercept = 0, colour = "Bias"), show.legend = FALSE) +
  theme_few() +
  facet_wrap(~country) +
  ylim(-0.12, 0.12) +
  labs(x = "Simulation", y = "Mean bias\n(% of polygon area)", colour = "",
       title = "Population-level mean bias error by simulation")

stratum_4_sim_bias_viz <- stratum_error %>%
  filter(poly_size == 60000 & stratum == 4 & match == 8) %>%
  ggplot(aes(x = as.factor(sim), y = stratum_bias)) +
  geom_point(shape = 18) +
  geom_hline(aes(yintercept = 0, colour = "Bias"), show.legend = FALSE) +
  theme_few() +
  facet_wrap(~country) +
  ylim(-6, 0) +
  labs(x = "Simulation", y = "Mean bias\n(% of polygon area)", colour = "",
       title = "Upper stratum mean bias by simulation")

## 7. RQ1 - Variable importance by country ----

importance_ordered <- importance_df %>%
  group_by(variable, country) %>%
  summarise(mean_weight = mean(min.loss.w, na.rm = TRUE)) %>%
  group_by(variable) %>%
  summarise(mean_country_weight = mean(mean_weight, na.rm = TRUE)) %>%
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
                          strip.text = element_text(face = "bold", size = 10),
                          legend.text = element_text(size = 10),
                          legend.title = element_text(size = 12),
                          axis.title = element_text(size = 12),
                          axis.text = element_text(size = 10)
)

# Importance - all strata

importance_viz_country <- importance_df %>%
  mutate(variable = ordered(variable, levels = importance_ordered$variable)) %>%
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

importance_viz_global <- importance_df %>%
  mutate(variable = ordered(variable, levels = importance_ordered$variable)) %>%
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

importance_viz_all <- importance_viz_country + importance_viz_global +
  plot_layout(ncol = 2, axis_titles = "collect")

ggsave(plot = importance_viz_all, filename = "results/figures/importance_plots/importance_all.png",
       width = 30, height = 24, units = "cm", dpi = 300)

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

importance_viz_all_s34 <- importance_viz_country_s34 + importance_viz_global_s34 +
  plot_layout(ncol = 2, axis_titles = "collect")

ggsave(plot = importance_viz_all_s34, filename = "results/figures/importance_plots/importance_all_s34.png",
       width = 30, height = 24, units = "cm", dpi = 300)

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



## 7. RQ2 - Mean absolute error and bias by polygon size --------

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

stratum_error_test_poly_size <- sc_df %>%
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
  facet_wrap(~country) +
  scale_fill_brewer(palette = "Set2") +
  theme_bw() +
  labs(x = "Forest loss bin", y = "Mean absolute prediction error\n(% of project area per year)",
       fill = "Project size") +
  theme(strip.background = element_rect(fill = "white", colour = "white"),
        strip.text = element_text(face = "bold"))

strata_bias_boxplot_poly_size <- stratum_error_test_poly_size %>%
  mutate(poly_size = ordered(paste0(poly_size / 1000, "k"),
                             levels = c("10k", "60k", "600k"))) %>%
  ggplot(aes(x = as.factor(stratum), y = bias, fill = as.factor(poly_size))) +
  geom_hline(yintercept = 0, colour = "#F8766D") +
  geom_boxplot(outlier.size = 0.2, coef = 1000) +
  # geom_point(aes(y = mean_loss), colour = "red", size = 3, shape = 18, show.legend = FALSE) +
  facet_wrap(~country, scales = "fixed") +
  scale_fill_brewer(palette = "Set2") +
  theme_bw() +
  labs(x = "Forest loss bin", y = "Mean bias\n(% of project area per year)",
       fill = "Project size") +
  theme(strip.background = element_rect(fill = "white", colour = "white"),
        strip.text = element_text(face = "bold"))

ggsave("results/figures/boxplots/mae_boxplot_by_poly_size.png",
       strata_error_boxplot_poly_size,
       width = 24, height = 16, units = "cm", dpi = 300)

ggsave("results/figures/boxplots/bias_boxplot_by_poly_size.png",
       strata_bias_boxplot_poly_size,
       width = 24, height = 16, units = "cm", dpi = 300)

# Bias plots

pop_bias_poly_viz <- pop_error %>%
  filter(match == 8 & sim == 5) %>%
  ggplot(aes(x = as.factor(poly_size), y = pop_bias)) +
  geom_point(shape = 18, size = 2) +
  geom_hline(yintercept = 0, colour = "#F8766D") +
  theme_few() +
  facet_wrap(~country) +
  ylim(-0.5, 0.5) +
  labs(x = "polygon size", y = "Mean prediction bias\n(% of polygon area)",
       title = "Population-level mean prediction bias by polygon size")

stratum_4_bias_poly_viz <- stratum_error %>%
  filter(match == 8 & stratum == 4 & sim == 5) %>%
  ggplot(aes(x = as.factor(poly_size), y = stratum_bias)) +
  geom_point(shape = 18, size = 2) +
  geom_hline(yintercept = 0, colour = "#F8766D") +
  theme_few() +
  facet_wrap(~country) +
  ylim(-8, 8) +
  labs(x = "polygon size", y = "Mean prediction bias\n(% of polygon area)", colour = "",
       title = "Upper stratum mean prediction bias by polygon size")

frac_bias_poly_viz <- pop_error %>%
  filter(match == 8 & sim == 5) %>%
  pivot_longer(cols = ends_with("bias_frac")) %>%
  ggplot(aes(x = as.factor(poly_size), y = value, colour = name)) +
  geom_point(shape = 18, size = 2) +
  scale_colour_manual(labels = c("Population", "Highest loss bin"), values = c("blue", "red")) +
  geom_hline(yintercept = 0, colour = "#F8766D") +
  theme_few() +
  facet_wrap(~country) +
  ylim(-1, 1) +
  labs(x = "polygon size", y = "Mean prediction bias\n(fraction of mean loss rate)", colour = "",
       title = "Fractional prediction bias by polygon size")

# Save to disk

ggsave("results/figures/mae_by_country_population_poly.png",
       pop_error_poly_viz,
       width = 24, height = 16, units = "cm", dpi = 300)

ggsave("results/figures/mae_by_country_s4_poly.png",
       stratum_4_error_poly_viz,
       width = 24, height = 16, units = "cm", dpi = 300)

ggsave("results/figures/mae_by_country_frac_poly.png",
       frac_error_poly_viz,
       width = 24, height = 16, units = "cm", dpi = 300)

ggsave("results/figures/bias_by_country_population_poly.png",
       pop_bias_poly_viz,
       width = 24, height = 16, units = "cm", dpi = 300)

ggsave("results/figures/bias_by_country_s4_poly.png",
       stratum_4_bias_poly_viz,
       width = 24, height = 16, units = "cm", dpi = 300)

ggsave("results/figures/bias_by_country_frac_poly.png",
       frac_bias_poly_viz,
       width = 24, height = 16, units = "cm", dpi = 300)

## 8. RQ3 - Mean absolute error and bias by match period --------

# For S5 (also need to do S1)

pop_error_match_viz <- stratum_error %>%
  filter(poly_size == 60000 & sim == 4) %>%
  ggplot(aes(x = match, y = stratum_mae, colour = as.factor(stratum))) +
  geom_point() +
  geom_line(alpha = 0.5) +
  # geom_hline(aes(yintercept = stratum_loss, colour = "Mean forest\nloss rate\n(% of project\narea)")) +
  theme_bw() +
  facet_wrap(~country) +
  # ylim(0, 6) +
  scale_x_continuous(breaks = seq(0, 24, 4)) +
  labs(x = "Match period (years)", y = "Mean absolute prediction error\n(% of polygon area)", colour = "Forest\nloss bin") +
  theme(strip.background = element_rect(fill = "white", colour = "white"),
        strip.text = element_text(face = "bold"),
        legend.position = "right")

pop_bias_match_viz <- stratum_error %>%
  filter(poly_size == 60000 & sim == 4) %>%
  ggplot(aes(x = match, y = stratum_bias, colour = as.factor(stratum))) +
  geom_point() +
  geom_line(alpha = 0.5) +
  geom_hline(yintercept = 0, colour = "#F8766D") +
  theme_bw() +
  facet_wrap(~country, scales = "free") +
  # ylim(-6, 6) +
  scale_x_continuous(breaks = seq(0, 24, 4)) +
  labs(x = "Match period (years)", y = "Mean bias\n(% of polygon area)", colour = "Forest\nloss bin") +
  theme(strip.background = element_rect(fill = "white", colour = "white"),
        strip.text = element_text(face = "bold"),
        legend.position = "right")

# Boxplot
stratum_error_test <- sc_df %>%
  filter(poly_size == 60000 & year > START_YEAR) %>%
  group_by(country, match, ID, sim, stratum) %>%
  summarise(mae = mean(abs(sc_loss - loss)),
            bias = mean(sc_loss - loss),
            mean_loss = mean(loss)) %>%
  group_by(country, sim, stratum) %>%
  mutate(mean_loss = mean(mean_loss))

strata_error_boxplot_match <- stratum_error_test %>%
  filter(sim == 5) %>%
  ggplot(aes(x = as.factor(stratum), y = mae, fill = as.factor(match))) +
  geom_boxplot(coef = 1000, outliers = FALSE) +
  geom_point(aes(y = mean_loss), colour = "red", size = 3, shape = 18, show.legend = FALSE) +
  facet_wrap(~country, scales = "free") +
  scale_fill_brewer(palette = "Blues") +
  theme_bw() +
  labs(x = "Forest loss bin", y = "Mean absolute prediction error\n(% of project area per year)",
       fill = "Match period (years)") +
  theme(strip.background = element_rect(fill = "white", colour = "white"),
        strip.text = element_text(face = "bold"))

strata_bias_boxplot_match <- stratum_error_test %>%
  filter(sim == 5) %>%
  ggplot(aes(x = as.factor(stratum), y = bias, fill = as.factor(match))) +
  geom_boxplot(coef = 1000, outliers = FALSE) +
  geom_hline(yintercept = 0, colour = "#F8766D") +
  facet_wrap(~country, scales = "free") +
  scale_fill_brewer(palette = "Blues") +
  theme_bw() +
  labs(x = "Forest loss bin", y = "Mean bias\n(% of project area per year)",
       fill = "Match period (years)") +
  theme(strip.background = element_rect(fill = "white", colour = "white"),
        strip.text = element_text(face = "bold"))

# Save to disk

ggsave("results/figures/mae_by_country_bins_match_s5.png",
       pop_error_match_viz,
       width = 24, height = 16, units = "cm", dpi = 300)

ggsave("results/figures/bias_by_country_bins_match_s5.png",
       pop_bias_match_viz,
       width = 24, height = 16, units = "cm", dpi = 300)

ggsave("results/figures/boxplots/mae_boxplot_by_match_s5.png",
       strata_error_boxplot_match,
       width = 24, height = 16, units = "cm", dpi = 300)

ggsave("results/figures/boxplots/bias_boxplot_by_match_s5.png",
       strata_bias_boxplot_match,
       width = 24, height = 16, units = "cm", dpi = 300)

## 9. RQ4 - Performance of SC method over a long test period --------

sc_df_rq4 <- map(COUNTRIES, function(country) {
  filepath <- paste0("results/sc_results/sc_results_", country, "_1998_60000.csv")
  
  country_df <- filepath %>%
    read_csv() %>%
    mutate(country = country)
  
  country_df
}) %>% bind_rows()


defor_rq4 <- map(COUNTRIES, function(country) {
  filepath <- paste0("data/processed/rds/", country, "_60000_1998_data.rds")
  
  country_df <- filepath %>%
    read_rds() %>%
    mutate(country = country)
  
  country_df
}) %>% 
  bind_rows() %>%
  select(ID, country, cum_loss, stratum, starts_with("loss"))


####


# Assign forest loss strata for all polygons
defor_long_rq4 <- defor_rq4 %>%
  st_drop_geometry() %>%
  pivot_longer(starts_with("loss")) %>%
  separate_wider_delim(name, delim = ".", names = c("var", "year")) %>%
  mutate(year = as.numeric(year)) %>%
  pivot_wider(names_from = var, values_from = value)

defor_long_loss_rq4 <- defor_long_rq4 %>%
  filter(year > RQ4_START_YEAR) %>%
  group_by(country, ID) %>%
  mutate(mean_loss = mean(loss) * 100) %>%
  group_by(country) %>%
  mutate(stratum = cut_interval(mean_loss, n = 4, labels = FALSE)) %>%
  group_by(country, stratum)

# Calculate relative frequency of strata for each country and polygon size
stratum_freq_rq4 <- defor_long_loss_rq4 %>%
  group_by(country, year) %>%
  mutate(n = n(),
         mean_loss_year = mean(loss) * 100,
         mean_loss_all = mean(mean_loss)) %>%
  group_by(country, stratum, year) %>%
  summarise(n_stratum = n(),
            frac_stratum = n_stratum / mean(n),
            mean_loss_year = mean(mean_loss_year),
            mean_loss_all = mean(mean_loss_all),
            stratum_loss = mean(mean_loss)) 

# Back-calculate population-level errors for each country and polygon size

stratum_error_rq4 <- sc_df_rq4 %>%
  filter(year > RQ4_START_YEAR) %>%
  group_by(country, stratum, ID, year) %>%
  summarise(mae = mean(abs(sc_loss - loss)) * 100,
            mean_bias = mean(sc_loss - loss) * 100) %>%
  group_by(country, stratum, year) %>%
  summarise(stratum_mae = mean(mae),
            stratum_bias = mean(mean_bias)) %>%
  left_join(stratum_freq_rq4)

pop_error_rq4 <- stratum_error_rq4 %>%
  group_by(country, year) %>%
  summarise(pop_mae = sum(frac_stratum * stratum_mae),
            pop_bias = sum(frac_stratum * stratum_bias),
            n = sum(n_stratum),
            mean_loss_year = mean(mean_loss_year),
            mean_loss_all = mean(mean_loss_all),
            pop_mae_frac = pop_mae / mean_loss_all,
            pop_bias_frac = pop_bias / mean_loss_all,
            s4_mae = stratum_mae[stratum == 4],
            s4_bias = stratum_bias[stratum == 4],
            s4_n = n_stratum[stratum == 4],
            s4_loss = stratum_loss[stratum == 4],
            s4_mae_frac = s4_mae / s4_loss,
            s4_bias_frac = s4_bias / s4_loss)


pop_error_rq4_viz <- pop_error_rq4 %>%
  ggplot(aes(x = year, y = pop_mae)) +
  geom_line(aes(colour = "Mean absolute\nprediction error")) +
  geom_line(aes(y = mean_loss_year, colour = "Mean forest\nloss rate")) +
  theme_few() +
  facet_wrap(~country) +
  scale_colour_manual(values = c("black", "#F8766D")) +
  labs(x = "Year", y = "% of polygon area", colour = "",
       title = "Population-level mean prediction error over test period")

s4_error_rq4_viz <- pop_error_rq4 %>%
  ggplot(aes(x = year, y = s4_mae)) +
  geom_line(aes(colour = "Mean absolute\nprediction error")) +
  geom_line(aes(y = mean_loss_year, colour = "Mean forest\nloss rate")) +
  theme_few() +
  facet_wrap(~country, scales = "free") +
  scale_colour_manual(values = c("black", "#F8766D")) +
  labs(x = "Year", y = "% of polygon area", colour = "",
       title = "Upper bin mean prediction error over test period")

annual_frac_error_rq4_viz <- pop_error_rq4 %>%
  mutate(pop_frac_mae = pop_mae / mean_loss_year,
         s4_frac_mae = s4_mae / mean_loss_year) %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = pop_frac_mae, colour = "Population")) +
  geom_line(aes(y = s4_frac_mae, colour = "Upper bin")) +
  geom_hline(yintercept = 1, colour = "grey30") +
  theme_few() +
  facet_wrap(~country, scales = "free") +
  labs(x = "Year",
       y = "Mean absolute prediction error\n(fraction of mean annual deforestation rate)",
       colour = "")

# Version 2 of above

annual_frac_error_rq4_viz <- pop_error_rq4 %>%
  mutate(pop_frac_mae = pop_mae / mean_loss_year,
         s4_frac_mae = s4_mae / mean_loss_year) %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = pop_frac_mae)) +
  # geom_line(aes(y = s4_frac_mae, colour = "Upper bin")) +
  geom_hline(yintercept = 1, colour = "#F8766D") +
  theme_few() +
  facet_wrap(~country) +
  labs(x = "Year",
       y = "Mean absolute prediction error\n(fraction of mean annual deforestation rate)",
       title = "Population fraction")



ggsave("results/figures/mae_by_country_population_rq4.png",
       pop_error_rq4_viz,
       width = 24, height = 16, units = "cm", dpi = 300)

ggsave("results/figures/mae_by_country_s4_rq4.png",
       s4_error_rq4_viz,
       width = 24, height = 16, units = "cm", dpi = 300)

ggsave("results/figures/mae_by_country_frac_annual_rq4.png",
       annual_frac_error_rq4_viz,
       width = 24, height = 16, units = "cm", dpi = 300)

# ## 7. Mean stratum 4 and population error and bias --------
# 
# # Plot mean stratum and population error values
# 
# pop_error_viz <- pop_error %>%
#   filter(poly_size == 60000 & match == 8) %>%
#   ggplot(aes(x = as.factor(sim), y = pop_mae)) +
#   geom_point(shape = 18) +
#   geom_hline(aes(yintercept = test_loss_all, colour = "Mean forest\nloss rate\n(% of polygon\narea)")) +
#   theme_few() +
#   facet_wrap(~country) +
#   ylim(0, 0.5) +
#   labs(x = "Simulation", y = "Mean absolute prediction error\n(% of polygon area)", colour = "",
#        title = "Population-level mean prediction error by simulation")
# 
# stratum_4_error_viz <- stratum_error %>%
#   filter(poly_size == 60000 & stratum == 4 & match == 8) %>%
#   ggplot(aes(x = as.factor(sim), y = stratum_mae)) +
#   geom_point(shape = 18) +
#   geom_hline(aes(yintercept = stratum_loss, colour = "Mean forest\nloss rate\n(% of polygon\narea)")) +
#   theme_few() +
#   facet_wrap(~country) +
#   ylim(0, 8) +
#   labs(x = "Simulation", y = "Mean absolute prediction error\n(% of polygon area)", colour = "",
#        title = "Upper stratum mean prediction error by simulation")
# 
# # frac_error_viz <- pop_error %>%
# #   filter(poly_size == 60000 & match == 8) %>%
# #   pivot_longer(cols = ends_with("mae_frac")) %>%
# #   ggplot(aes(x = as.factor(sim), y = value, colour = name)) +
# #   geom_point(shape = 18, size = 2) + 
# #   scale_colour_manual(labels = c("Population", "Highest loss bin"), values = c("blue", "red")) +
# #   geom_hline(yintercept = 1, colour = "#F8766D") +
# #   theme_few() +
# #   facet_wrap(~country) +
# #   ylim(0, 1.5) +
# #   labs(x = "Simulation", y = "Mean absolute prediction error\n(fraction of mean loss rate)", colour = "",
# #        title = "Fractional prediction error by simulation")
# 
# # Bias plots
# 
# pop_bias_viz <- pop_error %>%
#   filter(poly_size == 60000 & match == 8) %>%
#   ggplot(aes(x = as.factor(sim), y = pop_bias)) +
#   geom_point(shape = 18) +
#   geom_hline(yintercept = 0, colour = "#F8766D") +
#   theme_few() +
#   facet_wrap(~country) +
#   ylim(-0.2, 0.2) +
#   labs(x = "Simulation", y = "Mean prediction bias\n(% of polygon area)",
#        title = "Population-level mean prediction bias by simulation")
# 
# stratum_4_bias_viz <- stratum_error %>%
#   filter(poly_size == 60000 & stratum == 4 & match == 8) %>%
#   ggplot(aes(x = as.factor(sim), y = stratum_bias)) +
#   geom_point(shape = 18) +
#   geom_hline(yintercept = 0, colour = "#F8766D") +
#   theme_few() +
#   facet_wrap(~country) +
#   ylim(-6, 6) +
#   labs(x = "Simulation", y = "Mean prediction bias\n(% of polygon area)", colour = "",
#        title = "Upper stratum mean prediction bias by simulation")

## 8. Sampling scheme map figures

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
  mutate(type = "New Protected Area") %>%
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

country_results <- sc_df %>%
  filter(country == EXAMPLE_COUNTRY & sim == 5 & match == 8 & poly_size == 60000)

country_synth <- paste0("data/processed/rds/", EXAMPLE_COUNTRY, "_60000_2016_sc_results.rds") %>%
  read_rds() %>%
  bind_rows() %>%
  filter(match == 8 & sim == 5)

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

# Choosing ID 778

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

poly_weights <- country_donors %>%
  select(ID, x) %>%
  inner_join(
    country_synth_weights %>% filter(treated_id == fig_id),
    by = c("ID" = "donor_id")
  ) %>%
  rename(donor_id = ID) %>%
  select(treated_id, donor_id, weight, x)

poly_weights_gte_0.01 <- filter(poly_weights, weight >= 0.01)

treated_unit <- country_donors %>%
  filter(ID == fig_id) %>%
  mutate(type = "Treated unit")

panel_b <- tm_shape(country_poly) +
  tm_borders(lwd = 2) +
  tm_shape(poly_weights) +
  tm_borders(col = "grey80", lwd = 1) +
  tm_shape(poly_weights_gte_0.01) +
  tm_polygons(
    col = "black",
    lwd = 1.5,
    fill = "weight",
    fill.scale = tm_scale_continuous(
      values = "rd_pu",
      limits = c(0, 0.3),
      outliers.trunc = c(TRUE, TRUE),
      ticks = seq(0, 0.3, 0.1),
      labels = seq(0, 0.3, 0.1)
    ),
    fill.legend = tm_legend(reverse = TRUE, frame = FALSE, title = "Donor weights\n(>0.01 shown)")
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
  filter(ID %in% poly_weights_gte_0.01$donor_id) %>%
  wide_to_long() %>%
  select(ID, year, loss) %>%
  left_join(st_drop_geometry(poly_weights_gte_0.01),
            by = c("ID" = "donor_id")) %>%
  filter(year >= 2009)

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
  geom_label(label = "Project Start", x = 2016, y = 0.085, size = 6) +
  
  # Donor polygons coloured by weight
  geom_line(aes(group = as.factor(ID), colour = weight), lwd = 0.5, linetype = "dashed", alpha = 0.5) +
  scale_colour_distiller(type = "seq", palette = "Purples", direction = 1, limits = c(0, 0.3), breaks = seq(0, 0.3, 0.1)) +
  labs(colour = "Donor weight") +
  
  # Treated unit and synthetic control prediction coloured black and green
  ggnewscale::new_scale_colour() +
  geom_line(data = treated_sc_ts, aes(colour = var, linewidth = var, linetype = var)) +
  scale_colour_manual(values = c("#4A1486", "green3", "black")) +
  scale_linewidth_manual(values = c(0.5, 1.5, 1.5)) +
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
