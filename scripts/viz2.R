#### Data analysis script
#### Fits augmented synthetic controls for sample polygon data and writes the
#### results to disk as a CSV.

source("scripts/load.R")

### 1. Set parameters --------

# Data selection
COUNTRIES <- c(
  "Bolivia", "Brazil",
  "Colombia",
  "Cote d'Ivoire",
  "Democratic Republic of the Congo", "Madagascar",
  "Indonesia",
  "Malaysia", "Myanmar"
)
START_YEAR <- 2016  # Simulated start year of protection project
RQ4_START_YEAR <- 1998  # Simulated start year for RQ4 simulations
POLY_SIZE <- c(10000, 60000, 600000) # size of polygons in hectares
VIZ_SEED <- 111
CUMULATIVE <- TRUE

### 2. Load data --------

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
}) %>% bind_rows()

# Deforestation rates by country
defor <- map(COUNTRIES, function(country) {
  country_defor <- map(POLY_SIZE, function(size) {
    filename <- paste0(country, "_", size, "_", START_YEAR, "_data.rds")
    df <- read_rds(paste0("data/processed/rds/", filename)) %>% mutate(poly_size = size)
  }) %>% bind_rows()
  
  country_defor <- mutate(country_defor, country = country)
  
  country_defor
}) %>% bind_rows() %>%
  select(ID, country, poly_size, cum_loss, stratum, starts_with("loss"))

### 3. Create population-level and stratum-level results --------

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

pop_error_viz <- pop_error %>%
  filter(poly_size == 60000 & match == 8) %>%
  ggplot(aes(x = as.factor(sim), y = pop_mae)) +
  geom_point(shape = 18) +
  geom_hline(aes(yintercept = test_loss_all, colour = "Mean forest\nloss rate\n(% of polygon\narea)")) +
  theme_few() +
  facet_wrap(~country) +
  ylim(0, 0.5) +
  labs(x = "Simulation", y = "Mean absolute prediction error\n(% of polygon area)", colour = "",
       title = "Population-level mean prediction error by simulation")

stratum_4_error_viz <- stratum_error %>%
  filter(poly_size == 60000 & stratum == 4 & match == 8) %>%
  ggplot(aes(x = as.factor(sim), y = stratum_mae)) +
  geom_point(shape = 18) +
  geom_hline(aes(yintercept = stratum_loss, colour = "Mean forest\nloss rate\n(% of polygon\narea)")) +
  theme_few() +
  facet_wrap(~country) +
  ylim(0, 8) +
  labs(x = "Simulation", y = "Mean absolute prediction error\n(% of polygon area)", colour = "",
       title = "Upper stratum mean prediction error by simulation")

# frac_error_viz <- pop_error %>%
#   filter(poly_size == 60000 & match == 8) %>%
#   pivot_longer(cols = ends_with("mae_frac")) %>%
#   ggplot(aes(x = as.factor(sim), y = value, colour = name)) +
#   geom_point(shape = 18, size = 2) + 
#   scale_colour_manual(labels = c("Population", "Highest loss bin"), values = c("blue", "red")) +
#   geom_hline(yintercept = 1, colour = "#F8766D") +
#   theme_few() +
#   facet_wrap(~country) +
#   ylim(0, 1.5) +
#   labs(x = "Simulation", y = "Mean absolute prediction error\n(fraction of mean loss rate)", colour = "",
#        title = "Fractional prediction error by simulation")

# Bias plots

pop_bias_viz <- pop_error %>%
  filter(poly_size == 60000 & match == 8) %>%
  ggplot(aes(x = as.factor(sim), y = pop_bias)) +
  geom_point(shape = 18) +
  geom_hline(yintercept = 0, colour = "#F8766D") +
  theme_few() +
  facet_wrap(~country) +
  ylim(-0.2, 0.2) +
  labs(x = "Simulation", y = "Mean prediction bias\n(% of polygon area)",
       title = "Population-level mean prediction bias by simulation")

stratum_4_bias_viz <- stratum_error %>%
  filter(poly_size == 60000 & stratum == 4 & match == 8) %>%
  ggplot(aes(x = as.factor(sim), y = stratum_bias)) +
  geom_point(shape = 18) +
  geom_hline(yintercept = 0, colour = "#F8766D") +
  theme_few() +
  facet_wrap(~country) +
  ylim(-6, 6) +
  labs(x = "Simulation", y = "Mean prediction bias\n(% of polygon area)", colour = "",
       title = "Upper stratum mean prediction bias by simulation")

# frac_bias_viz <- pop_error %>%
#   filter(poly_size == 60000 & match == 8) %>%
#   pivot_longer(cols = ends_with("bias_frac")) %>%
#   ggplot(aes(x = as.factor(sim), y = value, colour = name)) +
#   geom_point(shape = 18, size = 2) + 
#   scale_colour_manual(labels = c("Population", "Highest loss bin"), values = c("blue", "red")) +
#   geom_hline(yintercept = 0, colour = "#F8766D") +
#   theme_few() +
#   facet_wrap(~country) +
#   ylim(-1, 1) +
#   labs(x = "Simulation", y = "Mean prediction bias\n(fraction of mean loss rate)", colour = "",
#        title = "Fractional prediction bias by simulation")


# Boxplots of stratum-level performance range

stratum_error_test <- sc_df %>%
  filter(match == 8 & poly_size == 60000 & year > START_YEAR & sim <= 5) %>%
  group_by(country, sim, ID, stratum) %>%
  summarise(mae = mean(abs(sc_loss - loss)),
            bias = mean(sc_loss - loss),
            mean_loss = mean(loss)) %>%
  group_by(country, sim, stratum) %>%
  mutate(mean_loss = mean(mean_loss))

strata_error_boxplot_sims <- ggplot(stratum_error_test,
                                    aes(x = as.factor(stratum), y = mae, fill = as.factor(sim))) +
  geom_boxplot(coef = 1000, outliers = FALSE) +
  geom_point(aes(y = mean_loss), colour = "red", size = 3, shape = 18, show.legend = FALSE) +
  facet_wrap(~country) +
  theme_bw() +
  labs(x = "Forest loss bin", y = "Mean absolute prediction error\n(% of project area per year)",
       fill = "Simulation") +
  theme(strip.background = element_rect(fill = "white", colour = "white"),
        strip.text = element_text(face = "bold"))

strata_bias_boxplot_sims <- ggplot(stratum_error_test,
                                   aes(x = as.factor(stratum), y = bias, fill = as.factor(sim))) +
  geom_boxplot(outliers = FALSE, coef = 1000) +
  geom_hline(yintercept = 0, colour = "#F8766D") +
  scale_fill_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00'),
                    labels = c("S1", "S2", "S3", "S4", "S5")) +
  facet_wrap(~country, scales = "free") +
  theme_bw() +
  labs(x = "Forest loss bin", y = "Mean bias\n(% of project area per year)",
       fill = "Matching\ncovariates") +
  theme(strip.background = element_rect(fill = "white", colour = "white"),
        strip.text = element_text(face = "bold"))

ggsave("results/figures/boxplots/strata_error_boxplots_by_sim.png",
       strata_error_boxplot_sims,
       width = 24, height = 16, units = "cm", dpi = 300)

ggsave("results/figures/boxplots/strata_bias_boxplots_by_sim.png",
       strata_bias_boxplot_sims,
       width = 24, height = 16, units = "cm", dpi = 300)

## Continuous plots of MAE and bias vs. observed forest loss ----

# Create summary df for easier plotting
sc_df_summary <- sc_df %>%
  mutate(period = ifelse(year > START_YEAR, "test", "matching")) %>%
  group_by(country, sim, poly_size, match, period, ID) %>%
  summarise(
    mean_loss = mean(loss) * 100,
    mae = mean(abs(sc_loss - loss)) * 100,
    bias = mean(sc_loss - loss) * 100
  ) %>%
  pivot_wider(names_from = "period", values_from = c("mean_loss", "mae", "bias"))

# Test period bias against test period observed forest loss
continuous_viz_bias_loss_country <- sc_df_summary %>%
  filter(sim == 5 & match == 8 & poly_size == 60000) %>%
  ggplot(aes(x = mean_loss_test, y = bias_test)) +
  geom_hline(yintercept = 0, colour = "#F8766D") +
  geom_point(size = 0.5, show.legend = FALSE, alpha = 0.5) +
  facet_wrap(~country, scales = "fixed") +
  # scale_colour_brewer(palette = "Dark2") +
  theme_bw() +
  labs(x = "Annual forest loss in test period\n(% of project area)",
       y = "Bias in test period\n(% of project area)")

continuous_viz_bias_loss_global <- sc_df_summary %>%
  filter(sim == 5 & match == 8 & poly_size == 60000) %>%
  ggplot(aes(x = mean_loss_test, y = bias_test)) +
  geom_hline(yintercept = 0, colour = "#F8766D") +
  geom_point(size = 1.2, alpha = 0.5) +
  # geom_smooth(method = "loess", colour = "steelblue") +
  # scale_colour_brewer(palette = "Dark2") +
  # coord_cartesian(ylim = c(-2, 2)) +
  theme_bw() +
  labs(x = "Annual forest loss in test period\n(% of project area)",
       y = "Bias in test period\n(% of project area)")

continuous_viz_bias_loss_all <- continuous_viz_bias_loss_country +
  continuous_viz_bias_loss_global +
  plot_layout(axis_titles = "collect")
  
continuous_viz_bias_loss_all

# MAE vs. test period forest loss
continuous_viz_mae_loss_country <- sc_df_summary %>%
  filter(sim == 5 & match == 8 & poly_size == 60000) %>%
  ggplot(aes(x = mean_loss_test, y = mae_test)) +
  geom_abline(intercept = 0, slope = 1, colour = "#F8766D") +
  geom_point(size = 0.5, alpha = 0.5, show.legend = FALSE) +
  facet_wrap(~country, scales = "fixed") +
  xlim(0, 7) + ylim(0, 7) +
  # scale_colour_viridis_c(option = "A") +
  theme_bw() +
  labs(x = "Annual forest loss in test period\n(% of project area)",
       y = "MAE in test period\n(% of project area)")

continuous_viz_mae_loss_global <- sc_df_summary %>%
  filter(sim == 5 & match == 8 & poly_size == 60000) %>%
  ggplot(aes(x = mean_loss_test, y = mae_test)) +
  # geom_abline(intercept = 0, slope = 1, colour = "#F8766D") +
  geom_point(size = 1.2, alpha = 0.5) +
  xlim(0, 7) + ylim(0, 7) +
  theme_bw() +
  labs(x = "Annual forest loss in test period\n(% of project area)",
       y = "MAE in test period\n(% of project area)")

continuous_viz_mae_loss_all <- continuous_viz_mae_loss_country + 
  continuous_viz_mae_loss_global +
  plot_layout(axis_titles = "collect")

continuous_viz_mae_loss_all

ggsave("results/figures/scatter_plots/continuous_viz_bias_loss_all.png",
       continuous_viz_bias_loss_all,
       width = 30, height = 16, units = "cm", dpi = 300)

ggsave("results/figures/scatter_plots/continuous_viz_mae_loss_all.png",
       continuous_viz_mae_loss_all,
       width = 30, height = 16, units = "cm", dpi = 300)

continuous_viz_global_mae_bias <- continuous_viz_mae_loss_global +
  continuous_viz_bias_loss_global +
  plot_layout(axis_titles = "keep")

ggsave("results/figures/scatter_plots/continuous_viz_global_mae_bias.png",
       continuous_viz_global_mae_bias,
       width = 30, height = 16, units = "cm", dpi = 300)

# Test period bias against matching period forest loss

continuous_viz_bias_matching_loss_country <- sc_df_summary %>%
  filter(sim == 5 & match == 8 & poly_size == 60000) %>%
  ggplot(aes(x = mean_loss_matching, y = bias_test)) +
  geom_hline(yintercept = 0, colour = "#F8766D") +
  geom_point(size = 0.5, show.legend = FALSE) +
  facet_wrap(~country, scales = "free") +
  theme_bw() +
  labs(x = "Annual forest loss in matching\nperiod (% of polygon area)",
       y = "Bias in test period\n(% of polygon area)")

continuous_viz_bias_matching_loss_global <- sc_df_summary %>%
  filter(sim == 5 & match == 8 & poly_size == 60000) %>%
  ggplot(aes(x = mean_loss_matching, y = bias_test)) +
  geom_hline(yintercept = 0, colour = "#F8766D") +
  geom_point(size = 1.2, alpha = 0.5) +
  theme_bw() +
  labs(x = "Annual forest loss in matching\nperiod (% of polygon area)",
       y = "Bias in test period\n(% of polygon area)")

continuous_viz_bias_matching_loss_all <- continuous_viz_bias_matching_loss_country + 
  continuous_viz_bias_matching_loss_global +
  plot_layout(axis_titles = "collect")

continuous_viz_bias_matching_loss_all

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
  filter(sim == 5 & match == 8 & poly_size == 60000) %>%
  ggplot(aes(x = bias_matching, y = bias_test)) +
  geom_hline(yintercept = 0, colour = "#F8766D") +
  geom_point(size = 0.5, show.legend = FALSE) +
  facet_wrap(~country, scales = "free") +
  theme_bw() +
  labs(x = "Bias in matching period\n(% of polygon area)",
       y = "Bias in test period\n(% of polygon area)")

continuous_viz_bias_matching_bias_global <- sc_df_summary %>%
  filter(sim == 5 & match == 8 & poly_size == 60000) %>%
  ggplot(aes(x = bias_matching, y = bias_test)) +
  geom_hline(yintercept = 0, colour = "#F8766D") +
  geom_vline(xintercept = 0, colour = "#F8766D") +
  geom_point(size = 1.2, alpha = 0.5) +
  scale_x_reverse() +
  theme_bw() +
  labs(x = "Bias in matching period\n(% of project area)",
       y = "Bias in test period\n(% of project area)")

continuous_viz_bias_matching_bias_all <- continuous_viz_bias_matching_bias_country + 
  continuous_viz_bias_matching_bias_global +
  plot_layout(axis_titles = "collect")

continuous_viz_bias_matching_bias_all

continuous_viz_bias_global_predictors <- continuous_viz_bias_matching_loss_global +
  continuous_viz_bias_matching_bias_global +
  plot_layout(axis_titles = "collect_y")

continuous_viz_bias_global_predictors

ggsave("results/figures/scatter_plots/continous_test_bias_vs_predictors.png",
       continuous_viz_bias_global_predictors,
       width = 30, height = 16, units = "cm", dpi = 300)

## Plot population and stratum summary figs

ggsave("results/figures/mae_by_country_population.png",
       pop_error_viz,
       width = 24, height = 16, units = "cm", dpi = 300)

ggsave("results/figures/mae_by_country_s4.png",
       stratum_4_error_viz,
       width = 24, height = 16, units = "cm", dpi = 300)

ggsave("results/figures/mae_by_country_frac.png",
       frac_error_viz,
       width = 24, height = 16, units = "cm", dpi = 300)

ggsave("results/figures/bias_by_country_population.png",
       pop_bias_viz,
       width = 24, height = 16, units = "cm", dpi = 300)

ggsave("results/figures/bias_by_country_s4.png",
       stratum_4_bias_viz,
       width = 24, height = 16, units = "cm", dpi = 300)

ggsave("results/figures/bias_by_country_frac.png",
       frac_bias_viz,
       width = 24, height = 16, units = "cm", dpi = 300)

ggsave("results/figures/mae_by_mean_loss_rate_global.png",
       continuous_viz,
       width = 24, height = 16, units = "cm", dpi = 300)

ggsave("results/figures/frac_mae_by_mean_loss_rate_global.png",
       continuous_viz_frac,
       width = 24, height = 16, units = "cm", dpi = 300)

### 4. Visualise SC trajectories --------

set.seed(VIZ_SEED)
sc_sample <- sc_df %>%
  filter(match == 8 & poly_size == 60000 & sim %in% c(1, 6)) %>%
  group_by(country, ID) %>%
  summarise(ID_all = cur_group_id(),
            mean_loss = mean(loss)) %>%
  group_by(country) %>%
  arrange(desc(mean_loss), .by_group = TRUE) %>%
  slice_sample(n = 6) %>%
  mutate(country_id = 1:n()) %>%
  left_join(sc_df) %>%
  mutate(country = ifelse(country == "Democratic Republic of the Congo", "DRC", country))

ts_plots <- sc_sample %>%
  group_by(country) %>%
  filter(match == 8 & poly_size == 60000 & year > (START_YEAR - match) & sim <= 5) %>%
  ggplot(aes(x = as.numeric(year))) +
  geom_line(aes(y = loss, colour = "Observed"), lwd = 1) +
  geom_line(aes(y = sc_loss, colour = as.factor(sim))) +
  geom_vline(xintercept = START_YEAR) +
  facet_grid(cols = vars(country), rows = vars(country_id)) +
  theme_bw() +
  labs(x = "Year", y = "Annual forest loss\n(% of project area)", colour = "Simulation") +
  scale_colour_manual(
    labels = c(paste0("S", 1:5), "Observed"),
    values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00', "black")) +
  theme(strip.background = element_rect(fill = "white", colour = "white"),
        strip.text = element_text(face = "bold"))

ts_plots

ts_plots_s5 <- sc_sample %>%
  group_by(country) %>%
  filter(match == 8 & poly_size == 60000 & year > (START_YEAR - match) & sim == 5) %>%
  ggplot(aes(x = as.numeric(year))) +
  geom_line(aes(y = loss, colour = "Observed"), lwd = 1.5) +
  geom_line(aes(y = sc_loss, colour = as.factor(sim)), lwd = 0.9) +
  geom_vline(xintercept = START_YEAR, colour = "steelblue") +
  facet_grid(cols = vars(country), rows = vars(country_id)) +
  theme_bw() +
  labs(x = "Year", y = "Annual forest loss\n(% of project area)", colour = "") +
  scale_colour_manual(
    labels = c("Synthetic control (S5)", "Observed"),
    values = c('#ff7f00', "black")) +
  scale_x_continuous(breaks = c(2012, 2020)) +
  theme(strip.background = element_rect(fill = "white", colour = "white"),
        strip.text = element_text(face = "bold", size = 12),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 12),
        legend.position = "bottom")

ggsave(
  paste0("results/figures/sc_plots/", START_YEAR, "_60000_SIM_ts_plots.jpg"),
  ts_plots,
  width = 34, height = 20, dpi = 300, units = "cm"
)

ggsave(
  paste0("results/figures/sc_plots/", START_YEAR, "_60000_S5_ts_plots_.jpg"),
  ts_plots_s5,
  width = 32, height = 18, dpi = 300, units = "cm"
)

### 5. RQ3 - Mean absolute error by match period ----

pop_error_match_viz <- pop_error %>%
  filter(poly_size == 60000 & sim == 1) %>%
  ggplot(aes(x = match, y = pop_mae)) +
  geom_line() +
  geom_hline(aes(yintercept = mean_loss_all, colour = "Mean forest\nloss rate\n(% of polygon\narea)")) +
  theme_few() +
  facet_wrap(~country) +
  ylim(0, 0.5) +
  scale_x_continuous(breaks = seq(0, 24, 4)) +
  labs(x = "Matching period", y = "Mean absolute prediction error\n(% of project area)", colour = "",
       title = "Population-level mean prediction error by matching period")

stratum_4_error_match_viz <- stratum_error %>%
  filter(poly_size == 60000 & stratum == 4 & sim == 1) %>%
  ggplot(aes(x = match, y = stratum_mae)) +
  geom_line() +
  geom_hline(aes(yintercept = stratum_loss, colour = "Mean forest\nloss rate\n(% of project\narea)")) +
  theme_few() +
  facet_wrap(~country) +
  ylim(0, 8) +
  scale_x_continuous(breaks = seq(0, 24, 4)) +
  labs(x = "Matching period", y = "Mean absolute prediction error\n(% of polygon area)", colour = "",
       title = "Upper stratum mean prediction error by match period")

frac_error_match_viz <- pop_error %>%
  filter(poly_size == 60000 & sim == 1) %>%
  pivot_longer(cols = ends_with("mae_frac")) %>%
  ggplot(aes(x = match, y = value, colour = name)) +
  geom_line() + 
  scale_colour_manual(labels = c("Population", "Highest loss bin"), values = c("blue", "red")) +
  geom_hline(yintercept = 1, colour = "#F8766D") +
  theme_few() +
  facet_wrap(~country) +
  # ylim(0, 1.5) +
  scale_x_continuous(breaks = seq(0, 24, 4)) +
  labs(x = "Matching period", y = "Mean absolute prediction error\n(fraction of mean loss rate)", colour = "",
       title = "Fractional prediction error by matching period")

# stratum_34_error_viz <- stratum_error %>%
#   filter(poly_size == 60000 & stratum %in% c(3,4) * match == 8) %>%
#   group_by(country, sim) %>%
#   summarise(stratum_mae = weighted.mean(stratum_mae, n_stratum),
#             stratum_loss = weighted.mean(stratum_loss, n_stratum)) %>%
#   ggplot(aes(x = as.factor(sim), y = stratum_mae)) +
#   geom_point(shape = 18) +
#   geom_hline(aes(yintercept = stratum_loss, colour = "Mean forest\nloss rate\n(% of polygon\narea)")) +
#   theme_few() +
#   facet_wrap(~country) +
#   ylim(0, 5) +
#   scale_x_continuous(breaks = seq(0, 24, 4)) +
#   labs(x = "Matching period", y = "Mean absolute prediction error\n(% of polygon area)", colour = "",
#        title = "Upper stratum mean prediction error by simulation")

# Bias plots

pop_bias_match_viz <- pop_error %>%
  filter(poly_size == 60000 & sim == 1) %>%
  ggplot(aes(x = match, y = pop_bias)) +
  geom_line() +
  geom_hline(yintercept = 0, colour = "#F8766D") +
  theme_few() +
  facet_wrap(~country) +
  ylim(-0.2, 0.2) +
  scale_x_continuous(breaks = seq(0, 24, 4)) +
  labs(x = "Matching period", y = "Mean prediction bias\n(% of polygon area)",
       title = "Population-level mean prediction bias by matching period")

stratum_4_bias_match_viz <- stratum_error %>%
  filter(poly_size == 60000 & stratum == 4 & sim == 1) %>%
  ggplot(aes(x = match, y = stratum_bias)) +
  geom_line() +
  geom_hline(yintercept = 0, colour = "#F8766D") +
  theme_few() +
  facet_wrap(~country) +
  ylim(-6, 6) +
  scale_x_continuous(breaks = seq(0, 24, 4)) +
  labs(x = "Matching period", y = "Mean prediction bias\n(% of polygon area)", colour = "",
       title = "Upper stratum mean prediction bias by matching period")

frac_bias_match_viz <- pop_error %>%
  filter(poly_size == 60000 & sim == 5) %>%
  pivot_longer(cols = ends_with("bias_frac")) %>%
  ggplot(aes(x = match, y = value, colour = name)) +
  geom_line() +
  scale_colour_manual(labels = c("Population", "Highest loss bin"), values = c("blue", "red")) +
  geom_hline(yintercept = 0, colour = "#F8766D") +
  theme_few() +
  facet_wrap(~country) +
  # ylim(-1, 1) +
  scale_x_continuous(breaks = seq(0, 24, 4)) +
  labs(x = "Matching period", y = "Mean prediction bias\n(fraction of mean loss rate)", colour = "",
       title = "Fractional prediction bias by matching period")

# Save to disk

ggsave("results/figures/mae_by_country_population_match.png",
       pop_error_match_viz,
       width = 24, height = 16, units = "cm", dpi = 300)

ggsave("results/figures/mae_by_country_s4_match.png",
       stratum_4_error_match_viz,
       width = 24, height = 16, units = "cm", dpi = 300)

ggsave("results/figures/mae_by_country_frac_match.png",
       frac_error_match_viz,
       width = 24, height = 16, units = "cm", dpi = 300)

ggsave("results/figures/bias_by_country_population_match.png",
       pop_bias_match_viz,
       width = 24, height = 16, units = "cm", dpi = 300)

ggsave("results/figures/bias_by_country_s4_match.png",
       stratum_4_bias_match_viz,
       width = 24, height = 16, units = "cm", dpi = 300)

ggsave("results/figures/bias_by_country_frac_match.png",
       frac_bias_match_viz,
       width = 24, height = 16, units = "cm", dpi = 300)

# STILL TO DO
# All the same results by polygon size
# Do a different viz about error over the post-intervention period (RQ4)
# Think about the forward problem - how does this inform us if we have a
# project with a particular synthetic control-estimated baseline and we
# want to know how accurate it might be?
# There are two indicators - what is the estimated error and bias conditional on the
# observed error in the matching period, and what is the estimated error and bias
# conditional upon the estimated baseline?


### 6. RQ2 - Mean absolute error and bias by polygon size ----

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
  filter(match == 8 & sim == 5 & year > START_YEAR) %>%
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
  facet_wrap(~country, scales = "free") +
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


### 7. RQ4 - Performance of SC method over a long test period

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
  facet_wrap(~country) +
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
  facet_wrap(~country) +
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
