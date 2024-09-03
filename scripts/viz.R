#### Data analysis script
#### Fits augmented synthetic controls for sample polygon data and writes the
#### results to disk as a CSV.

source("scripts/load.R")

### 1. Set parameters --------

# Data selection
COUNTRIES <- c("Myanmar", "Malaysia", "Bolivia", "Cote d'Ivoire", "Madagascar", "Colombia", "Indonesia")  # Target country
START_YEAR <- 2016  # Simulated start year of protection project
POLY_SIZE <- 60000 # size of polygons in hectares
VIZ_SEED <- 111

### 2. Load data

sc_df <- map(COUNTRIES, function(country) {
  country_df <- map(POLY_SIZE, function(size) {
    filename <- paste0("sc_results_", country, "_", START_YEAR, "_", size, ".csv")
    df <- read_csv(paste0("results/sc_results/", filename)) %>%
      mutate(poly_size = size)
    df
  }) %>% bind_rows()
  
  country_df <- mutate(country_df, country = country)
  
  country_df
}) %>% bind_rows()

set.seed(VIZ_SEED)
sc_sample <- sc_df %>%
  group_by(country, stratum) %>%
  reframe(ID = unique(ID)) %>%
  group_by(country, stratum) %>%
  slice_sample(n = 2) %>%
  mutate(country_id = 1:8) %>%
  left_join(sc_df)

ts_plots <- sc_sample %>%
  filter(match == 8) %>%
  ggplot(aes(x = as.numeric(year))) +
  geom_line(aes(y = loss, colour = "Observed"), lwd = 1) +
  geom_line(aes(y = sc_loss, colour = as.factor(sim))) +
  geom_vline(xintercept = START_YEAR) +
  facet_grid(rows = vars(country_id), cols = vars(country)) +
  theme_bw() +
  labs(x = "Year", y = "Annual loss rate (area frac)", colour = NULL) +
  scale_colour_manual(
    labels = c(paste0("S", 1:5), "Observed"),
    values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00', "black"))

sc_bin34 <- sc_df %>%
  filter(stratum %in% c(3, 4)) %>%
  filter(match == 8) %>%
  group_by(country, sim, poly_size) %>%
  mutate(country_id = 1:n()) %>%
  ggplot(aes(x = as.numeric(year))) +
  geom_line(aes(y = loss, colour = "Observed"), lwd = 1) +
  geom_line(aes(y = sc_loss, colour = as.factor(sim))) +
  geom_vline(xintercept = START_YEAR) +
  facet_grid(rows = vars(country_id), cols = vars(country)) +
  theme_bw() +
  labs(x = "Year", y = "Annual loss rate (area frac)", colour = NULL) +
  scale_colour_manual(
    labels = c(paste0("S", 1:5), "Observed"),
    values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00', "black"))


ggsave(
  paste0("results/figures/sc_plots/", COUNTRY, "_", START_YEAR, "_", POLY_SIZE, "SIM_ts_plots.jpg"),
  ts_plots,
  width = 24, height = 20, dpi = 300, units = "cm"
)

# Mean absolute error per simulation

sc_mae <- sc_df %>%
  mutate(test_period = (year > START_YEAR)) %>%
  mutate(abs_error = abs(sc_loss - loss)) %>%
  group_by(ID, stratum, sim, match, country, poly_size, test_period) %>%
  summarise(mae = mean(abs_error),
            mean_loss = mean(loss),
            mae_frac = mae / mean_loss)

# test_period_lookup <- tibble(
#   test_period = c(TRUE, FALSE),
#   label = c("Test period", "Matching period")
# )

mae_plot_rq1 <- sc_mae %>%
  filter(match == 8) %>%
  filter(poly_size == 60000) %>%
  # left_join(test_period_lookup) %>%
  ggplot(aes(x = as.factor(sim), y = mae, fill = as.factor(sim))) +
  geom_boxplot(alpha = 0.5, show.legend = FALSE) +
  scale_fill_brewer(palette = "Set1") +
  facet_wrap(~country) +
  theme_bw() +
  labs(x = "Simulation", y = "Mean absolute error\n(annual loss rate)")

mae_plot_rq1_frac <- sc_mae %>%
  filter(match == 8) %>%
  filter(poly_size == 60000) %>%
  # left_join(test_period_lookup) %>%
  ggplot(aes(x = as.factor(sim), y = mae_frac, fill = as.factor(stratum))) +
  geom_boxplot(alpha = 0.5, show.legend = FALSE) +
  scale_fill_brewer(palette = "Set1") +
  facet_wrap(~country) +
  theme_bw() +
  labs(x = "Simulation", y = "Mean absolute error\n(annual loss rate)") +
  coord_cartesian(ylim = c(0, 10))

# Mean MAE per country and simulation - bin 4 only

summary_fun_list <- list(mean = mean, sd = sd, se = ~ sd(.x) / sqrt(n()), max = max, min = min)

mae_means_bin4 <- sc_mae %>%
  filter(test_period == TRUE & stratum == 4 & match == 8) %>%
  group_by(sim, match, country, poly_size) %>%
  summarise(across(starts_with("mae"), .fns = summary_fun_list),
            n = n())

country_n_bin4 <- mae_means_bin4 %>%
  filter(poly_size == 60000) %>%
  group_by(country) %>%
  summarise(n = mean(n))

mae_plot_bin4_abs <- ggplot(mae_means_bin4, aes(x = as.factor(sim), y = mae_mean)) +
  geom_errorbar(
    aes(ymin = (mae_mean - mae_sd), ymax = (mae_mean + mae_sd)),
    width = 0.1,
    colour = "grey20") +
  geom_point(aes(colour = as.factor(sim)), show.legend = FALSE) +
  theme_bw() +
  facet_wrap(~country, labeller = as_labeller(~ paste0(.x, ", n = ", country_n_bin4$n[country_n_bin4$country == .x]))) +
  labs(
    x = "Simulation number",
    y = "Mean MAE in high-loss units\nas fraction of polygon area"
  )

mae_plot_bin4_frac <- ggplot(mae_means_bin4, aes(x = as.factor(sim), y = mae_frac_mean)) +
  geom_errorbar(
    aes(ymin = (mae_frac_mean - mae_frac_sd), ymax = (mae_frac_mean + mae_frac_sd)),
    width = 0.1,
    colour = "grey20") +
  geom_point(aes(colour = as.factor(sim)), show.legend = FALSE) +
  theme_bw() +
  facet_wrap(~country, labeller = as_labeller(~ paste0(.x, ", n = ", country_n_bin4$n[country_n_bin4$country == .x]))) +
  ylim(0, 1.2) +
  labs(
    x = "Simulation number",
    y = "Mean MAE in high-loss units\nas fraction of observed loss"
  )

mae_plot_bin4_abs
mae_plot_bin4_frac

# Mean values per country and simulation - full sample

mae_means_all <- sc_mae %>%
    filter(test_period == TRUE & match == 8) %>%
    group_by(sim, match, country, poly_size) %>%
    summarise(across(starts_with("mae"), .fns = summary_fun_list),
              n = n())
  
country_n_all <- mae_means_all %>%
    filter(poly_size == 60000) %>%
    group_by(country) %>%
    summarise(n = mean(n))

mae_plot_all_abs <- ggplot(mae_means_all, aes(x = as.factor(sim), y = mae_mean)) +
  geom_errorbar(
    aes(ymin = (mae_mean - mae_sd), ymax = (mae_max + mae_sd)),
    width = 0.1,
    colour = "grey20") +
  geom_point(aes(colour = as.factor(sim)), show.legend = FALSE) +
  theme_bw() +
  facet_wrap(~country, labeller = as_labeller(~ paste0(.x, ", n = ", country_n_all$n[country_n_all$country == .x]))) +
  labs(
    x = "Simulation number",
    y = "Mean MAE in all units\nas fraction of polygon area"
  )

mae_boxplot_all_abs <- ggplot(sc_mae, aes(x = as.factor(sim), y = mae, fill = as.factor(stratum))) +
  geom_boxplot(position = "dodge", alpha = 0.5, colour = "grey20", outliers = FALSE) +
  theme_bw() +
  facet_wrap(~country) +
  labs(
    x = "Simulation number",
    y = "MAE in all units\nas fraction of polygon area",
    fill = "Loss bin"
  ) +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 0),
               geom = "pointrange", colour = "black", shape = 18, size = 0.75,
               position = position_dodge(width = 0.75)) 

mae_plot_all_abs
mae_boxplot_all_abs



# ggsave(
#   paste0("results/figures/sc_plots/", COUNTRY, "_", START_YEAR, "_", POLY_SIZE, "_", MATCHING_PERIOD, "Y_mae_plot_sim.jpg"),
#   mae_plot_sim,
#   width = 20, height = 16, dpi = 300, units = "cm"
# )



## Mean absolute error by match period

mae_mean_match_bin4 <- sc_mae %>%
  filter(sim == 5 & test_period == TRUE & stratum == 4) %>%
  group_by(country, poly_size, match) %>%
  summarise(across(starts_with("mae"), .fns = summary_fun_list),
            n = n())

mae_plot_match_bin4 <- ggplot(mae_mean_match_bin4, aes(x = match, y = mae_mean)) +
  geom_errorbar(
    aes(ymin = (mae_mean - mae_sd), ymax = (mae_mean + mae_sd)),
    width = 0.1,
    colour = "grey20") +
  geom_point(shape = 18) +
  # scale_colour_brewer(palette = "Greens") +
  facet_wrap(~country) +
  theme_bw() +
  labs(x = "Matching period", y = "MAE in all units\nas fraction of polygon area")

mae_mean_match_all <- sc_mae %>%
  filter(sim == 5 & test_period == TRUE) %>%
  group_by(country, poly_size, match) %>%
  summarise(across(starts_with("mae"), .fns = summary_fun_list),
            n = n())

mae_plot_match_all <- ggplot(mae_mean_match_all, aes(x = match, y = mae_mean)) +
  geom_errorbar(
    aes(ymin = (mae_mean - mae_sd), ymax = (mae_mean + mae_sd)),
    width = 0.1,
    colour = "grey20") +
  geom_point(shape = 18) +
  # scale_colour_brewer(palette = "Greens") +
  facet_wrap(~country) +
  theme_bw() +
  labs(x = "Matching period", y = "MAE in all units\nas fraction of polygon area")

mae_plot_match_bin4
mae_plot_match_all

ggsave()



