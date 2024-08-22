#### Data analysis script
#### Fits augmented synthetic controls for sample polygon data and writes the
#### results to disk as a CSV.

source("scripts/load.R")

### 1. Set parameters --------

# Data selection
COUNTRIES <- c("Myanmar", "Malaysia", "Bolivia", "Cote d'Ivoire", "Madagascar", "Colombia")  # Target country
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

# set.seed(VIZ_SEED)
# viz_ids <- sample(sample_ids$ID, 36)
# 
# ts_plots <- sc_df %>%
#   filter(ID %in% viz_ids) %>%
#   filter(match == 8) %>%
#   # filter(sim %in% c(1, 5)) %>%
#   ggplot(aes(x = as.numeric(year))) +
#   geom_line(aes(y = loss, colour = "Observed"), lwd = 1) +
#   geom_line(aes(y = sc_loss, colour = as.factor(sim))) +
#   geom_vline(xintercept = START_YEAR) +
#   facet_wrap(~ID) +
#   theme_bw() +
#   labs(x = "Year", y = "Annual loss rate (area frac)", colour = NULL) +
#   scale_colour_manual(
#     labels = c(paste0("S", 1:5), "Observed"),
#     values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00', "black"))
# 
# 
# ggsave(
#   paste0("results/figures/sc_plots/", COUNTRY, "_", START_YEAR, "_", POLY_SIZE, "SIM_ts_plots.jpg"),
#   ts_plots,
#   width = 24, height = 20, dpi = 300, units = "cm"
# )

# Mean absolute error per simulation

sc_mae <- sc_df %>%
  mutate(test_period = (year > START_YEAR)) %>%
  mutate(abs_error = abs(sc_loss - loss)) %>%
  group_by(ID, stratum, sim, match, country, poly_size, test_period) %>%
  summarise(mae = mean(abs_error),
            mae_frac = mae / mean(loss))

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

# Mean MAE per country and simulation

mae_means <- sc_mae %>%
  group_by(country, sim, poly_size)


# ggsave(
#   paste0("results/figures/sc_plots/", COUNTRY, "_", START_YEAR, "_", POLY_SIZE, "_", MATCHING_PERIOD, "Y_mae_plot_sim.jpg"),
#   mae_plot_sim,
#   width = 20, height = 16, dpi = 300, units = "cm"
# )

# Normalised by mean deforestation rate

sc_mae_

# Mean absolute error by match period

mae_plot_match <- sc_mae %>%
  filter(sim == 5) %>%
  left_join(test_period_lookup) %>%
  ggplot(aes(x = as.factor(match), y = mae, fill = as.factor(match))) +
  geom_boxplot(alpha = 0.5, show.legend = FALSE) +
  scale_fill_brewer(palette = "Greens") +
  facet_wrap(~label) +
  theme_bw() +
  labs(x = "Matching period", y = "Mean absolute error\n(annual loss rate)")



