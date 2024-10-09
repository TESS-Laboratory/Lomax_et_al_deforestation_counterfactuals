#### Data analysis script
#### Fits augmented synthetic controls for sample polygon data and writes the
#### results to disk as a CSV.

source("scripts/load.R")

### 1. Set parameters --------

# Data selection
COUNTRIES <- c(
  "Bolivia", "Brazil", 
  "Colombia",
  "Cote d'Ivoire", "Democratic Republic of the Congo", "Madagascar",
  "Indonesia",
  "Malaysia", "Myanmar"
)
START_YEAR <- 2016  # Simulated start year of protection project
RQ4_START_YEAR <- 1998  # Simulated start year for RQ4 simulations
POLY_SIZE <- c(10000, 60000, 600000) # size of polygons in hectares
VIZ_SEED <- 111

### 2. Load data --------

# SC results by country
sc_df <- map(COUNTRIES, function(country) {
  country_df <- map(POLY_SIZE, function(size) {
    filename <- paste0("sc_results_", country, "_", START_YEAR, "_", size, ".csv")
    if(file.exists(paste0("results/sc_results/", filename))) {
      df <- read_csv(paste0("results/sc_results/", filename)) %>%
        mutate(poly_size = size)
      df
    } else {
      NULL
    }
  }) %>% 
    discard(is.null) %>%
    discard(\(x) nrow(x) == 0) %>%
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
  filter(year > START_YEAR) %>%
  group_by(country, poly_size, ID) %>%
  summarise(mean_loss = mean(loss)) %>%
  group_by(country, poly_size) %>%
  mutate(stratum = cut_interval(mean_loss, n = 4, labels = FALSE))

# Calculate relative frequency of strata for each country and polygon size
stratum_freq <- defor_long_loss %>%
  group_by(country, poly_size) %>%
  mutate(n = n(),
         mean_loss_all = mean(mean_loss)) %>%
  group_by(country, poly_size, stratum) %>%
  summarise(n_stratum = n(),
            frac_stratum = n_stratum / mean(n),
            mean_loss_all = mean(mean_loss_all) * 100,
            stratum_loss = mean(mean_loss) * 100) 

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
            mean_loss_all = mean(mean_loss_all),
            pop_mae_frac = pop_mae / mean_loss_all,
            pop_bias_frac = pop_bias / mean_loss_all,
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
  geom_hline(aes(yintercept = mean_loss_all, colour = "Mean forest\nloss rate\n(% of polygon\narea)")) +
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

frac_error_viz <- pop_error %>%
  filter(poly_size == 60000 & match == 8) %>%
  pivot_longer(cols = ends_with("mae_frac")) %>%
  ggplot(aes(x = as.factor(sim), y = value, colour = name)) +
  geom_point(shape = 18, size = 2) + 
  scale_colour_manual(labels = c("Population", "Highest loss bin"), values = c("blue", "red")) +
  geom_hline(yintercept = 1, colour = "#F8766D") +
  theme_few() +
  facet_wrap(~country) +
  ylim(0, 1.5) +
  labs(x = "Simulation", y = "Mean absolute prediction error\n(fraction of mean loss rate)", colour = "",
       title = "Fractional prediction error by simulation")

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
#   labs(x = "Simulation", y = "Mean absolute prediction error\n(% of polygon area)", colour = "",
#        title = "Upper stratum mean prediction error by simulation")

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

frac_bias_viz <- pop_error %>%
  filter(poly_size == 60000 & match == 8) %>%
  pivot_longer(cols = ends_with("bias_frac")) %>%
  ggplot(aes(x = as.factor(sim), y = value, colour = name)) +
  geom_point(shape = 18, size = 2) + 
  scale_colour_manual(labels = c("Population", "Highest loss bin"), values = c("blue", "red")) +
  geom_hline(yintercept = 0, colour = "#F8766D") +
  theme_few() +
  facet_wrap(~country) +
  ylim(-1, 1) +
  labs(x = "Simulation", y = "Mean prediction bias\n(fraction of mean loss rate)", colour = "",
       title = "Fractional prediction bias by simulation")

# Continuous plots of MAE vs. observed forest loss
continuous_viz <- sc_df %>%
  filter(poly_size == 60000 & match == 8 & sim == 5) %>%
  group_by(country, sim, ID) %>%
  summarise(mean_loss = mean(loss) * 100,
            mae_loss = mean(abs(loss - sc_loss)) * 100) %>%
  ggplot(aes(x = mean_loss, y = mae_loss, colour = country)) +
  geom_point(size = 2, shape = 18) +
  # geom_smooth(se = FALSE) +
  geom_abline(intercept = 0, slope = 1, colour = "grey30") +
  theme_bw() +
  labs(x = "Mean annual forest loss\n(% of polygon area)",
       y = "Mean absolute prediction error\n(% of polygon area)",
       colour = "Country")

continuous_viz_frac <- sc_df %>%
  filter(poly_size == 60000 & match == 8 & sim == 5) %>%
  group_by(country, sim, ID) %>%
  summarise(mean_loss = mean(loss) * 100,
            mae_loss = mean(abs(loss - sc_loss)) * 100,
            mae_loss_frac = mae_loss / mean_loss) %>%
  ggplot(aes(x = mean_loss, y = mae_loss_frac, colour = country)) +
  geom_point(size = 2, shape = 18) +
  # geom_smooth(se = FALSE) +
  geom_abline(intercept = 1, slope = 0, colour = "grey30") +
  theme_bw() +
  ylim(0, 2) +
  labs(x = "Mean annual forest loss\n(% of polygon area)",
       y = "Mean absolute prediction error\n(% of observed loss)",
       colour = "Country")

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

## IDEA - also visualise ratio between matching period MAE/bias and test period MAE/bias
## (i.e., how well does matching period performance predict test period?)

## IDEA - go back and re-run analysis for just covariates and with long matching
## periods (16-24 years). This might improve performance relative to only 8 years.

### 4. Visualise SC trajectories --------

set.seed(VIZ_SEED)
sc_sample <- sc_df %>%
  filter(match == 8) %>%
  group_by(country, ID) %>%
  summarise(ID_all = cur_group_id(),
            mean_loss = mean(loss)) %>%
  group_by(country) %>%
  arrange(desc(mean_loss), .by_group = TRUE) %>%
  slice_head(n = 6) %>%
  mutate(country_id = 1:n()) %>%
  left_join(sc_df)

ts_plots <- sc_sample %>%
  filter(match == 8) %>%
  group_by(country) %>%
  ggplot(aes(x = as.numeric(year))) +
  geom_line(aes(y = loss, colour = "Observed"), lwd = 1) +
  geom_line(aes(y = sc_loss, colour = as.factor(sim))) +
  geom_vline(xintercept = START_YEAR) +
  facet_grid(cols = vars(country_id), rows = vars(country)) +
  theme_bw() +
  labs(x = "Year", y = "Annual loss rate (area frac)", colour = "Simulation") +
  scale_colour_manual(
    labels = c(paste0("S", 1:5), "Observed"),
    values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00', "black"))


ggsave(
  paste0("results/figures/sc_plots/", COUNTRY, "_", START_YEAR, "_", POLY_SIZE, "SIM_ts_plots.jpg"),
  ts_plots,
  width = 24, height = 20, dpi = 300, units = "cm"
)

### 5. RQ3 - Mean absolute error by match period ----

pop_error_match_viz <- pop_error %>%
  filter(poly_size == 60000 & sim == 5) %>%
  ggplot(aes(x = match, y = pop_mae)) +
  geom_line() +
  geom_hline(aes(yintercept = mean_loss_all, colour = "Mean forest\nloss rate\n(% of polygon\narea)")) +
  theme_few() +
  facet_wrap(~country) +
  ylim(0, 0.5) +
  scale_x_continuous(breaks = seq(0, 24, 4)) +
  labs(x = "Matching period", y = "Mean absolute prediction error\n(% of polygon area)", colour = "",
       title = "Population-level mean prediction error by matching period")

stratum_4_error_match_viz <- stratum_error %>%
  filter(poly_size == 60000 & stratum == 4 & sim == 5) %>%
  ggplot(aes(x = match, y = stratum_mae)) +
  geom_line() +
  geom_hline(aes(yintercept = stratum_loss, colour = "Mean forest\nloss rate\n(% of polygon\narea)")) +
  theme_few() +
  facet_wrap(~country) +
  ylim(0, 8) +
  scale_x_continuous(breaks = seq(0, 24, 4)) +
  labs(x = "Matching period", y = "Mean absolute prediction error\n(% of polygon area)", colour = "",
       title = "Upper stratum mean prediction error by match period")

frac_error_match_viz <- pop_error %>%
  filter(poly_size == 60000 & sim == 5) %>%
  pivot_longer(cols = ends_with("mae_frac")) %>%
  ggplot(aes(x = match, y = value, colour = name)) +
  geom_line() + 
  scale_colour_manual(labels = c("Population", "Highest loss bin"), values = c("blue", "red")) +
  geom_hline(yintercept = 1, colour = "#F8766D") +
  theme_few() +
  facet_wrap(~country) +
  ylim(0, 1.5) +
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
  filter(poly_size == 60000 & sim == 5) %>%
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
  filter(poly_size == 60000 & stratum == 4 & sim == 5) %>%
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
  ylim(-1, 1) +
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

pop_error_poly_viz <- pop_error %>%
  filter(match == 8 & sim == 5) %>%
  pivot_longer(cols = c(pop_mae, mean_loss_all)) %>%
  ggplot(aes(x = as.factor(poly_size), y = value, colour = name)) +
  geom_point(shape = 18, size = 2) +
  # geom_point(
  #   aes(y = mean_loss_all, colour = "Mean forest\nloss rate\n(% of polygon\narea)"),
  #   shape = 18, size = 2) +
  theme_few() +
  facet_wrap(~country) +
  scale_colour_manual(labels = c("Mean forest loss rate", "MAE"), values = c("black", "blue")) +
  ylim(0, 0.5) +
  labs(x = "polygon size", y = "% of polygon area", colour = "",
       title = "Population-level mean prediction error by polygon size")

stratum_4_error_poly_viz <- stratum_error %>%
  filter(match == 8 & stratum == 4 & sim == 5) %>%
  pivot_longer(cols = c(stratum_mae, stratum_loss)) %>%
  ggplot(aes(x = as.factor(poly_size), y = value, colour = name)) +
  geom_point(shape = 18, size = 2) +
  theme_few() +
  facet_wrap(~country) +
  scale_colour_manual(labels = c("Mean forest loss rate", "MAE"), values = c("black", "red")) +
  ylim(0, 12) +
  labs(x = "polygon size", y = "% of polygon area", colour = "",
       title = "Upper stratum mean prediction error by match period")

frac_error_poly_viz <- pop_error %>%
  filter(match == 8 & sim == 5) %>%
  pivot_longer(cols = ends_with("mae_frac")) %>%
  ggplot(aes(x = as.factor(poly_size), y = value, colour = name)) +
  geom_point(shape = 18, size = 2) + 
  scale_colour_manual(labels = c("Population", "Highest loss bin"), values = c("blue", "red")) +
  geom_hline(yintercept = 1, colour = "#F8766D") +
  theme_few() +
  facet_wrap(~country) +
  ylim(0, 1.5) +
  labs(x = "polygon size", y = "Mean absolute prediction error\n(fraction of mean loss rate)", colour = "",
       title = "Fractional prediction error by polygon size")

# stratum_34_error_viz <- stratum_error %>%
#   filter(match == 8 & stratum %in% c(3,4) * match == 8) %>%
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
#   labs(x = "polygon size", y = "Mean absolute prediction error\n(% of polygon area)", colour = "",
#        title = "Upper stratum mean prediction error by simulation")

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
  summarise(mean_loss = mean(loss) * 100) %>%
  group_by(country) %>%
  mutate(stratum = cut_interval(mean_loss, n = 4, labels = FALSE)) %>%
  group_by(country, stratum)

# Calculate relative frequency of strata for each country and polygon size
stratum_freq_rq4 <- defor_long_loss_rq4 %>%
  group_by(country) %>%
  mutate(n = n(),
         mean_loss_all = mean(mean_loss)) %>%
  group_by(country, stratum) %>%
  summarise(n_stratum = n(),
            frac_stratum = n_stratum / mean(n),
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
  left_join(stratum_freq)

pop_error_rq4 <- stratum_error_rq4 %>%
  group_by(country, year) %>%
  summarise(pop_mae = sum(frac_stratum * stratum_mae),
            pop_bias = sum(frac_stratum * stratum_bias),
            n = sum(n_stratum),
            mean_loss_all = mean(mean_loss_all),
            pop_mae_frac = pop_mae / mean_loss_all,
            pop_bias_frac = pop_bias / mean_loss_all,
            s4_mae = stratum_mae[stratum == 4],
            s4_bias = stratum_bias[stratum == 4],
            s4_n = n_stratum[stratum == 4],
            s4_loss = stratum_loss[stratum == 4],
            s4_mae_frac = s4_mae / s4_loss,
            s4_bias_frac = s4_bias / s4_loss)


frac_error_rq4_viz <- pop_error_rq4 %>%
  pivot_longer(cols = ends_with("mae")) %>%
  ggplot(aes(x = year, y = value, colour = name)) +
  geom_line() + 
  scale_colour_manual(labels = c("Population", "Highest loss bin"), values = c("blue", "red")) +
  # geom_hline(yintercept = 1, colour = "#F8766D") +
  theme_few() +
  facet_wrap(~country) +
  # ylim(0, 1.5) +
  labs(x = "polygon size", y = "Mean absolute prediction error\n(fraction of mean loss rate)", colour = "",
       title = "Fractional prediction error by polygon size")

pop_error_rq4_viz <- pop_error_rq4 %>%
  ggplot(aes(x = year, y = pop_mae)) +
  geom_line() +
  geom_hline(aes(yintercept = mean_loss_all, colour = "Mean forest\nloss rate\n(% of polygon\narea)")) +
  theme_few() +
  facet_wrap(~country) +
  # ylim(0, 0.5) +
  # scale_x_continuous(breaks = seq(0, 24, 4)) +
  labs(x = "Year", y = "Mean absolute prediction error\n(% of polygon area)", colour = "",
       title = "Population-level mean prediction error over test period")

stratum_4_error_rq4_viz <- stratum_error_rq4 %>%
  filter(stratum == 4) %>%
  ggplot(aes(x = year, y = stratum_mae)) +
  geom_line() +
  geom_hline(aes(yintercept = stratum_loss, colour = "Mean forest\nloss rate\n(% of polygon\narea)")) +
  theme_few() +
  facet_wrap(~country) +
  # ylim(0, 8) +
  # scale_x_continuous(breaks = seq(0, 24, 4)) +
  labs(x = "Year", y = "Mean absolute prediction error\n(% of polygon area)", colour = "",
       title = "Upper stratum mean prediction error over test period")

frac_error_rq4_viz <- pop_error_rq4 %>%
  pivot_longer(cols = ends_with("mae_frac")) %>%
  ggplot(aes(x = year, y = value, colour = name)) +
  geom_line() + 
  scale_colour_manual(labels = c("Population", "Highest loss bin"), values = c("blue", "red")) +
  geom_hline(yintercept = 1, colour = "#F8766D") +
  theme_few() +
  facet_wrap(~country) +
  # ylim(0, 1.5) +
  # scale_x_continuous(breaks = seq(0, 24, 4)) +
  labs(x = "Year", y = "Mean absolute prediction error\n(fraction of mean loss rate)", colour = "",
       title = "Fractional prediction error over test period")


ggsave("results/figures/mae_by_country_population_rq4.png",
       pop_error_rq4_viz,
       width = 24, height = 16, units = "cm", dpi = 300)

ggsave("results/figures/mae_by_country_s4_rq4.png",
       stratum_4_error_rq4_viz,
       width = 24, height = 16, units = "cm", dpi = 300)

ggsave("results/figures/mae_by_country_frac_rq4.png",
       frac_error_rq4_viz,
       width = 24, height = 16, units = "cm", dpi = 300)
