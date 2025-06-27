# ============================================================================
# NDVI ANALYSIS - SAN FRANCISCO SCIENTIFIC STATION
# Cross-correlation, Monthly Analysis + Water Balance Analysis (2018-2022)
# ============================================================================

rm(list = ls())

# Install and load required packages
required_packages <- c("terra", "dplyr", "ggplot2", "lubridate", "viridis", 
                       "gridExtra", "scales", "tidyr", "readr")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# Load data
data_dir <- "D:/R/Curso_RemoteSensing/homework"
ndvi_timeseries <- read_csv(file.path(data_dir, "NDVI_TimeSeries_SF.csv"), show_col_types = FALSE)
weather_data <- read_csv(file.path(data_dir, "weather.csv"), show_col_types = FALSE)
s2_stack <- rast(file.path(data_dir, "S2_NDVI_Complete_Stack_SF.tif"))
s2_dates <- read_csv(file.path(data_dir, "S2_Band_Metadata_SF.csv"), show_col_types = FALSE)

# Data preparation with NDVI filter
ndvi_timeseries$date <- as.Date(ndvi_timeseries$date)
weather_data$Date <- dmy(weather_data$Date)
s2_dates$date <- as.Date(s2_dates$date)
s2_dates$year <- year(s2_dates$date)

# Filter NDVI > 0.5 to remove cloudy days
ndvi_clean <- ndvi_timeseries %>% 
  filter(!is.na(NDVI), NDVI > 0.5)

cat("==================== DATA SUMMARY ====================\n")
cat("Original observations:", nrow(ndvi_timeseries), "\n")
cat("After filtering NDVI > 0.5:", nrow(ndvi_clean), "\n")
cat("Data retention:", round(nrow(ndvi_clean)/nrow(ndvi_timeseries)*100, 1), "%\n")
cat("Study period:", as.character(min(ndvi_clean$date)), "to", as.character(max(ndvi_clean$date)), "\n")
cat("Observations by sensor:\n")
print(table(ndvi_clean$sensor))

# ============================================================================
# ENHANCED CLIMATE ANALYSIS WITH WATER BALANCE
# ============================================================================

# Calculate water balance (Precipitation - Evapotranspiration)
weather_enhanced <- weather_data %>%
  filter(!is.na(Rain_mm), !is.na(`Ta_°C`), !is.na(Eta_mm)) %>%
  mutate(
    Water_Balance = Rain_mm - Eta_mm,
    year = year(Date),
    month = month(Date),
    day_of_year = yday(Date)
  )

# Monthly aggregates for correlation analysis (including water balance)
weather_monthly <- weather_enhanced %>%
  group_by(year, month) %>%
  reframe(
    Rain_mm = mean(Rain_mm, na.rm = TRUE),
    Temp_C = mean(`Ta_°C`, na.rm = TRUE),
    ET_mm = mean(Eta_mm, na.rm = TRUE),
    Water_Balance = mean(Water_Balance, na.rm = TRUE),
    date = as.Date(paste(year, month, "15", sep = "-"))
  )

# Seasonal aggregates for water balance analysis
weather_seasonal <- weather_enhanced %>%
  mutate(
    season = case_when(
      month %in% c(12, 1, 2) ~ "Dry Season (Dec-Feb)",
      month %in% c(3, 4, 5) ~ "Transition (Mar-May)",
      month %in% c(6, 7, 8) ~ "Wet Season (Jun-Aug)",
      month %in% c(9, 10, 11) ~ "Transition (Sep-Nov)"
    )
  ) %>%
  group_by(year, season) %>%
  reframe(
    Rain_total = sum(Rain_mm, na.rm = TRUE),
    ET_total = sum(Eta_mm, na.rm = TRUE),
    Water_Balance_total = sum(Water_Balance, na.rm = TRUE),
    Temp_mean = mean(`Ta_°C`, na.rm = TRUE),
    n_days = n()
  ) %>%
  filter(n_days >= 60)  # Only complete seasons

ndvi_monthly <- ndvi_clean %>%
  mutate(year = year(date), month = month(date)) %>%
  group_by(year, month) %>%
  reframe(
    NDVI = mean(NDVI, na.rm = TRUE),
    date = as.Date(paste(year, month, "15", sep = "-")),
    n_obs = n()
  )

combined_data <- merge(ndvi_monthly, weather_monthly, by = "date", all = FALSE)

# ============================================================================
# METHOD 1: ENHANCED CROSS-CORRELATION ANALYSIS (INCLUDING WATER BALANCE)
# ============================================================================

calculate_cross_correlation <- function(x, y, max_lag = 6) {
  ccf_result <- ccf(x, y, lag.max = max_lag, plot = FALSE, na.action = na.pass)
  data.frame(
    lag = ccf_result$lag,
    correlation = as.vector(ccf_result$acf)
  )
}

ccf_rain <- calculate_cross_correlation(combined_data$NDVI, combined_data$Rain_mm)
ccf_temp <- calculate_cross_correlation(combined_data$NDVI, combined_data$Temp_C)
ccf_et <- calculate_cross_correlation(combined_data$NDVI, combined_data$ET_mm)
ccf_wb <- calculate_cross_correlation(combined_data$NDVI, combined_data$Water_Balance)

ccf_rain$variable <- "Precipitation"
ccf_temp$variable <- "Temperature"
ccf_et$variable <- "Evapotranspiration"
ccf_wb$variable <- "Water Balance"
ccf_combined <- rbind(ccf_rain, ccf_temp, ccf_et, ccf_wb)

optimal_lags <- ccf_combined %>%
  group_by(variable) %>%
  slice_max(abs(correlation), n = 1) %>%
  ungroup()

# Plot 1: Enhanced Cross-correlation analysis
p1 <- ggplot(ccf_combined, aes(x = lag, y = correlation)) +
  geom_line(linewidth = 1, color = "steelblue") +
  geom_point(size = 2, color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_hline(yintercept = c(-0.3, 0.3), linetype = "dotted", color = "red", alpha = 0.7) +
  facet_wrap(~variable, scales = "free_y", ncol = 2) +
  labs(
    title = "Cross-correlation between NDVI and Climate Variables",
    subtitle = "San Francisco Scientific Station (2018-2022) - Including Water Balance",
    x = "Lag (months)",
    y = "Correlation coefficient"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    strip.text = element_text(face = "bold")
  )

# ============================================================================
# METHOD 2: WATER BALANCE ANALYSIS WITH SEASONAL PATTERNS
# ============================================================================

# Plot 2: Water balance time series (simplified, no NDVI overlay)
p2_data <- weather_enhanced %>%
  mutate(year_month = floor_date(Date, "month")) %>%
  group_by(year_month) %>%
  reframe(
    Water_Balance_monthly = sum(Water_Balance, na.rm = TRUE),
    Rain_monthly = sum(Rain_mm, na.rm = TRUE),
    ET_monthly = sum(Eta_mm, na.rm = TRUE),
    date = first(year_month)
  )

p2 <- ggplot(p2_data, aes(x = date)) +
  # Water balance bars
  geom_col(aes(y = Water_Balance_monthly), alpha = 0.7, 
           fill = ifelse(p2_data$Water_Balance_monthly >= 0, "steelblue", "coral")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.8) +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  labs(
    title = "Monthly Water Balance Time Series",
    subtitle = "Water Balance = Precipitation - Evapotranspiration",
    x = "Date",
    y = "Water Balance (mm/month)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Plot 3: Monthly patterns with sensor differentiation (same as before)
p3 <- ggplot(ndvi_clean %>% mutate(month = month(date), year = year(date)), 
             aes(x = factor(month), y = NDVI)) +
  geom_boxplot(alpha = 0.3, outlier.shape = NA) +
  geom_point(aes(color = sensor, shape = sensor), size = 2.5, alpha = 0.8, 
             position = position_jitter(width = 0.3)) +
  scale_color_manual(values = c("Landsat-8" = "#FF6B35", "Sentinel-2" = "#228B22"),
                     name = "Sensor") +
  scale_shape_manual(values = c("Landsat-8" = 16, "Sentinel-2" = 17),
                     name = "Sensor") +
  scale_x_discrete(labels = month.abb) +
  labs(
    title = "Monthly NDVI Patterns by Sensor",
    subtitle = "Filtered data (NDVI > 0.5) - Interannual comparison",
    x = "Month",
    y = "NDVI"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

# Plot 4: Seasonal water balance analysis
p4 <- ggplot(weather_seasonal, aes(x = season, y = Water_Balance_total, fill = season)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.8, size = 2.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.7) +
  scale_fill_viridis_d(option = "plasma", alpha = 0.8) +
  labs(
    title = "Seasonal Water Balance Patterns",
    subtitle = "Water Balance = Precipitation - Evapotranspiration (by season)",
    x = "Season",
    y = "Water Balance (mm/season)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

# SPATIAL ANALYSIS: Create annual NDVI maps (same as before)
annual_maps <- list()
annual_dfs <- list()

for(yr in sort(unique(s2_dates$year))) {
  year_indices <- which(s2_dates$year == yr)
  if(length(year_indices) > 0) {
    annual_mean <- mean(s2_stack[[year_indices]], na.rm = TRUE)
    annual_filtered <- ifel(annual_mean < 0.5, NA, annual_mean)
    
    annual_df <- as.data.frame(annual_filtered, xy = TRUE, na.rm = FALSE)
    names(annual_df)[3] <- "NDVI"
    annual_df$year <- yr
    annual_dfs[[as.character(yr)]] <- annual_df
  }
}

combined_annual_df <- do.call(rbind, annual_dfs)

# Plot 5: Annual NDVI Maps
p5 <- ggplot(combined_annual_df, aes(x = x, y = y, fill = NDVI)) +
  geom_raster() +
  scale_fill_viridis_c(name = "NDVI", na.value = "gray90", 
                       limits = c(0.5, 1), option = "viridis") +
  facet_wrap(~year, ncol = 3, labeller = label_both) +
  coord_fixed() +
  labs(
    title = "Annual Mean NDVI Maps",
    subtitle = "San Francisco Scientific Station - Sentinel-2 (Filtered NDVI > 0.5)",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.text = element_text(size = 6),
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "bottom"
  )

# Plot 6: Sensor comparison (same as before)
p6 <- ggplot(ndvi_clean, aes(x = sensor, y = NDVI, fill = sensor)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.6, size = 2) +
  scale_fill_manual(values = c("Landsat-8" = "#FF6B35", "Sentinel-2" = "#228B22")) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 4, 
               fill = "white", color = "black") +
  labs(
    title = "NDVI Comparison Between Sensors",
    subtitle = "Diamonds show mean values - Filtered NDVI > 0.5",
    x = "Sensor",
    y = "NDVI"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "none"
  )

# Display only the 3 required plots
print(p2)  # Water Balance Time Series
print(p4)  # Seasonal Water Balance
print(p5)  # Annual NDVI Maps

# ============================================================================
# ENHANCED STATISTICAL ANALYSIS WITH WATER BALANCE
# ============================================================================

cat("\n==================== STATISTICAL ANALYSIS ====================\n")

# Sensor comparison
sentinel_ndvi <- ndvi_clean$NDVI[ndvi_clean$sensor == "Sentinel-2"]
landsat_ndvi <- ndvi_clean$NDVI[ndvi_clean$sensor == "Landsat-8"]

cat("SENSOR STATISTICS:\n")
cat("Sentinel-2: n =", length(sentinel_ndvi), ", Mean =", round(mean(sentinel_ndvi), 4), 
    ", SD =", round(sd(sentinel_ndvi), 4), "\n")
cat("Landsat-8:  n =", length(landsat_ndvi), ", Mean =", round(mean(landsat_ndvi), 4), 
    ", SD =", round(sd(landsat_ndvi), 4), "\n")

# T-test
t_test_result <- t.test(sentinel_ndvi, landsat_ndvi)
cat("\nT-TEST RESULTS:\n")
cat("t-statistic =", round(t_test_result$statistic, 4), "\n")
cat("p-value =", format(t_test_result$p.value, scientific = TRUE, digits = 4), "\n")
cat("Mean difference =", round(t_test_result$estimate[1] - t_test_result$estimate[2], 4), "\n")
if(t_test_result$p.value < 0.05) {
  cat("CONCLUSION: Significant difference between sensors (p < 0.05)\n")
} else {
  cat("CONCLUSION: No significant difference between sensors (p >= 0.05)\n")
}

# Effect size
pooled_sd <- sqrt(((length(sentinel_ndvi)-1)*var(sentinel_ndvi) + 
                     (length(landsat_ndvi)-1)*var(landsat_ndvi)) / 
                    (length(sentinel_ndvi) + length(landsat_ndvi) - 2))
cohens_d <- abs(mean(sentinel_ndvi) - mean(landsat_ndvi)) / pooled_sd
cat("Cohen's d (effect size) =", round(cohens_d, 4), "\n")

# Monthly ANOVA
anova_month <- aov(NDVI ~ factor(month(date)), data = ndvi_clean)
anova_summary <- summary(anova_month)
cat("\nMONTHLY ANOVA RESULTS:\n")
cat("F-statistic =", round(anova_summary[[1]]$`F value`[1], 4), "\n")
cat("p-value =", format(anova_summary[[1]]$`Pr(>F)`[1], scientific = TRUE, digits = 4), "\n")
if(anova_summary[[1]]$`Pr(>F)`[1] < 0.05) {
  cat("CONCLUSION: Significant monthly differences (p < 0.05)\n")
} else {
  cat("CONCLUSION: No significant monthly differences (p >= 0.05)\n")
}

# Enhanced climate correlations (including water balance)
cat("\nENHANCED CLIMATE CORRELATIONS:\n")
for(var in unique(optimal_lags$variable)) {
  opt_lag <- optimal_lags[optimal_lags$variable == var, ]
  cat(sprintf("%-18s: r = %6.3f at lag %2d months\n", 
              var, opt_lag$correlation, opt_lag$lag))
}

# Water balance statistics
cat("\nWATER BALANCE STATISTICS:\n")
wb_stats <- weather_enhanced %>%
  summarise(
    mean_wb = round(mean(Water_Balance, na.rm = TRUE), 3),
    sd_wb = round(sd(Water_Balance, na.rm = TRUE), 3),
    min_wb = round(min(Water_Balance, na.rm = TRUE), 3),
    max_wb = round(max(Water_Balance, na.rm = TRUE), 3),
    deficit_days = sum(Water_Balance < 0, na.rm = TRUE),
    surplus_days = sum(Water_Balance > 0, na.rm = TRUE),
    total_days = sum(!is.na(Water_Balance))
  )

cat("Daily Water Balance: Mean =", wb_stats$mean_wb, "mm, SD =", wb_stats$sd_wb, "mm\n")
cat("Range:", wb_stats$min_wb, "to", wb_stats$max_wb, "mm\n")
cat("Water deficit days:", wb_stats$deficit_days, "(", 
    round(wb_stats$deficit_days/wb_stats$total_days*100, 1), "%)\n")
cat("Water surplus days:", wb_stats$surplus_days, "(", 
    round(wb_stats$surplus_days/wb_stats$total_days*100, 1), "%)\n")

# Seasonal water balance ANOVA
if(nrow(weather_seasonal) > 0) {
  anova_wb <- aov(Water_Balance_total ~ season, data = weather_seasonal)
  anova_wb_summary <- summary(anova_wb)
  cat("\nSEASONAL WATER BALANCE ANOVA:\n")
  cat("F-statistic =", round(anova_wb_summary[[1]]$`F value`[1], 4), "\n")
  cat("p-value =", format(anova_wb_summary[[1]]$`Pr(>F)`[1], scientific = TRUE, digits = 4), "\n")
  if(anova_wb_summary[[1]]$`Pr(>F)`[1] < 0.05) {
    cat("CONCLUSION: Significant seasonal water balance differences (p < 0.05)\n")
  } else {
    cat("CONCLUSION: No significant seasonal water balance differences (p >= 0.05)\n")
  }
}

# Monthly and annual statistics (same as before)
cat("\nMONTHLY STATISTICS:\n")
monthly_stats <- ndvi_clean %>%
  mutate(month = month(date)) %>%
  group_by(month) %>%
  summarise(
    n = n(),
    mean_ndvi = round(mean(NDVI), 4),
    sd_ndvi = round(sd(NDVI), 4),
    .groups = 'drop'
  ) %>%
  mutate(month_name = month.abb[month])
print(monthly_stats)

cat("\nANNUAL STATISTICS:\n")
annual_stats <- ndvi_clean %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(
    n = n(),
    mean_ndvi = round(mean(NDVI), 4),
    sd_ndvi = round(sd(NDVI), 4),
    .groups = 'drop'
  )
print(annual_stats)

cat("\n==================== SUMMARY ====================\n")
cat("Overall mean NDVI:", round(mean(ndvi_clean$NDVI), 4), "\n")
cat("Overall SD NDVI:", round(sd(ndvi_clean$NDVI), 4), "\n")
cat("NDVI range:", round(min(ndvi_clean$NDVI), 3), "to", round(max(ndvi_clean$NDVI), 3), "\n")
cat("Coefficient of variation:", round(sd(ndvi_clean$NDVI)/mean(ndvi_clean$NDVI)*100, 2), "%\n")

# Save only the 3 required plots
ggsave("Water_Balance_TimeSeries.png", p2, width = 14, height = 8, dpi = 300)
ggsave("Seasonal_Water_Balance.png", p4, width = 10, height = 6, dpi = 300)
ggsave("NDVI_Annual_Maps.png", p5, width = 14, height = 10, dpi = 300)

cat("\n==================== ANALYSIS COMPLETE ====================\n")
cat("Generated 3 plots for report:\n")
cat("1. Water_Balance_TimeSeries.png - Monthly water balance over time\n")
cat("2. Seasonal_Water_Balance.png - Seasonal water balance patterns\n")  
cat("3. NDVI_Annual_Maps.png - Annual NDVI spatial maps\n")
cat("All plots saved as PNG files\n")