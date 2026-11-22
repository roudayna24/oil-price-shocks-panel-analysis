# THE GEOPOLITICS OF OIL
# Assessing the Macroeconomic Impact of the 2026 Gulf Crisis
# Advanced Econometrics project
#
# Source: code transcribed from the supplied PDF code document.
# Before running: place panel_data_clean.csv in the working directory,
# or update DATA_PATH below.

# SECTION 1: LOAD DATA
# INSTRUCTIONS FOR COLAB:
#      Option A (recommended): Upload the CSV using the Colab file upload widget.
#      (The Colab upload helper is Python code and is not part of this R script.)
#      Option B: Mount Google Drive and adjust the path below.
#      Option C: If running locally, set the path to your file directly.

#
DATA_PATH <- "panel_data_clean.csv"     # Change if stored elsewhere in Colab

data_raw <- read.csv(DATA_PATH, stringsAsFactors = FALSE)

cat("=== DATA LOADED SUCCESSFULLY ===\n")
cat("Dimensions:", nrow(data_raw), "rows ×", ncol(data_raw), "columns\n")
cat("Countries:", paste(unique(data_raw$country), collapse = ", "), "\n")
cat("Date range:", min(data_raw$date), "to", max(data_raw$date), "\n\n")
str(data_raw)
library(dplyr)

# SECTION 2: DATA PREPROCESSING

data_processed <- data_raw %>%
    # Convert date to a proper Date object (YYYY-MM format → first day of month)

    mutate(date = as.Date(paste0(date, "-01"))) %>%

    # Sort by country then by date so lags are computed correctly
    arrange(country, date) %>%

    # Compute key transformed variables
    group_by(country) %>%
    mutate(
       # Log-transform dependent variables (log-linearises the model)
       ln_cpi   = log(cpi),
       ln_ipi   = log(ipi),

       # Log-transform exchange rate (NEER)
       ln_neer = log(neer),

       # Monthly % change in Brent crude = the oil price shock variable
       # Formula: (P_t – P_{t-1}) / P_{t-1} × 100
       oil_pct_change = (brent - lag(brent)) / lag(brent) * 100,

       # Replace NA for first observation per country with 0
       oil_pct_change = ifelse(is.na(oil_pct_change), 0, oil_pct_change)
    ) %>%
    ungroup() %>%

    # Drop rows with any remaining NAs in modelling variables
    filter(!is.na(ln_cpi), !is.na(ln_ipi), !is.na(oil_pct_change))

cat("=== PREPROCESSING COMPLETE ===\n")
cat("Processed rows:", nrow(data_processed), "\n")
cat("Post-crisis observations:", sum(data_processed$post_crisis), "\n\n")

# Quick sanity check on the oil shock variable
cat("--- Oil Price Shock Summary ---\n")
print(summary(data_processed$oil_pct_change))
library(ggplot2)

# SECTION 3: EXPLORATORY DATA ANALYSIS (EDA)

#
crisis_date <- as.Date("2026-02-27")

fig1 <- ggplot(data_processed %>% distinct(date, brent),
                 aes(x = date, y = brent)) +
 geom_line(color = "darkred", linewidth = 1) +
 geom_vline(xintercept = as.numeric(crisis_date),
               linetype = "dashed", color = "steelblue", linewidth = 0.8) +
 annotate("text", x = crisis_date + 20, y = max(data_processed$brent) *
0.97,
            label = "Gulf Crisis\nFeb 27, 2026", hjust = 0, color =
"steelblue", size = 3.5) +
 scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
 labs(title = "Brent Crude Oil Price: 2023–2026",
        subtitle = "Sharp spike in March 2026 following the Gulf Crisis",
        x = "Date", y = "USD per Barrel") +
 theme_minimal(base_size = 12) +
 theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(fig1)

#
fig2 <- ggplot(data_processed %>% distinct(date, oil_pct_change),
                 aes(x = date, y = oil_pct_change,
                     fill = oil_pct_change > 0)) +
 geom_col(show.legend = FALSE) +
 geom_vline(xintercept = as.numeric(crisis_date),
               linetype = "dashed", color = "steelblue") +
 scale_fill_manual(values = c("TRUE" = "#2ecc71", "FALSE" = "#e74c3c")) +
 labs(title = "Monthly % Change in Brent Crude Oil Price",
        x = "Date", y = "% Change") +
 theme_minimal(base_size = 12) +
 theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(fig2)

#
fig3 <- ggplot(data_processed, aes(x = date, y = ln_cpi, color = group)) +
 stat_summary(fun = mean, geom = "line", linewidth = 1) +
 geom_vline(xintercept = as.numeric(crisis_date),
              linetype = "dashed", color = "black") +
 labs(title = "Average Log(CPI) by Country Group",
       subtitle = "Dashed line marks the start of the 2026 Gulf Crisis",
       x = "Date", y = "Log(CPI)", color = "Country Group") +
 theme_minimal(base_size = 12) +
 theme(axis.text.x   = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

print(fig3)

#
fig4 <- ggplot(data_processed, aes(x = date, y = ln_ipi, color = group)) +
 stat_summary(fun = mean, geom = "line", linewidth = 1) +
 geom_vline(xintercept = as.numeric(crisis_date),
              linetype = "dashed", color = "black") +
 labs(title = "Average Log(IPI) by Country Group",
       subtitle = "Industrial production response around the crisis",
       x = "Date", y = "Log(IPI)", color = "Country Group") +
 theme_minimal(base_size = 12) +
 theme(axis.text.x   = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

print(fig4)

#
fig5 <- ggplot(data_processed %>% distinct(date, vix),
                aes(x = date, y = vix)) +
 geom_line(color = "purple", linewidth = 1) +
 geom_vline(xintercept = as.numeric(crisis_date),
              linetype = "dashed", color = "steelblue") +

    labs(title = "CBOE VIX – Global Financial Uncertainty",
         x = "Date", y = "VIX Level") +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(fig5)

install.packages("plm")
library(plm)

# SECTION 4: DECLARE PANEL DATA STRUCTURE

pdata <- pdata.frame(data_processed,
                       index = c("country", "date"))

cat("=== PANEL DATA STRUCTURE ===\n")
cat("Individuals (countries):", length(unique(pdata$country)), "\n")
cat("Time periods:", length(unique(pdata$date)), "\n\n")
install.packages("stargazer")
library(stargazer)

# SECTION 5: PANEL DATA REGRESSION MODELS
# We estimate three specifications for each dependent variable:
#      (1) Pooled OLS – ignores panel structure
#      (2) Fixed Effects (FE / Within) – absorbs country-level heterogeneity
#      (3) Random Effects (RE) – treats country effects as random draws

#

cat("===============================================\n")
cat("     INFLATION MODELS – Dependent Variable: ln_CPI\n")
cat("===============================================\n\n")

# Model 1a: Pooled OLS – CPI
pols_cpi <- plm(ln_cpi ~ oil_pct_change + ln_neer + policy_rate + vix +
post_crisis,
                  data    = pdata,
                  model = "pooling")

# Model 2a: Fixed Effects – CPI
fe_cpi <- plm(ln_cpi ~ oil_pct_change + ln_neer + policy_rate + vix +
post_crisis,
                 data    = pdata,
                 model   = "within",      # 'within' = Fixed Effects estimator
                 effect = "individual")

# Model 3a: Random Effects – CPI
re_cpi <- plm(ln_cpi ~ oil_pct_change + ln_neer + policy_rate + vix +
post_crisis,
                 data    = pdata,
                 model   = "random",
                 effect = "individual")

# Display results
stargazer(pols_cpi, fe_cpi, re_cpi,
          type             = "text",
          title            = "Table 1: Inflation Models (Dependent: ln_CPI)",
          column.labels = c("Pooled OLS", "Fixed Effects", "Random Effects"),
          dep.var.labels = "ln(CPI)",
          covariate.labels = c("Oil Price Shock (%)",
                                     "ln(NEER)",
                                     "Policy Rate",
                                     "VIX",
                                     "Post-Crisis Dummy"),
          keep.stat        = c("n", "rsq", "adj.rsq"),
          notes            = "Robust SE reported separately. *** p<0.01, ** p<0.05, * p<0.1")

#

cat("\n===============================================================\n")
cat("   GROWTH MODELS – Dependent Variable: ln_IPI (Industrial Prod.)\n")
cat("===============================================================\n\n")

# Model 1b: Pooled OLS – IPI
pols_ipi <- plm(ln_ipi ~ oil_pct_change + ln_neer + policy_rate + vix +
post_crisis,
                  data    = pdata,
                  model = "pooling")

# Model 2b: Fixed Effects – IPI
fe_ipi <- plm(ln_ipi ~ oil_pct_change + ln_neer + policy_rate + vix +
post_crisis,
                 data    = pdata,
                 model   = "within",
                 effect = "individual")

# Model 3b: Random Effects – IPI
re_ipi <- plm(ln_ipi ~ oil_pct_change + ln_neer + policy_rate + vix +
post_crisis,
                 data    = pdata,
                 model   = "random",
                 effect = "individual")

stargazer(pols_ipi, fe_ipi, re_ipi,
          type             = "text",
          title            = "Table 2: Industrial Production Models (Dependent: ln_IPI)",
          column.labels = c("Pooled OLS", "Fixed Effects", "Random Effects"),
          dep.var.labels = "ln(IPI)",
          covariate.labels = c("Oil Price Shock (%)",
                                     "ln(NEER)",
                                     "Policy Rate",
                                     "VIX",
                                     "Post-Crisis Dummy"),
          keep.stat        = c("n", "rsq", "adj.rsq"),
          notes            = "Robust SE reported separately. *** p<0.01, ** p<0.05, * p<0.1")
# SECTION 6: DIAGNOSTIC TESTS

cat("\n==============================================\n")
cat("     SECTION 6: DIAGNOSTIC TESTS\n")
cat("==============================================\n\n")

#
# H0: Random Effects is consistent and efficient (RE preferred)
# H1: FE is consistent, RE is not (FE preferred)
# Rule: p < 0.05 → reject H0 → use Fixed Effects

cat("--- Hausman Test: CPI Model ---\n")
hausman_cpi <- phtest(fe_cpi, re_cpi)
print(hausman_cpi)
cat("Interpretation:",
       ifelse(hausman_cpi$p.value < 0.05,
             "p < 0.05 → REJECT H0 → Fixed Effects preferred over Random Effects.\n",
             "p ≥ 0.05 → FAIL to reject H0 → Random Effects is acceptable.\n"), "\n")

cat("--- Hausman Test: IPI Model ---\n")
hausman_ipi <- phtest(fe_ipi, re_ipi)
print(hausman_ipi)
cat("Interpretation:",
       ifelse(hausman_ipi$p.value < 0.05,
             "p < 0.05 → REJECT H0 → Fixed Effects preferred over Random Effects.\n",
             "p ≥ 0.05 → FAIL to reject H0 → Random Effects is acceptable.\n"), "\n")

#
# H0: Pooled OLS is sufficient (no individual-level random effects)
# H1: Random Effects model is needed
# Rule: p < 0.05 → reject H0 → panel effects exist → RE better than POLS

cat("--- Breusch-Pagan LM Test: CPI Model ---\n")
bp_cpi <- plmtest(pols_cpi, effect = "individual", type = "bp")
print(bp_cpi)
cat("Interpretation:",
       ifelse(bp_cpi$p.value < 0.05,
             "p < 0.05 → Significant individual effects → RE better than Pooled OLS.\n",
             "p ≥ 0.05 → No significant individual effects → Pooled OLS acceptable.\n"), "\n")

cat("--- Breusch-Pagan LM Test: IPI Model ---\n")
bp_ipi <- plmtest(pols_ipi, effect = "individual", type = "bp")
print(bp_ipi)
cat("Interpretation:",
       ifelse(bp_ipi$p.value < 0.05,
             "p < 0.05 → Significant individual effects → RE better than Pooled OLS.\n",
             "p ≥ 0.05 → No significant individual effects → Pooled OLS acceptable.\n"), "\n")

#
# Tests whether the country-specific intercepts are jointly significant
cat("--- F-test for Fixed Effects: CPI Model ---\n")
pFtest_cpi <- pFtest(fe_cpi, pols_cpi)
print(pFtest_cpi)

cat("--- F-test for Fixed Effects: IPI Model ---\n")
pFtest_ipi <- pFtest(fe_ipi, pols_ipi)
print(pFtest_ipi)

# SECTION 8: HETEROGENEITY ANALYSIS – IMPORTERS vs EXPORTERS

cat("\n==============================================\n")
cat("     SECTION 8: HETEROGENEITY ANALYSIS\n")

cat("==============================================\n\n")

# Split the panel by trade position
importers_data <- pdata %>% filter(oil_balance == "importer")
exporters_data <- pdata %>% filter(oil_balance == "exporter")

cat("Importer countries:", paste(unique(importers_data$country), collapse
= ", "), "\n")
cat("Exporter countries:", paste(unique(exporters_data$country), collapse
= ", "), "\n\n")

#
fe_importers_cpi <- plm(ln_cpi ~ oil_pct_change + ln_neer + policy_rate +
vix + post_crisis,
                       data = importers_data, model = "within")

fe_exporters_cpi <- plm(ln_cpi ~ oil_pct_change + ln_neer + policy_rate +
vix + post_crisis,
                       data = exporters_data, model = "within")

stargazer(fe_importers_cpi, fe_exporters_cpi,
          type         = "text",
          title        = "Table 3: Heterogeneity – CPI (Importers vs Exporters)",
          column.labels = c("Oil Importers", "Oil Exporters"),
          dep.var.labels = "ln(CPI)",
          covariate.labels = c("Oil Price Shock (%)", "ln(NEER)",
                               "Policy Rate", "VIX", "Post-Crisis Dummy"),
          keep.stat    = c("n", "rsq"))

#
fe_importers_ipi <- plm(ln_ipi ~ oil_pct_change + ln_neer + policy_rate +
vix + post_crisis,
                       data = importers_data, model = "within")

fe_exporters_ipi <- plm(ln_ipi ~ oil_pct_change + ln_neer + policy_rate +
vix + post_crisis,
                       data = exporters_data, model = "within")

stargazer(fe_importers_ipi, fe_exporters_ipi,
          type            = "text",
          title           = "Table 4: Heterogeneity – IPI (Importers vs Exporters)",
          column.labels = c("Oil Importers", "Oil Exporters"),
          dep.var.labels = "ln(IPI)",
          covariate.labels = c("Oil Price Shock (%)", "ln(NEER)",
                                 "Policy Rate", "VIX", "Post-Crisis Dummy"),
          keep.stat       = c("n", "rsq"))

#
advanced_data     <- pdata %>% filter(grepl("Advanced",   group))
emerging_data     <- pdata %>% filter(grepl("Emerging",   group))

fe_advanced_cpi <- plm(ln_cpi ~ oil_pct_change + ln_neer + policy_rate +
vix + post_crisis,
                          data = advanced_data, model = "within")

fe_emerging_cpi <- plm(ln_cpi ~ oil_pct_change + ln_neer + policy_rate +
vix + post_crisis,
                          data = emerging_data, model = "within")

stargazer(fe_advanced_cpi, fe_emerging_cpi,
          type            = "text",
          title           = "Table 5: Heterogeneity – CPI (Advanced vs Emerging)",
          column.labels = c("Advanced Economies", "Emerging Economies"),
          dep.var.labels = "ln(CPI)",
          covariate.labels = c("Oil Price Shock (%)", "ln(NEER)",
                                 "Policy Rate", "VIX", "Post-Crisis Dummy"),
          keep.stat       = c("n", "rsq"))

library(lmtest)
library(sandwich)

# SECTION 9: VISUALISATION – COEFFICIENT PLOT

# Extract coefficients and confidence intervals for the preferred FE model (CPI)

fe_cpi_vcov      <- vcovHC(fe_cpi, method = "arellano", type = "HC3")
# Get the coeftest result (which is a matrix)
ct_result_matrix <- coeftest(fe_cpi, vcov = fe_cpi_vcov)

# Create the data frame explicitly using column indices for robustness
fe_cpi_ci <- data.frame(
    Estimate     = ct_result_matrix[, 1], # Assuming 'Estimate' is the first column
    `Std. Error` = ct_result_matrix[, 2], # Assuming 'Std. Error' is the second column
    variable     = rownames(ct_result_matrix),
    check.names = FALSE # Important for column names like 'Std. Error'
)

fe_cpi_ci$lower95      <- fe_cpi_ci$Estimate - 1.96 * fe_cpi_ci$`Std. Error`
fe_cpi_ci$upper95      <- fe_cpi_ci$Estimate + 1.96 * fe_cpi_ci$`Std. Error`

# Adjust labels to match the 4 estimated coefficients
fe_cpi_ci$label        <- c("ln(NEER)",
                             "Policy Rate", "VIX", "Post-Crisis Dummy")

fig6 <- ggplot(fe_cpi_ci, aes(x = label, y = Estimate,
                                   ymin = lower95, ymax = upper95,
                                   color = Estimate > 0)) +
    geom_pointrange(size = 0.8) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
    coord_flip() +
    scale_color_manual(values = c("TRUE" = "#2ecc71", "FALSE" = "#e74c3c"),
                        guide   = "none") +
    labs(title      = "Figure 6: Fixed Effects Coefficient Plot – ln(CPI) Model",
         subtitle = "Point estimates with 95% confidence intervals (Robust SE)",
         x = NULL, y = "Coefficient Estimate") +

    theme_minimal(base_size = 12)

print(fig6)

#
fig7 <- ggplot(data_processed,
                   aes(x = group, y = oil_pct_change, fill = group)) +
    geom_boxplot(show.legend = FALSE, outlier.colour = "red") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = "Figure 7: Oil Price Shock Distribution by Country Group",
          x = "Country Group", y = "Monthly % Change in Brent") +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 20, hjust = 1))

print(fig7)

# SECTION 10: SUMMARY TABLE OF KEY RESULTS

cat("\n===============================================================\n")
cat("      SECTION 10: CONSOLIDATED RESULTS SUMMARY\n")
cat("===============================================================\n\n")

# Helper function to extract coefficient + p-value cleanly
extract_coef <- function(model, var, vcov_matrix = NULL) {
    if (is.null(vcov_matrix)) {
        ct <- coeftest(model)
    } else {
        ct <- coeftest(model, vcov = vcov_matrix)
    }

    # Check if the variable exists in the coefficient test output
    if (!(var %in% rownames(ct))) {
        return("N/A") # Return N/A if coefficient is not estimated
    }

    row <- ct[var, ]
    stars <- ifelse(row[4] < 0.01, "***",
               ifelse(row[4] < 0.05, "**",
               ifelse(row[4] < 0.10, "*", "")))
    sprintf("%.5f%s (p=%.3f)", row[1], stars, row[4])
}

vcov_fe_cpi <- vcovHC(fe_cpi, method = "arellano", type = "HC3")
vcov_fe_ipi <- vcovHC(fe_ipi, method = "arellano", type = "HC3")

results_summary <- data.frame(
    Model = c("POLS – CPI", "FE – CPI", "RE – CPI",
                "POLS – IPI", "FE – IPI", "RE – IPI"),
    Oil_Shock_Coef = c(
         extract_coef(pols_cpi, "oil_pct_change"),
         extract_coef(fe_cpi,   "oil_pct_change", vcov_fe_cpi),
         extract_coef(re_cpi,   "oil_pct_change"),
         extract_coef(pols_ipi, "oil_pct_change"),
         extract_coef(fe_ipi,   "oil_pct_change", vcov_fe_ipi),
         extract_coef(re_ipi,   "oil_pct_change")
    ),
    Post_Crisis_Coef = c(
         extract_coef(pols_cpi, "post_crisis"),
         extract_coef(fe_cpi,   "post_crisis", vcov_fe_cpi),
         extract_coef(re_cpi,   "post_crisis"),
         extract_coef(pols_ipi, "post_crisis"),
         extract_coef(fe_ipi,   "post_crisis", vcov_fe_ipi),
         extract_coef(re_ipi,   "post_crisis")
    )
)

print(results_summary, row.names = FALSE)
