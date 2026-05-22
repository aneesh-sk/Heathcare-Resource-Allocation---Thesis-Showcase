# Install and load required packages
if(!require(readxl)) install.packages("readxl")
if(!require(Benchmarking)) install.packages("Benchmarking")
if(!require(dplyr)) install.packages("dplyr")
if(!require(ggplot2)) install.packages("ggplot2")

library(readxl)
library(Benchmarking)
library(dplyr)
library(ggplot2)

# Step 1: Load the data
data <- read_excel("ERIC_DATA_CLEANED.xlsx", sheet = "Data Cleaned")

# Step 2: Prepare input and output variables (ensure numeric and handle missing)
data <- data %>%
  mutate(
    cap_new_build = as.numeric(`Capital investment for new build (£)`),
    cap_improve = as.numeric(`Capital investment for changing/improving existing buildings (£)`),
    cap_maintain = as.numeric(`Capital investment for maintaining (lifecycle) existing buildings (£)`),
    cap_equipment = as.numeric(`Capital investment for equipment (£)`),
    energy_costs = as.numeric(`Energy efficient schemes costs (£) ( Cleaned )`),
    total_contrib = as.numeric(`Total contributions (£)`),
    waste_savings = as.numeric(`Waste re-use scheme - Cost savings (£) ( Cleaned)`),
    car_park_income = as.numeric(`Income from car parking - patients and visitors (£) (Cleaned)`)
  ) %>%
  mutate(across(
    c(cap_new_build, cap_improve, cap_maintain, cap_equipment, energy_costs, 
      total_contrib, waste_savings, car_park_income),
    ~ifelse(is.na(.), 0, .)
  ))

# Step 3: Prepare input and output matrices for DEA
inputs <- as.matrix(data %>% select(cap_new_build, cap_improve, cap_maintain, cap_equipment, energy_costs))
outputs <- as.matrix(data %>% select(total_contrib, waste_savings, car_park_income))

# DEA requires non-negative outputs, so shift if needed
min_contrib <- min(outputs[, 1])
if(min_contrib < 0) {
  outputs[, 1] <- outputs[, 1] - min_contrib + 1
}

# Step 4: Run DEA (VRS and CRS)
dea_vrs <- dea(inputs, outputs, RTS = "vrs")
dea_crs <- dea(inputs, outputs, RTS = "crs")
data$efficiency_vrs <- dea_vrs$eff
data$efficiency_crs <- dea_crs$eff
data$scale_efficiency <- data$efficiency_crs / data$efficiency_vrs

# Step 5: Analyze and output results
ranked_trusts <- data %>%
  select(`Trust Code`, `Trust Name`, efficiency_vrs, efficiency_crs, scale_efficiency) %>%
  arrange(desc(efficiency_vrs))
print(head(ranked_trusts, 10))  # Top 10
print(tail(ranked_trusts, 10))  # Bottom 10

# Step 6: Visualize efficiency distribution
ggplot(data, aes(x = efficiency_vrs)) +
  geom_histogram(bins = 10, fill = "steelblue", color = "white") +
  labs(title = "Distribution of DEA Efficiency Scores",
       x = "Efficiency Score (VRS)",
       y = "Number of Trusts") +
  theme_minimal()

# Step 7: Resource allocation recommendations
recommendations <- data %>%
  select(`Trust Code`, `Trust Name`, efficiency_vrs) %>%
  mutate(
    efficiency_category = case_when(
      efficiency_vrs == 1 ~ "Efficient",
      efficiency_vrs >= 0.8 ~ "High efficiency",
      efficiency_vrs >= 0.5 ~ "Medium efficiency",
      TRUE ~ "Low efficiency"
    ),
    recommendation = case_when(
      efficiency_vrs == 1 ~ "Maintain current resource allocation",
      efficiency_vrs >= 0.8 ~ "Minor resource adjustments needed",
      efficiency_vrs >= 0.5 ~ "Moderate resource reallocation recommended",
      TRUE ~ "Significant resource reallocation required"
    )
  ) %>%
  arrange(efficiency_vrs)

print(head(recommendations, 10))
write.csv(recommendations, "DEA_Resource_Allocation_Recommendations.csv", row.names = FALSE)

# Step 8: Identify improvement targets for the least efficient Trust
least_efficient_index <- which.min(data$efficiency_vrs)
cat("Least efficient Trust:", data$`Trust Name`[least_efficient_index], "\n")
cat("Efficiency score:", data$efficiency_vrs[least_efficient_index], "\n")
cat("Current inputs:\n")
print(inputs[least_efficient_index, ])
cat("Current outputs:\n")
print(outputs[least_efficient_index, ])

# Step 9 : Calculate profit/loss for each trust
profitloss <- data %>%
  mutate(
    total_revenue = abs(total_contrib) + abs(waste_savings) + abs(car_park_income),
    total_investment = cap_new_build + cap_improve + cap_maintain + cap_equipment + energy_costs,
    net_profit_loss = total_revenue - total_investment
  )

profit_loss_table <- profitloss %>%
  select(`Trust Code`, `Trust Name`, efficiency_vrs, efficiency_crs, scale_efficiency,
         total_revenue, total_investment, net_profit_loss) %>%
  arrange(net_profit_loss) # Sort by profit/loss if desired

print(head(profit_loss_table, 10))
write.csv(profit_loss_table, "NHS_Trust_Profit_Loss_Table.csv", row.names = FALSE)

# -------------------------------------------------
# Step 10 - Weighted DEA (custom weights) and ADD TO DATASET
# -------------------------------------------------
# Define and apply custom weights per justification
input_weights <- c(2, 1, 1, 1, 1)  # Double weight for 'cap_new_build'
output_weights <- c(2, 1, 1)        # Double weight for 'total_contrib'

# Weighted input/output matrices
inputs_weighted <- sweep(inputs, 2, input_weights, `*`)
outputs_weighted <- sweep(outputs, 2, output_weights, `*`)

# Insert weighted measure columns into data for transparency
data$w_cap_new_build   <- data$cap_new_build   * input_weights[1]
data$w_cap_improve     <- data$cap_improve     * input_weights[2]
data$w_cap_maintain    <- data$cap_maintain    * input_weights[3]
data$w_cap_equipment   <- data$cap_equipment   * input_weights[4]
data$w_energy_costs    <- data$energy_costs    * input_weights[5]
data$w_total_contrib   <- data$total_contrib   * output_weights[1]
data$w_waste_savings   <- data$waste_savings   * output_weights[2]
data$w_car_park_income <- data$car_park_income * output_weights[3]

# Run weighted DEA (VRS)
dea_vrs_weighted <- dea(inputs_weighted, outputs_weighted, RTS = "vrs")
data$efficiency_vrs_weighted <- dea_vrs_weighted$eff

# Step 10b: Compare weighted vs unweighted; also impact of weighting
comparison_table <- data %>%
  select(`Trust Code`, `Trust Name`, efficiency_vrs, efficiency_vrs_weighted) %>%
  mutate(impact_of_weighting = efficiency_vrs_weighted - efficiency_vrs) %>%
  arrange(desc(efficiency_vrs_weighted))

print(head(comparison_table, 10))
print("Trusts with most positive impact from weighting:")
print(head(arrange(comparison_table, desc(impact_of_weighting)), 5))
print("Trusts with most negative impact from weighting:")
print(head(arrange(comparison_table, impact_of_weighting), 5))

write.csv(comparison_table, "DEA_Weighted_vs_Unweighted.csv", row.names = FALSE)

# -------------------------------------------------
# Step 11 - Correlation Analysis (critical factors)
# -------------------------------------------------
cor_results <- sapply(
  data %>% select(cap_new_build, cap_improve, cap_maintain, cap_equipment, energy_costs, 
                  total_contrib, waste_savings, car_park_income),
  function(x) cor(x, data$efficiency_vrs, use = "complete.obs")
)
print("Correlation of inputs/outputs with VRS efficiency:")
print(round(cor_results, 3))

# -------------------------------------------------
# Step 12 - Filter for Low Efficiency Trusts
# -------------------------------------------------
low_efficiency_trusts <- recommendations %>%
  filter(efficiency_category == "Low efficiency")
print("Trusts with Low Efficiency (VRS):")
print(low_efficiency_trusts)
write.csv(low_efficiency_trusts, "Low_Efficiency_Trusts.csv", row.names = FALSE)

# Bar plot for trusts by efficiency category
ggplot(recommendations, aes(x = efficiency_category, fill = efficiency_category)) +
  geom_bar() +
  labs(title = "Trust Count by Efficiency Category", x = "Efficiency Category", y = "Count") +
  scale_fill_brewer(palette = "Set3", guide = FALSE) +
  theme_minimal()

# -------------------------------------------------
# Step 13 - Additional: Regional breakdown
# -------------------------------------------------
if("Commissioning Region" %in% names(data)) {
  regional_summary <- data %>%
    group_by(`Commissioning Region`) %>%
    summarise(
      avg_eff_vrs = mean(efficiency_vrs, na.rm = TRUE),
      avg_eff_crs = mean(efficiency_crs, na.rm = TRUE),
      avg_scale_eff = mean(scale_efficiency, na.rm = TRUE)
    ) %>% arrange(desc(avg_eff_vrs))
  print(regional_summary)
}

# -------------------------------------------------
# Step 14 - Additional visualization: Efficiency vs Profit/Loss
# -------------------------------------------------
ggplot(profit_loss_table, aes(x = efficiency_vrs, y = net_profit_loss)) +
  geom_point(color = "coral") +
  labs(title = "Efficiency vs. Net Profit/Loss",
       x = "DEA Efficiency (VRS)",
       y = "Net Profit/Loss") +
  theme_minimal()

# -------------------------------------------------
# Step 15 - Advanced: Pairwise plot of main efficiency factors
# -------------------------------------------------
pairs(~ efficiency_vrs + cap_new_build + cap_improve + cap_maintain + cap_equipment + energy_costs, data = data)
