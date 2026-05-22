# Install and load required packages
if(!require(readxl)) install.packages("readxl")
if(!require(Benchmarking)) install.packages("Benchmarking")
if(!require(dplyr)) install.packages("dplyr")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(MCDA)) install.packages("MCDA") # For MCDA methods

library(readxl)
library(Benchmarking)
library(dplyr)
library(ggplot2)
library(MCDA)
library(readr)

# Step 1: Load data
data <- read_excel("ERIC_DATA_CLEANED.xlsx", sheet = "Data Cleaned")

# Step 2: Prepare variables (numeric, handle missing)
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

# Step 3: DEA inputs and outputs
inputs <- as.matrix(data %>% select(cap_new_build, cap_improve, cap_maintain, cap_equipment, energy_costs))
outputs <- as.matrix(data %>% select(total_contrib, waste_savings, car_park_income))

min_contrib <- min(outputs[, 1])
if(min_contrib < 0) {
  outputs[, 1] <- outputs[, 1] - min_contrib + 1
}

# Step 4: Run DEA (VRS, CRS)
dea_vrs <- dea(inputs, outputs, RTS = "vrs")
dea_crs <- dea(inputs, outputs, RTS = "crs")
data$efficiency_vrs <- dea_vrs$eff
data$efficiency_crs <- dea_crs$eff
data$scale_efficiency <- data$efficiency_crs / data$efficiency_vrs

# Step 5: Output results and ranking
ranked_trusts <- data %>%
  select(`Trust Code`, `Trust Name`, efficiency_vrs, efficiency_crs, scale_efficiency) %>%
  arrange(desc(efficiency_vrs))
print(head(ranked_trusts, 10))  # Top 10
print(tail(ranked_trusts, 10))  # Bottom 10

# Step 6: Efficiency distribution plot
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

# Step 8: Least efficient Trust diagnostic
least_efficient_index <- which.min(data$efficiency_vrs)
cat("Least efficient Trust:", data$`Trust Name`[least_efficient_index], "\n")
cat("Efficiency score:", data$efficiency_vrs[least_efficient_index], "\n")
cat("Current inputs:\n")
print(inputs[least_efficient_index, ])
cat("Current outputs:\n")
print(outputs[least_efficient_index, ])

# Step 9 : Calculate profit/loss 
profitloss <- data %>%
  mutate(
    total_revenue = abs(total_contrib) + abs(waste_savings) + abs(car_park_income),
    total_investment = cap_new_build + cap_improve + cap_maintain + cap_equipment + energy_costs,
    net_profit_loss = total_revenue - total_investment
  )

profit_loss_table <- profitloss %>%
  select(`Trust Code`, `Trust Name`, efficiency_vrs, efficiency_crs, scale_efficiency,
         total_revenue, total_investment, net_profit_loss) %>%
  arrange(net_profit_loss)

print(head(profit_loss_table, 10))
write.csv(profit_loss_table, "NHS_Trust_Profit_Loss_Table.csv", row.names = FALSE)

# Step 10 - Weighted DEA (emphasize strategic priorities)
input_weights <- c(2, 1, 1, 1, 1)
output_weights <- c(2, 1, 1)
inputs_weighted <- sweep(inputs, 2, input_weights, `*`)
outputs_weighted <- sweep(outputs, 2, output_weights, `*`)
dea_vrs_weighted <- dea(inputs_weighted, outputs_weighted, RTS = "vrs")
data$efficiency_vrs_weighted <- dea_vrs_weighted$eff

# Step 11 - Weighted vs unweighted comparison
comparison_table <- data %>%
  select(`Trust Code`, `Trust Name`, efficiency_vrs, efficiency_vrs_weighted) %>%
  arrange(desc(efficiency_vrs_weighted))
print(head(comparison_table, 10))

# Step 12 - Correlation Analysis 
cor_results <- sapply(
  data %>% select(cap_new_build, cap_improve, cap_maintain, cap_equipment, energy_costs, 
                  total_contrib, waste_savings, car_park_income),
  function(x) cor(x, data$efficiency_vrs, use = "complete.obs")
)
print("Correlation of inputs/outputs with VRS efficiency:")
print(round(cor_results, 3))

# Step 13 - Filter for low efficiency trusts 
low_efficiency_trusts <- recommendations %>%
  filter(efficiency_category == "Low efficiency")
print("Trusts with Low Efficiency (VRS):")
print(low_efficiency_trusts)
write.csv(low_efficiency_trusts, "Low_Efficiency_Trusts.csv", row.names = FALSE)

# Step 14: Bar plot by efficiency category
ggplot(recommendations, aes(x = efficiency_category, fill = efficiency_category)) +
  geom_bar() +
  labs(title = "Trust Count by Efficiency Category", x = "Efficiency Category", y = "Count") +
  scale_fill_brewer(palette = "Set3", guide = FALSE) +
  theme_minimal()

#======= MCDA Integration Section

# Normalization function for MCDA (0-1 range)
normalize <- function(x) { (x - min(x, na.rm=TRUE)) / (max(x, na.rm=TRUE) - min(x, na.rm=TRUE)) }

# Example: Combine DEA and financial criteria for MCDA
mcda_data <- data.frame(
  Trust = data$`Trust Name`,
  Efficiency = normalize(data$efficiency_vrs),
  ProfitLoss = normalize(profit_loss_table$net_profit_loss),
  ScaleEfficiency = normalize(data$scale_efficiency)
)

# Transparent, user-documented weights (change via stakeholder feedback/sensitivity analysis)
weights <- c(Efficiency = 0.5, ProfitLoss = 0.3, ScaleEfficiency = 0.2) # Example

# Weighted MCDA Score calculation
mcda_data$WeightedScore <- with(mcda_data,
                                Efficiency * weig
                                hts['Efficiency'] +
                                  ProfitLoss * weights['ProfitLoss'] +
                                  ScaleEfficiency * weights['ScaleEfficiency']
)

# Rank trusts on composite MCDA score
mcda_data <- mcda_data[order(-mcda_data$WeightedScore), ]
print(head(mcda_data, 10))

write.csv(mcda_data, "MCDA_Trust_Ranking.csv", row.names=FALSE)


# Scatter plot: DEA Efficiency vs MCDA Weighted Score
ggplot(mcda_data, aes(x = Efficiency, y = WeightedScore)) +
  geom_point(alpha = 0.6, color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "DEA Efficiency vs MCDA Weighted Score",
       x = "DEA Efficiency",
       y = "MCDA Weighted Score") +
  theme_minimal()
