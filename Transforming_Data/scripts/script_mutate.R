# ============================================================
# Title:   Penguin Analysis
# Author:  [your name]
# Description: Average penguin body mass per species in kg.
# ============================================================

# --- Question: What is the average body mass in kilograms
#               for each species and sex? ---

# Add a body_mass_kg column (body_mass / 1000), then compute
# average kg per species and sex.
avg_mass_kg <- penguins |>
  mutate(...) |>
  group_by(species, sex) |>
  summarise(avg_body_mass_kg = mean(body_mass_kg, na.rm = TRUE))

print(avg_mass_kg)
