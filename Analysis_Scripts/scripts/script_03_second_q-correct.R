# ============================================================
# Title:   Penguin Analysis
# Author:  [your name]
# Description: Exploring body size and bill dimensions in the
#              Palmer Station penguin dataset.
# ============================================================

# --- Question: How does average body mass differ by species and sex? ---
# Compute average body mass per species and sex.
# Save the result to a variable called mass_by_species_sex.
mass_by_species_sex <- penguins |>
  group_by(species, sex) |>
  summarise(avg_body_mass = mean(body_mass, na.rm = TRUE))

print(mass_by_species_sex)

# --- Question: What is the range (minimum and maximum) of bill length per island? ---
# Compute the minimum and maximum bill length per island.
# Save the result to a variable called bill_len_by_island.
bill_len_by_island <- penguins |>
  group_by(island) |>
  summarise(
    min_bill_len = min(bill_len, na.rm = TRUE),
    max_bill_len = max(bill_len, na.rm = TRUE)
  )

print(bill_len_by_island)
