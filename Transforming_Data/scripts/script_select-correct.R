# ============================================================
# Title:   Penguin Analysis
# Author:  [your name]
# Description: Comparing body mass across species and islands.
# ============================================================

# --- Question: What is the average body mass per species
#               and island? ---

# Use select() to keep only species, island, and body_mass,
# then compute average body mass per species and island.
mass_by_species_island <- penguins |>
  select(species, island, body_mass) |>
  group_by(species, island) |>
  summarise(avg_body_mass = mean(body_mass, na.rm = TRUE))

print(mass_by_species_island)
