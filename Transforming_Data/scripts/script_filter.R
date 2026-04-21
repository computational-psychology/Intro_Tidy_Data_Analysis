# ============================================================
# Title:   Penguin Analysis
# Author:  [your name]
# Description: Exploring penguin body mass by island and species.
# ============================================================

# --- Question: Do female Gentoo penguins on different islands
#               differ in average body mass? ---

# Filter for female Gentoo penguins, then compute average body
# mass per island.
female_gentoo_mass <- penguins |>
  filter(...) |>
  group_by(island) |>
  summarise(avg_body_mass = mean(body_mass, na.rm = TRUE))

print(female_gentoo_mass)
