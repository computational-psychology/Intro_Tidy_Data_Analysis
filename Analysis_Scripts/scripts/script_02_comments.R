# ============================================================
# Title:   Penguin Analysis
# Author:  [your name]
# Description: Exploring body size and bill dimensions in the
#              Palmer Station penguin dataset.
# ============================================================

# --- What is the average body mass of the penguins? ---
penguins |>
  summarise(avg_body_mass = mean(body_mass, na.rm = TRUE)) |>
  print()
