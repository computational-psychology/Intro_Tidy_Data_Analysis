# ============================================================
# Title:   Penguin Analysis
# Author:  [your name]
# Description: Bill length of large penguins by species and sex.
# ============================================================

# --- Question: Among large penguins (body mass > 4500g),
#               what is their average bill length and body mass
#               (in kg) per species and sex? ---

# Step by step:
# 1. filter() for penguins with body_mass > 4500
# 2. mutate() to add body_mass_kg
# 3. select() to keep only the columns you need:
#    species, sex, bill_len, body_mass_kg
# 4. group_by() species and sex
# 5. summarise() average bill length and average body mass in kg

large_penguin_summary <- penguins |>
  filter(...) |>
  mutate(...) |>
  select(...) |>
  group_by(species, sex) |>
  summarise(
    avg_bill_len = mean(bill_len, na.rm = TRUE),
    avg_body_mass_kg = mean(body_mass_kg, na.rm = TRUE)
  )

print(large_penguin_summary)
