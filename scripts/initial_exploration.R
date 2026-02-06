# Week 1: Initial Data Exploration ====
# Author: [Joel]
# Date: [06-02-2026]

# Load packages ====
library(tidyverse)
library(here)
library(naniar)
library(janitor)
library(skimr)
# Load data ====
mosquito_egg_raw <- read_csv(here("data", "mosquito_egg_data.csv"),
                             name_repair = janitor::make_clean_names)

# Basic overview ====
glimpse(mosquito_egg_raw)
summary(mosquito_egg_raw)
skim(mosquito_egg_raw)

# React table====
# view interactive table of data
view(mosquito_egg_raw)


# Counts by site and treatment====

mosquito_egg_raw |> 
  group_by(site, treatment) |> 
  summarise(n = n())

# Observations ====
# Your observations (add as comments below):
# - What biological system is this?
#   mosquito
# - What's being measured?
#   body mass mg
# - How many observations?
#   205
# - Anything surprising?
#   body mass is negative
# - Any obvious problems?
#   some values are missing



# FIX 1: [fixing negative values of body mass] ====

# Show the problem:
# [Code to demonstrate issue exists]
mosquito_egg_raw |> 
  filter(body_mass_mg <= 0)


# Fix it:
mosquito_egg_data_step1 <- mosquito_egg_raw |>
  # YOUR CODE HERE
  # Check for zero or negative values where zero doesn't make biological sense
  mosquito_egg_data_step1 <- mosquito_egg_raw |>
  mutate(
    body_mass_mg = if_else(body_mass_mg <= 0, NA_real_, body_mass_mg)
  )

  
  # Verify it worked:
  # [Code to check change happened]
  mosquito_egg_data_step1 |>
    filter(body_mass_mg <= 0)
  
  # What changed and why it matters:
  # [2-3 sentences explaining consequences]
  #the negative values of the body_mass changed, this is important as body mass 
  #can not be less than or equal to 0
  
  # FIX 2: [there are inconsistent capital letters throughout the data]  ====

# Show the problem:
# [Code]
  mosquito_egg_raw |>
    count(treatment)

# Fix it:
mosquito_egg_data_step2 <- mosquito_egg_data_step1 |>
  # YOUR CODE
  mosquito_egg_data_step2 <- mosquito_egg_data_step1 |>
  mutate(
    treatment = str_to_lower(treatment)
  )
  
  # Verify it worked:
  # [Code]
mosquito_egg_data_step2 |>
  count(treatment)
  
  # What changed and why it matters:
  # [2-3 sentences]
  # the treatment shows up to be lower case, and instead of a tibble of 12x2, 
#it has gone to 4x2 of meaningful treatment names.