---
title: "Independent Project Introduction"
author: "David Nelson"
date: "`r Sys.Date()`"
output:
  pdf_document: default
  html_document: default
---


```{r}
# loading (and generating) our dataset

library(tidyverse)
library(dplyr)
library(lme4)       # for mixed-effects models like GLMMs
library(ggplot2)    # for visualization

set.seed(20250428)

# Vectors
birdspecies <- c("WESA","LESA","MAGO", "WILL", "LBCU","SEPL", "BBPL", "DUNL")
Focal_IDs <- c("EC.20250501.01","EC.20250501.02","EC.20250501.03","EC.20250501.04","EC.20250501.05",
               "AB.20250506.01","AB.20250506.02","AB.20250506.03","AB.20250506.04","AB.20250506.05")
surveyarea <- c("Emeryville Crescent", "Albany Bulb")

# Empty list to store each observation
time_list <- list()

# Loop to generate data
for (idx in 1:200) {
  
  # Random selections
  species <- sample(birdspecies, 1)
  survey_area <- sample(surveyarea, 1)
  focal_ID <- sample(Focal_IDs, 1)
  biofilm_level <- sample(c("high", "medium", "low"), 1)
  
  # Weighted trends
  species_weight <- switch(species,
                           "WESA" = 15,
                           "LESA" = 14.5,
                           "MAGO" = 1,
                           "WILL" = 1.5,
                           "LBCU" = 1,
                           "SEPL" = 15,
                           "BBPL" = 2,
                           "DUNL" = 15,
                           1)
  
  area_weight <- ifelse(survey_area == "Albany Bulb", 1.5, 0)
  biofilm_weight <- switch(biofilm_level,
                           "high" = 2,
                           "medium" = 1,
                           "low" = 0,
                           0)

  base_time <- species_weight + area_weight + biofilm_weight

  # Simulate cumulative time with weights
  a <- runif(1, min = 1 + base_time, max = 10 + base_time)
  b <- runif(1, min = a, max = 30 + base_time) - a
  c <- runif(1, min = a + b, max = 40 + base_time) - (a + b)
  d <- runif(1, min = a + b + c, max = 80 + base_time) - (a + b + c)
  e <- runif(1, min = a + b + c + d, max = 90 + base_time) - (a + b + c + d)
  f <- runif(1, min = a + b + c + d + e, max = 110 + base_time) - (a + b + c + d + e)
  g <- runif(1, min = a + b + c + d + e + f, max = 120 + base_time) - (a + b + c + d + e + f)
  h <- runif(1, min = a + b + c + d + e + f + g, max = 140 + base_time) - (a + b + c + d + e + f + g)
  i <- runif(1, min = a + b + c + d + e + f + g + h, max = 160 + base_time) - (a + b + c + d + e + f + g + h)
  j <- runif(1, min = a + b + c + d + e + f + g + h + i, max = 180 + base_time) - (a + b + c + d + e + f + g + h + i)
  
  # Build temp df
  temp_df <- data.frame(
    focal_ID = focal_ID,
    survey_area = survey_area,
    species = species,
    time = c(a,b,c,d,e,f,g,h,i,j),
    biofilm = biofilm_level
  )
  
  # Store
  time_list[[idx]] <- temp_df
}

# Combine all
final_data <- do.call(rbind, time_list)

# Add environmental variables
final_data$elevation <- runif(nrow(final_data), min = -3, max = 0)
final_data$TPI <- runif(nrow(final_data), min = -1, max = 1)
final_data$dist_nearest <- runif(nrow(final_data), min = 1, max = 6700)

# Add foraging guilds
final_data <- final_data %>%
  dplyr::mutate(
    Guild = dplyr::case_when(
      species %in% c("WESA", "LESA", "SEPL", "BBPL", "DUNL") ~ "Small Shorebird",
      species %in% c("MAGO", "WILL", "LBCU") ~ "Large Shorebird",
      TRUE ~ NA_character_
    )
  )

```


# Project Introduction:

The tidal mudflats of San Francisco Bay provide critical wintering and stopover habitat for migratory shorebirds (Mathot et al., 2018). These mudflats offer nutrient rich macroinvertebrates and microphytobenthos biofilms (MPB) high in fatty acids that are vital food sources needed to carry these shorebirds thousands of miles up and down the Pacific Flyway (Quinn et al., 2017). Unfortunately many of these energy rich tidelands are disappearing due to anthropogenic reclamation and with the imminent threat of sea level rise (SLR) the fate of our surviving mudflats remains imperiled (French, 1997; Stralberg et al., 2011). It’s estimated that at least 30% of the existing mudflats in San Francisco Bay are expected to diminish in the next 50 years due to SLR and the diversion of sediment to breached and subsided salt ponds (Valoppi, 2018). Under current SLR predictions San Francisco Baylands, like estuaries around the globe, will experience coastal squeeze, whereby man made structures abutting the shoreline prevent the natural climb and expansion of tideland habitat into the uplands (Pontee, 2013) further exacerbating estuarine habitat loss. 

As the persistence of these crucial habitats hangs in uncertainty, improving our understanding of the interplay of biotic (shorebirds, MPB) and abiotic (microtopography, sedimentation patterning, elevation) features of mudflats could provide critical insight in assessing mudflat quality and value to shorebirds (Douglas et al., 2024). In these dynamic habitats even subtle ripples in surface sediment can influence biological patterning in biofilm and over time lead to topographically complex mudflats with even more pronounced physical and biological patterning (Beninger, 2018). While many studies have examined shorebird interactions with mudflats at large spatial scales (hundreds of meters to kilometers) (e.g., (Butler et al., 2001; Hockey et al., 1992)), few have explored this relationship at finer scales (under 10 centimeters). In this study I plan to use a combination of unoccupied aerial vehicle (UAV) technologies, machine learning avian detection models, and samples collected in the field to analyze the relationships between shorebirds and biotic and abiotic mudflat characteristics across areas with varying sedimentary surfaces in San Francisco Bay. These remote sensing technologies will allow me to vastly minimize site disturbance and explore fine-scale biological patterning on mudflats as they relate to shorebird foraging. Results from my study may help prioritize mudflat preservation and guide the optimization of existing or future habitat to support migratory shorebird success.

# Specific Aims:

1) Characterize the abiotic and biotic variation across two San Francisco bay mudflats under varying environmental and sedimentary influences using remote sensing technologies.

2) Identify which abiotic and biotic mudflat features drive shorebird foraging preference.

# Checking data structure

```{r}
# Check structure
str(final_data)

# Making sure the categorical variables are factors
final_data <- final_data %>%
  mutate(
    species = as.factor(species),
    survey_area = as.factor(survey_area),
    biofilm = as.factor(biofilm),
    Guild = as.factor(Guild),
    focal_ID = as.factor(focal_ID)
  )
```

# My Hypothesis:
# Null Hypothesis (H0): There is no significant difference in foraging time among the different biofilm coverages for each shorebird species.
# Alternative Hypothesis (H1): "Shorebird species spend different amounts of time foraging across different levels of biofilm.

# Visualizing data to look for trends within species across different biofilm densities
```{r}
#Visualization with interaction
#boxplot showing time by biofilm level, grouped by species:

library(ggplot2)

ggplot(final_data, aes(x = biofilm, y = time, fill = biofilm)) +
  geom_boxplot() +
  facet_wrap(~ species, scales = "free_y") +
  theme_minimal() +
  labs(title = "Time by Biofilm Level per Species",
       x = "Biofilm Level",
       y = "Time Spent (s)") +
  scale_fill_brewer(palette = "Pastel1")
```

# Visualizing data to look for obvious trends across Species within the same biofilm cover
```{r}
# Ordering biofilm by low, medium, high
filtered_data <- final_data %>%
  filter(!is.na(species)) %>%
  mutate(biofilm = factor(biofilm, levels = c("low", "medium", "high")))

# Created boxplots by biofilm density
ggplot(filtered_data, aes(x = species, y = time, fill = species)) +
  geom_boxplot() +
  facet_wrap(~ biofilm, ncol = 1, scales = "free_y") +
  theme_minimal() +
  labs(title = "Time Spent per Species at Each Biofilm Density",
       x = "Species",
       y = "Time Spent (s)") +
  scale_fill_brewer(palette = "Set3") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```


# Visualizing data to look for trends within Foraging Guild across different biofilm densities
```{r}
library(ggplot2)

ggplot(final_data, aes(x = biofilm, y = time, fill = biofilm)) +
  geom_boxplot() +
  facet_wrap(~ Guild, scales = "free_y") +
  theme_minimal() +
  labs(title = "Time by Biofilm Level per Foraging Guild",
       x = "Biofilm Level",
       y = "Time Spent (s)") +
  scale_fill_brewer(palette = "Pastel1")
```


# Visualizing data to look for trends across Foraging Guilds within in the same biofilm cover
```{r}

# Set desired order for biofilm and filter data
filtered_data <- final_data %>%
  filter(!is.na(Guild)) %>%
  mutate(biofilm = factor(biofilm, levels = c("low", "medium", "high")))

# Create boxplots by Guild, faceted by biofilm level
ggplot(filtered_data, aes(x = Guild, y = time, fill = Guild)) +
  geom_boxplot() +
  facet_wrap(~ biofilm, ncol = 1, scales = "free_y") +
  theme_minimal() +
  labs(title = "Time Spent by Foraging Guild at Each Biofilm Density",
       x = "Foraging Guild",
       y = "Time Spent (s)") +
  scale_fill_brewer(palette = "Set2") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

```


# Testing for Normality

```{r}
# checking if "time" is normally distributed
shapiro.test(final_data$time)
```

```{r}
# another look at normality
final_data$log_time <- log(final_data$time + 1)
shapiro.test(final_data$log_time)
hist(final_data$log_time)
qqnorm(final_data$log_time); qqline(final_data$log_time)
```

# Exploring possible models

I have decided to use GLMM's as the approach for my simulated data analysis. Because my shorebird foraging data points are grouped by focal ID  (how they will be collected) they lack independence of one another. With a GLMM I can account for Focal_ID as a random effect. In this data simulation my response variable "time" (aka time spent foraging) is right skewed and therefore non-normal.To work with the right-skewed nature of the data I applied a Gamma Distribution and log link to my GLMM.    

```{r}
# exploring a GLMM with interactions

glmm_species <- glmer(time ~ species * biofilm + (1 | focal_ID), 
              family = Gamma(link = "log"), data = final_data)
summary(glmm_species)
```

No individual species, no biofilm level, and no species–biofilm combination shows a statistically significant difference in time spent. Put simply, there's no evidence from this model that species spend different amounts of time in different biofilm conditions.

```{r}
#trying another GLMM without interactions to simplify
glmm_simple_species <- glmer(time ~ species + biofilm + (1 | focal_ID), 
                     family = Gamma(link = "log"), data = final_data)
summary(glmm_simple_species)
```
Still no meaningful outcomes with the data :(

```{r}

#trying a glm with guild instead of species to see if there is anything different
glmm_simple_guild <- glmer(time ~ Guild + biofilm + (1 | focal_ID), 
                     family = Gamma(link = "log"), data = final_data)
summary(glmm_simple_guild)
```

# Using the AIC to find the best model of the bunch
```{r}

#looking to see what is the best model of the bunch (even though none are significantly trendy)
AIC(glmm_species,glmm_simple_species, glmm_simple_guild)

```


The trends in my data, though insignificant are better captured by guild-level differences rather than species-level.


# Bibliography:

Beninger, P. G. (Ed.). (2018). Mudflat Ecology (Vol. 7). Springer International Publishing. https://doi.org/10.1007/978-3-319-99194-8

Butler, R. W., Davidson, N. C., & Morrison, R. I. G. (2001). Global-Scale Shorebird Distribution in Relation to Productivity of Near-Shore Ocean Waters. Waterbirds: The International Journal of Waterbird Biology, 24(2), 224–232. https://doi.org/10.2307/1522034

Douglas, T. J., Coops, N. C., Drever, M. C., Hunt, B. P. V., & Martin, T. G. (2024). Linking microphytobenthos distribution and mudflat geomorphology under varying sedimentary regimes using unoccupied aerial vehicle (UAV)-acquired multispectral reflectance and photogrammetry. Science of The Total Environment, 942, 173675. https://doi.org/10.1016/j.scitotenv.2024.173675

French, P. (1997). Coastal and Estuarine Management. Routledge. https://doi.org/10.4324/9780203437278

Hockey, P. A. R., Navarro, R. A., Kalejta, B., & Velasquez, C. R. (1992). The Riddle of the Sands: Why are Shorebird Densities so High in Southern Estuaries? The American Naturalist, 140(6), 961–979.

Mathot, K. J., Piersma, T., & Elner, R. W. (2018). Shorebirds as Integrators and Indicators of Mudflat Ecology. In P. G. Beninger (Ed.), Mudflat Ecology (pp. 309–338). Springer International Publishing. https://doi.org/10.1007/978-3-319-99194-8_12

Pontee, N. (2013). Defining coastal squeeze: A discussion. Ocean & Coastal Management, 84, 204–207. https://doi.org/10.1016/j.ocecoaman.2013.07.010

Quinn, J. T., Hamilton, D. J., & Hebert, C. E. (2017). Fatty acid composition and concentration of alternative food of Semipalmated Sandpipers (Calidris pusilla) in the upper Bay of Fundy, Canada. Canadian Journal of Zoology, 95(8), 565–573. https://doi.org/10.1139/cjz-2016-0246

Stralberg, D., Brennan, M., Callaway, J. C., Wood, J. K., Schile, L. M., Jongsomjit, D., Kelly, M., Parker, V. T., & Crooks, S. (2011). Evaluating Tidal Marsh Sustainability in the Face of Sea-Level Rise: A Hybrid Modeling Approach Applied to San Francisco Bay. PLoS ONE, 6(11), e27388. https://doi.org/10.1371/journal.pone.0027388

Valoppi, L. (2018). Phase 1 studies summary of major findings of the South Bay Salt Pond Restoration Project, South San Francisco Bay, California. In Open-File Report (Nos. 2018–1039). U.S. Geological Survey. https://doi.org/10.3133/ofr20181039
