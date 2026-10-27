#==============================================================================.
# Supplementary figures S1.2 - S1.4
# S1.2 = study foci by region / regional combination
# S1.3 = biomes represented in the study areas
# S1.4 = conflict data / conflict categories used
#
# All three use the same palette and theme as the manuscript figures
# (01_Fig2_perregion.R, 02_Fig3_NCP_pathway.R).
# start: 25.08.26
#==============================================================================.


# =============================================================================.
# 1. LOAD PACKAGES ####
# =============================================================================.

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(ggplot2)
library(scales)
library(here)


# =============================================================================.
# 2. SETTINGS ####
# =============================================================================.

# Biomes: how should studies coded "All Biomes" (i.e. global studies) be counted?
#   FALSE = keep "All Biomes" as its own category   <- used in the supplement
#   TRUE  = expand them into all 15 individual biomes
# NOTE: this toggle changes the headline number quoted in the Figure 2A caption.
#       FALSE -> ~60% of studies cover tropical/subtropical biomes
#       TRUE  -> ~90%
EXPAND_ALL_BIOMES <- FALSE


# --- Palette, taken from the manuscript figures --------------------------------
# Count gradient used in Figure 2A/2B and Figure 3 bubbles:
#   c("#A0A0A0", "#C99A82", "#C0805F", "#9E5A5A")
# The blues (#1D335F / #9DB4D3) are reserved for measured vs inferred evidence
# in Figure 2C, so they are deliberately NOT used here: these bars encode counts.
count_fill  <- "#9E5A5A"   # dark end of the manuscript count gradient
label_col   <- "white"     # n = labels sit inside the bars
BASE_SIZE   <- 18

# --- Shared theme ------------------------------------------------------------
theme_supp <- function(base_size = BASE_SIZE) {
  theme_light(base_size = base_size) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      axis.text          = element_text(color = "black"),
      plot.title         = element_blank()
    )
}

# --- Shared bar builder ------------------------------------------------------
# All three figures are the same object: horizontal bars, x = % of studies,
# n = labels inside the bar, 0-100% axis so panels stay comparable.
supp_barplot <- function(df, cat, pct, n) {
  df <- mutate(df, .cat = {{ cat }}, .pct = {{ pct }}, .n = {{ n }})
  # sort by size, unless the category was already given an explicit order
  if (!is.factor(df$.cat)) df$.cat <- fct_reorder(df$.cat, df$.pct)
  ggplot(df, aes(x = .pct, y = .cat)) +
    geom_col(width = 0.6, fill = count_fill) +
    # labels sit inside long bars in white, and outside short bars in black,
    # so they are never clipped
    geom_text(aes(label = paste0("n = ", .n),
                  hjust = ifelse(.pct > 12, 1.1, -0.15),
                  colour = .pct > 12),
              size = 4.5, show.legend = FALSE) +
    scale_colour_manual(values = c(`TRUE` = label_col, `FALSE` = "black"),
                        guide = "none") +
    scale_x_continuous(limits = c(0, 100),
                       breaks = seq(0, 100, by = 20),
                       labels = percent_format(scale = 1)) +
    labs(x = "Percentage of studies", y = NULL) +
    theme_supp()
}

save_supp <- function(plot, file, width = 11, height = 6) {
  ggsave(here(file), plot = plot,
         width = width, height = height, dpi = 300, bg = "white")
}


# =============================================================================.
# 3. LOAD AND CLEAN DATA ####
# =============================================================================.
# Paths resolve relative to the data_analysis folder via the .Rproj file.

d_raw <- read_excel(here("extraction_temp_ANALYSIS_26-07-06.xlsx"))

d <- d_raw %>%
  # blanks and the string "NA" both become real NA
  mutate(across(everything(), ~ ifelse(.x %in% c("", "NA"), NA, .x))) %>%
  # defensive: keep only included studies
  filter(tolower(included) == "yes") %>%
  # spelling error in the extraction template
  mutate(subregion = str_replace_all(subregion, "Caribean", "Caribbean")) %>%
  # "All Regions" is called "Global" in the figures
  mutate(region = str_replace_all(region, "All Regions", "Global"))

number_studies <- n_distinct(d$paper_id)   # denominator for all percentages: 69
message("Studies: ", number_studies)


# =============================================================================.
# 4. FIGURE S1.2 - REGIONS ####
# =============================================================================.
# One row per study. Studies spanning several regions get a combined label
# ("Africa & Asia"), so the categories are mutually exclusive and sum to n = 69.

region_count <- d %>%
  mutate(region = ifelse(is.na(region), "Not reported", region)) %>%
  separate_rows(region, sep = ",\\s*") %>%
  group_by(paper_id) %>%
  summarise(region = paste(sort(unique(region)), collapse = " & "), .groups = "drop") %>%
  count(region, name = "count") %>%
  mutate(percentage = count / number_studies * 100) %>%
  # size order, but "Not reported" always sits at the bottom
  mutate(region = fct_reorder(region, percentage),
         region = fct_relevel(region, "Not reported"))

fig_S1_2 <- supp_barplot(region_count, region, percentage, count)

save_supp(fig_S1_2, "Fig_S1.2_regions.png")
print(region_count)


# =============================================================================.
# 5. FIGURE S1.3 - BIOMES ####
# =============================================================================.
# Counted once per study per biome, so categories overlap and do NOT sum to 69.
# "Rock and Ice" is dropped: it is not a biome the studies address.

all_biomes <- c("Boreal Forests/Taiga",
                "Deserts & Xeric Shrublands",
                "Flooded Grasslands & Savannas",
                "Mangroves",
                "Mediterranean Forests, Woodlands & Scrub",
                "Montane Grasslands & Shrublands",
                "Rock and Ice",
                "Temperate Broadleaf & Mixed Forests",
                "Temperate Conifer Forests",
                "Temperate Grasslands, Savannas & Shrublands",
                "Tropical & Subtropical Coniferous Forests",
                "Tropical & Subtropical Dry Broadleaf Forests",
                "Tropical & Subtropical Grasslands, Savannas & Shrublands",
                "Tropical & Subtropical Moist Broadleaf Forests",
                "Tundra")

real_biomes <- setdiff(all_biomes, "Rock and Ice")

d_biome <- d
if (EXPAND_ALL_BIOMES) {
  d_biome <- d_biome %>%
    mutate(biome = str_replace_all(biome, "All Biomes",
                                   paste(real_biomes, collapse = ", ")))
}

# match whole biome names, so the commas inside biome names do not split them
biome_pattern <- paste(c(if (!EXPAND_ALL_BIOMES) "All Biomes", real_biomes),
                       collapse = "|")

biome_count <- d_biome %>%
  mutate(biome = str_extract_all(biome, biome_pattern)) %>%
  unnest(biome) %>%
  filter(!is.na(biome)) %>%          # drops studies with no biome coded
  distinct(paper_id, biome) %>%
  count(biome, name = "count") %>%
  mutate(percentage = count / number_studies * 100)

fig_S1_3 <- supp_barplot(biome_count, biome, percentage, count)

save_supp(fig_S1_3, "Fig_S1.3_biomes.png", height = 7)
print(biome_count %>% arrange(desc(count)))


# =============================================================================.
# 6. FIGURE S1.4 - CONFLICT DATA ####
# =============================================================================.
# Counted once per study per category. Categories are the conflict datasets a
# study used, not conflict types (see Box 1 and Buhaug et al. 2023).

conflict_count <- d %>%
  separate_rows(type_conflict, sep = ",\\s*") %>%
  filter(!is.na(type_conflict)) %>%
  distinct(paper_id, type_conflict) %>%
  count(type_conflict, name = "count") %>%
  mutate(percentage = count / number_studies * 100)

fig_S1_4 <- supp_barplot(conflict_count, type_conflict, percentage, count)

save_supp(fig_S1_4, "Fig_S1.4_conflict_data.png")
print(conflict_count %>% arrange(desc(count)))

#------------------------------------------------------------------------------.
