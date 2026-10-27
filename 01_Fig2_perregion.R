#==============================================================================.
# Figure 2
# a = Coropleth subregional map of study focus
# b = heatmap with regions and corresponding number of articles per indicator (conflict, nature, climate, ncps)
# c = stacked bar: per NCP, how many links are directly measured vs inferred
# # start: 21.05.25
#==============================================================================.


# =============================================================================.
# 1. LOAD PACKAGES ####
# =============================================================================.

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(viridis)
library(stringr)
library(cowplot)
library(RColorBrewer)
library(viridisLite)
library(here)      


# =============================================================================.
# 2. LOAD DATA ####
# =============================================================================.
# Paths are resolved relative to the project root with here(). Put an RStudio
# .Rproj file (or an empty file named ".here") in the "03 analysis" folder so
# here() anchors there. No setwd() - the script then runs on any machine.

# Data extraction data: make sure you are using the most updated excel!
d_raw <- read_excel(here("extraction_temp_ANALYSIS_26-07-06.xlsx"))
d <- d_raw


# UN M49 SF layers
# from https://geoportal.un.org/arcgis/home/user.html?user=United_Nations_Geospatial
subregions_sf <-
  st_read(here("UNM49-map", "Subregions.shp"))
intermediate_reg_sf <-
  st_read(here("UNM49-map", "intermediate_regions.shp"))


# =============================================================================.
# 3. DATA PROCESSING ####
# =============================================================================.

######################## --- ALWAYS RUN THIS --- ##############################.
# introduce NA's wherever field were left blank
d <- d %>%
  mutate(across(everything(), ~ifelse(.x %in% c("", "NA"), NA, .x)))

# correcting spelling error in extraction template
d <- d %>%
  mutate(subregion = str_replace_all(subregion, "Caribean", "Caribbean"))

# Number of distinct studies
number_studies <- n_distinct(d$paper_id)

# rename "All Regions" into "Global"
d <- d %>%
  mutate(region = str_replace_all(region, "All Regions", "Global"))

###############################################################################.
## 3.0. Fix Regions #####
# add regions Americas so the fact that there are no studies from this region is displayed in the figure.

# first: how many global studies?
global <- d %>% filter(region == "Global") %>%
  distinct(paper_id, region) # 24

all_regions <- c("Americas",
                 "Africa",
                 "Asia",
                 "Europe",
                 "Oceania",
                 "Global")

# this is for later to add to all the counts

## 3.1. Panel b of Figure 2 - studies per region and indicators ####

### 3.1.1. Conflicts per Region #####
# Split string of Regions and string of conflict types into several lines
conflict_region_clean <- d %>%
  mutate(type_conflict = strsplit(type_conflict, ", ")) %>%
  mutate(region = strsplit(region, ", ")) %>%
  unnest(region) %>%
  unnest(type_conflict) %>%
  distinct(paper_id, region, type_conflict) # in case there is duplicates and for the the entries that were split in the data extraction process

# Count frequency of conflicts per region and sort for Global to be on top
conflict_count_perreg <- conflict_region_clean %>%
  count(region, type_conflict) %>%
  arrange(factor(region, levels = c("Global", sort(unique(region[region != "Global"]))))) %>%
  na.omit()

# rename "other" into "Other Conflict types"
conflict_count_perreg <- conflict_count_perreg %>%
  mutate(type_conflict = str_replace_all(type_conflict, "Other", "Other conflict types"))


# Create levels, so plotting is done in right order
conflict_levels <- c(
  "UCDP-GED",
  "ACLED",
  "State-based armed conflict",
  "Non-state conflict",
  "One-sided violence",
  "Other conflict types"
)

# fill in missing regions
conflict_count_perreg <- conflict_count_perreg %>%
  complete(region = all_regions,
           fill = list(n = NA)) %>%
  filter(!is.na(type_conflict))

# prep for merging conflicts, NCPs, Nature indicator, ...
# in that you standardize the name of the Indicator
colnames(conflict_count_perreg)[2] <- "indicator"
conflict_count_perreg$indicator_type <- "Conflict"

# Apply levels
conflict_count_perreg <- conflict_count_perreg %>%
  mutate(indicator = factor(indicator,
                            levels = conflict_levels,
                            exclude = NULL))
#------------------------------------------------------------------------------.


### 3.1.2. Nature indicators per Region ####
# split entries into individual line and keep only distinct entries
nature_region_clean <- d %>%
  mutate(nature_indicator = strsplit(nature_indicator, ", ")) %>%   # Split entries into multiple rows
  mutate(region = strsplit(region, ", ")) %>%
  unnest(region) %>%
  unnest(nature_indicator) %>%                                       # Expand rows based on the split
  distinct(paper_id, region, nature_indicator)

# complete nature indicators
all_nat_indi <- c(
  "Ecosystem structure",
  "Ecosystem function",
  "Community composition",
  "Species populations",
  "Organismal traits",
  "Genetic composition"
)

nature_region_clean <- nature_region_clean %>%
  complete(nature_indicator = all_nat_indi,
           fill = list(0))


# count how many times which element appears
nature_count_perreg <- nature_region_clean %>%
  count(region, nature_indicator) %>%
  na.omit()

# complete regions
nature_count_perreg <- nature_count_perreg %>%
  complete(region = all_regions,
           nature_indicator = all_nat_indi,
           fill = list(n = NA)) %>%
  filter(!is.na(nature_indicator))

# prep for merging
colnames(nature_count_perreg)[2] <- "indicator"
nature_count_perreg$indicator_type <- "Nature"

# create levels
nature_levels <- c(
  "Ecosystem structure",
  "Ecosystem function",
  "Community composition",
  "Species populations",
  "Organismal traits",
  "Genetic composition"
)

nature_count_perreg$indicator <-
  factor(nature_count_perreg$indicator, levels = nature_levels, exclude = NULL)
#------------------------------------------------------------------------------.


### 3.1.3. Climate per Region ####

# Split several entries in "clim_indicator" into individual rows
clim_region_clean <- d %>%
  mutate(clim_indicator = strsplit(clim_indicator, ", ")) %>%
  mutate(region = strsplit(region, ", ")) %>%
  unnest(region) %>%
  unnest(clim_indicator) %>%
  distinct(paper_id, region, clim_indicator)


# Count Frequency of each clim_indicator  (only once per paper ID)
clim_count_perreg <- clim_region_clean %>%
  count(region, clim_indicator) %>%
  na.omit()

# make sure it is in the right order
climate_levels <- c(
  "Heat and Cold",
  "Wet and Dry",
  "Wind",
  "Snow and Ice",
  "Disasters",
  "Other"
)

clim_count_perreg <- clim_count_perreg %>%
  complete(region = all_regions,
           fill = list(n = NA)) %>%
  filter(!is.na(clim_indicator))

# prep for merging
colnames(clim_count_perreg)[2] <- "indicator"
clim_count_perreg$indicator_type <- "Climate"

clim_count_perreg <- clim_count_perreg %>%
  mutate(indicator = factor(indicator, levels = climate_levels, exclude = NULL))
#------------------------------------------------------------------------------.


### 3.1.4. NCPs per region ####
# - first create a list so you can seperate each ncp - seperating by "," does not
# work here as many ncps contain "," within themselves
# - seperate into individual lines
# - count frequency per area

# list of NCPs
all_ncp <- c(
  "Habitat creation and maintenance",
  "Pollination and dispersal of seeds",
  "Regulation of air quality",
  "Regulation of climate",
  "Regulation of ocean acidification",
  "Regulation of freshwater quantity, location and timing",
  "Regulation of freshwater and coastal water quality",
  "Formation, protection and decontamination of soils",
  "Regulation of hazards and extreme events",
  "Regulation of organisms detrimental to humans",
  "Energy",
  "Food and Feed",
  "Materials and assistance",
  "Medicinal, biochemical and genetic resources",
  "Learning and inspiration",
  "Physical and psychological experiences",
  "Supporting identities",
  "Maintenance of options",
  "Not specified"
)


# Create a regex pattern for splitting based on the ncp list
ncp_pattern <- paste(all_ncp, collapse = "|")

# Now, let's split the 'ncp_impact' column based on the ncps in the list and unnest
ncp_clean <- d %>%
  mutate(ncp_impact = str_extract_all(ncp_impact, ncp_pattern)) %>%
  mutate(region = str_split(region, ", ")) %>%
  unnest(region) %>%
  unnest(ncp_impact) %>%
  distinct(paper_id, ncp_impact, region)

# Count Frequency of each ncp impact per region  (only once per paper ID)
ncp_count_perregion <- ncp_clean %>%
  count(region, ncp_impact) %>%
  na.omit()

# Rename NCPs into short NCPs with Numbering
ncp_count_perregion <- ncp_count_perregion %>%
  mutate(ncp_impact = case_when(
    ncp_impact == "Habitat creation and maintenance" ~ "NCP 1: Habitat",
    ncp_impact == "Pollination and dispersal of seeds" ~ "NCP 2: Pollination",
    ncp_impact == "Regulation of air quality" ~ "NCP 3: Air quality",
    ncp_impact == "Regulation of climate" ~ "NCP 4: Climate",
    ncp_impact == "Regulation of ocean acidification" ~ "NCP 5: Ocean acidification",
    ncp_impact == "Regulation of freshwater quantity, location and timing" ~ "NCP 6: Water quantity",
    ncp_impact == "Regulation of freshwater and coastal water quality" ~ "NCP 7: Water quality",
    ncp_impact == "Formation, protection and decontamination of soils" ~ "NCP 8: Soils",
    ncp_impact == "Regulation of hazards and extreme events" ~ "NCP 9: Hazards",
    ncp_impact == "Regulation of organisms detrimental to humans" ~ "NCP 10: Pests",
    ncp_impact == "Energy" ~ "NCP 11: Energy",
    ncp_impact == "Food and Feed" ~ "NCP 12: Food",
    ncp_impact == "Materials and assistance" ~ "NCP 13: Materials",
    ncp_impact == "Medicinal, biochemical and genetic resources" ~ "NCP 14: Medicine",
    ncp_impact == "Learning and inspiration" ~ "NCP 15: Learning",
    ncp_impact == "Physical and psychological experiences" ~ "NCP 16: Experience",
    ncp_impact == "Supporting identities" ~ "NCP 17: Identity",
    ncp_impact == "Maintenance of options" ~ "NCP 18: Options",
    TRUE ~ ncp_impact # fallback in case of unmatched strings
  ))


# add the NCPs for which you do not have data so they will be
# displayed in the figure
all_ncp_short <- c(
  "NCP 1: Habitat",
  "NCP 2: Pollination",
  "NCP 3: Air quality",
  "NCP 4: Climate",
  "NCP 5: Ocean acidification",
  "NCP 6: Water quantity",
  "NCP 7: Water quality",
  "NCP 8: Soils",
  "NCP 9: Hazards",
  "NCP 10: Pests",
  "NCP 11: Energy",
  "NCP 12: Food",
  "NCP 13: Materials",
  "NCP 14: Medicine",
  "NCP 15: Learning",
  "NCP 16: Experience",
  "NCP 17: Identity",
  "NCP 18: Options",
  "Not specified"
)

# just double check if there are any different values
setdiff(unique(ncp_count_perregion$ncp_impact), all_ncp_short)

# complete the list so combinations of NCPs that did nor occure are still
# shown in the plot later
ncp_count_complete <- ncp_count_perregion %>%
  complete(
    region = all_regions,
    ncp_impact = all_ncp_short,
    fill = list(NA)) %>%
  filter(ncp_impact != "Not specified")


# prep for merging
colnames(ncp_count_complete)[2] <- "indicator"
ncp_count_complete$indicator_type <- "NCP"

ncp_count_complete <- ncp_count_complete %>%
  mutate(indicator = factor(indicator,
                            levels = all_ncp_short,
                            exclude = NULL)) %>%
  arrange((factor(region,
                  levels = c("Global",
                             sort(unique(region[region != "Global"]))))), indicator)

# check levels
levels(ncp_count_complete$indicator)
#------------------------------------------------------------------------------.


### 3.1.5. Merge all dataframes with indicator types ####

# Define region order
region_levels <- c("Global", "Africa", "Americas", "Asia", "Europe", "Oceania")

# Bind rows
combined_df <- bind_rows(
  conflict_count_perreg,
  nature_count_perreg,
  clim_count_perreg,
  ncp_count_complete
)

combined_df <- combined_df %>%
  mutate(
    
    # Convert to factor with desired order
    region = factor(region, levels = rev(region_levels)),
    indicator_type = factor(indicator_type, levels = c("Conflict", "Nature", "Climate", "NCP"))
  )

levels(combined_df$region)
#------------------------------------------------------------------------------.


## 3.2. Panel a - Choropleth map of subregions study frequency ####

# depending on what kind of map you want, run this to have global studies depicted
# all_subregions <- c("Australia and New Zealand", "Central Asia", "Eastern Asia",
#                     "Eastern Europe", "Melanesia", "Micronesia",
#                     "Northern Africa", "Northern America", "Northern Europe", "Polynesia",
#                     "South-eastern Asia", "Southern Asia", "Southern Europe", "Caribbean", "Central America",
#                     "Eastern Africa", "Middle Africa", "South America", "Southern Africa", "Western Africa",
#                     "Western Asia", "Western Europe")

# d$subregion <- gsub("All Subregions",
#                           paste(all_subregions, collapse = ", "),
#                           d$subregion)


# EXCLUDE SUBREGIONS
d_excl_global <- d %>%
  filter(subregion != "All Subregions")

# Merge/remove the split cases, split subregions
# into individual listings
d_subregion <- d_excl_global %>%
  mutate(subregion = strsplit(subregion, ", ")) %>%
  unnest(subregion) %>%
  distinct(paper_id, subregion)

# Count Frequency of each Subegion (only once per paper ID)
subregion_count <- d_subregion %>%
  group_by(subregion) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
head(subregion_count)



###  Merging for Coropleth map ####.
## 3.3. Choropleth Subregions ####
# I work with the data from UN M49 on intermediate and subregions
# In the data extraction I combined intermediate and subregions - hence I have
# to do this here to.
# a.) Base = subregions for polygons --> delete and add elements
# b.) merge with counts on subregions

### a.) Base = subregions for polygons ####.
# delete Sub-saharan Africa and Latin America and the Caribbean
minus_subregion_sf <- subregions_sf %>%
  filter(!subreg %in% c("Sub-Saharan Africa", "Latin America and the Caribbean"))

# extract intermediate regions that are needed
to_add_intermediates <- intermediate_reg_sf %>%
  filter(!intreg == "Channel Islands") %>%
  select(-c("int_cd", "subreg")) %>%
  rename("subreg" = intreg)

# bind subregion and intermediate region = both sf objects
combined_sf <- bind_rows(minus_subregion_sf, to_add_intermediates) %>%
  rename("subregion" = subreg)



### b.) Merge Count data with sf ####.
# - first check if there are differences

setdiff(unique(combined_sf$subregion), unique(subregion_count$subregion))
# nice, no differences so we can continue merging

combined_count_sf <- left_join(combined_sf, subregion_count, by = "subregion")
#------------------------------------------------------------------------------.


## 3.4. Panel c data - NCP evidence (measured vs inferred) ####
# For each NCP we count how many (paper x NCP) links are directly measured vs
# inferred from discussion. The evidence class comes from `emp_inter`:
#   "empirical"      -> Directly measured
#   "interpretative" -> Inferred from discussion
# Rows are evidence-homogeneous, so NCPs bundled in one cell inherit the class.

# map emp_inter to evidence class, split NCPs, keep one row per (paper, NCP)
ncp_evidence_clean <- d %>%
  mutate(evidence = case_when(
    str_trim(emp_inter) == "empirical"      ~ "Directly measured",
    str_trim(emp_inter) == "interpretative" ~ "Inferred from discussion",
    TRUE ~ NA_character_
  )) %>%
  mutate(ncp_impact = str_extract_all(ncp_impact, ncp_pattern)) %>%
  unnest(ncp_impact) %>%
  filter(!is.na(evidence), ncp_impact != "Not specified") %>%
  distinct(paper_id, ncp_impact, evidence)

# Rename NCPs into short NCPs with Numbering (same recode as panel b)
ncp_evidence_clean <- ncp_evidence_clean %>%
  mutate(ncp_impact = case_when(
    ncp_impact == "Habitat creation and maintenance" ~ "NCP 1: Habitat",
    ncp_impact == "Pollination and dispersal of seeds" ~ "NCP 2: Pollination",
    ncp_impact == "Regulation of air quality" ~ "NCP 3: Air quality",
    ncp_impact == "Regulation of climate" ~ "NCP 4: Climate",
    ncp_impact == "Regulation of ocean acidification" ~ "NCP 5: Ocean acidification",
    ncp_impact == "Regulation of freshwater quantity, location and timing" ~ "NCP 6: Water quantity",
    ncp_impact == "Regulation of freshwater and coastal water quality" ~ "NCP 7: Water quality",
    ncp_impact == "Formation, protection and decontamination of soils" ~ "NCP 8: Soils",
    ncp_impact == "Regulation of hazards and extreme events" ~ "NCP 9: Hazards",
    ncp_impact == "Regulation of organisms detrimental to humans" ~ "NCP 10: Pests",
    ncp_impact == "Energy" ~ "NCP 11: Energy",
    ncp_impact == "Food and Feed" ~ "NCP 12: Food",
    ncp_impact == "Materials and assistance" ~ "NCP 13: Materials",
    ncp_impact == "Medicinal, biochemical and genetic resources" ~ "NCP 14: Medicine",
    ncp_impact == "Learning and inspiration" ~ "NCP 15: Learning",
    ncp_impact == "Physical and psychological experiences" ~ "NCP 16: Experience",
    ncp_impact == "Supporting identities" ~ "NCP 17: Identity",
    ncp_impact == "Maintenance of options" ~ "NCP 18: Options",
    TRUE ~ ncp_impact
  ))

# Count measured vs inferred per NCP
ncp_evidence_count <- ncp_evidence_clean %>%
  count(ncp_impact, evidence, name = "n")

# Order NCPs by total number of links (largest ends up on top of the bar)
ncp_order <- ncp_evidence_count %>%
  group_by(ncp_impact) %>%
  summarise(total = sum(n), .groups = "drop") %>%
  arrange(total) %>%
  pull(ncp_impact)

ncp_evidence_count <- ncp_evidence_count %>%
  mutate(
    ncp_impact = factor(ncp_impact, levels = ncp_order),
    # stacking order: measured segment first (left), inferred to its right
    evidence   = factor(evidence,
                        levels = c("Directly measured", "Inferred from discussion"))
  )


## 3.4b. Per-NCP measured vs inferred counts (for the Results text) ####
ncp_split_table <- ncp_evidence_clean %>%
  mutate(evidence = if_else(evidence == "Directly measured", "measured", "inferred")) %>%
  count(ncp_impact, evidence) %>%
  tidyr::pivot_wider(names_from = evidence, values_from = n, values_fill = 0) %>%
  mutate(total = measured + inferred) %>%
  arrange(desc(total))
print(ncp_split_table)



# ! Caption numbers: distinct papers per category (matches the figure) ####
n <- number_studies
cnt <- function(col) {
  d %>%
    mutate(x = strsplit(.data[[col]], ", ")) %>% tidyr::unnest(x) %>%
    distinct(paper_id, x) %>% count(x, sort = TRUE) %>%
    mutate(pct = round(100 * n / !!n))
}
cnt("region"); cnt("type_conflict"); cnt("clim_indicator"); cnt("biome")

# =============================================================================.
# 4. VISUALIZATION ####
# =============================================================================.

## 4.1 PANEL b ####.
### Heatmap ####.

bubble_colors <- c("#A0A0A0", "#C99A82", "#C0805F", "#9E5A5A")

combined_heat <-
  ggplot(combined_df, aes(x = indicator, y = region)) +
  geom_point(data = combined_df,
             aes(size = n, fill = n), shape = 21, color = "#A0A0A0", alpha = 1) +
  geom_text(data = combined_df,
            aes(label = n),
            size = 14, fontface = "bold",
            color = "white") +
  facet_grid(~indicator_type, scales = "free_x", space = "free_x") +
  scale_fill_gradientn(
    colors = colorRampPalette(bubble_colors)(50),
    guide = "legend") +
  scale_size(range = c(15, 40), guide = "legend") +
  scale_x_discrete(position = "bottom") +
  theme_light() +
  labs(x = NULL,
       y = NULL,
       fill = "Number of studies",
       size = "Number of studies") +
  theme(
    panel.grid = element_blank(),
    panel.spacing = unit(1.5, "lines"),
    axis.ticks.x = element_line(color = "black", linewidth = 2),
    axis.ticks.length.x = unit(15, "pt"),
    axis.text.x = element_text(angle = 90,
                               hjust = 1,
                               vjust = 0.5,
                               size = 48,
                               color = "black"),
    axis.text.y = element_text(size = 48, color = "black"),
    axis.ticks.y = element_line(color = "black", linewidth = 2),
    axis.ticks.length = unit(15, "pt"),
    strip.text.x = element_text(size = 48, face = "bold"),
    legend.title = element_text(size = 44, face = "bold"),    # Legend title font size
    legend.text = element_text(size = 44),                    # Legend item font size
    legend.key.size = unit(2.5, "cm"),                        # Size of the boxes/symbols in legend
    legend.spacing.x = unit(1.5, "cm"),                         # Horizontal spacing between legend items
    legend.position = "top" ,
    strip.background =
      element_rect(fill = "#7A7570", color = "#7A7570", linewidth = 2)
  )

ggsave("Fig_2b_heatmap.png",
       combined_heat, width = 40, height = 25, dpi = 600)



## 4.2. PANEL a ####.
### Choropleth map with Regions (panel a of Figure 2) ####.

# Plot with rounded Earth effect
choro_subreg <-
  ggplot(data = combined_count_sf) +
  geom_sf(aes(fill = count), color = "white", size = 0.3) +
  coord_sf(crs = st_crs(combined_count_sf)) +
  scale_fill_gradientn(
    colors = c("#F2EBE7", "#D8B6A6", "#C0805F", "#9E5A5A"),
    na.value = "grey90",                 # subregions with no studies
    breaks = c(0, 15, 30, 45, 60),
    limits = c(0, 60),
    name = "Number of studies per subregion") +
  theme_minimal(base_size = 48) +
  theme(
    #plot.title = element_text(size = 24, face = "bold"),
    #plot.subtitle = element_text(size = 22),
    legend.position = "bottom",
    legend.title = element_text(size = 48),
    legend.text = element_text(size = 40),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  guides(
    fill = guide_colorbar(
      barheight = unit(1.2, "cm"),
      barwidth = unit(16.5, "cm"),
      title.position = "top",
      title.hjust = 0,
      direction = "horizontal",
      nbin = 5
    )
  )

# NOTE: added the missing ".png" extension so the file saves as a real image.
ggsave("Fig_2a_choro.png",
       plot = choro_subreg, width = 20, height = 16, dpi = 300)



## 4.3. PANEL c ####.
### Stacked bar: directly measured vs inferred per NCP ####.

# Two blues, chosen so they do NOT clash with the orange-red count gradient in
# panels a/b (which encodes a *count*, not evidence type). They also differ in
# lightness, so the panel stays readable under colour-vision deficiency.
evidence_colors <- c("Directly measured"        = "#1D335F",   # dark blue  = measured
                     "Inferred from discussion"  = "#9DB4D3")   # light blue = inferred

ncp_evidence_bar <-
  ggplot(ncp_evidence_count,
         aes(x = n, y = ncp_impact, fill = evidence)) +
  geom_col(width = 0.75) +
  scale_fill_manual(values = evidence_colors, labels = c("Measured", "Inferred"),
                    name = NULL) +
  labs(x = "Number of studies", y = NULL) +
  theme_light() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text.x  = element_text(size = 40, color = "black"),
    axis.text.y  = element_text(size = 40, color = "black"),
    axis.title.x = element_text(size = 44),
    legend.position = "top",
    legend.text  = element_text(size = 40),
    legend.key.size = unit(2, "cm")
  )

ggsave("Fig_2c_ncp_evidence.png",
       ncp_evidence_bar, width = 20, height = 20, dpi = 300, bg = "white")



## 4.4. Final Figure ####.
# Layout: a (map) and c (evidence bar) share the top row; b (region x indicator
# heatmap) spans the full width beneath.

top_row <-
  plot_grid(choro_subreg, ncp_evidence_bar,
            labels = c("a", "c"),
            label_size = 48,
            label_fontface = "bold",
            rel_widths = c(1.5, 1))

Figure_2 <-
  plot_grid(top_row, combined_heat,
            labels = c("", "b"),      # top row already labelled a/c
            label_size = 48,
            label_fontface = "bold",
            ncol = 1,
            rel_heights = c(1, 1.3))

# save
ggsave("Figure_2.tiff",
       Figure_2,
       width = 40,    # match your wider plot
       height = 42,   # extra height for the added top-row bar
       dpi = 300,
       bg = "white",
       limitsize = FALSE)

ggsave("Figure_2.eps",
       Figure_2,
       width = 40,
       height = 42,
       device = "eps",
       bg = "white",
       limitsize = FALSE)
#------------------------------------------------------------------------------.

