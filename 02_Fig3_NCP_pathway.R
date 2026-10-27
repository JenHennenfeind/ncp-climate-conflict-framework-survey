#==============================================================================.
# Figure 3: NCP - conflict pathway links, split by evidence type.
# (A) Empirical link   (B) Inferred from discussion.
#
# Evidence class is read directly from the extraction template's `emp_inter`
# column ("empirical" / "interpretative"). Rows are evidence-homogeneous, so
# each NCP inherits its row's class when the multi-NCP cells are unnested.
# start: 10.08.26.
#==============================================================================.


# =============================================================================.
# 1. LOAD PACKAGES ####
# =============================================================================.

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(here)      # resolves paths from the project root, no setwd() needed


# =============================================================================.
# 2. LOAD DATA ####
# =============================================================================.
# Paths are resolved relative to the project root with here(). Put an RStudio
# .Rproj file (or an empty file named ".here") in the "03 analysis" folder so
# here() anchors there. No setwd() - the script then runs on any machine.

d <- read_excel(here("extraction_temp_ANALYSIS_26-07-06.xlsx"))


# =============================================================================.
# 3. DATA PROCESSING ####
# =============================================================================.

######################## --- ALWAYS RUN THIS --- ##############################.
# introduce NA's wherever fields were left blank.
d <- d %>%
  mutate(across(everything(), ~ifelse(.x %in% c("", "NA"), NA, .x)))

number_studies <- n_distinct(d$paper_id)
######################## --- ALWAYS RUN THIS --- ##############################.


## 3.1. NCP lookup: full name -> number + short label ##########################.
ncp_lookup <- tibble(
  ncp_full = c("Habitat creation and maintenance",
               "Pollination and dispersal of seeds",
               "Regulation of air quality",
               "Regulation of climate",
               "Regulation of ocean acidification",
               "Regulation of freshwater quantity, location and timing",
               "Regulation of freshwater and coastal water quality",
               "Formation, protection and decontamination of soils",
               "Regulation of hazards and extreme events",
               "Regulation of organisms detrimental to humans",
               "Energy", "Food and Feed", "Materials and assistance",
               "Medicinal, biochemical and genetic resources",
               "Learning and inspiration",
               "Physical and psychological experiences",
               "Supporting identities", "Maintenance of options"),
  ncp_num  = 1:18,
  ncp_short = c("NCP 1: Habitat", "NCP 2: Pollination", "NCP 3: Air quality",
                "NCP 4: Climate", "NCP 5: Ocean acidification", "NCP 6: Water quantity",
                "NCP 7: Water quality", "NCP 8: Soils", "NCP 9: Hazards",
                "NCP 10: Pests", "NCP 11: Energy", "NCP 12: Food",
                "NCP 13: Materials", "NCP 14: Medicine", "NCP 15: Learning",
                "NCP 16: Experience", "NCP 17: Identity", "NCP 18: Options")
)

ncp_pattern <- paste(ncp_lookup$ncp_full, collapse = "|")
#------------------------------------------------------------------------------.


## 3.2. Derive evidence class from emp_inter ##################################.
# "empirical" -> Empirical link ; "interpretative" -> Inferred from discussion.
d <- d %>%
  mutate(evidence = case_when(
    str_trim(emp_inter) == "empirical"      ~ "Empirical link",
    str_trim(emp_inter) == "interpretative" ~ "Inferred from discussion",
    TRUE ~ NA_character_
  ))
#------------------------------------------------------------------------------.


## 3.3. Split NCPs and pathways, keep evidence ################################.
ncp_path_clean <- d %>%
  mutate(ncp_impact = str_extract_all(ncp_impact, ncp_pattern)) %>%
  mutate(pathways   = strsplit(pathways, ", ")) %>%
  unnest(pathways) %>%
  unnest(ncp_impact) %>%
  distinct(paper_id, ncp_impact, pathways, evidence) %>%
  left_join(ncp_lookup, by = c("ncp_impact" = "ncp_full"))
#------------------------------------------------------------------------------.


## 3.4. Count occurrences per NCP x pathway x evidence ########################.
ncp_path_count <- ncp_path_clean %>%
  filter(!is.na(evidence),
         !is.na(pathways),
         !pathways %in% c("Not specified", "None")) %>%
  group_by(ncp_short, pathways, evidence) %>%
  summarise(count = n(), .groups = "drop")
#------------------------------------------------------------------------------.


## 3.5. Complete the grid so all NCPs / pathways / panels are shown ###########.
pathway_order <- c("Resource competition/cooperation",
                   "Food insecurity & price shocks",
                   "Loss of income & living standards",
                   "Involuntary (im)mobility",
                   "Tactical aspects",
                   "Other")

ncp_path_complete <- ncp_path_count %>%
  complete(ncp_short = ncp_lookup$ncp_short,
           pathways  = pathway_order,
           evidence  = c("Empirical link", "Inferred from discussion")) %>%
  mutate(
    ncp_short = factor(ncp_short, levels = ncp_lookup$ncp_short),
    pathways  = factor(pathways,  levels = rev(pathway_order)),
    evidence  = factor(evidence,  levels = c("Empirical link",
                                             "Inferred from discussion"))
  ) %>%
  filter(!is.na(pathways), !is.na(evidence))   # guard: never render a stray NA panel/column
#------------------------------------------------------------------------------.


## 3.6. Summary numbers for the Results text (computed, not hard-coded) #######.
ncp_evidence <- d %>%
  mutate(ncp_impact = str_extract_all(ncp_impact, ncp_pattern)) %>%
  unnest(ncp_impact) %>%
  filter(!is.na(evidence)) %>%
  distinct(paper_id, ncp_impact, evidence) %>%
  left_join(ncp_lookup, by = c("ncp_impact" = "ncp_full"))

n_links    <- nrow(ncp_evidence)
n_measured <- sum(ncp_evidence$evidence == "Empirical link")
n_inferred <- sum(ncp_evidence$evidence == "Inferred from discussion")
n_ncp      <- n_distinct(ncp_evidence$ncp_num)
n_articles <- n_distinct(ncp_evidence$paper_id)
pct_art    <- round(100 * n_articles / number_studies)

cat(sprintf(
  "NCP links: %d | measured: %d | inferred: %d | NCPs: %d of 18 | articles: %d (%d%% of %d)\n",
  n_links, n_measured, n_inferred, n_ncp, n_articles, pct_art, number_studies))
#------------------------------------------------------------------------------.


# =============================================================================.
# 4. VISUALIZATION ####
# =============================================================================.
bubble_colors <- c("#A0A0A0", "#C99A82", "#C0805F", "#9E5A5A")

ncp_path_split <-
  ggplot(ncp_path_complete,
         aes(x = ncp_short, y = pathways, size = count, fill = count)) +
  geom_point(shape = 21, color = "#A0A0A0", alpha = 1) +
  geom_text(aes(label = count),
            size = 14, fontface = "bold", color = "white") +
  facet_wrap(~ evidence, ncol = 1) +
  scale_fill_gradientn(colors = colorRampPalette(bubble_colors)(50),
                       na.value = NA) +
  scale_size(range = c(15, 40)) +
  scale_x_discrete(position = "bottom") +
  scale_y_discrete(drop = FALSE) +
  labs(x = NULL, y = NULL) +
  theme_light() +
  theme(
    panel.grid       = element_blank(),
    axis.ticks.x     = element_line(color = "black", linewidth = 2),
    axis.ticks.length.x = unit(15, "pt"),
    axis.text.x      = element_text(angle = 90, hjust = 1, vjust = 0.5,
                                    size = 48, color = "black"),
    axis.text.y      = element_text(size = 48, color = "black"),
    axis.ticks.y     = element_line(color = "black", linewidth = 2),
    axis.ticks.length = unit(15, "pt"),
    strip.text       = element_text(size = 48, face = "bold"),
    strip.background = element_rect(fill = "#7A7570", color = "#7A7570",
                                    linewidth = 2),
    legend.position  = "none"
  )

# NOTE: fonts/sizes mirror your Figure 2 export (large canvas). Tune width/height
# if you prefer a tighter panel; dpi 600 for the final version.
ggsave("Fig_3_ncp_path_split.png",
       ncp_path_split,
       width = 30, height = 34, dpi = 300, bg = "white", limitsize = FALSE)
ggsave("Figure_3.eps",
       ncp_path_split,
       width = 30, height = 34, device = "eps", bg = "white", limitsize = FALSE)
#------------------------------------------------------------------------------.

