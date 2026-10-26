# 1. Setup ---------------------------------------------------------------------
pacman::p_load(tidyverse)

meta <- read_csv("data/processed/2025_sample_metadata_clean.csv", show_col_types = FALSE)
reads_file <- "data/raw/eDNA_explorer/eDNA Results/12S_MiFish_U/decontaminated/12S_MiFish_U_total_reads_decontaminated.tsv"
stopifnot(file.exists(reads_file))

min_reads <- 10
ranks <- c("kingdom", "phylum", "class", "order", "family", "genus", "species")

theme_poster <- theme_bw(base_size = 32) +
  theme(text = element_text(family = "Times New Roman"),
        panel.grid.minor = element_blank())

# 2. Read and keep only the six PBN tows --------------------------------------------
raw <- read_tsv(reads_file, show_col_types = FALSE)

tows <- raw %>%
  select(taxonomic_path, common_name, starts_with("PBN_TOW")) %>%   # 6 tow columns only
  pivot_longer(starts_with("PBN_TOW"), names_to = "sample", values_to = "reads") %>%
  filter(reads >= min_reads) %>%                                     # detections only
  separate(taxonomic_path, into = ranks, sep = ";",
           fill = "right", extra = "drop", remove = FALSE) %>%
  mutate(across(all_of(ranks), ~ na_if(.x, "NA")))

# 3. Keep fish + mammals identified to at least family; drop errors & contaminants ----
exclude_families <- c("Labridae",            # Cape wrasse (South Africa): database error
                      "Cheimarrichthyidae",  # NZ torrentfish: database error
                      "Suidae", "Hominidae") # pig, human: contamination

tows <- tows %>%
  filter(class %in% c("Actinopteri", "Chondrichthyes", "Mammalia"),
         !(family %in% exclude_families),
         !is.na(family))          # drops vague names: Chordata, Actinopteri, Perciformes...

# 4. Fix names that are plausible only at a higher rank ------------------------------
tows <- tows %>%
  mutate(
    species = case_when(
      species %in% c("Myoxocephalus stelleri",   # Asian sculpin -> keep as genus
                     "Lycodes mcallisteri")       # -> keep as genus
      ~ NA_character_,
      TRUE                             ~ species),
    genus   = if_else(family == "Clupeidae", "Clupea", genus),            # all herring ->
    species = if_else(family == "Clupeidae", "Clupea pallasii", species), # Pacific herring
    tax_rank = case_when(!is.na(species) ~ "species",
                         !is.na(genus)   ~ "genus",
                         TRUE            ~ "family"),
    taxon    = coalesce(species, genus, family))

# 5. Collapse nested names within each sample ----------------------------------------
# e.g. drop "Pholis" (genus) if "Pholis laeta" (species) was detected in the SAME sample
tows <- tows %>%
  group_by(sample) %>%
  mutate(redundant = case_when(
    tax_rank == "genus"  ~ genus  %in% genus[tax_rank == "species"],
    tax_rank == "family" ~ family %in% family[tax_rank %in% c("genus", "species")],
    TRUE                 ~ FALSE)) %>%
  ungroup()

tows %>% filter(redundant) %>% select(sample, taxon, reads)   # what's being collapsed

tows <- tows %>%
  filter(!redundant) %>%
  group_by(sample, class, family, genus, taxon, tax_rank) %>%
  summarise(reads = sum(reads), .groups = "drop") %>%
  left_join(select(meta, sample, depth_m), by = "sample") %>%
  mutate(replicate = str_extract(sample, "STBD|PORT|BTM"))

n_distinct(tows$taxon)   # 19

# Detections per depth (out of 3 replicates)
tows %>%
  group_by(taxon, depth_m) %>%
  summarise(n_reps = n_distinct(sample), .groups = "drop") %>%
  pivot_wider(names_from = depth_m, values_from = n_reps, values_fill = 0,
              names_prefix = "reps_")

# 6. Common names for labels -----------------------------------------------------------
common_names <- tribble(
  ~taxon,                    ~common,
  "Ammodytidae",             "Sand lances",
  "Artedius harringtoni",    "Scalyhead sculpin",
  "Beringraja",              "Skates",
  "Clupea pallasii",         "Pacific herring",
  "Enophrys bison",          "Buffalo sculpin",
  "Hexagrammos decagrammus", "Kelp greenling",
  "Hexagrammos stelleri",    "Whitespotted greenling",
  "Hydrolagus colliei",      "Spotted ratfish",
  "Icelinus borealis",       "Northern sculpin",
  "Lepidopsetta",            "Rock soles",
  "Lumpenus sagitta",        "Snake prickleback",
  "Lycodes",                 "Eelpouts",
  "Microstomus pacificus",   "Dover sole",
  "Myoxocephalus",           "Sculpins",
  "Oncorhynchus keta",       "Chum salmon",
  "Oncorhynchus kisutch",    "Coho salmon",
  "Phoca",                   "Seals",
  "Pholis laeta",            "Crescent gunnel",
  "Pholis ornata",           "Saddleback gunnel")

stopifnot(all(unique(tows$taxon) %in% common_names$taxon))   # every taxon has a name

# 7. Replicate counts and depth grouping ------------------------------------------------
reps <- tows %>%
  group_by(taxon, tax_rank) %>%
  summarise(reps_3  = n_distinct(sample[depth_m == 3]),
            reps_15 = n_distinct(sample[depth_m == 15]),
            .groups = "drop") %>%
  mutate(occurrence = case_when(reps_3 > 0 & reps_15 > 0 ~ "Both depths",
                                reps_3 > 0                ~ "3 m only",
                                TRUE                      ~ "15 m only"),
         occurrence = factor(occurrence, levels = c("Both depths", "3 m only", "15 m only"))) %>%
  left_join(common_names, by = "taxon") %>%
  arrange(occurrence, desc(reps_3 + reps_15), common)   # most consistent first

row_levels <- rev(reps$taxon)

# 8. One tile per taxon x replicate, including empty ones ------------------------------
heat <- tows %>%
  complete(nesting(taxon), sample) %>%       # add rows for taxa NOT detected in a sample
  select(taxon, sample, reads) %>%
  mutate(depth     = factor(if_else(str_detect(sample, "_15m_"), "15 m", "3 m"),
                            levels = c("3 m", "15 m")),
         replicate = factor(str_extract(sample, "STBD|PORT|BTM"),
                            levels = c("STBD", "PORT", "BTM"))) %>%
  left_join(select(reps, taxon, occurrence), by = "taxon") %>%
  mutate(taxon = factor(taxon, levels = row_levels))

# The "x/3" column
rep_labels <- reps %>%
  pivot_longer(c(reps_3, reps_15), names_to = "depth", values_to = "n") %>%
  mutate(depth     = factor(if_else(depth == "reps_3", "3 m", "15 m"), levels = c("3 m", "15 m")),
         replicate = "Reps",
         label     = paste0(n, "/3"),
         taxon     = factor(taxon, levels = row_levels))

# Labels: "Common name (Scientific name)", italic for genus/species
label_fun <- function(x) {
  common <- setNames(reps$common, reps$taxon)
  rank   <- setNames(reps$tax_rank, reps$taxon)
  lapply(x, function(t) {
    if (rank[[t]] %in% c("species", "genus")) bquote(.(common[[t]]) ~ "(" * italic(.(t)) * ")")
    else                                       bquote(.(common[[t]]) ~ "(" * .(t) * ")")
  }) %>% as.expression()
}

# 9. Plot ----------------------------------------------------------------------------
p_tows <- ggplot(heat, aes(x = replicate, y = taxon)) +
  geom_tile(aes(fill = reads), colour = "white", linewidth = 0.6) +
  facet_grid(occurrence ~ depth, scales = "free_y", space = "free_y") +
  scale_x_discrete(position = "top") +
  scale_y_discrete(labels = label_fun) +
  scale_fill_viridis_c(trans = "log10", option = "mako", direction = -1,
                       na.value = "grey95", labels = scales::label_comma(), name = "Reads") +
  labs(x = NULL, y = NULL) +
  theme_poster +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(colour = "grey75", linewidth = 0.5),   # thin, light frame
        strip.background = element_rect(fill = "grey92", colour = NA),     # label boxes, no outline
        strip.text.y = element_text(angle = 0, hjust = 0),
        axis.text.x.top = element_text(angle = 45, hjust = 0, vjust = 0))

p_tows

ggsave("output/figures/12S_MiFish/MiFish_PBN_tows_replicates.png", p_tows,
       width = 10, height = 8, dpi = 300, bg = "white")

# 10. Relative abundance by depth (replicates averaged) ------------------------------
tows_all <- tows      # keep every detection (incl. family/genus-level) for read proportions

fish_groups <- tows_all %>%
  mutate(group = case_when(
    family == "Clupeidae"      ~ "Pacific herring",
    family == "Ammodytidae"    ~ "Sand lance",
    family == "Salmonidae"     ~ "Salmon",
    family == "Cottidae"       ~ "Sculpins",
    family == "Pholidae"       ~ "Gunnels",
    family == "Hexagrammidae"  ~ "Greenlings",
    family == "Pleuronectidae" ~ "Flatfishes",
    family == "Stichaeidae"    ~ "Pricklebacks",
    TRUE                       ~ "Other"))        # eelpouts, ratfish, skates, seals

pie_tows <- fish_groups %>%
  mutate(depth = if_else(str_detect(sample, "_15m_"), "15 m", "3 m")) %>%
  group_by(depth, sample, group) %>%
  summarise(reads = sum(reads), .groups = "drop") %>%
  group_by(sample) %>%
  mutate(prop = reads / sum(reads)) %>%                              # 1) proportions WITHIN each replicate
  ungroup() %>%
  complete(nesting(depth, sample), group, fill = list(prop = 0)) %>%  # absent group = 0, not missing
  group_by(depth, group) %>%
  summarise(mean_prop = mean(prop), .groups = "drop") %>%            # 2) average the 3 replicates
  filter(mean_prop > 0)

group_levels <- c("Pacific herring", "Sand lance", "Salmon", "Sculpins", "Gunnels",
                  "Greenlings", "Flatfishes", "Pricklebacks", "Other")
group_cols <- c("Pacific herring" = "#0072B2", "Sand lance"   = "#A1CAF1",
                "Salmon"          = "#D55E00", "Sculpins"     = "#604E97",
                "Gunnels"         = "#F6A600", "Greenlings"   = "#F99379",
                "Flatfishes"      = "#BE0032", "Pricklebacks" = "#8DB600",
                "Other"           = "grey80")

pie_tows <- pie_tows %>%
  mutate(group = factor(group, levels = group_levels),
         depth = factor(depth, levels = c("3 m", "15 m")),
         label = if_else(mean_prop >= 0.05, sprintf("%.0f%%", 100 * mean_prop), ""))

p_tow_pie <- ggplot(pie_tows, aes(x = "", y = mean_prop, fill = group)) +
  geom_col(width = 1, colour = "white", linewidth = 0.6) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5),
            size = 5, family = "Times New Roman") +
  coord_polar(theta = "y") +
  facet_wrap(~ depth) +                                  # one pie per depth
  scale_fill_manual(values = group_cols) +
  labs(fill = NULL) +
  theme_void(base_size = 16, base_family = "Times New Roman") +
  theme(strip.text = element_text(size = 18, margin = margin(b = 6)))

p_tow_pie

ggsave("output/figures/12S_MiFish/MiFish_PBN_tows_pies.png", p_tow_pie,
       width = 10, height = 5, dpi = 300, bg = "white")
