# 1. Setup ---------------------------------------------------------------------
pacman::p_load(tidyverse, vegan)

# add my fav font
font_add("Times New Roman", "/Library/Fonts/Times New Roman.ttf")
showtext_auto()

#create objects
meta_clean_file <- "data/processed/2025_sample_metadata_clean.csv"
reads_file      <- "data/raw/eDNA_explorer/eDNA Results/CO1_Metazoa/decontaminated/CO1_metazoa_total_reads_decontaminated.tsv"
stopifnot(file.exists(meta_clean_file), file.exists(reads_file))

# copied these from 2025_18S_Euk.R script
# One theme and one colour set, reused by every plot -> consistent poster figures
theme_poster <- theme_bw(base_size = 32) +
  theme(text = element_text(family = "Times New Roman"),
        panel.grid.minor = element_blank())

station_cols <- c(PBKN = "#E8A302FF", PBKS = "#00EBF0FF", PBSH = "#CD2990",
                  PBN  = "#6495ED", PBS  = "#E2DD4DFF", PBW  = "#EE6A50",
                  DW1  = "#006400", DW2  = "#191970")

month_cols <- c(Jun = "#8DB600", Jul = "#191970", Aug = "#6495ED", Sep = "#CD2990")


# 2. Read, reshape, join metadata ---------------------------------------------------
meta <- read_csv(meta_clean_file, show_col_types = FALSE) %>%
  mutate(filter_type = factor(filter_type,
                              levels = c("1.2 \u00b5m", "5 \u00b5m", "50 \u00b5m tow")))

raw  <- read_tsv(reads_file, show_col_types = FALSE)
long <- raw %>%
  pivot_longer(cols = -c(taxonomic_path, common_name),
               names_to = "sample", values_to = "reads")

long %>% distinct(sample) %>% anti_join(meta, by = "sample")   # should be 0 rows

n_before <- nrow(long)
long <- long %>% left_join(meta, by = "sample")
stopifnot(nrow(long) == n_before)

# 3. Taxonomy -------------------------------------------------------------------
ranks <- c("kingdom", "phylum", "class", "order", "family", "genus", "species")

tax <- raw %>%
  select(taxonomic_path) %>%
  separate(taxonomic_path, into = ranks, sep = ";",
           fill = "right", extra = "drop", remove = FALSE) %>%
  mutate(across(all_of(ranks), ~ na_if(.x, "NA")),
         # Liparis is a snailfish AND an orchid genus; the database filed it under Plantae
         kingdom = if_else(kingdom %in% "Plantae" & phylum %in% "Chordata",
                           "Animalia", kingdom))

# CO1 targets ANIMALS, so everything non-animal is off-target here
tax <- tax %>%
  mutate(category = case_when(
    str_starts(kingdom, "unassigned")                          ~ "Unassigned",
    kingdom == "Eukaryota"                                     ~ "Unresolved eukaryote",
    kingdom != "Animalia"                                      ~ "Non-animal (off-target)",
    family %in% c("Hominidae", "Suidae", "Bovidae", "Canidae") ~ "Human/domestic mammals",
    class  %in% c("Insecta", "Arachnida", "Collembola")        ~ "Terrestrial invertebrates",
    family %in% "Lumbricidae"                                  ~ "Terrestrial invertebrates",
    TRUE                                                       ~ "Marine animals"))

n_before <- nrow(long)
long <- long %>% left_join(tax, by = "taxonomic_path")
stopifnot(nrow(long) == n_before, !any(is.na(long$category)))

long %>%
  group_by(category) %>%
  summarise(reads = sum(reads)) %>%
  mutate(pct = round(100 * reads / sum(reads), 1)) %>%
  arrange(desc(reads))

# 4. Detections (Partridge Bank, marine animals, >= 10 reads) --------------------------
min_reads_per_detection <- 10

det <- long %>%
  filter(category == "Marine animals", area == "Partridge Bank") %>%
  mutate(
    # most specific name available, and which rank it is
    taxon    = coalesce(species, genus, family, order, class, phylum),
    tax_rank = case_when(!is.na(species) ~ "species", !is.na(genus) ~ "genus",
                         !is.na(family)  ~ "family",  !is.na(order) ~ "order",
                         !is.na(class)   ~ "class",   TRUE ~ "phylum"),
    # strip database voucher codes: "Lepidonotus squamatus CMC01" -> "Lepidonotus squamatus"
    taxon    = str_remove(taxon, "\\s+[A-Z]{2,}[0-9-]*[0-9]+$")) %>%
  group_by(sample, station, date, depth_m, method, filter_type,
           phylum, class, taxon, tax_rank) %>%
  summarise(reads = sum(reads), .groups = "drop") %>%
  filter(reads >= min_reads_per_detection)

n_distinct(det$taxon)    # taxa detected
n_distinct(det$sample)   # samples with >= 1 detection

# 5. Inventory: one row per taxon ---------------------------------------------------
inventory <- det %>%
  group_by(phylum, class, taxon, tax_rank) %>%
  summarise(n_samples   = n_distinct(sample),
            n_stations  = n_distinct(station),
            niskin      = any(method != "SeQuench tow"),
            tow         = any(method == "SeQuench tow"),
            total_reads = sum(reads),
            .groups = "drop") %>%
  arrange(phylum, class, desc(n_samples))

print(inventory, n = Inf)

# Implausible / contaminant taxa to remove 
exclude_taxa <- c("Ceriodaphnia dubia", "Ceriodaphnia",   # freshwater lab test organism
                  "Armadillidium",                         # terrestrial pill bug
                  "Palinuridae")                           # not in the Salish Sea

det <- det %>%
  filter(!(taxon %in% exclude_taxa)) %>%
  mutate(taxon    = if_else(taxon == "Cliona chilensis", "Cliona", taxon),
         tax_rank = if_else(taxon == "Cliona", "genus", tax_rank))

# 6. Inventory heatmap ---------------------------------------------------------------
# Columns: kelp beds -> slopes -> deep water -> tows
col_order <- c("PBKN", "PBKS", "PBSH", "PBN", "PBS", "PBW", "DW1", "DW2", "Tow")

# Sampling effort: how many samples per column (INCLUDING ones with no detections)
effort <- long %>%
  filter(area == "Partridge Bank") %>%
  distinct(sample, station, method) %>%
  mutate(column = if_else(method == "SeQuench tow", "Tow", station)) %>%
  count(column, name = "n_total")
effort

# Detections per taxon per column, as a count and as a share of that column's samples
heat <- det %>%
  mutate(column = if_else(method == "SeQuench tow", "Tow", station)) %>%
  group_by(phylum, class, taxon, tax_rank, column) %>%
  summarise(n_det = n_distinct(sample), .groups = "drop") %>%
  left_join(effort, by = "column") %>%
  mutate(prop = n_det / n_total)

# Row order: grouped by phylum and class, most-detected first
taxon_order <- heat %>%
  group_by(phylum, class, taxon) %>%
  summarise(n = sum(n_det), .groups = "drop") %>%
  arrange(phylum, class, desc(n)) %>%
  pull(taxon)

heat <- heat %>%
  mutate(taxon  = factor(taxon, levels = rev(taxon_order)),
         column = factor(column, levels = col_order))

# Italicise genus and species names only (convention for scientific names)
italic_taxa <- unique(as.character(heat$taxon[heat$tax_rank %in% c("species", "genus")]))
italic_labels <- function(x) {
  lapply(x, function(t) if (t %in% italic_taxa) bquote(italic(.(t))) else t) %>%
    as.expression()}

# Column labels with sample effort, e.g. "PBN (27)"
col_labels <- setNames(paste0(effort$column, " (n = ", effort$n_total, ")"), effort$column)

p_inv <- ggplot(heat, aes(x = column, y = taxon, fill = prop)) +
  geom_tile(colour = "white", linewidth = 0.4) +
  geom_text(aes(label = n_det), size = 3, colour = "grey15") +
  facet_grid(phylum ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_x_discrete(position = "top", labels = col_labels, drop = FALSE) +
  scale_y_discrete(labels = italic_labels) +
  scale_fill_gradient(low = "#DEEBF7", high = "#08519C",
                      labels = scales::percent, limits = c(0, NA)) +
  labs(x = NULL, y = NULL, fill = "Samples with\ndetection") +
  theme_poster +
  theme(strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0, hjust = 1),
        panel.grid = element_blank(),
        panel.spacing = unit(2, "pt"),
        axis.text.x.top = element_text(angle = 45, hjust = 0, vjust = 0))

p_inv

ggsave("output/figures/2025_CO1_Metazoa/CO1_inventory_heatmap.png", p_inv,
       width = 9, height = 12, dpi = 300, bg = "white")

# 7. CO1 read fate per sample, by station -------------------------------------------
col_order <- c("PBKN", "PBKS", "PBSH", "PBN", "PBS", "PBW", "DW1", "DW2", "Tow")

fate_cols <- c("Marine animals (target)" = "#CD2990",   # the one colour that matters
               "Non-animal (off-target)" = "#6495ED",
               "Unresolved eukaryote"    = "grey45",
               "Unassigned"              = "grey85",
               "Contaminants"            = "#8DB600")

bar_fate <- long %>%
  filter(area == "Partridge Bank") %>%
  mutate(column = factor(if_else(method == "SeQuench tow", "Tow", station),
                         levels = col_order),
         fate = case_when(
           category == "Marine animals"          ~ "Marine animals (target)",
           category == "Non-animal (off-target)" ~ "Non-animal (off-target)",
           category == "Unassigned"              ~ "Unassigned",
           category == "Unresolved eukaryote"    ~ "Unresolved eukaryote",
           TRUE                                  ~ "Contaminants"),
         fate = factor(fate, levels = rev(names(fate_cols)))) %>%   # target at the bottom
  group_by(sample, column, depth_m, fate) %>%
  summarise(reads = sum(reads), .groups = "drop")

# Order samples by station, then shallow -> deep
sample_order <- bar_fate %>%
  distinct(sample, column, depth_m) %>%
  arrange(column, depth_m, sample) %>%
  pull(sample)

p_fate <- ggplot(bar_fate, aes(x = factor(sample, levels = sample_order),
                               y = reads, fill = fate)) +
  geom_col(position = "fill", width = 1) +
  facet_grid(~ column, scales = "free_x", space = "free_x") +
  scale_y_continuous(expand = c(0, 0), breaks = c(0, 0.5, 1)) +
  scale_fill_manual(values = fate_cols, breaks = names(fate_cols)) +
  labs(x = "Samples",
       y = "Proportion of reads", fill = NULL) +
  theme_poster +
  theme(axis.text.x  = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid   = element_blank(),
        panel.spacing = unit(3, "pt"),
        strip.text   = element_text(angle = 90, hjust = 0),   # narrow panels need vertical labels
        legend.position = "bottom")

p_fate

ggsave("output/figures/2025_CO1_metazoa/CO1_read_fate_by_station.png", p_fate,
       width = 13, height = 5.5, dpi = 300, bg = "white")
