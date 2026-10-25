# 1. Setup ---------------------------------------------------------------------
pacman::p_load(tidyverse, vegan)

# add my fav font
font_add("Times New Roman", "/Library/Fonts/Times New Roman.ttf")
showtext_auto()

#create objects
meta_clean_file <- "data/processed/2025_sample_metadata_clean.csv"
reads_file      <- "data/raw/eDNA_explorer/eDNA Results/18S_Euk/decontaminated/18S_Euk_total_reads_decontaminated.tsv"
stopifnot(file.exists(meta_clean_file), file.exists(reads_file))

# 2. Read ----------------------------------------------------------------------
meta <- read_csv(meta_clean_file, show_col_types = FALSE) %>%
  # CSVs don't remember factor order, so set it again
  mutate(filter_type = factor(filter_type,
                              levels = c("1.2 \u00b5m", "5 \u00b5m", "50 \u00b5m tow")))

raw <- read_tsv(reads_file, show_col_types = FALSE)

# 3. Pivot longer --------------------------------------------------------------
long <- raw %>%
  pivot_longer(cols = -c(taxonomic_path, common_name),
               names_to = "sample", values_to = "reads")

# 4. Join metadata -------------------------------------------------------------
# Check keys first: sequenced samples with no metadata (should be 0 rows)
long %>% distinct(sample) %>% anti_join(meta, by = "sample")

n_before <- nrow(long)
long <- long %>% left_join(meta, by = "sample")
stopifnot(nrow(long) == n_before)

# Reads per sample, by area and filter type
long %>%
  group_by(sample, area, filter_type) %>%
  summarise(total_reads = sum(reads), .groups = "drop") %>%
  group_by(area, filter_type) %>%
  summarise(n = n(), median_reads = median(total_reads),
            min_reads = min(total_reads), .groups = "drop")

# 5. Taxonomy ------------------------------------------------------------------
# taxonomic_path is one string: "Kingdom;Phylum;Class;Order;Family;Genus;Species".
# Split it into one column per rank so we can filter on any level.
ranks <- c("kingdom", "phylum", "class", "order", "family", "genus", "species")

tax <- raw %>%
  select(taxonomic_path) %>%
  separate(taxonomic_path, into = ranks, sep = ";",
           fill = "right", extra = "drop", remove = FALSE) %>%
  mutate(across(all_of(ranks), ~ na_if(.x, "NA")))   # the text "NA" -> real NA

# Sort each taxon into a category. case_when() uses the FIRST rule that's TRUE,
# so order matters: specific rules first, "everything else" last.
tax <- tax %>%
  mutate(category = case_when(
    str_starts(kingdom, "unassigned")                  ~ "Unassigned",
    kingdom == "Eukaryota"                             ~ "Unresolved eukaryote",
    kingdom %in% c("Bacteria", "Archaea")              ~ "Prokaryote (off-target)",
    species %in% "Homo sapiens"                        ~ "Human",
    phylum %in% "Streptophyta" & !(family %in% "Zosteraceae") ~ "Land plants",
    class %in% c("Insecta", "Arachnida", "Collembola") ~ "Terrestrial arthropods",
    TRUE                                               ~ "Marine eukaryotes"))

# Attach categories to the reads (one row per taxonomic_path, so no duplication)
n_before <- nrow(long)
long <- long %>% left_join(tax, by = "taxonomic_path")
stopifnot(nrow(long) == n_before, !any(is.na(long$category)))

# What share of all reads falls in each category?
long %>%
  group_by(category) %>%
  summarise(reads = sum(reads)) %>%
  mutate(pct = round(100 * reads / sum(reads), 1)) %>%
  arrange(desc(reads))

# 6. Per sample QC ---------------------------------------------------------------
sample_qc <- long %>%
  group_by(sample, area, station, filter_type, method, date, depth_m,
           flag_delaminated, flag_valve_open) %>%
  summarise(total_reads  = sum(reads),
            marine_reads = sum(reads[category == "Marine eukaryotes"]),
            .groups = "drop") %>%
  mutate(pct_marine = round(100 * marine_reads / total_reads, 1))

summary(sample_qc$marine_reads)

# The weakest samples
sample_qc %>%
  arrange(marine_reads) %>%
  select(sample, filter_type, marine_reads, pct_marine) %>%
  head(8)

# 7. Clean community table ------------------------------------------------------------
min_reads_per_detection <- 10   # taxon x sample cells below this become 0

clean_long <- long %>%
  filter(category == "Marine eukaryotes") %>%
  mutate(reads = if_else(reads < min_reads_per_detection, 0, reads)) %>%
  group_by(taxonomic_path) %>%
  filter(sum(reads) > 0) %>%          # drop taxa that are now zero everywhere
  ungroup()

# Excluding penicillium
exclude_samples <- c("JDFB_72_092325")   # 99% Penicillium = contamination??
exclude_genera  <- c("Penicillium")      

clean_long <- clean_long %>%
  filter(!(sample %in% exclude_samples),
         !(genus %in% exclude_genera))

n_distinct(clean_long$taxonomic_path)   # number of taxa

# Taxa per sample
taxa_per_sample <- clean_long %>%
  filter(reads > 0) %>%
  count(sample, filter_type, name = "n_taxa") %>%
  arrange(n_taxa)

taxa_per_sample

# 8. Alpha diversity -------------------------------------------------------------
# vegan wants a "community matrix": one ROW per sample, one COLUMN per taxon,
# cells = reads. This is the wide format again, but the other way round.
comm <- clean_long %>%
  select(sample, taxonomic_path, reads) %>%
  pivot_wider(names_from = taxonomic_path, values_from = reads, values_fill = 0) %>%
  column_to_rownames("sample") %>%      # sample names become row names
  as.matrix()


# Rarefy: subsample every sample to the same number of reads
rare_depth <- 7000
keep <- rowSums(comm) >= rare_depth
rownames(comm)[!keep]                    # who gets dropped (should be JDFB_28)

set.seed(2025)                           # makes the random subsample repeatable
comm_rare <- rrarefy(comm[keep, ], sample = rare_depth)
range(rowSums(comm_rare))                # every sample should now be 7000

# Richness = number of taxa present; Shannon = richness + evenness
alpha <- tibble(sample   = rownames(comm_rare),
                richness = specnumber(comm_rare),
                shannon  = diversity(comm_rare, index = "shannon")) %>%
  left_join(sample_qc, by = "sample")    # add station, depth, date, filter...

summary(alpha$richness)

# 9. Depth profiles: one panel per month, one line per station cast -----------------

# isolate partridge bank for the baseline
alpha_pb <- alpha %>%
  left_join(meta %>% select(sample, habitat), by = "sample") %>%
  filter(area == "Partridge Bank", method != "SeQuench tow") %>%
  mutate(month   = month(date, label = TRUE, abbr = TRUE),       # Jun, Jul, ...
         station = fct_reorder(station, depth_m, .fun = max))    # shallow -> deep

# How balanced is the design?
count(alpha_pb, month)
count(alpha_pb, station, habitat)

# One theme and one colour set, reused by every plot -> consistent poster figures
theme_poster <- theme_bw(base_size = 26) +
  theme(text = element_text(family = "Times New Roman"),
        panel.grid.minor = element_blank())

# Technical replicates (e.g. JDFB_24/25, both PBS 47 m) would make a sideways
# zigzag at one depth, so average samples taken at the same station, date and depth
profile_data <- alpha_pb %>%
  group_by(month, station, date, depth_m) %>%
  summarise(richness = mean(richness), n_reps = n(), .groups = "drop") %>%
  arrange(station, date, depth_m)        # geom_path() connects rows in THIS order

station_cols <- c(PBKN = "#E8A302FF", PBKS = "#00EBF0FF", PBSH = "#CD2990",
                  PBN  = "#6495ED", PBS  = "#E2DD4DFF", PBW  = "#EE6A50",
                  DW1  = "#006400", DW2  = "#191970")

p_profile <- ggplot(profile_data, aes(x = richness, y = depth_m, colour = station)) +
  geom_path(aes(group = interaction(station, date)), linewidth = 0.9) +
  geom_point(size = 2.2) +
  facet_wrap(~ month, nrow = 1) +
  scale_x_continuous(position = "top") +   # x-axis on top, like a CTD profile
  scale_y_reverse() +                      # surface at the top
  scale_colour_manual(values = station_cols) +
  labs(x = "Taxon richness (rarefied)", y = "Depth (m)", colour = "Station") +
  theme_poster

p_profile

ggsave("output/18S_richness_depth_profiles.png", p_profile,
       width = 13, height = 6.5, dpi = 300, bg = "white")

# 10. August community composition: top 20 taxa by depth -------------------------
aug_stations <- c("PBN", "PBW", "PBS", "DW1", "DW2")   # also sets the panel order
n_top <- 20

# August samples at these stations, with a readable taxon name and
# each taxon's share of its sample's reads
aug <- clean_long %>%
  filter(month(date) == 8, station %in% aug_stations) %>%
  mutate(taxon = coalesce(class, phylum, kingdom)) %>% # use this to revert back: mutate(taxon = coalesce(species, genus, family, order, class, phylum, kingdom)
  group_by(sample) %>%
  mutate(rel_abund = reads / sum(reads)) %>%
  ungroup()

# The 20 taxa with the most reads across these August samples
top_taxa <- aug %>%
  group_by(taxon) %>%
  summarise(reads = sum(reads)) %>%
  slice_max(reads, n = n_top) %>%
  pull(taxon)

top_taxa

# Lump everything else into "Other", then one bar per station x depth
bar_data <- aug %>%
  mutate(taxon = if_else(taxon %in% top_taxa, taxon, "Other")) %>%
  group_by(station, depth_m, sample, taxon) %>%
  summarise(rel_abund = sum(rel_abund), .groups = "drop") %>%   # add up the "Other"s
  group_by(station, depth_m, taxon) %>%
  summarise(rel_abund = mean(rel_abund), .groups = "drop") %>%  # average any reps
  mutate(taxon   = factor(taxon, levels = c(top_taxa, "Other")),
         station = factor(station, levels = aug_stations),
         depth_f = factor(depth_m, levels = sort(unique(depth_m), decreasing = TRUE)))

# 20 distinguishable colours
taxa_cols <- c("#F3C300", "#875692", "#F38400", "#A1CAF1", "#BE0032",
               "#C2B280", "#848482", "#008856", "#E68FAC", "#0067A5",
               "#F99379", "#604E97", "#F6A600", "#B3446C", "#DCD300",
               "#882D17", "#8DB600", "#654522", "#E25822", "#2B3D26",
               "#191970")
names(taxa_cols) <- levels(bar_data$taxon)

p_bar <- ggplot(bar_data, aes(x = rel_abund, y = depth_f, fill = taxon)) +
  geom_col(width = 0.85, colour = "white", linewidth = 0.1) +
  facet_wrap(~ station, nrow = 1, scales = "free_y") +
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
                     labels = scales::label_number(drop0trailing = TRUE),
                     expand = c(0, 0), position = "top") +
  scale_fill_manual(values = taxa_cols) +
  labs(x = "Relative read abundance", y = "Depth (m)", fill = NULL) +
  theme_poster +
  theme(legend.position = "bottom", legend.text = element_text(size = 18)) +
  guides(fill = guide_legend(nrow = 5))

p_bar

ggsave("output/18S_August_top20_by_depth.png", p_bar,
       width = 15, height = 8, dpi = 300, bg = "white")

# 11. Beta diversity and checks for NMDS----------------------------------------

# Same Partridge Bank Niskin samples as the alpha plots, minus JDFB_51
# (one barnacle dominates it, so it shares almost nothing with any other
#  sample and squashes the NMDS into a line)
beta_samples <- setdiff(alpha_pb$sample, "JDFB_51_082725")

comm_pb <- comm[beta_samples, ]
comm_pb <- comm_pb[, colSums(comm_pb) > 0]    # drop taxa absent from these samples
dim(comm_pb)                                   # 87 samples x ~614 taxa

# Reads -> proportions (each row sums to 1), then square-root so a few
# super-abundant dinoflagellates don't dominate the distances
comm_rel  <- decostand(comm_pb, method = "total")
comm_sqrt <- sqrt(comm_rel)

set.seed(2025)                                 # NMDS starts from random positions
nmds <- metaMDS(comm_sqrt, distance = "bray", k = 2,
                trymax = 100, autotransform = FALSE, trace = 0)

nmds$stress       # want < 0.2
stressplot(nmds)  # points should hug the red line

# 12. NMDS plot ---------------------------------------------------------------------
# Pull out each sample's position on the map and attach its metadata
nmds_scores <- scores(nmds, display = "sites") %>%
  as_tibble(rownames = "sample") %>%
  left_join(alpha_pb, by = "sample")

nmds_scores <- nmds_scores %>%
  mutate(depth_zone = if_else(depth_m <= 20, "\u2264 20 m", "> 20 m"))

month_cols <- c(Jun = "#8DB600", Jul = "#191970", Aug = "#6495ED", Sep = "#CD2990")

p_nmds <- ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2, colour = month)) +
  stat_ellipse(aes(group = month), level = 0.68, linewidth = 0.8) +
  geom_point(aes(shape = depth_zone), size = 3.5, alpha = 0.70) +
  scale_colour_manual(values = month_cols) +
  annotate("text", x = Inf, y = -Inf, hjust = 1.1, vjust = -0.6, size = 5,
           label = paste0("Stress = ", round(nmds$stress, 2)),
           family = "Times New Roman") +
  coord_equal() +
  labs(colour = "Month", shape = "Depth") +
  theme_poster

p_nmds

ggsave("output/18S_NMDS_month.png", p_nmds,
       width = 8, height = 7, dpi = 300, bg = "white")

# 13. PERMANOVA -------------------------------------------------------------------
# Distance matrix: same transformation as the NMDS
bray <- vegdist(comm_sqrt, method = "bray")

# Metadata in EXACTLY the same row order as the distance matrix
beta_meta <- alpha_pb[match(rownames(comm_sqrt), alpha_pb$sample), ]
stopifnot(identical(beta_meta$sample, rownames(comm_sqrt)))

# How much community variation do month, depth and station each explain?
set.seed(2025)
perm <- adonis2(bray ~ month + depth_m + station, data = beta_meta,
                by = "margin", permutations = 999)
perm

# Assumption check: do months differ in how SPREAD OUT they are?
disp <- betadisper(bray, beta_meta$month)
set.seed(2025)
permutest(disp, permutations = 999)

# checking with jaccard instead of bray-curtis to compare
jac <- vegdist(comm_sqrt > 0, method = "jaccard")
set.seed(2025)
perm_jac <- adonis2(jac ~ month + depth_m + station, data = beta_meta,
                    by = "margin", permutations = 999)
perm_jac
