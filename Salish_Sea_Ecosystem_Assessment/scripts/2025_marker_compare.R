# 1. Setup ---------------------------------------------------------------------
pacman::p_load(tidyverse)

meta <- read_csv("data/processed/2025_sample_metadata_clean.csv",
                 show_col_types = FALSE)

files <- c("12S MiFish" = "data/raw/eDNA_explorer/eDNA Results/12S_MiFish_U/decontaminated/12S_MiFish_U_total_reads_decontaminated.tsv",
           "12S vert"   = "data/raw/eDNA_explorer/eDNA Results/vert12S/decontaminated/vert12S_total_reads_decontaminated.tsv",
           "CO1"        = "data/raw/eDNA_explorer/eDNA Results/CO1_Metazoa/decontaminated/CO1_Metazoa_total_reads_decontaminated.tsv",
           "18S"        = "data/raw/eDNA_explorer/eDNA Results/18S_Euk/decontaminated/18S_Euk_total_reads_decontaminated.tsv")
stopifnot(all(file.exists(files)))       # <- edit paths above until this passes

ranks     <- c("kingdom", "phylum", "class", "order", "family", "genus", "species")
min_reads <- 10                          # same detection threshold as every other script

# 2. What counts as "target" for each marker -------------------------------------------
# Same rules as the individual marker scripts
is_target <- function(tax, marker) {
  tax %>% mutate(target = case_when(
    str_starts(kingdom, "unassigned") | kingdom %in% "Eukaryota" ~ FALSE,
    marker %in% c("12S MiFish", "12S vert") ~
      class %in% c("Actinopteri", "Chondrichthyes") |
      family %in% c("Phocidae", "Phocoenidae", "Otariidae", "Delphinidae",
                    "Balaenopteridae", "Eschrichtiidae"),
    marker == "CO1" ~
      kingdom %in% "Animalia" &
      !(class %in% c("Insecta", "Arachnida", "Collembola", "Mammalia")) &
      !(family %in% "Lumbricidae"),
    marker == "18S" ~
      !(kingdom %in% c("Bacteria", "Archaea")) &
      !(phylum %in% "Streptophyta" & !(family %in% "Zosteraceae")) &
      !(class %in% c("Insecta", "Arachnida", "Collembola")) &
      !(genus %in% "Penicillium") & !(species %in% "Homo sapiens"),
    TRUE ~ FALSE))
}

# 3. One function: read a marker file -> one row per sample ----------------------------
summarise_marker <- function(file, marker) {
  raw <- read_tsv(file, show_col_types = FALSE)
  
  tax <- raw %>%
    select(taxonomic_path) %>%
    separate(taxonomic_path, into = ranks, sep = ";",
             fill = "right", extra = "drop", remove = FALSE) %>%
    mutate(across(all_of(ranks), ~ na_if(.x, "NA")),
           kingdom = if_else(kingdom %in% "Plantae" & phylum %in% "Chordata",
                             "Animalia", kingdom),                  # Liparis fix
           taxon   = coalesce(species, genus, family, order, class, phylum, kingdom)) %>%
    is_target(marker)
  
  raw %>%
    pivot_longer(-c(taxonomic_path, common_name),
                 names_to = "sample", values_to = "reads") %>%
    left_join(tax, by = "taxonomic_path") %>%
    group_by(sample) %>%
    summarise(total_reads  = sum(reads),
              target_reads = sum(reads[target]),
              n_taxa       = n_distinct(taxon[target & reads >= min_reads]),
              .groups = "drop") %>%
    mutate(marker = marker)
}

# Try it on one marker first
summarise_marker(files[["CO1"]], "CO1")

# 4. Run the function on all four markers --------------------------------------------
perf_raw <- map2(files, names(files), summarise_marker) %>%
  list_rbind()

names(perf_raw)    # sample, total_reads, target_reads, n_taxa, marker

# 5. Add metadata and filter groups ----------------------------------------------------
pass_threshold <- 1000      # target reads needed to call a sample "passed" (all markers)

perf <- perf_raw %>%                         # always start from perf_raw
  left_join(meta, by = "sample") %>%
  filter(!is.na(method), method != "Field blank") %>%
  mutate(
    filter_group = case_when(
      method == "SeQuench tow"   ~ "50 \u00b5m\ntow",
      filter_um == 5             ~ "5 \u00b5m",
      area != "Partridge Bank"   ~ "1.2 \u00b5m\nrep",
      TRUE                       ~ "1.2 \u00b5m\nPB"),
    filter_group = factor(filter_group,
                          levels = c("1.2 \u00b5m\nPB", "1.2 \u00b5m\nrep",
                                     "5 \u00b5m", "50 \u00b5m\ntow")),
    marker = factor(marker, levels = c("12S MiFish", "12S vert", "CO1", "18S")),
    status = factor(if_else(target_reads >= pass_threshold, "Passed", "Failed"),
                    levels = c("Passed", "Failed")))

# Pass/fail counts: the numbers behind the figure
perf %>%
  count(marker, filter_group, status) %>%
  pivot_wider(names_from = status, values_from = n, values_fill = 0)

# 6. Plots ---------------------------------------------------------------------
# 6. Figure -------------------------------------------------------------------------
pacman::p_load(scales, patchwork)        # patchwork stacks plots; scales formats axes
pass_cols <- c(Passed = "#0072B2", Failed = "#D55E00")

theme_poster <- theme_bw(base_size = 32) +
  theme(text = element_text(family = "Times New Roman"),
        panel.grid.minor = element_blank())

# A: target reads per sample, with the pass threshold
p_pass <- ggplot(perf, aes(x = filter_group, y = target_reads + 1, colour = status)) +
  geom_hline(yintercept = pass_threshold, linetype = "dashed", colour = "grey40") +
  geom_jitter(width = 0.2, height = 0, size = 2, alpha = 0.75) +
  facet_wrap(~ marker, nrow = 1) +
  scale_y_log10(labels = label_comma(), breaks = 10^(0:5)) +
  scale_colour_manual(values = pass_cols) +
  labs(x = NULL, y = "Target reads\n(+1, log scale)", colour = NULL) +
  theme_poster +
  theme(legend.position = "top")

# B: target taxa per sample (dots coloured by pass/fail from A)
p_taxa <- ggplot(perf, aes(x = filter_group, y = n_taxa)) +
  geom_boxplot(outlier.shape = NA, fill = "grey95",
               width = 0.5,
               linewidth = 0.4,
               colour = "grey40") +
  geom_jitter(aes(colour = status), width = 0.15, height = 0, size = 1.6, alpha = 0.7) +
  facet_wrap(~ marker, nrow = 1, scales = "free_y") +
  scale_colour_manual(values = pass_cols, guide = "none") +
  expand_limits(y = 0) +
  labs(x = NULL, y = "Target taxa\nper sample") +
  theme_poster

# Stack them and label A / B
p_cross <- p_pass / p_taxa + plot_annotation(tag_levels = "A")
p_cross

ggsave("output/figures/cross_marker_filter_performance.png", p_cross,
       width = 14, height = 10, dpi = 300, bg = "white")
