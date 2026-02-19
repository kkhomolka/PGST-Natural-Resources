pacman::p_load(ggplot2,
               tidyr,
               dplyr,
               readxl,
               openxlsx,
               ggrepel)

# Aesthetics color palette
clrblind_pal <- c("#edbd00","#1dd2d3","#78b41f","#7487ff","#b41f78")

install.packages("showtext")
library(showtext)

font_add("Times New Roman", "C:/Windows/Fonts/times.ttf")
showtext_auto()

df<-read_excel("data_raw/2025_LLTK_Seine.xlsx")
df2 <- read_excel("data_raw/Fish_Density.xlsx")

df<- df %>%
  group_by(Site) %>%
  summarise(
    `Juvenile Salmon` = sum(`Juvenile_Salmon`, na.rm = TRUE),
    `Adult Salmon` = sum(`Adult_Salmon`, na.rm = TRUE),
    Herring = sum(Herring, na.rm = TRUE),
    `Sand Lance` = sum(Sand_Lance, na.rm = TRUE),
    Stickleback = sum(Stickleback, na.rm = TRUE),
    `Walleye Pollock` = sum(Walleye_Pollock, na.rm = TRUE))

df_long <- df %>%
  pivot_longer(
    cols = -Site,
    names_to = "Species",
    values_to = "Total")

df_long <- df_long %>%
  filter(Total > 0)

df_long <- df_long %>%
  group_by(Site) %>%
  mutate(Percent = Total / sum(Total) * 100)

df_sji<-df_long %>% 
  filter(Site == "Strawberry Island")

df_squ<-df_long %>% 
  filter(Site == "Squamish Harbor")

#Strawberry Island
ggplot(df_sji, aes(x = Site, y = Total, fill = Species)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  theme_void()

#Suquamish Harbor
ggplot(df_squ, aes(x = Site, y = Total, fill = Species)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  theme_void()

#Stacked Sites
ggplot(df_long, aes(x = Site, y = Percent, fill = Species)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  theme_void()

#Facet
ggplot(df_long, aes(x = "", y = Percent, fill = Species)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  facet_wrap(~Site) +
  theme_void()

#Facet with better aesthetics
ggplot(df_long, aes(x = "", y = Percent, fill = Species)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  facet_wrap(~Site) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 8),
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    strip.text = element_text(size = 12, face = "bold"))

#added times new roman and made the font bigger
ggplot(df_long, aes(x = "", y = Percent, fill = Species)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  facet_wrap(~Site) +
  theme_void(base_family = "Times New Roman", base_size = 24) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 24, family = "Times New Roman"),
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 24, family = "Times New Roman"),
    strip.text = element_text(size = 24, face = "bold", family = "Times New Roman")
  )


# 2. Fish Density Bar Plots ----------------------------------------------------

df_clean <- df2 %>%
  mutate(
    Bin = paste0(`Bin Depth Start m`, "-", `Bin Depth Stop m`, " m"),
    `Purse Seine Catch Density (m3)` = as.numeric(`Purse Seine Catch Density (m3)`),
    `Acoustic Fish Track Density (m3)` = as.numeric(`Acoustic Fish Track Density (m3)`)
  )

# Convert to long format so both density types can be plotted on same axis
df_long2 <- df_clean %>%
  select(Site, Bin,
         `Acoustic Fish Track Density (m3)`,
         `Purse Seine Catch Density (m3)`) %>%
  pivot_longer(
    cols = c(`Acoustic Fish Track Density (m3)`,
             `Purse Seine Catch Density (m3)`),
    names_to = "Method",
    values_to = "Density"
  ) %>%
  mutate(
    Method = recode(Method,
                    "Acoustic Fish Track Density (m3)" = "Acoustic",
                    "Purse Seine Catch Density (m3)" = "Purse Seine")
  )

# set custom colors
method_colors <- c(
  "Acoustic" = "#5d9aff",      
  "Purse Seine" = "orange"    
)

# Plot: barplot, two bins on x, density on y, one plot per site
ggplot(df_long2, aes(x = Bin, y = Density, fill = Method)) +
  geom_col(position = "dodge", na.rm = TRUE) +
  facet_wrap(~Site) +
  labs(
    x = "Depth Bin (m)",
    y = "Fish Density (per m³)",
    fill = "Method",
    title = "Fish Density by Depth Bin and Site"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(df_long2, aes(x = Bin, y = Density, fill = Method)) +
  geom_col(position = "dodge", na.rm = TRUE) +
  facet_grid(Method ~ Site, scales = "free_y") +
  scale_fill_manual(values = method_colors)+
  theme_bw()+
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 8),
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    strip.text = element_text(size = 12, face = "bold"))

ggplot(df_long2, aes(x = Bin, y = Density, fill = Method)) +
  geom_col(position = "dodge", na.rm = TRUE) +
  facet_grid(Method ~ Site, scales = "free_y") +
  scale_fill_manual(values = method_colors) +
  theme_bw(base_family = "Times New Roman", base_size = 24) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 24, family = "Times New Roman"),
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 24, family = "Times New Roman"),
    strip.text = element_text(size = 24, face = "bold", family = "Times New Roman"),
    axis.text = element_text(size = 24, family = "Times New Roman"),
    axis.title = element_text(size = 24, family = "Times New Roman")
  )

