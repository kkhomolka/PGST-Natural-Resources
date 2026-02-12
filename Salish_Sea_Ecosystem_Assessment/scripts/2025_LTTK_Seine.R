pacman::p_load(ggplot2,
               tidyr,
               dplyr,
               readxl,
               openxlsx,
               ggrepel)

# Aesthetics color palette
clrblind_pal <- c("#edbd00","#1dd2d3","#78b41f","#7487ff","#b41f78")

df<-read_excel("data_raw/2025_LLTK_Seine.xlsx")

df<- df %>%
  group_by(Site) %>%
  summarise(
    `Juvenile Salmon` = sum(`Total Juvenile_Salmon`, na.rm = TRUE),
    `Adult Salmon` = sum(`Total Adult_Salmon`, na.rm = TRUE),
    Herring = sum(Herring, na.rm = TRUE),
    `Sand Lance` = sum(Sand_Lance, na.rm = TRUE),
    Stickleback = sum(Stickleback, na.rm = TRUE),
    `Walleye Pollock` = sum(Walleye_Pollock, na.rm = TRUE))

df_long <- df_site %>%
  pivot_longer(
    cols = -Site,
    names_to = "Species",
    values_to = "Total")

df_long <- df_long %>%
  filter(Total > 0)

df_long <- df_long %>%
  group_by(Site) %>%
  mutate(Percent = Total / sum(Total) * 100)

df_oak<-df_long %>% 
  filter(Site == "Oak Bay")

df_sji<-df_long %>% 
  filter(Site == "Strawberry Island")

df_squ<-df_long %>% 
  filter(Site == "Squamish Harbor")

#Oak Bay
ggplot(df_oak, aes(x = Site, y = Total, fill = Species)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  theme_void()

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



