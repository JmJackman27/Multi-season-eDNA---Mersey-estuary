library(ggplot2)
library(ggalluvial)
library(dplyr)
library(tidyr)

# Step 1: Original data
data <- data.frame(
  Species = c("Brown trout", "European eel", "Atlantic salmon", "Atlantic salmon", 
              "Lampetra spp.", "Lampetra spp.", 
              "European smelt", "European smelt", "European smelt", 
              "European smelt", "European smelt", "European smelt", 
              "European smelt", "European smelt", "European smelt"),
  
  Season = c("All Seasons", "All Seasons", "Winter", "Autumn", 
             "Spring", "Winter", "All Seasons", "All Seasons", 
             "All Seasons", "All Seasons", "All Seasons", "All Seasons", "All Seasons", "All Seasons", "All Seasons"),
  
  Site = c("All Sites", "All Sites", "U1, U2, U3, L1, L2", "U1, U2, U3", 
           "L1", "U3, C1", "C2, U3", "C3", "C4", 
           "C2, C4", "C2", "U1, L3", "L3", "C4", "C2, U3")
)

# Step 2: Separate multiple sites into rows
data_long <- data %>%
  separate_rows(Site, sep = ",\\s*")

# Step 3: Plot the alluvial diagram with correct labels
ggplot(data_long, aes(axis1 = Species, axis2 = Season, axis3 = Site, y = 1)) +
  geom_alluvium(aes(fill = Species), width = 0.3) +
  geom_stratum() +
  
  # Add correct labels based on axis
  geom_text(
    stat = "stratum",
    aes(
      label = after_stat(
        ifelse(
          stratum == "Lampetra spp.", "italic(Lampetra)~spp.",  # italic label for species
          paste0("'", stratum, "'")                              # quoted for parse compatibility
        )
      )
    ),
    parse = TRUE,
    size = 3
  ) +
  
  scale_x_discrete(limits = c("Species", "Season", "Site")) +
  theme_void() +
  theme(legend.position = "none")






##detections between zones (eDNA transport)

data <- data.frame(
  Species = c("Abramis brama", "Agonus cataphractus", "Ammodytes marinus", 
              "Anguilla anguilla", "Aphia minuta", "Arnoglossus laterna",
              "Atherina boyeri", "Barbatula barbatula", "Barbus barbus", 
              "Blicca bjoerkna", "Buglossidium luteum", "Carassius carassius",
              "Chelidonichthys cuculus", "Chelon labrosus", "Chelon ramada",
              "Ciliata mustela", "Clupea harengus", "Conger conger", 
              "Cottus gobio", "Crystallogobius linearis", "Cyprinus carpio",
              "Dicentrarchus labrax", "Echiichthys vipera", "Engraulis encrasicolus",
              "Esox lucius", "Gadus morhua", "Gasterosteus aculeatus",
              "Gobio gobio", "Gymnammodytes semisquamatus", "Gymnocephalus cernua",
              "Lampetra fluviatilis", "Lampetra planeri", "Leuciscus idus",
              "Leuciscus leuciscus", "Limanda limanda", "Liparis liparis",
              "Lipophrys pholis", "Melanogrammus aeglefinus", "Merlangius merlangus",
              "Merluccius merluccius", "Mullus surmuletus", "Mustelus asterias",
              "Oncorhynchus mykiss", "Osmerus eperlanus", "Perca fluviatilis",
              "Phoxinus phoxinus", "Platichthys flesus", "Pleuronectes platessa",
              "Pollachius pollachius", "Pomatoschistus microps", "Pomatoschistus minutus",
              "Pungitius pungitius", "Raja clavata", "Rutilus rutilus",
              "Salmo salar", "Salmo trutta", "Scardinius erythrophthalmus",
              "Scomber scombrus", "Scophthalmus maximus", "Scophthalmus rhombus",
              "Solea solea", "Sparus aurata", "Sprattus sprattus", "Squalius cephalus",
              "Symphodus melops", "Syngnathus rostellatus", "Taurulus bubalis",
              "Thymallus thymallus", "Tinca tinca", "Trachurus trachurus",
              "Trisopterus luscus"),
  Tolerance = c("F,B", "M", "M,B", "M,B,F", "M,B", "M", 
                "M,B,F", "F", "F", "F,B", "M", "F,B",
                "M", "M,B,F", "M,B,F", "M", "M,B", "M", 
                "F,B", "M", "F,B", "M,B,F", "M", "M,B",
                "F,B", "M,B", "M,B,F", "F,B", "M", "F,B", 
                "M,B,F", "F", "F,B", "F,B", "M", "M",
                "M", "M", "M,B", "M", "M", "M", "M,B,F", 
                "M,B,F", "F,B", "F,B", "M,B,F", "M,B",
                "M", "M,B,F", "M,B", "M,B,F", "M", "F,B", 
                "M,B,F", "M,B,F", "F,B", "M,B", "M,B", "M",
                "M,B", "M,B", "M,B", "F,B", "M", "M,B", "M,B", 
                "F,B", "F,B", "M", "M,B"),
  eDNAZone = c("Upper, Central, Lower", "Central, Lower", "Upper, Central, Lower",
               "Upper, Central, Lower", "Upper, Central, Lower", "Upper, Central, Lower",
               "Central", "Upper, Central, Lower", "Central, Upper", "Upper, Central, Lower",
               "Upper, Central, Lower", "Upper", "Upper, Lower", "Upper, Central, Lower",
               "Upper, Central, Lower", "Central, Lower", "Upper, Central, Lower", "Central, Lower",
               "Upper, Central, Lower", "Central", "Upper", "Upper, Central, Lower",
               "Central, Lower", "Central, Lower", "Upper, Central, Lower", "Central, Lower",
               "Upper, Central, Lower", "Upper, Central, Lower", "Upper, Central",
               "Upper, Central", "Upper, Lower", "Upper, Lower", "Upper",
               "Upper, Central, Lower", "Upper, Central, Lower", "Lower", "Upper, Central, Lower",
               "Central, Lower", "Central, Lower", "Central", "Upper", "Lower", "Central, Lower",
               "Central, Lower", "Upper, Central, Lower", "Upper, Central, Lower",
               "Upper, Central, Lower", "Upper, Central, Lower", "Lower", "Upper, Central, Lower",
               "Upper, Central, Lower", "Upper, Central, Lower", "Lower", "Upper, Central, Lower",
               "Upper, Central, Lower", "Upper, Central, Lower", "Upper, Central",
               "Upper, Central, Lower", "Lower", "Upper, Central", "Upper, Central, Lower",
               "Upper, Lower", "Upper, Central, Lower", "Upper, Central, Lower",
               "Central", "Lower", "Lower", "Upper, Central, Lower", "Upper", "Lower",
               "Upper, Central, Lower"),
  stringsAsFactors = FALSE
)


plot_data <- data %>%
  mutate(eDNAZone = str_replace_all(eDNAZone, " ", "")) %>%
  separate_rows(eDNAZone, sep = ",") %>%
  filter(!is.na(eDNAZone) & eDNAZone != "") %>%
  mutate(
    Tolerance = case_when(
      Tolerance %in% c("F", "F,B") ~ "F/B",
      Tolerance %in% c("M", "M,B") ~ "M/B",
      Tolerance == "B" ~ "Both",
      TRUE ~ Tolerance
    ),
    eDNAZone = factor(eDNAZone, levels = c("Upper", "Central", "Lower"))
  )


# Filter to include Freshwater and Marine species (now includes F,B and M,B)
filtered_data <- plot_data %>%
  filter(Tolerance %in% c("F/B", "M/B"))

# Create the plot with updated filtered data
ggplot(filtered_data,
       aes(axis1 = Species,     # First axis - Species
           axis2 = Tolerance,   # Second axis - Tolerance
           axis3 = eDNAZone)) + # Third axis - Detection Zone
  geom_alluvium(aes(fill = Tolerance), 
                width = 1/12, 
                knot.pos = 0.3,
                alpha = 0.7) +
  geom_stratum(width = 1/6, fill = "white", color = "black") +
  
  # Species labels (left)
  geom_text(stat = "stratum",
            aes(label = ifelse(after_stat(x) == 1, 
                               as.character(after_stat(stratum)), 
                               ""),
                size = 3,
                angle = 0,
                hjust = 0,
                nudge_x = -0.4)) +
              
              # Tolerance labels (middle)
              geom_text(stat = "stratum",
                        aes(label = ifelse(after_stat(x) == 2, 
                                           as.character(after_stat(stratum)), 
                                           "")),
                        size = 3.5,
                        angle = 0,
                        hjust = 0.5) +
              
              # Detection zone labels (right)
              geom_text(stat = "stratum",
                        aes(label = ifelse(after_stat(x) == 3, 
                                           as.character(after_stat(stratum)), 
                                           "")),
                        size = 3,
                        angle = 0,
                        hjust = 1,
                        nudge_x = 0.4) +
              
              scale_x_discrete(limits = c("Species", "Tolerance", "Detection Zone"),
                               expand = c(0.1, 0.1)) +
              scale_fill_manual(values = c("F/B" = "#66C2A5", 
                                           "M/B" = "#8DA0CB"),
                                guide = "none") +
              labs(title = "Freshwater and Marine Species",
                   subtitle = "Including species with tolerance for brackish environments",
                   y = NULL) +
              theme_minimal() +
              theme(axis.text.y = element_blank(),
                    panel.grid = element_blank(),
                    axis.text.x = element_text(size = 10),
                    plot.margin = margin(1, 1, 1, 3, "cm"))
            
            # Count unique species per Tolerance category (now includes F,B and M,B)
            tolerance_counts <- filtered_data %>%
              distinct(Species, Tolerance) %>%
              count(Tolerance, name = "unique_species_count")
            
            # Get stratum positions (x = 1 is Tolerance axis)
            stratum_positions <- ggplot_build(
              ggplot(filtered_data, aes(axis1 = Tolerance, axis2 = eDNAZone)) +
                geom_alluvium(aes(fill = Tolerance)) +
                geom_stratum()
            )$data[[2]] %>%  # This is for geom_stratum
              filter(x == 1)  # Only left-side (Tolerance) nodes
            
            # Join with counts and shift number label slightly down
            label_data <- left_join(stratum_positions, tolerance_counts, by = c("stratum" = "Tolerance")) %>%
              mutate(y_count = y - 4)  # Move count slightly below center label
            
            # Plot with updated counts
            ggplot(filtered_data,
                   aes(axis1 = Tolerance,
                       axis2 = eDNAZone)) +
              geom_alluvium(aes(fill = Tolerance), 
                            width = 1/8,
                            alpha = 0.7) +
              geom_stratum(width = 1/8, 
                           fill = "white", 
                           color = "gray70") +
              
              # Tolerance node label (e.g. Freshwater) in center
              geom_text(stat = "stratum",
                        aes(label = after_stat(stratum)),
                        size = 3,
                        color = "black") +
              
              # Species count just below the Tolerance label
              geom_text(data = label_data,
                        aes(x = 1, y = y_count, label = unique_species_count),
                        inherit.aes = FALSE,
                        size = 3,
                        color = "black") +
              
              # Detection zone labels on right side
              geom_text(stat = "stratum",
                        aes(label = after_stat(stratum)),
                        size = 3,
                        color = "black") +
              
              scale_x_discrete(limits = c("Tolerance", "Detection Zone"),
                               expand = c(0.1, 0.1)) +
              scale_fill_manual(values = c("F/B" = "#66C2A5", 
                                           "M/B" = "#8DA0CB"),
                                guide = "none") +
              theme_minimal() +
              theme(axis.text.y = element_blank(),
                    panel.grid = element_blank(),
                    axis.text.x = element_blank(),
                    axis.title = element_blank(),
                    plot.margin = margin(5, 5, 5, 5))
            
            