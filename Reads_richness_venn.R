# Load required libraries
library(ggplot2)
library(dplyr)
library(patchwork)  # For side-by-side plots


# Create the dataset
data <- data.frame(
  Month = c("Sep 2023", "Oct 2023", "Nov 2023", "Dec 2023", "Jan 2024", "Feb 2024", 
            "Mar 2024", "Apr 2024", "May 2024", "Jun 2024", "Jul 2024", "Aug 2024"),
  Species = c(32, 38, 39, 43, 44, 44, 46, 41, 42, 30, 31, 37),
  Reads = c(4082179, 5245565, 2380848, 1526407, 2497583, 1336699, 
            2055260, 7406982, 10223301, 4290263, 5626381, 8119746)
)

# Scale reads for better visualization
data <- data %>%
  mutate(Reads_scaled = Reads / 1e6)  # Convert reads to millions

# Convert Month to a factor to maintain correct order
data$Month <- factor(data$Month, levels = rev(data$Month))  # Reverse order for better visualization

# Define the seasonal groups for color mapping
season_mapping <- list(
  Autumn = c("Sep 2023", "Oct 2023", "Nov 2023"),
  Winter = c("Dec 2023", "Jan 2024", "Feb 2024"),
  Spring = c("Mar 2024", "Apr 2024", "May 2024"),
  Summer = c("Jun 2024", "Jul 2024", "Aug 2024")
)

# Define the color gradient for each set of months
color_gradient <- c("lightcoral", "darkred", "black")

# Map each month to the corresponding color within its season
data$MonthColor <- mapply(function(month) {
  # Identify which season the month belongs to
  for (season in names(season_mapping)) {
    if (month %in% season_mapping[[season]]) {
      # Get the index of the month in its season and apply the color
      month_index <- match(month, season_mapping[[season]])
      return(color_gradient[month_index])
    }
  }
}, data$Month)

# Plot for species (with months on y-axis)
p1 <- ggplot(data, aes(x = Species, y = Month)) +
  geom_bar(stat = "identity", aes(fill = MonthColor), width = 0.6) +  # Apply the month color gradient
  labs(x = "Number of Species", y = NULL) +
  scale_fill_identity() +  # Use the exact colors
  theme_minimal(base_size = 10) +
  theme(panel.grid = element_blank(),  # Remove grid
        axis.line = element_blank())  # Remove axis lines

p1

# Plot for reads (aligned with species plot)
p2 <- ggplot(data, aes(x = Reads_scaled, y = Month)) +
  geom_bar(stat = "identity", fill = "black", width = 0.6) +
  labs(x = "Reads (millions)", y = NULL) +
  theme_minimal(base_size = 10) +  # Keeps axis text, removes grid
  theme(panel.grid = element_blank(),  # Remove grid
        axis.line = element_blank(),
        axis.text.y = element_blank(),  # Remove duplicate y-axis labels
        axis.ticks.y = element_blank())

p2 <- ggplot(data, aes(x = Reads_scaled, y = Month)) +
  geom_bar(stat = "identity", fill = "black", width = 0.6) +
  labs(x = "Reads (millions)", y = NULL) +
  theme_minimal(base_size = 10) +  # Keeps axis text, removes grid
  theme(panel.grid = element_blank(),  # Remove grid
        axis.line = element_blank(),
        axis.text.y = element_blank(),  # Remove duplicate y-axis labels
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 12, face = "bold"),  # Bigger, bold x-axis labels
        axis.title.x = element_text(size = 14, face = "bold")  # Bigger, bold x-axis title
  )


# Arrange side by side
p1 + p2 + plot_layout(ncol = 2, widths = c(3, 3))


##Venn diagram

library(VennDiagram)

# Example species lists
List1 <- c("Abramis brama", "Anguilla anguilla", "Barbatula barbatula", "Blicca bjoerkna", "Ciliata mustela", 
           "Clupea harengus", "Esox lucius", "Gasterosteus aculeatus", "Gobio gobio", "Gymnocephalus cernua", 
           "Leuciscus leuciscus", "Limanda limanda", "Merlangius merlangus", "Osmerus eperlanus", "Perca fluviatilis", 
           "Platichthys flesus", "Pleuronectes platessa", "Pomatoschistus minutus", "Rutilus rutilus", "Salmo trutta", 
           "Solea solea", "Sprattus sprattus", "Squalius cephalus", "Thymallus thymallus", "Trisopterus luscus", 
           "Agonus cataphractus", "Cottus gobio", "Cyprinus carpio", "Gadus morhua", "Salmo salar", "Scardinius erythrophthalmus", "Taurulus bubalis", "Tinca tinca", "Carassius carassius", 
           "Echiichthys vipera", "Lota lota", "Sardina pilchardus", 
           "Syngnathus rostellatus", "Torpedo sp.", "Gaidropsarus mediterraneus", "Scyliorhinus canicula", 
           "Acipenser sturio")

##"Lampetra fluviatilis", "Chelon labrosus",  "Chelon ramada", "Lampetra planeri", 

List2 <- c("Anguilla anguilla", "Barbatula barbatula", "Buglossidium luteum", "Ciliata mustela", "Clupea harengus", 
           "Dicentrarchus labrax", "Esox lucius", "Gasterosteus aculeatus", "Gymnocephalus cernua", "Limanda limanda", 
           "Merlangius merlangus", "Phoxinus phoxinus", "Platichthys flesus", "Pomatoschistus microps", "Rutilus rutilus", 
           "Solea solea", "Sprattus sprattus", "Thymallus thymallus", "Trisopterus luscus", "Agonus cataphractus", "Cottus gobio", "Cyprinus carpio", "Gadus morhua", "Raja clavata", "Salmo salar", "Syngnathus rostellatus", "Echiichthys vipera", 
           "Pholis gunnellus", "Petromyzon marinus", "Osmerus eperlanus", "Ammodytes marinus", "Trisopterus minitus", 
           "Pomatoschistus minutus", "Chelidonicthys lucernus", "Pleuronectes platessa", "Scyliorhinus canicula", 
           "Sardina pilchardus", "Salmo trutta")

##"Lampetra fluviatilis",

List3 <- c("Abramis brama", "Ammodytes marinus", "Anguilla anguilla", "Barbatula barbatula", "Blicca bjoerkna", 
           "Buglossidium luteum", "Ciliata mustela", "Clupea harengus", "Conger conger", "Dicentrarchus labrax", 
           "Esox lucius", "Gasterosteus aculeatus", "Gobio gobio", "Gymnocephalus cernua", "Leuciscus leuciscus", 
           "Limanda limanda", "Merlangius merlangus", "Oncorhynchus mykiss", "Osmerus eperlanus", "Perca fluviatilis", 
           "Phoxinus phoxinus", "Platichthys flesus", "Pleuronectes platessa", "Pomatoschistus microps", "Pomatoschistus minutus", 
           "Rutilus rutilus", "Salmo trutta", "Solea solea", "Sprattus sprattus", "Squalius cephalus", "Thymallus thymallus", 
           "Trisopterus luscus", "Agonus cataphractus", "Atherina boyeri", "Barbus barbus", "Cottus gobio", "Cyprinus carpio", 
           "Gadus morhua", "Lipophrys pholis", "Raja clavata", "Salmo salar", "Chelidonichthys cuculus", "Mullus surmuletus", 
           "Scardinius erythrophthalmus", "Scomber scombrus", "Sparus aurata", "Aphia minuta", "Leuciscus idus", "Merluccius merluccius", "Pollachius pollachius", "Pungitius pungitius", "Taurulus bubalis", 
           "Tinca tinca", "Trachurus trachurus", "Melanogrammus aeglefinus", "Symphodus melops", 
           "Scophthalmus maximus", "Syngnathus rostellatus", "Carassius carassius", "Crystallogobius linearis", "Echiichthys vipera", 
           "Gymnammodytes semisquamatus", "Liparis liparis", "Scophthalmus rhombus", "Arnoglossus laterna", "Engraulis encrasicolus", 
           "Mustelus asterias")

## "Lampetra fluviatilis", "Chelon labrosus",

# Create the Venn diagram
venn.plot <- venn.diagram(
  x = list(
    "Pre-industrial" = List1, 
    "2015-2020" = List2, 
    "12-month eDNA survey" = List3
  ),
  category.names = c(
    "Pre-industrial", 
    "2015-2020", 
    "12-month eDNA survey"
  ),
  filename = NULL,
  col = "transparent",
  fill = c("lightblue", "lightgreen", "lightcoral"), 
  alpha = 0.5,
  cex = 1.5,
  fontface = "bold",
  fontfamily = "serif",
  cat.cex = 1.2,
  cat.fontface = "bold",
  cat.fontfamily = "serif",
  rotation.degree = 0,
  cat.pos = c(0, 0, 180),  # Adjust category label positions
  cat.dist = 0.05,
  margin = 0.1
)

# Display the diagram
grid.draw(venn.plot)

# Get the total number of unique species across all lists
all_species <- union(union(List1, List2), List3)

# Print the total number of unique species
total_unique_species <- length(all_species)
cat("Total number of unique species across all lists:", total_unique_species, "\n")

# Calculate percentage of shared species relative to the total unique species
percent_shared_1_2_total <- length(shared_1_2) / total_unique_species * 100
percent_shared_1_3_total <- length(shared_1_3) / total_unique_species * 100
percent_shared_2_3_total <- length(shared_2_3) / total_unique_species * 100

# Calculate percentage of exclusive species relative to the total unique species
percent_exclusive_1_total <- length(exclusive_1) / total_unique_species * 100
percent_exclusive_2_total <- length(exclusive_2) / total_unique_species * 100
percent_exclusive_3_total <- length(exclusive_3) / total_unique_species * 100

# Print the results relative to the total unique species
cat("Percentage of shared species between List 1 and List 2 (relative to total unique species):", percent_shared_1_2_total, "%\n")
cat("Percentage of shared species between List 1 and List 3 (relative to total unique species):", percent_shared_1_3_total, "%\n")
cat("Percentage of shared species between List 2 and List 3 (relative to total unique species):", percent_shared_2_3_total, "%\n")

cat("Percentage of exclusive species in List 1 (relative to total unique species):", percent_exclusive_1_total, "%\n")
cat("Percentage of exclusive species in List 2 (relative to total unique species):", percent_exclusive_2_total, "%\n")
cat("Percentage of exclusive species in List 3 (relative to total unique species):", percent_exclusive_3_total, "%\n")


# Calculate species shared between all three lists
shared_all_three <- Reduce(intersect, list(List1, List2, List3))

# Calculate percentage of shared species relative to the total unique species
percent_shared_all_three_total <- length(shared_all_three) / total_unique_species * 100

# Print the result
cat("Percentage of species shared between all three lists (relative to total unique species):", percent_shared_all_three_total, "%\n")


# Species exclusively detected in List1
exclusive_1 <- setdiff(List1, union(List2, List3))

# Species exclusively detected in List2
exclusive_2 <- setdiff(List2, union(List1, List3))

# Species exclusively detected in List3
exclusive_3 <- setdiff(List3, union(List1, List2))

# Print the species names
cat("Species exclusively detected in List1 (Pre-industrial):\n")
print(exclusive_1)

cat("\nSpecies exclusively detected in List2 (2015-2020):\n")
print(exclusive_2)

cat("\nSpecies exclusively detected in List3 (12-month eDNA survey):\n")
print(exclusive_3)













library(ggplot2)
library(patchwork)  # For arranging plots

# Define a common theme for both plots
custom_theme <- theme_minimal(base_size = 10) +
  theme(panel.grid = element_blank(),  # Remove grid
        axis.line = element_blank(),
        axis.text.x = element_text(size = 12, face = "bold"),  # Bigger, bold x-axis labels
        axis.text.y = element_text(size = 12, face = "bold"),  # Bigger, bold y-axis labels
        axis.title.x = element_text(size = 12, face = "bold"), # Bigger, bold x-axis title
        axis.title.y = element_text(size = 12, face = "bold")  # Bigger, bold y-axis title
  )

# Plot for species (with months on y-axis)
p1 <- ggplot(data, aes(x = Species, y = Month)) +
  geom_bar(stat = "identity", aes(fill = MonthColor), width = 0.6) +  # Apply the month color gradient
  labs(x = "Number of Species", y = NULL) +
  scale_fill_identity() +  # Use the exact colors
  custom_theme

# Plot for reads (aligned with species plot)
p2 <- ggplot(data, aes(x = Reads_scaled, y = Month)) +
  geom_bar(stat = "identity", fill = "black", width = 0.6) +
  labs(x = "Reads (millions)", y = NULL) +
  custom_theme +
  theme(axis.text.y = element_blank(),  # Remove duplicate y-axis labels
        axis.ticks.y = element_blank()) 

# Arrange side by side
p1 + p2 + plot_layout(ncol = 2, widths = c(3, 3))





# Load required libraries
library(VennDiagram)
library(grid)

# Create the Venn diagram with improved aesthetics
venn.plot <- venn.diagram(
  x = list(
    "Pre-industrial" = List1, 
    "2015-2020" = List2, 
    "12-month eDNA survey" = List3
  ),
  category.names = c(
    "Pre-industrial", 
    "2015-2020", 
    "12-month eDNA survey"
  ),
  filename = NULL,
  # Improved color scheme
  fill = c("#4E79A7", "#59A14F", "#E15759"),  # More professional colors
  alpha = 0.6,  # Slightly more transparency
  # Circle border
  lwd = 2,
  lty = 'solid',
  # Text appearance
  cex = 1.3,
  fontface = "bold",
  fontfamily = "sans",  # More modern font
  # Category labels
  cat.cex = 1.3,
  cat.fontface = "bold",
  cat.fontfamily = "sans",
  cat.default.pos = "outer",
  cat.pos = c(-30, 30, 180),  # Better label positioning
  cat.dist = c(0.05, 0.05, 0.03),
  # Margins and rotation
  margin = 0.08,
  rotation.degree = 0,
  # Add count display
  print.mode = c("raw", "percent"),
  sigdigs = 2,
  # Improve label positioning
  ext.text = TRUE,
  ext.line.lwd = 1,
  ext.dist = -0.15,
  ext.length = 0.8,
  ext.pos = -4,
  # Rotation of category names
  rotation = 1
)

# Add title and display
grid.newpage()
grid.draw(venn.plot)
grid.text("Fish Species Comparison", y = 0.95, gp = gpar(fontsize = 16, fontface = "bold", fontfamily = "sans"))














