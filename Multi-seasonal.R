library(tidyverse)
library(vegan)
library(magrittr)
library(gridExtra)
library(dplyr)
library(tidyr)
library(UpSetR)
library(pairwiseAdonis)
library(patchwork)
library(pheatmap)
library(ggplot2)
library(openxlsx)
library(tibble)
library(pairwiseAdonis)
library(knitr)


##Alpha diversity
#All months richness

# List of months to iterate over
months <- c("Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug")

# Prepare an empty list to store plots for each month
plot_list <- list()

# Loop through each month and process the data
for (month in months) {
  
  # Load the data 
  combined <- read.csv(paste0(month, ".csv"))
  site_data <- read.csv("Meta_data.csv")
  
  # Process the data
  combined_jit1 <- combined[,-c(1)]  
  jit_data <- column_to_rownames(combined_jit1, var = "species")
  jit_data <- t(jit_data)
  jit_data <- decostand(jit_data, "pa")  # Presence/Absence standardisation
  
  species_rich <- as.data.frame(rowSums(jit_data))  # Calculate species richness per site
  colnames(species_rich)[1] <- "Species_Richness"
  
  # Combine the species richness data with the site metadata
  species_richness <- cbind(site_data, species_rich)
  
  # Plot the boxplot for this month's data
  p <- ggplot(species_richness, aes(x = Site, y = Species_Richness, fill = Site)) +
    geom_boxplot() +
    geom_jitter(width = 0.2, alpha = 0.5) +  # Add jitter to show individual data points
    labs(title = paste("Species Richness -", month), x = "Site", y = "Species Richness") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none")
  
  # Store the plot in the list
  plot_list[[month]] <- p
}

p

# print each plot for individual months
for (month in months) {
  print(plot_list[[month]])
}

p

# Combine all plots into a single layout
grid.arrange(grobs = plot_list, ncol = 3)  # Adjust 'ncol' as per your preference


season_colors <- list(
  "Spring" = "lightgreen",
  "Summer" = "yellow",
  "Autumn" = "orange",
  "Winter" = "lightblue"
)

# Define the mapping of months to seasons
season_mapping <- list(
  "Spring" = c("Mar", "Apr", "May"),
  "Summer" = c("Jun", "Jul", "Aug"),
  "Autumn" = c("Sep", "Oct", "Nov"),
  "Winter" = c("Dec", "Jan", "Feb")
)

# Define the custom order of sites
site_order <- c("L3", "L2", "L1", "C4", "C3", "C2", "C1", "U3", "U2", "U1")

# Prepare an empty list to store plots for each season
season_plot_list <- list()

# Loop through each season and process the data
for (season in names(season_mapping)) {
  
  # Combine data for the months in the current season
  season_data <- do.call(rbind, lapply(season_mapping[[season]], function(month) {
    # Load the data for each month
    combined <- read.csv(paste0(month, ".csv"))
    site_data <- read.csv("Meta_data.csv")
    
    # Process the data
    combined_jit1 <- combined[,-c(1)] 
    jit_data <- column_to_rownames(combined_jit1, var = "species")
    jit_data <- t(jit_data)
    jit_data <- decostand(jit_data, "pa") 
    
    # Calculate species richness per site
    species_rich <- as.data.frame(rowSums(jit_data))
    colnames(species_rich)[1] <- "Species_Richness"
    
    # Combine with metadata
    species_richness <- cbind(site_data, species_rich)
    species_richness$Month <- month  # Add month information
    
    # Reorder the sites
    species_richness$Site <- factor(species_richness$Site, levels = site_order)  # Reorder sites
    
    return(species_richness)
  }))
  
  
  # Plot the boxplot for this season's data
  p <- ggplot(season_data, aes(x = Site, y = Species_Richness, fill = Site)) +
    geom_boxplot(fill = season_colors[[season]]) +
    #geom_jitter(width = 0.1, alpha = 0.4) +  # Add jitter to show individual data points
    labs(title = paste(season), x = "Site", y = "Species Richness") +
    theme_minimal() +  # Use minimal theme
    theme(panel.grid = element_blank(),  # Remove gridlines
          axis.ticks = element_line(color = "black"),  # Keep axis ticks
          axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
          axis.line = element_line(color = "black"),  # Keep axis lines
          legend.position = "none")  # Remove legend
  
  
  # Store the plot in the list
  season_plot_list[[season]] <- p
}

# Display the plots for each season
season_plot_list

# Arrange the plots in a 2x2 grid
grid.arrange(
  season_plot_list[["Spring"]],
  season_plot_list[["Summer"]],
  season_plot_list[["Autumn"]],
  season_plot_list[["Winter"]],
  ncol = 2,
  nrow = 2
)


# Create a Zone variable
site_data$Zone <- substr(site_data$Site, 1, 1)  # U, C, L

# Calculate Jaccard distance
dist_mat <- vegdist(jit_data, method = "bray")

# Run PERMANOVA
adonis_result <- adonis2(dist_mat ~ Zone, data = site_data)
print(adonis_result)


dist_mat <- vegdist(jit_data, method = "bray")

pairwise_results <- pairwise.adonis(
  dist_mat,
  factors = site_data$Zone,
  perm = 999
)

print(pairwise_results)




# Define season colors
season_colors <- list(
  "Spring" = "lightgreen",
  "Summer" = "yellow",
  "Autumn" = "orange",
  "Winter" = "lightblue"
)

# Define season mapping
season_mapping <- list(
  "Spring" = c("Mar", "Apr", "May"),
  "Summer" = c("Jun", "Jul", "Aug"),
  "Autumn" = c("Sep", "Oct", "Nov"),
  "Winter" = c("Dec", "Jan", "Feb")
)

# Define site order
site_order <- c("L3", "L2", "L1", "C4", "C3", "C2", "C1", "U3", "U2", "U1")

# Find max species richness across all seasons
max_richness <- max(sapply(unlist(season_mapping), function(month) {
  combined <- read.csv(paste0(month, ".csv"))
  combined_jit1 <- combined[,-c(1)]
  jit_data <- column_to_rownames(combined_jit1, var = "species")
  jit_data <- t(jit_data)
  jit_data <- decostand(jit_data, "pa")
  max(rowSums(jit_data), na.rm = TRUE)
}), na.rm = TRUE)

# Prepare an empty list for season plots
season_plot_list <- list()

# Loop through each season and create boxplots
for (season in names(season_mapping)) {
  
  # Combine data for months in this season
  season_data <- do.call(rbind, lapply(season_mapping[[season]], function(month) {
    combined <- read.csv(paste0(month, ".csv"))
    site_data <- read.csv("Meta_data.csv")
    
    combined_jit1 <- combined[,-c(1)]
    jit_data <- column_to_rownames(combined_jit1, var = "species")
    jit_data <- t(jit_data)
    jit_data <- decostand(jit_data, "pa")
    
    species_rich <- as.data.frame(rowSums(jit_data))
    colnames(species_rich)[1] <- "Species_Richness"
    
    species_richness <- cbind(site_data, species_rich)
    species_richness$Month <- month
    species_richness$Site <- factor(species_richness$Site, levels = site_order)
    
    return(species_richness)
  }))
  
  # Create boxplot for this season
  p <- ggplot(season_data, aes(x = Site, y = Species_Richness, fill = Site)) +
    geom_boxplot(fill = season_colors[[season]]) +
    labs(title = season, x = "Site", y = "Species Richness") +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.ticks = element_line(color = "black"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.line = element_line(color = "black"),
          legend.position = "none") +
    ylim(0, max_richness)  # Set consistent y-axis
  
  # Store in the list
  season_plot_list[[season]] <- p
}

grid.arrange(grobs = season_plot_list, ncol = 2, nrow = 2)





# Combine species richness data across all seasons
all_season_data <- do.call(rbind, lapply(names(season_mapping), function(season) {
  # Combine data for the months in the current season
  season_data <- do.call(rbind, lapply(season_mapping[[season]], function(month) {
    combined <- read.csv(paste0(month, ".csv"))
    site_data <- read.csv("Meta_data.csv")
    
    # Process the data
    combined_jit1 <- combined[,-c(1)]  # Remove species column if necessary
    jit_data <- column_to_rownames(combined_jit1, var = "species")
    jit_data <- t(jit_data)
    jit_data <- decostand(jit_data, "pa")  # Presence/Absence standardization
    
    # Calculate species richness per site
    species_rich <- as.data.frame(rowSums(jit_data))
    colnames(species_rich)[1] <- "Species_Richness"
    
    # Combine with metadata
    species_richness <- cbind(site_data, species_rich)
    species_richness$Season <- season  # Add season information
    
    return(species_richness)
  }))
  
  return(season_data)
}))

# Reorder the sites
all_season_data$Site <- factor(all_season_data$Site, levels = site_order)

# Create a box plot for species richness by season
ggplot(all_season_data, aes(x = Season, y = Species_Richness, fill = Season)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, size = 1, alpha = 0.6, color = "black") +
  labs(
    x = "Season",
    y = "Species Richness") +
  scale_fill_manual(values = unlist(season_colors)) +  # Use custom season colors
  theme_minimal() +
  theme(panel.grid = element_blank(),  # Remove gridlines
        axis.ticks = element_line(color = "black"),  # Keep axis ticks
        axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
        axis.line = element_line(color = "black"),  # Keep axis lines
        legend.position = "none")  # Remove legend


# Remove rows with zero species richness or NAs
all_season_data <- all_season_data[rowSums(all_season_data[, "Species_Richness", drop = FALSE]) > 0, ]
all_season_data <- na.omit(all_season_data)

# Calculate Bray-Curtis distance matrix
dist_matrix_season <- vegdist(all_season_data$Species_Richness, method = "bray")

# Perform PERMANOVA to check for differences between seasons
permanova_result_seasons <- adonis2(dist_matrix_season ~ all_season_data$Season)

# Display the PERMANOVA result
print(permanova_result_seasons)


# Perform pairwise PERMANOVA between seasons
pairwise_results <- pairwise.adonis(dist_matrix_season, all_season_data$Season)

# Display the pairwise comparison results
print(pairwise_results)

# Convert pairwise PERMANOVA results to a data frame
pairwise_df <- as.data.frame(pairwise_results)


##Upset plot

# Initialize the list for storing species by month
species_list_by_month <- list()


# Define all months in calendar order
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# Define custom month order starting from September
start_month <- "Sep"
start_index <- match(start_month, months)
custom_month_order <- c(months[start_index:12], months[1:(start_index - 1)])

# Initialize the list for storing species by month
species_list_by_month <- list()

# Load species data for each month
for (month in months) {
  file_path <- paste0(month, ".csv")
  
  if (file.exists(file_path)) {
    combined <- read.csv(file_path)
    species_list_by_month[[month]] <- unique(combined$species)
    print(paste(month, ":", length(species_list_by_month[[month]]), "species found."))
  } else {
    warning(paste("File for", month, "not found. Skipping."))
    species_list_by_month[[month]] <- NULL
  }
}

# Filter out NULL entries (months without data)
species_list_by_month <- Filter(Negate(is.null), species_list_by_month)

# Reorder list according to custom month order
valid_months <- names(species_list_by_month)
ordered_months <- intersect(custom_month_order, valid_months)
species_list_by_month <- species_list_by_month[ordered_months]

# Convert to binary matrix and reorder columns
binary_matrix <- fromList(species_list_by_month)
binary_matrix <- binary_matrix[, ordered_months]

# Create UpSet plot with months in Sep–Aug order
upset(binary_matrix,
      sets = ordered_months,
      order.by = "freq",
      main.bar.color = "seagreen",
      sets.bar.color = "darkred",
      text.scale = 1,
      keep.order = TRUE,
      nsets = length(ordered_months),
      nintersects = NA)

# Convert to binary matrix
binary_matrix <- fromList(species_list_by_month)

# Reverse order to get Sep on top
ordered_months_rev <- rev(ordered_months)

# Reorder columns to match reversed order
binary_matrix <- binary_matrix[, ordered_months_rev]

# Plot
upset(binary_matrix,
      sets = ordered_months_rev,
      order.by = "freq",
      main.bar.color = "seagreen",
      sets.bar.color = "darkred",
      text.scale = 1,
      keep.order = TRUE,
      nsets = length(ordered_months_rev),
      nintersects = NA)







# Generate a list of species detected in each season
species_list_by_season <- list()
for (season in names(season_mapping)) {
  species_in_season <- unlist(lapply(season_mapping[[season]], function(month) {
    combined <- read.csv(paste0(month, ".csv"))
    unique(combined$species)
  }))
  species_list_by_season[[season]] <- unique(species_in_season)
}


# For seasonal analysis, ensure season_mapping uses ordered months
season_mapping <- list(
  Spring = c("March", "April", "May"),
  Summer = c("June", "July", "August"),
  Autumn = c("September", "October", "November"),
  Winter = c("December", "January", "February")
)

# Generate a list of species detected in each season
species_list_by_season <- list()
for (season in names(season_mapping)) {
  species_in_season <- unlist(lapply(season_mapping[[season]], function(month) {
    if (month %in% names(species_list_by_month)) {
      return(species_list_by_month[[month]])
    } else {
      return(NULL)
    }
  }))
  species_list_by_season[[season]] <- unique(na.omit(species_in_season))
}

# Create an UpSet plot for species richness by season
upset(fromList(species_list_by_season), 
      order.by = "freq", 
      main.bar.color = "seagreen", 
      sets.bar.color = "darkred", 
      text.scale = 1.25,  # Adjust text size
      keep.order = TRUE) 


#Species exclusive by season

exclusive_species_by_season <- list()

# Step 2: Identify exclusive species for each season
for (season in names(season_mapping)) {
  # Get species detected in the current season by iterating through the months
  species_in_season <- unlist(lapply(season_mapping[[season]], function(month) {
    file_path <- paste0(month, ".csv")
    if (file.exists(file_path)) {
      combined <- read.csv(file_path)
      return(unique(combined$species))
    } else {
      warning(paste("File for", month, "not found. Skipping."))
      return(NULL)
    }
  }))
  
  # Flatten and remove NULL entries
  species_in_season <- unique(na.omit(species_in_season))
  
  # Identify species exclusive to this season
  other_seasons_species <- unlist(lapply(setdiff(names(season_mapping), season), function(other_season) {
    unlist(lapply(season_mapping[[other_season]], function(month) {
      file_path <- paste0(month, ".csv")
      if (file.exists(file_path)) {
        combined <- read.csv(file_path)
        return(unique(combined$species))
      } else {
        return(NULL)
      }
    }))
  }))
  
  # Store the exclusive species for this season
  exclusive_species_by_season[[season]] <- setdiff(species_in_season, other_seasons_species)
}

# Step 3: Convert to a data frame to display in a table format
exclusive_species_df <- data.frame(
  Season = character(0),
  Species = character(0),
  stringsAsFactors = FALSE
)

# Loop through the exclusive species by season and add them to the data frame
for (season in names(exclusive_species_by_season)) {
  species_list <- exclusive_species_by_season[[season]]
  if (length(species_list) > 0) {
    temp_df <- data.frame(Season = rep(season, length(species_list)), Species = species_list, stringsAsFactors = FALSE)
    exclusive_species_df <- rbind(exclusive_species_df, temp_df)
  }
}

# Print the table of exclusive species by season
print(exclusive_species_df)


#Species exclusive by month

# Initialize the list for storing species by month
species_list_by_month <- list()

# Iterate through each month and check data loading
for (month in months) {
  file_path <- paste0(month, ".csv")
  
  if (file.exists(file_path)) {
    combined <- read.csv(file_path)
    species_list_by_month[[month]] <- unique(combined$species)
    
    # Debugging: Print the number of species for each month
    print(paste(month, ":", length(species_list_by_month[[month]]), "species found."))
  } else {
    warning(paste("File for", month, "not found. Skipping."))
    species_list_by_month[[month]] <- NULL
  }
}

# Filter out NULL entries
species_list_by_month <- Filter(Negate(is.null), species_list_by_month)

# Combine all species from each month into one list
all_species_combined <- unlist(species_list_by_month)

# Get unique species from the combined list
all_unique_species <- unique(all_species_combined)

# Convert the unique species list into a data frame
species_df <- data.frame(Species = all_unique_species)

# Save the unique species list to an Excel file
#write.xlsx(species_df, "unique_species_list.xlsx", rowNames = FALSE)


##Beta diversity 


#By site

# Initialize lists for storing NMDS results
nmds_results_by_month <- list()

# Loop through each month and process the data for NMDS
nmds_results_by_month <- list()

for (month in months) {
  
  # Load the data
  combined <- read.csv(paste0(month, ".csv"))
  site_data <- read.csv("Meta_data.csv")
  
  # Process the data
  combined_jit1 <- combined[,-c(1)]  # Remove species column if necessary
  jit_data <- column_to_rownames(combined_jit1, var = "species")
  jit_data <- t(jit_data)
  jit_data <- decostand(jit_data, "pa")  # Presence/Absence standardization
  
  # Remove empty rows
  jit_data <- jit_data[rowSums(jit_data) > 0, ]
  
  # Check if data is sufficient for NMDS
  if (nrow(jit_data) > 1) {
    # Calculate Bray-Curtis distance
    dist_matrix <- vegdist(jit_data, method = "jaccard")
    
    # Perform NMDS
    nmds <- metaMDS(dist_matrix, k = 2, trymax = 100)
    nmds_results_by_month[[month]] <- nmds
    
    # Prepare data for plotting
    nmds_plot <- data.frame(scores(nmds))
    nmds_plot$Site <- rownames(nmds_plot)
    nmds_plot <- merge(nmds_plot, site_data, by.x = "Site", by.y = "Site")
    
    # Plot NMDS
    p <- ggplot(nmds_plot, aes(x = NMDS1, y = NMDS2, color = Site)) +
      geom_point(size = 3) +
      labs(title = paste("NMDS -", month), x = "NMDS1", y = "NMDS2") +
      theme_minimal() +
      theme(legend.position = "right")
  } else {
    warning(paste("Insufficient data for NMDS in", month))
  }
}

print(p)



# Initialize an empty data frame to store all the NMDS results
combined_nmds_plot <- data.frame()

# Loop through each month and process the data for NMDS
for (month in months) {
  
  # Load the data
  combined <- read.csv(paste0(month, ".csv"))
  site_data <- read.csv("Meta_data.csv")
  
  # Process the data
  combined_jit1 <- combined[,-c(1)]  # Remove species column if necessary
  jit_data <- column_to_rownames(combined_jit1, var = "species")
  jit_data <- t(jit_data)
  jit_data <- decostand(jit_data, "pa")  # Presence/Absence standardization
  
  # Remove empty rows
  jit_data <- jit_data[rowSums(jit_data) > 0, ]
  
  # Check if data is sufficient for NMDS
  if (nrow(jit_data) > 1) {
    # Calculate Bray-Curtis distance
    dist_matrix <- vegdist(jit_data, method = "jaccard")
    
    # Perform NMDS
    nmds <- metaMDS(dist_matrix, k = 2, trymax = 100)
    
    
    
    # Prepare data for plotting
    nmds_plot <- data.frame(scores(nmds))
    nmds_plot$Site <- rownames(nmds_plot)
    nmds_plot$Month <- month  # Add month info for grouping
    
    # Merge with site_data for additional metadata (if needed)
    nmds_plot <- merge(nmds_plot, site_data, by.x = "Site", by.y = "Site")
    
    # Combine all monthly NMDS results into a single data frame
    combined_nmds_plot <- rbind(combined_nmds_plot, nmds_plot)
  } else {
    warning(paste("Insufficient data for NMDS in", month))
  }
}

# Define custom colors for each month
month_colors <- c("Sep" = "#1f77b4", "Oct" = "yellow", "Nov" = "#2ca02c", 
                  "Dec" = "#d62728", "Jan" = "#9467bd", "Feb" = "#8c564b", 
                  "Mar" = "#e377c2", "Apr" = "#7f7f7f", "May" = "#bcbd22", 
                  "Jun" = "#17becf", "Jul" = "darkorange", "Aug" = "black")

# Plot NMDS results for all months combined
ggplot(combined_nmds_plot, aes(x = NMDS1, y = NMDS2, color = Month)) +
  geom_point(size = 3) +
  labs(x = "NMDS1", y = "NMDS2") +  # Removed title
  scale_color_manual(values = month_colors) +  # Custom colors
  theme_minimal() +
  theme(legend.position = "right")


# Perform PERMANOVA to assess differences between months
# Ensure that combined_species_data contains the species presence/absence data for all months
combined_species_data <- combined_nmds_plot[, c("NMDS1", "NMDS2")]  # Get the NMDS axes for all months

# Generate the Bray-Curtis distance matrix for the entire dataset
combined_dist_matrix <- vegdist(combined_species_data, method = "jaccard")

# Create a factor for months, corresponding to each row in the combined data
month_factor <- factor(combined_nmds_plot$Month)

# Perform PERMANOVA using adonis2 to assess differences between months
permanova_result <- adonis2(combined_dist_matrix ~ month_factor)

# Print the result
print(permanova_result)


# Perform pairwise PERMANOVA to compare differences between months
pairwise_result <- pairwise.adonis(combined_dist_matrix, factors = month_factor, perm = 999)

# Print the pairwise comparison results
print(pairwise_result)

# Convert the pairwise PERMANOVA results to a data frame
pairwise_df <- as.data.frame(pairwise_result)

# Save as a CSV file
write.csv(pairwise_df, "Pairwise_PERMANOVA_Results_4.csv", row.names = FALSE)



# Initialize a data frame to store stress values
stress_values <- data.frame(Month = character(), Stress = numeric(), stringsAsFactors = FALSE)

for (month in months) {
  
  # Load the data
  combined <- read.csv(paste0(month, ".csv"))
  site_data <- read.csv("Meta_data.csv")
  
  # Process the data
  combined_jit1 <- combined[, -c(1)]  # Remove species column if necessary
  jit_data <- column_to_rownames(combined_jit1, var = "species")
  jit_data <- t(jit_data)
  jit_data <- decostand(jit_data, "pa")  # Presence/Absence standardization
  
  # Remove empty rows
  jit_data <- jit_data[rowSums(jit_data) > 0, ]
  
  if (nrow(jit_data) > 1) {
    # Compute Bray-Curtis distance and NMDS
    dist_matrix <- vegdist(jit_data, method = "jaccard")
    nmds <- metaMDS(dist_matrix, k = 2, trymax = 100)
    
    # Store the stress value
    stress_values <- rbind(stress_values, data.frame(Month = month, Stress = nmds$stress))
  }
}

# Calculate the overall average stress value
overall_stress <- mean(stress_values$Stress, na.rm = TRUE)
print(paste("Overall NMDS Stress Value:", overall_stress))



#Create whole panel figure of each month

# Initialize list for storing NMDS plots
nmds_plots_by_month <- list()

# Loop through each month and process the data for NMDS
for (month in months) {
  
  # Load the data
  combined <- read.csv(paste0(month, ".csv"))
  site_data <- read.csv("Meta_data.csv")
  
  # Process the data
  combined_jit1 <- combined[,-c(1)]  # Remove species column if necessary
  jit_data <- column_to_rownames(combined_jit1, var = "species")
  jit_data <- t(jit_data)
  jit_data <- decostand(jit_data, "pa")  # Presence/Absence standardization
  
  # Remove empty rows
  jit_data <- jit_data[rowSums(jit_data) > 0, ]
  
  # Check if data is sufficient for NMDS
  if (nrow(jit_data) > 1) {
    # Calculate Bray-Curtis distance
    dist_matrix <- vegdist(jit_data, method = "jaccard")
    
    # Perform NMDS
    nmds <- metaMDS(dist_matrix, k = 2, trymax = 100)
    
    # Prepare data for plotting
    nmds_plot <- data.frame(scores(nmds))
    nmds_plot$Site <- rownames(nmds_plot)
    nmds_plot <- merge(nmds_plot, site_data, by.x = "Site", by.y = "Site")
    
    # Create the plot
    p <- ggplot(nmds_plot, aes(x = NMDS1, y = NMDS2)) +
      geom_point(size = 1) +  # Plot points
      geom_text(aes(label = Site), vjust = 1, hjust = 1, size = 1) +  # Label points with site names
      labs(title = paste("NMDS -", month), x = "NMDS1", y = "NMDS2") +
      theme_minimal() +
      theme(legend.position = "none")  # Remove the legend
    
    # Add plot to list
    nmds_plots_by_month[[month]] <- p
  } else {
    warning(paste("Insufficient data for NMDS in", month))
  }
}

# Use patchwork to combine all plots into a single panel
combined_plot <- patchwork::wrap_plots(nmds_plots_by_month, ncol = 3)  # Adjust ncol to change layout
print(combined_plot)

##

# Create and print four figure panels
for (i in seq_along(month_groups)) {
  selected_months <- month_groups[[i]]
  selected_plots <- nmds_plots_by_month[selected_months]
  
  # Combine three plots per panel
  panel_plot <- wrap_plots(selected_plots, ncol = 3) +
    plot_annotation(title = paste("NMDS - Panel", i))
  
  print(panel_plot)  # Display each panel separately
}


# Define the months for each season
seasons <- list(
  Winter = c("Dec", "Jan", "Feb"),
  Spring = c("Mar", "Apr", "May"),
  Summer = c("Jun", "Jul", "Aug"),
  Autumn = c("Sep", "Oct", "Nov")
)

  
  # Initialize a list to store the plots for this season
  nmds_plots_for_season <- list()
  
  # Loop through the months in the current season
  for (month in seasons[[season]]) {
    
    # Load the data
    combined <- read.csv(paste0(month, ".csv"))
    site_data <- read.csv("Meta_data.csv")
    
    # Process the data
    combined_jit1 <- combined[,-c(1)]  # Remove species column if necessary
    jit_data <- column_to_rownames(combined_jit1, var = "species")
    jit_data <- t(jit_data)
    jit_data <- decostand(jit_data, "pa")  # Presence/Absence standardization
    
    # Remove empty rows
    jit_data <- jit_data[rowSums(jit_data) > 0, ]
    
    # Check if data is sufficient for NMDS
    if (nrow(jit_data) > 1) {
      # Calculate Bray-Curtis distance
      dist_matrix <- vegdist(jit_data, method = "jaccard")
      
      # Perform NMDS
      nmds <- metaMDS(dist_matrix, k = 2, trymax = 100)
      
      # Prepare data for plotting
      nmds_plot <- data.frame(scores(nmds))
      nmds_plot$Site <- rownames(nmds_plot)
      nmds_plot <- merge(nmds_plot, site_data, by.x = "Site", by.y = "Site")
      
      # Create the plot with labels instead of legend
      p <- ggplot(nmds_plot, aes(x = NMDS1, y = NMDS2)) +
        geom_point(size = 3) +  # Plot points
        geom_text(aes(label = Site), vjust = 1.5, hjust = 1.5, size = 3) +  # Label points with site names
        labs(title = paste("NMDS -", month), x = "NMDS1", y = "NMDS2") +
        theme_minimal() +
        theme(legend.position = "none")  # Remove the legend
      
      # Add plot to the list for the season
      nmds_plots_for_season[[month]] <- p
    } else {
      warning(paste("Insufficient data for NMDS in", month))
    }
  }
  
  # Create a separate plot for this season (you can adjust ncol to manage layout within a season)
  # Save or display each season as a separate figure
  season_plot <- patchwork::wrap_plots(nmds_plots_for_season, ncol = 3)  # Adjust ncol based on layout preference
  print(season_plot)  # Display the plot for the current season
  

##By month

  # List of months
  months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", 
              "Aug", "Sep", "Oct", "Nov", "Dec")
  
  # Initialize an empty list to store the aggregated data for each month
  aggregated_data <- list()
  
  # Loop through each month, read, and aggregate the data
  for (month in months) {
    # Read the data for the current month
    temp_data <- read.csv(paste0(month, ".csv"))
    
    # Assuming the first column is "species" 
    species_col <- temp_data[, 1]
    
    # Extract numeric data for species occurrences (assuming all other columns are numeric)
    temp_data_species <- temp_data[, -1]  # Drop the first column
    
    # Convert all columns to numeric (handle any non-numeric issues)
    temp_data_species <- sapply(temp_data_species, as.numeric)
    
    # Aggregate the data by summing across all rows (sites/replicates)
    aggregated_data[[month]] <- colSums(temp_data_species, na.rm = TRUE)
  }
  
  # Combine all aggregated data into a single data frame
  combined_aggregated_data <- do.call(rbind, aggregated_data)
  rownames(combined_aggregated_data) <- months  # Set row names as months
  
  # Perform NMDS on the aggregated data
  dist_matrix <- vegdist(combined_aggregated_data, method = "jaccard")
  nmds <- metaMDS(dist_matrix, k = 2, trymax = 100)
  
  # Extract NMDS scores for plotting
  nmds_scores <- as.data.frame(scores(nmds))
  nmds_scores$Month <- rownames(nmds_scores)  # Add month labels
  
  # Define seasons for each month
  season_labels <- c("Winter", "Winter", "Spring", "Spring", "Spring", 
                     "Summer", "Summer", "Summer", "Autumn", "Autumn", "Autumn", "Winter")
  
  # Add the season information to the NMDS scores
  nmds_scores$Season <- factor(season_labels, levels = c("Winter", "Spring", "Summer", "Autumn"))
  

  ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2, color = Season)) +
    geom_point(size = 4) +  # Add points
    geom_text(aes(label = Month), vjust = 1.5, hjust = 1.5, size = 3, color = "black") +  # Add labels with a different color
    labs(x = "NMDS1", y = "NMDS2", color = "Season") +  # Explicit legend title
    theme_minimal() +
    theme(
      legend.position = "right",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10)
    ) +
    scale_color_manual(
      values = c("Winter" = "lightblue", "Spring" = "lightgreen", "Summer" = "yellow", "Autumn" = "orange")
    )
  
  
  # Extract the stress value from the NMDS result
  stress_value <- nmds$stress
  
  # Convert the stress value to a formatted string
  stress_label <- paste("Stress =", round(stress_value, 3))
  
  # Plot the NMDS results and include the stress value
  ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2, color = Season)) +
    geom_point(size = 4) +  # Add points
    geom_text(aes(label = Month), vjust = 1.5, hjust = 1.5, size = 3, color = "black") +  # Add month labels
    labs(x = "NMDS1", y = "NMDS2", color = "Season") +  # Explicit legend title
    theme_minimal() +
    theme(
      legend.position = "right",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10)
    ) +
    scale_color_manual(
      values = c("Winter" = "lightblue", "Spring" = "lightgreen", "Summer" = "yellow", "Autumn" = "orange")
    ) +
    annotate("text", x = max(nmds_scores$NMDS1), y = min(nmds_scores$NMDS2), 
             label = stress_label, hjust = 1, vjust = 1, size = 4)
  
  
 