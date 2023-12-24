---
layout: single
title: "TidyTuesday and Haunted places in USA"
date: "13 October, 2023"
categories: bioinformatics data-visualisation tutorial
tags: data-visualisation tidytuesday
---


This week's TidyTuesday (10/10/2023) is representing the [USA haunting index](https://www.theshadowlands.net/places). The basemap of the USA was obtained from Nasa's [Blue Marble](https://visibleearth.nasa.gov/collection/1484/blue-marble) collection which I overlap with USA and states lines obtains from the ggpackage ```maps```. I make use of the ```ggshadow``` package for glowing points, for a spooky effect.

[Click here](https://github.com/amycjack/TidyTuesdays/tree/main/10.10.23%20Haunted%20Locations) for the repo, or see below for the R code.

![This is an image](/images/plot13102023.png)

```R
# Load required libraries
library(raster)
library(sf)
library(ggplot2)
library(ggspatial)
library(extrafont)

# Load the raster image
bm <- raster("eo_base_2020_clean_geo.tif")

# Crop the image to a specific extent
bm <- crop(bm, extent(-124.67, -66.95, 10, 60))

# Desaturate the image
bm_desat <- colorize(bm, to = "hsl")
bm_desat[[2]] <- 0.05 # Set the saturation ratio
set.RGB(bm_desat, 1:3, "hsl")
bm_desat <- colorize(bm_desat, to = "rgb")

# Plot the desaturated image
plotRGB(bm_desat)

# Add Google Fonts
font_add_google("Nosifer", "nosifer")
font_add_google("Ubuntu", "ubuntu")
showtext_auto()
body_font <- "ubuntu"
title_font <- "nosifer"

# Create a spatial object of the USA
usa <- st_as_sf(maps::map("state", fill = TRUE, plot = FALSE))

# Crop the desaturated image to match the extent of the USA
r2 <- crop(bm_desat, extent(usa))

# Mask the cropped image with the USA spatial object
r3 <- mask(r2, usa)

# Plot the masked image
plot(r3)

# Import fonts
font_import()

# Create the final plot
plot <- ggplot(usa) +
  layer_spatial(data = stack(r3)) +
  geom_sf(fill = "#e9f2e4", size = 2, colour = "#f5f5f5", alpha = 0.2) +
  geom_point(data = haunted_places, aes(x = longitude, y = latitude), size = 0.5, color = "#9cff90") +
  geom_point(data = haunted_places, aes(x = longitude, y = latitude), size = 0.1, color = "#ffffff") +
  coord_sf(xlim = c(-130, -65), ylim = c(23, 50), expand = FALSE) +
  labs(title = "<span style='color:#427a46'>Haunted places</span> within the USA",
       subtitle = "Locations of all hauntings and ghosts.") +
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#f5f5f5", color = NA), 
    panel.background = element_rect(fill = "#f5f5f5", color = NA), 
    legend.background = element_rect(fill = "#f5f5f5", color = NA),
    panel.border = element_blank()
  ) +
  theme(
    plot.title = element_markdown(size = 60, family = title_font, lineheight = 1.3, color = "#7DC286"),
    plot.subtitle = element_markdown(size = 50, family = title_font, lineheight = 1.3, color = "#7DC286"),
    plot.caption = element_text(size = 30, family = title_font, color = "#7DC286", hjust = .5, margin = margin(t = 10))
  ) +
  labs(caption = "#TidyTuesday 13-10-23: Haunted Places in the USA | Github: amycjack | Twitter: amycjack")

# Save the plot as a PDF file with a timestamp in the filename
ggsave(paste0("plot", format(Sys.time(), "%d%m%Y"), ".pdf"),
       dpi = 500,
       width = 6,
       height = 8)
```
