library(ggplot2)
library(dplyr)
library(plotly)
library(gganimate)
library(av)  # Add av library for MP4 rendering

# Read the data
data <- read.csv("land_ocean_index.csv")

# Create spiral coordinates
# 12 points per revolution (2*pi/12 radians per point)
data <- data %>%
  arrange(Year) %>%  # Make sure data is in chronological order
  mutate(
    point_number = row_number() - 1,  # Start from 0
    theta = (point_number * (2*pi/12)), # Angle in radians
    # Use temperature for radius instead of time
    radius = No_Smoothing + 2,  # Add 2 to ensure all values are positive and visible
    # Convert to cartesian coordinates
    x = radius * cos(theta),
    y = radius * sin(theta),
    temp = No_Smoothing,
    # Create lead coordinates for segments
    next_x = lead(radius * cos(theta)),
    next_y = lead(radius * sin(theta))
  )

# Create the base plot design
base_plot <- ggplot() +
  geom_segment(data = head(data, -1),
               aes(x = x, y = y, 
                   xend = next_x, yend = next_y,
                   color = temp),
               linewidth = 0.6, alpha = 0.7) +
  geom_point(data = data,
             aes(x = x, y = y,
                 color = temp,
                 text = paste("Year:", Year,
                            "\nTemperature:", sprintf("%.2f°C", temp))),
             size = 2.5) +
  scale_color_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    name = "Temperature\nAnomaly (°C)"
  ) +
  coord_equal() +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "grey10", color = NA),
    plot.background = element_rect(fill = "grey10", color = NA),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(color = "white"),
    plot.subtitle = element_text(color = "white"),
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white"),
    legend.background = element_rect(fill = "grey10")
  ) +
  labs(title = "Temperature Anomalies (1880-2024)")

# Create interactive plotly version
p_interactive <- ggplotly(base_plot, tooltip = "text") %>%
  layout(
    paper_bgcolor = "rgb(26,26,26)",  # grey10 in RGB
    plot_bgcolor = "rgb(26,26,26)",
    font = list(color = "white")
  )

# Save interactive version
htmlwidgets::saveWidget(p_interactive, "temperature_spiral_interactive.html")

# Create animated version
p_animated <- base_plot +
  labs(subtitle = "Year: {frame_time}") +
  transition_reveal(Year) +
  shadow_wake(wake_length = 1, alpha = TRUE) +
  ease_aes('linear')

# Save as MP4
anim <- animate(p_animated, 
               nframes = 300,
               fps = 30,
               width = 800, 
               height = 800,
               renderer = av_renderer(),
               res = 120)
anim_save("temperature_spiral.mp4", anim)

# Save as WebM
anim <- animate(p_animated, 
               nframes = 300,
               fps = 30,
               width = 800, 
               height = 800,
               renderer = av_renderer(vcodec = "libvpx-vp9"),
               res = 120)
anim_save("temperature_spiral.webm", anim)
