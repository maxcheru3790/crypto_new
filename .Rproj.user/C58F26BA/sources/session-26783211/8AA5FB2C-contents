library(tidyverse)
library(patchwork)

# Gender data
gender_data <- tibble(
  Group = c("Women", "Men"),
  Count = c(64, 36)
)

# Age data
age_data <- tibble(
  Group = c("Under 35", "35 and over"),
  Count = c(78, 22)
)

# Gender pie chart (no labels, with legend)
p1 <- ggplot(gender_data, aes(x = "", y = Count, fill = Group)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("Women" = "#FF69B4", "Men" = "#1E90FF")) +
  labs(title = "Gender Distribution", fill = "Gender") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

# Age pie chart (no labels, with legend)
p2 <- ggplot(age_data, aes(x = "", y = Count, fill = Group)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("Under 35" = "#00BFC4", "35 and over" = "#F8766D")) +
  labs(title = "Age Distribution", fill = "Age Group") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

# Combine vertically in portrait layout
(p1 / p2) +
  plot_annotation(
    title = "Gender and Age Distribution – CEP 2023",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"))
  )


library(tidyverse)

# Create the data
platform_reach <- tibble(
  Platform = c(
    "Website Hits", "Twitter Impressions", "Facebook Reach",
    "LinkedIn Impressions", "Instagram Reach", "Media Mentions"
  ),
  Count = c(1400, 139279, 16249, 44530, 89720, 15)
)

# Format values for nicer labels
platform_reach <- platform_reach %>%
  mutate(
    Label = scales::comma(Count),
    Platform = fct_reorder(Platform, Count)
  )

# Plot
ggplot(platform_reach, aes(x = Platform, y = Count, fill = Platform)) +
  geom_col(show.legend = FALSE, width = 0.7) +
  geom_text(aes(label = Label), vjust = -0.5, size = 4.5, fontface = "bold") +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = c(
    "#1f77b4", "#ff7f0e", "#2ca02c",
    "#d62728", "#9467bd", "#8c564b"
  )) +
  coord_flip()

  labs(
    title = "Platform Reach Overview – 2023",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    axis.text.x = element_text(angle = 20, hjust = 1, face = "bold")
  )


  library(tidyverse)
  
  # Create the data
  platform_reach <- tibble(
    Platform = c(
      "Website Hits", "Twitter Impressions", "Facebook Reach",
      "LinkedIn Impressions", "Instagram Reach", "Media Mentions"
    ),
    Count = c(1400, 139279, 16249, 44530, 89720, 15)
  )
  
  # Format values for nicer labels
  platform_reach <- platform_reach %>%
    mutate(
      Label = scales::comma(Count),
      Platform = fct_reorder(Platform, Count),
      Label_position = ifelse(Count < 2000, -5000, Count / 2)  # offset for small values
    )
  
  # Plot
  ggplot(platform_reach, aes(x = Platform, y = Count, fill = Platform)) +
    geom_col(show.legend = FALSE, width = 0.7) +
    geom_text(aes(y = Label_position, label = Label), 
              color = "white", size = 4.5, fontface = "bold") +
    scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.1))) +
    scale_fill_manual(values = c(
      "#1f77b4", "#ff7f0e", "#2ca02c",
      "#d62728", "#9467bd", "#8c564b"
    )) +
    coord_flip() +
    labs(
      title = "Platform Reach Overview – 2023",
      x = NULL,
      y = NULL
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
      axis.text.x = element_text(angle = 20, hjust = 1, face = "bold")
    )
  library(tidyverse)
  
  # Create the data
  platform_reach <- tibble(
    Platform = c(
      "Website Hits", "Twitter Impressions", "Facebook Reach",
      "LinkedIn Impressions", "Instagram Reach", "Media Mentions"
    ),
    Count = c(1400, 139279, 16249, 44530, 89720, 15)
  )
  
  # Format values for nicer labels
  platform_reach <- platform_reach %>%
    mutate(
      Label = scales::comma(Count),
      Platform = fct_reorder(Platform, Count)
    )
  
  # Plot with labels outside the bars
  ggplot(platform_reach, aes(x = Platform, y = Count, fill = Platform)) +
    geom_col(show.legend = FALSE, width = 0.7) +
    geom_text(aes(label = Label), 
              hjust = -0.1, size = 4.5, fontface = "bold") +
    scale_y_continuous(labels = scales::comma, 
                       expand = expansion(mult = c(0, 0.15))) +
    scale_fill_manual(values = c(
      "#1f77b4", "#ff7f0e", "#2ca02c",
      "#d62728", "#9467bd", "#8c564b"
    )) +
    coord_flip() +
    labs(
      title = "Platform Reach Overview – 2023",
      x = NULL,
      y = NULL
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
      axis.text.x = element_text(angle = 20, hjust = 1, face = "bold")
    )
  library(tidyverse)
  
  # Create the data
  platform_reach <- tibble(
    Platform = c(
      "Website Hits", "Twitter Impressions", "Facebook Reach",
      "LinkedIn Impressions", "Instagram Reach", "Media Mentions"
    ),
    Count = c(1400, 139279, 16249, 44530, 89720, 15)
  )
  
  # Format values for nicer labels
  platform_reach <- platform_reach %>%
    mutate(
      Label = scales::comma(Count),
      Platform = fct_reorder(Platform, Count)
    )
  
  # Plot with labels outside the bars
  ggplot(platform_reach, aes(x = Platform, y = Count)) +
    geom_col(fill = "#2C7BB6", width = 0.7) +
    geom_text(aes(label = Label), 
              hjust = -0.1, size = 4.5, fontface = "normal", color = "black") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    coord_flip() +
    labs(
      title = "Platform Reach Overview – 2023",
      x = NULL,
      y = NULL
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
      axis.text.x = element_blank(),         # Remove x-axis numbers
      axis.ticks.x = element_blank(),        # Remove x-axis ticks
      axis.text.y = element_text(face = "bold", size = 13),  # Emphasize y-axis labels
      panel.grid.major.y = element_blank()   # Optional: remove grid lines on y-axis
    )
  
  

library(ggplot2)
library(dplyr)

# Create the data
workspace_data <- data.frame(
  Space_Type = rep(c("Coworking Space", "Event Space"), each = 2),
  User_Type = rep(c("Free Users", "Paid Users"), times = 2),
  Count = c(48, 0, 11, 0)
)

# Plot
ggplot(workspace_data, aes(x = Space_Type, y = Count, fill = User_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Free Users" = "#31a354", "Paid Users" = "#de2d26")) +
  labs(
    title = "Workspace Utilization",
    x = "Space Type",
    y = "Number of Users",
    fill = "User Type"
  ) +
  theme_minimal(base_size = 14)



library(tidyverse)

# Create the data
platform_reach <- tibble(
  Platform = c(
    "Website Hits", "Twitter Impressions", "Facebook Reach",
    "LinkedIn Impressions", "Instagram Reach", "Media Mentions"
  ),
  Count = c(1400, 139279, 16249, 44530, 89720, 15)
)

# Format values for nicer labels
platform_reach <- platform_reach %>%
  mutate(
    Label = scales::comma(Count),
    Platform = fct_reorder(Platform, Count)
  )

# Plot
ggplot(platform_reach, aes(x = Platform, y = Count)) +
  geom_col(fill = "#2C7BB6", width = 0.7) +
  geom_text(aes(label = Label), 
            hjust = -0.1, size = 4.5, fontface = "bold", color = "black") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  coord_flip() +
  labs(
    title = "Platform Reach Overview – 2023",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(face = "plain", size = 13),  # ← plain font here
    panel.grid.major.y = element_blank()
  )


# Load libraries
library(tidyverse)

# Create the data
training_data <- tibble(
  Activity = c(
    "Individuals Trained",
    "Startups Trained (Bootcamps etc.)",
    "Incubation Support",
    "Acceleration Support",
    "Startups Funded (Grants)"
  ),
  Count = c(3297, 1835, 3, 3, 6)
)

# Format label
training_data <- training_data %>%
  mutate(Label = scales::comma(Count),
         Activity = fct_reorder(Activity, Count))

# Plot
ggplot(training_data, aes(x = Activity, y = Count)) +
  geom_col(fill = "#2ca02c", width = 0.7) +
  geom_text(aes(label = Label), hjust = -0.1, size = 5, fontface = "bold", color = "black") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(
    title = "Training & Capacity Building – 2023",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    axis.text.y = element_text(color = "black", size = 13),
    axis.text.x = element_blank(),
    panel.grid.major.y = element_blank()
  )


library(tidyverse)

# Data for bar chart
training_bar_data <- tibble(
  Category = c("Individuals Trained", "Startups Trained"),
  Count = c(3297, 1835)
)

# Reorder for plotting
training_bar_data <- training_bar_data %>%
  mutate(Category = fct_reorder(Category, Count))

# Bar chart
ggplot(training_bar_data, aes(x = Category, y = Count)) +
  geom_col(fill = "#1f77b4", width = 0.6) +
  geom_text(aes(label = Count), hjust = -0.1, size = 6, fontface = "bold", color = "black") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(
    title = "Training Reach in 2023",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 13),
    panel.grid.major.y = element_blank()
  )
# Other startup support summary
other_support <- tibble::tibble(
  Activity = c(
    "Incubation Support", 
    "Acceleration Support", 
    "Startups Funded (Grants)"
  ),
  Count = c(3, 3, 6)
)

# Print as card-like text
library(glue)

other_support %>%
  mutate(Card = glue("{Activity}: {Count}")) %>%
  pull(Card)

library(tidyverse)
library(patchwork)

# ---- 1. Bar Chart: Training Data ----
training_bar_data <- tibble(
  Category = c("Individuals Trained", "Startups Trained"),
  Count = c(3297, 1835)
) %>%
  mutate(Category = fct_reorder(Category, Count))

bar_plot <- ggplot(training_bar_data, aes(x = Category, y = Count)) +
  geom_col(fill = "#1f77b4", width = 0.6) +
  geom_text(aes(label = Count), hjust = -0.1, size = 6, fontface = "bold", color = "black") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(
    title = "Training Reach in 2023",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 13),
    panel.grid.major.y = element_blank()
  )

# ---- 2. Card-like Bars: Support Data ----
support_data <- tibble(
  Activity = c("Incubation Support", "Acceleration Support", "Startups Funded (Grants)"),
  Count = c(3, 3, 6)
) %>%
  mutate(Activity = fct_reorder(Activity, Count))

support_plot <- ggplot(support_data, aes(x = Activity, y = Count, fill = Activity)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = Count), vjust = -0.5, size = 6, fontface = "bold") +
  scale_fill_manual(values = c("#ff7f0e", "#2ca02c", "#9467bd")) +
  labs(title = "Startup Support Activities", x = NULL, y = NULL) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "none"
  )

# ---- Combine Plots ----
combined_plot <- bar_plot + support_plot +
  plot_layout(ncol = 2) +
  plot_annotation(title = "Training & Startup Support Overview (2023)",
                  theme = theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold")))

# Display
combined_plot



library(tidyverse)
library(patchwork)

# ---- 1. Bar Chart: Training Data ----
training_bar_data <- tibble(
  Category = c("Individuals Trained", "Startups Trained"),
  Count = c(3297, 1835)
) %>%
  mutate(Category = fct_reorder(Category, Count))

bar_plot <- ggplot(training_bar_data, aes(x = Category, y = Count)) +
  geom_col(fill = "#1f77b4", width = 0.6) +
  geom_text(aes(label = Count), hjust = -0.1, size = 6, fontface = "bold", color = "black") +
  coord_flip(clip = "off") +  # prevent label clipping
  scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
  labs(
    title = "Training Reach in 2023",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 13),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(10, 40, 10, 10)  # extra right space for labels
  )

# ---- 2. Support Data ----
support_data <- tibble(
  Activity = c("Incubation", "Acceleration", "Startups Funded (Grants)"),
  Count = c(3, 3, 6)
) %>%
  mutate(Activity = fct_reorder(Activity, Count))

support_plot <- ggplot(support_data, aes(x = Activity, y = Count, fill = Activity)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = Count), vjust = -0.5, size = 6, fontface = "bold") +
  scale_fill_manual(values = c("#ff7f0e", "#2ca02c", "#9467bd")) +
  labs(title = "Startup Support Activities", x = NULL, y = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.3))) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "none",
    plot.margin = margin(10, 10, 10, 10)
  )

# ---- Combine Plots ----
combined_plot <- bar_plot + support_plot +
  plot_layout(ncol = 2, widths = c(1.2, 1)) +
  plot_annotation(title = "Training & Startup Support Overview (2023)",
                  theme = theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold")))

# Display the plot
combined_plot






library(tidyverse)

# Data for research outputs
research_data <- tibble(
  Output = c(
    "Policy Briefs Published",
    "Research Publications / Working Papers",
    "Citations",
    "Research Partnerships"
  ),
  Count = c(3, 2, 33, 12)
) %>%
  mutate(Output = fct_reorder(Output, Count))

# Plot
ggplot(research_data, aes(x = Output, y = Count)) +
  geom_col(fill = "#2c7fb8", width = 0.6) +
  geom_text(aes(label = Count), hjust = -0.1, size = 5.5, fontface = "bold", color = "black") +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
  labs(
    title = "Research Outputs and Publications – 2023",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 12),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(10, 40, 10, 10)
  )



library(tidyverse)

# Filtered data
platform_reach <- tibble(
  Platform = c("Twitter", "Facebook", "Instagram", "LinkedIn"),
  Count = c(139279, 16249, 89720, 44530)
)

# Format values
platform_reach <- platform_reach %>%
  mutate(
    Label = scales::comma(Count),
    Platform = fct_reorder(Platform, Count)
  )

# Plot
ggplot(platform_reach, aes(x = Platform, y = Count)) +
  geom_col(fill = "#2ca02c", width = 0.7) +
  geom_text(aes(label = Label), hjust = -0.1, size = 5, fontface = "bold") +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.15))) +
  coord_flip() +
  labs(
    title = "Social Media Platform Reach ",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_text(face = "plain"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18)
  )



library(tidyverse)

# Create the data
collaboration_data <- tibble(
  Metric = c("Strategic Partnerships", "MoUs Signed", "Collaborative Events"),
  Count = c(15, 6, 12)
)

# Plot
ggplot(collaboration_data, aes(x = Metric, y = Count, fill = Metric)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = Count), vjust = -0.3, size = 5, fontface = "bold") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = c("#2ca02c", "#1f77b4", "#ff7f0e")) +
  labs(
    title = "Strategic Collaboration and Networks",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.text.x = element_text(face = "bold", size = 12),
    axis.text.y = element_blank(),
    panel.grid.major.x = element_blank()
  )


library(tidyverse)

# Create the dataset
collaboration_data <- tibble(
  Year = rep(c("2023", "2024"), each = 2),
  Metric = rep(c("Strategic Partnerships", "Collaborative Events"), times = 2),
  Count = c(9, 7, 6, 5)  # example values: adjust to your actual data
)

# Plot
ggplot(collaboration_data, aes(x = Metric, y = Count, fill = Year)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = Count), position = position_dodge(width = 0.7), 
            vjust = -0.4, size = 5, fontface = "bold", color = "black") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = c("2023" = "#1f77b4", "2024" = "#ff7f0e")) +
  labs(
    title = "Strategic Collaboration and Networks (2023 vs 2024)",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.text.x = element_text(face = "bold", size = 12),
    axis.text.y = element_blank(),
    panel.grid.major.x = element_blank()
  )

library(tidyverse)

# Combined data (summed or available counts only, no year labels)
collab_combined <- tibble(
  Metric = c("Strategic Partnerships", "Collaborative Events"),
  Count = c(9 + 6, 7 + 5)  # Replace with actual combined values or just use available year
)

# Plot
ggplot(collab_combined, aes(x = Metric, y = Count, fill = Metric)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = Count), vjust = -0.4, size = 5, fontface = "bold", color = "black") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e")) +
  labs(
    title = "Strategic Collaboration and Networks",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.text.x = element_text(face = "bold", size = 12),
    axis.text.y = element_blank(),
    panel.grid.major.x = element_blank()
  )


library(tidyverse)

# Combined demographic and inclusion data (example values)
demo_inclusion <- tibble(
  Metric = c("Women Trained", "Youth Trained", "PWDs Trained", "Startups Trained", "Individuals Trained"),
  Count = c(120, 340, 18, 45, 520)  # Replace with your actual totals across 2023 & 2024
)

# Plot (bar chart)
ggplot(demo_inclusion, aes(x = fct_reorder(Metric, Count), y = Count, fill = Metric)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = Count), hjust = -0.1, size = 5, fontface = "bold", color = "black") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  scale_fill_brewer(palette = "Set2") +
  coord_flip() +
  labs(
    title = "Demographic and Inclusion",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.text.x = element_blank(),
    axis.text.y = element_text(face = "plain", size = 12),
    panel.grid.major.y = element_blank()
  )



library(tidyverse)

# Combined demographic and inclusion data
demo_inclusion <- tibble(
  Metric = c("Women Trained", "Youth Trained", "PWDs Trained", "Startups Trained", "Individuals Trained"),
  Count = c(120, 340, 18, 45, 520)  # Replace with your actual totals
)

# Plot with consistent color
ggplot(demo_inclusion, aes(x = fct_reorder(Metric, Count), y = Count)) +
  geom_col(width = 0.6, fill = "#1f77b4") +  # Consistent blue color
  geom_text(aes(label = Count), hjust = -0.1, size = 5, fontface = "bold", color = "black") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  coord_flip() +
  labs(
    title = "Demographic and Inclusion",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.text.x = element_blank(),
    axis.text.y = element_text(face = "plain", size = 12),
    panel.grid.major.y = element_blank()
  )

# Load required libraries
library(tidyverse)
library(patchwork)

# ---- Gender Distribution Data ----
gender_data <- tibble(
  Gender = c("Women", "Men"),
  Percent = c(64, 36)
) %>%
  mutate(
    ypos = cumsum(Percent) - 0.5 * Percent,
    label = paste0(Gender, "\n", Percent, "%")
  )

# ---- Age Distribution Data ----
age_data <- tibble(
  AgeGroup = c("35 and below", "Above 35"),
  Percent = c(78, 22)
) %>%
  mutate(
    ypos = cumsum(Percent) - 0.5 * Percent,
    label = paste0(AgeGroup, "\n", Percent, "%")
  )

# ---- Gender Pie Chart ----
gender_plot <- ggplot(gender_data, aes(x = "", y = Percent, fill = Gender)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(y = ypos, label = label), color = "black", size = 5, fontface = "bold") +
  scale_fill_manual(values = c("Women" = "#FF69B4", "Men" = "#1E90FF")) +
  labs(title = "Gender Distribution") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    legend.position = "none"
  )

# ---- Age Pie Chart ----
age_plot <- ggplot(age_data, aes(x = "", y = Percent, fill = AgeGroup)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(y = ypos, label = label), color = "black", size = 5, fontface = "bold") +
  scale_fill_manual(values = c("35 and below" = "#2ca02c", "Above 35" = "#d62728")) +
  labs(title = "Age Distribution") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    legend.position = "none"
  )

# ---- Combine Charts Side-by-Side ----
gender_plot + age_plot





# Load required libraries
library(tidyverse)

# Workspace Utilization data (combined for 2023 and 2024)
workspace_data <- tibble(
  Type = c("Co-working Spaces", "Private Offices", "Meeting Rooms", "Event/Training Rooms"),
  Count = c(95, 45, 30, 20)
)

# Plot
ggplot(workspace_data, aes(x = fct_reorder(Type, Count), y = Count, fill = Type)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = Count), hjust = -0.1, size = 5, fontface = "bold") +
  coord_flip() +
  scale_fill_manual(values = rep("#1f77b4", 4)) +
  labs(
    title = "Workspace Utilization",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.text.y = element_text(face = "plain"),
    axis.text.x = element_blank(),
    axis.ticks = element_blank()
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))




# Load required libraries
library(tidyverse)
library(patchwork)

# ----------------------
# Workspace Utilization Data
# ----------------------
workspace_data <- tibble(
  Type = c("Co-working Spaces", "Private Offices", "Meeting Rooms", "Event/Training Rooms"),
  Count = c(95, 45, 30, 20)
)

# Bar Chart
bar_plot <- ggplot(workspace_data, aes(x = fct_reorder(Type, Count), y = Count, fill = Type)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = Count), hjust = -0.1, size = 5, fontface = "bold") +
  coord_flip() +
  scale_fill_manual(values = rep("#1f77b4", 4)) +
  labs(
    title = "Workspace Utilization",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.text.y = element_text(face = "plain"),
    axis.text.x = element_blank(),
    axis.ticks = element_blank()
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

# ----------------------
# Event vs Co-working Space Users (Pie Chart)
# ----------------------
user_data <- tibble(
  Category = c("Event Space Users", "Co-working Space Users"),
  Count = c(600, 1400)
)

user_data <- user_data %>%
  mutate(
    Fraction = Count / sum(Count),
    Label = paste0(Category, "\n", scales::percent(Fraction))
  )

pie_chart <- ggplot(user_data, aes(x = "", y = Count, fill = Category)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5), size = 5) +
  scale_fill_manual(values = c("#ff7f0e", "#1f77b4")) +
  labs(
    title = "Users by Space Type",
    x = NULL,
    y = NULL
  ) +
  theme_void(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    legend.position = "none"
  )

# ----------------------
# Combine Both Plots
# ----------------------
bar_plot + pie_chart + plot_layout(ncol = 2)




# Bar chart for startups + individuals
bar_chart <- ggplot(program_metrics, aes(x = fct_reorder(Category, Count), y = Count, fill = Category)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = Count), vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_manual(values = c("#1f77b4", "#1f77b4")) +
  labs(title = "Program Delivery – Key Training Outputs", x = NULL, y = NULL) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    axis.text.y = element_blank(),
    axis.text.x = element_text(face = "bold"),
    axis.ticks = element_blank()
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

# Card-style bar chart
card_plot <- ggplot(card_metrics, aes(x = fct_reorder(Category, Count), y = Count, fill = Category)) +
  geom_col(width = 0.5, show.legend = FALSE) +
  geom_text(aes(label = Count), vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_manual(values = rep("#ff7f0e", 3)) +
  labs(title = "Additional Program Support", x = NULL, y = NULL) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    axis.text.y = element_blank(),
    axis.text.x = element_text(face = "bold"),
    axis.ticks = element_blank()
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.3)))

# Combine both visuals
bar_chart + card_plot + plot_layout(ncol = 2)




# Load libraries
library(tidyverse)
library(patchwork)

# Combined totals for both years (summed)
program_metrics <- tibble(
  Category = c("Startups Supported", "Individuals Trained"),
  Count = c(1835, 3297)
)

# Summary metrics combined (2023 + 2024)
card_metrics <- tibble(
  Category = c("Startups Funded", "Accelerated", "Incubated"),
  Count = c(6, 3, 3)
)

# Bar chart: Programs delivered (startups + individuals)
bar_chart <- ggplot(program_metrics, aes(x = fct_reorder(Category, Count), y = Count, fill = "Programs")) +
  geom_col(width = 0.7, fill = "#1f77b4", show.legend = FALSE) +
  geom_text(aes(label = Count), vjust = -0.5, size = 5, fontface = "bold") +
  labs(title = "Program Delivery ", x = NULL, y = NULL) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    axis.text.y = element_blank(),
    axis.text.x = element_text(face = "bold"),
    axis.ticks = element_blank()
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

# Card-style bar chart: Additional support
card_plot <- ggplot(card_metrics, aes(x = fct_reorder(Category, Count), y = Count, fill = "Cards")) +
  geom_col(width = 0.5, fill = "#ff7f0e", show.legend = FALSE) +
  geom_text(aes(label = Count), vjust = -0.5, size = 5, fontface = "bold") +
  labs(title = "Additional Program Support", x = NULL, y = NULL) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    axis.text.y = element_blank(),
    axis.text.x = element_text(face = "bold"),
    axis.ticks = element_blank()
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.3)))

# Combine both plots
bar_chart + card_plot + plot_layout(ncol = 2)




# Load libraries
library(tidyverse)
library(ggtext)

# Combined Training and Capacity Building Data
training_data <- tibble(
  Category = c(
    "Individuals Trained", 
    "Startups Trained", 
    "Workshops & Bootcamps", 
    "Fellowships", 
    "Seminars & Webinars"
  ),
  Count = c(
    3297,     # Total individuals trained
    1835,     # Startups supported through training
    28,       # Example count (replace with actual if available)
    10,       # Example
    22        # Example
  )
)

# Create Bar Chart
ggplot(training_data, aes(x = fct_reorder(Category, Count), y = Count)) +
  geom_col(fill = "#2ca02c", width = 0.7) +
  geom_text(aes(label = Count), hjust = -0.1, size = 5, fontface = "bold", color = "black") +
  coord_flip() +
  labs(
    title = "Training & Capacity Building Overview",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    axis.text.x = element_blank(),
    axis.text.y = element_text(face = "bold"),
    axis.ticks = element_blank()
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2)))



# Load libraries
library(tidyverse)
library(patchwork)

# Data: Pie chart section (Individual vs Startup Trained)
pie_data <- tibble(
  Group = c("Individuals Trained", "Startups Trained"),
  Count = c(3297, 1835)
)

# Data: Bar chart section
bar_data <- tibble(
  Category = c("Workshops & Bootcamps", "Fellowships", "Seminars & Webinars"),
  Count = c(28, 10, 22)  # Replace with actual data if different
)

# Pie Chart
pie_chart <- ggplot(pie_data, aes(x = "", y = Count, fill = Group)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(Group, "\n", Count)), 
            position = position_stack(vjust = 0.5), size = 5) +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e")) +
  labs(title = "Training Participants") +
  theme_void(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# Bar Chart
bar_chart <- ggplot(bar_data, aes(x = fct_reorder(Category, Count), y = Count)) +
  geom_col(fill = "#2ca02c", width = 0.6) +
  geom_text(aes(label = Count), hjust = -0.1, size = 5, fontface = "bold") +
  coord_flip() +
  labs(title = "Other Capacity Building Activities", x = NULL, y = NULL) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.y = element_text(face = "bold"),
    axis.text.x = element_blank(),
    axis.ticks = element_blank()
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2)))

# Combine Pie + Bar Charts Side-by-Side
combined_chart <- pie_chart + bar_chart + plot_layout(widths = c(1, 1.1))

# Display
combined_chart






# Load required libraries
library(tidyverse)

# Bar chart data: Website + Media Mentions
web_media_data <- tibble(
  Platform = c("Website Hits", "Media Mentions"),
  Count = c(19400, 29)
)

# --- Bar Chart ---
ggplot(web_media_data, aes(x = fct_reorder(Platform, Count), y = Count)) +
  geom_col(fill = "#2ca02c", width = 0.5) +
  geom_text(aes(label = base::prettyNum(Count, big.mark = ",")), 
            hjust = -0.1, size = 4) +
  coord_flip() +
  labs(title = "Digital Engagement", x = NULL, y = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2)))+
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_blank(),
    axis.text = element_text(size = 12),
    axis.text.x = element_blank(),
    axis.ticks = element_blank()
  ) 



# Load required libraries
library(tidyverse)

# Create the data
research_data <- tibble(
  Category = c("Reports Produced", "Publications"),
  Count = c(6, 3)
)

# Compute label positions
research_data <- research_data %>%
  arrange(desc(Category)) %>%
  mutate(
    fraction = Count / sum(Count),
    ymax = cumsum(fraction),
    ymin = c(0, head(ymax, n = -1)),
    label_pos = (ymax + ymin) / 2,
    label = paste0(Category, "\n", Count)
  )

# Plot pie chart
ggplot(research_data, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 2, fill = Category)) +
  geom_rect() +
  coord_polar(theta = "y") +
  xlim(c(1, 4)) +
  theme_void() +
  geom_text(aes(x = 3, y = label_pos, label = label), color = "black", size = 5) +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e")) +
  labs(title = "Research Outputs and Publications ") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.position = "none"
  )





library(tidyverse)

# Example combined data of funding partners
funding_data <- tibble(
  Partner = c(
    "Bill & Melinda Gates Foundation",
    "UNICEF",
    "World Bank",
    "UNDP",
    "Mastercard Foundation",
    "GIZ",
    "Sida",
    "ITU"
  ),
  Count = c(3, 2, 2, 1, 1, 1, 1, 1)
)

# Plot the bar chart
ggplot(funding_data, aes(x = reorder(Partner, Count), y = Count, fill = Partner)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Funding Partners (2023 & 2024 Combined)",
    x = NULL,
    y = "Number of Collaborations"
  ) +
  geom_text(aes(label = Count), hjust = -0.2, size = 5) +
  theme_minimal(base_size = 14) +
  scale_fill_brewer(palette = "Paired") +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.y = element_text(size = 12)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2)))




library(tidyverse)

# Sample data: Replace 'Count' with actual values from your combined dataset
funding_data <- tibble(
  Partner = c(
    "Bill & Melinda Gates Foundation",
    "UNICEF",
    "World Bank",
    "UNDP",
    "Mastercard Foundation",
    "GIZ",
    "Sida",
    "ITU"
  ),
  Count = c(3, 2, 2, 1, 1, 1, 1, 1)
)

# Define consistent fill color
fill_color <- "#2c7fb8"  # A clean blue tone

# Plot
ggplot(funding_data, aes(x = reorder(Partner, Count), y = Count)) +
  geom_col(fill = fill_color, width = 0.7) +
  geom_text(aes(label = Count), hjust = -0.2, color = "black", size = 5, fontface = "bold") +
  coord_flip() +
  labs(
    title = "Funding Partners (2023 & 2024 Combined)",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
    axis.text.x = element_blank(),  # Remove x-axis numbering
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 12)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2)))




library(tidyverse)

# Sample data: Replace 'value' with your actual data
area_data <- tibble(
  Year = c(2023, 2024),
  Value = c(5, 10)  # change values accordingly
)

# Create area plot
ggplot(area_data, aes(x = Year, y = Value)) +
  geom_area(fill = "green", alpha = 0.7) +
  scale_x_continuous(breaks = c(2023, 2024)) +
  scale_y_continuous(limits = c(0, 12), breaks = 0:12) +
  labs(
    title = "Continental Reach and Market Penetration",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )



library(tidyverse)

# Sample data: Replace 'value' with your actual data
area_data <- tibble(
  Year = c(2023, 2024),
  Value = c(5, 10)  # change values accordingly
)

# Create area plot
ggplot(area_data, aes(x = Year, y = Value)) +
  geom_area(fill = "#008000", alpha = 0.7) +  # R comment green
  scale_x_continuous(breaks = c(2023, 2024)) +
  scale_y_continuous(limits = c(0, 12), breaks = 0:12) +
  labs(
    title = "Continental Reach and Market Penetration",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),  # center and bold
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )







library(tidyverse)
library(scales)

# Generalized combined funding summary
funding_summary <- tibble(
  Category = c("Total Funds Raised", "Number of Funding Partners", "Average Grant Size"),
  Value = c(3500000, 8, 437500)  # Replace with real totals if needed
)

# Plot as horizontal bars
ggplot(funding_summary, aes(x = fct_reorder(Category, Value), y = Value, fill = Category)) +
  geom_col(show.legend = FALSE, width = 0.6) +
  geom_text(aes(label = comma(Value)), hjust = -0.1, size = 5, fontface = "bold") +
  coord_flip() +
  labs(
    title = "Funding Metrics",
    x = NULL,
    y = NULL
  ) +
  scale_fill_manual(values = rep("blue", nrow(funding_summary))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    axis.text.y = element_text(face = "plain"),
    axis.text.x = element_blank(),
    panel.grid.major.x = element_blank()
  )



library(tidyverse)
library(scales)

# Combined funding summary
funding_summary <- tibble(
  Category = c("Total Funds Raised", "Number of Funding Partners", "Average Grant Size"),
  Value = c(3500000, 8, 437500)
)

# Separate for bar graph and card
bar_data <- funding_summary %>% 
  filter(Category != "Number of Funding Partners")

card_value <- funding_summary %>%
  filter(Category == "Number of Funding Partners") %>%
  pull(Value)

# Plot
ggplot(bar_data, aes(x = fct_reorder(Category, Value), y = Value, fill = Category)) +
  geom_col(show.legend = FALSE, width = 0.6) +
  geom_text(aes(label = comma(Value)), hjust = -0.1, size = 5, fontface = "bold") +
  # Add card-style annotation for funding partners
  annotate("label", 
           x = 2.3, y = max(bar_data$Value) * 0.7,
           label = paste("Funding Partners:\n", card_value),
           fill = "#f0f0f0", color = "black", fontface = "bold",
           size = 5, label.size = 0.5, label.r = unit(0.15, "lines")) +
  coord_flip() +
  labs(
    title = "Funding Metrics",
    x = NULL,
    y = NULL
  ) +
  scale_fill_manual(values = rep("#1f77b4", nrow(bar_data))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.3))) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    axis.text.y = element_text(face = "plain"),
    axis.text.x = element_blank(),
    panel.grid.major.x = element_blank()
  )



library(tidyverse)
library(scales)

# Combined funding summary (excluding "Number of Funding Partners")
funding_summary <- tibble(
  Category = c("Total Funds Raised", "Average Grant Size"),
  Value = c(3500000, 437500)
)

# Plot
ggplot(funding_summary, aes(x = fct_reorder(Category, Value), y = Value, fill = Category)) +
  geom_col(show.legend = FALSE, width = 0.6) +
  geom_text(aes(label = comma(Value)), hjust = -0.1, size = 5, fontface = "bold") +
  coord_flip() +
  labs(
    title = "Funding Metrics",
    x = NULL,
    y = NULL
  ) +
  scale_fill_manual(values = rep("#1f77b4", nrow(funding_summary))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    axis.text.y = element_text(face = "plain"),
    axis.text.x = element_blank(),
    panel.grid.major.x = element_blank()
  )



library(tidyverse)

# Card data
funding_card_data <- tibble(
  Label = "Funding Partners",
  Value = 8
)

# Card-style plot
ggplot(funding_card_data) +
  geom_text(aes(x = 1, y = 1.4, label = Label), size = 6, fontface = "bold") +
  geom_text(aes(x = 1, y = 0.6, label = Value), size = 12, fontface = "bold", color = "#1f77b4") +
  xlim(0.5, 1.5) +
  ylim(0, 2) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#f8f9fa", color = "grey80", linewidth = 1),
    plot.margin = margin(20, 20, 20, 20)
  )




# Load required libraries
library(tidyverse)
library(patchwork)

# ---- Gender Distribution Data ----
gender_data <- tibble(
  Gender = c("Women", "Men"),
  Percent = c(64, 36)
) %>%
  arrange(desc(Gender)) %>%
  mutate(
    ypos = cumsum(Percent) - 0.5 * Percent,
    label = paste0(Gender, "\n", Percent, "%")
  )

# ---- Age Distribution Data ----
age_data <- tibble(
  AgeGroup = c("35 and below", "Above 35"),
  Percent = c(78, 22)
) %>%
  arrange(desc(AgeGroup)) %>%
  mutate(
    ypos = cumsum(Percent) - 0.5 * Percent,
    label = paste0(AgeGroup, "\n", Percent, "%")
  )

# ---- Gender Pie Chart ----
gender_plot <- ggplot(gender_data, aes(x = "", y = Percent, fill = Gender)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_label(aes(y = ypos, label = label), 
             color = "black", fill = "white", 
             size = 5, fontface = "bold", label.size = 0) +
  scale_fill_manual(values = c("Women" = "#FF69B4", "Men" = "#1E90FF")) +
  labs(title = "Gender Distribution") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    legend.position = "none"
  )

# ---- Age Pie Chart ----
age_plot <- ggplot(age_data, aes(x = "", y = Percent, fill = AgeGroup)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_label(aes(y = ypos, label = label), 
             color = "black", fill = "white", 
             size = 5, fontface = "bold", label.size = 0) +
  scale_fill_manual(values = c("35 and below" = "#2ca02c", "Above 35" = "#d62728")) +
  labs(title = "Age Distribution") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    legend.position = "none"
  )

# ---- Combine Charts Side-by-Side ----
gender_plot + age_plot

library(tidyverse)
library(patchwork)

# ---- Gender Distribution Data ----
gender_data <- tibble(
  Gender = c("Women", "Men"),
  Percent = c(64, 36)
) %>%
  arrange(desc(Gender)) %>%
  mutate(
    ypos = cumsum(Percent) - 0.5 * Percent,
    label = paste0(Gender, "\n", Percent, "%")
  )

# ---- Age Distribution Data ----
age_data <- tibble(
  AgeGroup = c("35 and below", "Above 35"),
  Percent = c(78, 22)
) %>%
  arrange(desc(AgeGroup)) %>%
  mutate(
    ypos = cumsum(Percent) - 0.5 * Percent,
    label = paste0(AgeGroup, "\n", Percent, "%")
  )

# ---- Gender Donut Chart ----
gender_plot <- ggplot(gender_data, aes(x = 2, y = Percent, fill = Gender)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_label(aes(y = ypos, label = label), 
             color = "black", fill = "white", 
             size = 5, fontface = "bold", label.size = 0) +
  scale_fill_manual(values = c("Women" = "#FF69B4", "Men" = "#1E90FF")) +
  xlim(0.5, 2.5) +
  labs(title = "Gender Distribution") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    legend.position = "none"
  )

# ---- Age Donut Chart ----
age_plot <- ggplot(age_data, aes(x = 2, y = Percent, fill = AgeGroup)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_label(aes(y = ypos, label = label), 
             color = "black", fill = "white", 
             size = 5, fontface = "bold", label.size = 0) +
  scale_fill_manual(values = c("35 and below" = "#2ca02c", "Above 35" = "#d62728")) +
  xlim(0.5, 2.5) +
  labs(title = "Age Distribution") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    legend.position = "none"
  )

# ---- Combine Charts Side-by-Side ----
gender_plot + age_plot



library(tidyverse)
library(patchwork)

# ---- Gender Distribution Data ----
gender_data <- tibble(
  Gender = c("Women", "Men"),
  Percent = c(64, 36)
) %>%
  arrange(desc(Percent)) %>%
  mutate(
    ypos = cumsum(Percent) - 0.5 * Percent
  )

# ---- Age Distribution Data ----
age_data <- tibble(
  AgeGroup = c("35 and below", "Above 35"),
  Percent = c(78, 22)
) %>%
  arrange(desc(Percent)) %>%
  mutate(
    ypos = cumsum(Percent) - 0.5 * Percent
  )

# ---- Gender Donut Chart ----
gender_plot <- ggplot(gender_data, aes(x = 2, y = Percent, fill = Gender)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  annotate("text", x = 0.5, y = 0, label = paste0(gender_data$Percent[1], "%"),
           size = 8, fontface = "bold", color = "black") +
  scale_fill_manual(values = c("Women" = "#FF69B4", "Men" = "#1E90FF")) +
  xlim(0.5, 2.5) +
  labs(title = "Gender Distribution", fill = "Gender") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    legend.title = element_text(face = "bold"),
    legend.position = "right"
  )

# ---- Age Donut Chart ----
age_plot <- ggplot(age_data, aes(x = 2, y = Percent, fill = AgeGroup)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  annotate("text", x = 0.5, y = 0, label = paste0(age_data$Percent[1], "%"),
           size = 8, fontface = "bold", color = "black") +
  scale_fill_manual(values = c("35 and below" = "#2ca02c", "Above 35" = "#d62728")) +
  xlim(0.5, 2.5) +
  labs(title = "Age Distribution", fill = "Age Group") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    legend.title = element_text(face = "bold"),
    legend.position = "right"
  )

# ---- Combine Charts Side-by-Side ----
gender_plot + age_plot




library(tidyverse)
library(patchwork)

# ---- Gender Distribution Data ----
gender_data <- tibble(
  Gender = c("Women", "Men"),
  Percent = c(64, 36)
) %>%
  arrange(desc(Percent)) %>%
  mutate(ypos = cumsum(Percent) - 0.5 * Percent)

# Define gender colors
gender_colors <- c("Women" = "#FF69B4", "Men" = "#1E90FF")

# ---- Age Distribution Data ----
age_data <- tibble(
  AgeGroup = c("35 and below", "Above 35"),
  Percent = c(78, 22)
) %>%
  arrange(desc(Percent)) %>%
  mutate(ypos = cumsum(Percent) - 0.5 * Percent)

# Define age group colors
age_colors <- c("35 and below" = "#2ca02c", "Above 35" = "#d62728")

# ---- Gender Donut Chart ----
gender_plot <- ggplot(gender_data, aes(x = 2, y = Percent, fill = Gender)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  annotate("text", x = 0.5, y = 0, 
           label = paste0(gender_data$Percent[1], "%"),
           size = 8, fontface = "bold", 
           color = gender_colors[gender_data$Gender[1]]) +
  scale_fill_manual(values = gender_colors) +
  xlim(0.5, 2.5) +
  labs(title = "Gender Distribution", fill = "Gender") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    legend.title = element_text(face = "bold"),
    legend.position = "right"
  )

# ---- Age Donut Chart ----
age_plot <- ggplot(age_data, aes(x = 2, y = Percent, fill = AgeGroup)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  annotate("text", x = 0.5, y = 0, 
           label = paste0(age_data$Percent[1], "%"),
           size = 8, fontface = "bold", 
           color = age_colors[age_data$AgeGroup[1]]) +
  scale_fill_manual(values = age_colors) +
  xlim(0.5, 2.5) +
  labs(title = "Age Distribution", fill = "Age Group") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    legend.title = element_text(face = "bold"),
    legend.position = "right"
  )

# ---- Combine Charts Side-by-Side ----
gender_plot + age_plot




library(tidyverse)

# Combine 2023 and 2024 data
platform_reach <- tibble(
  Platform = c("Twitter", "Facebook", "LinkedIn", "Instagram"),
  `2023` = c(139279, 16249, 44530, 89720),
  `2024` = c(0, 4500000, 307029, 42691)
) %>%
  mutate(
    Total = `2023` + `2024`,
    Label = scales::comma(Total),
    Platform = fct_reorder(Platform, Total)
  )

# Plot in descending order
ggplot(platform_reach, aes(x = Platform, y = Total)) +
  geom_col(fill = "#2ca02c", width = 0.7) +
  geom_text(aes(label = Label), hjust = -0.1, size = 5, fontface = "bold") +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.15))) +
  coord_flip() +
  labs(
    title = "Combined Social Media Platform Reach (2023 & 2024)",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_text(face = "plain"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18)
  )




library(tidyverse)

# Combine 2023 and 2024 data
platform_reach <- tibble(
  Platform = c("Twitter", "Facebook", "LinkedIn", "Instagram"),
  `2023` = c(139279, 16249, 44530, 89720),
  `2024` = c(0, 4500000, 307029, 42691)
) %>%
  mutate(
    Total = `2023` + `2024`,
    Label = scales::comma(Total),
    Platform = fct_reorder(Platform, Total)
  )

# Plot in descending order with improved y-axis labels
ggplot(platform_reach, aes(x = Platform, y = Total)) +
  geom_col(fill = "#2ca02c", width = 0.7) +
  geom_text(aes(label = Label), hjust = -0.1, size = 5, fontface = "bold") +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.15))) +
  coord_flip() +
  labs(
    title = "Social Media Platform Reach",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_text(face = "bold", size = 14),  # <<-- Improved labels
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18)
  )



# Load required libraries
library(tidyverse)
library(scales)

# Data
funding_summary <- tibble(
  Category = c("Total Funding / Income", "Grants to Startups (Non-Dilutive)"),
  Amount = c(4456803.90, 113000)
)

# Plot
ggplot(funding_summary, aes(x = reorder(Category, Amount), y = Amount, fill = Category)) +
  geom_col(show.legend = FALSE, width = 0.6) +
  geom_text(aes(label = dollar(Amount)), hjust = -0.1, size = 5, fontface = "bold") +
  coord_flip() +
  scale_y_continuous(labels = dollar, expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Total Funding vs. Grants Disbursed to Startups",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    axis.text.y = element_text(face = "plain"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  scale_fill_manual(values = c("#1f77b4", "#1f77b4"))






library(tidyverse)

# Data
collab_data <- tibble(
  Type = c("Collaboration Events", "Strategic Partnerships"),
  Count = c(12, 15)
)

# Bar Chart
ggplot(collab_data, aes(x = reorder(Type, Count), y = Count, fill = Type)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = Count), vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_manual(values = c("#1f77b4", "#2ca02c")) +
  labs(
    title = "Strategic Collaborations and Networks",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.text.x = element_text(face = "plain"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank()
  )


library(tidyverse)

# Data
collab_data <- tibble(
  Category = c("Collaboration Events", "Strategic Partnerships"),
  Count = c(12, 15)
) %>%
  mutate(
    Percent = round(Count / sum(Count) * 100),
    Label = paste0(Category, "\n", Percent, "%"),
    ypos = cumsum(Percent) - 0.5 * Percent
  )

# Pie Chart
ggplot(collab_data, aes(x = "", y = Percent, fill = Category)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_label(aes(y = ypos, label = Label),
             color = "black", fill = "white", fontface = "bold", label.size = 0, size = 5) +
  scale_fill_manual(values = c("#1f77b4", "#2ca02c")) +
  labs(title = "Strategic Collaborations and Networks") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    legend.position = "none"
  )




# Load libraries
library(tidyverse)

# Sample data
collab_data <- tibble(
  Category = c("Collaboration Events", "Strategic Partnerships"),
  Count = c(12, 15)
)

# Bar chart with flipped axes and same fill color
ggplot(collab_data, aes(x = Count, y = fct_reorder(Category, Count))) +
  geom_col(fill = "#2ca02c", width = 0.6) +
  geom_text(aes(label = Count), hjust = -0.2, size = 5, fontface = "bold") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Strategic Collaborations and Networks",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )
