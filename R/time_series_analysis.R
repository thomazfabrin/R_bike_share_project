library(mgcv)
library(ggalt)
library(ggstream)
library(ggpubr)

monthly_analysis <- complete_dataset_cleaned %>% 
  filter(rideable_type != "docked_bike") %>% 
  mutate(ride_length_min = ride_length_hour*60) %>% 
  group_by(member_casual, month) %>% 
  summarise(mean_min = as.numeric(mean(ride_length_min)), 
            sd_min = as.numeric(sd(ride_length_min)), 
            median_min = as.numeric(median(ride_length_min)), 
            rides = n_distinct(ride_id)) %>%
  ungroup() %>% 
  mutate(ride_norm_120 = (rides/max(rides)) * 120) %>% 
  filter(member_casual != "NA") %>% 
  arrange(member_casual, month)

chicago_temp <- c(-6, -4, 3, 10, 15, 21 , 23, 22, 18, 12, 5, -3,
                  -6, -4, 3, 10, 15, 21 , 23, 22, 18, 12, 5, -3)

monthly_analysis <- bind_cols(monthly_analysis, temp = chicago_temp)

cf_y_right = max(monthly_analysis$rides)/120

complete_dataset_cleaned_without_docked <- complete_dataset_cleaned %>% 
  filter(rideable_type != "docked_bike") 

member_labs <- c("Casual", "Membership")
names(member_labs) <- c("casual", "member")

bike_labs <- c("Classic bike", "Electric bike")
names(bike_labs) <- c("classic_bike", "electric_bike")

month_labs <- c("Jul-22", "Aug-22", "Sep-22", "Oct-22", "Nov-22", 
                "Dec-22", "Jan-23" , "Feb-23", "Mar-23", "Apr-23", 
                "May-23", "Jun-23")
names(month_labs) <- c("07", "08", "09", "10", "11", "12", "01", "02", "03", "04",
                       "05", "06")

head(monthly_analysis)

distribution_monthly_time_violin_plot <- 
  ggplot(
    data = complete_dataset_cleaned_without_docked,
    mapping = aes(x = factor(month, levels = c("07", "08", "09", "10", "11",
                                               "12", "01", "02", "03", "04",
                                               "05", "06")), 
                  y = as.numeric(ride_length_hour * 60), 
                  group = month, color = member_casual, fill = member_casual)
    ) +
  geom_violin(
    width = 1
    ) +
  facet_wrap(
    facets = ~ member_casual, 
    ncol = 1,
    labeller = labeller(member_casual = member_labs,
                        rideable_type = bike_labs)
    ) +
  scale_y_continuous(
    expand = c(0.0015, 0), 
    breaks = c(60, 120),
    limits = c(0, 125),
    labels = function(x) format(x, scientific = FALSE),
    ) +
  scale_x_discrete(
    labels = month_labs
    ) +
  scale_fill_manual(
    values = c("#EEA47F", "#37A1FF"),
    labels = c("Casual", "Membership")
    ) +
  scale_color_manual(
    values = c("#E5743B", "#00539C"),
    labels = c("Casual", "Membership"),
    guide = FALSE
    ) +
  labs(
    y = "Ride duration (min)",
    x = "Month",
    fill = "Type of member"
    ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, size = 10, hjust = 1),
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    axis.title = element_text(size = 11, face = "bold"),
    axis.title.x = element_blank(),
    axis.text = element_text(size = 12),
    axis.line = element_line(linewidth = 1.1, colour = "darkgray"),
    axis.ticks = element_line(linewidth = 1.2, colour = "darkgray"),
    legend.position = "none",
    axis.ticks.x = element_blank()
    )

distribution_monthly_time_violin_plot


mean_median_monthly_plot <- 
  ggplot(
    mapping = aes(x = factor(month, levels = c("07", "08", "09", "10", "11",
                                               "12", "01", "02", "03", "04",
                                               "05", "06")), 
                  y = median_min, 
                  group = member_casual,
                  shape = member_casual,
                  fill = member_casual,
                  color = member_casual),
    data = monthly_analysis) +
  geom_rect(
    mapping = aes(xmin = 11, xmax = 12, ymin = 0, ymax = Inf),
    fill = "pink",
    color = NA,
    alpha = 0.05
    ) +
  geom_rect(
    mapping = aes(xmin = 1, xmax = 3, ymin = 0, ymax = Inf),
    fill = "pink",
    color = NA,
    alpha = 0.05
    ) +
  geom_rect(
    mapping = aes(xmin = 6, xmax = 9, ymin = 0, ymax = Inf),
    fill = "lightblue",
    color = NA,
    alpha = 0.05
    ) +
  geom_point(
    size = 5
    ) +
  geom_line(
    linewidth = 0.5
    ) +
  scale_y_continuous(
    expand = c(0.0015, 0), 
    breaks = c(5, 10, 15),
    limits = c(0, 16),
    labels = function(x) format(x, scientific = FALSE)
    ) +
  scale_x_discrete(
    labels = month_labs
    ) +
  scale_shape_manual(
    values = c(21, 24),
    labels = c("Casual", "Membership")
    ) +
  scale_fill_manual(
    values = c("#EEA47F", "#37A1FF"),
    labels = c("Casual", "Membership"),
    guide = "none"
    ) +
  scale_color_manual(
    values = c("#E5743B", "#00539C"),
    labels = c("Casual", "Membership"),
    guide = "none"
    ) +
  guides(
    shape = guide_legend(override.aes = list(fill = c("#EEA47F", "#37A1FF")))
    ) +
  labs(
    y = "Ride duration - median (min)",
    x = "Month",
    shape = "Type of member"
    ) +
  theme_classic() +
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    strip.background = element_blank(),
    axis.title = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 12),
    axis.line = element_line(linewidth = 1.1, colour = "darkgray"),
    axis.ticks = element_line(linewidth = 1.2, colour = "darkgray"),
    #legend.position = "none",
    axis.ticks.x = element_blank(),
    #axis.line.x.bottom = element_blank()
    ) +
  geom_hline(
    yintercept = mean(as.numeric(complete_dataset_cleaned_without_docked$
    ride_length_hour)) * 60,
    linetype = "dotted", linewidth = 1, color = "red"
    ) +
  geom_hline(
    yintercept = median(as.numeric(complete_dataset_cleaned_without_docked$
    ride_length_hour)) * 60,
    linetype = "solid", linewidth = 1, color = "red"
    ) +
  geom_text(
    mapping = aes(x = 7.5, y = 2),
    label = "COLD",
    color = "darkgray", size = 4, hjust = 0.5
    ) +
  geom_text(
    mapping = aes(x = 11.5, y = 2),
    label = "WARM",
    color = "darkgray", size = 4, hjust = 0.5
    ) +
  geom_text(
    mapping = aes(x = 2, y = 2),
    label = "WARM",
    color = "darkgray", size = 4, hjust = 0.5
    ) + 
  geom_curve(
    mapping = aes(x = 1.6, xend = 0.75, y = 7, yend = 9.2),
    arrow = arrow(length = unit(0.04, "npc")),
    curvature = -0.3, size = 0.7, color = "darkgray"
    ) +
  geom_text(
    mapping = aes(x = 1.7, y = 7),
    label = "Median of complete data",
    color = "darkgray", size = 3.5, hjust = 0
    ) + 
  geom_curve(
    mapping = aes(x = 4.6, xend = 3.75, y = 15.5, yend = 13.8),
    arrow = arrow(length = unit(0.04, "npc")),
    curvature = 0.3, size = 0.7, color = "darkgray"
    ) +
  geom_text(
    mapping = aes(x = 4.7, y = 15.55),
    label = "Mean of complete data",
    color = "darkgray", size = 3.5, hjust = 0
    )


mean_median_monthly_plot

stream_rides_monthly_plot <-
  ggplot(
    mapping = aes(x = factor(month, levels = c("07", "08", "09", "10", "11",
                                               "12", "01", "02", "03", "04",
                                               "05", "06")), 
                  y = rides, 
                  group = member_casual,
                  shape = member_casual,
                  fill = member_casual,
                  color = member_casual),
    data = monthly_analysis) +
  stat_smooth(
    mapping = aes(fill = member_casual),
    se = FALSE, geom = "area",
    method = 'loess', alpha = 1,
    span = 0.7, position = "stack"
    ) +
  scale_y_continuous(
    expand = c(0.0015, 0), 
    #n.breaks = 3,
    breaks = c(250000, 500000, 750000),
    labels = c("250", "500", "750")
    ) +
  scale_x_discrete(
    labels = month_labs
    ) +
  scale_fill_manual(
    values = c("#EEA47F", "#37A1FF"),
    labels = c("Casual", "Membership"),
    guide = "none"
    ) +
  scale_color_manual(
    values = c("#E5743B", "#00539C"),
    labels = c("Casual", "Membership"),
    guide = "none"
    ) +
  labs(
    y = "Rides (thousand)", x = "Month", shape = "Type of member"
    ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, size = 10, hjust = 1),
    axis.title.x = element_blank(),
    strip.background = element_blank(),
    axis.title = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 12),
    axis.line = element_line(linewidth = 1.1, colour = "darkgray"),
    axis.ticks = element_line(linewidth = 1.2, colour = "darkgray"),
    #legend.position = "none",
    #axis.ticks.x = element_blank()
    #axis.line.x.bottom = element_blank()
    )

stream_rides_monthly_plot

temperature_regression_inset_plot <-
  ggplot(
    aes(x = rides, y = temp, group = member_casual,
        fill = member_casual, color = member_casual),
    data = monthly_analysis
    ) +
  geom_point(
    shape = 21,
    size = 4,
    alpha = 0.5
    ) +
  geom_smooth(
    method = "lm",
    se = FALSE
    ) +
  scale_y_continuous(
    expand = c(0.0015, 0), 
    #breaks = c(5, 10, 15),
    #limits = c(0, 16),
    labels = function(x) format(x, scientific = FALSE)
    ) +
  scale_x_continuous(
    expand = c(0.0015, 0), 
    breaks = c(200000, 400000),
    #limits = c(0, 16),
    labels = c("200 ", "400 ")
    ) +
  scale_fill_manual(
    values = c("#EEA47F", "#37A1FF"),
    labels = c("Casual", "Membership"),
    guide = "none"
    ) +
  scale_color_manual(
    values = c("#E5743B", "#00539C"),
    labels = c("Casual", "Membership"),
    guide = "none"
    ) +
  labs(
    y = "Temp. (ºC)", x = "Rides (thousand)", fill = "Type of member"
    ) +
  theme_classic() +
  theme(
    panel.grid.major = element_line(linetype = "dashed", color = "lightgray"),
    #axis.text.x = element_blank(),
    #axis.title.x = element_blank(),
    axis.title = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 12),
    axis.line = element_line(linewidth = 1.1, colour = "darkgray"),
    axis.ticks = element_line(linewidth = 1.2, colour = "darkgray"),
    legend.position = "none"
    #axis.line.x.bottom = element_blank()
  )
  
  temperature_regression_inset_plot

time_series_analysis_plot <- 
  mean_median_monthly_plot +
  distribution_monthly_time_violin_plot +
  stream_rides_monthly_plot +
  temperature_regression_inset_plot +
  plot_layout(
  design = "aaaa
            bbbb
            cccd"
  ) +
  plot_annotation(
    title = "Time series analysis of the Cyclistic data",
    subtitle = "understanding members usage patterns from Jul-22 to Jun-23",
    tag_levels = "A",
    theme = theme(plot.title = element_text(size = 15, 
                                            face = "bold",
                                            hjust = 0.5,
                                            color = "darkgray"),
                  plot.subtitle = element_text(size = 10, face = "italic",
                  hjust = 0.5, color = "darkgray"),
                  plot.tag = element_text(size = 10, face = "bold"))
    ) 

time_series_analysis_plot

ggsave("fig2_time_description.svg",
       path = "plot",
       plot = time_series_analysis_plot,
       dpi = 300,
       units = "in",
       width = 10.8,
       height = 8.5,
       device = "svg") 
ggsave("fig2_time_description.png",
       path = "plot",
       plot = time_series_analysis_plot,
       dpi = 300,
       units = "in",
       width = 10.8,
       height = 8.5,
       device = "png")
