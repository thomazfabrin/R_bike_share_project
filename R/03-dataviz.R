# Load necessary libraries
library(ggplot2)
library(patchwork)
library(ggtext)

# Find the range of dates in the data ----
date_range <- range(month_descriptive$year_month_started)
hour_range <- range(hour_descriptive$hour_started)

# Heatmap total rides monthly and daily ----
p_heatmap_total_rides_med_time <- ggplot(
    month_descriptive,
    aes(x = day_started,
        y = year_month_started,
        fill = total_rides,
        group = year_month_started)) +
    geom_tile() +
    geom_point(aes(size = med_ride_time, group = year_month_started)) +
    scale_fill_gradient(
        low = "white",
        high = "darkred") +
    scale_x_continuous(
        breaks = c(1, 10, 20, 30),
        limits = c(1, 31)) +
    scale_y_date(
        breaks = seq(as.Date(date_range[1]), as.Date(date_range[2]),
                     by = "month"),
        labels = format(seq(min(month_descriptive$year_month_started),
                            max(month_descriptive$year_month_started),
                            by = "month"), "%Y-%b")) +
    labs(
        title = "Total rides",
        subtitle = "Daily overview",
        x = "Day of the month",
        y = "Month",
        size = "Median ride time (min)",
        fill = "Total rides") +
    facet_grid(
        member_casual ~ rideable_type,
        labeller = labeller(
            member_casual = c("casual" = "Casual",
                              "member" = "Member"),
            rideable_type = c("electric_bike" = "Electric bike",
                              "classic_bike" = "Classic bike"))) +
    theme_minimal() +
    theme(
        plot.background = element_rect(fill = "white", color = "white"),
        #plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.title = element_blank(),
        plot.subtitle = element_text(size = 15, hjust = 0.5, face = "italic"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 15, hjust = 0.5, color = "grey50",
                                    face = "bold"),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 15, face = "bold", color = "grey50"))

ggsave(
    filename = "quarto/figures/heatmap_total_rides_med_time.png",
    plot = p_heatmap_total_rides_med_time,
    width = 20,
    height = 10,
    units = "in",
    dpi = 300)

# Heatmap total rides weekday ----
p_heatmap_t_rides_med_wday <- ggplot(
    weekday_descriptive,
    aes(x = wday_started,
        y = year_month_started,
        fill = total_rides,
        group = year_month_started)) +
    geom_tile() +
    geom_point(aes(size = med_ride_time, group = year_month_started)) +
    scale_fill_gradient(
        low = "white",
        high = "darkred") +
        scale_y_date(
        breaks = seq(as.Date(date_range[1]), as.Date(date_range[2]), by = "month"),
        labels = format(seq(min(month_descriptive$year_month_started), max(month_descriptive$year_month_started), by = "month"), "%Y-%b")) +
    labs(
        title = "Total rides",
        subtitle = "Weekday overview",
        x = "Day of the week",
        y = "Month",
        size = "Median ride time (min)",
        fill = "Total rides") +
    facet_grid(
        member_casual ~ rideable_type,
        labeller = labeller(
            member_casual = c("casual" = "Casual",
                              "member" = "Member"),
            rideable_type = c("electric_bike" = "Electric bike",
                              "classic_bike" = "Classic bike"))) +
    theme_minimal() +
    theme(
        plot.background = element_rect(fill = "white", color = "white"),
        #plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.title = element_blank(),
        plot.subtitle = element_text(size = 15, hjust = 0.5, face = "italic"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 15, hjust = 0.5, color = "grey50", face = "bold"),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 15, face = "bold", color = "grey50"))

ggsave(
    filename = "quarto/figures/heatmap_t_rides_med_time_wday.png",
    plot = p_heatmap_t_rides_med_wday,
    width = 10,
    height = 10,
    units = "in",
    dpi = 300)

# Heatmap total rides hourly ----
p_heatmap_t_rides_med_hourly <- ggplot(
    hour_descriptive,
    aes(x = wday_started,
        y = hour_started,
        fill = total_rides,
        group = hour_started)) +
    geom_tile() +
    geom_point(aes(size = med_ride_time, group = hour_started)) +
    scale_fill_gradient(
        low = "white",
        high = "darkred") +
        scale_y_continuous(
        breaks = seq(hour_range[1], hour_range[2], by = 6)) +
    labs(
        title = "Total rides",
        subtitle = "Hourly overview",
        x = "Day of the week",
        y = "Hour of the day",
        size = "Median ride time (min)",
        fill = "Total rides") +
    facet_grid(
        member_casual ~ rideable_type,
        labeller = labeller(
            member_casual = c("casual" = "Casual",
                              "member" = "Member"),
            rideable_type = c("electric_bike" = "Electric bike",
                              "classic_bike" = "Classic bike"))) +
    theme_minimal() +
    theme(
        plot.background = element_rect(fill = "white", color = "white"),
        #plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.title = element_blank(),
        plot.subtitle = element_text(size = 15, hjust = 0.5, face = "italic"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 15, hjust = 0.5, color = "grey50", face = "bold"),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 15, face = "bold", color = "grey50"))

ggsave(
    filename = "quarto/figures/heatmap_t_rides_med_time_hourly.png",
    plot = p_heatmap_t_rides_med_hourly,
    width = 12,
    height = 12,
    units = "in",
    dpi = 300)


# Heatmap total rides monthly and weekday
p1 <- p_heatmap_total_rides_med_time +
    p_heatmap_t_rides_med_wday +
    plot_layout(design = "aab") +
    plot_annotation(title = "Total rides and median ride time",
                    tag_levels = "a",
                    theme = theme(
                        plot.title = element_text(size = 20, face = "bold", hjust = 0.5)))

ggsave(
    filename = "figures/heatmap_concat.png",
    plot = p1,
    width = 30,
    height = 10,
    units = "in",
    dpi = 300)

# Line plot total rides monthly and daily
p_line_plot_total_rides <- ggplot(
    month_line_plot_total,
    aes(x = year_month_started,
        y = total_rides,
        color = member_casual,
        fill = member_casual,
        linetype = rideable_type)) +
    annotate(
        geom = "rect",
        xmin = as.Date("2022-11-01"),
        xmax = as.Date("2023-03-01"),
        ymin = -Inf,
        ymax = Inf,
        fill = "#6cc2df",
        alpha = 0.2,
        linetype = 1) +
    geom_line(
        linewidth = 1.5) +
    geom_point(
        size = 5,
        stroke = 1,
        shape = 21,) +
    facet_wrap(
        ~ rideable_type) +
    scale_x_date(
        breaks = seq(as.Date(date_range[1]), as.Date(date_range[2]), by = "3 months"),
        labels = format(seq(min(month_descriptive$year_month_started), max(month_descriptive$year_month_started), by = "3 months"), "%Y-%b")) +
    labs(
        title = "Total rides",
        subtitle = "Monthly overview",
        x = "Month",
        y = "Total rides",
        color = "User type",
        fill = "User type",
        linetype = "Bike type") +
    scale_color_manual(
        values = c("casual" = "#03165a",
                   "member" = "#aa5b33"),
        labels = c("Casual", "Member")) +
    scale_fill_manual(
        values = c("casual" = "#00539C",
                   "member" = "#EEA47F"),
        labels = c("Casual", "Member")) +
    scale_linetype_manual(
        values = c("classic_bike" = "solid",
                   "electric_bike" = "dotted"),
        labels = c("Classic", "Electric")) +
    annotate(
        geom = "text",
        x = as.Date("2023-01-01"),
        y = 200000,
        label = "paste(bold('Cold months'))",
        parse = TRUE,
        size = 4,
        color = "grey50") +
    theme_minimal() +
    theme(
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title = element_blank(),
        plot.subtitle = element_text(size = 15, hjust = 0.5, face = "italic"),
        axis.title.y = element_text(size = 15, hjust = 0.5, color = "grey50", face = "bold"),
        axis.title.x = element_text(size = 15, hjust = 0.5, color = "grey50", face = "bold"),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 15, face = "bold", color = "grey50"))

ggsave(
    filename = "quarto/figures/line_plot_total_rides.png",
    plot = p_line_plot_total_rides,
    width = 9,
    height = 5,
    units = "in",
    dpi = 300)

p_distribution_violin <- ggplot(
    dataset,
    aes(x = member_casual,
        y = ride_time,
        fill = member_casual,
        color = member_casual)) +
    geom_violin(
        trim = FALSE,
        alpha = 0.5,
        size = 0.5) +
    geom_boxplot(
        width = 0.1,
        alpha = 0.5,
        size = 0.5) +
    scale_y_continuous(
        breaks = seq(0, 100, by = 10),
        limits = c(0, 100)) +
    labs(
        title = "Ride time distribution",
        y = "Ride time (min)",
        fill = "User type",
        color = "User type") +
    scale_x_discrete(
        labels = c("casual" = "Casual",
                   "member" = "Member")) +
    scale_y_continuous(
        breaks = seq(0, 100, by = 20),
        limits = c(0, 100)) +
    scale_fill_manual(
        values = c("casual" = "#00539C",
                   "member" = "#EEA47F"),
        labels = c("Casual", "Member")) +
   scale_color_manual(
        values = c("casual" = "#03165a",
                   "member" = "#aa5b33"),
        labels = c("Casual", "Member")) +
    theme_minimal() +
    theme(
        plot.background = element_rect(fill = "white", color = "white"),
        plot.title = element_blank(),
        plot.subtitle = element_text(size = 15, hjust = 0.5, face = "italic"),
        axis.title.y = element_text(size = 15, hjust = 0.5, color = "grey50", face = "bold"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 15, face = "bold", color = "grey50"))

ggsave(
    filename = "quarto/figures/distribution_violin.png",
    plot = p_distribution_violin,
    width = 9,
    height = 5,
    units = "in",
    dpi = 300)
