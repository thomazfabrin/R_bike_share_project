library(tidyr)

route_dataset <- complete_dataset_cleaned_without_docked %>% 
  filter(start_station_id != is.na(start_station_id),
         end_station_id != is.na(end_station_id)) %>% 
  unite(route, start_station_id, end_station_id)

route_dataset_grouped <- route_dataset %>% 
  group_by(member_casual, route) %>% 
  summarise(#mean_time_min = (mean(ride_length_hour))*60,
            #median_time_min = (median(ride_length_hour))*60,
            #sd_time_min = (sd(ride_length_hour))*60,
            rides = n_distinct(ride_id)) %>% 
  arrange(desc(rides))

most_frequent_route_casual <- route_dataset_grouped %>% 
  filter(member_casual == "casual") %>% 
  arrange(desc(rides))

head(most_frequent_route_casual)

most_frequent_route_membership <- route_dataset_grouped %>% 
  filter(member_casual == "member") %>% 
  arrange(desc(rides))

head(most_frequent_route_membership)

route_dataset_month <- route_dataset %>%
  filter(route %in% c("13022_13022", "KA1503000014_KA1503000071")) %>%
  group_by(member_casual, route, month) %>% 
  summarise(mean_time_min = (mean(ride_length_hour))*60,
            median_time_min = (median(ride_length_hour))*60,
            sd_time_min = (sd(ride_length_hour))*60,
            rides = n_distinct(ride_id))

head(route_dataset_month)

route_stream_rides_monthly_plot <-
  ggplot(
    mapping = aes(x = factor(month, levels = c("07", "08", "09", "10", "11",
                                               "12", "01", "02", "03", "04",
                                               "05", "06")), 
                  y = rides,
                  fill = member_casual,
                  color = member_casual),
    data = route_dataset_month) +
  facet_wrap(
    facets = ~ route, nrow = 2,
    labeller = labeller(route = c("13022_13022" = "Casual users most used route",
                        "KA1503000014_KA1503000071" = "Membership users most used route"))
  ) +
  geom_col(
    position = "stack"
  ) +
  #stat_smooth(
  #  mapping = aes(fill = member_casual),
  #  se = FALSE, geom = "area",
  #  method = 'loess', alpha = 1,
  #  span = 0.7, position = "stack"
  #) +
  scale_y_continuous(
    expand = c(0.0015, 0),
    limits = c(0, 2200),
    #n.breaks = 3,
    breaks = c(1000, 2000),
    labels = c("1000", "2000")
  ) +
  scale_x_discrete(
    labels = month_labs
  ) +
  scale_fill_manual(
    values = c("#EEA47F", "#37A1FF"),
    labels = c("Casual", "Membership")
    #guide = "none"
  ) +
  scale_color_manual(
    values = c("#E5743B", "#00539C"),
    labels = c("Casual", "Membership"),
    guide = "none"
  ) +
  labs(
    y = "Rides", x = "Month", fill = "Type of member",
    title = "Most used routes",
    subtitle = "From Jul-22 to Jun-23"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 17, color = "darkgray", face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, color = "darkgray", face = "italic", hjust = 0.5),
    axis.text.x = element_text(angle = 45, size = 10, hjust = 1),
    axis.title.x = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size = 10, color = "darkgray", face = "bold"),
    axis.title = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 12),
    axis.line = element_line(linewidth = 1.1, colour = "darkgray"),
    axis.ticks = element_line(linewidth = 1.2, colour = "darkgray"),
    #legend.position = "none"
    #axis.ticks.x = element_blank()
    #axis.line.x.bottom = element_blank()
  )

route_stream_rides_monthly_plot

route_stream_rides_monthly_plot_label <-
  ggdraw(route_stream_rides_monthly_plot) +
  draw_label(bquote(bold("Start station:")~"Streeter Dr & Grand Ave"), x = 0.5, y = 0.80, 
             hjust = 0.5, size = 11) +
  draw_label(bquote(bold("End station:")~"Streeter Dr & Grand Ave"), x = 0.5, y = 0.783, 
             hjust = 0.5, size = 11) +
  draw_label(bquote(bold("Start station:")~"Ellis Ave & 60th St"), x = 0.5, y = 0.35, 
             hjust = 0.5, size = 11) +
  draw_label(bquote(bold("End station:")~"University Ave & 57th St"), x = 0.5, y = 0.333, 
             hjust = 0.5, size = 11)

route_stream_rides_monthly_plot_label


ggsave("fig3_routes.svg",
       path = "plot",
       plot = route_stream_rides_monthly_plot_label,
       dpi = 300,
       units = "in",
       width = 10.8,
       height = 8.5,
       device = "svg") 
ggsave("fig3_routes.png",
       path = "plot",
       plot = route_stream_rides_monthly_plot_label,
       dpi = 300,
       units = "in",
       width = 10.8,
       height = 8.5,
       device = "png")




route_casual <- route_dataset %>% 
  filter(route == "13022_13022")



route_member <- route_dataset %>% 
  filter(route == "KA1503000014_KA1503000071")




