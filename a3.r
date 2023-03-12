# Mia Truong 
# A3 Data Visualization
# analysis.r file

# Analysis -------------------------------------------------------------------------

library (dplyr)
incarceration_df <- read.csv(url("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv"))

# The state with highest total jail population in the most recent year and show
# its population in `highest_jail_state`
highest_jail_state <- incarceration_df %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  filter(total_jail_pop == max(total_jail_pop, na.rm = TRUE)) %>%
  select(state)

# The year with the highest jail population and show its highest population 
# in `highest_jail_year`
highest_jail_year <- incarceration_df %>%
  filter(total_jail_pop == max(total_jail_pop, na.rm = TRUE)) %>%
  select(year)

# The total white jail population in `white_jail_total`
white_jail_sum <- incarceration_df %>%
  group_by(year) %>%
  filter(white_jail_pop == white_jail_pop) %>%
  summarise(amount = sum(white_jail_pop))

white_jail_total <- sum(white_jail_sum[, "amount"])

# Ratio of white jail population compare to total jail population in 
# of all year in the dataset in `white_to_total_ratio`
jail_pop_sum <- incarceration_df %>%
  group_by(year) %>%
  filter(total_jail_pop == total_jail_pop) %>%
  summarise(amount = sum(total_jail_pop))
jail_pop_total <- sum(jail_pop_sum[, "amount"])
white_to_total_ratio <- white_jail_total/ jail_pop_total

# The total other races jail population in `other_jail_total` and 
# their ratio in `other_to_total_ratio`

other_jail_total <- jail_pop_total - white_jail_total 
other_to_total_ratio <- other_jail_total/ jail_pop_total

# Map -------------------------------------------------------------------------

library(usmap)
library(ggplot2)

white_to_total_pop <- incarceration_df %>%
  group_by(state) %>%
  filter(white_jail_pop == white_jail_pop) %>%
  filter(total_jail_pop == total_jail_pop) %>%
  mutate(total_jail_pop / white_jail_pop) %>%
  summarise(
    pop = sum(white_jail_pop), total = max(total_jail_pop),
    mutate = sum(total_jail_pop / white_jail_pop)
  )

map <- plot_usmap(
  data = white_to_total_pop, values = "pop", color = "black",
  name = "White Jail Population Distribution Across the U.S"
) +
  coord_fixed(1) +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  ) +
  scale_fill_gradientn(
    colours = c("white", "navy"),
    breaks = c(10, 100, 1000, 10000),
    trans = "log10", name = "White Jail Population"
  ) +
  labs(title = "The United States", subtitle = "White Jail Population in
       2018", name = "White Jail Population") 

# Scatterplot ------------------------------------------------------------
state_df <- incarceration_df %>%
  group_by(year) %>%
  filter(white_jail_pop == white_jail_pop) %>%
  summarise(jail = sum(white_jail_pop))

scatter_plot <- ggplot(state_df, aes(x = year, y = jail)) +
  geom_point(aes(color = jail)) +
  scale_color_continuous("Total White Jail Population") +
  labs(x = "Year", y = "Total White Jail Population") +
  ggtitle("Total White Jail Population In The U.S Over The Years")

# Line Chart: Trend Over Time-------------------------------------------------
options(scipen = 999)

jail_df <- incarceration_df %>%
  group_by(year) %>%
  filter(year == max(year)) %>%
  filter(total_jail_pop == total_jail_pop) %>%
  filter(latinx_jail_pop == latinx_jail_pop) %>%
  filter(white_jail_pop == white_jail_pop) %>%
  filter(aapi_jail_pop == aapi_jail_pop) %>%
  filter(black_jail_pop == black_jail_pop) %>%
  filter(native_jail_pop == native_jail_pop) %>%
  summarize(
    jail_total = sum(total_jail_pop), latinx_total = sum(latinx_jail_pop),
    white_total = sum(white_jail_pop), aapi_total = sum(aapi_jail_pop),
    black_total = sum(black_jail_pop), native_total = sum(native_jail_pop)
  )

line_chart <- ggplot(data = jail_df) +
  geom_line(mapping = aes(x = year, y = jail_total, color = "purple")) +
  geom_line(mapping = aes(x = year, y = latinx_total, color = "brown")) +
  geom_line(mapping = aes(x = year, y = aapi_total, color = "yellow")) +
  geom_line(mapping = aes(x = year, y = native_total, color = "pink")) +
  geom_line(mapping = aes(x = year, y = black_total, color = "red")) +
  geom_line(mapping = aes(x = year, y = white_total, color = "blue")) +
  scale_color_manual(
    name = "Races:",
    values = c("purple", "brown", "yellow", "pink", "red", "blue"),
    labels = c("Total", "Latinx", "AAPI", "Native", "Black", "White")
  ) +
  scale_fill_manual(
    name = "Races:",
    values = c("purple", "brown", "yellow", "pink", "red", "blue"),
    labels = c("Total", "Latinx", "AAPI", "Native", "Black", "White")
  ) +
  ggtitle("Yearly Total Jail Population of Races") +
  labs(x = "Year", y = "Jail Population")