library(tidyverse)
library(dplyr)
library(ggplot2)
library(stringr)
library("usmap")

# The functions might be useful for A4
source("C:/Users/chenc/Documents/info201/assignments/a4-cmchen22/source/a4-helpers.R")
prison_data <- read.csv("C:/Users/chenc/Documents/info201/data/incarceration-trends/incarceration_trends.csv")
View(prison_data)

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 



# Mutates black percentage
prison_data <- prison_data %>%
  mutate(black_prison_percentage = (black_jail_pop/black_pop_15to64) *100)


# State that has the highest black jail population in 2018:

highest_state_pop <- prison_data %>%
  filter(year == 2018)%>%
  filter(black_jail_pop == max(black_jail_pop, na.rm = TRUE))%>%
  pull(state, black_jail_pop)

# Year that the most black Americans were jailed
highest_black_year <- prison_data %>%
  filter(black_jail_pop == max(black_jail_pop, na.rm = TRUE))%>%
  pull(year)

# County that has the highest black jail population in 2018:
highest_county_pop <- prison_data %>%
  filter(year == 2018)%>%
  filter(black_jail_pop == max(black_jail_pop, na.rm = TRUE))%>%
  pull(county_name, black_jail_pop)

#State with highest black jail percentage in 2018
black_state_rate <- prison_data %>%
  filter(year == 2018)%>%
  filter(black_prison_percentage == max(black_prison_percentage, na.rm = TRUE))%>%
  pull(state, black_prison_percentage)







## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population

#Data wrangling
get_year_jail_pop <- function() {

  total_pop_year <- data.frame(Year = prison_data$year, Jail_Pop = prison_data$total_jail_pop)
  return(total_pop_year)   
}

View(get_year_jail_pop())

#Bar chart
plot_jail_pop_for_us <- function()  {

  growth_chart <- ggplot(get_year_jail_pop()) +
    geom_col(mapping = aes(x = Year, y = Jail_Pop), fill = "#FF6666") +
    labs(x = "Year", y = "Total Jail Population in America", caption = "This is a visual representation of the American total Jail population between 1970 and 2018") +
    ggtitle("American Jail Population") +
    scale_y_continuous(
      labels = c("0", "200,000", "400,000", "600,000", "800,000") 
    )
  return(growth_chart)   
} 

plot_jail_pop_for_us()
## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
---------------------------------------------------#
##This function gets the growth of jail population for each state
get_jail_pop_by_states <- function(states){
  state_growth <- prison_data%>%
    filter(state %in% states, na.rm = TRUE) %>%
    group_by(state, year)%>%
    summarise(pop_by_year = sum(total_jail_pop, na.rm = TRUE))
  return (state_growth)
}
View(get_jail_pop_by_states("WA"))

##This function creates a line chart that demonstrates the amount of growth for each state
plot_jail_pop_by_states <- function(states){
  state_growth_chart <- ggplot(get_jail_pop_by_states(states)) +
    geom_line(mapping = aes(x = year, y = pop_by_year, color = state)) +
    labs(x = "Year", y = "Total Jail Population", caption = "This is a visual representation of the growth of jail population for a state ") + 
    ggtitle("Jail Population growth by State (1970 - 2018)")
  return(state_growth_chart)
  
}
plot_jail_pop_by_states(c("WA", "CA"))

## Section 5  ---- 
#----------------------------------------------------------------------------#
#Scatterplot that compares the black vs white jail percentage in America

race <- function(){
  rate <- data.frame(Black_Jail_Percentage = ((prison_data$black_jail_pop/prison_data$black_pop_15to64) * 100),
                     White_Jail_Percentage = ((prison_data$white_jail_pop/prison_data$white_pop_15to64)*100),
                     Year = prison_data$year)
  filter_rate <- rate%>%
    #filtering out outliners
    filter(Black_Jail_Percentage <= 10, White_Jail_Percentage <= 5, Year == 2018)
  return(filter_rate)
}
View(race())

race_plot <- function(){
  race_chart <- ggplot(race()) +
    geom_point(mapping = aes(x = Black_Jail_Percentage, y = White_Jail_Percentage))+
    ggtitle("White vs Black jail population percentage in the USA (2018)")+
    labs(x = "Black Jail Population percentage", y = "White Jail Population percentage", caption = "This chart shows differences between White and Black jail population percentage.")
  
  return(race_chart)
  
  
}
race_plot()

## Section 6  ---- 
#----------------------------------------------------------------------------#
# Map of black prison percentage for each state of America and Map of white prison percentage for each state of America

get_pop_black_per_state <- function() {
  dataframe <- prison_data %>%
    group_by(state)%>%
    filter(year == 2018) %>%
    summarize(black_jail = sum(black_jail_pop, na.rm = TRUE) , total_black = sum(black_pop_15to64,  na.rm= TRUE)) %>% 
    mutate(percentage = (black_jail / total_black) * 100, na.rm = TRUE )%>%
    return(dataframe)
}

frame_for_map <- get_pop_black_per_state()
frame_for_map$state <- tolower(state.name[match(frame_for_map$state, state.abb)])

state_shape <- map_data("state") %>%
  rename(state = region) %>%
  left_join(frame_for_map, by = "state")

map_black_jail_prop <- function() {
  plot <- ggplot(state_shape) +
    scale_fill_continuous(low = "White", high = "Purple", limits = c(0, 2.5)) +
    geom_polygon( 
      mapping = aes(x = long, y = lat, group = group, fill = percentage),
      color = "white",
      size = 0.1
    ) +
    coord_map() +
    labs( title = "Percentage of Black Americans in prison for each state (2018)",
          caption = "The percentage of Black Americans in prison for each state") +
    theme_bw() +
    theme( 
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      plot.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      plot.margin = unit(c(-0.30, 0, 0, 0), "null") 
    )
  return (plot)
}

map_black_jail_prop()

get_pop_white_per_state <- function() {
  dataframe2 <- prison_data %>%
    group_by(state)%>%
    filter(year == 2018) %>%
    summarize(white_jail = sum(white_jail_pop, na.rm = TRUE) , total_white = sum(white_pop_15to64,  na.rm= TRUE)) %>% 
    mutate(percentage = (white_jail / total_white) * 100, na.rm = TRUE )%>%
    return(dataframe2)
}
frame_for_map_white <- get_pop_white_per_state()
view(frame_for_map_white)

frame_for_map_white$state <- tolower(state.name[match(frame_for_map_white$state, state.abb)])
view(frame_for_map_white)
state_shape_2 <- map_data("state") %>%
  rename(state = region) %>%
  left_join(frame_for_map_white, by = "state")

map_white_jail_prop <- function() {
  plot <- ggplot(state_shape_2) +
    scale_fill_continuous(low = "White", high = "Purple", limits = c(0, 2.5)) +
    geom_polygon( 
      mapping = aes(x = long, y = lat, group = group, fill = percentage),
      color = "white",
      size = 0.1
    ) +
    coord_map() +
    labs( title = "Percentage of White Americans in prison for each state (2018)",
          caption = "The percentage of White Americans in prison for each state") +
    theme_bw() +
    theme( 
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      plot.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      plot.margin = unit(c(-0.30, 0, 0, 0), "null") 
    )
  return (plot)
}



map_white_jail_prop()
