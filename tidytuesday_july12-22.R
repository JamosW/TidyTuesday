library(dplyr)
library(ggplot2)

tuesdata <- tidytuesdayR::tt_load(2022, week = 28)

############################################Change in Frequency Of Flights#########################################################
 flight_data <- tuesdata$flights |> 
  as_tibble() |> 
  select(YEAR, FLT_DATE, STATE_NAME, FLT_TOT_1, MONTH_NUM) |> 
  group_by(YEAR) |> 
   filter(!STATE_NAME %in% c("Morocco")) |> 
    summarise(flights_per_month = sum(FLT_TOT_1)/length(unique(as.numeric(MONTH_NUM)))) |> 
  mutate(change = c("", paste0(round(diff(flight_data$flights_per_month/1000), 2), "k")))
 
#x axis labels   
xlabs <- seq(250000, 1750000, 250000)
#divider
div <- 100000

#prelim plot
  ggplot(data = flight_data) +
  geom_point(shape="\u2708", aes(y = YEAR, x = flights_per_month ), size = 8, color = "lightblue") +
    scale_x_continuous(limits = c(0, 1900000), breaks = c(0, xlabs), 
                       labels = c(0, ifelse(xlabs >= 1000000, paste0(xlabs/(div * 10),"M"), paste0(xlabs/(div/100), "K")))) +
    scale_y_continuous(n.breaks = 7) +
    geom_hline(yintercept = 2019.5, color = "red", linetype = "dashed", size = 2) +
    geom_text(aes(x = flights_per_month, y = YEAR, label = change), 
              color = ifelse(flight_data$change > 0, "#049660", "red"), size = 5, nudge_x = 220000) +
    geom_point(aes(x = flights_per_month + 430000, y = YEAR), size = 8,  
               shape = case_when(flight_data$change > 0 ~ "\u2191", 
                                 flight_data$change == "" ~ "", 
                                 flight_data$change < 1 ~ "\u2193"), color = "grey") +
    annotate("segment", x = 625000,  xend = 875000,  y = 2018.75, yend = 2019.45,
             arrow = arrow(), color = "orange", size = 1) +
    geom_text(aes(x = 625000, y = 2018.65, label = "COVID-19"), color = "red", size = 5) +
    theme(plot.margin = margin(2.5, 1.5, 1, 1.5, "cm"),
          panel.background = element_rect(fill = "white"),
          axis.ticks.y = element_blank(),
          axis.text.y = element_text(size = 15),
          axis.text.x = element_text(size = 10, face = "bold"),
          plot.title.position = "panel",
          plot.title = element_text(color = "#4096b1", size = 18, vjust = c(1))
          ) +
    labs(y = NULL, x = NULL,
         title = "European flights per month from 2016 to 2020")
    
  