library(dplyr)
library(ggplot2)
library(cowplot)

rent <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-05/rent.csv')
permits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-05/sf_permits.csv')
new_construction <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-05/new_construction.csv')

#Base data, removes negative number and replaces it with 0
base_data <- new_construction |> 
  select(-c(1:3,10) ) |> 
  mutate(totalproduction = ifelse(totalproduction < 0, 0, totalproduction))

#colors assigned to each county
colrs <- scales::hue_pal(h = c(0, 360) + 15, c =90, l = 60, h.start = 30, direction = 2)(10)
#grey color
colrs[1] <- "#dcdcdc"

all_plot <- base_data |> 
  ggplot(aes(x = year, y= totalproduction, fill = county)) +
  geom_col(position = "fill")  +
  scale_x_continuous(limits = c(1989,2019),
                     n.breaks = 7) +
  ggthemes::theme_pander() +
  theme(axis.text.y = element_blank(),
        legend.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5)) +
  labs(x = NULL,
       y = NULL,
       fill = NULL)+
  scale_fill_discrete(
    type = colrs[2:10]
  ) 

plot_counties <- function(county, color){
  
 
  
    p <- base_data |> 
    group_by(year, grp = county %in% {{county}}) |> 
    summarise(s = sum(totalproduction), .groups = "drop_last") |> 
    mutate(m = s/sum(s), b = sum(s)) |> 
    ggplot(aes(x = year, y = s, fill = factor(grp))) +
    geom_col(position = "fill") +
    scale_fill_discrete(
      type = {{color}}
    ) +
    labs(x = NULL,
         y = NULL,
         fill = "Counties",
         title = {{county}}) +
    theme(
      axis.text.y = element_blank(),
      axis.text.x = element_blank(),
      legend.text = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "none",
      plot.title = element_text(hjust = 0.5))
  
  return(p)
}


plots <- Map(\(x,y) plot_counties(x,colrs[c(1,y)]), new_construction$county |> unique(), 2:10)

ggdraw() +
  draw_plot(all_plot, x = 0.2, y = 0.2, width = 0.6, height = 0.6 ) +
  Map(\(x,y,z) draw_plot(plot = x, x = y, y = z, width = 0.2, height = 0.2),
       plots, c(c(0.2,0.4,0.6),rep(0,3), rep(0.8, 3)), 
       c(rep(0,3),rep(c(0.2,0.4,0.6), 2))) +
  draw_label(label = "Yearly New Construction In Sanfranciso Counties (1990-2018)",
             x = 0.5, y = 0.9)





