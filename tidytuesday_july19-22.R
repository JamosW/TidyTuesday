library(dplyr)
library(scales)
library(ggplot2)
library(cowplot)


tuesdata <- tidytuesdayR::tt_load(2022, week = 29)
countries <- DescTools::d.countries

  
  elec_data <- tuesdata$technology |> 
    filter(variable == "elec_cons") |> 
    left_join(countries[c(3,5)], by = c("iso3c" = "a3")) |> 
    group_by(region, year) |> 
    summarise(value = sum(value)) |> 
    filter(value > 0, !is.na(region)) |> 
    mutate(cutValues = cut(year, breaks = c(seq(1899, 2020, 10)), labels = 
                             paste0(seq(1900, 2010, 10) , "'s"))) |> 
    group_by(cutValues, region) |> 
    summarise(value = sum(value)/n_distinct(year), .groups = "drop") |> 
    group_by(cutValues) |> 
    mutate(percentage = value/sum(value)) |> 
    filter(cutValues %in% c("1970's", "2010's"))
  
  
  p <- ggplot(data = elec_data) +
    geom_col(aes(x = cutValues, y = percentage, 
                 fill = reorder(region, value, min)), color = "black", position = position_dodge())  +
    labs(
         y = "Electric power consumption (kWh)",
         fill = "Region") +
    scale_y_sqrt( labels = label_percent(), breaks = c(0.02,seq(0.1, 0.6, 0.1))) +
    scale_fill_brewer(type = "div", palette = 1) +
    theme_classic() +
    theme(legend.position = "bottom",
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 12),
          plot.title = element_text(hjust = 0.5, size = 15),
          panel.grid.major.y = element_line(),
          plot.margin = margin(30,30,30,30),
          axis.title.y = element_text(size = 13))

  
  final_p <- ggdraw(add_sub(p, "Figure: Relative regional percentage electricity consumpion, averaged by decade. Source: data.nber.org", size= 11))
  final_p
  ggsave2(filename = "tidytuesday_jul19_2022.png")

