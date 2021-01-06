library(tidyverse)
library(invacost)


invacost.HM <- readxl::read_xlsx("data/invaCost_3.0_complete.xlsx", sheet = "heatmap3")
invacost.HM$log.cost <- log10(invacost.HM$Raw_cost_estimate_2017_USD_exchange_rate)

invacost.HM <- dplyr::arrange(invacost.HM, desc(Probable_starting_year_adjusted)) %>%
  filter(Binned_Decade %in% c("1977-87", "1987-97", "1997-07", "2007-17"))

#-------------------------------------------------------------------------------
heatmap <- ggplot(data = invacost.HM, mapping = aes(x = invacost.HM$Type_of_cost_short,
                                                    y = invacost.HM$Binned_Decade,
                                                    fill = log.cost)) +
  geom_tile() +
  xlab(label = "Cost Category") +
  ylab(label = "Decade") +
  facet_grid(~ invacost.HM$Environment_IAS, switch = "x", scales = "free_x", space = "free_x") +
  facet_wrap(~ invacost.HM$Environment_IAS, scales = "free_x", ncol = 1)+
  scale_fill_gradient(name = "Log Cost (USD millions)",
                      low = "#FFFFFF",
                      high = "#012345",
                      na.value = "white") +
  theme(strip.placement = "Outside",
        legend.position = "bottom",
        axis.text = element_text(size = 12))+
  theme_bw() +
  theme(text = element_text(colour = "black", size = 12, face = "bold"),
        axis.text.x = element_text(colour = "black", size = 12, face = "bold"),
        axis.text.y = element_text(colour = "black", size = 12, face = "bold"),
        legend.position = "bottom")
heatmap


#-------------------------------------------------------------------------------
