library(tidyverse)
library(invacost)


invacost.HM <- readxl::read_xlsx("data/invaCost_3.0_complete.xlsx", sheet = "heatmap1")
invacost.HM$log.cost <- log10(invacost.HM$Raw_cost_estimate_2017_USD_exchange_rate)

invacost.HM <- dplyr::filter(invacost.HM, Probable_starting_year_adjusted >= 1977 | 
                               is.na(Probable_starting_year_adjusted))
#-------------------------------------------------------------------------------
heatmap <- ggplot(data = invacost.HM, mapping = aes(x = invacost.HM$Management_Type,
                                                    y = invacost.HM$Applicable_Decade,
                                                    fill = log.cost)) +
  geom_tile() +
  xlab(label = "Category") +
  ylab(label = "Decade") +
  facet_grid(~ invacost.HM$Environment_IAS, switch = "x", scales = "free_x", space = "free_x") +
  facet_wrap(~ invacost.HM$Environment_IAS, scales = "free_x", ncol = 2)+
  scale_fill_gradient(name = "Log Cost (USD billions)",
                      low = "#FFFFFF",
                      high = "#012345",
                      na.value = "white") +
  theme(strip.placement = "Outside",
        legend.position = "bottom",
        axis.text = element_text(size = 15))+
  theme_bw() +
  theme(text = element_text(colour = "black", size = 12, face = "bold"),
        axis.text.x = element_text(colour = "black", size = 12, face = "bold"),
        axis.text.y = element_text(colour = "black", size = 12, face = "bold"),
        legend.position = "bottom")
heatmap


#-------------------------------------------------------------------------------
