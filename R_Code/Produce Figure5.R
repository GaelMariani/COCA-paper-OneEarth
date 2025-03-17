########################################
##########                    ##########
##########  Produce Figure 4  ##########
##########                    ##########
########################################
rm(list = ls(), envir = .GlobalEnv)
library(ggplot2)


### ---------------------------------------------
### Load R functions
### ---------------------------------------------
### ---- 
source(here::here("R_Code", "R_functions.R"))
### ---- 



### ---------------------------------------------
### Load RData
### ---------------------------------------------
### ---- 

  ### ----- Bivariate data between Cexport and Fishing
  load(here::here("RData", "data_figure5_map.RData"))
  
  ### ----- Bivariate data between Cceq and Fishing
  load(here::here("RData", "data_figure5_donut.RData"))
  

### ----
  

### ---------------------------------------------
### Produce the panels and the figure
### ---------------------------------------------
### ---- 

  ### ----- Panel A
  map_all <- univariate_map(data_map       = trawl_pel, 
                            legend         = "",
                            delta          = FALSE,
                            show.legend    = TRUE,
                            overlap        = TRUE,
                            factor_overlap = FALSE,
                            values         = c("Pelagic"  = "seagreen4",
                                               "Trawling" = "slateblue3",
                                               "Both"     = "firebrick3"),
                            name           = NULL)
  
  
  ### ----- Panel B
  donut <- ggplot(data_donut, aes(x = x, y = perc, fill = reorder(layer, perc))) +
    geom_col(show.legend = F) +
    geom_text(aes(label = paste0(round(perc, 1), "%"), x = x, y = pos), color = "white") +
    scale_fill_manual(values = c("Pelagic"  = "seagreen4",
                                 "Trawling" = "slateblue3",
                                 "Both"     = "firebrick3"),
                      name = NULL) +
    scale_x_discrete(limits = c(" ", "xaxis")) +
    coord_polar("y") +
    theme_void()
  
  ### ----- Assemble the figure
  fig <- cowplot::ggdraw() + 
    cowplot::draw_plot(map_all,  x = 0.00, y = 0, width = 0.75, height = 1) +
    cowplot::draw_plot(donut,    x = 0.75, y = 0, width = 0.25, height = 1) +
    cowplot::draw_plot_label(label = c("A", "B"),
                             size = 15,
                             x = c(0, 0.73),
                             y = c(0.75, 0.75))
  
  ### ----- Save the figure
  ggplot2::ggsave(here::here("Figures", "Figure5.jpeg"), width = 9, height = 6, device = "jpeg", dpi = 600)
  
  
  ### 
  