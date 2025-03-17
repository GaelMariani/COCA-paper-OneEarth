########################################
##########                    ##########
##########  Produce Figure 2  ##########
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

  ### ---- Export data
  load(here::here("RData", "Cexport_at_75m.RData"))
  
  ### ---- Sequestration origin data
  load(here::here("RData", "Cseq_origin.RData"))
  
  ### ---- Time of sequestration data
  load(here::here("RData", "Tseq.RData"))

### ---- 



### ---------------------------------------------
### Produce the panels and the figure
### ---------------------------------------------
### ---- 
  
  ### ----- Map of C export
  map_Cexport <- univariate_map(data_map    = data2map_carbon,
                                color_scale = viridis::viridis(6, direction = 1),
                                legend      = expression(atop("(tC."~km^-2*"."~yr^-1*")")),
                                delta       = FALSE,
                                show.legend = TRUE,
                                overlap     = FALSE,
                                name        = NULL) ; map_Cexport
  
  
  ### ----- Map of C sequestration (by origin)
  map_Cseq <- univariate_map(data_map    = data2map_Cseq,
                             color_scale = viridis::viridis(6, direction = 1),
                             legend      = expression(atop("x"~10^3*" (tC."~km^-2*")")),
                             delta       = FALSE,
                             show.legend = TRUE,
                             overlap     = FALSE,
                             name        = NULL) ; map_Cseq
  
  
  ### ----- Map of sequestration time
  map_Tseq <- univariate_map(data_map    = data2map_Tseq,
                             color_scale = viridis::viridis(6, direction = 1),
                             legend      = expression(atop("(Years)")),
                             delta       = FALSE,
                             show.legend = TRUE,
                             overlap     = FALSE,
                             name        = NULL) ; map_Tseq



  ### ----- Arrange the figure
  fig <- cowplot::ggdraw() + 
    
    cowplot::draw_plot(map_Cexport + ggtitle("Export"), x = 0.00, y = 0.66, width = 1, height = 0.32) +
    cowplot::draw_plot(map_Cseq + ggtitle("Sequestration (Origin)"), x = -0.02, y = 0.33, width = 1, height = 0.32) +
    cowplot::draw_plot(map_Tseq + ggtitle("Time of Sequestation"), x = 0.00, y = 0.00, width = 1, height = 0.32) +
    cowplot::draw_plot_label(label = c("A", "B", "C"),
                             size = 15,
                             x = c(0, 0, 0),
                             y = c(0.98, 0.67, 0.33))

  ggplot2::ggsave(here::here("Figures", "Figure2.jpeg"), width = 6, height = 7, device = "jpeg", dpi = 600)



### ---- 
