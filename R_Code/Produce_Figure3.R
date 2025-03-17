########################################
##########                    ##########
##########  Produce Figure 3  ##########
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
  load(here::here("RData", "data_bivar_Cexport_fishEff.RData"))
  
  ### ----- Bivariate data between Cceq and Fishing
  load(here::here("RData", "data_bivar_Cseq_fishEff.RData"))
  
  ### ----- COCA between Cexport and Fishing
  load(here::here("RData", "data_quantile_0.75_Cexp.RData"))
  
  ### ----- COCA between Csequestration and Fishing
  load(here::here("RData", "data_quantile_0.75_Cseq.RData"))
  
  ### ----- Binded data (for panel E)
  load(here::here("RData", "binded.data75.pelagic.RData"))
  
  ### ----- Data for the donut plot
  load(here::here("RData", "data_donut75.pelagic.RData"))
  
  ### ----- Bivriate color scale
  load(here::here("RData", "bivariate_color_scale.RData"))

### ---- 
  

### ---------------------------------------------
### Produce the panels and the figure
### ---------------------------------------------
### ---- 
  
  ### ----- Panel A
  map_Cexport_FE <- bivariate_map(data_map = data_bivar_Cexport_fishEff, 
                                  bivariate_color_scale = bivariate_color_scale,
                                  ylab = "Cexport", 
                                  xlab = "Fishing",
                                  name = NULL)
  
  
  ### ----- Panel B
  map_Cseq_FE <- bivariate_map(data_map = data_bivar_Cseq_fishEff, 
                               bivariate_color_scale = bivariate_color_scale,
                               ylab = "Cseq", 
                               xlab = "Fishing",
                               name = NULL)
  
  ### ----- Panel C
  map_q75_Cexport <- univariate_map(data_map    = data_quantile_0.75_Cexp,
                                    color_scale = "lightsalmon",
                                    legend      = "",
                                    delta       = FALSE,
                                    show.legend = FALSE,
                                    overlap     = FALSE,
                                    name        = NULL)
  
  ### ----- Panel D
  map_q75_Cseq <- univariate_map(data_map    = data_quantile_0.75_Cseq,
                                 color_scale = "lightseagreen",
                                 legend      = "",
                                 delta       = FALSE,
                                 show.legend = FALSE,
                                 overlap     = FALSE,
                                 name        = NULL)
  
  ### ----- Panel E
  map_overlap <- univariate_map(data_map       = binded.data75, 
                                legend         = "",
                                delta          = FALSE,
                                show.legend    = TRUE,
                                overlap        = TRUE,
                                factor_overlap = FALSE,
                                values         = c("Export"        = "lightsalmon",# alpha("firebrick4", alpha = 0.9),
                                                   "Sequestration" = "lightseagreen", # alpha("navyblue", alpha = 0.8),
                                                   "Both"          = "goldenrod3"),
                                name           = NULL)
  
  ### ----- Panel F
  donut <- ggplot(data_donut75, aes(x = x, y = perc, fill = reorder(layer, perc))) +
    geom_col(show.legend = F) +
    geom_text(aes(label = paste0(round(perc, 1), "%"), x = x, y = pos), color = "white") +
    scale_fill_manual(values = c("Export"        = "lightsalmon",# alpha("firebrick4", alpha = 0.9),
                                 "Sequestration" = "lightseagreen", # alpha("navyblue", alpha = 0.8),
                                 "Both"          = "goldenrod3"),
                      name = NULL) + 
    scale_x_discrete(limits = c(" ", "xaxis")) +
    coord_polar("y")+
    theme_void() 
  
  
  ### ----- Arrange the figure
  fig <- cowplot::ggdraw() + 
    cowplot::draw_plot(map_Cexport_FE,  x = -0.08, y = 0.61, width = 0.60, height = 0.39) +
    cowplot::draw_plot(map_Cseq_FE,   x = 0.42,  y = 0.61, width = 0.60, height = 0.39) +
    cowplot::draw_plot(map_q75_Cexport, x = 0.00,  y = 0.33, width = 0.45, height = 0.29) +
    cowplot::draw_plot(map_q75_Cseq,  x = 0.50,  y = 0.33, width = 0.45, height = 0.29) +
    cowplot::draw_plot(map_overlap,   x = 0.00,  y = 0.00, width = 0.60, height = 0.33) +
    cowplot::draw_plot(donut,         x = 0.50,  y = 0.00, width = 0.50, height = 0.33) +
    cowplot::draw_plot_label(label = c("A", "B", "C", "D", "E", "F"),
                             size = 15,
                             x = c(0, 0.5, 0, 0.5, 0, 0.5),
                             y = c(0.99, 0.99, 0.61, 0.61, 0.30, 0.30)) 
  
  
  ### ----- Save the figure
  ggplot2::ggsave(here::here("Figures", "Figure3.jpeg"), width = 12, height = 10, device = "jpeg", dpi = 600)
  
  
### 