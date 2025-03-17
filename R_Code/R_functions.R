#' Univariate Maps
#'
#' @param data_map a list with output from CarcasSink::format_data_to_map()
#' @param values values for the color scale
#' @param color_scale the color scale of cells
#' @param delta if map delta data, = TRUE
#' @param name the names of the map to be saved
#' @param legend the name of the color scale legend
#' @param show.legend 
#' @param overlap TRUE/FALSE
#'
#' @return
#' @export
#'
#' @examples
univariate_map <- function(data_map, values, color_scale, delta = FALSE, legend, show.legend, overlap, factor_overlap, name = NULL){
  
  ### Produce the map
  map <- ggplot2::ggplot() +
    
    ## DBEM output grid
    ggplot2::geom_sf(data    = data_map$data,
                     mapping = ggplot2::aes(fill     = layer,
                                            geometry = geometry),
                     color   = NA,
                     size    = 0.01,
                     show.legend = show.legend) +
    
    ## Add graticules
    ggplot2::geom_sf(data     = data_map$graticules, 
                     linetype = "dotted", 
                     color    = "black", 
                     size     = 0.4) +
    
    ## Add borders grid
    ggplot2::geom_sf(data   = data_map$borders, 
                     colour = NA,  
                     fill   = "gray70") +
    
    ggplot2::geom_sf(data   = data_map$box, 
                     colour = "black", 
                     fill   = NA, 
                     size   = 0.1) +
    
    
    
    ## Add latitude and longitude labels
    ggplot2::geom_text(data = data_map$lat_text, mapping = ggplot2::aes(x = X.prj2-1*10e5, y = Y.prj,          label = lbl), color = "grey20", size = 1.5) +
    ggplot2::geom_text(data = data_map$lon_text, mapping = ggplot2::aes(x = X.prj,         y = Y.prj-0.5*10e5, label = lbl), color = "black",  size = 1.5) + 
    
    ggplot2::labs(fill = legend) +
    
    ggplot2::guides(size = "none", fill = guide_colourbar(title.position = "right", barwidth = 0.7)) +
    
    ## Theme
    ggplot2::theme(panel.grid.major.x = ggplot2::element_line(color = NA),
                   panel.background   = ggplot2::element_blank(),
                   axis.text          = ggplot2::element_blank(),
                   axis.ticks         = ggplot2::element_blank(), 
                   axis.title         = ggplot2::element_blank(),
                   plot.margin        = ggplot2::unit(c(0,0,0,0), "cm"),
                   plot.title         = ggplot2::element_text(size  = 12, 
                                                              face  = "bold", 
                                                              hjust = 0.5, 
                                                              vjust = -0.5),
                   legend.title       = ggplot2::element_text(size  = 12, 
                                                              face  = "bold", 
                                                              hjust = 0.5, 
                                                              vjust = 0.5, angle = 90),
                   legend.title.align = 0.5, 
                   legend.direction   = "vertical",
                   legend.text        = ggplot2::element_text(size = 12))
  

  ### Scale fill colors
  if(overlap == FALSE){
    map <- map +
      ggplot2::scale_fill_gradientn(colors   = color_scale,
                                    values   = if(delta == TRUE){scales::rescale(values)}, 
                                    limits   = if(delta == TRUE){c(round(min(values),0), round(max(values), 0))} else {c(min(data_map$data$layer), max(data_map$data$layer))},
                                    na.value = "transparent") 
    
  }
  
  if(overlap == TRUE){
    
    if(factor_overlap == FALSE){
      map <- map +
        ggplot2::scale_fill_manual(values = values, na.value = "transparent") +
        theme(legend.position = "right", #"bottom",
              legend.spacing.y = unit(1, 'lines')) +
        # legend.direction = "horizontal") +
        # theme(legend.key = element_rect(size = 3)) +
        guides(fill = guide_legend(override.aes = list(color = "black", size = 1))) # , nrow = 1
    }
    
    if(factor_overlap == TRUE){
      map <- map +
        # scale_fill_discrete(values = viridis::viridis(option = color_scale, direction = -1, n = 9), na.value = "transparent", na.tranlate = F) +
        ggplot2::scale_fill_viridis_d(option = color_scale, direction = -1) +
        theme(legend.position = "right",
              legend.spacing.y = unit(1, 'lines')) +
        # theme(legend.key = element_rect(size = 3)) +
        guides(fill = guide_legend(override.aes = list(color = "black", size = 1)))
    }
    
  }
  

  ### Save map
  # if(! is.null(name)) {
  #   
  #   save(map, file = here::here("results", paste0(name, ".RData")))
  #   ggplot2::ggsave(here::here("figures", paste0(name, ".jpeg")), width = 7, height = 4.5, device = "jpeg", dpi = 300)
  #   
  # }
  
  return(map)
  
  
}



#' Bivariate Map
#'
#' @param data_map the data ready to map obtained with CarcasSink::format_data_bivariate_map() 
#' @param bivariate_color_scale a df of the bivariate color scale, obtained with CarcasSink::color_bivariate_map()
#' @param name the name of the map to be saved
#'
#' @return
#' @export
#'
#' @examples
bivariate_map <- function(data_map, bivariate_color_scale, xlab, ylab, name){
  
  # data_map <- tibble::as.tibble(data_map)
  
  ### Produce the map
  map <- ggplot2::ggplot() +
    
    ## DBEM output grid
    ggplot2::geom_sf(data    = data_map$data, 
                     mapping = ggplot2::aes(fill     = fill,
                                            geometry = geometry), 
                     color   = NA, 
                     size    = 0.01) +
    
    ggplot2::scale_fill_identity(na.value = "grey80") +
    ggplot2::theme_void() +
    
    ## Add graticules
    ggplot2::geom_sf(data     = data_map$graticules,
                     linetype = "dotted",
                     color    = "black",
                     size     = 0.4) +
    
    ## Add borders grid
    ggplot2::geom_sf(data   = data_map$borders,
                     colour = NA,
                     fill   = "gray70") +
    
    ggplot2::geom_sf(data   = data_map$box,
                     colour = "black",
                     fill   = NA,
                     size   = 0.1) +
    
    ## Add latitude and longitude labels
    ggplot2::geom_text(data = data_map$lat_text, mapping = ggplot2::aes(x = X.prj2-1*10e5, y = Y.prj,          label = lbl), color = "grey20", size = 1.5) +
    ggplot2::geom_text(data = data_map$lon_text, mapping = ggplot2::aes(x = X.prj,         y = Y.prj-0.5*10e5, label = lbl), color = "black",  size = 1.5) +
    
    ## Theme
    ggplot2::theme(panel.grid.major.x = ggplot2::element_line(color = NA),
                   panel.background   = ggplot2::element_blank(),
                   axis.text          = ggplot2::element_blank(),
                   axis.ticks         = ggplot2::element_blank(), 
                   axis.title         = ggplot2::element_blank(),
                   plot.margin        = ggplot2::unit(c(0,0,0,0), "cm"),
                   plot.title         = ggplot2::element_text(size  = 12, 
                                                              face  = "bold", 
                                                              hjust = 0.5, 
                                                              vjust = -0.5),
                   legend.title       = ggplot2::element_text(size  = 20, 
                                                              face  = "bold", 
                                                              hjust = 0.5, 
                                                              vjust = 0.5),
                   legend.text        = ggplot2::element_text(size = 16))
  
  
  ### Color legend
  
  ## Separate groups
  color <- bivariate_color_scale |> 
    dplyr::mutate(x = as.integer(rep(seq(1, 10, 1), 10)),
                  y = as.integer(rep(1:10, each = 10)))
  
  
  ## Plot
  legend <- ggplot2::ggplot() +
    
    ggplot2::geom_tile(data    = color, 
                       mapping = ggplot2::aes(x = x, y = y, fill = fill)) +
    
    ggplot2::scale_fill_identity() +
    ggplot2::labs(x = xlab, y = ylab) +
    # ggplot2::geom_hline(yintercept = 3.5, color = "red") +
    cowplot::theme_map() +
    ggplot2::theme(axis.title      = ggplot2::element_text(size = 16), 
                   axis.title.x    = ggplot2::element_text(margin = ggplot2::margin(t = 0, 
                                                                                    r = 0, 
                                                                                    b = 0, 
                                                                                    l = 0)),
                   axis.title.y    = ggplot2::element_text(angle  = 90,
                                                           margin = ggplot2::margin(t = 0,
                                                                                    r = 5,
                                                                                    b = 0,
                                                                                    l = 0)),
                   plot.background = ggplot2::element_rect(fill  = "white", 
                                                           color = "transparent")) +
    ggplot2::coord_fixed()
  
  
  ### Arrange map with legend
  map_bi <- cowplot::ggdraw() +
    # cowplot::draw_plot(map,    x = 0.0, y = 0.00, width = 0.70, height = 1.0) +
    # cowplot::draw_plot(legend, x = 0.65, y = 0.30, width = 0.35, height = 0.35)
    cowplot::draw_plot(map,    x = 0.0, y = 0.30, width = 1.0, height = 0.7) +
    cowplot::draw_plot(legend, x = 0.35, y = 0.00, width = 0.3, height = 0.3)
  
  
  ### Save map
  if(! is.null(name)) {
    
    save(map_bi, file = here::here("results", paste0(name, ".RData")))
    ggplot2::ggsave(here::here("figures", paste0(name, ".jpeg")), width = 8.5, height = 6, device = "jpeg", dpi = 300)
    
  }
  
  return(map_bi)
  
}