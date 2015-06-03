#' Build ggplot for rendering. (optimized for data.table)
#'
#' This function takes the plot object, and performs all steps necessary to
#' produce an object that can be rendered.  This function outputs two pieces:
#' a list of data frames (one for each layer), and a panel object, which
#' contain all information about axis limits, breaks etc.
#'
#' @param plot ggplot object
#' @seealso \code{\link{print.ggplot}} and \code{link{benchplot}} for
#'  for functions that contain the complete set of steps for generating
#'  a ggplot2 plot.
#' @keywords internal
#' @export
ggplot_build <- function(plot) {
  if (length(plot$layers) == 0) stop("No layers in plot", call.=FALSE)

  plot <- ggplot2:::plot_clone(plot)
  layers <- plot$layers
  layer_data <- lapply(layers, function(y) y$data)

  scales <- plot$scales
  # Apply function to layer and matching data
  dlapply <- function(f) {
    out <- vector("list", length(data))
    for(i in seq_along(data)) {
      
      out[[i]] <- f(d = data[[i]], p = layers[[i]])
    }
    out
  }

  # Initialise panels, add extra data for margins & missing facetting
  # variables, and add on a PANEL variable to data
  
  pd <- pData(plot$data)
  attr(pd, "raw") <- plot$data
  panel <- ggplot2:::new_panel()
  panel <- ggplot2:::train_layout(panel, plot$facet, layer_data, pd)
  data <- ggplot2:::map_layout(panel, plot$facet, layer_data, pd)

  # Compute aesthetics to produce data with generalised variable names
  
  data <- dlapply(function(d, p)compute_aesthetics(p, d, plot))
  data <- lapply(data, ggplot2:::add_group)
  
  # Transform all scales
  #TODO: overwrite scales_transform_df
  data <- lapply(data, ggplot2:::scales_transform_df, scales = scales)

  # Map and train positions so that statistics have access to ranges
  # and all positions are numeric
  scale_x <- function() scales$get_scales("x")
  scale_y <- function() scales$get_scales("y")

  panel <- ggplot2:::train_position(panel, data, scale_x(), scale_y())
  data <- ggplot2:::map_position(panel, data, scale_x(), scale_y())

  # Apply and map statistics
  data <- calculate_stats(panel, data, layers)
  data <- dlapply(function(d, p) p$map_statistic(d, plot))
  data <- lapply(data, order_groups)

  # Make sure missing (but required) aesthetics are added
  scales_add_missing(plot, c("x", "y"))

  # Reparameterise geoms from (e.g.) y and width to ymin and ymax
  data <- dlapply(function(d, p) p$reparameterise(d))

  # Apply position adjustments
  data <- dlapply(function(d, p) p$adjust_position(d))

  # Reset position scales, then re-train and map.  This ensures that facets
  # have control over the range of a plot: is it generated from what's
  # displayed, or does it include the range of underlying data
  reset_scales(panel)
  panel <- train_position(panel, data, scale_x(), scale_y())
  data <- map_position(panel, data, scale_x(), scale_y())

  # Train and map non-position scales
  npscales <- scales$non_position_scales()
  if (npscales$n() > 0) {
    lapply(data, scales_train_df, scales = npscales)
    data <- lapply(data, scales_map_df, scales = npscales)
  }

  # Train coordinate system
  panel <- train_ranges(panel, plot$coordinates)

  list(data = data, panel = panel, plot = plot)
}

compute_aesthetics <- function(., data, plot) {
  
  aesthetics <- .$layer_mapping(plot$mapping)
  
  if (!is.null(.$subset)) {
  
    include <- data.frame(eval.quoted(.$subset, data, plot$env))
    data <- data[rowSums(include, na.rm = TRUE) == ncol(include), ]
  }
  
  # Override grouping if set in layer.
  if (!is.null(.$geom_params$group)) {
    aesthetics["group"] <- .$geom_params$group
  }
  
  ggplot2:::scales_add_defaults(plot$scales, data, aesthetics, plot$plot_env)
  
  #skip evaluation if it is pData
  if(!is.null(attr(data, "raw"))){
    data
  }else{
    # Evaluate aesthetics in the context of their data frame
    evaled <- compact(
      eval.quoted(aesthetics, data, plot$plot_env))  
  
  
    lengths <- vapply(evaled, length, integer(1))
    n <- if (length(lengths) > 0) max(lengths) else 0
    
    wrong <- lengths != 1 & lengths != n
    if (any(wrong)) {
      stop("Aesthetics must either be length one, or the same length as the data",
           "Problems:", paste(aesthetics[wrong], collapse = ", "), call. = FALSE)
    }
    
    if (empty(data) && n > 0) {
      # No data, and vectors suppled to aesthetics
      evaled$PANEL <- 1
    } else {
      evaled$PANEL <- data$PANEL
    }
    data.frame(evaled)
  }
}

# Calculate statistics
#
# @param layers list of layers
# @param data a list of data frames (one for each layer)
calculate_stats <- function(panel, data, layers) {
  
  lapply(seq_along(data), function(i) {
    d <- data[[i]]
    l <- layers[[i]]
  browser()  
  attr(d, "raw")
    ddply(d, "PANEL", function(panel_data) {
      scales <- panel_scales(panel, panel_data$PANEL[1])
      l$calc_statistic(panel_data, scales)
    })
  })
}