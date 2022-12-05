library(UpSetR)
library(grid)
library(gridExtra)
library(ggplot2)

#Code taken from:
##https://stackoverflow.com/questions/46223273/use-a-color-palette-for-matrix-points-in-upsetr
Create_layout <- function (setup, mat_color, mat_col, matrix_dot_alpha) 
{
  Matrix_layout <- expand.grid(y = seq(nrow(setup)), x = seq(ncol(setup)))
  Matrix_layout <- data.frame(Matrix_layout, value = as.vector(setup))
  for (i in 1:nrow(Matrix_layout)) {
    if (Matrix_layout$value[i] > as.integer(0)) {
      # Here I propose to change Matrix_layout$color[i] <- mat_color with
      # Matrix_layout$color[i] <- mat_color[i]
      Matrix_layout$color[i] <- mat_color[i]
      Matrix_layout$alpha[i] <- 1
      Matrix_layout$Intersection[i] <- paste(Matrix_layout$x[i], 
                                             "yes", sep = "")
    }
    else {
      Matrix_layout$color[i] <- "gray83"
      Matrix_layout$alpha[i] <- matrix_dot_alpha
      Matrix_layout$Intersection[i] <- paste(i, "No", sep = "")
    }
  }
  if (is.null(mat_col) == F) {
    for (i in 1:nrow(mat_col)) {
      mat_x <- mat_col$x[i]
      mat_color <- as.character(mat_col$color[i])
      for (i in 1:nrow(Matrix_layout)) {
        if ((Matrix_layout$x[i] == mat_x) && (Matrix_layout$value[i] != 
                                              0)) {
          Matrix_layout$color[i] <- mat_color
        }
      }
    }
  }
  return(Matrix_layout)
}


assignInNamespace(x="Create_layout", value=Create_layout, ns="UpSetR")
###

##Code taken from:
###https://stackoverflow.com/questions/58131712/upset-plot-with-set-size-bars-in-right-side
NoAttBasePlot <- function (legend, size_plot_height, Main_bar_plot, Matrix_plot, 
                           hratios, Size_plot, query_legend, set_metadata, set_metadata_plots, 
                           newpage) {
  top <- 1
  bottom <- 100
  if ((!is.null(legend)) && (query_legend != tolower("none"))) {
    if (query_legend == tolower("top")) {
      top <- 3
      bottom <- 102
      legend_top <- 1
      legend_bottom <- 3
      size_plot_height <- (size_plot_height + 2)
    }
    else if (query_legend == tolower("bottom")) {
      legend_top <- 101
      legend_bottom <- 103
    }
  }
  if (is.null(set_metadata)) {
    matrix_and_mainbar_right <- 100
    matrix_and_mainbar_left <- 21
    size_bar_right <- 20
    size_bar_left <- 1
  }
  else if (!is.null(set_metadata)) {
    matrix_and_mainbar_right <- set_metadata$ncols + 100
    matrix_and_mainbar_left <- set_metadata$ncols + 21
    size_bar_right <- set_metadata$ncols + 20
    size_bar_left <- set_metadata$ncols + 1
    metadata_right <- set_metadata$ncols
    metadata_left <- 1
  }
  if (newpage) {
    grid::grid.newpage()
  }
  if ((!is.null(legend)) && (query_legend != tolower("none"))) {
    if (query_legend == tolower("top")) {
      pushViewport(viewport(layout = grid.layout(102, matrix_and_mainbar_right)))
    }
    else if (query_legend == tolower("bottom")) {
      pushViewport(viewport(layout = grid.layout(103, matrix_and_mainbar_right)))
    }
  }
  else if ((is.null(legend)) || (query_legend == tolower("none"))) {
    pushViewport(viewport(layout = grid.layout(100, matrix_and_mainbar_right)))
  }
  # Modified
  vp = UpSetR:::vplayout(top:bottom, 1:(matrix_and_mainbar_right-matrix_and_mainbar_left))
  pushViewport(vp)
  grid.draw(arrangeGrob(Main_bar_plot, Matrix_plot, heights = hratios))
  popViewport()
  # Modified
  vp = UpSetR:::vplayout(size_plot_height:bottom, (matrix_and_mainbar_right-matrix_and_mainbar_left-1):96)
  pushViewport(vp)
  grid.draw(arrangeGrob(Size_plot))
  popViewport()
  if (!is.null(set_metadata)) {
    for (i in 1:length(set_metadata_plots)) {
      if (i != 1) {
        metadata_left <- 1 + metadata_right
        metadata_right <- metadata_right + set_metadata$plots[[i]]$assign
      }
      else {
        metadata_left <- 1
        metadata_right <- set_metadata$plots[[i]]$assign
      }
      vp = UpSetR:::vplayout(size_plot_height:bottom, metadata_left:metadata_right)
      pushViewport(vp)
      grid.draw(arrangeGrob(set_metadata_plots[[i]]))
      popViewport()
    }
  }
  if ((!is.null(legend)) && (query_legend != tolower("none"))) {
    vp = UpSetR:::vplayout(legend_top:legend_bottom, matrix_and_mainbar_left:matrix_and_mainbar_right)
    pushViewport(vp)
    grid.draw(arrangeGrob(legend))
    popViewport()
  }
}

Make_size_plot <- function (Set_size_data, sbar_color, ratios, ylabel, scale_sets, 
                            text_scale, set_size_angle, set_size.show, set_size.scale_max, 
                            set_size.number_size) {
  if (length(text_scale) > 1 && length(text_scale) <= 6) {
    x_axis_title_scale <- text_scale[3]
    x_axis_tick_label_scale <- text_scale[4]
  }
  else {
    x_axis_title_scale <- text_scale
    x_axis_tick_label_scale <- text_scale
  }
  if (ylabel == "Set Size" && scale_sets != "identity") {
    ylabel <- paste("Set Size", paste0("( ", 
                                       scale_sets, " )"))
    if (scale_sets == "log2") {
      Set_size_data$y <- log2(Set_size_data$y)
    }
    if (scale_sets == "log10") {
      Set_size_data$y <- log10(Set_size_data$y)
    }
  }
  if (!is.null(set_size.number_size)) {
    num.size <- (set_size.number_size/1) * x_axis_tick_label_scale
  }
  else {
    num.size <- (7/2.845276) * x_axis_tick_label_scale
  }
  Size_plot <- (ggplot(data = Set_size_data, aes_string(x = "x", 
                                                        y = "y")) + geom_bar(stat = "identity", colour = sbar_color, 
                                                                             width = 0.4, fill = sbar_color, position = "identity") + 
                  scale_x_continuous(limits = c(0.5, (nrow(Set_size_data) + 
                                                        0.5)), breaks = c(0, max(Set_size_data)), expand = c(0, 
                                                                                                             0)) + theme(panel.background = element_rect(fill = "white"), 
                                                                                                                         plot.margin = unit(c(-0.11, -1.3, 0.5, 0.5), "lines"), 
                                                                                                                         axis.title.x = element_text(size = 8.3 * x_axis_title_scale), 
                                                                                                                         axis.text.x = element_text(size = 7 * x_axis_tick_label_scale, 
                                                                                                                                                    vjust = 1, hjust = 0.5), axis.line = element_line(colour = "gray0"), 
                                                                                                                         axis.line.y = element_blank(), axis.line.x = element_line(colour = "gray0", 
                                                                                                                                                                                   size = 0.3), axis.text.y = element_blank(), axis.ticks.y = element_blank(), 
                                                                                                                         panel.grid.minor = element_blank(), panel.grid.major = element_blank()) + 
                  xlab(NULL) + ylab(ylabel) + coord_flip())
  if (set_size.show == TRUE) {
    Size_plot <- (Size_plot + geom_text(aes(label = y, vjust = 0.5, 
                                            hjust = 1.2, angle = set_size_angle), size = num.size))
  }
  if (scale_sets == "log10") {
    if (!is.null(set_size.scale_max)) {
      Size_plot <- (Size_plot + scale_y_continuous(limits = c(set_size.scale_max, 
                                                              0), trans = log10_reverse_trans()))
    }
    else {
      Size_plot <- (Size_plot + scale_y_continuous(trans = log10_reverse_trans()))
    }
  }
  else if (scale_sets == "log2") {
    if (!is.null(set_size.scale_max)) {
      Size_plot <- (Size_plot + scale_y_continuous(limits = c(set_size.scale_max, 
                                                              0), trans = log2_reverse_trans()))
    }
    else {
      Size_plot <- (Size_plot + scale_y_continuous(trans = log2_reverse_trans()))
    }
  }
  else {
    if (!is.null(set_size.scale_max)) {
      Size_plot <- (Size_plot + scale_y_continuous(limits = c(set_size.scale_max, 
                                                              0), trans = "reverse"))
    }
    else {
      # Modified
      #Size_plot <- (Size_plot + scale_y_continuous(trans = "reverse"))
    }
  }
  Size_plot <- ggplot_gtable(ggplot_build(Size_plot))
  return(Size_plot)
}

assignInNamespace(x="NoAttBasePlot", value=NoAttBasePlot, ns="UpSetR")
assignInNamespace(x="Make_size_plot", value=Make_size_plot, ns="UpSetR")

##

#Including Other
Proport <- c(
  "Sequence errors" = 88,
  "Cell-line errors" = 63,
  "Figure errors" = 22,
  "Sequence errors&Cell-line errors" = 15,
  "Sequence errors&Figure errors" = 8,
  "Cell-line errors&Figure errors" = 8,
  "Sequence errors&Cell-line errors&Figure errors" = 1,
  "Other" = 1
)

upset(fromExpression(Proport),
      nintersects = NA,
      nsets = 1,
      order.by = "freq",
      decreasing = T,
      mb.ratio = c(0.7, 0.3),
      number.angles = 0,
      text.scale = 1.5,
      line.size = 0,
      sets = c("Sequence errors", "Cell-line errors", "Figure errors", "Other"), 
      point.size=4,
      main.bar.color=c("#E95C20FF", "#006747FF", "#4F2C1DFF",
                       "#af5f2aFF", "#284a32FF", "#9c441e",
                       "black", "grey"),
      matrix.color=c("#E95C20FF","#006747FF","#4F2C1DFF", "grey",
                     "#E95C20FF","#006747FF","#4F2C1DFF", "grey",
                     "#E95C20FF","#006747FF","#4F2C1DFF", "grey",
                     "#E95C20FF","#006747FF","#4F2C1DFF", "grey",
                     "#E95C20FF","#006747FF","#4F2C1DFF", "grey",
                     "#E95C20FF","#006747FF","#4F2C1DFF", "grey",
                     "#E95C20FF","#006747FF","#4F2C1DFF", "grey"),
      sets.bar.color=c("#E95C20FF","#006747FF","#4F2C1DFF", "grey"),
      set_size.show = TRUE)

#Excluding other
Proport_no_other <- c(
  "Sequence errors" = 88,
  "Cell-line errors" = 63,
  "Figure errors" = 22,
  "Sequence errors&Cell-line errors" = 15,
  "Sequence errors&Figure errors" = 8,
  "Cell-line errors&Figure errors" = 8,
  "Sequence errors&Cell-line errors&Figure errors" = 1
)

upset(fromExpression(Proport_no_other),
      nintersects = NA,
      nsets = 1,
      order.by = "freq",
      decreasing = T,
      mb.ratio = c(0.7, 0.3),
      number.angles = 0,
      text.scale = 1.5,
      line.size = 0,
      sets = c("Sequence errors", "Cell-line errors", "Figure errors"), 
      point.size=4,
      main.bar.color=c("#F9A12E","#FC766A","#9B4A97",
                       "#F9A12E", "#FC766A", "#F9A12E",
                       "#F9A12E"),
      matrix.color=c("#F9A12E","#FC766A","#9B4A97",
                     "#F9A12E","#FC766A","#9B4A97",
                     "#F9A12E","#FC766A","#9B4A97",
                     "#F9A12E","#FC766A","#9B4A97",
                     "#F9A12E","#FC766A","#9B4A97",
                     "#F9A12E","#FC766A","#9B4A97",
                     "#F9A12E","#FC766A","#9B4A97"),
      sets.bar.color=c("#F9A12E","#FC766A","#9B4A97"),
      set_size.show = TRUE)
