#' ggplot_add
#'
#'ggplot_add
#'
#'ggplot_add
#'
#'@export
#'
#'
ggplot_add.survivalverse_plot = function(object, plot, object_name){
    args = attributes(object)
    args$fn(args, plot)
}
