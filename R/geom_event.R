#' Create event plot
#'
#' Create an event plot from provided data
#'
#' This function creates an event plot from the provided data. On an event plot,
#' the x-axis is time, while the y-axis has each event separated. By default, the
#' graph shows solid lines where it is known that an event does not occur, and dashed
#' lines where an event might have occurred, based on the provided data. If exact
#' information is known about the event it is marked by an X. Otherwise, the time period
#' where an event could have occurred is separated by triangles enclosing the dashed area.
#'
#'
#' @param mapping Aesthetics mapping with the following inputs: \cr
#'    * time: time column \cr
#'    * time2: optional secondary time column (e.g. time2 or event/status column specified in Surv() arguments) \cr
#'    * treatments: A single treatments or a vector of treatments.  Aliased arguments include treatment, trt, and trts. \cr
#'    * strata: A single stratum or a vector of strata values \cr
#' @param ... Other graphical arguments pertaining to the primary failure/survival curve (color, linetype, linewidth, etc). 
#' @param data The data to use, if not provided in the ggplot call
#' @param inherit.aes Logical, whether to inherit graphics from the ggplot call. TRUE by default.
#' @param order Logical, whether to order the event plot based on when it is censored. FALSE by default.
#' @param xmax Maximum maximum time for the x-axis.  Defaults to 1.1x maximum of (time1,time2) after the data is converted to interval censored format.
#'
#' @export
#' @import tidyverse
#' @import survival
#' @import showtext
#' @import systemfonts
#'
#' @examples
#' p = mfg %>% ggplot()
#' #Basic plot
#' p + geom_event(ttr, status)
#' #Coloring by status
#' p + geom_event(ttr, status, aes(color = status))
geom_event = function(mapping = NULL, ..., inherit.aes = T, order = T, xmax = NULL, data = NULL) {
    # This function simply packages the inputs that are then dealt with when ggplot
    # tries to add an object of the "event_plot" class to a ggplot object
    extras = list2(...)
    #Check if they piped in
    structure(
        "",
        class = "survivalverse_plot",
        fn = create_event_plot,
        mapping = mapping,
        extras = list2(...),
        order = order,
        shape = extras[["shape"]],
        point.color = extras[["point.color"]],
        segment.color = extras[["segment.color"]],
        line.alpha = extras[["line.alpha"]],
        inherit.aes = inherit.aes,
        xmax = xmax,
        data = data
    )
}

# note: function had a hard time deciphering point and line color, so I separated them.  
# If color argument is provided, it'll default to the point color
# also: b/c it's using geom_text to do the point plotting

# ------------------------------------------------------------------------------
# UMBRELLA FUNCTION
# ------------------------------------------------------------------------------

create_event_plot = function(args, plot){
    # do basic data checks before creating event plot layers
    args = check_aliases(args)
    args$extras = check_aliases(args$extras)
    names(args); names(args$extras)
    # catch other arguments that may have been assigned to args instead of extras (not ideal, but necessary until I detangle things)
    #args$extras = c(unlist(args$extras), args[! names(args) %in% c("mapping", "data","order","inherit.aes","xmax","class","fn")])
    args = wrangle_event_global_args(args)
    # print("post wrangle_event_global_args(args)"); print(args)
    args = event_wrangle_args(args, plot)
    plot$survivalverse_inherit = args$survivalverse_inherit
    add_event_layers(args, plot)
}


# ------------------------------------------------------------------------------
# CREATE EVENT PLOT LAYERS
# ------------------------------------------------------------------------------

# library(showtext)
# font_add_google("Noto Sans","Noto Sans")
# showtext_auto()

add_event_layers = function(args, plot){
    var_args=as.list(args$mapping)
    Time = as_quosure(var_args$time)
    Treatment = as_quosure(var_args$treatments)
    
    ds_set=event_create_ds(args)  # wrangle the dataset to get the start and end points
    ds=ds_set[["ds"]]  # dataset for line segments
    ds_points=ds_set[["ds_points"]]  # dataset for left and/or right points
    
    print("ds")
    print(head(ds,10))
    print("ds_points")
    print(head(ds_points,10))
    
    extra_args_points = Filter(Negate(is.null), args$point_extra_args)
    extra_args_line = Filter(Negate(is.null), args$line_extra_args)
    
    if(! is.null(args$extras$shape)){
        ds_points = ds_points %>%
            mutate(
                point_shape=args$extras$shape
            )
    }

    if(! has_name(ds,"treatments")){  # No treatment variable
        # points layer
        points_layer = do.call(geom_text,c(
                    list( 
                        data=ds_points,
                        mapping=aes(x = time_value, y=ids, label=point_char),
                        inherit.aes=F, show.legend=F, family = "sans"
                    ),
                    extra_args_points
                    )
                )
    
        # line segment
        line_segment = do.call(geom_segment,c(
                    list( 
                        data=ds,
                        mapping=aes(x=line_start, xend = end, y=ids, linetype=cens), 
                        inherit.aes=F
                    ),
                    extra_args_line
                    )
                )
        
    } else{ # with treatments
        # points layer
        ds_points = ds_points %>% rename( !!Treatment := treatments )
        ds = ds_points %>% rename( !!Treatment := treatments )

        points_layer = do.call(geom_text,c(
                    list( 
                        data=ds_points,
                        mapping=aes(x = time_value, y=ids, color=!!Treatment, label=point_char),
                        inherit.aes=F, show.legend=F, family = "sans"
                    ),
                    extra_args_points
                    )
                )
    
        # line segment
        line_segment = do.call(geom_segment,c(
                    list( 
                        data=ds,
                        mapping=aes(x=line_start, xend = end, y=ids, linetype=cens, color=!!Treatment), 
                        inherit.aes=F
                    ),
                    extra_args_line
                    )
                )

    }
    # putting it together
      plot + line_segment + points_layer + 
        guides( linetype="none" ) + xlim(0,args$xmax*1.10)  +
        labs(
            y="",
            x=as_label(Time)
        ) +
        # scale_shape_manual(
        #     values=c(
        #         "right_start"="\u25BB", # right facing arrow
        #         "right_end"="",
        #         "left_start"="",
        #         "left_end"="\u25C5", # left facing arrow
        #         "none_start"="",
        #         "none_end"="\u00D7",  # x
        #         "interval_start"="\u25BB", # right facing arrow
        #         "interval_end"="\u25C5" # left facing arrow
        #       )
        # ) +
        coord_cartesian(clip = "off")
}

# ------------------------------------------------------------------------------
# CREATE EVENT PLOT DATASET
# ------------------------------------------------------------------------------


event_create_ds = function(args){
    xmax=args$xmax
    ds=args$ds
    cens_type=args$cens_type
  
    # converting to interval format
    ds=ds %>%
        mutate(
            cens=case_when(
                ! has_name(ds,"time2") ~ "none",
                time==time2 ~ "none",
                time==0 | time==-Inf | is.na(time) ~ "left",
                time2==Inf | is.na(time2) ~ "right",
                time2==1 & cens_type=="right" ~ "none",
                time2==0 & cens_type=="right" ~ "right",
                .default = "interval"
            ),
            start=case_when(
                is.finite(time) ~ pmax(0,time),  # make sure time is nonnegative
                is.na(time) | time==-Inf ~ 0,
                .default = NA
            ),
            line_start=ifelse(cens %in% c("none","left"), 0, start),
            end=case_when(
                cens=="none" ~ time,
                cens=="right" ~ xmax,
                is.na(time2) ~ xmax,  # right censored
                is.infinite(time2) ~ xmax,
                .default = time2
            ),
            cens2=ifelse(cens=="none","no censoring","censoring")
        ) 

    if(args$order==T | args$order=="asc"){
        ds=ds %>%
          mutate(ids=fct_drop(fct_reorder(ids,time,.desc=FALSE))) %>%
          arrange(time)
    } else if(args$order=="desc"){
        ds=ds %>%
          mutate(ids=fct_drop(fct_reorder(ids,time,.desc=TRUE))) %>%
          arrange(desc(time))
    }  
    
    ds_points = ds %>%
        pivot_longer(c(start,end), names_to = "start_end", values_to="time_value") %>%
        mutate(
            coord_desc=paste(cens,start_end,sep="_"),
            point_char=case_match(coord_desc,
                "right_start" ~ ">", # "\u25BB", # right facing triangle
                "right_end" ~ NA,
                "left_start" ~ NA,
                "left_end" ~ "<", # "\u25C5", # left facing triangle
                "none_start" ~ NA,
                "none_end" ~ "X", # "\u00D7",  # x
                "interval_start" ~ ">", #"\u25BB", # right facing triangle
                "interval_end" ~  "<" #"\u25C5" # left facing triangle
              )
        ) 

    list(ds=ds, ds_points=ds_points)
}


# 
#     if(! has_name(ds,"treatments")){  # No treatment variable
#         # points layer
#         points_layer = do.call(geom_text,c(
#                     list( 
#                         data=ds_points,
#                         mapping=aes(x = time_value, y=ids, label=point_shape,
#                                   angle=point_angle),
#                         inherit.aes=F, show.legend=F
#                     ),
#                     extra_args_points
#                     )
#                 )
#     
#         # line segment
#         line_segment = do.call(geom_segment,c(
#                     list( 
#                         data=ds,
#                         mapping=aes(x=line_start, xend = end, y=ids, linetype=cens), 
#                         inherit.aes=F
#                     ),
#                     extra_args_line
#                     )
#                 )
#         
#     } else{ # with treatments
#         # points layer
#         points_layer = do.call(geom_text,c(
#                     list( 
#                         data=ds_points,
#                         mapping=aes(x = time_value, y=ids, label=point_shape,
#                                   angle=point_angle, color=treatments),
#                         inherit.aes=F, show.legend=F
#                     ),
#                     extra_args_points
#                     )
#                 )
#     
#         # line segment
#         line_segment = do.call(geom_segment,c(
#                     list( 
#                         data=ds,
#                         mapping=aes(x=line_start, xend = end, y=ids, linetype=cens, color=treatments), 
#                         inherit.aes=F
#                     ),
#                     extra_args_line
#                     )
#                 )
