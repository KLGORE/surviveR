#' Create a Weibull Plot
#'
#' @description
#' Create a Weibull Plot Using the Tidyverse Grammar-of-Graphics
#' 
#'
#' The `geom_weibull` function utilizes [survival::survfit] to generate a
#' Weibull plot for the provided survival data.  As discussed in chapter 6 of 
#' \emph{Statistical Methods for Reliability Data (2nd edition)} by Meeker, Escobar, 
#' and Pascual (2022), data that is well-modeled by the Weibull distribution should yield a linear
#' log(-log(S(t)) vs. log(t) relationship, where \eqn{t} represents time and 
#' \eqn{S(t)} represents the nonparametric survival probability estimate.
#'
#' The [surviveR::geom_weibull] function color-codes data points by the
#' treatment variable.  It assigns data point characters
#' according to the strata variable.
#'
#' \bold{Note on additional non-aesthetics arguments:} \cr
#' Users may customize graphical attributes of the data points 
#' and corresponding line fit(s).  Options include the following:
#' 
#'    * Weibull Points: color, size, alpha \cr
#'      
#'    * Line fit: line.color, linewidth, linetype, line.alpha  \cr
#'        * \emph{line.color}: Color of  fit line(s). Must be a string.
#'        * \emph{linewidth}: Line width of fit line(s).
#'        * \emph{linetype}: Line type of the fit line(s) ("dotted", "dashed", "solid", etc).
#'        * \emph{line.alpha}: Opacity of fit line(s).  0 = transparent.  1 = fully opaque.
#'
#'
#' @param mapping Aesthetics mapping with the following inputs: \cr
#'    * time: time column \cr
#'    * time2: optional secondary time column (e.g. time2 or event/status column specified in Surv() arguments) \cr
#'    * treatments: optional column or vector corresponding to the treatment variable.  Aliased arguments include treatment, trt, and trts \cr
#'    * strata: optional column or vector corresponding to the stratifying variable  \cr
#' @param ... Other graphical arguments pertaining to the primary failure/survival curve (color, linetype, linewidth, etc) \cr
#' @param line.fit TRUE or FALSE.  Indicates whether a line should be plotted for each subgroup.  Default = TRUE.
#' @param data The reference dataset. May be inherited from the plot data or a previous surviveR layer data.
#' @param inherit.aes TRUE or FALSE. TRUE inherits arguments from earlier ggplot calls.  Default = TRUE.
#'
#' @export
#' @import survival
#' @import broom
#' @import dplyr
#' @import ggplot2
#' @import rlang
#' @import stringr
#' @import tidyr
#' @import scales
#' @import forcats
#' @import utils
#' @importFrom dplyr filter
#'
#' @examples
#'library(ggplot2); library(dplyr)
#'
#'# Univariate Weibull plot (no treatment or strata variables)
#'mfg %>% filter(device_type == "A") %>%
#'     ggplot() +
#'     geom_weibull(aes(time = ttf, time2 = status))
#'
#'
#'# Weibull plot with treatment variable
#'mfg %>% filter(device_type == "A") %>%
#'     ggplot() +
#'     geom_weibull(aes(time = ttf, time2 = status, 
#'         treatments = mfg_location))
#'
#'
#'# Weibull plot with no line fit
#'mfg %>% filter(device_type == "A") %>%
#'     ggplot() +
#'     geom_weibull(aes(time = ttf, time2 = status, 
#'         treatments = mfg_location), line.fit = FALSE)
#'
#'
#'# Weibull plot with treatment and strata variables
#'mfg %>% ggplot() +
#'     geom_weibull(aes(time = ttf, time2 = status, 
#'         treatments = device_type, strata = mfg_location)) +
#'     facet_grid(.~mfg_location, scale = "free_x")
#'

geom_weibull = function(mapping = NULL, ..., line.fit = TRUE, data = NULL, inherit.aes = TRUE){
    extras = list2(...)
    # Assigns all our data as attributes of a string so that we can access it later
    structure(
        "",
        class = "surviveR_plot",
        fn = create_weibull_plot,
        mapping = mapping,
        extras = enquos(...),
        linetype = extras[["linetype"]],
        linewidth = extras[["linewidth"]],
        line.color = extras[["line.color"]],
        line.alpha = extras[["line.alpha"]],
        inherit.aes = inherit.aes,
        line.fit = line.fit,  # T/F
        env = rlang::caller_env(),
        name = "geom_weibull",
        data = data
    )
}



# ------------------------------------------------------------------------------
# PUTTING EVERYTHING TOGETHER
# ------------------------------------------------------------------------------

create_weibull_plot = function(args, plot){
    args = km.survreg.coxph_wrangle_args(args, plot)
    args = wrangle_weibull_args(args)  # wrangle remaining args that are specific to geom_weibull
    # plot$surviveR_inherit = args$surviveR_inherit
    args$survfit = survfit(args$formula, data = args$ds)
    add_weibull_layers(args, plot)
}

# ------------------------------------------------------------------------------
# WRANGLE WEIBULL PLOT POINT AND LINE ARGUMENTS
# ------------------------------------------------------------------------------

wrangle_weibull_args=function(args){

    # ---------------------------------------------------------------------
    # POINT ARGS: COLOR, FILL, SHAPE, ALPHA, SIZE
    # ---------------------------------------------------------------------

    # SHAPE (for strata)
    args$extras$shape=eval_tidy(args$extras$shape)

    if(! is.null(args$extras$shape)){
        if(length(args$extras$shape) != 1){
            stop("ERROR: shape argument must be of length 1.")
        } else if(is_string(args$extras$shape)){
            args$extras$shape=as.character(args$extras$shape)
        } else if(is.numeric(args$extras$shape)){
            args$extras$shape = args$extras$shape  # numeric plot character
        } else{
            stop(paste("Invalid input for shape:", args$extras$shape))
        }
    }

    # SIZE
    if(is.null(args$extras$size)){
        args$extras$size = 1 # default
    }

    args$point_extra_args=list2(
        color=args$extras$color,
        fill=args$extras$fill,
        shape=args$extras$shape,
        size=args$extras$size,
        alpha=args$extras$alpha
    )

    args$point_extra_args = Filter(Negate(is.null), args$point_extra_args)

    # ---------------------------------------------------------------------
    # LINE ARGS: LINE.COLOR, LINEWIDTH, LINETYPE, LINEALPHA <-not currently an option
    # ---------------------------------------------------------------------

    # LINE.FIT
    if(! is.null(args$line.fit)){
        if(length(args$line.fit) != 1){
            stop("ERROR: line.fit argument must be of length 1.")
        } else if(is.na(args$line.fit) | args$line.fit==F){
            args$line.fit = F
        } else if(args$line.fit==T){
            args$line.fit = T
        } else{
            args$line.fit = F
            warning("Invalid argument for line.fit.  Will assume line.fit=F instead.")
        }
    } else{
        args$line.fit = T
    }

    if(args$line.fit==T){

        # LINE ARGS
        args$weibull_line_extra_args=list2(
            color=args$extras$line.color,
            linetype=args$extras$linetype,
            linewidth=args$extras$linewidth,
            alpha=args$extras$line.alpha
        )

        args$weibull_line_extra_args = Filter(Negate(is.null), args$weibull_line_extra_args)

    } else{
        args$weibull_line_extra_args=NULL
    }

    args
}

# ------------------------------------------------------------------------------
# CREATING THE GGPLOT OBJECT
# ------------------------------------------------------------------------------

add_weibull_layers = function(args, plot){

    var_args=as.list(args$mapping)
    Treatment = as_quosure(var_args$treatments)
    Strata = as_quosure(var_args$strata)
    ds = km_create_ds(args) %>% filter(time > 0)

    # Note: add error message if any time<=0 cases exist.
    extra_args_points = Filter(Negate(is.null), args$point_extra_args)
    extra_args_line = Filter(Negate(is.null), args$weibull_line_extra_args)

    # PRIMARY POINTS & LINE FIT LAYERS (if specified)

    legend_correction = NULL
    line_fit = NULL

    # Note: treatment/strata cases are hyperspecified b/c ggplot had a hard
    # time getting the grouping correct
    if(!quo_is_null(Treatment) & !quo_is_null(Strata)){  # treatment & strata
        weibull_points = do.call(geom_point,c(
                list(
                    data = ds,
                    mapping=aes(x = log(time), y=log(-log(prob)),
                        color = !!Treatment, shape = !!Strata,
                        # group=interaction(!!Treatment, !!Strata)
                    )
                ),
                extra_args_points
                )
            )
        if(args$line.fit==T){
            line_fit=do.call(geom_smooth,c(
                list(
                    data = ds,
                    mapping=aes(x = log(time), y=log(-log(prob)),
                        color=!!Treatment, linetype=!!Strata,
                        # group=interaction(!!Treatment, !!Strata)
                    ),
                    method="lm", formula = y ~ x, se=FALSE
                ),
                extra_args_line
                )
            )
            legend_correction = guides(
                shape = guide_legend(override.aes = list(linetype = 0, size = 1.5)),
                color = guide_legend(override.aes = list(shape = NA))
            )
        }
    } else if(!quo_is_null(Treatment) & quo_is_null(Strata)){  # treatment only
        weibull_points = do.call(geom_point,c(
                list(
                    data = ds,
                    mapping=aes(x = log(time), y=log(-log(prob)),
                    color = !!Treatment, group=!!Treatment)
                ),
                extra_args_points
                )
            )

        if(args$line.fit==T){
            line_fit=do.call(geom_smooth,c(
                list(
                    data = ds,
                    mapping=aes(x = log(time), y=log(-log(prob)),
                        color=!!Treatment, group=!!Treatment),
                    method="lm", formula = y ~ x, se=FALSE
                ),
                extra_args_line
                )
            )
        }
    } else if(quo_is_null(Treatment) & !quo_is_null(Strata)){  # strata only
        weibull_points = do.call(geom_point,c(
                list(
                    data = ds,
                    mapping=aes(x = log(time), y=log(-log(prob)),
                    shape = !!Strata, group=!!Strata)
                ),
                extra_args_points
                )
            )

        if(args$line.fit==T){
            line_fit=do.call(geom_smooth,c(
                list(
                    data = ds,
                    mapping=aes(x = log(time), y=log(-log(prob)),
                        linetype=!!Strata, group=!!Strata),
                    method="lm", formula = y ~ x, se=FALSE
                ),
                extra_args_line
                )
            )
        }
    } else{ # no treatment or strata
        weibull_points = do.call(geom_point,c(
                list( data = ds,
                      mapping=aes(x = log(time), y=log(-log(prob)))
                ),
                extra_args_points
                )
            )

        if(args$line.fit==T){
            line_fit=do.call(geom_smooth,c(
                list(
                    data = ds,
                    mapping=aes(x = log(time), y=log(-log(prob))),
                    method="lm", formula = y ~ x, se=FALSE
                ),
                extra_args_line
                )
            )
        }
    }

    #Create the plot and return it
    plot + weibull_points + line_fit + legend_correction +
        labs(
            x="log(time)",
            y="log[-log(S(t)]",
            title="Weibull Plot"
        )
}
