#' Create a parametric survival plot
#'
#' Create a parametric survival plot using aesthetic mapping arguments
#'
#' This function allows users to visualize parametric survival fits using
#' the ggplot Grammar of Graphics.  It leverages the flexsurvreg function in the [flexsurv::flexsurv-package]
#' package.  For more information on the distributions supported by geom_survreg and
#' geom_survreg2, please see the documentation for [flexsurv::flexsurvreg].  Note that
#' flexsurvreg (and, by extension, geom_survreg2) may run sluggishly with large datasets.
#'
#' @param mapping Aesthetics mapping with the following inputs: \cr
#'    * time: time column \cr
#'    * time2: optional secondary time column (e.g. time2 or event/status column specified in Surv() arguments) \cr
#'    * treatments: optional column or vector corresponding to the treatment variable.  Aliased arguments include treatment, trt, and trts. \cr
#'    * strata: optional column or vector corresponding to the stratifying variable.   \cr
#' @param ... Non-aes arguments pertaining to the primary failure/survival curve (color, linetype, linewidth, etc).
#' @param dist Distribution to be used in parametric survival fit.  Default value: "weibull".  Accepts all distributions accepted by [flexsurv::flexsurvreg], including
#' "genf","genf.orig" ,"gengamma", "gengamma.orig", "exp", "weibull", "weibullPH", "lnorm", "gamma", "gompertz", "llogis", "exponential", and "lognormal".  For a list of distributions supported by this function, please reference the documentation for [flexsurv::flexsurvreg].
#' @param failure TRUE or FALSE (default).  Values of TRUE will result in a failure curve being plotted.
#' @param conf.int Confidence level for displayed confidence intervals. Must be a value between 0 and 1. Values of FALSE, NULL, or NA will result in no confidence intervals being plotted.
#' @param conf.fill Default = NULL, which indicates that confidence intervals will be color-coded by treatment. Other accepted values include the following:
#' * String of color name (ex: "black")
#' * TRUE indicates the color-coding fill color by treatment.
#' * Values of FALSE, NA, and "transparent" may be used to omit shading.
#' @param conf.linetype Line type of confidence interval lines ("dotted", "dashed", "solid", etc).
#' @param conf.linewidth Line width of confidence interval lines.
#' @param conf.color Color of confidence interval lines. Value provided must be a string.
#' @param conf.alpha Opacity value between 0 and 1.  0 = transparent.  1 = fully opaque.
#' @param length.out Length of the time vector used in the prediction dataset being plotted.
#' @param xmax Maximum time value on x-axis.  Defaults to 1.15 times max(time) if exact or right-censored data and 1.15 times max(time2) for interval-censored data.
#' @param data The reference dataset. May be inherited from the plot data or a previous surviveR layer data.
#' @param inherit.aes TRUE or FALSE. TRUE inherits arguments from earlier ggplot calls.  Default = TRUE.
#'
#' @export
#' @import tidyverse
#' @import survival
#' @import broom
#' @import flexsurv
#'
#' @seealso
#' The alternate version that accepts formulas: [geom_survreg2] \cr
#' @examples
#'# Parametric survival plot with treatment variable (default: Weibull distribution)
#'mfg %>% filter(device_type == "A") %>%
#'     ggplot() +
#'     geom_survreg(aes(time = ttf, time2 = status, treatments = mfg_location), conf.int = 0.90)
#'
#'
#'# Lognormal survival fit with treatment variable
#'mfg %>% filter(device_type == "A") %>%
#'     ggplot() +
#'     geom_survreg(aes(time = ttf, time2 = status, treatments = mfg_location), dist = "lognormal")
#'
#'
#'# Parametric failure plot with treatment variable
#'mfg %>% filter(device_type == "A") %>%
#'     ggplot() +
#'     geom_survreg(aes(time = ttf, time2 = status, treatments = mfg_location), failure = T, conf.int = 0.90)
#'
#'
#'# Weibull fit with treatment and strata variables
#'mfg %>% 
#'     ggplot() +
#'     geom_survreg(aes(time = ttf, time2 = status, treatments = device_type, strata = mfg_location)) +
#'     facet_grid(.~mfg_location)
#'
#'
#'# Overlaying KM curve on top of parametric fit
#' mfg %>% filter(device_type == "A") %>%
#'     ggplot() +
#'     geom_survreg(aes(time = ttf, time2 = status, treatments = mfg_location)) +
#'     geom_km(conf.int = F)
#'
#'# Note: By default, geom_km, geom_coxph, and geom_survreg inherit the mapping (time, time2, treatments, strata) and failure variables.


geom_survreg = function(mapping = NULL, dist = "weibull", ..., conf.int = 0.95, failure = F, length.out = 1000, xmax = NULL, data = NULL, inherit.aes = T){
    extras = list2(...)
    structure(
        "",
        class = "surviveR_plot",
        fn = create_survreg_plot,
        mapping = mapping,
        extras = enquos(...),
        dist = dist,
        data = data,
        length.out = length.out,
        xmax = xmax,
        inherit.aes = inherit.aes,
        conf.int = conf.int,
        failure = failure,
        env = rlang::caller_env(),
        name = "geom_survreg"
    )
}

#' Create a parametric survival plot
#'
#' Create a parametric survival plot using formula-based arguments
#'
#' This function allows users to visualize parametric survival fits using
#' the ggplot Grammar of Graphics.  It leverages the flexsurvreg function in the [flexsurv::flexsurv-package]
#' package.  For more information on the distributions supported by geom_survreg and
#' geom_survreg2, please see the documentation for [flexsurv::flexsurvreg].  Note that
#' flexsurvreg (and, by extension, geom_survreg2) may run sluggishly with large datasets.
#'
#'
#' @param formula Survival formula
#' @param dist Distribution to be used in parametric survival fit.  Default value: "weibull".  Accepts all distributions accepted by [flexsurv::flexsurvreg], including
#' "genf","genf.orig" ,"gengamma", "gengamma.orig", "exp", "weibull", "weibullPH", "lnorm", "gamma", "gompertz", "llogis", "exponential", and "lognormal".  For a list of distributions supported by this function, please reference the documentation for [flexsurv::flexsurvreg].
#' @param failure TRUE or FALSE (default).  Values of TRUE will result in a failure curve being plotted.
#' @param ... Other graphical arguments pertaining to the primary failure/survival curve (color, linetype, linewidth, etc).
#' @param dist Distribution to be used in parametric survival fit.  Default value: "weibull".  Accepts all distributions accepted by [flexsurv::flexsurvreg], including
#' "genf","genf.orig" ,"gengamma", "gengamma.orig", "exp", "weibull", "weibullPH", "lnorm", "gamma", "gompertz", "llogis", "exponential", and "lognormal".  For a list of distributions supported by this function, please reference the documentation for [flexsurv::flexsurvreg].
#' @param conf.int Confidence level for displayed confidence intervals. Must be a value between 0 and 1. Values of FALSE, NULL, or NA will result in no confidence intervals being plotted.
#' @param conf.fill Default = NULL, which indicates that confidence intervals will be color-coded by treatment. Other accepted values include the following:
#' * String of color name (ex: "black")
#' * TRUE indicates the color-coding fill color by treatment.
#' * Values of FALSE, NA, and "transparent" may be used to omit shading.
#' @param length.out length of the time vector created in the prediction dataset that is plotted
#' @param xmax Maximum time value on x-axis.  Defaults to 1.15 times max(time) if exact or right-censored data and 1.15 times max(time2) for interval-censored data.
#' @param conf.linetype Line type of confidence interval lines ("dotted", "dashed", "solid", etc).
#' @param conf.linewidth Line width of confidence interval lines.
#' @param conf.color Color of confidence interval lines. Value provided must be a string.
#' @param conf.alpha The opacity of the confidence interval fill. Values of 0 and 1 correspond to full and no transparency, respectively.
#' @param data The reference dataset. May be inherited from the plot data or a previous surviveR layer data.
#' @param inherit.aes TRUE or FALSE. TRUE inherits arguments from earlier ggplot calls.  Default = TRUE.
#'
#' @export
#' @import tidyverse
#' @import survival
#' @import broom
#' @import flexsurv
#'
#' @examples
#'# Parametric survival plot with treatment variable (default: Weibull distribution)
#'mfg %>% filter(device_type == "A") %>%
#'     ggplot() +
#'     geom_survreg2(formula = Surv(ttf,status) ~ mfg_location, conf.int = 0.90)
#'
#'# ^ equivalent to
#'# mfg %>% filter(device_type == "A") %>%
#'#      ggplot() +
#'#      geom_survreg(aes(time = ttf, time2 = status, treatments = mfg_location), conf.int = 0.90)
#'
#'
#'# Lognormal survival fit with treatment variable
#'mfg %>% filter(device_type == "A") %>%
#'     ggplot() +
#'     geom_survreg2(formula = Surv(ttf,status) ~ mfg_location, dist = "lognormal")
#'
#'# ^ equivalent to
#'# mfg %>% filter(device_type == "A") %>%
#'#      ggplot() +
#'#      geom_survreg(aes(time = ttf, time2 = status, treatments = mfg_location), dist = "lognormal")
#'
#'
#'# Parametric failure plot with treatment variable and 90% confidence interval
#'mfg %>% filter(device_type == "A") %>%
#'     ggplot() +
#'     geom_survreg2(formula = Surv(ttf,status) ~ mfg_location, failure = T, conf.int = 0.90)
#'
#'# ^ equivalent to
#'# mfg %>% filter(device_type == "A") %>%
#'#      ggplot() +
#'#      geom_survreg(aes(time = ttf, time2 = status, treatments = mfg_location), failure = T, conf.int = 0.90)
#'
#'
#'# Weibull fit with treatment and strata variables
#'mfg %>% 
#'     ggplot() +
#'     geom_survreg2(formula = Surv(ttf,status) ~ treatments + strata(mfg_location))
#'     facet_grid(.~mfg_location)
#'
#'# ^ equivalent to
#'# mfg %>% 
#'       ggplot() +
#'#      geom_survreg(aes(time = ttf, time2 = status, treatments = device_type, strata = mfg_location)) +
#'#      facet_grid(.~mfg_location)
#'
#'
#'# Overlaying KM curve on top of parametric fit
#' mfg %>% filter(device_type == "A") %>%
#'     ggplot() +
#'     geom_survreg2(formula = Surv(ttf,status) ~ mfg_location)) +
#'     geom_km2(conf.int = F)
#'
#'# Note: By default, geom_km2, geom_coxph2, and geom_survreg2 inherit the formula and failure variables.

geom_survreg2 = function(formula = NULL, dist = "weibull", ..., conf.int = 0.95, failure = F, length.out = 1000, xmax = NULL, data = NULL, inherit.aes = T){
    extras = list2(...)
    structure(
        "",
        class = "surviveR_plot",
        fn = create_survreg_plot,
        formula = formula,
        extras = enquos(...),
        dist = dist,
        length.out = length.out,
        xmax = xmax,
        inherit.aes = inherit.aes,
        conf.int = conf.int,
        failure = failure,
        data = data,
        env = rlang::caller_env(),
        name = "geom_survreg2"
    )
}


# Reformatting the dist name for flexsurvreg
format_dist=function(dist){
    formatted_dist=switch(dist,
                          "exponential" = "exp",
                          "lognormal" = "lnorm",
                          "loglogistic" = "llogis",
                          dist
    )
    formatted_dist
}


create_survreg_plot = function(args, plot){
    # if it's a formula, it's from geom_survreg2.
    # => parse the formula first, and then run it through the wrangling function
    if (!is.null(args$formula)){
        args = km.survreg.coxph_wrangle_formula(args)
    }
    # once the formula has been parsed, run args through the regular wrangling function
    args = km.survreg.coxph_wrangle_args(args, plot)

    # data check for length.out
    if(is.null(args$length.out)){
        inform("length.out cannot be NULL.  Defaulting to length.out=1000.")
        args$length.out=1000
    } else if(is.numeric(args$length.out) & (args$length.out %% 1 == 0) & (args$length.out > 2)){
        args$length.out=args$length.out
    } else if(length(args$length.out) != 1){
        warning("length.out must have length 1.  Defaulting to length.out=1000")
    } else if(!is.numeric(args$length.out)){
        warning("length.out must be numeric.  Defaulting to length.out=1000.")
    } else{
        warning("Invalid value for length.out.  Defaulting to length.out=1000.")
        args$length.out=1000
    }

    # data check for dist
    if(is.null(args$dist)){
        args$dist="weibull"
    } else if(is.na(args$dist)){
        args$dist="weibull"
    } else if(is.character(args$dist) | is.factor(args$dist)){
        args$dist=format_dist(as.character(args$dist))
    } else{
        stop("invalid dist provided")
    }

    plot$surviveR_inherit = args$surviveR_inherit

    # Note: using flexsurvreg instead of survreg b/c it does pointwise CI's for the survival probs
    var_args=as.list(args$mapping)
    Strata = as_quosure(var_args$strata)
    # b/c flexsurvreg uses "shape" instead of "strata" in its formulas, make the switch before fitting the model
    if(!quo_is_null(Strata)){
        revised_formula=deparse1(args$formula)
        revised_formula=str_replace_all(revised_formula,fixed("strata("),fixed("shape("))
        args$formula = as.formula(revised_formula)
    }
    args$survreg = flexsurvreg(args$formula, data = args$ds, dist = args$dist)
    add_survreg_layers(args, plot)
}

# ------------------------------------------------------------------------------
# CREATING THE GGPLOT OBJECT
# ------------------------------------------------------------------------------

survreg_create_ds=function(args){
    var_args=as.list(args$mapping)
    Treatment = as_quosure(var_args$treatments)
    Strata = as_quosure(var_args$strata)

    mod=args$survreg
    time_vec=seq(10^(floor(log10(max(args$ds$time, na.rm=T)))-1),max(args$ds$time,na.rm=T),length.out=args$length.out)

    if(!rlang::quo_is_null(Treatment) & !rlang::quo_is_null(Strata)){  # Treatment & Strata
        newdata=args$ds %>%  distinct(!!Treatment, !!Strata)
        pred=predict(mod, newdata = newdata, type = "survival", t = time_vec,
                     conf.int=T, conf.level=args$conf.int)
        conf_ds_list=list()

        for(i in 1:nrow(newdata)){
            conf_ds_list[[i]]=bind_cols(
                !!Treatment := newdata[i,] %>% pull(!!Treatment),
                !!Strata := newdata[i,] %>% pull(!!Strata),
                pred[[".pred"]][[i]]
            )
        }

        conf_ds=bind_rows(conf_ds_list) %>%
            rename(
                time=`.eval_time`,
                prob=`.pred_survival`,
                lower=`.pred_lower`,
                upper=`.pred_upper`
            )

    } else if(!rlang::quo_is_null(Treatment) & rlang::quo_is_null(Strata)){  # Treatment only
        newdata=args$ds %>%  distinct(!!Treatment)
        pred=predict(mod, newdata = newdata, type = "survival", t = time_vec,
                     conf.int=T, conf.level=args$conf.int)
        conf_ds_list=list()

        for(i in 1:nrow(newdata)){
            conf_ds_list[[i]]=bind_cols(
                !!Treatment := newdata[i,] %>% pull(!!Treatment),
                pred[[".pred"]][[i]]
            )
        }

        conf_ds=bind_rows(conf_ds_list) %>%
            rename(
                time=`.eval_time`,
                prob=`.pred_survival`,
                lower=`.pred_lower`,
                upper=`.pred_upper`
            )

    } else if(rlang::quo_is_null(Treatment) & !rlang::quo_is_null(Strata)){ # Strata only
        newdata=args$ds %>%  distinct(!!Strata)
        pred=predict(mod, newdata = newdata, type = "survival", t = time_vec,
                     conf.int=T, conf.level=args$conf.int)
        conf_ds_list=list()

        for(i in 1:nrow(newdata)){
            conf_ds_list[[i]]=bind_cols(
                !!Strata := newdata[i,] %>% pull(!!Strata),
                pred[[".pred"]][[i]]
            )
        }

        conf_ds=bind_rows(conf_ds_list) %>%
            rename(
                time=`.eval_time`,
                prob=`.pred_survival`,
                lower=`.pred_lower`,
                upper=`.pred_upper`
            )

    } else{  # Neither treatment nor strata
        newdata=tibble(x=1) # dummy tibble
        pred=predict(mod, newdata = newdata, type = "survival", t = time_vec,
                     conf.int=T, conf.level=args$conf.int)

        conf_ds=pred[[".pred"]][[1]] %>%
            rename(
                time=`.eval_time`,
                prob=`.pred_survival`,
                lower=`.pred_lower`,
                upper=`.pred_upper`
            )
    }
    conf_ds
}


# ------------------------------------------------------------------------------
# CREATING THE GGPLOT OBJECT
# ------------------------------------------------------------------------------

add_survreg_layers = function(args, plot){
    var_args=as.list(args$mapping)
    Treatment = as_quosure(var_args$treatments)
    Strata = as_quosure(var_args$strata)

    conf_ds=survreg_create_ds(args)

    if (args$failure==TRUE){

        conf.low=conf_ds$lower
        conf.high=conf_ds$upper

        conf_ds = conf_ds %>%
            mutate(
                prob=1-prob,
                lower=1-conf.high,
                upper=1-conf.low
            )
        rm(conf.low); rm(conf.high)
    }


    ## GLOBAL GRAPHING ARGUMENTS (ONLY INCLUDING NON-NULL ARGS)
    conf_extra_args = args$conf_int_args
    conf_extra_args = Filter(Negate(is.null), conf_extra_args)
    primary_line_extra_args=args$line_extra_args
    primary_line_extra_args = Filter(Negate(is.null), primary_line_extra_args)

    ## CREATING THE CONF INT RIBBONS + PRIMARY LINES
    if(! rlang::quo_is_null(Treatment) & ! rlang::quo_is_null(Strata) ){ # Treatments and Strata
        # print("treatment & strata")
        primary_lines = do.call(geom_line,c(
            list(
                data = conf_ds,
                mapping = aes(x = time, y=prob, color = !!Treatment,
                              linetype = !!Strata,
                              group = interaction(!!Treatment, !!Strata))
            ),
            primary_line_extra_args
            )
        )

        confints = do.call(geom_ribbon,c(
            list(
                data=conf_ds,
                mapping = aes(x = time, ymin = lower, ymax = upper,
                      fill = !!Treatment, linetype = !!Strata, color=!!Treatment,
                      group = interaction(!!Treatment, !!Strata))
            ),
            conf_extra_args
            )
        )

    } else if(! rlang::quo_is_null(Treatment) & rlang::quo_is_null(Strata)){  # Treatments only
        # print("treatment only")
        primary_lines = do.call(geom_line,c(
            list(
                data = conf_ds,
                mapping = aes(x = time, y=prob, color = !!Treatment,
                              group=!!Treatment)
            ),
            primary_line_extra_args
            )
        )

        confints = do.call(geom_ribbon,c(
            list(
                data=conf_ds,
                mapping = aes(x = time, ymin = lower, ymax = upper,
                            fill = !!Treatment, color=!!Treatment,
                            group=!!Treatment)
            ),
            conf_extra_args
            )
        )
    } else if(rlang::quo_is_null(Treatment) & ! rlang::quo_is_null(Strata)){  # strata only
        # print("strata only")
        primary_lines = do.call(geom_line,c(
            list(
                data = conf_ds,
                mapping = aes(x = time, y=prob, linetype = !!Strata,
                              group = !!Strata)
            ),
            primary_line_extra_args
            )
        )

        confints = do.call(geom_ribbon,c(
            list(
                data=conf_ds,
                mapping = aes(x = time, ymin = lower, ymax = upper,
                              linetype = !!Strata, group = !!Strata)
            ),
            conf_extra_args
            )
        )
    } else{
        # print("neither treatment nor strata")
        primary_lines = do.call(geom_line,c(
            list(
                data = conf_ds,
                mapping = aes(x = time, y=prob)
            ),
            primary_line_extra_args
            )
        )

        confints = do.call(geom_ribbon,c(
            list(
                data=conf_ds,
                mapping = aes(x = time, ymin = lower, ymax = upper)
            ),
            conf_extra_args
            )
        )
    }

    if(is.null(plot$scales$get_scales("y"))){
        y_axis_formatting=scale_y_continuous(labels=scales::percent_format(), limits=c(-0.01,1.01))
    } else{
        y_axis_formatting=NULL
    }

    # Create the plot and return it
    plot +
        confints + primary_lines +
        labs(
            x="Time (t)",
            y=ifelse(args$failure==T, "F(t)", "S(t)")
        ) +
        y_axis_formatting
}


