#' Create a Kaplan-Meier Curve Using the Tidyverse Grammar-of-Graphics
#'
#' @description Create Kaplan-Meier curve using aesthetic mapping arguments
#' 
#' @details
#' The `geom_km` function generates KM curves that display nonparametric
#' survival estimates S(t) versus time (t).  Users may plot cumulative failure curves 
#' in lieu of the survival probability by using the `failure=TRUE` argument.  For more 
#' statistical details on the Kaplan-Meier methodology, please see chapter 3 of
#' \emph{Statistical Methods for Reliability Data (2nd edition)} by Meeker, Escobar,
#' and Pascual.
#' 
#' \bold{Note on additional non-aesthetics arguments:} \cr
#' Users may customize graphical attributes of the primary KM curve(s) 
#' and confidence intervals.  Options include the following:
#' 
#'    * KM Curve(s): color, linewidth, linetype, alpha \cr
#'      
#'    * Confidence Interval(s): conf.color, conf.fill, conf.linewidth, conf.linetype, conf.alpha  \cr
#'        * \emph{conf.color}: Color of confidence interval lines. Value provided must be a string.
#'        * \emph{conf.fill}: Default = NULL, which indicates that confidence intervals will be 
#'              color-coded by treatment. Other accepted values include the following:
#'            * String of color name (ex: "black")
#'            * TRUE indicates the color-coding fill color by treatment.
#'            * Values of FALSE, NA, and "transparent" may be used to omit shading.
#'        * \emph{conf.linewidth}: Line width of confidence interval lines.
#'        * \emph{conf.linetype}: Line type of confidence interval lines ("dotted", "dashed", "solid", etc).
#'        * \emph{conf.alpha}: Opacity value between 0 and 1.  0 = transparent.  1 = fully opaque.
#'        
#' @seealso The [surviveR::geom_km2], which accepts a survival formula in lieu of variable 
#' specification using aesthetics mappings. \cr
#' 
#' 
#' @param mapping Aesthetics mapping with the following inputs:
#'    * time: time column \cr
#'    * time2: optional secondary time column (e.g. time2 or event/status column specified in Surv() arguments) \cr
#'    * treatments: optional column or vector corresponding to the treatment variable.  Aliased arguments include treatment, trt, and trts. \cr
#'    * strata: optional column or vector corresponding to the stratifying variable.   \cr
#' @param ... Non-aes graphical arguments pertaining to the primary failure/survival curve (color, linetype, linewidth, etc).
#' @param conf.int Confidence level for displayed confidence intervals. Must be a value between 0 and 1. Values of FALSE, NULL, or NA will result in no confidence intervals being plotted.
#' @param failure TRUE or FALSE (default). Values of TRUE will result in a failure curve being plotted, FALSE will result in a survival curve.
#' @param data The reference dataset. May be inherited from the plot data or a previous surviveR layer data.
#' @param inherit.aes TRUE or FALSE. TRUE inherits arguments from earlier ggplot calls.  Default = TRUE.
#'
#' @export
#' @import tibble
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
#'
#' @examples
#'library(ggplot2); library(dplyr)
#'
#'# Univariate survival plot (no treatment or strata variables)
#'mfg %>%
#'     ggplot() +
#'     geom_km(aes(time = ttf, time2 = status), 
#'         conf.int = 0.90)
#'
#'
#'# Univariate failure plot (no treatment or strata variables)
#'mfg %>%
#'     ggplot() +
#'     geom_km(aes(time = ttf, time2 = status), 
#'         failure = TRUE)
#'
#'
#'# Survival plot with treatment variable
#'mfg %>% filter(device_type == "A") %>%
#'     ggplot() +
#'     geom_km(aes(time = ttf, time2 = status, 
#'         treatments = mfg_location), conf.int = 0.90)
#'
#'
#'# Survival plot with treatment and strata variables
#'mfg %>% 
#'     ggplot() +
#'     geom_km(aes(time = ttf, time2 = status, 
#'         treatments = device_type, strata = mfg_location)) +
#'     facet_grid(.~mfg_location)
#'
#'
#'# Overriding color & removing confidence intervals (ideal for overlaying fits)
#' mfg %>% 
#'     ggplot() +
#'     geom_km(aes(time = ttf, time2 = status, 
#'        treatments = mfg_location),
#'        conf.int = FALSE, color="black")
#'
#'
#'# Overlaying KM curve on semiparametric & parametric fits
#'mfg %>% 
#'     ggplot() +
#'     geom_coxph(aes(time = ttf, time2 = status, 
#'         treatments = device_type, strata = mfg_location)) +
#'     geom_km(color = "black", conf.int = FALSE) +
#'     facet_grid(.~mfg_location)
#'
#'mfg %>% 
#'     ggplot() +
#'     geom_survreg(aes(time = ttf, time2 = status, 
#'         treatments = device_type, strata = mfg_location)) +
#'     geom_km(color = "black", conf.int = FALSE) +
#'     facet_grid(.~mfg_location)
#'
#'mfg %>% 
#'     ggplot() +
#'     geom_survreg(aes(time = ttf, time2 = status, 
#'         treatments = device_type, strata = mfg_location)) +
#'     geom_coxph() +
#'     geom_km(color = "black", conf.int = FALSE) +
#'     facet_grid(.~mfg_location)
#'
#'# Note: By default, geom_km2, geom_coxph2, and geom_survreg2 
#'# inherit the formula and failure variables.
#'

geom_km = function(mapping = NULL, ..., conf.int = 0.95, failure = FALSE, data = NULL, inherit.aes = T){
    extras = list2(...)
    # Assigns all our data as attributes of a string so that we can access it later
    structure(
        "",
        class = "surviveR_plot",
        fn = create_km_plot,
        mapping = mapping,
        extras = enquos(...),
        conf.int = conf.int,
        failure = failure,
        data = data,
        inherit.aes = inherit.aes,
        env = rlang::caller_env(),
        name = "geom_km"
    )
}

#' Create a Kaplan-Meier Curve Using the Tidyverse Grammar-of-Graphics
#'
#' @description Create Kaplan-Meier curve by providing a survival formula
#' 
#' @details
#' The `geom_km2` function generates KM curves that display nonparametric
#' survival estimates S(t) versus time (t).  Unlike its companion function `geom_km`,
#' `geom_km2` allows users to input a survival formula instead of specifying
#' the aesthetics mapping.  Cumulative failure curves can be plotting
#' in lieu of the survival probability by using the `failure=TRUE` argument.  For more 
#' statistical details on the Kaplan-Meier methodology, please see chapter 3 of
#' \emph{Statistical Methods for Reliability Data (2nd edition)} by Meeker, Escobar,
#' and Pascual.
#' 
#' \bold{Note on additional non-aesthetics arguments:} \cr
#' Users may customize graphical attributes of the primary KM curve(s) 
#' and confidence intervals.  Options include the following:
#' 
#'    * KM Curve(s): color, linewidth, linetype, alpha \cr
#'      
#'    * Confidence Interval(s): conf.color, conf.fill, conf.linewidth, conf.linetype, conf.alpha  \cr
#'        * \emph{conf.color}: Color of confidence interval lines. Value provided must be a string.
#'        * \emph{conf.fill}: Default = NULL, which indicates that confidence intervals will be 
#'              color-coded by treatment. Other accepted values include the following:
#'            * String of color name (ex: "black")
#'            * TRUE indicates the color-coding fill color by treatment.
#'            * Values of FALSE, NA, and "transparent" may be used to omit shading.
#'        * \emph{conf.linewidth}: Line width of confidence interval lines.
#'        * \emph{conf.linetype}: Line type of confidence interval lines ("dotted", "dashed", "solid", etc).
#'        * \emph{conf.alpha}: Opacity value between 0 and 1.  0 = transparent.  1 = fully opaque.
#'        
#' @seealso [surviveR::geom_km]. \cr
#' 
#' @param formula A formula compatible with survfit(), or a survfit object
#' @param failure TRUE or FALSE (default). Values of TRUE will result in a failure curve being plotted, FALSE will result in a survival curve.
#' @param ... Non-aes graphical arguments pertaining to the primary failure/survival curve (color, linetype, linewidth, etc).
#' @param conf.int Confidence level for displayed confidence intervals. Must be a value between 0 and 1. Values of FALSE, NULL, or NA will result in no confidence intervals being plotted.
#' @param data The reference dataset. May be inherited from the plot data or a previous surviveR layer data.
#' @param inherit.aes TRUE or FALSE. TRUE inherits arguments from earlier ggplot calls.  Default = TRUE.
#'
#'
#' @export
#' @import survival
#' @import tibble
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
#'# Univariate survival plot (no treatment or strata variables)
#'mfg %>%
#'     ggplot() +
#'     geom_km2(formula = Surv(ttf,status) ~ 1, conf.int = 0.90)
#'
#'# ^ equivalent to
#'# mfg %>%
#'#      ggplot() +
#'#      geom_km(aes(time = ttf, time2 = status), 
#'#           conf.int = 0.90)
#'
#'
#'# Univariate failure plot (no treatment or strata variables)
#'mfg %>%
#'     ggplot() +
#'     geom_km2(
#'         formula = Surv(ttf,status) ~ 1, 
#'         failure = TRUE)
#'
#'# ^ equivalent to
#'# mfg %>%
#'#      ggplot() +
#'#      geom_km(aes(time = ttf, time2 = status), failure = TRUE)
#'
#'
#'# Survival plot with treatment variable
#'mfg %>% filter(device_type == "A") %>%
#'     ggplot() +
#'     geom_km2(
#'         formula = Surv(ttf,status) ~ mfg_location, 
#'         conf.int = 0.90)
#'
#'# ^ equivalent to
#'# mfg %>% filter(device_type == "A") %>%
#'#      ggplot() +
#'#      geom_km(aes(time = ttf, time2 = status, 
#'#           treatments = mfg_location), conf.int = 0.90)
#'
#'
#'# Survival plot with treatment and strata variables
#'mfg %>% 
#'     ggplot() +
#'     geom_km2(
#'         formula = Surv(ttf, status) ~ device_type + 
#'                   strata(mfg_location)) +
#'     facet_grid(.~mfg_location)
#'
#'# ^ equivalent to
#'# mfg %>% 
#'#      ggplot() +
#'#      geom_km(aes(time = ttf, time2 = status, 
#'#           treatments = device_type, strata = mfg_location)) +
#'#      facet_grid(.~mfg_location)
#'
#'
#'# Overlaying KM curve on semiparametric & parametric fits
#'mfg %>% 
#'     ggplot() +
#'     geom_coxph2(formula = Surv(ttf, status) ~ 
#'                   device_type + strata(mfg_location)) +
#'     geom_km2(color = "black", conf.int = FALSE) +
#'     facet_grid(.~mfg_location)
#'
#'mfg %>% 
#'     ggplot() +
#'     geom_survreg2(formula = Surv(ttf, status) ~ 
#'         device_type + strata(mfg_location)) +
#'     geom_km2(color = "black", conf.int = FALSE) +
#'     facet_grid(.~mfg_location)
#'
#'mfg %>% 
#'     ggplot() +
#'     geom_survreg2(formula = Surv(ttf, status) ~ 
#'         device_type + strata(mfg_location)) +
#'     geom_coxph2() +
#'     geom_km2(color = "black", conf.int = FALSE) +
#'     facet_grid(.~mfg_location)

#'# Note: By default, geom_km2, geom_coxph2, and 
#'# geom_survreg2 inherit the formula and failure variables.
#'


geom_km2 = function(formula = NULL, ..., conf.int = 0.95, failure = FALSE, data = NULL, inherit.aes = T){
    extras = list2(...)
    #Check if they piped in
    if (!is.null(formula)){
        if (!inherits(formula, "formula")){
            if (rlang::quo_get_expr(enquo(formula)) == "."){
                if (tryCatch(length(unlist(rlang::eval_tidy(enquo(formula)))) > 1, error = function(e){FALSE})){
                    stop("piped in to geom_km2 instead of using +")
                } else{
                    stop("Invalid input for formula argument")
                }
            }
        }
    }
    #Assigns all our data as attributes of a string so that we can access it later
    structure(
        "Creates a Kaplan-Meier plot",
        class = "surviveR_plot",
        fn = create_km_plot,
        formula = formula,
        extras = enquos(...),
        conf.int = conf.int,
        failure = failure,
        data = data,
        inherit.aes = inherit.aes,
        env = rlang::caller_env(),
        name = "geom_km2"
    )
}


# ------------------------------------------------------------------------------
# PUTTING EVERYTHING TOGETHER
# ------------------------------------------------------------------------------

create_km_plot = function(args, plot){
    if (!is.null(args$formula)){
        args = km.survreg.coxph_wrangle_formula(args)
    }
    args = km.survreg.coxph_wrangle_args(args, plot)
    plot$surviveR_inherit = args$surviveR_inherit
    args$survfit = survfit(args$formula, data = args$ds, conf.int = args$conf.int)
    add_km_layers(args, plot)
}


# ------------------------------------------------------------------------------
# CREATING THE GGPLOT OBJECT
# ------------------------------------------------------------------------------

add_km_layers = function(args, plot){
    ds = km_create_ds(args)
    conf_ds = km_conf_stepwise(ds, args)  # create the dataset needed for geom_ribbon to plot stepwise lines

    var_args=as.list(args$mapping)
    Treatment = as_quosure(var_args$treatments)
    Strata = as_quosure(var_args$strata)


    if (eval_tidy(args$failure)==T){
        conf.low=conf_ds$lower
        conf.high=conf_ds$upper

        conf_ds = conf_ds %>%
            mutate(
                prob=1-prob,
                lower=1-conf.high,
                upper=1-conf.low
            )

        rm(conf.low); rm(conf.high)
        
        conf_ds = conf_ds %>% arrange(prob)
    }

    # CREATING THE PRIMARY LINE & CONFIDENCE INTERVAL RIBBON

    # non-aes settings for primary stepwise line
    extra_args_primary = args$line_extra_args  # this is handled in ggplot_add.surviveR_plot.R
    extra_args_primary = Filter(Negate(is.null), extra_args_primary)
    conf_extra_args = args$conf_int_args  # this is handled in ggplot_add.surviveR_plot.R
    conf_extra_args = Filter(Negate(is.null), conf_extra_args)

    # Treatments and Strata
    if( (!quo_is_null(Treatment)) & (!quo_is_null(Strata)) ){
        # print("Treatments and Strata")

        primary_lines = do.call(geom_step,c(
                list(         data = conf_ds %>% filter(!is.na(prob)),
                              mapping=aes(x = time, y=prob, color = !!Treatment, linetype = !!Strata,
                                          group = interaction(!!Treatment, !!Strata))
                ),
                extra_args_primary
                )
            )

        confints = do.call(geom_ribbon,c(
                list(
                        data=conf_ds,
                        mapping=aes(x = time, ymin = lower, ymax = upper,
                                    fill = !!Treatment, color=!!Treatment, linetype=!!Strata,
                                    group = interaction(!!Treatment, !!Strata))
                    ),
                    conf_extra_args
                )
            )

    # Treatments only
    } else if( !quo_is_null(Treatment) & quo_is_null(Strata) ){

        # print("Treatments only")

        primary_lines = do.call(geom_step,c(
                list(         data = conf_ds %>% filter(!is.na(prob)),
                              mapping=aes(x = time, y=prob, color = !!Treatment,
                                          group = !!Treatment)
                ),
                extra_args_primary
                )
            )

        confints = do.call(geom_ribbon,c(
                list(   data=conf_ds,
                        mapping=aes(x = time, ymin = lower, ymax = upper,
                                    fill = !!Treatment, color=!!Treatment,
                                    group = !!Treatment)
                    ),
                    conf_extra_args
                )
            )

    # Strata only
    } else if( (quo_is_null(Treatment)) & (! quo_is_null(Strata)) ){

        # print("Strata only")

        primary_lines = do.call(geom_step,c(
                list(   data = conf_ds %>% filter(!is.na(prob)),
                        mapping=aes(x = time, y=prob, linetype = !!Strata,
                                    group = !!Strata)
                ),
                extra_args_primary
                )
            )

        confints = do.call(geom_ribbon,c(
                list(   data=conf_ds,
                        mapping=aes(x = time, ymin = lower, ymax = upper,
                                    linetype=!!Strata, group = !!Strata)
                    ),
                    conf_extra_args
                )
            )
    # No Treatment or Strata
    } else{

        # print("No Treatments or Strata")

        primary_lines = do.call(geom_step,c(
                list(         data = conf_ds %>% filter(!is.na(prob)),
                              mapping=aes(x = time, y=prob)
                ),
                extra_args_primary
                )
            )
        confints = do.call(geom_ribbon,c(
                list(
                    data=conf_ds,
                    mapping=aes(x = time, ymin = lower, ymax = upper)
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

    #Create the plot and return it
    plot + confints + primary_lines +
        labs(
            x="Time (t)",
            y=ifelse(eval_tidy(args$failure)==T, "F(t)", "S(t)")
        ) +
        y_axis_formatting
}


km_create_ds = function(args){
    # Get a KM curve in tibble format using the broom::tidy function

    var_args=as.list(args$mapping)
    Treatment = as_quosure(var_args$treatments)
    Strata = as_quosure(var_args$strata)

    ds=tidy(args$survfit) %>%
        rename(
            prob=estimate, lower=conf.low, upper=conf.high
        )

    if( quo_is_null(Strata) & quo_is_null(Treatment)){  # no treatments or strata
        start_at_0=tibble(
            time=0, prob=1, lower=max(ds$lower,na.rm=T), upper=max(ds$upper,na.rm=T)
        )

        ds=bind_rows(start_at_0,ds)

    } else if(!rlang::quo_is_null(Treatment) & !rlang::quo_is_null(Strata)){  # treatment & strata
        ds=ds %>%
            separate_wider_delim(strata,delim=paste(", strata(",as_name(Strata),")=",sep=""), names=c("treatment","strata"), too_few = "align_start") %>%
            separate_wider_delim(treatment, "=", names = c(NA,"treatment"), too_few = "align_end", too_many="drop") %>%
            separate_wider_delim(strata, "=", names = c(NA,"strata"), too_few = "align_end", too_many="drop")

      start_at_0=ds %>%
          group_by(treatment,strata) %>%
          summarize(
              time=0,
              prob=1,
              lower=max(lower,na.rm=T),
              upper=max(upper,na.rm=T)
          )

      ds=bind_rows(start_at_0,ds) %>%
          mutate(
              treatment=trimws(treatment),
              strata=trimws(strata)
          ) %>%
          rename(!!Treatment := treatment, !!Strata := strata)

    } else if(rlang::quo_is_null(Treatment) & !rlang::quo_is_null(Strata)) {  # if there's just a stratifying variable
      ds=ds %>%
          mutate(strata=str_remove(strata,fixed(")"))) %>%  # remove the last right parentheses
          mutate(strata=str_remove_all(strata,fixed(paste("strata(",as_name(Strata),")",sep="")))) %>%
          separate_wider_delim(strata, "=", names = c(NA,"strata"), too_many="drop")

      start_at_0=ds %>%
          group_by(strata) %>%
          summarize(
              time=0,
              prob=1,
              lower=max(lower,na.rm=T),
              upper=max(upper,na.rm=T)
          )

      ds=bind_rows(start_at_0,ds) %>%
          mutate(
              strata=trimws(strata)
          ) %>%
          rename(!!Strata := strata)

    } else{  # treatments only
        ds=ds %>%
            separate_wider_delim(strata,delim=fixed("="), names=c(NA,"treatment"), too_many = "drop",too_few = "align_end")

        start_at_0=ds %>%
            group_by(treatment) %>%
            summarize(
                time=0,
                prob=1,
                lower=max(lower,na.rm=T),
                upper=max(upper,na.rm=T)
            )
        ds=bind_rows(start_at_0,ds) %>%
          mutate(
              treatment=trimws(treatment)
          ) %>%
          rename(!!Treatment := treatment)
    }
    ds
}

# ------------------------------------------------------------------------------
# CREATING A DATASET FOR STEPWISE CONFIDENCE INTERVALS (GEOM_RIBBON)
# ------------------------------------------------------------------------------

km_conf_stepwise=function(ds, args){

    conf_ds=list()

    var_args=as.list(args$mapping)
    Treatment = as_quosure(var_args$treatments)
    Strata = as_quosure(var_args$strata)

    if(! rlang::quo_is_null(Treatment)){
        treatment_levels=ds %>% mutate(!!Treatment := factor(!!Treatment)) %>% pull(!!Treatment) %>% levels()
    }
    if(! rlang::quo_is_null(Strata)){
        strata_levels=ds %>% mutate(!!Strata := factor(!!Strata)) %>% pull(!!Strata) %>% levels()
    }


    if( !rlang::quo_is_null(Treatment) & !rlang::quo_is_null(Strata) ){ # Treatment and Strata
        ds = ds %>% arrange(!!Treatment, !!Strata, time)
        for(i in treatment_levels){
            for(j in strata_levels){
                ds_i=ds %>%
                    filter(!!Treatment==i, !!Strata==j)  #!!sym(rlang::as_name(Strata))==j)
                conf_ds[[paste(i,j)]]=tibble(
                    !!Treatment := i,
                    !!Strata := j,
                    time=ds_i[-nrow(ds_i),"time"] %>% pull(),
                    prob=NA,
                    lower=ds_i[-1,"lower"] %>% pull(),
                    upper=ds_i[-1,"upper"] %>% pull()
                )
                t0_coord=ds_i %>%
                    slice_min(time) %>%  # grab the first time>0 and add the confidence intervals
                    mutate(
                        !!Treatment := i,
                        !!Strata := j,
                        prob=NA,
                        upper=pmin(1,upper),
                        time=0
                    ) %>%
                    select(!!Treatment,!!Strata,time,prob,lower,upper)

                conf_ds[[paste(i,j)]]=bind_rows(conf_ds[[paste(i,j)]],t0_coord)
            }
        }
        conf_ds=bind_rows(conf_ds) %>%
            mutate(
                !!Treatment := factor(!!Treatment,levels=treatment_levels),
                !!Strata := factor(!!Strata,levels=strata_levels),
            ) %>%
            bind_rows(ds %>% select(!!Treatment,!!Strata, time, prob, lower, upper)) %>%
            arrange(!!Treatment, !!Strata, time, desc(prob))

    } else if( !rlang::quo_is_null(Treatment) & rlang::quo_is_null(Strata) ){ # Treatment only
        ds = ds %>% arrange(!!Treatment, time)

        for(i in treatment_levels){
            ds_i=ds %>% filter(!!Treatment==i)
            conf_ds[[i]]=tibble(
                !!Treatment := i,
                time=ds_i[-nrow(ds_i),"time"] %>% pull(),
                prob=NA,
                lower=ds_i[-1,"lower"] %>% pull(),
                upper=ds_i[-1,"upper"] %>% pull()
            )

            t0_coord=ds_i %>%
                slice_min(time) %>%  # grab the first time>0 and add the confidence intervals
                mutate(
                    !!Treatment := i,
                    prob=NA,
                    lower=pmax(0,lower),
                    upper=pmin(1,upper),
                    time=0
                ) %>%
                select(!!Treatment,time,prob,lower,upper)

            conf_ds[[i]]=bind_rows(conf_ds[[i]],t0_coord)
        }
        conf_ds=bind_rows(conf_ds) %>%
            mutate(
                !!Treatment := factor(!!Treatment, levels=treatment_levels)
            ) %>%
            bind_rows(ds %>% select(!!Treatment, time, prob, lower, upper)) %>%
            arrange(!!Treatment, time, desc(prob))

    } else if(rlang::quo_is_null(Treatment) & ! rlang::quo_is_null(Strata)){ # Strata only
        ds = ds %>% arrange(!!Strata, time)
        for(i in strata_levels){
            ds_i=ds %>% filter(!!Strata==i)
            conf_ds[[i]]=tibble(
                !!Strata := i,
                time=ds_i[- nrow(ds_i),"time"] %>% pull(),
                prob=NA,
                lower=ds_i[-1,"lower"] %>% pull(),
                upper=ds_i[-1,"upper"] %>% pull()
            )
            t0_coord=ds_i %>%
                slice_min(time) %>%  # grab the first time>0 and add the confidence intervals
                mutate(
                    !!Strata := i,
                    prob=NA,
                    upper=pmin(1,upper),
                    lower=pmax(0,lower),
                    time=0
                ) %>% select(!!Strata, time, prob, lower,upper)

            conf_ds[[i]]=bind_rows(conf_ds[[i]],t0_coord)
        }

        conf_ds=bind_rows(conf_ds) %>%
            mutate(
                !!Strata := factor(!!Strata,levels=strata_levels),
            ) %>%
            bind_rows(ds %>% select(!!Strata, time, prob, lower, upper)) %>%
            arrange(!!Strata, time, desc(prob))

    } else{  # no strata or treatments
        ds = ds %>% arrange(time)

        conf_ds=bind_cols(
            ds[- nrow(ds),] %>% select(time),
            ds[-1,] %>% select(prob,lower,upper) %>%
              mutate(prob=NA)
        )
        t0_coord=conf_ds %>%
            slice_min(time) %>%  # grab the first time>0 and add the confidence intervals
            mutate(
                prob=NA,
                time=0,
                lower=pmax(0,lower),
                upper=pmin(1,upper)
            ) %>% select(time, prob, lower,upper)
        conf_ds=bind_rows(conf_ds, t0_coord, ds) %>%
            select(time,prob,lower,upper) %>%
            arrange(time,desc(prob))
    }
    conf_ds
}