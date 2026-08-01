#' Fit a Cox proportional Hazards Regression Model
#'
#' Utilizes tidyverse syntax to visualize Cox proportional hazards predictions.
#'
#' @param mapping Aesthetics mapping with the following inputs:
#'    * time: time column \cr
#'    * time2: optional secondary time column (e.g. time2 or event/status column specified in Surv() arguments) \cr
#'    * treatments: optional column or vector corresponding to the treatment variable.  Aliased arguments include treatment, trt, and trts. \cr
#'    * strata: optional column or vector corresponding to the stratifying variable.   \cr
#' @param failure TRUE or FALSE (default).  Values of TRUE will result in a failure curve being plotted.
#' @param ... Other non-aes graphical arguments pertaining to the primary failure/survival curve (color, linetype, linewidth, etc).
#' @param conf.int Confidence level for displayed confidence intervals. Must be a value between 0 and 1. Values of FALSE, NULL, or NA will result in no confidence intervals being plotted.
#' @param conf.fill Default = NULL, which indicates that confidence intervals will be color-coded by treatment. Other accepted values include the following:
#' * String of color name (ex: "black")
#' * TRUE indicates the color-coding fill color by treatment.
#' * Values of FALSE, NA, and "transparent" may be used to omit shading.
#' @param conf.linetype Line type of confidence interval lines ("dotted", "dashed", "solid", etc).
#' @param conf.linewidth Line width of confidence interval lines.
#' @param conf.color Color of confidence interval lines. Value provided must be a string.
#' @param conf.alpha Opacity value between 0 and 1.  0 = transparent.  1 = fully opaque.
#' @param data The reference dataset. May be inherited from the plot data or a previous surviveR layer data.
#' @param inherit.aes TRUE or FALSE. TRUE inherits arguments from earlier ggplot calls.  Default=TRUE.
#'
#' @details
#' This function uses the coxph function in the survival package to visualize Cox Proportional Hazards (CPH) survival curves within the Tidyverse ecosystem.
#' The Cox proportional hazards regression model is a versatile semiparametric method used to model survival data.  It applies to cases in which hazards can be modeled as follows:
#'
#' \deqn{ h(t) = h_0(t) \exp (b_1 x_1 + b_2 x_2 + \ldots )}
#'
#' upon estimation of the model coefficients, the above formula can be reexpressed in terms of the survival probability.  This expression is given by
#'
#' \deqn{ S(t) = S_0(t)^{\exp(\beta)} }
#' \deqn{ \log[S(t)] = \exp(\beta) \log[S_0(t)] }
#'
#' Full details of this model are found in the foundational "Regression Models and Life-Tables" paper by D.R. Cox (1972).
#' Note: Though this function accepts strata in its arguments, the CPH predictions are identical to that of the model in which the stratifying variable is a treatment instead.  Specifying strata only impacts the line type.
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
#' @import rlang
#' @import utils
#' @importFrom dplyr filter
#' @importFrom stats as.formula
#' @importFrom stats predict
#' @importFrom stats qnorm
#' @importFrom stats terms
#' 
#' @seealso
#' Note: the [surviveR::geom_coxph2] function allows for an input of a survival formula rather than specifying elements through the aesthetics mapping. \cr
#'
#' @examples
#'library(ggplot2); library(dplyr)
#'
#'# CoxPH survival plot with treatment variable
#'mfg %>% filter(device_type == "A") %>%
#'     ggplot() +
#'     geom_coxph(aes(time = ttf, time2 = status, 
#'         treatments = mfg_location), conf.int = 0.90)
#'
#'
#'# CoxPH failure plot with treatment variable
#'mfg %>% filter(device_type == "A") %>%
#'     ggplot() +
#'     geom_coxph(aes(time = ttf, time2 = status, 
#'         treatments = mfg_location), failure = TRUE)
#'
#'
#'# CoxPH survival plot with treatment and strata variables
#'mfg %>% 
#'     ggplot() +
#'     geom_coxph(aes(time = ttf, time2 = status, 
#'         treatments = device_type, strata = mfg_location)) +
#'     facet_grid(.~mfg_location)
#'
#'
#'# Overlaying KM curve on CoxPH fit
#'mfg %>% 
#'     ggplot() +
#'     geom_coxph(aes(time = ttf, time2 = status, 
#'         treatments = device_type, strata = mfg_location)) +
#'     geom_km(color = "black", conf.int = FALSE) +
#'     facet_grid(.~mfg_location)
#'
#'# Note: By default, geom_km, geom_coxph, and geom_survreg 
#'# inherit the formula and failure variables.
#'
#' 
geom_coxph = function(mapping = NULL, ..., conf.int = 0.95, failure = FALSE, data = NULL, inherit.aes = TRUE){
    extras = list2(...)
    structure(
        "",
        class = "surviveR_plot",
        fn = create_coxph_plot,
        mapping = mapping,
        extras = enquos(...),
        # conf.color = extras[["conf.color"]],
        # conf.linetype = extras[["conf.linetype"]],
        # conf.linewidth = extras[["conf.linewidth"]],
        # conf.fill = extras[["conf.fill"]],
        # conf.alpha = extras[["conf.alpha"]],
        data = data,
        inherit.aes = inherit.aes,
        conf.int = conf.int,
        failure = failure,
        env = rlang::caller_env(),
        name = "geom_coxph"
    )
}

#' Fit a Cox proportional Hazards Regression Model
#'
#' Utilizes tidyverse syntax to visualize Cox proportional hazards predictions.
#' @param formula A formula of the form you would input into [survival::coxph]
#' @param failure TRUE or FALSE (default).  Values of TRUE will result in a failure curve being plotted.
#' @param ... Other non-aes graphical arguments pertaining to the primary failure/survival curve (color, linetype, linewidth, etc).
#' @param conf.int Confidence level for displayed confidence intervals. Must be a value between 0 and 1. Values of FALSE, NULL, or NA will result in no confidence intervals being plotted.
#' @param conf.fill Default = NULL, which indicates that confidence intervals will be color-coded by treatment. Other accepted values include the following:
#' * String of color name (ex: "black")
#' * TRUE indicates the color-coding fill color by treatment.
#' * Values of FALSE, NA, and "transparent" may be used to omit shading.
#' @param conf.linetype Line type of confidence interval lines ("dotted", "dashed", "solid", etc).
#' @param conf.linewidth Line width of confidence interval lines.
#' @param conf.color Color of confidence interval lines. Value provided must be a string.
#' @param conf.alpha The opacity of the confidence interval fill. Values of 0 and 1 correspond to full and no transparency, respectively.
#' @param data The reference dataset. May be inherited from the plot data or a previous surviveR layer data.
#' @param inherit.aes TRUE or FALSE. TRUE inherits arguments from earlier ggplot calls.  Default = TRUE.
#'
#' @details
#' For description of the functionality, see the main version [surviveR::geom_coxph]
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
#' @import rlang
#' @import utils
#' @importFrom dplyr filter
#'
#' @examples
#'library(ggplot2); library(dplyr)
#'
#'# CoxPH survival plot with treatment variable
#'mfg %>% filter(device_type == "A") %>%
#'     ggplot() +
#'     geom_coxph2(
#'         formula = Surv(ttf,status) ~ mfg_location, 
#'         conf.int = 0.90)
#'
#'# ^ equivalent to
#'# mfg %>% filter(device_type == "A") %>%
#'#      ggplot() +
#'#      geom_coxph(aes(time = ttf, time2 = status, 
#'#           treatments = mfg_location), conf.int = 0.90)
#'
#'
#'# CoxPH failure plot with treatment variable
#'mfg %>% filter(device_type == "A") %>%
#'     ggplot() +
#'     geom_coxph2(
#'         formula = Surv(ttf,status) ~ mfg_location, 
#'         failure = TRUE)
#'
#'# ^ equivalent to
#'# mfg %>% filter(device_type == "A") %>%
#'#      ggplot() +
#'#      geom_coxph2(
#'#           formula = Surv(ttf,status) ~ mfg_location, 
#'#           failure = TRUE)
#'
#'
#'# CoxPH survival plot with treatment and strata variables
#'mfg %>% 
#'     ggplot() +
#'     geom_coxph2(formula = Surv(ttf, status) ~ 
#'         device_type + strata(mfg_location)) +
#'     facet_grid(.~mfg_location)
#'
#'# ^ equivalent to
#'# mfg %>% 
#'#      ggplot() +
#'#      geom_coxph(aes(time = ttf, time2 = status, 
#'#          treatments = device_type, strata = mfg_location)) +
#'#      facet_grid(.~mfg_location)
#'
#'
#'# Overlaying KM curve on CoxPH fit
#'mfg %>% 
#'     ggplot() +
#'     geom_coxph2(formula = Surv(ttf,status) ~ 
#'         device_type + strata(mfg_location)) +
#'     geom_km2(color = "black", conf.int = FALSE) +
#'     facet_grid(.~mfg_location)
#'
#'# Note: By default, geom_km2, geom_coxph2, and 
#'# geom_survreg2 inherit the formula and failure variables.
#'



geom_coxph2 = function(formula = NULL, ..., conf.int = 0.95, failure = FALSE, data = NULL, inherit.aes = TRUE){
    extras = list2(...)
    structure(
        "",
        class = "surviveR_plot",
        fn = create_coxph_plot,
        formula = formula,
        extras = enquos(...),
        # conf.color = extras[["conf.color"]],
        # conf.linetype = extras[["conf.linetype"]],
        # conf.linewidth = extras[["conf.linewidth"]],
        # conf.fill = extras[["conf.fill"]],
        # conf.alpha = extras[["conf.alpha"]],
        data = data,
        inherit.aes = inherit.aes,
        conf.int = conf.int,
        failure = failure,
        env = rlang::caller_env(),
        name = "geom_coxph2"
    )
}

# ------------------------------------------------------------------------------
# PUTTING EVERYTHING TOGETHER
# ------------------------------------------------------------------------------

create_coxph_plot = function(args, plot){
    if (!is.null(args$formula)){
        args = km.survreg.coxph_wrangle_formula(args)
    }
    args = km.survreg.coxph_wrangle_args(args, plot)

    plot$surviveR_inherit = args$surviveR_inherit
    args$coxph = coxph(args$formula, data = args$ds)
    add_coxph_layers(args, plot)
}

# ------------------------------------------------------------------------------
# CREATING THE GGPLOT OBJECT
# ------------------------------------------------------------------------------

add_coxph_layers = function(args, plot){
    ds=coxph_create_ds(args)
    conf_ds=coxph_conf_stepwise(ds,args)

    var_args=as.list(args$mapping)
    Treatment = as_quosure(var_args$treatments)
    Strata = as_quosure(var_args$strata)

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
            list(
                data = conf_ds %>% filter(!is.na(prob)),
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
            list(
                data = conf_ds %>% filter(!is.na(prob)),
                mapping=aes(x = time, y=prob, color = !!Treatment,
                            group = !!Treatment)
            ),
            extra_args_primary
            )
        )

        confints = do.call(geom_ribbon,c(
            list(
                data=conf_ds,
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
            list(
                data = conf_ds %>% filter(!is.na(prob)),
                mapping=aes(x = time, y=prob, linetype = !!Strata,
                            group = !!Strata)
            ),
            extra_args_primary
            )
        )

        confints = do.call(geom_ribbon,c(
            list(
                data=conf_ds,
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
            list(
                data = conf_ds %>% filter(!is.na(prob)),
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

    # Create the plot and return it
    plot +
        confints + primary_lines +
        labs(
            x="Time (t)",
            y=ifelse(args$failure==T, "F(t)", "S(t)")
        ) +
        y_axis_formatting
}



# ------------------------------------------------------------------------------
# CREATING THE COXPH PREDICTION DATASET (FOR THE PRIMARY LINE)
# ------------------------------------------------------------------------------

coxph_create_ds = function(args){

    var_args=as.list(args$mapping)
    Treatment = as_quosure(var_args$treatments)
    Strata = as_quosure(var_args$strata)

    # can't use se.fit=T if not treatment or strata
    if(rlang::quo_is_null(Treatment) & rlang::quo_is_null(Strata)){
        pred_object=predict(args$coxph, type="survival")
        ds=tibble(
            args$ds,
            SE=NA,
            prob=as.numeric(pred_object)
        )
    } else{
        pred_object=predict(args$coxph, type="survival", se.fit = T)
        ds=tibble(
            args$ds,
            SE=pred_object$se.fit,
            prob=pred_object$fit
        )
    }
    # Will add (0,1) to all treatment/strata combinations in the coxph_conf_stepwise function

    # calculating the lower and upper confidence intervals
    if(as.numeric(args$conf.int)>0){
        ds=ds %>%
            mutate(
                lower=pmax(0,prob - qnorm(1-(1-args$conf.int)/2) * SE),
                upper=pmin(1,prob + qnorm(1-(1-args$conf.int)/2) * SE)
            )
    } else{  # handle conf.int=FALSE case
        ds=ds %>%
            mutate(
                lower=NA,
                upper=NA
            )
    }
    ds
}

# ------------------------------------------------------------------------------
# CREATING A DATASET FOR STEPWISE CONFIDENCE INTERVALS (GEOM_RIBBON)
# ------------------------------------------------------------------------------

coxph_conf_stepwise=function(ds, args){

    conf_ds=list()

    var_args=as.list(args$mapping)
    Treatment = as_quosure(var_args$treatments)
    Strata = as_quosure(var_args$strata)

    if(! rlang::quo_is_null(Treatment)){
        treatment_levels=ds %>% mutate(treatment=factor(!!Treatment)) %>% pull(treatment) %>% levels()
        # print(treatment_levels)
    }
    if(! rlang::quo_is_null(Strata)){
        strata_levels=ds %>% mutate(strata=factor(!!Strata)) %>% pull(strata) %>% levels()
        # print(strata_levels)
    }


    if( !rlang::quo_is_null(Treatment) & !rlang::quo_is_null(Strata) ){ # Treatment and Strata
        ds = ds %>% arrange(!!Treatment, !!Strata, time)
        for(i in treatment_levels){
            for(j in strata_levels){
                ds_i=ds %>%
                    filter(!!Treatment==i, #     !!sym(rlang::as_name(Treatment))==i,
                           !!Strata==j)  #!!sym(rlang::as_name(Strata))==j)
                conf_ds[[paste(i,j)]]=tibble(
                    treatment = i,
                    strata = j,
                    time=ds_i[-nrow(ds_i),"time"] %>% pull(),
                    prob=NA,
                    #prob=ds_i[-1,"prob"] %>% pull(),
                    lower=ds_i[-1,"lower"] %>% pull(),
                    upper=ds_i[-1,"upper"] %>% pull()
                )
                t0_coord=ds_i %>%
                    slice_min(time) %>%  # grab the first time>0 and add the confidence intervals
                    mutate(
                        treatment=i,
                        strata=j,
                        prob=1,
                        upper=pmin(1,upper),
                        time=0
                    ) %>%
                    select(treatment,strata,time,prob,lower,upper)

                conf_ds[[paste(i,j)]]=bind_rows(conf_ds[[paste(i,j)]],t0_coord)
            }
        }
        conf_ds=bind_rows(conf_ds) %>%
            mutate(
                treatment=factor(treatment,levels=treatment_levels),
                strata=factor(strata,levels=strata_levels),
            ) %>%
            rename(!!Treatment := treatment,!!Strata := strata) %>%
            bind_rows(ds %>% select(!!Treatment,!!Strata, time, prob, lower, upper)) %>%
            arrange(!!Treatment, !!Strata, time, desc(prob))
    } else if( !rlang::quo_is_null(Treatment) & rlang::quo_is_null(Strata) ){ # Treatment only
        ds = ds %>% arrange(!!Treatment, time)

        for(i in treatment_levels){
            ds_i=ds %>% filter(!!Treatment==i)
            conf_ds[[i]]=tibble(
                treatment=i,
                time=ds_i[-nrow(ds_i),"time"] %>% pull(),
                #prob=ds_i[-1,"prob"] %>% pull(),
                prob=NA,
                lower=ds_i[-1,"lower"] %>% pull(),
                upper=ds_i[-1,"upper"] %>% pull()
            )

            t0_coord=ds_i %>%
                slice_min(time) %>%  # grab the first time>0 and add the confidence intervals
                mutate(
                    treatment=i,
                    prob=1,
                    upper=pmin(1,upper),
                    time=0
                ) %>%
                select(treatment,time,prob,lower,upper)

            conf_ds[[i]]=bind_rows(conf_ds[[i]],t0_coord)
        }
        conf_ds=bind_rows(conf_ds) %>%
            mutate(
                treatment=factor(treatment, levels=treatment_levels)
            ) %>%
            rename(!!Treatment := treatment) %>%
            bind_rows(ds %>% select(!!Treatment, time, prob, lower, upper)) %>%
            arrange(!!Treatment, time, desc(prob))

    } else if(rlang::quo_is_null(Treatment) & ! rlang::quo_is_null(Strata)){ # Strata only
        ds = ds %>% arrange(!!Strata, time)
        for(i in strata_levels){
            ds_i=ds %>% filter(!!Strata==i)
            conf_ds[[i]]=tibble(
                strata=i,
                time=ds_i[- nrow(ds_i),"time"] %>% pull(),
                prob=NA,
                lower=ds_i[-1,"lower"] %>% pull(),
                upper=ds_i[-1,"upper"] %>% pull()
            )
            t0_coord=ds_i %>%
                slice_min(time) %>%  # grab the first time>0 and add the confidence intervals
                mutate(
                    strata=i,
                    prob=1,
                    upper=pmin(1,upper),
                    time=0
                ) %>% select(strata, time, prob, lower,upper)

            conf_ds[[i]]=bind_rows(conf_ds[[i]],t0_coord)
        }

        conf_ds=bind_rows(conf_ds) %>%
            mutate(
                strata=factor(strata,levels=strata_levels),
            ) %>%
            rename(!!Strata := strata) %>%
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
                prob=1,
                time=0,
                upper=pmin(1,upper)
            ) %>% select(time, prob, lower,upper)
        conf_ds=bind_rows(conf_ds, t0_coord, ds) %>%
            select(time,prob,lower,upper) %>%
            arrange(time,desc(prob))
    }
    conf_ds
}
