#' Create Mean Cumulative Function
#'
#' This function creates a Mean Cumulative Function (MCF) plot from the provided data.
#'
#' @param mapping Aesthetics mapping with the following inputs:
#'     * time: Event times (must be numeric). 
#'     * ids: Unit ID column. Alias arguments include id, Id, Ids, ID, IDS. 
#'     * treatments: A single treatment or a vector of treatments. Alias arguments include treatment, trt, or trts.
#'     * strata: Grouping variable, typically used for faceting. 
#' @param ... Other arguments pertaining to the MCF curves. Examples include, color, linetype, linewidth, etc. 
#' @param conf.int Confidence level for displayed confidence intervals. Must be a value between 0 and 1. Values of FALSE, NULL, or NA will result in no confidence intervals being plotted.
#' @param conf.fill Default = NULL, which indicates that confidence intervals will be color-coded by treatment. Other accepted values include the following:
#' * String of color name (ex: "black")
#' * TRUE indicates the color-coding fill color by treatment.
#' * Values of FALSE, NA, and "transparent" may be used to omit shading.
#' @param conf.linetype Line type of confidence interval lines ("dotted", "dashed", "solid", etc).
#' @param conf.linewidth Line width of the confidence interval lines. 
#' @param conf.color The line color of confidence interval lines. Value provided must be a string.
#' @param conf.alpha The opacity of the confidence interval fill. Values of 0 and 1 correspond to full and no transparency, respectively.
#' @param data The data to use, if not provided in the ggplot call. 
#' @param inherit.aes TRUE or FALSE. TRUE inherits arguments from earlier ggplot calls.  Default = TRUE.
#'
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
#' @importFrom dplyr lag
#' @importFrom stats as.formula
#' @importFrom stats predict
#' @importFrom stats qnorm
#' @importFrom stats terms
#'
#' @details
#' The mean cumulative function (MCF) is a nonparametric survival statistic used to model the average number of events for a repairable 
#' system.  Suppose a system consists of \eqn{N} total system units (hereby termed "units").  Furthermore, let \eqn{i} index the units, and let \eqn{k} index all event times \eqn{t}.  
#' Vector \eqn{(t_1, t_2, ..., t_n)} represents the collective event times of all units in the system.  
#' 
#' Each raw observation minimally consists of the following: 
#'  * The unit ID: \eqn{i}
#'  * The event time: \eqn{t}
#'  * The risk set status for unit \eqn{i} at time \eqn{t}: \eqn{\delta_i(t)}
#'    where values of 1 indicate that unit \eqn{i} is still active (i.e. in the risk set) and values of 0 indicate that unit \eqn{i} is no longer active (i.e. NOT in the risk set)
#'  * The event status of unit \eqn{i} at time \eqn{t}: \eqn{d_i(t_k)}
#'    where values of 1 indicate that an event occurred for unit \eqn{i} at time \eqn{t_k} and values of 0 indicate that an event did NOT occur for unit \eqn{i} at time \eqn{t_k}.
#' 
#' Letting \eqn{d_.(t_k)} denote the total number of events that happen at time \eqn{t_k} and \eqn{\delta_.(t_k)} 
#' represent the risk set at time \eqn{t_k}, the MCF estimate at time \eqn{t_j} may be expressed as 
#' 
#' \deqn{\sum_{k=1}^n \frac{d_.(t_k)}{\delta_.(t_k)} = \sum_{k=1}^n \bar{d}(t_k)}
#' 
#' where \eqn{\bar{d}(t_k)} is the mean cumulative number of events per unit at time \eqn{t_k}.  
#' 
#' The confidence intervals geom_mcf displays are Wald estimates, which leverage asymptotic normality assumptions in their calculation.  
#' The nonparametric standard deviation for the MCF at time \eqn{t_k} is estimated using the following expression:
#' 
#' \deqn{\sqrt{ \sum_{i=1}^n \left\{ \sum_{k=1}^j  \frac{\delta_i(t_k)}{\delta_.(t_k)} \times [d_i(t_k) - \bar{d}(t_k)] \right\} ^2}}
#' Note: The statistical validity (read: bias) of the above MCF statistics hinges on a fairly strong assumption: the recurrence rate of the events
#' is constant for all units.  This assumption can be violated in common scenarios, so one must check diagnostics to verify the appropriateness of the metric.
#' 
#' Many of the details and notation of this page were heavily adapted from "Statistical Methods
#' for Reliability Data: Second Edition" (2022) by William Q. Meeker, Luis A. Escobar, and Francis G. Pascual.  
#' For more statistical details on the MCF methodology, please reference pages 45-47 and 522-528 of this fantastic book.
#' 
#' 
#' @examples
#'library(ggplot2); library(dplyr)
#'
#'# Univariate MCF plot (no treatment or strata variables)
#'device_repair %>% filter(device_type == "A") %>%
#'    ggplot() + 
#'    geom_mcf(aes(time = repair_time, ids = device_id))
#'
#'
#'# MCF plot with treatment variable
#'device_repair %>% 
#'    ggplot() + 
#'    geom_mcf(aes(time = repair_time, ids = device_id, 
#'       treatment = mfg_location))
#'    
#'    
#'# MCF plot with strata variable 
#'device_repair %>% 
#'    ggplot() + 
#'    geom_mcf(aes(time = repair_time, ids = device_id, 
#'       strata = device_type))
#'
#'
#'# MCF plot with treatment and strata variables
#'device_repair %>% 
#'    ggplot() + 
#'    geom_mcf(aes(time = repair_time, ids = device_id, 
#'       treatment = mfg_location, strata = device_type)) +
#'    facet_grid(.~device_type)
#'
#'# MCF plot with additional confidence interval arguments
#'device_repair %>% filter(device_type == "A") %>%
#'    ggplot() + 
#'    geom_mcf(aes(time = repair_time, ids = device_id), 
#'           conf.int = 0.95, conf.fill = "gray30", 
#'           conf.linetype = "dotted", conf.linewidth = 0.5, 
#'           conf.color = "darkred", conf.alpha = 0.15)
#'

geom_mcf = function(mapping = NULL, ..., conf.int = 0.95, data = NULL, inherit.aes = TRUE) {
    # This function simply packages the inputs that are then dealt with when ggplot
    # tries to add an object of the "mcf_plot" class to a ggplot object
    extras = list2(...)
    structure(
        "Creates a Mean Cumulative Function (MCF) plot",
        class = "surviveR_plot",
        fn = create_mcf_plot,
        mapping = mapping,
        extras = enquos(...),
        # color = extras[["color"]],
        # linetype = extras[["linetype"]],
        # linewidth = extras[["linewidth"]],
        # fill = extras[["fill"]],
        # alpha = extras[["alpha"]],
        # conf.int = extras[["conf.int"]],
        # conf.color = extras[["conf.color"]],
        # conf.linetype = extras[["conf.linetype"]],
        # conf.linewidth = extras[["conf.linewidth"]],
        # conf.fill = extras[["conf.fill"]],
        # conf.alpha = extras[["conf.alpha"]],
        data = data,
        inherit.aes = inherit.aes,
        env = rlang::caller_env(),
        name = "geom_mcf"
    )
}


create_mcf_plot = function(args, plot){
    args = mcf_wrangle_args(args, plot)
    #Create the plots from the data
    add_layers_mcf(args, plot)
}


add_layers_mcf = function(args, plot){
    # Create dataset for plot generation
    ds = mcf_create_ds(args)

    var_args = as.list(args$mapping)
    Treatment = as_quosure(var_args$treatments)
    Strata = as_quosure(var_args$strata)

    conf.int = args$conf.int
  
    # PRIMARY LINES
    # non-aes settings for primary stepwise line
    extra_args_primary = args$line_extra_args
    extra_args_primary = Filter(Negate(is.null), extra_args_primary)
    
    if((!rlang::quo_is_null(Treatment)) & !rlang::quo_is_null(Strata) ){ # Treatments and Strata
        primary_lines = do.call(geom_step,c(
            list(
                data = ds%>%filter(!is.na(mcf)),
                mapping = aes(x = time, y = mcf, color = !!Treatment, linetype = !!Strata)
            ),
            extra_args_primary
        )
        )
    } else if(! rlang::quo_is_null(Treatment) & rlang::quo_is_null(Strata)){  # Treatments only
        primary_lines = do.call(geom_step,c(
            list(
                data = ds%>%filter(!is.na(mcf)),
                mapping = aes(x = time, y = mcf, color = !!Treatment)
            ),
            extra_args_primary
        )
        )
    } else if(rlang::quo_is_null(Treatment) & ! rlang::quo_is_null(Strata)){  # strata only 
        primary_lines = do.call(geom_step,c(
            list(
                data = ds%>%filter(!is.na(mcf)),
                mapping = aes(x = time, y = mcf, color = !!Strata, group = !!Strata)
            ),
            extra_args_primary
        )
        )
    } else{
        primary_lines = do.call(geom_step,c(
            list(
                data = ds%>%filter(!is.na(mcf)),
                mapping = aes(x = time, y = mcf)
            ),
            extra_args_primary
        )
        )
    }
    
    ## CONFIDENCE INTERVAL RIBBONS
    conf_extra_args = args$conf_int_args  # this is handled in ggplot_add.surviveR_plot.R
    conf_extra_args = Filter(Negate(is.null), conf_extra_args)
    #print(conf_extra_args)
    
    if(! rlang::quo_is_null(Treatment) & ! rlang::quo_is_null(Strata) ){ # Treatments and Strata
        confints = do.call(geom_ribbon,c(
            list(
                data=ds,
                mapping = aes(x = time, ymin = lower, ymax = upper,
                              fill = !!Treatment, linetype = !!Strata, color = !!Treatment)
            ),
            conf_extra_args
        )
        )
    } else if( (! rlang::quo_is_null(Treatment)) & rlang::quo_is_null(Strata)){  # Treatments only
        confints = do.call(geom_ribbon,c(
            list(
                data=ds,
                mapping = aes(x = time, ymin = lower, ymax = upper,
                              fill = !!Treatment, color = !!Treatment)
            ),
            conf_extra_args
        )
        )
    } else if(rlang::quo_is_null(Treatment) & ! rlang::quo_is_null(Strata)){  # strata only 
        #print("strata")
        confints = do.call(geom_ribbon,c(
            list(
                data=ds,
                mapping = aes(x = time, ymin = lower, ymax = upper, 
                              linetype = !!Strata)
            ),
            conf_extra_args
        )
        )
    } else{
        #print("none")
        confints = do.call(geom_ribbon,c(
            list(
                data=ds,
                mapping = aes(x = time, ymin = lower, ymax = upper)
            ),
            conf_extra_args
        )
        )
    }
  
    plot + 
        confints + primary_lines +
        labs(
            x = "Time (t)",
            y = "MCF"
        )
  
}



mcf_create_ds = function(args){
    # plotmapping other version here. all inheritance + datachecks
    var_args=as.list(args$mapping)
    Treatment = as_quosure(var_args$treatments)
    Strata = as_quosure(var_args$strata)
    
    
    #need some if rlang::quo_is_null(args$strata / trt)?
    the_data = args$ds
    time = the_data$time
    ids = the_data$ids
    
    treatments = the_data$treatments
    strata = the_data$strata

    the_list=list()
    
    # treatments..
    trt_strata = args$ds %>% distinct(treatments, strata)

    ds0 = tibble(time = time, ids = ids, treatments = treatments, strata = strata) %>%
        arrange(treatments, strata, time, ids)
    

    for (j in 1:nrow(trt_strata)){
        #print(j)
        the_trt = trt_strata$treatments[j]
        the_strata = trt_strata$strata[j]
        
        if (length(trt_strata) > 1){ # more than 1 combo exists, should filter to some extent
            ds = ds0 %>%
                filter(treatments == the_trt, 
                       strata == the_strata) # whatever the current strata/trt is
        } else{
            ds = ds0  # when no strata/treatment
        }

        # find last event time for each id
        time_vec=ds %>% distinct(time) %>% arrange(time) %>% pull()
        id_vec=ds %>% distinct(ids) %>% pull()
        
        last_event=ds %>%
            group_by(ids) %>%
            summarize(
                OOS_time=max(time,na.rm=T)
            )
        
        # create status column
        ds = ds%>%
            left_join(last_event,by=c("ids"="ids")) %>%
            mutate(status=ifelse(time==OOS_time,0,1))
        
        # tally all events at each unique time
        time_freq=ds %>%
            filter(status==1) %>%
            count(ids,time) %>%
            arrange(time)
        
        
        raw_data=expand_grid(ids=id_vec,time=time_vec) %>%
            arrange(time) %>%
            left_join(last_event,by=c("ids"="ids")) %>%
            left_join(time_freq,by=c("ids"="ids","time"="time")) %>%
            mutate(
                n=ifelse(is.na(n),0,n)
            )
        
        mcf_data=raw_data %>%
            group_by(time) %>%
            summarize(
                events=sum(n),
                risk_set=sum(OOS_time >= time),
                mean_i=events/risk_set
              ) %>%
            ungroup() %>%
            mutate(
                mcf=cumsum(mean_i)   # calculate mcf
            ) %>%
            select(-mean_i)
        
        
        # CONFIDENCE INTERVAL SECTION
        conf.int=args$conf.int
        #print(conf.int)

        var_ds_raw=raw_data %>%
            rename(events_id_time=n) %>%
            mutate(
                delta_id_time=ifelse(OOS_time>=time,1,0)
            ) %>%
            group_by(time) %>%
            mutate(
                mean_events_time=mean(events_id_time),
                risk_set_time=sum(delta_id_time)
            ) %>% ungroup()
        
        var_ds=tibble(time=time_vec,var_time=NA)
        
        for(k in 1:length(time_vec)){
            ds=var_ds_raw %>% filter(time<=time_vec[k])
            var_ds[k,"var_time"]=ds %>%
                mutate(
                    var_time_id=delta_id_time/risk_set_time*
                      (events_id_time - mean_events_time)
              ) %>%
              group_by(ids) %>%
              summarize(
                  var_total_id=sum(var_time_id)^2
              ) %>%
              summarize(
                  var_total_time=sum(var_total_id)
              ) %>%
              pull(var_total_time)
        }
        
        mcf_data = left_join(mcf_data,var_ds,by=c("time"="time")) %>%
            mutate(
                lower=mcf-qnorm(1-(1-conf.int)/2)*sqrt(var_time),
                upper=mcf+qnorm(1-(1-conf.int)/2)*sqrt(var_time)
            ) %>%
            mutate(lower=pmax(lower,0),
                   upper=pmax(upper,0))
        
        # time 0 coord first
        t0_coord = expand_grid(
            mcf = c(0,NA),
            time = 0,
            lower = 0,
            upper = 0
        )
        
        mcf_data = bind_rows(mcf_data,t0_coord) %>%
            arrange(time,mcf,lower)
        
        # the shift 
        mcf_conf = tibble(
            time = mcf_data[-nrow(mcf_data), "time"] %>% pull(),
            mcf = NA, 
            lower = mcf_data[-1,"lower"] %>% pull(),
            upper=mcf_data[-1,"upper"] %>% pull()
        )
        
        mcf_data = bind_rows(mcf_data, t0_coord, mcf_conf) %>%
          arrange(time,mcf,lower)

        # print(head(mcf_data,20))
        
        the_list[[j]]=tibble(treatments=the_trt,strata=the_strata,mcf_data)
    }
      
    finalds = bind_rows(the_list)
    
    if (! rlang::quo_is_null(Treatment)){
        finalds = finalds %>%
          rename(!!Treatment := treatments)   
    }
    if (! rlang::quo_is_null(Strata)){
          finalds = finalds %>%
            rename(!!Strata := strata)
    }
    
    finalds
}
