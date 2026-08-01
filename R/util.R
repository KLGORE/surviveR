#' @importFrom ggplot2 ggplot_add
#' @export
#' 
ggplot_add.surviveR_plot = function(object, plot, object_name, ...){
    args = attributes(object)
    args$fn(args, plot)
}


#' @importFrom ggplot2 ggplot_add
#' @export
print.surviveR_plot = function(x, ...){
    attributes(x)
    rlang::inform(paste("This object must be added to a ggplot object"))
}


# ------------------------------------------------------------------------------
# FUNCTION: km.survreg.coxph_wrangle_args
# WHAT IT DOES: takes in the arguments + dataset and wrangles it into the output dataset "ds" that has time, time2, treatments, and strata columns.
# ------------------------------------------------------------------------------

check_aliases=function(the_list){
    arg_names=names(the_list)
    aliases=c(
        "trt" = "treatments",
        "trts" = "treatments",
        "treatment" = "treatments",
        "Ids" = "ids",
        "Id" = "ids",
        "id" = "ids",
        "ID" = "ids",
        "IDS" = "ids",
        "status" = "time2",
        "type"  = "time2",
        "line.color" = "color",
        "line.colour" = "color",
        "colour" = "color",
        "conf.colour" = "conf.color",
        "ordered" = "order"
    )

    name_matches = arg_names %in% names(aliases)

    if (any(name_matches)) {
        names(the_list)[name_matches] = aliases[arg_names[name_matches]]
    }
    the_list=the_list[!duplicated(names(the_list))]

    the_list
}


check_length_1=function(args_list){
    arg_lengths = lengths(args_list)

    invalid_idx = which(arg_lengths != 1)

    if (length(invalid_idx) > 0) {
        # Extract the names (or indices if names don't exist)
        bad_names = names(args_list)[invalid_idx]

        # Fallback if the list elements aren't named
        if (is.null(bad_names)) {
            bad_names = paste0("[[", invalid_idx, "]]")
        }

        # Collapse names into a single string for the error message
        bad_elements_str = paste(bad_names, collapse = ", ")

        stop("The following arguments must be of length of 1: ", bad_elements_str)
    }
}


check_color_args=function(arg_name,extras_arg){  # "color", "conf.color", "fill", "conf.fill"
    arg_value=eval_tidy(extras_arg)
    if(length(arg_value)!=1){
        stop(paste0(arg_name," must be of length 1."))
    } else if(is_string(arg_value)){
        arg_value=as.character(arg_value)
    } else if(is.na(arg_value)){
        arg_value="transparent"
    } else{
        stop(paste0("Invalid input for ", arg_name,": ",arg_value))
    }
    arg_value
}


check_linetype_args=function(arg_name,extras_arg){ # "linetype", "conf.linetype"
    arg_value=eval_tidy(extras_arg)
    #if(is.numeric(extras_arg) | is_string(extras_arg)){
    if(length(arg_value)!=1){
        stop(paste0(arg_name," must be of length 1."))
    } else if(is.numeric(arg_value) | is_string(arg_value)){
        arg_value=arg_value
        print(arg_value)
        #args$extras$linetype = args$extras$linetype
    } else{
        #stop(paste("Invalid input for linetype ", args$extras$linetype))
        stop(paste0("Invalid input for ", arg_name,": ",arg_value))
        
    }
    arg_value
}


check_numeric_args=function(arg_name, extras_arg){ # linewidth, conf.linewidth, size
    arg_value=eval_tidy(extras_arg)
    if(length(arg_value)!=1){
        stop(paste0(arg_name," must be of length 1."))
    } else if(is.numeric(arg_value) & arg_value>=0){
        arg_value = arg_value
    } else{
        stop(paste0("Invalid input for ", arg_name,": ",arg_value))
    }
    arg_value
}


check_alpha_args=function(arg_name,extras_arg){
    arg_value=eval_tidy(extras_arg)
    if(length(arg_value)!=1){
        stop(paste0(arg_name," must be of length 1."))
    } else if(as.numeric(arg_value)>=0 & as.numeric(arg_value<=1)){
        arg_value=as.numeric(arg_value)
    } else if(is.na(arg_value)){
        arg_value=0
    } else{
        stop(paste0("Invalid input for ", arg_name,": ",arg_value))
    }
    arg_value
}


check_conf_int=function(conf_int){
    if(! is.null(conf_int)){
        if(length(conf_int)!=1){
            stop("ERROR: conf.int argument must be of length 1.")
        } else if(as.numeric(conf_int) > 0 & as.numeric(conf_int<1)){
            conf_int=as.numeric(conf_int)
        } else if(conf_int == T){
            conf_int = 0.95
        } else if (conf_int == F | is.na(conf_int) | conf_int==0){
            conf_int = F  # this is just so the confidence interval stuff in the geoms won't break
        } else{
            # raise_error("Invalid input for conf.int: ", args$extras$conf.int)
            stop("Invalid confidence interval value.  Please provide a constant between 0 and 1.")
        }
    } else{
        conf_int=0.95
    }
    conf_int
}

# Handle everything that doesn't pertain to variables (line color, CI fill, etc)
wrangle_global_args=function(args){   # wrangle non-aes args
    # convert it to a basic list to prevent the error regarding mixed quosure/non quosure lists
    args = as.list(args)
    args$extras = as.list(args$extras)

    args$extras = Filter(Negate(is.null), args$extras)  # get rid of null arguments
    # check_length_1(args$extras)  # check that all non-null extra args are of length 1

    # filter out all of the non-convenient
    for(i in names(args$extras)){
        if(i %in% c("color", "conf.color", "fill", "conf.fill")){
            args$extras[[i]]=check_color_args(i,args$extras[[i]])
        } else if(i %in% c("linewidth", "conf.linewidth", "size")){
            args$extras[[i]]=check_numeric_args(i,args$extras[[i]])
        } else if(i %in% c("alpha", "conf.alpha")){
            args$extras[[i]]=check_alpha_args(i,args$extras[[i]])
        } else if (i %in% c("linetype", "conf.linetype")){  # EMMA ADDED
            args$extras[[i]]=check_linetype_args(i,args$extras[[i]])
        }
    }

    # ---------------- CONFIDENCE INTERVAL ----------------

    args$conf.int = check_conf_int(args$conf.int)
    if(args$conf.int==F){
        args$conf.int = 0.95
        args$extras$conf.alpha = 0
        args$extras$conf.fill = "transparent"
        args$extras$conf.color = "transparent"
    }


    # ------------------------------------- PROCESS THE PRIMARY LINE ARGUMENTS -------------------------------------

    args$line_extra_args=list2(
        color=args$extras$color,
        linetype=args$extras$linetype,
        linewidth=args$extras$linewidth,
        alpha=args$extras$alpha
    )

    args$line_extra_args = Filter(Negate(is.null), args$line_extra_args)
    
    #  ------------------------------------- PROCESS THE CONFIDENCE INTERVAL ARGUMENTS -------------------------------------

    # conf.alpha (set default if user didn't provide an input)
    args$extras$conf.alpha=eval_tidy(args$extras$conf.alpha)
    if(is.null(args$extras$conf.alpha)){
        args$extras$conf.alpha=0.15
    }

    # conf.linewidth
    args$extras$conf.linewidth=eval_tidy(args$extras$conf.linewidth)
    if(is.null(args$extras$conf.linewidth)){
        args$extras$conf.linewidth=0.5
    }

    args$conf_int_args=list2(
        color=args$extras$conf.color,
        linetype=args$extras$conf.linetype,
        linewidth=args$extras$conf.linewidth,
        alpha=args$extras$conf.alpha,
        fill=args$extras$conf.fill
    )

    args$conf_int_args = Filter(Negate(is.null), args$conf_int_args)

    args
}

#' @importFrom stats as.formula
#' @importFrom stats predict
#' @importFrom stats qnorm
#' @importFrom stats terms
#' @importFrom vctrs vec_equal

km.survreg.coxph_wrangle_args = function(args, plot){
    # convert args to a basic list to prevent the error regarding mixed quosure/non quosure lists
    args = as.list(args)
    args$extras = as.list(args$extras)

    # data inheritance
    if(inherits(plot$data, "waiver")){
        if(! is.null(args$data)){
            args$data=args$data  # if the plot doesn't have data but geom_[blah] does, take the geom's data
        } else{
            args$data=tibble()  # empty tibble (neither plot nor geom_[blah] has data specified)
        }
    } else{
        if( is.null(args$data)){
            args$data=plot$data  # plot has data, but geom_doesn't => inherit the plot data regardless of inherit.aes
        } else if(args$inherit.aes==T){
            args$data=plot$data  # inherit plot data if args doesn't have any data (won't make a difference)
        } else{
            args$data=args$data  # don't inherit plot data
        }
    }

    # standardizing aliases before checking for data inheritance
    var_args=as.list(args$mapping)
    var_args=check_aliases(var_args)
    args$mapping=do.call(aes, var_args)  # add aesthetics back to args after checking for aliases


    # data inheritance
    inheriting = c("mapping","failure","formula")  # not sure about formula... also need to incorporate inherit.aes
    if (is.null(plot$surviveR_inherit)){
        args$surviveR_inherit = args[inheriting]
    } else{
        args$surviveR_inherit <- args[inheriting] <- plot$surviveR_inherit
    }

    var_args=as.list(args$mapping) # reprocess the mapping

    if(quo_is_null(var_args$time)){
        stop("ERROR: time must be provided.")
    }

    time=eval_tidy(var_args$time, args$data)
    time2=eval_tidy(var_args$time2, args$data)
    treatments=eval_tidy(var_args$treatments, args$data)
    strata=eval_tidy(var_args$strata, args$data)

    # ---------------------------------------------------------------
    # EXTRACT DATA FOR DATASET
    # ---------------------------------------------------------------

    ds=tibble(time=time, time2=time2, treatments=treatments, strata=strata)

    # If Treatment & Strata are the same, get rid of the strata column to
    # avoid duplicate columns in the dataset.  Otherwise some models won't fit.

    if(! is.null(treatments) & ! is.null(strata)){  # Treatment & Strata
        if(all(vctrs::vec_equal(ds$treatments, ds$strata, na_equal = TRUE))){
            ds=tibble(time=time, time2=time2, treatments=treatments)
        } else{
            ds=tibble(time=time, time2=time2, treatments=treatments, strata=strata)
        }
    }

    if(rlang::has_name(ds,"treatments")){
        ds=ds %>%
            mutate(treatments = factor(treatments)) %>%
            rename(!!var_args$treatments := treatments)
    }

    if(rlang::has_name(ds,"strata")){
        ds=ds %>%
            mutate(strata = factor(strata)) %>%
            rename(!!var_args$strata := strata)
    }

    args$ds=ds

    # ---------------------------------------------------------------
    # OTHER VARIABLES (FAILURE, CONF.INT)
    # ---------------------------------------------------------------

    if(! is.null(args$failure)){
        args$failure=eval_tidy(args$failure)
        if( length(args$failure) != 1 ){
            stop("ERROR: failure argument must be of length 1.")
        } else if(as.numeric(args$failure)==1){
            args$failure=T
        } else{
            args$failure=F
        }
    } else{
        args$failure=F
    }

    #checking aliases of graphical variables
    args=check_aliases(args)  # just in case...
    args$extras=check_aliases(args$extras)
    args=wrangle_global_args(args)  # take care of the global line & conf int arguments

    # CREATE FORMULA TEXT FOR SURVIVAL MODEL:
    args=km.survreg.coxph_create_formula(args)

    args
}


# ------------------------------------------------------------------------------
# FUNCTION: km.survreg.coxph_create_formula
# WHAT IT DOES: takes in mapping obj from args (time,time2,treatments,strata)
#               and then creates a formula string
# ------------------------------------------------------------------------------

#' @importFrom stats as.formula
#' @importFrom stats predict
#' @importFrom stats qnorm
#' @importFrom stats terms

km.survreg.coxph_create_formula=function(args){

    var_args=as.list(args$mapping)

    # Names of treatments
    if (! is_null(var_args$treatments)){
        treatmenttxt = as_label(var_args$treatments)
    } else{
        treatmenttxt = "1"
    }
    # Same as above, except with the strata() wrapper
    if (! is_null(var_args$strata)){
        stratatxt = paste("strata(",as_label(var_args$strata), ")", sep = "")
    } else{
        stratatxt = NULL
    }

    time2txt="time2";  if(quo_is_null(var_args$time2)){ time2txt=NULL}

    LHS=paste("Surv(",  str_c("time", time2txt , sep=","), ")", sep="")
    RHS=str_c(treatmenttxt, stratatxt, sep="+")

    # Save formula to args
    args$formula=as.formula(paste(LHS, RHS, sep=" ~ "))

    args

}

# ------------------------------------------------------------------------------
# FUNCTION: km.survreg.coxph_wrangle_formula
# WHAT IT DOES: takes in a formula + dataset and wrangles it into the
#               (time,time2,treatments,strata) aes mapping.
# ------------------------------------------------------------------------------

km.survreg.coxph_wrangle_formula = function(args){
    # convert it to a basic list to prevent the error regarding mixed quosure/non quosure lists
    args = as.list(args)

    args$formula=as.formula(args$formula)
    the_formula = args$formula
    # LHS of formula
    surv_object = the_formula[[2]]
    time_quo = sym(as_name(surv_object[[2]]))
    time2_quo = as_name(surv_object[[3]])
    if(length(time2_quo)==0){ time2_quo=NULL } else{
        time2_quo=sym(time2_quo)
    }

    # RHS of formula
    formula_terms = attr(terms(the_formula),"term.labels")
    # If there aren't any treatments or strata, set both inputs to NULL.  Otherwise, check for strata first, then treatments.
    if(length(formula_terms)==0){
        treatments_quo=strata_quo=NULL
    } else{
        # Check if there is a stratifying variable.  If there is, find and enquo it.  Otherwise, set it to NULL.
        if(sum(str_detect(formula_terms,"^strata\\(")>0)){
            strata_index=which(str_detect(formula_terms,"^strata\\("))
            strata=formula_terms[strata_index]
            strata_quo=sym(sub("^strata\\((.*)\\)$", "\\1", strata)) # strip the strata wrapping
            rm(strata)
            treatments_quo=formula_terms[-strata_index] # the remaining variable is the treatment variable
        } else{
            strata_quo=NULL
        }
        # Check if there is a treatment variable remaining
        treatments_quo=formula_terms[1]
        if(length(treatments_quo)==0){
            treatments_quo=NULL
        } else{
            treatments_quo=sym(treatments_quo)
        }
    }

    time_quo = rlang::new_quosure(time_quo, args$env)
    time2_quo = rlang::new_quosure(time2_quo, args$env)
    treatments_quo=rlang::as_quosure(treatments_quo, args$env)
    strata_quo=rlang::as_quosure(strata_quo, args$env)

    # put them into an aesthetics mapping object again
    var_args=list(time=time_quo, time2=time2_quo,
                  treatments=treatments_quo, strata=strata_quo)
    args$mapping=do.call(aes, var_args)
    rm(time_quo, time2_quo, treatments_quo, strata_quo)

    args
}


# ------------------------------------------------------------------------------
# FUNCTION: mcf_wrangle_formula
# WHAT IT DOES: formats data for geom_mcf function
# ------------------------------------------------------------------------------

mcf_wrangle_args = function(args, plot){
    # convert it to a basic list to prevent the error regarding mixed quosure/non quosure lists
    args = as.list(args)
    args$extras = as.list(args$extras)

    if(inherits(plot$data, "waiver")){
        if(! is.null(args$data)){
            args$data=args$data  # if the plot doesn't have data but geom_[blah] does, take the geom's data
        } else{
            args$data=tibble()  # empty tibble (neither plot nor geom_[blah] has data specified)
        }
    } else{
        if( is.null(args$data)){
            args$data=plot$data  # plot has data, but geom_doesn't => inherit the plot data regardless of inherit.aes
        } else if(args$inherit.aes==T){
            args$data=plot$data  # inherit plot data if args doesn't have any data (won't make a difference)
        } else{
            args$data=args$data  # don't inherit plot data
        }
    }

    # extracting variables from mapping
    var_args=as.list(args$mapping)
    var_args=check_aliases(var_args)
    args$mapping=do.call(aes, var_args)  # add aesthetics back to args after checking for aliases

    if(quo_is_null(var_args$time)){
        stop("ERROR: time must be provided.")
    }

    # data inheritance
    inheriting = "mapping"
    if (is.null(plot$surviveR_inherit)){
        args$surviveR_inherit = args[inheriting]
    } else{
        args[inheriting] = args$surviveR_inherit = plot$surviveR_inherit
    }

    time=eval_tidy(var_args$time, args$data)
    ids=eval_tidy(var_args$ids, args$data)
    treatments=eval_tidy(var_args$treatments, args$data)
    strata=eval_tidy(var_args$strata, args$data)


    # DATA CHECKS

    # time
    if(!is.numeric(time)){
        stop("time vector must be numeric")
    }

    # ids
    if (is.null(ids)){
        ids = rep("1",length(time))
        warning("Note: No ids provided; all observations will be assumed to come from one group.")
    } else if(length(ids)==1){
        ids = rep(ids,length(time))
    } else if(length(ids) != length(time)){
        stop(paste("ERROR: non-null ids vector must be of length 1 or",length(time)))
    } else{
        ids=ids
    }

    # treatments
    if (is.null(treatments)){
        treatments=rep("",length(time))
    } else if(length(treatments)==1){
        treatments = rep(treatments,length(time))
    } else if(length(treatments) != length(time)){
        stop(paste("ERROR: non-null treatments vector must be of length 1 or",length(time)))
    } else{
        treatments=treatments
    }

    # strata
    if (is.null(strata)){
        strata=rep("",length(time))
    } else if(length(strata)==1){
        strata = rep(strata,length(time))
    } else if(length(strata) != length(time)){
        stop(paste("ERROR: non-null strata vector must be of length 1 or",length(time)))
    } else{
        strata=strata
    }

    ds=tibble(ids=ids, time=time, treatments=treatments, strata=strata)

    args$ds=ds

    #checking aliases of graphical variables
    args=check_aliases(args)  # just in case...
    args$extras=check_aliases(args$extras)

    args=wrangle_global_args(args)  # take care of the global line & conf int arguments

    args
}


# ------------------------------------------------------------------------------
# FUNCTION:
# WHAT IT DOES: formats data for geom_event function
# ------------------------------------------------------------------------------

# TO DO: Remove redundancy in the following function. -KLG 7/9
wrangle_event_global_args=function(args){

    args$extras = Filter(Negate(is.null), args$extras)  # get rid of null arguments
    args$extras = check_aliases(args$extras)
    check_length_1(args$extras)  # check that all non-null extra args are of length 1

    # filter out all of the non-convenient
    for(i in names(args$extras)){
        if(i %in% c("color", "conf.color", "fill", "conf.fill", "segment.color")){
            args$extras[[i]]=check_color_args(i,args$extras[[i]])
        } else if(i %in% c("linewidth", "conf.linewidth", "size")){
            args$extras[[i]]=check_numeric_args(i,args$extras[[i]])
        } else if(i %in% c("alpha", "conf.alpha")){
            args$extras[[i]]=check_alpha_args(i,args$extras[[i]])
        } else if (i %in% c("linetype", "conf.linetype")){  # EMMA ADDED
            args$extras[[i]]=check_linetype_args(i,args$extras[[i]])
        }
    }

    args$point_extra_args=list2(
        color=args$extras$color,
        fill=args$extras$fill,
        # shape=args$extras$shape, # <- handled in dataset creation for geom_event
        size=args$extras$size,
        alpha=args$extras$alpha
    )

    args$point_extra_args = Filter(Negate(is.null), args$point_extra_args)

    # ---------------------------------------------------------------------
    # LINE ARGS: SEGMENT.COLOR, LINEWIDTH, LINETYPE, LINEALPHA <-not currently an option
    # ---------------------------------------------------------------------

    if(is.null(args$extras$linewidth)){
        args$extras$linewidth=0.75  # default linewidth
    }

    args$line_extra_args=list2(
        color=args$extras$segment.color,
        linetype=args$extras$linetype,
        linewidth=args$extras$linewidth,
        alpha=args$extras$line.alpha
    )

    args
}

# ------------------------------------------------------------------------------
# FUNCTION: event_wrangle_args
# WHAT IT DOES: formats data for geom_event function
# ------------------------------------------------------------------------------

event_wrangle_args = function(args, plot){
    # convert it to a basic list to prevent the error regarding mixed quosure/non quosure lists
    args = as.list(args)
    args$extras = as.list(args$extras)

    # data inheritance
    if(inherits(plot$data, "waiver")){
        if(! is.null(args$data)){
            args$data=args$data  # if the plot doesn't have data but geom_[blah] does, take the geom's data
        } else{
            args$data=tibble()  # empty tibble (neither plot nor geom_[blah] has data specified)
        }
    } else{
        if( is.null(args$data)){
            args$data=plot$data  # plot has data, but geom_doesn't => inherit the plot data regardless of inherit.aes
        } else if(args$inherit.aes==T){
            args$data=plot$data  # inherit plot data if args doesn't have any data (won't make a difference)
        } else{
            args$data=args$data  # don't inherit plot data
        }
    }

    #checking aliases of graphical variables
    args=check_aliases(args)
    args$extras=check_aliases(args$extras)

    # extracting variables from mapping
    var_args=as.list(args$mapping)
    var_args=check_aliases(var_args)
    args$mapping=do.call(aes, var_args)  # add aesthetics back to args after checking for aliases

    if(quo_is_null(var_args$time)){
        stop("ERROR: time must be provided.")
    }

    # data inheritance
    inheriting = "mapping"
    if (is.null(plot$surviveR_inherit)){
        args$surviveR_inherit = args[inheriting]
    } else{
        args[inheriting] = args$surviveR_inherit = plot$surviveR_inherit
    }

    time=eval_tidy(var_args$time, args$data)
    time2=eval_tidy(var_args$time2, args$data)
    ids=eval_tidy(var_args$ids, args$data)
    treatments=eval_tidy(var_args$treatments, args$data)

    # DATA CHECKS

    # time, time2
    if(!is.numeric(time)){
        stop("time vector must be numeric")
    }


    if(is.null(time2)){
        time2=rep(1,length(time))
    } else{
        if(!is.numeric(time2)){
            stop("time2 vector must be numeric")
        }
    }

    # ids
    if (is.null(ids)){
        ids = factor(1:length(time))
        warning("Note: No IDs provided; default ID will be the row number.")
    } else if(length(ids) != length(time)){
        stop(paste("ERROR: non-null ID vector must be of length",length(time)))
    } else{
        ids=factor(ids)
    }

    ds=tibble(ids=ids, time=time, time2=time2, treatments=treatments)

    if(has_name(ds,"time2")){
        ds=ds %>% filter(! ( is.na(time) & is.na(time2)) ) # throw out any completely missing observations.
    }

    args$ds=ds

    # cens type (evaluate whether time2 is really a status column)

    if(is.null(ds$time2)){
        args$cens_type="none"
    } else if( all(ds$time2 == 1) | all(ds$time==ds$time2) ){
        args$cens_type="none"
    } else if( all(ds$time2 %in% c(0, 1))){
        args$cens_type="right"
    } else{
        args$cens_type="interval/other"
        # one last check: check that time1<=time2
        if(any(ds$time2 < ds$time1, na.rm = TRUE)){
            stop("ERROR: time2 must be >= time for all interval censored observations.")
        }
    }

    # xmax
    time_max=max(ds$time,na.rm=T)
    time2_max=ifelse(has_name(ds,"time2"),max(ds$time2,na.rm=T),NA)

    if(!is.null(args$xmax)){
        if(is.na(args$xmax)){
            args$xmax=1.2*max(time2_max,time1_max,na.rm=T)
        } else if(! is.numeric(args$xmax)){
            stop(paste("ERROR: invalid input for xmax: ", args$xmax, ".  Argument must be numeric or NULL.",sep=""))
        } else if(args$xmax < max(ds$time,na.rm=T)){
            warning("ERROR: xmax must be less than or equal to the maximum value of time vector.  Default xmax will be assumed.")
            args$xmax=1.2*max(time2_max,time1_max,na.rm=T)
        }
    } else{ # use rules to determine xmax
        if(args$cens_type=="none" | args$cens_type=="right"){
            args$xmax=1.2*time_max
        } else if(args$cens_type=="left"){
            args$xmax=1.2*time2_max
        } else{
            args$xmax=1.2*max(time2_max,time1_max,na.rm=T)
        }
    }

    # order
    if(is.null(args$order)){
        args$order=TRUE
    } else if(is.na(args$order) | args$order==F){
        args$order=FALSE
    } else if(is_string(args$order)){
        order_string=substr(tolower(args$order),1,3)
        if(order_string=="des"){
            args$order="desc"
        } else if(order_string=="asc"){
            args$order="asc"
        } else{
            stop(paste("ERROR: Invalid input for order argument:", args$order))
        }
    } else{
        args$order=TRUE
    }

    args

}

