#' Plot impulse responses of VAR and SVAR
#' 
#' @param irf impulse responses of VAR or SVAR
#' @param main main title of plot
#' @param sub subtitle of plot (plan to be deprecated)
#' @param cap caption of plot
#' @param imp_name variable names of impulse: ex. imp_name=c("Emp")
#' @param resp_name variable names of reponse: ex. resp_name=c("Emp", "Prod", "RW", "Unemp")
#' @param dev_new logical. If TRUE, open a new graphics device.
#' @param \dots further arguments passed to or from other methods 
#' (currently not used).
#' @return A ggplot object
#' 
#' @import stats
#' @importFrom ggplot2 ggplot labs ylab xlab ggtitle facet_grid 
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_line geom_ribbon geom_hline
#' @importFrom ggplot2 scale_linetype_manual scale_x_continuous
#' @importFrom dplyr full_join mutate filter
#' @importFrom tibble is_tibble add_column
#' @importFrom tidyr unnest pivot_longer
#' @importFrom ggplot2 is.ggplot
#' @importFrom grid is.grob
#' @importFrom patchwork wrap_plots
#' 
#' @references Victor Espinoza, (2022), 
#' "Plot() impulse response function - show more than one in one window?,"
#' stackoverflow. \url{https://stackoverflow.com/questions/68010256/plot-impulse-response-function-show-more-than-one-in-one-window}
#' @author Koichi (Koiti) Yano, Victor Espinoza (stackoverflow)
#'
#' @examples
#'\donttest{
#' data(Canada)
#' ## For VAR
#' var.2c <- VAR(Canada, p = 2, type = "const")
#' irf.2c <- irf(var.2c, impulse = "e", response = c("e", "prod", "rw", "U"), boot =
#' TRUE)
#' vars_plot(irf.2c, sub="Canada", cap="Caption", imp_name=c("Emp"),
#' resp_name=c("Emp", "Prod", "Real Wage", "Unemp"))
#'
#' ## For SVAR
#' amat <- diag(4)
#' diag(amat) <- NA
#' svar.a <- SVAR(var.2c, estmethod = "direct", Amat = amat)
#' irf.sa <- irf(svar.a)
#' vars_plot(irf.sa, main="Canada", sub="Structural IRF", 
#' cap="Caption: The original time series are published by the OECD.",
#' var_name=c("Emp", "Prod", "Real Wage", "Unemp"), dev_new=TRUE)
#' }
#' @export
"vars_plot.varirf" <- function(irf, main=NULL, cap=NULL,
                            resp_name=NULL, imp_name=NULL, dev_new=FALSE, 
                            graph_style="pw", ...){
  
  # Check class
  if (class(irf) %in% "varirf") {
  } else{
    stop("Only 'varirf' class object from vars::irf()")
  }

  # dev.new() if dev_new is TRUE.
  if (isTRUE(dev_new)){ dev.new() } else { } 
  
  
  if(graph_style=="pw"){
    plot_list <- list(NaN)
    
    irf_mean <- irf$irf
    irf_lower <- irf$Lower
    irf_upper <- irf$Upper
    impulse <- irf$impulse
    response <- irf$response
    
    #browser()
    if ((length(impulse)==length(imp_name)) & (length(response) == length(resp_name))){
      impulse <- imp_name
      response <- resp_name
    }

    t_end <- dim(irf_mean[[1]])[1]
    
    num_imp <- length(impulse)
    num_rsp <- length(response)
    
    plot_num <- 1
    for (imp in 1:num_imp) {
      for (rsp in 1:num_rsp) {
        
        if(is.null(irf_lower[[imp]][,rsp])){
          data.frame(time=1:t_end, mean=irf_mean[[imp]][,rsp],
                     low=irf_mean[[imp]][,rsp], 
                     upp=irf_mean[[imp]][,rsp]) -> irf_tbl
        } else {
          data.frame(time=1:t_end, mean=irf_mean[[imp]][,rsp],
                     low=irf_lower[[imp]][,rsp], 
                     upp=irf_upper[[imp]][,rsp]) -> irf_tbl
        } 
        
        # Title and subtitle of IRF
        irf_title <- paste("Impulse from ",impulse[imp])
        if(is.null(cap)) {
          irf_caption <- NULL
        } else {
          irf_caption <- cap
        }
        
        if(rsp==1){ # Add title if rsp=1
          irf_tbl |> ggplot() +
            geom_line(aes(x = time, y = mean)) +
            geom_ribbon(aes(x=time, y=mean, ymin=low,ymax=upp),alpha=0.3) + 
            ylab(paste("Resp. of ",response[rsp])) + 
            labs(title=irf_title, x="Time") +
            geom_hline(yintercept = 0, col = "red", 
                       linewidth = 0.5, linetype = "dashed") -> plot_list[[plot_num]] 
        } else if (rsp==num_rsp) { # Add caption if rsp=num_rsp
          irf_tbl |> ggplot() + 
            geom_line(aes(x = time, y = mean)) +
            geom_ribbon(aes(x=time, y=mean, ymin=low,ymax=upp),alpha=0.3) + 
            ylab(paste("Resp. of ",response[rsp])) + xlab("Time") +
            # labs(caption=irf_caption) + # caption is added by plot_annotation
            geom_hline(yintercept = 0, col = "red", 
                       linewidth = 0.5, linetype = "dashed") -> plot_list[[plot_num]]
          
        } else { # The other cases
          irf_tbl |> ggplot() + 
            geom_line(aes(x = time, y = mean)) +
            geom_ribbon(aes(x=time, y=mean, ymin=low,ymax=upp),alpha=0.3) + 
            ylab(paste("Resp. of ",response[rsp])) + xlab("Time") +
            geom_hline(yintercept = 0, col = "red", 
                       linewidth = 0.5, linetype = "dashed") -> plot_list[[plot_num]]
          
        }
        plot_num <- plot_num + 1
        
      }
    }
    plot <- wrap_plots(plot_list, byrow=FALSE, 
                       axes = "collect_x", 
                       ncol = num_imp, nrow = num_rsp) +
      patchwork::plot_annotation(
        title = main,
        caption = cap)
  } else if (graph_style=="vic"){
    # No visible binding for global variable
    # https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/
    imp <- Time <- type <- name <- value <- NULL
    fortify <- function(data){
      result <- vector(mode = "list",length = 0L)
      # var_nameの要素数が内生変数の数と異なる場合を考慮していない。要改善
      if (!is.null(resp_name) ){ 
        # 
        for (d in 1:length(data)) {
          #        browser()
          colnames(data[[d]]) <- resp_name
          result[[length(result)+1]] <- tibble::tibble(imp = resp_name[d],
                                                       Time = 0:{nrow(data[[d]])-1},
                                                       tibble::as_tibble(data[[d]]))
        }
      } else {
        # use column names if resp_name is NULL
        for (d in 1:length(data)) {
          result[[length(result)+1]] <- tibble::tibble(imp = names(data)[d],
                                                       Time = 0:{nrow(data[[d]])-1},
                                                       tibble::as_tibble(data[[d]]))
        }
      }
      data <- tidyr::unnest(tibble::tibble(result),cols = result)
      return(data)}
    
    
    data_irf <- fortify(irf$irf)
    
    # Prepare plot_data
    if(!is.null(irf$Upper)){
      data_lower <- fortify(irf$Lower)
      data_upper <- fortify(irf$Upper)
      suppressMessages(
        plot_data <- tibble::add_column(data_irf,type = "mean") |>
          dplyr::full_join(tibble::add_column(data_lower,type = "lower")) |>
          dplyr::full_join(tibble::add_column(data_upper,type = "upper")) |>
          tidyr::pivot_longer(cols = -c(imp,Time,type)) |>
          dplyr::mutate(imp = paste(imp,"(impls.)"),
                        name = paste(name,"(resp.)")))
    } else {
      suppressMessages(
        plot_data <- tibble::add_column(data_irf,type = "mean") |>
          tidyr::pivot_longer(cols = -c(imp,Time,type)) |>
          dplyr::mutate(imp = paste(imp,"(impls.)"),
                        name = paste(name,"(resp.)"))) # Original code
    }
    
    # ggplot2
    if (!is.null(main)) {
      # Do nothing
    } else {
      if(irf$model == "varest" & irf$ortho == TRUE){
        main="Orthogonal Impulse Response"
      } else if (irf$model == "svarest") {
        main="SVAR Impulse Response"
      }
    }
    
    if(!is.null(irf$Upper)){
      plot <- ggplot2::ggplot(plot_data) +
        ggplot2::geom_line(ggplot2::aes(x = Time, y= value,
                                        lty = type), show.legend = F) + 
        ggplot2::labs(title = main, subtitle = sub, caption = cap) + 
        ggplot2::facet_grid(cols = ggplot2::vars(imp),
                            rows = ggplot2::vars(name)) +
        ggplot2::scale_linetype_manual(values = c("lower"=2,
                                                  "upper"=2,
                                                  "mean"=1)) +
        ggplot2::geom_hline(yintercept = 0,lty = 3) +
        ggplot2::scale_x_continuous(labels = as.integer) 
    } else {
      plot <- ggplot2::ggplot(plot_data) +
        ggplot2::geom_line(ggplot2::aes(x = Time,y = value,
                                        lty = type), show.legend = F) +
        ggplot2::labs(title = main, subtitle = sub, caption = cap) + 
        ggplot2::facet_grid(cols = ggplot2::vars(imp),
                            rows = ggplot2::vars(name)) +
        ggplot2::scale_linetype_manual(values = c("mean"=1)) +
        ggplot2::geom_hline(yintercept = 0,lty = 3) +
        ggplot2::scale_x_continuous(labels = as.integer) 
    }
  }
  return(plot)
  
  }
