#' Plot impulse responses of lp_lin in lpirfs 
#'  
#' @param lplin_irf impulse responses of lp_lin in lpirfs 
#' @param main main title of plot (The default is NULL, in which case the main
#' title is generated automatically.)
#' @param sub subtitile of plot 
#' @param cap caption of plot
#' @param var_name variable names: ex. var_name=c("Emp", "Prod", "RW", "Unemp")
#' @param dev_new logical. If TRUE, open a new graphics device.
#' @param \dots further arguments passed to or from other methods 
#' (currently not used).
#' @return A ggplot object
#' 
#' @import stats
#' @importFrom ggplot2 ggplot labs facet_grid geom_hline 
#' @importFrom ggplot2 scale_linetype_manual scale_x_continuous
#' @importFrom dplyr full_join mutate
#' @importFrom tibble is_tibble add_column
#' @importFrom tidyr unnest pivot_longer
#'
#' @references Victor Espinoza, (2022), 
#' "Plot() impulse response function - show more than one in one window?,"
#' stackoverflow. \url{https://stackoverflow.com/questions/68010256/plot-impulse-response-function-show-more-than-one-in-one-window}
#' @author Victor Espinoza (stackoverflow), Koichi (Koiti) Yano
#'
#' @examples
#'\donttest{
#' }
#' @export
"ggplot.lpirfs_lin_obj" <- function(lplin_irf, main=NULL, sub=NULL, cap=NULL,
                            var_name=NULL, dev_new=FALSE, ...){
  
  # Check class
  if (class(lplin_irf) %in% "lpirfs_lin_obj") {
  } else{
    stop("Only 'lpirfs_lin_obj' class object from lpirfs::lp_lin()")
  }

  # dev.new() if dev_new is TRUE.
  if (isTRUE(dev_new)){ dev.new() } else { } 
  
  # Convert lp_lin_irf object to varirf object
  # https://stackoverflow.com/questions/20198751/three-dimensional-array-to-list
  lplin_irf$specs$column_names -> var_name
  dimnames(lplin_irf$irf_lin_mean)[[1]] <- var_name
  dimnames(lplin_irf$irf_lin_mean)[[3]] <- var_name
  # Mean of IRF
  lplin_irf$irf_lin_mean -> irf_tmp
  irf<- lapply(seq(dim(irf_tmp)[3]), function(x) t(irf_tmp[ , , x]))
  names(irf) <- var_name
  
  # Make irf of varirf object
  irf <- list(irf)
  
  # No visible binding for global variable
  # https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/
  imp <- Time <- type <- name <- value <- NULL
  fortify <- function(data){
    result <- vector(mode = "list",length = 0L)
    # var_nameの要素数が内生変数の数と異なる場合を考慮していない。要改善
    if (!is.null(var_name) ){ 
      for (d in 1:length(data)) {
        colnames(data[[d]]) <- var_name
        result[[length(result)+1]] <- tibble::tibble(imp = var_name[d],
                                                     Time = 0:{nrow(data[[d]])-1},
                                                     tibble::as_tibble(data[[d]]))
      }
    } else {
      # use column names if var_name is NULL
      for (d in 1:length(data)) {
        result[[length(result)+1]] <- tibble::tibble(imp = names(data)[d],
                                                     Time = 0:{nrow(data[[d]])-1},
                                                     tibble::as_tibble(data[[d]]))
      }
    }
    data <- tidyr::unnest(tibble::tibble(result),cols = result)
    return(data)
  }
  
  
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
  return(plot)
}
