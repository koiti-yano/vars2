#' Plot impulse responses of lp_lin_iv in lpirfs 
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
#' @export

ggplot.lpirfs_lin_iv_obj <- function(irf, main=NULL, sub=NULL, cap=NULL,
                                   var_name=NULL, dev_new=FALSE, ...){

  if (class(irf) %in% "lpirfs_lin_iv_obj") {
  } else {
    stop("Object is not of class 'lpirfs_lin_iv_obj'") 
  }

  # dev.new() if dev_new is TRUE.
  if (isTRUE(dev_new)){ dev.new() } else { } 
  
  browser()
  
  #======================================
  # Convert lp_lin_irf object to varirf object
  # https://stackoverflow.com/questions/20198751/three-dimensional-array-to-list
  #======================================
  irf$specs$column_names -> var_name
  # Mean of IRF
  dimnames(irf$irf_lin_mean)[[1]] <- var_name
  t(irf$irf_lin_mean) -> irf_mean
  #dimnames(irf$irf_lin_mean)[[3]] <- var_name
  #irf_mean <- lapply(seq(dim(irf_tmp)[3]), 
  #                  function(x) t(irf_tmp[ , , x]))
  #names(irf_mean) <- var_name
  # Lower bound of IRF
  dimnames(irf$irf_lin_low)[[1]] <- var_name
  t(irf$irf_lin_low) -> irf_low
  #dimnames(irf$irf_lin_low)[[3]] <- var_name
  #irf$irf_lin_low -> irf_lower_tmp
  #irf_lower <- lapply(seq(dim(irf_lower_tmp)[3]), 
  #                    function(x) t(irf_lower_tmp[ , , x]))
  # Upper bound of IRF 
  dimnames(irf$irf_lin_up)[[1]] <- var_name
  t(irf$irf_lin_up) -> irf_up
  #dimnames(irf$irf_lin_up)[[3]] <- var_name
  #irf$irf_lin_up -> irf_upper_tmp
  #irf_upper <- lapply(seq(dim(irf_upper_tmp)[3]), 
  #                    function(x) t(irf_upper_tmp[ , , x]))
  # Make irf of varirf object
  irf <- NULL
  irf <- list(irf=irf_mean, Upper=irf_up, Lower=irf_low,
              model="varest", ortho=TRUE)
  
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

