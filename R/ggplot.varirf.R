#' Plot impulse responses of VAR and SVAR
#' 
#' @param irf impulse responses of VAR or SVAR
#' @param main main title of plot (The default is NULL, in which case the main
#' title is generated automatically.)
#' @param sub subtitile of plot 
#' @param cap caption of plot
#' @param var_name variable names: var_name=c("Emp", "Prod", "RW", "Unemp")
#' @param \dots further arguments passed to or from other methods 
#' (currently not used).
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
#' @author Victor Espinoza (on stackoverflow), Koichi (Koiti) Yano
#'
#' @examples
#' data(Canada)
#' ## For VAR
#' var.2c <- VAR(Canada, p = 2, type = "const")
#' irf.2c <- irf(var.2c, impulse = "e", response = c("e", "prod", "rw", "U"), boot =
#' TRUE)
#' ggplot(irf.2c, sub="Canada", cap="Caption")
#'
#' ## For SVAR
#' amat <- diag(4)
#' diag(amat) <- NA
#' svar.a <- SVAR(var.2c, estmethod = "direct", Amat = amat)
#' irf.sa <- irf(svar.a)
#' ggplot(irf.sa, main="Canada", sub="Structural IRF", 
#' cap="Caption: The original time series are published by the OECD.")
#' @export
"ggplot.varirf" <- function(irf, main=NULL, sub=NULL, cap=NULL,
                            var_name=NULL, ...){

  if (class(irf) %in% "varirf") {
  } else{
    stop("Only 'varirf' class object from vars::irf()")
  }

  # No visible binding for global variable
  # https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/
  imp <- Time <- type <- name <- value <- NULL
  fortify <- function(data){
    result <- vector(mode = "list",length = 0L)
    # var_nameの要素数が内生変数の数と異なる場合を考慮していない。要改善
    if (!is.null(var_name) ){ 
      # 
      for (d in 1:length(data)) {
#        browser()
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
  return(plot)
}
