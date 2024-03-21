#' @import stats
#' @importFrom ggplot2 ggplot labs facet_grid geom_hline 
#' @importFrom ggplot2 scale_linetype_manual scale_x_continuous
#' @importFrom dplyr full_join mutate
#' @importFrom tibble is_tibble add_column
#' @importFrom tidyr unnest pivot_longer
#'
#' @references Victor Espinoza, (2022),
#' "Plot() impulse response function - show more than one in one window?,"
#' stackoverflow.
#' @author Victor Espinoza (stackoverflow), Koichi (Koiti) Yano
#'
#' @export
"ggplot.varirf" <- function(irf, sub=NULL, cap=NULL, ...){

  if (class(irf) %in% "varirf") {
  } else{
    stop("Only 'varirf' class object from vars::irf()")
  }

  # No visible binding for global variable
  # https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/
  imp <- Time <- type <- name <- value <- NULL
  fortify <- function(data){
    result <- vector(mode = "list",length = 0L)
    for (d in 1:length(data)) {
      result[[length(result)+1]]<- tibble::tibble(imp = names(data)[d],
                                                  Time = 0:{nrow(data[[d]])-1},
                                                  tibble::as_tibble(data[[d]]))
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
                      name = paste(name,"(resp.)")))
  }

  # ggplot2
  if(irf$model == "varest" & irf$ortho == TRUE){
    main="orthogonal Impulse Response"
  } else if (irf$model == "svarest") {
    main="SVAR Impulse Response"
  } else {
    main=NULL
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
