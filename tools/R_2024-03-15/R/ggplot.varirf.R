#' ggplot.varirf: Plot impulse responses with ggplot2
#'
#' @param irf a list of impulse responses
#'
#' @return plot
#' @export
#' @import stats
#' @importFrom ggplot2 ggplot facet_grid scale_linetype_manual
#' @importFrom ggplot2 geom_hline scale_x_continuous
#' @importFrom dplyr full_join mutate
#' @importFrom tibble is_tibble add_column
#' @importFrom tidyr unnest pivot_longer
#'
#' @author Victor Espinoza, Koichi (Koiti) Yano


"ggplot.varirf" <- function(irf){

  if (class(irf) %in% "varirf") {
  } else{
    stop("Only 'varirf' class object from vars::irf()")
  }

  # https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/
  utils::globalVariables(c("imp", "Time", "type", "name",
                           "value"))

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
  if(!is.null(irf$Upper)){
    plot <- ggplot2::ggplot(plot_data) +
      ggplot2::geom_line(ggplot2::aes(x = Time,value,
                                      lty = type),show.legend = F) +
      ggplot2::facet_grid(cols = ggplot2::vars(imp),
                          rows = ggplot2::vars(name)) +
      ggplot2::scale_linetype_manual(values = c("lower"=2,
                                                "upper"=2,
                                                "mean"=1)) +
      ggplot2::geom_hline(yintercept = 0,lty = 3) +
      ggplot2::scale_x_continuous(labels = as.integer)
  } else {
    plot <- ggplot2::ggplot(plot_data) +
      ggplot2::geom_line(ggplot2::aes(x = Time,value,
                                      lty = type),show.legend = F) +
      ggplot2::facet_grid(cols = ggplot2::vars(imp),
                          rows = ggplot2::vars(name)) +
      ggplot2::scale_linetype_manual(values = c("mean"=1)) +
      ggplot2::geom_hline(yintercept = 0,lty = 3) +
      ggplot2::scale_x_continuous(labels = as.integer)
  }
  return(plot)
}
