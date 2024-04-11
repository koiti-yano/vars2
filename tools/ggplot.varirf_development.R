# ggplot.varirf
# References:
# "Plot() impulse response function - show more than one in one window?"
# https://stackoverflow.com/questions/68010256/plot-impulse-response-function-show-more-than-one-in-one-window
# Copyright (c) 2022, Victor Espinoza (stackoverflow)
# Thank you for sharing this code!
# Slightly modified by Koichi (Koiti) Yano

"ggplot.varirf" <- function(irf){

  if (class(irf) %in% "varirf") {
  } else{
    stop("Only 'varirf' class object from vars::irf()")
  }

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

require(vars2)
data("Canada")
var_p2 <- VAR(Canada,2)

# boot=TRUE
var_p2_irf_boot <- irf(var_p2)
ggplot.varirf(var_p2_irf_boot)

# boot=FALSE
var_p2_irf <- irf(var_p2, boot=F)
ggplot.varirf(var_p2_irf)

# boot=TRUE
var_p2_irf_e_boot <- irf(var_p2,impulse = "e",
                         response = c("e", "prod", "rw", "U"))
ggplot.varirf(var_p2_irf_e_boot)

# boot=FALSE
var_p2_irf_e_boot <- irf(var_p2,impulse = "e",
                         response = c("e", "prod", "rw", "U"),
                         boot=FALSE)
ggplot.varirf(var_p2_irf_e_boot)
