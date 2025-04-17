#' @author Gabriel Gaona
#' @title Inter-event Time for Rain Events Definition
#' @description This function separates subdaily rain data into events and calculates the rainfall characteristics for each event. # nolint
#' This function uses date-times and rainfall data in mm.
#' @param .x A data.frame with at least these two columns:
#' @param IETD Time threshold. A difftime or numeric in hours.
#' @param thres Rainfall threshold in mm/h.

#' \itemize{
#' \item \code{date_time}. A datetime vector with the date-time of the measurement. # nolint
#' \item \code{rainfall} A numeric vector with the rainfall in mm.
#' }
#' @return A list of \code{data.frames}:
#' \itemize{
#' \item \code{Rainfall_Characteristics} A data frame n events characteristics.
#' \item \code{Rainfall_Events} A data frame with rainfall data and event identifiers. # nolint
#' }
#' @importFrom foreach foreach %dopar%
#' @importFrom rlang !! := is_empty
#' @importFrom doParallel registerDoParallel stopImplicitCluster
#' @importFrom paralell detectCores
#' @importFrom lubridate is.difftime
#' @importFrom tibble tibble
#' @importFrom dplyr mutate left_join filter select vars one_of row_number

library(foreach)
library(rlang)
library(lubridate)
ietd_re <- function(.x = data,
                    ietd = as.difftime(2, units = "hours"),
                    thres = 0.5) {
  if (!lubridate::is.difftime(ietd))
    ietd <- as.difftime(ietd, units = "hours")
  nms <- names(.x)
  time_step <- median(diff(.x[[1]]))
  units(time_step) <- "hours"

  if (any(diff(.x[[1]]) != time_step)) {
    .x <- tibble::tibble(!!nms[1] := seq(min(.x[[1]]),
                                         max(.x[[1]]),
                                         by = time_step)) |>
      dplyr::left_join(.x, by = nms[1]) |>
      dplyr::mutate_at(.vars = dplyr::vars(dplyr::one_of(nms[2])),
                       .funs = ~ifelse(is.na(.x), 0, .x))
  }

  ncores <- ifelse(parallel::detectCores() > 2,
                   parallel::detectCores() - 1, 1)

  date_time <- .x[[1]]
  rainfall <- .x[[2]]

  # window <- as.integer(round(1 / as.numeric(time_step))) #no-lint

  rain_idx <- which(rainfall > 0)
  if (rlang::is_empty(rain_idx)) {
    warning("No rain events in the input data")
    return(NULL)
    }
  ts <- date_time[rain_idx]
  tsd <- difftime(ts[-1], ts[-length(ts)], units = "hours")
  time_acc <- zoo::rollsum(tsd, FUN = sum, k = 1, align = "right")
  dates_end <- c(min(date_time),
                 ts[which(time_acc > ietd)],
                 date_time[rain_idx[length(rain_idx)]])


  #calc_event_data
  doParallel::registerDoParallel(ncores)

  event_characteristics <-
    foreach::foreach(
      i = seq_along(dates_end[-1]),
      .combine = "rbind") %dopar% {

      rng <- which(date_time > (dates_end[i]) & date_time <= dates_end[i + 1]) # nolint
      if (i == 1) rng <- c(1, rng) # nolint

      rf_dt <- rainfall[rng]
      rng <- rng[which(rf_dt > 0)[1]:length(rng)]
      rf_dt <- rainfall[rng]
      rf_tm <- date_time[rng]
      duration <- difftime(max(rf_tm) + time_step, min(rf_tm), units = "hours")
      volume <- sum(rf_dt)
      intensity <- sum(rf_dt) / (length(rf_dt) * as.numeric(time_step))
      characteristics <- tibble::tibble(event = i,
                                        start = min(rf_tm),
                                        end = max(rf_tm),
                                        duration = duration,
                                        volume = volume,
                                        intensity = intensity,
                                        control = volume >= thres)
    }
  doParallel::stopImplicitCluster()

  event_characteristics <- dplyr::filter(event_characteristics, control) |>
    dplyr::mutate(event = dplyr::row_number()) |>
    dplyr::select(-control)

  if (nrow(event_characteristics) == 0) {
    warning("No rain events over rain threshod in the input data")
    return(NULL)
  }

  doParallel::registerDoParallel(ncores)
  event_data <- foreach::foreach(k = seq_len(nrow(event_characteristics)),
                                 .combine = "rbind") %dopar% {
      ev_dt <- tibble::tibble(
        datetime = seq(min(event_characteristics$start[k]),
                      max(event_characteristics$end[k]),
                      by = time_step)) |>
        dplyr::left_join(tibble::tibble(datetime = date_time,
                                 rainfall = rainfall),
                  by = "datetime") |>
        `colnames<-`(value = colnames(.x)) |>
        dplyr::mutate(Event = k)
    }
  doParallel::stopImplicitCluster()

  return(list("Rainfall_Characteristics" = event_characteristics,
              "Rainfall_Events" = event_data))
}

