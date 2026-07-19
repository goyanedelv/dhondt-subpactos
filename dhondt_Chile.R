#' Compute D'Hondt seat allocation for Chilean elections (with party sub-pacts)
#'
#' Applies the D'Hondt algorithm in two stages: first to allocate seats among
#' coalitions (pactos), then within each coalition to allocate seats among
#' parties (partidos). Elected candidates within each party are selected by
#' highest vote count.
#'
#' @param data_all Data frame with columns: Candidato, Pacto, Partido, Distrito,
#'   votacion.
#' @param distrito Integer. District number to compute results for.
#' @param asientos_p Integer. Total seats available in the district.
#'
#' @return A data frame of elected candidates for the district.
#'
#' @examples
#' \dontrun{
#' library(electoral)
#' library(plyr)
#' data <- read.csv("vote_candidate_2017.csv", fileEncoding = "UTF-8-BOM")
#' dhondt_chile(data, 1, 3)
#' }
dhondt_chile <- function(data_all, distrito, asientos_p) {

  stopifnot(
    is.data.frame(data_all),
    is.numeric(distrito), length(distrito) == 1,
    is.numeric(asientos_p), length(asientos_p) == 1, asientos_p > 0
  )

  # Stage 1: allocate seats among coalitions using D'Hondt
  data_primario <- subset(data_all, Distrito == distrito)

  data_agg <- aggregate(
    x  = data_primario$votacion,
    by = list(data_primario$Pacto),
    FUN = sum
  )
  colnames(data_agg) <- c("Pacto", "votacion")

  primario <- seats(
    parties = data_agg$Pacto,
    votes   = data_agg$votacion,
    n_seats = asientos_p,
    method  = "dhondt"
  )
  primario_df <- data.frame(Party = primario$PARTY, Seats = primario$SEATS)

  # Stage 2: within each coalition, allocate seats among parties using D'Hondt
  set_coalicion <- primario_df$Party
  lista_dhondt_output <- list()
  result_idx <- 1

  for (coalition in seq_along(set_coalicion)) {

    coalition_seats <- primario_df$Seats[primario_df$Party == set_coalicion[coalition]]

    if (coalition_seats > 0) {
      data_secundario <- subset(
        data_all,
        Distrito == distrito & Pacto == set_coalicion[coalition]
      )

      data_agg2 <- aggregate(
        x  = data_secundario$votacion,
        by = list(data_secundario$Partido),
        FUN = sum
      )
      colnames(data_agg2) <- c("Partido", "votacion")

      secundario <- seats(
        parties = data_agg2$Partido,
        votes   = data_agg2$votacion,
        n_seats = coalition_seats,
        method  = "dhondt"
      )
      secundario_df <- data.frame(Party = secundario$PARTY, Seats = secundario$SEATS)
      secundario_df <- subset(secundario_df, secundario_df$Seats > 0)

      set_partido <- secundario_df$Party

      # Stage 3: select top candidates by vote count within each party
      for (party in seq_along(set_partido)) {
        data_partido <- subset(
          data_all,
          Distrito == distrito & Partido == set_partido[party]
        )
        data_partido <- data_partido[order(-data_partido$votacion), ]

        lista_dhondt_output[[result_idx]] <- head(data_partido, secundario_df$Seats[party])
        result_idx <- result_idx + 1
      }
    }
  }

  distribucion_partidos <- plyr::ldply(lista_dhondt_output, data.frame)

  return(distribucion_partidos)
}
