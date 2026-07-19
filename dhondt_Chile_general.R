#' Compute D'Hondt seat allocation for Chilean elections (coalition-level only)
#'
#' A "blind" version of \code{\link{dhondt_chile}} that does not apply the
#' party-level sub-pact iteration. Seats won by each coalition are filled
#' directly by the top vote-getters in that coalition.
#'
#' @param data_all Data frame with columns: Candidato, Pacto, Partido, Distrito,
#'   votacion.
#' @param distrito Integer. District number to compute results for.
#' @param asientos_p Integer. Total seats available in the district.
#'
#' @return A data frame of elected candidates for the district.
dhondt_chile_general <- function(data_all, distrito, asientos_p) {

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
    seats   = asientos_p
  )
  primario_df <- data.frame(Party = primario$PARTY, Seats = primario$SEATS)

  # Stage 2: select top candidates by vote count within each coalition
  set_coalicion <- primario_df$Party
  lista_dhondt_output <- list()
  result_idx <- 1

  for (coalition in seq_along(set_coalicion)) {
    data_secundario <- subset(
      data_all,
      Distrito == distrito & Pacto == set_coalicion[coalition]
    )
    data_secundario <- data_secundario[order(-data_secundario$votacion), ]

    lista_dhondt_output[[result_idx]] <- head(data_secundario, primario_df$Seats[coalition])
    result_idx <- result_idx + 1
  }

  distribucion_partidos <- plyr::ldply(lista_dhondt_output, data.frame)

  return(distribucion_partidos)
}
