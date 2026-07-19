#' Compute D'Hondt seat allocation for Chilean elections with a vote threshold
#'
#' Extends \code{\link{dhondt_chile}} by applying a minimum vote-share threshold.
#' Candidates elected by the base algorithm who did not reach the threshold are
#' replaced by the highest-vote-share eligible candidates from the same coalition
#' who did meet the threshold.
#'
#' @param data_all Data frame with columns: Candidato, Pacto, Partido, Distrito,
#'   votacion, proporcion.
#' @param distrito Integer. District number to compute results for.
#' @param asientos_p Integer. Total seats available in the district.
#' @param umbral Numeric. Minimum vote share (proportion) a candidate must
#'   reach to be eligible, e.g. \code{0.05} for 5\%.
#'
#' @return A data frame of elected candidates for the district.
dhondt_chile_threshold <- function(data_all, distrito, asientos_p, umbral) {

  stopifnot(
    is.data.frame(data_all),
    is.numeric(distrito), length(distrito) == 1,
    is.numeric(asientos_p), length(asientos_p) == 1, asientos_p > 0,
    is.numeric(umbral), length(umbral) == 1, umbral >= 0, umbral <= 1
  )

  data_primario <- subset(data_all, Distrito == distrito)
  inicial <- dhondt_chile(data_all, distrito, asientos_p)

  # Identify elected candidates who did not meet the threshold
  subumbral <- inicial[inicial$proporcion < umbral, ]

  if (nrow(subumbral) > 0) {
    # Remove all initially elected candidates from the candidate pool
    secundario <- data_primario[!(data_primario$Candidato %in% inicial$Candidato), ]

    # Keep only remaining candidates who did meet the threshold
    terciario <- subset(secundario, secundario$proporcion >= umbral)

    pactos_cupo_extra <- subset(as.data.frame(table(subumbral$Pacto)), Freq > 0)
    colnames(pactos_cupo_extra) <- c("Pacto", "Cupos")

    lista_dhondt_output <- list()
    result_idx <- 1

    for (i in seq_along(pactos_cupo_extra$Pacto)) {
      candidatos_pacto <- subset(terciario, Pacto == pactos_cupo_extra$Pacto[i])
      candidatos_pacto <- candidatos_pacto[order(-candidatos_pacto$votacion), ]

      lista_dhondt_output[[result_idx]] <- head(candidatos_pacto, pactos_cupo_extra$Cupos[i])
      result_idx <- result_idx + 1
    }

    cuaternario <- inicial[!(inicial$Candidato %in% subumbral$Candidato), ]
    nuevos <- plyr::ldply(lista_dhondt_output, data.frame)
    cuaternario <- rbind(cuaternario, nuevos)

    return(cuaternario)
  }

  return(inicial)
}
