
dhondt_chile_general <- function(data_all, distrito, asientos_p) {

###Iteracion a Coalicion
data_primario <- subset(data_all, Distrito == distrito) # pick the district

data_agg <- aggregate(x = data_primario$votacion,
  by = list(data_primario$Pacto), FUN = sum) # collapse by pact
colnames(data_agg) <- c("Pacto", "votacion")

primario <- seats(parties = data_agg$Pacto, votes = data_agg$votacion,
 seats = asientos_p) # dHondt Coaliciones
primario_df <- data.frame(Party = primario$PARTY, Seats = primario$SEATS)

###Iteracion a Subpacto
set_Coalicion <- primario_df$Party
lista_dhondt_output <- list()
t <- 1

for (g in 1:length(set_Coalicion)) {

  data_secundario <- subset(data_all, Distrito == distrito &
   Pacto == set_Coalicion[g])
  data_secundario <- data_secundario[order(-data_secundario$votacion), ]

  terciario <- head(data_secundario, primario_df$Seats[g])
  lista_dhondt_output[[t]] <- terciario
  t <- t + 1
}

distribucion_partidos <- plyr::ldply(lista_dhondt_output, data.frame)

return(distribucion_partidos)
}
