
dhondt_Chile <- function(data_all, distrito, asientos_p) {

### Coalition iteration
data_primario <- subset(data_all, Distrito == distrito)

data_agg <- aggregate(x = data_primario$votacion,
 by = list(data_primario$Pacto), FUN = sum)
colnames(data_agg) <- c("Pacto", "votacion")

primario <- seats(parties = data_agg$Pacto, votes = data_agg$votacion,
 n_seats = asientos_p, method = "dhondt") # dHondt coalition level
primario_df <- data.frame(Party = primario$PARTY, Seats = primario$SEATS)

### Subpact iteration
set_Coalicion <- primario_df$Party
lista_dhondt_output <- list()
t <- 1
for (x in 1:length(set_Coalicion)){

if (primario_df$Seats[primario_df$Party == set_Coalicion[x]] > 0) {
  data_secundario <- subset(data_all,Distrito == distrito &
   Pacto == set_Coalicion[x])

  data_agg2 <- aggregate(x = data_secundario$votacion,
   by = list(data_secundario$Partido), FUN = sum)
   colnames(data_agg2) <- c("Partido", "votacion")

  secundario <- seats(parties = data_agg2$Partido, votes = data_agg2$votacion,
   n_seats = primario_df$Seats[primario_df$Party == set_Coalicion[x]],
   method = 'dhondt')
  secundario_df <- data.frame(Party = secundario$PARTY,
   Seats = secundario$SEATS)
  secundario_df <- subset(secundario_df,secundario_df$Seats > 0)

  set_Partido <- secundario_df$Party
  for (g in 1:length(set_Partido)) {

    data_secundario <- subset(data_all, Distrito == distrito &
     Partido == set_Partido[g])
    data_secundario <- data_secundario[order(-data_secundario$votacion), ]

    terciario <- head(data_secundario, secundario_df$Seats[g])
    lista_dhondt_output[[t]] <- terciario
    t <- t + 1
  }
 }
}


distribucion_partidos <- plyr::ldply(lista_dhondt_output, data.frame)

return(distribucion_partidos)
}
