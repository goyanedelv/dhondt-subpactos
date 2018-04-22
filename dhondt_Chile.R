
dhondt_Chile <- function(data_all, distrito, asientos_p){

#library(SciencesPo)
#library(dplyr)

###Iteracion a Coalicion
data_primario<- subset(data_all,Distrito==distrito) #me quedo solo con el D=8

data_agg <- aggregate(x = data_primario$votacion, by = list(data_primario$Pacto), FUN = sum)#colapso por pacto
colnames(data_agg)=c("Pacto","votacion") #pongo nombres correctos al output de aggregate

primario<-dHondt(parties=data_agg$Pacto, votes=data_agg$votacion, seats=asientos_p)#dHondt Coaliciones
primario_df <- data.frame(Party=primario$Party,Seats=primario$Seats)#convierto a data frame

###Iteracion a Subpacto
set_Coalicion<-primario_df$Party
lista_dhondt_output <-list()
t<-1
for (x in 1:length(set_Coalicion)){

	if(primario_df$Seats[primario_df$Party==set_Coalicion[x]]>0){
		data_secundario<- subset(data_all,Distrito==distrito & Pacto==set_Coalicion[x])

		data_agg2 <- aggregate(x = data_secundario$votacion, by = list(data_secundario$Partido), FUN = sum)
		colnames(data_agg2)=c("Partido","votacion")

		secundario<-dHondt(parties=data_agg2$Partido, votes=data_agg2$votacion,
 					seats=primario_df$Seats[primario_df$Party==set_Coalicion[x]])
		secundario_df <- data.frame(Party=secundario$Party,Seats=secundario$Seats)
		secundario_df <- subset(secundario_df,secundario_df$Seats>0)
			
			set_Partido<-secundario_df$Party
			for (g in 1:length(set_Partido)){

					data_secundario<- subset(data_all,Distrito==distrito & Partido==set_Partido[g])
					data_secundario<- data_secundario[order(-data_secundario$votacion),]

					terciario <- head(data_secundario,secundario_df$Seats[g])
					lista_dhondt_output[[t]]<-terciario
					t<-t+1
			 	}
	}
}


distribucion_partidos<-plyr::ldply(lista_dhondt_output, data.frame)

return(distribucion_partidos)
}
