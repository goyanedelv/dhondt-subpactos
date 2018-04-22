
dhondt_Chile_unsubpact<- function(data_all, distrito, asientos_p){

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

			for (g in 1:length(set_Coalicion)){

					data_secundario<- subset(data_all,Distrito==distrito & Pacto==set_Coalicion[g])
					data_secundario<- data_secundario[order(-data_secundario$votacion),]

					terciario <- head(data_secundario,primario_df$Seats[g])
					lista_dhondt_output[[t]]<-terciario
					t<-t+1
			 	}


distribucion_partidos<-plyr::ldply(lista_dhondt_output, data.frame)

return(distribucion_partidos)
}
