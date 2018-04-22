umbral_dhondt<- function(data_all, distrito, asientos_p,umbral){

	data_primario<- subset(data_all,Distrito==distrito) #me quedo solo con el D=8

	inicial=dhondt_Chile(data_all, distrito, asientos_p)

	#de los electos, quienes no pasaron el umbral
	subumbral=inicial[inicial$proporcion<umbral,]

	if(nrow(subumbral)>0){
		#elimino todos los electos del pool inicial
		secundario=data_primario[!(data_primario$Candidato %in% inicial$Candidato),]

		#de secundario, todos quienes no pasaron el umbral
		terciario=subset(secundario,secundario$proporcion>umbral)

		pactos_cupo_extra=subset(as.data.frame(table(subumbral$Pacto)),Freq>0)
		colnames(pactos_cupo_extra)=c("Pacto","Cupos")

		lista_dhondt_output <-list()
		t<-1
		for(i in 1:length(pactos_cupo_extra$Pacto)){

			candidatos_pacto<- subset(terciario,Pacto==pactos_cupo_extra$Pacto[i])
			candidatos_pacto<- candidatos_pacto[order(-candidatos_pacto$votacion),]
			final <- head(candidatos_pacto,pactos_cupo_extra$Cupos[i])
			lista_dhondt_output[[t]]<-final
			t<-t+1

		}

		cuaternario=inicial[!(inicial$Candidato %in% subumbral$Candidato),]
		nuevos<-plyr::ldply(lista_dhondt_output, data.frame)
		cuaternario=rbind(cuaternario,nuevos)

		return(cuaternario)
	}
	return(inicial)
}
