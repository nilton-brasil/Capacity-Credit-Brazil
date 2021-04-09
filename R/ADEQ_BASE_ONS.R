##### Dia 30/12/2007 - tem a hora 00:00 como hora 01:00 Para os dados de carga, de geracao eolica 


#Carga = read.xlsx("D:\\Erick\\Credito Capacidade Solar Brasil\\Doutorado\\Carga Futura\\Teste1Artigo.xlsx", namedRegion= "Carga_Brasil_Bruta")
Carga = read.xlsx("D:\\Erick\\Credito Capacidade Solar Brasil\\Dados_NE.xlsx", namedRegion= "Carga_NE")

Carga$Data.Escala.de.Tempo.1.CDH.Simp.4 = as.numeric(Carga$Data.Escala.de.Tempo.1.CDH.Simp.4)
Carga$Selecione.Tipo.de.CDH.Simp.4 = as.numeric(Carga$Selecione.Tipo.de.CDH.Simp.4)

#Horas repetidas
Carga[78832,1] = Carga[78832,1] - 1/24
Carga[78832,2] = Carga[78832,2] - 1/24




################## Bloco Carga ############################


contador = vector()
c = 1

for (i in  1:(nrow(Carga)-1)){
  
  aux = Carga[i,1] - Carga[i+1,1]
  
  if(aux < (-0.05)){
    
    contador[c] = i
    
    c = c+1
    
  }
  
}

b = 0 


for (i in 1:length(contador)){
  
  aux = Carga[contador[i]+b,1] - Carga[contador[i]+1+b,1]
  
  aux2 = c(Carga[contador[i]+b,1]-aux/2,Carga[contador[i]+b,1]-aux/2, "*", (Carga[contador[i]+b,4] + Carga[contador[i]+1+b,4])/2)
   Carga_corr = rbind(Carga[1:(contador[i]+b),], aux2,Carga[(contador[i]+b+1):nrow(Carga),])
   Carga_corr[contador[1]+1+b,1] = as.numeric(Carga_corr[contador[1]+1+b,1])
   Carga_corr[contador[1]+1+b,2] = as.numeric(Carga_corr[contador[1]+1+b,2])
   Carga_corr[contador[1]+1+b,4] = as.numeric(Carga_corr[contador[1]+1+b,4])
  
  Carga = Carga_corr
  
  Carga$Data.Escala.de.Tempo.1.CDH.Simp.4 = as.numeric(Carga$Data.Escala.de.Tempo.1.CDH.Simp.4)
  Carga$Selecione.Tipo.de.CDH.Simp.4 = as.numeric(Carga$Selecione.Tipo.de.CDH.Simp.4)
  
  b = b+1
  
}



####################### Bloco Geracao Eolica ####################################################

#Ger_EOL = read.xlsx("D:\\Erick\\Credito Capacidade Solar Brasil\\Doutorado\\Carga Futura\\Teste1Artigo.xlsx", namedRegion= "GER_EOL_BR")
Ger_EOL = read.xlsx("D:\\Erick\\Credito Capacidade Solar Brasil\\Dados_NE.xlsx", namedRegion= "Eolica_NE")

Ger_EOL[15407,1] = Ger_EOL[15407,1] - 1/24
Ger_EOL[15407,2] = Ger_EOL[15407,2] - 1/24
contador = vector()
c = 1

for (i in  1:(nrow(Ger_EOL)-1)){
  
  aux = Ger_EOL[i,1] - Ger_EOL[i+1,1]
  
  if(aux < (-0.05)){
    
    contador[c] = i
    
    c = c+1
    
  }
  
}

b = 0 


for (i in 1:length(contador)){
  
  cat(i, "\n")
  
  aux = Ger_EOL[contador[i]+b,1] - Ger_EOL[contador[i]+1+b,1]
  
  aux2 = c(Ger_EOL[contador[i]+b,1]-aux/2,Ger_EOL[contador[i]+b,1]-aux/2, "*","*", "Eólica","*", " ", (Ger_EOL[contador[i]+b,8] + Ger_EOL[contador[i]+1+b,8])/2)
  Ger_EOL_corr = rbind(Ger_EOL[1:(contador[i]+b),], aux2,Ger_EOL[(contador[i]+b+1):nrow(Ger_EOL),])
  Ger_EOL_corr[contador[1]+1+b,1] = as.numeric(Ger_EOL_corr[contador[1]+1+b,1])
  Ger_EOL_corr[contador[1]+1+b,2] = as.numeric(Ger_EOL_corr[contador[1]+1+b,2])
  Ger_EOL_corr[contador[1]+1+b,8] = as.numeric(Ger_EOL_corr[contador[1]+1+b,8])
  
  Ger_EOL = Ger_EOL_corr
  
 Ger_EOL$Data.Escala.de.Tempo.1.GE.Simp.4 = as.numeric(Ger_EOL$Data.Escala.de.Tempo.1.GE.Simp.4 )
 Ger_EOL$Data.Dica = as.numeric(Ger_EOL$Data.Dica)
 Ger_EOL$Selecione.Tipo.de.GE.Simp.4 = as.numeric(Ger_EOL$Selecione.Tipo.de.GE.Simp.4)
  
  b = b+1
  
}


################################### Bloco Ger_PV #################################################

#Ger_PV = read.xlsx("D:\\Erick\\Credito Capacidade Solar Brasil\\Doutorado\\Carga Futura\\Teste1Artigo.xlsx", namedRegion= "GER_PV_BR")
Ger_PV = read.xlsx("D:\\Erick\\Credito Capacidade Solar Brasil\\Dados_NE.xlsx", namedRegion= "Solar_NE")


contador = vector()
c = 1

for (i in  1:(nrow(Ger_PV)-1)){
  
  aux = Ger_PV[i,1] - Ger_PV[i+1,1]
  
  if(aux < (-0.05)){
    
    contador[c] = i
    
    c = c+1
    
  }
  
}

b = 0 


for (i in 1:length(contador)){
  
  cat(i, "\n")
  
  aux = Ger_PV[contador[i]+b,1] - Ger_PV[contador[i]+1+b,1]
  
  aux2 = c(Ger_PV[contador[i]+b,1]-aux/2,Ger_PV[contador[i]+b,1]-aux/2, "*","*", "Solar","*", " ", (Ger_PV[contador[i]+b,8] + Ger_PV[contador[i]+1+b,8])/2)
  Ger_PV_corr = rbind(Ger_PV[1:(contador[i]+b),], aux2,Ger_PV[(contador[i]+b+1):nrow(Ger_PV),])
  Ger_PV_corr[contador[1]+1+b,1] = as.numeric(Ger_PV_corr[contador[1]+1+b,1])
  Ger_PV_corr[contador[1]+1+b,2] = as.numeric(Ger_PV_corr[contador[1]+1+b,2])
  Ger_PV_corr[contador[1]+1+b,8] = as.numeric(Ger_PV_corr[contador[1]+1+b,8])
  
  Ger_PV = Ger_PV_corr
  
  Ger_PV$Data.Escala.de.Tempo.1.GE.Simp.4 = as.numeric(Ger_PV$Data.Escala.de.Tempo.1.GE.Simp.4)
  Ger_PV$Data.Dica = as.numeric(Ger_PV$Data.Dica)
  Ger_PV$Selecione.Tipo.de.GE.Simp.4 = as.numeric(Ger_PV$Selecione.Tipo.de.GE.Simp.4)
  
  b = b+1
  
}

####################### Formacao dos dados ##########################

Carga_consolidada = as.data.frame(Carga$Selecione.Tipo.de.CDH.Simp.4)
colnames(Carga_consolidada) = "Carga"
Ger_EOL_consolidada = as.data.frame(Ger_EOL$Selecione.Tipo.de.GE.Simp.4)

Ger_PV_consolidada = as.data.frame(Ger_PV$Selecione.Tipo.de.GE.Simp.4)

A = as.data.frame(rep(0, nrow(Carga_consolidada)-nrow(Ger_EOL_consolidada) ) )
colnames(A) = "Geracao Eolica"
colnames(Ger_EOL_consolidada) = "Geracao Eolica"

Ger_EOL_consolidada_comp = rbind(A, Ger_EOL_consolidada)


B = as.data.frame(rep(0, nrow(Carga_consolidada) - nrow(Ger_PV_consolidada) ))
colnames(B) = "Geracao PV"
colnames(Ger_PV_consolidada) = "Geracao PV"

Ger_PV_consolidada_comp = rbind(B, Ger_PV_consolidada)

Datas = as.data.frame(Carga$Data.Escala.de.Tempo.1.CDH.Simp.4)
colnames(Datas) = "Dia"

Dados_Carga_Ger_BR = cbind(Datas, Carga_consolidada, Ger_EOL_consolidada_comp, Ger_PV_consolidada_comp)


#write.xlsx(Dados_Carga_Ger_BR, "D:\\Erick\\Credito Capacidade Solar Brasil\\Doutorado\\Carga Futura\\Dados_Finais_BR_1999_2019.xlsx")
write.xlsx(Dados_Carga_Ger_BR, "D:\\Erick\\Credito Capacidade Solar Brasil\\Doutorado\\Carga Futura\\Dados_Finais_NE_2017_2020.xlsx")
