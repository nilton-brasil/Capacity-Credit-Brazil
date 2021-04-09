

days = c(1,32,60,91,121,152,182,213,244,274,305,335)
days_s = c(60,121,182,244,305,335)

jpeg("D:\\Erick\\Credito Capacidade Solar Brasil\\Doutorado\\Artigo CESP\\COPT.jpeg", width = 12 , height = 10 , units = "cm", res = 900, bg = "white")

plot(ELCC_BR_2019_dia$COPT[[1]]$Capacity,ELCC_BR_2019_dia$COPT[[1]]$Prob
     ,ylab = "Probability", xlab = "Capacity [MW]",cex.axis = 0.7, cex.lab = 0.7, type ="l", col = "blue",lwd = 1)

colour = c("green" , "darkgoldenrod1", "darkorchid4", "red", "magenta", "pink")

a=1
for (i in days_s){  
lines(ELCC_BR_2019_dia$COPT[[i]]$Capacity,ELCC_BR_2019_dia$COPT[[i]]$Prob
      ,ylab = "Probability", xlab = "Capacity [MW]",cex.axis = 0.7, cex.lab = 0.7, type ="l", col = colour[a], lwd = 1)
  a=a+1
}

legend("topleft", legend = c("January", "March", "May", "July", "September", "November", "December"),
       col = c("blue", colour),inset =0, lwd =1, bty = "n", cex = 0.6)

dev.off()






days = c(1,32,60,91,121,152,182,213,244,274,305,335)
days_s = c(60,121,182,244,305,335)

jpeg("D:\\Erick\\Credito Capacidade Solar Brasil\\Doutorado\\Artigo CESP\\COPT_Acum.jpeg", width = 12 , height = 10 , units = "cm", res = 900, bg = "white")

plot(ELCC_BR_2019_dia$COPT[[1]]$Capacity,ELCC_BR_2019_dia$COPT[[1]]$LOLP
     ,ylab = "Probability", xlab = "Capacity [MW]", cex.axis = 0.7, cex.lab = 0.7, type ="l", col = "blue",lwd = 1)

colour = c("green" , "darkgoldenrod1", "darkorchid4", "red", "magenta", "pink")

a=1
for (i in days_s){  
  lines(ELCC_BR_2019_dia$COPT[[i]]$Capacity,ELCC_BR_2019_dia$COPT[[i]]$LOLP
        ,ylab = "Probability", xlab = "Capacity [MW]", cex.axis = 0.7, cex.lab = 0.7, type ="l", col = colour[a], lwd = 1)
  a=a+1
}

legend("topleft", legend = c("January", "March", "May", "July", "September", "November", "December"),
       col = c("blue", colour),inset =0, lwd =1, bty = "n", cex = 0.6)

dev.off()
