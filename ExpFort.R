#Carga de los datos obtenidosn el experimento

Sujetos<-gl(12,1,12,c("S1","S2","S3","S4","S5","S6","S7","S8","S9","S10","S11","S12"))
P.sin.bebida<-c(1,2,1,1,2,1,2,2,2,1,1,1)
P.con.bebida<-c(2,1,2,2,1,2,1,1,1,2,2,2)
P.sin.bebida=factor(P.sin.bebida, levels=c(1,2), labels=c("1","2"))
P.con.bebida=factor(P.con.bebida, levels=c(1,2), labels=c("1","2"))
Pts.Arena<-c(800,800,1000,1000,4500,4250,1200,4750,4000,1000,5000,3500)
Pts.Arena=factor(Pts.Arena, levels=c(800,4500,4250,1200,4750,4000,1000,5000,3500),
                 labels=c("800","1000","1200","4250","4000","4500","4750","3500","5000") )
RP.sin.bebida<-c(11,20,9,7,31,30,30,23,31,10,34,24)
RP.con.bebida<-c(13,12,12,11,34,32,34,16,30,10,32,25)
RR.sin.bebida<-c(2,27,23,23,42,12,27,20,39,15,40,27)
RR.con.bebida<-c(4,14,24,38,40,21,27,21,22,4,38,31)
str(Pts.Arena)
Jug.Fortnite<-data.frame(Sujetos,Pts.Arena,P.sin.bebida,P.con.bebida,RP.sin.bebida,RP.con.bebida,RR.sin.bebida,RR.con.bebida)

#Creación de la tabla con la forma y la simbología del diseño 2^2

#Es de mucha ayuda este paso ya que agrupamos los resultados por niveles 
#y facilita la graficación

A<-gl(2,6,24,c("-","+"))
A
B<-gl(2,12,24,c("-","+"))
B
RP<-c(11,20,9,7,30,10,13,12,12,11,34,10,31,30,23,31,34,24,34,32,16,30,32,25)
RR<-c(2,27,23,23,27,15,4,14,24,38,27,4,42,12,20,39,40,27,40,21,21,22,38,31)
Factorial2.k<-data.frame(A,B,RP,RR)

#Inicia el ANOVA para las variables de puntería y reflejos

j<-aov(Factorial2.k$RP~Factorial2.k$A*Factorial2.k$B)
summary(j)
j1<-aov(Factorial2.k$RR~Factorial2.k$A*Factorial2.k$B)
summary(j1)

#Agrupación de los resultados en puntería para graficar

Y<-(Factorial2.k %>% group_by(A) %>% summarise(RP=mean(RP)))
Y1<-(Factorial2.k %>% group_by(B) %>% summarise(RP=mean(RP)))

#Graficación de los resultados en puntería

con2x1 = matrix(c(1:2), nrow=1, byrow=TRUE)
lay.1=layout(con2x1)
layout.show(lay.1)

plot(NA,xlim=c(1,4),xlab=("A:Bebida              B:Habilidad"),
     ylim=c(10,30), ylab=("Puntaje"))
Y1$B=c(3,4)
lines(Y$A,Y$RP,lw=3)
lines(Y1$B,Y1$RP,col='red',lw=3)
title('Medias de los factores A y B en puntería')
grid(nx = NULL, ny = NULL,
     lty = 1, col = "gray", lwd = 1)
text(1,23,"-1.0", cex=0.6,font=2)
text(2,23,"1.0", cex=0.6,font=2)
text(3,14,"-1.0", cex=0.6,font=2)
text(4,27,"1.0", cex=0.6,font=2)

j<-(Factorial2.k %>% group_by(A,B) %>% summarise(P=mean(RP)))
plot(NA,xlim=c(1,2),xlab=("A:Bebida"), ylim=c(10,30), ylab=("Puntaje"),labfont=2)
for (a in split(j,j$B)) {
  lines(a$A,a$P,col=a$B[1],lw=3)
  
}
text(1.5,29,"B:Habilidad = +", cex=0.7,font=2)
text(1.5,16,"B:Habilidad = -",cex= 0.7,font=2)
text(1.1,16,"A = -",cex= 0.7,font=2)
text(1.9,16,"A = +",cex= 0.7,font=2)
text(1.1,29,"A = -",cex= 0.7,font=2)
text(1.9,29,"A = +",cex= 0.7,font=2)
title('Medias de los puntajes en puntería separados por niveles')
grid(nx = NULL, ny = NULL,
     lty = 1, col = "gray", lwd = 1)

#Agrupación de los resultados en reflejos para graficar

Y2<-(Factorial2.k %>% group_by(A) %>% summarise(RR=mean(RR)))
Y3<-(Factorial2.k %>% group_by(B) %>% summarise(RR=mean(RR)))

#Graficas de la variable reflejos

plot(NA,xlim=c(1,4),xlab=("A:Bebida              B:Habilidad"),
     ylim=c(15,35), ylab=("Puntaje"))
Y3$B=c(3,4)
lines(Y2$A,Y2$RR,lw=3)
lines(Y3$B,Y3$RR,col='red',lw=3)
title('Medias de los factores A y B en reflejos')
grid(nx = NULL, ny = NULL,
     lty = 1, col = "gray", lwd = 1)
text(1,24,"-1.0", cex=0.5,font=2)
text(2,23,"1.0", cex=0.5,font=2)
text(3,18,"-1.0", cex=0.5,font=2)
text(4,28,"1.0", cex=0.5,font=2)

j1<-(Factorial2.k %>% group_by(A,B) %>% summarise(P=mean(RR)))
plot(NA,xlim=c(1,2),xlab=("A:Bebida"), ylim=c(15,35), ylab=("Puntaje"))
for (a in split(j1,j1$B)) {
  lines(a$A,a$P,col=a$B[1],lw=3)
  
}
text(1.5,31,"B:Habilidad = +", cex=0.7,font=2)

text(1.5,18,"B:Habilidad = -",cex= 0.7,font=2)
title('Medias de los puntajes en reflejos separados por niveles')
grid(nx = NULL, ny = NULL,
     lty = 1, col = "gray", lwd = 1)
text(1.1,18,"A = -",cex= 0.7,font=2)
text(1.9,18,"A = +",cex= 0.7,font=2)
text(1.1,31,"A = -",cex= 0.7,font=2)
text(1.9,31,"A = +",cex= 0.7,font=2)