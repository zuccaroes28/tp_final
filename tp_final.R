rm(list = ls())
require(ggplot2)
require(gdata)
require(lubridate)
require(xlsx)
require(here)
require(gridExtra)

#setwd("C:/Users/Usuario/Documents/Facultad/FCEN/Laboratorio de procesamiento de informacion meteorologica/Tp_Final/Sondeos")
setwd("/media/dcao-19/STORE N GO/Laboratorio de procesamiento de informacion meteorologica/Tp_Final/Sondeos")

#archivos<-list.files(path="C:/Users/Usuario/Documents/Facultad/FCEN/Laboratorio de procesamiento de informacion meteorologica/Tp_Final/Sondeos/")
archivos<-list.files(path="/media/dcao-19/STORE N GO/Laboratorio de procesamiento de informacion meteorologica/Tp_Final/Sondeos/")

#Lista de Data Frames donde cada uno de ellos es un sondeo diario realizado a las 12 UTC
sondeos1<-list()

for(i in 1:365){

datos<-read.table(file=archivos[i],nrows =-1,skip = 10,fill = TRUE,stringsAsFactors = FALSE,colClasses = numeric())
datos<-as.data.frame(sapply(datos, as.numeric))
datos<-datos[complete.cases(datos),]

sondeos2<-list(datos)

sondeos<-c(sondeos1,sondeos2)
sondeos1<-sondeos
}


#Genero vector donde cada posicion es la altura de la Isoterma de 0 grados
isoterma1<-numeric()

for(i in 1:length(sondeos)){

x<-which(sondeos[[i]][[3]]<15 & sondeos[[i]][[3]]>-15)

if(length(x)==0){

  isoterma2<-NA

}else{
  
  alt<-sondeos[[i]][[2]][x]
  temp<-sondeos[[i]][[3]][x]

if(length(alt)==1){
  
  isoterma2<-alt

  }else{
  interp<-approx(temp,alt,xout = 0,method = "linear")$y
  isoterma2<-round(interp,digits = 0)
}

}
isoterma<-c(isoterma1,isoterma2)
isoterma1<-isoterma
}

fechas<-seq(as.Date("2009/01/01"),by="day",length.out = 365)

serie<-data.frame(Fechas=fechas,Altura=isoterma)

#punto A
#write.xlsx(serie,"C:/Users/Usuario/Documents/Facultad/FCEN/Laboratorio de procesamiento de informacion meteorologica/Tp_Final/Serie Temporal.xls")
write.xlsx(serie,"/media/dcao-19/STORE N GO/Laboratorio de procesamiento de informacion meteorologica/Tp_Final/Serie Temporal.xls")

#punto B
vec<-c(1,32,60,91,121,152,182,213,244,274,305,335,366)

medias<-numeric()
desvios<-numeric()
for(i in 1:12){

  media<-round(mean(serie[(vec[i]):((vec[i+1])-1),2],na.rm = TRUE),digits = 0)
  desvio<-round(sd(serie[(vec[i]):((vec[i+1])-1),2],na.rm = TRUE),digits = 0)
  medias<-c(medias,media)
  desvios<-c(desvios,desvio)

  }
meses<-month(seq(fechas[1],by="month",length.out = 12))

medidas<-data.frame(Month=meses,Mean=medias,Std=desvios)

grafico<-ggplot(medidas,aes(meses,medias))+
  geom_line(color="blue")+
  scale_x_continuous(breaks = seq(1,12,1))+
  ylab("Altura")+
  xlab("Meses")+
  ggtitle("Marcha anual de altura media de Isoterma de 0 grados")+
  theme_bw()

grafico  

grafico2<-ggplot(medidas,aes(meses,desvios))+
  geom_line(color="red")+
  scale_x_continuous(breaks = seq(1,12,1))+
  ylab("Altura")+
  xlab("Meses")+
  ggtitle("Marcha anual dispersion de altura Isoterma de 0 grados")+
  theme_bw()

grafico2

graficos<-grid.arrange(grafico,grafico2)

graficos

#ggsave(graficos,filename = "C:/Users/Usuario/Documents/Facultad/FCEN/Laboratorio de procesamiento de informacion meteorologica/Tp_Final/Marchas.jpeg")
ggsave(graficos,filename = "/media/dcao-19/STORE N GO/Laboratorio de procesamiento de informacion meteorologica/Tp_Final/Marchas.jpeg")

#punto C
graphics.off()

#source("C:/Users/Usuario/Documents/Facultad/FCEN/Laboratorio de procesamiento de informacion meteorologica/Tp_Final/revlog.R")
source("/media/dcao-19/STORE N GO/Laboratorio de procesamiento de informacion meteorologica/Tp_Final/revlog.R")

vec1<-c(1,60,183,246)
vec2<-c("1 de Enero","1 de Marzo","2 de Julio","3 de Septiembre")

glist<-list()

for (i in 1:4)
  
local({
  i<-i
  p1<-ggplot(data = sondeos[[vec1[i]]],aes(sondeos[[vec1[i]]][[1]],sondeos[[vec1[i]]][[3]]))+
  geom_line(color="black")+
  scale_x_continuous(breaks = seq(100,1000,200),trans = revlog_trans(base = 2),position = "bottom")+
  scale_y_continuous(breaks = seq(-100,30,20))+
  coord_flip()+
  geom_line(color="red",data=sondeos[[vec1[i]]],aes(sondeos[[vec1[i]]][[1]],sondeos[[vec1[i]]][[4]]))+
  ylab("Temperatura")+
  xlab("Presion")+
  ggtitle(vec2[i])+
  theme(plot.title = element_text(size = 11),axis.title = element_text(size = 10))  
  theme_bw()
  glist[[i]]<<-p1
})

perfiles<-grid.arrange(glist[[1]],glist[[2]],glist[[3]],glist[[4]],top="Sondeos realizados en 4 dias")

#ggsave(perfiles,filename = "C:/Users/Usuario/Documents/Facultad/FCEN/Laboratorio de procesamiento de informacion meteorologica/Tp_Final/Perfiles.jpeg")
ggsave(perfiles,filename = "/media/dcao-19/STORE N GO/Laboratorio de procesamiento de informacion meteorologica/Tp_Final/Perfiles.jpeg")

