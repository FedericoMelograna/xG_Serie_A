###Rvest



# Inizio, dati ARTISTI E RANK ---------------------------------------------

##pacchetto di manipolazione dati 
library(tidyverse)
library(rvest)
###
#Identify the url from where you want to extract data
# semplicemnete la url del sito

#scraping web
base_url <- "https://understat.com/team/Lazio/2018"
webpage <- read_html(base_url)  
artista2<-html_nodes(webpage,"div")[[1]] ##css trovato
aa=html_text(artista2)
base_url=paste0("https://understat.com/team/",squadre[1],"/2018")
squadre2019=c("Lazio","Bologna","Napoli","Frosinone","Chievo","Torino",
          "Atalanta","Sassuolo","Cagliari","Udinese","Fiorentina","Genoa",
          "Inter","Empoli","Roma","Parma Calcio 1913","Sampdoria",
          "Juventus","SPAL 2013","AC Milan")

testo=strsplit(aa,split="x22")
testo[[1]][1]
arr=matrix(0,nrow=76,ncol=20)
k=0
for (el in squadre){
  k=k+1
  vettorxg=vector()
  base_url=paste0("https://understat.com/team/",el,"/2018")
  webpage <- read_html(base_url)  
  artista2<-html_nodes(webpage,"div")[[1]] ##css trovato
  aa=html_text(artista2)
  testo=strsplit(aa,split="x22")
  j=0
  for(i in 1:length(testo[[1]])){
    if (str_detect(testo[[1]][i],"^\\d\\.")){
      j=j+1
      vettorxg[j]=substring(testo[[1]][i],1,5)
    }
  }
  arr[,k]=vettorxg[1:76]
}
#####

##
arr[,2]
arr[,20]
#write.csv(arr,"arrayxG")


#inseriamo i dati
dataset_seriea <- read.csv("C:/Users/federico/Desktop/homework colloqui/dataset_seriea.csv")
dati2019=dataset_seriea[dataset_seriea$start_season=="2018-08-19",]
indicisquadre2019=data.frame(squadre=squadre2019,indici=c(1:20))
squadre2_2019=unique(dati2019$home_team_name)
squadre2_2019=factor(squadre2_2019)
squadre3_2019=squadre2019
squadre3_2019[16]="Parma"
squadre3_2019[19]="SPAL"
squadre3_2019[20]="Milan"
squadre3_2019[13]="Internazionale"

#cambio nomi 
tabella_2019=data.frame(squa2019=squadre3_2019,squascrap=squadre2019,indice=c(1:20))
#prova
i=39
squadra=as.character(dati2019$home_team_name[i])
indix=tabella$indice[tabella$squa2019==squadra]
giornata=dati2019$round[i]
xGcasa=arr[2*(giornata-1)+1,indix]
xGtrasferta=arr[2*(giornata-1)+2,indix]
dati2019[i,]

#cambiamenti
dati2019[dati2019$home_team_name=="Milan",c(3:7)]
dati2019[19,"round"]=2
dati2019[49,"round"]=4
dati2019[77,"round"]=7
dati2019[94,"round"]=9
dati2019[100,"round"]=10
#100 genoa
dati2019[dati2019$home_team_name=="Sampdoria",c(3:7)]
dati2019[25,"round"]=2
dati2019[39,"round"]=4
dati2019[dati2019$home_team_name=="Genoa",c(3:7)]
dati2019[13,"round"]=1
dati2019[34,"round"]=3
dati2019[52,"round"]=5
dati2019[79,"round"]=7
dati2019[95,"round"]=9
#firentina 39
dati2019[dati2019$home_team_name=="Fiorentina",c(3:7)]
dati2019[15,"round"]=1
dati2019[22,"round"]=2

dati2019[dati2019$round==27,c(3:7)]
dati2019[dati2019$home_team_name=="Internazionale",c(3:7)]

dati2019[dati2019$home_team_name=="Napoli",c(3:7)]

dati2019[dati2019$home_team_name=="Genoa",c(3:7)]
dati2019[dati2019$home_team_name=="Milan",c(3:7)]
dati2019[dati2019$round==27,c(3:7)]
dati2019[dati2019$home_team_name=="Juventus",c(3:7)]
dati2019[dati2019$home_team_name=="Udinese",c(3:7)]

dati2019[dati2019$round==27,c(3:7)]
dati2019[dati2019$home_team_name=="Chievo",c(3:7)]
dati2019[dati2019$home_team_name=="Milan",c(3:7)]

dati2019[dati2019$round==27,c(3:7)]
dati2019[dati2019$home_team_name=="Parma",c(3:7)]
dati2019[dati2019$home_team_name=="Genoa",c(3:7)]

dati2019[dati2019$round==27,c(3:7)]
dati2019[dati2019$home_team_name=="Sassuolo",c(3:7)]
dati2019[dati2019$home_team_name=="Napoli",c(3:7)]

dati2019[dati2019$round==27,c(3:7)]
dati2019[dati2019$home_team_name=="Sampdoria",c(3:7)]
dati2019[dati2019$home_team_name=="Atalanta",c(3:7)]

dati2019[dati2019$round==27,c(3:7)]
dati2019[dati2019$home_team_name=="Internazionale",c(3:7)]
dati2019[dati2019$home_team_name=="SPAL",c(3:7)]

dati2019[dati2019$round==27,c(3:7)]
dati2019[dati2019$home_team_name=="Frosinone",c(3:7)]
dati2019[dati2019$home_team_name=="Torino",c(3:7)]

dati2019[dati2019$round==27,c(3:7)]
dati2019[dati2019$home_team_name=="Bologna",c(3:7)]
dati2019[dati2019$home_team_name=="Cagliari",c(3:7)]

dati2019[dati2019$round==27,c(3:7)]
dati2019[dati2019$home_team_name=="Roma",c(3:7)]
dati2019[dati2019$home_team_name=="Empoli",c(3:7)]

dati2019[dati2019$round==27,c(3:7)]
dati2019[dati2019$home_team_name=="Fiorentina",c(3:7)]
dati2019[dati2019$home_team_name=="Lazio",c(3:7)]



# cambiare la lista in 1:38 -----------------------------------------------


##assegniamo xG ad ogni partita
xGcasa=vector()
xGtrasferta=vector()

###############
xGcasa=vector()
xGtrasferta=vector()
tabella2019=tabella_2019
arr2019=arr
##formula per calcolo
for (i in 1:nrow(dati2019)){
  squadra=as.character(dati2019$home_team_name[i])
  indix=tabella2019$indice[tabella2019$squa2019==squadra]
  giornata=dati2019$round[i]
  print(giornata)
  print(indix)
  print(i)
  xGcasa[i]=as.numeric(as.character(arr2019[2*(giornata-1)+1,indix]))
  xGtrasferta[i]=as.numeric(as.character(arr2019[2*(giornata-1)+2,indix]))
}




#####
dati2019xG=cbind(dati2019,xGcasa,xGtrasferta)
dati2019xG[dati2019xG$home_team_name=="Atalanta" | 
             dati2019xG$away_team_name=="Atalanta",c(3:7,16,17)]

dati2019xG[dati2019xG$home_team_name=="Udinese" | 
             dati2019xG$away_team_name=="Udinese",c(3:7,16,17)]

dati2019xG[dati2019xG$home_team_name=="Juventus" | 
             dati2019xG$away_team_name=="Juventus",c(3:7,16,17)]

dati2019xG[dati2019xG$home_team_name=="Milan" | 
             dati2019xG$away_team_name=="Milan",c(3:7,16,17)]

listasquadre=list()
listasquadre[[1]]
n=0
for (team in tabella2019$squa2019){
  n=n+1
  datitemp=dati2019xG[dati2019xG$home_team_name==team | 
                        dati2019xG$away_team_name==team,]
  listasquadre[[n]]=data.frame(giornata=1:37,xGcasa=datitemp$xGcasa,xGtrasferta=datitemp$xGtrasferta,
                               home_goals=datitemp$home_goals,away_goals=datitemp$away_goals,
                               home_team=as.character(datitemp$home_team_name),away_team=as.character(datitemp$away_team_name))}



squadra=dati2019$home_team_name[i]
indix=tabella2019$indice[tabella2019$squa2019==squadra]
giornata=dati2019$round[i]


dati2019xG$xGhomehome=0
dati2019xG$punti_homehome=0
dati2019xG[100,]
for (i in 1:nrow(dati2019xG)){
  if (dati2019xG$round[i]>=10){
    indice=tabella2019$indice[as.character(tabella2019$squa2019)==
                                as.character(dati2019xG$home_team_name[i])]
    gg=listasquadre[[indice]]$giornata[as.character(listasquadre[[indice]]$away_team)==as.character(dati2019xG$away_team_name[i])]
    j=seq(gg-6,gg-1,1)
    contxGcasa=0
    cont=0
    punti=0
    #listasquadre[[indice]]
    for (k in 1:nrow(listasquadre[[indice]])){
      if (listasquadre[[indice]]$giornata[k] %in% j & 
          as.character(listasquadre[[indice]]$home_team[k])==as.character(dati2019xG$home_team_name[i])){
        cont=cont+1
        print(as.numeric(as.character(listasquadre[[indice]]$xGcasa[k])))
        contxGcasa=contxGcasa+as.numeric(as.character(listasquadre[[indice]]$xGcasa[k]))
        punto=ifelse(listasquadre[[indice]]$home_goals[k]>listasquadre[[indice]]$away_goals[k],3,
                     ifelse(listasquadre[[indice]]$home_goals[k]==listasquadre[[indice]]$away_goals[k],1,0))
        punti=punti+punto
        
      }
      
    }
    contxGcasa=contxGcasa/cont
    dati2019xG$xGhomehome[i]=contxGcasa
    punti=punti/cont
    dati2019xG$punti_homehome[i]=punti
  }
}
dati2019xG[100,]

dati2019xG$xGawayaway=0
dati2019xG$punti_awayaway=0
for (i in 1:nrow(dati2019xG)){
  if (dati2019xG$round[i]>=10){
    indice=tabella2019$indice[as.character(tabella2019$squa2019)==
                                as.character(dati2019xG$away_team_name[i])]
    gg=listasquadre[[indice]]$giornata[as.character(listasquadre[[indice]]$home_team)==as.character(dati2019xG$home_team_name[i])]
    j=seq(gg-6,gg-1,1)
    contxGcasa=0
    cont=0
    punti=0
    #listasquadre[[indice]]
    for (k in 1:nrow(listasquadre[[indice]])){
      if (listasquadre[[indice]]$giornata[k] %in% j & 
          as.character(listasquadre[[indice]]$away_team[k])==as.character(dati2019xG$away_team_name[i])){
        cont=cont+1
        print(as.numeric(as.character(listasquadre[[indice]]$xGtrasferta[k])))
        contxGcasa=contxGcasa+as.numeric(as.character(listasquadre[[indice]]$xGtrasferta[k]))
        punto=ifelse(listasquadre[[indice]]$home_goals[k]>listasquadre[[indice]]$away_goals[k],0,
                     ifelse(listasquadre[[indice]]$home_goals[k]==listasquadre[[indice]]$away_goals[k],1,3))
        punti=punti+punto
      }
      
    }
    contxGcasa=contxGcasa/cont
    dati2019xG$xGawayaway[i]=contxGcasa
    punti=punti/cont
    dati2019xG$punti_awayaway[i]=punti
  }
}
dati2019xG[100,]

dati2019xG$xGhome_team_away=0
dati2019xG$punti_home_team_away=0
for (i in 1:nrow(dati2019xG)){
  if (dati2019xG$round[i]>=10){
    #team home=home
    indice=tabella2019$indice[as.character(tabella2019$squa2019)==
                                as.character(dati2019xG$home_team_name[i])]
    gg=listasquadre[[indice]]$giornata[as.character(listasquadre[[indice]]$away_team)==as.character(dati2019xG$away_team_name[i])]
    j=seq(gg-6,gg-1,1)
    contxGcasa=0
    cont=0
    punti=0
    #listasquadre[[indice]]
    for (k in 1:nrow(listasquadre[[indice]])){
      if (listasquadre[[indice]]$giornata[k] %in% j & 
          as.character(listasquadre[[indice]]$away_team[k])==as.character(dati2019xG$home_team_name[i])){
        cont=cont+1
        print(as.numeric(as.character(listasquadre[[indice]]$xGtrasferta[k])))
        contxGcasa=contxGcasa+as.numeric(as.character(listasquadre[[indice]]$xGtrasferta[k]))
        punto=ifelse(listasquadre[[indice]]$home_goals[k]>listasquadre[[indice]]$away_goals[k],0,
                     ifelse(listasquadre[[indice]]$home_goals[k]==listasquadre[[indice]]$away_goals[k],1,3))
        punti=punti+punto
      }
      
    }
    contxGcasa=contxGcasa/cont
    dati2019xG$xGhome_team_away[i]=contxGcasa
    punti=punti/cont
    dati2019xG$punti_home_team_away[i]=punti
  }
}
dati2019xG[100,]



dati2019xG$xGaway_team_home=0
dati2019xG$punti_away_team_home=0
for (i in 1:nrow(dati2019xG)){
  if (dati2019xG$round[i]>=10){
    indice=tabella2019$indice[as.character(tabella2019$squa2019)==
                                as.character(dati2019xG$away_team_name[i])]
    gg=listasquadre[[indice]]$giornata[as.character(listasquadre[[indice]]$home_team)==as.character(dati2019xG$home_team_name[i])]
    j=seq(gg-6,gg-1,1)
    contxGcasa=0
    cont=0
    punti=0
    #listasquadre[[indice]]
    for (k in 1:nrow(listasquadre[[indice]])){
      if (listasquadre[[indice]]$giornata[k] %in% j & 
          as.character(listasquadre[[indice]]$home_team[k])==as.character(dati2019xG$away_team_name[i])){
        cont=cont+1
        print(as.numeric(as.character(listasquadre[[indice]]$xGcasa[k])))
        contxGcasa=contxGcasa+as.numeric(as.character(listasquadre[[indice]]$xGcasa[k]))
        punto=ifelse(listasquadre[[indice]]$home_goals[k]>listasquadre[[indice]]$away_goals[k],3,
                     ifelse(listasquadre[[indice]]$home_goals[k]==listasquadre[[indice]]$away_goals[k],1,0))
        punti=punti+punto
      }
      
    }
    contxGcasa=contxGcasa/cont
    dati2019xG$xGaway_team_home[i]=contxGcasa
    punti=punti/cont
    dati2019xG$punti_away_team_home[i]=punti
  }
}
dati2019xG[100,]
dati2019xG$classificahome=0
dati2019xG$classificatrasf=0

for (i in 1:nrow(dati2019xG)){
  if (dati2019xG$round[i]>=10){
    indice=tabella2019$indice[as.character(tabella2019$squa2019)==
                                as.character(dati2019xG$home_team_name[i])]
    gg=listasquadre[[indice]]$giornata[as.character(listasquadre[[indice]]$away_team)==as.character(dati2019xG$away_team_name[i])]
    indice2=tabella2019$indice[as.character(tabella2019$squa2019)==
                                 as.character(dati2019xG$away_team_name[i])]
    gg2=listasquadre[[indice2]]$giornata[as.character(listasquadre[[indice2]]$home_team)
                                         ==as.character(dati2019xG$home_team_name[i])]
    
    j=seq(1,gg-1,1)
    j2=seq(1,gg2-1,1)
    puntic=0
    puntit=0
    for (k in 1:nrow(listasquadre[[indice]])){
      if (listasquadre[[indice]]$giornata[k] %in% j & 
          as.character(listasquadre[[indice]]$home_team[k])==as.character(dati2019xG$home_team_name[i])){
        punto=ifelse(listasquadre[[indice]]$home_goals[k]>listasquadre[[indice]]$away_goals[k],3,
                     ifelse(listasquadre[[indice]]$home_goals[k]==listasquadre[[indice]]$away_goals[k],1,0))
        puntic=puntic+punto }
      if (listasquadre[[indice]]$giornata[k] %in% j & 
          as.character(listasquadre[[indice]]$away_team[k])==as.character(dati2019xG$home_team_name[i])){
        punto=ifelse(listasquadre[[indice]]$home_goals[k]>listasquadre[[indice]]$away_goals[k],0,
                     ifelse(listasquadre[[indice]]$home_goals[k]==listasquadre[[indice]]$away_goals[k],1,3))
        puntic=puntic+punto }
    }
    for (k in 1:nrow(listasquadre[[indice]])){
      if (listasquadre[[indice2]]$giornata[k] %in% j2 & 
          as.character(listasquadre[[indice2]]$home_team[k])==as.character(dati2019xG$away_team_name[i])){
        punto=ifelse(listasquadre[[indice2]]$home_goals[k]>listasquadre[[indice2]]$away_goals[k],3,
                     ifelse(listasquadre[[indice2]]$home_goals[k]==listasquadre[[indice2]]$away_goals[k],1,0))
        puntit=puntit+punto }
      if (listasquadre[[indice2]]$giornata[k] %in% j2 & 
          as.character(listasquadre[[indice2]]$away_team[k])==as.character(dati2019xG$away_team_name[i])){
        punto=ifelse(listasquadre[[indice2]]$home_goals[k]>listasquadre[[indice2]]$away_goals[k],0,
                     ifelse(listasquadre[[indice2]]$home_goals[k]==listasquadre[[indice2]]$away_goals[k],1,3))
        puntit=puntit+punto }
    }
    dati2019xG$classificahome[i]=puntic
    dati2019xG$classificatrasf[i]=puntit
    
  }
}


dati2019xG[100,]


#prepariamo dati per previsioni

dati2019xG$risultato=0
dati2019xG$risultato=as.factor(ifelse(dati2019xG$home_goals>dati2019xG$away_goals,"1",
                                      ifelse(dati2019xG$home_goals==dati2019xG$away_goals,"X","2"))
)

traindata2019=dati2019xG %>% select(c("round","xGhomehome","xGawayaway",
                                      "xGhome_team_away","xGaway_team_home",
                                      "punti_homehome","punti_awayaway", "punti_home_team_away",
                                      "punti_away_team_home", "classificahome","classificatrasf","risultato"))
train2019=traindata2019[traindata2019$round>=10 & traindata2019$round<19,]
test2019=traindata2019[traindata2019$round>=19,]
colnames(train)
str(train)
train2019[70:80,"risultato"]

dati2019xG[100,]
dati2017xG[10,]


write.csv2(dati2019xG,"dati2019.csv")

#####


# adesso dobbiamo vedere altri anni ---------------------------------------

dati_train=rbind(train2017,train2016,train2019,test2016,test2017)
dati_test=test2019
colnames(dati_train)
sum(is.na(dati_test))
str(dati_test)

summary(dati_train)
str(dati_train)
str(dati_test)
