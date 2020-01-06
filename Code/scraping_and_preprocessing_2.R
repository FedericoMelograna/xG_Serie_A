

#####


# adesso dobbiamo vedere altri anni ---------------------------------------
squadre2017=c("Lazio","Bologna","Napoli","Chievo","Torino",
              "Atalanta","Sassuolo","Cagliari","Udinese","Fiorentina","Genoa",
              "Inter","Roma","Sampdoria",
              "Juventus","SPAL 2013","AC Milan","Verona","Benevento","Crotone")

# testo2017=strsplit(aa,split="x22")
# testo[[1]][1]
arr2017=matrix(0,nrow=76,ncol=20)
k=0
#reading pagine
for (el in squadre2017){
  k=k+1
  vettorxg=vector()
  base_url=paste0("https://understat.com/team/",el,"/2017")
  webpage <- read_html(base_url)  
  artista2<-html_nodes(webpage,"div")[[1]] ##css trovato
  aa=html_text(artista2)
  testo=strsplit(aa,split="x22")
  j=0
  for(i in 1:length(testo[[1]])){
    if (str_detect(testo[[1]][i],"^\\d\\.") ){
      j=j+1
      vettorxg[j]=substring(testo[[1]][i],1,5)
    }
  }
  arr2017[,k]=vettorxg[1:76]
}

#prendiamo dati che ci interessano
dati2017=dataset_seriea[dataset_seriea$start_season=="2017-08-19",]
indicisquadre2017=data.frame(squadre=squadre2017,indici=c(1:20))
squadre2017_2=unique(dati2017$home_team_name)
squadre2017_2=factor(squadre2017_2)
squadre2017_3=squadre2017
squadre2017_3[18]="Hellas Verona"
squadre2017_3[16]="SPAL"
squadre2017_3[17]="Milan"
squadre2017_3[12]="Internazionale"

#cambio nomi 
tabella2017=data.frame(squa2017=squadre2017_3,squascrap=squadre2017,indice=c(1:20))
#prova
#fixing inter de merda
temp=arr2017[arr2017[,12]!=0,12]
arr2017[65,12]
tempi=arr2017[66:75,12]
ar=c(arr2017[1:65,12],"0",arr2017[66:75,12])
arr2017[,12]=ar

tempi=arr2017[66:75,8]
ar=c(arr2017[1:65,8],"0",arr2017[66:75,8])
arr2017[,8]=ar


i=39
squadra=as.character(dati2017$home_team_name[i])
indix=tabella20172017$indice[tabella20172017$squa2017==squadra]
giornata=dati2017$round[i]
xGcasa=arr2017[2*(giornata-1)+1,indix]
xGtrasferta=arr2017[2*(giornata-1)+2,indix]
dati2017[i,]

arr2017[,indix]

arr2017[,8]
#27-30esima
rbind(dati2017xG[dati2017xG$home_team_name=="Torino",c(3:7,16,17)],
      dati2017xG[dati2017xG$away_team_name=="Torino",c(3:7,16,17)])
dati2017[dati2017$home_team_name=="Torino",c(3:7)]
dataset_seriea[853,"round"]=28
#incriminata
dataset_seriea[831,"round"]=30
dati2017[dati2017$round==27,]
dati2017[dati2017$home_team_name=="Crotone",c(3:7)]
dataset_seriea[863,"round"]=27
dataset_seriea[854,"round"]=28
dati2017[dati2017$round==27,]
dati2017[dati2017$home_team_name=="Milan",c(3:7)]
dataset_seriea[832,"round"]=30
dataset_seriea[852,"round"]=28
dati2017[dati2017$home_team_name=="Internazionale",c(3:7)]
dataset_seriea[843,"round"]=29
dataset_seriea[859,"round"]=27
dati2017[dati2017$round==27,]
dati2017[dati2017$home_team_name=="Chievo",c(3:7)]
dataset_seriea[833,"round"]=30
dataset_seriea[840,"round"]=29
dati2017[dati2017$home_team_name=="Sassuolo",c(3:7)]
dataset_seriea[839,"round"]=29
dataset_seriea[864,"round"]=27
dati2017[dati2017$round==27,]
dati2017[dati2017$home_team_name=="Benevento",c(3:7)]
dataset_seriea[834,"round"]=30
dataset_seriea[851,"round"]=28
dati2017[dati2017$home_team_name=="Hellas Verona",c(3:7)]
dataset_seriea[867,"round"]=27
dataset_seriea[850,"round"]=28
dati2017[dati2017$round==27,]
dati2017[dati2017$home_team_name=="Genoa",c(3:7)]
dataset_seriea[835,"round"]=30
dataset_seriea[844,"round"]=29
dataset_seriea[860,"round"]=27
dati2017[dati2017$home_team_name=="Cagliari",c(3:7)]
dataset_seriea[862,"round"]=27
dataset_seriea[845,"round"]=29
dati2017[dati2017$round==27,]
#12-21
dati2017[dati2017$home_team_name=="Udinese",c(3:7)]
dataset_seriea[836,"round"]=30
dataset_seriea[857,"round"]=28
dataset_seriea[930,"round"]=20
dataset_seriea[959,"round"]=17
dataset_seriea[979,"round"]=15
dataset_seriea[998,"round"]=13
dataset_seriea[1009,"round"]=12
dati2017[dati2017$home_team_name=="Fiorentina",c(3:7)]
dataset_seriea[842,"round"]=29
dataset_seriea[866,"round"]=27
dati2017[dati2017$round==27,]
dati2017[dati2017$home_team_name=="Atalanta",c(3:7)]
dataset_seriea[837,"round"]=30
dataset_seriea[846,"round"]=29
dati2017[dati2017$home_team_name=="Sampdoria",c(3:7)]
dataset_seriea[855,"round"]=28
dataset_seriea[921,"round"]=21
dataset_seriea[929,"round"]=20
dataset_seriea[949,"round"]=18
dataset_seriea[968,"round"]=16
dataset_seriea[985,"round"]=14
dataset_seriea[1006,"round"]=12
dataset_seriea[1026,"round"]=10
dataset_seriea[1051,"round"]=8
dataset_seriea[1057,"round"]=7
dataset_seriea[1078,"round"]=5
#3-21
dati2017[dati2017$home_team_name=="Roma",c(3:7)]
dataset_seriea[934,"round"]=19
dataset_seriea[948,"round"]=18
dataset_seriea[970,"round"]=16
dataset_seriea[992,"round"]=14
dataset_seriea[1012,"round"]=12
dataset_seriea[1030,"round"]=10
dataset_seriea[1038,"round"]=9
dataset_seriea[1060,"round"]=7
dataset_seriea[1081,"round"]=5
dataset_seriea[1099,"round"]=3
dati2017[dati2017$home_team_name=="Napoli",c(3:7)]
dati2017[dati2017$home_team_name=="Juventus",c(3:7)]
dataset_seriea[858,"round"]=28
dataset_seriea[865,"round"]=27

dati2017[dati2017$home_team_name=="Lazio",c(3:7)]
dataset_seriea[922,"round"]=21
dataset_seriea[927,"round"]=20
dataset_seriea[960,"round"]=17
dataset_seriea[974,"round"]=15
dataset_seriea[995,"round"]=13
dati2017[dati2017$round==27,]
dati2017[dati2017$home_team_name=="SPAL",c(3:7)]
dati2017[dati2017$home_team_name=="Bologna",c(3:7)]
dati2017=dataset_seriea[dataset_seriea$start_season=="2017-08-19",]

##assegniamo xG ad ogni partita
xGcasa=vector()
xGtrasferta=vector()
##formula per calcolo
for (i in 1:nrow(dati2017)){
  squadra=as.character(dati2017$home_team_name[i])
  indix=tabella2017$indice[tabella2017$squa2017==squadra]
  giornata=dati2017$round[i]
  print(giornata)
  print(indix)
  print(i)
  xGcasa[i]=as.numeric(as.character(arr2017[2*(giornata-1)+1,indix]))
  xGtrasferta[i]=as.numeric(as.character(arr2017[2*(giornata-1)+2,indix]))
}


# COLPA DI ASTORI!! -------------------------------------------------------


#####
dati2017xG=cbind(dati2017,xGcasa,xGtrasferta)
dati2017xG[dati2017xG$home_team_name=="Atalanta" | 
             dati2017xG$away_team_name=="Atalanta",c(3:7,16,17)]

dati2017xG[dati2017xG$home_team_name=="Udinese" | 
             dati2017xG$away_team_name=="Udinese",c(3:7,16,17)]

dati2017xG[dati2017xG$home_team_name=="Juventus" | 
             dati2017xG$away_team_name=="Juventus",c(3:7,16,17)]

n=0
for (el in squadre){
  el=vector()
}
listasquadre=list()
listasquadre[[1]]="ciao"
listasquadre[[2]]=data.frame(ciao="ciao",bi="bi")
n=0
for (team in tabella2017$squa2017){
  n=n+1
  datitemp=dati2017xG[dati2017xG$home_team_name==team | 
               dati2017xG$away_team_name==team,]
  listasquadre[[n]]=data.frame(giornata=38:1,xGcasa=datitemp$xGcasa,xGtrasferta=datitemp$xGtrasferta,
                          home_goals=datitemp$home_goals,away_goals=datitemp$away_goals,
                          home_team=as.character(datitemp$home_team_name),away_team=as.character(datitemp$away_team_name))}
  


squadra=dati2017$home_team_name[i]
indix=tabella2017$indice[tabella2017$squa2017==squadra]
giornata=dati2017$round[i]
punto=ifelse(as.numeric(as.character(dati2017xG$home_goals[k]))>as.numeric(as.character(dati2017xG$away_goals[k])),3,
             ifelse(as.numeric(as.character(dati2017xG$home_goals[k]))==as.numeric(as.character(dati2017xG$away_goals[k])),1,0))
punti=punti+punto
}

}
punti=punti/cont
dati2017xG$punti_homehome[i]=punti

dati2017xG$xGhomehome=0
dati2017xG$punti_homehome=0
dati2017xG[i,]
for (i in 1:nrow(dati2017xG)){
  if (dati2017xG$round[i]>=10){
    indice=tabella2017$indice[as.character(tabella2017$squa2017)==
                                as.character(dati2017xG$home_team_name[i])]
    gg=listasquadre[[indice]]$giornata[as.character(listasquadre[[indice]]$away_team)==as.character(dati2017xG$away_team_name[i])]
    j=seq(gg-6,gg-1,1)
    contxGcasa=0
    cont=0
    punti=0
    #listasquadre[[indice]]
    for (k in 1:nrow(listasquadre[[indice]])){
      if (listasquadre[[indice]]$giornata[k] %in% j & 
          as.character(listasquadre[[indice]]$home_team[k])==as.character(dati2017xG$home_team_name[i])){
        cont=cont+1
        print(as.numeric(as.character(listasquadre[[indice]]$xGcasa[k])))
        contxGcasa=contxGcasa+as.numeric(as.character(listasquadre[[indice]]$xGcasa[k]))
        punto=ifelse(listasquadre[[indice]]$home_goals[k]>listasquadre[[indice]]$away_goals[k],3,
                     ifelse(listasquadre[[indice]]$home_goals[k]==listasquadre[[indice]]$away_goals[k],1,0))
        punti=punti+punto
        
        }
      
    }
    contxGcasa=contxGcasa/cont
    dati2017xG$xGhomehome[i]=contxGcasa
    punti=punti/cont
    dati2017xG$punti_homehome[i]=punti
  }
}
dati2017xG[100,]

dati2017xG$xGawayaway=0
dati2017xG$punti_awayaway=0
for (i in 1:nrow(dati2017xG)){
  if (dati2017xG$round[i]>=10){
    indice=tabella2017$indice[as.character(tabella2017$squa2017)==
                                as.character(dati2017xG$away_team_name[i])]
    gg=listasquadre[[indice]]$giornata[as.character(listasquadre[[indice]]$home_team)==as.character(dati2017xG$home_team_name[i])]
    j=seq(gg-6,gg-1,1)
    contxGcasa=0
    cont=0
    punti=0
    #listasquadre[[indice]]
    for (k in 1:nrow(listasquadre[[indice]])){
      if (listasquadre[[indice]]$giornata[k] %in% j & 
          as.character(listasquadre[[indice]]$away_team[k])==as.character(dati2017xG$away_team_name[i])){
        cont=cont+1
        print(as.numeric(as.character(listasquadre[[indice]]$xGtrasferta[k])))
        contxGcasa=contxGcasa+as.numeric(as.character(listasquadre[[indice]]$xGtrasferta[k]))
        punto=ifelse(listasquadre[[indice]]$home_goals[k]>listasquadre[[indice]]$away_goals[k],0,
                     ifelse(listasquadre[[indice]]$home_goals[k]==listasquadre[[indice]]$away_goals[k],1,3))
        punti=punti+punto
      }
      
    }
    contxGcasa=contxGcasa/cont
    dati2017xG$xGawayaway[i]=contxGcasa
    punti=punti/cont
    dati2017xG$punti_awayaway[i]=punti
  }
}
dati2017xG[100,]


dati2017xG[100,c(4:8,15:19)]


dati2017xG$xGhome_team_away=0
dati2017xG$punti_home_team_away=0
for (i in 1:nrow(dati2017xG)){
  if (dati2017xG$round[i]>=10){
    #team home=home
    indice=tabella2017$indice[as.character(tabella2017$squa2017)==
                                as.character(dati2017xG$home_team_name[i])]
    gg=listasquadre[[indice]]$giornata[as.character(listasquadre[[indice]]$away_team)==as.character(dati2017xG$away_team_name[i])]
    j=seq(gg-6,gg-1,1)
    contxGcasa=0
    cont=0
    punti=0
    #listasquadre[[indice]]
    for (k in 1:nrow(listasquadre[[indice]])){
      if (listasquadre[[indice]]$giornata[k] %in% j & 
          as.character(listasquadre[[indice]]$away_team[k])==as.character(dati2017xG$home_team_name[i])){
        cont=cont+1
        print(as.numeric(as.character(listasquadre[[indice]]$xGtrasferta[k])))
        contxGcasa=contxGcasa+as.numeric(as.character(listasquadre[[indice]]$xGtrasferta[k]))
        punto=ifelse(listasquadre[[indice]]$home_goals[k]>listasquadre[[indice]]$away_goals[k],0,
                     ifelse(listasquadre[[indice]]$home_goals[k]==listasquadre[[indice]]$away_goals[k],1,3))
        punti=punti+punto
      }
      
    }
    contxGcasa=contxGcasa/cont
    dati2017xG$xGhome_team_away[i]=contxGcasa
    punti=punti/cont
    dati2017xG$punti_home_team_away[i]=punti
  }
}
dati2017xG[100,]



dati2017xG$xGaway_team_home=0
dati2017xG$punti_away_team_home=0
for (i in 1:nrow(dati2017xG)){
  if (dati2017xG$round[i]>=10){
    indice=tabella2017$indice[as.character(tabella2017$squa2017)==
                                as.character(dati2017xG$away_team_name[i])]
    gg=listasquadre[[indice]]$giornata[as.character(listasquadre[[indice]]$home_team)==as.character(dati2017xG$home_team_name[i])]
    j=seq(gg-6,gg-1,1)
    contxGcasa=0
    cont=0
    punti=0
    #listasquadre[[indice]]
    for (k in 1:nrow(listasquadre[[indice]])){
      if (listasquadre[[indice]]$giornata[k] %in% j & 
          as.character(listasquadre[[indice]]$home_team[k])==as.character(dati2017xG$away_team_name[i])){
        cont=cont+1
        print(as.numeric(as.character(listasquadre[[indice]]$xGcasa[k])))
        contxGcasa=contxGcasa+as.numeric(as.character(listasquadre[[indice]]$xGcasa[k]))
        punto=ifelse(listasquadre[[indice]]$home_goals[k]>listasquadre[[indice]]$away_goals[k],3,
                     ifelse(listasquadre[[indice]]$home_goals[k]==listasquadre[[indice]]$away_goals[k],1,0))
        punti=punti+punto
      }
      
    }
    contxGcasa=contxGcasa/cont
    dati2017xG$xGaway_team_home[i]=contxGcasa
    punti=punti/cont
    dati2017xG$punti_away_team_home[i]=punti
  }
}
dati2017xG[100,]
dati2017xG$classificahome=0
dati2017xG$classificatrasf=0

for (i in 1:nrow(dati2017xG)){
  if (dati2017xG$round[i]>=10){
    indice=tabella2017$indice[as.character(tabella2017$squa2017)==
                                as.character(dati2017xG$home_team_name[i])]
    gg=listasquadre[[indice]]$giornata[as.character(listasquadre[[indice]]$away_team)==as.character(dati2017xG$away_team_name[i])]
    indice2=tabella2017$indice[as.character(tabella2017$squa2017)==
                                 as.character(dati2017xG$away_team_name[i])]
    gg2=listasquadre[[indice2]]$giornata[as.character(listasquadre[[indice2]]$home_team)
                                       ==as.character(dati2017xG$home_team_name[i])]
    
    j=seq(1,gg-1,1)
    j2=seq(1,gg2-1,1)
    puntic=0
    puntit=0
    for (k in 1:nrow(listasquadre[[indice]])){
      if (listasquadre[[indice]]$giornata[k] %in% j & 
          as.character(listasquadre[[indice]]$home_team[k])==as.character(dati2017xG$home_team_name[i])){
        punto=ifelse(listasquadre[[indice]]$home_goals[k]>listasquadre[[indice]]$away_goals[k],3,
                     ifelse(listasquadre[[indice]]$home_goals[k]==listasquadre[[indice]]$away_goals[k],1,0))
        puntic=puntic+punto }
      if (listasquadre[[indice]]$giornata[k] %in% j & 
          as.character(listasquadre[[indice]]$away_team[k])==as.character(dati2017xG$home_team_name[i])){
        punto=ifelse(listasquadre[[indice]]$home_goals[k]>listasquadre[[indice]]$away_goals[k],0,
                     ifelse(listasquadre[[indice]]$home_goals[k]==listasquadre[[indice]]$away_goals[k],1,3))
        puntic=puntic+punto }
    }
      for (k in 1:nrow(listasquadre[[indice]])){
        if (listasquadre[[indice2]]$giornata[k] %in% j2 & 
            as.character(listasquadre[[indice2]]$home_team[k])==as.character(dati2017xG$away_team_name[i])){
          punto=ifelse(listasquadre[[indice2]]$home_goals[k]>listasquadre[[indice2]]$away_goals[k],3,
                       ifelse(listasquadre[[indice2]]$home_goals[k]==listasquadre[[indice2]]$away_goals[k],1,0))
          puntit=puntit+punto }
        if (listasquadre[[indice2]]$giornata[k] %in% j2 & 
            as.character(listasquadre[[indice2]]$away_team[k])==as.character(dati2017xG$away_team_name[i])){
          punto=ifelse(listasquadre[[indice2]]$home_goals[k]>listasquadre[[indice2]]$away_goals[k],0,
                       ifelse(listasquadre[[indice2]]$home_goals[k]==listasquadre[[indice2]]$away_goals[k],1,3))
          puntit=puntit+punto }
      }
    dati2017xG$classificahome[i]=puntic
    dati2017xG$classificatrasf[i]=puntit
    
  }
}


dati2017xG[100,]


#prepariamo dati per previsioni

dati2017xG$risultato=0
dati2017xG$risultato=as.factor(ifelse(dati2017xG$home_goals>dati2017xG$away_goals,"1",
                                      ifelse(dati2017xG$home_goals==dati2017xG$away_goals,"X","2"))
)

traindata2017=dati2017xG %>% select(c("round","xGhomehome","xGawayaway",
                                  "xGhome_team_away","xGaway_team_home",
                                  "punti_homehome","punti_awayaway", "punti_home_team_away",
                                  "punti_away_team_home", "classificahome","classificatrasf","risultato"))
train2017=traindata2017[traindata2017$round>=10 & traindata2017$round<29,]
test2017=traindata2017[traindata2017$round>=29,]
colnames(train)
str(train)
train2017[90:100,"risultato"]

dati2017xG[100,]




#####


# adesso dobbiamo vedere altri anni ---------------------------------------

