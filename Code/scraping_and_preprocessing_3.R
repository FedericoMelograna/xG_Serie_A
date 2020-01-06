

#####


# adesso dobbiamo vedere altri anni ---------------------------------------
dataset_seriea <- read.csv("C:/Users/federico/Desktop/homework colloqui/dataset_seriea.csv")

base_url <- "https://understat.com/team/Lazio/2018"
webpage <- read_html(base_url)  
artista2<-html_nodes(webpage,"div")[[1]] ##css trovato
aa=html_text(artista2)
base_url=paste0("https://understat.com/team/",squadre[1],"/2018")
squadre2016=c("Lazio","Bologna","Napoli","Chievo","Torino",
              "Atalanta","Sassuolo","Cagliari","Udinese","Fiorentina","Genoa",
              "Inter","Roma","Sampdoria",
              "Juventus","AC Milan","Crotone"
              ,"Pescara","Palermo","Empoli")

# testo2016=strsplit(aa,split="x22")
# testo[[1]][1]
arr2016=matrix(0,nrow=76,ncol=20)
k=0
for (el in squadre2016){
  k=k+1
  vettorxg=vector()
  base_url=paste0("https://understat.com/team/",el,"/2016")
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
  arr2016[,k]=vettorxg[1:76]
}



dati2016=dataset_seriea[dataset_seriea$start_season=="2016-08-20",]
indicisquadre2016=data.frame(squadre=squadre2016,indici=c(1:20))
squadre2016_2=unique(dati2016$home_team_name)
squadre2016_2=factor(squadre2016_2)
squadre2016_3=squadre2016
squadre2016_3[16]="Milan"
squadre2016_3[12]="Internazionale"

#cambio nomi 
tabella2016=data.frame(squa2016=squadre2016_3,squascrap=squadre2016,indice=c(1:20))
#prova
#fixing inter de merda
# temp=arr2016[arr2016[,12]!=0,12]
# arr2016[65,12]
# tempi=arr2016[66:75,12]
# ar=c(arr2016[1:65,12],"0",arr2016[66:75,12])
# arr2016[,12]=ar

# tempi=arr2016[66:75,8]
# ar=c(arr2016[1:65,8],"0",arr2016[66:75,8])
# arr2016[,8]=ar

dati2016[dati2016$round==27,]
#18-23
dati2016[dati2016$home_team_name=="Milan",c(3:7)]
dataset_seriea[531,"round"]=22
dataset_seriea[552,"round"]=20
dataset_seriea[565,"round"]=18
dati2016[dati2016$home_team_name=="Bologna",c(3:7)]
dataset_seriea[521,"round"]=23
dataset_seriea[532,"round"]=22
dataset_seriea[549,"round"]=20
dati2016[dati2016$home_team_name=="Lazio",c(3:7)]
dati2016[dati2016$round==27,]
dati2016[dati2016$home_team_name=="Empoli",c(3:7)]
dati2016[dati2016$home_team_name=="Genoa",c(3:7)]
#3-16
dataset_seriea[591,"round"]=16

dataset_seriea[618,"round"]=13
dataset_seriea[636,"round"]=11
dataset_seriea[661,"round"]=9
dataset_seriea[674,"round"]=7
dataset_seriea[695,"round"]=5
dataset_seriea[705,"round"]=4
dati2016[dati2016$home_team_name=="Fiorentina",c(3:7)]
dati2016[dati2016$away_team_name=="Fiorentina",c(3:7)]
#19-22
dataset_seriea[537,"round"]=21
dataset_seriea[555,"round"]=19
#3-16
dataset_seriea[593,"round"]=15
dataset_seriea[604,"round"]=14
dataset_seriea[633,"round"]=11
dataset_seriea[654,"round"]=9
dataset_seriea[678,"round"]=7
dataset_seriea[694,"round"]=5
dataset_seriea[712,"round"]=3
dati2016[dati2016$round==27,]
dati2016[dati2016$home_team_name=="Cagliari",c(3:7)]
dati2016[dati2016$home_team_name=="Internazionale",c(3:7)]
dati2016[dati2016$round==27,]
dati2016[dati2016$home_team_name=="Udinese",c(3:7)]
#18-23
dati2016[dati2016$home_team_name=="Juventus",c(3:7)]
dataset_seriea[524,"round"]=22
dataset_seriea[551,"round"]=20
dataset_seriea[564,"round"]=18
dati2016[dati2016$round==27,]
dati2016[dati2016$home_team_name=="Torino",c(3:7)]
dati2016[dati2016$home_team_name=="Palermo",c(3:7)]
dati2016[dati2016$round==27,]
#18-23
dati2016[dati2016$home_team_name=="Crotone",c(3:7)]
dataset_seriea[522,"round"]=23
dataset_seriea[536,"round"]=21
dataset_seriea[563,"round"]=19

dati2016[dati2016$home_team_name=="Sassuolo",c(3:7)]
dati2016[dati2016$round==27,]
dati2016[dati2016$home_team_name=="Atalanta",c(3:7)]
dati2016[dati2016$round==27,]
dati2016[dati2016$home_team_name=="Chievo",c(3:7)]
dati2016[dati2016$round==27,]
dati2016[dati2016$home_team_name=="Sampdoria",c(3:7)]

#19-22
dati2016[dati2016$home_team_name=="Pescara",c(3:7)]
dataset_seriea[533,"round"]=22
dataset_seriea[548,"round"]=20
dati2016[dati2016$round==27,]
dati2016[dati2016$home_team_name=="Roma",c(3:7)]

dati2016[dati2016$home_team_name=="Napoli",c(3:7)]
#####
dati2016=dataset_seriea[dataset_seriea$start_season=="2016-08-20",]

##assegniamo xG ad ogni partita
xGcasa=vector()
xGtrasferta=vector()
##formula per calcolo
i=248
for (i in 1:nrow(dati2016)){
  #if (dati2016$home_team_name[i]=="Genoa" & dati2016$away_team_name[i]=="Juventus"){print(i)}

  squadra=as.character(dati2016$home_team_name[i])
  indix=tabella2016$indice[tabella2016$squa2016==squadra]
  giornata=dati2016$round[i]
#  print(giornata)
#  print(indix)
 # print(i)
  xGcasa[i]=as.numeric(as.character(arr2016[2*(giornata-1)+1,indix]))
  xGtrasferta[i]=as.numeric(as.character(arr2016[2*(giornata-1)+2,indix]))
}


# COLPA DI ASTORI!! -------------------------------------------------------


#####
dati2016xG=cbind(dati2016,xGcasa,xGtrasferta)
dati2016xG[dati2016xG$home_team_name=="Atalanta" | 
             dati2016xG$away_team_name=="Atalanta",c(3:7,16,17)]

dati2016xG[dati2016xG$home_team_name=="Udinese" | 
             dati2016xG$away_team_name=="Udinese",c(3:7,16,17)]

dati2016xG[dati2016xG$home_team_name=="Juventus" | 
             dati2016xG$away_team_name=="Juventus",c(3:7,16,17)]

listasquadre=list()
n=0
for (team in tabella2016$squa2016){
  n=n+1
  datitemp=dati2016xG[dati2016xG$home_team_name==team | 
                        dati2016xG$away_team_name==team,]
  listasquadre[[n]]=data.frame(giornata=38:1,xGcasa=datitemp$xGcasa,xGtrasferta=datitemp$xGtrasferta,
                               home_goals=datitemp$home_goals,away_goals=datitemp$away_goals,
                               home_team=as.character(datitemp$home_team_name),away_team=as.character(datitemp$away_team_name))}

listasquadre[[1]]


dati2016xG$xGhomehome=0
dati2016xG$punti_homehome=0
dati2016xG[i,]
for (i in 1:nrow(dati2016xG)){
  if (dati2016xG$round[i]>=10){
    indice=tabella2016$indice[as.character(tabella2016$squa2016)==
                                as.character(dati2016xG$home_team_name[i])]
    gg=listasquadre[[indice]]$giornata[as.character(listasquadre[[indice]]$away_team)==as.character(dati2016xG$away_team_name[i])]
    j=seq(gg-6,gg-1,1)
    contxGcasa=0
    cont=0
    punti=0
    #listasquadre[[indice]]
    for (k in 1:nrow(listasquadre[[indice]])){
      if (listasquadre[[indice]]$giornata[k] %in% j & 
          as.character(listasquadre[[indice]]$home_team[k])==as.character(dati2016xG$home_team_name[i])){
        cont=cont+1
        print(as.numeric(as.character(listasquadre[[indice]]$xGcasa[k])))
        contxGcasa=contxGcasa+as.numeric(as.character(listasquadre[[indice]]$xGcasa[k]))
        punto=ifelse(listasquadre[[indice]]$home_goals[k]>listasquadre[[indice]]$away_goals[k],3,
                     ifelse(listasquadre[[indice]]$home_goals[k]==listasquadre[[indice]]$away_goals[k],1,0))
        punti=punti+punto
        
      }
      
    }
    contxGcasa=contxGcasa/cont
    dati2016xG$xGhomehome[i]=contxGcasa
    punti=punti/cont
    dati2016xG$punti_homehome[i]=punti
  }
}
dati2016xG[100,]

dati2016xG$xGawayaway=0
dati2016xG$punti_awayaway=0
for (i in 1:nrow(dati2016xG)){
  if (dati2016xG$round[i]>=10){
    indice=tabella2016$indice[as.character(tabella2016$squa2016)==
                                as.character(dati2016xG$away_team_name[i])]
    gg=listasquadre[[indice]]$giornata[as.character(listasquadre[[indice]]$home_team)==as.character(dati2016xG$home_team_name[i])]
    j=seq(gg-6,gg-1,1)
    contxGcasa=0
    cont=0
    punti=0
    #listasquadre[[indice]]
    for (k in 1:nrow(listasquadre[[indice]])){
      if (listasquadre[[indice]]$giornata[k] %in% j & 
          as.character(listasquadre[[indice]]$away_team[k])==as.character(dati2016xG$away_team_name[i])){
        cont=cont+1
        print(as.numeric(as.character(listasquadre[[indice]]$xGtrasferta[k])))
        contxGcasa=contxGcasa+as.numeric(as.character(listasquadre[[indice]]$xGtrasferta[k]))
        punto=ifelse(listasquadre[[indice]]$home_goals[k]>listasquadre[[indice]]$away_goals[k],0,
                     ifelse(listasquadre[[indice]]$home_goals[k]==listasquadre[[indice]]$away_goals[k],1,3))
        punti=punti+punto
      }
      
    }
    contxGcasa=contxGcasa/cont
    dati2016xG$xGawayaway[i]=contxGcasa
    punti=punti/cont
    dati2016xG$punti_awayaway[i]=punti
  }
}
dati2016xG[100,]


dati2016xG[100,c(4:8,15:19)]


dati2016xG$xGhome_team_away=0
dati2016xG$punti_home_team_away=0
for (i in 1:nrow(dati2016xG)){
  if (dati2016xG$round[i]>=10){
    #team home=home
    indice=tabella2016$indice[as.character(tabella2016$squa2016)==
                                as.character(dati2016xG$home_team_name[i])]
    gg=listasquadre[[indice]]$giornata[as.character(listasquadre[[indice]]$away_team)==as.character(dati2016xG$away_team_name[i])]
    j=seq(gg-6,gg-1,1)
    contxGcasa=0
    cont=0
    punti=0
    #listasquadre[[indice]]
    for (k in 1:nrow(listasquadre[[indice]])){
      if (listasquadre[[indice]]$giornata[k] %in% j & 
          as.character(listasquadre[[indice]]$away_team[k])==as.character(dati2016xG$home_team_name[i])){
        cont=cont+1
        print(as.numeric(as.character(listasquadre[[indice]]$xGtrasferta[k])))
        contxGcasa=contxGcasa+as.numeric(as.character(listasquadre[[indice]]$xGtrasferta[k]))
        punto=ifelse(listasquadre[[indice]]$home_goals[k]>listasquadre[[indice]]$away_goals[k],0,
                     ifelse(listasquadre[[indice]]$home_goals[k]==listasquadre[[indice]]$away_goals[k],1,3))
        punti=punti+punto
      }
      
    }
    contxGcasa=contxGcasa/cont
    dati2016xG$xGhome_team_away[i]=contxGcasa
    punti=punti/cont
    dati2016xG$punti_home_team_away[i]=punti
  }
}
dati2016xG[100,]



dati2016xG$xGaway_team_home=0
dati2016xG$punti_away_team_home=0
for (i in 1:nrow(dati2016xG)){
  if (dati2016xG$round[i]>=10){
    indice=tabella2016$indice[as.character(tabella2016$squa2016)==
                                as.character(dati2016xG$away_team_name[i])]
    gg=listasquadre[[indice]]$giornata[as.character(listasquadre[[indice]]$home_team)==as.character(dati2016xG$home_team_name[i])]
    j=seq(gg-6,gg-1,1)
    contxGcasa=0
    cont=0
    punti=0
    #listasquadre[[indice]]
    for (k in 1:nrow(listasquadre[[indice]])){
      if (listasquadre[[indice]]$giornata[k] %in% j & 
          as.character(listasquadre[[indice]]$home_team[k])==as.character(dati2016xG$away_team_name[i])){
        cont=cont+1
        print(as.numeric(as.character(listasquadre[[indice]]$xGcasa[k])))
        contxGcasa=contxGcasa+as.numeric(as.character(listasquadre[[indice]]$xGcasa[k]))
        punto=ifelse(listasquadre[[indice]]$home_goals[k]>listasquadre[[indice]]$away_goals[k],3,
                     ifelse(listasquadre[[indice]]$home_goals[k]==listasquadre[[indice]]$away_goals[k],1,0))
        punti=punti+punto
      }
      
    }
    contxGcasa=contxGcasa/cont
    dati2016xG$xGaway_team_home[i]=contxGcasa
    punti=punti/cont
    dati2016xG$punti_away_team_home[i]=punti
  }
}
dati2016xG[100,]
dati2016xG$classificahome=0
dati2016xG$classificatrasf=0

for (i in 1:nrow(dati2016xG)){
  if (dati2016xG$round[i]>=10){
    indice=tabella2016$indice[as.character(tabella2016$squa2016)==
                                as.character(dati2016xG$home_team_name[i])]
    gg=listasquadre[[indice]]$giornata[as.character(listasquadre[[indice]]$away_team)==as.character(dati2016xG$away_team_name[i])]
    indice2=tabella2016$indice[as.character(tabella2016$squa2016)==
                                 as.character(dati2016xG$away_team_name[i])]
    gg2=listasquadre[[indice2]]$giornata[as.character(listasquadre[[indice2]]$home_team)
                                         ==as.character(dati2016xG$home_team_name[i])]
    
    j=seq(1,gg-1,1)
    j2=seq(1,gg2-1,1)
    puntic=0
    puntit=0
    for (k in 1:nrow(listasquadre[[indice]])){
      if (listasquadre[[indice]]$giornata[k] %in% j & 
          as.character(listasquadre[[indice]]$home_team[k])==as.character(dati2016xG$home_team_name[i])){
        punto=ifelse(listasquadre[[indice]]$home_goals[k]>listasquadre[[indice]]$away_goals[k],3,
                     ifelse(listasquadre[[indice]]$home_goals[k]==listasquadre[[indice]]$away_goals[k],1,0))
        puntic=puntic+punto }
      if (listasquadre[[indice]]$giornata[k] %in% j & 
          as.character(listasquadre[[indice]]$away_team[k])==as.character(dati2016xG$home_team_name[i])){
        punto=ifelse(listasquadre[[indice]]$home_goals[k]>listasquadre[[indice]]$away_goals[k],0,
                     ifelse(listasquadre[[indice]]$home_goals[k]==listasquadre[[indice]]$away_goals[k],1,3))
        puntic=puntic+punto }
    }
    for (k in 1:nrow(listasquadre[[indice]])){
      if (listasquadre[[indice2]]$giornata[k] %in% j2 & 
          as.character(listasquadre[[indice2]]$home_team[k])==as.character(dati2016xG$away_team_name[i])){
        punto=ifelse(listasquadre[[indice2]]$home_goals[k]>listasquadre[[indice2]]$away_goals[k],3,
                     ifelse(listasquadre[[indice2]]$home_goals[k]==listasquadre[[indice2]]$away_goals[k],1,0))
        puntit=puntit+punto }
      if (listasquadre[[indice2]]$giornata[k] %in% j2 & 
          as.character(listasquadre[[indice2]]$away_team[k])==as.character(dati2016xG$away_team_name[i])){
        punto=ifelse(listasquadre[[indice2]]$home_goals[k]>listasquadre[[indice2]]$away_goals[k],0,
                     ifelse(listasquadre[[indice2]]$home_goals[k]==listasquadre[[indice2]]$away_goals[k],1,3))
        puntit=puntit+punto }
    }
    dati2016xG$classificahome[i]=puntic
    dati2016xG$classificatrasf[i]=puntit
    
  }
}


dati2016xG[100,]


#prepariamo dati per previsioni

dati2016xG$risultato=0
dati2016xG$risultato=as.factor(ifelse(dati2016xG$home_goals>dati2016xG$away_goals,"1",
                                      ifelse(dati2016xG$home_goals==dati2016xG$away_goals,"X","2"))
)

traindata2016=dati2016xG %>% select(c("round","xGhomehome","xGawayaway",
                                      "xGhome_team_away","xGaway_team_home",
                                      "punti_homehome","punti_awayaway", "punti_home_team_away",
                                      "punti_away_team_home", "classificahome","classificatrasf","risultato"))
train2016=traindata2016[traindata2016$round>=10 & traindata2016$round<29,]
test2016=traindata2016[traindata2016$round>=29,]
colnames(train)
str(train)
train2016[90:100,"risultato"]

dati2016xG[100,]




#####
save.image(file="Environment.RData")

# adesso dobbiamo vedere altri anni ---------------------------------------
