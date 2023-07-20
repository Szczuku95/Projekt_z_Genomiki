#zad 9.4

#wczytanie danych
library(ggplot2)
library(shiny)
Dane_GDP <- read.csv2("GDP.csv")
Dane_Life_Expectancy <- read.csv2("Life_expectancy.csv")
Dane_Population <- read.csv2("Population.csv")
Dane_Continents <- read.csv2("Continents.csv")

#nazywamy ladnie kolumny
colnames(Dane_GDP) <- c("Country",substr(colnames(Dane_GDP)[-1],2,5))
colnames(Dane_Life_Expectancy) <- c("Country",substr(colnames(Dane_Life_Expectancy)[-1],2,5))
colnames(Dane_Population) <- c("Country",substr(colnames(Dane_Population)[-1],2,5))
colnames(Dane_Continents) <- c("Country","Continent")

#usuwamy z 3 tabel kraje, dla ktorych nie ma zadnych danych
NA_GDP <-  apply(Dane_GDP[,-1], 1, function(x) all(is.na(x)))
Dane_GDP <- Dane_GDP[!NA_GDP, ]
NA_Life_Expectancy <- apply(Dane_Life_Expectancy[,-1],1,function(x) all(is.na(x)))
Dane_Life_Expectancy <- Dane_Life_Expectancy[!NA_Life_Expectancy, ]
NA_Population <- apply(Dane_Population[,-1], 1, function(x) all(is.na(x)))
Dane_Population <- Dane_Population[!NA_Population, ]
remove(NA_GDP)
remove(NA_Life_Expectancy)
remove(NA_Population)

#Posortowanie alfabetycznie krajow we wszystkich tabelach - wczesniej 
#niektore kraje byly przestawione
Dane_Continents <- Dane_Continents[order(Dane_Continents[,1]), ]
Dane_GDP <- Dane_GDP[order(Dane_GDP[,1]), ]
Dane_Population<- Dane_Population[order(Dane_Population[,1]), ]
Dane_Life_Expectancy <- Dane_Life_Expectancy[order(Dane_Life_Expectancy[,1]), ]

#Sprawdzenie, dla ktorych krajow nie ma w ogole ktorejs z danych i usuniecie ich
Do_wywalenia_z_GDP <- is.na(match(Dane_GDP$Country,Dane_Life_Expectancy$Country))

Dane_GDP[which(Do_wywalenia_z_GDP==TRUE),1] #sprawdzenie, ktorych krajow rzeczywiscie brakuje, czy 
# np nie sa po prostu inaczej nazwane, np. US i USA, UK i United Kingdom/Great Britain
Do_wywalenia_z_Life_Expectancy <- is.na(match(Dane_Life_Expectancy$Country,Dane_GDP$Country))
Dane_Life_Expectancy[which(Do_wywalenia_z_Life_Expectancy==TRUE),1]
Dane_GDP <- Dane_GDP[!Do_wywalenia_z_GDP, ]
Dane_Life_Expectancy <- Dane_Life_Expectancy[!Do_wywalenia_z_Life_Expectancy, ]

all(match(Dane_Life_Expectancy$Country,Dane_GDP$Country)==1:191) #sprawdzenie, czy
#wszystkie kraje sa w odpowiedniej kolejnosci
remove(Do_wywalenia_z_GDP)
remove(Do_wywalenia_z_Life_Expectancy)

#wyrzucenie niepotrzebnych krajow z tabeli z populacja
Do_wywalenia_z_population <- is.na(match(Dane_Population$Country,Dane_GDP$Country))
Dane_Population <- Dane_Population[!Do_wywalenia_z_population, ]
all(match(Dane_Population$Country,Dane_GDP$Country)==1:191) #znow sprawdzenie poprawnej kolejnosci
remove(Do_wywalenia_z_population)

#dopasowanie nazw krajow z tabeli z kontynentami do pozostalych tabel,
#wyrzucenie niepotrzebnych krajow z tabeli z kontynentami

Dane_GDP[is.na(match(Dane_GDP$Country,Dane_Continents$Country)),1] #sprawdzenie, 
#dla ktorych krajow brakuje kontynentow w tabeli z kontynentami 

Dane_GDP[is.na(match(Dane_Continents$Country,Dane_GDP$Country)),1] #sprawdzenie, 
#ktore kraje w tabeli z kontynentami nie pojawiaja sie w pozostalych tabelach -
#porownujac z krajami wywolanymi poprzednia komenda mozemy stwierdzic, ktore kraje
#sa po prostu inaczej nazwane (np. Ivory Coast - Cote d'Ivoire)

Dane_Continents$Country <- as.character(Dane_Continents$Country)

Dane_Continents[Dane_Continents$Country=="Macau",1]="Macao, China"
Dane_Continents[Dane_Continents$Country=="Hong Kong",1]="Hong Kong, China"
Dane_Continents[Dane_Continents$Country=="Democratic Republic of the Congo",1]="Congo, Dem. Rep."
Dane_Continents[Dane_Continents$Country=="Republic of the Congo",1]="Congo, Rep."
Dane_Continents[Dane_Continents$Country=="Laos",1]="Lao"
Dane_Continents[Dane_Continents$Country=="Saint Lucia",1]="St. Lucia"
Dane_Continents[Dane_Continents$Country=="Slovakia",1]="Slovak Republic"
Dane_Continents[Dane_Continents$Country=="Micronesia, Federated States of",1]="Micronesia, Fed. Sts."  
Dane_Continents[Dane_Continents$Country=="Saint Vincent and the Grenadines",1]="St. Vincent and the Grenadines"
Dane_Continents[Dane_Continents$Country=="Ivory Coast",1]="Cote d'Ivoire"
Dane_Continents[Dane_Continents$Country=="Macedonia",1]="Macedonia, FYR"
Dane_Continents[Dane_Continents$Country=="Kyrgyzstan",1]="Kyrgyz Republic"  

#dodajemy teraz recznie Channel Islands i West Bank and Gaza - ich nie bylo w ogole
Dane_Continents=rbind(Dane_Continents,c("Channel Islands","Europe"),c("West Bank and Gaza","Asia"))

#wracamy do postaci faktora     
Dane_Continents$Country <- as.factor(Dane_Continents$Country) 

#Wyrzucenie zbednych krajow z tabeli z kontynentami              
Do_wywalenia_z_Continents <- is.na(match(Dane_Continents$Country,Dane_GDP$Country))
Dane_Continents <- Dane_Continents[!Do_wywalenia_z_Continents, ]

#jeszcze raz sortujemy po nazwie kraju
Dane_Continents <- Dane_Continents[order(Dane_Continents[,1]), ]

all(match(Dane_Continents$Country,Dane_GDP$Country)==1:191)

#przenumerowanie wierszy
rownames(Dane_GDP) = rownames(Dane_Life_Expectancy) = rownames(Dane_Population) = rownames(Dane_Continents) = 1:191

#usuwamy zbedne lata z danych o dlugosci zycia i z danych o populacji
Dane_Life_Expectancy <- subset(Dane_Life_Expectancy, 
                               select = which(colnames(Dane_Life_Expectancy) %in% colnames(Dane_GDP)))
Dane_Population <- subset(Dane_Population, 
                          select = which(colnames(Dane_Population) %in% colnames(Dane_GDP)))

#zmieniamy typ danych w population na numeric
Dane_Population[,-1] <- apply(Dane_Population[,-1],2,function(x) as.character(x))
Dane_Population[,-1] <- apply(Dane_Population[,-1],2,function(x) as.numeric(x))

#mamy teraz przygotowane dane - przechodzimy do wykresu

ui <- fluidPage(
    titlePanel("Wykres zależności oczekiwanej długości życia od wartości PKB/mieszkańca"),
    sliderInput("Rok",label="Określ rok, dla którego ma zostać wygenerowany wykres",
                min=1960,max=2011,value=1960,step=1),
    plotOutput("wykres")
  )

  
  server <- function(input,output){
    Rok <- reactive({input$Rok-1958})
    output$wykres <- renderPlot({
                    ggplot(data.frame(Dane_Continents,GDP=Dane_GDP[,Rok()],
                    Life_Expectancy = Dane_Life_Expectancy[,Rok()],
                    Population=Dane_Population[,Rok()]),
                    aes(x=GDP,y=Life_Expectancy,size=log(Population),col=Continent)) +
                    geom_point() +
                    theme(legend.position="right",legend.direction="vertical") +
                    guides(color=guide_legend(override.aes = list(size=5))) #zmiana rozmiaru
                    #symboli legendy - by?y malutkie
                    })
  }
  shinyApp(ui,server)


# a dodam se komentarz na końcu żeby sprawdzić jak działa commit w gicie