library(shiny)

#definiowanie UI
ui <- fluidPage(

    # Application title
    titlePanel("Szacowanie prawdopodobieństwa, centralne twierdzenie graniczne:"),
    h2("Wizualizacja rozkładu zmiennej Zn:"),
    sidebarLayout(
        sidebarPanel(
            sliderInput("sample_size",
                        "N:",
                        min = 1,
                        max = 100,
                        value = 25)
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(plotOutput("histPlot", height = "400px", width="400px")),
            tabPanel(plotOutput("qqPlot", height = "400px", width="400px"))
          )
        )
    ),
    h2("Empiryczne szacowanie prawdopodobieństwa:"),
    sidebarLayout(
      sidebarPanel(
        sliderInput("sample_size_prob",
                    "N:",
                    min = 1,
                    max = 1000,
                    value = 25),
        sliderInput("rating_greater",
                    "B:",
                    min = 0,
                    max = 10,
                    value = 5.0,
                    step = 0.01)
      ),
      tabsetPanel(
          tabPanel(textOutput("prob"))
      )
    ),
)

#Funkcja służąco do policzenia odpowiedniej dystrybuanty rozkładu normalnego standardowego
calcProbability <- function(N, B, m, sd) {
  return ((pnorm((B - m) / (sd / sqrt(N)), lower.tail = FALSE)))
}

server <- function(input, output) {
    #Importowanie danych oraz obliczanie średniej i odchylenia standardowego dla całego zbioru
    movies <- read.csv(file = 'dataset/IMDB-Movie-Data.csv')[, c('Title', 'Director', 'Year', 'Rating')]
    m <- mean(movies$Rating)
    sd = sd(movies$Rating)

    output$histPlot <- renderPlot({
        n <- input$sample_size
        sample <- rep(NA, 1000)
        
        for(x in 1:1000){
          sample[x] <- (mean(sample(movies$Rating, n)) - m) / (sd / sqrt(n))
        }
        #Rysowanie odpowiedniego histogramu dla zmiennej Zn
        hist(sample, breaks = "Scott", main = "Zn distribution", xlab="")
    })
    
    output$qqPlot <- renderPlot({
      n <- input$sample_size
      sample <- rep(NA, 1000)
      
      for(x in 1:1000){
        sample[x] <- (mean(sample(movies$Rating, n)) - m) / (sd / sqrt(n))
      }
      #Rysowanie odpowiendiego wykresu q-q
      qqnorm(sample, pch = 1, frame = FALSE, ylab="Zn Quantiles")
      qqline(sample)
    })
    
    output$prob <- renderText({
      paste("Prawdopodobienstwo że średnia ocena(rating) próby N-elementowej jest większa niż B:", calcProbability(input$sample_size_prob, input$rating_greater, m, sd))
    })
}

shinyApp(ui = ui, server = server)
