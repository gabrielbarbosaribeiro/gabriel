library(shiny)
library(readxl)
library(dplyr)

# Interface
ui <- fluidPage(
  tags$head(
    tags$script(HTML("
      $(document).on('keypress', function(e) {
        if(e.which == 13 && $('#codigo').is(':focus')) {
          $('#adicionar').click();
        }
      });
    "))
  ),
  
  titlePanel("Orçamento de Exames"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("codigo", "Código do Exame", ""),
      numericInput("quantidade", "Quantidade", value = 1, min = 1),
      selectInput("lab", "Laboratório", choices = c("LABMED", "OUTROS")),
      actionButton("adicionar", "Adicionar Exame", class = "btn btn-primary"),
      actionButton("limpar", "Limpar Tabela", class = "btn btn-danger")
    ),
    mainPanel(
      tableOutput("tabela")
    )
  )
)

# Servidor
server <- function(input, output, session) {
  exames_df <- read_excel("CSV base teste.xlsx", col_types = c("text", "text", "numeric"))
  
  ch_labs <- list("LABMED" = 0.61, "OUTROS" = 0.218)
  
  orcamento <- reactiveVal(data.frame(
    Código = character(),
    Nome = character(),
    Quantidade = numeric(),
    Laboratório = character(),
    ValorUT = numeric(),
    CH = numeric(),
    Total = numeric(),
    stringsAsFactors = FALSE
  ))
  
  # Adicionar exame ao orçamento
  observeEvent(input$adicionar, {
    req(nchar(input$codigo) >= 5)
    
    cod <- as.character(input$codigo)
    quant <- as.numeric(input$quantidade)
    lab <- input$lab
    ch <- ch_labs[[lab]]
    
    exames_df$CODIGO <- as.character(exames_df$CODIGO)
    exame <- exames_df %>% filter(CODIGO == cod)
    
    if (nrow(exame) > 0) {
      valor_ut <- exame$VALORUT[1]
      total <- valor_ut * ch * quant
      novo <- data.frame(
        Código = cod,
        Nome = exame$NOME[1],
        Quantidade = quant,
        Laboratório = lab,
        ValorUT = valor_ut,
        CH = ch,
        Total = total
      )
      orcamento(rbind(orcamento(), novo))
    } else {
      showNotification("Exame não encontrado na base.", type = "error")
    }
    
    updateTextInput(session, "codigo", value = "")
  })
  
  # Limpar tabela
  observeEvent(input$limpar, {
    orcamento(data.frame(
      Código = character(),
      Nome = character(),
      Quantidade = numeric(),
      Laboratório = character(),
      ValorUT = numeric(),
      CH = numeric(),
      Total = numeric(),
      stringsAsFactors = FALSE
    ))
  })
  
  # Exibir tabela com total
  output$tabela <- renderTable({
    df <- orcamento()
    if (nrow(df) == 0) return()
    
    df_formatado <- df
    df_formatado$ValorUT <- ifelse(is.na(df$ValorUT), "-", sprintf("%.2f", df$ValorUT))
    df_formatado$Total <- ifelse(is.na(df$Total), "-", sprintf("%.2f", df$Total))
    
    total_geral <- sum(df$Total, na.rm = TRUE)
    
    df_final <- rbind(
      df_formatado,
      data.frame(
        Código = "",
        Nome = "",
        Quantidade = "",
        Laboratório = "",
        ValorUT = "",
        CH = "Total Geral:",
        Total = sprintf("%.2f", total_geral),
        stringsAsFactors = FALSE
      )
    )
    
    df_final
  })
}

# Executar o app
shinyApp(ui = ui, server = server)
