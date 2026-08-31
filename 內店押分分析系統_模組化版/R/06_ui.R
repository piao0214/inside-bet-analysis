# ========================================================
# Shiny UI
# ========================================================

build_ui <- function() {
  shiny::fluidPage(
    shiny::titlePanel("內店押分分析系統 + Ollama AI分析"),
    
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::fileInput("file", "上傳 Excel"),
        
        shiny::numericInput("start_year", "起始年", 114),
        shiny::numericInput("start_month", "起始月", 3, min = 1, max = 12),
        shiny::numericInput("end_year", "結束年", 115),
        shiny::numericInput("end_month", "結束月", 6, min = 1, max = 12),
        
        shiny::actionButton("load", "讀取資料"),
        shiny::hr(),
        
        shiny::tags$strong("選擇店家與遊戲"),
        shiny::selectInput("store", "選擇店家", choices = "--"),
        shiny::selectInput("game", "選擇遊戲", choices = "--"),
        shiny::helpText(
          "店家選 --：代表全部店家。",
          shiny::br(),
          "遊戲選 --：代表全部遊戲。"
        ),
        
        shiny::hr(),
        shiny::uiOutput("ollama_controls"),
        shiny::actionButton("analyze_ai", "產生 AI 分析"),
        width = 3
      ),
      
      shiny::mainPanel(
        shiny::tabsetPanel(
          # 1. 兩期預測（原本未來兩期預測）
          shiny::tabPanel(
            "兩期預測",
            shiny::plotOutput("future_return_forecast_plot", height = "500px")
          ),
          
          # 2. LOG RETURN測試（原本預測）
          shiny::tabPanel(
            "LOG RETURN測試",
            shiny::plotOutput("forecast_plot", height = "500px")
          ),
          
          # 3. RETURN測試（原本RETURN預測）
          shiny::tabPanel(
            "RETURN測試",
            shiny::plotOutput("return_forecast_plot", height = "500px")
          ),
          
          # 4. AI分析
          shiny::tabPanel(
            "AI分析",
            shiny::tags$br(),
            shiny::verbatimTextOutput("ai_text")
          )
        )
      )
    )
  )
}