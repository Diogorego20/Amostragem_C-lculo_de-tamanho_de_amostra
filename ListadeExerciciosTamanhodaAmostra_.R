################################################################################
# DISCIPLINA: Amostragem I - UFPB CCEN
# PROFESSOR: Hemílio Fernandes Campos Coêlho
# ALUNO: Diogo da Silva Rego - 
# MATRÍCULA: 20240045381
# DATA: 01/03/2026
# DESCRIÇÃO: Aplicação Shiny para resoluções completas da Lista de Exercícios
#            sobre Cálculo de Tamanho de Amostra (Questões 01 a 05)
# AMBIENTE: Compatível com R Positron / RStudio
# VERSÃO: 3.0 (Questões)
################################################################################

# --- Carregamento Seguro de Pacotes ---
pacotes_necessarios <- c("shiny", "shinydashboard", "DT", "ggplot2", "knitr")

for (pacote in pacotes_necessarios) {
  if (!require(pacote, character.only = TRUE)) {
    message(paste("Instalando pacote:", pacote))
    install.packages(pacote, repos = "https://cran.rstudio.com/", quiet = TRUE)
    library(pacote, character.only = TRUE)
  }
}

# --- Funções para Cálculo de Tamanho de Amostra ---

#' Tamanho de Amostra para Proporção (baseado em Coeficiente de Variação)
#' Fórmula: n = (z^2 * (1-P)) / (CV^2 * P)
calc_n_prop_cv <- function(P, CV, z = 1.96) {
  if (P <= 0 || P >= 1) stop("Erro: Proporção P deve estar entre 0 e 1.")
  if (CV <= 0) stop("Erro: Coeficiente de Variação deve ser positivo.")
  
  n <- (z^2 * (1 - P)) / (CV^2 * P)
  return(ceiling(n))
}

#' Tamanho de Amostra para Proporção (baseado em Erro Padrão)
#' Fórmula: n = (P * (1-P)) / EP^2
calc_n_prop_ep <- function(P, EP) {
  if (P <= 0 || P >= 1) stop("Erro: Proporção P deve estar entre 0 e 1.")
  if (EP <= 0) stop("Erro: Erro Padrão deve ser positivo.")
  
  n <- (P * (1 - P)) / (EP^2)
  return(ceiling(n))
}

#' Tamanho de Amostra para Proporção (baseado em Margem de Erro)
#' Fórmula: n = (z^2 * P * (1-P)) / MOE^2
calc_n_prop_moe <- function(P, MOE, z = 1.96) {
  if (P < 0 || P > 1) stop("Erro: Proporção P deve estar entre 0 e 1.")
  if (MOE <= 0) stop("Erro: Margem de Erro deve ser positiva.")
  
  # Se P é desconhecido (NA), usa-se o pior caso (P=0.5)
  if (is.na(P) || P <= 0 || P >= 1) {
    P <- 0.5
  }
  
  n <- (z^2 * P * (1 - P)) / (MOE^2)
  return(ceiling(n))
}

#' Tamanho de Amostra para Detectar Mudança na Média (com Poder)
#' Baseado em teste bilateral com nível de significância alpha e poder (1-beta)
#' Fórmula: n = ((z_alpha + z_beta)^2 * S^2) / (mu1 - mu0)^2
calc_n_media_poder <- function(mu0, mu1, S, alpha = 0.05, poder = 0.80) {
  if (mu0 == mu1) stop("Erro: As médias H0 e H1 devem ser diferentes.")
  if (S <= 0) stop("Erro: Desvio padrão S deve ser positivo.")
  if (alpha <= 0 || alpha >= 1) stop("Erro: Nível de significância deve estar entre 0 e 1.")
  if (poder <= 0 || poder >= 1) stop("Erro: Poder deve estar entre 0 e 1.")
  
  z_alpha <- qnorm(1 - alpha / 2)
  z_beta <- qnorm(poder)
  
  n <- ((z_alpha + z_beta)^2 * S^2) / (mu1 - mu0)^2
  return(ceiling(n))
}

#' Poder Estatístico para Detectar Mudança em Proporção
#' Calcula o poder de um teste bilateral para proporções
calc_poder_prop <- function(p1, p2, n, alpha = 0.05) {
  if (p1 < 0 || p1 > 1 || p2 < 0 || p2 > 1) {
    stop("Erro: Proporções devem estar entre 0 e 1.")
  }
  if (n <= 0) stop("Erro: Tamanho da amostra deve ser positivo.")
  
  p_bar <- (p1 + p2) / 2
  z_alpha <- qnorm(1 - alpha / 2)
  z_beta <- (sqrt(n) * abs(p1 - p2) - z_alpha * sqrt(2 * p_bar * (1 - p_bar))) / 
            sqrt(p1 * (1 - p1) + p2 * (1 - p2))
  poder <- pnorm(z_beta)
  
  return(poder)
}

#' Calcular Semi-largura do Intervalo de Confiança
#' Fórmula: Semi-largura = z * sqrt(P(1-P)/n)
calc_semi_largura_ic <- function(P, n, z = 1.96) {
  se <- sqrt(P * (1 - P) / n)
  return(z * se)
}

# --- Interface do Usuário (UI) ---
ui <- dashboardPage(
  dashboardHeader(
    title = "Amostragem I - Cálculo de Tamanho de Amostra",
    titleWidth = 400,
    tags$head(tags$style(HTML("
      .main-header .logo { font-weight: bold; font-size: 14px; }
      .box-title { font-weight: bold; }
      .shiny-output-error { color: #d9534f; }
      .shiny-output-error:before { content: 'Erro: '; font-weight: bold; }
    ")))
  ),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Dashboard Inicial", tabName = "home", icon = icon("home")),
      menuItem("Questão 01", tabName = "q1", icon = icon("calculator")),
      menuItem("Questão 02", tabName = "q2", icon = icon("microscope")),
      menuItem("Questão 03", tabName = "q3", icon = icon("chart-line")),
      menuItem("Questão 04", tabName = "q4", icon = icon("bar-chart")),
      menuItem("Questão 05", tabName = "q5", icon = icon("percentage")),
      menuItem("Comparação Geral", tabName = "comparacao", icon = icon("table")),
      menuItem("Relatório Completo", tabName = "report", icon = icon("file-pdf")),
      br(),
      downloadButton("downloadReport", "Baixar Relatório Final", 
                     class = "btn-block btn-primary", 
                     style = "margin: 15px; width: 270px; font-weight: bold;")
    )
  ),
  dashboardBody(
    tabItems(
      # Aba Home
      tabItem(tabName = "home",
        fluidRow(
          box(width = 12, title = "Bem-vindo ao Sistema de Análise de Tamanho de Amostra", 
              status = "primary", solidHeader = TRUE, collapsible = FALSE,
              h3("Autor: Diogo da Silva Rego"),
              p(strong("Matrícula:"), "20240045381"),
              p(strong("Disciplina:"), "1108202 - AMOSTRAGEM I"),
              p(strong("Professor:"), "Hemílio Fernandes Campos Coêlho"),
              p(strong("Instituição:"), "UFPB - Centro de Ciências Exatas e da Natureza"),
              hr(),
              p("Esta aplicação resolve todos os exercícios da Lista de Exercícios sobre Cálculo de Tamanho de Amostra,
                integrando a teoria das Aulas 08 e 09 do Professor Hemílio."),
              hr(),
              h4("Conteúdo:"),
              p("• Questão 01: Tamanho de amostra para proporções com diferentes critérios"),
              p("• Questão 02: Prevalência de característica rara"),
              p("• Questão 03: Tamanho de amostra para detectar mudança em média (Poder)"),
              p("• Questão 04: Tamanho de amostra para detectar mudança em IMC"),
              p("• Questão 05: Poder para detectar mudança em proporção")
          )
        )
      ),
      
      # Aba Questão 01
      tabItem(tabName = "q1",
        h2("Questão 01: Tamanho de Amostra para Proporções"),
        fluidRow(
          box(width = 12, title = "Enunciado", 
              status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
              p("De acordo com o US Bureau of Labor Statistics, 71% de todos os trabalhadores da iniciativa privada 
                tinham acesso a planos de assistência médica patrocinados pelo empregador, 52% de todos os 
                trabalhadores participavam de planos de assistência médica em março de 2006, e 7% dos trabalhadores 
                em tempo parcial participavam de um programa de assistência oftalmológica."),
              p("Calcule o tamanho de uma amostra aleatória simples de empregados que seria necessária para estimar 
                cada uma dessas proporções, usando as metas de estimação em (a), (b) e (c).")
          )
        ),
        fluidRow(
          box(width = 12, title = "Cálculos Interativos", 
              status = "success", solidHeader = TRUE, collapsible = FALSE,
              fluidRow(
                column(3, numericInput("p_q1_1", "Proporção 1 (p₁):", 0.71, min = 0, max = 1, step = 0.01)),
                column(3, numericInput("p_q1_2", "Proporção 2 (p₂):", 0.52, min = 0, max = 1, step = 0.01)),
                column(3, numericInput("p_q1_3", "Proporção 3 (p₃):", 0.07, min = 0, max = 1, step = 0.01)),
                column(3, numericInput("z_q1", "Valor de z (IC 95%):", 1.96, step = 0.01))
              ),
              hr(),
              fluidRow(
                column(4, numericInput("cv_q1", "CV para item (a):", 0.10, step = 0.01)),
                column(4, numericInput("ep_q1", "EP para item (b):", 0.03, step = 0.001)),
                column(4, numericInput("moe_q1", "MOE para item (c):", 0.03, step = 0.001))
              ),
              actionButton("calcular_q1", "Calcular", class = "btn-primary", style = "margin-top: 10px;")
          )
        ),
        fluidRow(
          box(width = 12, title = "Resultados - Questão 01", 
              status = "success", solidHeader = TRUE, collapsible = FALSE,
              verbatimTextOutput("out_q1")
          )
        )
      ),
      
      # Aba Questão 02
      tabItem(tabName = "q2",
        h2("Questão 02: Prevalência de Característica Rara"),
        fluidRow(
          box(width = 12, title = "Enunciado", 
              status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
              p("Um(a) pesquisador(a) deseja estimar a prevalência de uma característica que se supõe ser rara. 
                A melhor estimativa do(a) pesquisador(a) é que a prevalência seja de 2%. Ele(a) gostaria de estimar 
                a prevalência com uma margem de erro (MOE) de 0,005."),
              p("(a) Qual tamanho amostral é necessário?"),
              p("(b) Como o(a) pesquisador(a) parece muito incerto(a) sobre a prevalência real, que cálculo alternativo 
                você poderia fazer para ilustrar os efeitos de diferentes tamanhos amostrais?")
          )
        ),
        fluidRow(
          box(width = 12, title = "Cálculos Interativos", 
              status = "success", solidHeader = TRUE, collapsible = FALSE,
              fluidRow(
                column(4, numericInput("p_q2", "Proporção Estimada (p):", 0.02, min = 0, max = 1, step = 0.001)),
                column(4, numericInput("moe_q2", "Margem de Erro (MOE):", 0.005, step = 0.0001)),
                column(4, numericInput("z_q2", "Valor de z (IC 95%):", 1.96, step = 0.01))
              ),
              actionButton("calcular_q2", "Calcular", class = "btn-primary", style = "margin-top: 10px;")
          )
        ),
        fluidRow(
          box(width = 12, title = "Resultados - Questão 02", 
              status = "success", solidHeader = TRUE, collapsible = FALSE,
              verbatimTextOutput("out_q2")
          ),
          box(width = 12, title = "Análise de Sensibilidade (Diferentes Proporções)", 
              status = "warning", solidHeader = TRUE, collapsible = FALSE,
              plotOutput("plot_q2_sensibilidade", height = "400px")
          )
        )
      ),
      
      # Aba Questão 03
      tabItem(tabName = "q3",
        h2("Questão 03: Tamanho de Amostra para Detectar Mudança em Média"),
        fluidRow(
          box(width = 12, title = "Enunciado", 
              status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
              p("A renda disponível média do trabalho por trabalhador no México, em 2002, foi aproximadamente 
                6100 dólares americanos. Suponha que uma nova pesquisa será conduzida em 2010 e que você 
                deseja determinar o tamanho de uma amostra aleatória que permita detectar que a média aumentou para 
                7000 dólares. Assuma que a variância relativa unitária da renda em 2002 foi 2,5 e que ela será 
                aproximadamente a mesma em 2010."),
              p("Calcule um tamanho amostral para um teste ao nível de significância de 0,05 quando o poder desejado é 0,80; 
                trate a média de 2002 como constante neste problema.")
          )
        ),
        fluidRow(
          box(width = 12, title = "Cálculos Interativos", 
              status = "success", solidHeader = TRUE, collapsible = FALSE,
              fluidRow(
                column(3, numericInput("mu0_q3", "Média H0 (μ0 - 2002):", 6100)),
                column(3, numericInput("mu1_q3", "Média H1 (μ1 - 2010):", 7000)),
                column(3, numericInput("cv_unitaria_q3", "Variância Relativa Unitária:", 2.5, step = 0.1)),
                column(3, numericInput("alpha_q3", "Nível de Significância (α):", 0.05, step = 0.01))
              ),
              fluidRow(
                column(6, numericInput("poder_q3", "Poder Desejado:", 0.80, min = 0, max = 1, step = 0.01)),
                column(6, verbatimTextOutput("out_q3_desvio"))
              ),
              actionButton("calcular_q3", "Calcular", class = "btn-primary", style = "margin-top: 10px;")
          )
        ),
        fluidRow(
          box(width = 12, title = "Resultados - Questão 03", 
              status = "success", solidHeader = TRUE, collapsible = FALSE,
              verbatimTextOutput("out_q3")
          )
        )
      ),
      
      # Aba Questão 04
      tabItem(tabName = "q4",
        h2("Questão 04: Tamanho de Amostra para Detectar Mudança em IMC"),
        fluidRow(
          box(width = 12, title = "Enunciado", 
              status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
              p("Você recebeu um contrato para conduzir um estudo sobre obesidade em crianças de 6 a 14 anos. 
                Dados sobre hábitos alimentares e níveis de atividade física são coletados por meio de um questionário 
                aplicado aos pais; medidas físicas são coletadas por profissionais de enfermagem treinados. 
                Sua tarefa é determinar tamanhos amostrais nos cenários a seguir, com 80% de poder e nível de significância de 0,05."),
              p("(a) O cliente está interessado em determinar se o IMC médio de crianças no 1º ano (idades 6-7) 
                aumentou em 1,5% em relação a uma média previamente estimada de 17,5. Qual é o tamanho amostral 
                necessário para detectar essa diferença, dado que o desvio-padrão populacional é 0,70?"),
              p("(b) Como o tamanho amostral muda se o cliente estiver disposto a aceitar ser capaz de detectar um 
                aumento de 3,0%?"),
              p("(c) Como o tamanho amostral muda se o cliente quiser detectar um aumento de 0,5%?"),
              p("(d) Comente a diferença em seus cálculos de tamanho amostral.")
          )
        ),
        fluidRow(
          box(width = 12, title = "Cálculos Interativos", 
              status = "success", solidHeader = TRUE, collapsible = FALSE,
              fluidRow(
                column(3, numericInput("mu0_q4", "Média Base (μ0):", 17.5)),
                column(3, numericInput("sd_q4", "Desvio Padrão (σ):", 0.70, step = 0.01)),
                column(3, numericInput("alpha_q4", "Nível de Significância (α):", 0.05, step = 0.01)),
                column(3, numericInput("poder_q4", "Poder Desejado:", 0.80, min = 0, max = 1, step = 0.01))
              ),
              fluidRow(
                column(4, numericInput("pct_aumento_4a", "Aumento (a) em %:", 1.5, step = 0.1)),
                column(4, numericInput("pct_aumento_4b", "Aumento (b) em %:", 3.0, step = 0.1)),
                column(4, numericInput("pct_aumento_4c", "Aumento (c) em %:", 0.5, step = 0.1))
              ),
              actionButton("calcular_q4", "Calcular", class = "btn-primary", style = "margin-top: 10px;")
          )
        ),
        fluidRow(
          box(width = 12, title = "Resultados - Questão 04", 
              status = "success", solidHeader = TRUE, collapsible = FALSE,
              verbatimTextOutput("out_q4")
          ),
          box(width = 12, title = "Comparação Gráfica dos Cenários", 
              status = "warning", solidHeader = TRUE, collapsible = FALSE,
              plotOutput("plot_q4_comparacao", height = "400px")
          )
        )
      ),
      
      # Aba Questão 05
      tabItem(tabName = "q5",
        h2("Questão 05: Poder para Detectar Mudança em Proporção"),
        fluidRow(
          box(width = 12, title = "Enunciado", 
              status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
              p("Deve-se selecionar uma amostra da população de pessoas em um distrito que possuem idade de 18 anos ou mais. 
                A proporção de pessoas desempregadas é a medida de interesse. Três meses depois, essa medida será novamente 
                registrada em uma amostra de acompanhamento. Espera-se que 75% da amostra do tempo 1 coopere no tempo 2. 
                O mesmo tamanho de amostra será mantido no tempo 2 por meio da seleção de pessoas adicionais para compor a amostra."),
              p("(a) Se a taxa de desemprego no tempo 1 é estimada em 8% e você deseja ser capaz de detectar uma 
                queda de 1,5 ponto percentual com poder 0,8 em um teste unilateral ao nível de 0,05, qual deve 
                ser o tamanho da amostra em cada período de tempo?"),
              p("(b) Se você só pode testar uma amostra de 500 pessoas, qual é o poder para detectar uma mudança 
                de 1,5 ponto percentual em um eventual comparação?")
          )
        ),
        fluidRow(
          box(width = 12, title = "Cálculos Interativos", 
              status = "success", solidHeader = TRUE, collapsible = FALSE,
              fluidRow(
                column(3, numericInput("p1_q5", "Proporção Tempo 1 (p1):", 0.08, min = 0, max = 1, step = 0.01)),
                column(3, numericInput("mudanca_q5", "Mudança em Proporção:", 0.015, step = 0.001)),
                column(3, numericInput("poder_q5", "Poder Desejado:", 0.80, min = 0, max = 1, step = 0.01)),
                column(3, numericInput("alpha_q5", "Nível de Significância (α):", 0.05, step = 0.01))
              ),
              fluidRow(
                column(6, numericInput("n_q5_fixo", "Tamanho Fixo da Amostra (b):", 500)),
                column(6, verbatimTextOutput("out_q5_p2"))
              ),
              actionButton("calcular_q5", "Calcular", class = "btn-primary", style = "margin-top: 10px;")
          )
        ),
        fluidRow(
          box(width = 12, title = "Resultados - Questão 05", 
              status = "success", solidHeader = TRUE, collapsible = FALSE,
              verbatimTextOutput("out_q5")
          )
        )
      ),
      
      # Aba Comparação Geral
      tabItem(tabName = "comparacao",
        h2("Comparação Geral de Resultados"),
        fluidRow(
          box(width = 12, title = "Resumo de Todos os Tamanhos Amostrais Calculados", 
              status = "primary", solidHeader = TRUE, collapsible = FALSE,
              dataTableOutput("tabela_resumo")
          )
        ),
        fluidRow(
          box(width = 12, title = "Visualização Comparativa", 
              status = "info", solidHeader = TRUE, collapsible = FALSE,
              plotOutput("plot_comparacao_geral", height = "500px")
          )
        )
      ),
      
      # Aba Relatório
      tabItem(tabName = "report",
        h2("Relatório Técnico Completo"),
        htmlOutput("full_report")
      )
    )
  )
)

# --- Lógica do Servidor (Server) ---
server <- function(input, output, session) {

  # --- Questão 01 ---
  observeEvent(input$calcular_q1, {
    output$out_q1 <- renderPrint({
      tryCatch({
        props <- c(input$p_q1_1, input$p_q1_2, input$p_q1_3)
        names_props <- c("p₁ = 71% (Acesso a planos)", "p₂ = 52% (Participação)", "p₃ = 7% (Oftalmologia)")
        
        cat("=" %+% strrep("=", 78) %+% "\n")
        cat("QUESTÃO 01: TAMANHO DE AMOSTRA PARA PROPORÇÕES\n")
        cat("=" %+% strrep("=", 78) %+% "\n\n")
        
        cat("Proporções fornecidas:\n")
        for (i in 1:3) {
          cat(sprintf("  %s: %.4f\n", names_props[i], props[i]))
        }
        cat("\n")
        
        # Item (a)
        cat("(a) COEFICIENTE DE VARIAÇÃO DE 10%\n")
        cat("-" %+% strrep("-", 76) %+% "\n")
        cat("Fórmula: CV = σ_p̂ / p = 0,10\n")
        cat("         n = (1-p) / (0,01 × p)\n\n")
        
        n_cv <- sapply(props, function(p) calc_n_prop_cv(p, input$cv_q1, input$z_q1))
        for (i in 1:3) {
          cat(sprintf("  Para %s: n = %.0f\n", names_props[i], n_cv[i]))
        }
        cat("\n")
        
        # Item (b)
        cat("(b) ERRO-PADRÃO DE 3 PONTOS PERCENTUAIS\n")
        cat("-" %+% strrep("-", 76) %+% "\n")
        cat("Fórmula: SE = √[p(1-p)/n] = 0,03\n")
        cat("         n = p(1-p) / (0,03)²\n\n")
        
        n_ep <- sapply(props, function(p) calc_n_prop_ep(p, input$ep_q1))
        for (i in 1:3) {
          cat(sprintf("  Para %s: n = %.0f\n", names_props[i], n_ep[i]))
        }
        cat("\n")
        
        # Item (c)
        cat("(c) MARGEM DE ERRO (MOE) DE 3 PONTOS PERCENTUAIS\n")
        cat("-" %+% strrep("-", 76) %+% "\n")
        cat(sprintf("Fórmula: MOE = z_{α/2} × √[p(1-p)/n] = 0,03\n"))
        cat(sprintf("         Para IC 95%%, z = %.2f\n", input$z_q1))
        cat(sprintf("         n = (%.2f)² × p(1-p) / (0,03)²\n\n", input$z_q1))
        
        n_moe <- sapply(props, function(p) calc_n_prop_moe(p, input$moe_q1, input$z_q1))
        for (i in 1:3) {
          cat(sprintf("  Para %s: n = %.0f\n", names_props[i], n_moe[i]))
        }
        cat("\n")
        
        # Item (d)
        cat("(d) SEMI-LARGURAS DOS INTERVALOS DE CONFIANÇA DE 95%\n")
        cat("-" %+% strrep("-", 76) %+% "\n")
        cat("Fórmula: Semi-largura = 1,96 × √[p(1-p)/n]\n\n")
        
        cat("Para a meta (a) - Coeficiente de Variação de 10%:\n")
        for (i in 1:3) {
          sl <- calc_semi_largura_ic(props[i], n_cv[i], input$z_q1)
          cat(sprintf("  %s: %.4f (%.2f%%)\n", names_props[i], sl, sl*100))
        }
        cat("\n")
        
        cat("Para a meta (b) - Erro-padrão de 3 pontos percentuais:\n")
        for (i in 1:3) {
          sl <- calc_semi_largura_ic(props[i], n_ep[i], input$z_q1)
          cat(sprintf("  %s: %.4f (%.2f%%)\n", names_props[i], sl, sl*100))
        }
        cat("\n")
        
        cat("Para a meta (c) - Margem de Erro de 3 pontos percentuais:\n")
        for (i in 1:3) {
          sl <- calc_semi_largura_ic(props[i], n_moe[i], input$z_q1)
          cat(sprintf("  %s: %.4f (%.2f%%)\n", names_props[i], sl, sl*100))
        }
        cat("\n")
        
        # Item (e)
        cat("(e) COMPARAÇÃO DOS TAMANHOS AMOSTRAIS\n")
        cat("-" %+% strrep("-", 76) %+% "\n\n")
        
        cat("Resumo dos tamanhos amostrais calculados:\n")
        cat(sprintf("%-35s %15s %15s %15s\n", "Proporção", "CV=10%", "SE=3%", "MOE=3%"))
        cat(strrep("-", 65) %+% "\n")
        for (i in 1:3) {
          cat(sprintf("%-35s %15.0f %15.0f %15.0f\n", names_props[i], n_cv[i], n_ep[i], n_moe[i]))
        }
        cat("\n")
        
        cat("Observações:\n")
        cat("  • A meta (a) com CV=10% resulta em tamanhos amostrais MENORES\n")
        cat("  • A meta (b) com SE=3% resulta em tamanhos amostrais INTERMEDIÁRIOS\n")
        cat("  • A meta (c) com MOE=3% resulta em tamanhos amostrais MAIORES\n\n")
        cat("  Razão: A margem de erro inclui o multiplicador z, enquanto o erro-padrão não.\n")
        cat("  Portanto, para a mesma margem de 3%, a MOE requer uma amostra maior.\n\n")
        cat(sprintf("  Relação: n_MOE / n_SE ≈ (%.2f)² = %.2f\n", input$z_q1, input$z_q1^2))
        for (i in 1:3) {
          ratio <- n_moe[i] / n_ep[i]
          cat(sprintf("    Para %s: %.2f\n", names_props[i], ratio))
        }
        
      }, error = function(e) {
        cat("Erro:", conditionMessage(e), "\n")
      })
    })
  })
  
  # Trigger inicial para Q1
  observeEvent(TRUE, {
    shinyjs::runjs("$('#calcular_q1').click();")
  }, once = TRUE)
  
  # --- Questão 02 ---
  observeEvent(input$calcular_q2, {
    output$out_q2 <- renderPrint({
      tryCatch({
        cat("=" %+% strrep("=", 78) %+% "\n")
        cat("QUESTÃO 02: PREVALÊNCIA DE CARACTERÍSTICA RARA\n")
        cat("=" %+% strrep("=", 78) %+% "\n\n")
        
        cat("Dados fornecidos:\n")
        cat(sprintf("  Proporção Estimada (p): %.4f (2%%)\n", input$p_q2))
        cat(sprintf("  Margem de Erro (MOE): %.5f (0,5%%)\n", input$moe_q2))
        cat(sprintf("  Nível de Confiança: 95%% (z = %.2f)\n\n", input$z_q2))
        
        # Item (a)
        cat("(a) TAMANHO AMOSTRAL NECESSÁRIO\n")
        cat("-" %+% strrep("-", 76) %+% "\n")
        cat("Fórmula: n = (z² × p × (1-p)) / MOE²\n\n")
        
        n_q2 <- calc_n_prop_moe(input$p_q2, input$moe_q2, input$z_q2)
        cat(sprintf("n = (%.2f)² × %.4f × %.4f / (%.5f)²\n", 
                    input$z_q2, input$p_q2, 1-input$p_q2, input$moe_q2))
        cat(sprintf("n = %.2f / %.8f\n", input$z_q2^2 * input$p_q2 * (1-input$p_q2), input$moe_q2^2))
        cat(sprintf("n = %.0f\n\n", n_q2))
        
        # Item (b)
        cat("(b) ANÁLISE DE SENSIBILIDADE\n")
        cat("-" %+% strrep("-", 76) %+% "\n")
        cat("Como o pesquisador está incerto sobre a prevalência real, vamos calcular\n")
        cat("o tamanho amostral para diferentes valores de proporção:\n\n")
        
        props_teste <- c(0.01, 0.02, 0.03, 0.05, 0.10)
        cat(sprintf("%-15s %15s\n", "Proporção", "Tamanho (n)"))
        cat(strrep("-", 30) %+% "\n")
        for (p in props_teste) {
          n <- calc_n_prop_moe(p, input$moe_q2, input$z_q2)
          cat(sprintf("%-15.2f%% %15.0f\n", p*100, n))
        }
        
      }, error = function(e) {
        cat("Erro:", conditionMessage(e), "\n")
      })
    })
    
    output$plot_q2_sensibilidade <- renderPlot({
      props_range <- seq(0.01, 0.20, by = 0.01)
      n_values <- sapply(props_range, function(p) calc_n_prop_moe(p, input$moe_q2, input$z_q2))
      
      df <- data.frame(Proporcao = props_range * 100, Tamanho = n_values)
      
      ggplot(df, aes(x = Proporcao, y = Tamanho)) +
        geom_line(color = "#2E86AB", size = 1.2) +
        geom_point(color = "#2E86AB", size = 3) +
        geom_vline(xintercept = input$p_q2 * 100, linetype = "dashed", color = "red", size = 1) +
        geom_hline(yintercept = calc_n_prop_moe(input$p_q2, input$moe_q2, input$z_q2), 
                   linetype = "dashed", color = "red", size = 1) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          axis.text = element_text(size = 11),
          panel.grid.major = element_line(color = "gray90"),
          panel.grid.minor = element_line(color = "gray95")
        ) +
        labs(
          title = "Análise de Sensibilidade: Tamanho de Amostra vs Proporção",
          x = "Proporção (%)",
          y = "Tamanho de Amostra (n)",
          caption = sprintf("MOE = %.4f | z = %.2f", input$moe_q2, input$z_q2)
        )
    })
  })
  
  # Trigger inicial para Q2
  observeEvent(TRUE, {
    shinyjs::runjs("$('#calcular_q2').click();")
  }, once = TRUE)
  
  # --- Questão 03 ---
  observeEvent(input$calcular_q3, {
    output$out_q3_desvio <- renderPrint({
      S <- input$mu0_q3 * sqrt(input$cv_unitaria_q3)
      cat(sprintf("Desvio Padrão (σ):\n%.2f", S))
    })
    
    output$out_q3 <- renderPrint({
      tryCatch({
        S <- input$mu0_q3 * sqrt(input$cv_unitaria_q3)
        
        cat("=" %+% strrep("=", 78) %+% "\n")
        cat("QUESTÃO 03: TAMANHO DE AMOSTRA PARA DETECTAR MUDANÇA EM MÉDIA\n")
        cat("=" %+% strrep("=", 78) %+% "\n\n")
        
        cat("Dados fornecidos:\n")
        cat(sprintf("  Média H0 (μ0 - 2002): $%.2f\n", input$mu0_q3))
        cat(sprintf("  Média H1 (μ1 - 2010): $%.2f\n", input$mu1_q3))
        cat(sprintf("  Variância Relativa Unitária: %.2f\n", input$cv_unitaria_q3))
        cat(sprintf("  Desvio Padrão (σ): $%.2f\n", S))
        cat(sprintf("  Nível de Significância (α): %.2f\n", input$alpha_q3))
        cat(sprintf("  Poder Desejado: %.2f (80%%)\n\n", input$poder_q3))
        
        cat("Cálculo:\n")
        cat("-" %+% strrep("-", 76) %+% "\n")
        cat("Fórmula: n = ((z_α + z_β)² × σ²) / (μ1 - μ0)²\n\n")
        
        z_alpha <- qnorm(1 - input$alpha_q3 / 2)
        z_beta <- qnorm(input$poder_q3)
        
        cat(sprintf("z_α/2 (α=%.2f) = %.4f\n", input$alpha_q3, z_alpha))
        cat(sprintf("z_β (poder=%.2f) = %.4f\n", input$poder_q3, z_beta))
        cat(sprintf("Diferença de médias: %.2f - %.2f = $%.2f\n\n", 
                    input$mu1_q3, input$mu0_q3, input$mu1_q3 - input$mu0_q3))
        
        n_q3 <- calc_n_media_poder(input$mu0_q3, input$mu1_q3, S, input$alpha_q3, input$poder_q3)
        
        cat(sprintf("n = ((%.4f + %.4f)² × (%.2f)²) / (%.2f)²\n", 
                    z_alpha, z_beta, S, input$mu1_q3 - input$mu0_q3))
        cat(sprintf("n = (%.4f)² × %.2f / %.2f\n", 
                    z_alpha + z_beta, S^2, (input$mu1_q3 - input$mu0_q3)^2))
        cat(sprintf("n = %.0f\n\n", n_q3))
        
        cat("Interpretação:\n")
        cat("-" %+% strrep("-", 76) %+% "\n")
        cat(sprintf("Para detectar um aumento de $%.2f na renda média (de $%.2f para $%.2f),\n",
                    input$mu1_q3 - input$mu0_q3, input$mu0_q3, input$mu1_q3))
        cat(sprintf("com poder de %.0f%% e nível de significância de %.0f%%,\n", 
                    input$poder_q3*100, input$alpha_q3*100))
        cat(sprintf("é necessária uma amostra de %.0f trabalhadores.\n", n_q3))
        
      }, error = function(e) {
        cat("Erro:", conditionMessage(e), "\n")
      })
    })
  })
  
  # Trigger inicial para Q3
  observeEvent(TRUE, {
    shinyjs::runjs("$('#calcular_q3').click();")
  }, once = TRUE)
  
  # --- Questão 04 ---
  observeEvent(input$calcular_q4, {
    output$out_q4 <- renderPrint({
      tryCatch({
        cat("=" %+% strrep("=", 78) %+% "\n")
        cat("QUESTÃO 04: TAMANHO DE AMOSTRA PARA DETECTAR MUDANÇA EM IMC\n")
        cat("=" %+% strrep("=", 78) %+% "\n\n")
        
        cat("Dados fornecidos:\n")
        cat(sprintf("  Média Base (μ0): %.2f\n", input$mu0_q4))
        cat(sprintf("  Desvio Padrão (σ): %.2f\n", input$sd_q4))
        cat(sprintf("  Nível de Significância (α): %.2f\n", input$alpha_q4))
        cat(sprintf("  Poder Desejado: %.2f (80%%)\n\n", input$poder_q4))
        
        # Item (a)
        cat("(a) AUMENTO DE 1,5%\n")
        cat("-" %+% strrep("-", 76) %+% "\n")
        
        mu1_a <- input$mu0_q4 * (1 + input$pct_aumento_4a / 100)
        n_4a <- calc_n_media_poder(input$mu0_q4, mu1_a, input$sd_q4, input$alpha_q4, input$poder_q4)
        
        cat(sprintf("Aumento: %.2f × (1 + %.2f%%) = %.2f\n", 
                    input$mu0_q4, input$pct_aumento_4a, mu1_a))
        cat(sprintf("Diferença: %.2f\n", mu1_a - input$mu0_q4))
        cat(sprintf("Tamanho de Amostra: %.0f\n\n", n_4a))
        
        # Item (b)
        cat("(b) AUMENTO DE 3,0%\n")
        cat("-" %+% strrep("-", 76) %+% "\n")
        
        mu1_b <- input$mu0_q4 * (1 + input$pct_aumento_4b / 100)
        n_4b <- calc_n_media_poder(input$mu0_q4, mu1_b, input$sd_q4, input$alpha_q4, input$poder_q4)
        
        cat(sprintf("Aumento: %.2f × (1 + %.2f%%) = %.2f\n", 
                    input$mu0_q4, input$pct_aumento_4b, mu1_b))
        cat(sprintf("Diferença: %.2f\n", mu1_b - input$mu0_q4))
        cat(sprintf("Tamanho de Amostra: %.0f\n\n", n_4b))
        
        # Item (c)
        cat("(c) AUMENTO DE 0,5%\n")
        cat("-" %+% strrep("-", 76) %+% "\n")
        
        mu1_c <- input$mu0_q4 * (1 + input$pct_aumento_4c / 100)
        n_4c <- calc_n_media_poder(input$mu0_q4, mu1_c, input$sd_q4, input$alpha_q4, input$poder_q4)
        
        cat(sprintf("Aumento: %.2f × (1 + %.2f%%) = %.2f\n", 
                    input$mu0_q4, input$pct_aumento_4c, mu1_c))
        cat(sprintf("Diferença: %.2f\n", mu1_c - input$mu0_q4))
        cat(sprintf("Tamanho de Amostra: %.0f\n\n", n_4c))
        
        # Item (d)
        cat("(d) COMENTÁRIO SOBRE AS DIFERENÇAS\n")
        cat("-" %+% strrep("-", 76) %+% "\n\n")
        
        cat("Resumo dos Tamanhos Amostrais:\n")
        cat(sprintf("%-20s %20s %20s\n", "Cenário", "Diferença de IMC", "Tamanho (n)"))
        cat(strrep("-", 60) %+% "\n")
        cat(sprintf("%-20s %20.4f %20.0f\n", "(a) 1,5% aumento", mu1_a - input$mu0_q4, n_4a))
        cat(sprintf("%-20s %20.4f %20.0f\n", "(b) 3,0% aumento", mu1_b - input$mu0_q4, n_4b))
        cat(sprintf("%-20s %20.4f %20.0f\n", "(c) 0,5% aumento", mu1_c - input$mu0_q4, n_4c))
        cat("\n")
        
        cat("Observações:\n")
        cat(sprintf("  • Quanto MENOR a diferença a ser detectada, MAIOR o tamanho amostral necessário.\n"))
        cat(sprintf("  • Razão (a)/(b) = %.2f: Detectar 1,5%% requer %.0f%% mais observações que 3,0%%\n", 
                    n_4a/n_4b, (n_4a/n_4b - 1)*100))
        cat(sprintf("  • Razão (c)/(a) = %.2f: Detectar 0,5%% requer %.0f%% mais observações que 1,5%%\n", 
                    n_4c/n_4a, (n_4c/n_4a - 1)*100))
        cat(sprintf("  • A relação é inversamente proporcional ao quadrado da diferença.\n"))
        
      }, error = function(e) {
        cat("Erro:", conditionMessage(e), "\n")
      })
    })
    
    output$plot_q4_comparacao <- renderPlot({
      mu1_a <- input$mu0_q4 * (1 + input$pct_aumento_4a / 100)
      mu1_b <- input$mu0_q4 * (1 + input$pct_aumento_4b / 100)
      mu1_c <- input$mu0_q4 * (1 + input$pct_aumento_4c / 100)
      
      n_4a <- calc_n_media_poder(input$mu0_q4, mu1_a, input$sd_q4, input$alpha_q4, input$poder_q4)
      n_4b <- calc_n_media_poder(input$mu0_q4, mu1_b, input$sd_q4, input$alpha_q4, input$poder_q4)
      n_4c <- calc_n_media_poder(input$mu0_q4, mu1_c, input$sd_q4, input$alpha_q4, input$poder_q4)
      
      df <- data.frame(
        Cenario = c("(a) 1,5%", "(b) 3,0%", "(c) 0,5%"),
        Tamanho = c(n_4a, n_4b, n_4c),
        Diferenca = c(mu1_a - input$mu0_q4, mu1_b - input$mu0_q4, mu1_c - input$mu0_q4)
      )
      
      ggplot(df, aes(x = reorder(Cenario, -Tamanho), y = Tamanho, fill = Cenario)) +
        geom_bar(stat = "identity", color = "black", size = 1) +
        geom_text(aes(label = sprintf("n = %.0f", Tamanho)), vjust = -0.5, size = 5, fontface = "bold") +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          axis.text = element_text(size = 12),
          legend.position = "none"
        ) +
        labs(
          title = "Questão 04: Comparação de Tamanhos Amostrais por Cenário",
          y = "Tamanho de Amostra (n)",
          x = "Cenário",
          caption = "Poder = 80% | α = 5%"
        )
    })
  })
  
  # Trigger inicial para Q4
  observeEvent(TRUE, {
    shinyjs::runjs("$('#calcular_q4').click();")
  }, once = TRUE)
  
  # --- Questão 05 ---
  observeEvent(input$calcular_q5, {
    output$out_q5_p2 <- renderPrint({
      p2 <- input$p1_q5 - input$mudanca_q5
      cat(sprintf("p₂ = %.4f\n(%.2f%%)", p2, p2*100))
    })
    
    output$out_q5 <- renderPrint({
      tryCatch({
        p2 <- input$p1_q5 - input$mudanca_q5
        
        cat("=" %+% strrep("=", 78) %+% "\n")
        cat("QUESTÃO 05: PODER PARA DETECTAR MUDANÇA EM PROPORÇÃO\n")
        cat("=" %+% strrep("=", 78) %+% "\n\n")
        
        cat("Dados fornecidos:\n")
        cat(sprintf("  Proporção Tempo 1 (p1): %.4f (8%%)\n", input$p1_q5))
        cat(sprintf("  Mudança em Proporção: %.4f (1,5 ponto percentual)\n", input$mudanca_q5))
        cat(sprintf("  Proporção Tempo 2 (p2): %.4f\n", p2))
        cat(sprintf("  Nível de Significância (α): %.2f\n", input$alpha_q5))
        cat(sprintf("  Tamanho da Amostra (n): %.0f\n\n", input$n_q5_fixo))
        
        cat("Cálculo do Poder:\n")
        cat("-" %+% strrep("-", 76) %+% "\n")
        
        poder <- calc_poder_prop(input$p1_q5, p2, input$n_q5_fixo, input$alpha_q5)
        
        cat(sprintf("Poder Estatístico: %.4f (%.2f%%)\n\n", poder, poder*100))
        
        cat("Interpretação:\n")
        cat("-" %+% strrep("-", 76) %+% "\n")
        cat(sprintf("Com uma amostra de %.0f pessoas, o poder para detectar uma mudança\n", input$n_q5_fixo))
        cat(sprintf("de %.2f%% em proporção (de %.2f%% para %.2f%%) é de %.2f%%.\n\n", 
                    input$mudanca_q5*100, input$p1_q5*100, p2*100, poder*100))
        
        if (poder >= 0.80) {
          cat("✓ Este poder é ADEQUADO (≥ 80%)\n")
        } else {
          cat("✗ Este poder é INADEQUADO (< 80%)\n")
          cat("  Seria necessária uma amostra maior para atingir 80% de poder.\n")
        }
        
      }, error = function(e) {
        cat("Erro:", conditionMessage(e), "\n")
      })
    })
  })
  
  # Trigger inicial para Q5
  observeEvent(TRUE, {
    shinyjs::runjs("$('#calcular_q5').click();")
  }, once = TRUE)
  
  # --- Tabela de Resumo ---
  output$tabela_resumo <- renderDataTable({
    data.frame(
      "Questão" = c("Q1 (a)", "Q1 (b)", "Q1 (c)", "Q2", "Q3", "Q4 (a)", "Q4 (b)", "Q4 (c)", "Q5"),
      "Descrição" = c(
        "CV = 10% (p=0.71)",
        "SE = 3% (p=0.71)",
        "MOE = 3% (p=0.71)",
        "MOE = 0.5% (p=0.02)",
        "Detectar mudança em renda",
        "IMC: aumento 1.5%",
        "IMC: aumento 3.0%",
        "IMC: aumento 0.5%",
        "Poder para p=0.08"
      ),
      "Tamanho/Poder" = c(
        sprintf("%.0f", calc_n_prop_cv(0.71, 0.10)),
        sprintf("%.0f", calc_n_prop_ep(0.71, 0.03)),
        sprintf("%.0f", calc_n_prop_moe(0.71, 0.03)),
        sprintf("%.0f", calc_n_prop_moe(0.02, 0.005)),
        sprintf("%.0f", calc_n_media_poder(6100, 7000, 6100*sqrt(2.5))),
        sprintf("%.0f", calc_n_media_poder(17.5, 17.5*1.015, 0.70)),
        sprintf("%.0f", calc_n_media_poder(17.5, 17.5*1.030, 0.70)),
        sprintf("%.0f", calc_n_media_poder(17.5, 17.5*1.005, 0.70)),
        sprintf("%.2f%%", calc_poder_prop(0.08, 0.065, 500)*100)
      )
    )
  }, options = list(pageLength = 10, searching = FALSE))
  
  # --- Gráfico de Comparação Geral ---
  output$plot_comparacao_geral <- renderPlot({
    tamanhos <- c(
      calc_n_prop_cv(0.71, 0.10),
      calc_n_prop_ep(0.71, 0.03),
      calc_n_prop_moe(0.71, 0.03),
      calc_n_prop_moe(0.02, 0.005),
      calc_n_media_poder(6100, 7000, 6100*sqrt(2.5)),
      calc_n_media_poder(17.5, 17.5*1.015, 0.70),
      calc_n_media_poder(17.5, 17.5*1.030, 0.70),
      calc_n_media_poder(17.5, 17.5*1.005, 0.70)
    )
    
    df <- data.frame(
      Questao = c("Q1(a)", "Q1(b)", "Q1(c)", "Q2", "Q3", "Q4(a)", "Q4(b)", "Q4(c)"),
      Tamanho = tamanhos,
      Tipo = c("Proporção", "Proporção", "Proporção", "Proporção", "Média", "Média", "Média", "Média")
    )
    
    ggplot(df, aes(x = reorder(Questao, -Tamanho), y = Tamanho, fill = Tipo)) +
      geom_bar(stat = "identity", color = "black", size = 1) +
      geom_text(aes(label = sprintf("%.0f", Tamanho)), vjust = -0.5, size = 4, fontface = "bold") +
      scale_fill_manual(values = c("Proporção" = "#2E86AB", "Média" = "#A23B72")) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.text = element_text(size = 11),
        legend.position = "bottom"
      ) +
      labs(
        title = "Comparação Geral: Tamanhos Amostrais de Todas as Questões",
        y = "Tamanho de Amostra (n)",
        x = "Questão",
        fill = "Tipo de Estimação"
      )
  })
  
  # --- Relatório Completo ---
  output$full_report <- renderUI({
    HTML("
      <div style='padding: 20px; background-color: #f5f5f5; border-radius: 5px;'>
        <h2>Relatório Técnico Completo</h2>
        <h4>Autor: Diogo da Silva Rego (Matrícula: 20240045381)</h4>
        <p><strong>Disciplina:</strong> 1108202 - AMOSTRAGEM I</p>
        <p><strong>Professor:</strong> Hemílio Fernandes Campos Coêlho</p>
        <p><strong>Data:</strong> 01/03/2026</p>
        <hr>
        
        <h3>Resumo Executivo</h3>
        <p>Esta aplicação Shiny resolve todos os exercícios da Lista de Exercícios sobre Cálculo de Tamanho de Amostra, 
        integrando conceitos de amostragem, inferência estatística e testes de hipótese.</p>
        
        <h3>Funções Implementadas</h3>
        <ul>
          <li><b>calc_n_prop_cv:</b> Tamanho de amostra para proporção baseado em Coeficiente de Variação.</li>
          <li><b>calc_n_prop_ep:</b> Tamanho de amostra para proporção baseado em Erro Padrão.</li>
          <li><b>calc_n_prop_moe:</b> Tamanho de amostra para proporção baseado em Margem de Erro.</li>
          <li><b>calc_n_media_poder:</b> Tamanho de amostra para detectar mudança em média com poder estatístico.</li>
          <li><b>calc_poder_prop:</b> Poder estatístico para detectar mudança em proporção com tamanho fixo.</li>
          <li><b>calc_semi_largura_ic:</b> Semi-largura do intervalo de confiança.</li>
        </ul>
        
        <h3>Questões Resolvidas</h3>
        <p><b>Questão 01:</b> Tamanho de amostra para estimar proporções (71%, 52%, 7%) com diferentes critérios de precisão.</p>
        <p><b>Questão 02:</b> Estimação de prevalência rara (2%) com MOE de 0,5% e análise de sensibilidade.</p>
        <p><b>Questão 03:</b> Tamanho de amostra para detectar mudança em renda média (de $6100 para $7000).</p>
        <p><b>Questão 04:</b> Tamanho de amostra para detectar mudanças em IMC (1,5%, 3,0%, 0,5%).</p>
        <p><b>Questão 05:</b> Poder para detectar mudança em taxa de desemprego com amostra fixa (n=500).</p>
        
        <hr>
        <p><i>Clique em 'Baixar Relatório Final' para gerar um arquivo HTML com todas as resoluções detalhadas.</i></p>
      </div>
    ")
  })
  
  # --- Download do Relatório ---
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste0("Relatorio_Amostragem_DiogoRego_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
    },
    content = function(file) {
      report_html <- paste0(
        "<!DOCTYPE html>",
        "<html>",
        "<head>",
        "<meta charset='UTF-8'>",
        "<title>Relatório de Amostragem</title>",
        "<style>",
        "body { font-family: 'Arial', sans-serif; margin: 30px; background-color: #f9f9f9; line-height: 1.6; }",
        "h1, h2, h3 { color: #2E86AB; }",
        ".section { background-color: white; padding: 20px; margin: 15px 0; border-radius: 5px; border-left: 5px solid #2E86AB; }",
        "table { border-collapse: collapse; width: 100%; margin: 15px 0; }",
        "th, td { border: 1px solid #ddd; padding: 12px; text-align: left; }",
        "th { background-color: #2E86AB; color: white; font-weight: bold; }",
        "tr:nth-child(even) { background-color: #f2f2f2; }",
        ".formula { background-color: #f0f0f0; padding: 10px; margin: 10px 0; border-radius: 3px; font-family: monospace; }",
        ".result { background-color: #e8f4f8; padding: 10px; margin: 10px 0; border-radius: 3px; font-weight: bold; }",
        "</style>",
        "</head>",
        "<body>",
        "<h1>Relatório Final de Amostragem</h1>",
        "<p><strong>Autor:</strong> Diogo da Silva Rego</p>",
        "<p><strong>Matrícula:</strong> 20240045381</p>",
        "<p><strong>Disciplina:</strong> 1108202 - AMOSTRAGEM I</p>",
        "<p><strong>Professor:</strong> Hemílio Fernandes Campos Coêlho</p>",
        "<p><strong>Data de Geração:</strong> ", format(Sys.time(), "%d/%m/%Y %H:%M:%S"), "</p>",
        "<hr>",
        
        "<div class='section'>",
        "<h2>Questão 01: Tamanho de Amostra para Proporções</h2>",
        "<p><strong>Proporções:</strong> p₁ = 0.71, p₂ = 0.52, p₃ = 0.07</p>",
        "<table>",
        "<tr><th>Critério</th><th>p₁ = 0.71</th><th>p₂ = 0.52</th><th>p₃ = 0.07</th></tr>",
        "<tr><td>CV = 10%</td><td>", calc_n_prop_cv(0.71, 0.10), "</td><td>", calc_n_prop_cv(0.52, 0.10), "</td><td>", calc_n_prop_cv(0.07, 0.10), "</td></tr>",
        "<tr><td>SE = 3%</td><td>", calc_n_prop_ep(0.71, 0.03), "</td><td>", calc_n_prop_ep(0.52, 0.03), "</td><td>", calc_n_prop_ep(0.07, 0.03), "</td></tr>",
        "<tr><td>MOE = 3%</td><td>", calc_n_prop_moe(0.71, 0.03), "</td><td>", calc_n_prop_moe(0.52, 0.03), "</td><td>", calc_n_prop_moe(0.07, 0.03), "</td></tr>",
        "</table>",
        "</div>",
        
        "<div class='section'>",
        "<h2>Questão 02: Prevalência Rara</h2>",
        "<p><strong>Proporção Estimada:</strong> 2%</p>",
        "<p><strong>Margem de Erro:</strong> 0.5%</p>",
        "<p><strong>Tamanho de Amostra Necessário:</strong> <span class='result'>", calc_n_prop_moe(0.02, 0.005), "</span></p>",
        "</div>",
        
        "<div class='section'>",
        "<h2>Questão 03: Mudança em Renda Média</h2>",
        "<p><strong>Mudança:</strong> $6100 → $7000</p>",
        "<p><strong>Poder:</strong> 80% | <strong>Nível de Significância:</strong> 5%</p>",
        "<p><strong>Tamanho de Amostra Necessário:</strong> <span class='result'>", 
        calc_n_media_poder(6100, 7000, 6100*sqrt(2.5)), "</span></p>",
        "</div>",
        
        "<div class='section'>",
        "<h2>Questão 04: Mudança em IMC</h2>",
        "<table>",
        "<tr><th>Cenário</th><th>Aumento</th><th>Tamanho (n)</th></tr>",
        "<tr><td>(a) 1,5%</td><td>", sprintf("%.4f", 17.5*0.015), "</td><td>", 
        calc_n_media_poder(17.5, 17.5*1.015, 0.70), "</td></tr>",
        "<tr><td>(b) 3,0%</td><td>", sprintf("%.4f", 17.5*0.030), "</td><td>", 
        calc_n_media_poder(17.5, 17.5*1.030, 0.70), "</td></tr>",
        "<tr><td>(c) 0,5%</td><td>", sprintf("%.4f", 17.5*0.005), "</td><td>", 
        calc_n_media_poder(17.5, 17.5*1.005, 0.70), "</td></tr>",
        "</table>",
        "</div>",
        
        "<div class='section'>",
        "<h2>Questão 05: Poder para Proporção</h2>",
        "<p><strong>Proporção Tempo 1:</strong> 8%</p>",
        "<p><strong>Mudança:</strong> 1,5 ponto percentual</p>",
        "<p><strong>Tamanho da Amostra:</strong> 500</p>",
        "<p><strong>Poder Estatístico:</strong> <span class='result'>", 
        sprintf("%.2f%%", calc_poder_prop(0.08, 0.065, 500)*100), "</span></p>",
        "</div>",
        
        "<hr>",
        "<p><em>Relatório gerado automaticamente pela Aplicação Shiny de Amostragem.</em></p>",
        "</body>",
        "</html>"
      )
      
      writeLines(report_html, file)
    }
  )
}

# --- Rodar a Aplicação ---
shinyApp(ui = ui, server = server)
