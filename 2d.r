# 필요한 라이브러리 불러오기
library(shiny)
library(readxl)
library(tidyverse)
library(writexl)
library(stringr)
library(gt)
library(showtext) # 한글
library(viridis) #특정 color 묶음
library(patchwork)
showtext_auto()

# UI 함수 정의
ui <- fluidPage(
  # 데이터 불러오기 버튼
  actionButton("load_data", "데이터 불러오기"),
  
  # Polar Chart 그리기 버튼
  actionButton("draw_polar_chart", "Polar Chart 그리기"),
  
  # Contour Graph 보이기 버튼
  actionButton("show_contour_graph", "Contour Graph 보이기"),
  
  # 이미지 저장 버튼
  actionButton("save_image", "이미지 저장"),
  
  # Polar Chart 출력
  plotOutput("polar_chart"),
  
  # Contour Graph 출력
  plotOutput("contour_graph")
)

# 서버 함수 정의
server <- function(input, output) {
  # 데이터 불러오기
  df <- eventReactive(input$load_data, {
    read.csv("D:/Non_Documents/AI/R/data/dE.csv")
  })
  
  # Polar Chart 그리기
  output$polar_chart <- renderPlot({
    req(input$draw_polar_chart)
    req(df())
    
    # 데이터 처리
    df() |> 
      select(1:3) |> 
      filter(Theta %in% c(seq(0,88,10)), Phi %in% seq(0,360,10)) -> df_processed
    
    df_processed |> 
      pivot_longer(-1, names_to = "Phi", values_to = "L") |> 
      mutate(Phi = as.numeric(Phi))
    
    # Polar Chart 그리기
    PolarImagePlot(Mat_ref)
  })
  
  # Contour Graph 보이기
  output$contour_graph <- renderPlot({
    req(input$show_contour_graph)
    req(df())
    
    # 데이터 처리
    df() |> 
      select(1:3) |> 
      filter(Theta %in% c(seq(0,88,10)), Phi %in% seq(0,360,10)) -> df_processed
    
    df_processed |> 
      pivot_longer(-1, names_to = "Phi", values_to = "L") |> 
      mutate(Phi = as.numeric(Phi))
    
    # Contour Graph 그리기
    p1 <- df_processed  %>%
      select(Phi, Theta, L) %>%
      mutate(direction  = ifelse(Phi %in% c(0, 180), "좌우", "상하")) %>%
      mutate(Theta = ifelse(Phi==180 | Phi == 270, -Theta, Theta)) %>%
      filter(direction == "좌우") %>%
      ggplot(aes(x=Theta, y=L, col=L))+
      geom_point()+geom_line()+
      theme_bw()+
      theme(legend.position = "none")+
      labs(y="Luminance", x = "Theta angle[˚]",
           title = "8k normal(w/o MBM) Cut off 특성",
           subtitle = "좌우방향")
    
    p2 <- data.frame(Phi = df_processed$Phi[1:(nrow(df_processed)-1)], dY = diff(df_processed$L)/diff(df_processed$Theta),
                     Theta = rowMeans(embed(df_processed$Theta,2))) %>%
      mutate(direction  = ifelse(Phi %in% c(0, 180), "좌우", "상하")) %>%
      mutate(Theta = ifelse(Phi==180 | Phi == 270, -Theta, Theta)) %>%
      filter(direction == "좌우") %>%
      
      ggplot(aes(x=Theta, y=dY))+
      
      geom_smooth()+
      theme_bw()+
      theme(legend.position = "none")+
      labs(y="dL/dx", x = "Theta angle[˚]",
           title = "8k normal(w/o MBM) Cut off 특성",
           subtitle = paste0(panel_color))
    
    grid.arrange(p1, p2)
  })
  
  # 이미지 저장
  observeEvent(input$save_image, {
    req(df())
    
    # 데이터 처리
    df() |> 
      select(1:3) |> 
      filter(Theta %in% c(seq(0,88,10)), Phi %in% seq(0,360,10)) -> df_processed
    
    df_processed |> 
      pivot_longer(-1, names_to = "Phi", values_to = "L") |> 
      mutate(Phi = as.numeric(Phi))
    
    # Polar Chart 그리기
    p <- PolarImagePlot(Mat_ref)
    
    # 이미지 저장
    ggsave("polar_chart.png", p, width = 10, height = 10, dpi = 300)
  })
}

# Shiny App 실행
shinyApp(ui = ui, server = server)


# UI 함수 정의
ui <- fluidPage(
  # 데이터 불러오기 버튼
  fileInput("load_data", "데이터 불러오기", accept = ".csv"),
  
  # Polar Chart 그리기 버튼
  actionButton("draw_polar_chart", "Polar Chart 그리기"),
  
  # Contour Graph 보이기 버튼
  actionButton("show_contour_graph", "Contour Graph 보이기"),
  
  # 이미지 저장 버튼
  actionButton("save_image", "이미지 저장"),
  
  # Polar Chart 출력
  plotOutput("polar_chart"),
  
  # Contour Graph 출력
  plotOutput("contour_graph")
)

# 서버 함수 정의
server <- function(input, output) {
  # 데이터 불러오기
  df <- reactive({
    req(input$load_data)
    read.csv(input$load_data$datapath)
  })
  
  # Polar Chart 그리기
  output$polar_chart <- renderPlot({
    req(input$draw_polar_chart)
    req(df())
    
    # 데이터 처리
    df() |> 
      select(1:3) |> 
      filter(Theta %in% c(seq(0,88,10)), Phi %in% seq(0,360,10)) -> df_processed
    
    df_processed |> 
      pivot_longer(-1, names_to = "Phi", values_to = "L") |> 
      mutate(Phi = as.numeric(Phi))
    
    # Polar Chart 그리기
    PolarImagePlot(Mat_ref)
  })
  
  # Contour Graph 보이기
  output$contour_graph <- renderPlot({
    req(input$show_contour_graph)
    req(df())
    
    # 데이터 처리
    df() |> 
      select(1:3) |> 
      filter(Theta %in% c(seq(0,88,10)), Phi %in% seq(0,360,10)) -> df_processed
    
    df_processed |> 
      pivot_longer(-1, names_to = "Phi", values_to = "L") |> 
      mutate(Phi = as.numeric(Phi))
    
    # Contour Graph 그리기
    p1 <- df_processed  %>%
      select(Phi, Theta, L) %>%
      mutate(direction  = ifelse(Phi %in% c(0, 180), "좌우", "상하")) %>%
      mutate(Theta = ifelse(Phi==180 | Phi == 270, -Theta, Theta)) %>%
      filter(direction == "좌우") %>%
      ggplot(aes(x=Theta, y=L, col=L))+
      geom_point()+geom_line()+
      theme_bw()+
      theme(legend.position = "none")+
      labs(y="Luminance", x = "Theta angle[˚]",
           title = "8k normal(w/o MBM) Cut off 특성",
           subtitle = "좌우방향")
    
    p2 <- data.frame(Phi = df_processed$Phi[1:(nrow(df_processed)-1)], dY = diff(df_processed$L)/diff(df_processed$Theta),
                     Theta = rowMeans(embed(df_processed$Theta,2))) %>%
      mutate(direction  = ifelse(Phi %in% c(0, 180), "좌우", "상하")) %>%
      mutate(Theta = ifelse(Phi==180 | Phi == 270, -Theta, Theta)) %>%
      filter(direction == "좌우") %>%
      
      ggplot(aes(x=Theta, y=dY))+
      
      geom_smooth()+
      theme_bw()+
      theme(legend.position = "none")+
      labs(y="dL/dx", x = "Theta angle[˚]",
           title = "8k normal(w/o MBM) Cut off 특성",
           subtitle = paste0(panel_color))
    
    grid.arrange(p1, p2)
  })
  
  # 이미지 저장
  observeEvent(input$save_image, {
    req(df())
    
    # 데이터 처리
    df() |> 
      select(1:3) |> 
      filter(Theta %in% c(seq(0,88,10)), Phi %in% seq(0,360,10)) -> df_processed
    
    df_processed |> 
      pivot_longer(-1, names_to = "Phi", values_to = "L") |> 
      mutate(Phi = as.numeric(Phi))
    
    # Polar Chart 그리기
    p <- PolarImagePlot(Mat_ref)
    
    # 이미지 저장
    ggsave("polar_chart.png", p, width = 10, height = 10, dpi = 300)
  })
}

# Shiny App 실행
shinyApp(ui = ui, server = server)



# UI 함수 정의
ui <- fluidPage(
  # 데이터 불러오기 버튼
  fileInput("load_data", "데이터 불러오기", accept = ".csv"),
  
  # Polar Chart 그리기 버튼
  actionButton("draw_polar_chart", "Polar Chart 그리기"),
  
  # Contour Graph 보이기 버튼
  actionButton("show_contour_graph", "Contour Graph 보이기"),
  
  # 이미지 저장 버튼
  actionButton("save_image", "이미지 저장"),
  
  # Polar Chart 출력
  plotOutput("polar_chart"),
  
  # Contour Graph 출력
  plotOutput("contour_graph")
)

# 서버 함수 정의
server <- function(input, output) {
  # 데이터 불러오기
  df <- reactive({
    req(input$load_data)
    read.csv(input$load_data$datapath)
  })
  
  # Polar Chart 그리기
  output$polar_chart <- renderPlot({
    if (is.null(input$draw_polar_chart) || input$draw_polar_chart == 0) {
      return(NULL)
    }
    
    # 데이터 처리
    df() |> 
      select(1:3) |> 
      filter(Theta %in% c(seq(0,88,10)), Phi %in% seq(0,360,10)) -> df_processed
    
    df_processed |> 
      pivot_longer(-1, names_to = "Phi", values_to = "L") |> 
      mutate(Phi = as.numeric(Phi))
    
    # Polar Chart 그리기
    PolarImagePlot(Mat_ref)
  })
  
  # Contour Graph 보이기
  output$contour_graph <- renderPlot({
    if (is.null(input$show_contour_graph) || input$show_contour_graph == 0) {
      return(NULL)
    }
    
    # 데이터 처리
    df() |> 
      select(1:3) |> 
      filter(Theta %in% c(seq(0,88,10)), Phi %in% seq(0,360,10)) -> df_processed
    
    df_processed |> 
      pivot_longer(-1, names_to = "Phi", values_to = "L") |> 
      mutate(Phi = as.numeric(Phi))
    
    # Contour Graph 그리기
    p1 <- df_processed  %>%
      select(Phi, Theta, L) %>%
      mutate(direction  = ifelse(Phi %in% c(0, 180), "좌우", "상하")) %>%
      mutate(Theta = ifelse(Phi==180 | Phi == 270, -Theta, Theta)) %>%
      filter(direction == "좌우") %>%
      ggplot(aes(x=Theta, y=L, col=L))+
      geom_point()+geom_line()+
      theme_bw()+
      theme(legend.position = "none")+
      labs(y="Luminance", x = "Theta angle[˚]",
           title = "8k normal(w/o MBM) Cut off 특성",
           subtitle = "좌우방향")
    
    p2 <- data.frame(Phi = df_processed$Phi[1:(nrow(df_processed)-1)], dY = diff(df_processed$L)/diff(df_processed$Theta),
                     Theta = rowMeans(embed(df_processed$Theta,2))) %>%
      mutate(direction  = ifelse(Phi %in% c(0, 180), "좌우", "상하")) %>%
      mutate(Theta = ifelse(Phi==180 | Phi == 270, -Theta, Theta)) %>%
      filter(direction == "좌우") %>%
      
      ggplot(aes(x=Theta, y=dY))+
      
      geom_smooth()+
      theme_bw()+
      theme(legend.position = "none")+
      labs(y="dL/dx", x = "Theta angle[˚]",
           title = "8k normal(w/o MBM) Cut off 특성",
           subtitle = paste0(panel_color))
    
    grid.arrange(p1, p2)
  })
  
  # 이미지 저장
  observeEvent(input$save_image, {
    if (is.null(input$save_image) || input$save_image == 0) {
      return(NULL)
    }
    
    # 데이터 처리
    df() |> 
      select(1:3) |> 
      filter(Theta %in% c(seq(0,88,10)), Phi %in% seq(0,360,10)) -> df_processed
    
    df_processed |> 
      pivot_longer(-1, names_to = "Phi", values_to = "L") |> 
      mutate(Phi = as.numeric(Phi))
    
    # Polar Chart 그리기
    p <- PolarImagePlot(Mat_ref)
    
    # 이미지 저장
    ggsave("polar_chart.png", p, width = 10, height = 10, dpi = 300)
  })
}

# Shiny App 실행
shinyApp(ui = ui, server = server)

