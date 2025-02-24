#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)
library(signs)
library(gfonts)
library(ragg)
library(bslib)
library(katex)
library(shinyWidgets)
# gfonts::setup_font("roboto-condensed", output_dir = "www", variants = "regular")

my_katex <- purrr::partial(katex::katex_html, displayMode = F, preview = F)
my_labels <- function(x, space = " ", ...) {
  x_new <- paste0(signs::signs(x, ...), ifelse(x < 0, space, 
                                               ""))
  Encoding(x_new) <- "UTF-8"
  x_new
}


# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = bslib::bs_theme(primary = "#1e6b65"),
  tags$head(
    tags$link(
      rel="stylesheet",
      href="https://cdn.jsdelivr.net/npm/katex@0.16.4/dist/katex.min.css",
      integrity="sha384-vKruj+a13U8yHIkAyGgK1J3ArTLzrFGBbBc0tDp4ad/EyewESeXE/Iv67Aj8gKZ0",
      crossorigin="anonymous"
    )
  ),
  tags$link(rel = "stylesheet", type = "text/css", href = "css/roboto-condensed.css"),
  tags$style("body {font-family: 'Roboto Condensed', sans-serif;}\n label {font-weight: 400; font-size: 10pt}\n td {text-align: center; padding: 5px}"),
    # Application title
    titlePanel("Multilevel Model with Three Levels"),

    # Sidebar ----
    sidebarLayout(
        sidebarPanel(
          width = 5,
          fluidRow(
            column(8,
                   HTML("<h5><strong>Level 1:</strong> ", my_katex("Y_{ijk}=b_{0jk}+b_{1jk}S_{ijk}+e_{ijk}"), "</h5>"),
                   HTML(paste0("<small>",my_katex(
                     '\\begin{aligned}Y_{ijk}&=\\text{Reading Score for Student } i \\text{ in class } j \\text{ in school } k\\\\
            S_{ijk}&=\\text{Effort for Student }i \\text{ in class } j \\text{ in school } k\\\\
            T_{jk}&=\\text{Effort for Teacher in class } j \\text{ in school } k\\\\
            P_{k}&=\\text{Effort for Principal in school } k
            \\end{aligned}'), "</small>")),), 
            column(4,sliderInput("tau1",
                                 HTML(paste0(my_katex("\\tau_{1}"), " (L1 Variance)")),
                                 value = 10, 
                                 min = 0, 
                                 max = 100, 
                                 step = .01))),
          
          # tags$hr(style = "border-color: #505050"),
          ## lb b0jk ----
          fluidRow(
            style = "border-top: 1px solid #aaa; padding-top: 7px; margin-top: 3px",
            column(width = 8,uiOutput("b0jk")),
            column(width = 4,
                   sliderInput("tau200",
                               HTML(paste0(my_katex("\\tau_{2.00}"), " (L2 Intercept Variance)")),
                                      value = 0, 
                                      min = 0, 
                                      max = 100, 
                                      step = .01))),
          ### lb b00k ----
          uiOutput("b00k"),
          fluidRow(column(
            width = 4,
            sliderInput(
              "b000",
              HTML(paste0(my_katex("b_{000}"), " (Fixed Intercept)")),
              min = 0,
              max = 200,
              value = 100,
              step = .1
            )
          ),
          column(
            width = 4,
            sliderInput(
              "b001",
              HTML(paste0(my_katex("b_{001}"), " (Principal Slope)")),
              min = -5,
              max = 5,
              value = 0,
              step = .01
            )
          ),
          column(width = 4,
                 sliderInput(
                   "tau300",
                   HTML(paste0(my_katex("\\tau_{3.00}"), " (L3 Intercept Variance)")),
                   min = 0,
                   max = 100,
                   value = 0,
                   step = .1
                 ))), 
          ### lb b01k ----
          uiOutput("b01k"),
          fluidRow(column(
            width = 4,
            sliderInput(
              "b010",
              HTML(paste0(my_katex("b_{010}"), " (Teacher Slope)")),
              min = -5,
              max = 5,
              value = 0,
              step = .01
            )
          ),
          column(
            width = 4,
            sliderInput(
              "b011",
              HTML("<em>b</em><sub>011</sub> (T&times;P Interaction)"),
              min = -5,
              max = 5,
              value = 0,
              step = .01
            )
          ),
          column(width = 4,
                 sliderInput(
                   "tau311",
                   HTML(paste0(my_katex("\\tau_{3.11}"), " (L3 T Slope Variance)")),
                   min = 0,
                   max = 100,
                   value = 0,
                   step = .1
                 ))), 
          ## lb b1jk ----
          fluidRow(style = "border-top: 1px solid #aaa; padding-top: 7px; margin-top: 3px",
                   column(width = 8,uiOutput("b1jk")),
                   column(width = 4,
                          sliderInput(
                            "tau211",
                            HTML(paste0(my_katex("\\tau_{1.11}"), " (L2 S Slope Variance)")),
                                      value = 0, min = 0, max = 100, step = .01))),
          ### lb b10k ----
          uiOutput("b10k"),
          fluidRow(column(
            width = 4,
            sliderInput(
              "b100",
              HTML(paste0(my_katex("b_{100}"), " (Student Slope)")),
              min = -5,
              max = 5,
              value = 0,
              step = .01
            )
          ),
          column(
            width = 4,
            sliderInput(
              "b101",
              HTML(paste0(my_katex("b_{101}"), " (S&times;P Interaction)")),
              min = -5,
              max = 5,
              value = 0,
              step = .01
            )
          ),
          column(width = 4,
                 sliderInput(
                   "tau322",
                   HTML(paste0(my_katex("\\tau_{3.22}"), " (L3 S Slope Variance)")),
                   min = 0,
                   max = 100,
                   value = 0,
                   step = .1
                 ))), 
          ### lb b11k ----
          uiOutput("b11k"),
          fluidRow(column(
            width = 4,
            sliderInput(
              "b110",
              HTML(paste0(my_katex("b_{110}"), " (S &times; T Interaction)")),
              min = -5,
              max = 5,
              value = 0,
              step = .01
            )
          ),
          column(
            width = 4,
            sliderInput(
              "b111",
              HTML(paste0(my_katex("b_{111}"), " (S &times; T &times; P Interaction)")),
              min = -5,
              max = 5,
              value = 0,
              step = .01
            )
          ),
          column(width = 4,
                 sliderInput(
                   "tau333",
                   HTML(paste0(my_katex("\\tau_{3.33}"), " (L3 S &times; T Interaction Variance)")),
                   min = 0,
                   max = 100,
                   value = 0,
                   step = .1
                 ))),
          sliderInput("tau210", "L2 Slope-Intercept Correlation", min = -1, max = 1, value = 0, step = .01),
          strong("L3 Random Effect Correlations"),
          fluidRow(tags$table(
            tags$colgroup(tags$col(style="width:2cm"),
                          tags$col(style="width:auto"),
                          tags$col(style="width:auto"),
                          tags$col(style="width:auto")),
            tags$tr(
              tags$td(),
              tags$td(HTML(my_katex("e_{00k}"))),
              tags$td(HTML(my_katex("e_{01k}"))),
              tags$td(HTML(my_katex("e_{10k}")))
            ),
            tags$tr(
              tags$td(HTML(my_katex("e_{01k}"))),
              tags$td(sliderInput("tau310", NULL, min = -1, max = 1, value = 0, step = .01)),
              tags$td(),
              tags$td()
            ),
            tags$tr(
              tags$td(HTML(my_katex("e_{10k}"))),
              tags$td(sliderInput("tau320", NULL, min = -1, max = 1, value = 0, step = .01)),
              tags$td(sliderInput("tau321", NULL, min = -1, max = 1, value = 0, step = .01)),
              tags$td()
            ),
            tags$tr(
              tags$td(HTML(my_katex("e_{11k}"))),
              tags$td(sliderInput("tau330", NULL, min = -1, max = 1, value = 0, step = .01)),
              tags$td(sliderInput("tau331", NULL, min = -1, max = 1, value = 0, step = .01)),
              tags$td(sliderInput("tau332", NULL, min = -1, max = 1, value = 0, step = .01))
            )
          ))
                   
          
        ),

        # Mainpanel ----
        mainPanel(width = 7,
            plotOutput("distPlot", width = 800, height = 800),
            uiOutput("equation")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    # output$b0jk <- renderUI({
    #   withMathJax(h5(paste0("Level 2 Intercept \\(b_{0jk}=b_{00k}+b_{01k}T_{jk}",
    #          ifelse(input$e0jk, "+e_{0jk}", ""),
    #          "\\)")))
    # })
  
  output$b0jk <- renderUI(
    HTML("<strong>Level 2 Intercept:</strong> ", my_katex(paste0("b_{0jk}=b_{00k}+b_{01k}T_{jk}",
              ifelse(input$tau200 > 0, "+e_{0jk}", "")
              )))
  )
  #  Coefficients ----
    ## b00k----
    output$b00k <- renderUI({
      b000 <- input$b000
      b001 <- input$b001
      tau300 <- input$tau300
      eq <- c(b000, b001, tau300)
      eq_display <- c(paste0("\\underset{b_{000}}{",b000,"}"),
                      paste0("\\underset{b_{001}}{",b001,"}P_{k}"),
                      "e_{00k}")
      eq_display[eq == 0] <- NA
      
      HTML("<strong>Level 3 Intercept:<strong> ", katex_html(paste0("b_{00k}=",
                                  paste0(eq_display[!is.na(eq_display)], collapse = "+")), displayMode = F, preview = F))
      
      # withMathJax(helpText(paste0("Level 3 Intercept: \\(b_{00k}=",
      #                             paste0(eq_display[!is.na(eq_display)], collapse = "+"),
      #                       "\\)")))
    })
    ## b01k----
    output$b01k <- renderUI({
      b010 <- input$b010
      b011 <- input$b011
      tau311 <- input$tau311
      eq <- c(b010, b011, tau311)
      eq_display <- c(paste0("\\underset{b_{010}}{",b010,"}"),
                      paste0("\\underset{b_{011}}{",b011,"}P_{k}"),
                      "e_{01k}")
      eq_display[eq == 0] <- NA
      eq_display <- paste0(eq_display[!is.na(eq_display)],
                          collapse = "+")
      if (eq_display != "") {
        eq_display <- my_katex(paste0("b_{01k}=", eq_display))
      }
      
      HTML("<strong>Level 3 Slope (Teacher):</strong> ", 
           eq_display)
    })
    
    ## b1jk----
    

    output$b1jk <- renderUI({
      HTML(paste0("<strong>Level 2 Slope (Student):</strong> ",my_katex(paste0("b_{1jk}=b_{10k}+b_{11k}T_{jk}",
                            ifelse(input$tau211 > 0, "+e_{1jk}", "")
                            ))))
    })
    
    output$b10k <- renderUI({
      withMathJax(helpText(paste0("Level 3 Slope (Student) \\(b_{10k}=b_{100}+b_{101}P_{k}",
                            ifelse(input$e10k, "+e_{10k}", ""),
                            "\\)")))
    })
    
    ## b10k----
    output$b10k <- renderUI({
      b100 <- input$b100
      b101 <- input$b101
      tau322 <- input$tau322
      eq <- c(b100, b101, tau322)
      eq_display <- c(paste0("\\underset{b_{100}}{",b100,"}"),
                      paste0("\\underset{b_{101}}{",b101,"}P_{k}"),
                      "e_{10k}")
      eq_display[eq == 0] <- NA
      eq_display <- paste0(eq_display[!is.na(eq_display)],
                          collapse = "+")
      if (eq_display != "") {
        eq_display <- my_katex(paste0("b_{10k}=", eq_display))
      }
      
      HTML("<strong>Level 3 Slope (Student):</strong> ", 
           eq_display)
    })
    
    ## b11k----
    output$b11k <- renderUI({
      b110 <- input$b110
      b111 <- input$b111
      tau333 <- input$tau333
      eq <- c(b110, b111, tau333)
      eq_display <- c(paste0("\\underset{b_{110}}{",b110,"}"),
                      paste0("\\underset{b_{111}}{",b111,"}P_{k}"),
                      "e_{11k}")
      eq_display[eq == 0] <- NA
      eq_display <- paste0(eq_display[!is.na(eq_display)],
                           collapse = "+")
      if (eq_display != "") {
        eq_display <- my_katex(paste0("b_{11k}=", eq_display))
      }
      
      HTML("<strong>Level 3 Interaction (Student &times; Teacher):</strong> ", 
           eq_display)
    })
    

    

    # Equation ----

    output$equation <- renderUI({
      b000 <- input$b000
      b100 <- input$b100
      b010 <- input$b010
      b001 <- input$b001
      b110 <- input$b110
      b101 <- input$b101
      b011 <- input$b011
      b111 <- input$b111
      
      
      b000 <- ifelse(b000 == 0, NA, paste0("\\underset{b_{000}}{", b000, "}"))
      b001 <- ifelse(b001 == 0, NA, paste0("\\underset{b_{001}}{", b001, "}P_{k}"))
      b010 <- ifelse(b010 == 0, NA, paste0("\\underset{b_{010}}{", b010, "}"))
      b011 <- ifelse(b011 == 0, NA, paste0("\\underset{b_{011}}{", b011, "}P_{k}"))
      
      b100 <- ifelse(b100 == 0, NA, paste0("\\underset{b_{100}}{", b100, "}"))
      b101 <- ifelse(b101 == 0, NA, paste0("\\underset{b_{101}}{", b101, "}P_{k}"))
      b110 <- ifelse(b110 == 0, NA, paste0("\\underset{b_{110}}{", b110, "}"))
      b111 <- ifelse(b111 == 0, NA, paste0("\\underset{b_{111}}{", b111, "}P_{k}"))
      
      e00k <- ifelse(input$tau300 > 0, "e_{00k}", NA)
      e01k <- ifelse(input$tau311 > 0, "e_{01k}", NA)
      e10k <- ifelse(input$tau322 > 0, "e_{10k}", NA)
      e11k <- ifelse(input$tau333 > 0, "e_{11k}", NA)
      
      e0jk <- ifelse(input$tau200 > 0, "e_{0jk}", NA)
      e1jk <- ifelse(input$tau211 > 0, "e_{1jk}", NA)
      
      v <- c(b000, b001, e00k)
      b00k <- paste0(v[!is.na(v)], collapse = "+")
      b00k <- ifelse(b00k == "", NA, 
                     paste0("\\left(\\underbrace{",b00k,"}_{b_{00k}}\\right)"))
      
      v <- c(b010, b011, e01k)
      b01k <- paste0(v[!is.na(v)], collapse = "+")
      b01k <- ifelse(b01k == "", NA, 
                     paste0("\\left(\\underbrace{",b01k,"}_{b_{01k}}\\right)T_{jk}"))
      
      

      v <- c(b100, b101, e10k)
      b10k <- paste0(v[!is.na(v)], collapse = "+")
      b10k <- ifelse(b10k == "", NA, 
                     paste0("\\left(\\underbrace{",b10k,"}_{b_{10k}}\\right)"))
      
  
      
      v <- c(b110, b111, e11k)
      b11k <- paste0(v[!is.na(v)], collapse = "+")
      b11k <- ifelse(b11k == "", NA, 
                     paste0("\\left(\\underbrace{",b11k,"}_{b_{11k}}\\right)T_{jk}"))
      
      
      v <- c(b00k, b01k, e0jk)

      b0jk <- paste0(v[!is.na(v)], collapse = "+")
      b0jk <- ifelse(b0jk == "",NA,
                     paste0("\\underbrace{",
                            b0jk,
                            "}_{b_{0jk}}"))
      
      
      v <- c(b10k, b11k, e1jk)
      b1jk <- paste0(v[!is.na(v)], collapse = "+")
      b1jk <- ifelse(b1jk == "",NA,
                     paste0("\\left(\\underbrace{",
                            b1jk,
                            "}_{b_{1jk}}\\right)S_{ijk}"))

      v <- c(b0jk, b1jk)
      v_fixed <- c(b000, b001)
      v_fixed <- paste0(v_fixed[!is.na(v_fixed)], 
                        collapse = "+")
      v_fixed_teacher <- c(b010, b011)
      v_fixed_teacher <- paste0(v_fixed_teacher[!is.na(v_fixed_teacher)], 
                                collapse = "+")
      v_fixed_teacher <- ifelse(v_fixed_teacher == "",NA,
                                paste0(ifelse(is.na(b011),"", "\\left("),
                                       v_fixed_teacher,
                                       ifelse(is.na(b011), "","\\right)"), "T_{jk}"))
      
      v_fixed_student <- c(b100, b101)
      v_fixed_student <- paste0(v_fixed_student[!is.na(v_fixed_student)], 
                                collapse = "+")
      
      v_fixed_student_teacher <- c(b110, b111)
      
      v_fixed_student_teacher <- paste0(
        v_fixed_student_teacher[!is.na(v_fixed_student_teacher)], 
        collapse = "+")
      
      v_fixed_student_teacher <- ifelse(v_fixed_student_teacher == "",NA,
                                paste0("\\left(",
                                       v_fixed_student_teacher,
                                       "\\right)T_{jk}"))
      
      
      v_student_dynamic <- c(v_fixed_student, v_fixed_student_teacher)
      v_student_dynamic[v_student_dynamic == ""] <- NA
      v_student_dynamic <- paste0(
        v_student_dynamic[!is.na(v_student_dynamic)], 
        collapse = "+")

      v_student_dynamic <- ifelse(v_student_dynamic == "",NA,
                                        paste0("\\left(",
                                               v_student_dynamic,
                                               "\\right)S_{ijk}"))
      
      v_eq <- c(v_fixed, v_fixed_teacher, v_student_dynamic)
      v_eq <- paste0(v_eq[!is.na(v_eq)], collapse = "+")
      v_eq <- ifelse(v_eq == "", "", paste0("\\underbrace{", v_eq, "}_{\\text{Fixed}}"))
      
      e01k <- ifelse(is.na(e01k), NA, paste0(e01k, "T_{jk}"))
      e11k <- ifelse(is.na(e11k), NA, paste0(e11k, "T_{jk}"))
      
      v_random_3_student <- c(e10k, e11k)
      v_random_3_student <- paste0(
        v_random_3_student[!is.na(v_random_3_student)], 
        collapse = "+")
     
      v_random_3_student <- ifelse(
        v_random_3_student == "",
        NA, 
        paste0(ifelse(is.na(e11k), "","\\left("),
               v_random_3_student, 
               ifelse(is.na(e11k), "", "\\right)"),"S_{ijk}"))

      e_3 <- c(e00k, 
               e01k,
               v_random_3_student)
      e_3 <- paste0(e_3[!is.na(e_3)], collapse = "+")
      e_3 <- ifelse(e_3 == "", NA, paste0("\\underbrace{", e_3,"}_{\\text{L3}}"))
      
      e1jk <- ifelse(is.na(e1jk), NA, paste0(e1jk, "S_{ijk}"))
      e_2 <- c(e0jk, e1jk)
      e_2 <- paste0(e_2[!is.na(e_2)], collapse = "+")
      e_2 <- ifelse(e_2 == "", NA, paste0("\\underbrace{", e_2,"}_{\\text{L2}}"))
      
      e <- c(e_3, e_2, "\\underbrace{e_{ijk}}_{\\text{L1}}")
      e <- paste0(e[!is.na(e)], collapse = "+")
      e <- paste0("+\\underbrace{", e,"}_{\\text{Random}}")
      
      tau1 <- paste0("\\\\ e_{ijk}&=\\mathcal{N}\\left(0,\\underbrace{", input$tau1,"}_{\\tau_1}\\right)")
      
      tau2 <- paste0("
      \\\\ \\begin{bmatrix}e_{0jk}\\\\ e_{1jk}\\end{bmatrix}&=\\mathcal{N}\\left(\\boldsymbol{0},\\begin{bmatrix}
      \\underset{\\tau_{2.00}}{", input$tau200,"} & \\\\
      \\underset{\\tau_{2.10}}{", round(input$tau210 * sqrt(input$tau200 * input$tau211), 2),"}&\\underset{\\tau_{2.11}}{", input$tau211,"}
\\end{bmatrix}\\right)")
      
      tau3 <- paste0("
      \\\\ \\begin{bmatrix}e_{00k}\\\\ e_{01k}\\\\ e_{10k}\\\\ e_{11k}\\end{bmatrix}&=\\mathcal{N}\\left(\\boldsymbol{0},\\begin{bmatrix}
      \\underset{\\tau_{3.00}}{", input$tau300,"} & & & \\\\
      \\underset{\\tau_{3.10}}{", round(input$tau310 * sqrt(input$tau300 * input$tau311), 2),"}&\\underset{\\tau_{3.11}}{", input$tau311,"} & & \\\\
      \\underset{\\tau_{3.20}}{", round(input$tau320 * sqrt(input$tau300 * input$tau322), 2),"}&\\underset{\\tau_{3.21}}{", round(input$tau321 * sqrt(input$tau311 * input$tau322), 2),"}&\\underset{\\tau_{3.22}}{", input$tau322,"} &
      \\\\
      \\underset{\\tau_{3.30}}{", round(input$tau330 * sqrt(input$tau300 * input$tau333), 2),"}&\\underset{\\tau_{3.31}}{", round(input$tau331 * sqrt(input$tau311 * input$tau333), 2),"}&\\underset{\\tau_{3.32}}{", round(input$tau332 * sqrt(input$tau322 * input$tau333), 2),"}&\\underset{\\tau_{3.33}}{", input$tau333,"}
\\end{bmatrix}\\right)")

      HTML(my_katex(
        paste0("\\begin{aligned}Y_{ijk}&=",
            paste0(c(paste0(v[!is.na(v)], collapse = "+"), "e_{ijk}"), collapse = "+"),  
            "\\\\ Y_{ijk}&=",
            v_eq,
            e,
            tau1,
            tau2,
            tau3,
            "\\end{aligned}")))
      
    })

    output$distPlot <- renderPlot({
      
      b000 <- input$b000
      b100 <- input$b100
      b010 <- input$b010
      b001 <- input$b001
      b110 <- input$b110
      b101 <- input$b101
      b011 <- input$b011
      b111 <- input$b111
      
      tidyr::crossing(A = c(-2,2), 
                      B = -1:1,
                      C = -1:1) %>% 
        mutate(Y = b000 + b100 * A + b010 * B + b001 * C + b110 * A * B + b101 * A * C + b011 * B * C + b111 * A * B * C,
               fB = factor(B, labels = c("−1SD", "Mean", "+1SD")),
               `Principal Effort` = factor(C, labels = c("−1SD", "Mean", "+1SD"))) %>% 
        ggplot(aes(A, Y)) + 
        geom_line(aes(color = fB), linewidth = 1) + 
        facet_grid(cols = vars(`Principal Effort`), labeller = label_both) +
        coord_cartesian(ylim = b000 + c(-12,12)) + 
        scale_color_manual("Teacher Effort", values = c(`−1SD` = "royalblue4", `Mean` = "gray30", `+1SD` = "firebrick4")) +
        scale_x_continuous("Student Effort", labels = my_labels) +
        scale_y_continuous("Predicted Reading") +
        theme_light(base_size = 20, base_family = "Roboto Condensed") + 
        theme(legend.position = "top") + 
          labs(caption = "Level 1 = Student, Level 2 = Teacher, Level 3 = Principal")
        
        
       
    }, width = 800, height = 800)
}

# Run the application 
shinyApp(ui = ui, server = server)
