ui = fluidPage( theme=("css/style.css"),
                htmlOutput("masthead"),
                navbarPage("Modelling Infectious Epidemics",id="tabs",
                           tabPanel("The SIR model",value=1,id=1,
                                    sidebarLayout(
                                      sidebarPanel(
                                        sliderInput("ip1", HTML("Infectious period, <i>ip</i> (days)"), 
                                                    value = ip_default,
                                                    min = 1, max = ip_max, step = 1),
                                        sliderInput("R01", HTML("<i>R</i><sub>0</sub>"), value = 3,
                                                    min = 1, max = R0_max, step = R0_step),
                                        sliderInput("vac", HTML("vaccination level, <i>p</i> (%)"), 0,
                                                    min = 0, max = 99, step = 1),
                                        checkboxInput("log1", HTML("y-axis log scale"), FALSE)
                                      ),
                                      mainPanel(h3("The SIR model infection spread"),
                                                div(htmlOutput("text1"),class="copy"),
                                                div(
                                                div(htmlOutput("title1a"),class="title"),
                                                div(plotOutput("plot1"),class="plot"),
                                                div(htmlOutput("caption1"),class="caption"),
                                                class="plotbox")
                                      )
                                    )),
                           
                           tabPanel(HTML("Effect of <i>R</i><sub>0</sub> mitigation"),value=2,id=2,
                                    sidebarLayout(
                                      sidebarPanel(
                                        sliderInput("ip2", HTML("Infectious period, <i>ip</i> (days)"), value = ip_default,
                                                    min = 1, max = ip_max, step = 1),
                                        sliderInput("R0init", HTML("<i>R</i><sub>0</sub>"), 3,
                                                    min = 1.1, max = R0_max, step = R0_step),
                                        sliderInput("R0final", HTML("<i>R</i><sub>0</sub> with mitigation"), 2,
                                                    min = 1, max = R0_max, step = R0_step),
                                        sliderInput("capacity", HTML("Hospital capacity, <i>C</i> %"), 5,
                                                    min = 1, max = 100, step = 1),
                                        checkboxInput("show2", HTML("show intermediate trajectories"), FALSE)
                                      ),
                                      mainPanel(
                                        h3(HTML("Effect of <i>R</i><sub>0</sub> mitigation on infection spread")),
                                        div(htmlOutput("text2"),class="copy"),
                                        div(
                                        div(htmlOutput("title2a"),class="title"),
                                        div(plotOutput("plot2a"),class="plot"),
                                        div(htmlOutput("caption2a"),class="caption"),
                                        class="plotbox"),
                                        div(
                                        div(htmlOutput("title2b"),class="title"),
                                        div(plotOutput("plot2b"),class="plot"),
                                        div(htmlOutput("caption2b"),class="caption"),
                                        class="plotbox"),
                                        div(
                                        div(htmlOutput("title2c"),class="title"),
                                        div(plotOutput("plot2c"),class="plot"),
                                        div(htmlOutput("caption2c"),class="caption"),
                                        class="plotbox"),
                                        div(
                                        div(htmlOutput("title2d"),class="title"),
                                        div(plotOutput("plot2d"),class="plot"),
                                        div(htmlOutput("caption2d"),class="caption"),
                                        class="plotbox"),
                                        div(
                                        div(htmlOutput("title2e"),class="title"),
                                        div(plotOutput("plot2e"),class="plot"),
                                        div(htmlOutput("caption2e"),class="caption"),
                                        class="plotbox"),
                                        div(
                                        div(htmlOutput("title2f"),class="title"),
                                        div(plotOutput("plot2f"),class="plot"),
                                        div(htmlOutput("caption2f"),class="caption"),
                                        class="plotbox")
                                      )
                                    )),
                           
                           tabPanel("Effect of vaccination",value=3,id=3,
                                    sidebarLayout(
                                      sidebarPanel(
                                        sliderInput("ip3",HTML("Infectious period, <i>ip</i> (days)"), value = ip_default,
                                                    min = 1, max = ip_max, step = 1),
                                        sliderInput("R0v", HTML("<i>R</i><sub>0</sub>"), 2,
                                                    min = 1, max = R0_max, step = R0_step)
                                      ),
                                      mainPanel(h3("Effect of vaccination on infection spread"),
                                                div(htmlOutput("text3"),class="copy"),
                                                div(
                                                div(htmlOutput("title3a"),class="title"),
                                                div(plotOutput("plot3a"),class="plot"),
                                                div(htmlOutput("caption3a"),class="caption"),
                                                class="plotbox"),
                                                div(
                                                div(htmlOutput("title3b"),class="title"),
                                                div(plotOutput("plot3b"),class="plot"),
                                                div(htmlOutput("caption3b"),class="caption"),
                                                class="plotbox"),
                                                div(
                                                div(htmlOutput("title3c"),class="title"),
                                                div(plotOutput("plot3c"),class="plot"),
                                                div(htmlOutput("caption3c"),class="caption"),
                                                class="plotbox"),
                                                div(
                                                div(htmlOutput("title3d"),class="title"),
                                                div(plotOutput("plot3d"),class="plot"),
                                                div(htmlOutput("caption3d"),class="caption"),
                                                class="plotbox")
                                      )
                                      
                                    )),
                           tabPanel("Equations",value=4,id=4,
                                    withMathJax(
                                      helpText("Susceptible $$\\frac{dS}{dt} = - \\frac{\\beta I S}{N}$$"),
                                      helpText("Infectious $$\\frac{dI}{dt} = \\frac{\\beta I S}{N} - \\gamma I$$"),
                                      helpText("Recovered $$\\frac{dR}{dt} = \\gamma I$$"),
                                      helpText("Recovery rate $$\\gamma = \\frac{1}{\\text{infectious period}}$$"),
                                      helpText("Basic reproduction number $$R_0 =  \\frac{\\beta}{\\gamma}$$")             
                                    ),
                                    p("Infection trajectories show a numerical solution to the SIR equations with 5,000 time steps and initial parameters"),
                                    withMathJax(
                                      helpText("$$S(0) = 0.999 - p$$"),
                                      helpText("$$I(0) = 0.001$$"),
                                      helpText("$$R(0) = p$$"),
                                      helpText("$$S + I + R = N = 1$$")
                                    ),
                                    p("where",tags$i("p"),"is the vaccination fraction.")),
                           tabPanel("Credits",value=5,id="5",
                                    mainPanel(
                                      h3("Points of Significance: Modelling Infectious Epidemics"),
                                      p(HTML("Ottar Bjornstad<sup>1,2</sup>, Katriona Shea<sup>1</sup>, Martin Krzywinski<sup>3*</sup>, Naomi Altman<sup>4</sup>")),
                                      div(
                                      p("1. Department of Biology, The Pennsylvania State University, State College, PA, USA."),
                                      p("2. Department of Entomology, The Pennsylvania State University, State College, PA, USA."),
                                      p("3. Canada’s Michael Smith Genome Sciences Center, Vancouver, British Columbia, Canada."),
                                      p("4. Department of Statistics, The Pennsylvania State University, State College, PA, USA."),
                                        class="affiliations"),
                                      p("*",tags$a(href="mailto:martink@bcgsc.ca",tags$i("martink@bcgsc.ca"))),
                                      p(HTML("<i>Nature Methods</i> (2020) <b>17</b>:xxx&ndash;xxx.")),
                                      
                                      hr(),
                                      h4("Download code"),
                                      p(tags$a(href="https://github.com/martinkrz/posepi1","https://github.com/martinkrz/posepi1")),
                                      hr(),
                                  
                                      h4("Version history"),
                                      
                                      h5("7 April 2020"),
                                      p("Initial public release."),
                                      width=16
                                      
                                    ))
                           
                ))