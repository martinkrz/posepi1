ui = fluidPage( theme=("css/style.css"),
                shinyjs::useShinyjs(),
                htmlOutput("masthead"),
                navbarPage("Modelling infectious epidemics",id="tabs",
                           tabPanel("The SIR model",value=1,id=1,
                                    sidebarLayout(
                                      sidebarPanel(
                                          div(id="form1",
                                          sliderInput("ip1", HTML("Infectious period, 1/<i>&gamma;</i> (days)"), 
                                                      value = ip_default,
                                                    min = 1, max = ip_max, step = 1),
                                          sliderInput("R01", HTML("<i>R</i><sub>0</sub>"), value = 3,
                                                    min = 1, max = R0_max, step = R0_step),
                                          sliderInput("vac", HTML("vaccination level, <i>p</i> (%)"), 0,
                                                    min = 0, max = 99, step = 1),
                                          checkboxInput("log1", HTML("log<sub>10</sub> y-axis scale"), FALSE),
                                          checkboxInput("text1", HTML("interpretive text"), interpretive_default),
                                          checkboxInput("captions1", HTML("figure captions"), captions_default),
                                          actionButton("refresh1","Reset")
                                              )
                                      ),
                                      mainPanel(h3("The SIR model of infection spread"),
                                                div(htmlOutput("text1"),class="copy copy1"),
                                                div(
                                                div(htmlOutput("title1a"),class="title"),
                                                div(plotOutput("plot1"),class="plot"),
                                                div(htmlOutput("caption1"),class="caption caption1"),
                                                class="plotbox")
                                      )
                                    )),
                           
                           tabPanel(HTML("Effect of <i>R</i><sub>0</sub> mitigation"),value=2,id=2,
                                    sidebarLayout(
                                      sidebarPanel(
                                          div(id="form2",
                                        sliderInput("ip2", HTML("Infectious period, <i>ip</i> (days)"), value = ip_default,
                                                    min = 1, max = ip_max, step = 1),
                                        sliderInput("R0init", HTML("<i>R</i><sub>0</sub>"), 3,
                                                    min = 1.1, max = R0_max, step = R0_step),
                                        sliderInput("R0final", HTML("<i>R</i><sub>0</sub> with mitigation"), 2,
                                                    min = 1, max = R0_max, step = R0_step),
                                          sliderInput("capacity", HTML("Hospital capacity, <i>C</i> %"), 5,
                                                      min = 1, max = 100, step = 1),
                                          checkboxInput("show2", HTML("show intermediate trajectories"), FALSE),
                                          checkboxInput("text2", HTML("interpretive text"), interpretive_default),
                                          checkboxInput("captions2", HTML("figure captions"), captions_default),
                                          actionButton("refresh2","Reset")
                                              )
                                          ),
                                      mainPanel(
                                        h3(HTML("Effect of <i>R</i><sub>0</sub> mitigation on infection spread")),
                                        div(htmlOutput("text2"),class="copy copy2"),
                                        div(
                                        div(htmlOutput("title2a"),class="title"),
                                        div(plotOutput("plot2a"),class="plot"),
                                        div(htmlOutput("caption2a"),class="caption caption2"),
                                        class="plotbox"),
                                        div(
                                        div(htmlOutput("title2b"),class="title"),
                                        div(plotOutput("plot2b"),class="plot"),
                                        div(htmlOutput("caption2b"),class="caption caption2"),
                                        class="plotbox"),
                                        div(
                                        div(htmlOutput("title2c"),class="title"),
                                        div(plotOutput("plot2c"),class="plot"),
                                        div(htmlOutput("caption2c"),class="caption caption2"),
                                        class="plotbox"),
                                        div(
                                        div(htmlOutput("title2d"),class="title"),
                                        div(plotOutput("plot2d"),class="plot"),
                                        div(htmlOutput("caption2d"),class="caption caption2"),
                                        class="plotbox"),
                                        div(
                                        div(htmlOutput("title2e"),class="title"),
                                        div(plotOutput("plot2e"),class="plot"),
                                        div(htmlOutput("caption2e"),class="caption caption2"),
                                        class="plotbox"),
                                        div(
                                        div(htmlOutput("title2f"),class="title"),
                                        div(plotOutput("plot2f"),class="plot"),
                                        div(htmlOutput("caption2f"),class="caption caption2"),
                                        class="plotbox")
                                      )
                                    )),
                           
                           tabPanel("Effect of vaccination",value=3,id=3,
                                    sidebarLayout(
                                      sidebarPanel(
                                          div(id="form3",
                                        sliderInput("ip3",HTML("Infectious period, <i>ip</i> (days)"), value = ip_default,
                                                    min = 1, max = ip_max, step = 1),
                                        sliderInput("R0v", HTML("<i>R</i><sub>0</sub>"), 2,
                                                    min = 1, max = R0_max, step = R0_step),
                                          checkboxInput("text3", HTML("interpretive text"), interpretive_default),
                                          checkboxInput("captions3", HTML("figure captions"), captions_default),
                                          actionButton("refresh3","Reset")
                                              )
                                      ),
                                      mainPanel(h3("Effect of vaccination on infection spread"),
                                                div(htmlOutput("text3"),class="copy copy3"),
                                                div(
                                                div(htmlOutput("title3a"),class="title"),
                                                div(plotOutput("plot3a"),class="plot"),
                                                div(htmlOutput("caption3a"),class="caption caption3"),
                                                class="plotbox"),
                                                div(
                                                div(htmlOutput("title3b"),class="title"),
                                                div(plotOutput("plot3b"),class="plot"),
                                                div(htmlOutput("caption3b"),class="caption caption3"),
                                                class="plotbox"),
                                                div(
                                                div(htmlOutput("title3c"),class="title"),
                                                div(plotOutput("plot3c"),class="plot"),
                                                div(htmlOutput("caption3c"),class="caption caption3"),
                                                class="plotbox"),
                                                div(
                                                div(htmlOutput("title3d"),class="title"),
                                                div(plotOutput("plot3d"),class="plot"),
                                                div(htmlOutput("caption3d"),class="caption caption3"),
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
                                    p("Infection trajectories show a numerical solution to the SIR equations with 1,000 time steps and initial parameters"),
                                    withMathJax(
                                      helpText("$$S(0) = 0.999 - p$$"),
                                      helpText("$$I(0) = 0.001$$"),
                                      helpText("$$R(0) = p$$"),
                                      helpText("$$S + I + R = N = 1$$")
                                    ),
                                    p("where",tags$i("p"),"is the vaccination fraction.")),
                           tabPanel("Download & Credits",value=5,id="5",
                                    mainPanel(
                                      h3("Points of Significance: Modelling infectious epidemics"),
                                      p(HTML("Ottar Bjørnstad<sup>1,2</sup>, Katriona Shea<sup>1</sup>, <a href='mailto:martink@bcgsc.ca'>Martin Krzywinski</a><sup>3</sup>, Naomi Altman<sup>4</sup>")),
                                      div(
                                      p("1. Department of Biology, The Pennsylvania State University, State College, PA, USA."),
                                      p("2. Department of Entomology, The Pennsylvania State University, State College, PA, USA."),
                                      p("3. Canada’s Michael Smith Genome Sciences Center, Vancouver, British Columbia, Canada."),
                                      p("4. Department of Statistics, The Pennsylvania State University, State College, PA, USA."),
                                        class="affiliations"),

                                      hr(),
                                      h4("Download code"),
                                      p(tags$a(href="https://martinkrz.github.io/posepi1/","https://martinkrz.github.io/posepi1/")),
                                      
                                      hr(),
                                      h4("Citation"),
                                      p(HTML("Bjørnstad, O., Shea, K., Krzywinski, M. & Altman, N. <a href='https://www.nature.com/articles/s41592-020-0822-z'>Points of Significance: Modelling infectious epidemics</a>. (2020) <i>Nature Methods</i> <b>17</b>:455&ndash;456.")),
                                      
                                      hr(),
                                      h4("Version history"),
                                      h5("9 April 2020 v1.0.0"),
                                      p("Initial public release."),
                                      h5("17 April 2020 v1.0.1"),
                                      p("Minor text changes. Fixed typos."),
                                      h5("13 May 2020 v1.0.2"),
                                      p("UI tweaks."),
                                      h5("18 June 2020 v1.0.3"),
                                      p("Added link to SEIRS column."),
                                      h5("17 August 2020 v1.0.4"),
                                      p("Added link to uncertainty and management column."),
                                      
                                      hr(),
                                      h4("RELATED COLUMNS"),
                                      p(HTML("Bjørnstad, O., Shea, K., Krzywinski, M. & Altman, N. <a href='https://www.nature.com/articles/s41592-020-0856-2'>Points of Significance: The SEIRS model for infectious disease dynamics</a>. (2020) <i>Nature Methods</i> <b>17</b>:557&ndash;558. (<a href='http://shiny.bcgsc.ca/posepi2'>interactive figures</a>, <a href='https://martinkrz.github.io/posepi2/'>download code</a>).")),
                                      p(HTML("Shea, K., Bjørnstad, O., Krzywinski, M. & Altman, N. Points of Significance: Uncertainty and the management of epidemics. (2020) <i>Nature Methods</i> <b>17</b> (in press). (<a href='http://shiny.bcgsc.ca/posepi3'>interactive figures</a>, <a href='https://martinkrz.github.io/posepi3/'>download code</a>)")),
                                      width=16
                                      
                                    ))
                           
                ))
