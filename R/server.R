server = function(input, output, session) {
  
  source("R/models.R",local=T)
  source("R/reactives.R",local=T)
  source("R/titles.R",local=T)
  
  source("R/1.plot.R",local=T)
  source("R/1.text.R",local=T)
  source("R/2.plot.R",local=T)
  source("R/2.text.R",local=T)
  source("R/3.plot.R",local=T)
  source("R/3.text.R",local=T)
  source("R/masthead.captions.R",local=T)
  
  # Make sure that the mitigation R0final max is smaller than R0init
  observe(updateSliderInput(session, "R0final", max = input$R0init-R0_step))

  toggleText = function(value) {
    updateCheckboxInput(session,"text1",value=value) 
    updateCheckboxInput(session,"text2",value=value) 
    updateCheckboxInput(session,"text3",value=value) 
    updateCheckboxInput(session,"text4",value=value) 
    toggle(anim = TRUE, animType = "slide", time = 0.5, selector =".copy1", condition = value)
    toggle(anim = TRUE, animType = "slide", time = 0.5, selector =".copy2", condition = value)
    toggle(anim = TRUE, animType = "slide", time = 0.5, selector =".copy3", condition = value)
    toggle(anim = TRUE, animType = "slide", time = 0.5, selector =".copy4", condition = value)
  }
  toggleCaptions = function(value) {
    updateCheckboxInput(session,"captions1",value=value) 
    updateCheckboxInput(session,"captions2",value=value) 
    updateCheckboxInput(session,"captions3",value=value) 
    updateCheckboxInput(session,"captions4",value=value) 
    toggle(anim = TRUE, animType = "slide", time = 0.5, selector =".caption1", condition = value)
    toggle(anim = TRUE, animType = "slide", time = 0.5, selector =".caption2", condition = value)
    toggle(anim = TRUE, animType = "slide", time = 0.5, selector =".caption3", condition = value)
    toggle(anim = TRUE, animType = "slide", time = 0.5, selector =".caption4", condition = value)
  }  

  observeEvent(input$text1,     { toggleText(input$text1) })
  observeEvent(input$text2,     { toggleText(input$text2) })
  observeEvent(input$text3,     { toggleText(input$text3) })  
  observeEvent(input$captions1, { toggleCaptions(input$captions1) })
  observeEvent(input$captions2, { toggleCaptions(input$captions2) })
  observeEvent(input$captions3, { toggleCaptions(input$captions3) })  

  observeEvent(input$refresh1,  { shinyjs::reset("form1") })
  #observeEvent(input$text1,     { toggle(anim = TRUE, animType = "slide", time = 0.5, selector =".copy1", condition = input$text1) })
  #observeEvent(input$captions1, { toggle(anim = TRUE, animType = "slide", time = 0.5, selector =".caption1", condition = input$captions1) })

  observeEvent(input$refresh2,  { shinyjs::reset("form2") })
  #observeEvent(input$text2,     { toggle(anim = TRUE, animType = "slide", time = 0.5, selector =".copy2", condition = input$text2) })
  #observeEvent(input$captions2, { toggle(anim = TRUE, animType = "slide", time = 0.5, selector =".caption2", condition = input$captions2) })

  observeEvent(input$refresh3,  { shinyjs::reset("form3") })
  #observeEvent(input$text3,     { toggle(anim = TRUE, animType = "slide", time = 0.5, selector =".copy3", condition = input$text3) })
  #observeEvent(input$captions3, { toggle(anim = TRUE, animType = "slide", time = 0.5, selector =".caption3", condition = input$captions3) })
  
  # generate the SIR trajectories and summaries for each panel, as needed
  df1 = reactive(calculate1(input$R01,input$ip1,input$vac/100)) %>% throttle(1000)
  df2 = reactive(calculate2(input$R0init,input$R0final,input$ip2)) %>% throttle(1000)
  df3 = reactive(calculate3(input$R0v,input$ip3)) %>% throttle(1000)
  
  # precompute all plots, indexes and titles for a panel, as needed
  p1 = reactive(plots1(input$R01,input$ip1,input$vac/100)) %>% throttle(1000)
  p2 = reactive(plots2(input$R0init,input$R0final,input$ip2)) %>% throttle(1000)
  p3 = reactive(plots3(input$R0v,input$ip3)) %>% throttle(1000)
 
}
