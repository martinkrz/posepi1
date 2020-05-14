output$masthead = renderPrint({ 
  cat(paste("<div id=natmeth><img src='img/nature.methods.png'/></div>",sep=""))
  cat(paste("<div id=mast>Bjørnstad, O., Shea, K., Krzywinski, M. & Altman, N. <a href='https://www.nature.com/articles/s41592-020-0822-z'>Points of Significance: Modelling infectious epidemics</a>. (2020) <i>Nature Methods</i> <b>17</b>:455&ndash;456.</div>",sep=""))
  
})

sir_caption = function(tmax,vac) {
  paste("Plots were computed numerically using the SIR model (see Equation tab) from",varfmt("t",0),"to",varfmt("t",tmax),"in",formatC(sir_system_steps,format="f",big.mark=",",digits=0),"time steps initialized at <i>t</i> = 0 with",varfmt("S(0),",1-sir_init_i-vac,prec=3)," ",varfmt("I(0),",sir_init_i,prec=3)," and",varfmt("R(0).",vac,prec=3),sep=" ")
}
sir_caption_p = function(tmax) {
  paste("Plots were computed numerically using the SIR model (see Equation tab) from",varfmt("t",0),"to",varfmt("t",tmax),"in",formatC(sir_system_steps,format="f",big.mark=",",digits=0),"time steps initialized at <i>t</i> = 0 to",varfmt("S(0)",1-sir_init_i,prec=3)," &ndash; <i>p</i>,",varfmt("I(0),",sir_init_i,prec=3)," and",varfmt("R(0)")," = <i>p</i> for vaccination fraction",varfmt("p."),sep=" ")
}



