export
======
export is an R package to easily export R graphs and statistical output to Microsoft Office, HTML and Latex.

Usefull links: 

* Report a bug: 
[**Bug report**]
(http://github.com/tomwenseleers/export/issues "please provide a reproducible example"). 
If you report a bug, try to send a reproducible example and don't forget to send the result of 
    
        sessionInfo()
        
Features
--------
* Save active R graphs or ggplot2, lattice or base R plots in Microsoft Word, Powerpoint, 
  HTML, Latex or various other bitmap or vector formats using a single command.
* Fully editable Powerpoint vector format output, enabling manual tidy-up of plot layout.
* Save the output of statistical analysis in R as tables in Word, PowerPoint or HTML documents.
* Customize formatting of R outputs.

Installation
------------

### Dependencies

Java (it has been tested with java version >= 1.6).

export needs some R packages ; run the following script to install them if needed.

    install.packages("rJava")
    install.packages("ReporteRs")
    install.packages("ReporteRsjars")
    install.packages("ggplot2")
    install.packages("rtable")
    install.packages("xtable")
    install.packages("taRifx")
    install.packages("stargazer")
    install.packages("tikzDevice")


### Github

**Get the latest release:**  

    install.packages("devtools")
    library(devtools)
    devtools::install_github('tomwenseleers/export',local=F)

  
Getting Started
---------------

    library(export)
       
    ?graph2ppt
    ?table2doc
    ?graph2tex
    ?table2tex
    ?graph2svg
    ?graph2png

    ## export of ggplot2 plot
    library(ggplot2)
    qplot(Sepal.Length, Petal.Length, data = iris, color = Species, 
          size = Petal.Width, alpha = I(0.7))
    graph2ppt(file="ggplot2 plot.pptx", aspectr=1.7)

    # add 2nd slide with same graph in different aspect ratio
    graph2ppt(file="ggplot2 plot.pptx", aspectr=1.3, append=T) 
    # add 3d slide with same graph with fixed width & height
    graph2ppt(file="ggplot2 plot.pptx", width=6, height=5, append=T) 

    # export of aov Anova output
    fit=aov(yield ~ block + N * P + K, npk)
    summary(fit)
    table2doc(file="table_aov.docx")
    summary(fit)
    table2doc(file="table_aov.docx",append=T,digits=4)
    summary(fit)
    table2doc(file="table_aov.docx",append=T,digits=4,digitspvals=1)
    summary(fit)
    table2html(file="table_aov.html")
  
License
-------
The export package is licensed under the GPLv2.
