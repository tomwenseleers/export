# export of aov Anova output
fit=aov(yield ~ block + N * P + K, npk)
x=summary(fit)
# export to Latex in standAlone format
table2tex(x=x,file="table_aov1.tex") 
# export to Latex to paste in tex document
table2tex(x=x,file="table_aov2.tex", standAlone = FALSE) 
# export to HTML
table2html(x=x,file="table_aov.html") 

\dontrun{
# export output of last evaluation
summary(fit)
table2html(file="table_aov.doc") # export to Word
}
