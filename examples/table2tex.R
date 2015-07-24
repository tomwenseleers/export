# export of aov Anova output
fit=aov(yield ~ block + N * P + K, npk)
x=summary(fit)
table2tex(x=x,file="table_aov1.tex") # export to Latex in standAlone format
table2tex2(x=x,file="table_aov2.tex") # export to Latex to paste in document
table2doc(x=x,file="table_aov.doc") # export to Word
table2html(x=x,file="table_aov.html") # export to HTML

\dontrun{
# export output of last evaluation
summary(fit)
table2doc(file="table_aov.doc") # export to Word
}
