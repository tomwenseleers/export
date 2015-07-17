\dontrun{
# export of aov Anova output
fit=aov(yield ~ block + N * P + K, npk)
summary(fit)
table2tex(file="table_aov.tex") # export to Latex in standAlone format
summary(fit)
table2tex2(file="table_aov.tex") # export to Latex to paste in document
summary(fit)
table2doc(file="table_aov.doc") # export to Word
table2doc(file="table_aov.doc",obj=fit) # export to Word, but pass as object
# pass as object
s=summary(fit)
table2html(obj=s,file="table_aov.html") # export to HTML
}
