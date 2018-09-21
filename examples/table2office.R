# export of aov Anova output
fit=aov(yield ~ block + N * P + K, npk)
x=summary(fit)
table2ppt(x=x,file="table_aov.pptx", digits = 1, digitspvals = 3)  
table2ppt(x=x,file="table_aov.pptx", width=9, append=TRUE) # append table to previous slide

table2doc(x=x,file="table_aov.docx")  
table2doc(x=x,file="table_aov.docx", width=9, append=TRUE) # append table to previous slide


\dontrun{
# export output of last evaluation
summary(fit)
table2ppt(file="table_aov.docx", font="Times New Roman", pointsize=14, 
          digits=4, digitspvals=1, append=TRUE)
}
