\dontrun{
# export of aov Anova output
fit=aov(yield ~ block + N * P + K, npk)
summary(fit)
table2ppt(file="table_aov.pptx")  
summary(fit)
table2ppt(file="table_aov.pptx", width=9, append=TRUE) # append table to previous slide
# pass object
s=summary(fit)
table2ppt(obj=s,file="table_aov.docx", font="Times New Roman", pointsize=14, 
          digits=4, digitspvals=1, append=TRUE)
}
