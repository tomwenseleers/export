# export of aov Anova output
fit=aov(yield ~ block + N * P + K, npk)
x=summary(fit)
# CSV
tab <- table2csv(x=x,file="table_aov.csv", digits = 1, digitspvals = 3)
tab 
# Excel
# Without formatting of the worksheet
tab <- table2excel(x=x,file="table_aov.xlsx", sheetName="aov_noformatting", 
                   digits = 1, digitspvals = 3) 
# With formatting of the worksheet
tab <- table2excel(x=x,file="table_aov.xlsx", sheetName="aov_formated", 
                   append = TRUE, add.rownames=TRUE, fontName="Arial", 
                   fontSize = 14, fontColour = rgb(0.15,0.3,0.75), 
                   border=c("top", "bottom"), fgFill = rgb(0.9,0.9,0.9), 
                   halign = "center", valign = "center", textDecoration="italic") 
tab


