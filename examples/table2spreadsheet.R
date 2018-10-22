# Create a file name
filen <- tempfile(pattern = "table_aov") # or 
# filen <- paste("YOUR_DIR/table_aov")

# Generate ANOVA output
fit=aov(yield ~ block + N * P + K, data = npk) # 'npk' dataset from base 'datasets'
x=summary(fit)

# Save ANOVA table as a CSV
### Option 1: pass output as object
table2csv(x=x,file=filen, digits = 1, digitspvals = 3)
### Option 2: get output from console 
summary(fit)
table2csv(file=filen, digits = 2, digitspvals = 4)

# Save ANOVA table as an Excel
# Without formatting of the worksheet
x
table2excel(file=filen, sheetName="aov_noformatting", 
            digits = 1, digitspvals = 3) 
# With formatting of the worksheet
table2excel(x=x,file=filen, sheetName="aov_formated", 
            append = TRUE, add.rownames=TRUE, fontName="Arial", 
            fontSize = 14, fontColour = rgb(0.15,0.3,0.75), 
            border=c("top", "bottom"), fgFill = rgb(0.9,0.9,0.9), 
            halign = "center", valign = "center", textDecoration="italic") 


