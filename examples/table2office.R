# Create a file name
filen <- tempfile(pattern = "table_aov") # or 
# filen <- paste("YOUR_DIR/table_aov")

# Generate ANOVA output
fit=aov(yield ~ block + N * P + K, data = npk) # 'npk' dataset from base 'datasets'

# Save ANOVA table as a PPT
### Option 1: pass output as object
x=summary(fit)
if (interactive()) 
    table2ppt(x=x,file=filen, digits = 1, digitspvals = 3)
### Option 2: get output from console 
summary(fit)
if (interactive())
    table2ppt(x=x,file=filen, width=5, font="Times New Roman", pointsize=14, 
          digits=4, digitspvals=1, append=TRUE) # append table to previous slide

# Save ANOVA table as a DOC file
if (interactive())
    table2doc(x=x,file=filen, digits = 1, digitspvals = 3)
summary(fit)
if (interactive())
    table2doc(file=filen, width=3.5, font="Times New Roman", pointsize=14, 
          digits=4, digitspvals=1, append=TRUE) # append table at end of document
