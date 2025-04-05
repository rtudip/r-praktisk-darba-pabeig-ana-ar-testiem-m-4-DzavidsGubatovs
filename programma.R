# 1. Ielasa datni un attīra atstarpes, uzstāda pirmo kolonnu kā rindu nosaukumus
kordat <- read.table("variants3.txt", header = TRUE, sep = "\t", dec = ",", strip.white = TRUE)
rownames(kordat) <- kordat[[1]]
kordat <- kordat[ , -1]

# 2. Kolonnas no 9. un uz augšu par faktoriem
kordat[ , 9:ncol(kordat)] <- lapply(kordat[ , 9:ncol(kordat)], as.factor)

# Saglabā faktoru kolonnas kā atsevišķas mainīgās
f_column <- kordat[["f"]]

# 3. Iestata teksta failu rezultātu uzkrāšanai un pārliecinās, ka fails tiks izveidots, ja neeksistē
results_path <- "results.txt"
if (!file.exists(results_path)) file.create(results_path)

# 4. Kopsavilkums par katru faktoru līmeni
sink(results_path)
cat("Faktoru līmeņu biežuma kopsavilkums:\n")
for (col in names(kordat)[9:ncol(kordat)]) {
  cat("\nKolonna '", col, "':\n", sep = "")
  print(summary(kordat[[col]]))
}

# 5. Slope sadalīts pēc b faktora
sl.by.b <- split(kordat$Slope, kordat$b)
cat("\n\n'Slope' sagrupēts pēc b faktora (sl.by.b):\n")
print(sl.by.b)

# 6. Izveido "Average" kolonnu ar apply
kordat$Average <- apply(kordat[ , c("Slope", "Intercept", "adj.r.squared")], 1, mean)

# 7. Standartnovirze pa f faktora līmeņiem ar tapply+sapply
cat("\n\nStandartnovirzes pa f faktora līmeņiem:\n")
std_by_f <- sapply(kordat[ , sapply(kordat, is.numeric)], function(col) tapply(col, f_column, sd))
print(std_by_f)

# 8. Filtrē prockordat pēc adj.r.squared
if (any(kordat$adj.r.squared > 0)) {
  prockordat <- subset(kordat, adj.r.squared > 0.7)
} else {
  prockordat <- subset(kordat, adj.r.squared > -0.3)
}

# 9. Pārrēķina Slope pēc funkcijas
f1 <- function(k) { 1 - 1 / k }
prockordat$Slope <- sapply(prockordat$Slope, f1)

# 10. Izdrukā prockordat
cat("\n\nDatu satvars 'prockordat' (filtrēts un apstrādāts):\n")
print(prockordat)
sink()  # Beidz rakstīt uz failu

# 11. Izkliedes grafiks
svg("scatter.svg")
plot(kordat$MAD, kordat$Average, xlab = "MAD", ylab = "Average", main = "Izkliedes grafiks: MAD vs Average")
dev.off()

# 13. Kastīšu grafiks ar split
svg("boxplot.svg")
boxplot(split(kordat$Intercept, f_column), main = "Kastīšu grafiks: Intercept pēc f", xlab = "f faktors", ylab = "Intercept")
dev.off()

# Papilduzdevums: biežāk sastopamais līmenis no rindu nosaukumiem
library(stringr)
lvli <- unlist(str_extract_all(rownames(kordat), "[a-z]+\\d+"))
most_common <- names(sort(table(lvli), decreasing = TRUE))[1]

matching_rows <- prockordat[grepl(most_common, rownames(prockordat)), ]
print(matching_rows)
