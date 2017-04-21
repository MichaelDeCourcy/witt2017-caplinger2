library(data.table)

joe.data <- read.csv('C:\\Users\\Jason\\Desktop\\Joe Sub Sample.csv')

j.data <- as.data.table(joe.data)


in.metro <- j.data[METRO > 1]

fips <- in.metro$STATEFIP

result <- lapply(X = unique(fips)[1:5],
                 FUN = function(x) {
         
         stuff = in.metro[STATEFIP == x & POVERTY > 100]
         model = lm(POVERTY~AGE+SEX, data = stuff)
         file <- paste('model',x,'.R', sep = '')
         dump('model', file)
         
       })

na.metro <- j.data[METRO == 0]

no.metro <- j.data[METRO == 1]
