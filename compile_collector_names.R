library(hdimDB)

coll <- readGoogle('https://docs.google.com/spreadsheets/d/1_KGLPEcOneLvcRR8--CjEVeKTPDXJkI7YqwSTM2BVJc/pub?output=csv')

cor <- unlist(strsplit(coll$correctCollector, ', '))

verb <- lapply(strsplit(coll$verbatimCollector, ','), function(x) {
    if(length(x) != 1) {
        return(x)
    } else {
        test <- try(grepl('\\.', x))
        if(class(test) == 'try-error') browser()
        if(length(test) ==  0) browser()
        if(test) {
            return(x)
        } else {
            strsplit(x, ' ')
        }
    }
})

syn <- data.frame(verb = unlist(verb), cor = cor)
unique(syn)
