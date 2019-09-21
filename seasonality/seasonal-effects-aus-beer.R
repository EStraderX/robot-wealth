library('fpp')

data(ausbeer)

beer_raw <- ts(ausbeer, frequency = 4, start = 1956)
plot(beer_raw, ylab = 'ML/Quarter', main = 'Australian Beer Production')

beer_decomposed = decompose(beer_raw, 'additive')
beer_adjusted = beer_raw - beer_decomposed$seasonal
plot(beer_decomposed)
plot(beer_adjusted)