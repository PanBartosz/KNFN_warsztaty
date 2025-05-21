# Przetestowaliśmy Jamesa Bonda na to, czy potrafi odróżnić Martini zmieszane od wstrząśniętego. 
# Zgadł w 12 przypadkach na 16.
# Czy możemy stwierdzić, że jest lepszy, niż gdyby zgadywał na chybił-trafił?

# Liczba sukcesów: 12 
# Liczba prób: 16
# Hipoteza zerowa: Bond zgaduje na chybił-trafił (50-50)
# Prawdopodobieństwo sukcesu, jeśli zgaduje na chybił-trafił: 0.5
# Rozkład prawdopodobieństwa dla liczby sukcesów, jeśli "strzela":

barplot(dbinom(0:16, 16, 0.5), names.arg = 0:16)

# Prawdopodobieństwo w 12, 13, 14, 15 i 16 przypadkach:
dbinom(12:16, 16, 0.5)

# Prawdopodobieństwo wyniku takiego, jak uzyskał, lub bardziej skrajnego (= lepszego)

sum(dbinom(12:16, 16, 0.5))

# Ta wartość nazywana jest w statystyce p-value.
# Jest podstawą do decyzji o tym, czy odrzucamy hipotezę zerową.