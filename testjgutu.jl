require("jguru.jl")

@time println("Résultat : $(Guru.teste(int(ARGS[1]), int(ARGS[2]), Guru.appheur))")
