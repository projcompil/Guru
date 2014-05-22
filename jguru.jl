nw = 3000
np = 1000

function estime_moyenne(calcul, m, p, l)
	(@parallel (+) for i=1:m
		calcul(p,l)
	end)/m
end

function generep(n, k)
	if k == 0
		{zeros(Bool, n)}
	elseif n == 0
		{[]}
	elseif n == k
		{ones(Bool, k)}
	else
		cat(1, map(a -> cat(1, false, a), generep(n-1,k)), map(a -> cat(1, true, a), generep(n-1,k-1)))
	end
end


function calct(pi, w, p, eps)
	const n = length(w)
	function aux(A, i)
		if i > n
			0.0
		else
			w[i]/A + eps[i] * aux(A+p[i], i+1) + (1 - eps[i]) * aux(A, i+1)
		end
	end
	aux(pi, 1)
end

function genere(n)
	rand(1:nw, n), rand(1:np, n), rand(n)
end

function naif(pi, w, p, eps)
	const n = length(w)
	const perms = permutations(1:n)
	mini = Inf
	pmini = zeros(Int64, n)
	for sigma in perms
		let c = calct(pi, w[sigma], p[sigma], eps[sigma])
			if c < mini
				mini = c
				pmini = sigma
			end
		end
	end
	mini, pmini
end

function calcfinal(pi, w, p, eps)
	const n = length(w)
	function aux(A, i)
		if i == n
			w[i] / A
		else
			eps[i] * aux(A+p[i], i+1) + (1 - eps[i]) * aux(A, i+1)
		end
	end
	aux(pi, 1)
end


function resout(pi, wd, pd, epsd, w, p, eps)
	const n = length(w)
	n == 0 && return 0., []
	n == 1 && return calcfinal(pi, cat(1, wd, w), cat(1, pd, p), cat(1, epsd, eps)), (w, p, eps)
	const r = div(n,2)
	const oppose = trues(n)
	const combs = combinations(1:n, r)
	mini = (Inf, [])
	for c in combs
		fill!(t, true)
		oppose[c] = false
		wl, pl, epsl = w[c], p[c], eps[c]
		wr, pr, epsr = w[oppose], p[oppose], eps[oppose]
		rd, rld = resout(pi, wd, pd, epsd, wl, pl, epsl)
		rf, rlf = resout(pi, cat(1, wd, wl), cat(1, pd, pl), cat(1, epsd, epsl), wr, pr, epsr)
		mini = min((rd + rf, cat(1, rld, rlf)), mini)
	end
	mini
end

function heuris(pi, ordi)
	ordi[3] * ordi[2] / (ordi[1] * (pi + ordi[2]))
end


function appheur(pi, w, p, eps)
	t = collect(zip(w, p, eps))
	sort!(t, rev = true, by = (ordi-> (heuris(pi, ordi))))
	t
end

function teste(m, n, approx, resol = naif, calcul = calct, ferreur = identity)
	1/n * @parallel (+) for i=1:n
		pi = rand(1:np)
		w, p, eps = genere(m)
		optimal, lr = resol(pi, w, p, eps)
		lt = approx(pi, w, p, eps)
		for i=1:m
			w[i], p[i], eps[i] = lt[i]
		end
		ct = calcul(pi, w, p, eps)
		ferreur(ct/optimal -1)
	end
end

