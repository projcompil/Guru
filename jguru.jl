nw = 3000
np = 1000

function estime_moyenne(calcul, m, p, l)
	(@parallel for i=1:m
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
	for sigma in perms
		mini = min(calct(pi, w[sigma], p[sigma], eps[sigma]))
	end
	mini
end



function heuris(pi, (w, p, eps))
	eps * p / (w * (pi + p))
end



