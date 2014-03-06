Random.self_init()
let nw = 30
let np = 10

let generec () = (float_of_int (2+ (Random.int nw)) , float_of_int (2 + (Random.int np)), Random.float 1.)

let rec genere = function
	| 0 -> []
	| n -> generec()::(genere (n-1))


let rec repete x = function
	| 0 -> []
	| n -> x::(repete x (n-1))

let aj a l = a::l

let rec generep n k = match (n,k) with
	| _, 0  -> [(repete 0 n)]
	| 0, _ -> [[]]
	| n, k when n = k -> [(repete 1 k)]
	| n, k -> (List.map (aj 0) (generep (n-1) k)) @ (List.map (aj 1) (generep (n-1) (k-1)))

let rec calct pi = function
	| [] -> 0.
	| (w, p, eps)::l -> w /. pi +. (eps *. (calct (pi +. p) l) +. (1. -. eps) *. (calct pi l))

let rec associe l ens = match (l, ens) with
	| [], _ | _, [] -> []
	| a::l, 0::ens -> (associe l ens)
	| a::l, _::ens -> a::(associe l ens)

let lg x = log(x) /. log(2.)

let heuris1 a b c (w,p, eps) =
	-. a *. (lg (w)) +.  b *. (lg(p)) +. c *. (lg (eps))

(*let heurisg1 t a b c  =
	a(t) *.*)
	
let cmp f a b =
	if f a > f b then -1
	else if f a < f b then 1 
	else 0

let appheur l f =
	List.sort (cmp f) l




let distribute c l =
	let rec insert acc1 acc2 = function
    	| [] -> acc2
		| hd::tl -> insert (hd::acc1) ((List.rev_append acc1 (hd::c::tl)) :: acc2) tl
	in insert [] [c::l] l

let rec permutation = function
	| [] -> [[]]
	| hd::tl -> List.fold_left (fun acc x -> List.rev_append (distribute hd x) acc) [] (permutation tl)

let minimum l p =
	let rec aux (acc,m) = function
		| [] -> (acc,m)
		| x::l -> let y = calct p x in
					if y < acc || acc < 0. then
						aux (y,x) l
					else aux (acc, m) l
	in aux (-1.,[]) l


let naif l p = 
	let ll = permutation l in
		minimum ll p

let creet n =
	let t = Array.create n [|[||]|] in
		for i = 0 to n-1 do
			t.(i) <- Array.create n [||] ;
		done;
		for i = 0 to n-1 do
			for j=0 to n-1 do
				t.(i).(j) <- Array.create n (0., 0., 0.) ;
			done
		done;
		let r = 1. /. float_of_int(n) in
			for i=0 to n-1 do
				for j=0 to n-1 do
					for k=0 to n-1 do
						t.(i).(j).(k) <- (float_of_int(i+1) *. r, float_of_int(j+1) *.r, float_of_int(k+1) *. r)
					done
				done
			done;
			t

let ecart l p a b c opti =
	let e = calct p (appheur l (heuris1 a b c)) in
		(e /. opti ) -. 1.

let carre x = x *. x

let rec app_sample s p a b c optimal = match s, optimal with
	| [], _ | _, [] -> 0.
	| l::s, o::optimal -> carre(ecart l p a b c o) +. (app_sample s p a b c optimal)
(*	List.fold_left (fun x l -> x +. (carre (ecart l p a b c))) 0. s*)
	
	

let rec genere_sample m = function
	| 0 -> []
	| n -> genere(m)::(genere_sample m (n-1))


let trouve_min_hyp t p m n =
	let np = Array.length t in
	let s = genere_sample m n in
	let optimal = List.map (fun l -> fst (naif l p)) s in
		let a = ref 0. and b = ref 0. and c = ref 0. and mini = ref (-1.) in
			for i=0 to np-1 do
				for j=0 to np-1 do
					for k=0 to np-1 do
						let (at, bt, ct) = t.(i).(j).(k) in
							let e = app_sample s p at bt ct optimal in
								if (!mini < 0.) || (e < !mini) then begin
									a := at;
									b := bt;
									c := ct;
									mini := e
								end
					done
				done
			done;
			(!a, !b, !c, !mini)

let teste p m n taille =
	trouve_min_hyp (creet taille) p m n

let rec calc_chemin_alea pi = function
	| [] -> 0.
	| (w, p, eps)::l -> w /. pi +. (if Random.float 1. <= eps then (calc_chemin_alea (pi +. p) l) else (calc_chemin_alea pi l))

let estime_moyenne m p l =
	let r = ref 0. in
		for i = 0 to m-1 do
			r := !r +. (calc_chemin_alea p l) /. float_of_int(m)
		done;
		!r

let rec affiche = function
	| [] -> print_newline()
	| (a,b,c)::l -> begin 
						Printf.printf "(%F, %F, %F) " a b c ;
						affiche l;
					end;;
(* *)
let taille = (Array.length Sys.argv) -1 in
	let p = float_of_int(2 + (Random.int 10)) in
	(**)
	if taille = 2 then
		let n = int_of_string Sys.argv.(1) in
			let l = genere n in
			let ll =  (appheur l (heuris1 1.5 1. 1.5)) in
				let r = estime_moyenne (int_of_string Sys.argv.(2)) p ll in 
					begin
						print_float(p);
						print_newline();
						affiche ll;
						let exact = calct p ll in Printf.printf "\n%F\n%F\nPourcentage : %F\n" r exact (100. *. (r /. exact -. 1.));
					end

	else if taille = 3 then
		let (a,b,c, mini) = teste p (int_of_string Sys.argv.(1)) (int_of_string Sys.argv.(2)) (int_of_string Sys.argv.(3)) in
			Printf.printf "%F\n%F\n%F\n\n%F\n" a b c mini ;
	
	else let n = (int_of_string(Sys.argv.(1))) in
		let l = genere n in
		let r = appheur l (heuris1 (float_of_string Sys.argv.(2)) (float_of_string Sys.argv.(3)) (float_of_string Sys.argv.(4))) in 
		let b = calct p r in
		let (c,d) = naif l p in
			Printf.printf "\n\nPuissance : %F\n" p ;
			affiche r ;
			Printf.printf "%F\n\n%F\n" b c ;
			affiche d;
			Printf.printf "\n\n\n%F\n" (100. *. ((b /. c) -. 1.));;
