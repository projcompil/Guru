Random.self_init()

(*

a, b, et c : coefficients de l'heuristique
w : temps pour convertir
p : puissance du converti
eps : probabilité de conversion

*)

let nw = 300000 (* borne maximale sur les w *)
let np = 10000 (* idem sur les p *)

(* generec[omputer] : génère un ordinateur *)
let generec () = (float_of_int (2+ (Random.int nw)) , float_of_int (2 + (Random.int np)), Random.float 1.)

(* génère un ensemble d'ordinateurs à attaquer *)
let rec genere = function
	| 0 -> []
	| n -> generec()::(genere (n-1))


let rec repete x = function
	| 0 -> []
	| n -> x::(repete x (n-1))

let aj a l = a::l

let rec generep n k = match (n,k) with
	| _, 0  -> [(repete false n)]
	| 0, _ -> [[]]
	| n, k when n = k -> [(repete true k)]
	| n, k -> (List.map (aj false) (generep (n-1) k)) @ (List.map (aj true) (generep (n-1) (k-1)))

(* Fonction de calcul de l'espérance de temps d'un chemin *)
let rec calct pi = function
	| [] -> 0.
	| (w, p, eps)::l -> w /. pi +. (eps *. (calct (pi +. p) l) +. (1. -. eps) *. (calct pi l))


let rec calca pi = function
        | [] -> 0.
        | l ->  0.

(* fait la moyenne sur m calculs de chemins aléatoires *)
let estime_moyenne calcul m p l  =
	let r = ref 0. in
		for i = 0 to m-1 do
			r := !r +. (calcul p l) /. float_of_int(m)
		done;
		!r
(* trouve l'hypothèse minimum sur les coefficients donnés par le tableau t, pour une puissance p, sur n listes de taille m*)

let rec calcd pi = function
	| [] -> 0.
	| (w,p, eps)::l -> w/.pi +. (calcd (pi +. p) l)


let valeurd pi (w1, p1, eps1) (w2, p2, eps2) =
	 w1 /. pi +. eps1 *. w2 /. ( pi +. p1 ) +. (1. -. eps1 ) *. w2 /. pi 

let compared pi x y  =
	(valeurd pi x y) < (valeurd pi y x)
(*	w1 /. pi +. eps1 *. w2 /. ( pi +. p1 ) +. (1. -. eps1 ) *. w2 /. pi < w2
        /. pi +. eps2 *. w1 /. ( pi +. p2 ) +.( 1. -. eps2 ) *. w1 /. pi
*)		


let rec minid pi l = match l with
	| [] -> failwith "Minid d'une liste vide."
	| [x] -> x, []
	| x::l -> let y, lr = minid pi l in
								if compared pi x y then
									x, (y::lr)
								else y, (x::lr)

let rec trid l pi = match l with
	| [] | [_] -> l
	| l -> let ((w,p,eps), lr) = minid pi l in
				(w, p, eps)::(trid lr (pi +. eps *. p))

let sold l pi =
	let lr = trid l pi in
		(calct pi l), lr


let rec calcfinal pi l = match l with
	| [] -> 0.
	| [(w, p, eps)] -> w /. pi
	| (w, p, eps)::l -> eps *. (calcfinal (pi +. p) l) +. (1. -. eps) *. (calcfinal pi l)

let rec associe l ens = match (l, ens) with
	| [], _ | _, [] -> [], []
	| x::l, y::ens -> let (a, b) = associe l ens in
				if y then (x::a), b
				else a, (x::b)

let resout l pi =
	let rec aux ldebut l = match l with
		| [] -> 0., []
		| [x] -> (calcfinal pi (ldebut @ [x])), [x]
		| l -> let n = List.length l in
			let r = n/2 in
				let rec auxil liste (min, rmin) = match liste with
					| [] -> (min, rmin)
					| ens::liste -> let (ld, lf) = associe l ens in
								let rd, rld = aux ldebut ld in
								let rf, rlf = aux (ldebut @ rld) lf in
									if rd +. rf < min || min = -1. then
										auxil liste ((rd +. rf), (rld @ rlf))
									else auxil liste (min, rmin)
				in auxil (generep n r) (-1., [])
	in aux [] l

(* essayer via échantillonnage *)
let rec calcfin pi l = match l with
	| [] | [_] -> 0.
	| [x ; y ] -> valeurd pi x y
	| (w, p, eps)::l -> eps *. (calcfin (pi +. p) l) +. (1. -. eps) *. (calcfin pi l)

let rec calcfin_alea pi = function
	| [] -> 0.
	| (w, p, eps)::l -> (if Random.float 1. <= eps then (calcfin_alea (pi +. p) l) else (calcfin_alea pi l))

let construite f (w, p, eps) pi x y =
	eps *. (f (pi +. p) x y) +. (1. -. eps) *. (f pi x y)

(*
let rec minie f pi = function
	| [] -> failwith "Liste vide dans minie !"
	| [x] -> x, []
	| x::l -> let y, lr =
			minie f pi l in
			if (f pi x y) < (f pi y x) then
				x, (y::lr)
			else y, (x::lr)
*)

let rec unepasse calcul pi ldebut b = function
	| [] -> [], b
	| [x] -> [x], b
	| x::y::l -> 	if (calcfin pi (List.rev (y::x::ldebut))) < (calcfin pi (List.rev (x::y::ldebut))) then
				let (lr, br) = (unepasse calcul pi (x::ldebut) b (y::l)) in
					(x::lr), br
			else let (lr, br) =  (unepasse calcul pi (y::ldebut) true (x::l)) in
				(y::lr), true

let rec trif calcul l pi = match l with
	| [] | [_] -> l
	| l -> let (lr, br) = unepasse calcul pi [] false l in
		if br then lr
		else (List.hd lr)::(trif calcul (List.tl lr) pi)
let forall t =
	let n = Array.length t in
		let b = ref true and i = ref 0 in
			while !b && !i < n do
				b := t.(!i);
				i := !i + 1;
			done;
			!b
let starm m =
	let n = Array.length m in
		let i = ref (-1) in
		let j = ref 0 in
			while !i = -1 && !j < n do
				if forall m.(!j) then
					i := !j ;
				j := !j + 1
			done;
			!i
let extrait t i =
	let n = Array.length t in
		let rec aux j =
			if j >= n then []
			else if j = i then aux (j+1)
			else t.(j)::(aux (j+1))
		in aux 0

let stargraphe calcul pi ldebut l =
	let t = Array.of_list l in
	let n = Array.length t in
		let g = Array.make_matrix n n false in
			for i=0 to n-1 do
				for j=0 to n-1 do
					if (calcul pi (List.rev (t.(i)::t.(j)::ldebut))) > (calcul pi (List.rev (t.(j)::t.(i)::ldebut))) then
						g.(i).(j) <- true
					else g.(j).(i) <- true ;
				done
			done;
			let i = starm g in
				let l = extrait t i in
					t.(i), l
(* il faut essayer toutes les paires et trouver la star du graphe !!! *)
let rec minie pi ldebut = function
	| [] -> failwith "Erreur dans minie"
	| [x] -> x, []
	| x::l -> let y, lr = minie pi ldebut l in
			if (calcfin pi (List.rev (y::x::ldebut))) < (calcfin pi (List.rev (x::y::ldebut))) then
				x, (y::lr)
			else y, (x::lr)

let trie calcm liste pi =
	(* let rec auxi l f = match l with
		| [] ->  []
		| [x] -> [x]
		| l -> let (x, lr) = minie f pi l in
				x::(auxi lr (construite f x))
	
	*)
	let rec auxi l ld = match l with
		| [] -> List.rev ld
		| [x] -> List.rev (x::ld)
		| l -> let x, lr = stargraphe calcm pi ld l(*minie pi ld l*) in
				auxi lr (x::ld)
	in auxi liste []


let sole calcm l pi =
	let lr = trie calcm l pi in
		(calct pi l), lr

(*
let rec divise = function
	| [] -> [], []
	| [x] -> [x], []
	| x::y::l -> let (l1, l2) = divise l in (x::l1, y::l2)

let rec fusion l1 l2 pi = match (l1, l2) with
	| [], [] -> [], pi
	| (w, p, eps)::l1, [] | [], (w, p, eps)::l1 -> let (l, pr) = fusion l1 [] (p +. pi) in
								((w, p, eps)::l), pr
	| ((w1, p1, eps1)::l1) , ((w2, p2, (eps2)::l2) -> if (w1 -. w2) /. pi +. w2 /. (pi +. p1) -. w1 /. (pi +. p2) < 0. then
												let (l, pr) = fusion l1 ((w2, p2, eps2)::l2) (pi +. p1) in
													((w1, p1, eps1)::l1), pr
												else let (l, pr) = fusion ((w1, p1, eps1)::l1) l2 (pi +. p2) in
													((w2, p2, eps2)::l1), pr
let rec trifusion pi = function
	| [] -> [], pi 
	| [(w, p, eps)] -> [(w, p, eps)], (pi +. p)
	| l -> let (l1, l2) = divise l in
			let (l1r, p1r) = trifusion pi l1 in
				let (l2r, p2r) = trifusion pi l2 in
					fusion l1r l2r pi

type fllist = (float * float * float) list
let sold (l : fllist ) pi = fst (trifusion pi l)
*)
(* 0 pour l'ensemble, 1 pour le complémentaire *)

let rec resoutd pi l = ()

(* log en base 2 *)
let lg x = log(x) /. log(2.)

(* fonction de l'heuristique *)
let heuris1 pi a b c (w,p, eps) =
	-. a *. (lg (w (*/. float_of_int(nw)*))) +.  b *. (lg(p/.(pi +. p))) +. c *. (lg (eps))

(* fonction de comparaison à passer au tri *)	
let cmp f a b =
	if f a > f b then -1
	else if f a < f b then 1 
	else 0

(* applique l'heuristique f à l *)
let appheur l f =
	List.sort (cmp f) l

let triheur1 a b c l p =
	appheur l (heuris1 p a b c)

let solheur l p a b c =
	let lr = triheur1 a b c l p in
		(calct p lr), lr
(* distribute et permutation permettent de créer la liste des permutations *)
let distribute c l =
	let rec insert acc1 acc2 = function
    	| [] -> acc2
		| hd::tl -> insert (hd::acc1) ((List.rev_append acc1 (hd::c::tl)) :: acc2) tl
	in insert [] [c::l] l

let rec permutation = function
	| [] -> [[]]
	| hd::tl -> List.fold_left (fun acc x -> List.rev_append (distribute hd x) acc) [] (permutation tl)


(* calcule le minimum des espérances de temps dans la liste l des permutations *)
let minimum l p =
	let rec aux (acc,m) = function
		| [] -> (acc,m)
		| x::l -> let y = calct p x in
					if y < acc || acc < 0. then
						aux (y,x) l
					else aux (acc, m) l
	in aux (-1.,[]) l



(* algorithme naif appliqué à une instance *)
let naif l p = 
	let ll = permutation l in
		minimum ll p

let cree3d n m l x =
	let t = Array.create n [|[||]|] in
                for i = 0 to n-1 do
                        t.(i) <- Array.create m [||] ;
                done;
                for i = 0 to n-1 do
                        for j=0 to m-1 do
                                t.(i).(j) <- Array.create l x ;
                        done
                done;
		t
(* crée un tableau de taille n sur m sur l pour tester différentes heuristiques *)
let creet n m l (c1, c2, c3) =
	let t = cree3d n m l (0., 0., 0.) in 
		let r1 = 1. /. float_of_int(n) in
		let r2 = 1. /. float_of_int(m) in
		let r3 = 1. /. float_of_int(l) in
			for i=0 to n-1 do
				for j=0 to m-1 do
					for k=0 to l-1 do
						t.(i).(j).(k) <- (c1 +. float_of_int(i+1) *. r1, c2 +. float_of_int(j+1) *.r2, c3 +. float_of_int(k+1) *. r3)
					done
				done
			done;
			t
(* écart entre opti et l'espérance donné par calcul sur la liste lk ordonnée selon l'heuristique *)
let ecart l p a b c opti calcul =
	let e = calcul p (appheur l (heuris1 p a b c)) in
		(e /. opti ) -. 1.

let carre x =  x *. x
let identite x = x
let valabs x = if x >= 0. then x else -. x

(* applique l'heuristique et calcule la somme des écarts au carré sachant la liste "optimal" des solutions otpimales *)
let rec app_sample s p a b c optimal calcul = match s, optimal with
	| [], _ | _, [] -> 0.
	| l::s, o::optimal -> carre(ecart l p a b c o calcul) +. (app_sample s p a b c optimal calcul)
(*	List.fold_left (fun x l -> x +. (carre (ecart l p a b c))) 0. s*)
	
	
(* génère un échantillon *)

let genere_tout m = (float_of_int(2 + Random.int(np)), genere(m))

let rec genere_sample m = function
	| 0 -> []
	| n -> (genere_tout m)::(genere_sample m (n-1))

(* calcule le temps mis sur un chemin aléatoire *)
let rec calc_chemin_alea pi = function
	| [] -> 0.
	| (w, p, eps)::l -> w /. pi +. (if Random.float 1. <= eps then (calc_chemin_alea (pi +. p) l) else (calc_chemin_alea pi l))

let trouve_min_hyp t m n resol calcul ferreur =
	let np = Array.length t in
	let s = genere_sample m n in
	let optimal = List.map (fun (p,l) -> fst (resol l p)) s in
	let tab = cree3d np (Array.length t.(0)) (Array.length t.(0).(0)) 0. in
		let rec aux liste opti = match liste, opti with
			| [], _ | _, [] -> ()
			| ((p,l)::liste), (o::opti) -> let h = Hashtbl.create m in
					Hashtbl.add h [] 0. ;
					for i=0 to np-1 do
						for j=0 to Array.length(t.(i))-1 do
							for k=0 to Array.length(t.(i).(j))-1 do
                                                		let (at, bt, ct) = t.(i).(j).(k) in
									let lt = appheur l (heuris1 p at  bt  ct) in
										if Hashtbl.mem h lt then
											tab.(i).(j).(k) <- tab.(i).(j).(k) +. (Hashtbl.find h lt)
										else let r = ferreur((calcul p lt) /. o -. 1.) in 
											begin
												Hashtbl.add h lt r ;
												tab.(i).(j).(k) <- tab.(i).(j).(k) +. r ;
											end;
							done;
						done;
					done;
					aux liste opti ;
		in
			aux s optimal ;								
			let im = ref 0 and jm = ref 0 and km = ref 0 and mini = ref tab.(0).(0).(0) in
				for i=0 to np-1 do
					for j=0 to Array.length(t.(i))-1 do
						for k=0 to Array.length(t.(i).(j))-1 do
								(*let e = app_sample s p at bt ct optimal calct  in*)
							if tab.(i).(j).(k) < !mini then begin
								im := i;
								jm := j;
								km := k;
								mini := tab.(i).(j).(k)
							end
						done
					done
				done;
			let (a,b,c) = t.(!im).(!jm).(!km) in 
				(a, b, c, !mini)

let teste m n taille1 taille2 taille3 (c1, c2, c3) resol calcul ferreur=
	trouve_min_hyp (creet taille1 taille2 taille3 (c1, c2, c3)) m n resol calcul ferreur


let trig calcul l pi = (trif calcul (appheur l (heuris1 pi 1. 0.5 1.)) pi)

let solg calcul l pi =
	let lr = (trif calcul (appheur l (heuris1 pi 1. 1. 1.)) pi)
		in (calct pi lr), l
(* affiche une instance *)
let rec affiche = function
	| [] -> print_newline()
	| (a,b,c)::l -> begin 
						Printf.printf "(%F, %F, %F, %F) " a b c (c *. b /. a) ;
						affiche l;
					end;;

(* m taille de l'instance
   n nombre d'instances
   resol fonction de résolution (exacte)
   approx : approximation
   calcul : calcul de l'espérance du temps
   ferreur : fonction de coût
*)
let testeun m n resol approx calcul ferreur =
        let rec aux n acc = match n with
                | 0 -> acc
                | n -> let (p,l) = genere_tout m in
                                let optimal,lr = (resol l p) in
                                        let lt = approx l p in
                                        let ct = calcul p lt in
                                               	(*if ct > optimal +. 1e-10 then
							begin
                                                        print_endline("Optimal :");
							affiche lr;
                                                        print_endline("Approx :");
							affiche lt;
							Printf.printf "\n%F  ::: %F pourcent de différence\n"  (ct -. optimal) (100. *.(ct /. optimal -. 1.));
							end;
						*)
						aux (n-1) (acc +. ferreur(ct /. optimal -. 1.))
        in aux n 0.;;

(* début du traitement des arguments*)

let taille = (Array.length Sys.argv) -1 in
	let p = float_of_int(2 + (Random.int np)) in
	(**)
	if taille = 1 then
		let n = int_of_string Sys.argv.(1) in
			let l = genere n in
			(*let ll =  (apph(int_of_string Sys.argv.(1))eur l (heuris1 1.5 1. 1.5)) in
				let r = estime_moyenne (int_of_string Sys.argv.(2)) p ll in *)
				let llapp = trie calcfin(*estime_moyenne calcfin_alea (n*n)*) l p (*snd(sold l p) *) in
				let (exact, ll) =  naif l p (*solheur l p 1. 1. 1. *) in
				let cllapp = calct p llapp in
					begin
						print_float(p);
						print_newline();
						affiche llapp;
						affiche ll;
						(*let exact = calct p ll in *) Printf.printf "\n%F\n%F\nPourcentage : %F\n" (*r*) cllapp  exact (100. *. (cllapp /. exact -. 1.));
					end
        else if taille = 2 then
                let n = int_of_string Sys.argv.(2) in
		let m = int_of_string Sys.argv.(1) in
                        Printf.printf  "%F,%d,%d\n" (
                        (testeun m n  (resout) (*fun l p -> solheur l p 1. 1.
                        1.*) (*fun l p -> snd(resout l p)*) (*fun l p -> trig
                        calcfin l p*) (triheur1 1. 1. 1.) (*trig
                        (estime_moyenne calcfin_alea (m*m))*) (*trid*) calct identite)/. float_of_int(n)) m n


	else if taille = 5 || taille = 8 then
		let n = int_of_string Sys.argv.(2) in
		let m = int_of_string Sys.argv.(1) in
		let (a,b,c, mini) = teste m n (int_of_string Sys.argv.(3)) (int_of_string Sys.argv.(4)) (int_of_string Sys.argv.(5)) (if taille = 5 then (0., 0., 0.) else ((float_of_string Sys.argv.(6)), (float_of_string Sys.argv.(7)), (float_of_string Sys.argv.(8)))) resout (*sole calcfin*)(* (fun l p -> let (pr, lr) = guru_rapide l p in ((calct p lr), lr))*) calct identite in
			Printf.printf "%F\n%F\n%F\n\nécart type en pourcent : %F\n" a b c (100. *. (*sqrt*)(mini /. float_of_int(n))) ;
	
	else let n = (int_of_string(Sys.argv.(1))) in
		let l = genere n in
		let r = appheur l (heuris1 p (float_of_string Sys.argv.(2)) (float_of_string Sys.argv.(3)) (float_of_string Sys.argv.(4))) in 
		let b = calct p r in
		let (c,d) = naif l p in
		(*let (e,f) = guru_rapide l p in
		let ce = calct p f in*)
			Printf.printf "\n\nPuissance : %F\n" p ;
			affiche r ;
			Printf.printf "%F\n\n%F\n" (* ce *) b  c ;
			affiche d;
			Printf.printf "\n\n\n%F\n" (100. *. (((*ce*)b  /. c) -. 1.));;
