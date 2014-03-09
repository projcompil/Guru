Random.self_init()

(*

a, b, et c : coefficients de l'heuristique
w : temps pour convertir
p : puissance du converti
eps : probabilité de conversion

*)
let nw = 300 (* borne maximale sur les w *)
let np = 100 (* idem sur les p *)

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
	| _, 0  -> [(repete 0 n)]
	| 0, _ -> [[]]
	| n, k when n = k -> [(repete 1 k)]
	| n, k -> (List.map (aj 0) (generep (n-1) k)) @ (List.map (aj 1) (generep (n-1) (k-1)))

(* Fonction de calcul de l'espérance de temps d'un chemin *)
let rec calct pi = function
	| [] -> 0.
	| (w, p, eps)::l -> w /. pi +. (eps *. (calct (pi +. p) l) +. (1. -. eps) *. (calct pi l))

let rec calcd pi = function
	| [] -> 0.
	| (w,p, eps)::l -> w/.pi +. (calcd (pi +. p) l)

(* 0 pour l'ensemble, 1 pour le complémentaire *)
let rec associe b l ens = match (l, ens) with
	| [], _ | _, [] -> []
	| a::l, x::ens when x = b -> (associe b l ens)
	| a::l, _::ens -> a::(associe b l ens)

let rec resoutd pi l = ()

(* log en base 2 *)
let lg x = log(x) /. log(2.)

(* fonction de l'heuristique *)
let heuris1 a b c (w,p, eps) =
	-. a *. (lg (w)) +.  b *. (lg(p)) +. c *. (lg (eps))

(*let heurisg1 t a b c  =
	a(t) *.*)
	
(* fonction de comparaison à passer au tri *)	
let cmp f a b =
	if f a > f b then -1
	else if f a < f b then 1 
	else 0

(* applique l'heuristique f à l *)
let appheur l f =
	List.sort (cmp f) l

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


module Val_etat = Map.Make( struct  type t= float let compare = compare end)

type state = float*float Val_etat.t 
 
type ordi = float*float*float
 
type cible = ordi list

 
 (*liste_des crÃ©er la liste des n entiers*)
 let rec liste_des =function 
	|0 -> []
	|n -> (n-1)::(liste_des (n-1))
 
(* make_choice l renvoit la liste des liste de longueur (longueur l)/2 elements de l*)
let ajout_liste k l = List.map (fun (x,y)->(x,(k::y))) l 

let rec adding i l=function
	|[]->l 
	|(a,b)::q->adding i (((i::a),b)::l) q

let etape_choix t k i=
	let t'=t in 
		for j=(k-1) downto 0 do 
			(t.(j+1) <- (ajout_liste i t.(j+1)));
			let c =(adding i t.(j+1) t'.(j)) in t.(j+1) <- c
				done;
		t.(0) <- (ajout_liste i t.(0))

let rec aux_choice t k = function
	|[]->t.(k)
	| a::q -> (etape_choix t k a);aux_choice t k q 

let make_choice l = 
	let t = (Array.make (((List.length l)/2)+1) [])
	and k = (List.length l)/2 in
	t.(0)<- [([],[])] ;
	aux_choice t k l 

	
(*mix genere la liste de toute les permutations(tableaux) d'elements de la liste l *)
let rec aux_mix1 k l =function
	|[] -> l
	|a::q -> aux_mix1 k ((k::a)::l) q 

let rec aux_mix2 l =function 
	|0 -> l
	|p -> aux_mix2 (aux_mix1 p l l) (p-1)

let mix l= aux_mix2 [l] (List.length l)


(*sort renvoit le plus rapide (temps,etat) parmi une liste*)
let rec sort = function
	|[]->((float_of_int(max_int), Val_etat.empty),[])
	| ((a,b),l)::q -> let ((c,d),l1)= (sort q) in if a<c then ((a,b),l) else ((c,d),l1)

	
(*ajoute et nouvel_etat font evoluer le graphe des etatsde 1 itÃ©rations*)
let ajoute p (c,t) etat= 
	if (Val_etat.mem p etat ) then (let (c1,t1)= (Val_etat.find p etat) in ( (Val_etat.add p (c +. c1,((c *. t) +. (c1 *. t1))) etat)))
	else (Val_etat.add p (c,t) etat)

let nouvel_etat etat (temps,pui,chance) =
	Val_etat.fold (fun p (c,t) etat_acc -> ajoute (p +. pui) ((c *. chance), (t +. (temps /. p)))
	(ajoute p ((c *. (1. -. chance)), (t +. (temps /. p))) etat_acc)) etat Val_etat.empty

(*main_aux est la fonction clÃ© qui applique l'algo*)
let rec main_aux  (tm,etat) = function
	|[] -> ((tm,etat),[])
	| [(temps,pui,chance)] -> let etat_n = (nouvel_etat etat (temps,pui,chance)) in (((Val_etat.fold (fun p (c,t)  s -> (s +. ( c *. t )))  etat_n 0.) , etat_n ),[(temps,pui,chance)])
	|l->   (sort (List.map (fun (x,y) -> let ((tm_debut,etat_debut),chemin_debut)= (main_aux (tm,etat) x) in (let ((t,a),b)= (main_aux (tm_debut,etat_debut) y) in ((t,a),(chemin_debut@b))))
		(make_choice l)))
	
(*guru_rapide est une simple application de main_aux Ã  l'Ã©tat de base *)
let guru_rapide (env : cible) p = 
		let ((tm,etat),chemin) = (main_aux (0., (Val_etat.singleton p (1.,0.))) env) in (tm,chemin)

	
		(*main est la fonction avec gÃ©nÃ©ration*)

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
	let e = calcul p (appheur l (heuris1 a b c)) in
		(e /. opti ) -. 1.

let carre x = x *. x

(* applique l'heuristique et calcule la somme des écarts au carré sachant la liste "optimal" des solutions otpimales *)
let rec app_sample s p a b c optimal calcul = match s, optimal with
	| [], _ | _, [] -> 0.
	| l::s, o::optimal -> carre(ecart l p a b c o calcul) +. (app_sample s p a b c optimal calcul)
(*	List.fold_left (fun x l -> x +. (carre (ecart l p a b c))) 0. s*)
	
	
(* génère un échantillon *)
let rec genere_sample m = function
	| 0 -> []
	| n -> genere(m)::(genere_sample m (n-1))

(* calcule le temps mis sur un chemin aléatoire *)
let rec calc_chemin_alea pi = function
	| [] -> 0.
	| (w, p, eps)::l -> w /. pi +. (if Random.float 1. <= eps then (calc_chemin_alea (pi +. p) l) else (calc_chemin_alea pi l))

(* fait la moyenne sur m calculs de chemins aléatoires *)
let estime_moyenne m p l =
	let r = ref 0. in
		for i = 0 to m-1 do
			r := !r +. (calc_chemin_alea p l) /. float_of_int(m)
		done;
		!r
(* trouve l'hypothèse minimum sur les coefficients donnés par le tableau t, pour une puissance p, sur n listes de taille m*)
let trouve_min_hyp t p m n =
	let np = Array.length t in
	let s = genere_sample m n in
	let optimal = List.map (fun l -> fst (naif l p)) s in
	let tab = cree3d np (Array.length t.(0)) (Array.length t.(0).(0)) 0. in
		let rec aux liste opti = match liste, opti with
			| [], _ | _, [] -> ()
			| (l::liste), (o::opti) -> let h = Hashtbl.create m in
					Hashtbl.add h [] 0. ;
					for i=0 to np-1 do
						for j=0 to Array.length(t.(i))-1 do
							for k=0 to Array.length(t.(i).(j))-1 do
                                                		let (at, bt, ct) = t.(i).(j).(k) in
									let lt = appheur l (heuris1 at bt ct) in
										if Hashtbl.mem h lt then
											tab.(i).(j).(k) <- tab.(i).(j).(k) +. (Hashtbl.find h lt)
										else let r = carre((calct p lt) /. o -. 1.) in 
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

let teste p m n taille1 taille2 taille3 (c1, c2, c3) =
	trouve_min_hyp (creet taille1 taille2 taille3 (c1, c2, c3)) p m n

(* affiche une instance *)
let rec affiche = function
	| [] -> print_newline()
	| (a,b,c)::l -> begin 
						Printf.printf "(%F, %F, %F, %F) " a b c (a/.b) ;
						affiche l;
					end;;
(* début du traitement des arguments*)

let taille = (Array.length Sys.argv) -1 in
	let p = float_of_int(2 + (Random.int np)) in
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

	else if taille = 5 || taille = 8 then
		let n = int_of_string Sys.argv.(2) in
		let (a,b,c, mini) = teste p (int_of_string Sys.argv.(1)) n (int_of_string Sys.argv.(3)) (int_of_string Sys.argv.(4)) (int_of_string Sys.argv.(5)) (if taille = 5 then (0., 0., 0.) else ((float_of_string Sys.argv.(6)), (float_of_string Sys.argv.(7)), (float_of_string Sys.argv.(8)))) in
			Printf.printf "%F\n%F\n%F\n\nécart type en pourcent : %F\n" a b c (100. *. sqrt(mini /. float_of_int(n))) ;
	
	else let n = (int_of_string(Sys.argv.(1))) in
		let l = genere n in
		let r = appheur l (heuris1 (float_of_string Sys.argv.(2)) (float_of_string Sys.argv.(3)) (float_of_string Sys.argv.(4))) in 
		let b = calct p r in
		let (c,d) = naif l p in
		(*let (e,f) = guru_rapide l p in
		let ce = calct p f in*)
			Printf.printf "\n\nPuissance : %F\n" p ;
			affiche r ;
			Printf.printf "%F\n\n%F\n" (* ce *) b  c ;
			affiche d;
			Printf.printf "\n\n\n%F\n" (100. *. (((*ce*)b  /. c) -. 1.));;
