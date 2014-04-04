type piece = {piece : int; mutable proprietaire : int; apport : float; travail : float; proba : float}
(* proprietaire est le joueur détenant la pièce : -1 pour neutre, i \in |N pour le joueur i *)

type joueur = {joueur : int; mutable focus : int; mutable puissance : float; mutable score_piece : float; mutable score_total : float}
(* focus est la piece visée : -1 pour aucune, i \in |N pour la piece i *)

type jeu = piece array

let np = 10 (* Nombre de pièces *);;

let nj = 2 (* Nombre de joueurs *);;

let joueurs = Array.create nj {joueur = 0; focus = -1; puissance = 0.; score_piece = 0.; score_total = 0.};;

for i = 0 to (nj-1) do
	joueurs.(i) <- {joueur = i; focus = -1; puissance = 0.; score_piece = 0.; score_total = 0.}
done;;

let pieces = Array.create np {piece = 0; proprietaire = -1; apport = 0.; travail = 0.; proba = 0.};;

for i = 0 to np-1 do
	pieces.(i) <- { piece = i; proprietaire = -1; apport = Random.float 1.; travail = Random.float 1.; proba = Random.float 1.}
done;;

let actualiser_scores () =
	for i = 0 to nj-1 do
		joueurs.(i).score_total <- joueurs.(i).score_total +. joueurs.(i).puissance;
		if joueurs.(i).focus <> -1 then joueurs.(i).score_piece <- joueurs.(i).score_piece +. joueurs.(i).puissance
	done;;

let actualiser_puissance () =
	for i = 0 to nj-1 do
		let s = ref 0. in
		for j = 0 to np-1 do
			if pieces.(j).proprietaire = i then s := !s +. pieces.(j).apport;
		done;
		joueurs.(i).puissance <- !s
	done;;

(* Principe : si plusieurs joueurs veulent prendre la meme piece, ils se mettent en difficulté :
	- course poursuite car le premier qui dépassera travail tentera sa chance en premier avec proba p, le second a donc une proba (1-p)p de l'avoir, le 3eme p(1-p)^2, etc... 
	- si très proches (dépassent travail au meme tour), choix aléatoire homogène de celui qui tentera sa chance à ce tour, les autres perdent alors un tour *)

let pick l = 
	let n = List.length l in
	let k = Random.int (n-1) in
	let rec aux i = function
		|[] -> assert false
		|h::t -> if i=0 then h else aux (i-1) t
	in aux k l;;

let actualiser_appartenance () =
	for i = 0 to np-1 do
		let l = ref [] in
		for j = 0 to nj-1 do
			if (joueurs.(j).focus = i)&&(joueurs.(j).score_piece >= pieces.(i).travail) then l := j::(!l)
		done;
		if !l <> [] then 
			( let j0 = pick !l in
			let p0 = Random.float 1. in
			joueurs.(j0).score_piece <- 0.;				
			( if p0 <= pieces.(i).proba then pieces.(i).proprietaire <- j0 );
			joueurs.(j0).score_piece <- 0.;
			joueurs.(j0).focus <- -1 )
	done;;


			
			




