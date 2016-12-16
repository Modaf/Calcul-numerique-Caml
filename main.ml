type floatant = {nombre : int; mutable virgule : int list};;

let e = {nombre = 2; virgule = [7;1;8;7;1;6]};;
let f = {nombre = 2; virgule = [0;0;0;9;8;2]};;
let u = {nombre = 1; virgule = [0]};;

let a = 1750980903774927596;;
a*3;;
(*negatif => à regler peut être*)
(*fact 20 = juste; fact 21 = erroné*)

let rec decalage f1 puiss = match puiss with(*multiplication par e-puiss*)
	|x when x > 0 -> let jack = f1.nombre in
						  let uniter = jack - (jack/10)*10 in
						  decalage {nombre = jack/10; virgule = uniter :: f1.virgule} (x-1);
	|_ -> f1;;

let rec fact n = match n with
	|0 -> 1
	|n -> n * fact (n-1);;

let diff f1 f2 = (*quantifie la ressemblance de nombres de floatants*)
	let l1 = f1.virgule in
	let l2 = f2.virgule in
	let rec aux l1 l2 prec= match l1 with
		|[] -> prec
		|h :: q when h = hd l2 -> aux q (tl l2) (prec + 1)
		|_ -> prec
	in aux l1 l2 0;;

let rec pow x i = match i with
	|0 -> 1.
	|i -> x *. (pow x (i-1));;

let ecrit f1 =
	(string_of_int f1.nombre) ^ "." ^
	let rec jack l = match l with
		|[] -> ""
		|h :: q -> (string_of_int h) ^ (jack q)
	in jack (f1.virgule);;

let extract float pos = (*extrait un chiffre d'un floatant*)
	let a = ref float in
	for i=1 to pos do
		a := !a -. (float_of_int (int_of_float !a));
		a := !a *. 10.;
	done;
	int_of_float !a;;

let rec plus_aux l1 l2 acc retenue= match l1 with
	|[] -> if (acc=[]) then [retenue] else ((hd acc) + 10 * retenue) :: (tl acc)
	|h :: q -> let res = retenue + h + (hd l2) in
				  if (res > 9) then let a = res - 10 in
				  plus_aux q (tl l2) (a :: acc) 1 else
				  plus_aux q (tl l2) (res :: acc) 0;;

let rec len l = match l with
	|[] -> 0
	|h :: q -> 1 + len q;;

let rec egalisation l1 l2 = (*rev*)
	let tmp1 = ref l1 and tmp2 = ref l2 in
	let n1 = len !tmp1 and n2 = len !tmp2 in
		while (len !tmp2 < len !tmp1) do
			tmp2 := 0 :: !tmp2;
		done;
		while (len !tmp2 > len !tmp1) do
			tmp1 := 0 :: !tmp1;
		done;
	(!tmp1, !tmp2);;

let plus f1 f2 = (*additionne 2 floatants*)
		let jack = egalisation (rev f1.virgule) (rev f2.virgule) in
		let vir = plus_aux (fst jack) (snd jack) [] 0 in
		if (hd vir > 9) then let b = (hd vir) - 10 in
			let vir2 = b :: (tl vir) in
			let final = {nombre = f1.nombre + f2.nombre + 1; virgule = vir2} in
			final;
		else let final = {nombre = f1.nombre + f2.nombre; virgule = vir} in
		final;;

let m1 entier f = (*multiplie 2 floatants 1er aux*)
	let rep = ref {nombre = 0; virgule = []} in
	for i=1 to entier do
		rep := plus !rep f
	done;
	!rep;;

let rec m2 liste f= (*multiplie 2 floatants 1er aux*)
	let v = vect_of_list liste in
	let n = vect_length v in
	let rep = ref {nombre = 0; virgule = []} in
	for i=0 to n-1 do
		rep := plus !rep (m1 (v.(i)) (decalage f (i+1)))
	done;
	!rep;;

let multiplication f1 f2 = plus (m1 f1.nombre f2) (m2 f1.virgule f2);; (*f1 le plus petit pour rapidité de calcul, faudrait le faire automatiquement avec un test mais flemme*)

let rec puissance f n = match n with
	|1 -> f
	|x -> multiplication f (puissance f (x-1));; (*ici f le plus petit => ok*)

let transfo flo prec = (*gros probleme de precision avec caml*)
	let res2 = extract flo 0 in
	let tmp = ref flo in
	let res1 = ref [] in
	for i=0 to prec do
		tmp := (!tmp -. (float_of_int (extract !tmp 0))) *. 10.;
		if (!tmp < 0.000000000001) then tmp := 0. else
		res1 := !res1 @ [extract !tmp 0]
	done;
	let res = {nombre = res2; virgule = !res1} in
	res;;

let division flo1 flo2 prec= (*prends deux float et on renvoit un floatant le quotient des deux*)
	(*flo1 / flo2 = res*)
	if (flo1 = flo2) then {nombre=1; virgule=[0]} else
	let u = {nombre = 1; virgule = []} in
	let res = ref {nombre = 0; virgule = [0]} in
	while ((float_of_int !res.nombre) *. flo2) +. 1. <. flo1 do (*l'entier*)
		res := !res +! u
	done;
	(*division de f1 par f2*)
	(*ici des entiers car series entieres*)
	let f1 = {nombre = (int_of_float flo1); virgule = [0]} in
	let f2 = {nombre = (int_of_float flo2); virgule = [0]} in
	for i=1 to prec do
		while ((!res +! (decalage u i)) *! f2) <! f1 do (*l'entier*)
			res := !res +! (decalage u i)
		done;
	done;
	!res;;

let div_entier f e prec= (*prends un floatant et un entier et on renvoit un floatant le quotient des deux*)
	(*f / e = res*)
	let u = {nombre = 1; virgule = []} in
	let res = ref {nombre = 0; virgule = [0]} in
	for i=0 to prec do
		while ((!res +! (decalage u i)) *! ({nombre=e; virgule=[]})) <! f do (*l'entier*)
			res := !res +! (decalage u i)
		done;
	done;
	!res;;

ecrit e;;
(div_entier e 200000 10) *! ({nombre=200000; virgule=[]});;

pow 1. 0;;

let exponentielle x precision =
	let tmp = ref {nombre = 0; virgule = [0]} in
	let tmp2 = ref {nombre = 0; virgule = [0]} in
	let res = ref "" in
	(*while (diff !valeur !tmp) < precision do*)
	for i=0 to precision do
		tmp2 := ({nombre=int_of_float (pow x i); virgule=[]});
		for j=1 to i do
			tmp2 := div_entier !tmp2 j (precision)
		done;
		tmp := !tmp +! !tmp2
		(*la factorielle devient trop grande et part dans les negatifs*)
	done;
	!tmp;;

let rec clean l = match l with
	|[] -> []
	|0 :: q -> clean q
	|x -> x;;

let comp ff1 ff2 = (*f1 > f2*)
	let f1 = {nombre = ff1.nombre; virgule=rev (clean (rev ff1.virgule))} in
	let f2 = {nombre = ff2.nombre; virgule=rev (clean (rev ff2.virgule))} in
	if (f1.nombre > f2.nombre) then true else
	if (f2.nombre > f1.nombre) then false else
	let rec aux l1 l2 = match (l1, l2) with
		|(_, []) -> true
		|([], _) -> false
		|(h1::q1, h2::q2) when h1>h2 -> true
		|(h1::q1, h2::q2) when h2>h1 -> false
		|(_, _) -> aux (tl l1) (tl l2)
	in aux f1.virgule f2.virgule;;

let prefix +! f1 f2 = plus f1 f2;;
let prefix *! f1 f2 = multiplication f1 f2;;
let prefix ^! f int = puissance f int;;
let prefix /! f1 f2 prec = division f1 f2 prec;;
let prefix >! f1 f2 = comp f1 f2;;
let prefix <! f1 f2 = comp f2 f1;;

(*loi : precision = ln (2 temps) ** 2*)
(* y = ln (2x) ^ 2*)
(*sqrt(y) = ln (2x)*)
(*2x = exp (sqrt y)*)
(*x = exp (sqrt y) / 2*)
(*loi bis : temps = (exp (sqrt precision)) / 2*)
let e10 = "2.7182818006";;
let e15 = "2.718281828458987";; (*5s*) (*modele : 24s*)
let e20 = "2.71828182845904523525";; (*16s*) (*modele : 43s*)
let e25 = "2.7182818284590452353602862";; (*46s*) (*modele : 74s*)
let e30 = "2.718281828459045235360287471339";; (*1m45s = 105s*) (*modele : 119s*)
let e35 = "2.71828182845904523536028747135266232";; (*3m13 = 193s*) (*modele : 185s*)
let e40 = "2.7182818284590452353602874713526624977552";;
let aaa = "2,7182818284590452353602874713527";;
let valeur_calc_windows = "2,7182818284590452353602874713527";;
let a = valeur_calc_windows;;
ecrit (exponentielle 1. 35);;
