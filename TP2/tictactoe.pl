	/*********************************
	DESCRIPTION DU JEU DU TIC-TAC-TOE
	*********************************/

	/*
	Une situation est decrite par une matrice 3x3.
	Chaque case est soit un emplacement libre (Variable LIBRE), soit contient le symbole d'un des 2 joueurs (o ou x)

	Contrairement a la convention du tp precedent, pour modeliser une case libre
	dans une matrice on n'utilise pas une constante speciale (ex : nil, 'vide', 'libre','inoccupee' ...);
	On utilise plut�t un identificateur de variable, qui n'est pas unifiee (ex : X, A, ... ou _) .
	La situation initiale est une "matrice" 3x3 (liste de 3 listes de 3 termes chacune)
	o� chaque terme est une variable libre.	
	Chaque coup d'un des 2 joureurs consiste a donner une valeur (symbole x ou o) a une case libre de la grille
	et non a deplacer des symboles deja presents sur la grille.		
	
	Pour placer un symbole dans une grille S1, il suffit d'unifier une des variables encore libres de la matrice S1,
	soit en ecrivant directement Case=o ou Case=x, ou bien en accedant a cette case avec les predicats member, nth1, ...
	La grille S1 a change d'etat, mais on n'a pas besoin de 2 arguments representant la grille avant et apres le coup,
	un seul suffit.
	Ainsi si on joue un coup en S, S perd une variable libre, mais peut continuer a s'appeler S (on n'a pas besoin de la designer
	par un nouvel identificateur).
	*/

situation_initiale([ [_,_,_],
                     [_,_,_],
                     [_,_,_] ]).

	% Convention (arbitraire) : c'est x qui commence

joueur_initial(x).


	% Definition de la relation adversaire/2

adversaire(x,o).
adversaire(o,x).


	/****************************************************
	 DEFINIR ICI a l'aide du predicat ground/1 comment
	 reconnaitre une situation terminale dans laquelle il
	 n'y a aucun emplacement libre : aucun joueur ne peut
	 continuer a jouer (quel qu'il soit).
	 ****************************************************/

situation_terminale(_Joueur, Situation) :- ground(Situation).

	/***************************
	DEFINITIONS D'UN ALIGNEMENT
	***************************/

alignement(L, Matrix) :- ligne(    L,Matrix).
alignement(C, Matrix) :- colonne(  C,Matrix).
alignement(D, Matrix) :- diagonale(D,Matrix).

	/********************************************
	 DEFINIR ICI chaque type d'alignement maximal 
 	 existant dans une matrice carree NxN.
	 ********************************************/
	
ligne(L, M) :-  
	nth1(_, M, L).


get_transpose_line(_, [], []).
get_transpose_line(Index, [L | Tm], [E | Tl]) :-
	nth1(Index, L, E),
	get_transpose_line(Index, Tm, Tl).


do_transpose(0, [], _, []).
do_transpose(IndexC, [_ | T], M, [Transposed_line | TT]) :-
	get_transpose_line(IndexC, M, Transposed_line),
	do_transpose(I, T, M, TT),
	IndexC is I + 1.

transpose(M, Transpose) :-
	do_transpose(_, M, M, Transpose_tmp),
	reverse(Transpose_tmp, Transpose).

colonne(C,M) :- 
	transpose(M, Transpose),
	ligne(C, Transpose).


	/* Definition de la relation liant une diagonale D a la matrice M dans laquelle elle se trouve.
		il y en a 2 sortes de diagonales dans une matrice carree(https://fr.wikipedia.org/wiki/Diagonale) :
		- la premiere diagonale (principale)  : (A I)
		- la seconde diagonale                : (Z R)
		A . . . . . . . Z
		. \ . . . . . / .
		. . \ . . . / . .
		. . . \ . / . . .
		. . . . X . . .
		. . . / . \ . . . 
		. . / . . . \ . .
		. / . . . . . \ .
		R . . . . . . . I
	*/
		
list_length([], 0).
list_length([_ | T], N) :-
	list_length(T, N_tmp),
	N is N_tmp + 1.

diagonale(D, M) :- premiere_diag(1,D,M).
diagonale(D, M) :- 
	list_length(M, Length),
	seconde_diag(Length,D,M).

premiere_diag(_,[],[]).
premiere_diag(K,[E|D],[Ligne|M]) :-
	nth1(K,Ligne,E),
	K1 is K+1,
	premiere_diag(K1,D,M).

% definition de la seconde diagonale
seconde_diag(_,[],[]).
seconde_diag(K, [E|D], [Ligne|M]) :- 
	nth1(K,Ligne,E),
	K1 is K-1,
	seconde_diag(K1,D,M).


	/*****************************
	 DEFINITION D'UN ALIGNEMENT 
	 POSSIBLE POUR UN JOUEUR DONNE
	 *****************************/

possible([X|L], J) :- unifiable(X,J), possible(L,J).
possible(  [],  _).

	/* Attention 
	il faut juste verifier le caractere unifiable
	de chaque emplacement de la liste, mais il ne
	faut pas realiser l'unification.
	*/

unifiable(X,_) :- var(X).
unifiable(X,J) :- ground(X), X = J.
	
	/**********************************
	 DEFINITION D'UN ALIGNEMENT GAGNANT
	 OU PERDANT POUR UN JOUEUR DONNE J
	 **********************************/

alignement_gagnant([], _).
alignement_gagnant([H | T], J) :-
	ground(H),
	H == J,
	alignement_gagnant(T, J).


alignement_perdant(Ali, J) :-
	adversaire(J, Adv),
	alignement_gagnant(Ali, Adv).

	/* ****************************
	DEFINITION D'UN ETAT SUCCESSEUR
	****************************** */

	/* 
	Il faut definir quelle operation subit la matrice
	M representant l'Etat courant
	lorsqu'un joueur J joue en coordonnees [L,C]
	*/	

successeur(J, Etat, [L,C]) :-
	nth1(L, Etat, Ligne),
	nth1(C, Ligne, Case),
	unifiable(Case, J),
	Case = J.

	/**************************************
   	 EVALUATION HEURISTIQUE D'UNE SITUATION
  	 **************************************/

	/*
	1/ l'heuristique est +infini si la situation J est gagnante pour J
	2/ l'heuristique est -infini si la situation J est perdante pour J
	3/ sinon, on fait la difference entre :
	   le nombre d'alignements possibles pour J
	moins
 	   le nombre d'alignements possibles pour l'adversaire de J
*/
nb_alignement_gagnant(_, [], 0).

nb_alignement_gagnant(J, [Alig | TAlig], N) :-
	not(possible(Alig, J)),
	nb_alignement_gagnant(J, TAlig, N).

nb_alignement_gagnant(J, [Alig | TAlig], N) :-
	possible(Alig, J),
	nb_alignement_gagnant(J, TAlig, N_tmp),
	N is N_tmp + 1.

heuristique(J,Situation,H) :-		% cas 1
   H = 10000,				% grand nombre approximant +infini
   alignement(Alig,Situation),
   alignement_gagnant(Alig,J), !.
	
heuristique(J,Situation,H) :-		% cas 2
   H = -10000,				% grand nombre approximant -infini
   alignement(Alig,Situation),
   alignement_perdant(Alig,J), !.	


% on ne vient ici que si les cut precedents n'ont pas fonctionne,
% c-a-d si Situation n'est ni perdante ni gagnante.

heuristique(J,Situation,H) :- 
    findall(G, (alignement(G, Situation), possible(G,J)), Al_gagnants),
    length(Al_gagnants, Lg),
    adversaire(J, J2),
    findall(P, (alignement(P, Situation), possible(P,J2)), Al_perdants),
    length(Al_perdants, Lp),
    H is Lg-Lp.
