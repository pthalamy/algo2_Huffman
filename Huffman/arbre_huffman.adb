with Ada.Text_IO, Comparaisons, File_Priorite;
use Ada.Text_IO, Comparaisons;

package body Arbre_Huffman is

   package Ma_File is new File_Priorite(Natural, Compare, Arbre);
   use Ma_File;

   type TabFils is array(ChiffreBinaire) of Arbre ;

   type Noeud(EstFeuille: Boolean) is record
      case EstFeuille is
	 when True => Char : Character;
	 when False =>
	    Fils: TabFils;
	    -- on a: Fils(0) /= null and Fils(1) /= null
      end case ;
   end record;

   procedure Affiche_Arbre(A: Arbre) is
      
      --  Garde en memoire le chemin (suite de 0 et 1) parcouru
      --  jusqu'a une feuille
      type Trace is array(1..8) of ChiffreBinaire;
      
      --  Visite tout l'arbre A recursivement et en memorisant le chemin
      --  parcouru dans T et l'affiche une a la rencontre d'une feuille
      --  ainsi que le caractere qui lui est associe
      --  La variable C indique le nombre de 0 et 1 ecrit dans T, soit
      --  son indice courant
      procedure Affiche_Rec(A : in Arbre;
			    T : in out Trace;
			    C : in out Natural) is
      begin
	 if A.all.EstFeuille then
	    --  Affichage du chemin parcouru
	    for I in T'First..C loop
	       Put (Integer'Image(Integer(T(I))));
	    end loop;
	    --  Et du caractere correspondant
	    Put_Line (" -> " & A.Char);
	 else
	    C := C + 1;
	    
	    --  Visite du fils gauche
	    if A.Fils(0) /= null then
	       T(C) := 0;
	       Affiche_Rec (A.Fils(0), T, C);
	    end if;
	    
	    --  Visite du fils droit
	    if A.Fils(1) /= null then
	       T(C) := 1;
	       Affiche_Rec (A.Fils(1), T, C);
	    end if;

	    C := C - 1;
	 end if;

      end Affiche_Rec;

      T : Trace;
      C : Natural := 0;
   begin
      Put_Line ("=> Affichage de l'arbre de Huffman :");

      if A = null then
	 Put_Line ("Arbre inexistant.");
	 return;
      end if;

      Affiche_Rec (A, T, C);

   end Affiche_Arbre;

   --algo principal : calcul l'arbre a partir des frequences
   function Calcul_Arbre(Frequences : in Tableau_Ascii;
			 Nb_Feuilles : out Natural) return Arbre is
      A : Arbre; 
      F : File; 
      Feuille : Arbre; 

      P : Natural := 0; 		--  Priorite du meilleur element
      PSum : Natural := 0;		--  Somme des priorites des deux 
					--  meilleurs elements
      D : Arbre;			--  Donnee du meilleur element
      Statut : Boolean;			--  Indique si la file est vide ou non
      Fils_Gauche, Fils_Droite : Arbre; 
   begin
      Put_Line ("=> Calcul de l'arbre de Huffman");

      Nb_Feuilles := 0;

      -- Mise en place de la file de priorité
      --  Et memorisation du nombre de feuille, utilise dans la suite
      for I in Frequences'Range loop
	 if Frequences(I) > 0 then
	    Nb_Feuilles := Nb_Feuilles + 1;
	 end if;
      end loop;
      
      --  Initialisation de la file de taille constante = nombre de feuilles
      F := Nouvelle_File (Nb_Feuilles);
      
      --  Creation d'un noeud feuille par caractere non nul dans la File de priorite
      for I in Frequences'Range loop
	 if Frequences(I) > 0 then
	    Feuille := new Noeud'(EstFeuille => True, Char => I);
	    Insertion(F, Frequences(I), Feuille);
	 end if;
      end loop;
      
      --  Tant qu'il reste des elements dans la file, fusion
      --  des deux arbres de priorite min tel que decrit dans le sujet
      loop
	 --  Recuperation de l'arbre de prio min
	 --  En tant que FG du futur arbre
	 Meilleur (F, P, D, Statut);
	 exit when not Statut;
	 PSum := PSum + P;
	 Fils_Gauche := D;
	 Suppression (F);

	 --  Recuperation de l'arbre de 2e prio min
	 --  En tant que FD du futur arbre
	 Meilleur (F, P, D, Statut);
	 exit when not Statut;
	 PSum := PSum + P;
	 Fils_Droite := D;
	 Suppression (F);
	 
	 --  Fusion des deux arbres recuperes via creation d'un 
	 --  noeud pere aux deux
	 A := new Noeud'(EstFeuille => False,
			 Fils => (Fils_Gauche, Fils_Droite));
	 --  Puis ajout de cet arbre dans la file avec prio = pFG + pFD
	 Insertion(F, PSum, A);
	 PSum := 0;
      end loop;

      return A;
   end Calcul_Arbre;
   
   -- Pour un arbre A en entree, retourne en sortie
   --  un tableau de bits T qui represente l'encodage binaire
   --  de l'arbre A
   procedure Encode_Arbre(A : in Arbre;
			  T : in out Code) is

      Bit_Cour : Natural := 1; 		--  Indice courant dans T

      procedure Encode_Arbre_Rec(A : in Arbre;
				 T : in out Code;
				 Bit_Cour : in out Natural) is
	 Tmp, R : Integer; 		--  Variables pour stockage char dans tab_bits
      begin
	 if A.all.EstFeuille then
	    --  Codage d'une feuille : 
	    --   + Un 1
	    T(Bit_Cour) := 1;
	    Bit_Cour := Bit_Cour + 1;
	    
	    --  + Suivi du code binaire de son caractere ASCII
	    Tmp := Character'Pos(A.Char);
   	    for I in 1..8 loop
   	       R := Tmp mod 2;
   	       T(Bit_Cour+(8-I)) := R;
   	       Tmp := Tmp / 2;
   	    end loop;

	    Bit_Cour := Bit_Cour + 8;
	 else
	    --  Codage d'un noeud intermediaire :
	    --   + Un 0
	    T(Bit_Cour) := 0;
	    Bit_Cour := Bit_Cour + 1;
	    
	    --   + Codage du FG
	    if A.Fils(0) /= null then
	       Encode_Arbre_Rec (A.Fils(0), T, Bit_Cour);
	    end if;
	    
	    --   + Codage du FD
	    if A.Fils(1) /= null then
	       Encode_Arbre_Rec (A.Fils(1), T, Bit_Cour);
	    end if;
	 end if;
      end Encode_Arbre_Rec;

   begin
      Put_Line ("=> Encodage de l'arbre de Huffman");

      Encode_Arbre_Rec (A, T, Bit_Cour);
      
      --  Remplissage du reste du tableau de 0
      --   (necessaire car on force T'Length a un multiple de 8)
      for I in Bit_Cour..T'Last loop
	 T(I) := 0;
      end loop;
   end Encode_Arbre;
   
   -- fonction inverse de Encode_Arbre
   -- Prend en entree un tableau de bits T qui represente l'encodage binaire
   --  de l'arbre A retourne par la fonction
   function Decode_Arbre(T : in out Code) return Arbre is
      A : Arbre := NULL;		--  L'arbre a retourner
      Bit_Cour : Natural := 1;		--  Indice courant de T

      procedure Decode_Arbre_Rec(A : in out Arbre;
				 T : in out Code;
				 Bit_Cour : in out Natural) is
	 CarPos : Integer := 0;		--  Valeur entiere correspondant au
					--  caractere ascii decode
      begin
	 if T(Bit_Cour) = 0 then
	    --  On a un 0, creation d'un noeud intermediaire
	    --   et appel recursif pour determiner ses fils
	    Bit_Cour := Bit_Cour + 1;
	    A := new Noeud'(EstFeuille => False,
			    Fils => (null,null));
	    
	    Decode_Arbre_Rec(A.Fils(0), T, Bit_Cour);
	    Decode_Arbre_Rec(A.Fils(1), T, Bit_Cour);
	 else
	    --  On a un 1, on calcule le caractere ascii correspondant
	    --  aux 8 prochains bits
	    CarPos := CarPos + T(Bit_Cour + 1);
	    for J in 2..8 loop
	       CarPos := CarPos * 2;
   	       CarPos := CarPos + T(Bit_Cour + J);
   	    end loop;
	    
	    Bit_Cour := Bit_Cour + 9;
	    
	    --  Creation de la feuille correspondante
	    A := new Noeud'(EstFeuille => True,
			    Char => Character'Val(CarPos));
	 end if;
      end Decode_Arbre_Rec;
   begin
      Put_Line ("=> Décodage de l'arbre de Huffman");

      Decode_Arbre_Rec (A, T, Bit_Cour);
      
      --  T n'est plus utile, on le libere
      Liberer (T);
      T := null;

      return A;
   end Decode_Arbre;
   
   --  A partir d'un arbre A en entier, stocke 
   --  la valeur binaire correspondant a chaque feuille 
   --  dans un dico indexe par caracteres des feuilles
   --  
   -- Fonctionnement similaire a Affichage_Arbre, mais avec stockage dans D
   function Calcul_Dictionnaire(A : Arbre) return Dico is
      type Trace is array(1..8) of ChiffreBinaire;

      procedure Calcul_Dictionnaire_Rec (A : in Arbre;
					 D : in out Dico;
					 T : in out Trace;
					 C : in out Natural) is
      begin
	 if A.all.EstFeuille then
	    --  Si on a une feuille, on stocke la trace du chemin
	    --  dans le dico a d'indice correspondant au caractere de la feuille
	    D(A.Char) := new TabBits(T'First..C);
	    for I in D(A.Char)'range loop
	       D(A.Char)(I) := T(I);
	    end loop;

	 else
	    --  Sinon, on procede a des appels recursifs jusqu'a tomber sur une feuille
	    C := C + 1;

	    if A.Fils(0) /= null then
	       T(C) := 0;
	       Calcul_Dictionnaire_Rec (A.Fils(0), D, T, C);
	    end if;

	    if A.Fils(1) /= null then
	       T(C) := 1;
	       Calcul_Dictionnaire_Rec (A.Fils(1), D, T, C);
	    end if;

	    C := C - 1;
	 end if;

      end Calcul_Dictionnaire_Rec;

      D : Dico;
      C : Natural := 0;
      T : Trace;
   begin
      Put_Line ("=> Calcul du dictionnaire");

      Calcul_Dictionnaire_Rec (A, D, T, C);

      return D;
   end;
   
   --  Afin de palier aux allocations / Desallocations a chaque iterations.
   --  On a ajoute l'argument Bit_Cour, qui associe au Tab_Bits pointe par
   --  Reste de taille fixe, memorise l'indice auquel on peut ecrire
   procedure Decodage_Code(Reste : in out Code;
   			   Arbre_Huffman : Arbre;
   			   Caractere : out Character;
			   Bit_Cour : in out Natural) is

      Position_Courante : Arbre;
      Tmp,R : Natural;
   begin
      Position_Courante := Arbre_Huffman;
      while not Position_Courante.EstFeuille loop
   	 if Reste = null then
   	    -- chargement de l'octet suivant du fichier
   	    Reste := new TabBits(1..8); --  Reste.all de taille fixe = 8 "bits"
	    Bit_Cour := 1;		--  Reste vide, Bit_Cour sur premier elt
   	    Caractere := Octet_Suivant;
   	    Tmp := Character'Pos(Caractere);
   	    for I in Reste'Range loop
   	       R := Tmp mod 2;
   	       Reste(Reste'Last + Reste'First - I) := R;
   	       Tmp := Tmp / 2;
   	    end loop;
   	 end if;

   	 Position_Courante := Position_Courante.Fils(Reste(Bit_Cour)) ;

   	 if Bit_Cour = 8 then 		--  Le Reste est plein, liberation
   	    Liberer(Reste);
   	    Reste := null;
   	 else
   	    Bit_Cour := Bit_Cour + 1;
   	 end if;
      end loop;
      
      Caractere := Position_Courante.Char;
   end;

end;
