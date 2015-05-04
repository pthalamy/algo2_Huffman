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

      type Trace is array(1..8) of ChiffreBinaire;

      procedure Affiche_Rec(A : in Arbre;
			    T : in out Trace;
			    C : in out Natural) is
      begin
	 if A.all.EstFeuille then
	    for I in T'First..C loop
	       Put (Integer'Image(Integer(T(I))));
	    end loop;

	    Put_Line (" -> " & A.Char);
	 else
	    C := C + 1;

	    if A.Fils(0) /= null then
	       T(C) := 0;
	       Affiche_Rec (A.Fils(0), T, C);
	    end if;

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

      P : Natural := 0;
      PSum : Natural := 0;
      D : Arbre;
      Statut : Boolean;
      Fils_Gauche, Fils_Droite : Arbre;
   begin
      Put_Line ("=> Calcul de l'arbre de Huffman");

      Nb_Feuilles := 0;

      -- Mise en place de la file de priorité
      for I in Frequences'Range loop
	 if Frequences(I) > 0 then
	    Nb_Feuilles := Nb_Feuilles + 1;
	 end if;
      end loop;

      --  Put_Line ("Taille:" & Integer'Image(Taille));

      F := Nouvelle_File (Nb_Feuilles);

      for I in Frequences'Range loop
	 if Frequences(I) > 0 then
	    Feuille := new Noeud'(EstFeuille => True, Char => I);
	    Insertion(F, Frequences(I), Feuille);
	 end if;
      end loop;

      loop
	 Meilleur (F, P, D, Statut);
	 exit when not Statut;
	 --  Put_Line (Integer'Image(P));
	 PSum := PSum + P;
	 Fils_Gauche := D;
	 Suppression (F);

	 Meilleur (F, P, D, Statut);
	 exit when not Statut;
	 PSum := PSum + P;
	 Fils_Droite := D;
	 Suppression (F);

	 A := new Noeud'(EstFeuille => False,
			 Fils => (Fils_Gauche, Fils_Droite));
	 Insertion(F, PSum, A);
	 PSum := 0;
      end loop;

      return A;
   end Calcul_Arbre;

   procedure Encode_Arbre(A : in Arbre;
			  T : in out Code) is

      Bit_Cour : Natural := 1;

      procedure Encode_Arbre_Rec(A : in Arbre;
				 T : in out Code;
				 Bit_Cour : in out Natural) is
	 Tmp, R : Integer;
      begin
	 if A.all.EstFeuille then
	    --  Put ("1" & A.Char);
	    T(Bit_Cour) := 1;
	    Bit_Cour := Bit_Cour + 1;

	    Tmp := Character'Pos(A.Char);
   	    for I in 1..8 loop
   	       R := Tmp mod 2;
   	       T(Bit_Cour+(8-I)) := R;
   	       Tmp := Tmp / 2;
   	    end loop;

	    Bit_Cour := Bit_Cour + 8;
	 else
	    --  Put ("0");
	    T(Bit_Cour) := 0;
	    Bit_Cour := Bit_Cour + 1;

	    if A.Fils(0) /= null then
	       Encode_Arbre_Rec (A.Fils(0), T, Bit_Cour);
	    end if;

	    if A.Fils(1) /= null then
	       Encode_Arbre_Rec (A.Fils(1), T, Bit_Cour);
	    end if;
	 end if;
      end Encode_Arbre_Rec;

   begin
      Put_Line ("=> Encodage de l'arbre de Huffman");

      Encode_Arbre_Rec (A, T, Bit_Cour);

      for I in Bit_Cour..T'Last loop
	 T(I) := 0;
      end loop;
   end Encode_Arbre;

   function Decode_Arbre(T : in out Code) return Arbre is
      A : Arbre := NULL;
      Bit_Cour : Natural := 1;

      procedure Decode_Arbre_Rec(A : in out Arbre;
				 T : in out Code;
				 Bit_Cour : in out Natural) is
	 CarPos : Integer := 0;
      begin
	 if T(Bit_Cour) = 0 then
	    Bit_Cour := Bit_Cour + 1;
	    A := new Noeud'(EstFeuille => False,
			    Fils => (null,null));
	    Decode_Arbre_Rec(A.Fils(0), T, Bit_Cour);
	    Decode_Arbre_Rec(A.Fils(1), T, Bit_Cour);
	 else
	    CarPos := CarPos + T(Bit_Cour + 1);
	    for J in 2..8 loop
	       CarPos := CarPos * 2;
   	       CarPos := CarPos + T(Bit_Cour + J);
   	    end loop;
	    Bit_Cour := Bit_Cour + 9;
	    A := new Noeud'(EstFeuille => True,
			    Char => Character'Val(CarPos));
	 end if;
      end Decode_Arbre_Rec;
   begin
      Put_Line ("=> Décodage de l'arbre de Huffman");

      Decode_Arbre_Rec (A, T, Bit_Cour);

      Liberer (T);
      T := null;

      return A;
   end Decode_Arbre;

   function Calcul_Dictionnaire(A : Arbre) return Dico is
      type Trace is array(1..8) of ChiffreBinaire;

      procedure Calcul_Dictionnaire_Rec (A : in Arbre;
					 D : in out Dico;
					 T : in out Trace;
					 C : in out Natural) is
      begin
	 if A.all.EstFeuille then

	    D(A.Char) := new TabBits(T'First..C);
	    for I in D(A.Char)'range loop
	       D(A.Char)(I) := T(I);
	    end loop;

	 else
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
   	    Reste := new TabBits(1..8);
	    Bit_Cour := 1;
   	    Caractere := Octet_Suivant;
   	    Tmp := Character'Pos(Caractere);
   	    for I in Reste'Range loop
   	       R := Tmp mod 2;
   	       Reste(Reste'Last + Reste'First - I) := R;
   	       Tmp := Tmp / 2;
   	    end loop;
   	 end if;

   	 Position_Courante := Position_Courante.Fils(Reste(Bit_Cour)) ;

   	 if Bit_Cour = 8 then
   	    Liberer(Reste);
   	    Reste := null;
   	 else
   	    Bit_Cour := Bit_Cour + 1;
   	 end if;
      end loop;
      Caractere := Position_Courante.Char;
   end;

end;
