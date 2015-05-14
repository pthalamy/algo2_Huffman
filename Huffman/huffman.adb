with Ada.Text_IO, Ada.Integer_Text_IO, Ada.Command_Line, Ada.Streams.Stream_IO, Arbre_Huffman;
use Ada.Text_IO, Ada.Integer_Text_IO, Ada.Command_Line, Ada.Streams.Stream_IO, Arbre_Huffman;

procedure Huffman is

   Fichier_Invalide : exception;
   --calcul des frequences d'apparition des lettre dans un fichier
   procedure Lecture_Frequences(Nom_Fichier: in String ;
				Frequences : out Tableau_Ascii ;
				Taille : out Natural) is

      Fichier: Ada.Streams.Stream_IO.File_Type;
      Acces : Stream_Access;
      Char: Character;
   begin
      for I in Frequences'range loop
	 Frequences(I) := 0;
      end loop;

      Open(Fichier, In_File, Nom_Fichier);
      Acces := Stream (Fichier);
      Taille := 0;

      while not End_Of_File(Fichier) loop
	 Char := Character'input(Acces);
	 Taille := Taille + 1;
	 Frequences(Char) := Frequences(Char) + 1;
      end loop;
      Close(Fichier);
   end Lecture_Frequences;

   --affichage pour info et verifications
   --affiche pour chaque caractere du fichier le nombre de fois qu'il apparait
   procedure Affiche_Frequences(Frequences: Tableau_Ascii) is
   begin
      for I in Frequences'range loop
	 if Frequences(I) > 0 then
	    Put("le caractère '");
	    Put(I);
	    Put("' apparait ");
	    Put(Frequences(I));
	    Put(" fois");
	    New_Line;
	 end if;
      end loop;
   end Affiche_Frequences;

   --recupere le prochain caractere a ecrire dans le fichier compresse
   procedure Recuperation_Caractere(Reste : in out Code;
				    Entree : Ada.Streams.Stream_IO.File_Type ;
				    Acces : in out Stream_Access ;
				    Caractere_Sortie : out Character ;
				    D : Dico) is

      Compte : Natural; --combien de bits on a reussi a generer
      Nouveau_Reste : Code; --les bits encore inutilises apres generation
			    -- du caractere
      Caractere_Entree : Character; --le prochain caractere lu dans
				    -- le fichier non compresse
   begin
      -- on recupere les 8 premiers octets du code
      -- si il n'y en a pas assez, on lit un nouveau code a partir
      -- du fichier d'entree
      Compte := 0;
      Caractere_Sortie := Character'Val(0);
      while Compte /= 8 loop
	 if (Reste = null) then
	    --lecture d'un nouveau code a partir du fichier
	    if (End_Of_File(Entree)) then
	       -- a la fin du fichier,
	       -- il est necessaire de rajouter quelques zero
	       Reste := new TabBits(1..(8-Compte));
	       for I in Reste'Range loop
		  Reste(I) := 0;
	       end loop;
	    else
	       Caractere_Entree := Character'Input(Acces);
	       --attention, il faut faire une copie de l'original
	       --afin de pouvoir liberer la memoire plus tard
	       Reste := D(Caractere_Entree);
	       Nouveau_Reste := new TabBits(Reste'Range);
	       For I in Reste'Range loop
		  Nouveau_Reste(I) := Reste(I);
	       end loop;
	       Reste := Nouveau_Reste;
	    end if;
	 end if;

	 for I in Reste'Range loop
	    Caractere_Sortie := Character'Val(Character'Pos(Caractere_Sortie)*2
						+ Reste(I));
	    Compte := Compte + 1;
	    if Compte = 8 then
	       --mise a jour du reste
	       if (Reste'Last - I) > 0 then
		  Nouveau_Reste := new TabBits(1..(Reste'Last - I));
		  for J in Nouveau_Reste'Range loop
		     Nouveau_Reste(J) := Reste(I+J);
		  end loop;
		  Liberer(Reste);
		  Reste := Nouveau_Reste;
	       else
		  Liberer(Reste);
		  Reste := null;
	       end if;
	       return;
	    end if;
	 end loop;
	 Liberer(Reste);
	 Reste := null;
      end loop;

   end Recuperation_Caractere;

   -- Ecrit le code binaire de l'arbre encodé dans le fichier .huff
   procedure Output_Arbre_Enc(Huff_Enc : in out Code;
			      SAcces : in out Stream_Access) is
      Compte : Natural; -- combien de bits on a reussi a generer
      Caractere_Sortie : Character; 	--  Octet a ecrire
   begin
      Compte := 0;
      Caractere_Sortie := Character'Val(0);
      
      --  Remplissage de Caractere_Sortie a partir des valeurs de Huff_Enc
      for I in Huff_Enc'Range loop
	 Caractere_Sortie := Character'Val(Character'Pos(Caractere_Sortie)*2
					     + Huff_Enc(I));
	 Compte := Compte + 1;
	 if Compte = 8 then
	    Character'Output(SAcces, Caractere_Sortie);
	    Caractere_Sortie := Character'Val(0);
	    Compte := 0;
	 end if;
      end loop;
      
      --  Huff_Enc plus utile, liberation
      Liberer(Huff_Enc);
      Huff_Enc := null;
   end Output_Arbre_Enc;

   -- Récupère le code binaire de l'arbre encodé dans le fichier .huff
   --  Et le stocke dans Huff_Enc
   procedure Input_Arbre_Enc(Huff_Enc : in out Code;
			     EAcces : in out Stream_Access) is
      Bit_Cour : Natural;  		--  Indice courant dans Huff_Enc
      Caractere_Entree : Character; 	--  Mot de 8 bits lu

      R, Tmp : Natural; 		--  Utilises pour char -> tab_bits
   begin
      Bit_Cour := 1;

      while Bit_Cour < Huff_Enc'Last loop
	 Caractere_Entree := Character'Input(EAcces);

	 -- Character to TabBits
	 Tmp := Character'Pos(Caractere_Entree);
	 for I in 1..8 loop
	    R := Tmp mod 2;
	    Huff_Enc(Bit_Cour + 8 - I) := R;
	    Tmp := Tmp / 2;
	 end loop;

	 Bit_Cour := Bit_Cour + 8;
      end loop;
   end Input_Arbre_Enc;
   
   --  Retourne la taille de l'encodage de l'arbre a partir du nb_feuilles
   --  selon la formule taille = 10f - 1 bits
   --  !! ON PROCEDE A UN ARRONDI AU MULTIPLE DE 8 SUPERIEUR !!
   --  Ceci afin d'eviter des problemes lies a la technique d'ecriture
   --  dans le fichier binaire
   function Calcul_Taille_Huff(Nb_Feuilles : Natural) return Natural is
   begin
      if ((10*Nb_Feuilles)-1) mod 8 = 0 then
	 return ((10*Nb_Feuilles) - 1);
      else
	 return ((10*Nb_Feuilles) - 1)
	   + 8 - (((10*Nb_Feuilles) - 1) mod 8);
      end if;
   end Calcul_Taille_Huff;

   procedure Compression(Fichier_Entree, Fichier_Sortie: String) is
      Arbre_Huffman : Arbre;
      Frequences : Tableau_Ascii;
      Taille : Positive;
      Entree, Sortie: Ada.Streams.Stream_IO.File_Type;
      EAcces, SAcces : Stream_Access;
      Reste : Code;
      Caractere_Sortie : Character;
      D : Dico;
      Nb_Feuilles : Natural;
      Huff_Enc : Code;
      Taille_Enc : Natural;
   begin
      Lecture_Frequences(Fichier_Entree, Frequences, Taille);
      Affiche_Frequences(Frequences);
      Arbre_Huffman := Calcul_Arbre(Frequences, Nb_Feuilles);
      Affiche_Arbre(Arbre_Huffman);
      D := Calcul_Dictionnaire(Arbre_Huffman);
      Create(Sortie, Out_File, Fichier_Sortie);
      SAcces := Stream( Sortie );
      Natural'Output(Sacces, Taille);
      Natural'Output(Sacces, Nb_Feuilles);
      
      Taille_Enc := Calcul_Taille_Huff (Nb_Feuilles);
      Huff_Enc := new TabBits(1..Taille_Enc);
      Encode_Arbre(Arbre_Huffman, Huff_Enc);
      Output_Arbre_Enc (Huff_Enc, SAcces);

      Open(Entree, In_File, Fichier_Entree);
      EAcces := Stream(Entree);
      Reste := null;
      while (not End_Of_File(Entree)) or Reste /= null loop
	 Recuperation_Caractere(Reste, Entree, EAcces, Caractere_Sortie, D);
	 Character'Output(SAcces, Caractere_Sortie);
      end loop;
      Close(Entree);
      Close(Sortie);
   end Compression;

   procedure Decompression(Fichier_Entree: String; Fichier_Sortie: String) is
      Arbre_Huffman: Arbre;
      Taille, Octets_Ecrits: Natural;
      Caractere: Character;
      Entree, Sortie: Ada.Streams.Stream_IO.File_Type;
      Reste : Code;
      EAcces, SAcces : Stream_Access;

      function Lecture_Octet_Compresse return Character is
      begin
	 return Character'Input(EAcces);
      end;

      procedure Caractere_Suivant is new Decodage_Code(Lecture_Octet_Compresse);
      Bit_Cour : Natural := 0;
      Nb_Feuilles : Natural;
      Taille_Enc : Natural;
   begin
      Open(Entree, In_File, Fichier_Entree);
      EAcces := Stream( Entree );
      Taille := Natural'Input(EAcces);
      Nb_Feuilles := Natural'Input(EAcces);

      Taille_Enc := Calcul_Taille_Huff (Nb_Feuilles);
      declare
	 Huff_Enc : Code := new TabBits(1..Taille_Enc);
      begin
	 Input_Arbre_Enc(Huff_Enc, EAcces);
	 Arbre_Huffman := Decode_Arbre (Huff_Enc);
      end;

      Create(Sortie, Out_File, Fichier_Sortie);
      SAcces := Stream (Sortie);
      Reste := new TabBits(1..8);
      Octets_Ecrits := 0;
      while(Octets_Ecrits < Taille) loop
	 Caractere_Suivant(Reste, Arbre_Huffman, Caractere, Bit_Cour);
	 Octets_Ecrits := Octets_Ecrits + 1;
	 Character'Output(SAcces, Caractere);
      end loop;
      Liberer (Reste);
      Close(Entree);
      Close(Sortie);
      if (Octets_Ecrits /= Taille) then
	 Put(Standard_Error, "Fichier Invalide");
	 raise Fichier_Invalide;
      end if;
   end Decompression;

begin

   if (Argument_Count /= 3) then
      Put_Line("utilisation:");
      Put_Line("  compression : ./huffman -c fichier.txt fichier.txt.huff");
      Put_Line("  decompression : ./huffman -d fichier.txt.huff fichier.txt");
      Set_Exit_Status(Failure);
      return;
   end if;

   if (Argument(1) = "-c") then
      Compression(Argument(2), Argument(3));
   else
      Decompression(Argument(2), Argument(3));
   end if;

   Set_Exit_Status(Success);

end Huffman;
