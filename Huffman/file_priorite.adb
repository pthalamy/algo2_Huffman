
With Ada.Text_IO;
use Ada.Text_IO;

package body File_Priorite is
   
   --  IMPLEMENTATION D'UNE FILE DE PRIORITE MINIMALE 
   --  SOUS FORME D'UN TAS
   
   Nb_Elts : Natural; 			--  Variable globale correspondant au 
					-- nombre d'elements dans la file

   type Elt is record
      P : Priorite;
      D : Donnee;
   end record;
   
   --  Tas
   -- Pour Elt a T(I), FG a T(2I) et FD a T(2I + 1)
   type File_Interne is array (Natural range <>) of Elt;
   
   --  Echange les Elt X et Y
   procedure Echange (X, Y : in out Elt) is
      Tmp : Elt := X;
   begin
      X := Y;
      Y := Tmp;
   end Echange;
   
   --  Retourne une nouvelle file de taille Taille 
   function Nouvelle_File(Taille: Positive) return File is
      F: File;
   begin
      F := new File_Interne(1..Taille);
      Nb_Elts := 0;
      return F;
   end Nouvelle_File;
   
   --  Insere un element de priorite P et de donnee D 
   --  a la fin de la file et remonte dans l'arbre pour trouver sa place
   procedure Insertion(F: in out File; P: Priorite; D: Donnee) is
      Pos, Pere : Natural; 		--  Pos : Position courante dans F
					--  Pere : Indice du Pere
   begin
      --  Insertion du nouvel element en fin de F
      Nb_Elts := Nb_Elts + 1;
      F(Nb_Elts) := (P, D);

      Pos := Nb_Elts;
      
      --  Tant que la priorite du pere est superieure a celle de l'elt ajoute
      --  On echange les places
      loop
	 --  Necessaire car 1 / 2 = Int{0} => constraint_error a Pos = 1
	 if Pos / 2 = 0 then
	    Pere := 1;
	 else
	    Pere := Pos / 2;
	 end if;

	 exit when Compare(F(Pos).P, F(Pere).P) /= INF;

	 Echange (F(Pos), F(Pere));

	 Pos := Pere;
      end loop;
   end Insertion;
   
   --  Renvoi P et D avec les attributs de l'element de priorite minimale
   --  (1er element de F)
   procedure Meilleur(F: in File;
		      P: out Priorite;
		      D: out Donnee;
		      Statut: out Boolean) is
   begin
      if Nb_Elts > 0 then
	 P := F(F'First).P;
	 D := F(F'First).D;

	 Statut := True;
      else
	 --  La file est vide, statut a false pour en notifier l'appelant
	 Statut := False;
      end if;
   end;
   
   --  Suppresion du premier element (min p) de F
   --  Met ensuite le dernier element de F en tete et le redescend par echange
   --  jusqu'a sa place dans l'arbre
   procedure Suppression(F: in out File) is
      I : Natural := 1; 		--  Element courant
      J : Natural;			--  Element considere pour echange
   begin
      --  On ecrase le premier elt par le dernier
      F(I) := F(Nb_Elts);
      Nb_Elts := Nb_Elts - 1;

      while I < Nb_Elts / 2 loop

      	 -- Recherche du plus petit des deux fils et affectation a J
      	 if Compare (F(2*I).P, F(2*I + 1).P) = SUP then
	    J := 2*I + 1;
         else
	    J := 2*I;
         end if;
	 
      	 -- On compare avec le dernier element, alors à la racine
      	 if Compare (F(J).P, F(I).P) = INF then
	    --  Echange si prio de elt considere < prio elt courant
      	    Echange (F(J), F(I));
      	 end if;

      	 I := I + 1;
      end loop;
   end;

end;
