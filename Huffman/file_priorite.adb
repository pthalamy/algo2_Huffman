
With Ada.Text_IO;
use Ada.Text_IO;

package body File_Priorite is

   Nb_Elts : Natural;

   type Elt is record
      P : Priorite;
      D : Donnee;
   end record;

   type File_Interne is array (Natural range <>) of Elt;

   procedure Echange (X, Y : in out Elt) is
      Tmp : Elt := X;
   begin
      X := Y;
      Y := Tmp;
   end Echange;

   function Nouvelle_File(Taille: Positive) return File is
      F: File;
   begin
      F := new File_Interne(1..Taille);
      Nb_Elts := 0;
      return F;
   end Nouvelle_File;

   procedure Insertion(F: in out File; P: Priorite; D: Donnee) is
      Pos, Pere : Natural;
   begin
      Nb_Elts := Nb_Elts + 1;
      F(Nb_Elts) := (P, D);

      Pos := Nb_Elts;

      loop
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
	 Statut := False;
      end if;
   end;

   procedure Suppression(F: in out File) is
      I : Natural := 1;
      J : Natural;
   begin

      --  F(1..Nb_Elts) := F(2..Nb_Elts + 1);
      --  Nb_Elts := Nb_Elts - 1;

      F(I) := F(Nb_Elts);
      Nb_Elts := Nb_Elts - 1;

      while I < Nb_Elts loop
      	 -- Recherche du plus petit des deux fils
      	 if Compare (F(2*I).P, F(2*I + 1).P) = SUP then
	    J := 2*I + 1;
	    --  Put ("J := 2I + 1 – ");
      	 else
	    J := 2*I;
	    --  Put ("J := 2I – ");
      	 end if;

	 exit when J > Nb_Elts;

      	 -- On compare avec le dernier element, alors à la racine
      	 if Compare (F(J).P, F(I).P) = INF then
      	    --  Put_Line ("échange de F(" & Integer'Image(J)
      	    --  		& " ) et F(" & Integer'Image(I) & " )") ;
      	    Echange (F(J), F(I));
      	 end if;

      	 I := I + 1;
      end loop;
   end;

end;
