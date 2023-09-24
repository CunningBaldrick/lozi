-----------------------------------------------------------------------------
--  File: muprinio.adb; see specification (muprinio.ads)
-----------------------------------------------------------------------------

package body Multi_precision_integers_IO is

  table: constant array(basic_int'(0)..15) of character:= 
         ('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F');

   -- 15-Feb-2002: Bugfix case i=0. Spotted by Duncan Sands

  function Chiffres_i_non_nul(i: multi_int; base: number_base:= 10) return natural is
    nombre: multi_int(i.last_used);
    la_base    : constant basic_int :=  basic_int(base);
    nchiffres: natural:= 1;

    procedure Comptage_rapide( C: positive ) is
      test  : multi_int(i.n);
      base_puiss_C: constant multi_int:= Multi( basic_int(base) ) ** C;
    begin
      loop
        Fill(test, nombre / base_puiss_C );	
        exit when test.zero;
        -- quotient non nul, donc on a au moins C chiffres
        Fill(nombre, test);
        nchiffres:= nchiffres + C;
      end loop;
    end Comptage_rapide;

  begin
    Fill(nombre, i);
    Comptage_rapide( 400 );
    Comptage_rapide( 20 );
    loop
      Fill(nombre, nombre / la_base);	
      exit when nombre.zero;
      nchiffres:= nchiffres + 1;
    end loop;
    return nchiffres;
  end Chiffres_i_non_nul;

  function Chiffres(i: multi_int; base: number_base:= 10) return natural is
  begin
    if i.zero then
      return 1;
    else
      return Chiffres_i_non_nul(i,base);
    end if;
  end Chiffres;
  
  function Str(i: multi_int; base: number_base:= 10) return String is
    res: String(1..1 + Chiffres(i,base)):= (others=> 'x');
    nombre : multi_int(i.n):= i;
    chiffre: basic_int;
    la_base: constant basic_int :=  basic_int(base);

    begin
      if nombre.zero or else not nombre.neg then
        res(1):= ' ';
      else
        res(1):= '-';
      end if;
      nombre.neg:= false;

      -- maintenant nombre et base sont >=0, MOD=REM
      for k in reverse 2 .. res'last loop
        Div_Rem( nombre, la_base, nombre, chiffre );
        res(k):= table( chiffre );
        exit when nombre.zero;
      end loop;
      return res;

    end Str;


-- !!! recursion !!!

  function Val(s: String) return multi_int is
    formatting_error: exception;
    begin
      if s="" then
        return Multi(0);
      elsif s(s'first)='-' then
        return -Val(s(2..s'last));
      elsif s(s'first)='+' then
        return  Val(s(2..s'last));
      elsif s(s'last) in '0'..'9' then
        return basic_int'value(s(s'last..s'last)) + 10 *  
               Val(s(s'first..s'last-1));
      else
        raise formatting_error;
      end if;
    end Val;

  procedure Put_in_blocks(File  : in File_Type;
                          Item  : in multi_int) is
    begin
      if Item.neg then put(File,'-'); else put(File,'+'); end if;
      Put(File,'{');
      if Item.n > Item.last_used then
        Put(File, index_int'image(Item.n - Item.last_used) & " unused |");
      end if;
      for k in reverse 0 .. Item.last_used loop
        Put(File, block'image(Item.blk(k)));
        if k>0 then Put(File,'|'); end if;
      end loop;
      Put(File,'}');
    end Put_in_blocks;

  procedure Put_in_blocks(Item  : in multi_int) is
    begin Put_in_blocks( Standard_Output, Item );
    end;


  procedure Get(File  : in  File_Type;
                Item  : out multi_int;
                Width : in Field := 0) is 
    begin 
      Null; -- !!!
    end;
				
  procedure Get(Item  : out multi_int;
                Width : in  Field := 0) is

    begin   Get(Standard_Input, Item, Width);   end Get;


  procedure Put(File  : in File_Type;
                Item  : in multi_int;
                Width : in Field := 0;
                Base  : in Number_Base := Default_Base) is

    begin
      if Width = 0 then       -- No padding required (default) 
        Put(File, Str(Item, Base));
      else                    -- Left padding required -> slow
        declare
          la_chaine: String(1..Width);
        begin
          Put(la_chaine, Item, Base);
          Put(File, la_chaine);
        end;
      end if;
    end Put;
				
  procedure Put(Item  : in multi_int;
                Width : in Field := 0;
                Base  : in Number_Base := Default_Base) is

    begin   Put(Standard_Output, Item, Width, Base);  end Put;
				
  procedure Get(From : in  String;
                Item : out multi_int;
                Last : out Positive) is 
    begin
      Last:= 1;
      Null; -- !!!
    end Get;

				
  procedure Put(To   : out String;
                Item : in multi_int;
                Base : in Number_Base := Default_Base) is

    nchiffres: natural:= Chiffres(Item, Base);
    blancs: String(To'range):= (others=> ' ');

    begin
      if nchiffres > To'Length then
        raise Layout_Error;
      else
        To:= blancs;
        To( To'last - nchiffres .. To'last ):= Str(Item, Base);
      end if;
    end Put;
    
end Multi_precision_integers_IO;
