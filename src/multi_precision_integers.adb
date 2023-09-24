-----------------------------------------------------------------------------
--  File: mupreint.adb; see specification (mupreint.ads)
-----------------------------------------------------------------------------
-- 15-Feb-2002: "zero" bugs fixed by Duncan Sands


-- To-do/bug symbol: !!!

-- with Text_IO; use Text_IO;  --   <--- still here for debugging purposes

package body Multi_precision_integers is
--  package IIO is new Integer_IO(integer); use IIO; --   <--- for debugging

  DEBUG: constant Boolean:= True;

  Internal_error: exception;
  Not_done: exception;

  type compar is (smaller, equal, greater);

  function Min(a,b:index_int) return index_int is
    begin if a<b then return a; else return b; end if; end;

  function Max(a,b:index_int) return index_int is
    begin if a>b then return a; else return b; end if; end;

  procedure Test( m: multi_int; test_last: boolean:= true ) is
    last_nz: index_int:= 0;
    Negative_block, Overflown_block,
    Last_index_has_zero,
    Field_last_outside_range, Field_last_is_negative: exception;
    begin
      if m.zero then return; end if; -- 0, nothing to test
      if m.last_used > m.n then raise Field_last_outside_range; end if;
      if m.last_used <   0 then raise Field_last_is_negative; end if;
      for i in 0 .. m.last_used loop
        if m.blk(i) < 0 then
          raise Negative_block;
        end if;
        if not (m.blk(i) in block) then
          raise Overflown_block;
        end if;
        if m.blk(i) /= 0 then
          last_nz:= i;
        end if;
      end loop;
      if test_last and then 0 < last_nz and then last_nz < m.last_used then
        raise Last_index_has_zero;
      end if; 
    end Test;

  -- Another names (because of randomness in DEC Ada trace-back's line numbers)
  procedure Testarossa( m: multi_int ) is begin Test(m); end;
  procedure Testaverde( m: multi_int ) is begin Test(m); end;
  procedure Testazzuro( m: multi_int ) is begin Test(m); end;

  procedure Reduce_last_nonzero( n: in out multi_int ) is
    old_last: index_int:= n.last_used;
  begin
    if DEBUG then Test(n, test_last=> false); end if;

    if n.zero then   -- We avoid de-zeroing accidentally
      return;        -- and returning a false non-zero with rubbish :-)
    end if;

    n.zero:= True;
    n.last_used:= 0;
    for i in 0 .. old_last loop
      if n.blk(i) /= 0 then
        n.zero:= False;
        n.last_used:= i;      -- NB: can be 0
      end if;
    end loop;
  end Reduce_last_nonzero;

  function Compare_absolute (i1,i2: multi_int) return compar is
    l1, l2: index_int;
  begin
    -- On ne compare que ABS(i1) et ABS(i2)
    l1:= i1.last_used;
    l2:= i2.last_used;
    if l1 > l2 then         -- i1 a plus de blocs non nuls
      return greater;
    elsif l1 < l2 then      -- i1 a moins de blocs non nuls
      return smaller;
    else                       -- i1 et i2 ont le meme nb de blocs
      for i in reverse 0 .. l1 loop -- on parcourt du + signifiant au -
        if    i1.blk(i) > i2.blk(i) then -- <<chiffre>> de i1 plus grand
          return greater;
        elsif i1.blk(i) < i2.blk(i) then -- <<chiffre>> de i1 plus petit
          return smaller;
        end if;
        -- M\^emes chiffres -> au suivant!
      end loop;
      -- Bon, les 2 nombres sont egaux!
      return equal;
    end if;
  end Compare_absolute;

----- Informations, conversions

  function Multi(small: basic_int) return multi_int is
    abss: basic_int:= ABS(small);
    reste: basic_int;
    negs: boolean:= small < 0;
    Conversion_overflow : exception;
    
  begin

    if abss<= maxblock then
      return ( n=>         0,          -- 1 bloc suffit
               blk=>      (0=> abss),  -- le bloc contient le nombre
               neg=>       negs,
               zero=>      small = 0,
               last_used=> 0
             );
    else
      reste:= abss  /  cardblock;
      if reste <= maxblock then
        return ( n=>         1,            -- il faut 2 blocs
                 blk=>      (0=> abss MOD cardblock,   -- bloc 0
                             1=> reste),               -- bloc 1
                 neg=>       negs,
                 zero=>      false,
                 last_used=> 1
               );
      else
        if reste / cardblock > maxblock then
           Raise Conversion_overflow;
        end if;

        return ( n=>         2,  -- il faut 3 blocs (e.g. 31 bits 15+15+1)
                 blk=>      (0=> abss MOD cardblock,   -- bloc 0
                             1=> reste MOD cardblock,  -- bloc 1
                             2=> reste  /  cardblock), -- bloc 2
                 neg=>       negs,
                 zero=>      false,
                 last_used=> 2
               );
      end if;
    end if;
  end;

  -- Convert multi_int to basic_int (when possible, else: Cannot_fit raised)
  function Basic(large:multi_int) return basic_int is
  begin
    if large.zero then return 0; end if; -- <- 17-Feb-2002
    if large.last_used > 0 then raise Cannot_fit; end if;
    return large.blk(0);
  end;

  -- 14-Feb-2002: "zero" bug fixed by Duncan Sands
  procedure Fill(what: out multi_int; with_smaller:multi_int) is
    l: constant index_int:= with_smaller.last_used;
  begin
    if DEBUG then Test(with_smaller); end if;
    what.zero:= with_smaller.zero;

    if with_smaller.zero then
       return;
    end if;

    if what.n < l then
       raise Array_too_small;   -- contenant trop petit
    end if;

    for i in 0 .. l loop -- copie
       what.blk (i) := with_smaller.blk(i);
    end loop;

    what.neg:=  with_smaller.neg;
    what.last_used:= l;
  end Fill;

---------------------------
----- Unary operators -----
---------------------------

  function "+" (i: multi_int) return multi_int is begin return i; end;

  procedure Opp(i: in out multi_int) is
  begin
    i.neg:= NOT i.neg; -- -0 possible
  end Opp;

  function "-" (i: multi_int) return multi_int is 
    res: multi_int(i.n):= i; -- copy + stack :-(
  begin
    Opp(res);
    return res;
  end "-";

  procedure Abso(i: in out multi_int) is
  begin
    i.neg:= False;
  end Abso;

  function "Abs" (i: multi_int) return multi_int is
    abs_i: multi_int(i.n):= i; -- copy + stack :-(
  begin
    if DEBUG then Test(i); end if;
    abs_i.neg:= False;
    return abs_i;
  end "Abs";

  function Sign(i: multi_int) return basic_int is
  begin
    if    i.zero then return  0;
    elsif i.neg  then return -1;
    else              return +1;
    end if;
  end Sign;
  
  function Even(i: multi_int) return boolean is
  begin
    return      i.zero  or else  i.blk(0) MOD 2 = 0;
  end Even;

  function Odd (i: multi_int) return boolean is
  begin
    return (NOT i.zero) and then i.blk(0) MOD 2 = 1;
  end Odd;

----------------------------
----- Binary operators -----
----------------------------

  -- Internal algorithm to add two numbers AS POSITIVE ( > 0 ) !

  procedure Add_absolute(i1,i2: in multi_int; i3: out multi_int) is
    l1: constant index_int:= i1.last_used;
    l2: constant index_int:= i2.last_used;
    min_ind: constant index_int:= Min( l1, l2 );
    max_ind: constant index_int:= Max( l1, l2 );
    retenue_finale, s: basic_int:= 0;

  begin
    if DEBUG then Test(i1); Test(i2); end if;

    if max_ind > i3.n then raise Result_undersized; end if; -- 17-Feb-2002
    
    -- (1) On additionne sur le <<support commun>>
    for ind in 0 .. min_ind loop   
      s:= i1.blk(ind) + i2.blk(ind) + 
              s / cardblock;                            --  (retenue)
      i3.blk(ind):=  s MOD cardblock;
    end loop;

    -- (2) On poursuit au besoin si i1 a plus de blocs...
    if l1 > min_ind then
      for ind in min_ind+1 .. max_ind loop
        s:= i1.blk(ind) +
                s / cardblock;                          --  (retenue)
        i3.blk(ind):=  s MOD cardblock;
      end loop;
    -- ... ou bien si i2 en a plus.
    elsif l2 > min_ind then
      for ind in min_ind+1 .. max_ind loop
        s:= i2.blk(ind) +
                s / cardblock;                          --  (retenue)
        i3.blk(ind):=  s MOD cardblock;
      end loop;
    end if;

    -- (3) Il peut rester une retenue

    retenue_finale:= s / cardblock;
    if retenue_finale /= 0 then
      if max_ind+1 > i3.n then raise Result_undersized; end if; -- 17-Feb-2002
      i3.blk(max_ind+1):= retenue_finale;
      i3.last_used:= max_ind+1;
    else
      i3.last_used:= max_ind;
    end if;

    -- (4) i3 = i1+i2 > 0
    i3.neg:= False;
    i3.zero:= False;

  end Add_absolute;

  -- Internal algorithm to subtract two numbers AS POSITIVE ( > 0 ) !

  procedure Sub_absolute(i1,i2: in multi_int; i3: in out multi_int;
                         sgn: out boolean) is
    l1: constant index_int:= i1.last_used;
    l2: constant index_int:= i2.last_used;
    max_ind: constant index_int:= Max( l1, l2 );
    ai, bi, s: basic_int;

  begin
    if DEBUG then Test(i1); Test(i2); end if;

    if max_ind > i3.n then raise Result_undersized; end if; -- 17-Feb-2002

    i3.last_used:= 0;
    i3.zero:= true;
    s:= 0;

    -- (1) Soustraction avec retenue
    for ind in 0 .. max_ind loop
      if ind <= l1 then ai:= i1.blk(ind);     else ai:= 0; end if;
      if ind <= l2 then bi:= i2.blk(ind) + s; else bi:= s; end if;

      if ai < bi then
        ai:= ai + cardblock;
        s:= 1;
      else
        s:= 0;
      end if;
      
      i3.blk(ind):= ai-bi;
      if ai-bi /= 0 then    -- au passage, on corrige .last_used et .zero
        i3.last_used:= ind;
        i3.zero:= False;
      end if;
    end loop;
    
    -- (2) Traitement de la derni\`ere retenue
    if s = 0 then
      i3.neg := False;
      sgn    := False;
    else
      i3.neg := True;
      sgn    := True;
      i3.last_used:= 0;
      s:= 1; -- on fait "9-chaque chiffre" et on ajoute 1 au tout (s=retenue)
      for i in 0 .. max_ind loop
        s:= maxblock - i3.blk(i) + s;
        i3.blk(i):= s MOD cardblock;
        if i3.blk(i) /= 0 then
          i3.last_used:= i;
        end if;
        s:= s / cardblock;
      end loop;
    end if;

  end Sub_absolute;

  procedure Add(i1,i2: in multi_int; i3: in out multi_int) is
    sgn: Boolean;
  begin
    -- (1) Les cas o\`u i1 ou i2 = 0
    if    i1.zero and i2.zero then i3.zero:= True;
    elsif i1.zero then Fill( i3, i2 );
    elsif i2.zero then Fill( i3, i1 );

    -- (2) Maintenant: i1 /= 0 et i2 /= 0; on regarde les signes

    -- (2.1) Facile: i1 et i2 de m\^eme signe
    elsif i1.neg = i2.neg then
      Add_absolute( i1,i2, i3 ); -- On fait comme si i1>0 et i2>0
      i3.neg:= i1.neg;           -- et on met le bon signe

    -- (2.2) i1 < 0, i2 > 0, donc i3 = i2 - abs(i1)
    elsif i1.neg and not i2.neg then
      Sub_absolute( i2,i1, i3, sgn);

    -- (2.3) i1 > 0, i2 < 0, donc i3 = i1 - abs(i2)
    elsif i2.neg and not i1.neg then
      Sub_absolute( i1,i2, i3, sgn );
    end if;

  end Add;

  function "+" (i1,i2: multi_int) return multi_int is
    somme: multi_int( Max(i1.n, i2.n) + 1 );
  begin
    Add( i1,i2, somme );
    return somme;
  end "+";

  procedure Sub(i1,i2: in multi_int; i3: in out multi_int) is
    sgn: Boolean;
  begin
    -- (1) Les cas o\`u i1 ou i2 = 0
    if    i1.zero and i2.zero then i3.zero:= True;
    elsif i1.zero then Fill( i3, i2 ); i3.neg:= NOT i2.neg;
    elsif i2.zero then Fill( i3, i1 );

    -- (2) Maintenant: i1 /= 0 et i2 /= 0; on regarde les signes

    -- (2.1) Facile: i1 et i2 de m\^eme signe
    elsif i1.neg = i2.neg then
      Sub_absolute( i1,i2, i3, sgn ); -- On fait comme si i1>0 et i2>0
      if i1.neg then                  -- et on met le bon signe
        i3.neg:= NOT sgn;
      else
        i3.neg:= sgn;
      end if;

    -- (2.2) i1 < 0, i2 > 0, donc i3 = i1-i2 = - (abs(i1) + abs(i2))
    elsif i1.neg and not i2.neg then
      Add_absolute( i1,i2, i3 );
      i3.neg:= True;

    -- (2.3) i1 > 0, i2 < 0, donc i3 = i1-i2 = i1 + (-i2) = i1 + abs(i2)
    elsif i2.neg and not i1.neg then
      Add_absolute( i1,i2, i3 );
    end if;

  end Sub;

  function "-" (i1,i2: multi_int) return multi_int is
    diff: multi_int( Max(i1.n, i2.n) + 1); -- +1: retenue possible (add_abs.)
  begin
    Sub( i1,i2, diff );
    return diff;
  end "-";

  function "+" (i1: multi_int; i2: basic_int) return multi_int is
  begin return i1 + Multi(i2); end;

  function "+" (i1: basic_int; i2: multi_int) return multi_int is
  begin return Multi(i1) + i2; end;

  function "-" (i1: multi_int; i2: basic_int) return multi_int is
  begin return i1 - Multi(i2); end;

  function "-" (i1: basic_int; i2: multi_int) return multi_int is
  begin return Multi(i1) - i2; end;
    
  procedure Multiply(i1,i2: in multi_int; i3: in out multi_int) is
    l1: index_int:= i1.last_used;
    l2: index_int:= i2.last_used;
    d,s : long_basic_int;
    k   : index_int;
    res : block_array( i3.blk'range );
    -- res: buffer to avoid problems with Multiply(i,j,i) or Multiply(j,i,i)
  begin
    if DEBUG then Test(i1); Test(i2); end if;
    if l1+l2+2 > i3.n then raise Result_undersized; end if; -- 17-Feb-2002

    if i1.zero or i2.zero then
      i3.zero:= True;
      Return;
    end if;

    for k in res'range loop res(k):= 0; end loop;
    i3.zero:= False;
    i3.last_used:= i3.n;

    for j in 0..l1 loop
      d:= 0;
      k:= j;
      for i in 0..(l2+1) loop
        if i <= l2 then
          d:= d / cardblock +
              long_basic_int(i1.blk(j)) * long_basic_int(i2.blk(i));
        else
          d:= d / cardblock;
        end if;
        s:= long_basic_int(res(k)) + d MOD cardblock;
        res(k):= block(s MOD cardblock); -- somme
        res(k+1):= res(k+1) + block(s / cardblock); -- retenue
        k:= k + 1;
      end loop;
    end loop;

    i3.blk:= res;
    Reduce_last_nonzero( i3 );

    i3.neg:= i1.neg /= i2.neg;

  end Multiply;

  function "*" (i1,i2: multi_int) return multi_int is
  begin
    if i1.zero or i2.zero then
      return Multi(0);
    else
      declare
        prod: Multi_int( i1.last_used + i2.last_used + 2 );
      begin
        Multiply( i1,i2, prod );
        Return prod;
      end;
    end if;
  end "*";

  function "*" (i1: multi_int; i2: basic_int) return multi_int is
  begin return i1 * Multi(i2); end;

  function "*" (i1: basic_int; i2: multi_int) return multi_int is
  begin return Multi(i1) * i2; end;

----- Begin of DIVISION part -----

  -- Interne: Division et reste en 1 coup

  procedure Div_Rem(a,b: long_basic_int; q,r: in out long_basic_int) is
    Conflict_with_REM: exception;
  begin
    q:= a / b;
    r:= a - b*q;
    if DEBUG and then r /= (a rem b) then
      Raise Conflict_with_REM;
    end if;
  end Div_Rem;

  procedure Divide_absolute_normalized ( u: in out multi_int;
                                         v: in     multi_int;
                                         q: in out multi_int  ) is
    qi: index_int      := u.last_used - v.last_used - 1; -- was: q.n; D.S. Feb-2002
    v1: long_basic_int := long_basic_int(v.blk(v.last_used  ));
    v2: long_basic_int := long_basic_int(v.blk(v.last_used-1));

    udigits : block_array renames u.blk;
    vdigits : block_array renames v.blk;

    vlast     : index_int      := v.last_used;
    v1L       : long_basic_int := long_basic_int(v1);
    guess     : long_basic_int ;
    comparand : long_basic_int ;

    function Divide_subtract ( ustart: index_int ) return block is
      ui    : index_int      := ustart;
      carry : long_basic_int := 0;

    begin
      if guess = 0 then
        return 0;
      end if;

      -- On soustrait (le chiffre du quotient) * diviseur au dividende

      for vi in 0 .. vlast loop
        declare
          prod: long_basic_int:= long_basic_int(vdigits(vi)) * guess + carry;
          bpro: block         := block(prod MOD cardblock);
          diff: basic_int     := udigits(ui) - bpro;
        begin
          if diff < 0 then
            udigits(ui) := diff + cardblock;
            carry := (prod / cardblock) + 1;
          else
            udigits(ui) := diff;
            carry := (prod / cardblock);
          end if;
          ui:= ui + 1;
        end;
      end loop;
  
      if carry = 0 then
        return  block(guess MOD cardblock);
      end if;
  
      declare
        diff: basic_int := udigits(ui) - basic_int(carry MOD cardblock);
      begin
          if diff < 0 then
            udigits(ui) := diff + cardblock; -- carry generated
          else
            udigits(ui) := diff;
            return block(guess MOD cardblock);
          end if;
      end;

      -- Carry was generated
      declare
        icarry: basic_int := 0;
      begin
        ui := ustart;
        for vi in 0 .. vlast loop
          declare
            sum: basic_int := vdigits(vi) + udigits(ui) + icarry;
          begin
            udigits(ui) := sum MOD cardblock;
            ui:= ui + 1;
            icarry := sum / cardblock;
          end;
        end loop;

        if icarry = 1 then
          udigits(ui) := (udigits(ui)+1) MOD cardblock;
        end if;
      end;

      return block( (guess-1) MOD cardblock );

    end Divide_subtract;
    
  begin -- Divide_absolute_normalized
  
    -- In this algorithm, we are using q's contents although,
    -- for a while, q.zero = True
  
    for i in q.blk'range loop q.blk(i):= 0; end loop;
    q.last_used:= qi; -- was: q.n; D.S. Feb-2002
    q.zero:= True;

    for j in reverse vlast+1 .. u.last_used loop
      declare
        uj : long_basic_int := long_basic_int(udigits(j));
        uj1: long_basic_int := long_basic_int(udigits(j-1));
        uj2: long_basic_int := long_basic_int(udigits(j-2));
        ujL: long_basic_int;
        rmL: long_basic_int;
      begin
--        if uj = v1 then

-- Code BigInt suspect (resultat faux)
--          guess := cardblock-1;
--          comparand := uj1 * cardblock + uj2 + v1;

-- Code du cas general, adapte au fait que uj=v1=v1L
--          -- ujL / uj = cardblock, donc...
--          guess := cardblock;
--          -- ujL rem uj = uj1
--          comparand := (uj1 * cardblock) + uj2;

--        else -- cas general

          ujL := uj * cardblock + uj1;
          Div_Rem( ujL, v1L, guess, rmL );          
          comparand := (rmL * cardblock) + uj2;
--        end if;
        
        while comparand < v2 * guess loop
          guess:= guess - 1;
          comparand:= comparand + v1L * cardblock;
          exit when comparand > cardblock * cardblock;
        end loop;
        
        q.blk(qi) := Divide_subtract( j - vlast - 1 );

        if DEBUG and then NOT (q.blk(qi) in block) then
          raise Internal_error;
        end if;

        if q.zero and then q.blk(qi) /= 0 then -- n'arrive que 0 ou 1 fois
          q.zero:= false;
          q.last_used:= qi;
        end if;

        qi:= qi - 1;
      end;
      
    end loop; -- j

    if DEBUG then Test(q); end if;

    end Divide_absolute_normalized;

  -- Calculate u/v

  procedure Divide_absolute ( u,v: in     multi_int;
                              q,r: in out multi_int  ) is
    shift: integer:= 0;
    v1: block:= v.blk(v.last_used);
    v_zero, v1_zero: exception;
    u_work: multi_int(u.last_used+2);

    procedure Normalization ( source: in     multi_int;
                              target: in out multi_int ) is
      carry: integer:= 0;
      tl: constant index_int:= target.last_used;
      to_left_factor  : constant integer:= 2 ** shift;
      to_right_factor : constant integer:= cardblock / to_left_factor;
      blk:  block;
      no_room_for_carry: exception;
    begin
      for i in 0 .. source.last_used loop
        blk:= source.blk(i);
        target.blk(i) := ((blk MOD to_right_factor) * to_left_factor) + carry;
        carry         :=   blk  /  to_right_factor;
      end loop;
      -- tabuler les MOD et / pour faire AND et shift !!!
      if source.last_used < tl then
        target.blk(source.last_used+1):= carry;
      end if;
      for i in source.last_used+2 .. tl  loop
        target.blk(i):= 0;
      end loop;
    end Normalization;

    procedure Unnormalization ( m: in out multi_int) is
      carry: integer:= 0;
      to_right_factor : constant integer:= 2 ** shift;
      to_left_factor  : constant integer:= cardblock / to_right_factor;
      blk:  block;
    begin
      for i in reverse 0 .. m.last_used loop
        blk:= m.blk(i);
        m.blk(i) := (blk  /  to_right_factor) + carry;
        carry    := (blk MOD to_right_factor) * to_left_factor;
      end loop;
    end Unnormalization;

  begin -- Divide_absolute (multi u / multi v)

    if DEBUG then
      if v.zero then raise v_zero; end if;
      if v1=0 then raise v1_zero; end if;
    end if;

    -- Calculate shift needed to normalize
    u_work.last_used:= u_work.n;
    u_work.zero:= false;
    while v1 < cardblock/2 loop
      shift:= shift + 1;
      v1:= v1 * 2;
    end loop;
    if shift = 0 then                  -- no shift needed
      u_work.blk( 0 .. u.last_used ):= u.blk( 0 .. u.last_used );
      u_work.blk( u.last_used+1 .. u_work.last_used):= (0,0);
      Divide_absolute_normalized( u_work,v, q );
    else
      declare                          -- shift needed
        v_work: multi_int(v.last_used);
      begin
        v_work.last_used:= v_work.n;
        Normalization( u, u_work );
        Normalization( v, v_work );
        Reduce_last_nonzero( v_work );
        Divide_absolute_normalized( u_work,v_work, q );
        Unnormalization( u_work );
      end;
    end if;
    u_work.neg:= false; -- check friendly
    q.neg:= false; -- check friendly
    Reduce_last_nonzero( u_work );
    Fill( r, u_work );

  end Divide_absolute;

  procedure Divide_absolute_big_small ( u:   in     multi_int;
                                        v:   in     basic_int;
                                        q:   in out multi_int;
                                        r:   in out basic_int ) is
    lr: long_basic_int:= 0;
    ln: long_basic_int;
    lv: long_basic_int:= long_basic_int(v);
    Quotient_constraint_error: exception;
    last_u_nz:  constant index_int:= u.last_used;
    u_zero: constant boolean:= u.zero;
    -- in case u and q are the same variables
  begin
    if q.n < last_u_nz then raise Quotient_constraint_error; end if;
    q.last_used:= 0;
    q.neg:= false;
    q.zero:= True;
    if u_zero then
      r:= 0;
    else
      for i in reverse 0 .. last_u_nz loop
        ln:= long_basic_int(u.blk(i)) + lr * cardblock;
        lr:=             ln MOD lv;
        q.blk(i):= block(ln  /  lv);
        if q.zero and then q.blk(i)/= 0 then 
          q.last_used:= i;
          q.zero:= False;
        end if;
      end loop;
      r:= basic_int(lr);
    end if;
  end Divide_absolute_big_small;

  procedure Solve_signs_for_Div_Rem (i1n,i2n: in boolean; qn,rn: out boolean) is
  begin
    -- Invariant: i1= i2*q+r   on cherche (pos) = (pos)*(pos)+(pos)

    if i1n and i2n then        -- i1<0;  i2<0  (-i1) = (-i2) *  q  + (-r)
      qn:= False; -- Quotient > 0
--      rn:= True;  -- Reste    < 0
    elsif i1n then             -- i1<0;  i2>0  (-i1) =   i2  *(-q) + (-r)
      qn:= True;  -- Quotient < 0
--      rn:= True;  -- Reste    < 0
    elsif i2n then             -- i1>0;  i2<0    i1  = (-i2) *(-q) +   r
      qn:= True;  -- Quotient < 0
--      rn:= False; -- Reste    > 0
    else                       -- i1>0;  i2>0    i1  =   i2  *  q  +   r
      qn:= False; -- Quotient > 0
--      rn:= False; -- Reste    > 0
    end if;
    -- on observe que... "(A rem B) has the sign of A " ARM 4.5.5
    -- en effet on peut mettre:
    rn:= i1n;
  end Solve_signs_for_Div_Rem;

  procedure Div_Rem (i1: in     multi_int; i2: in     basic_int;
                     q : in out multi_int;  r: in out basic_int) is
    i1_neg: constant boolean:= i1.neg;
    -- in case i1 and q are the same variables
    rneg: boolean;
  begin
    if DEBUG then Test(i1); end if;
    if i2=0 then Raise Division_by_zero; end if;

    if i1.zero then -- 15-Feb-2002: 0/i2
      q.zero:= True;
      r:= 0;
      return;
    end if;

    Divide_absolute_big_small( i1, Abs(i2), q,r );

    Solve_signs_for_Div_Rem( i1_neg,i2<0, q.neg, rneg );
    if rneg then r:= -r; end if;

  end Div_Rem;

  procedure Div_Rem (i1,i2: in multi_int; q,r: in out multi_int) is
    Remainder_constraint_error: exception;
    l1: constant index_int:= i1.last_used;
    l2: constant index_int:= i2.last_used;
  begin
    if DEBUG then Test(i1); Test(i2); end if;
    if i2.zero then Raise Division_by_zero; end if;
    
    if i1.zero then -- 15-Feb-2002: 0/i2
      q.zero:= True;
      r.zero:= True;
      return;
    end if;

    if q.n < l1 - l2 or r.n < Max( l1, l2 ) then -- 17-Feb-2002
      raise Result_undersized;
    end if; 

    if l2 = 0 then
      if l1 = 0 then      -- On a affaire a une ridicule division d'entiers
         q.blk(0):= i1.blk(0) / i2.blk(0);
         r.blk(0):= i1.blk(0) - i2.blk(0)*q.blk(0);

         q.blk(0):= Abs(q.blk(0)); -- signes mis a la sortie...
         q.zero:= q.blk(0) = 0;
         q.last_used:= 0;
      else                -- multi / entier
         Div_Rem ( i1, i2.blk(0), q, r.blk(0) );
      end if;
      r.blk(0):= Abs(r.blk(0)); -- signes mis a la sortie...
      r.zero:= r.blk(0) = 0;
      r.last_used:= 0;

    else  -- multi / multi

      case Compare_absolute(i2 , i1) is

        when greater =>
          q.zero:= True;    -- q:=  0;
          q.last_used:= 0;
          q.neg:= false;

          Fill( r, i1 );  -- r:= i1, q:=0  car i1 = 0 * i2 (>i1 en v.abs) + r
          Return;

        when equal =>
          Fill( q, Multi(1) );
          Fill( r, Multi(0) );
          
        when smaller => -- cas <<normal>>: diviseur < dividende
          
          if l2 > r.n then
            Raise Remainder_constraint_error;
          end if;

          Divide_absolute( i1,i2, q,r );
          if DEBUG then Testazzuro(i1); Testaverde(i2); end if;
          Reduce_last_nonzero( r );
          if DEBUG then Test(q); Testarossa(r); end if;

      end case;
    end if;

    Solve_signs_for_Div_Rem( i1.neg,i2.neg, q.neg,r.neg );
  end Div_Rem;

  function "/" (i1,i2: multi_int) return multi_int is
    q: Multi_int( Max( 0, i1.last_used - i2.last_used + 1) );
    r: Multi_int( Max( i1.last_used, i2.last_used) + 2 );
  begin
    Div_Rem(i1,i2, q,r);
    return q;
  end "/";

  function "/" (i1: multi_int; i2: basic_int) return multi_int is
    q: multi_int(i1.last_used + 1);
    r: basic_int;
  begin
    Div_Rem(i1,i2, q,r);
    return q;
  end "/";

  function "Rem" (i1,i2: multi_int) return multi_int is
    q: multi_int(Max(0,i1.last_used - i2.last_used + 1));
    r: multi_int(Max(i1.last_used,i2.last_used) + 2);
  begin
    Div_Rem(i1,i2, q,r);
    return r;
  end "Rem";

  function "Rem" (i1: multi_int; i2: basic_int) return multi_int is
  begin return i1 Rem Multi(i2); end "Rem";

  function "Rem" (i1: multi_int; i2: basic_int) return basic_int is
    q: multi_int(i1.last_used + 1);
    r: basic_int;
  begin
    Div_Rem(i1,i2, q,r);
    return r;
  end "Rem";

  function "Mod" (i1,i2: multi_int) return multi_int is
    q: multi_int(Max(0,i1.last_used - i2.last_used + 1));
    r: multi_int(Max(i1.last_used,i2.last_used) + 2);
  begin
    -- Ada RM, 4.5.5 Multiplying Operators
    -- (8)
    -- The signed integer modulus operator is defined such that
    -- the result of A mod B has the sign of B and an absolute value
    -- less than the absolute value of B; in addition, for some signed
    -- integer value N, this result satisfies the relation:
    -- (9) A = B*N + (A mod B)

    Div_Rem(i1,i2, q,r);
    if r.zero or else i2.neg = r.neg then  --  (A rem B) est nul ou
      return r;     -- a le meme signe que B, donc (A mod B) = (A rem B)
    else  -- signe opposes
      return i2+r;  -- alors (B + (A rem B)) est le bon candidat
    end if;
  end "Mod";

  function "Mod" (i1: multi_int; i2: basic_int) return multi_int is
  begin return i1 Mod Multi(i2); end "Mod";

  function "Mod" (i1: multi_int; i2: basic_int) return basic_int is
    r: basic_int:= i1 Rem i2;
  begin
    if r=0 or else (i2<0) = (r<0) then  --  (A rem B) est nul ou
      return r;     -- a le meme signe que B, donc (A mod B) = (A rem B)
    else  -- signe opposes
      return i2+r;  -- alors (B + (A rem B)) est le bon candidat
    end if;
  end "Mod";

----- End of DIVISION part ------

----- Begin of POWER part -------

  procedure Power (i: multi_int; n: Natural; ipn: out multi_int) is
    max_ipn_last: Index_int; -- 17-Feb-2002
  begin
    if i.zero then
      if n=0 then
        raise Zero_power_zero;
      else
        Fill( ipn, Multi(0) ); -- the 0**n = 0 case (17-Feb-2002)
        return;
      end if;
    end if;
    
    max_ipn_last:= ((1+i.last_used) * Index_int(n)-1)+2;
    if ipn.n < max_ipn_last then
      raise Result_undersized;
    end if;

    case n is
      when 0 => Fill( ipn, Multi(1) ); -- the i**0 = 1 case
      when 1 => Fill( ipn, i);         -- the i**1 = i case
      when others =>
        declare
          nn: natural:= n-1;
          i0, ii: Multi_int( max_ipn_last );
        begin
          Fill(i0, i);
          Fill(ii, i0 );

          while nn > 0 loop
            if nn MOD 2 = 0 then -- x^(2 c) = (x^2) ^c
              Mult(i0,i0, i0);
              nn:= nn / 2;
            else
              Mult(i0,ii, ii);
              nn:= nn - 1;
            end if;
          end loop;
          Fill( ipn, ii);
        end;
    end case;
  end Power;

  function "**" (i: multi_int; n: Natural) return multi_int is
    ipn: Multi_int( (1+i.last_used) * index_int(n)+2 );
  begin
    Power(i,n,ipn);
    return ipn;
  end "**";

----- End of POWER part ---------

----- Comparisons

  function Equal (i1,i2: multi_int) return boolean is
  begin
    if i1.zero and then i2.zero then
      return True;
    end if;

    if i1.zero = i2.zero and then
       i1.neg  = i2.neg  and then
       i1.last_used = i2.last_used then
      Return i1.blk(0..i1.last_used) = i2.blk(0..i2.last_used);
    else
      Return False;
    end if;
  end Equal;

  function Equal (i1: multi_int; i2:basic_int) return boolean is
  begin
    return Equal( i1, Multi(i2) );
  end Equal;

  function ">" (i1,i2: multi_int) return Boolean is
  begin
    -- (1) Cas \'evident o\`u:         i1 <= i2
    if (i1.zero or i1.neg) and then             -- i1 <= 0 et
       (i2.zero or not i2.neg) then             -- i2 >= 0
        return False;
    end if;
    
    -- (2.1) Cas \'evident o\`u:       i1 > i2
    if ((not i1.zero) and not i1.neg) and then  -- i1 > 0 et
       (i2.zero or i2.neg) then                 -- i2 <= 0 
        return True;
    end if;

    -- (2.2) Cas \'evident o\`u:       i1 > i2
    if (i1.zero or not i1.neg) and then         -- i1 >= 0 et
       ((not i2.zero) and i2.neg) then          -- i2 < 0 
        return True;
    end if;

    -- Cas faciles resolus:
    -- i1 > i2  -  0  +
    -------------------
    --  -       #  F  F
    --  0       T  F  F
    --  +       T  T  #
    
    -- On a les cas avec "#", o\`u i1 et i2 ont le meme signe

    if i1.neg then
      Return NOT (Compare_absolute (i1,i2) = greater);
    else
      Return     (Compare_absolute (i1,i2) = greater);
    end if;

  end ">";

  function ">" (i1: multi_int; i2:basic_int) return Boolean is
  begin
    return i1 > Multi(i2);
  end ">";

  function "<" (i1,i2: multi_int) return Boolean is
  begin return i2>i1; end;

  function "<" (i1: multi_int; i2:basic_int) return Boolean is
  begin
    return i1 < Multi(i2);
  end "<";

  function ">=" (i1,i2: multi_int) return Boolean is
  begin return not (i2>i1); end;

  function ">=" (i1: multi_int; i2:basic_int) return Boolean is
  begin
    return i1 >= Multi(i2);
  end ">=";

  function "<=" (i1,i2: multi_int) return Boolean is 
  begin return not (i1>i2); end;

  function "<=" (i1: multi_int; i2:basic_int) return Boolean is
  begin
    return i1 <= Multi(i2);
  end "<=";

begin
  if 2*bitsblock >= long_basic_int'size then
    raise constraint_error; -- long_basic_int type is too small !
  end if;
  if bitsblock >= basic_int'size then
    raise constraint_error; -- basic_int type is too small !
  end if;
end Multi_precision_integers;
