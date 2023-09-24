------------------------------------------------------------------------------
--  File:            mupreint.ads
--  Description:     Multiple precision integers package
--
--  Date/version:    17-Feb-2002 / 19-Feb-2001 / 7.XII.1999
--
--                   Revised 14.XI.1999 with :
--                     a) procedures (no stack, nor copy !)
--                     b) new data structure
--
--                   First (operators only) XII.1996 - V.1997
--
--  Author:          G. de Montmollin
--                   Gautier.deMontmollin@Winterthur.ch
--
--  Thanks to:       Duncan Sands
--
--  Division algorithm adaptated from BigInt 1.0 library,
--  by Stephen Adams, that refers to
--  D. E. Knuth, the Art of computer programming
--  volume 2, "Seminumerical Algorithms"
--  section 4.3.1, "Multiple-Precision Arithmetic"
--
------------------------------------------------------------------------------

package Multi_precision_integers is

  -- Integers for values --

  subtype Basic_int is Integer;
  subtype Long_basic_int is Long_Integer; -- works even if long is not long

  bitsblock: constant:= Long_basic_int'Size / 2 - 2;
  -- -1 to avoid sign, -1 to allow carry for add/sub
  cardblock: constant:= 2 ** ( bitsblock ); -- # possible values
  -- With cardblock as power of 2, the MOD are optimized to AND
  -- and *, / are optimized to shifts.
  maxblock:  constant:= cardblock -1;
  subtype Block is Basic_int range 0 .. maxblock;

  -- Integers for indices --

  type Index_int is new integer;  
  type Block_array is array( index_int range <> ) of Basic_int;
  
  type Multi_int(n: Index_int) is record
    blk:       Block_array( 0..n ); -- the n blocks with ABSOLUTE value
    neg:       Boolean;             -- negative flag
    zero:      Boolean:=True;       -- zero flag (supercedes the other fields)
    last_used: Index_int;           -- the others blocks are supposed 0
  end record;

  -- NB the `zero' field supercedes EVERY other information (last_used, neg)

----------------------------------------------------------------------------
--   Format of type multi_int.blk: ( i_0, i_1, ..., i_k, *, ..., * )      --
--   i_0..i_k are >=0 ; others (*) are treated as 0                       --
----------------------------------------------------------------------------

----- Informations, conversions, filling

  -- Convert basic_int to multi_int
  function Multi(small: Basic_int) return Multi_int;

  -- Convert multi_int to basic_int (when possible, else: Cannot_fit raised)
  function Basic(large: Multi_int) return Basic_int;

  -- Fill an multi_int of greater array dimension with a smaller one
  procedure Fill(what:out Multi_int; with_smaller: Multi_int);

  -- Test procedure, to check a number's integrity
  procedure Test( m: Multi_int; test_last: Boolean:= true );

  ---------------------------------------------------------------------------
  -------- Arithmetic operators.                                   ----------
  -------- For speed, the "procedure" variants should be preffered ----------
  ---------------------------------------------------------------------------

  ---------------------------
  ----- Unary operators -----
  ---------------------------

  procedure Opp(i: in out multi_int);
  function "+" (i: multi_int) return multi_int;
  function "-" (i: multi_int) return multi_int;

  procedure Abso(i: in out multi_int);
  function "ABS" (i: multi_int) return multi_int;

  function Sign(i: multi_int) return basic_int;
  function Even(i: multi_int) return boolean;
  function Odd (i: multi_int) return boolean;
  
  ----------------------------
  ----- Binary operators -----
  ----------------------------

  procedure Add(i1,i2: in multi_int; i3: in out multi_int);

  function "+" (i1,i2: multi_int) return multi_int;
  function "+" (i1: multi_int; i2: basic_int) return multi_int;
  function "+" (i1: basic_int; i2: multi_int) return multi_int;

  procedure Sub     (i1,i2: in multi_int; i3: in out multi_int);
  procedure Subtract(i1,i2: in multi_int; i3: in out multi_int)
    renames Sub;

  function "-" (i1,i2: multi_int) return multi_int;
  function "-" (i1: multi_int; i2: basic_int) return multi_int;
  function "-" (i1: basic_int; i2: multi_int) return multi_int;

  procedure Multiply(i1,i2: in multi_int; i3: in out multi_int);
  procedure Mult    (i1,i2: in multi_int; i3: in out multi_int)
    renames Multiply;

  function "*" (i1,i2: multi_int) return multi_int;
  function "*" (i1: multi_int; i2: basic_int) return multi_int;
  function "*" (i1: basic_int; i2: multi_int) return multi_int;

  procedure Div_Rem (i1: in     multi_int; i2: in     basic_int;
                     q : in out multi_int;  r: in out basic_int);
  procedure Div_Rem (i1,i2: in multi_int; q,r: in out multi_int);

  function "/" (i1,i2: multi_int) return multi_int;
  function "/" (i1: multi_int; i2: basic_int) return multi_int;
  function "Rem" (i1,i2: multi_int) return multi_int;
  function "Rem" (i1: multi_int; i2: basic_int) return multi_int;
  function "Rem" (i1: multi_int; i2: basic_int) return basic_int;
  function "Mod" (i1,i2: multi_int) return multi_int;
  function "Mod" (i1: multi_int; i2: basic_int) return multi_int;
  function "Mod" (i1: multi_int; i2: basic_int) return basic_int;
  
  procedure Power (i: multi_int; n: Natural; ipn: out multi_int);
  
  function "**" (i: multi_int; n: Natural) return multi_int;

  function Equal (i1,i2: multi_int) return boolean;
  function Equal (i1: multi_int; i2:basic_int) return boolean;
  function ">" (i1,i2: multi_int) return Boolean;
  function ">" (i1: multi_int; i2:basic_int) return Boolean;
  function "<" (i1,i2: multi_int) return Boolean;
  function "<" (i1: multi_int; i2:basic_int) return Boolean;
  function ">=" (i1,i2: multi_int) return Boolean;
  function ">=" (i1: multi_int; i2:basic_int) return Boolean;
  function "<=" (i1,i2: multi_int) return Boolean; 
  function "<=" (i1: multi_int; i2:basic_int) return Boolean;

  Cannot_fit, Empty_multi_int : exception;

  Array_too_small : exception;

  Result_undersized: exception;

  Division_by_zero: exception;

  Zero_power_zero: exception;

end Multi_precision_integers;
