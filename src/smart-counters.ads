package Smart.Counters is

   pragma Preelaborate;

   type Unsigned is mod 2 ** 32;

   package Unsigned_Counters is new Discrete_Counters (Unsigned);

end Smart.Counters;
