with Ada.Finalization;

package Smart is

   pragma Preelaborate;

   generic
      type Target_Type is limited private;
      with procedure Decrement_Count (
        Target  : in out Target_Type;
        Is_Zero :    out Boolean
      );
      with procedure Increment_Count (Target : in out Target_Type);

      type Initializer_Type (<>) is limited private;
      with procedure Initialize (
        Target : in out Target_Type;
        Using  : in     Initializer_Type
      ) is null; --  The count must be initialized to zero
      with procedure Finalize (Target : in out Target_Type) is null;
   package Internal_Smart_Pointers is

      type Smart_Pointer_Type is private;

      Null_Smart_Pointer : constant Smart_Pointer_Type;

      function Allocate (Using : Initializer_Type) return Smart_Pointer_Type;

      generic
         with procedure Action (Target : in out Target_Type);
      procedure Access_Contents (Smart_Pointer : Smart_Pointer_Type);
      pragma Inline (Access_Contents);

--      function Dereference (Smart_Pointer : Smart_Pointer_Type) return Target_Type;
--      pragma Inline (Dereference);

      function Is_Null (Smart_Pointer : Smart_Pointer_Type) return Boolean;
      pragma Inline (Is_Null);

   private

      type Target_Access is access Target_Type;

      type Smart_Pointer_Type is new Ada.Finalization.Controlled with record
         Pointer : Target_Access;
      end record;

      procedure Adjust (Smart_Pointer : in out Smart_Pointer_Type);
      pragma Inline (Adjust);

      procedure Finalize (Smart_Pointer : in out Smart_Pointer_Type);
      pragma Inline (Finalize);

      Null_Smart_Pointer : constant Smart_Pointer_Type := (Ada.Finalization.Controlled with null);

   end Internal_Smart_Pointers;


   generic
      type Counter_Type is limited private;
      with procedure Decrement_Count (
        Counter : in out Counter_Type;
        Is_Zero :    out Boolean
      );
      with procedure Increment_Count (Counter : in out Counter_Type);
      with procedure Zero_Count (Counter : out Counter_Type);
   package Counters_Signature is end;


   generic
      with package Counters is new Counters_Signature (<>);

      type Target_Type is limited private;
      type Initializer_Type (<>) is limited private;
      with procedure Initialize (
        Target : in out Target_Type;
        Using  : in     Initializer_Type
      ) is null;
      with procedure Finalize (Target : in out Target_Type) is null;
   package Smart_Pointers is

      type Smart_Pointer_Type is private;

      Null_Smart_Pointer : constant Smart_Pointer_Type;

      function Allocate (Using : Initializer_Type) return Smart_Pointer_Type;

      generic
         with procedure Action (Target : in out Target_Type);
      procedure Access_Contents (Smart_Pointer : Smart_Pointer_Type);
      pragma Inline (Access_Contents);

--      function Dereference (Smart_Pointer : Smart_Pointer_Type) return Target_Type;
--      pragma Inline (Dereference);

      function Is_Null (Smart_Pointer : Smart_Pointer_Type) return Boolean;
      pragma Inline (Is_Null);

   private

      type Wrapper_Type is record
         Target  : Target_Type;
         Counter : Counters.Counter_Type;
      end record;

      procedure Decrement_Count (
        Wrapper : in out Wrapper_Type;
        Is_Zero :    out Boolean
      );
      pragma Inline (Decrement_Count);

      procedure Increment_Count (Wrapper : in out Wrapper_Type);
      pragma Inline (Increment_Count);

      procedure Initialize (
        Wrapper : in out Wrapper_Type;
        Using   : in     Initializer_Type
      );
      pragma Inline (Initialize);

      procedure Finalize (Wrapper : in out Wrapper_Type);
      pragma Inline (Finalize);

      package P is new Internal_Smart_Pointers (
        Target_Type      => Wrapper_Type,
        Decrement_Count  => Decrement_Count,
        Increment_Count  => Increment_Count,
        Initializer_Type => Initializer_Type,
        Initialize       => Initialize,
        Finalize         => Finalize
      );

      type Smart_Pointer_Type is new P.Smart_Pointer_Type;

      Null_Smart_Pointer : constant Smart_Pointer_Type := Smart_Pointer_Type (P.Null_Smart_Pointer);

   end Smart_Pointers;


   generic
      with package Counters is new Counters_Signature (<>);

      type Target_Type (<>) is private;
      type Initializer_Type (<>) is limited private;
      with function Create (Using : Initializer_Type) return Target_Type;
      with procedure Finalize (Target : in out Target_Type) is null;
   package Indefinite_Smart_Pointers is

      type Smart_Pointer_Type is private;

      Null_Smart_Pointer : constant Smart_Pointer_Type;

      function Allocate (Using : Initializer_Type) return Smart_Pointer_Type;

      generic
         with procedure Action (Target : in out Target_Type);
      procedure Access_Contents (Smart_Pointer : Smart_Pointer_Type);
      pragma Inline (Access_Contents);

--      function Dereference (Smart_Pointer : Smart_Pointer_Type) return Target_Type;
--      pragma Inline (Dereference);

      function Is_Null (Smart_Pointer : Smart_Pointer_Type) return Boolean;
      pragma Inline (Is_Null);

   private

      type Target_Pointer is access Target_Type;

      type Holder_Type is new Ada.Finalization.Limited_Controlled with record
         Target : Target_Pointer;
      end record;

      overriding procedure Finalize (Holder : in out Holder_Type);

      not overriding procedure Initialize (
        Holder : in out Holder_Type;
        Using  : in     Initializer_Type
      );
      pragma Inline (Initialize);

      package S is new Smart_Pointers (
        Counters         => Counters,
        Target_Type      => Holder_Type,
        Initializer_Type => Initializer_Type,
        Initialize       => Initialize,
        Finalize         => Finalize
      );

      type Smart_Pointer_Type is new S.Smart_Pointer_Type;

      Null_Smart_Pointer : constant Smart_Pointer_Type := Smart_Pointer_Type (S.Null_Smart_Pointer);

   end Indefinite_Smart_Pointers;


   generic
      type Counter_Type is (<>);
   package Discrete_Counters is

      procedure Decrement_Count (
        Counter : in out Counter_Type;
        Is_Zero :    out Boolean
      );
      pragma Inline (Decrement_Count);

      procedure Increment_Count (Counter : in out Counter_Type);
      pragma Inline (Increment_Count);

      procedure Zero_Count (Counter : out Counter_Type);
      pragma Inline (Zero_Count);

      package Signature is new Counters_Signature (
        Counter_Type, Decrement_Count, Increment_Count, Zero_Count
      );

   end Discrete_Counters;

end Smart;
