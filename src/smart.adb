with Ada.Unchecked_Deallocation;

package body Smart is

   -----------------------
   -- Discrete_Counters --
   -----------------------

   package body Discrete_Counters is

      Zero : constant Counter_Type := Counter_Type'First;

      ---------------------
      -- Decrement_Count --
      ---------------------

      procedure Decrement_Count (
        Counter : in out Counter_Type;
        Is_Zero :    out Boolean
      ) is
      begin
         Counter := Counter_Type'Pred (Counter);
         Is_Zero := Counter = Zero;
      end Decrement_Count;


      ---------------------
      -- Increment_Count --
      ---------------------

      procedure Increment_Count (Counter : in out Counter_Type) is
      begin
         Counter := Counter_Type'Succ (Counter);
      end Increment_Count;


      ----------------
      -- Zero_Count --
      ----------------

      procedure Zero_Count (Counter : out Counter_Type) is
      begin
         Counter := Zero;
      end Zero_Count;

   end Discrete_Counters;


   -----------------------------
   -- Internal_Smart_Pointers --
   -----------------------------

   package body Internal_Smart_Pointers is

      procedure Free is new Ada.Unchecked_Deallocation (Target_Type, Target_Access);

      ------------
      -- Adjust --
      ------------

      procedure Adjust (Smart_Pointer : in out Smart_Pointer_Type) is
      begin
         if Smart_Pointer.Pointer /= null then
            Increment_Count (Smart_Pointer.Pointer.all);
         end if;
      end Adjust;


      ---------------------
      -- Access_Contents --
      ---------------------

      procedure Access_Contents (Smart_Pointer : Smart_Pointer_Type) is
--QQ         --  Increase the reference count by taking a copy of the smart pointer.
--QQ         --  This protects against the reference count going to zero during the
--QQ         --  call to Action (this could happen for example if Action writes a
--QQ         --  different smart pointer on top of Smart_Pointer, causing Smart_Pointer
--QQ         --  to be finalized - remember, Smart_Pointer is passed by reference!).
--QQ         Smart_Copy : constant Smart_Pointer_Type := Smart_Pointer;
      begin
--QQ         Action (Smart_Copy.Pointer.all);
         Action (Smart_Pointer.Pointer.all);
      end Access_Contents;


      --------------
      -- Allocate --
      --------------

      function Allocate (Using : Initializer_Type) return Smart_Pointer_Type is
         Pointer : Target_Access := new Target_Type;
      begin
         Initialize (Pointer.all, Using);
         begin
            Increment_Count (Pointer.all);
            return (Ada.Finalization.Controlled with Pointer);
         exception
            when others =>
               begin
                  Finalize (Pointer.all);
               exception
                  when others =>
                     null; -- fall through
               end;
               raise;
         end;
      exception
         when others =>
            Free (Pointer);
            raise;
      end Allocate;


--      -----------------
--      -- Dereference --
--      -----------------
--
--      function Dereference (Smart_Pointer : Smart_Pointer_Type) return Target_Type is
--      begin
--         return Smart_Pointer.Pointer.all;
--      end Dereference;


      --------------
      -- Finalize --
      --------------

      procedure Finalize (Smart_Pointer : in out Smart_Pointer_Type) is
         Is_Zero : Boolean;
      begin
         if Smart_Pointer.Pointer /= null then
            Decrement_Count (Smart_Pointer.Pointer.all, Is_Zero);
            if Is_Zero then
               begin
                  Finalize (Smart_Pointer.Pointer.all);
               exception
                  when others =>
                     Free (Smart_Pointer.Pointer);
                     raise;
               end;
               Free (Smart_Pointer.Pointer);
            end if;
         end if;
      end Finalize;


      -------------
      -- Is_Null --
      -------------

      function Is_Null (Smart_Pointer : Smart_Pointer_Type) return Boolean is
      begin
         return Smart_Pointer.Pointer = null;
      end Is_Null;

   end Internal_Smart_Pointers;


   --------------------
   -- Smart_Pointers --
   --------------------

   package body Smart_Pointers is

      ---------------------
      -- Decrement_Count --
      ---------------------

      procedure Decrement_Count (
        Wrapper : in out Wrapper_Type;
        Is_Zero :    out Boolean
      ) is
      begin
         Counters.Decrement_Count (Wrapper.Counter, Is_Zero);
      end Decrement_Count;


      --------------
      -- Finalize --
      --------------

      procedure Finalize (Wrapper : in out Wrapper_Type) is
      begin
         Finalize (Wrapper.Target);
      end Finalize;


      ---------------------
      -- Increment_Count --
      ---------------------

      procedure Increment_Count (Wrapper : in out Wrapper_Type) is
      begin
         Counters.Increment_Count (Wrapper.Counter);
      end Increment_Count;


      ----------------
      -- Initialize --
      ----------------

      procedure Initialize (
        Wrapper : in out Wrapper_Type;
        Using   : in     Initializer_Type
      ) is
      begin
         Counters.Zero_Count (Wrapper.Counter);
         Initialize (Wrapper.Target, Using);
      end Initialize;


      --------------
      -- Allocate --
      --------------

      function Allocate (Using : Initializer_Type) return Smart_Pointer_Type is
      begin
         return Smart_Pointer_Type (P.Allocate (Using));
      end Allocate;


      ---------------------
      -- Access_Contents --
      ---------------------

      procedure Access_Contents (Smart_Pointer : Smart_Pointer_Type) is

         procedure Dispatch (Wrapper : in out Wrapper_Type);
         procedure Dispatch (Wrapper : in out Wrapper_Type) is
         begin
            Action (Wrapper.Target);
         end Dispatch;

         procedure AC is new P.Access_Contents (Dispatch);
      begin
         AC (P.Smart_Pointer_Type (Smart_Pointer));
      end Access_Contents;


--      -----------------
--      -- Dereference --
--      -----------------
--
--      function Dereference (Smart_Pointer : Smart_Pointer_Type) return Target_Type is
--      begin
--         return P.Dereference (P.Smart_Pointer_Type (Smart_Pointer)).Target;
--      end Dereference;


      -------------
      -- Is_Null --
      -------------

      function Is_Null (Smart_Pointer : Smart_Pointer_Type) return Boolean is
      begin
         return P.Is_Null (P.Smart_Pointer_Type (Smart_Pointer));
      end Is_Null;

   end Smart_Pointers;


   -------------------------------
   -- Indefinite_Smart_Pointers --
   -------------------------------

   package body Indefinite_Smart_Pointers is

      --------------
      -- Finalize --
      --------------

      procedure Finalize (Holder : in out Holder_Type) is
         procedure Free is new Ada.Unchecked_Deallocation (Target_Type, Target_Pointer);
      begin
         begin
            Finalize (Holder.Target.all);
         exception
            when others =>
               Free (Holder.Target);
               raise;
         end;
         Free (Holder.Target);
      end Finalize;


      ----------------
      -- Initialize --
      ----------------

      procedure Initialize (
        Holder : in out Holder_Type;
        Using  : in     Initializer_Type
      ) is
      begin
         Holder.Target := new Target_Type'(Create (Using));
      end Initialize;


      --------------
      -- Allocate --
      --------------

      function Allocate (Using : Initializer_Type) return Smart_Pointer_Type is
      begin
         return Smart_Pointer_Type (S.Allocate (Using));
      end Allocate;


      ---------------------
      -- Access_Contents --
      ---------------------

      procedure Access_Contents (Smart_Pointer : Smart_Pointer_Type) is

         procedure Dispatch (Holder : in out Holder_Type);
         procedure Dispatch (Holder : in out Holder_Type) is
         begin
            Action (Holder.Target.all);
         end Dispatch;

         procedure AC is new S.Access_Contents (Dispatch);
      begin
         AC (S.Smart_Pointer_Type (Smart_Pointer));
      end Access_Contents;


--      -----------------
--      -- Dereference --
--      -----------------
--
--      function Dereference (Smart_Pointer : Smart_Pointer_Type) return Target_Type is
--      begin
--         --  Holder_Type is returned by reference so there is minimal copying here
--         return S.Dereference (S.Smart_Pointer_Type (Smart_Pointer)).Target.all;
--      end Dereference;


      -------------
      -- Is_Null --
      -------------

      function Is_Null (Smart_Pointer : Smart_Pointer_Type) return Boolean is
      begin
         return S.Is_Null (S.Smart_Pointer_Type (Smart_Pointer));
      end Is_Null;

   end Indefinite_Smart_Pointers;

end Smart;
