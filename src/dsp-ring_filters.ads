with Ada.Finalization;

use Ada;

generic
   type Sample_Type is private;
   type Coefficient_Type is private;

   One :  Sample_Type;
   Zero : Sample_Type;

   Zero_Coeff : Coefficient_Type;

   with function "+" (X, Y : Sample_Type) return Sample_Type is <>;
   with function "-" (X, Y : Sample_Type) return Sample_Type is <>;
   with function "*" (X : Coefficient_Type; Y : Sample_Type) return Sample_Type is <>;
package Dsp.Ring_Filters is

   type Signal is access function (N : Integer) return Sample_Type;

   type Coefficient_Array is array (Natural range<>) of Coefficient_Type;
   type Sample_Array is array (Integer range<>) of Sample_Type;

   function Delta_Signal (K : Integer) return Sample_Type
   is ((if K = 0 then One else Zero));

   function Apply (F : Signal; From, To : Integer) return Sample_Array
     with
       Pre => To >= From,
       Post => Apply'Result'First = From and Apply'Result'Last = To;


   type Ring_Filter_Interface is limited interface;
   type Filter_Status is (Unready, Ready, Running);

   function Status (Item : Ring_Filter_Interface)
                    return Filter_Status
                    is abstract ;

   procedure Reset (Item : in out Ring_Filter_Interface)
   is abstract
     with Pre'Class => Item.Status /= Unready,
       Post'Class => Item.Status = Ready;

   function Filter (Item  : in out Ring_Filter_Interface;
                    Input : Sample_Type)
                    return Sample_Type
                    is abstract
     with Pre'Class => Item.Status /= Unready,
       Post'Class => Item.Status = Running;

   function Filter (Item  : in out Ring_Filter_Interface'Class;
      Input : Sample_Array;
      Keep_Status : Boolean := False)
                    return Sample_Array;


   type Ring_FIR is
     new Finalization.Limited_Controlled
     and Ring_Filter_Interface
   with private;

   procedure Reset (Item : in out Ring_FIR);

   function Status (Item : Ring_Fir) return Filter_Status;

   function Filter (Item  : in out Ring_FIR;
                    Input : Sample_Type)
                    return Sample_Type;

   --   function Is_Empty (F : Ring_FIR) return Boolean;

   procedure Set (Filter           : in out Ring_FIR;
                  Impulse_Response : Coefficient_Array)
     with
       Pre => Filter.Status = Unready,
       Post => Filter.Status = Ready;

   type Ring_IIR is
     new Ada.Finalization.Limited_Controlled
     and Ring_Filter_Interface
   with private;

   function Status (Item : Ring_IIR) return Filter_Status;

   procedure Reset (Item : in out Ring_IIR);

   function Filter (Item  : in out Ring_IIR;
                    Input : Sample_Type)
                    return Sample_Type;


   type Ring_IIR_Spec (Num_Deg, Den_Deg : Natural) is
      record
         Numerator   : Coefficient_Array (0 .. Num_Deg);
         Denominator : Coefficient_Array (1 .. Den_Deg);
      end record;

   procedure Set (Filter : in out Ring_IIR;
                  Specs  : Ring_IIR_Spec)
     with
       Pre => Filter.Status = Unready,
       Post => Filter.Status = Ready;

   procedure Set (Filter      : in out Ring_IIR;
                  Numerator   : Coefficient_Array;
                  Denominator : Coefficient_Array)
     with
       Pre =>
         Filter.Status = Unready
         and Numerator'First >= 0
         and Denominator'First >= 0,
         Post =>
           Filter.Status = Ready;
private
   type Coefficient_Array_Access is access Coefficient_Array;
   type Sample_Array_Access is access Sample_Array;

   type Ring_FIR is
     new Ada.Finalization.Limited_Controlled
     and Ring_Filter_Interface
   with
      record
         Current_Status : Filter_Status := Unready;
         Spec           : Coefficient_Array_Access := null;
         Buffer         : Sample_Array_Access := null;
      end record
     with Type_Invariant =>
       ((Spec = null) = (Buffer = null))
       and then
         (Spec = null
          or else
            (Spec.all'First = 0
             and Buffer.all'First = 1
             and Buffer.all'Last = Spec.all'Last));

   overriding procedure Finalize (Object : in out Ring_FIR);


   function Status (Item : Ring_Fir) return Filter_Status
   is (Item.Current_Status);


   type Ring_IIR is
     new Ada.Finalization.Limited_Controlled
     and Ring_Filter_Interface
   with
      record
         Current_Status : Filter_Status := Unready;
         Num            : Coefficient_Array_Access := null;
         Den            : Coefficient_Array_Access := null;
         Buffer         : Sample_Array_Access := null;
      end record
     with Type_Invariant =>
       ((Num = null) = (Den = null) and (Num = null) = (Buffer = null))
       and then
         (Num = null
          or else
            (Num.all'First = 0
             and Den.all'First = 1
             and Buffer.all'First = 1
             and Buffer.all'Last = Num.all'Last
             and Buffer.all'Last = Den.all'Last));

   overriding procedure Finalize (Object : in out Ring_IIR);


   function Status (Item : Ring_IIR) return Filter_Status
   is (Item.Current_Status);

end Dsp.Ring_Filters;
