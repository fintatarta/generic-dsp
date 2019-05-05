with Ada.Numerics.Generic_Complex_Types;
with Dsp.Ring_Filters;

generic
   type Scalar_Type is digits <>;

   with package Complex_Types is
     new Ada.Numerics.Generic_Complex_Types (Scalar_Type);
package DSP.New_Generic_Functions is
   subtype Complex_Type is Complex_Types.Complex;
   use type Complex_Types.Complex;

   package Real_Filters is
     new Dsp.Ring_Filters (Sample_Type      => Scalar_Type,
                           Coefficient_Type => Scalar_Type,
                           One              => 1.0,
                           Zero             => 0.0,
                           Zero_Coeff       => 0.0);

   function Delta_Signal (K : Integer) return Scalar_Type
                          renames Real_Filters.Delta_Signal;

   subtype Real_FIR is Real_Filters.Ring_FIR;

   subtype Real_IIR is Real_Filters.Ring_IIR;

   subtype Real_Filter_Interface is Real_Filters.Ring_Filter_Interface;

   subtype Real_IIR_Spec is Real_Filters.Ring_IIR_Spec;

   subtype Scalar_Array is Real_Filters.Sample_Array;


   package Complex_Filters is
     new Dsp.Ring_Filters (Sample_Type      => Complex_Type,
                           Coefficient_Type => Complex_Type,
                           One              => (1.0, 0.0),
                           Zero             => (0.0, 0.0),
                           Zero_Coeff       => (0.0, 0.0));

   function Delta_Signal (K : Integer) return Complex_Type
                          renames Complex_Filters.Delta_Signal;


   subtype Complex_Filter_Interface is Complex_Filters.Ring_Filter_Interface;

   subtype Complex_FIR is Complex_Filters.Ring_FIR;

   subtype Complex_IIR is Complex_Filters.Ring_IIR;

   subtype Complex_IIR_Spec is Complex_Filters.Ring_IIR_Spec;

   type Notch_Type is (Passband, Stopband);

   function Notch_Specs (Freq        : Normalized_Frequency;
                         Pole_Radius : Float;
                         Class       : Notch_Type := Stopband)
                         return Complex_IIR_Spec;

   function Notch_Specs (Freq        : Normalized_Frequency;
                         Pole_Radius : Float;
                         Class       : Notch_Type := Stopband)
                         return Real_IIR_Spec;

   function Complexify (X : Real_Filters.Coefficient_Array)
                        return Complex_Filters.Coefficient_Array;

end DSP.New_Generic_Functions;
