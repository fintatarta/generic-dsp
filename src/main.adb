with Dsp.Functions;
with Ada.Complex_Text_IO;
with Ada.Text_IO;
procedure Main is
   use Dsp.Functions;
   use Ada.Complex_Text_IO;
   use Ada;

   F1 : Complex_FIR;
   F2 : Complex_IIR;
begin
   F1.Set (Scalar_Array'(0 => 1.0, 1 => 2.0, 2 => 3.0));

   for K in 0..5 loop
      Put (F1.Filter (Delta_Signal (K)));
      Text_IO.New_Line;
   end loop;

   F2.Set (Numerator => Scalar_Array'(0 => 1.0),
           Denominator => Scalar_Array'(0 => 1.0, 1 => -0.5));


   Text_IO.Put_Line ("------------");

   for K in 0..5 loop
      Put (F2.Filter (Delta_Signal (K)));
      Text_IO.New_Line;
   end loop;
end Main;
