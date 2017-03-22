with Measures;
with Wheel;
with RandomNumber;

with Ada.Text_IO; use Ada.Text_IO;-- for debugging
with Ada.Float_Text_IO; -- for debugging

package body Brake is

   -- used to add some random error into the value applied to the wheel
   Error : constant Float := 0.05;

   procedure Init(Br : out Brake) is
   begin
      Br.Pressure := Measures.BrakePressure(Measures.MIN_BRAKE_PRESSURE);
   end;

   procedure SetPressure(Br : in out Brake; Pr : in Measures.BrakePressure) is
   begin
      Br.Pressure := Pr;
   end;

   function GetPressure(Br : in Brake) return Measures.BrakePressure is
   begin
      return Br.Pressure;
   end GetPressure;

   procedure Tick(Br : in Brake; Wh : in out Wheel.Wheel) is
      -- a simplistic linear translation of brake pressure into decelleration
      Pressure : constant Float := Float(Br.Pressure);
      NewAccel : constant Float := Float(Br.Pressure) * Float(Measures.MAX_PRESSURE_MPS2);

      -- insert some random noise
      NewMPS2 : constant Measures.MPS2 :=
        Measures.LimitMPS2(RandomNumber.UniformFloatWithError(NewAccel,
                                                              Error));
   begin
      -- translate zero pressure to zero accelleration
      if Float(Br.Pressure) = 0.0 then
         Wheel.SetAccel(Wh, Measures.MPS2(0.0));
      else
         Wheel.SetAccel(Wh, NewMPS2);
      end if;
   end;

   procedure DebugPrint(Br : in Brake) is
   begin
      Ada.Text_IO.Put("Brake: ");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put("    Pressure: ");
      Ada.Float_Text_IO.Put(Float(Br.Pressure));
      Ada.Text_IO.New_Line;
   end DebugPrint;

   function CheckPressure(Br : Brake; P : BrakePressure) return Boolean is
   begin
     if Br.Pressure = P then
       return true;
     else
       Put_Line("FAILED: B.Pressure = " & Br.Pressure'Img & ", should be " & P'Img);
       return False;
     end if;
   end CheckPressure;


end Brake;
