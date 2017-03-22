with Measures; use Measures;
with Wheel;
with RandomNumber;

with Ada.Text_IO; use Ada.Text_IO;

package body SpeedSensor is

   -- error in speed sensor readings at high velocity
   HighVelocityError : constant Float := 0.05;

   -- error in speed sensor readings at low velocity
   LowVelocityError : constant Float := 0.5;

   -- threshold for low/high velocity for these errors:
   -- when actual velocity is below this threshold, the LowVelocityError
   -- is used; otherwise the HighVelocityError is used
   LowHighVelocityThreshold : constant Measures.KMPH :=
     Measures.KMPH(10.0);


   procedure Init(SS : out SpeedSensor) is
   begin
      -- begin with a meaningless value of initial Measured Velocity
      SS.LastReadSpeed := Measures.LimitKMPH(Measures.MIN_KMPH);
   end Init;

   -- add some error to Velocity to produce Measured Velocity readings
   function AddReadingError(Vel : in Measures.KMPH) return Measures.KMPH is
      Error : Float;
   begin
      -- decide which error to use
      if Vel < LowHighVelocityThreshold then
         Error := LowVelocityError;
      else
         Error := HighVelocityError;
      end if;

      return
        Measures.LimitKMPH(
                           RandomNumber.UniformFloatWithError(Float(Vel),Error)
                          );

   end AddReadingError;

   procedure Tick(SS : in out SpeedSensor; Wh : in Wheel.Wheel) is
      Velocity : Measures.KMPH;
   begin
      -- get the wheel's velocity
      Wheel.GetVelocity(Wh,Velocity);
      -- simulate errors in reading
      SS.LastReadSpeed := AddReadingError(Velocity);
   end Tick;

   function GetSpeed(SS : in SpeedSensor) return KMPH is
   begin
      return SS.LastReadSpeed;
   end GetSpeed;

end SpeedSensor;
