-- This package provides some basic measures and limit functions for
--  the car case study
package Measures is

   -- minimum velocity
   MIN_KMPH : constant Float := 0.0;
   -- maximum velocity
   MAX_KMPH : constant Float := 500.0;

   -- units of velocity
   type KMPH is digits 4 range MIN_KMPH .. MAX_KMPH;

   -- minimum acceleration
   -- typical braking distances give max deceleration due to brakes
   -- of up to 10m/s^2 according to the web.
   MIN_MPS2 : constant Float := -20.0;

   -- maximum acceleration
   -- typical rate of acceleration for ordinary cars is 3 to 4 m/s^2
   MAX_MPS2 : constant Float := 10.0;

   -- units of acceleration
   type MPS2 is digits 8 range MIN_MPS2 .. MAX_MPS2;

   -- braking when at maximum brake pressure
   -- typical maximum braking deceleration for cars is about 10 mps^2
   -- according to the web
   MAX_PRESSURE_MPS2 : constant MPS2 := MPS2(-10.0);

   -- minimum brake pressure: when the brake is not applied at all
   MIN_BRAKE_PRESSURE : constant Float := 0.0;

   -- maximum braking pressure: when the brake is fully applied
   MAX_BRAKE_PRESSURE : constant Float := 1.0;

   -- the amount of pressure that the brake applies to the wheel
   type BrakePressure is digits 3 range
     MIN_BRAKE_PRESSURE .. MAX_BRAKE_PRESSURE;


   -- A function to limit Floats
   function Limit(Input : in Float; Fst : in Float; Lst : in Float)
                 return Float;
   --# pre Fst <= Lst;
   --# return Output => (Fst <= Output and Output <= Lst);

   -- A function to limit Km/H measures
   function LimitKMPH(Input : in Float) return KMPH;
   --# return Output => (KMPH'First <= Output and Output <= KMPH'Last);

   -- A function to limit MPS2 measures
   function LimitMPS2(Input : in Float) return MPS2;
   --# return Output => (MPS2'First <= Output and Output <= MPS2'Last);

   -- A function to limit brake pressure measures
   function LimitBrakePressure(Input : in Float) return BrakePressure;
   --# return Output => (BrakePressure'First <= Output and
   --#                   Output <= BrakePressure'Last);

end Measures;
