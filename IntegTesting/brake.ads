with Measures; use Measures;
with Wheel;

-- a simulation of a Brake
package Brake is

   -- a type to represent a brake
   type Brake is private;

   -- initialise the brake with minimum pressure
   procedure Init(Br : out Brake);

   -- change the brake pressure
   procedure SetPressure(Br : in out Brake; Pr : in Measures.BrakePressure);

   -- get the brake pressure
   function GetPressure(Br : in Brake) return Measures.BrakePressure;

   -- tick: apply the pressure to the wheel, by changing its accelleration
   procedure Tick(Br : in Brake; Wh : in out Wheel.Wheel);

   -- for debugging
   procedure DebugPrint(Br : in Brake);

   function CheckPressure(Br : Brake; P : BrakePressure) return Boolean;

private
   type Brake is record
      Pressure : Measures.BrakePressure;
   end record;
end Brake;
