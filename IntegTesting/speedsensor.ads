with Measures;
with Wheel;

-- This package models a Speed Sensor whose readings are more inaccurate
-- when the Wheel is has low Velocity
package SpeedSensor is

   -- the type of the speed sensor
   type SpeedSensor is private;

   -- initialise the speed sensor. It will take on some initial
   -- Measured Velocity which will be meaningless until Tick is called.
   procedure Init(SS : out SpeedSensor);

   -- tick the speed sensor, getting the wheel's current speed
   procedure Tick(SS : in out SpeedSensor; Wh : in Wheel.Wheel);

   -- get the current Measured Velocity of the Wheel;
   -- result is meaningless until Tick is called for the first time.
   function GetSpeed(SS : in SpeedSensor) return Measures.KMPH;

private
   type SpeedSensor is record
      -- the current Measured Velocity of the Wheel
      LastReadSpeed : Measures.KMPH;
   end record;

end SpeedSensor;
