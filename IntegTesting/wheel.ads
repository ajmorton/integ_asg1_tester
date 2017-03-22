with Measures; use Measures;

-- This package defines a very simple and crude simulation of a wheel.
package Wheel is

   -- a type to represent a wheel, implemented below
   type Wheel is private;

   -- Initialise the wheel, setting an initial velocity from a
   -- probability distribution, with a small decelleration due to friction.
   procedure Init(Wh : out Wheel);

   -- Set the wheel's accelleration to be applied in the next clock tick
   procedure SetAccel(Wh : in out Wheel; Accel : in Measures.MPS2);

   -- Read the wheel's accelleration
   function GetAccel(Wh : in Wheel) return Measures.MPS2;

   -- Access the wheel's real velocity.
   -- This should only be called by the Speed Sensor.
   procedure GetVelocity(Wh : in Wheel;
                         Velocity : out Measures.KMPH);

   -- Tick the clock, providing the accelleration to the wheel
   procedure Tick(Wh : in out Wheel);

   -- for debugging
   procedure DebugPrint(Wh : in Wheel);

   -- manually lock wheel
   procedure LockWheel(Wh : out Wheel);

   -- manually set Wheel Speed
   procedure SetVelocity(Wh: in out Wheel; Vel: in KMPH);

private
   -- A type representing a wheel
   type Wheel is record
      -- The wheel's velocity
      Velocity : Measures.KMPH;

      -- The wheel's current accelleration
      Accel : Measures.MPS2;

      -- Whether the wheel has locked
      IsLocked : Boolean;
   end record;

end Wheel;
