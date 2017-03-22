with Measures;  use Measures;
with RandomNumber;
with ClockTick;
with Ada.Numerics.Float_Random;          use Ada.Numerics.Float_Random;

with Ada.Text_IO; -- for debugging
with Ada.Float_Text_IO; -- for debugging

package body Wheel is

   -- Parameters for generating random  upon initialisation
   VelocityMu : constant Float := 100.0;
   VelocitySigma : constant Float := 10.0;

   -- Initial accelleration
   InitialAccel : constant Measures.MPS2 := Measures.MPS2(-0.05);

   -- in this crude wheel model we allow the wheel to lock only when
   -- it's velocity is above this level
   LockingVelocityThreshold : constant Measures.KMPH := Measures.KMPH(10.0);

   -- decelleration greater than this threshold (i.e. a value that is
   -- less than this negative number) can potentially cause the wheel to lock
   HighDecellerationThreshold : constant Measures.MPS2 := Measures.MPS2(-5.0);

   -- Probability of the wheel locking on a given clock tick when
   -- under this kind of high decelleration
   LockingProbabilityPerTick : constant Uniformly_Distributed := 0.1;

   procedure Init(Wh : out Wheel) is
   begin
      -- Generate a random velocity
      Wh.Velocity :=
        Measures.LimitKMPH(RandomNumber.
                             NormalFloat(VelocityMu,VelocitySigma));
      Wh.Accel := InitialAccel;

      Wh.IsLocked := False;
   end Init;

   procedure SetAccel(Wh : in out Wheel; Accel : in Measures.MPS2) is
   begin
      Wh.Accel := Accel;
   end SetAccel;

   function GetAccel(Wh : in Wheel) return Measures.MPS2 is
   begin
      return Wh.Accel;
   end GetAccel;

   procedure GetVelocity(Wh : in Wheel;
                         Velocity : out Measures.KMPH) is
   begin
      if Wh.IsLocked then
         -- when the wheel is locked it reports zero velocity
         Velocity := Measures.KMPH(0.0);
      else
         Velocity := Wh.Velocity;
      end if;
   end GetVelocity;

   -- we have accelleration in metres per second per second
   -- we have velocity in kilometres per hour
   -- we need to turn the accelleration into kilometres per hour per tick
   function MPS2ToKMPHPTick(Accel : Measures.MPS2) return Float is
      MPSPTick : constant Float :=
        Float(Accel)/Float(ClockTick.TicksPerSecond);
   begin
      -- 1 metre per second is 3.6 KMPH
      return MPSPTick * 3.6;
   end MPS2ToKMPHPTick;

   procedure Tick(Wh : in out Wheel) is
      Vel : Float;
      Lock : constant Boolean :=
        RandomNumber.RandomBooleanWithBias(LockingProbabilityPerTick);
   begin

      if Wh.IsLocked then
         -- check to see if we can unlock
         if Float(Wh.Accel) >= 0.0 then
            -- unlock the wheel
            Wh.IsLocked := False;
         else
            -- stay locked since it still has negative acceleration applied.
            -- in this crude model of a locked wheel we allow it to keep its
            -- full velocity while locked but in practice it would likely
            -- decrease due to raod friction etc.
            return;
         end if;
      end if;

      -- if we get here, Wh.IsLocked must be false
      -- each clock tick, decide whether the wheel should lock
      if Wh.Accel < HighDecellerationThreshold and
        Wh.Velocity >= LockingVelocityThreshold and
        Lock then
          Ada.Text_IO.Put_Line("================================");
          Ada.Text_IO.Put_Line("LOCKLOCKLOCKLOCKLOCKLOCKLOCKLOCK");
          Ada.Text_IO.Put_Line("================================");
         Wh.IsLocked := True;
      else
         -- we don't lock, so apply the acceleration
         Vel :=
           Float(Wh.Velocity) + MPS2ToKMPHPTick(Wh.Accel);

         Wh.Velocity :=
           Measures.LimitKMPH(Vel);
      end if;

   end Tick;

   procedure DebugPrint(Wh : in Wheel) is
   begin
      Ada.Text_IO.Put("Wheel: "); Ada.Text_IO.New_Line;
      Ada.Text_IO.Put("    Velocity: ");
      Ada.Float_Text_IO.Put(Float(Wh.Velocity));
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put("    Accel:    ");
      Ada.Float_Text_IO.Put(Float(Wh.Accel));
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put("    Locked:  "); Ada.Text_IO.Put(Wh.IsLocked'Image);
      Ada.Text_IO.New_Line;
   end DebugPrint;


   -- manually lock wheel
   procedure LockWheel(Wh : out Wheel) is
   begin
     Wh.IsLocked := true;
   end LockWheel;


   -- manually set Wheel Speed
   procedure SetVelocity(Wh: in out Wheel; Vel: in KMPH) is
   begin
     Wh.Velocity := Vel;
   end SetVelocity;


end Wheel;
