with ClosedLoop; use ClosedLoop;
with Brake; use Brake;
with Wheel; use Wheel;
with SpeedSensor; use SpeedSensor;
with CANBus; use CANBus;
with BrakeController; use BrakeController;

with Measures; use Measures;
with Ada.Text_IO; use Ada.Text_IO;

procedure CLTester is
  -- All components moved up out of ClosedLoop for visibility purposes
  -- shouldn't require a change to your code.
  Br : Brake.Brake;
  Wh : Wheel.Wheel;
  Sp : SpeedSensor.SpeedSensor;
  Con : BrakeController.BrakeController;
  Bus : CANBus.CANBus;

  -- used to intentionally set the CANBus Message on CANBus.Tick(Msg)
  Msg : CANBusMessage;

  -- In my implementation SpeedSensor ticks after message passing
  -- this causes errors with test 18, vel tracks the Velocity before the
  -- tick to correct this
  Vel : KMPH;

  -- Takes in a Boolean (typically a function returning bool) and prints
  -- out the success of failure results of the test
  procedure Test(Test : Boolean; TestName : String) is
  begin
    if(Test) then
      Put_Line(ASCII.ESC & "[32m " & "SUCCESS    " & ASCII.ESC & "[39;49m" & TestName);
    else
      Put_Line(ASCII.ESC & "[31m " & "FAILURE    " & ASCII.ESC & "[39;49m" & TestName);
    end if;
  end Test;

begin

  New_Line;
  Put_Line("=============================");
  Put_Line("        EBS Testing          ");
  Put_Line("=============================");
  New_Line;


  Test(true,  "Test Passed");
  Test(false, "Test Failed");
  Put_Line(ASCII.ESC & "[33m " & "MANUAL     " & ASCII.ESC & "[39;49m" & "Dependant on individual implemntation");


  New_Line;
  Put_Line("=============================");
  Put_Line("    INITIAL STATE CHECKS");
  Put_Line("=============================");
  New_Line;

  Msg := (MessageType => BrakePressureUpdate,
          Pressure => BrakePressure(0.0));

  ClosedLoop.Init(Br, Wh, Sp, Con, Bus, Msg);

  -- TEST 1: INIT MODE
  Put_Line(ASCII.ESC & "[33m " & "MANUAL     " & ASCII.ESC & "[39;49m" & " 1: INIT_MODE_IS_OFF");

  -- TEST 2: INIT THRESHOLDS
  Put_Line(ASCII.ESC & "[33m " & "MANUAL     " & ASCII.ESC & "[39;49m" & " 2: INIT_THRESHOLDS");

  New_Line;
  Put_Line("=============================");
  Put_Line("      OFF MODE CHECKS        ");
  Put_Line("=============================");
  New_Line;

  -- TEST 3: BRAKE IS ON FULL
  Test(Brake.CheckPressure(Br, BrakePressure(MAX_BRAKE_PRESSURE)), " 3: BRAKE_ON_MAX_WHEN_OFF");

  -- TEST 4: BRAKE_CANT_CHANGE_IN_OFF
  Msg := (MessageType => BrakePressureUpdate,
          Pressure => 0.333);
  ClosedLoop.Tick(Br, Wh, Sp, Con, Bus, Msg);
  Test(Brake.CheckPressure(Br, BrakePressure(MAX_BRAKE_PRESSURE)), " 4: BRAKES_CANT_CHANGE_WHEN_OFF");

  -- TEST 5: ReadSettingsReq DRIVER
  Msg := (MessageType => ReadSettingsRequest,
          RSSender => Driver);
  ClosedLoop.Tick(Br, Wh, Sp, Con, Bus, Msg);
  Msg := (MessageType => None);
  Test(CANBus.CheckSentMessage(Bus, Msg), " 6: DRIVER_READ_REQ_REFUSED");

  -- TEST 6: ReadSettingsRequest MECH
  -- NOTE: if this is failing (seeing None instead of intended message type)
  --       it may be that SendMessage is being called on Bus in ClosedLoop,
  --       and not the CLTester's version of Bus
  Msg := (MessageType => ReadSettingsRequest,
          RSSender => Mechanic);
  ClosedLoop.Tick(Br, Wh, Sp, Con, Bus, Msg);
  Msg := (MessageType => ReadSettingsResponse,
          RSDestination => Mechanic,
          RSVelocityChangeThreshold => KMPH(10.0),
          RSAverageVelocityThreshold => KMPH(15.0));
  Test(CANBus.CheckSentMessage(Bus, Msg), " 6: MECH_READ_REQ_ACCEPTED");


  -- TEST 7: ReadSettingsRequest MANU
  Msg := (MessageType => ReadSettingsRequest,
          RSSender => Manufacturer);
  ClosedLoop.Tick(Br, Wh, Sp, Con, Bus, Msg);
  Msg := (MessageType => ReadSettingsResponse,
          RSDestination => Manufacturer,
          RSVelocityChangeThreshold => KMPH(10.0),
          RSAverageVelocityThreshold => KMPH(15.0));
  Test(CANBus.CheckSentMessage(Bus, Msg), " 7: MANU_READ_REQ_ACCEPTED");

  -- TEST 8: ChangeSettingsRequest Driver
  Msg := (MessageType => ChangeSettingsRequest,
          CSSender => Driver,
          CSVelocityChangeThreshold => 1.0,
          CSAverageVelocityThreshold => 1.0);
  ClosedLoop.Tick(Br, Wh, Sp, Con, Bus, Msg);
  Msg := (MessageType => None);
  Test(CANBus.CheckSentMessage(Bus, Msg), " 8: DRIVER_CHANGE_REQ_REFUSED");

  -- TEST 9: ChangeSettingsRequest Mechanic
  Msg := (MessageType => ChangeSettingsRequest,
          CSSender => Mechanic,
          CSVelocityChangeThreshold => 1.0,
          CSAverageVelocityThreshold => 1.0);
  ClosedLoop.Tick(Br, Wh, Sp, Con, Bus, Msg);
  Msg := (MessageType => None);
  Test(CANBus.CheckSentMessage(Bus, Msg), " 9: MECH_CHANGE_REQ_REFUSED");

  -- TEST 10 Refused ChangeSettingsRequest no change in thresholds
  Msg := (MessageType => ReadSettingsRequest,
          RSSender => Manufacturer);
  ClosedLoop.Tick(Br, Wh, Sp, Con, Bus, Msg);

  Msg := (MessageType => ReadSettingsResponse,
          RSDestination => Manufacturer,
          RSVelocityChangeThreshold => KMPH(10.0),
          RSAverageVelocityThreshold => KMPH(15.0));
  Test(CANBus.CheckSentMessage(Bus, Msg), "10: THRESHOLDS_SAME_AFTER_REFUSED_CHANGE_REQ");

  -- TEST 11: ChangeSettingsRequest Manufacturer
  Msg := (MessageType => ChangeSettingsRequest,
          CSSender => Manufacturer,
          CSVelocityChangeThreshold => 2.0,
          CSAverageVelocityThreshold => 2.0);
  ClosedLoop.Tick(Br, Wh, Sp, Con, Bus, Msg);
  Msg := (MessageType => ChangeSettingsResponse,
          CSDestination => Manufacturer);
  Test(CANBus.CheckSentMessage(Bus, Msg), "11: MANU_CHANGE_REQ_ACCEPTED");

-- TEST 12 Accepted ChangeSettingsRequest has change in thresholds
Msg := (MessageType => ReadSettingsRequest,
        RSSender => Manufacturer);
ClosedLoop.Tick(Br, Wh, Sp, Con, Bus, Msg);

Msg := (MessageType => ReadSettingsResponse,
        RSDestination => Manufacturer,
        RSVelocityChangeThreshold => KMPH(2.0),
        RSAverageVelocityThreshold => KMPH(2.0));
Test(CANBus.CheckSentMessage(Bus, Msg), "12: THRESHOLDS_DIFF_AFTER_ACCEPTED_CHANGE_REQ");


-- TEST 13: Wheels Stationary when MODE is off
-- Test Unneccesary, intentionally seeded fault
-- Test(SpeedSensor.GetSpeed(Sp) = KMPH(0.0), "13: WHEEL_STATIONARY_IN_OFF_MODE");


New_Line;
Put_Line("=============================");
Put_Line("      ON MODE CHECKS        ");
Put_Line("=============================");
New_Line;


-- TEST 14 EngineON
Msg := (MessageType => EngineStart);
ClosedLoop.Tick(Br, Wh, Sp, Con, Bus, Msg);
Put_Line(ASCII.ESC & "[33m " & "MANUAL     " & ASCII.ESC & "[39;49m" & "14: ENGINE_STARTED_MODE_IS_ON");

-- TEST 15 BrakePressureUpdate no ABS
Put_Line(ASCII.ESC & "[33m " & "MANUAL     " & ASCII.ESC & "[39;49m" & "15: B_PRESSURE_UPDATE => wheel lock breaks this");

--Msg := (MessageType => BrakePressureUpdate,
--        Pressure => BrakePressure(0.666));
--ClosedLoop.Tick(Br, Wh, Sp, Con, Bus, Msg);
--Test(Brake.CheckPressure(Br, BrakePressure(0.666)), "15: B_PRESSURE_UPDATE_SUCCESS");

-- TEST 16 ChangeSettingsRequest Denied when Mode = ON
-- only checking Manufacturer as they have highest priveleges
Msg := (MessageType => ChangeSettingsRequest,
        CSSender => Manufacturer,
        CSVelocityChangeThreshold => 3.0,
        CSAverageVelocityThreshold => 3.0);
ClosedLoop.Tick(Br, Wh, Sp, Con, Bus, Msg);
Msg := (MessageType => None);
Test(CANBus.CheckSentMessage(Bus, Msg), "16: CHANGE_SETTINGS_DENIED");


-- TEST 17 ReadSettingsRequest Denied when Mode = ON
-- only checking Manufacturer as they have highest priveleges
Msg := (MessageType => ReadSettingsRequest,
        RSSender => Manufacturer);
ClosedLoop.Tick(Br, Wh, Sp, Con, Bus, Msg);
Msg := (MessageType => None);
Test(CANBus.CheckSentMessage(Bus, Msg), "17: READ_SETTINGS_DENIED");

-- TEST 18 Driver VelocityRequest
-- currentTick may differ if you start on CurrentTick = 1
Msg := (MessageType => VelocityRequest,
        VSender => Driver);
Vel := BrakeController.GetCurrentVelocity(Con);
ClosedLoop.Tick(Br, Wh, Sp, Con, Bus, Msg);
Msg := (MessageType => VelocityResponse,
        VDestination => Driver,
        CurrentTick => 13,
        CurrentVelocity => Vel);
Test(CANBus.CheckSentMessage(Bus, Msg), "18: VEL_REQ_DRIVER");

-- TEST 19 Mechanic VelocityRequest
Msg := (MessageType => VelocityRequest,
        VSender => Mechanic);
ClosedLoop.Tick(Br, Wh, Sp, Con, Bus, Msg);
Msg := (MessageType => None);
Test(CANBus.CheckSentMessage(Bus, Msg), "19: VEL_REQ_MECHANIC_DENIED");

-- TEST 20 Manufacturer VelocityRequest
Msg := (MessageType => VelocityRequest,
        VSender => Manufacturer);
ClosedLoop.Tick(Br, Wh, Sp, Con, Bus, Msg);
Msg := (MessageType => None);
Test(CANBus.CheckSentMessage(Bus, Msg), "20: VEL_REQ_MANU_DENIED");

New_Line;
Put_Line("=============================");
Put_Line("        ABS TESTING          ");
Put_Line("=============================");
New_Line;

Put_Line(ASCII.ESC & "[33m " & "MANUAL     " & ASCII.ESC & "[39;49m" & "21: MAYBE_LATER");


  --- ABS
  --	- detect
  --		- BRAKES OFF		(Immediate)
  --		- BRAKES ON
  --		- BRAKES OFF		(Continues till 0 pressure)
  --		- BRAKES ON
  --		- BRAKES OFF		(Update Pressure, doesn’t change till next cycle)
  --		- TERMINATE		(Catch 0 Pressure message)
  --							(no need to test in brakes off stage —> already zero)
  --
  --





end CLTester;
