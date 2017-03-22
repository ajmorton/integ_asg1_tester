# Testing Instructions
Testing for SWEN90010 Assignment 1

Copy in your BrakeController and ControlLoop into `Testing/`
and make changes in the following files:

## `ControlLoop.adb`
replace

      procedure Init;
      procedure Tick;
      
with

      procedure Init (Br : in out Brake.Brake;
                      Wh : in out Wheel.Wheel;
                      Sp : in out SpeedSensor.SpeedSensor;
                      Con : in out BrakeController.BrakeController;
                      Bus : in out CANBus.CANBus;
                      Msg : in CANBus.CANBusMessage);

      procedure Tick (Br : in out Brake.Brake;
                      Wh : in out Wheel.Wheel;
                      Sp : in out SpeedSensor.SpeedSensor;
                      Con : in out BrakeController.BrakeController;
                      Bus : in out CANBus.CANBus;
                      Msg : in CANBus.CANBusMessage);

add package imports as needed

## `ControlLoop.adb`

Replace the procedure calls for Init and Tick as above.

Odds are you'll have some issue where you're not modifying the passed in
components (`Br`, `Wh` etc), but instead your own local components.
Comment out your own local components to bug check this.

In the changed Init and Tick procedures, replace

        Bus.Init(Bus)
        Bus.Tick(Bus)
with

        Bus.Init(Bus, Msg)
        Bus.Tick(Bus, Msg)

as needed

## Additional Notes


  - CANBus.SendMessage( ) now modifies the state of the bus passed in.
    Make sure the procedure that calls CANBus.SendMessage() passes Bus as an
    `in out` parameter

  - Suppress all printing to console from your ControlLoop and BrakeController

  -  gnatmake CLTester.adb
     ./cltester


## Bug Checking

  1. If you see

    	Message Mismatch:
        	Bus.SentMessage:
        	Expected: {SOME MESSAGE}
        
	Where Bus.SentMessage is not followed by a DebugPrintMessage then check the first point in General.
