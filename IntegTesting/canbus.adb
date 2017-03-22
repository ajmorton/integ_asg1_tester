with RandomNumber;
with Ada.Numerics.Float_Random;          use Ada.Numerics.Float_Random;
with Ada.Text_IO;  use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Assertions;

package body CANBus  is

  -- Removed Randomness from CANBus

   -- initialise the bus by generating its first state
   procedure Init(Bus : out CANBus; Msg : CANBusMessage) is
   begin
      Bus.CurrentMessageAvailable := true;
      if Bus.CurrentMessageAvailable then
         Bus.CurrentMessage := Msg;
      end if;
      Bus.SentMessage := (MessageType => None);
   end Init;

   -- initialise the bus by generating its first state
   procedure Init(Bus : out CANBus) is
   begin
     -- Stubbed for testing
     Put_Line("===============================");
     Put_Line("ERROR: SHOULDN'T SEE IN TESTING");
     New_Line;
     Put_Line("there is a CANBus.Init(Bus) that");
     Put_Line("should be CANBus.Init(Bus, Msg)");
     Put_Line("in Tick or in Init");
     Put_Line("===============================");
     Bus.CurrentMessageAvailable := False;
     null;
   end Init;


   procedure GetNewMessage(Bus : in out CANBus;
                           NewMessageAvailable : out Boolean;
                           Message : out CANBusMessage) is
   begin
      NewMessageAvailable := Bus.CurrentMessageAvailable;
      if Bus.CurrentMessageAvailable then
         Message := Bus.CurrentMessage;
      end if;
   end GetNewMessage;



   procedure SendMessage(Bus : in out CANBus;
                         Message : in CANBusMessage) is
   begin
      -- in this simulation, just print the message to mimic sending it
      --Ada.Text_IO.Put("Sending message on CAN Bus: "); New_Line;
      --DebugPrintMessage(Message);
      Bus.SentMessage := Message;
   end SendMessage;

   procedure DebugPrintMessage(Message : in CANBusMessage) is
   begin
      case Message.MessageType is
         when BrakePressureUpdate =>
           Put("BrakePressureUpdate (Pressure: ");
           Put(Float(Message.Pressure)); Put(")"); New_Line;
         when EngineStart =>
            Put("EngineStart"); New_Line;
         when VelocityRequest =>
           Put("VelocityRequest (Sender: ");
           Put(Message.VSender'Image); Put(")"); New_Line;
         when ReadSettingsRequest =>
           Put("ReadSettingsRequest (Sender: ");
           Put(Message.RSSender'Image); Put(")"); New_Line;
         when ChangeSettingsRequest =>
           Put("ChangeSettingsRequest (Sender: ");
           Put(Message.CSSender'Image); Put("; VelocityChangeThreshold: ");
           Put(Float(Message.CSVelocityChangeThreshold));
           Put("; AverageVelocityThreshold: ");
           Put(Float(Message.CSAverageVelocityThreshold));
           Put(")"); New_Line;
         when VelocityResponse =>
           Put("VelocityResponse (Destination: ");
           Put(Message.VDestination'Image); Put("; CurrentTick: ");
           Put(Message.CurrentTick); Put("; CurrentVelocity: ");
           Put(Float(Message.CurrentVelocity)); Put(")"); New_Line;
         when ReadSettingsResponse =>
           Put("ReadSettingsResponse (Destination: ");
           Put(Message.RSDestination'Image);
           Put("; VelocityChangeThreshold: ");
           Put(Float(Message.RSVelocityChangeThreshold));
           Put("; AverageVelocityThreshold: ");
           Put(Float(Message.RSAverageVelocityThreshold));
           Put(")"); New_Line;
         when ChangeSettingsResponse =>
           Put("ChangeSettingsResponse (Destination: ");
           Put(Message.CSDestination'Image); Put(")"); New_Line;
         when None =>
          null;

      end case;
   end DebugPrintMessage;

   -- Stubbed for testing
   procedure Tick(Bus : in out CANBus) is
   begin
     Put_Line("===============================");
     Put_Line(" CANBus.Tick");
     Put_Line("ERROR: SHOULDN'T SEE IN TESTING");
     Put_Line("===============================");
     null;
   end Tick;


   -- TICK TESTING
   procedure Tick(Bus : in out CANBus; Msg : in CANBusMessage) is
   begin
      -- since the state of the bus never depends on its history
      -- (i.e. its past states) in this crude model, we can just
      -- reinitalise the bus to generate a new state
      Init(Bus, Msg);
   end Tick;


   -- check SentMessage
   function CheckSentMessage(Bus : in CANBus; Msg : in CANBusMessage) return Boolean is
   begin
     if Bus.SentMessage = Msg then
       return true;
     else
       New_Line;
       Put_Line("Message Mismatch: ");
       Put("    Bus.SentMessage: ");
       DebugPrintMessage(Bus.SentMessage);
       New_Line;
       Put("    Expected: ");
       DebugPrintMessage(Msg);
       New_Line;
       return false;
     end if;
   end CheckSentMessage;



end CANBUS;
