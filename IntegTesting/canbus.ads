with Measures;

-- Models a CAN bus on which the electronic braking system can receive
-- messages, at most one message per clock tick
package CANBus is

   -- a type to represent a CAN bus
   type CANBus is private;

   -- the various CAN Bus message types
   -- Note that all message types for messages going TO the EBS
   -- (i.e. those received by the EBS) are listed together purposefully
   type CANBusMessageType is (BrakePressureUpdate,
                              EngineStart,
                              VelocityRequest,
                              ReadSettingsRequest,
                              ChangeSettingsRequest,
                              VelocityResponse,
                              ReadSettingsResponse,
                              ChangeSettingsResponse,
                              None);


   -- the position of the first message type above that is received
   -- by the EBS
   FIRST_TOEBS_MESSAGE_INDEX : constant Integer :=
     CANBusMessageType'Pos(BrakePressureUpdate);

   -- the position of the last message type above that is received
   -- by the EBS
   LAST_TOEBS_MESSAGE_INDEX : constant Integer :=
     CANBusMessageType'Pos(ChangeSettingsRequest);

   -- The system's Principals
   type Principal is (Driver, Manufacturer, Mechanic);

   -- the type for CAN Messages. Note that some field names
   -- have a leading character or two to ensure that field names for
   -- the various message types are disjoint, as required by Ada.
   -- Thus e.g. the Sender field of the VelocityRequest message
   -- has the name VSender below; likewise the Destination field of
   -- VelocityResponse messages is called here VDestination, and so on.
   type CANBusMessage(MessageType : CANBusMessageType := EngineStart) is record
      case MessageType is
         when BrakePressureUpdate =>
            Pressure : Measures.BrakePressure;
         when VelocityRequest =>
            VSender : Principal;
         when ReadSettingsRequest =>
            RSSender : Principal;
         when ChangeSettingsRequest =>
            CSSender : Principal;
            CSVelocityChangeThreshold : Measures.KMPH;
            CSAverageVelocityThreshold : Measures.KMPH;
         when VelocityResponse =>
            VDestination : Principal;
            CurrentTick : Natural;
            CurrentVelocity : Measures.KMPH;
         when ReadSettingsResponse =>
            RSDestination : Principal;
            RSVelocityChangeThreshold : Measures.KMPH;
            RSAverageVelocityThreshold : Measures.KMPH;
         when ChangeSettingsResponse =>
            CSDestination : Principal;
         when others =>
            null;
      end case;
   end record;

   -- call this procedure to query the CAN bus for a new message.
   -- Whether a new message is available is placed in the NewMessageAvailable
   -- boolean. When this is set to True, the new message is placed in Message
   procedure GetNewMessage(Bus : in out CANBus;
                           NewMessageAvailable : out Boolean;
                           Message : out CANBusMessage);

   -- call this procedure to send a message on the CAN bus
   procedure SendMessage(Bus : in out CANBus;
                         Message : in CANBusMessage);

   -- for debugging to print out CAN bus messages
   procedure DebugPrintMessage(Message : in CANBusMessage);

   -- called once per clock tick to update the CAN Bus
   procedure Tick(Bus : in out CANBus);

   -- called once per clock tick to update the CAN Bus
   procedure Tick(Bus : in out CANBus; Msg : in CANBusMessage);

   -- initialise the CAN Bus
   procedure Init(Bus : out CANBus);

   -- initialise the CAN Bus TESTING
   procedure Init(Bus : out CANBus ; Msg : in CANBusMessage);


   -- check SentMessage
   function CheckSentMessage(Bus : in CANBus; Msg : in CANBusMessage) return Boolean;

private
   type CANBus is record
      -- whether there is a message currently available to be read on the bus
      CurrentMessageAvailable : Boolean;

      -- if CurrentMessageAvailable, this holds the currently available message
      CurrentMessage : CANBusMessage;

      SentMessage : CANBusMessage;
   end record;

end CANBus;
