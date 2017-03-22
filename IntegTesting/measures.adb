package body Measures is

   function Limit(Input : in Float; Fst : in Float; Lst : in Float)
                 return Float
   is
      Output : Float;
   begin
      if Input < Fst then
         Output := Fst;
      elsif Input > Lst then
         Output := Lst;
      else
         Output := Input;
      end if;

      return Output;
   end Limit;

   function LimitKMPH(Input : in Float) return KMPH
   is begin
      return KMPH(Limit(Input, Float(KMPH'First), Float(KMPH'Last)));
   end LimitKMPH;

   function LimitMPS2(Input : in Float) return MPS2
   is begin
      return MPS2(Limit(Input, Float(MPS2'First), Float(MPS2'Last)));
   end LimitMPS2;

   function LimitBrakePressure(Input : in Float) return BrakePressure
   is begin
      return BrakePressure(Limit(Input,
                                 Float(BrakePressure'First),
                                 Float(BrakePressure'Last)));
   end LimitBrakePressure;

end Measures;

