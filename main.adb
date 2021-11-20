pragma SPARK_Mode (On);

with AS_Io_Wrapper; use AS_Io_Wrapper;
with Road_Intersection; use Road_Intersection;

procedure Main
is
   subtype Mode is Road_Intersection.Mode;
   subtype Segment_State_Type is Road_Intersection.Segment_State_Type;
   subtype Signal_State_Type is Road_Intersection.Signal_State_Type;

   Success: Boolean;
   Route: Route_Type;
   The_Mode: Mode;
   Segment_State : Segment_State_Type;
   Signal_State  : Signal_State_Type;

begin
   As_Init_Standard_Input;
   As_Init_Standard_Output;
   Init(Segment_State,Signal_State);
   loop
      pragma Loop_Invariant(Signals_Correct (Segment_State,Signal_State  ));
      As_Put_Line(" ");
      Print_State(Segment_State,Signal_State);
      As_Put_Line(" ");

      Get_Action(Route, The_Mode);

      if The_Mode = Mode_Open
      then
	 Open(Segment_State, Signal_State,Route, Success);
      else
	 Move(Segment_State, Signal_State, Route, Success);
      end if;

      if Success
      then
	 As_Put_Line("*** Success *** ");
      else
	 As_Put_Line("*** Route not Allowed *** ");
      end if;
   end loop;
end Main;
