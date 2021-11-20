pragma SPARK_Mode (On);

with Spark.Text_Io; use Spark.Text_Io;

package Road_Intersection is

   type Mode is (Mode_Open, Mode_Move);

   type Route_Type is (Route_South_North,
                       Route_South_East,
                       Route_South_West,
                       Route_North_South,
                       Route_North_East,
                       Route_North_West,
                       Route_West_East,
                       Route_West_South,
                       Route_West_North,
                       Route_East_West,
                       Route_East_North,
                       Route_East_South,
                       Route_Leave_North,
                       Route_Enter_North,
                       Route_Leave_South,
                       Route_Enter_South,
                       Route_Leave_West,
                       Route_Enter_West,
                       Route_Leave_East,
                       Route_Enter_East);

   type One_Signal_State is (Red,Green);

   type One_Segment_State is (Occupied_Standing,
			      Occupied_Moving_West,
                              Occupied_Moving_North,
                              Occupied_Moving_East,
                              Occupied_Moving_South,
			      Reserved_Moving_From_East,
                              Reserved_Moving_From_South,
                              Reserved_Moving_From_North,
                              Reserved_Moving_From_West,
                              Free);

   type Segment_State_Type is
      record
         North,
         South,
         West,
         East: One_Segment_State;
      end record;

   type Signal_State_Type is
      record
         South_West,
         South_East,
         South_North,
         West_North,
         West_South,
         West_East,
         North_South,
         North_West,
         North_East,
         East_West,
         East_South,
         East_North,
         Leave_South,
         Enter_South,
         Leave_North,
         Enter_North,
         Leave_East,
         Enter_East,
         Leave_West,
         Enter_West: One_Signal_State;
      end record;

   function Signals_Correct (Segment_State : Segment_State_Type;
			     Signal_State  : Signal_State_Type)
                             return Boolean is
      ((if Signal_State.South_West = Green
        then (Segment_State.West = Reserved_Moving_From_South
                  and Segment_State.South = Occupied_Moving_West))

       and
         (if Signal_State.South_East = Green
          then (Segment_State.East = Reserved_Moving_From_South
                  and Segment_State.South = Occupied_Moving_East))

       and
         (if Signal_State.South_North = Green
          then (Segment_State.North = Reserved_Moving_From_South
                and Segment_State.South = Occupied_Moving_North))

       and
         (if Signal_State.West_North = Green
          then (Segment_State.North = Reserved_Moving_From_West
                and Segment_State.West = Occupied_Moving_North))

       and
         (if Signal_State.West_South = Green
          then (Segment_State.South = Reserved_Moving_From_West
                and Segment_State.West = Occupied_Moving_South))

       and
         (if Signal_State.West_East = Green
          then (Segment_State.East = Reserved_Moving_From_West
                and Segment_State.West = Occupied_Moving_East))

       and
         (if Signal_State.North_South = Green
          then (Segment_State.South = Reserved_Moving_From_North
                and Segment_State.North = Occupied_Moving_South))

       and
         (if Signal_State.North_West = Green
          then (Segment_State.West = Reserved_Moving_From_North
                and Segment_State.North = Occupied_Moving_West))

       and
         (if Signal_State.North_East = Green
          then (Segment_State.East = Reserved_Moving_From_North
                and Segment_State.North = Occupied_Moving_East))

       and
         (if Signal_State.East_West = Green
          then (Segment_State.West = Reserved_Moving_From_East
                and Segment_State.East = Occupied_Moving_West))

       and
         (if Signal_State.East_South = Green
          then (Segment_State.South = Reserved_Moving_From_East
                and Segment_State.East = Occupied_Moving_South))

       and
         (if Signal_State.East_North = Green
          then (Segment_State.North = Reserved_Moving_From_East
                and Segment_State.East = Occupied_Moving_North))

       and
        (if Signal_State.Leave_South = Green
         then Segment_State.South = Occupied_Moving_South)

       and
        (if Signal_State.Enter_South = Green
         then Segment_State.South = Reserved_Moving_From_South)

       and
        (if Signal_State.Leave_North = Green
         then Segment_State.North = Occupied_Moving_North)

       and
        (if Signal_State.Enter_North = Green
         then Segment_State.North = Reserved_Moving_From_North)

       and
        (if Signal_State.Leave_West = Green
         then Segment_State.West = Occupied_Moving_West)

       and
        (if Signal_State.Enter_West = Green
         then Segment_State.West = Reserved_Moving_From_West)

       and
        (if Signal_State.Leave_East = Green
         then Segment_State.East = Occupied_Moving_East)

       and
        (if Signal_State.Enter_East = Green
         then Segment_State.East = Reserved_Moving_From_East)
      );


   function Segment_Occupied(Segment_State : One_Segment_State) return Boolean is
       (Segment_State = Occupied_Moving_West or
        Segment_State = Occupied_Moving_East or
        Segment_State = Occupied_Moving_North or
        Segment_State = Occupied_Moving_South or
        Segment_State = Occupied_Standing);


   function Traffic_Stay (Segment_State_Old,
			  Segment_State_New : Segment_State_Type)
			   return Boolean is
	 ((if Segment_Occupied(Segment_State_Old.North)
	    then  Segment_Occupied(Segment_State_New.North)) and
	  (if Segment_Occupied(Segment_State_Old.South)
            then  Segment_Occupied(Segment_State_New.South)) and
         (if Segment_Occupied(Segment_State_Old.West)
	    then  Segment_Occupied(Segment_State_New.West)) and
	  (if Segment_Occupied(Segment_State_Old.East)
            then  Segment_Occupied(Segment_State_New.East)));

   function Move_Traffic_Correct (Segment_State_Old,
                                  Segment_State_New : Segment_State_Type;
				  Route: Route_Type)
                                  return Boolean is
      ----- Segment North ------
      ((if Segment_State_Old.North = Occupied_Moving_South
         then
            (Segment_State_New.North = Occupied_Moving_South
             or
            (Route = Route_North_South
		    and Segment_State_Old.South = Reserved_Moving_From_North
		    and Segment_State_New.North = Free
                    and Segment_State_New.South = Occupied_Standing)))
       and
       (if Segment_State_Old.North = Occupied_Moving_East
         then
            (Segment_State_New.North = Occupied_Moving_East
             or
            (Route = Route_North_East
		    and Segment_State_Old.East = Reserved_Moving_From_North
		    and Segment_State_New.North = Free
             and Segment_State_New.East = Occupied_Standing)))
       and
       (if Segment_State_Old.North = Occupied_Moving_West
         then
            (Segment_State_New.North = Occupied_Moving_West
             or
            (Route = Route_North_West
		    and Segment_State_Old.West = Reserved_Moving_From_North
		    and Segment_State_New.North = Free
                    and Segment_State_New.West = Occupied_Standing)))
       and
       (if Segment_State_Old.North = Occupied_Moving_North
         then
            (Segment_State_New.North = Occupied_Moving_North
             or
            (Route = Route_Leave_North
                    and Segment_State_New.North = Free)))
       and
       (if Segment_State_Old.North = Occupied_Standing
           then
           Segment_State_New.North = Occupied_Standing)

       ------- Segment South -------
       and
       (if Segment_State_Old.South = Occupied_Moving_North
         then
            (Segment_State_New.South = Occupied_Moving_North
             or
            (Route = Route_South_North
		    and Segment_State_Old.North = Reserved_Moving_From_South
		    and Segment_State_New.South = Free
                    and Segment_State_New.North = Occupied_Standing)))
       and
       (if Segment_State_Old.South = Occupied_Moving_East
         then
            (Segment_State_New.South = Occupied_Moving_East
             or
            (Route = Route_South_East
		    and Segment_State_Old.East = Reserved_Moving_From_South
		    and Segment_State_New.South = Free
                    and Segment_State_New.East = Occupied_Standing)))
       and
       (if Segment_State_Old.South = Occupied_Moving_West
         then
            (Segment_State_New.South = Occupied_Moving_West
             or
            (Route = Route_South_West
		    and Segment_State_Old.West = Reserved_Moving_From_South
		    and Segment_State_New.South = Free
                    and Segment_State_New.West = Occupied_Standing)))
       and
       (if Segment_State_Old.South = Occupied_Moving_South
         then
            (Segment_State_New.South = Occupied_Moving_South
             or
            (Route = Route_Leave_South
                    and Segment_State_New.South = Free)))
       and
       (if Segment_State_Old.South = Occupied_Standing
           then
           Segment_State_New.South = Occupied_Standing)

       -------- Segment East ---------
       and
       (if Segment_State_Old.East = Occupied_Moving_North
         then
            (Segment_State_New.East = Occupied_Moving_North
             or
            (Route = Route_East_North
		    and Segment_State_Old.North = Reserved_Moving_From_East
		    and Segment_State_New.East = Free
                    and Segment_State_New.North = Occupied_Standing)))
       and
       (if Segment_State_Old.East = Occupied_Moving_South
         then
            (Segment_State_New.East = Occupied_Moving_South
             or
            (Route = Route_East_South
		    and Segment_State_Old.South = Reserved_Moving_From_East
		    and Segment_State_New.East = Free
                    and Segment_State_New.South = Occupied_Standing)))
       and
       (if Segment_State_Old.East = Occupied_Moving_West
         then
            (Segment_State_New.East = Occupied_Moving_West
             or
            (Route = Route_East_West
		    and Segment_State_Old.West = Reserved_Moving_From_East
		    and Segment_State_New.East = Free
                    and Segment_State_New.West = Occupied_Standing)))
       and
       (if Segment_State_Old.East = Occupied_Moving_East
         then
            (Segment_State_New.East = Occupied_Moving_East
             or
            (Route = Route_Leave_East
                    and Segment_State_New.East = Free)))
       and
       (if Segment_State_Old.East = Occupied_Standing
           then
           Segment_State_New.East = Occupied_Standing)

       --------- Segment West --------
       and
       (if Segment_State_Old.West = Occupied_Moving_North
         then
            (Segment_State_New.West = Occupied_Moving_North
             or
            (Route = Route_West_North
		    and Segment_State_Old.North = Reserved_Moving_From_West
		    and Segment_State_New.West = Free
                    and Segment_State_New.North = Occupied_Standing)))
       and
       (if Segment_State_Old.West = Occupied_Moving_South
         then
            (Segment_State_New.West = Occupied_Moving_South
             or
            (Route = Route_West_South
		    and Segment_State_Old.South = Reserved_Moving_From_West
		    and Segment_State_New.West = Free
                    and Segment_State_New.South = Occupied_Standing)))
       and
       (if Segment_State_Old.West = Occupied_Moving_East
         then
            (Segment_State_New.West = Occupied_Moving_East
             or
            (Route = Route_West_East
		    and Segment_State_Old.East = Reserved_Moving_From_West
		    and Segment_State_New.West = Free
                    and Segment_State_New.East = Occupied_Standing)))
       and
       (if Segment_State_Old.West = Occupied_Moving_West
         then
            (Segment_State_New.West = Occupied_Moving_West
             or
            (Route = Route_Leave_West
                    and Segment_State_New.West = Free)))
       and
       (if Segment_State_Old.West = Occupied_Standing
           then
           Segment_State_New.West = Occupied_Standing));


   procedure Init(Segment_State : out Segment_State_Type;
                  Signal_State  : out Signal_State_Type)
     With
     Depends => (Signal_State => null,
                 Segment_State => null),
     Post => (Signals_Correct(Segment_State,Signal_State));


   procedure Open(Segment_State : in out Segment_State_Type;
                  Signal_State  : in out Signal_State_Type;
                  Route         : in Route_Type;
                  Success       : out Boolean)
     with
     Depends => (Segment_State => (Segment_State,Route),
                 Success       => (Segment_State,Route),
                 Signal_State  => (Segment_State,Route,Signal_State)),
     Pre => (Signals_Correct(Segment_State,Signal_State)),
     Post=> (Signals_Correct(Segment_State,Signal_State) and
             Traffic_Stay(Segment_State'Old,Segment_State));


   procedure Move(Segment_State : in out Segment_State_Type;
                  Signal_State  : in out Signal_State_Type;
                  Route         : in Route_Type;
                  Success       : out Boolean)
     with
     Depends => (Signal_State  =>  (Segment_State, Route, Signal_State),
                 Segment_State =>  (Segment_State, Route, Signal_State),
                 Success       =>  (Segment_State, Route, Signal_State)),
     Pre => (Signals_Correct(Segment_State,Signal_State)),
     Post=> (Signals_Correct(Segment_State,Signal_State) and
                     Move_Traffic_Correct(Segment_State'Old,Segment_State,Route));


   procedure Print_One_Signal(The_Signal_State: in One_Signal_State)
     with
     Global => (In_Out => Standard_Output),
     Depends => (Standard_Output => (Standard_Output, The_Signal_State));


   procedure Print_One_Segment_State(the_State: in One_Segment_State)
     with
     Global => (In_Out => Standard_Output),
     Depends => (Standard_Output => (Standard_Output, The_State));


   procedure Print_State(Segment_State : in Segment_State_Type;
                         Signal_State  : in Signal_State_Type)
     with
     Global => (In_Out => Standard_Output),
     Depends => (Standard_Output => (Standard_Output, Segment_State, Signal_State));


   procedure Get_Action(Route    : out Route_Type;
                        The_Mode : out Mode)
     with
     Global => (In_Out => (Standard_Input,Standard_Output)),
     Depends => (Standard_Output => (Standard_Output, Standard_Input),
                 Standard_Input =>   Standard_Input,
                 Route =>   Standard_Input,
                 The_Mode =>   Standard_Input);

end Road_Intersection;
