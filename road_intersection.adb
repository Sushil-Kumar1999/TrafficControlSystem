pragma SPARK_Mode (On);

with AS_Io_Wrapper; use AS_Io_Wrapper;

package body Road_Intersection is

   procedure Init(Segment_State : out Segment_State_Type;
		  Signal_State  : out Signal_State_Type)  is
   begin
      Signal_State := Signal_State_Type'(South_West => Red,
                                         South_East => Red,
                                         South_North => Red,
                                         West_North => Red,
                                         West_South => Red,
                                         West_East => Red,
                                         North_South => Red,
                                         North_West => Red,
                                         North_East => Red,
                                         East_West => Red,
                                         East_South => Red,
                                         East_North => Red,
                                         Leave_South => Red,
                                         Enter_South => Red,
                                         Leave_North => Red,
                                         Enter_North => Red,
                                         Leave_East => Red,
                                         Enter_East => Red,
                                         Leave_West => Red,
                                         Enter_West => Red);

      Segment_State := Segment_State_Type'(North => Free,
                                           South => Free,
                                           West => Free,
                                           East => Free);
   end Init;

   procedure Open(Segment_State : in out Segment_State_Type;
		  Signal_State  : in out Signal_State_Type;
		  Route: in Route_Type;
		  Success: out Boolean) is
   begin
      Success := False;
      if Route = Route_South_West
          then
             if (Segment_State.South = Occupied_Standing
                 and
                 Segment_State.West = Free)
             then
                Segment_State.South := Occupied_Moving_West;
                Segment_State.West := Reserved_Moving_From_South;
                Signal_State.South_West := Green;
                Success := True;
             else
                Success := False;
             end if;
      elsif Route = Route_South_East
          then
             if (Segment_State.South = Occupied_Standing
                 and Segment_State.East = Free)
             then
                Segment_State.South := Occupied_Moving_East;
                Segment_State.East := Reserved_Moving_From_South;
                Signal_State.South_East := Green;
                Success := True;
             else
                Success := False;
             end if;
      elsif Route = Route_South_North
          then
             if (Segment_State.South = Occupied_Standing
                 and Segment_State.North = Free
                 and Segment_State.West /= Reserved_Moving_From_East
                 and Segment_State.East /= Reserved_Moving_From_West)
             then
                Segment_State.South := Occupied_Moving_North;
                Segment_State.North := Reserved_Moving_From_South;
                Signal_State.South_North := Green;
                Success := True;
             else
                Success := False;
             end if;
      elsif Route = Route_West_North
          then
             if (Segment_State.West = Occupied_Standing
                 and Segment_State.North = Free)
             then
                Segment_State.West := Occupied_Moving_North;
                Segment_State.North := Reserved_Moving_From_West;
                Signal_State.West_North := Green;
                Success := True;
             else
                Success := False;
             end if;
      elsif Route = Route_West_South
          then
             if (Segment_State.West = Occupied_Standing
                 and Segment_State.South = Free)
             then
                Segment_State.West := Occupied_Moving_South;
                Segment_State.South := Reserved_Moving_From_West;
                Signal_State.West_South := Green;
                Success := True;
             else
                Success := False;
             end if;
      elsif Route = Route_West_East
          then
             if (Segment_State.West = Occupied_Standing
                 and Segment_State.East = Free
                 and Segment_State.South /= Reserved_Moving_From_North
                 and Segment_State.North /= Reserved_Moving_From_South)
             then
                Segment_State.West := Occupied_Moving_East;
                Segment_State.East := Reserved_Moving_From_West;
                Signal_State.West_East := Green;
                Success := True;
             else
                Success := False;
             end if;
      elsif Route = Route_North_South
          then
             if (Segment_State.North = Occupied_Standing
                 and Segment_State.South = Free
                 and Segment_State.West /= Reserved_Moving_From_East
                 and Segment_State.East /= Reserved_Moving_From_West)
             then
                Segment_State.North := Occupied_Moving_South;
                Segment_State.South := Reserved_Moving_From_North;
                Signal_State.North_South := Green;
                Success := True;
             else
                Success := False;
             end if;
      elsif Route = Route_North_East
          then
             if (Segment_State.North = Occupied_Standing
                 and Segment_State.East = Free)
             then
                Segment_State.North := Occupied_Moving_East;
                Segment_State.East := Reserved_Moving_From_North;
                Signal_State.North_East := Green;
                Success := True;
             else
                Success := False;
             end if;
      elsif Route = Route_North_West
          then
             if (Segment_State.North = Occupied_Standing
                 and Segment_State.West = Free)
             then
                Segment_State.North := Occupied_Moving_West;
                Segment_State.West := Reserved_Moving_From_North;
                Signal_State.North_West := Green;
                Success := True;
             else
                Success := False;
             end if;
      elsif Route = Route_East_West
          then
             if (Segment_State.East = Occupied_Standing
                 and Segment_State.West = Free
                 and Segment_State.South /= Reserved_Moving_From_North
                 and Segment_State.North /= Reserved_Moving_From_South)
             then
                Segment_State.East := Occupied_Moving_West;
                Segment_State.West := Reserved_Moving_From_East;
                Signal_State.East_West := Green;
                Success := True;
             else
                Success := False;
            end if;
      elsif Route = Route_East_South
          then
             if (Segment_State.East = Occupied_Standing
                 and Segment_State.South = Free)
             then
                Segment_State.East := Occupied_Moving_South;
                Segment_State.South := Reserved_Moving_From_East;
                Signal_State.East_South := Green;
                Success := True;
             else
                Success := False;
             end if;
      elsif Route = Route_East_North
          then
             if (Segment_State.East = Occupied_Standing
                 and Segment_State.North = Free)
             then
                Segment_State.East := Occupied_Moving_North;
                Segment_State.North := Reserved_Moving_From_East;
                Signal_State.East_North := Green;
                Success := True;
             else
                Success := False;
         end if;
      elsif Route = Route_Enter_South
            then
               if Segment_State.South = Free
               then
                  Segment_State.South := Reserved_Moving_From_South;
                  Signal_State.Enter_South:= Green;
                  Success := True;
               else
                  Success := False;
               end if;
      elsif Route = Route_Enter_North
            then
               if Segment_State.North = Free
               then
                  Segment_State.North := Reserved_Moving_From_North;
                  Signal_State.Enter_North:= Green;
                  Success := True;
               else
                  Success := False;
               end if;
      elsif Route = Route_Enter_East
            then
               if Segment_State.East = Free
               then
                  Segment_State.East := Reserved_Moving_From_East;
                  Signal_State.Enter_East:= Green;
                  Success := True;
               else
                  Success := False;
               end if;
      elsif Route = Route_Enter_West
            then
               if Segment_State.West = Free
               then
                  Segment_State.West := Reserved_Moving_From_West;
                  Signal_State.Enter_West:= Green;
                  Success := True;
               else
                  Success := False;
               end if;
      elsif Route = Route_Leave_North
          then
             if Segment_State.North = Occupied_Standing
               then
                  Segment_State.North := Occupied_Moving_North;
                  Signal_State.Leave_North := Green;
                  Success := True;
               else
                  Success := False;
             end if;
      elsif Route = Route_Leave_South
          then
             if Segment_State.South = Occupied_Standing
               then
                  Segment_State.South := Occupied_Moving_South;
                  Signal_State.Leave_South := Green;
                  Success := True;
               else
                  Success := False;
         end if;
      elsif Route = Route_Leave_East
          then
             if Segment_State.East = Occupied_Standing
               then
                  Segment_State.East := Occupied_Moving_East;
                  Signal_State.Leave_East := Green;
                  Success := True;
               else
                  Success := False;
             end if;
      elsif Route = Route_Leave_West
          then
             if Segment_State.West = Occupied_Standing
               then
                  Segment_State.West := Occupied_Moving_West;
                  Signal_State.Leave_West := Green;
                  Success := True;
               else
                  Success := False;
              end if;
      end if;
   end Open;

   procedure Move(Segment_State : in out Segment_State_Type;
		   Signal_State  : in out Signal_State_Type;
		   Route: in Route_Type;
		   Success: out Boolean) is
   begin
      Success := False;
      if Route = Route_South_West
          then
             if (Segment_State.South = Occupied_Moving_West
                 and
 		  Segment_State.West = Reserved_Moving_From_South
 		  and
 	          Signal_State.South_West = Green)
             then
                Signal_State.South_West := Red;
                Segment_State.South := Free;
                Segment_State.West := Occupied_Standing;
                Success := True;
             else
                Success := False;
         end if;
      elsif Route = Route_South_North
          then
             if (Segment_State.South = Occupied_Moving_North
                 and
 		  Segment_State.North = Reserved_Moving_From_South
 		  and
 	          Signal_State.South_North = Green)
             then
                Signal_State.South_North := Red;
                Segment_State.South := Free;
                Segment_State.North := Occupied_Standing;
                Success := True;
             else
                Success := False;
         end if;
      elsif Route = Route_South_East
          then
             if (Segment_State.South = Occupied_Moving_East
                 and
 		  Segment_State.East = Reserved_Moving_From_South
 		  and
 	          Signal_State.South_East = Green)
             then
                Signal_State.South_East := Red;
                Segment_State.South := Free;
                Segment_State.East := Occupied_Standing;
                Success := True;
             else
                Success := False;
         end if;
      elsif Route = Route_West_East
          then
             if (Segment_State.West = Occupied_Moving_East
                 and
 		  Segment_State.East = Reserved_Moving_From_West
 		  and
 	          Signal_State.West_East = Green)
             then
                Signal_State.West_East := Red;
                Segment_State.West := Free;
                Segment_State.East := Occupied_Standing;
                Success := True;
             else
                Success := False;
             end if;
      elsif Route = Route_West_South
          then
             if (Segment_State.West = Occupied_Moving_South
                 and
 		  Segment_State.South = Reserved_Moving_From_West
 		  and
 	          Signal_State.West_South = Green)
             then
                Signal_State.West_South := Red;
                Segment_State.West := Free;
                Segment_State.South := Occupied_Standing;
                Success := True;
             else
                Success := False;
             end if;
      elsif Route = Route_West_North
          then
             if (Segment_State.West = Occupied_Moving_North
                 and
 		  Segment_State.North = Reserved_Moving_From_West
 		  and
 	          Signal_State.West_North = Green)
             then
                Signal_State.West_North := Red;
                Segment_State.West := Free;
                Segment_State.North := Occupied_Standing;
                Success := True;
             else
                Success := False;
             end if;
      elsif Route = Route_North_East
          then
             if (Segment_State.North = Occupied_Moving_East
                 and
 		  Segment_State.East = Reserved_Moving_From_North
 		  and
 	          Signal_State.North_East = Green)
             then
                Signal_State.North_East := Red;
                Segment_State.North := Free;
                Segment_State.East := Occupied_Standing;
                Success := True;
             else
                Success := False;
             end if;
      elsif Route = Route_North_West
          then
             if (Segment_State.North = Occupied_Moving_West
                 and
 		  Segment_State.West = Reserved_Moving_From_North
 		  and
 	          Signal_State.North_West = Green)
             then
                Signal_State.North_West := Red;
                Segment_State.North := Free;
                Segment_State.West := Occupied_Standing;
                Success := True;
             else
                Success := False;
             end if;
      elsif Route = Route_North_South
          then
             if (Segment_State.North = Occupied_Moving_South
                 and
 		  Segment_State.South = Reserved_Moving_From_North
 		  and
 	          Signal_State.North_South = Green)
             then
                Signal_State.North_South := Red;
                Segment_State.North := Free;
                Segment_State.South := Occupied_Standing;
                Success := True;
             else
                Success := False;
             end if;
      elsif Route = Route_East_West
          then
             if (Segment_State.East = Occupied_Moving_West
                 and
 		  Segment_State.West = Reserved_Moving_From_East
 		  and
 	          Signal_State.East_West = Green)
             then
                Signal_State.East_West := Red;
                Segment_State.East := Free;
                Segment_State.West := Occupied_Standing;
                Success := True;
             else
                Success := False;
             end if;
      elsif Route = Route_East_South
          then
             if (Segment_State.East = Occupied_Moving_south
                 and
 		  Segment_State.South = Reserved_Moving_From_East
 		  and
 	          Signal_State.East_South = Green)
             then
                Signal_State.East_South := Red;
                Segment_State.East := Free;
                Segment_State.South := Occupied_Standing;
                Success := True;
             else
                Success := False;
             end if;
      elsif Route = Route_East_North
          then
             if (Segment_State.East = Occupied_Moving_North
                 and
 		  Segment_State.North = Reserved_Moving_From_East
 		  and
 	          Signal_State.East_North = Green)
             then
                Signal_State.East_North := Red;
                Segment_State.East := Free;
                Segment_State.North := Occupied_Standing;
                Success := True;
             else
                Success := False;
             end if;
      elsif Route = Route_Enter_East
            then
               if (Segment_State.East = Reserved_Moving_From_East
 		  and Signal_State.Enter_East = Green)
               then
                  Signal_State.Enter_East := Red;
                  Segment_State.East := Occupied_Standing;
                  Success := True;
               else
                  Success := False;
               end if;
      elsif Route = Route_Enter_West
            then
               if (Segment_State.West = Reserved_Moving_From_West
 		  and Signal_State.Enter_West = Green)
               then
                  Signal_State.Enter_West := Red;
                  Segment_State.West := Occupied_Standing;
                  Success := True;
               else
                  Success := False;
               end if;
      elsif Route = Route_Enter_North
            then
               if (Segment_State.North = Reserved_Moving_From_North
 		  and Signal_State.Enter_North = Green)
               then
                  Signal_State.Enter_North := Red;
                  Segment_State.North := Occupied_Standing;
                  Success := True;
               else
                  Success := False;
               end if;
      elsif Route = Route_Enter_South
            then
               if (Segment_State.South = Reserved_Moving_From_South
 		  and Signal_State.Enter_South = Green)
               then
                  Signal_State.Enter_South := Red;
                  Segment_State.South := Occupied_Standing;
                  Success := True;
               else
                  Success := False;
               end if;
      elsif Route = Route_Leave_East
          then
             if (Segment_State.East = Occupied_Moving_East
 		   and Signal_State.Leave_East = Green)
             then
                  Signal_State.Leave_East := Red;
                  Segment_State.East := Free;
                  Success := True;
             else
                  Success := False;
             end if;
      elsif Route = Route_Leave_West
          then
             if (Segment_State.West = Occupied_Moving_West
 		   and Signal_State.Leave_West = Green)
               then
                  Signal_State.Leave_West := Red;
                  Segment_State.West := Free;
                  Success := True;
               else
                  Success := False;
             end if;
      elsif Route = Route_Leave_North
          then
             if (Segment_State.North = Occupied_Moving_North
 		   and Signal_State.Leave_North = Green)
               then
                  Signal_State.Leave_North := Red;
                  Segment_State.North := Free;
                  Success := True;
               else
                  Success := False;
         end if;
      elsif Route = Route_Leave_South
          then
             if (Segment_State.South = Occupied_Moving_South
 		   and Signal_State.Leave_South = Green)
               then
                  Signal_State.Leave_South := Red;
                  Segment_State.South := Free;
                  Success := True;
               else
                  Success := False;
             end if;
      end if;
   end Move;

    procedure Print_One_Signal(The_Signal_State: in One_Signal_State) is
    begin
       case The_Signal_State is
	  when Red => AS_Put("Red");
	  when Green => AS_Put("Green");
       end case;
   end Print_One_Signal;

   procedure Print_One_Segment_State(the_State: in One_Segment_State) is
   begin
       case The_State is
         when Occupied_Standing => AS_Put("Occupied_Standing");
         when Occupied_Moving_East => AS_Put("Occupied_Moving_East");
         when Occupied_Moving_West => AS_Put("Occupied_Moving_West");
         when Occupied_Moving_North => AS_Put("Occupied_Moving_North");
         when Occupied_Moving_South => AS_Put("Occupied_Moving_South");
         when Reserved_Moving_From_East => AS_Put("Reserved_Moving_From_East");
         when Reserved_Moving_From_West => AS_Put("Reserved_Moving_From_West");
         when Reserved_Moving_From_North => AS_Put("Reserved_Moving_From_North");
         when Reserved_Moving_From_South => AS_Put("Reserved_Moving_From_South");
         when Free => AS_Put("Free");
       end case;
   end Print_One_Segment_State;

   procedure Print_State(Segment_State : in Segment_State_Type;
		         Signal_State  : in Signal_State_Type) is
   begin
       AS_Put_Line("-------------- North side -------------------");
       AS_Put_Line(" ");
       AS_Put("Signal Enter_North : ");
       Print_One_Signal(Signal_State.Enter_North);
       AS_Put_Line(" ");
       AS_Put("Signal Leave_North : ");
       Print_One_Signal(Signal_State.Leave_North);
       AS_Put_Line(" ");
       AS_Put("Segment North : ");
       Print_One_Segment_State(Segment_State.North);
       AS_Put_Line(" ");
       AS_Put("Signal North_West : ");
       Print_One_Signal(Signal_State.North_West);
       AS_Put_Line(" ");
       AS_Put("Signal North_East : ");
       Print_One_Signal(Signal_State.North_East);
       AS_Put_Line(" ");
       AS_Put("Signal North_South : ");
       Print_One_Signal(Signal_State.North_South);
       AS_Put_Line(" ");

       AS_Put_Line(" ");
       AS_Put_Line("----------------- South side ------------------");
       AS_Put_Line(" ");
       AS_Put("Signal Enter_South : ");
       Print_One_Signal(Signal_State.Enter_South);
       AS_Put_Line(" ");
       AS_Put("Signal Leave_South : ");
       Print_One_Signal(Signal_State.Leave_South);
       AS_Put_Line(" ");
       AS_Put("Segment South : ");
       Print_One_Segment_State(Segment_State.South);
       AS_Put_Line(" ");
       AS_Put("Signal South_West : ");
       Print_One_Signal(Signal_State.South_West);
       AS_Put_Line(" ");
       AS_Put("Signal South_East : ");
       Print_One_Signal(Signal_State.South_East);
       AS_Put_Line(" ");
       AS_Put("Signal South_North : ");
       Print_One_Signal(Signal_State.South_North);
       AS_Put_Line(" ");

       AS_Put_Line(" ");
       AS_Put_Line("------------------ East side ---------------------");
       AS_Put_Line(" ");
       AS_Put("Signal Enter_East : ");
       Print_One_Signal(Signal_State.Enter_East);
       AS_Put_Line(" ");
       AS_Put("Signal Leave_East : ");
       Print_One_Signal(Signal_State.Leave_East);
       AS_Put_Line(" ");
       AS_Put("Segment East : ");
       Print_One_Segment_State(Segment_State.East);
       AS_Put_Line(" ");
       AS_Put("Signal East_West : ");
       Print_One_Signal(Signal_State.East_West);
       AS_Put_Line(" ");
       AS_Put("Signal East_North : ");
       Print_One_Signal(Signal_State.East_North);
       AS_Put_Line(" ");
       AS_Put("Signal East_South : ");
       Print_One_Signal(Signal_State.East_South);
       AS_Put_Line(" ");

       AS_Put_Line(" ");
       AS_Put_Line("------------------- West side ---------------------");
       AS_Put_Line(" ");
       AS_Put("Signal Enter_West : ");
       Print_One_Signal(Signal_State.Enter_West);
       AS_Put_Line(" ");
       AS_Put("Signal Leave_West : ");
       Print_One_Signal(Signal_State.Leave_West);
       AS_Put_Line(" ");
       AS_Put("Segment West : ");
       Print_One_Segment_State(Segment_State.West);
       AS_Put_Line(" ");
       AS_Put("Signal West_East : ");
       Print_One_Signal(Signal_State.West_East);
       AS_Put_Line(" ");
       AS_Put("Signal West_North : ");
       Print_One_Signal(Signal_State.West_North);
       AS_Put_Line(" ");
       AS_Put("Signal West_South : ");
       Print_One_Signal(Signal_State.West_South);
       AS_Put_Line(" ");
   end Print_State;

   procedure Get_Action(Route: out Route_Type;
                        The_Mode: out Mode) is
      subtype Index_3 is Integer range 1 .. 3;
      subtype String_3 is String(Index_3);
      Item : String_3;
      subtype Index_1 is Integer range 1 .. 1;
      subtype String_1 is String(Index_1);
      Item_1 : String_1;
      subtype Index_2 is Integer range 1 .. 2;
      subtype String_2 is String(Index_2);
      Item_2 : String_2;

      Stop : Integer;
      Success_Route: Boolean;
      Success_Mode: Boolean;

   begin
       loop
          AS_Put_Line("Select Mode and Route");
          AS_Put_Line("First Letter O for Open, M for Move");
          AS_Put_Line("Letters 2 and 3 as follows");
          AS_Put_Line("SN for South_North");
          AS_Put_Line("SE for South_East");
          AS_Put_Line("SW for South_West");
          AS_Put_Line("NS for North_South");
          AS_Put_Line("NE for North_East");
          AS_Put_Line("NW for North_West");
          AS_Put_Line("WE for West_East");
          AS_Put_Line("WN for West_North");
          AS_Put_Line("WS for West_South");
          AS_Put_Line("EW for East_West");
          AS_Put_Line("ES for East_South");
          AS_Put_Line("EN for East_North");
          AS_Put_Line("eN for Enter_North");
          AS_Put_Line("eS for Enter_South");
          AS_Put_Line("eE for Enter_East");
          AS_Put_Line("eW for East_West");
          AS_Put_Line("lN for Leave_North");
          AS_Put_Line("lS for Leave_South");
          AS_Put_Line("lE for Leave_East");
          AS_Put_Line("lW for Leave_West");
          loop
             AS_Get_Line(Item, Stop);
             exit when not (Stop = 0);
          end loop;

          Success_Mode  := False;
          Item_1 := " ";
          Item_1(1):= Item(1);
          Item_2 := "  ";
          Item_2(1):= Item(2);
          Item_2(2):= Item(3);
          The_Mode := Mode_Open;

          if Item_1 = "O"
          then
             The_Mode := Mode_Open;
             Success_Mode:= True;
          elsif Item_1 = "M"
          then
             The_Mode := Mode_Move;
             Success_Mode:= True;
          else
             AS_Put_Line("*** Mode (first letter) not valid. Should be O/M***");
          end if;

          Route := Route_South_North;
          if Item_2 = "SN"
          then
             Success_Route := True;
             Route := Route_South_North;
          elsif Item_2 = "SE"
          then
             Success_Route := True;
             Route := Route_South_East;
          elsif Item_2 = "SW"
          then
             Success_Route := True;
             Route := Route_South_West;
          elsif Item_2 = "NS"
          then
             Success_Route := True;
             Route := Route_North_South;
          elsif Item_2 = "NE"
          then
             Success_Route := True;
             Route := Route_North_East;
          elsif Item_2 = "NW"
          then
             Success_Route := True;
             Route := Route_North_West;
          elsif Item_2 = "WE"
          then
             Success_Route := True;
             Route := Route_West_East;
          elsif Item_2 = "WN"
          then
             Success_Route := True;
             Route := Route_West_North;
          elsif Item_2 = "WS"
          then
             Success_Route := True;
             Route := Route_West_South;
          elsif Item_2 = "EW"
          then
             Success_Route := True;
             Route := Route_East_West;
          elsif Item_2 = "EN"
          then
             Success_Route := True;
             Route := Route_East_North;
          elsif Item_2 = "ES"
          then
             Success_Route := True;
             Route := Route_East_South;
          elsif Item_2 = "eN"
          then
             Success_Route := True;
             Route := Route_Enter_North;
          elsif Item_2 = "eS"
          then
             Success_Route := True;
             Route := Route_Enter_South;
          elsif Item_2 = "eE"
          then
             Success_Route := True;
             Route := Route_Enter_East;
          elsif Item_2 = "eW"
          then
             Success_Route := True;
             Route := Route_Enter_West;
          elsif Item_2 = "lN"
          then
             Success_Route := True;
             Route := Route_Leave_North;
          elsif Item_2 = "lS"
          then
             Success_Route := True;
             Route := Route_Leave_South;
          elsif Item_2 = "lE"
          then
             Success_Route := True;
             Route := Route_Leave_East;
          elsif Item_2 = "lW"
          then
             Success_Route := True;
             Route := Route_Leave_West;
          else
             Success_Route:= False;
             AS_Put_Line("*** Route (letter 2 and 3) not valid ***");
          end if;
          exit when (Success_Route and Success_Mode);
       end loop;
   end Get_Action;

end Road_Intersection;
