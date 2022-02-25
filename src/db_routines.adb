-- Copyright (c) 2021 Bob Goddard <git@1.git.bgcomp.co.uk>
--
-- This file is free software: you may copy, redistribute and/or modify it
-- under the terms of the GNU General Public License as published by the
-- Free Software Foundation, either version 2 of the License, or (at your
-- option) any later version.
--
-- This file is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

with Ada.Calendar;    use Ada.Calendar;
with Ada.Calendar.Conversions;
with Ada.Containers; use Ada.Containers;
with Ada.Strings;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Exceptions;
with Ada.Float_Text_IO;
with Ada.Text_IO;
with AdaBase;        use AdaBase;
with AdaBase.Results;
with AdaBase.Results.Sets;
with GNAT.Source_Info;
with GNAT.Traceback.Symbolic;
with GNAT.Traceback;
with Convert;
with List_Handlers;

package body DB_Routines is
   DR_Envir : AdaBase.Driver.Base.MySQL.MySQL_Driver;
   Trace    : GNAT.Traceback.Tracebacks_Array (1 .. 1_000);
   Length   : Natural;

   function DB_Connect return Local_Defs.Trilean is
      delay_time       : Duration          := 1.0;
      delay_time_count : Duration          := 0.0;
      target_time      : Ada.Calendar.Time := Ada.Calendar.Clock;
   begin
      DB_Disconnect;
      delay_loop :
      loop
         delay until target_time;

         if DB_Connect_Private = Local_Defs.TTrue then
            exit delay_loop;
         end if;

         if delay_time_count = delay_time then
            delay_time       := delay_time * 2.0;
            delay_time_count :=              0.0;
         else
            delay_time       := delay_time + 1.0;
         end if;

         target_time := target_time + delay_time;
         delay_time_count := delay_time_count + 1.0;
      end loop delay_loop;

      return Local_Defs.TTrue;
   end DB_Connect;

   function DB_Connect_Private return Local_Defs.Trilean is
   begin
      DR_Envir.basic_connect (database => Ada.Strings.Unbounded.To_String (Database),
                        username => Ada.Strings.Unbounded.To_String (DB_User),
                        password => Ada.Strings.Unbounded.To_String (DB_Pass),
                        hostname => Ada.Strings.Unbounded.To_String (DB_Host),
                        port     => DB_Port);

      return Local_Defs.TTrue;

   exception
      when E : others =>
         if Local_Defs.Do_Syslog then
            GNAT.Traceback.Call_Chain (Trace, Length);
            Ada.Text_IO.Put_Line ("WTF!! rivet "                      &
                                    GNAT.Source_Info.Source_Location  &
                                    " "                               &
                                    Ada.Exceptions.Exception_Name (E) &
                                    " message: "                      &
                                    Ada.Exceptions.Exception_Message (E));
            Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
         end if;

         return Local_Defs.TBroken;
   end DB_Connect_Private;

   procedure DB_Disconnect is
   begin
      DR_Envir.disconnect;
   end DB_Disconnect;

   procedure Execute_Statements is
      Num_Rows      : AdaBase.Affected_Rows := 0;
      SQL_Statement : Ada.Strings.Unbounded.Unbounded_String;
   begin
      while List_Handlers.SQL_Queue.Current_Use > 0 loop
         List_Handlers.SQL_Queue.Dequeue (SQL_Statement);
         Num_Rows := DR_Envir.execute (sql => Ada.Strings.Unbounded.To_String (SQL_Statement));

         if Num_Rows = 1 then
            DR_Envir.commit;
         end if;
      end loop;
   exception when E : others =>
         Ada.Text_IO.Put_Line ("Exception raised "                &
                                 " - "                            &
                                 GNAT.Source_Info.Source_Location &
                                 " "                              &
                                 Ada.Exceptions.Exception_Name (E));
   end Execute_Statements;

   function Get_Earliest_TS return Interfaces.C.long is
      Earliest : Interfaces.C.long := Interfaces.C.long'Last;
      Row      : AdaBase.Results.Sets.Datarow;
   begin
      for S of List_Handlers.Location_Environ_Map loop
         declare
            STMT_Env : Stmt_Type_Local := DR_Envir.prepare ("SELECT MIN(ts) FROM " &
                                                              Ada.Strings.Unbounded.To_String (List_Handlers.Location_Environ_Map.Element (S.Sensor).DBTable));
         begin
            if STMT_Env.execute then
               Row := STMT_Env.fetch_next;
            end if;
         exception
            when E : others =>
               GNAT.Traceback.Call_Chain (Trace, Length);
               Ada.Text_IO.Put_Line ("WTF!! rivet "                      &
                                       GNAT.Source_Info.Source_Location  &
                                       " "                               &
                                       Ada.Exceptions.Exception_Name (E) &
                                       " message: " & Ada.Exceptions.Exception_Message (E));
               Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
               return -1;
         end;

         if Interfaces.C.long (Row.column (1).as_byte4) < Earliest then
            Earliest := Interfaces.C.long (Row.column (1).as_byte4);
         end if;
      end loop;

      return Earliest;
   exception
      when E : others =>
         GNAT.Traceback.Call_Chain (Trace, Length);
         Ada.Text_IO.Put_Line ("WTF!! rivet "                      &
                                 GNAT.Source_Info.Source_Location  &
                                 " "                               &
                                 Ada.Exceptions.Exception_Name (E) &
                                 " message: "                      &
                                 Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
         return -1;
   end Get_Earliest_TS;

   procedure Load_Data (DB_Connect_Res : out Local_Defs.Trilean;
                        Start_Epoch    :     Ada.Calendar.Time;
                        End_Epoch      :     Ada.Calendar.Time;
                        DB_Table       :     Ada.Strings.Unbounded.Unbounded_String;
                        Min_And_Max    : out Local_Defs.MM) is
      SQL_Str             : constant String := "SELECT *"                                             &
                              " FROM "    & Ada.Strings.Unbounded.To_String (DB_Table)                &
                            " WHERE ts>=" & Ada.Calendar.Conversions.To_Unix_Time (Start_Epoch)'Image &
                              " AND ts<=" & Ada.Calendar.Conversions.To_Unix_Time   (End_Epoch)'Image  &
                            " ORDER BY ts";
      STMT_Data           :          Stmt_Type_Local := DR_Envir.prepare (SQL_Str);
      Num_Rows            :          AdaBase.Affected_Rows;
      Row                 :          AdaBase.Results.Sets.Datarow;
      Column_TS           :          Natural;
      Column_Humidity     :          Natural := 0;
      Column_Pressure     :          Natural := 0;
      Column_Temperature  :          Natural := 0;
      Column_Lux          :          Natural := 0;
   begin
      Ada.Float_Text_IO.Default_Fore := 0;
      Ada.Float_Text_IO.Default_Aft  := 4;
      Ada.Float_Text_IO.Default_Exp  := 0;
      DB_Connect_Res                 := Local_Defs.TFalse;
      List_Handlers.Sensor_Data_Map.Clear;

      if STMT_Data.execute then
         if STMT_Data.rows_returned = 0 then
            return;
         end if;
      else
         return;
      end if;

      Num_Rows := STMT_Data.rows_returned;

      if Num_Rows > 0 then
         Min_And_Max.Min_Humidity      :=     100.0;
         Min_And_Max.Max_Humidity      :=       0.0;
         Min_And_Max.Min_Pressure      :=   9_999.9;
         Min_And_Max.Max_Pressure      :=       0.0;
         Min_And_Max.Min_Pressure_Co   :=   9_999.9;
         Min_And_Max.Max_Pressure_Co   :=       0.0;
         Min_And_Max.Min_Temperature   :=     200.0;
         Min_And_Max.Max_Temperature   :=    -200.0;
         Min_And_Max.Min_Lux           := 100_000;
         Min_And_Max.Max_Lux           :=       0;
         Min_And_Max.Humidity_Valid    := False;
         Min_And_Max.Pressure_Valid    := False;
         Min_And_Max.Temperature_Valid := False;
         Min_And_Max.Lux_Valid         := False;

         process_column_names :
         for C in 1 .. STMT_Data.column_count loop
            if Ada.Strings.Equal_Case_Insensitive    (STMT_Data.column_name (C),
                                                      "ts")
            then
               Column_TS           := C;
            elsif Ada.Strings.Equal_Case_Insensitive (STMT_Data.column_name (C),
                                                      "humidity")
            then
               Column_Humidity     := C;
            elsif Ada.Strings.Equal_Case_Insensitive (STMT_Data.column_name (C),
                                                      "lux")
            then
               Column_Lux          := C;
            elsif Ada.Strings.Equal_Case_Insensitive (STMT_Data.column_name (C),
                                                      "pressure")
            then
               Column_Pressure     := C;
            elsif Ada.Strings.Equal_Case_Insensitive (STMT_Data.column_name (C),
                                                      "temperature")
            then
               Column_Temperature  := C;
            end if;
         end loop process_column_names;

         data_loop :
         loop
            declare
               Data : Local_Defs.Sensor_Data;
            begin
               Row  := STMT_Data.fetch_next;
               exit data_loop when Row.data_exhausted = True;

               Data.TS := Interfaces.C.long (Row.column (Column_TS).as_byte4);
               if Column_Humidity /= 0 then
                  Data.Humidity := Float'Value (Row.column (Column_Humidity).as_string);

                  if Data.Humidity > Min_And_Max.Max_Humidity then
                     Min_And_Max.Max_Humidity   := Data.Humidity;
                     Min_And_Max.Humidity_Valid := True;
                  end if;

                  if Data.Humidity < Min_And_Max.Min_Humidity then
                     Min_And_Max.Min_Humidity   := Data.Humidity;
                     Min_And_Max.Humidity_Valid := True;
                  end if;
               end if;

               if Column_Temperature /= 0 then
                  Data.Temperature := Float'Value (Row.column (Column_Temperature).as_string);

                  if Data.Temperature > Min_And_Max.Max_Temperature then
                     Min_And_Max.Temperature_Valid := True;
                     Min_And_Max.Max_Temperature   := Data.Temperature;
                  end if;

                  if Data.Temperature < Min_And_Max.Min_Temperature then
                     Min_And_Max.Temperature_Valid := True;
                     Min_And_Max.Min_Temperature   := Data.Temperature;
                  end if;
               end if;

               if Column_Lux /= 0 then
                  Data.Lux := Natural'Value (Row.column (Column_Lux).as_string);

                  if Data.Lux > Min_And_Max.Max_Lux then
                     Min_And_Max.Lux_Valid := True;
                     Min_And_Max.Max_Lux   := Data.Lux;
                  end if;

                  if Data.Lux < Min_And_Max.Min_Lux then
                     Min_And_Max.Lux_Valid := True;
                     Min_And_Max.Min_Lux   := Data.Lux;
                  end if;
               end if;

               if Column_Pressure /= 0 then
                  Data.Pressure                     := Float'Value (Row.column (Column_Pressure).as_string);
                  Data.Pressure_Co                  := Convert.To_Pressure_Corrected (Data.Pressure, Data.Temperature);

                  if Data.Pressure > Min_And_Max.Max_Pressure then
                     Min_And_Max.Pressure_Valid     := True;
                     Min_And_Max.Max_Pressure       := Data.Pressure;
                  end if;

                  if Data.Pressure < Min_And_Max.Min_Pressure then
                     Min_And_Max.Pressure_Valid     := True;
                     Min_And_Max.Min_Pressure       := Data.Pressure;
                  end if;

                  if Data.Pressure_Co > Min_And_Max.Max_Pressure_Co then
                     Min_And_Max.Pressure_Co_Valid  := True;
                     Min_And_Max.Max_Pressure_Co    := Data.Pressure_Co;
                  end if;

                  if Data.Pressure_Co < Min_And_Max.Min_Pressure_Co then
                     Min_And_Max.Pressure_Co_Valid  := True;
                     Min_And_Max.Min_Pressure_Co    := Data.Pressure_Co;
                  end if;
               end if;

               List_Handlers.Sensor_Data_Map.Insert (Data.TS, Data);
            exception
               when E : others =>
                  Ada.Text_IO.Put_Line ("TS: " &
                                          Data.TS'Image);
                  GNAT.Traceback.Call_Chain (Trace,
                                             Length);
                  Ada.Text_IO.Put_Line ("WTF!! rivet "                      &
                                          GNAT.Source_Info.Source_Location  &
                                          " "                               &
                                          Ada.Exceptions.Exception_Name (E) &
                                          " message: "                      &
                                          Ada.Exceptions.Exception_Message (E));
                  Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
            end;
         end loop data_loop;

         DB_Connect_Res := Local_Defs.TTrue;
      end if;
   exception
      when E : others =>
         if Local_Defs.Do_Syslog then
            GNAT.Traceback.Call_Chain (Trace, Length);
            Ada.Text_IO.Put_Line ("WTF!! rivet "                      &
                                    GNAT.Source_Info.Source_Location  &
                                    " "                               &
                                    Ada.Exceptions.Exception_Name (E) &
                                    " message: "                      &
                                    Ada.Exceptions.Exception_Message (E));
            Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
         end if;

         DB_Connect_Res := Local_Defs.TBroken;
   end Load_Data;

   procedure Load_Locations (DB_Connect_Res : out Local_Defs.Trilean) is
      Select_Env_Locs : constant String          := "SELECT sensor, location, dbtable, host, temperature, pressure, pressure_corrected, humidity, lux FROM locations";
      STMT_Env        :          Stmt_Type_Local := DR_Envir.prepare (Select_Env_Locs);
      Num_Rows        :          AdaBase.Affected_Rows;
      Row             :          AdaBase.Results.Sets.Datarow;
   begin
      DB_Connect_Res := Local_Defs.TFalse;
      if STMT_Env.execute then
         if STMT_Env.rows_returned = 0 then
            return;
         end if;
      else
         return;
      end if;

      Num_Rows := STMT_Env.rows_returned;

      if Num_Rows > 0 then
         process_loops_env :
         loop
            declare
               Data : Local_Defs.Location_Environ;
            begin
               Row                     := STMT_Env.fetch_next;
               exit process_loops_env when Row.data_exhausted;
               Data.Sensor             := Ada.Strings.Unbounded.To_Unbounded_String (Row.column (1).as_string);
               Data.Location           := Ada.Strings.Unbounded.To_Unbounded_String (Row.column (2).as_string);
               Data.DBTable            := Ada.Strings.Unbounded.To_Unbounded_String (Row.column (3).as_string);
               Data.Host               := Ada.Strings.Unbounded.To_Unbounded_String (Row.column (4).as_string);
               Data.Temperature        := Ada.Strings.Unbounded.To_Unbounded_String (Row.column (5).as_string);
               Data.Pressure           := Ada.Strings.Unbounded.To_Unbounded_String (Row.column (6).as_string);
               Data.Pressure_Corrected := Ada.Strings.Unbounded.To_Unbounded_String (Row.column (7).as_string);
               Data.Humidity           := Ada.Strings.Unbounded.To_Unbounded_String (Row.column (8).as_string);
               Data.Lux                := Ada.Strings.Unbounded.To_Unbounded_String (Row.column (9).as_string);
               List_Handlers.Location_Environ_Map.Insert (Data.Sensor,
                                                          Data);
            exception
               when E : others =>
                  GNAT.Traceback.Call_Chain (Trace, Length);
                  Ada.Text_IO.Put_Line ("WTF!! rivet "                      &
                                          GNAT.Source_Info.Source_Location  &
                                          " "                               &
                                          Ada.Exceptions.Exception_Name (E) &
                                          " message: "                      &
                                          Ada.Exceptions.Exception_Message (E));
                  Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
            end;
         end loop process_loops_env;

         DB_Connect_Res := Local_Defs.TTrue;
      end if;
   exception
      when E : others =>
         if Local_Defs.Do_Syslog then
            GNAT.Traceback.Call_Chain (Trace, Length);
            Ada.Text_IO.Put_Line ("WTF!! rivet "                      &
                                    GNAT.Source_Info.Source_Location  &
                                    " "                               &
                                    Ada.Exceptions.Exception_Name (E) &
                                    " message: "                      &
                                    Ada.Exceptions.Exception_Message (E));
            Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
         end if;

         DB_Connect_Res := Local_Defs.TBroken;
   end Load_Locations;

   procedure Set_Account_Details (Host, DB, User, Pass : Ada.Strings.Unbounded.Unbounded_String; Port : Integer := 3306) is
   begin
      Database := DB;
      DB_Host  := Host;
      DB_User  := User;
      DB_Pass  := Pass;
      DB_Port  := Port;
   end Set_Account_Details;
end DB_Routines;
