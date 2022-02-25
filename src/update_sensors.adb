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

with Ada.Calendar;
with Ada.Calendar.Conversions;
with Ada.Exceptions;
with Ada.Float_Text_IO;
with Ada.Integer_Text_IO;
with Ada.Long_Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO;
with GNAT.Source_Info;
with GNAT.Traceback.Symbolic;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with System;
with System.CRTL;
with Convert;
with DB_Routines;
with List_Handlers;
with Local_Defs;
with OWCapi;

package body Update_Sensors is
   procedure Iterate is
      pragma Warnings (Off, "OW_Get_Size");
      OW_Get_Size  : access System.CRTL.size_t;
      pragma Warnings (On, "OW_Get_Size");
      Data_Sys     :        Interfaces.C.Strings.chars_ptr;
      Loc_Env      :        Local_Defs.Location_Environ;
   begin
      Ada.Float_Text_IO.Default_Fore         := 5;
      Ada.Float_Text_IO.Default_Aft          := 4;
      Ada.Float_Text_IO.Default_Exp          := 0;
      Ada.Integer_Text_IO.Default_Width      := 5;
      Ada.Long_Integer_Text_IO.Default_Width := 0;
      List_Handlers.Data_Environ_Map.Clear;

      for Env_Cursor in List_Handlers.Location_Environ_Map.Iterate loop
         Loc_Env := List_Handlers.Location_Environ_Map.Reference (Env_Cursor);

         if Loc_Env.Host /= Ada.Strings.Unbounded.To_Unbounded_String ("X") and then Long_Integer (OWCapi.OW_init (Interfaces.C.Strings.New_String (Ada.Strings.Unbounded.To_String (Loc_Env.Host) & ":4304"))) = 0 then
            declare
               Result : Local_Defs.Data_Environ;
            begin
               Result.Host     := Loc_Env.Host;
               Result.Location := Loc_Env.Location;
               Result.Sensor   := Loc_Env.Sensor;

               if Ada.Strings.Unbounded.Length (Loc_Env.Temperature) /= 0 then
                  if Long_Integer (OWCapi.OW_get (Interfaces.C.Strings.New_String ("/"     &
                                     Ada.Strings.Unbounded.To_String (Loc_Env.Sensor)      &
                                     Ada.Strings.Unbounded.To_String (Loc_Env.Temperature) &
                                     "temperature"),
                                   Data_Sys'Address,
                                   OW_Get_Size)) > 0
                  then
                     Result.Temperature := Ada.Strings.Unbounded.To_Unbounded_String (Interfaces.C.Strings.Value (Data_Sys));
                     Result.Valid       := True;
                  end if;
                  Interfaces.C.Strings.Free (Data_Sys);
               end if;

               if Ada.Strings.Unbounded.Length (Loc_Env.Pressure) /= 0 then
                  if Long_Integer (OWCapi.OW_get (Interfaces.C.Strings.New_String ("/"  &
                                     Ada.Strings.Unbounded.To_String (Loc_Env.Sensor)   &
                                     Ada.Strings.Unbounded.To_String (Loc_Env.Pressure) &
                                     "pressure"),
                                   Data_Sys'Address,
                                   OW_Get_Size)) > 0
                  then
                     Result.Pressure := Ada.Strings.Unbounded.To_Unbounded_String (Interfaces.C.Strings.Value (Data_Sys));
                     Result.Valid    := True;
                  end if;
                  Interfaces.C.Strings.Free (Data_Sys);
               end if;

               if Ada.Strings.Unbounded.Length (Loc_Env.Humidity) /= 0 then
                  if Long_Integer (OWCapi.OW_get (Interfaces.C.Strings.New_String ("/"  &
                                     Ada.Strings.Unbounded.To_String (Loc_Env.Sensor)   &
                                     Ada.Strings.Unbounded.To_String (Loc_Env.Humidity) &
                                     "humidity"),
                                   Data_Sys'Address,
                                   OW_Get_Size)) > 0
                  then
                     Result.Humidity := Ada.Strings.Unbounded.To_Unbounded_String (Interfaces.C.Strings.Value (Data_Sys));
                     Result.Valid    := True;
                  end if;
                  Interfaces.C.Strings.Free (Data_Sys);
               end if;

               if Ada.Strings.Unbounded.Length (Loc_Env.Lux) /= 0 then
                  if Long_Integer (OWCapi.OW_get (Interfaces.C.Strings.New_String ("/" &
                                     Ada.Strings.Unbounded.To_String (Loc_Env.Sensor)  &
                                     Ada.Strings.Unbounded.To_String (Loc_Env.Lux)     &
                                     "light"),
                                   Data_Sys'Address,
                                   OW_Get_Size)) > 0
                  then
                     Result.Lux   := Ada.Strings.Unbounded.To_Unbounded_String (Interfaces.C.Strings.Value (Data_Sys));
                     Result.Valid := True;
                  end if;
                  Interfaces.C.Strings.Free (Data_Sys);
               end if;

               if Result.Valid then
                  List_Handlers.Data_Environ_Map.Insert (Result.Sensor, Result);
               end if;
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

            OWCapi.OW_finish;
         end if;
      end loop;

      if Local_Defs.Do_Console then
         Write_Details;
      end if;

      if Local_Defs.Do_DB_Update then
         Write_DB_List;
         DB_Routines.Execute_Statements;
      end if;
   exception when E : others =>
         GNAT.Traceback.Call_Chain (Trace, Length);
         Ada.Text_IO.Put_Line ("What happened? " &
                                 GNAT.Source_Info.Source_Location &
                                 " " &
                                 Ada.Exceptions.Exception_Name (E) &
                                 " message: " &
                                 Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
   end Iterate;

   procedure Write_DB_List is
      Current_Time : Interfaces.C.long := Ada.Calendar.Conversions.To_Unix_Time (Ada.Calendar.Clock);
   begin
      Ada.Integer_Text_IO.Default_Width := 0;
      Current_Time := Current_Time - Current_Time rem 600;

      for Result of List_Handlers.Data_Environ_Map loop
         declare
            SQL_UBString : Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.To_Unbounded_String ("");
            Sensor_UBStr : Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.To_Unbounded_String ("");
            Tmp_UBStr    : Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.To_Unbounded_String ("");
            Pre_UBStr    : Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.To_Unbounded_String ("");
            PrC_UBStr    : Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.To_Unbounded_String ("");
            Hum_UBStr    : Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.To_Unbounded_String ("");
            Lux_UBStr    : Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.To_Unbounded_String ("");
         begin
            Sensor_UBStr := Result.Sensor;
            SQL_UBString := "INSERT INTO Sensor_" &
              Ada.Strings.Unbounded.Delete (Sensor_UBStr, 3, 3) &
              " (ts";

            if Ada.Strings.Unbounded.Length (Result.Temperature) > 0 then
               Tmp_UBStr := "," & Result.Temperature;
               SQL_UBString := SQL_UBString & ",temperature";
            end if;

            if Ada.Strings.Unbounded.Length (Result.Pressure) > 0 then
               Pre_UBStr := "," & Result.Pressure;
               SQL_UBString := SQL_UBString & ",pressure";
            end if;

            if Ada.Strings.Unbounded.Length (Result.Pressure) > 0 then
               PrC_UBStr := "," & Convert.To_Pressure_Corrected (Result.Pressure, Result.Temperature);
               SQL_UBString := SQL_UBString & ",pressure_corrected";
            end if;

            if Ada.Strings.Unbounded.Length (Result.Humidity) > 0 then
               Hum_UBStr := "," & Result.Humidity;
               SQL_UBString := SQL_UBString & ",humidity";
            end if;

            if Ada.Strings.Unbounded.Length (Result.Lux) > 0 then
               Lux_UBStr := "," & Result.Lux;
               SQL_UBString := SQL_UBString & ",lux";
            end if;

            SQL_UBString := SQL_UBString & ") values (" & Current_Time'Image & Tmp_UBStr & Pre_UBStr & PrC_UBStr & Hum_UBStr & Lux_UBStr & ")";
            List_Handlers.SQL_Queue.Enqueue (SQL_UBString);
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
      end loop;
   exception when E : others =>
         GNAT.Traceback.Call_Chain (Trace, Length);
         Ada.Text_IO.Put_Line ("What happened? "                   &
                                 GNAT.Source_Info.Source_Location  &
                                 " "                               &
                                 Ada.Exceptions.Exception_Name (E) &
                                 " message: "                      &
                                 Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
   end Write_DB_List;

   procedure Write_Details is
      Max_Length : Natural := 0;
   begin
      for Result of List_Handlers.Data_Environ_Map loop
         if Ada.Strings.Unbounded.Length (Result.Host) > Max_Length then
            Max_Length := Ada.Strings.Unbounded.Length (Result.Host);
         end if;
      end loop;

      for Result of List_Handlers.Data_Environ_Map loop
         if Ada.Strings.Unbounded.Length (Result.Temperature) /= 0 then
            Write_Line (Result.Host,
                        Result.Sensor,
                        Local_Defs.Sensor_Console_Names (Local_Defs.Temperature),
                        Float'Value (Ada.Strings.Unbounded.To_String (Result.Temperature)));
         end if;
         if Ada.Strings.Unbounded.Length (Result.Humidity) /= 0 then
            Write_Line (Result.Host,
                        Result.Sensor,
                        Local_Defs.Sensor_Console_Names (Local_Defs.Humidity),
                        Float'Value (Ada.Strings.Unbounded.To_String (Result.Humidity)));
         end if;
         if Ada.Strings.Unbounded.Length (Result.Pressure) /= 0 then
            Write_Line (Result.Host,
                        Result.Sensor,
                        Local_Defs.Sensor_Console_Names (Local_Defs.Pressure),
                        Float'Value (Ada.Strings.Unbounded.To_String (Result.Pressure)));
            Write_Line (Result.Host,
                        Result.Sensor,
                        Local_Defs.Sensor_Console_Names (Local_Defs.Pressure_Corrected),
                        Convert.To_Pressure_Corrected (Result.Pressure, Result.Temperature));
         end if;
         if Ada.Strings.Unbounded.Length (Result.Lux) /= 0 then
            Write_Line (Result.Host,
                        Result.Sensor,
                        Local_Defs.Sensor_Console_Names (Local_Defs.Lux),
                        Integer'Value (Ada.Strings.Unbounded.To_String (Result.Lux)));
         end if;
      end loop;
   end Write_Details;

   procedure Write_Line (Host        : Ada.Strings.Unbounded.Unbounded_String;
                         Sensor_Name : Ada.Strings.Unbounded.Unbounded_String;
                         Sensor_Type : String;
                         F           : Ada.Strings.Unbounded.Unbounded_String) is
   begin
      if Host /= Ada.Strings.Unbounded.To_Unbounded_String ("localhost") then
         Ada.Strings.Unbounded.Text_IO.Put (Host);
      end if;
      Ada.Text_IO.Put                   (ASCII.HT);
      Ada.Strings.Unbounded.Text_IO.Put (Sensor_Name);
      Ada.Text_IO.Put                   (ASCII.HT);
      Ada.Text_IO.Put                   (Sensor_Type);
      Ada.Strings.Unbounded.Text_IO.Put (F);
      Ada.Text_IO.New_Line;
   end Write_Line;

   procedure Write_Line (Host, Sensor_Name : Ada.Strings.Unbounded.Unbounded_String;
                         Sensor_Type       : String;
                         F                 : Float) is
   begin
      if Host /= Ada.Strings.Unbounded.To_Unbounded_String ("localhost") then
         Ada.Strings.Unbounded.Text_IO.Put (Host);
      end if;
      Ada.Text_IO.Put                   (ASCII.HT);
      Ada.Strings.Unbounded.Text_IO.Put (Sensor_Name);
      Ada.Text_IO.Put                   (ASCII.HT);
      Ada.Text_IO.Put                   (Sensor_Type);
      Ada.Float_Text_IO.Put             (F);
      Ada.Text_IO.New_Line;
   end Write_Line;

   procedure Write_Line (Host, Sensor_Name : Ada.Strings.Unbounded.Unbounded_String;
                         Sensor_Type       : String;
                         I                 : Integer) is
   begin
      if Host /= Ada.Strings.Unbounded.To_Unbounded_String ("localhost") then
         Ada.Strings.Unbounded.Text_IO.Put (Host);
      end if;
      Ada.Text_IO.Put                   (ASCII.HT);
      Ada.Strings.Unbounded.Text_IO.Put (Sensor_Name);
      Ada.Text_IO.Put                   (ASCII.HT);
      Ada.Text_IO.Put                   (Sensor_Type);
      Ada.Integer_Text_IO.Put           (I);
      Ada.Text_IO.New_Line;
   end Write_Line;
end Update_Sensors;
