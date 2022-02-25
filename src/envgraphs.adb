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

with Ada.Calendar.Conversions;
with Ada.Calendar;                  use Ada.Calendar;
with Ada.Containers;                use Ada.Containers;
with Ada.Directories;               use Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO;
with Interfaces.C;                  use Interfaces.C;
with GNAT.Command_Line;
with GNAT.Source_Info;
with GNAT.Traceback.Symbolic;
with Construct_SVG;
with Date_Functions;
with DB_Routines;
with List_Handlers;
with Local_Defs;                    use Local_Defs;
with Update_Sensors;
with Write_Image_Index;
with Write_Index;
with Unicode_Symbols;
with Config_Handler;

procedure Envgraphs is
   Do_Epoch       :           Boolean           := False;
   Do_Seconds     :           Boolean           := False;
   Second_Stamp   :           Interfaces.C.long := 0;
   Start_Time_Ada :           Ada.Calendar.Time;
   End_Time_Ada   :           Ada.Calendar.Time;
   DB_Connect_Res :           Local_Defs.Trilean;
   Meta_Data      :           Local_Defs.Meta_Data_Type;
   Graph_Period   :           Local_Defs.Graph_Period_Type;
   Config         :           GNAT.Command_Line.Command_Line_Configuration;
   Min_And_Max    :           Local_Defs.MM;
   Trace          :           GNAT.Traceback.Tracebacks_Array (1 .. 1_000);
   Length         :           Natural;
   Current_Time   : constant  Ada.Calendar.Time := Ada.Calendar.Clock;
   XML_Settings   :           Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.To_Unbounded_String ("/etc/envgraphs.xml");
   CMD_Args       : exception;
   Error_Epoch    : exception;
begin
   GNAT.Command_Line.Define_Switch (Config, "-c", Help => "Ouput environmental data to console");
   GNAT.Command_Line.Define_Switch (Config, "-d", Help => "Store environmental data to Maria database");
   GNAT.Command_Line.Define_Switch (Config, "-e", Help => "Restart from earliest point in time for which data exists");
   GNAT.Command_Line.Define_Switch (Config, "-g", Help => "Construct SVG graphs");
   GNAT.Command_Line.Define_Switch (Config, "-i", Help => "Construct all indexes of data");
   GNAT.Command_Line.Define_Switch (Config, "-s", Help => "<seconds> restart from this point in time");
   GNAT.Command_Line.Define_Switch (Config, "-x",  Help => "<seconds> XML DB config file");
   Second_Stamp := Ada.Calendar.Conversions.To_Unix_Time (Ada.Calendar.Clock) - 60;

   loop
      case GNAT.Command_Line.Getopt ("c d e g i s: x:") is
         when 'c' => Do_Console   := True;
         when 'd' => Do_DB_Update := True;
         when 'e' => Do_Epoch     := True;
         when 'g' => Do_Graphs    := True;
         when 'i' => Do_Indexes   := True;
         when 's' => Do_Seconds   := True;
            Second_Stamp := Interfaces.C.long'Value (GNAT.Command_Line.Parameter);
         when 'x' =>
            XML_Settings := Ada.Strings.Unbounded.To_Unbounded_String (GNAT.Command_Line.Parameter);
         when ASCII.NUL => exit;
            when others => GNAT.Command_Line.Display_Help (Config);
            return;
      end case;
   end loop;

   GNAT.Command_Line.Free (Config);

   if not Ada.Directories.Exists (Ada.Strings.Unbounded.To_String (XML_Settings)) then
      raise CMD_Args with "Problem reading " & Ada.Strings.Unbounded.To_String (XML_Settings);
   end if;

   if Ada.Directories.Kind (Ada.Strings.Unbounded.To_String (XML_Settings)) /= Ada.Directories.Ordinary_File then
      raise CMD_Args with "Config file does not appear to be an ordinary file";
   end if;

   Config_Handler.Set_Config_Name (XML_Settings);
   Config_Handler.Load_Config;

   if DB_Routines.DB_Connect /= Local_Defs.TTrue then
      raise Program_Error;
   end if;

   DB_Routines.Load_Locations (DB_Connect_Res);

   if DB_Connect_Res /= Local_Defs.TTrue then
      Ada.Strings.Unbounded.Text_IO.Put_Line (Ada.Strings.Unbounded.To_Unbounded_String ("DB_Routines.Get_Locations failed"));
   end if;

   if Do_Epoch and then Do_Seconds then
      raise Error_Epoch with "Cannot supply time of start (-s) & epoch (-e beginning of data)";
   end if;

   if Do_Epoch then
      Second_Stamp := DB_Routines.Get_Earliest_TS;
   end if;

   if Second_Stamp < 0 then
      Second_Stamp := Ada.Calendar.Conversions.To_Unix_Time (Ada.Calendar.Clock) - Second_Stamp;
   end if;

   if Local_Defs.Do_Console or else Local_Defs.Do_DB_Update then
      Update_Sensors.Iterate;
   end if;

   if Local_Defs.Do_Graphs then
      Unicode_Symbols.Setup_Symbols;
      for Period in Local_Defs.Period_Type loop
         Start_Time_Ada := Ada.Calendar.Conversions.To_Ada_Time (Second_Stamp);

         while Start_Time_Ada <= Current_Time loop
            for S of List_Handlers.Location_Environ_Map loop
               Date_Functions.Get_Date_Type (Start_Time_Ada,
                                             Period,
                                             End_Time_Ada,
                                             Graph_Period);
               Meta_Data.Host        := S.Host;
               Meta_Data.Location    := S.Location;
               Meta_Data.Period      := Period;
               Meta_Data.Sensor_Name := S.Sensor;
               Meta_Data.Date_Start  := Start_Time_Ada;
               Meta_Data.Date_End    := End_Time_Ada;
               Meta_Data.GPT         := Graph_Period;
               DB_Routines.Load_Data (DB_Connect_Res, Start_Time_Ada, End_Time_Ada, List_Handlers.Location_Environ_Map.Element (S.Sensor).DBTable, Min_And_Max);

               if List_Handlers.Sensor_Data_Map.Length > 0 then
                  Construct_SVG.Process_All_Data (Meta_Data, Min_And_Max);
               end if;
            end loop;

            Start_Time_Ada := End_Time_Ada;
         end loop;
      end loop;
   end if;

   DB_Routines.DB_Disconnect;

   if Local_Defs.Do_Indexes then
      for Period in Local_Defs.Period_Type loop
         Write_Image_Index.Write_HTML_Record (Period);
      end loop;

      Write_Index.Write_Main_Index;
   end if;
exception
   when GNAT.Command_Line.Invalid_Switch => GNAT.Command_Line.Display_Help (Config);
   when E : others =>
      GNAT.Traceback.Call_Chain (Trace, Length);
      Ada.Text_IO.Put_Line ("WTF!! rivet "                      &
                              GNAT.Source_Info.Source_Location  &
                              " "                               &
                              Ada.Exceptions.Exception_Name (E) &
                              " message: "                      &
                              Ada.Exceptions.Exception_Message (E));
      Ada.Text_IO.Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (Trace (1 .. Length)));
end Envgraphs;
