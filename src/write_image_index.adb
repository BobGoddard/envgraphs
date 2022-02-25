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
with Ada.Containers;        use Ada.Containers;
with Ada.Exceptions;
with Ada.Directories;
with Ada.Integer_Text_IO;
with Ada.Strings.Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with Interfaces.C;
with Interfaces.C.Strings;
with System; use System;
with GNAT.Calendar;
with GNAT.Directory_Operations;
with GNAT.Directory_Operations.Iteration;
with GNAT.Source_Info;
with GNAT.Traceback.Symbolic;
with GNAT.Traceback;
with List_Handlers;         use List_Handlers;
with Unix_Utils;

package body Write_Image_Index is
   Base_Dir     : constant String := "/srv/www/htdocs/";
   Env_Dir      : constant String := "env/";
   Image_Dir    : constant String := "i/";
   HTML_Dir     : constant String := "h/";
   Start_Year   :          String (1 .. 2);
   Start_Month  :          String (1 .. 2);
   Earliest     :          Integer;
   File_Handle  :          Ada.Text_IO.File_Type;
   Write_Mode   : constant Ada.Text_IO.File_Mode := Ada.Text_IO.Out_File;
   Trace        :          GNAT.Traceback.Tracebacks_Array (1 .. 1_000);
   Length       :          Natural;

   procedure Find_Earliest_File (Start_Directory : String) is procedure Earliest_File is new GNAT.Directory_Operations.Iteration.Find (Update_Start_Time);
   begin
      Earliest_File (Start_Directory, ".*\.svg");
   end Find_Earliest_File;

   procedure Find_SVG_File (Start_Directory : String;
                            SVG_Regex       : String) is procedure SVG_File is new GNAT.Directory_Operations.Iteration.Find (Store_Directory);
   begin
      SVG_File (Start_Directory, SVG_Regex);
   end Find_SVG_File;

   procedure Store_Directory (Name        : String;
                              Store_Index : Positive;
                              Quit        : in out Boolean) is
      pragma Unreferenced (Store_Index);
   begin
      Quit := False;
      List_Handlers.Directory_Set.Insert (Ada.Strings.Unbounded.To_Unbounded_String (Name));
   end Store_Directory;

   procedure Update_Start_Time (Name        : String;
                                Store_Index : Positive;
                                Quit        : in out Boolean) is
      pragma Unreferenced (Store_Index);
      TMP_Time : Integer;
      BS       : constant String := GNAT.Directory_Operations.Base_Name (Name);
   begin
      Quit := False;
      TMP_Time := Integer'Value (BS (BS'First .. BS'First + 5));

      if TMP_Time < Earliest then
         Earliest := TMP_Time;
      end if;
   end Update_Start_Time;

   procedure Write_Data (Period : Local_Defs.Period_Type) is
      Sensor_Name               : Ada.Strings.Unbounded.Unbounded_String;
      Sensor_Code               : Ada.Strings.Unbounded.Unbounded_String;
      SVG_File                  : Ada.Strings.Unbounded.Unbounded_String;
      Dir_Name                  : Ada.Strings.Unbounded.Unbounded_String;
      Current_Name              : Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.To_Unbounded_String ("");
      Current_Code              : Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.To_Unbounded_String ("");
   begin
      for s of List_Handlers.Directory_Set loop
         SVG_File    := Ada.Strings.Unbounded.To_Unbounded_String (GNAT.Directory_Operations.Base_Name (Ada.Strings.Unbounded.To_String (s)));
         Dir_Name    := Ada.Strings.Unbounded.To_Unbounded_String (GNAT.Directory_Operations.Dir_Name  (Ada.Strings.Unbounded.To_String (s)));
         Dir_Name    := Ada.Strings.Unbounded.Head                                                     (Dir_Name, Ada.Strings.Unbounded.Length (Dir_Name) - 1);
         Sensor_Code := Ada.Strings.Unbounded.To_Unbounded_String                                      (GNAT.Directory_Operations.Base_Name (Ada.Strings.Unbounded.To_String (Dir_Name)));
         Dir_Name    := Ada.Strings.Unbounded.To_Unbounded_String                                      (GNAT.Directory_Operations.Dir_Name  (Ada.Strings.Unbounded.To_String (Dir_Name)));
         Dir_Name    := Ada.Strings.Unbounded.Head (Dir_Name, Ada.Strings.Unbounded.Length (Dir_Name) - 1);
         Sensor_Name := Ada.Strings.Unbounded.To_Unbounded_String                                      (GNAT.Directory_Operations.Base_Name (Ada.Strings.Unbounded.To_String (Dir_Name)));

         if Current_Name = Sensor_Name then
            if Current_Code /= Sensor_Code then
               Ada.Strings.Unbounded.Text_IO.Put_Line (File_Handle, "<h2>" & Sensor_Code & " - " & List_Handlers.Location_Environ_Map.Element (Sensor_Code).Location & "</h2></p />");
               Current_Code := Sensor_Code;
            end if;
            Ada.Strings.Unbounded.Text_IO.Put_Line (File_Handle, "<object data=""/" & Env_Dir & Image_Dir & Local_Defs.Period_Names (Period) & "/" & Sensor_Name & "/" & Sensor_Code & "/" & SVG_File & """ type=""image/svg+xml""" & "></object><br /><br />");
         else
            Ada.Strings.Unbounded.Text_IO.Put_Line (File_Handle, "<h1>" & Ada.Strings.Unbounded.Translate (Sensor_Name, Ada.Strings.Maps.To_Mapping ("_C", " c")) & "</h1><p />");
            Ada.Strings.Unbounded.Text_IO.Put_Line (File_Handle, "<h2>" & Sensor_Code & " - " & List_Handlers.Location_Environ_Map.Element (Sensor_Code).Location & "</h2></p />");
            Ada.Strings.Unbounded.Text_IO.Put_Line (File_Handle, "<object data=""/" & Env_Dir & Image_Dir & Local_Defs.Period_Names (Period) & "/" & Sensor_Name & "/" & Sensor_Code & "/" & SVG_File & """ type=""image/svg+xml""" & "></object><br /><br />");
            Current_Name := Sensor_Name;
            Current_Code := Sensor_Code;
         end if;
      end loop;
   end Write_Data;

   procedure Write_Header (Period : Local_Defs.Period_Type) is
   begin
      Ada.Text_IO.Put_Line (File_Handle, "<!DOCTYPE html PUBLIC ""-//W3C//DTD XHTML 1.0 Strict//EN""  ""http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd""/>");
      Ada.Text_IO.Put_Line (File_Handle, "<html xmlns=""http://www.w3.org/1999/xhtml"" lang=""en-GB"" xml:lang=""en-GB"" dir=""ltr""/>");
      Ada.Text_IO.Put_Line (File_Handle, "<head>");
      Ada.Text_IO.Put_Line (File_Handle, "<title>Environment Graphs - " & Ada.Strings.Unbounded.To_String (Local_Defs.Period_Names (Period)) & "</title>");
      Ada.Text_IO.Put_Line (File_Handle, "<meta http-equiv=""Content-Type"" content=""text/html; charset=iso-8859-1""/>");
      Ada.Text_IO.Put_Line (File_Handle, "</head>");
      Ada.Text_IO.Put_Line (File_Handle, "<body>");
      Ada.Text_IO.Put_Line (File_Handle, "<h1>" & Ada.Strings.Unbounded.To_String (Local_Defs.Period_Names (Period)) & " Environment Graphs - Humidity, Lux, Pressure, Temperature</h1>");
      Ada.Text_IO.Put_Line (File_Handle, "<p />");
      Ada.Text_IO.Put_Line (File_Handle, "<hr />");
   end Write_Header;

   procedure Write_HTML_Record (Period : Local_Defs.Period_Type) is
      Current_Date :          Local_Defs.TM;
      Index_Date   :          Local_Defs.TM;
      CHOwn_Res    :          Interfaces.Integer_32;
      Web_User     :          Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.New_String ("wwwrun");
      Web_PWD      :          Unix_Utils.Password_Entry;
      Buffer_PWD   :          String (1 .. 2 ** 16);
      Buffer_Len   : constant Interfaces.Unsigned_64         := Buffer_PWD'Length;
      Result_PWD   :          System.Address;
      C_Ptr        :          Interfaces.C.Strings.chars_ptr;
   begin
      GNAT.Calendar.Split_At_Locale (Ada.Calendar.Clock,
                                     Current_Date.Year,
                                     Current_Date.Month,
                                     Current_Date.Day,
                                     Current_Date.Hour,
                                     Current_Date.Minute,
                                     Current_Date.Second,
                                     Current_Date.Sub_Second);
      Ada.Integer_Text_IO.Default_Width := 1;
      List_Handlers.File_Set.Clear;
      Earliest := Integer'Last;
      Ada.Directories.Set_Directory (Base_Dir);
      Find_Earliest_File (Env_Dir     &
                            Image_Dir &
                            Ada.Strings.Unbounded.To_String (Local_Defs.Period_Names (Period)));

      if Earliest = Integer'Last then
         return;
      end if;

      Index_Date.Year  := 2000 + Earliest / 100 / 100;
      Index_Date.Month := (Earliest / 100) rem 100;
      Ada.Integer_Text_IO.Put (Start_Year,
                               Index_Date.Year - 2000);
      Ada.Integer_Text_IO.Put (Start_Month,
                               Index_Date.Month);

      date_sequence :
      loop
         List_Handlers.Directory_Set.Clear;
         Ada.Integer_Text_IO.Put (Start_Year,
                                  Index_Date.Year - 2000);
         Ada.Integer_Text_IO.Put (Start_Month,
                                  Index_Date.Month);

         if Start_Month (1) = ' ' then
            Start_Month (1) := '0';
         end if;

         Find_SVG_File (Env_Dir       &
                          Image_Dir   &
                          Ada.Strings.Unbounded.To_String (Local_Defs.Period_Names (Period)),
                        Start_Year    &
                          Start_Month &
                          ".*.svg");

         declare
            Path_Name : constant String := Base_Dir                                          &
                          Env_Dir                                                            &
                          HTML_Dir                                                           &
                          Ada.Strings.Unbounded.To_String (Local_Defs.Period_Names (Period)) &
                          "/"                                                                &
                          Start_Year                                                         &
                          Start_Month;
            Non_Root  : constant String := "-test";
            Extension : constant String := ".html";
         begin
            if List_Handlers.Directory_Set.Length > 0 then
               if Unix_Utils.getuid = 0 then
                  Ada.Text_IO.Create (File_Handle,
                                      Write_Mode,
                                      Path_Name & Extension);
               else
                  Ada.Text_IO.Create (File_Handle,
                                      Write_Mode,
                                      Path_Name & Non_Root & Extension);
               end if;
               Write_Header (Period);
               Write_Data   (Period);
               Write_Tail;
               Ada.Text_IO.Close (File_Handle);

               if Unix_Utils.getuid = 0 then
                  CHOwn_Res := Unix_Utils.Get_PW_Name_R (Web_User,
                                                         Web_PWD'Address,
                                                         Buffer_PWD'Address,
                                                         Buffer_Len,
                                                         Result_PWD'Address);

                  if Result_PWD = System.Null_Address or else CHOwn_Res /= 0 then
                     Ada.Text_IO.Put_Line ("Get_PW_Name_R failed, Null returned or CHOwn_Res not zero: " &
                                             Web_PWD.pw_uid'Image                                        &
                                             ", "                                                        &
                                             Web_PWD.pw_gid'Image                                        &
                                             ", "                                                        &
                                             CHOwn_Res'Image);
                     Interfaces.C.Strings.Free (Web_User);
                     return;
                  end if;

                  C_Ptr := Interfaces.C.Strings.New_String (Path_Name & Extension);
                  CHOwn_Res := Unix_Utils.chown (C_Ptr,
                                                 Web_PWD.pw_uid,
                                                 Web_PWD.pw_gid);
                  Interfaces.C.Strings.Free (C_Ptr);

                  if CHOwn_Res /= 0 then
                     Ada.Text_IO.Put_Line ("C chown failed - " & Path_Name & Extension & " - " & CHOwn_Res'Image);
                     Interfaces.C.Strings.Free (Web_User);
                     return;
                  end if;
               end if;
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

         if Period = Local_Defs.Year then
            Index_Date.Year := Index_Date.Year + 1;

            if Index_Date.Year > Current_Date.Year then
               exit date_sequence;
            end if;
         else
            if Index_Date.Month = Ada.Calendar.Month_Number'Last then
               Index_Date.Month := Ada.Calendar.Month_Number'First;
               Index_Date.Year  := Index_Date.Year + 1;
            else
               Index_Date.Month := Index_Date.Month + 1;
            end if;

            if Index_Date.Year > Current_Date.Year then
               exit date_sequence;
            end if;

            if Index_Date.Year = Current_Date.Year and then Index_Date.Month > Current_Date.Month then
               exit date_sequence;
            end if;
         end if;

         List_Handlers.File_Set.Clear;
      end loop date_sequence;

      pragma Warnings (Off, "value might not be referenced");
      Interfaces.C.Strings.Free (Web_User);
      pragma Warnings (On, "value might not be referenced");
   end Write_HTML_Record;

   procedure Write_Tail is
   begin
      Ada.Text_IO.Put_Line (File_Handle,
                            "</body>");
      Ada.Text_IO.Put_Line (File_Handle,
                            "</html>");
      Ada.Text_IO.New_Line (File_Handle);
   end Write_Tail;
end Write_Image_Index;
