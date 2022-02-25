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

with Ada.Containers;        use Ada.Containers;
with Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Interfaces;            use Interfaces;
with Interfaces.C.Strings;
with System;                use System;
with List_Handlers;
with Local_Defs;
with Unix_Utils;

package body Write_Index is
   Base_Path   : constant String := "/srv/www/htdocs/env/";
   HTML_Base   : constant String := "h/";
   HTML_Week   : constant String := "Weekly";
   HTML_Month  : constant String := "Monthly";
   HTML_Year   : constant String := "Yearly";
   Output_File : constant String := Base_Path & "index";
   Non_Root    : constant String := "-test";
   Extension   : constant String := ".html";
   File_Handle :          Ada.Text_IO.File_Type;

   procedure Store_Directory (DN : Ada.Directories.Directory_Entry_Type) is
   begin
      List_Handlers.File_Set.Insert (Ada.Strings.Unbounded.To_Unbounded_String (Ada.Directories.Simple_Name (DN)));
   end Store_Directory;

   procedure Write_Header is
   begin
      Ada.Text_IO.Put_Line (File_Handle, "<!DOCTYPE html PUBLIC ""-//W3C//DTD XHTML 1.0 Strict//EN""  ""http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd""/>");
      Ada.Text_IO.Put_Line (File_Handle, "<html xmlns=""http://www.w3.org/1999/xhtml"" lang=""en-GB"" xml:lang=""en-GB"" dir=""ltr""/>");
      Ada.Text_IO.Put_Line (File_Handle, "<head>");
      Ada.Text_IO.Put_Line (File_Handle, "<title>Environment Graphs</title>");
      Ada.Text_IO.Put_Line (File_Handle, "<meta http-equiv=""Content-Type"" content=""text/html; charset=iso-8859-1""/>");
      Ada.Text_IO.Put_Line (File_Handle, "</head>");
      Ada.Text_IO.Put_Line (File_Handle, "<body>");
      Ada.Text_IO.Put_Line (File_Handle, "<h1>Environment Graphs - Humidity, Lux, Pressure, Temperature</h1>");
      Ada.Text_IO.Put_Line (File_Handle, "<p />");
      Ada.Text_IO.Put_Line (File_Handle, "<hr />");
   end Write_Header;

   procedure Write_Main_Index is
      CHOwn_Res   :          Interfaces.Integer_32;
      Write_Mode  : constant Ada.Text_IO.File_Mode          := Ada.Text_IO.Out_File;
      Web_User    :          Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.New_String ("wwwrun");
      Web_PWD     :          Unix_Utils.Password_Entry;
      Buffer_PWD  :          String (1 .. 2 ** 16);
      Buffer_Len  : constant Interfaces.Unsigned_64         := Buffer_PWD'Length;
      Result_PWD  :          System.Address;
      C_Ptr       :          Interfaces.C.Strings.chars_ptr;
   begin
      Ada.Integer_Text_IO.Default_Width := 0;

      if Unix_Utils.getuid = 0 then
         Ada.Text_IO.Create (File_Handle,
                             Write_Mode,
                             Output_File & Extension);
      else
         Ada.Text_IO.Create (File_Handle,
                             Write_Mode,
                             Output_File & Non_Root & Extension);
      end if;
      Write_Header;
      Write_Week;
      Write_Month;
      Write_Year;
      Write_Tail;
      Ada.Text_IO.Close (File_Handle);

      if Unix_Utils.getuid = 0 then
         CHOwn_Res := Unix_Utils.Get_PW_Name_R (Web_User,
                                                Web_PWD'Address,
                                                Buffer_PWD'Address,
                                                Buffer_Len,
                                                Result_PWD'Address);
         Interfaces.C.Strings.Free (Web_User);

         if Result_PWD = System.Null_Address or else CHOwn_Res /= 0 then
            Ada.Text_IO.Put_Line ("Get_PW_Name_R failed, Null returned or CHOwn_Res not zero: " &
                                    Web_PWD.pw_uid'Image                                        &
                                    ", "                                                        &
                                    Web_PWD.pw_gid'Image                                        &
                                    ", "                                                        &
                                    CHOwn_Res'Image);
            return;
         end if;

         if Unix_Utils.getuid = 0 then
            C_Ptr := Interfaces.C.Strings.New_String (Output_File & Extension);
            CHOwn_Res := Unix_Utils.chown (C_Ptr,
                                           Web_PWD.pw_uid,
                                           Web_PWD.pw_gid);
            Interfaces.C.Strings.Free (C_Ptr);

            if CHOwn_Res /= 0 then
               Ada.Text_IO.Put_Line ("A chown failed: " &
                                       CHOwn_Res'Image);
               return;
            end if;
         else
            C_Ptr := Interfaces.C.Strings.New_String (Output_File & Non_Root & Extension);
            CHOwn_Res := Unix_Utils.chown (C_Ptr,
                                           Web_PWD.pw_uid,
                                           Web_PWD.pw_gid);
            Interfaces.C.Strings.Free (C_Ptr);

            if CHOwn_Res /= 0 then
               Ada.Text_IO.Put_Line ("B chown failed: " &
                                       CHOwn_Res'Image);
               return;
            end if;
         end if;
      end if;
   end Write_Main_Index;

   procedure Write_Month is
      Month_Str    : String (1 .. 2);
      Year_Str     : String (1 .. 2);
      Year_Old_Str : String (1 .. 2) := "  ";
   begin
      List_Handlers.File_Set.Clear;

      if Unix_Utils.getuid = 0 then
         Ada.Directories.Search (Directory => Base_Path & HTML_Base & HTML_Month,
                                 Pattern   => "????" & Extension,
                                 Filter    => (Ada.Directories.Ordinary_File => True, others => False),
                                 Process   => Store_Directory'Access);
      else
         Ada.Directories.Search (Directory => Base_Path & HTML_Base & HTML_Month,
                                 Pattern   => "????" & Non_Root & Extension,
                                 Filter    => (Ada.Directories.Ordinary_File => True, others => False),
                                 Process   => Store_Directory'Access);
      end if;

      if List_Handlers.File_Set.Length = 0 then
         return;
      end if;

      Ada.Text_IO.Put_Line (File_Handle,
                            "<H2>Months</H2>");
      Ada.Text_IO.Put_Line (File_Handle,
                            "<H3>");

      for File_Name of List_Handlers.File_Set loop
         Month_Str := Ada.Strings.Unbounded.To_String (File_Name) (3 .. 4);
         Year_Str  := Ada.Strings.Unbounded.To_String (File_Name) (1 .. 2);

         if Year_Old_Str /= Year_Str then
            Ada.Text_IO.New_Line    (File_Handle);
            Ada.Text_IO.Put_Line    (File_Handle,
                                     "<P>");
            Ada.Integer_Text_IO.Put (File_Handle,
                                     2000 + Integer'Value (Year_Str));
            Ada.Text_IO.New_Line    (File_Handle);
            Year_Old_Str := Year_Str;
         end if;

         Ada.Text_IO.Put      (File_Handle,
                               "&nbsp<a href="                               &
                                 ASCII.Quotation                             &
                                 HTML_Base                                   &
                                 HTML_Month                                  &
                                 "/"                                         &
                                 Ada.Strings.Unbounded.To_String (File_Name) &
                                 ASCII.Quotation                             &
                                 ">");
         Ada.Text_IO.Put_Line (File_Handle,
                               Local_Defs.Short_Month_Names (Integer'Value (Month_Str)) &
                                 "</a>");
      end loop;

      Ada.Text_IO.Put_Line (File_Handle, "</H3>");
   end Write_Month;

   procedure Write_Tail is
   begin
      Ada.Text_IO.Put_Line (File_Handle,
                            "</body>");
      Ada.Text_IO.Put_Line (File_Handle,
                            "</html>");
      Ada.Text_IO.New_Line (File_Handle);
   end Write_Tail;

   procedure Write_Week is
      Month_Str    : String (1 .. 2);
      Year_Str     : String (1 .. 2);
      Year_Old_Str : String (1 .. 2) := "  ";
   begin
      List_Handlers.File_Set.Clear;

      if Unix_Utils.getuid = 0 then
         Ada.Directories.Search (Directory => Base_Path & HTML_Base & HTML_Week,
                                 Pattern   => "????" & Extension,
                                 Filter    => (Ada.Directories.Ordinary_File => True, others => False),
                                 Process   => Store_Directory'Access);
      else
         Ada.Directories.Search (Directory => Base_Path & HTML_Base & HTML_Week,
                                 Pattern   => "????" & Non_Root & Extension,
                                 Filter    => (Ada.Directories.Ordinary_File => True, others => False),
                                 Process   => Store_Directory'Access);
      end if;

      if List_Handlers.File_Set.Length = 0 then
         return;
      end if;

      Ada.Text_IO.Put_Line (File_Handle,
                            "<H2>Weeks</H2>");
      Ada.Text_IO.Put_Line (File_Handle,
                            "<H3>");

      for File_Name of List_Handlers.File_Set loop
         Month_Str := Ada.Strings.Unbounded.To_String (File_Name) (3 .. 4);
         Year_Str  := Ada.Strings.Unbounded.To_String (File_Name) (1 .. 2);

         if Year_Old_Str /= Year_Str then
            Ada.Text_IO.New_Line    (File_Handle);
            Ada.Text_IO.Put_Line    (File_Handle,
                                     "<P>");
            Ada.Integer_Text_IO.Put (File_Handle,
                                     2000 + Integer'Value (Year_Str));
            Ada.Text_IO.New_Line    (File_Handle);
            Year_Old_Str := Year_Str;
         end if;

         Ada.Text_IO.Put            (File_Handle,
                                     "&nbsp<a href="                               &
                                       ASCII.Quotation                             &
                                       HTML_Base                                   &
                                       HTML_Week                                   &
                                       "/"                                         &
                                       Ada.Strings.Unbounded.To_String (File_Name) &
                                       ASCII.Quotation                             &
                                       ">");
         Ada.Text_IO.Put_Line       (File_Handle,
                                     Local_Defs.Short_Month_Names (Integer'Value (Month_Str)) &
                                       "</a>");
      end loop;

      Ada.Text_IO.Put_Line          (File_Handle, "</H3>");
   end Write_Week;

   procedure Write_Year is
      Year_Str : String (1 .. 2);
   begin
      List_Handlers.File_Set.Clear;

      if Unix_Utils.getuid = 0 then
         Ada.Directories.Search (Directory => Base_Path & HTML_Base & HTML_Year,
                                 Pattern   => "??01" & Extension,
                                 Filter    => (Ada.Directories.Ordinary_File => True, others => False),
                                 Process   => Store_Directory'Access);
      else
         Ada.Directories.Search (Directory => Base_Path & HTML_Base & HTML_Year,
                                 Pattern   => "??01" & Non_Root & Extension,
                                 Filter    => (Ada.Directories.Ordinary_File => True, others => False),
                                 Process   => Store_Directory'Access);
      end if;

      if List_Handlers.File_Set.Length = 0 then
         return;
      end if;

      Ada.Text_IO.Put_Line (File_Handle, "<H2>Years</H2>");
      Ada.Text_IO.Put_Line (File_Handle, "<H3>");

      for File_Name of List_Handlers.File_Set loop
         Year_Str := Ada.Strings.Unbounded.To_String (File_Name) (1 .. 2);
         Ada.Text_IO.Put         (File_Handle,
                                  "&nbsp<a href="                               &
                                    ASCII.Quotation                             &
                                    HTML_Base                                   &
                                    HTML_Year                                   &
                                    "/"                                         &
                                    Ada.Strings.Unbounded.To_String (File_Name) &
                                    ASCII.Quotation                             &
                                    ">");
         Ada.Integer_Text_IO.Put (File_Handle,
                                  2000 + Integer'Value (Year_Str));
         Ada.Text_IO.Put_Line    (File_Handle,
                                  "</a>");
      end loop;

      Ada.Text_IO.Put_Line (File_Handle,
                            "</H3>");
   end Write_Year;
end Write_Index;
